package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet,Queue,ArrayBuffer}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait PIRSplitter extends Traversal with PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR._

  // Assumed there is only up to one vector local for now

  override val name = "PIR Splitting"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  def allocateCU(pipe: Exp[Any]): ComputeUnit = cuMapping(pipe)

  val cus = HashMap[Exp[Any], List[ComputeUnit]]()

  override def run[A:Manifest](b: Block[A]) = {
    debug(s"\n\n\n")
    for ((pipe,cu) <- cuMapping) {
      val splitCUs = splitCU(cu)
      cus += pipe -> splitCUs
    }
    (b)
  }

  def splitCU(cu: ComputeUnit): List[ComputeUnit] = cu match {
    case tu: TileTransferUnit => List(tu)
    case cu: BasicComputeUnit => splitComputeCU(cu)
  }

  /**
    CU splitting is graph partitioning, where each partition has a limited number of inputs, outputs, and nodes

    Given a set of stages, we want to find a set of minimum cuts where each partition satisfies these limits

    This is more restricted/complex than the standard graph partitioning problem as nodes and edges cannot be treated uniformly:
    - Reduction stages represent multiple real ALU stages in the architecture (logV + 1 ?)
    - Reduction stages cannot directly consume CU inputs (requires a bypass stage)
    - Reduction stages cannot directly produce CU outputs (requires a bypass stage)
    - Write address computation stages MUST be preserved/copied within consumer CUs
    - Local vector writes: address and data must be available in the same CU

    Nodes must be duplicated/created depending on how the graph is partitioned
    Could model this as conditional costs for nodes in context of their partition?
   **/

  val REDUCE_STAGES = 5  // Number of stages required to do a full reduction

  class SplitCost(scalarIn: Int, scalarOut: Int, vectorIn: Int, vectorOut: Int, vectorLocal: Int, compute: Int)
  case object ComputeMax extends SplitCost(scalarIn=4, scalarOut=4, vectorIn=4, vectorOut=4, vectorLocal=1, compute=10)
  case object UnitMax    extends SplitCost(scalarIn=1, scalarOut=1, vectorIn=1, vectorOut=1, vectorLocal=1, compute=10)

  def inputsOf(stage: Stage): List[LocalMem] = {
    stage.inputMems.filterNot(stage.outputMems contains _ ) // Ignore self-cycles
  }
  def outputsOf(stage: Stage) = stage.outputMems

  def computeCost(stages: Set[Stage], unstages: Set[Stage])(implicit cu: BasicComputeUnit) = {
    val allIns       = stages.flatMap(inputsOf(_))
    val allOuts      = stages.flatMap(outputsOf(_))
    val externalUsed = unstages.flatMap(inputsOf(_))
    val liveOuts     = allOuts.filter(externalUsed contains _)

    val rawStageCosts = stages.map{
      case stage:MapStage => 1
      case ReduceStage(op,init,in,acc) =>
        val bypassInputCost  = if (allOuts.contains(in.reg)) 0 else 1     // Needs bypass added at input
        val bypassOutputCost = if (liveOuts.contains(acc)) 1 else 0   // Needs bypass added at output
        REDUCE_STAGES + bypassOutputCost + bypassInputCost
    }
    val readSRAMs = allIns.flatMap{case SRAMRead(mem) => Some(mem); case _ => None}

    // TODO: SRAM cost?
    rawStageCosts.fold(0){_+_}
  }

  def partitionCost(stages: Set[Stage], unstages: Set[Stage])(implicit cu: BasicComputeUnit) = {
    val scalarsIn  = stages.flatMap{stage => inputsOf(stage).filter{case _:ScalarIn => true; case _ => false }}
    val scalarsOut = stages.flatMap{stage => outputsOf(stage).filter{case _:ScalarOut => true; case _ => false}}
    val vectorsIn  = stages.flatMap{stage => inputsOf(stage).filter{case _:SRAMRead => true; case _:VectorIn => true; case _ => false }}
    val vectorsOut = stages.flatMap{stage => outputsOf(stage).filter{case _:VectorOut => true; case _ => false }}
    val vectorsLocal = stages.flatMap{stage => outputsOf(stage).filter{case _:VectorLocal => true; case _ => false }}
    val compute = computeCost(stages, unstages)
    new SplitCost(scalarsIn.size, scalarsOut.size, vectorsIn.size, vectorsOut.size, vectorsLocal.size, compute)
  }

  def splitComputeCU(cu: BasicComputeUnit): List[BasicComputeUnit] = List(cu) /*{
    debug(s"Splitting CU: $cu")


    val stages = cu.stages.toSet



    def requiresSplitting(stages: Set[Stage]) = {

      val nStages = stages.map{case _:MapStage => 1; case _:ReduceStage => REDUCE_STAGES }.fold(0)(_+_)

      debug(s"Stages: ")
      stages.foreach{stage => debug(s"  $stage") }
      debug(s"Scalar inputs: $scalarsIn")
      debug(s"Scalar outputs: $scalarsOut")
      debug(s"Vector inputs: $vectorsIn")
      debug(s"Vector outputs: $vectorsOut")
      debug(s"Vector locals: $vectorsLocal")
      debug(s"Actual stages: $nStages")

      scalarsIn.size > MAX_SCALAR_IN || scalarsOut.size > MAX_SCALAR_OUT || nStages > MAX_STAGES ||
      vectorsIn.size > MAX_VECTOR_IN || vectorsOut.size > MAX_VECTOR_OUT || vectorsLocal.size > MAX_VECTOR_LOCAL
    }


    def getScheduleFor(stages: Set[Stage])(result: Stage*) = {
      val frontier = Queue[Stage](result:_*)
      var visited  = HashSet[Stage](result:_*)
      var schedule = ArrayBuffer[Stage]()

      while (frontier.nonEmpty) {
        val stage = frontier.dequeue()
        schedule += stage
        val deps = inputsOf(stage).flatMap{in => stages.find{stage => outputsOf(stage) contains in}}.filterNot(visited contains _)
        frontier.enqueue(deps:_*)
        visited ++= deps
      }
      schedule.toList
    }

    val stagesSet = cu.stages.toSet

    val schedules = Map(outStages.map{stage => stage -> getScheduleFor(stagesSet)(stage).toSet }:_*)

    val unusedOutputs = HashSet[Stage]() ++ outStages
    val groups = ArrayBuffer[HashSet[Stage]]()
    def curGroup = groups.last
    var groupSchedule: Set[Stage] = Set.empty

    while (unusedOutputs.nonEmpty) {
      if (groups.isEmpty || groupIsFull(curGroup)) {
        val stage = unusedOutputs.head
        groups += HashSet(stage)
        unusedOutputs -= stage
        groupSchedule = schedules(stage)
      }
      else {
        val stage = unusedOutputs.map{stage => (stage, (groupSchedule union schedules(stage)).size)}.reduce{(a,b) =>
            if (a._2 > b._2) a._1 else b._1
        }
        curGroup += stage
        unusedOutputs -= stage
        groupSchedule ++= schedules(stage)
      }
    }



    /*val computedOutputs = HashMap[Stage, List[LocalMem]]()

    def bfs(frontier: List[Stage], live: List[List[LocalMem]]): Unit = if (frontier.nonEmpty) {
      debug(s"Frontier:")
      frontier.zip(live).foreach{case (stage, liveIn) =>
        debug(s"  $stage")
        debug(s"  $liveIn")
        debug("")
        liveIns(stage) = liveIn
      }

      val inputLivePairs = frontier.zip(live).flatMap{case (stage,liveIn) =>
        inputsOf(stage).map{input => input -> (liveIn :+ input).filterNot(outputsOf(stage) contains _) }
      }
      val inputMap = inputLivePairs.groupBy(_._1).mapValues{lists => lists.map(_._2).flatten.distinct }
      debug(s"Input Mapping: ")
      for ((input,lives) <- inputMap) {
        debug(s"$input : (${lives.length}) - $lives ")
      }
      val inputList: List[(LocalMem, List[LocalMem])] = inputMap.toList
      val inputLiveList: List[(Stage, List[LocalMem])] = inputList.flatMap{case (input,liveIn) => cu.stages.find{stage => outputsOf(stage).contains(input) }.map(stage => stage -> liveIn) }
      val newFrontier = inputLiveList.map(_._1)
      val newLiveIns  = inputLiveList.map(_._2)
      bfs(newFrontier, newLiveIns)
    }

    bfs(outStages, liveOuts)*/
  }*/

}

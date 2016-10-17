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

  case class SplitCost(scalarIn: Int, scalarOut: Int, vectorIn: Int, vectorOut: Int, vectorLocal: Int, compute: Int, write: Int, read: Int, mems: Int) {
    def >(that: SplitCost) = (this.scalarIn > that.scalarIn || this.scalarOut > that.scalarOut ||
                               this.vectorIn > that.vectorIn || this.vectorOut > that.vectorOut ||
                               this.vectorLocal > that.vectorLocal ||
                               this.compute > (that.compute+that.write) ||
                               this.write > that.write ||
                               this.read > that.read ||
                               this.mems > that.mems)
  }
  val ComputeMax = SplitCost(scalarIn=4, scalarOut=4, vectorIn=4, vectorOut=4, vectorLocal=1, compute=6, write=4, read=4, mems=16)
  val UnitMax    = SplitCost(scalarIn=1, scalarOut=1, vectorIn=1, vectorOut=1, vectorLocal=1, compute=6, write=4, read=4, mems=1)

  def report(cost: SplitCost) = {
    val SplitCost(scalarIn, scalarOut, vectorIn, vectorOut, vectorLocal, compute, writes, reads, mems) = cost
    debug(s"  compute   = $compute")
    debug(s"  writes    = $writes")
    debug(s"  reads     = $reads")
    debug(s"  memories  = $mems")
    debug(s"  scalarIn  = $scalarIn")
    debug(s"  scalarOut = $scalarOut")
    debug(s"  vectorIn  = $vectorIn")
    debug(s"  vectorOut = $vectorOut")
    debug(s"  vectorLocal = $vectorLocal")
  }

  case class WriteGroup(mems: List[CUMemory], stages: List[Stage])

  // Mutable compute/write partitioning
  class Partition(write: Map[Int, WriteGroup], compute: ArrayBuffer[Stage]) {
    var wstages = Map[Int, WriteGroup]() ++ write
    val cstages = ArrayBuffer[Stage]() ++ compute
    def nonEmpty = wstages.nonEmpty || cstages.nonEmpty

    def writeStages = wstages.values.flatMap(_.stages).toList
    def memories = wstages.values.flatMap(_.mems).toList

    // Remove a compute stage and re-evaluate the write stages needed to support compute
    def popCompute(sramOwners: Set[LocalMem])(implicit ctx: ComputeContext) = {
      val stage = cstages.remove(0)
      val oldStages = wstages
      val (keep, drop) = recomputeWrites(this, sramOwners)
      wstages = keep
      debug(s"  drop stage = $stage")
      (stage, drop)
    }
    def addCompute(drop: (Stage, Map[Int, WriteGroup])) {
      cstages += drop._1
      val keys = wstages.keySet ++ drop._2.keySet
      val joined = keys.map{k => (wstages.get(k), drop._2.get(k)) match {
        case (Some(a),Some(b)) => k -> WriteGroup((a.mems ++ b.mems).distinct, a.stages)
        case (Some(a),None)    => k -> a
        case (None,Some(b))    => k -> b
      }}.toMap
      wstages = joined
    }

    // Remove a write stage (no recompute needed)
    def popWrite()(implicit ctx: SourceContext) = {
      val (longest, size) = wstages.map{case (i,grp) => (i, grp.stages.length) }.reduce{(a,b) => if (a._2 > b._2) a else b}
      val grp = wstages(longest)
      wstages += longest -> WriteGroup(grp.mems, grp.stages.tail)
      longest -> grp.stages.head
    }
    def addWrite(drop: (Int, Stage)) {
      if (wstages.contains(drop._1)) {
        val cur = wstages(drop._1)
        if (!cur.stages.contains(drop._2)) {
          wstages += drop._1 -> WriteGroup(cur.mems, cur.stages ++ List(drop._2))
        }
      }
      else {
        wstages += drop._1 -> WriteGroup(Nil, List(drop._2))
      }
    }
  }
  object Partition {
    def empty = new Partition(Map.empty, ArrayBuffer.empty)
  }


  def inputsOf(stage: Stage): List[LocalMem] = {
    stage.inputMems.filterNot(stage.outputMems contains _ ) // Ignore self-cycles
  }
  def outputsOf(stage: Stage) = stage.outputMems

  // Handle nested memory reads
  def recomputeWrites(partition: Partition, sramOwners: Set[LocalMem])(implicit ctx: ComputeContext) = {

    def writeStages(ins: Set[LocalMem]) = {
      val ownedSRAMs = (ins intersect sramOwners).flatMap{case SRAMRead(mem) => Some(mem); case _ => None}
      //debug(s"Owned SRAMs: $ownedSRAMs")
      partition.wstages.filter{case (i,grp) => grp.mems.exists(ownedSRAMs contains _) }
    }

    var ins: Set[LocalMem] = partition.cstages.flatMap(inputsOf(_)).toSet
    var writes = writeStages(ins)
    var oldSize = -1
    while (ins.size != oldSize) {
      oldSize = ins.size
      ins ++= writes.values.map(_.stages).flatten.flatMap(inputsOf(_))
      writes = writeStages(ins)
    }

    val ownedSRAMs = (ins intersect sramOwners).flatMap{case SRAMRead(mem) => Some(mem); case _ => None}
    val keep = writes.flatMap{case (i, grp) =>
      val keys = grp.mems filter (ownedSRAMs contains _ )
      if (keys.nonEmpty) Some(i -> WriteGroup(keys,grp.stages)) else None
    }
    val drop = writes.flatMap{case (i, grp) =>
      val keys = grp.mems filterNot (ownedSRAMs contains _)
      if (keys.nonEmpty) Some(i -> WriteGroup(keys,grp.stages)) else None
    }
    debug(s"    keep = $keep")
    debug(s"    drop = $drop")
    (keep, drop)
  }

  def breakdown(partition: Partition)(implicit ctx: ComputeContext) = {
    val local  = partition.writeStages ++ partition.cstages
    val remote = ctx.cu.allStages diff local
    val ins: Set[LocalMem]   = local.flatMap(inputsOf(_)).toSet     // All inputs to stages in this partition
    val outs: Set[LocalMem]  = local.flatMap(outputsOf(_)).toSet    // All outputs from stages in this partition
    val used: Set[LocalMem]  = remote.flatMap(inputsOf(_)).toSet    // Values used by stages in other partitions
    val lives: Set[LocalMem] = outs intersect used                  // Live outputs from this partition
    (ins, outs, used, lives)
  }

  // the scala collection implementation of count on Sets sometimes inexplicably fails type checking
  implicit class CountHack[A](x: Set[A]) {
    def countx(f: A => Boolean): Int = {
      var n = 0
      for (a <- x if f(a)) {n += 1}
      n
    }
  }

  def partitionCost(partition: Partition, sramOwners: Set[LocalMem])(implicit ctx: ComputeContext) = {
    val (ins, outs, used, lives) = breakdown(partition)

    val computeCost = partition.cstages.map{
      case stage:MapStage => 1
      case stage@ReduceStage(op,init,in,acc) =>
        val bypassInputCost  = if (outs.contains(in.reg)) 0 else 1     // Needs bypass added at input
        val bypassOutputCost = if (lives.contains(acc)) 1 else 0   // Needs bypass added at output
        REDUCE_STAGES + bypassOutputCost + bypassInputCost
    }.fold(0){_+_}

    val ownedSRAMs = partition.memories
    val writeCost = partition.writeStages.length

    val scalarsIn  = ins.countx(_.isInstanceOf[ScalarIn])
    val scalarsOut = if (ctx.isUnitCompute) lives.size else lives.countx(_.isInstanceOf[ScalarOut])
    val vectorsIn  = ownedSRAMs.size + ins.countx(_.isInstanceOf[VectorIn])
    val vectorsOut = if (ctx.isUnitCompute) 0 else lives.countx{x => !x.isInstanceOf[ScalarOut]}
    val vectorsLocal = if (ctx.isUnitCompute) 0 else outs.countx{x => x.isInstanceOf[VectorLocal]}
    val readCost = ins.countx(sramOwners contains _)
    val mems = ownedSRAMs.map{case mem => 1 }.fold(0){_+_}

    SplitCost(scalarsIn, scalarsOut, vectorsIn, vectorsOut, vectorsLocal, computeCost, writeCost, readCost, mems)
  }

  def requiresSplitting(cost: SplitCost)(implicit ctx: ComputeContext) = {
    if (ctx.isUnitCompute) cost > UnitMax else cost > ComputeMax
  }

  def requiresComputeSplitting(cost: SplitCost)(implicit ctx: ComputeContext) = {
    val max = if (ctx.isUnitCompute) UnitMax else ComputeMax
    requiresSplitting(cost) && (cost.compute > max.compute || cost.read > max.read)
  }


  def splitComputeCU(cu: BasicComputeUnit): List[BasicComputeUnit] = {
    debug(s"Splitting CU: $cu")
    implicit val ctx = ComputeContext(cu)
    // Mark the first read of each SRAM as the "owner" of this SRAM -- the SRAM must be present in the same CU as its owner
    val sramComputeReads: ArrayBuffer[LocalMem] = cu.stages.flatMap{stage => stage.inputMems.filter(_.isInstanceOf[SRAMRead])}
    val sramComputeOwners: Set[LocalMem] = sramComputeReads.groupBy{case SRAMRead(mem) => mem}.map{case (mem,regs) => regs.head}.toSet

    val sramWriteReads: List[ArrayBuffer[LocalMem]] = cu.writeStages.values.map{stages => stages.flatMap{stage => stage.inputMems.filter(_.isInstanceOf[SRAMRead])}}.toList
    val sramWriteOwners: Set[LocalMem] = sramWriteReads.flatMap{reads => reads.groupBy{case SRAMRead(mem) => mem}.map{case (mem,regs) => regs.head}}.toSet

    val sramOwners = sramComputeOwners ++ sramWriteOwners
    debug(s"SRAM owners: $sramOwners")

    val writeGroups = Map[Int,WriteGroup]() ++ cu.writeStages.zipWithIndex.map{case ((mems,stages),i) => i -> WriteGroup(mems,stages.toList) }
    debug(s"SRAM write groups: ")
    writeGroups.foreach{case (i,grp) => debug(s"  Group #$i: $grp") }

    val partitions: ArrayBuffer[Partition] = ArrayBuffer.empty
    var current: Partition = new Partition(writeGroups, cu.stages)
    var remote: Partition = Partition.empty

    // Early termination check
    while (remote.nonEmpty || current.nonEmpty) {
      debug(s"Computing partition ${partitions.length}")
      var cost = partitionCost(current, sramOwners)
      while(requiresComputeSplitting(cost)) {
        remote addCompute current.popCompute(sramOwners) // split off a compute stage and associated write stages, if any
        cost = partitionCost(current, sramOwners)
      }
      while (requiresSplitting(cost)) {
        remote addWrite current.popWrite()   // split off a write stage
        cost = partitionCost(current, sramOwners)
      }
      partitions += current
      current = remote
      remote = Partition.empty
    }
    debug(s"Proposed partitioning: ")
    partitions.zipWithIndex.foreach{case (p,i) =>
      debug(s"Partition #$i: ")
      debug(s"  Write stages: ")
      for ((i,grp) <- p.wstages) {
        debug(s"    Write Group #$i (local = ${grp.mems}):")
        grp.stages.foreach{stage => debug(s"      $stage")}
      }
      debug(s"  Compute stages: ")
      p.cstages.foreach{stage => debug(s"    $stage") }
      report(partitionCost(p, sramOwners))
    }

    //val cus = partitions.zipWithIndex{case (p,i) => schedulePartitioned(ctx, p, i) }
    List(cu)
  }

  /*def connectPartitions(partitions: ArrayBuffer[ArrayBuffer[Stage]])(implicit ctx: ComputeContext) = {
    val cus = partitions.zipWithIndex.map{case (p,i) =>
      val cu = BasicComputeUnit(ctx.cu.name+"_"+i, ctx.cu.pipe, ctx.cu.parent, ctx.cu.tpe)
      cu.isUnitCompute = ctx.cu.isUnitCompute
    }
  }*/

  /*def schedulePartitioned(orig: ComputeContext, stages: ArrayBuffer[Stage], i: Int): BasicComputeUnit = {

    val part = ComputeContext(cu)

    val (ins, outs, used, lives) = breakdown(stages)(orig)

    val writeStages = writeStages(ins, sramOwners)(orig)

    debug(s"  Compute:")
    stages.foreach{stage => debug(s"    $stage")}

    cu.writeStages ++= writeStages
    cu.regs ++= orig.cu.regs intersect (ins union outs)
    if (i == 0) cu.cchains ++= orig.cu.cchains

  }*/

 /*{
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

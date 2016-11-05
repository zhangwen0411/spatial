package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet,Queue,ArrayBuffer}
import scala.virtualization.lms.util.GraphUtil

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import ppl.delite.framework.Config

trait PIRRetiming extends RetimingOps {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_until => _, _}

  override val name = "PIR Retiming"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug


  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal CU Retiming")
    for ((pipe,cuGrp) <- cus) {
      retime(cuGrp)
    }
    (b)
  }
}


trait RetimingOps extends PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_until => _, _}

  val cus = HashMap[Exp[Any], List[ComputeUnit]]()
  def allocateCU(pipe: Exp[Any]) = cus(pipe).head

  def allCUs = cus.values.flatten
  /**
   * For all vector inputs for each CU, if inputs have mismatched delays or come
   * from other stages, put them through a retiming FIFO
   **/
  def retime(cus: List[ComputeUnit]) = if (cus.length > 1) {
    // Global inputs and outputs potentially needing retiming
    var producer = Map[GlobalMem, ComputeUnit]()
    var deps     = Map[ComputeUnit, List[GlobalMem]]()

    val compute = cus.filter{cu => cu.allStages.nonEmpty && !cu.isInstanceOf[TileTransferUnit]}

    // Create mapping for producers of all global memories
    compute.foreach{cu => cu.allStages.foreach{stage =>
      stage.outputMems.foreach{
        case VectorOut(mem) => producer += mem -> cu
        case ScalarOut(mem) => producer += mem -> cu
        case _ =>
      }
    }}
    // Create mapping for dependencies of all CUs
    compute.foreach{cu =>
      val inputs = cu.allStages.flatMap(_.inputMems).collect{
        case VectorIn(mem) => mem
        case ScalarIn(mem) => mem
      }
      deps += cu -> inputs
    }

    def getDelay(input: GlobalMem, cur: Int): Int = producer.get(input) match {
      case Some(cu) => (1 +: deps(cu).map{dep => getDelay(dep,cur+1)}).max
      case None => cur
    }

    compute.foreach{cu =>
      val delays = 1 +: deps(cu).map{dep => getDelay(dep, 0) }
      val criticalPath = delays.max

      deps(cu).zip(delays).foreach{case (dep,dly) =>
        if (dly < criticalPath && dly != 0) {
          insertFIFO(cu, dep, (criticalPath - dly)*10)
        }
        else if (dly == 0) {
          val produce = allCUs.find(cu => (scalarOuts(cu) ++ vectorOuts(cu)) contains dep)
          if (produce.isDefined) {
            lca(cu, produce.get) match {
              case Some(parent:BasicComputeUnit) if parent.tpe == StreamPipe => // Do nothing
              case None => // Do nothing
              case _ => insertFIFO(cu, dep, 1000) // TODO: Calculate this
            }
          }
        }
      }

      cu.deps ++= deps(cu).flatMap{dep => producer.get(dep) }
    }
  }

  def lca(a: ComputeUnit, b: ComputeUnit) = {
    GraphUtil.leastCommonAncestor[ComputeUnit](a, b, {node => node.parent })
  }

  def insertFIFO(cu: ComputeUnit, global: GlobalMem, depth: Int) {
    debug(s"Inserting FIFO in $cu for input $global")
    val sram = allocateFIFO(global, depth)
    cu.srams += sram
    cu.allStages.foreach{
      case stage@MapStage(op, ins, outs) =>
        stage.ins = ins.map{
          case LocalRef(_,ScalarIn(`global`)) => LocalRef(-1, SRAMRead(sram))
          case LocalRef(_,VectorIn(`global`)) => LocalRef(-1, SRAMRead(sram))
          case ref => ref
        }
      case _ =>
    }
  }

  // TODO: Is it ok to create a FIFO with scalar input?
  def allocateFIFO(global: GlobalMem, depth: Int) = {
    val name = global match {
      case g:VectorMem => g.name + "_fifo"
      case g:ScalarMem => g.name + "_fifo"
      case _ => throw new Exception(s"Cannot create FIFO with input $global")
    }
    val sram = CUMemory(name, depth)
    sram.isFIFO = true
    sram.vector = Some(global)
    sram.banking = Some(Strided(1))
    sram
  }
}
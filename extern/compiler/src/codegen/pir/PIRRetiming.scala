package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable
import scala.virtualization.lms.util.GraphUtil

trait PIRRetiming extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR.{infix_until => _, _}

  var STAGES: Int = 10

  /**
   * For all vector inputs for each CU, if inputs have mismatched delays or come
   * from other stages (and LCA is not a stream controller), put them through a retiming FIFO
   **/
  def retime(cus: List[CU], others: Iterable[CU]): Unit = {
    var producer = Map[GlobalBus, CU]()
    var deps     = Map[CU, List[GlobalBus]]()

    val compute = cus.filter(_.allStages.nonEmpty)

    compute.foreach{cu =>
      globalOutputs(cu).foreach{bus => producer += bus -> cu}

      // Ignore inputs for cchains, srams here - SRAM vectors are already retimed, others are scalars which are retimed differently
      val ins = globalInputs(cu.allStages)
      deps += cu -> ins.toList
    }

    //       |
    //       A
    //       |
    //       B <-- F
    //      / \    |
    //     C   D - E

    /*def getDelay(input: GlobalBus, cur: Int, visit: Set[CU]): Int = producer.get(input) match {
      case Some(cu) if visit.contains(cu) =>
        debug(s"    [CYCLE]")
        -1  // Don't retime cycles
      case Some(cu) if deps(cu).isEmpty =>
        debug(s"    ${cu.name}")
        cur+1
      case Some(cu) =>
        debug(s"    ${cu.name} -> " + deps(cu).mkString(", "))
        val delays = deps(cu).map{dep => getDelay(dep,cur+1,visit+cu)}
        if (delays.contains(-1)) -1 else delays.max
      case None => cur
    }*/

    compute.foreach{cu => if (deps(cu).nonEmpty) {
      debug(s"Retiming inputs to CU ${cu.name}: ")

      val vecIns = deps(cu).iterator.collect{case bus: VectorBus => bus}

      vecIns.foreach{dep =>
        if (producer.contains(dep) || isInterCU(dep)) { // Inputs from DRAM should already have FIFOs, input args don't need them
          // Size of FIFO can't be statically predicted here (routing costs) and doesn't matter to config anyway
          insertFIFO(cu, dep, 4096)
        }
        /*else if (isInterCU(dep)) {
          insertFIFO(cu, dep, 4096)
        }
          val produce = others.find{cu => globalOutputs(cu) contains dep }
          if (produce.isDefined) {
            lca(cu, produce.get) match {
              case Some(parent) if parent.style == StreamCU => // No retiming within stream controllers?
              case None => // What?
              case _ => insertFIFO(cu, dep, 4096)
            }
          }
        }*/
      }

      cu.deps ++= deps(cu).flatMap{dep => producer.get(dep) } // Only use dependencies within this virtual CU
    }}






    /*    val produce = producer.getOrElse(dep, )
        if (produce.isDefined) {
          lca(cu, produce.get) match {
            case Some(parent) if parent.style == StreamCU => // No action
            case None => // ???
            case _ => insertFIFO(cu, dep, 4096)
          }
          insertFIFO(cu, dep, 4096 ) // size isn't used anyway

      }

      val delays = deps(cu).map{dep =>
        debug(s"  $dep")
        getDelay(dep, 0, Set(cu))
      }
      val criticalPath = delays.max



      deps(cu).zip(delays).foreach{case (dep,dly) =>
        if (dly <= criticalPath && dly > 0) {
          insertFIFO(cu, dep, (criticalPath - dly + 1)*STAGES)
        }
        else if (dly == 0) {
          val produce = others.find{cu => globalOutputs(cu) contains dep}
          if (produce.isDefined) {

          }
        }
      }*/


    //}}
  }

  def lca(a: CU, b: CU) = {
    GraphUtil.leastCommonAncestor[CU](a, b, {node => node.parentCU })
  }

  def insertFIFO(cu: CU, bus: GlobalBus, depth: Int) {
    debug(s"Inserting FIFO in $cu for input $bus")
    val sram = allocateFIFO(bus, depth)
    cu.srams += sram
    cu.allStages.foreach{
      case stage@MapStage(op, ins, outs) =>
        stage.ins = ins.map{
          case LocalRef(_,ScalarIn(`bus`)) => LocalRef(-1, SRAMReadReg(sram))
          case LocalRef(_,VectorIn(`bus`)) => LocalRef(-1, SRAMReadReg(sram))
          case ref => ref
        }
      case _ =>
    }
  }

  def allocateFIFO(bus: GlobalBus, depth: Int) = {
    val name = bus match {
      case bus:ScalarBus => bus.name+"_fifo"
      case bus:VectorBus => bus.name+"_fifo"
    }
    val sram = CUMemory(name, depth, fresh[Any], fresh[Any])
    sram.mode = FIFOMode
    sram.vector = Some(bus)
    sram.banking = Some(Strided(1))
    sram
  }

}
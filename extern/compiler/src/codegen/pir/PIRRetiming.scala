package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable
import scala.virtualization.lms.util.GraphUtil

trait PIRRetiming extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR.{infix_until => _, _}

  /**
   * For all vector inputs for each CU, if inputs have mismatched delays or come
   * from other stages (and LCA is not a stream controller), put them through a retiming FIFO
   **/
  def retime(cus: List[CU], others: Iterable[CU]): Unit = if (cus.length > 1) {
    var producer = Map[GlobalBus, CU]()
    var deps     = Map[CU, List[GlobalBus]]()

    val compute = cus.filter(_.allStages.nonEmpty)

    compute.foreach{cu =>
      globalOutputs(cu).foreach{bus => producer += bus -> cu}
      deps += cu -> globalInputs(cu).toList
    }

    def getDelay(input: GlobalBus, cur: Int): Int = producer.get(input) match {
      case Some(cu) if deps(cu).isEmpty => 1
      case Some(cu) =>  (1 +: deps(cu).map{dep => getDelay(dep,cur+1)}).max // max or 1 if empty
      case None => cur
    }

    compute.foreach{cu => if (deps(cu).nonEmpty) {
      val delays = deps(cu).map{dep => getDelay(dep, 0) }
      val criticalPath = delays.max

      deps(cu).zip(delays).foreach{case (dep,dly) =>
        if (dly < criticalPath && dly != 0) {
          insertFIFO(cu, dep, (criticalPath - dly)*10)
        }
        else if (dly == 0) {
          val produce = others.find{cu => globalOutputs(cu) contains dep}
          if (produce.isDefined) {
            lca(cu, produce.get) match {
              case Some(parent) if parent.style == StreamCU => // No action
              case None => // ???
              case _ => insertFIFO(cu, dep, 1000) // TODO: Calculate this!
            }
          }
        }
      }

      cu.deps ++= deps(cu).flatMap{dep => producer.get(dep) }
    }}
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
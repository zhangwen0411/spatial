package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

trait PIRHacks extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  override val name = "PIR Hacks"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mappingIn = mutable.HashMap[Symbol, List[CU]]()

  val mappingOut = mutable.HashMap[Symbol, List[CU]]()

  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal PIR Hacks")
    for ((pipe, cus) <- mappingIn) {
      mappingOut += pipe -> mcHack(pipe, cus)
    }
    streamHack()
    counterHack()

    b
  }

  def mcHack(pipe: Symbol, cus: List[CU]): List[CU] = {
    def allCUs = mappingIn.values.flatten

    // Set all CUs which write to a memory controller to StreamCUs
    // Either set parent to a streamcontroller, or make one and redirect parent
    cus.flatMap{cu =>
      val writesMC = globalOutputs(cu) exists (_.isInstanceOf[DRAMBus])

      debug(s"${cu.name}: $writesMC")

      // Set everything but first stages to streaming pipes
      if (writesMC && cu.deps.nonEmpty) cu.style = StreamCU

      val add = if (writesMC) cu.parent match {
        case Some(parent: CU) if parent.style != StreamCU =>
          val cusWithParent = allCUs.filter(_.parent == cu.parent).toSet
          val cusByMC = cusWithParent.groupBy(writtenMC)

          // All CUs with this parent communicate with the same memory controller(s) as this CU
          if (cusByMC.keys.size == 1) {
            parent.style = StreamCU
            Nil
          }
          else {
            val parent = makeStreamController(pipe, cu.parent)
            cu.parent = Some(parent)
            List(parent)
          }
        case None =>
          val parent = makeStreamController(pipe, None)
          cu.parent = Some(parent)
          List(parent)
        case _ =>
          Nil
      }
      else Nil

      List(cu) ++ add
    }
  }

  // Ensure that stream controllers have exactly one leaf
  def streamHack() {
    val cus = mappingOut.values.flatten.toList
    for (cu <- cus) {
      if (cu.allStages.isEmpty && cu.style == StreamCU && !cu.isDummy) {
        val children = cus.filter(_.parent == Some(cu))
        val writesMC = children.exists{child => globalOutputs(child) exists (_.isInstanceOf[DRAMBus]) }

        val deps = children.flatMap(_.deps).toSet

        // Leaves - no CU is dependent on this child
        val leaves = children.filterNot(deps contains _)

        if (leaves.size > 1 && !writesMC) {
          val leaf = ComputeUnit(quote(cu.pipe)+"_leaf", cu.pipe, StreamCU)
          copyIterators(leaf, cu)
          leaf.parent = Some(cu)
          leaf.deps ++= leaves
          leaf.isDummy = true
          mappingOut(cu.pipe) = mappingOut(cu.pipe) ++ List(leaf)
        }
      }
    }
  }

  // Change strides of last counter in inner, parallelized loops to LANES
  def counterHack() {
    val cus = mappingOut.values.flatten.toList
    for (cu <- cus) {
      if (!cu.isUnit && (cu.allStages.nonEmpty || cu.isDummy)) {
        cu.cchains.foreach{
          case CChainInstance(name, ctrs) =>
            val innerCtr = ctrs.last
            if (innerCtr.end != ConstReg("1i")) {
              assert(innerCtr.stride == ConstReg("1i"))
              innerCtr.stride = ConstReg(s"${LANES}i")
            }

          case _ => // Do nothing
        }
      }
    }
  }

  def writtenMC(cu: CU): Set[MemoryController] = globalOutputs(cu).collect{
    case DRAMDataOut(mc) => mc
    case DRAMAddress(mc) => mc
    case DRAMLength(mc) => mc
    case DRAMOffset(mc) => mc
  }


  def makeStreamController(pipe: Symbol, parent: Option[ACU]): CU = {
    val cu = ComputeUnit(quote(pipe)+"_sc", pipe, StreamCU)
    cu.parent = parent
    cu.cchains += UnitCChain(quote(pipe)+"_unitcc")
    cu
  }
}

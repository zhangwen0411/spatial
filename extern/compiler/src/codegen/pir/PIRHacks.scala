package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

trait PIRHacks extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR.{assert => _, _}

  override val name = "PIR Hacks"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mappingIn = mutable.HashMap[Symbol, List[CU]]()

  val mappingOut = mutable.HashMap[Symbol, List[CU]]()

  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal PIR Hacks")
    for ((pipe, cus) <- mappingIn) {
      mcHack(pipe, cus)
      mappingOut += pipe -> cus
    }
    streamHack()
    counterHack()

    b
  }

  def mcHack(pipe: Symbol, cus: List[CU]) {
    def allCUs = mappingIn.values.flatten

    debug(s"MC Hack")

    // Set all CUs which write to a memory controller to StreamCUs
    // Either set parent to a streamcontroller, or make one and redirect parent
    cus.foreach{cu =>
      val writesMC = globalOutputs(cu) exists (_.isInstanceOf[DRAMBus])

      debug(s"${cu.name}: $writesMC")

      // Set everything but first stages to streaming pipes
      if (writesMC && cu.deps.nonEmpty) cu.style = StreamCU


      if (writesMC) cu.parent match {
        case Some(parent: CU) if parent.style != StreamCU =>
          val cusWithParent = allCUs.filter(_.parent == cu.parent).toSet
          val cusByMC = cusWithParent.groupBy(writtenMC)

          debug(s"  cu: $cu")
          debug(s"  parent: $parent")
          debug(s"  w/ parent: $cusWithParent")
          debug(s"  cus by MC: $cusByMC")

          // All CUs with this parent communicate with the same memory controller(s) as this CU
          if (cusByMC.keys.size == 1) {
            parent.style = StreamCU
          }
          /*else {
            val parent = makeStreamController(pipe, cu.parent)
            cu.parent = Some(parent)
            List(parent)
          }*/
        /*case None =>
          val parent = makeStreamController(pipe, None)
          cu.parent = Some(parent)
          List(parent)*/
        case _ =>
      }
    }
  }

  def writesToMC(cu: CU, cus: List[CU]): Boolean = {
    val children = cus.filter(_.parent == Some(cu))

    (cu +: children).exists{child => globalOutputs(child) exists (_.isInstanceOf[DRAMBus]) }
  }

  // Ensure that outer controllers have exactly one leaf
  def streamHack() {
    val cus = mappingOut.values.flatten.toList
    for (cu <- cus) {
      if (cu.allStages.isEmpty && !cu.isDummy) {
        val children = cus.filter(_.parent == Some(cu))
        val writesMC = writesToMC(cu, cus)

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
        else {
          // If we have a child controller leaf which itself has leaves which write to DRAM data bus
          val leafWritesMC = leaves.exists{leaf =>
            val leafChildren = cus.filter(_.parent == Some(leaf))
            leafChildren.exists{child => globalOutputs(child) exists(_.isInstanceOf[DRAMDataOut]) }
          }
          if (leafWritesMC) {
            // insert a dummy pipe after the writing leaf
            val newLeaf = ComputeUnit(quote(cu.pipe)+"_leafX", cu.pipe, PipeCU)
            copyIterators(newLeaf, cu)
            newLeaf.parent = Some(cu)
            newLeaf.deps ++= leaves
            newLeaf.isDummy = true
            mappingOut(leaves.last.pipe) = mappingOut(leaves.last.pipe) ++ List(newLeaf)
          }
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
      else if (cu.allStages.isEmpty && !cu.isDummy) {
        // Eliminate cchain copies in outer loops
        cu.cchains = cu.cchains.filter{
          case _:CChainInstance | _:UnitCChain => true
          case _ => false
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

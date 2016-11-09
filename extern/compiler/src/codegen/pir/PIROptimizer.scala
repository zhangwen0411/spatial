package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

trait PIROptimizer extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  override val name = "PIR Optimization"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mapping = mutable.HashMap[Symbol, CU]()

  def cus = mapping.values

  override def run[A:Manifest](b: Block[A]): Block[A] = {
    msg("Starting traversal PIR Optimizer")
    for (cu <- cus) removeRouteThrus(cu)
    for (cu <- cus) removeUnusedCUComponents(cu)
    for (cu <- cus) removeDeadStages(cu)
    removeEmptyCUs(mapping.values.toList)
    removeUnusedGlobalBuses()
    for (cu <- cus) removeDeadStages(cu)
    removeEmptyCUs(mapping.values.toList)

    b
  }


  def removeUnusedCUComponents(cu: CU) {
    val stages = cu.allStages.collect{case m:MapStage => m}
    // Remove all unused temporary registers
    val ins  = stages.flatMap{stage => stage.inputMems.filter{t => isReadable(t) && isWritable(t) }}.toSet
    val outs = stages.flatMap{stage => stage.outputMems.filter{t => isReadable(t) && isWritable(t) }}.toSet
    val unusedRegs = outs diff ins

    debug(s"Removing unused registers from $cu: " + unusedRegs.mkString(", "))
    stages.foreach{stage => stage.outs = stage.outs.filterNot{ref => unusedRegs contains ref.reg}}
    cu.regs --= unusedRegs

    // Remove unused counterchain copies
    val usedCCs = usedCChains(cu)
    val unusedCopies = cu.cchains.collect{case cc:CChainCopy if !usedCCs.contains(cc) => cc}

    debug(s"Removing unused counterchain copies from $cu: " + unusedCopies.mkString(", "))
    cu.cchains --= unusedCopies
  }


  def removeUnusedGlobalBuses() {
    val buses = globals.collect{case bus:GlobalBus if isInterCU(bus) => bus}
    val inputs = cus.flatMap{cu => globalInputs(cu) }.toSet

    debug(s"Buses: ")
    buses.foreach{bus => debug(s"  $bus")}

    debug(s"Used buses: ")
    inputs.foreach{in => debug(s"  $in")}


    val unusedBuses = buses filterNot(inputs contains _)

    def isUnusedReg(reg: LocalComponent) = reg match {
      case ScalarOut(out) => unusedBuses contains out
      case VectorOut(out) => unusedBuses contains out
      case _ => false
    }
    def isUnusedRef(ref: LocalRef) = isUnusedReg(ref.reg)

    debug(s"Removing unused global buses:\n  " + unusedBuses.mkString("\n  "))
    cus.foreach{cu =>
      val stages = cu.allStages.collect{case m:MapStage => m}
      stages.foreach{stage => stage.outs = stage.outs.filterNot(isUnusedRef) }
      cu.regs = cu.regs.filterNot(isUnusedReg)
    }
    globals --= unusedBuses
  }

  // Remove route-through stages from the IR after scheduling
  // Rationale (for post scheduling): The Spatial IR has a number of nodes which are
  // effectively no-ops in PIR, which makes detecting route through cases difficult.
  // Once scheduled, a typical route-through case just looks like a CU with a single stage
  // which takes a vecIn and bypasses to a vecOut, which is easier to pattern match on
  def removeRouteThrus(cu: CU) = if (cu.parent.isDefined) {
    val bypassStages = cu.computeStages.flatMap{
      case bypass@MapStage(Bypass, List(LocalRef(_,VectorIn(in: DRAMDataIn))), List(LocalRef(_,VectorOut(out: VectorBus)))) =>
        swapBus(cus, out, in)
        Some(bypass)
      case bypass@MapStage(Bypass, List(LocalRef(_,VectorIn(in: VectorBus))), List(LocalRef(_,VectorOut(out: VectorBus)))) =>
        cus.find{cu => vectorOutputs(cu) contains in} match {
          case Some(producer) if producer.parent == cu.parent =>
            swapBus(cus, out, in)
            Some(bypass)
          case _ => None
        }
      case bypass@MapStage(Bypass, List(LocalRef(_,ScalarIn(in: ScalarBus))), List(LocalRef(_,ScalarOut(out: ScalarBus)))) =>
        cus.find{cu => scalarOutputs(cu) contains in} match {
          case Some(producer) if producer.parent == cu.parent =>
            swapBus(cus, out, in)
            Some(bypass)
          case _ => None
        }
      case _ => None
    }
    if (bypassStages.nonEmpty) {
      debug(s"Removing route through stages from $cu: ")
      bypassStages.foreach{stage => debug(s"  $stage")}
      removeComputeStages(cu, bypassStages.toSet)
    }
  }

  // TODO: This could be iterative with removing unused outputs
  // Right now only does one layer
  def removeDeadStages(cu: CU) {
    val deadStages = cu.computeStages.collect{case stage:MapStage if stage.outs.isEmpty => stage}
    if (deadStages.nonEmpty) {
      debug(s"Removing dead stages from $cu: ")
      deadStages.foreach{stage => debug(s"  $stage") }
      removeComputeStages(cu, deadStages.toSet)
    }
  }


  def removeEmptyCUs(cus: List[CU]) = cus.foreach {cu =>
    // 1. This CU has no children, no write stages, and no compute stages
    // 2. This CU has a sibling (same parent) CU or no counterchain instances
    val children = cus.filter{c => c.parent.contains(cu) }
    if (cu.writeStages.isEmpty && cu.computeStages.isEmpty && children.isEmpty) {
      val sibling = cus.find{c => c != cu && c.parent == cu.parent}

      val globallyUsedCCs = cus.filterNot(_ != cu).flatMap(usedCChains(_))

      val usedCCs = cu.cchains.filter{
        case _:CChainCopy => false
        case cc:CChainInstance => globallyUsedCCs.exists(_.name == cc)
        case cc:UnitCChain => globallyUsedCCs.exists(_.name == cc)
      }
      if (sibling.isDefined && usedCCs.nonEmpty) {
        val sib = sibling.get
        sib.cchains ++= usedCCs
        // Change owners of cchains, bypass dependencies
        cus.foreach{c =>
          c.cchains.foreach{
            case cchain@CChainCopy(name, inst, `cu`) => cchain.owner = sib
            case _ => // No action
          }
          if (c.deps.contains(cu)) {
            c.deps -= cu
            c.deps ++= cu.deps
          }
        }
      }
      if (usedCCs.isEmpty || sibling.isDefined) {
        mapping.retain{case (pipe,c) => c != cu }
      }
    }
  }

}
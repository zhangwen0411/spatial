package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{ArrayBuffer,HashMap}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait PIROptimizer extends Traversal with PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Optimization"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  def allocateCU(pipe: Exp[Any]): ComputeUnit = cuMapping(pipe)

  lazy val cus = cuMapping.values.toSet

  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal PIR Optimization")
    for (cu <- cus) removeRouteThruStages(cu)
    for (cu <- cus) removeDeadStages(cu)
    for (cu <- cus) removeUnusedCUComponents(cu)
    for (cu <- cus) removeEmptyCUs(cu)
    removeUnusedGlobals()
    for (cu <- cus) removeDeadStages(cu)
    for (cu <- cus) removeEmptyCUs(cu)

    b
  }

  def removeUnusedCUComponents(cu: ComputeUnit) {
    val stages = allMapStages(cu)
    // Remove all temporary registers from outputs when they are not used in any input
    val tempIns = stages.flatMap{stage => stage.inputMems.filter(_.isInstanceOf[TempReg]) }
    val tempOuts = stages.flatMap{stage => stage.outputMems.filter(_.isInstanceOf[TempReg]) }
    val unusedTemps = tempOuts.filterNot(tempIns contains _)
    stages.foreach{stage => stage.outs = stage.outs.filterNot{ref => unusedTemps contains ref.reg} }
    // Also remove from set of registers
    cu.regs --= unusedTemps

    // Remove unused counterchain copies
    val usedCCs = usedCounterChains(cu)
    val unusedCopies = cu.cchains.filter{cc=> cc.isInstanceOf[CounterChainCopy] && !usedCCs.contains(cc) }
    cu.cchains --= unusedCopies
  }

  def removeUnusedGlobals() {
    val outs = globals.filter{case _:VectorMem | _:ScalarMem => true; case _ => false}
    val ins = cus.flatMap{cu => scalarIns(cu) ++ vectorIns(cu) }

    val unusedOuts = outs filterNot (out => ins.contains(out))

    def isUnusedReg(reg: LocalMem) = reg match {
      case ScalarOut(out) => unusedOuts contains out
      case VectorOut(out) => unusedOuts contains out
      case _ => false
    }
    def isUnusedRef(ref: LocalRef) = isUnusedReg(ref.reg)

    debug(s"Removing unused globals: " + unusedOuts.mkString(", "))

    for (cu <- cus){
      val stages = allMapStages(cu)
      stages.foreach{stage => stage.outs = stage.outs.filterNot(isUnusedRef(_)) }
      cu.regs = cu.regs.filterNot(isUnusedReg(_))
    }

    globals --= unusedOuts
  }

  // Remove route-through CUs from the IR after scheduling
  // Rationale (for post scheduling): The Spatial IR has a number of nodes which are
  // effectively no-ops in PIR, which makes detecting route through cases difficult.
  // Once scheduled, a typical route-through case just looks like a CU with a single stage
  // which takes a vecIn and bypasses to a vecOut, which is easier to pattern match on
  def removeRouteThruStages(cu: ComputeUnit) = cu match {
    case cu: BasicComputeUnit if cu.tpe == InnerPipe && cu.parent.isDefined =>
      // 1. The compute stage is a map stage which bypasses a vector in to a vector out
      // 2. The vector input's producer's parent is the same as this CU's parent
      val bypassStages = cu.stages.flatMap{
        case bypass@MapStage(Bypass, List(LocalRef(_,VectorIn(in))), List(LocalRef(_,VectorOut(out)))) =>
          cus.find{cu => vectorOuts(cu) contains in} match {
            case Some(producer) if producer.parent == cu.parent =>
              debug(s"Removing route-thru stage $bypass from cu $cu")
              swapGlobal(out, in) // Reroute all consumers of this CU to the producer
              Some(bypass)

            case _ => None
          }
        case bypass@MapStage(Bypass, List(LocalRef(_,ScalarIn(in) )), List(LocalRef(_,outReg@ScalarOut(out)))) =>
          cus.find{cu => scalarOuts(cu) contains in} match {
            case Some(producer) if producer.parent == cu.parent => // TODO: Is common parent necessary here?
              val ctx = ComputeContext(producer)
              val origOut = producer.regs.find{case ScalarOut(`in`) => true; case _ => false}.get
              ctx.addOutput(origOut, outReg)
              debug(s"Adding register $outReg to $producer")
              debug(s"Removing route-thru stage $bypass from cu $cu")
              Some(bypass)
            case  _ => None
          }
        case _ => None
      }
      if (bypassStages.nonEmpty) {
        // Remove bypass stages from CU
        removeStages(cu, bypassStages.toSet)
      }
    case _ =>
  }

  def removeDeadStages(cu: ComputeUnit) = {
    val deadStages = cu.stages.flatMap{case stage: MapStage if stage.outs.isEmpty => Some(stage); case _ => None }
    if (deadStages.nonEmpty) {
      debug(s"Stages in $cu: ")
      cu.stages.foreach{stage => debug(s"  $stage")}
      debug("")
      debug(s"Removing dead stages from $cu:")
      deadStages.foreach{stage => debug(s"  $stage")}
      debug("")
      removeStages(cu, deadStages.toSet)
    }
  }

  def removeEmptyCUs(cu: ComputeUnit) = cu match {
    case cu: BasicComputeUnit if cu.tpe == InnerPipe =>
      // 1. This CU has no children, no write stages, and no compute stages
      // 2. This CU has a "sibling" (same parent) CU, or has no counter chain instances
      val children = cus.filter{c => c.parent.contains(cu)}
      if (cu.writeStages.isEmpty && cu.stages.isEmpty && children.isEmpty) {
        val siblingCU = cus.find{c => c != cu && c.parent == cu.parent && c.isInstanceOf[TileTransferUnit]}

        val globallyUsedCCs = cus.filterNot(_ != cu).flatMap(usedCounterChains(_))

        val usedCCs = cu.cchains.filter{
          case _:CounterChainCopy => false
          case _:CounterChainInstance => true
          case cc:UnitCounterChain => globallyUsedCCs.exists(_.name == cc.name)
        }
        if (siblingCU.isDefined && usedCCs.nonEmpty) {
          val sibling = siblingCU.get
          debug(s"Adding counterchains to $sibling: $usedCCs")

          sibling.cchains ++= usedCCs
          usedCCs.foreach{cc => cc.isStreaming = true }
          cus.foreach{c =>
            // Swap references to this CU's counter chain to the sibling
            c.cchains = c.cchains.map{
              case CounterChainCopy(name, `cu`) => CounterChainCopy(name, sibling)
              case cc => cc
            }
            // Swap dependencies on this CU for dependencies on this CU's dependencies
            if (c.deps.contains(cu)) {
              c.deps -= cu
              c.deps ++= cu.deps
            }
          }
        }
        if (usedCCs.isEmpty || siblingCU.isDefined) {
          debug(s"Removing empty cu $cu")
          cuMapping.retain{case (pipe,c) => c != cu}
        }
      }
    case _ =>
  }

  // --- Utility functions for optimization

  def usedCounterChains(cu: ComputeUnit) = {
    val stages = allMapStages(cu)
    stages.flatMap{stage => stage.inputMems.flatMap{case CounterReg(cchain,_) => Some(cchain); case _ => None}} ++
    cu.srams.flatMap{sram => sram.readAddr match {case Some(CounterReg(cchain,_)) => Some(cchain); case _ => None}} ++
    cu.srams.flatMap{sram => sram.writeAddr match {case Some(CounterReg(cchain,_)) => Some(cchain); case _ => None}} ++
    cu.srams.flatMap{sram => sram.swapRead match {case Some(cchain) => Some(cchain); case _ => None}} ++
    cu.srams.flatMap{sram => sram.swapWrite match {case Some(cchain) => Some(cchain); case _ => None}} ++
    cu.srams.flatMap{sram => sram.writeCtrl match {case Some(cchain) => Some(cchain); case _ => None}}
  }

  // --- Stage removal
  def removeStages(cu: ComputeUnit, remove: Set[Stage]): Unit = {
    val ctx = ComputeContext(cu)
    val stages = ctx.stages
    cu.stages = ArrayBuffer.empty

    debug(s"  Stages are now: ")
    stages.foreach{
      case stage@MapStage(op,ins,outs) if !remove.contains(stage) =>
        stage.ins = ins.map{case LocalRef(i,reg) => ctx.refIn(reg) }
        stage.outs = outs.map{case LocalRef(i,reg) => ctx.refOut(reg) }
        debug(s"    $stage")
        ctx.addStage(stage)

      case stage@ReduceStage(op,init,in,acc) =>
        debug(s"    $stage")
        ctx.addStage(stage)

      case _ =>
    }
  }


  // --- Swapping helper functions

  def swapGlobal(orig: GlobalMem, swap: GlobalMem) = cus.foreach { cu =>
    def swapGlobal_Stage(stage: Stage) = stage match {
      case stage@MapStage(_,ins,outs) =>
        stage.ins = ins.map{ref => swapGlobal_Ref(ref) }
        stage.outs = outs.map{case LocalRef(i,reg) => LocalRef(i, swapGlobal_Reg(reg)) }
      case _ =>
    }
    def swapGlobal_Ref(ref: LocalRef) = ref match {
      case LocalRef(i,reg) => LocalRef(i, swapGlobal_Reg(reg))
    }
    def swapGlobal_Reg(reg: LocalMem) = reg match {
      case VectorIn(`orig`) => VectorIn(swap.asInstanceOf[VectorMem])
      case VectorOut(`orig`) => VectorOut(swap)
      case ScalarIn(`orig`) => ScalarIn(swap)
      case ScalarOut(`orig`) => ScalarOut(swap)
      case _ => reg
    }
    def swapGlobal_SRAM(sram: CUMemory) {
      sram.vector = sram.vector match { case Some(`orig`) => Some(swap); case vec => vec}
    }


    allMapStages(cu).foreach{stage => swapGlobal_Stage(stage)}
    cu.srams.foreach{sram => swapGlobal_SRAM(sram)}

    cu match {
      case tu@TileTransferUnit(name,pipe,parent,ctrl,`orig`,mode) => tu.vec = swap
      case _ =>
    }
  }

}

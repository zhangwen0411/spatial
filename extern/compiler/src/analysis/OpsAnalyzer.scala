package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait OpsAnalysisExp extends LatencyAnalysisExp with OpsModel {
  this: SpatialExp =>
}

trait OpsAnalyzer extends ModelingTraversal {
  val IR: OpsAnalysisExp with SpatialExp
  import IR._

  override val name = "Ops Analyzer"
  debugMode = SpatialConfig.debugging || SpatialConfig.loudModels
  verboseMode = SpatialConfig.verbose

  var totalOps = AppStatistics()
  var opScope: List[AppStatistics] = Nil

  def opsIn(e: Exp[Any]) = IR.opsIn(e, inHwScope)

  def opsInBlock(b: Block[Any]) = {
    val outerScope = opScope
    opScope = Nil
    traverseBlock(b)
    val ops = opScope.fold(NoOps){_+_}
    opScope = outerScope
    ops
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    val ops = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        val body = opsInBlock(blk)
        inHwScope = false
        body

      case EatReflect(ParallelPipe(func)) =>
        opsInBlock(func)

      case EatReflect(UnitPipe(func)) =>
        val body = opsInBlock(func)
        debug(s"Pipe $lhs: ")
        debug(s"  body: $body")
        body

      case EatReflect(OpForeach(cchain, func, _)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func)

        debug(s"Foreach $lhs (N = $N, P = $P):")
        debug(s"  body: $body")
        body * P * N

      case EatReflect(e@OpReduce(cchain,accum,zero,fA,ld,st,func,rFunc,_,_,_,_)) =>
        val P = parsOf(cchain).reduce(_*_)
        val N = nIters(cchain)
        val body = opsInBlock(func) * N * P
        val reduce = opsInBlock(rFunc) * N * P
        val load = opsInBlock(ld) * N * P
        val store = opsInBlock(st) * N * P

        debug(s"Fold $lhs (N = $N, P = $P)")
        debug(s"  body: $body")
        debug(s"  reduce: $reduce")
        debug(s"  load: $load")
        debug(s"  store: $store")
        body + reduce + load + store

      case EatReflect(e@OpMemReduce(ccOuter,ccInner,accum,zero,fA,func,ld1,ld2,rFunc,st,_,_,_,_,_,_)) =>
        val Pm = parsOf(ccOuter).reduce(_*_)
        val Pr = parsOf(ccInner).reduce(_*_)
        val Nm = nIters(ccOuter)
        val Nr = nIters(ccInner)

        val body = opsInBlock(func) * Nm * Pm
        val reduce = opsInBlock(rFunc) * Nm * Pm * Nr * Pr
        val load = opsInBlock(ld1) * Nm * Pm * Nr * Pr
        val store = opsInBlock(ld2) * Nr * Pr

        debug(s"AccumFold $lhs (Nm = $Nm, Pm = $Pm, Nr = $Nr, Pr = $Pr)")
        debug(s"  body: $body")
        debug(s"  reduce: $reduce")
        debug(s"  load: $load")
        debug(s"  store: $store")
        body + reduce + load + store

      case _ =>
        blocks(rhs).map{blk => opsInBlock(blk)}.fold(NoOps){_+_} + opsIn(lhs)
    }
    opScope ::= ops
  }

  override def preprocess[A:Manifest](b: Block[A]) = {
    opScope = Nil
    super.preprocess(b)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    val G = 1000f*1000f*1000f
    val GB = 1024f*1024f*1024f*8f

    val total = opScope.fold(NoOps){_+_}
    totalOps = total

    val totalInsts = total.insts.toFloat / G
    val totalFLOPs = total.flops.toFloat / G
    val totalOnChipIn = total.onChipIn.toFloat / GB
    val totalOnChipOut = total.onChipOut.toFloat / GB
    val totalOffChipIn = total.dataIn.toFloat / GB
    val totalOffChipOut = total.dataOut.toFloat / GB

    val totalOnChip = totalOnChipIn + totalOnChipOut
    val totalOffChip = totalOffChipIn + totalOffChipOut

    val cycleAnalyzer = new LatencyAnalyzer{val IR: OpsAnalyzer.this.IR.type = OpsAnalyzer.this.IR}
    cycleAnalyzer.run(b)

    report("Instruction statistics:")
    report(s"  Instructions: ${total.insts} " + "(%.3f GI)".format(totalInsts))
    report(s"  FLOPs: ${total.flops} " + "(%.3f GFLOPs)".format(totalFLOPs))
    report(s"  On-chip loads: ${total.onChipOut} bits " + "(%.3f GB)".format(totalOnChipOut))
    report(s"  On-chip stores: ${total.onChipIn} bits " + "(%.3f GB)".format(totalOnChipIn))
    report(s"  Off-chip loads: ${total.dataIn} bits " + "(%.3f GB)".format(totalOffChipIn))
    report(s"  Off-chip stores: ${total.dataOut} bits " + "(%.3f GB)".format(totalOffChipOut))
    report("--")
    report("  Total on-chip transfers: " + "%.3f GB".format(totalOnChip))
    report("  Total off-chip transfers: " + "%.3f GB".format(totalOffChip))

    val totalCycles = cycleAnalyzer.totalCycles
    val runtime = totalCycles/(IR.CLK*1000000f)

    report("")
    report("Performance statistics (based on estimated runtime):")
    val MIPS = totalInsts / runtime
    val FLOPS = totalFLOPs / runtime
    val onChipLdBW = totalOnChipOut / runtime
    val onChipStBW = totalOnChipIn / runtime
    val offChipLdBW = totalOffChipIn / runtime
    val offChipStBW = totalOffChipOut / runtime

    val totalOnChipBW = totalOnChip / runtime
    val totalOffChipBW = totalOffChip / runtime

    report("  Instructions: %.3f GIPS".format(MIPS))
    report("  Floating point: %.3f GFLOPS".format(FLOPS))
    report("  On-chip load bandwidth: %.3f GB/s".format(onChipLdBW))
    report("  On-chip store bandwidth: %.3f GB/s".format(onChipStBW))
    report("  Off-chip load bandwidth: %.3f GB/s".format(offChipLdBW))
    report("  Off-chip store bandwidth: %.3f GB/s".format(offChipStBW))
    report("--")
    report("  Total on-chip bandwidth: %.3f GB/s".format(totalOnChipBW))
    report("  Total off-chip bandwidth: %.3f GB/s".format(totalOffChipBW))

    super.postprocess(b)
  }

}

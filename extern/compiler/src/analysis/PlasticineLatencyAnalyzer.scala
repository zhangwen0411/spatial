package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import java.io.{File,FileWriter,PrintWriter}


trait PlasticineLatencyAnalyzer extends ModelingTraversal {
  val IR: SpatialExp with NodeMetadataOpsExp with LatencyModel
  import IR._
  import ReductionTreeAnalysis._

  override val name = "Plasticine Latency"
  debugMode = SpatialConfig.debugging || SpatialConfig.loudModels || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  lazy val model = new PlasticineLatencyModel{val IR: PlasticineLatencyAnalyzer.this.IR.type = PlasticineLatencyAnalyzer.this.IR}

  override def latencyOf(e: Exp[Any]) = if (inHwScope) model.latencyOf(e, inReduce) else 0L

  override def latencyOfPipe(b: Block[Any]): Long = {
    getStages(b).map{stage => latencyOf(stage)}.sum
  }

  override def silence() {
    model.silence()
    super.silence()
  }

  var cycleScope: List[Long] = Nil
  var totalCycles: Long = 0L

  val table_init = """<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""
  val pipe_diagram = new PrintWriter(new File("plasticene_pipe_diagram.html" ))

  def print_stage_prefix(name: String, i: Int=0) {
    pipe_diagram.write("<TR>" + "<TD></TD>"*i + s"<TD>$name: ")
    pipe_diagram.write(s"""<div data-role="collapsible">
    <h4>expand</h4>${table_init}""")
  }
  def print_stage_suffix(cycs: Long) {
    pipe_diagram.write("""</TABLE></div>""")
    pipe_diagram.write(s"Latency: ${cycs}</TD></TR>")

  }


  override def resume() {
    inHwScope = true
    cycleScope ::= latencyOfBlock(savedBlock.get).sum
    inHwScope = false
  }

  var indent = 0

  def latencyOfBlock(b: Block[Any], par_mask: Boolean = false): List[Long] = {
    val outerScope = cycleScope
    cycleScope = Nil
    tab += 1
    //traverseBlock(b) -- can cause us to see things like counters as "stages"
    getControlNodes(b).zipWithIndex.foreach{ case (n, i) =>
      indent = i
      n match {
        case s@Def(d) => traverse(s.asInstanceOf[Sym[Any]], d)
        case _ =>
      }
    }
    tab -= 1

    val cycles = cycleScope
    cycleScope = outerScope
    (cycles)
  }


  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    val cycles = rhs match {
      case EatReflect(Hwblock(blk)) =>
        inHwScope = true
        val body = latencyOfBlock(blk).sum
        save(blk)
        inHwScope = false
        body

      case EatReflect(Pipe_parallel(func)) =>
        debugs(s"Parallel $lhs: ")
        print_stage_prefix(s"parallel $lhs", 0)
        val blks = latencyOfBlock(func, true)
        print_stage_suffix(blks.max  + latencyOf(lhs))
        if (debugMode) blks.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}
        blks.max + latencyOf(lhs)

      // --- Pipe
      case EatReflect(Unit_pipe(func)) if isInnerPipe(lhs) =>
        debugs(s"Pipe $lhs: ")
        val pipe = latencyOfPipe(func)
        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent*0)
        pipe_diagram.write(s"""<TD>inner_unit stage${indent}: <p> Cycles: ${pipe}</TD>""")
        pipe_diagram.write(s"""<TR>""")
        debugs(s"- pipe = $pipe")
        pipe + latencyOf(lhs)

      case EatReflect(Pipe_foreach(cchain, func, _)) if isInnerPipe(lhs) =>
        val N = Math.ceil(nIters(cchain,ignorePar=true).toDouble / model.LANES).toLong
        debugs(s"Foreach $lhs (N = $N):")
        val pipe = latencyOfPipe(func)
        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent*0)
        pipe_diagram.write(s"""<TD>inner_foreach stage${indent}: <p> Cycles: ${pipe + latencyOf(lhs)} <p> x${N} iters...</TD> """)
        // if (N > 1) {pipe_diagram.write(s"""<TD> x${N} iters...</TD>""")}
        pipe_diagram.write(s"""<TR>""")

        debugs(s"- pipe = $pipe")
        pipe + N - 1 + latencyOf(lhs)

      case EatReflect(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,_,_,_,rV)) if isInnerPipe(lhs) =>
        val N = Math.ceil(nIters(cchain,ignorePar=true).toDouble / model.LANES).toLong
        val P = model.LANES

        debugs(s"Reduce $lhs (N = $N):")

        val body = latencyOfPipe(func)
        val internal = latencyOfPipe(rFunc) * reductionTreeHeight(P)

        val cycle = latencyOfCycle(iFunc) + latencyOfCycle(ld) + latencyOfCycle(rFunc) + latencyOfCycle(st)

        debugs(s"- body  = $body")
        debugs(s"- tree  = $internal")
        debugs(s"- cycle = $cycle")
        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent*0)
        pipe_diagram.write(s"""<TD>inner_fold stage${indent}: <p> Cycles: ${cycle} <p> x${N} iters...</TD>""")
        // if (N > 1) {pipe_diagram.write(s"""<TD> x${N} iters...</TD>""")}
        pipe_diagram.write(s"""<TR>""")

        body + internal + N*cycle + latencyOf(lhs)

      // --- Sequential
      case EatReflect(Unit_pipe(func)) if isSequential(lhs) =>
        debugs(s"Outer Pipe $lhs:")
        print_stage_prefix(s"unit_pipe $lhs", 0)
        val stages = latencyOfBlock(func)
        print_stage_suffix(stages.sum)
        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}

        stages.sum + latencyOf(lhs)


      // --- Metapipeline and Sequential
      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val N = nIters(cchain)
        debugs(s"Outer Foreach $lhs (N = $N):")
        print_stage_prefix(s"foreach $lhs", 0)
        val stages = latencyOfBlock(func)
        print_stage_suffix(stages.sum)
        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}

        if (isMetaPipe(lhs)) { stages.max * (N - 1) + stages.sum + latencyOf(lhs) }
        else                 { stages.sum * N + latencyOf(lhs) }

      case EatReflect(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,_,_,_,_)) =>
        val N = nIters(cchain)
        val P = parsOf(cchain).reduce(_*_)
        debugs(s"Outer Reduce $lhs (N = $N):")

        print_stage_prefix(s"pipe_fold_map $lhs", 0)
        val mapStages = latencyOfBlock(func)
        print_stage_suffix(mapStages.sum)
        val internal = latencyOfPipe(rFunc) * P //* reductionTreeHeight(P)
        val cycle = latencyOfCycle(iFunc) + latencyOfCycle(ld) + latencyOfCycle(rFunc) + latencyOfCycle(st)

        val reduceStage = internal + cycle
        pipe_diagram.write(s"<TR><TD>pipe_fold_reduce $lhs <p> latency $reduceStage </TD></TR>")
        val stages = mapStages :+ reduceStage

        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}

        if (isMetaPipe(lhs)) { stages.max * (N - 1) + stages.sum + latencyOf(lhs) }
        else                 { stages.sum * N + latencyOf(lhs) }

      case EatReflect(Accum_fold(ccOuter,ccInner,accum,zero,fA,iFunc,func,ld1,ld2,rFunc,st,_,_,_,_,_,_,_)) =>
        val Nm = nIters(ccOuter)
        val Nr = nIters(ccInner)
        val Pm = parsOf(ccOuter).reduce(_*_) // Parallelization factor for map
        val Pr = parsOf(ccInner).reduce(_*_) // Parallelization factor for reduce

        debugs(s"Block Reduce $lhs (Nm = $Nm, Nr = $Nr)")

        print_stage_prefix(s"accum_fold_map $lhs", 0)
        val mapStages: List[Long] = latencyOfBlock(func)
        print_stage_suffix(mapStages.sum)
        val internal: Long = latencyOfPipe(iFunc) + latencyOfPipe(ld1) + latencyOfPipe(rFunc) * Pm
        val accumulate: Long = latencyOfPipe(ld2) + latencyOfPipe(rFunc) + latencyOfPipe(st)

        val reduceStage: Long = internal + accumulate + Nr - 1
        pipe_diagram.write(s"<TR><TD>accum_fold_reduce $lhs <p> latency $reduceStage </TD></TR>")
        val stages = mapStages :+ reduceStage

        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}

        if (isMetaPipe(lhs)) { stages.max * (Nm - 1) + stages.sum + latencyOf(lhs) }
        else                 { stages.sum * Nm + latencyOf(lhs) }

      case _ =>
        // No general rule for combining blocks
        blocks(rhs).foreach{blk => traverseBlock(blk)}
        latencyOf(lhs)
    }
    cycleScope ::= cycles
  }

  override def run[A:Manifest](b: Block[A]) = {
    if (SpatialConfig.genCGRA) super.run(b) else b
  }

  override def preprocess[A:Manifest](b: Block[A]) = {
    cycleScope = Nil
    pipe_diagram.write("""<!DOCTYPE html>
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css">
<script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
<script src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"></script>
</head><body>

  <div data-role="main" class="ui-content">
    <h2>Plasticene Pipe Diagram</h2>
<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""
    )
    super.preprocess(b)
  }
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    // TODO: Could potentially have multiple accelerator designs in a single program
    // Eventually want to be able to support multiple accel scopes
    totalCycles = cycleScope.sum

    report(s"Plasticine Report: ")
    report(s"Estimated cycles: $totalCycles")
    report(s"Estimated runtime (at " + "%.2f".format(model.CLK) +"MHz): " + "%.8f".format(totalCycles/(model.CLK*1000000f)) + "s")

    pipe_diagram.write(s"""  </TABLE>
Estimated cycles: $totalCycles
</body>
</html>""")
    pipe_diagram.close

    (b)
  }

}

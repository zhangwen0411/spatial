package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import java.io.{File,FileWriter,PrintWriter}

trait LatencyAnalysisExp extends LatencyModel {
  this: SpatialExp =>

  val interruptCycles = 96
  val flushCycles = 512
  val pcieCycles = 42500
  val baseCycles = flushCycles + interruptCycles + pcieCycles

  var CLK = 150.0f // Clock frequency in MHz
}

trait LatencyAnalyzer extends ModelingTraversal {
  val IR: SpatialExp with LatencyAnalysisExp
  import IR._
  import ReductionTreeAnalysis._

  override val name = "Latency Analyzer"
  debugMode = SpatialConfig.debugging || SpatialConfig.loudModels || SpatialConfig.latency
  verboseMode = SpatialConfig.verbose

  override def silence() {
    IR.silenceLatencyModel()
    super.silence()
  }
  val table_init = """<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""
  val pipe_diagram = new PrintWriter(new File("pipe_diagram.html" ))

  def print_stage_prefix(name: String, i: Int=0) {
    pipe_diagram.write("<TR>" + "<TD></TD>"*i + s"<TD>$name: ")
    pipe_diagram.write(s"""<div data-role="collapsible">
    <h4>expand</h4>${table_init}""")
  }
  def print_stage_suffix(cycs: Long) {
    pipe_diagram.write("""</TABLE></div>""")
    pipe_diagram.write(s"Latency: ${cycs}</TD></TR>")

  }
  def emit_pipe_diagram(diagram: List[(Int,Sym[Any],List[Long],String,Long)] ) {
    val layers = diagram.map{case (l, s, c, n, its) => l}
    val syms = diagram.map{case (l, s, c, n, its) => l}
    val show_iters = 3
    val max_layer:Int = layers.max
    pipe_diagram.write(s"""Diagram to print: <p>${diagram.mkString("<p>")}<p><p>""")

//     val hierarchical_html = layers.distinct.sorted.filter{ i => i > 0 }.map { cur_layer =>
//       val layer = diagram.filter{case (l,_,_,_,_) => l == cur_layer}
//       val table = layer.zipWithIndex.map { case (this_layer,l) =>
//         this_layer._3.zipWithIndex.map { case (cyc, i) =>
//           var ans = s"""<TR>"""
//           ans += s"""<TD></TD>"""*l*show_iters // Stage delay
//           ans += s"""<TD></TD>"""*i // Pipeline delay
//           ans += s"""<TD> Niters:${this_layer._5}<p>
// ${this_layer._4}_$i - $cyc cycles<p>
// expand</TD>"""*(show_iters-i)
//           ans += s"""<TD>...</TD>"""
//           ans += s"""</TR>
//     """
//           ans
//         }.mkString(" ")
//       }.mkString(" ")

//       val layer_html = s"""Level: $cur_layer
// <TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">""" + table + """
//   </TABLE>"""

//       layer_html
//     }

//     hierarchical_html.foreach{ case a => 
//       // pipe_diagram.write(s"""$a""")
//     }

  }

  var cycleScope: List[Long] = Nil
  var totalCycles: Long = 0L
  var scopeLevel: Int = 0
  var diagram: List[(Int, Sym[Any], List[Long], String, Long)] = List()

  def debugs(x: => Any) = debug(".."*scopeLevel + x)

  override def resume() {
    inHwScope = true
    cycleScope ::= latencyOfBlock(savedBlock.get).sum
    inHwScope = false
  }

  var indent = 0

  def latencyOfBlock(b: Block[Any], par_mask: Boolean = false): List[Long] = {
    val outerScope = cycleScope
    cycleScope = Nil
    scopeLevel += 1
    //traverseBlock(b) -- can cause us to see things like counters as "stages"
    getControlNodes(b).zipWithIndex.foreach{ case (n, i) =>
      indent = if (!par_mask) {i} else {0}
      n match {
        case s@Def(d) => traverse(s.asInstanceOf[Sym[Any]], d)
        case _ =>
      }
    }
    scopeLevel -= 1

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
        // print_diagram(lhs, List(body), "hwblock")
        diagram = diagram :+ ((scopeLevel, lhs, List(body), "hwblock", 1))
        inHwScope = false
        body

      case EatReflect(Pipe_parallel(func)) =>
        debugs(s"Parallel $lhs: ")
        print_stage_prefix(s"parallel $lhs", 0)

        val blks = latencyOfBlock(func, true)
        print_stage_suffix(blks.max  + latencyOf(lhs))
        if (debugMode) blks.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}
        // print_diagram(lhs, List(1), "parallel")
        blks.max + latencyOf(lhs)

      // --- Pipe
      case EatReflect(Unit_pipe(func)) if isInnerPipe(lhs) =>
        debugs(s"Pipe $lhs: ")
        val pipe = latencyOfPipe(func)
        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent)
        pipe_diagram.write(s"""<TD>inner_unit $lhs: <p> Cycles: ${pipe}</TD>""")
        pipe_diagram.write(s"""<TR>""")
        debugs(s"- pipe = $pipe")
        pipe + latencyOf(lhs)

      case EatReflect(Pipe_foreach(cchain, func, _)) if isInnerPipe(lhs) =>
        val N = nIters(cchain)
        debugs(s"Foreach $lhs (N = $N):")
        val pipe = latencyOfPipe(func)
        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent)
        pipe_diagram.write(s"""<TD>inner_foreach $lhs: <p> Cycles: ${pipe + latencyOf(lhs)}</TD>""")
        if (N > 1) {pipe_diagram.write(s"""<TD> x${N} iters...</TD>""")}
        pipe_diagram.write(s"""<TR>""")
        debugs(s"- pipe = $pipe")
        pipe + N - 1 + latencyOf(lhs)

      case EatReflect(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,_,_,_,rV)) if isInnerPipe(lhs) =>
        val N = nIters(cchain)
        val P = parsOf(cchain).reduce(_*_)

        debugs(s"Reduce $lhs (N = $N):")

        val fuseMapReduce = canFuse(func,rFunc,rV,P)

        val body = latencyOfPipe(func)
        val internal = if (fuseMapReduce) Math.max(reductionTreeHeight(P) - 2, 0)
                       else latencyOfPipe(rFunc) * reductionTreeHeight(P)


        val cycle = latencyOfCycle(iFunc) + latencyOfCycle(ld) + latencyOfCycle(rFunc) + latencyOfCycle(st)


        debugs(s"- body  = $body")
        debugs(s"- tree  = $internal")
        debugs(s"- cycle = $cycle")

        pipe_diagram.write(s"""<TR>""" + """<TD></TD>"""*indent)
        pipe_diagram.write(s"""<TD>inner_fold $lhs: <p> Cycles: ${cycle}</TD>""")
        if (N > 1) {pipe_diagram.write(s"""<TD> x${N} iters...</TD>""")}
        pipe_diagram.write(s"""<TR>""")

        body + internal + N*cycle + latencyOf(lhs)

      // --- Sequential
      case EatReflect(Unit_pipe(func)) if isSequential(lhs) =>
        // print_diagram(lhs, List(1), "seq_unit_pipe")
        diagram = diagram :+ ((scopeLevel, lhs, List(1), "seq_unit_pipe", 1))
        debugs(s"Outer Pipe $lhs:")
        val stages = latencyOfBlock(func)
        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}

        stages.sum + latencyOf(lhs)


      // --- Metapipeline and Sequential
      case EatReflect(Pipe_foreach(cchain, func, _)) =>
        val N = nIters(cchain)
        debugs(s"Outer Foreach $lhs (N = $N):")
        val stages = latencyOfBlock(func)
        if (debugMode) stages.reverse.zipWithIndex.foreach{case (s,i) => debugs(s"- $i. $s")}
        // print_diagram(lhs, List(1), "foreach")
        diagram = diagram :+ ((scopeLevel, lhs, List(1), "foreach", 1))

        if (isMetaPipe(lhs)) { stages.max * (N - 1) + stages.sum + latencyOf(lhs) }
        else                 { stages.sum * N + latencyOf(lhs) }

      case EatReflect(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,_,_,_,_,_)) =>
        // print_diagram(lhs, List(1), "fold")
        diagram = diagram :+ ((scopeLevel, lhs, List(1), "fold", 1))
        val N = nIters(cchain)
        val P = parsOf(cchain).reduce(_*_)
        debugs(s"Outer Reduce $lhs (N = $N):")

        val mapStages = latencyOfBlock(func)
        val internal = latencyOfPipe(rFunc) * reductionTreeHeight(P)
        val cycle = latencyOfCycle(iFunc) + latencyOfCycle(ld) + latencyOfCycle(rFunc) + latencyOfCycle(st)

        val reduceStage = internal + cycle
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
        val internal: Long = latencyOfPipe(iFunc) + latencyOfPipe(ld1) + latencyOfPipe(rFunc) * reductionTreeHeight(Pm)
        val accumulate: Long = latencyOfPipe(ld2) + latencyOfPipe(rFunc) + latencyOfPipe(st)

        print_stage_prefix(s"accum_fold_reduce $lhs", 1)
        val reduceStage: Long = internal + accumulate + Nr - 1
        print_stage_suffix(reduceStage)
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
    <h2>Pipe Diagram</h2>
<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""
    )

    super.preprocess(b)
  }
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    // TODO: Could potentially have multiple accelerator designs in a single program
    // Eventually want to be able to support multiple accel scopes
    totalCycles = cycleScope.sum + IR.baseCycles

    report(s"Estimated cycles: $totalCycles")
    report(s"Estimated runtime (at " + "%.2f".format(IR.CLK) +"MHz): " + "%.8f".format(totalCycles/(IR.CLK*1000000f)) + "s")
    emit_pipe_diagram(diagram)
    pipe_diagram.write("""  </TABLE>
</body>
</html>""")
    pipe_diagram.close
    (b)
  }

}

package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal
import scala.virtualization.lms.common.EffectExp

import ppl.delite.framework.analysis.QuickTraversal
import ppl.delite.framework.Config

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import spatial.compiler.transform._

import scala.collection.mutable.{HashMap,HashSet,ArrayBuffer}
import java.io.PrintStream

import scala.virtualization.lms.util.GraphUtil._

trait DSE extends Traversal {
  val IR: SpatialCompiler
  import IR.{infix_until => _, _}

  override val name = "Design Space Exploration"
  debugMode = SpatialConfig.enableDSE

  lazy val ctrlAnalyzer = new ControlSignalAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val paramAnalyzer = new ParameterAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val bndAnalyzer = new BoundAnalyzer with QuickTraversal{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val contention = new ContentionAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
  lazy val memAnalyzer = new MemoryAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}

  lazy val tileSizes = paramAnalyzer.tileSizes
  lazy val parParams = paramAnalyzer.parParams
  lazy val localMems = ctrlAnalyzer.localMems
  lazy val metapipes = ctrlAnalyzer.metapipes
  lazy val topController = ctrlAnalyzer.top

  override def run[A:Manifest](b: Block[A]) = {
    debug("Starting traversal " + name)
    bndAnalyzer.run(b)
    ctrlAnalyzer.run(b)
    paramAnalyzer.run(b)

    if (SpatialConfig.enableDSE) {
      debug("Tile Sizes: ")
      def ctx(x: Exp[Any]) = {
        val c = topContext(mpos(x.pos)); s"${c.fileName}:${c.line}"
      }
      tileSizes.foreach{t => debug(s"  ${ctx(t)}: $t")}
      debug("Parallelization Factors:")
      parParams.foreach{t => debug(s"  ${ctx(t)}: $t")}
      debug("Metapipelining Toggles:")
      metapipes.foreach{t => debug(s"  ${ctx(t)}: $t")}
    }

    if (SpatialConfig.enableDSE) {
      val (space, restrict, numericFactors) = pruneSpace()
      dse(b, space, restrict, numericFactors)
    }

    debug("Freezing DSE parameters")
    tileSizes.foreach{p => p.fix}
    parParams.foreach{p => p.fix}
    contention.run(topController)
    (b)
  }

  /*def compressSpace(space: Map[Param[Int],Domain[Int]], restrict: List[Restrict]) = {
    val params = space.keys.toList
    val edges = restrict.map(_.productIterator.flatMap{case p: Param[_] => Some(p.asInstanceOf[Param[Int]]), case _ => None })
    val sccs = stronglyConnectedComponents(params, {param: Param[Int] =>
      edges.flatMap{e => if (e.contains(param)) e.filter{p => p != param} else Nil }.distinct
    })

    // These are almost exactly the same as database joins - should talk to Chris A about this
    sccs.flatMap{scc =>
      if (scc.length > 1 && scc.map(_.len).reduce{_*_} < 50000) {
        crossFilter(scc){pt =>
          scc.zip(pt).foreach{case (domain,v) => domain.setValue(v) }
          restrict.forall(_.evaluate)
        }
      }
      else scc
    }
  }*/

  // A. Get lists of parameters and prune space
  def pruneSpace() = {
    val restrict = paramAnalyzer.restrict
    val ranges   = paramAnalyzer.range

    val numericFactors = tileSizes ++ parParams

    // HACK: All par factors for readers and writers of a given SRAM must be equal or one
    /*for ((mem,factors) <- accFactors) {
      val distFactors = factors.distinct
      if (distFactors.length > 1) restrict ::= REqualOrOne(distFactors)
    }*/

    debug("Pruning design space...")
    debug("Found the following parameter restrictions: ")
    for (r <- restrict) { debug(s"  $r") }
    for ((p,r) <- ranges if numericFactors.contains(p)) { debug(s"  $p: ${r.start}:${r.step}:${r.end}") }

    // Prune single factors
    val initialSpace = prune(numericFactors, ranges, restrict)

    // Remove restrictions that have already been pruned for (1 param only)
    val prunedRestrict = restrict.filterNot{r => r.deps.length <= 1}

    // HACK: Don't sequentialize pipes with tile load/store children
    val nonTxMetapipes = metapipes.filterNot(pipe => childrenOf(pipe).exists(isOffChipTransfer(_)))

    val mps = nonTxMetapipes.map{mp => Domain(List(true,false), {c: Boolean => c match {case true => styleOf(mp) = CoarsePipe; case false => styleOf(mp) = SequentialPipe}; () }) }
    val space = (initialSpace ++ mps)

    debug("")
    debug("Pruned space:")
    if (debugMode) {
      (numericFactors ++ nonTxMetapipes).zip(space).foreach{case (p, d) => debug(s"$p: $d")}
    }

    (space, prunedRestrict, numericFactors)
  }


  def dse[A:Manifest](b: Block[A], space: List[Domain[_]], restrict: Set[Restrict], numericFactors: List[Param[Int]]) {

    def isLegalSpace() = restrict.forall(_.evaluate)

    // B. Specify FPGA target (hardcoded right now)/
    val target = FPGAResourceSummary(alms=262400,regs=524800,dsps=1963,bram=2567,streams=13)

    val areaAnalyzer = new AreaAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    val cycleAnalyzer = new LatencyAnalyzer{val IR: DSE.this.IR.type = DSE.this.IR}
    cycleAnalyzer.silence()
    areaAnalyzer.silence()
    paramAnalyzer.silence()
    bndAnalyzer.silence()
    memAnalyzer.silence()

    // C. Calculate space
    debug("Running DSE")


    val N = space.size
    val indexedSpace = (space, xrange(0,N,1).toList).zipped

    // D. DSE
    val dims = space.map(_.len)
    val spaceSize = dims.map(_.toLong).fold(1L){_*_}
    val prods = List.tabulate(N){i => dims.slice(i+1,N).map(_.toLong).fold(1L){_*_}}

    debug(s"Total space size is $spaceSize")

    if (spaceSize > Int.MaxValue) throw new Exception("Spaces over 2^32 currently unsupported.")
    val size = spaceSize.toInt

    // --- Find all legal points
    // FIXME: This could take a long time if space size is really large.
    // Maybe take a random sample of all points instead, accounting that some will be illegal?
    val legalPoints = ArrayBuffer[Int]()

    val legalStart = System.currentTimeMillis
    debug(s"Enumerating all legal points...")
    var startTime = System.currentTimeMillis
    var nextNotify = 0.0; var notifyStep = 20000
    for (i <- 0 until size) {
      indexedSpace.foreach{case (domain,d) => domain.set( ((i / prods(d)) % dims(d)).toInt ) }
      if (isLegalSpace()) legalPoints += i

      if (i > nextNotify) {
        val time = System.currentTimeMillis - startTime
        debug("%.4f".format(100*(i/size.toFloat)) + s"% ($i / $size) Complete after ${time/1000} seconds")

        nextNotify += notifyStep
      }
    }
    val legalCalcTime = System.currentTimeMillis - legalStart
    var legalSize = legalPoints.length
    debug(s"Legal space size is $legalSize (elapsed time: ${legalCalcTime/1000} seconds)")

    // Take 200,000 legal points at most
    val points = util.Random.shuffle(legalPoints.toList).take(100000)
    legalSize = points.length // = min(200000, legalSize)


    // --- PROFILING
    val PROFILING = true
    var clockRef = 0L
    def resetClock() { clockRef = System.currentTimeMillis }

    var setTime = 0L
    def endSet() { setTime += System.currentTimeMillis - clockRef; resetClock() }
    var bndTime = 0L
    def endBnd() { bndTime += System.currentTimeMillis - clockRef; resetClock() }
    var conTime = 0L
    def endCon() { conTime += System.currentTimeMillis - clockRef; resetClock() }
    var areaTime = 0L
    def endArea() { areaTime += System.currentTimeMillis - clockRef; resetClock() }
    var cyclTime = 0L
    def endCycles() { cyclTime += System.currentTimeMillis - clockRef; resetClock() }
    var paretoTime = 0L
    def endPareto() { paretoTime += System.currentTimeMillis - clockRef; resetClock() }

    def resetAllTimers() {
      setTime = 0; bndTime = 0; conTime = 0; areaTime = 0; cyclTime = 0; paretoTime = 0;
    }
    def getPercentages(total: Long) = {
      val t = total.toFloat
      val set = 100*setTime / t
      val bnd = 100*bndTime / t
      val con = 100*conTime / t
      val area = 100*areaTime / t
      val cycl = 100*cyclTime / t
      val pareto = 100*paretoTime / t
      debug("set: %.1f, bnd: %.1f, con: %.1f, area: %.1f, cycles: %.1f, pareto: %.1f".format(set, bnd,con,area,cycl,pareto))
    }

    def evaluate() = {
      bndAnalyzer.run(b)
      if (PROFILING) endBnd()

      memAnalyzer.run(localMems)
      if (PROFILING) endSet()

      contention.run(topController)
      if (PROFILING) endCon()

      areaAnalyzer.run(b)
      if (PROFILING) endArea()

      cycleAnalyzer.run(b)
      if (PROFILING) endCycles()
      (areaAnalyzer.totalArea, cycleAnalyzer.totalCycles)
    }

    // --- Run DSE
    case class ParetoPt(idx: Int, alms: Int, cycles: Long)


    val pareto = ArrayBuffer[ParetoPt]()     // Set of pareto points

    // Resource and area estimates for all points (including legal but invalid ones)
    val valid = new Array[Boolean](legalSize)
    val alms = new Array[Int](legalSize)
    val regs = new Array[Int](legalSize)
    val dsps = new Array[Int](legalSize)
    val bram = new Array[Int](legalSize)
    val cycles = new Array[Long](legalSize)

    var nValid = 0

    debug("And aaaawaaaay we go!")
    startTime = System.currentTimeMillis
    nextNotify = 0.0; notifyStep = 200
    for (p <- 0 until legalSize) {
      if (PROFILING) resetClock() // PROFILING

      // Get and set current parameter combination
      val pt = points(p)
      indexedSpace.foreach{case (domain,d) => domain.set( ((pt / prods(d)) % dims(d)).toInt ) }

      val (area,runtime) = evaluate()
      alms(p) = area.alms
      regs(p) = area.regs
      dsps(p) = area.dsps
      bram(p) = area.bram
      cycles(p) = runtime

      val isValid = area <= target
      valid(p) = isValid

      if (isValid) {
        nValid += 1
        // Check if this is a pareto frontier candidate
        var wasAdded = false
        var candidate = true

        var j = 0
        while (j < pareto.size && !wasAdded && candidate) {
          val prev = pareto(j)
          if (area.alms < prev.alms && runtime < prev.cycles) {
            pareto(j) = ParetoPt(p,area.alms,runtime) // Strictly less than existing frontier point: replace
            wasAdded = true
          }
          candidate = area.alms < prev.alms || runtime < prev.cycles
          j += 1
        }
        if (!wasAdded && candidate) pareto += ParetoPt(p,area.alms,runtime)
      }
      if (PROFILING) endPareto()

      if (p > nextNotify) {
        val time = System.currentTimeMillis - startTime
        debug("%.4f".format(100*(p/legalSize.toFloat)) + s"% ($p / $legalSize) Complete after ${time/1000} seconds")

        if (PROFILING) getPercentages(time)

        nextNotify += notifyStep
      }
    }

    val paretoSize = pareto.length
    val time = (System.currentTimeMillis - startTime)/1000.0
    debug(s"Completed space search in $time seconds")
    debug(s"Valid designs generated: $nValid / $legalSize")
    debug(s"Pareto frontier size: $paretoSize / $nValid")

    val names = numericFactors.map{p => nameOf(p).getOrElse(p.toString) } ++ metapipes.map{mp => nameOf(mp).getOrElse(mp.toString) }

    val pwd = sys.env("HYPER_HOME")
    val name = Config.degFilename.replace(".deg","")
    val filename = s"${pwd}/data/${name}.csv"

    debug(s"Printing results to file: $filename")
    val printStart = System.currentTimeMillis
    val pw = new PrintStream(filename)
    pw.println(names.mkString(", ") + ", ALMS, Regs, DSPs, BRAM, Cycles, Valid, Pareto, Synthesized")
    for (p <- 0 until legalSize) {
      val pt = points(p)
      indexedSpace.foreach{case (domain,d) => domain.set( ((pt / prods(d)) % dims(d)).toInt ) }
      val values = numericFactors.map{p => p.x.toString} ++ metapipes.map{mp => isMetaPipe(mp).toString}

      val isPareto = pareto.exists{pt => pt.idx == p}
      pw.println(values.mkString(", ") + s", ${alms(p)}, ${regs(p)}, ${dsps(p)}, ${bram(p)}, ${cycles(p)}, ${valid(p)}, $isPareto, false")
    }
    pw.close()
    val printTime = System.currentTimeMillis - printStart
    debug(s"Finished printing in ${printTime/1000} seconds")

    val ppw = new PrintStream(s"${pwd}/data/${name}_pareto.csv")
    ppw.println(names.mkString(", ") + ", ALMs, Cycles, ALM Usage (%), Runtime (sec)")
    pareto.foreach{paretoPt =>
      val p = paretoPt.idx
      val pt = points(p)
      indexedSpace.foreach{case (domain,d) => domain.set( ((pt / prods(d)) % dims(d)).toInt ) }
      val values = numericFactors.map{p => p.x.toString} ++ metapipes.map{mp => isMetaPipe(mp).toString}

      val runtime = paretoPt.cycles/(IR.CLK*1000000.0f)
      val almUsage = 100.0f*paretoPt.alms/target.alms

      ppw.println(values.mkString(", ") + s", ${paretoPt.alms}, ${paretoPt.cycles}, ${almUsage}, ${runtime}")
    }
    ppw.close()

    msg("DSE completed.")

    // F. Show user results, pick point on pareto curve
    // TODO
    // By default pick most performant pareto point

    // G. Set parameters
    if (pareto.nonEmpty) {
      val paretoPt = pareto.reduce{(a,b) => if (a.cycles < b.cycles) a else b}
      val pt = points(paretoPt.idx)
      indexedSpace.foreach{case (domain,d) => domain.set( ((pt / prods(d)) % dims(d)).toInt ) }
    }
    else {
      stageError("No valid points discovered for design space")
    }
  }
}

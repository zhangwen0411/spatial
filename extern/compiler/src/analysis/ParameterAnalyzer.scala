package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Expressions,Traversal}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.collection.mutable.{HashMap,ArrayBuffer}

trait ParamRestrictions extends Expressions with NameOpsExp {
  this: GenOverloadHack =>

  private def qt(x: Param[_]) = nameOf(x).getOrElse(s"$x")

  type RRange = scala.collection.immutable.Range

  trait Restrict {this: Product =>
    def evaluate: Boolean
    def deps = this.productIterator.flatMap{
      case p: Param[Int] => List(p)
      case x: List[_] => x.flatMap{case p: Param[Int] => Some(p); case _ => None}
      case _ => Nil
    }
    def dependsOnlyOn(x: Param[Int]*) = {
      val d = deps.toList.distinct
      val c = x.toList.distinct
      d.length == c.length && c.forall(d contains _)
    }
  }

  case class RLess(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = a.x < b.x
    override def toString = s"${qt(a)} < ${qt(b)}"
  }
  case class RLessEqual(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = a.x <= b.x
    override def toString = s"${qt(a)} <= ${qt(b)}"
  }
  case class RDivides(a: Param[Int], b: Param[Int]) extends Restrict {
    def evaluate = b.x % a.x == 0
    override def toString = s"${qt(a)} divides ${qt(b)}"
  }
  case class RDividesConst(a: Param[Int], b: Int) extends Restrict {
    def evaluate = b % a.x == 0
    override def toString = s"${qt(a)} divides $b"
  }
  case class RDividesQuotient(a: Param[Int], n: Int, d: Param[Int]) extends Restrict {
    def evaluate = {
      val q = Math.ceil(n.toDouble / d.x).toInt
      a.x < q && (q % a.x == 0)
    }
    override def toString = s"${qt(a)} divides $n/${qt(d)}"
  }
  case class RProductLessThan(ps: List[Param[Int]], y: Int) extends Restrict {
    def evaluate = ps.map(_.x).fold(1){_*_} < y
    override def toString = "product(" + ps.map(qt(_)).mkString(",") + s") < $y"
  }
  case class REqualOrOne(ps: List[Param[Int]]) extends Restrict {
    def evaluate = {
      val p = ps.map(_.x).distinct
      p.length == 1 || (p.length == 2 && p.contains(1))
    }
    override def toString = "(" + ps.map(qt(_)).mkString(",") + ") equal or one"
  }
  case class Domain[T](options: List[T], setter: T => Unit) {
    def apply(i: Int) = options(i)
    def set(i: Int) = setter(options(i))
    def setValue(v: T) = setter(v)
    def len: Int = options.length
    override def toString = if (len < 10) "Domain(" + options.mkString(",") + ")" else "Domain(" + options.take(10).mkString(", ") + "... [" + (len-10) + " more])"

    def filter(cond: () => Boolean) = {
      new Domain(options.filter{t => setValue(t); cond()}, setter)
    }
  }
  object Domain {
    def apply(r: RRange, setter: Int => Unit) = {
      if (r.start % r.step != 0) {
        val start = r.step*(r.start/r.step + 1)
        new Domain[Int]((start to r.end by r.step).toList :+ r.start, setter)
      }
      else new Domain[Int](r.toList, setter)
    }
    def restricted(r: RRange, setter: Int => Unit, cond: () => Boolean) = {
      val values = ArrayBuffer[Int]()
      var start = r.start
      if (r.start % r.step != 0) {
        start = r.step*((r.start/r.step) + 1)
        setter(r.start);
        if (cond()) values += r.start
      }
      for (i <- start to r.end by r.step) {
        setter(i)
        if (cond()) values += i
      }
      new Domain[Int](values.toList, setter)
    }
  }

  def prune(params: List[Param[Int]], ranges: HashMap[Param[Int],RRange], restrict: Set[Restrict]) = {
    val pruneSingle = params.map{t =>
      val restricts = restrict.filter(_.dependsOnlyOn(t))
      t -> Domain.restricted(ranges(t), {c: Int => t.setValue(c)}, () => restricts.forall(_.evaluate))
    }
    // TODO: prune pairs?
    pruneSingle.map(_._2)
  }


  def xrange(start: Int, end: Int, step: Int) = new scala.collection.immutable.Range(start,end,step)
}


trait ParameterAnalysisExp extends ParamRestrictions with NodeMetadataOpsExp { this: SpatialExp => }
trait ParameterAnalyzer extends Traversal {
  val IR: SpatialExp with ParameterAnalysisExp
  import IR._

  override val name = "Parameter Analyzer"
  override val recurse = Always
  override val eatReflect = true
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  val MIN_TILE_SIZE = 96    // words
  val MAX_TILE_SIZE = 96000 // words
  val MAX_TILE      = 51340 // words, unused

  val MAX_PAR_FACTOR = 192  // duplications
  val MAX_OUTER_PAR  = 15

  var tileSizes: List[Param[Int]] = Nil  // Params used to calculate SRAM size
  var parParams: List[Param[Int]] = Nil  // Params used as parallelization factors for counters
  val range = HashMap[Param[Int],RRange]()

  var restrict: Set[Restrict] = Set.empty   // Restrictions on parameters
  var innerLoop = false

  override def preprocess[A:Manifest](b: Block[A]) = {
    for ((s,m) <- metadata) {
      if (domainOf(s).isDefined && s.isInstanceOf[Param[_]] && s.tp == manifest[Int]) {
        val d = domainOf(s).get
        range(s.asInstanceOf[Param[Int]]) = xrange(d._1, d._2, d._3)
      }
    }
    (b)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    tileSizes = tileSizes.distinct
    parParams = parParams.distinct
    super.postprocess(b)
  }

  def setRange(p: Param[Int], mn: Int, mx: Int, step: Int) = {
    if (!range.contains(p)) {
      range(p) = xrange(mn,mx,step)
    }
    else {
      val old = range(p)
      range(p) = xrange(Math.max(mn,old.start),Math.min(mx,old.end),Math.max(step,old.step))
    }
  }
  def setMax(p: Param[Int], mx: Int) = {
    if (!range.contains(p))
      range(p) = xrange(1,mx,1)
    else
      range(p) = xrange(range(p).start,Math.min(mx,range(p).end),range(p).step)
  }

  // ASSUMPTION: Parallelize by only last parameter
  def getParams(x: List[Exp[Int]]): List[Param[Int]] = x.last match {
    case p: Param[_] => List(p.asInstanceOf[Param[Int]])
    case _ => Nil
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Fifo_new(ParamFix(p),_) =>
      stageWarn("Paramterized fifo size is not yet supported")
      //tileSizes ::= p
      //setRange(p, 1, MAX_TILE_SIZE, MIN_TILE_SIZE)

    case Sram_new(_,_) =>
      val dims = dimsOf(lhs)

      val (consts,params) = dims.partition{ case ConstFix(_) => true; case _ => false }
      val cSize = consts.map{case ConstFix(c) => c.asInstanceOf[Int] }.fold(1){_*_}

      val tiles = params.flatMap{case ParamFix(p) => Some(p); case _ => None}
      debug(s"Found SRAM with parameterized dimensions: " + tiles.map(p => nameOf(p).getOrElse(p.toString)).mkString(", "))

      tiles.zipWithIndex.foreach{
        case (p, idx) =>
          tileSizes ::= p
          if (idx < params.length - 1) { setRange(p, 1, MAX_TILE_SIZE, 1) }
          else                         { setRange(p, 1, MAX_TILE_SIZE, MIN_TILE_SIZE) }
      }

      //if (tiles.length > 1) restrict ::= RProductLessThan(tiles, )

    case Counter_new(start,end,step,par) =>
      var max = MAX_PAR_FACTOR
      debug(s"Found counter with start=$start, end=$end, step=$step, par=$par")
      debug(s"  bound($start) = " + bound(start))
      debug(s"  bound($end) = " + bound(end))
      debug(s"  bound($step) = " + bound(step))

      // Set constraints on par factor
      (start,end,step) match {
        case (Exact(0),ParamFix(p),Exact(1)) =>
          val r1 = RLessEqual(par, p)
          val r2 = RDivides(par, p)
          debug(s"  Case #1: Adding restriction $r1")
          debug(s"  Case #2: Adding restriction $r2")
          restrict += r1
          restrict += r2

        case (_,ParamFix(e),ParamFix(p)) => // ???

        case (Exact(0),Bound(e),ParamFix(p)) =>
          val r = RDividesQuotient(par, e.toInt, p)
          debug(s"  Case #3: Adding restriction $r")
          restrict += r

        case (Bound(s),Bound(e),ParamFix(p)) =>
          val r = RDividesQuotient(par, (e-s).toInt, p)
          debug(s"  Case #4: Adding restriction $r")
          restrict += r

        case (Bound(s),Bound(e),Bound(t)) =>
          val nIters = (e - s)/t
          if (nIters < max) max = nIters.toInt
          val r = RDividesConst(par, nIters.toInt)  // HACK: par factor divides bounded loop size (avoid edge case)
          debug(s"  Case #5: Adding restriction $r")
          restrict += r

        case _ => // No restrictions
      }
      setRange(par, 1, max, 1)

      // Set constraints on step size
      (start,end,step) match {
        case (ParamFix(s),ParamFix(p),ParamFix(_)) => // ???

        case (Exact(0),ParamFix(p),ParamFix(s)) =>
          val r1 = RLessEqual(s, p)
          val r2 = RDivides(s, p)   // HACK: avoid edge case
          debug(s"  Case #6: Adding restriction $r1")
          debug(s"  Case #7: Adding restriction $r2")
          restrict += r1
          restrict += r2

        case (Bound(s),Bound(b),ParamFix(p)) =>
          val l = b - s
          setRange(p, 1, l.toInt, MIN_TILE_SIZE)
          val r = RDividesConst(p, l.toInt) // HACK: avoid edge case
          debug(s"  Case #8: Adding restriction $r")
          restrict += r

        case _ => // No restrictions
      }

    case BurstStore(mem,stream,ofs,len,p: Param[Int]) =>
      parParams ::= p

    case BurstLoad(mem,stream,ofs,len,p: Param[Int]) =>
      parParams ::= p

    // HACK: Parallelize innermost loop only
    case e:OpForeach =>
      val pars = getParams(parFactorsOf(e.cchain))
      parParams :::= pars
      if (!isParallelizableLoop(lhs)) pars.foreach{p => domainOf(p) = (1,1,1) }
      else if (!isInnerPipe(lhs)) pars.foreach{p => setMax(p, MAX_OUTER_PAR) }
      else pars.foreach{p => setMax(p, MAX_PAR_FACTOR) }

    case e:OpReduce[_,_] =>
      val pars = getParams(parFactorsOf(e.cchain))
      parParams :::= pars
      if (!isParallelizableLoop(lhs)) pars.foreach{p => domainOf(p) = (1,1,1) }
      else if (!isInnerPipe(lhs)) pars.foreach{p => setMax(p, MAX_OUTER_PAR) }
      else pars.foreach{p => setMax(p, MAX_PAR_FACTOR) }

    case e:OpMemReduce[_,_] =>
      val opars = getParams(parFactorsOf(e.ccOuter))
      val ipars = getParams(parFactorsOf(e.ccInner))
      parParams :::= opars
      parParams :::= ipars
      if (!isParallelizableLoop(lhs)) opars.foreach{p => domainOf(p) = (1,1,1) }
      else opars.foreach{p => setMax(p, MAX_OUTER_PAR) }

    case _ => super.traverse(lhs,rhs)
  }
}


package spatial.compiler.ops

import scala.virtualization.lms.common.{BaseExp, ScalaGenEffect, CGenEffect, MaxJGenEffect}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait SpatialCounter
trait SpatialCounterChain

trait ExternCounterTypesExp extends ExternCounterTypes with BaseExp {
  type Counter = SpatialCounter
  type CounterChain = SpatialCounterChain

  def counterManifest: Manifest[Counter] = manifest[SpatialCounter]
  def counterChainManifest: Manifest[CounterChain] = manifest[SpatialCounterChain]

  def isCounter[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[SpatialCounter])
  def isCounterChain[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[SpatialCounterChain])
}

trait ExternCounterOpsExp extends ExternCounterTypesExp with CounterOpsExp with CounterChainOpsExp with NodeMetadataOpsExp {
  this: SpatialExp =>


  // --- Nodes
  case class Counter_new(start: Rep[Idx], end: Rep[Idx], step: Rep[Idx], par: Param[Int])(implicit val ctx: SourceContext) extends Def[Counter]
  case class Counterchain_new(counters: List[Rep[Counter]])(implicit val ctx: SourceContext) extends Def[CounterChain]

  // --- Internal API
  def counter_new(start: Rep[Idx],end: Rep[Idx],step: Rep[Idx], par: Rep[Int])(implicit ctx: SourceContext) = {
    val truePar: Param[Int] = parize(par)
    reflectEffect[Counter](Counter_new(start,end,step,truePar)(ctx))
  }

  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    // HACK: Not actually mutable, but isn't being scheduled properly otherwise
    reflectMutable(Counterchain_new(counters)(ctx))
  }

  // --- Analysis tools
  def offsets(cc: Rep[CounterChain]) = cc match {
    case Deff(Counterchain_new(ctrs)) => ctrs.map{
      case Deff(Counter_new(start,_,_,par)) => start
    }
  }

  def ccMaxes(cc: Rep[CounterChain]) = cc match {
    case Deff(Counterchain_new(ctrs)) => ctrs.map{
      case Deff(Counter_new(start,end,_,_)) => end
    }
  }

  def isUnitCounterChain(e: Exp[Any]): Boolean = e match {
    case Deff(Counterchain_new(ctrs)) if ctrs.length == 1 => isUnitCounter(ctrs(0))
    case _ => false
  }

  def isUnitCounter(e: Exp[Any]): Boolean = e match {
    case Deff(Counter_new(ConstFix(0),ConstFix(1),ConstFix(1),_)) => true
    case _ => false
  }

  // TODO: Default number of iterations if bound can't be computed?
  // TODO: Warn user if bounds can't be found?
  def nIters(x: Rep[CounterChain], ignorePar: Boolean = false): Long = x match {
    case Deff(Counterchain_new(ctrs)) =>
      val loopIters = ctrs.map{ case Deff(Counter_new(start,end,stride,par)) =>
        val min = bound(start).getOrElse(0.0)
        val max = bound(end).getOrElse(1.0)
        val step = bound(stride).getOrElse(1.0)
        val p = bound(par).getOrElse(1.0)

        val nIters = Math.ceil(max - min/step)
        if (ignorePar)
          nIters.toLong
        else
          Math.ceil(nIters/p).toLong
      }
      loopIters.fold(1L){_*_}
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Counter_new(s,end,t,p) => reflectPure( Counter_new(f(s),f(end),f(t),p)(pos) )(mtype(manifest[A]), pos)
    case Reflect(e@Counter_new(s,end,t,p), u, es) => reflectMirrored(Reflect(Counter_new(f(s),f(end),f(t),p)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Counterchain_new(counters) => reflectPure(Counterchain_new(f(counters))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Counterchain_new(counters), u, es) => reflectMirrored(Reflect(Counterchain_new(f(counters))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }
}

trait ScalaGenExternCounterOps extends ScalaGenEffect {
  val IR: ExternCounterOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialCounter" => "FixedPointRange[Signed,B32,B0]"
    case "SpatialCounterChain" => "Array[FixedPointRange[Signed,B32,B0]]"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>
      stream.println("val "+quote(sym)+" = "+quote(start)+" until "+quote(end)+" by "+quote(step)+" par "+quote(par))

    case e@Counterchain_new(counters) =>
      emitValDef(sym, "Array(" + counters.map(quote).mkString(", ") + ")")

    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenExternCounterOps extends CGenEffect {
  val IR: ExternCounterOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialCounter" => "int32_t"
    case "SpatialCounterChain" => "int32_t*"
    case _ => super.remap(m)
  }
}

trait MaxJGenExternCounterOps extends MaxJGenEffect {
  val IR: ExternCounterOpsExp with SpatialMetadataOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialCounter" => "SpatialCounter"
    case "SpatialCounterChain" => "CounterChain"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>


    case e@Counterchain_new(counters) =>
      // See emitMaxJCounterChain() in PipeTemplateOpsExp.scala

    case _ => super.emitNode(sym,rhs)
  }
}

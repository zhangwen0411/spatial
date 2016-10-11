package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait RewriteTransformer extends SinglePassTransformer {
  val IR: SpatialExp
  import IR._

  override val name = "Rewrite Transformer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  object Mirrored {
    def unapply[T](x: Exp[T]) = Some(f(x).asInstanceOf[Exp[T]])
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Reg_write(Mirrored(reg), Mirrored(value), Mirrored(en))) => value match {
      // Keep the previous value if sel is true, otherwise write 'b'
      case Deff(Mux2(sel, Deff(Reg_read(`reg`)), b)) =>
        val lhs2 = reg_write(reg, b, en && !sel)
        val Def(rhs2) = lhs2
        debug(s"Rewrote $lhs = $rhs")
        debug(s"  to $lhs2 = $rhs2")

        setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
        Some(lhs2)

      // Keep the previous value if sel is false, otherwise write 'a'
      case Deff(Mux2(sel, a, Deff(Reg_read(`reg`)))) =>
        val lhs2 = reg_write(reg, a, en && sel)
        val Def(rhs2) = lhs2
        debug(s"Rewrote $lhs = $rhs")
        debug(s"  to $lhs2 = $rhs2")

        setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
        Some(lhs2)

      case _ => None
    }

    case _ => None
  }

  override def self_mirror[A](lhs: Sym[A], rhs: Def[A]): Exp[A] = {
    //debugs(s"Mirroring: $lhs = $rhs")
    //getProps(lhs).foreach{props => props.data.foreach{(k,m) => debugs(" -" + readable(k) + makeString(m)) }}

    val lhs2 = super.self_mirror(lhs, rhs)
    debug(s"$lhs -> $lhs2")
    //val rhs2 = lhs2 match {case Def(d) => d; case _ => null}
    //debugs(s"Created:   $lhs2 = $rhs2")
    //getProps(lhs2).foreach{props => props.data.foreach{(k,m) => debugs(" -" + readable(k) + makeString(m)) }}

    lhs2
  }

}
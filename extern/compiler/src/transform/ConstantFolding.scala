package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.collection.mutable.HashMap

// Replaces all fixed value statements with corresponding constant value
// Assumed to be either a fixed or floating point value
trait ConstantFolding extends SinglePassTransformer {
  val IR: SpatialExp
  import IR._

  override val name = "Constant Folding"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  def convertType[T:Manifest](x: Int)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Long)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Float)(implicit ctx: SourceContext) = x.as[T]
  def convertType[T:Manifest](x: Double)(implicit ctx: SourceContext) = x.as[T]

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = {
    self_mirror(lhs, rhs) match {
      case lhs2@Def(rhs2) => foldConstants(lhs2.asInstanceOf[Sym[A]],rhs2.asInstanceOf[Def[A]]) match {
        case Some(c) if c != lhs2 =>
          debug(s"Replacing $lhs = $rhs")
          c match {
            case Def(d) => debug(s"with $c = $d")
            case _ => debug(s"with $c")
          }
          scrubSym(lhs2.asInstanceOf[Sym[Any]]) // Remove mirrored version
          setProps(c, getProps(lhs2))
          Some(c)

        case _ => Some(lhs2)
      }
      case lhs2 => Some(lhs2)
    }
  }

  def foldConstants[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = lhs match {
    case Def(Reflect(_,_,_)) => None // Never replace effectful statements (for now)

    case Fixed(v) if isBits(lhs.tp) =>
      val c = if (v.toInt == v)         convertType(v.toInt)(lhs.tp, ctx)
              else if (v.toLong == v)   convertType(v.toLong)(lhs.tp, ctx)
              else if (v.toFloat == v)  convertType(v.toFloat)(lhs.tp, ctx)
              else                      convertType(v.toDouble)(lhs.tp, ctx)
      Some(c)

    case Deff(FixPt_Mul(a,Fixed(1))) => Some(a)
    case Deff(FixPt_Mul(Fixed(1),b)) => Some(b)
    case Deff(FixPt_Add(a,Fixed(0))) => Some(a)
    case Deff(FixPt_Add(Fixed(0),b)) => Some(b)
    case Deff(FixPt_Sub(a,Fixed(0))) => Some(a)
    case Deff(FixPt_Sub(Fixed(0),b)) => Some(b)
    case Deff(FixPt_Div(a,Fixed(1))) => Some(a)
    case n@Deff(FixPt_Mod(a,b)) => 
      (a,b) match {
        case (ConstFix(aa: Int), ConstFix(bb: Int)) =>
          Some(lift_to(aa % bb)(manifest[Int],lhs.tp,ctx,implicitly[Numeric[Int]]))
        case _ => 
          Some(n)
      }
    // Can't do 0/b, as b may be zero..

    case _ => None
  }
}

package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.shared.typeclass._

trait ExternPrimitiveTypes {
  // MUST be invariant (otherwise can get something like Fix[Any,Any,Any] which is not great)
  type FixPt[SIGN,INT,FRAC]
  type FltPt[SIG,EXP] // Sign bit is included in bits for significand
  type Bit // Left undefined here so we can use Boolean for emulation in library if needed

  def isFixPtType[T:Manifest]: Boolean
  def isFltPtType[T:Manifest]: Boolean
  def isBitType[T:Manifest]: Boolean
  def isTupleType[T:Manifest]: Boolean

  implicit def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]]
  implicit def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]]
  implicit def bitManifest: Manifest[Bit]
}

trait ExternPrimitiveOps extends ExternPrimitiveTypes with NumOps with OrderOps {
  this: Spatial =>

  def min2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext): Rep[T]
  def max2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext): Rep[T]
}
trait ExternPrimitiveCompilerOps extends ExternPrimitiveTypes with MemoryTemplateTypes {
  this: SpatialIdentifiers =>

  lazy val bx = "B([0-9]+)".r
  lazy val tupx = "Tupl?e?([0-9]+)".r
  object BXX {
    // HACK: Given Manifest, match using regex bx, return * (where * must be numeric)
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case bx(bits) => Some(bits.toInt)
      case _ => None
    }
  }
  object TupX {
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case tupx(n) => Some(n.toInt)
      case _ => None
    }
  }

  object FixPtType {
    def unapply[T](x: Manifest[T]): Option[(Manifest[_],Manifest[_],Manifest[_])] = x match {
      case mT if isFixPtType(x) => Some((x.typeArguments(0),x.typeArguments(1),x.typeArguments(2)))
      case _ => None
    }
  }
  object FltPtType {
    def unapply[T](x: Manifest[T]): Option[(Manifest[_], Manifest[_])] = x match {
      case mT if isFltPtType(x) => Some((x.typeArguments(0), x.typeArguments(1)))
      case _ => None
    }
  }
  object TupNType {
    def unapply[T](x: Manifest[T]): Option[(Int,List[Manifest[_]])] = x match {
      case TupX(n) => Some((n, x.typeArguments.take(n)))
      case _ => None
    }
  }

  def isTupleType[T:Manifest]: Boolean = manifest[T] match {case TupX(n) => true; case _ => false}

  def sign[T:Manifest]: Boolean = manifest[T] match {
    case mA if isFixPtType(mA) => sign(mA.typeArguments(0))
    case mA if isFltPtType(mA) => true
    case mA if mA == manifest[Signed] => true
    case mA if mA == manifest[Unsign] => false
    case mA => throw new Exception("Unknown type in signed test: " + mA.runtimeClass.getSimpleName)
  }
  def nbits[T:Manifest]: Int = manifest[T] match {
    case mA if isFixPtType(mA) => nbits(mA.typeArguments(1)) + nbits(mA.typeArguments(2))
    case mA if isFltPtType(mA) => nbits(mA.typeArguments(0)) + nbits(mA.typeArguments(1))
    case mA if isBitType(mA)   => 1
    case mA if isReg(mA)       => nbits(mA.typeArguments(0))
    case BXX(bits) => bits
    case mA => throw new Exception("Unknown type in nbits: " + mA.runtimeClass.getSimpleName)
  }

  def isStaticSize[T:Manifest](x: Rep[T]): Boolean
}

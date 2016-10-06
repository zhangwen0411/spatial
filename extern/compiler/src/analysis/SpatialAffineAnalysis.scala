package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.{AffineAnalysisExp, AffineAnalyzer}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait SpatialAffineAnalysisExp extends AffineAnalysisExp {
  this: SpatialExp =>

  def isIndexType(t: Manifest[_]) = isFixPtType(t) && sign(t) && nbits(t.typeArguments(1)) == 32 && nbits(t.typeArguments(2)) == 0

  // Pair of symbols for nodes used in address calculation addition nodes
  override def indexPlusUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Add(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]])) // annoying erasure here
    case _ => None
  }
  // Pair of symbols for nodes used in address calculation multiplication nodes
  override def indexTimesUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Mul(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]]))
    case _ => None
  }
  // List of loop scopes. Each scope contains a list of iterators and blocks to traverse for loop nodes
  override def loopUnapply(x: Exp[Any]): Option[List[(List[Sym[Index]], List[Block[Any]])]] = x match {
    case Deff(Pipe_foreach(cchain, func, inds)) =>
      Some( List(inds -> List(func)) )
    case Deff(Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV)) =>
      Some( List(inds -> List(iFunc,ld,st,func,rFunc)) )
    case Deff(Accum_fold(c1,c2,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV)) =>
      Some( List(inds1 -> List(func), (inds1 ++ inds2) -> List(iFunc,ld1,ld2,rFunc,st)) )
    case _ => None
  }
  // Memory being read + list of addresses (for N-D access)
  override def readUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Bram_load(bram,addr)) => Some((bram, accessIndicesOf(x)))
    case _ => None
  }
  // Memory being written + list of addresses (for N-D access)
  override def writeUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Bram_store(bram,addr,y)) => Some((bram, accessIndicesOf(x)))
    case _ => None
  }
  // Usually None, but allows other exceptions for symbols being loop invariant
  override def invariantUnapply(x: Exp[Index]): Option[Exp[Index]] = x match {
    case Exact(_) => Some(x)  // May not be constant yet but will be in future
    case _ => super.invariantUnapply(x)
  }
}

trait SpatialAffineAnalyzer extends AffineAnalyzer {
  val IR: SpatialAffineAnalysisExp with SpatialExp
  import IR._

  override val name = "Affine Analysis"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case EatReflect(e:Scatter[_]) =>
      accessPatternOf(lhs) = List(LinearAccess(e.i))
    case EatReflect(e:Gather[_]) =>
      accessPatternOf(lhs) = List(LinearAccess(e.i))

    // Pipe_fold - idx is loop invariant anyway

    case EatReflect(e: Accum_fold[_,_]) =>
      // Add access pattern for bound idx variable prior to traversal
      boundIndexPatterns += e.idx -> e.indsInner.map{i => LinearAccess(i)}
      super.traverse(lhs, rhs)

    case _ => super.traverse(lhs,rhs)
  }
}

package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.analysis.{AffineAnalysisExp, AffineAnalyzer}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait SpatialAffineAnalysisExp extends AffineAnalysisExp {
  this: SpatialExp =>

  // Pair of symbols for nodes used in address calculation addition nodes
  override def indexPlusUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Add(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]])) // annoying erasure here
    case Deff(FixPt_Sub(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]])) //
    case _ => None
  }
  // Pair of symbols for nodes used in address calculation multiplication nodes
  override def indexTimesUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])] = x match {
    case Deff(FixPt_Mul(a,b)) => Some((a.asInstanceOf[Exp[Index]],b.asInstanceOf[Exp[Index]]))
    case _ => None
  }
  // List of loop scopes. Each scope contains a list of iterators and blocks to traverse for loop nodes
  override def loopUnapply(x: Exp[Any]): Option[List[(List[Sym[Index]], List[Block[Any]])]] = x match {
    case Deff(OpForeach(cchain, func, inds)) =>
      Some( List(inds -> List(func)) )
    case Deff(OpReduce(cchain,accum,zero,fA,ld,st,func,rFunc,inds,acc,res,rV)) =>
      Some( List(inds -> List(ld,st,func,rFunc)) )
    case Deff(OpMemReduce(c1,c2,a,zero,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV)) =>
      Some( List(inds1 -> List(func), (inds1 ++ inds2) -> List(ld1,ld2,rFunc,st)) )
    case _ => None
  }
  // Memory being read + list of addresses (for N-D access)
  override def readUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Sram_load(sram,addr)) => Some((sram, accessIndicesOf(x)))
    case _ => None
  }
  // Memory being written + list of addresses (for N-D access)
  override def writeUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])] = x match {
    case Deff(Sram_store(sram,addr,y,en)) => Some((sram, accessIndicesOf(x)))
    case _ => None
  }
}

trait SpatialAffineAnalyzer extends AffineAnalyzer {
  val IR: SpatialAffineAnalysisExp with SpatialExp
  import IR._

  override val name = "Affine Analysis"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose


  override def invariantUnapply(x: Exp[Index]): Option[Exp[Index]] = x match {
    case Exact(_) => Some(x)  // May not be constant yet but will be in future
    case LocalReader(reads) =>
      debug(s"  Checking if index $x is loop invariant: ")
      debug(s"    reads: $reads")
      debug(s"  writes in scope:")
      loopScope.foreach{case TP(writer@LocalWriter(writes), _) => debug(s"  $writer: $writes"); case _ =>  }
      // A local read is loop invariant if none of the read memories
      // are written within this loop scope (outside is ok)
      val readMems = reads.map(_._1)
      val writerInScope = loopScope.exists{
        case TP(LocalWriter(writes), _) => writes.exists{write => readMems.contains(write._1) }
        case _ => false
      }
      debug(s"  writer-in-scope: $writerInScope")

      if (!writerInScope) Some(x) else None

    case _ => super.invariantUnapply(x)
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case EatReflect(e:Scatter[_]) =>
      accessPatternOf(lhs) = List(LinearAccess(e.i))
    case EatReflect(e:Gather[_]) =>
      accessPatternOf(lhs) = List(LinearAccess(e.i))

    case EatReflect(e:Convolve[_]) =>
      accessPatternOf(lhs) = e.inds.map(LinearAccess(_))
    case EatReflect(e:ConvLayer[_]) =>
      accessPatternOf(lhs) = e.inds.map(LinearAccess(_))

    case _ => super.traverse(lhs,rhs)
  }
}

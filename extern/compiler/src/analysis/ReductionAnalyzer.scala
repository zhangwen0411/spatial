package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

// TODO: Eventually this analyzer should be superceded by more generalized reduction code generation

trait ReductionAnalysisExp extends NodeMetadataOpsExp {
  this: SpatialExp =>

  sealed abstract class ReduceFunction
  case object FixPtSum extends ReduceFunction
  case object FltPtSum extends ReduceFunction
  case object FixPtMin extends ReduceFunction
  case object FixPtMax extends ReduceFunction
  case object OtherReduction extends ReduceFunction

  case class MReduceType(func: Option[ReduceFunction]) extends Metadata

  object reduceType {
    def update(e: Exp[Any], func: ReduceFunction) = setMetadata(e, MReduceType(Some(func)))
    def update(e: Exp[Any], func: Option[ReduceFunction]) = setMetadata(e, MReduceType(func))
    def apply(e: Exp[Any]) = meta[MReduceType](e).flatMap(_.func)
  }

  def identifyReduceFunc(rFunc: Block[Any], a: Exp[Any], b: Exp[Any]) = getBlockResult(rFunc) match {
    case Deff(FixPt_Add(`a`,`b`)) => FixPtSum
    case Deff(FltPt_Add(`a`,`b`)) => FltPtSum
    case Deff(Min2(`a`,`b`)) if isFixPtType(a.tp) => FixPtMin
    case Deff(Max2(`a`,`b`)) if isFixPtType(a.tp) => FixPtMax
    case _ =>
      Console.println(s"[WARNING] $rFunc on $a and $b does not match any reduce type!")
      OtherReduction
  }
}

trait ReductionAnalyzer extends Traversal with SpatialTraversalTools {
  val IR: SpatialExp with ReductionAnalysisExp
  import IR._

  override val name = "Reduction Analyzer"
  override val recurse = Always    // Always follow default traversal scheme
  override val eatReflect = true   // Ignore reflect wrappers
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case OpReduce(c,a,z,fA,ld,st,func,rFunc,inds,acc,res,rV) =>
      val funcType = identifyReduceFunc(rFunc, rV._1, rV._2)
      getStages(rFunc).foreach{sym => reduceType(sym) = funcType}
      getStages(ld).foreach{sym => reduceType(sym) = funcType}
      getStages(st).foreach{sym => reduceType(sym) = funcType}
      reduceType(a) = funcType
      reduceType(acc) = funcType
      reduceType(lhs) = funcType
      debug(s"pipefold acc $acc to $funcType in $lhs")
      debug(s"a $a to $funcType in $lhs")

    case OpMemReduce(c1,c2,a,z,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) =>
      val funcType = identifyReduceFunc(rFunc, rV._1, rV._2)
      getStages(rFunc).foreach{sym => reduceType(sym) = funcType}
      getStages(ld1).foreach{sym => reduceType(sym) = funcType}
      getStages(ld2).foreach{sym => reduceType(sym) = funcType}
      getStages(st).foreach{sym => reduceType(sym) = funcType}
      reduceType(a) = funcType
      reduceType(acc) = funcType
      reduceType(lhs) = funcType
      debug(s"accumfold acc $acc to $funcType in $lhs")
      debug(s"a $a to $funcType in $lhs")

    case _ => super.traverse(lhs, rhs)
  }
}

/*package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.collection.mutable.{HashSet,HashMap}

trait ReachingWritesAnalysisExp extends ControlSignalAnalysisExp {this: SpatialExp => }

trait ReachingWritesAnalyzer extends Traversal with SpatialTraversalTools {
  val IR: SpatialExp with ReachingWritesAnalysisExp
  import IR._

  override val name = "Reaching Writes"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose
  override val recurse = Always    // Always follow default traversal scheme
  override val eatReflect = true   // Ignore reflect wrappers

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    for ((s,p) <- metadata) { // TODO: Better way to clear stale data?
      if (meta[ReachingWrites](s).isDefined) reachingWrites(s) = Nil
    }
    super.preprocess(b)
  }

  def gatherAccesses(x: Exp[Any]) = {
    if (isOuterControl(x)) {
      val childAccesses = childrenOf(x).map(gatherAccesses)
    }
    else x match {
      case Unit_pipe(func) => getStages(func).filter{s => isReader(s) || isWriter(s) }
      case
    }
  }


  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {

    b
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case

    case _ => super.traverse(lhs, rhs)
  }

}*/
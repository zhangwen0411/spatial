package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

// ISSUE #2: Eventually this analyzer should be superceded by more generalized coarse controller dataflow

trait StageAnalysisExp extends NodeMetadataOpsExp {this: SpatialExp => }

trait StageAnalyzer extends Traversal with SpatialTraversalTools {
  val IR: SpatialExp with StageAnalysisExp
  import IR._

  override val name = "Stage Analyzer"
  override val recurse = Always    // Always follow default traversal scheme
  override val eatReflect = true   // Ignore reflect wrappers
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    // Accel block (equivalent to a Sequential unit pipe)
    case Hwblock(blk) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(blk)
      if (debugMode) list(stages)
      nStages(lhs) = stages.length

    // Parallel
    case ParallelPipe(func) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list(stages)
      nStages(lhs) = stages.length

    // MetaPipes / Sequentials
    case UnitPipe(func) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list(stages)
      nStages(lhs) = stages.length

    case OpForeach(_,func,_) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length

    case OpReduce(c,a,z,fA,ld,st,func,rFunc,inds,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(ld,func,rFunc,st)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case OpMemReduce(c1,c2,a,z,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) =>
      debug(s"$lhs = $rhs:")
      val stages = getControlNodes(func,rFunc)
      if (debugMode) list( stages )
      nStages(lhs) = stages.length + 1  // Account for implicit reduction pipe

    case _ => super.traverse(lhs, rhs)
  }
}

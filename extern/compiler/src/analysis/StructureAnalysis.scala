package spatial.compiler.ops

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.virtualization.lms.internal.Traversal
import scala.reflect.{Manifest,SourceContext}

trait StructureAnalysisExp extends NodeMetadataOpsExp {this: SpatialExp => }

trait StructureAnalyzer extends Traversal {
  val IR: SpatialExp with StructureAnalysisExp
  import IR._

  override val name = "Structure Analyzer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  var stage = 0

  override def traverseBlock[A](block: Block[A]) {
    val prevStage = stage
    stage = 0
    super.traverseBlock(block)
    stage = prevStage
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    if (isControlNode(lhs)) {
      debugs(s"$stage. $lhs = $rhs")
      stage += 1
    }

    // Recursive traversal
    lhs match {
      case Deff(Hwblock(blk))         => traverseBlock(blk)
      case Deff(Pipe_parallel(blk))   => traverseBlock(blk)
      case Deff(Unit_pipe(blk))       => traverseBlock(blk)
      case Deff(e:Pipe_foreach)       => traverseBlock(e.func)
      case Deff(e:ParPipeForeach)     => traverseBlock(e.func)
      case Deff(e:ParPipeReduce[_,_]) => traverseBlock(e.func)

      case Deff(e:Pipe_fold[_,_]) if isOuterControl(lhs) =>
        tab += 1
        debugs("0. [Map Stage]")
        traverseBlock(e.func)
        debugs("1. [Reduce Stage]")
        tab -= 1

      case Deff(e:Accum_fold[_,_]) =>
        tab += 1
        debugs("0. [Map Stage]")
        traverseBlock(e.func)
        debugs("1. [Reduce Stage]")
        tab -= 1

      case _ => super.traverse(lhs, rhs)
    }
  }
}
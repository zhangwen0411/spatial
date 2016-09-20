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

  var tab = 0
  var stage = 0
  var hasParent = false

  def debugs(x: => Any) = debug(".."*tab + x)

  override def traverseBlock[A](block: Block[A]) {
    val prevTab = 0
    val prevParent = hasParent
    val prevStage = stage
    tab += 1
    hasParent = true
    super.traverseBlock(block)
    tab = prevTab
    hasParent = prevParent
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
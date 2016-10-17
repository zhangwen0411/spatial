package spatial.compiler.ops

import ppl.delite.framework.analysis.QuickTraversal
import scala.collection.mutable.HashMap

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait ModelingTraversal extends QuickTraversal with SpatialTraversalTools {
  val IR: SpatialExp with NodeMetadataOpsExp with LatencyModel
  import IR._

  // --- State
  var inHwScope = false // In hardware scope
  var inReduce = false  // In tight reduction cycle (accumulator update)
  def latencyOf(e: Exp[Any]) = if (inHwScope) IR.latencyOf(e, inReduce) else 0L

  // TODO: Could optimize further with dynamic programming
  def latencyOfPipe(b: Block[Any]): Long = {
    val scope = getStages(b)
    var paths = HashMap[Exp[Any],Long]()
    //debug(s"Pipe latency $b:")

    def quickDFS(cur: Exp[Any]): Long = cur match {
      case Def(d) if scope.contains(cur) && !isGlobal(cur) =>
        //debug(s"Visit $cur in quickDFS")
        val deps = symDeps(d)
        if (deps.isEmpty) {
          stageWarn(s"$cur = $d has no dependencies but is not global")
          latencyOf(cur)
        }
        else {
          latencyOf(cur) + deps.map{e => paths.getOrElseUpdate(e,quickDFS(e))}.max
        }
      case _ => 0L
    }
    if (scope.isEmpty) 0L else scope.last match {
      case e@Def(d:Reify[_]) => symDeps(d).map{e => paths.getOrElseUpdate(e,quickDFS(e))}.max
      case e => quickDFS(e)
    }
  }
  def latencyOfCycle(b: Block[Any]): Long = {
    val outerReduce = inReduce
    inReduce = true
    val out = latencyOfPipe(b)
    inReduce = outerReduce
    out
  }

  // Not a true traversal. Should it be?
  def pipeDelays(b: Block[Any], oos: Map[Exp[Any],Long] = Map.empty): List[(Exp[Any],Long)] = {
    val scope = getStages(b).filterNot(s => isGlobal(s))
    var delays = HashMap[Exp[Any],Long]() ++ scope.map{node => node -> 0L}
    var paths  = HashMap[Exp[Any],Long]() ++ oos

    def fullDFS(cur: Exp[Any]): Long = cur match {
      case Def(d) if scope.contains(cur) =>
        val deps = symDeps(d) filter (scope contains _)

        if (!deps.isEmpty) {
          val dlys = deps.map{e => paths.getOrElseUpdate(e, fullDFS(e)) }
          val critical = dlys.max

          deps.zip(dlys).foreach{ case(dep, path) =>
            if (path < critical && (critical - path) > delays(dep))
              delays(dep) = critical - path
          }
          critical + latencyOf(cur)
        }
        else latencyOf(cur)

      case s => paths.getOrElse(s, 0L) // Get preset out of scope delay
                                       // Otherwise assume 0 offset
    }
    if (!scope.isEmpty) scope.last match {
      case Def(d: Reify[_]) =>
        val deps = symDeps(d) filter (scope contains _)
        for (e <- deps) {
          paths.getOrElseUpdate(e,fullDFS(e)) // Not yet visited by any other path
        }
        // No synchronization between paths - we're done when the longest is done

      case e => fullDFS(e)
    }
    delays.toList
  }

  // Reset state
  override def preprocess[A:Manifest](b: Block[A]) = {
    inHwScope = false
    inReduce = false
    super.preprocess(b)
  }
}


package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait BufferAnalysisExp extends ControlSignalAnalysisExp with MemoryAnalysisExp {this: SpatialExp => }

trait BufferAnalyzer extends Traversal with ControllerTools {
  val IR: SpatialExp with BufferAnalysisExp
  import IR._

  override val name = "Buffer Analyzer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  lazy val ctrlAnalyzer = new UnrolledControlSignalAnalyzer{val IR: BufferAnalyzer.this.IR.type = BufferAnalyzer.this.IR}

  def run(localMems: List[Exp[Any]]): Unit = localMems.foreach{mem =>
    val readers = readersOf(mem)
    val writers = writersOf(mem)
    val duplicates = duplicatesOf(mem)

    debug(s"Setting buffer controllers for $mem:")

    duplicates.zipWithIndex.foreach{case (dup, i) =>
      debug(s"  Duplicate #$i: $dup")
      val reads = readers.filter{read => instanceIndicesOf(read, mem).contains(i) }
      val writes = writers.filter{write => instanceIndicesOf(write, mem).contains(i) }
      val accesses = reads ++ writes
      debug("  accesses: " + accesses.mkString(", "))

      val (metapipe, _) = findMetapipe(mem, reads, writes)

      if (metapipe.isDefined && dup.depth > 1) {
        val ctrl = metapipe.get
        accesses.foreach{access =>
          val child = lca(access.controller, ctrl).get
          if (child == ctrl) {
            val swap = childContaining(ctrl, access)
            debug(s"    - PORT ACCESS $access [swap = $swap]")
            topControllerOf(access, mem, i) = swap
          }
          else debug(s"    - MUX ACCESS  $access [lca = $child]")
        }
      }

    }
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    ctrlAnalyzer.run(b)
    run(ctrlAnalyzer.localMems)
    b
  }
}

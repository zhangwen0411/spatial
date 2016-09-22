package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.SinglePassTransformer

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait RegisterCleanupTransformExp extends MemoryAnalysisExp {this: SpatialExp => }

trait RegisterCleanupTransformer extends SinglePassTransformer {
  val IR: SpatialExp with RegisterCleanupTransformExp
  import IR._

  override val name = "Register Cleanup"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Reg_read(reg)) =>
      val hasReader = readersOf(reg).exists{_.access == lhs}
      debug(s"Checking if $reg contains reader $lhs: $hasReader")
      debug(s"  (Readers = ${readersOf(reg)})")
      if (!hasReader) debug(s"REMOVING register read $lhs")
      if (!hasReader) Some(Const(0)) else None // Unused, so it shouldn't matter what this is.

    case _ => super.transform(lhs, rhs)
  }
}


package spatial.compiler.ops
import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.common.BaseExp
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait SpatialExceptionsOpsExp extends SpatialExceptionsCompilerOps with BaseExp {
  this: SpatialExp =>

  override def name(x: Rep[Any]): String = name(x.tp) + " " + nameOf(x).map(x => x + " ").getOrElse("") + s"($x)"

  override def context(x: Rep[Any]) = mpos(x.pos)
  override def repType(x: Rep[Any]) = name(x.tp)
}
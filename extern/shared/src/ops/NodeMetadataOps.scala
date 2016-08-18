package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait NodeMetadataTypes extends Base {
  def parOf(e: Rep[Any]): Int
}

trait NodeMetadataOps extends Base {this: Spatial => }
trait NodeMetadataCompilerOps extends NodeMetadataOps with NodeMetadataTypes {
  this: Spatial =>
}

package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait NodeMetadataTypes extends Base {
  type Controller = (Rep[Any], Boolean)
  type Access     = (Rep[Any], Controller)

  implicit class ControllerOps(x: Controller) {
    def node = x._1
    def inReduce = x._2
  }
  implicit class AccessOps(x: Access) {
    def access = x._1
    def controller = x._2
    def controlNode = x._2._1
    def inReduce = x._2._2
  }

  def parOf(e: Rep[Any]): Int
}

trait NodeMetadataOps extends Base {this: Spatial => }
trait NodeMetadataCompilerOps extends NodeMetadataOps with NodeMetadataTypes {
  this: Spatial =>
}

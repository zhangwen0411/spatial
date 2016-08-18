package spatial.library.classes
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.library._
import spatial.library.classes._


trait NodeMetadataWrapper {
  this: SpatialBase with SpatialClasses =>

  def parOf(e: Rep[Any]): Int = 1
}

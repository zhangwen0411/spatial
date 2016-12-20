package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

// No user-facing methods
trait UnrolledOps extends Base { this: Spatial => }
trait UnrolledCompilerOps extends UnrolledOps { this: Spatial => }

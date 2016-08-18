package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait LoweredPipeOps extends Base { this: Spatial => }
trait LoweredPipeCompilerOps extends LoweredPipeOps { this: Spatial => }

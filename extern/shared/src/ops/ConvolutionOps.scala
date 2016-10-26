package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait ConvolutionOps extends Base { this: Spatial =>
  def convolve[T:Manifest](image: Rep[DRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit]
  def convLayer[T:Manifest](image: Rep[SRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit]
}
trait ConvolutionCompilerOps extends ControllerOps { this: Spatial => }

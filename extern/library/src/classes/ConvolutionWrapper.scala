package spatial.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.library._
import spatial.library.classes._

trait ConvolutionWrapper {
  this: SpatialBase with SpatialClasses =>

  def convolve[T:Manifest](image: Rep[DRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    throw new Exception("convolution not supported in library")
  }

  def convLayer[T:Manifest](image: Rep[SRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    throw new Exception("convolution not supported in library")
  }
}

package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait MemoryTypes {
  type Reg[T]
  type CAM[K,V]
  type SRAM[T]
  type FIFO[T]
  type DRAM[T]
  type Cache[T]
  type Vector[T]

  type Indices
  type Pipeline

  def isReg[T:Manifest]: Boolean
  def isCAM[T:Manifest]: Boolean
  def isSRAM[T:Manifest]: Boolean
  def isFIFO[T:Manifest]: Boolean
  def isDRAM[T:Manifest]: Boolean
  def isCache[T:Manifest]: Boolean
  def isVector[T:Manifest]: Boolean

  def isIndices[T:Manifest]: Boolean
  def isPipeline[T:Manifest]: Boolean

  implicit def regManifest[T:Manifest]: Manifest[Reg[T]]
  implicit def camManifest[K:Manifest,V:Manifest]: Manifest[CAM[K,V]]
  implicit def sramManifest[T:Manifest]: Manifest[SRAM[T]]
  implicit def fifoManifest[T:Manifest]: Manifest[FIFO[T]]
  implicit def dramManifest[T:Manifest]: Manifest[DRAM[T]]
  implicit def cacheManifest[T:Manifest]: Manifest[Cache[T]]
  implicit def vectorManifest[T:Manifest]: Manifest[Vector[T]]

  implicit def indicesManifest: Manifest[Indices]
  implicit def pipelineManifest: Manifest[Pipeline]
}

trait MemoryOps extends MemoryTypes with Base { this: Spatial => }
trait MemoryCompilerOps extends MemoryOps {
  this: Spatial =>

  def vectorize[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]]

  def gather[T:Manifest](mem: Rep[DRAM[T]], local: Rep[SRAM[T]], addrs: Rep[SRAM[Index]], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def scatter[T:Manifest](mem: Rep[DRAM[T]], local: Rep[SRAM[T]], addrs: Rep[SRAM[Index]], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]

  def burst_load[T:Manifest](mem: Rep[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def burst_store[T:Manifest](mem: Rep[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

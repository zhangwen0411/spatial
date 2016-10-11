package spatial.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,RefinedManifest,SourceContext}
import scala.virtualization.lms.common.Record

import scala.collection.mutable.{HashMap, Queue}

import spatial.shared._
import spatial.shared.ops._
import spatial.library._
import spatial.library.classes._

trait MemoryWrapper extends ControllerWrapper with ExternPrimitiveWrapper {
  this: SpatialBase with SpatialClasses with SpatialExceptionsCompilerOps =>

  type Reg[T] = Array[T]
  type CAM[K,V] = HashMap[K,V]
  type SRAM[T] = Array[T]
  type FIFO[T] = Queue[T]
  type DRAM[T] = Array[T]
  type Cache[T] = Array[T]
  type Vector[T] = Array[T]

  type Indices = RecordImpl
  type Pipeline = Unit

  // Shouldn't use these methods in library - can't definitely tell which is which
  def isReg[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isCAM[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isSRAM[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isFIFO[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isDRAM[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isCache[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isVector[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])

  def isIndices[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])
  def isPipeline[T:Manifest]: Boolean = throw UnknownLibraryManifest(manifest[T])

  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Array[T]]
  def camManifest[K:Manifest,V:Manifest]: Manifest[CAM[K,V]] = manifest[HashMap[K,V]]
  def sramManifest[T:Manifest]: Manifest[SRAM[T]] = manifest[Array[T]]
  def fifoManifest[T:Manifest]: Manifest[FIFO[T]] = manifest[Queue[T]]
  def dramManifest[T:Manifest]: Manifest[DRAM[T]] = manifest[Array[T]]
  def cacheManifest[T:Manifest]: Manifest[Cache[T]] = manifest[Array[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[Array[T]]

  def indicesManifest: Manifest[Indices] = manifest[RecordImpl]
  def pipelineManifest: Manifest[Pipeline] = manifest[Unit]

  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = elems.toArray

  def gather[T:Manifest](mem: Rep[DRAM[T]],local: Rep[SRAM[T]],addrs: Rep[SRAM[FixPt[Signed,B32,B0]]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    for (i <- 0 until len.toInt) {
      if (i < local.length && i < addrs.length && addrs(i).toInt < mem.length) {
        local(i) = mem( addrs(i).toInt )
      }
    }
  }
  def scatter[T:Manifest](mem: Rep[DRAM[T]],local: Rep[SRAM[T]],addrs: Rep[SRAM[FixPt[Signed,B32,B0]]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    for (i <- 0 until len.toInt) {
      if (i < addrs.length && addrs(i).toInt < mem.length) {
        mem( addrs(i).toInt ) = local(i)
      }
    }
  }

  def burst_load[T:Manifest](mem: Rep[DRAM[T]],fifo: Rep[FIFO[T]],ofs: Rep[FixPt[Signed,B32,B0]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    for (i <- 0 until len.toInt) { if (i + ofs.toInt < mem.length) fifo.enqueue( mem(i + ofs.toInt) ) else fifo.enqueue(mem(0)) }
  }
  def burst_store[T:Manifest](mem: Rep[DRAM[T]],fifo: Rep[FIFO[T]],ofs: Rep[FixPt[Signed,B32,B0]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    for (i <- 0 until len.toInt) { if (i + ofs.toInt < mem.length) mem(i + ofs.toInt) = fifo.dequeue() }
  }

  def sram_load[T:Manifest](mem: Rep[SRAM[T]], addr: Rep[Vector[FixPt[Signed,B32,B0]]])(implicit ctx: SourceContext): Rep[T] = {
    val x = calcAddress(addr.toList, dimsOf(mem)).toInt
    if (x < mem.length) mem(x) else mem(0)
  }
  def sram_store[T:Manifest](mem: Rep[SRAM[T]], addr: Rep[Vector[FixPt[Signed,B32,B0]]], value: Rep[T], en: Rep[Bit])(implicit ctx: SourceContext): Rep[Unit] = {
    if (en) {
      val x = calcAddress(addr.toList, dimsOf(mem)).toInt
      if (x < mem.length) mem(x) = value
    }
  }

}

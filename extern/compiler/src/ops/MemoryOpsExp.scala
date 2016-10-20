package spatial.compiler.ops
import java.io.{File,FileWriter,PrintWriter}

import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, CGenEffect, DotGenEffect, MaxJGenEffect, MaxJGenFat, Record}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}
import scala.collection.mutable.Set

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait SpatialReg[T]
trait SpatialCAM[K,V]
trait SpatialSRAM[T]
trait SpatialFIFO[T]
trait SpatialDRAM[T]
trait SpatialCache[T]
trait SpatialVector[T]

trait SpatialPipeline
trait SpatialIndices

trait MemoryTypesExp extends MemoryTypes with BaseExp {

  type Reg[T]    = SpatialReg[T]
  type CAM[K,V]  = SpatialCAM[K,V]
  type SRAM[T]   = SpatialSRAM[T]
  type FIFO[T]   = SpatialFIFO[T]
  type DRAM[T]   = SpatialDRAM[T]
  type Cache[T]  = SpatialCache[T]
  type Vector[T] = SpatialVector[T]

  type Indices   = SpatialIndices
  type Pipeline  = SpatialPipeline

  def isReg[T:Manifest]      = isSubtype(manifest[T].runtimeClass, classOf[SpatialReg[_]])
  def isCAM[T:Manifest]      = isSubtype(manifest[T].runtimeClass, classOf[SpatialCAM[_,_]])
  def isSRAM[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[SpatialSRAM[_]])
  def isFIFO[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[SpatialFIFO[_]])
  def isDRAM[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[SpatialDRAM[_]])
  def isCache[T:Manifest]    = isSubtype(manifest[T].runtimeClass, classOf[SpatialCache[_]])
  def isVector[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[SpatialVector[_]])
  // def isTup2[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[Tup2[_,_]])

  def isIndices[T:Manifest]  = isSubtype(manifest[T].runtimeClass, classOf[SpatialIndices])
  def isPipeline[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[SpatialPipeline])

  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[SpatialReg[T]]
  def camManifest[K:Manifest,V:Manifest]: Manifest[CAM[K,V]] = manifest[SpatialCAM[K,V]]
  def sramManifest[T:Manifest]: Manifest[SRAM[T]] = manifest[SpatialSRAM[T]]
  def fifoManifest[T:Manifest]: Manifest[FIFO[T]] = manifest[SpatialFIFO[T]]
  def dramManifest[T:Manifest]: Manifest[DRAM[T]] = manifest[SpatialDRAM[T]]
  def cacheManifest[T:Manifest]: Manifest[Cache[T]] = manifest[SpatialCache[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[SpatialVector[T]]

  def indicesManifest: Manifest[Indices] = manifest[SpatialIndices]
  def pipelineManifest: Manifest[Pipeline] = manifest[SpatialPipeline]
}


trait MemoryOpsExp extends MemoryTypesExp with ExternPrimitiveOpsExp with SRAMOpsExp {
  this: SpatialExp =>

  def isTup2[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Tup2[_,_]])

  val stream_offset_guess = 20
  // --- Nodes
  case class ListVector[T](elems: List[Exp[T]])(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  case class Gather[T](mem: Exp[DRAM[T]],local: Exp[SRAM[T]],addrs: Exp[SRAM[FixPt[Signed,B32,B0]]],len: Exp[FixPt[Signed,B32,B0]],par: Exp[Int], i: Sym[FixPt[Signed,B32,B0]])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[Unit]
  case class Scatter[T](mem: Exp[DRAM[T]],local: Exp[SRAM[T]],addrs: Exp[SRAM[FixPt[Signed,B32,B0]]],len: Exp[FixPt[Signed,B32,B0]],par: Exp[Int], i: Sym[FixPt[Signed,B32,B0]])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[Unit]

  case class BurstLoad[T](mem: Exp[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[Unit]
  case class BurstStore[T](mem: Exp[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[Unit]

  case class Sram_load[T](mem: Exp[SRAM[T]], addr: Exp[Vector[Index]])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[T]
  case class Sram_store[T](mem: Exp[SRAM[T]], addr: Exp[Vector[Index]], value: Exp[T], en: Exp[Bit])(implicit val ctx: SourceContext, val mT: Manifest[T]) extends Def[Unit]

  // --- Internal API
  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = reflectEffect(ListVector(elems), Simple)

  def gather[T:Manifest](mem: Rep[DRAM[T]],local: Rep[SRAM[T]],addrs: Rep[SRAM[FixPt[Signed,B32,B0]]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    reflectWrite[Unit](local)(Gather[T](mem,local,addrs,len,par,fresh[FixPt[Signed,B32,B0]])(ctx, implicitly[Manifest[T]]))
  }
  def scatter[T:Manifest](mem: Rep[DRAM[T]],local: Rep[SRAM[T]],addrs: Rep[SRAM[FixPt[Signed,B32,B0]]],len: Rep[FixPt[Signed,B32,B0]],par: Rep[Int])(implicit ctx: SourceContext) = {
    reflectWrite[Unit](mem)(Scatter[T](mem,local,addrs,len,par,fresh[FixPt[Signed,B32,B0]])(ctx, implicitly[Manifest[T]]))
  }

  def burst_load[T:Manifest](mem: Rep[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = {
    reflectWrite[Unit](fifo)(BurstLoad[T](mem,fifo,ofs,len,par)(ctx, implicitly[Manifest[T]]))
  }

  def burst_store[T:Manifest](mem: Rep[DRAM[T]], fifo: Rep[FIFO[T]], ofs: Rep[Index], len: Rep[Index], par: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = {
    reflectWrite[Unit](mem)(BurstStore[T](mem,fifo,ofs,len,par)(ctx, implicitly[Manifest[T]]))
  }

  def sram_load[T:Manifest](mem: Rep[SRAM[T]], addr: Rep[Vector[Index]])(implicit ctx: SourceContext) = {
    reflectPure(Sram_load(mem, addr)(ctx, manifest[T]))
  }
  def sram_store[T:Manifest](mem: Rep[SRAM[T]], addr: Rep[Vector[Index]], value: Rep[T], en: Rep[Bit])(implicit ctx: SourceContext) = {
    reflectWrite(mem)(Sram_store(mem, addr, value, en)(ctx, manifest[T]))
  }

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ListVector(elems) => reflectPure(ListVector(f(elems))(e.mT, e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@ListVector(elems), u, es) => reflectMirrored(Reflect(ListVector(f(elems))(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@Gather(mem,local,addrs,len,p,i), u, es) => reflectMirrored(Reflect(Gather(f(mem),f(local),f(addrs),f(len),f(p),i)(e.ctx,e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@Scatter(mem,local,addrs,len,p,i), u, es) => reflectMirrored(Reflect(Scatter(f(mem),f(local),f(addrs),f(len),f(p),i)(e.ctx,e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@BurstLoad(mem,fifo,ofs,len,p), u, es) => reflectMirrored(Reflect(BurstLoad(f(mem),f(fifo),f(ofs),f(len),f(p))(e.ctx,e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@BurstStore(mem,fifo,ofs,len,p), u, es) => reflectMirrored(Reflect(BurstStore(f(mem),f(fifo),f(ofs),f(len),f(p))(e.ctx,e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@Sram_load(mem,addr) => sram_load(f(mem),f(addr))(e.mT, e.ctx)
    case Reflect(e@Sram_load(mem,addr), u, es) => reflectMirrored(Reflect(Sram_load(f(mem),f(addr))(e.ctx, e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case e@Sram_store(mem,addr,value,en) => sram_store(f(mem),f(addr),f(value),f(en))(e.mT, e.ctx)
    case Reflect(e@Sram_store(mem,addr,value,en), u, es) => reflectMirrored(Reflect(Sram_store(f(mem),f(addr),f(value),f(en))(e.ctx, e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Scatter(mem,local,addrs,len,p,i) => syms(mem) ::: syms(local) ::: syms(addrs) ::: syms(len) ::: syms(p)
    case Gather(mem,local,addrs,len,p,i) => syms(mem) ::: syms(local) ::: syms(addrs) ::: syms(len) ::: syms(p)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Scatter(mem,local,addrs,len,p,i) => readSyms(mem) ::: readSyms(local) ::: readSyms(addrs) ::: readSyms(len) ::: readSyms(p)
    case Gather(mem,local,addrs,len,p,i) => readSyms(mem) ::: readSyms(local) ::: readSyms(addrs) ::: readSyms(len) ::: readSyms(p)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Scatter(mem,local,addrs,len,p,i) => freqNormal(mem) ::: freqNormal(local) ::: freqNormal(addrs) ::: freqNormal(len) ::: freqNormal(p)
    case Gather(mem,local,addrs,len,p,i) => freqNormal(mem) ::: freqNormal(local) ::: freqNormal(addrs) ::: freqNormal(len) ::: freqNormal(p)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Scatter(mem,local,addrs,len,p,i) => List(i)
    case Gather(mem,local,addrs,len,p,i) => List(i)
    case _ => super.boundSyms(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e:Gather[_] => Nil
    case e:Scatter[_] => Nil
    case e:BurstLoad[_] => Nil
    case e:BurstStore[_] => Nil
    case e:Sram_load[_] => Nil
    case e:Sram_store[_] => Nil
    case _ => super.aliasSyms(e)
  }
}



// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryOps extends ScalaGenEffect {
  val IR: MemoryOpsExp with SpatialCodegenOps
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialReg"  => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialCAM"  => "scala.collection.mutable.HashMap[" + remap(m.typeArguments(0)) + ", " + remap(m.typeArguments(1)) + "]"
    case "SpatialSRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialFIFO" => "scala.collection.mutable.Queue[" + remap(m.typeArguments(0)) + "]"
    case "SpatialDRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialCache"  => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialVector" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialPipeline" => "Unit"
    case _ => super.remap(m)
  }

  def emitSramStrides(dims: List[Exp[FixPt[Signed,B32,B0]]]) = if (dims.nonEmpty) {
    stream.println(s"val stride${dims.length-1} = 1")
    if (dims.length > 1) {
      (dims.length-2 to 0).foreach{d =>
        stream.println(s"val stride$d = stride${d+1} * ${quote(dims(d+1))}.toInt")
      }
    }
  } else { (new Exception("empty dimensions")) printStackTrace }

  def emitSramAddress(flatAddr: String, addrVector: String, dims: List[Exp[FixPt[Signed,B32,B0]]]) = {
    val quotedIndices = List.tabulate(dims.length){d => s"$addrVector($d).toInt"}
    stream.println(s"val $flatAddr = {")
    emitSramStrides(dims)
    stream.println(quotedIndices.zipWithIndex.map{case (index,i) => s"$index * stride$i" }.mkString(" + "))
    stream.println("}")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListVector(elems) =>
      emitValDef(sym, "Array" + elems.map(quote).mkString("(", ",", ")"))

    case Scatter(mem,local,addrs,len,par,i) =>
      stream.println("val "+quote(sym)+" = {")
      stream.println("for ("+quote(i)+" <- 0 until "+quote(len)+".toInt) { if ("+quote(i)+" < "+quote(addrs)+".length && "+quote(addrs)+"("+quote(i)+").toInt < "+quote(mem)+".length) "+quote(mem)+"( "+quote(addrs)+"("+quote(i)+").toInt ) = "+quote(local)+"("+quote(i)+") }" + "")
      stream.println("}")

    case Gather(mem,local,addrs,len,par,i) =>
      stream.println("val "+quote(sym)+" = {")
      stream.println("for ("+quote(i)+" <- 0 until "+quote(len)+".toInt) { if ("+quote(i)+" < "+quote(local)+".length && "+quote(i)+" < "+quote(addrs)+".length && "+quote(addrs)+"("+quote(i)+").toInt < "+quote(mem)+".length) "+quote(local)+"("+quote(i)+") = "+quote(mem)+"( "+quote(addrs)+"("+quote(i)+").toInt ) }" + "")
      stream.println("}")

    case BurstLoad(mem,fifo,ofs,len,par) =>
      stream.println("val "+quote(sym)+" = {")
      stream.println("for (i <- 0 until "+quote(len)+".toInt) { if (i + "+quote(ofs)+".toInt < "+quote(mem)+".length) "+quote(fifo)+".enqueue( "+quote(mem)+"(i + "+quote(ofs)+".toInt) ) else "+quote(fifo)+".enqueue("+quote(mem)+"(0)) }" + "")
      stream.println("}")

    case BurstStore(mem,fifo,ofs,len,par) =>
      stream.println("val "+quote(sym)+" = {")
      stream.println("for (i <- 0 until "+quote(len)+".toInt) { if (i + "+quote(ofs)+".toInt < "+quote(mem)+".length) "+quote(mem)+"(i + "+quote(ofs)+".toInt) = "+quote(fifo)+".dequeue() }" + "")
      stream.println("}")

    case Sram_load(mem@EatAlias(sram), addr) =>
      if (dimsOf(sram).isEmpty) sys.error(s"$sym : $sram has no dimensions!")
      stream.println(s"val ${quote(sym)} = {")
      emitSramAddress("addr", quote(addr), dimsOf(sram))
      stream.println(s"if (addr < ${quote(mem)}.length) ${quote(mem)}(addr) else ${quote(mem)}(0)")
      stream.println("}")

    case Sram_store(mem@EatAlias(sram), addr, value, en) =>
      if (dimsOf(sram).isEmpty) sys.error(s"$sym : $sram has no dimensions!")
      stream.println(s"val ${quote(sym)} = {")
      emitSramAddress("addr", quote(addr), dimsOf(sram))
      stream.println(s"if (${quote(en)} && addr < ${quote(mem)}.length) ${quote(mem)}(addr) = ${quote(value)}")
      stream.println("()")
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMemoryOps extends CGenEffect {
  val IR: ControllerOpsExp with SpatialIdentifiers with DRAMOpsExp
  with NosynthOpsExp
  import IR._

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each DRAM memory a 384MB chunk now
  val burstSize = 384
  var nextLMemAddr: Long = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr
  }

//  private def bitsToStringInt(bits: Int): String = {
//    if (bits <= 8) "8"
//    else if (bits <= 16) "16"
//    else if (bits <= 32) "32"
//    else "64"
//  }

//  private def bitsToFloatType(bits: Int) = {
//    if (bits <= 32) "float"
//    else "double"
//  }


  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialReg" => remapWithRef(m.typeArguments(0))
    // case "SpatialCAM" => ???
    case "SpatialSRAM" => remapWithRef(m.typeArguments(0))
    // case "SpatialFIFO" => ???
    case "SpatialDRAM"   => "maxjLmem"
    case "SpatialVector" => remapWithRef(m.typeArguments(0))
    case "SpatialPipeline" => "void"
    case _ => super.remap(m)
  }


  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + "maxjLmem.h")
    stream.println(
"""
#ifndef __MAXJLMEM_H__
#define __MAXJLMEM_H__
#include <stdint.h>

class maxjLmem {
public:
  uint64_t baseAddr;
  uint32_t size;

  maxjLmem(uint64_t base, int size) {
    this->baseAddr = base;
    this->size = size;
  }
};
#endif
""")
    stream.close()

    typesStream.println(s"""#include "maxjLmem.h" """)
    typesStream.println(s"""#include <Top.h>""")
    typesStream.println(s"""extern max_engine_t *engine;""")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Set_mem(fpgamem, cpumem) =>
      stream.println(s"""
      // Transfer DRAM -> LMEM
      Top_writeLMem_actions_t ${quote(fpgamem)}_wrAct;
      ${quote(fpgamem)}_wrAct.param_size = ${quote(cpumem)}->length * sizeof(${remap(fpgamem.tp.typeArguments.head)});
      ${quote(fpgamem)}_wrAct.param_start = ${quote(fpgamem)}->baseAddr;
      ${quote(fpgamem)}_wrAct.instream_fromcpu = (const uint8_t*) ${quote(cpumem)}->data;
      Top_writeLMem_run(engine, &${quote(fpgamem)}_wrAct);""")
    case Get_mem(fpgamem, cpumem) =>
      stream.println(s"""
      // Transfer LMEM -> DRAM
      // (sizeInBytes, address, dstptr)
      Top_readLMem_actions_t ${quote(fpgamem)}_rdAct;
      ${quote(fpgamem)}_rdAct.param_size = ${quote(cpumem)}->length *sizeof(${remap(fpgamem.tp.typeArguments.head)});
      ${quote(fpgamem)}_rdAct.param_start = ${quote(fpgamem)}->baseAddr;
      ${quote(fpgamem)}_rdAct.outstream_tocpu = (uint8_t*) ${quote(cpumem)}->data;
      fprintf(stderr, "Starting FPGA -> CPU copy\\n");
      Top_readLMem_run(engine, &${quote(fpgamem)}_rdAct);
      fprintf(stderr, "FPGA -> CPU copy done\\n");""")
    case Dram_new(size) =>
      emitValDef(sym, s"""new maxjLmem(${getNextLMemAddr()}, ${quote(size)})""")

    case Forloop(start, end, step, body, idx) =>
      val itp = remap(start.tp)
      stream.println(s"""for(${quote(itp)} i=${quote(start)}; i<${quote(end)}; i+=${quote(step)}) {""")
      stream.println(s"""${quote(itp)} ${quote(idx)} = i;""")
      emitBlock(body)
      stream.println(s"""}""")


    case _ => super.emitNode(sym, rhs)
  }
}


trait MaxJGenMemoryOps extends MaxJGenExternPrimitiveOps with MaxJGenFat with MaxJGenControllerOps {
  val IR: UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform
          with UnrolledOpsExp with ControllerOpsExp with ExternPrimitiveOpsExp with ReductionAnalysisExp

  import IR.{println => _, assert => _, infix_until => _, _}

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialVector" => "DFEVector<DFEVar>"
    case "Tuple2"  => "Something[" + remap(m.typeArguments(0)) + ", " + remap(m.typeArguments(1)) + "]"
    case _ => super.remap(m)
  }

  override def consumesMemFifo(node: Exp[Any]) = {
    // Console.println(s"on fifo $node")
    parentOf(node) match {
      case Some(parent) =>
        val isMem = childrenOf(parent).map { n =>
          val Deff(nn) = n
          // Console.println(s"  parent $parent, $n - $nn")
          n match {
            case Deff(BurstLoad(mem, fifo, ofs, len, par)) => if (quote(fifo) == quote(node)) true else false
            case Deff(BurstStore(mem, fifo, ofs, len, par)) => if (quote(fifo) == quote(node)) true else false
            case _ => false
          }}
      isMem.reduce{_|_}
      case None => false
    }
  }

  def getTrashBool(node: Exp[Any]) = {
    val ctr = node match {
      case Deff(d:OpForeach) => d.cchain
      case Deff(d:UnrolledForeach) =>
        val Deff(Counterchain_new(counters)) = d.cc
        counters(0)
      case Deff(d) => throw new Exception(s"Unknown tileld consumer $d from $node!")
    }
    val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
    val c = bound(end).get.toInt
    if (parOf(ctr) == 1) {
      s"${quote(ctr)} < ${quote(end)}"
    } else {
      s"${quote(ctr)}[${parOf(ctr)-1}] < ${quote(end)}"
    }
  }

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each DRAM memory a 384MB chunk now
  val burstSize = 384
  var nextLMemAddr: Long = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr/burstSize
  }

  var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
    emittedSize = Set.empty[Exp[Any]]
    nextLMemAddr = burstSize * 1024 * 1024
    super.initializeGenerator(buildDir)
  }

  val srams = Set[Exp[SRAM[Any]]]()
  val regs = Set[Exp[Reg[Any]]]()
  val connectedArgs = Set[Exp[Any]]()

  def emitBufferControlSignals() {
    withStream(baseStream) {
      emit(s"""{
// rdone signals for N-Buffers go here, eventually put them in their own class extending this one""")

      // Quote duplicate
      // TODO: Change to common "quote duplicate" method?
      def quoteDuplicate(mem: Exp[Any], i: Int): String = {
        if      (isSRAM(mem.tp)) s"""${quote(mem)}_${i}"""
        else if (isReg(mem.tp))  s"""${quote(mem)}_${i}_lib"""
        else throw new Exception("Cannot double buffer type " + mem.tp)
      }

      val nonBoundMemories = (srams ++ regs).map(aliasOf(_)).filter{case Def(_) => true; case _ => false}

      nonBoundMemories.foreach{mem =>
        val buffers = duplicatesOf(mem).zipWithIndex.filter{case (d,i) => d.depth > 1}
        val readers = readersOf(mem)
        val writers = writersOf(mem)

        buffers.foreach{ case (d, i) =>
          // Note: Grouping by sets of integers here. Accesses to a buffer should either have one port or all ports, so this is ok
          val readsByPort = readers.filter{reader => instanceIndicesOf(reader, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }
          val writesByPort = writers.filter{writer => instanceIndicesOf(writer, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }

          if (readsByPort.isEmpty || writesByPort.isEmpty) throw EmptyDuplicateException(mem, i)
          // Get all siblings of read/write ports and match to ports of buf
          val topCtrl = readsByPort.map{case (_, readers) => readers.flatMap{a => topControllerOf(a,mem,i)}.head}.head.node
          val subReads = readsByPort.map{case (_,r) => r.flatMap{ a => topControllerOf(a, mem, i)}}.filter{case l => l.length > 0}.map{case all => all.head.node}
          val subWrites = writesByPort.map{case (_,r) => r.flatMap{ a => topControllerOf(a, mem, i)}}.filter{case l => l.length > 0}.map{case all => all.head.node}

          val prnt = parentOf(topCtrl).get
          val allSiblings = childrenOf(prnt) //-- List(prnt).map{ case (c,_) => c}.toSet

          def emitPortConnections(ports: scala.collection.immutable.Set[Int], accesses: List[Access], connect: String, comment: String = "") {
            val controllers = accesses.flatMap{access => topControllerOf(access, mem, i) }.distinct
            val isBuffered = ports.size == 1 && accesses.nonEmpty

            if (isBuffered && controllers.length > 1)
              throw MultipleSwapControllersException(mem, i, accesses, ports.head)
            else if (isBuffered && controllers.isEmpty)
              throw UndefinedSwapControllerException(mem, i, accesses, ports.head)
            else if (isBuffered) {
              val portlist = ports.mkString{","} // TODO: Can probably use ports.head here
              emit(s"""${quoteDuplicate(mem, i)}.${connect}(${quote(controllers.head.node)}_done, ${quote(controllers.head.node)}_en, new int[] { $portlist }); /*$comment*/""")
            }
          }
          if (d.depth > 1) { // Deprecated dblbuf
            val suff = if (isSRAM(mem.tp)) {""} else if (isReg(mem.tp)) {"_lib"}
            val wPorts = writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length == 1 }.flatten
            val broadcastPorts = writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length > 1 }
            val rPorts = readsByPort.map{case (ports, writers) => ports.toList.map{a => a}}.flatten
            val fullPorts = (0 until d.depth).map{ i => i }.toSet
            val dummyWPorts = fullPorts -- wPorts
            val dummyRPorts = fullPorts -- rPorts
            val firstActivePort = math.min(allSiblings.indexOf(subReads.head), allSiblings.indexOf(subWrites.head))
            val dummyDonePorts = fullPorts -- wPorts -- rPorts
            val dummyDoneCtrlIds = dummyDonePorts.map{ a => a + firstActivePort }
            readsByPort.foreach{case (ports, readers) => emitPortConnections(ports, readers, "connectStageCtrl",s"read")}
            writesByPort.foreach{case (ports, writers) => emitPortConnections(ports, writers, "connectStageCtrl","write") }
            dummyDonePorts.foreach{ port =>
              val ctrlId = port + firstActivePort
              val node = allSiblings(ctrlId)
              emit(s"""${quoteDuplicate(mem,i)}.connectStageCtrl(${quote(node)}_done, ${quote(node)}_en, new int[] {$port}); /*orphan, connecting port ${port} + ${firstActivePort}*/""")
            }

            emit(s"""${quote(mem)}_${i}${suff}.connectUnwrittenPorts(new int[] {${dummyWPorts.mkString(",")}});""")
            emit(s"""${quote(mem)}_${i}${suff}.connectUnreadPorts(new int[] {${dummyRPorts.mkString(",")}});""")
            // emit(s"""${quote(mem)}_${i}${suff}.connectUntouchedPorts(new int[] {}); //new int[] {${dummyDonePorts.mkString(",")}});""")
            if (writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length > 1 }.toList.length == 0) {
              emit(s"""${quote(mem)}_${i}${suff}.connectDummyBroadcast();""")
            }
          } else {
            readsByPort.foreach{case (ports, readers) => emitPortConnections(ports, readers, "connectRdone") }
            writesByPort.foreach{case (ports, writers) => emitPortConnections(ports, writers, "connectWdone") }
          }
        }
      }
      emit(s"}")
    }
  }

  override def emitFileFooter() = {
    emitBufferControlSignals()
    withStream(baseStream) {
      emit(s"""// Emit argin reads""")
      emitted_argins.toList.foreach {
        case (sym, regStr) =>
          emit(s"""${maxJPre(sym)} ${quote(sym)} = $regStr; // reg read""")
      }
    }
    super.emitFileFooter()
  }

  def getBanking(sram: MemInstance) = {
    val bnks = sram.banking.map(_.banks)
    sram.banking.length match {
      case 1 => bnks(0)
      case 2 => bnks.mkString("new int[] {", ",", "}")
      case _ => throw new Exception(s"Can't handle ${sram.banking.length}-D memory!")
    }
  }
  def getStride(sram: MemInstance) = {
    val strds = sram.banking.map{
      case DiagonalBanking(strides, _) => throw new Exception(s"Can't handle Diagonal banking yet")
      case StridedBanking(stride, _) => stride
      case _ => 1
    }
    sram.banking.length match {
      case 1 => strds(0)
      case 2 => strds.mkString("new int[] {", ",", "}")
      case _ => throw new Exception(s"Can't handle ${sram.banking.length}-D memory!")
    }

  }

  def isBoundSym(x: Sym[Any]) = x match {
    case Def(_) => false // Is not a bound sym
    case _ => true
  }

  def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
  def row_col_indices(inds: List[List[Exp[Any]]], offsetPre: String, offsetPost: String) = {
    if (inds.map{ind => quote2D(ind, 1)}.distinct.length == 1) {
      val addr0 = inds.map{ind => quote2D(ind,0) }
      val addr1 = quote2D(inds(0), 1)
      emit(s"""// All accesses share column. vectorized """)
      s"new DFEVectorType<DFEVar>(dfeInt(32), ${inds.length}).newInstance(this, Arrays.asList(" + addr0.map{a => offsetPre + a + offsetPost}.mkString(",") + s")), " + offsetPre + addr1 + offsetPost
    } else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
      val addr0 = quote2D(inds(0), 0)
      val addr1 = inds.map{ind => quote2D(ind, 1) }
      emit(s"""// All accesses share row. vectorized""")
      offsetPre + addr0 + offsetPost + s", new DFEVectorType<DFEVar>(dfeInt(32), ${inds.length}).newInstance(this, Arrays.asList(" + addr1.map{a => offsetPre + a + offsetPost}.mkString(",") + s"))"
    } else {
      throw new Exception("Cannot handle this parallel reader because not exclusively row-wise or column-wise access!")
      "rekt"
    }
  }

  def sramLoad(read: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], par: Boolean = false) {
    emitComment("Sram_load {")
    val dups = duplicatesOf(sram)
    val readers = readersOf(sram)
    val reader = readers.find{_.node == read}.get   // Corresponding reader for this read node
    val b_i = instanceIndicesOf(reader, sram).head    // Instance indices should have exactly one index for reads
    val p = portsOf(read, sram, b_i).head
    val readCtrl = reader.controlNode

    val sram_name = s"${quote(sram)}_${b_i}"
    val pre = if (!par) maxJPre(sram) else "DFEVector<DFEVar>"
    val num_dims = dimsOf(sram).length
    val inds = parIndicesOf(read)
    var rdPre = ""
    var rdPost = ""
    val match_tuple = (inds.length, num_dims, par)
    var addrString = ""

    val portInfo = if (!isDummy(sram)) s", new int[] {$p}" else ""

    match_tuple match { //(inds_length, num_dims, par)
      case (1,1,false) =>
        emit(s"// This could be part of indirect addressing, so delay it for BFS")
        rdPre = s"stream.offset(${quote(sram_name)}.connectRport("
        rdPost = s"),-${quote(readCtrl)}_offset)"
        addrString = quote(inds(0)(0))
        addEmittedConsts(inds(0)(0))
      case (1,1,true) =>
        rdPre = s"new DFEVectorType<DFEVar>(${sram_name}.type, ${inds.length}).newInstance(this, Arrays.asList(${quote(sram_name)}.connectRport("
        rdPost = ")))"
        addrString = quote(inds(0)(0))
        addEmittedConsts(inds(0)(0))
      case (_,1,false) => // Is this one even possible?
        rdPre = s"${quote(sram_name)}.connectRport("
        rdPost = ")"
        addrString = quote(addr)
      case (_,1,true) =>
        rdPre = s"${quote(sram_name)}.connectRport("
        rdPost = ")"
        addrString = quote(addr)
      case (1,2,false) =>
        rdPre = s"${quote(sram_name)}.connectRport("
        rdPost = ")"
        addrString = quote(inds(0)(0)) + ", " + quote(inds(0)(1))
        addEmittedConsts(inds(0)(0), inds(0)(1))
      case (1,2,true) =>
        rdPre = s"new DFEVectorType<DFEVar>(${sram_name}.type, ${inds.length}).newInstance(this, Arrays.asList(${quote(sram_name)}.connectRport("
        rdPost = ")))"
        addrString = quote(inds(0)(0)) + ", " + quote(inds(0)(1))
        addEmittedConsts(inds(0)(0), inds(0)(1))
      case (_,2,_) =>
        rdPre = s"${quote(sram_name)}.connectRport("
        rdPost = ")"
        addrString = row_col_indices(inds,"","")
      case (_,_,_) =>
        throw new Exception(s"No codegen rule for this kind of read! $match_tuple")
    }

    val dummyOverride = if (isDummy(sram)) {bankOverride(read) match {
        case -1 => 
          rdPre = s"${quote(sram_name)}.connectRport("
          rdPost = ")"
          addrString = quote(addr)
          ""
        case b => 
          rdPre = s"${quote(sram_name)}.connectRport("
          rdPost = ")"
          addrString = quote(addr)  
          s", $b"
      }} else {""}

    emit(s"""$pre ${quote(read)} = ${rdPre}${addrString}${portInfo}${dummyOverride}${rdPost}; // matched $match_tuple ${nameOf(sram).getOrElse("")}""")
    emit(s"""// debug.simPrintf(<insert enable>, "read ${nameOf(sram).getOrElse("")}-${quote(sram_name)} %d @ %d", ${quote(read)}, ${addrString});""")
    // Handle if loading a composite type
    //n.compositeValues.zipWithIndex.map { t =>
    //  val v = t._1
    //  val idx = t._2
    //  visitNode(v)
    //  emit(s"""${quote(v)} <== ${quote(read)}[$idx];""")
    //}
    emitComment("} Sram_load")


  }

  def sramStore(write: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], value: Exp[Any], ens: Exp[Any]) {
    emitComment("Sram_store {")
    val dataStr = quote(value)
    val allDups = duplicatesOf(sram)

    val writers = writersOf(sram)
    val writer = writers.find(_.node == write).get
    //val EatAlias(ww) = write -- This is unnecessary (can't be bound)
    val distinctParents = writers.map{writer => parentOf(writer.controlNode)}.distinct
    val allParents = writers.map{writer => parentOf(writer.controlNode)}
    if (distinctParents.length < allParents.length) {
      Console.println("[WARNING] Bram $sram has multiple writers controlled by the same controller, which should only happen in CharBramTest!")
      // throw MultipleWriteControllersException(sram, writersOf(sram))
    }
    val writeCtrl = writer.controlNode

    // Figure out if this Ctrl is an accumulation, since we need to do this now that there can be many writers
    val isAccumCtrl = writeCtrl match {
        case Deff(d:OpReduce[_,_]) => true
        case Deff(d:OpForeach) => false
        case Deff(d:UnrolledReduce[_,_]) => true
        case Deff(d:UnrolledForeach) =>
          if (childrenOf(parentOf(writeCtrl).get).indexOf(writeCtrl) == childrenOf(parentOf(writeCtrl).get).length-1) {
            styleOf(writeCtrl) match {
              case InnerPipe => true
              case _ => false
            }
          } else {
            false
          }
        case Deff(d:UnitPipe) => true // Not sure why but this makes matmult work
        case p => throw UnknownParentControllerException(sram, write, writeCtrl)
    }

    val isAlwaysEn = ens match { // Hack to turn off damn stream offset if wr always enabled. MaxJ sucks
      case Deff(ConstBit(_)) => true
      case _ => false
    }

    val dups = allDups.zipWithIndex.filter{dup => instanceIndicesOf(writer,sram).contains(dup._2) }

    val inds = parIndicesOf(write)
    val num_dims = dimsOf(sram).length
    val match_tuple = (writers.length, distinctParents.length, inds.length, num_dims)
    if (inds.isEmpty) throw NoParIndicesException(sram, write)

    emit(s"// BTW, always add offset b/c it doesn't matter anyway")
    var offsetPre = if (isAccumCtrl & !isAlwaysEn) "stream.offset(" else ""
    var offsetPost = if (isAccumCtrl & !isAlwaysEn) {", -" + quote(writeCtrl) + "_offset)"} else "" 
    val parentPipe = parentOf(sram).getOrElse(throw UndefinedParentException(sram))
    var accEn = s"${quote(ens)}"//s"${quote(writeCtrl)}_datapath_en"
    var globalEnComma = ""
    val globalEn = if (isAccum(sram) & isAccumCtrl) {
      offsetPre = "stream.offset("
      offsetPost = ", -" + quote(writeCtrl) + "_offset)"
      globalEnComma = ", "
      writeCtrl match {
        case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
        case Deff(a) => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*wtf pipe is $a*/"
        case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*no def node*/"
      }
    } else {
      globalEnComma = ", "
      s"${quote(writeCtrl)}_datapath_en /*old behavior mask*/"
    }

    var addrString = ""
    val addrDbg = if (num_dims > 1) "%d %d" else "%d"
    var wrType = "connectWport("

    // TODO: Matches can probably be simplified
    dups.foreach {case (dd, ii) =>
      val p = portsOf(write, sram, ii).mkString(",")
      match_tuple match { // (writers_length, distinctParents_length, inds_length(wpar), sram_dim)
        case (1,1,_,1) =>
          addrString = offsetPre + quote(addr) + offsetPost
        case (1,1,1,2) =>
          addrString = offsetPre + quote(inds(0)(0)) + offsetPost + ", " + offsetPre + quote(inds(0)(1)) + offsetPost
        case (_,1,1,1) => // Hardcode banks to writers and hope for best
          offsetPre = "";offsetPost = "" // Turn off offset b/c it makes BramTest work....
          val bank_num = writersOf(sram).map{_.node}.indexOf(write)
          wrType = s"connectBankWport($bank_num, "
          addrString = offsetPre + quote(addr) + offsetPost
        case (_,_,1,1) => // distinctParents > 1, so writers_length must be > 1
          wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport("} else {"connectWport("}
          addrString = offsetPre + quote(addr) + offsetPost
        case (_,1,_,1) =>
          offsetPre = "";offsetPost = "" // Turn off offset b/c it makes BramTest work....
          val bank_num = writersOf(sram).map{_.node}.indexOf(write)
          wrType = s"connectBankWport($bank_num, "
          addrString = offsetPre + quote(addr) + offsetPost
        case (_,1,1,2) =>
          offsetPre = "";offsetPost = "" // Turn off offset b/c it makes BramTest work....
          val bank_num = writersOf(sram).map{_.node}.indexOf(write)
          wrType = s"connectBankWport($bank_num, "
          addrString = offsetPre + quote(inds(0)(0)) + offsetPost + ", " + offsetPre + quote(inds(0)(1)) + offsetPost
        case (1,_,1,2) =>
          throw new Exception(s"Cannot have only one writer ${writers} but multiple distinct writers ${distinctParents}!")
        case (1,1,_,2) =>
          addrString = row_col_indices(inds, offsetPre, offsetPost)
        case (_,_,_,1) =>
          wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport("} else {"connectWport("}
          addrString = offsetPre + quote(addr) + offsetPost
        case (_,_,1,2) =>
          wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport("} else {"connectWport("}
          addrString = offsetPre + quote(inds(0)(0)) + offsetPost + ", " + offsetPre + quote(inds(0)(1)) + offsetPost
        case (_,1,_,2) =>
          offsetPre = "";offsetPost = "" // Turn off offset b/c it makes BramTest work....
          val bank_num = writersOf(sram).map{_.node}.indexOf(write)
          wrType = s"connectBankWport($bank_num, "
          addrString = row_col_indices(inds, offsetPre, offsetPost)
        case (_,_,_,2) =>
          addrString = row_col_indices(inds, offsetPre, offsetPost)
          wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport("} else {"connectWport("}
        case (_,_,_,_) =>
          throw new Exception("MaxJ generation of more than 2D sRAMs is currently unsupported.")
      }
      val dataString = offsetPre + dataStr + offsetPost
      val accString = offsetPre + accEn + offsetPost
      val globalEnString = globalEnComma + offsetPre + globalEn + offsetPost
      if (isDummy(sram)) {
        addrString = quote(addr)} // Dummy override for char test
      emit(s"""${quote(sram)}_${ii}.${wrType}${addrString},
        $dataString, ${accString}${globalEnString}, new int[] {$p}); // tuple $match_tuple to ${nameOf(sram).getOrElse("")}""")
      emit(s"""// debug.simPrintf(${accString}[0],"${nameOf(sram).getOrElse("")}-${quote(sram)}_${ii} wr %f @ ${addrDbg} on {$p}\\n", ${dataString}[0], ${addrString}[0]);""")
    }
    emitComment("} Sram_store")
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Dram_new(size) =>
        emitComment(s""" Dram_new(${quote(size)}) {""")
        alwaysGen { emit(s"""int ${quote(sym)} = ${getNextLMemAddr()};""") }
        emitComment(s""" Dram_new(${quote(size)}) }""")

    case Gather(mem,local,addrs,len,_,i) =>
      val worker = childrenOf(parentOf(sym).get).indexOf(sym)
      val par = childrenOf(parentOf(sym).get).length
      print_stage_prefix(s"Gather par${quote(par)}",s"",s"${quote(sym)}", false)
      val access = writersOf(local).find(_.node == sym).get
      val i = instanceIndicesOf(access, addrs).head
      val parStr = if (par == 1) {
//         emit(s"""DFEVar ${quote(sym)}_waddr = ${quote(addrs)}_$i.type.newInstance(this);
// DFEVar ${quote(sym)}_wdata = ${quote(local)}_0.type.newInstance(this); // Assume duplicate _0 exists
// DFEVar ${quote(sym)}_wen = dfeBool().newInstance(this);""")
        ""
      } else {
        s"${quote(par)},"
      }

      emit("{")
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_waddr = new DFEVectorType<DFEVar>(${quote(addrs)}_$i.type, 1).newInstance(this);
DFEVector<DFEVar> ${quote(sym)}_wdata = new DFEVectorType<DFEVar>(${quote(local)}_0.type, 1).newInstance(this);
DFEVar ${quote(sym)}_wen = dfeBool().newInstance(this);""")

      emit(s"""DFEVar ${quote(sym)}_forceLdSt = constant.var(true);""")
      emit(s"""DFEVar ${quote(sym)}_isLdSt = dfeBool().newInstance(this);""")
      emit(s"""GatherLib ${quote(sym)} = new GatherLib(
        this,
        ${quote(sym)}_en, ${quote(sym)}_done, ${bound(worker).get.toInt}, $parStr
        ${quote(sym)}_isLdSt, ${quote(sym)}_forceLdSt,
        ${quote(addrs)}_$i, ${quote(len)},
        ${quote(mem)},  "${quote(mem)}_${quote(sym)}_in",
        ${quote(sym)}_waddr, ${quote(sym)}_wdata, ${quote(sym)}_wen);""")
      val wType = if ((par == 1)) {"connectWport("} else {s"connectDirectWport($worker,"}
      duplicatesOf(local).zipWithIndex.foreach { case (m,i) =>
        emit(s"""${quote(local)}_$i.${wType}${quote(sym)}_waddr, ${quote(sym)}_wdata, ${quote(sym)}_wen);""")
      }
      emit("}")
      print_stage_suffix(quote(sym),false)

    case Scatter(mem,local,addrs,len,_,i) =>
      val worker = childrenOf(parentOf(sym).get).indexOf(sym)
      val par = childrenOf(parentOf(sym).get).length
      print_stage_prefix(s"Scatter par${quote(par)}",s"",s"${quote(sym)}", false)
      val localReader = readersOf(local).find(_.node == sym).get
      val addrsReader = readersOf(addrs).find(_.node == sym).get
      val i = instanceIndicesOf(addrsReader, addrs).head
      val j = instanceIndicesOf(localReader, local).head
      val parStr = if (par == 1) {
//         emit(s"""DFEVar ${quote(sym)}_waddr = ${quote(addrs)}_$i.type.newInstance(this);
// DFEVar ${quote(sym)}_wdata = ${quote(local)}_0.type.newInstance(this); // Assume duplicate _0 exists
// DFEVar ${quote(sym)}_wen = dfeBool().newInstance(this);""")
        ""
      } else {
        s"${quote(par)},"
      }
      emit("{")
      emit(s"""DFEVar ${quote(sym)}_forceLdSt = constant.var(true);""")
      emit(s"""DFEVar ${quote(sym)}_isLdSt = dfeBool().newInstance(this);""")
      emit(s"""ScatterLib ${quote(sym)} = new ScatterLib(
        this,
        ${quote(sym)}_en, ${quote(sym)}_done, ${bound(worker).get.toInt}, ${parStr}
        ${quote(sym)}_isLdSt, ${quote(sym)}_forceLdSt,
        ${quote(addrs)}_$i, ${quote(local)}_$j, ${quote(len)},
        ${quote(mem)}, "${quote(mem)}_${quote(sym)}_out");""")
      emit("}")
      print_stage_suffix(quote(sym),false)

    case BurstLoad(mem, fifo, ofs, len, par) =>
      print_stage_prefix(s"Offchip Load",s"",s"${quote(sym)}", false)
      withStream(baseStream) {
        emit(s"""DFEVar ${quote(fifo)}_trashEn = dfeBool().newInstance(this); // Send stream to trash for when read is not burst-aligned""")
      }
      len match {
        case ConstFix(length) =>
          emit(s"""MemoryCmdGenLib ${quote(sym)} = new MemoryCmdGenLib(
              this,
              ${quote(sym)}_en, ${quote(sym)}_done,
              ${quote(mem)}, ${quote(ofs)},
              "${quote(mem)}_${quote(sym)}_in",
              ${length},
              ${quote(fifo)}_readEn, ${quote(fifo)}_rdata);""")
        case _ =>
          emit(s"""MemoryCmdGenLib ${quote(sym)} = new MemoryCmdGenLib(
              this,
              ${quote(sym)}_en, ${quote(sym)}_done,
              ${quote(mem)}, ${quote(ofs)},
              "${quote(mem)}_${quote(sym)}_in",
              ${quote(len)},
              ${quote(fifo)}_readEn, ${quote(fifo)}_rdata);""")
      }
      emit(s"""${quote(fifo)}_writeEn <== ${quote(sym)}_en;""")
      emit(s"""${quote(fifo)}_wdata <== ${quote(fifo)}_rdata;""")
      print_stage_suffix(quote(sym), false)

    case BurstStore(mem, fifo, ofs, len, par) =>
      // TODO: Offchip stores with burst not aligned
      print_stage_prefix(s"Offchip Store",s"",s"${quote(sym)}", false)
      emit(s"""// ${quote(sym)}: BurstStore(${quote(mem)},${quote(fifo)}, ${quote(ofs)}, ${quote(len)}, ${quote(par)})""")
      emit(s"""MemoryCmdStLib ${quote(sym)} = new MemoryCmdStLib(
          this,
          ${quote(sym)}_en, ${quote(sym)}_done,
          ${quote(mem)}, ${quote(ofs)},
          "${quote(mem)}_${quote(sym)}_out",
          ${quote(len)},
          ${quote(fifo)}_writeEn, ${quote(fifo)}_wdata);""")
      emit(s"""${quote(fifo)}_readEn <== ${quote(sym)}_en;""")
      print_stage_suffix(quote(sym), false)
//      emitComment("Offchip store from fifo")

    case Reg_new(init) =>
      val tp = sym.tp.typeArguments(0)

      // TODO: This is known to have a def, so it shouldn't be necessary to use EatAlias here
      // sym.tp.typeArguments.head.erasure match {
      //   case a: Tup2[_,_] => Console.println("TUP2")
      //   case _ => Console.println("not TUP2")
      // }
      val EatAlias(alias) = sym
      val bits = try { // TODO: Spent way too much time trying to figure out how to get bit info from tuples (╯°□°)╯︵ ┻━┻
        nbits(sym.tp.typeArguments(0).typeArguments(0).typeArguments(1)) + nbits(sym.tp.typeArguments(0).typeArguments(0).typeArguments(2)) + nbits(sym.tp.typeArguments(0).typeArguments(1).typeArguments(0)) + nbits(sym.tp.typeArguments(0).typeArguments(1).typeArguments(1))
      } catch { // Float
        case _ : Throwable => try {nbits(sym.tp.typeArguments(0).typeArguments(0)) + nbits(sym.tp.typeArguments(0).typeArguments(1))}
        catch { // Fixed
        case _ : Throwable => nbits(sym.tp.typeArguments(0).typeArguments(2)) + nbits(sym.tp.typeArguments(0).typeArguments(1))
        }
      }

      // val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])

      if (!regs.contains(alias.asInstanceOf[Sym[Reg[Any]]])) {
        regs += alias.asInstanceOf[Sym[Reg[Any]]]

        withStream(baseStream) {
          emitComment("Reg_new {")
          // val ts = if (!isTup2(sym.tp)) {tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])} else
          //   {List(tpstr(parOf(sym))(sym.tp.typeArguments(0).typeArguments(0), implicitly[SourceContext]),tpstr(parOf(sym))(sym.tp.typeArguments(0).typeArguments(1), implicitly[SourceContext]))}
          val duplicates = duplicatesOf(sym)
          val rstVal = resetValue(sym.asInstanceOf[Sym[Reg[Any]]]) match {
            case ConstFix(rv) => rv
            case ConstFlt(rv) => rv
            case _ => 0
          }
          duplicates.zipWithIndex.foreach { case (d, i) =>
            val skipKerneledReg = (reduceType(sym), i) match {
              case (Some(fps: ReduceFunction), 0) =>
                fps match {
                  case FixPtSum => true
                  case FltPtSum => true
                  case _ => false
                }
              case _ => false
            }
            regType(sym) match {
              case Regular =>
                if (!skipKerneledReg | (skipKerneledReg & d.depth>1)) {
                  val parent = if (parentOf(sym).isEmpty) "top" else quote(parentOf(sym).get) //TODO
                  if (d.depth > 1) {
                    emit(s"""NBufReg ${quote(sym)}_${i}_lib = new NBufReg(this, $bits, "${quote(sym)}_${i}", ${parOf(sym)}, $rstVal, ${d.depth}); // ${nameOf(sym).getOrElse("")}""")
                  } else {
                    emit(s"""DelayLib ${quote(sym)}_${i}_lib = new DelayLib(this, $bits, $rstVal); // ${nameOf(sym).getOrElse("")} readers ${readersOf(sym)}""")
                    val readstr = if (parOf(sym) > 1) "readv" else "read"
                    emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}(); // ${nameOf(sym).getOrElse("")}""")
                    emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = dfeRawBits(${bits}).newInstance(this); // ${nameOf(sym).getOrElse("")}""")
                  }
                }
              case _ => throw new Exception(s"""Unknown reg type ${regType(sym)}""")
            }
          }
          emitComment("Reg_new }")
        }
      } else {
        withStream(baseStream) {
          emit(s"// Already emitted register $sym under alias $alias")
        }
      }

    case Argin_new(init) =>
      withStream(baseStream) {
        val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
        emit(s"""DFEVar ${quote(sym)} = io.scalarInput("${quote(sym)}", $ts );""")
      }
      if (argToExp.contains(sym.asInstanceOf[Sym[Reg[Any]]])) {
        emit(s"""${quote(argToExp(sym.asInstanceOf[Sym[Reg[Any]]]))} <== ${quote(sym)};""")
      }


    case Argout_new(init) => //emitted in reg_write

    case e@Reg_read(EatAlias(reg)) =>
      val isTup =  try { // TODO: Spent way too much time trying to figure out how to get bit info from tuples (╯°□°)╯︵ ┻━┻
        val dummy = nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(1))
        true
      } catch { // not Tup2
        case _ : Throwable => false
      }

      val readers = readersOf(reg).filter(_.node == sym) // There can be more than one!
      readers.foreach{reader =>
        if (!isReduceStarter(sym)) { // Hack to check if this is reduction read
          rTreeMap(sym) match {
            case Nil =>
            case m => Console.println(s"LOAD METADATA on $sym -> $m")
          }

          val pre = maxJPre(sym)
          val inst = instanceIndicesOf(reader, reg).head // Reads should only have one index
          val port = portsOf(reader, reg, inst).head
          val nbuf = if (duplicatesOf(reg)(inst).depth > 1) {s"_lib.read($port)"} else ""

          val regStr = regType(reg) match {
            case Regular =>
              val suffix = {
                if (!controlNodeStack.isEmpty) controlNodeStack.top match {
                  case Deff(n: UnrolledReduce[_,_]) => if (n.acc == reg) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                  case top@Deff(UnitPipe(_)) => if (isAccum(reg) && writtenIn(top).contains(reg)) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                  case _ => ""
                }
                else ""
              }
              quote(reg) + "_" + inst + nbuf + suffix
            case _ =>
              quote(reg)
          }

          val regCast = if (!isTup) {".cast(" + tpstr(parOf(reg))(reg.tp.typeArguments(0), implicitly[SourceContext]) + ")"} else {
            ".cast(dfeRawBits(" + nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(1)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(2)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(1).typeArguments(0)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(1).typeArguments(1)) + "))"
          }

          // Specialized reductions (regular accumulators with a reduction type) just use sym directly
          // TODO: Why was the statement below in the if statement?  Seems like we always want it printed...
          // if (regType(reg) != Regular || !isAccum(reg) || !reduceType(reg).map{t => t != OtherReduction}.getOrElse(false) ) {
          // Do not emit reg read twice
          regType(reg) match {
            case ArgumentIn => // emit in baselib suffix
              if (!emitted_argins.contains((sym, regStr))) {
                emitted_argins += ((sym,regStr))
              }
            case _ => // Otherwise emit here
              if (!emitted_reglibreads.contains((sym, regStr))) {
                emit(s"""$pre ${quote(sym)} = ${regStr}${regCast}; // reg read ${nameOf(reg).getOrElse("")} $isTup""")
                emitted_reglibreads += ((sym, regStr))
              }
          }

        }
        else {
          emit(s"""// ${quote(sym)} is just a register read""")
        }
      }

    case e@Reg_write(EatAlias(reg), value, en) =>
      emitComment("Reg_write {")

      assert(writersOf(reg).nonEmpty, s"Register ${quote(reg)} is not written by a controller")

      val writer = writersOf(reg).find(_.node == sym).get

      val writeCtrl = writersOf(reg).head.controlNode  // Regs have unique writer which also drives reset
      val isTup =  try { // TODO: Spent way too much time trying to figure out how to get bit info from tuples (╯°□°)╯︵ ┻━┻
        val dummy = nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(1))
        true
      } catch { // not Tup2
        case _ : Throwable => false
      }

      val ts = if (!isTup) {tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])} else {
        nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(1)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(0).typeArguments(2)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(1).typeArguments(0)) + "+" + nbits(reg.tp.typeArguments(0).typeArguments(1).typeArguments(1))
      }
      val allDups = duplicatesOf(reg).zipWithIndex
      val dups = allDups.filter{case (dup, i) => instanceIndicesOf(writer, reg).contains(i) }
      val enable = en match {
        case Deff(ConstBit(true)) => s"${quote(writeCtrl)}_done"
        case _ => quote(en)
      }

      regType(reg) match {
        case ArgumentIn => throw new Exception("Cannot write to ArgIn " + quote(reg) + "!")
        case ArgumentOut =>
          if (isAccum(reg)) throw new Exception(s"""ArgOut (${quote(reg)}) cannot be used as an accumulator!""")

          val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
          emit(s"""io.scalarOutput("${quote(reg)}", ${quote(value)}, $ts, $controlStr);""")

        case _ =>
          if (isAccum(reg)) {
            var delayWrenToo = false // Hack to fix specialized accumulators where inputs to accum are delayed (since it makes BFS work)
            // Not sure how to decide this now...
            val accEn = writeCtrl match {
              case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
              case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done"
            }

            val rstStr = quote(parentOf(reg).get) + "_done /*because _rst_en goes hi on each iter*/"
            writeCtrl match {
              // case p@Def(EatReflect(_:OpForeach | _:UnrolledForeach)) => // Safe to comment this out??
              //   throw new Exception(s"Foreaches may not have accumulators ($reg in $p)")

              case p@Deff(_:OpReduce[_,_] | _:UnrolledReduce[_,_] | _:UnitPipe | _:OpForeach | _:UnrolledForeach) =>
                emit(s"// Write to accumulator register")
                emit(s"""DFEVar ${quote(reg)}_en = ${accEn};""")
                reduceType(reg) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                      delayWrenToo = true
                      emit(s"""Accumulator.Params ${quote(reg)}_accParams = Reductions.accumulator.makeAccumulatorConfig($ts).withClear(stream.offset(${rstStr}, -1) /*-1 for BFS*/).withEnable(${quote(reg)}_en);""")
                      emit(s"""DFEVar ${quote(reg)} = Reductions.accumulator.makeAccumulator(stream.offset(${quote(value)}, /*found dlay empirically*/1-${quote(writeCtrl)}_offset), ${quote(reg)}_accParams);""")
                      emit(s"""debug.simPrintf(${quote(reg)}_en & stream.offset(${quote(reg)}_en, -1) /* uncommented because maxj sucks */, "accum has %d (+ %d)\\n", ${quote(reg)}, ${quote(value)});""")
                    case FltPtSum =>
                      emit(s"""DFEVar ${quote(reg)} = FloatingPointAccumulator.accumulateWithReset(${quote(value)}, ${quote(reg)}_en, $rstStr, true);""")
                      emit(s"""// debug.simPrintf(${quote(reg)}_en, "accum has %d (+ %d)\\n", ${quote(reg)}, ${quote(value)});""")
                    case _ =>
                      // TODO: This is very bad assumption!  Actually check which reg to write to!!!
                      emit(s"""DFEVar ${quote(reg)} = ${quote(value)}; // redtype ${fps} unknown, just assign wire""")
                      val port = portsOf(writer, reg, 0).head
                      emit(s"""${quote(reg)}_0_lib.write(${quote(reg)}.cast(dfeRawBits(${quote(reg)}_0_lib.bits)), $enable, constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")
                      emit(s"""${quote(reg)}_0_delayed <== stream.offset(${quote(reg)}_0, -${quote(writeCtrl)}_offset);""")
                      // throw new Exception(s"Reduction $fps codegen unknown!")
                  }
                  // TODO: Assume duplicate 0 is used for reduction, all others need writes
                  val e = if (delayWrenToo) {s"stream.offset($enable, -${quote(writeCtrl)}_offset)"} else s"$enable"
                  dups.foreach { case (dup, ii) => 
                    val port = portsOf(writer, reg, ii).head 
                    writeCtrl match { // Match is necessary for DotProduct because damn thing hangs at compile time if I offset enable and data together
                      case pp@Deff(_:UnitPipe) =>
                        if (ii > 0 | (ii == 0 & dup.depth > 1)) emit(s"""${quote(reg)}_${ii}_lib.write(${quote(reg)}.cast(dfeRawBits(${quote(reg)}_${ii}_lib.bits)),
     $enable /*makes simplefold work*/, constant.var(false), $port); // 1 ${nameOf(reg).getOrElse("")}""")
                      case _ =>
                        if (ii > 0 | (ii == 0 & dup.depth > 1)) emit(s"""${quote(reg)}_${ii}_lib.write(${quote(reg)}.cast(dfeRawBits(${quote(reg)}_${ii}_lib.bits))/*offset makes BFS work*/,
     $enable /*makes simplefold work*/, constant.var(false), $port); // 2 ${nameOf(reg).getOrElse("")}""")
                    }
                  }
                  case None =>
                    dups.foreach { case (dup, ii) => 
                      val port = portsOf(writer, reg, ii).head 
                      emit(s"""${quote(reg)}_${ii}_lib.write(${quote(value)}.cast(dfeRawBits(${quote(reg)}_${ii}_lib.bits)) /*offset makes BFS work*/, 
 $enable /*makes BFS work*/, constant.var(false), $port); // 3 ${nameOf(reg).getOrElse("")}""")
                    }
                    // throw new Exception(s"No reduce function found for $reg ${reduceType(reg)}")
                }
              case _ =>
                emit(s"DFEVar ${quote(reg)}_en = constant.var(true)")
            }
          }
          else { // Non-accumulator registers
            dups.foreach{case (dup, ii) =>
              val regname = s"${quote(reg)}_${ii}"
              val port = portsOf(writer, reg, ii).head
              val rstStr = quote(parentOf(reg).get) + "_rst_en"
              // emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), $rstStr);""")
              if (dup.depth > 1) {
                emit(s"""${regname}_lib.write(${quote(value)}.cast(dfeRawBits(${quote(reg)}_${ii}_lib.bits)),
 $enable, constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")                    
              }
              else {
                // Using an enable signal instead of "always true" is causing an illegal loop.
                // Using a reset signal instead of "always false" is causing an illegal loop.
                // These signals don't matter for pass-through registers anyways.
                emit(s"""${regname}_lib.write(${quote(value)}.cast(dfeRawBits(${quote(reg)}_${ii}_lib.bits)), constant.var(true), constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")
              }
            }
          } // End non-accumulator case
      }
      emitComment(s"} Reg_write // regType ${regType(reg)}, numDuplicates = ${allDups.length}")

    case Sram_new(size, zero) =>
      if (!isBoundSym(sym)) { // TODO: I don't think I need this anymore
        srams += sym.asInstanceOf[Sym[SRAM[Any]]]

        val distinctParents = writersOf(sym).map{writer => parentOf(writer.controlNode)}.distinct
        val allParents = writersOf(sym).map{writer => parentOf(writer.controlNode)}

        withStream(baseStream) {
          emitComment("Sram_new {")
          val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
          //TODO: does templete assume sram has 2 dimension?
          val dims = dimsOf(sym)
          val sizes = dims.map{dim => bound(dim).get.toInt}
          val size0 = sizes(0)
          val size1 = sizes.size match {
            case 1 => 1
            case 2 => sizes(1)
            case _ => throw new Exception("MaxJ generation does not yet support SRAMs with more than 2 dimensions.")
          }
          val dups = duplicatesOf(sym)
          dups.zipWithIndex.foreach { case (r, i) =>
            val banks = getBanking(r)
            val strides = getStride(r)
            if (isDummy(sym)) {
              emit(s"""DummyMemLib ${quote(sym)}_${i} = new DummyMemLib(this, ${ts}, ${banks}); //dummymem""")
            } else {
              if (r.depth == 1) {
                emit(s"""BramLib ${quote(sym)}_${i} = new BramLib(this, ${quote(size0)}, ${quote(size1)}, ${ts}, /*banks*/ ${banks}, /* stride */ ${strides}, ${distinctParents.length}); // ${nameOf(sym).getOrElse("")}""")
              // } else if (r.depth == 2) {
              //   val numReaders_for_this_duplicate = readersOf(sym).filter{r => instanceIndicesOf(r, sym).contains(i) }.map{r => parentOf(r.controlNode)}.distinct.length
              //   emit(s"""SMIO ${quote(sym)}_${i}_sm = addStateMachine("${quote(sym)}_${i}_sm", new ${quote(sym)}_${i}_DblBufSM(this));""")
              //   emit(s"""DblBufKernelLib ${quote(sym)}_${i} = new DblBufKernelLib(this, ${quote(sym)}_${i}_sm,
              //     ${quote(size0)}, ${quote(size1)}, $ts, ${banks}, ${strides}, ${numReaders_for_this_duplicate});""")
              } else {
                def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
                val row_majors = readersOf(sym).filter{read => 
                  instanceIndicesOf(read,sym).head == i
                }.map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.distinct.length == 1}
                val all_same = (row_majors.reduce{_&_} == row_majors.reduce{_|_})
                // {
                //   throw new Exception(s"Cannot handle NBuf memory with both row- and column-major reads!")
                // }
                val read_pars = readersOf(sym).filter{ read =>
                  instanceIndicesOf(read,sym).head == i
                }.map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.length}
                val read_head = read_pars.head
                val varying_rd_sizes = if (!(read_pars.map{a => a == read_head}.reduce{_&_})) {true} else {false}
                  // Console.println(s"Warning!  NBuf has readers of different pars.  Template will do its best to handle this")
                  // throw new Exception(s"""Cannot handle multiple NBuf readers on ${nameOf(sym).getOrElse("")} if they do not have the same access par! ($read_pars)""")
                val write_pars = writersOf(sym).map{write => parIndicesOf(write.node).map{ind => quote2D(ind, 0)}.length }
                val write_head = write_pars.head
                if (!(write_pars.map{a => a == write_head}.reduce{_&_})) {
                  throw new Exception(s"Cannot handle multiple NBuf writers if they do not have the same access par!")
                }
                emit(s"""NBufKernelLib ${quote(sym)}_${i} = new NBufKernelLib(this, "${quote(sym)}_${i}",
                  ${quote(size0)}, ${quote(size1)}, /*size0, size1*/
                  $ts, ${banks}, ${strides}, ${r.depth}, /*banks, strides, depth*/
                  ${all_same}, /*all_same access (row_major or col_major)*/
                  new boolean[] {${row_majors.map{a => a | size1==1}.mkString(",")}}, /*rowmajor read?*/
                  ${write_head}, ${read_head} /*writepar, readpar*/,
                  ${varying_rd_sizes}, new int[] {${read_pars.map{a => a}.mkString(",")}} /*varying rd sizes, rdpars*/); // ${nameOf(sym).getOrElse("")} readers ${readersOf(sym)}""")
              }
            }
          }
          emitComment("} Sram_new")
        }
      } else {
        Console.println(s"$sym is a bound sym!")
      }

    case Sram_load(EatAlias(sram), addr) =>
      sramLoad(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr)

    case Par_sram_load(EatAlias(sram), addr) =>
      sramLoad(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, true)

    case Sram_store(EatAlias(sram), addr, value, en) =>
      sramStore(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, value, en)

    case Par_sram_store(EatAlias(sram), addr, value, ens) =>
      sramStore(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, value, ens)

    case Fifo_new(size, zero) =>  // FIFO is always parallel
      val duplicates = duplicatesOf(sym)
      if (duplicates.size != 1) throw new Exception(s"More than 1 duplicates: $duplicates. Don't know how to handle.")
      if (duplicates.head.banking.size != 1) throw new Exception(s"More than 1 banking dimension: Don't know how to handle.")
      val par = duplicates.head.banking.head.banks
      val ts = tpstr(1)(sym.tp.typeArguments.head, implicitly[SourceContext])
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_rdata = new DFEVectorType<DFEVar>($ts, $par).newInstance(this);""")
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_wdata = new DFEVectorType<DFEVar>($ts, $par).newInstance(this);""")
      emit(s"""DFEVar ${quote(sym)}_readEn = dfeBool().newInstance(this);""")
      emit(s"""DFEVar ${quote(sym)}_writeEn = dfeBool().newInstance(this);""")
      if (!memLdFifos.contains(sym)) { // Make a real fifo
        withStream(baseStream){
          emit(s"""Fifo ${quote(sym)} = new Fifo(this, $ts, ${bound(size).get.toInt * 2}, ${par});""")
        }
      }

    case Par_push_fifo(fifo, value, en, shuffle) =>
      emit(s"""// Par_push_fifo(${quote(fifo)}, ${quote(value)}, ${quote(en)}, ${quote(shuffle)});""")
      val writer = quote(writersOf(fifo).head.controlNode)  // Not using 'en' or 'shuffle'
      emit(s"""${quote(fifo)}_writeEn <== ${writer}_ctr_en;""")
      if (memLdFifos.contains(fifo)) {
        emit(s"""${quote(fifo)}_wdata <== ${quote(value)};""")
      } else {
        emit(s"""${quote(fifo)}.push(${quote(value)}, ${quote(en)}, ${quote(fifo)}_writeEn); // Real fifo push""")
      }


    case Par_pop_fifo(fifo, en) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, ${quote(en)});""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      val readEn = s"${reader}_ctr_en"
      if (insideReduceKernel) {
        emit(s"""DFEVar ${quote(fifo)}_readEn = ${readEn}; // Make a new one""")
      } else {
        emit(s"""${quote(fifo)}_readEn <== ${readEn};""")
      }
      if (memLdFifos.contains(fifo)) {
        emit(s"""DFEVector<DFEVar> ${quote(sym)} = ${quote(fifo)}_rdata;""")
      } else {
        emit(s"""DFEVector<DFEVar> ${quote(sym)} = ${quote(fifo)}.pop(${readEn});""")
      }

    case Pop_fifo(fifo,en) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, 1);""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      if (insideReduceKernel) {
        emit(s"""DFEVar ${quote(fifo)}_readEn = ${reader}_ctr_en; // Make a new one""")
      } else {
        emit(s"""${quote(fifo)}_readEn <== ${reader}_ctr_en;""")
      }
      if (memLdFifos.contains(fifo)) {
        emit(s"""DFEVar ${quote(sym)} = ${quote(fifo)}_rdata[0];""")
      } else {
        emit(s"""DFEVar ${quote(sym)} = ${quote(fifo)}.popSingle(${reader}_ctr_en);""")
      }


    case _ => super.emitNode(sym, rhs)
  }

}

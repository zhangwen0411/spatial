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

trait BlockRAM[T]
trait SparseTile[T]
trait SpatialFIFO[T]
trait SpatialCAM[K,V]
trait SpatialVector[T]
trait CACHE[T]
trait Register[T]
trait DRAM[T]

trait SpatialPipeline
trait SpatialIndices

trait MemoryTemplateTypesExp extends MemoryTemplateTypes with BaseExp {
  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type STile[T] = SparseTile[T]
  type FIFO[T] = SpatialFIFO[T]
  type CAM[K,V] = SpatialCAM[K,V]
  type Vector[T] = SpatialVector[T]
  type Cache[T] = CACHE[T]
  type Reg[T] = Register[T]

  type Pipeline = SpatialPipeline
  type Indices = SpatialIndices

  def isPipeline[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[SpatialPipeline])
  def isRegister[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Register[_]])
  def isBRAM[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[BlockRAM[_]])
  def isSparseTile[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[SparseTile[_]])
  def isFIFO[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[SpatialFIFO[_]])
  def isCache[T:Manifest]    = isSubtype(manifest[T].runtimeClass, classOf[CACHE[_]])
  def isCAM[T:Manifest]      = isSubtype(manifest[T].runtimeClass, classOf[SpatialCAM[_,_]])
  def isVector[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[SpatialVector[_]])

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def stileManifest[T:Manifest]: Manifest[STile[T]] = manifest[SparseTile[T]]
  def fifoManifest[T:Manifest]: Manifest[FIFO[T]] = manifest[SpatialFIFO[T]]
  def camManifest[K:Manifest,V:Manifest]: Manifest[CAM[K,V]] = manifest[SpatialCAM[K,V]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[SpatialVector[T]]
  def cacheManifest[T:Manifest]: Manifest[Cache[T]] = manifest[CACHE[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def pipelineManifest: Manifest[Pipeline] = manifest[SpatialPipeline]

  // TODO: Should be refined manifest? But how to know how many fields to fill in?
  def indicesManifest: Manifest[Indices] = manifest[SpatialIndices]
}

trait MemoryTemplateOpsExp extends MemoryTemplateTypesExp with ExternPrimitiveOpsExp with EffectExp with BRAMOpsExp {
  this: SpatialExp =>

  val stream_offset_guess = 20
  // --- Nodes
  case class Vector_from_list[T](elems: List[Exp[T]])(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  // --- Internal API
  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = reflectPure(Vector_from_list(elems))

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Vector_from_list(elems) => reflectPure(Vector_from_list(f(elems))(e.mT, e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Vector_from_list(elems), u, es) => reflectMirrored(Reflect(Vector_from_list(f(elems))(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }
}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect with ScalaGenControllerTemplateOps {
  val IR: ControllerTemplateOpsExp with SpatialCodegenOps
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialFIFO" => "scala.collection.mutable.Queue[" + remap(m.typeArguments(0)) + "]"
    case "SpatialCAM"  => "scala.collection.mutable.HashMap[" + remap(m.typeArguments(0)) + ", " + remap(m.typeArguments(1)) + "]"
    case "SpatialVector" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "CACHE"     => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"
    case "SpatialPipeline" => "Unit"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Vector_from_list(elems) =>
      emitValDef(sym, "Array" + elems.map(quote).mkString("(", ",", ")"))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMemoryTemplateOps extends CGenEffect {
  val IR: ControllerTemplateOpsExp with SpatialIdentifiers with OffChipMemOpsExp
  with NosynthOpsExp
  import IR._

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each OffChipArray a 384MB chunk now
  val burstSize = 384
  var nextLMemAddr: Long = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr
  }

  def bitsToStringInt(x: Int) = x match {
    case n: Int if n <= 8 => "8"
    case n: Int if n <= 16 => "16"
    case n: Int if n <= 32 => "32"
    case _ => "64"
  }

  def bitsToFloatType(bits: Int) = bits match {
    case n: Int if n <= 32 => "float"
    case _ => "double"
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
    case "BlockRAM" => remapWithRef(m.typeArguments(0))
    case "SpatialVector" => remapWithRef(m.typeArguments(0))
    // case "SpatialFIFO" => ???
    case "Register" => remapWithRef(m.typeArguments(0))
    case "DRAM"     => "maxjLmem"

    case "SpatialCounter" => "int32_t"
    case "SpatialCounterChain" => "int32_t*"
    case "SpatialPipeline" => "void"

    case "SpatialBit" => "bool"
    case "Signed" => ""
    case "Unsign" => "u"
    case "FixedPoint" => remap(m.typeArguments(0)) + "int" + bitsToStringInt(remap(m.typeArguments(1)).toInt + remap(m.typeArguments(2)).toInt) + "_t"
    case "FloatPoint" => bitsToFloatType(remap(m.typeArguments(0)).toInt + remap(m.typeArguments(1)).toInt)
    case bx(n) => n
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
    case Offchip_new(size) =>
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

trait MaxJGenMemoryTemplateOps extends MaxJGenExternPrimitiveOps with MaxJGenFat with MaxJGenControllerTemplateOps {
  val IR: UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform
          with LoweredPipeOpsExp with ControllerTemplateOpsExp with ExternPrimitiveOpsExp with ReductionAnalysisExp

  import IR.{println => _, assert => _, _}

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialVector" => "DFEVector<DFEVar>"
    case _ => super.remap(m)
  }

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each OffChipArray a 384MB chunk now
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

  val brams = Set[Exp[BRAM[Any]]]()
  val regs = Set[Exp[Reg[Any]]]()

  def emitBufferControlSignals() {
    emit(s"""// rdone signals for N-Buffers go here""")

    // Quote duplicate
    // TODO: Change to common "quote duplicate" method?
    def quoteDuplicate(mem: Exp[Any], i: Int): String = {
      if      (isBRAM(mem.tp))     s"""${quote(mem)}_${i}"""
      else if (isRegister(mem.tp)) s"""${quote(mem)}_${i}_lib"""
      else throw new Exception("Cannot double buffer type " + mem.tp)
    }

    val nonBoundMemories = (brams ++ regs).map(aliasOf(_)).filter{case Def(_) => true; case _ => false}

    nonBoundMemories.foreach{mem =>
      val buffers = duplicatesOf(mem).zipWithIndex.filter{case (d,i) => d.depth > 1}
      val readers = readersOf(mem)
      val writers = writersOf(mem)

      buffers.foreach{ case (d, i) =>
        val readsByPort = readers.filter{reader => instanceIndicesOf(reader.access, mem).contains(i) }.groupBy{a => portOf(a.access, mem, i) }
        val writesByPort = writers.filter{writer => instanceIndicesOf(writer.access, mem).contains(i) }.groupBy{a => portOf(a.access, mem, i) }

        if (readsByPort.isEmpty) throw new Exception(s"Memory ${quote(mem)} duplicate #$i has no reader")
        if (writesByPort.isEmpty) throw new Exception(s"Memory ${quote(mem)} duplicate #$i has no writer")
        readsByPort.foreach{ case (port, readers) =>
          val controllers = readers.flatMap{reader => topControllerOf(reader.access, mem, i) }.distinct
          assert(controllers.length <= 1, s"Port $port of memory $mem contains multiple read done control signals")
          // TODO: Syntax for port-specific read done?
          val portlist = port.mkString{","}
          if (controllers.nonEmpty)
            emit(s"""${quoteDuplicate(mem, i)}.connectRdone(${quote(controllers.head.node)}_done, new int[] { $portlist });""")
        }
        writesByPort.foreach{ case (port, writers) =>
          val controllers = writers.flatMap{writer => topControllerOf(writer.access, mem, i) }.distinct
          assert(controllers.length <= 1, s"Port $port of memory $mem contains multiple write done control signals")
          // TODO: Syntax for port-specific write done?
          val portlist = port.mkString{","}
          if (controllers.nonEmpty)
            emit(s"""${quoteDuplicate(mem, i)}.connectWdone(${quote(controllers.head.node)}_done, new int[] { $portlist });""")
        }
      }
    }

  }

  override def emitFileFooter() = {
    emitBufferControlSignals()
    super.emitFileFooter()
  }

  def getBanking(bram: MemInstance) = {
    val bnks = bram.banking.map(_.banks)
    bram.banking.length match {
      case 1 => bnks(0)
      case 2 => bnks.mkString("new int[] {", ",", "}")
      case _ => throw new Exception(s"Can't handle ${bram.banking.length}-D memory!")
    }
  }
  def getStride(bram: MemInstance) = {
    val strds = bram.banking.map{
      case DiagonalBanking(strides, _) => throw new Exception(s"Can't handle Diagonal banking yet")
      case StridedBanking(stride, _) => stride
      case _ => 1
    }
    bram.banking.length match {
      case 1 => strds(0)
      case 2 => strds.mkString("new int[] {", ",", "}")
      case _ => throw new Exception(s"Can't handle ${bram.banking.length}-D memory!")
    }

  }

  def isBoundSym(x: Sym[Any]) = {
    x match {
      case Def(_) => false // Is not a bound sym
      case _ => true
    }
  }

  def bramLoad(read: Sym[Any], bram: Exp[BRAM[Any]], addr: Exp[Any], par: Boolean = false) {
    emitComment("Bram_load {")
    val dups = duplicatesOf(bram)
    val readers = readersOf(bram)
    val reader = readers.find{_.access == read}.get // Corresponding reader for this read node
    val b_i = instanceIndicesOf(read, bram).head    // Instance indices should have exactly one index for reads

    val bram_name = s"${quote(bram)}_${b_i}"
    val pre = if (!par) maxJPre(bram) else "DFEVector<DFEVar>"
    val num_dims = dimsOf(bram).length
    if (isDummy(bram)) {
      val pre = if (!par) maxJPre(bram) else "DFEVector<DFEVar>"
      bankOverride(read) match {
        case -1 => emit(s"""${pre} ${quote(read)} = ${quote(bram_name)}.connectRport(${quote(addr)}); //r1.0""")
        case b => emit(s"""${pre} ${quote(read)} = ${quote(bram_name)}.connectRport(${quote(addr)}, $b); //r1.5""")

      }
    } else {

      val inds = parIndicesOf(read)
      assert(inds.nonEmpty, s"Empty par access indices for read $read of $bram")

      num_dims match {
        case 1 => // 1D bram
          if (inds.length == 1) {
            // One address
            // Spit out DFEVar if not already done
            val addr0 = inds(0)(0)
            addEmittedConsts(addr0)

            if (par)
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${bram_name}.type, 1).newInstance(this, Arrays.asList(${quote(bram_name)}.connectRport(${quote(addr0)}))); //r2""")
            else
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}); //r3""")
          }
          else {
            // Many addresses
            emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr)}); //r4""")
          }
        case 2 => // 2D bram
          if (inds.length == 1) {
            // One address
            val addr0 = inds(0)(0)
            val addr1 = inds(0)(1)
            addEmittedConsts(addr0, addr1)

            if (par)
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${bram_name}.type, 1).newInstance(this, Arrays.asList(${bram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}))); //r5""")
            else
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}); //r6""")

          }
          else {
            // TODO: This may not quite be right
            def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
            // Many addresses
            // Same columns?
            if (inds.map{ind => quote2D(ind, 1)}.distinct.length == 1) {
              val addr0 = inds.map{ind => quote2D(ind,0) }
              val addr1 = quote2D(inds(0), 1)
              emit(s"""// All readers share column. vectorized """)
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.map(quote).mkString(",")})), ${quote(addr1)}); //r7""")
            }
            // Same rows?
            else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
              val addr0 = quote2D(inds(0), 0)
              val addr1 = inds.map{ind => quote2D(ind, 0) }
              emit(s"""// All readers share row. vectorized""")
              emit(s"""${pre} ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}, new DFEVectorType<DFEVar>(${quote(addr1(0))}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.map(quote).mkString(",")}))); //r8""")
            }
            else {
              throw new Exception("Cannot handle this parallel reader because not exclusively row-wise or column-wise access!")
            }
          }
        case _ =>
          throw new Exception("MaxJ generation of more than 2D BRAMs is currently unsupported.")
      }
    }

    // Handle if loading a composite type
    //n.compositeValues.zipWithIndex.map { t =>
    //  val v = t._1
    //  val idx = t._2
    //  visitNode(v)
    //  emit(s"""${quote(v)} <== ${quote(read)}[$idx];""")
    //}
    emitComment("} Bram_load")


  }

  def bramStore(write: Sym[Any], bram: Exp[BRAM[Any]], addr: Exp[Any], value: Exp[Any]) {
    emitComment("Bram_store {")
    val dataStr = quote(value)
    val allDups = duplicatesOf(bram)

    val writers = writersOf(bram)
    val EatAlias(ww) = write
    Console.println(s"bram $bram, writers $writers, matching this $write $ww")
    val writeCtrl = writers.find{_.access == write}.get.controlNode

    val dups = allDups.zipWithIndex.filter{dup => instanceIndicesOf(write,bram).contains(dup._2) }

    val inds = parIndicesOf(write)
    val num_dims = dimsOf(bram).length

    assert(inds.nonEmpty, s"Empty par access indices for write $write of $bram")

    if (isAccum(bram)) {
      val offsetStr = quote(writersOf(bram).head.controlNode) + "_offset"
      val parentPipe = parentOf(bram).getOrElse(throw new Exception(s"Bram ${quote(bram)} does not have a parent!"))
      val parentCtr = parentPipe match {
        case Deff(d:Pipe_fold[_,_]) => d.cchain
        case Deff(d:Pipe_foreach) => d.cchain
        case Deff(d:ParPipeReduce[_,_]) => d.cc
        case Deff(d:ParPipeForeach) => d.cc
        case p => throw new Exception(s"Unknown accumulator parent type $p!")
      }

      val Def(rhss) = parentCtr
      Console.println(s"the parent counter for $write $bram is $parentCtr from $parentPipe, def $rhss")
      val accEn = writeCtrl match {
        case Deff(_: Unit_pipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
        case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done"
      }
      if (writers.length == 1) {
        dups.foreach {case (dd, ii) =>
          num_dims match {
            case 1 =>
              emit(s"""${quote(bram)}_${ii}.connectWport(stream.offset(${quote(addr)}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr)); //w3""")
            case _ =>
              emit(s"""${quote(bram)}_${ii}.connectWport(stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr)); //w4""")
          }
        }
      } /*else {
        val bank_num = i
        dups.foreach {case (dd, ii) =>
          emit(s"""${quote(bram)}_${ii}.connectBankWport(${bank_num}, stream.offset(${quote(addr)}, -$offsetStr),
            stream.offset($dataStr, -$offsetStr), $accEn); //5""")
        }
      }*/
    }
    else { // Not accum
      if (isDummy(bram)) {
        dups.foreach {case (dd, ii) =>
          emit(s"""${quote(bram)}_$ii.connectWport(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w6""")
        }
      }
      else num_dims match {
        case 1 =>
          dups.foreach {case (dd, ii) =>
            emit(s"""${quote(bram)}_${ii}.connectWport(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //8""")
          }
        case 2 =>
          if (inds.length == 1) {
            val addrs = inds(0)
            dups.foreach {case (dd, ii) =>
              emit(s"""${quote(bram)}_${ii}.connectWport(${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //10""")
            }
          }
          else {
            // TODO: This may not quite be right
            def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
            // Many addresses
            // Same columns?
            if (inds.map{ind => quote2D(ind, 1)}.distinct.length == 1) {
              val addr0 = inds.map{ind => quote2D(ind,0) }
              val addr1 = quote2D(inds(0), 1)
              emit(s"""// All readers share column. vectorized """)
              dups.foreach {case (dd, ii) =>
                emit(s"""${quote(bram)}_${ii}.connectWport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                ${dataStr}, ${quote(writeCtrl)}_datapath_en); //13""")
              }
            }
            // Same rows?
            else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
              val addr0 = quote2D(inds(0), 0)
              val addr1 = inds.map{ind => quote2D(ind, 0) }
              emit(s"""// All readers share row. vectorized""")
              dups.foreach {case (dd, ii) =>
                emit(s"""${quote(bram)}_${ii}.connectWport(${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                ${dataStr}, ${quote(writeCtrl)}_datapath_en); // 15""")
              }
            }
            else {
              throw new Exception("Cannot handle this parallel reader because not exclusively row-wise or column-wise access!")
            }
          }
        case _ =>
          throw new Exception("MaxJ generation of more than 2D BRAMs is currently unsupported.")
      }
    }
    emitComment("} Bram_store")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Offchip_new(size) =>
        emitComment(s""" Offchip_new(${quote(size)}) {""")
        alwaysGen { emit(s"""int ${quote(sym)} = ${getNextLMemAddr()};""") }
        emitComment(s""" Offchip_new(${quote(size)}) }""")

    case Gather(mem,local,addrs,len,par) =>
      print_stage_prefix(s"Gather: <b>${quote(sym)}</b>", false)
      // TODO: Should Matt assume instanceIndicesOf returns set of size 1?
      val i = instanceIndicesOf(sym, addrs).head

      val parStr = if (quote(par) == "1") {
//         emit(s"""DFEVar ${quote(sym)}_waddr = ${quote(addrs)}_$i.type.newInstance(this);
// DFEVar ${quote(sym)}_wdata = ${quote(local)}_0.type.newInstance(this); // Assume duplicate _0 exists
// DFEVar ${quote(sym)}_wen = dfeBool().newInstance(this);""")
        ""
      } else {
        "${quote(par)},"
      }
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_waddr = new DFEVectorType<DFEVar>(${quote(addrs)}_$i.type, ${quote(par)}).newInstance(this);
DFEVector<DFEVar> ${quote(sym)}_wdata = new DFEVectorType<DFEVar>(${quote(local)}_0.type, ${quote(par)}).newInstance(this);
DFEVar ${quote(sym)}_wen = dfeBool().newInstance(this);""")        

      emit(s"""DFEVar ${quote(sym)}_forceLdSt = constant.var(true);""")
      emit(s"""DFEVar ${quote(sym)}_isLdSt = dfeBool().newInstance(this);""")
      emit(s"""GatherLib ${quote(sym)} = new GatherLib(
        this,
        ${quote(sym)}_en, ${quote(sym)}_done, $parStr
        ${quote(sym)}_isLdSt, ${quote(sym)}_forceLdSt,
        ${quote(addrs)}_$i, ${quote(len)},
        ${quote(mem)},  "${quote(mem)}_${quote(sym)}_in",
        ${quote(sym)}_waddr, ${quote(sym)}_wdata, ${quote(sym)}_wen);""")
      duplicatesOf(local).zipWithIndex.foreach { case (m,i) => 
        emit(s"""${quote(local)}_$i.connectWport(${quote(sym)}_waddr, ${quote(sym)}_wdata, ${quote(sym)}_wen);""")
      }
      print_stage_suffix(quote(sym),false)

    case Scatter(mem,local,addrs,len,par) =>
      print_stage_prefix(s"Scatter: <b>${quote(sym)}</b>", false)
      // TODO: Should Matt assume instanceIndicesOf returns set of size 1?
      val i = instanceIndicesOf(sym, addrs).head
      // TODO: Should Matt assume instanceIndicesOf returns set of size 1?
      val j = instanceIndicesOf(sym, local).head

      emit(s"""DFEVar ${quote(sym)}_forceLdSt = constant.var(true);""")
      emit(s"""DFEVar ${quote(sym)}_isLdSt = dfeBool().newInstance(this);""")
      emit(s"""ScatterLib ${quote(sym)} = new ScatterLib(
        this,
        ${quote(sym)}_en, ${quote(sym)}_done,
        ${quote(sym)}_isLdSt, ${quote(sym)}_forceLdSt,
        ${quote(addrs)}_$i, ${quote(local)}_$j, ${quote(len)},
        ${quote(mem)}, "${quote(mem)}_${quote(sym)}_out");""")
      print_stage_suffix(quote(sym),false)

    case Offchip_load_cmd(mem, fifo, ofs, len, par) =>
      print_stage_prefix(s"Offchip Load: <b>${quote(sym)}</b>", false)
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

    case Offchip_store_cmd(mem, fifo, ofs, len, par) =>
      // TODO: Offchip stores with burst not aligned
      print_stage_prefix(s"Offchip Store: <b>${quote(sym)}</b>", false)
      emit(s"""// ${quote(sym)}: Offchip_store_cmd(${quote(mem)},${quote(fifo)}, ${quote(ofs)}, ${quote(len)}, ${quote(par)})""")
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
      val EatAlias(alias) = sym
      // TODO: Is it safe to emit things for sym below, or should I emit for alias?
      if (!regs.contains(alias.asInstanceOf[Sym[Reg[Any]]])) {
        regs += alias.asInstanceOf[Sym[Reg[Any]]]

        withStream(baseStream) {
          emitComment("Reg_new {")
          val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
          val duplicates = duplicatesOf(sym)
          val rstVal = resetValue(sym.asInstanceOf[Sym[Reg[Any]]]) match {
            case ConstFix(rv) => rv
            case ConstFlt(rv) => rv
          }
          duplicates.zipWithIndex.foreach { case (d, i) =>
            regType(sym) match {
              case Regular =>
                (reduceType(sym), i) match {
                  case (Some(fps: ReduceFunction), 0) =>
                    Console.println(s"[WARNING] Assume duplicate 0 is the inside reduction register and do not emit lib")
                  case _ =>
                    val parent = if (parentOf(sym).isEmpty) "top" else quote(parentOf(sym).get) //TODO
                    if (d.depth > 1) {
                      emit(s"""DblBufReg ${quote(sym)}_${i}_lib = new DblBufReg(this, $ts, "${quote(sym)}_${i}", ${parOf(sym)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal)); //${d.depth} depth""")
                      val readstr = if (parOf(sym)>1) "readv" else "read"
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}();""")
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = ${ts}.newInstance(this);""")
                    } else {
                      emit(s"""DelayLib ${quote(sym)}_${i}_lib = new DelayLib(this, ${quote(ts)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal));""")
                      val readstr = if (parOf(sym) > 1) "readv" else "read"
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}();""")
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = ${ts}.newInstance(this);""")
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
      val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
            emit(s"""DFEVar ${quote(sym)} = io.scalarInput("${quote(sym)}", $ts );""")
            if (argToExp.contains(sym.asInstanceOf[Sym[Reg[Any]]])) {
              emit(s"""${quote(argToExp(sym.asInstanceOf[Sym[Reg[Any]]]))} <== ${quote(sym)};""")
            }

    case Argout_new(init) => //emitted in reg_write

    case e@Reg_read(reg) =>
      // TODO: Should I eat alias here?
      Console.println(s"Would like to read $reg $sym")
      if (!isReduceStarter(sym)) { // Hack to check if this is reduction read
        rTreeMap(sym) match {
          case Nil =>
          case m => Console.println(s"LOAD METADATA on $sym -> $m")
        }

        val pre = maxJPre(sym)
        val inst = instanceIndicesOf(sym, reg).head // Reads should only have one index

        val regStr = regType(reg) match {
          case Regular =>
            val suffix = {
              if (!controlNodeStack.isEmpty) controlNodeStack.top match {
                case Deff(n: ParPipeReduce[_,_]) => if (n.acc == reg) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                case top@Deff(Unit_pipe(_)) => if (isAccum(reg) && writtenIn(top).contains(reg)) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                case _ => ""
              }
              else ""
            }
            quote(reg) + "_" + inst + suffix
          case _ =>
            quote(reg)
        }

        // Specialized reductions (regular accumulators with a reduction type) just use sym directly
        // TODO: Why was the statement below in the if statement?  Seems like we always want it printed...
        // if (regType(reg) != Regular || !isAccum(reg) || !reduceType(reg).map{t => t != OtherReduction}.getOrElse(false) ) {
        emit(s"""$pre ${quote(sym)} = $regStr;""")
        // } else {
        //   emit(s"""// Wanted to print $pre ${quote(sym)} = $regStr but ${regType(reg)} not Regular, or $reg not accum, or the third bool false""")
        // }
      }
      else {
        emit(s"""// ${quote(sym)} is just a register read""")
      }

    case e@Reg_write(EatAlias(reg), value) =>
      emitComment("Reg_write {")

      assert(writersOf(reg).nonEmpty, s"Register ${quote(reg)} is not written by a controller")

      val writeCtrl = writersOf(reg).head.controlNode  // Regs have unique writer which also drives reset
      val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
      val allDups = duplicatesOf(reg).zipWithIndex
      val dups = allDups.filter{case (dup, i) => instanceIndicesOf(sym, reg).contains(i) }

      regType(reg) match {
        case ArgumentIn => throw new Exception("Cannot write to ArgIn " + quote(reg) + "!")
        case ArgumentOut =>
          if (isAccum(reg)) throw new Exception(s"""ArgOut (${quote(reg)}) cannot be used as an accumulator!""")

          val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
          emit(s"""io.scalarOutput("${quote(reg)}", ${quote(value)}, $ts, $controlStr);""")

        case _ =>
          if (isAccum(reg)) {
            val accEn = writeCtrl match {
              case Deff(_: Unit_pipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
              case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done"
            }

            val rstStr = quote(parentOf(reg).get) + "_done /*because _rst_en goes hi on each iter*/"
            writeCtrl match {
              case p@Def(EatReflect(_:Pipe_foreach | _:ParPipeForeach)) =>
                throw new Exception(s"Foreaches may not have accumulators ($reg in $p)")

              case p@Deff(_:Pipe_fold[_,_] | _:ParPipeReduce[_,_] | _:Unit_pipe) =>
                emit(s"// Write to accumulator register")
                emit(s"""DFEVar ${quote(reg)}_en = $accEn;""")
                reduceType(reg) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                      emit(s"""Accumulator.Params ${quote(reg)}_accParams = Reductions.accumulator.makeAccumulatorConfig($ts).withClear(${rstStr}).withEnable(${quote(reg)}_en);""")
                      emit(s"""DFEVar ${quote(reg)} = Reductions.accumulator.makeAccumulator(${quote(value)}, ${quote(reg)}_accParams);""")
                    case FltPtSum =>
                      emit(s"""DFEVar ${quote(reg)} = FloatingPointAccumulator.accumulateWithReset(${quote(value)}, ${quote(reg)}_en, $rstStr, true);""")
                  }
                  // Assume duplicate 0 is used for reduction, all others need writes
                  dups.foreach { case (dup, ii) => if (ii > 0) emit(s"""${quote(reg)}_${ii}_lib.write(${quote(reg)}, ${quote(writeCtrl)}_done, constant.var(false));""")}
                }
              case _ =>
                emit(s"DFEVar ${quote(reg)}_en = constant.var(true)")
            }
          }
          else { // Non-accumulator registers
            dups.foreach{case (dup, ii) =>
              val regname = s"${quote(reg)}_${ii}"
              regType(reg) match {
                case _ =>
                  val rstStr = quote(parentOf(reg).get) + "_rst_en"
                  // emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), $rstStr);""")
                  if (dup.depth > 1) {
                    emit(s"""${regname}_lib.write(${quote(value)}, ${quote(writeCtrl)}_done, constant.var(false));""")
                  }
                  else {
                    // Using an enable signal instead of "always true" is causing an illegal loop.
                    // Using a reset signal instead of "always false" is causing an illegal loop.
                    // These signals don't matter for pass-through registers anyways.
                    emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), constant.var(false));""")
                  }
              }
            }
          } // End non-accumulator case
      }
      emitComment(s"} Reg_write // regType ${regType(reg)}, numDuplicates = ${allDups.length}")

    case Bram_new(size, zero) =>
      if (!isBoundSym(sym)) {
        brams += sym.asInstanceOf[Sym[BRAM[Any]]]

        withStream(baseStream) {
          emitComment("Bram_new {")
          val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
          //TODO: does templete assume bram has 2 dimension?
          val dims = dimsOf(sym)
          val sizes = dims.map{dim => bound(dim).get.toInt}
          val size0 = sizes(0)
          val size1 = sizes.size match {
            case 1 => 1
            case 2 => sizes(1)
            case _ => throw new Exception("MaxJ generation does not yet support BRAMs with more than 2 dimensions.")
          }
          val dups = duplicatesOf(sym)
          dups.zipWithIndex.foreach { case (r, i) =>
            Console.println(s"emitting bram $sym - $i, instance $r")
            val banks = getBanking(r)
            val strides = getStride(r)
            if (isDummy(sym)) {
              emit(s"""DummyMemLib ${quote(sym)}_${i} = new DummyMemLib(this, ${ts}, ${banks}); //dummymem""")
            } else {
              if (r.depth == 1) {
                emit(s"""BramLib ${quote(sym)}_${i} = new BramLib(this, ${quote(size0)}, ${quote(size1)}, ${ts}, /*banks*/ ${banks}, /* stride */ ${strides});""") // [TODO] Raghu: Stride from metadata
              }
              else if (r.depth == 2) {
                val numReaders_for_this_duplicate = readersOf(sym).filter{q => instanceIndicesOf(q.access, sym).contains(i)}.length

                emit(s"""SMIO ${quote(sym)}_${i}_sm = addStateMachine("${quote(sym)}_${i}_sm", new ${quote(sym)}_${i}_DblBufSM(this));""")
                emit(s"""DblBufKernelLib ${quote(sym)}_${i} = new DblBufKernelLib(this, ${quote(sym)}_${i}_sm,
                  ${quote(size0)}, ${quote(size1)}, $ts, ${banks}, ${strides}, ${numReaders_for_this_duplicate});""")
              } else {
                emit(s"""CANNOT EMIT ${r.depth}-buffered mem yet!!""")
              }
            }
          }
          emitComment("} Bram_new")
        }
      } else {
        Console.println(s"$sym is a bound sym!")
      }

    case Bram_load(EatAlias(bram), addr) =>
      bramLoad(sym, bram.asInstanceOf[Exp[BRAM[Any]]], addr)

    case Par_bram_load(EatAlias(bram), addr) =>
      bramLoad(sym, bram.asInstanceOf[Exp[BRAM[Any]]], addr, true)

    case Bram_store(EatAlias(bram), addr, value) =>
      bramStore(sym, bram.asInstanceOf[Exp[BRAM[Any]]], addr, value)

    case Par_bram_store(EatAlias(bram), addr, value) =>
      bramStore(sym, bram.asInstanceOf[Exp[BRAM[Any]]], addr, value)

    case Fifo_new(size, zero) =>  // FIFO is always parallel
      val duplicates = duplicatesOf(sym)
      if (duplicates.size != 1) throw new Exception(s"More than 1 duplicates: $duplicates. Don't know how to handle.")
      if (duplicates.head.banking.size != 1) throw new Exception(s"More than 1 banking dimension: Don't know how to handle.")
      val par = duplicates.head.banking.head.banks
      val ts = tpstr(1)(sym.tp.typeArguments.head, implicitly[SourceContext])
      emit(s"""// FIFO ${quote(sym)} = Fifo_new[$ts](${quote(size)}, ${quote(zero)});""")
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_rdata = new DFEVectorType<DFEVar>($ts, $par).newInstance(this);""")
      emit(s"""DFEVector<DFEVar> ${quote(sym)}_wdata = new DFEVectorType<DFEVar>($ts, $par).newInstance(this);""")
      emit(s"""DFEVar ${quote(sym)}_readEn = dfeBool().newInstance(this);""")
      emit(s"""DFEVar ${quote(sym)}_writeEn = dfeBool().newInstance(this);""")

    case Par_push_fifo(fifo, value, en, shuffle) =>
      emit(s"""// Par_push_fifo(${quote(fifo)}, ${quote(value)}, ${quote(en)}, ${quote(shuffle)});""")
      val writer = quote(writersOf(fifo).head.controlNode)  // Not using 'en' or 'shuffle'
      emit(s"""${quote(fifo)}_writeEn <== ${writer}_ctr_en;""")
      emit(s"""${quote(fifo)}_wdata <== ${quote(value)};""")


    case Par_pop_fifo(fifo, par) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, ${quote(par)});""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      emit(s"""${quote(fifo)}_readEn <== ${reader}_ctr_en;""")
      emit(s"""DFEVector<DFEVar> ${quote(sym)} = ${quote(fifo)}_rdata;""")

    case Pop_fifo(fifo) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, 1);""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      emit(s"""${quote(fifo)}_readEn <== ${reader}_ctr_en;""")
      emit(s"""DFEVar ${quote(sym)} = ${quote(fifo)}_rdata[0];""")

    case Vec_apply(vec, idx) =>
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""DFEVar ${quote(sym)} = ${quote(vec)}[${quote(idx)}];""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case Vector_from_list(elems) =>
      val ts = tpstr(1)(elems(0).tp, implicitly[SourceContext])
      emit(s"""DFEVector<DFEVar> ${quote(sym)} = new DFEVectorType<DFEVar>($ts, ${elems.size}).newInstance(this, Arrays.asList(${elems.map(quote).mkString(",")}));""")

    case _ => super.emitNode(sym, rhs)
  }

}

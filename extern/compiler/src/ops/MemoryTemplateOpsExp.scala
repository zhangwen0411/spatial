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

  import IR.{println => _, assert => _, infix_until => _, _}

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
        // Note: Grouping by sets of integers here. Accesses to a buffer should either have one port or all ports, so this is ok
        val readsByPort = readers.filter{reader => instanceIndicesOf(reader, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }
        val writesByPort = writers.filter{writer => instanceIndicesOf(writer, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }

        if (readsByPort.isEmpty || writesByPort.isEmpty) throw EmptyDuplicateException(mem, i)

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
          val suff = if (isBRAM(mem.tp)) {""} else if (isRegister(mem.tp)) {"_lib"}
          val wPorts = writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length == 1 }.flatten
          val broadcastPorts = writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length > 1 }
          val rPorts = readsByPort.map{case (ports, writers) => ports.toList.map{a => a}}.flatten
          val fullPorts = (0 until d.depth).map{ i => i }.toSet
          // val fullPorts = d.depth match {// TODO: proper way to make this list?
          //   case 1 => Set(0)
          //   case 2 => Set(0,1)
          //   case 3 => Set(0,1,2)
          //   case 4 => Set(0,1,2,3)
          //   case 5 => Set(0,1,2,3,4)
          //   case 6 => Set(0,1,2,3,4,5)
          //   case _ => throw new Exception(s"Cannot handle nBuf this big! How to I do 0 until d.depth properly?")
          // }
          val dummyWPorts = fullPorts -- wPorts
          val dummyRPorts = fullPorts -- rPorts
          val dummyDonePorts = fullPorts -- wPorts -- rPorts
          readsByPort.foreach{case (ports, readers) => emitPortConnections(ports, readers, "connectStageCtrl","read") }
          writesByPort.foreach{case (ports, writers) => emitPortConnections(ports, writers, "connectStageCtrl","write") }
          emit(s"""${quote(mem)}_${i}${suff}.connectUnwrittenPorts(new int[] {${dummyWPorts.mkString(",")}});""")
          emit(s"""${quote(mem)}_${i}${suff}.connectUnreadPorts(new int[] {${dummyRPorts.mkString(",")}});""")
          emit(s"""${quote(mem)}_${i}${suff}.connectUntouchedPorts(new int[] {${dummyDonePorts.mkString(",")}});""")
          if (writesByPort.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length > 1 }.toList.length == 0) {
            emit(s"""${quote(mem)}_${i}${suff}.connectDummyBroadcast();""")
          }
        } else {
          readsByPort.foreach{case (ports, readers) => emitPortConnections(ports, readers, "connectRdone") }
          writesByPort.foreach{case (ports, writers) => emitPortConnections(ports, writers, "connectWdone") }
        }
      }
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
    val reader = readers.find{_.node == read}.get   // Corresponding reader for this read node
    val b_i = instanceIndicesOf(reader, bram).head    // Instance indices should have exactly one index for reads
    val p = portsOf(read, bram, b_i).head

    val bram_name = s"${quote(bram)}_${b_i}"
    val pre = if (!par) maxJPre(bram) else "DFEVector<DFEVar>"
    val num_dims = dimsOf(bram).length
    if (isDummy(bram)) {
      val pre = if (!par) maxJPre(bram) else "DFEVector<DFEVar>"
      bankOverride(read) match {
        case -1 => emit(s"""${pre} ${quote(read)} = ${quote(bram_name)}.connectRport(${quote(addr)}); //r1.0 ${nameOf(bram).getOrElse("")}""")
        case b => emit(s"""${pre} ${quote(read)} = ${quote(bram_name)}.connectRport(${quote(addr)}, $b); //r1.5 ${nameOf(bram).getOrElse("")}""")

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
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${bram_name}.type, 1).newInstance(this, Arrays.asList(${quote(bram_name)}.connectRport(${quote(addr0)}, new int[] {$p}))); //r2 ${nameOf(bram).getOrElse("")}""")
            else
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}, new int[] {$p}); //r3 ${nameOf(bram).getOrElse("")}""")
          }
          else {
            // Many addresses
            emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr)}, new int[] {$p}); //r4 ${nameOf(bram).getOrElse("")}""")
          }
        case 2 => // 2D bram
          if (inds.length == 1) {
            // One address
            val addr0 = inds(0)(0)
            val addr1 = inds(0)(1)
            addEmittedConsts(addr0, addr1)

            if (par)
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${bram_name}.type, 1).newInstance(this, Arrays.asList(${bram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}, new int[] {$p}))); //r5 ${nameOf(bram).getOrElse("")}""")
            else
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}, new int[] {$p}); //r6 ${nameOf(bram).getOrElse("")}""")

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
              emit(s"""$pre ${quote(read)} = ${bram_name}.connectRport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.map(quote).mkString(",")})), ${quote(addr1)}, new int[] {$p}); //r7 ${nameOf(bram).getOrElse("")}""")
            }
            // Same rows?
            else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
              val addr0 = quote2D(inds(0), 0)
              val addr1 = inds.map{ind => quote2D(ind, 0) }
              emit(s"""// All readers share row. vectorized""")
              emit(s"""${pre} ${quote(read)} = ${bram_name}.connectRport(${quote(addr0)}, new DFEVectorType<DFEVar>(${quote(addr1(0))}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.map(quote).mkString(",")})), new int[] {$p}); //r8 ${nameOf(bram).getOrElse("")}""")
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
    val writer = writers.find(_.node == write).get
    //val EatAlias(ww) = write -- This is unnecessary (can't be bound)
    val distinctParents = writers.map{writer => parentOf(writer.controlNode)}.distinct
    val allParents = writers.map{writer => parentOf(writer.controlNode)}
    if (distinctParents.length < allParents.length) {
      Console.println("[WARNING] Bram $bram has multiple writers controlled by the same controller, which should only happen in CharBramTest!")
      // throw MultipleWriteControllersException(bram, writersOf(bram))
    }
    val writeCtrl = writer.controlNode

    // Figure out if this Ctrl is an accumulation, since we need to do this now that there can be many writers
    val isAccumCtrl = writeCtrl match {
        case Deff(d:Pipe_fold[_,_]) => true
        case Deff(d:Pipe_foreach) => false
        case Deff(d:ParPipeReduce[_,_]) => true
        case Deff(d:ParPipeForeach) => 
          if (childrenOf(parentOf(writeCtrl).get).indexOf(writeCtrl) == childrenOf(parentOf(writeCtrl).get).length-1) {
            styleOf(writeCtrl) match {
              case InnerPipe => true
              case _ => false
            }
          } else {
            false
          }
        case Deff(d:Unit_pipe) => true // Not sure why but this makes matmult work
        case p => throw UnknownParentControllerException(bram, write, writeCtrl)
    }


    val dups = allDups.zipWithIndex.filter{dup => instanceIndicesOf(writer,bram).contains(dup._2) }

    val inds = parIndicesOf(write)
    val num_dims = dimsOf(bram).length

    if (inds.isEmpty) throw NoParIndicesException(bram, write)


    if (isAccum(bram) & isAccumCtrl) {
      val offsetStr = quote(writeCtrl) + "_offset"
      val parentPipe = parentOf(bram).getOrElse(throw UndefinedParentException(bram))
      val parentCtr = parentPipe match {
        case Deff(d:Pipe_fold[_,_]) => d.cchain
        case Deff(d:Pipe_foreach) => d.cchain
        case Deff(d:ParPipeReduce[_,_]) => d.cc
        case Deff(d:ParPipeForeach) => d.cc
        case p => throw UnknownAccumControllerException(bram, write, p)
      }

      val Def(rhss) = parentCtr
      val accEn = writeCtrl match {
        case Deff(_: Unit_pipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
        case Deff(a) => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*wtf pipe is $a*/"
        case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*no def node*/"
      }
      dups.foreach {case (dd, ii) =>
        val p = portsOf(write, bram, ii).mkString(",")
        if (writers.length == 1) {
          num_dims match {
            case 1 =>
              emit(s"""${quote(bram)}_${ii}.connectWport(stream.offset(${quote(addr)}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w3 ${nameOf(bram).getOrElse("")}""")
            case _ =>
              emit(s"""${quote(bram)}_${ii}.connectWport(stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w4 ${nameOf(bram).getOrElse("")}""")
          }
        } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
          val wrType = if (portsOf(write,bram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
          num_dims match {
            case 1 =>
              emit(s"""${quote(bram)}_${ii}.${wrType}(stream.offset(${quote(addr)}, -$offsetStr),
              stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w3.2 ${nameOf(bram).getOrElse("")}""")
            case _ =>
              emit(s"""${quote(bram)}_${ii}.${wrType}(stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w4.2 ${nameOf(bram).getOrElse("")}""")
          }
        } else { // Hardcode writers to banks and hope for the best
          val bank_num = writersOf(bram).map{_.node}.indexOf(write)
          emit(s"""${quote(bram)}_${ii}.connectBankWport(${bank_num}, stream.offset(${quote(addr)}, -$offsetStr),
            stream.offset($dataStr, -$offsetStr), $accEn); //w5 ${nameOf(bram).getOrElse("")}""")
        }
      }
    }
    else { // Not accum
      if (isDummy(bram)) {
        dups.foreach {case (dd, ii) =>
          emit(s"""${quote(bram)}_$ii.connectWport(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w6 ${nameOf(bram).getOrElse("")}""")
        }
      }
      else num_dims match {
        case 1 =>
          dups.foreach {case (dd, ii) =>
            val p = portsOf(write, bram, ii).mkString(",")
            if (writers.length == 1) {
              emit(s"""${quote(bram)}_${ii}.connectWport(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w8 ${nameOf(bram).getOrElse("")}""")
            } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
              val wrType = if (portsOf(write,bram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
              emit(s"""${quote(bram)}_${ii}.${wrType}(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w8.2 ${nameOf(bram).getOrElse("")}""")
            }
          }
        case 2 =>
          if (inds.length == 1) {
            val addrs = inds(0)
            dups.foreach {case (dd, ii) =>
              val p = portsOf(write, bram, ii).mkString(",")
              if (writers.length == 1) {
                emit(s"""${quote(bram)}_${ii}.connectWport(${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w10 ${nameOf(bram).getOrElse("")}""")
              } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                val wrType = if (portsOf(write,bram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                emit(s"""${quote(bram)}_${ii}.${wrType}(${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w10.2 ${nameOf(bram).getOrElse("")}""")
              } else { // Hardcode writers to banks and hope for the best
                val bank_num = writers.map{ w => w.controlNode }.indexOf(write)
                emit(s"""${quote(bram)}_${ii}.connectBankWport(${bank_num}, ${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w10.5 ${nameOf(bram).getOrElse("")}""")
              }
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
                if (writers.length == 1) {
                  emit(s"""${quote(bram)}_${ii}.connectWport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w13 ${nameOf(bram).getOrElse("")}""")                  
                } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                  val p = portsOf(write, bram, ii).mkString(",")
                  val wrType = if (portsOf(write,bram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                  emit(s"""${quote(bram)}_${ii}.${wrType}((new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w13.2 ${nameOf(bram).getOrElse("")}""")
                } else { // Hardcode writers to banks and hope for the best
                  val bank_num = writers.map{ w => w.controlNode }.indexOf(write)
                  emit(s"""${quote(bram)}_${ii}.connectBankWport(${bank_num}, new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w13.5 ${nameOf(bram).getOrElse("")}""")
                }
              }
            }
            // Same rows?
            else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
              val addr0 = quote2D(inds(0), 0)
              val addr1 = inds.map{ind => quote2D(ind, 0) }
              emit(s"""// All readers share row. vectorized""")
              dups.foreach {case (dd, ii) =>
                val p = portsOf(write, bram, ii).mkString(",")
                if (writers.length == 1) {
                  emit(s"""${quote(bram)}_${ii}.connectWport(${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {${p}}); //w16 ${nameOf(bram).getOrElse("")}""")
                } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                  val wrType = if (portsOf(write,bram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                  emit(s"""${quote(bram)}_${ii}.${wrType}(${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en, new int[] {$p}); //w16.2 ${nameOf(bram).getOrElse("")}""")
                } else { // Hardcode writers to banks and hope for the best
                  val bank_num = writers.map{ w => w.node }.indexOf(write)
                  emit(s"""${quote(bram)}_${ii}.connectBankWport(${bank_num}, ${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                  ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w16.5 ${nameOf(bram).getOrElse("")}""")
                }
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
      print_stage_prefix(s"Gather",s"",s"${quote(sym)}", false)
      val access = writersOf(local).find(_.node == sym).get
      val i = instanceIndicesOf(access, addrs).head

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
      print_stage_prefix(s"Scatter",s"",s"${quote(sym)}", false)
      val localReader = readersOf(local).find(_.node == sym).get
      val addrsReader = readersOf(addrs).find(_.node == sym).get
      val i = instanceIndicesOf(addrsReader, addrs).head
      val j = instanceIndicesOf(localReader, local).head

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

    case Offchip_store_cmd(mem, fifo, ofs, len, par) =>
      // TODO: Offchip stores with burst not aligned
      print_stage_prefix(s"Offchip Store",s"",s"${quote(sym)}", false)
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
      // TODO: This is known to have a def, so it shouldn't be necessary to use EatAlias here
      val EatAlias(alias) = sym
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
                    if (false/*d.depth == 2*/) {
                      emit(s"""DblBufReg ${quote(sym)}_${i}_lib = new DblBufReg(this, $ts, "${quote(sym)}_${i}", ${parOf(sym)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal)); //${d.depth} depth""")
                      val readstr = if (parOf(sym)>1) "readv" else "read"
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}(); // ${nameOf(sym).getOrElse("")}""")
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = ${ts}.newInstance(this); // ${nameOf(sym).getOrElse("")}""")
                    } else if (d.depth > 1) {
                      emit(s"""NBufReg ${quote(sym)}_${i}_lib = new NBufReg(this, $ts, "${quote(sym)}_${i}", ${parOf(sym)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal), ${d.depth}); // ${nameOf(sym).getOrElse("")}""")
                    } else {
                      emit(s"""DelayLib ${quote(sym)}_${i}_lib = new DelayLib(this, ${quote(ts)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal));""")
                      val readstr = if (parOf(sym) > 1) "readv" else "read"
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}(); // ${nameOf(sym).getOrElse("")}""")
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = ${ts}.newInstance(this); // ${nameOf(sym).getOrElse("")}""")
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
                  case Deff(n: ParPipeReduce[_,_]) => if (n.acc == reg) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                  case top@Deff(Unit_pipe(_)) => if (isAccum(reg) && writtenIn(top).contains(reg)) "_delayed" else "" // Use the delayed (stream-offset) version inside reduce
                  case _ => ""
                }
                else ""
              }
              quote(reg) + "_" + inst + nbuf + suffix
            case _ =>
              quote(reg)
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
                emit(s"""$pre ${quote(sym)} = $regStr; // reg read ${nameOf(reg).getOrElse("")}""")
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

      Console.println(s"Checking writers of $reg (" + writersOf(reg).mkString(", ") + s") for $sym")
      val writer = writersOf(reg).find(_.node == sym).get

      val writeCtrl = writersOf(reg).head.controlNode  // Regs have unique writer which also drives reset
      val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
      val allDups = duplicatesOf(reg).zipWithIndex
      val dups = allDups.filter{case (dup, i) => instanceIndicesOf(writer, reg).contains(i) }

      regType(reg) match {
        case ArgumentIn => throw new Exception("Cannot write to ArgIn " + quote(reg) + "!")
        case ArgumentOut =>
          if (isAccum(reg)) throw new Exception(s"""ArgOut (${quote(reg)}) cannot be used as an accumulator!""")

          val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
          emit(s"""io.scalarOutput("${quote(reg)}", ${quote(value)}, $ts, $controlStr);""")

        case _ =>
          if (isAccum(reg)) {
            en match {
              case Deff(ConstBit(true)) =>
              case _ => throw new Exception("Enabled register write is not yet supported for an accumulator!")
            }

            // Not sure how to decide this now...
            val accEn = writeCtrl match {
              case Deff(_: Unit_pipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
              case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done"
            }

            val rstStr = quote(parentOf(reg).get) + "_done /*because _rst_en goes hi on each iter*/"
            writeCtrl match {
              // case p@Def(EatReflect(_:Pipe_foreach | _:ParPipeForeach)) => // Safe to comment this out??
              //   throw new Exception(s"Foreaches may not have accumulators ($reg in $p)")

              case p@Deff(_:Pipe_fold[_,_] | _:ParPipeReduce[_,_] | _:Unit_pipe | _:Pipe_foreach | _:ParPipeForeach) =>
                emit(s"// Write to accumulator register")
                emit(s"""DFEVar ${quote(reg)}_en = ${accEn};""")
                reduceType(reg) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                      emit(s"""Accumulator.Params ${quote(reg)}_accParams = Reductions.accumulator.makeAccumulatorConfig($ts).withClear(${rstStr}).withEnable(${quote(reg)}_en);""")
                      emit(s"""DFEVar ${quote(reg)} = Reductions.accumulator.makeAccumulator(${quote(value)}, ${quote(reg)}_accParams);""")
                    case FltPtSum =>
                      emit(s"""DFEVar ${quote(reg)} = FloatingPointAccumulator.accumulateWithReset(${quote(value)}, ${quote(reg)}_en, $rstStr, true);""")
                  }
                  // Assume duplicate 0 is used for reduction, all others need writes
                  dups.foreach { case (dup, ii) => 
                    val port = portsOf(writer, reg, ii).head 
                    if (ii > 0) emit(s"""${quote(reg)}_${ii}_lib.write(${quote(reg)}, ${quote(writeCtrl)}_done, constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")
                  }
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
                  val port = portsOf(writer, reg, ii).head 
                  val rstStr = quote(parentOf(reg).get) + "_rst_en"
                  // emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), $rstStr);""")
                  if (false/*dup.depth == 2*/) {
                    emit(s"""${regname}_lib.write(${quote(value)}, ${quote(writeCtrl)}_done, constant.var(false)); // ${nameOf(reg).getOrElse("")}""")
                  } else if (dup.depth > 1) {
                    val port = portsOf(writer, reg, ii).head 
                    emit(s"""${regname}_lib.write(${quote(value)}, ${quote(writeCtrl)}_done, constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")                    
                  }
                  else {
                    // Using an enable signal instead of "always true" is causing an illegal loop.
                    // Using a reset signal instead of "always false" is causing an illegal loop.
                    // These signals don't matter for pass-through registers anyways.
                    emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), constant.var(false), $port); // ${nameOf(reg).getOrElse("")}""")
                  }
              }
            }
          } // End non-accumulator case
      }
      emitComment(s"} Reg_write // regType ${regType(reg)}, numDuplicates = ${allDups.length}")

    case Bram_new(size, zero) =>
      if (!isBoundSym(sym)) { // TODO: I don't think I need this anymore
        brams += sym.asInstanceOf[Sym[BRAM[Any]]]

        val distinctParents = writersOf(sym).map{writer => parentOf(writer.controlNode)}.distinct
        val allParents = writersOf(sym).map{writer => parentOf(writer.controlNode)}

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
                val row_majors = readersOf(sym).map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.distinct.length == 1}
                val all_same = (row_majors.reduce{_&_} == row_majors.reduce{_|_}) 
                // {
                //   throw new Exception(s"Cannot handle NBuf memory with both row- and column-major reads!")
                // }
                val read_pars = readersOf(sym).map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.length}
                val read_head = read_pars.head
                if (!(read_pars.map{a => a == read_head}.reduce{_&_})) {
                  throw new Exception(s"Cannot handle multiple NBuf readers if they do not have the same access par! ($read_pars)")
                }
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
                  ${write_head}, ${read_head} /*writepar, readpar*/); // ${nameOf(sym).getOrElse("")}""")
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

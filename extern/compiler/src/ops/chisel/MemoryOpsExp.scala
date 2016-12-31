package spatial.compiler.ops
import java.io.{File,FileWriter,PrintWriter}

import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, CGenEffect, DotGenEffect, MaxJGenEffect, MaxJGenFat, ChiselGenEffect, ChiselGenFat, Record}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}
import scala.collection.mutable.Set
import ppl.delite.framework.{Config}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._



trait ChiselGenMemoryOps extends ChiselGenExternPrimitiveOps with ChiselGenFat with ChiselGenControllerOps {
  val IR: UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform
          with UnrolledOpsExp with ControllerOpsExp with ExternPrimitiveOpsExp with ReductionAnalysisExp

  import IR.{println => _, assert => _, infix_until => _, _}

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialVector" => "DFEVector<DFEVar>"
    case _ => super.remap(m)
  }

  override def consumesMemFifo(node: Exp[Any]) = {
    parentOf(node) match {
      case Some(parent) => childrenOf(parent).map { n => n match {
        case Deff(BurstLoad(mem, fifo, ofs, len, par)) => true
        case _ => false
      }}.reduce{_|_}
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

  def emitBufferControlSignals() {
    emit(s"""// rdone signals for N-Buffers go here""")

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
          emit(s"""val ${quote(sym)} = io.ArgIn.$regStr; // reg read""")
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

  def sramLoad(read: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], par: Boolean = false) {
    emitComment("Bram_load {")
    val dups = duplicatesOf(sram)
    val readers = readersOf(sram)
    val reader = readers.find{_.node == read}.get   // Corresponding reader for this read node
    val b_i = instanceIndicesOf(reader, sram).head    // Instance indices should have exactly one index for reads
    val p = portsOf(read, sram, b_i).head

    val sram_name = s"${quote(sram)}_${b_i}"
    val pre = if (!par) maxJPre(sram) else "DFEVector<DFEVar>"
    val num_dims = dimsOf(sram).length
    if (isDummy(sram)) {
      val pre = if (!par) maxJPre(sram) else "DFEVector<DFEVar>"
      bankOverride(read) match {
        case -1 =>
          emit(s"""${pre} ${quote(read)} = ${quote(sram_name)}.connectRport(${quote(addr)}); //r1.0 ${nameOf(sram).getOrElse("")}""")
          emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d\\n", ${quote(read)}, ${quote(addr)});""")
        case b => emit(s"""${pre} ${quote(read)} = ${quote(sram_name)}.connectRport(${quote(addr)}, $b); //r1.5 ${nameOf(sram).getOrElse("")}""")

      }
    } else {

      val inds = parIndicesOf(read)
      assert(inds.nonEmpty, s"Empty par access indices for read $read of $sram")

      num_dims match {
        case 1 => // 1D sram
          if (inds.length == 1) {
            // One address
            // Spit out DFEVar if not already done
            val addr0 = inds(0)(0)
            addEmittedConsts(addr0)

            if (par){
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${sram_name}.type, 1).newInstance(this, Arrays.asList(${quote(sram_name)}.connectRport(${quote(addr0)}, new int[] {$p}))); //r2 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr0)}[0]);""")
            } else{
              emit(s"""$pre ${quote(read)} = ${sram_name}.connectRport(${quote(addr0)}, new int[] {$p}); //r3 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}, ${quote(addr0)});""")
            }
          }
          else {
            // Many addresses
            emit(s"""$pre ${quote(read)} = ${sram_name}.connectRport(${quote(addr)}, new int[] {$p}); //r4 ${nameOf(sram).getOrElse("")}""")
            emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr)}[0]);""")
          }
        case 2 => // 2D sram
          if (inds.length == 1) {
            // One address
            val addr0 = inds(0)(0)
            val addr1 = inds(0)(1)
            addEmittedConsts(addr0, addr1)

            if (par){
              emit(s"""$pre ${quote(read)} = new DFEVectorType<DFEVar>(${sram_name}.type, 1).newInstance(this, Arrays.asList(${sram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}, new int[] {$p}))); //r5 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr0)}[0], ${quote(addr1)}[0]);""")
            }else{
              emit(s"""$pre ${quote(read)} = ${sram_name}.connectRport(${quote(addr0)}, ${quote(addr1)}, new int[] {$p}); //r6 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr0)}[0], ${quote(addr1)}[0]);""")
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
              emit(s"""$pre ${quote(read)} = ${sram_name}.connectRport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.map(quote).mkString(",")})), ${quote(addr1)}, new int[] {$p}); //r7 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr0.head)}[0], ${quote(addr1)}[0]);""")
            }
            // Same rows?
            else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
              val addr0 = quote2D(inds(0), 0)
              val addr1 = inds.map{ind => quote2D(ind, 0) }
              emit(s"""// All readers share row. vectorized""")
              emit(s"""${pre} ${quote(read)} = ${sram_name}.connectRport(${quote(addr0)}, new DFEVectorType<DFEVar>(${quote(addr1(0))}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.map(quote).mkString(",")})), new int[] {$p}); //r8 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(<insert enable>, "read ${quote(sram_name)} %f @ %d from port {$p}\\n", ${quote(read)}[0], ${quote(addr0)}[0], ${quote(addr1.head)}[0]);""")
            }
            else {
              throw new Exception("Cannot handle this parallel reader because not exclusively row-wise or column-wise access!")
            }
          }
        case _ =>
          throw new Exception("MaxJ generation of more than 2D SRAMs is currently unsupported.")
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

  def sramStore(write: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], value: Exp[Any]) {
    emitComment("Bram_store {")
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


    val dups = allDups.zipWithIndex.filter{dup => instanceIndicesOf(writer,sram).contains(dup._2) }

    val inds = parIndicesOf(write)
    val num_dims = dimsOf(sram).length

    if (inds.isEmpty) throw NoParIndicesException(sram, write)


    if (isAccum(sram) & isAccumCtrl) {
      val offsetStr = quote(writeCtrl) + "_offset"
      val parentPipe = parentOf(sram).getOrElse(throw UndefinedParentException(sram))

      var addr0Dbg = ""
      var addr1Dbg = ""
      var dataDbg = ""
      var dupDbg = ""

      val accEn = writeCtrl match {
        case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
        case Deff(a) => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*wtf pipe is $a*/"
        case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*no def node*/"
      }
      dups.foreach {case (dd, ii) =>
        val p = portsOf(write, sram, ii).mkString(",")
        if (writers.length == 1) {
          num_dims match {
            case 1 =>
              emit(s"""${quote(sram)}_${ii}.connectWport(stream.offset(${quote(addr)}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w3 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(stream.offset($accEn, -$offsetStr),"${quote(sram)}_${ii} wr %f @ %d on {$p}\\n", stream.offset($dataStr, -$offsetStr), stream.offset(${quote(addr)}, -$offsetStr);""")
            case _ =>
              emit(s"""${quote(sram)}_${ii}.connectWport(stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w4 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(stream.offset($accEn, -$offsetStr),"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", stream.offset($dataStr, -$offsetStr), stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr);""")
          }
        } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
          val wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
          num_dims match {
            case 1 =>
              emit(s"""${quote(sram)}_${ii}.${wrType}(stream.offset(${quote(addr)}, -$offsetStr),
              stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w3.2 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(stream.offset($accEn, -$offsetStr),"${quote(sram)}_${ii} wr %f @ %d on {$p}\\n", stream.offset($dataStr, -$offsetStr), stream.offset(${quote(addr)}, -$offsetStr);""")
            case _ =>
              emit(s"""${quote(sram)}_${ii}.${wrType}(stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr),
                stream.offset($dataStr, -$offsetStr), stream.offset($accEn, -$offsetStr), new int[] {$p}); //w4.2 ${nameOf(sram).getOrElse("")}""")
              emit(s"""// debug.simPrintf(stream.offset($accEn, -$offsetStr),"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", stream.offset($dataStr, -$offsetStr), stream.offset(${quote(inds(0)(0))}, -$offsetStr), stream.offset(${quote(inds(0)(1))}, -$offsetStr);""")
          }
        } else { // Hardcode writers to banks and hope for the best
          val bank_num = writersOf(sram).map{_.node}.indexOf(write)
          emit(s"""${quote(sram)}_${ii}.connectBankWport(${bank_num}, stream.offset(${quote(addr)}, -$offsetStr),
            stream.offset($dataStr, -$offsetStr), $accEn); //w5 ${nameOf(sram).getOrElse("")}""")
          emit(s"""// debug.simPrintf(stream.offset($accEn, -$offsetStr),"${quote(sram)}_${ii} wr %f @ %d on bank ${bank_num}\\n", stream.offset($dataStr, -$offsetStr), stream.offset(${quote(addr)}, -$offsetStr);""")
        }
      }
    }
    else { // Not accum
      if (isDummy(sram)) {
        dups.foreach {case (dd, ii) =>
          emit(s"""${quote(sram)}_$ii.connectWport(${quote(addr)}, ${dataStr}, ${quote(writeCtrl)}_datapath_en); //w6 ${nameOf(sram).getOrElse("")}""")
          emit(s"""// debug.simPrintf(${quote(writeCtrl)}_datapath_en,"${quote(sram)}_${ii} wr %f @ %d\\n", $dataStr, ${quote(addr)});""")

        }
      } else {
        // val enable = if (consumesMemFifo(writeCtrl)) {s"${quote(writeCtrl)}_datapath_en & ${getTrashBool(writeCtrl)}"} else {s"${quote(writeCtrl)}_datapath_en"}
        val enable = s"${quote(writeCtrl)}_datapath_en"
        num_dims match {
          case 1 =>
            dups.foreach {case (dd, ii) =>
              val p = portsOf(write, sram, ii).mkString(",")
              if (writers.length == 1) {
                emit(s"""${quote(sram)}_${ii}.connectWport(${quote(addr)}, ${dataStr}, ${enable}, new int[] {$p}); //w8 ${nameOf(sram).getOrElse("")}""")
                emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d on {$p}\\n", $dataStr, ${quote(addr)});""")
              } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                val wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                emit(s"""${quote(sram)}_${ii}.${wrType}(${quote(addr)}, ${dataStr}, ${enable}, new int[] {$p}); //w8.2 ${nameOf(sram).getOrElse("")}""")
                emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d on {$p}\\n", $dataStr, ${quote(addr)});""")
              }
            }
          case 2 =>
            if (inds.length == 1) {
              val addrs = inds(0)
              dups.foreach {case (dd, ii) =>
                val p = portsOf(write, sram, ii).mkString(",")
                if (writers.length == 1) {
                  emit(s"""${quote(sram)}_${ii}.connectWport(${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${enable}, new int[] {$p}); //w10 ${nameOf(sram).getOrElse("")}""")
                  emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", $dataStr, ${quote(addrs(0))}, ${quote(addrs(1))});""")
                } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                  val wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                  emit(s"""${quote(sram)}_${ii}.${wrType}(${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${enable}, new int[] {$p}); //w10.2 ${nameOf(sram).getOrElse("")}""")
                  emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", $dataStr, ${quote(addrs(0))}, ${quote(addrs(1))});""")
                } else { // Hardcode writers to banks and hope for the best
                  val bank_num = writers.map{ w => w.controlNode }.indexOf(write)
                  emit(s"""${quote(sram)}_${ii}.connectBankWport(${bank_num}, ${quote(addrs(0))}, ${quote(addrs(1))}, ${dataStr}, ${enable}); //w10.5 ${nameOf(sram).getOrElse("")}""")
                  emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", $dataStr, ${quote(addrs(0))}, ${quote(addrs(1))});""")
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
                    emit(s"""${quote(sram)}_${ii}.connectWport(new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                    ${dataStr}, ${enable}); //w13 ${nameOf(sram).getOrElse("")}""")
                  } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                    val p = portsOf(write, sram, ii).mkString(",")
                    val wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                    emit(s"""${quote(sram)}_${ii}.${wrType}((new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                    ${dataStr}, ${enable}, new int[] {$p}); //w13.2 ${nameOf(sram).getOrElse("")}""")
                  } else { // Hardcode writers to banks and hope for the best
                    val bank_num = writers.map{ w => w.controlNode }.indexOf(write)
                    emit(s"""${quote(sram)}_${ii}.connectBankWport(${bank_num}, new DFEVectorType<DFEVar>(${addr0(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr0.mkString(",")})), ${addr1},
                    ${dataStr}, ${enable}); //w13.5 ${nameOf(sram).getOrElse("")}""")
                  }
                  emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d %d\\n", ${dataStr}[0], ${quote(addr0(0))}, ${addr1});""")
                }
              }
              // Same rows?
              else if (inds.map{ind => quote2D(ind, 0)}.distinct.length == 1) {
                val addr0 = quote2D(inds(0), 0)
                val addr1 = inds.map{ind => quote2D(ind, 0) }
                emit(s"""// All readers share row. vectorized""")
                dups.foreach {case (dd, ii) =>
                  val p = portsOf(write, sram, ii).mkString(",")
                  if (writers.length == 1) {
                    emit(s"""${quote(sram)}_${ii}.connectWport(${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                    ${dataStr}, ${enable}, new int[] {${p}}); //w16 ${nameOf(sram).getOrElse("")}""")
                  } else if (distinctParents.length > 1) { // Connect writers of various parents to mux
                    val wrType = if (portsOf(write,sram,ii).toList.length > 1) {"connectBroadcastWport"} else {"connectWport"}
                    emit(s"""${quote(sram)}_${ii}.${wrType}(${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                    ${dataStr}, ${enable}, new int[] {$p}); //w16.2 ${nameOf(sram).getOrElse("")}""")
                  } else { // Hardcode writers to banks and hope for the best
                    val bank_num = writers.map{ w => w.node }.indexOf(write)
                    emit(s"""${quote(sram)}_${ii}.connectBankWport(${bank_num}, ${addr0}, new DFEVectorType<DFEVar>(${addr1(0)}.getType(), ${inds.length}).newInstance(this, Arrays.asList(${addr1.mkString(",")})),
                    ${dataStr}, ${enable}); //w16.5 ${nameOf(sram).getOrElse("")}""")
                  }
                  emit(s"""// debug.simPrintf(${enable},"${quote(sram)}_${ii} wr %f @ %d %d on {$p}\\n", ${dataStr}[0], ${addr0}, ${quote(addr1(0))});""")
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
    }
    emitComment("} Bram_store")
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
        // emit(s"""${quote(local)}_$i.${wType}${quote(sym)}_waddr, ${quote(sym)}_wdata, ${quote(sym)}_wen);""")
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
      // print_stage_prefix(s"Offchip Load",s"",s"${quote(sym)}", false)
      // withStream(baseStream) {
      //   emit(s"""DFEVar ${quote(fifo)}_trashEn = dfeBool().newInstance(this); // Send stream to trash for when read is not burst-aligned""")
      // }
      // len match {
      //   case ConstFix(length) =>
      //     emit(s"""MemoryCmdGenLib ${quote(sym)} = new MemoryCmdGenLib(
      //         this,
      //         ${quote(sym)}_en, ${quote(sym)}_done,
      //         ${quote(mem)}, ${quote(ofs)},
      //         "${quote(mem)}_${quote(sym)}_in",
      //         ${length},
      //         ${quote(fifo)}_readEn, ${quote(fifo)}_rdata);""")
      //   case _ =>
      //     emit(s"""MemoryCmdGenLib ${quote(sym)} = new MemoryCmdGenLib(
      //         this,
      //         ${quote(sym)}_en, ${quote(sym)}_done,
      //         ${quote(mem)}, ${quote(ofs)},
      //         "${quote(mem)}_${quote(sym)}_in",
      //         ${quote(len)},
      //         ${quote(fifo)}_readEn, ${quote(fifo)}_rdata);""")
      // }
      // emit(s"""${quote(fifo)}_writeEn <== ${quote(sym)}_en;""")
      // emit(s"""${quote(fifo)}_wdata <== ${quote(fifo)}_rdata;""")
      // print_stage_suffix(quote(sym), false)

    case BurstStore(mem, fifo, ofs, len, par) =>
//       // TODO: Offchip stores with burst not aligned
//       print_stage_prefix(s"Offchip Store",s"",s"${quote(sym)}", false)
//       emit(s"""// ${quote(sym)}: BurstStore(${quote(mem)},${quote(fifo)}, ${quote(ofs)}, ${quote(len)}, ${quote(par)})""")
//       emit(s"""MemoryCmdStLib ${quote(sym)} = new MemoryCmdStLib(
//           this,
//           ${quote(sym)}_en, ${quote(sym)}_done,
//           ${quote(mem)}, ${quote(ofs)},
//           "${quote(mem)}_${quote(sym)}_out",
//           ${quote(len)},
//           ${quote(fifo)}_writeEn, ${quote(fifo)}_wdata);""")
//       emit(s"""${quote(fifo)}_readEn <== ${quote(sym)}_en;""")
//       print_stage_suffix(quote(sym), false)
// //      emitComment("Offchip store from fifo")

    case Reg_new(init) =>
      val tp = sym.tp.typeArguments(0)
      Console.println(s"$sym = reg_new")
      Console.println(s"tp = $tp [${isTup2(tp)}]")

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
                      emit(s"""DblBufReg ${quote(sym)}_${i}_lib = new DblBufReg(this, $ts, "${quote(sym)}_${i}", ${parOf(sym)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal)); //${d.depth} depth ${nameOf(sym).getOrElse("")}""")
                      val readstr = if (parOf(sym)>1) "readv" else "read"
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.${readstr}(); // ${nameOf(sym).getOrElse("")}""")
                      emit(s"""${maxJPre(sym)} ${quote(sym)}_${i}_delayed = ${ts}.newInstance(this); // ${nameOf(sym).getOrElse("")}""")
                    } else if (d.depth > 1) {
                      emit(s"""NBufReg ${quote(sym)}_${i}_lib = new NBufReg(this, $ts, "${quote(sym)}_${i}", ${parOf(sym)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal), ${d.depth}); // ${nameOf(sym).getOrElse("")}""")
                    } else {
                      emit(s"""DelayLib ${quote(sym)}_${i}_lib = new DelayLib(this, ${quote(ts)}, new Bits(${quote(ts)}.getTotalBits(), $rstVal)); // ${nameOf(sym).getOrElse("")}""")
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
      // withStream(baseStream) {
      //   val tsb = ctpstrb(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
      //   val tse = ctpstre(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
      //   emit(s"""val ${quote(sym)} = io.ArgIn.${quote(sym)}""")
      // }
      // if (argToExp.contains(sym.asInstanceOf[Sym[Reg[Any]]])) {
      //   emit(s"""${quote(argToExp(sym.asInstanceOf[Sym[Reg[Any]]]))} <== ${quote(sym)};""")
      // }
      // emit(s"""val ${quote(sym)} = Wire(io.ArgIn.${quote(sym)})""")

    case Argout_new(init) => //emitted in reg_write

    case e@Reg_read(EatAlias(reg)) =>
      val readers = readersOf(reg).filter(_.node == sym) // There can be more than one!
      readers.foreach{reader =>
        // if (!isReduceStarter(sym)) { // Hack to check if this is reduction read

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

          // Specialized reductions (regular accumulators with a reduction type) just use sym directly
          // TODO: Why was the statement below in the if statement?  Seems like we always want it printed...
          // if (regType(reg) != Regular || !isAccum(reg) || !reduceType(reg).map{t => t != OtherReduction}.getOrElse(false) ) {
          // Do not emit reg read twice
          regType(reg) match {
            case ArgumentIn => // emit in baselib suffix
              if (!emitted_argins.contains((sym, regStr))) {
                emit(s"""// Placing regread ${quote(sym)} in BaseModule""")
                emitted_argins += ((sym,regStr))
              }
            case _ => // Otherwise emit here
              if (!emitted_reglibreads.contains((sym, regStr))) {
                emit(s"""val ${quote(sym)} = $regStr; // reg read ${nameOf(reg).getOrElse("")}""")
                emitted_reglibreads += ((sym, regStr))
              }
          }

        // }
        // else {
        //   emit(s"""// ${quote(sym)} is just a register read""")
        // }
      }

    case e@Reg_write(EatAlias(reg), value, en) =>
      emitComment("Reg_write {")

      assert(writersOf(reg).nonEmpty, s"Register ${quote(reg)} is not written by a controller")

      Console.println(s"Checking writers of $reg (" + writersOf(reg).mkString(", ") + s") for $sym")
      val writer = writersOf(reg).find(_.node == sym).get

      val writeCtrl = writersOf(reg).head.controlNode  // Regs have unique writer which also drives reset
      val tsb = ctpstrb(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
      val tse = ctpstre(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
      val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])// Deprecated
      val allDups = duplicatesOf(reg).zipWithIndex
      val dups = allDups.filter{case (dup, i) => instanceIndicesOf(writer, reg).contains(i) }

      regType(reg) match {
        case ArgumentIn => throw new Exception("Cannot write to ArgIn " + quote(reg) + "!")
        case ArgumentOut =>
          if (isAccum(reg)) throw new Exception(s"""ArgOut (${quote(reg)}) cannot be used as an accumulator!""")

          val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
          // emitGlobal(s"""var ${quote(reg)} =  $tsb OUTPUT $tse;""")
          emit(s"""io.ArgOut.${quote(reg)} := ${quote(value)}""")


        case _ =>
          if (isAccum(reg)) {
            en match {
              case Deff(ConstBit(true)) =>
              case _ => throw new Exception("Enabled register write is not yet supported for an accumulator!")
            }

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

    case Sram_new(size, zero) =>
      srams += sym.asInstanceOf[Sym[SRAM[Any]]]

      val numDistinctWriters = writersOf(sym).map{writer => parentOf(writer.controlNode)}.distinct.length

      withStream(baseStream) {
        emitComment("Sram_new {")
        val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
        //TODO: does templete assume sram has 2 dimension?
        val dims = dimsOf(sym)
        val sizes = dims.map{dim => bound(dim).get.toInt}
        // val size0 = sizes(0)
        // val size1 = sizes.size match {
        //   case 1 => 1
        //   case 2 => sizes(1)
        //   case _ => throw new Exception("MaxJ generation does not yet support SRAMs with more than 2 dimensions.")
        // }
        val dups = duplicatesOf(sym)
        dups.zipWithIndex.foreach { case (r, i) =>
          val banks = getBanking(r)
          val strides = getStride(r)
          if (isDummy(sym)) {
            emit(s"""DummyMemLib ${quote(sym)}_${i} = new DummyMemLib(this, ${ts}, ${banks}); //dummymem""")
          } else {
            // if (r.depth == 1) {
              emit(s"""val ${quote(sym)}_${i} = Module(new SRAM(${r.depth} /*bufs*/, List(${sizes.map(quote).mkString(",")}) /*dims*/, ${ts}, ${banks} /*banks*/, ${strides} /*strides*/, $numDistinctWriters /*numWriters*/)) // ${nameOf(sym).getOrElse("")}""")
            // } else if (r.depth == 2) {
            //   val numReaders_for_this_duplicate = readersOf(sym).filter{r => instanceIndicesOf(r, sym).contains(i) }.map{r => parentOf(r.controlNode)}.distinct.length
            //   emit(s"""SMIO ${quote(sym)}_${i}_sm = addStateMachine("${quote(sym)}_${i}_sm", new ${quote(sym)}_${i}_DblBufSM(this));""")
            //   emit(s"""DblBufKernelLib ${quote(sym)}_${i} = new DblBufKernelLib(this, ${quote(sym)}_${i}_sm,
            //     ${quote(size0)}, ${quote(size1)}, $ts, ${banks}, ${strides}, ${numReaders_for_this_duplicate});""")
            // } else {
            //   def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
            //   val row_majors = readersOf(sym).map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.distinct.length == 1}
            //   val all_same = (row_majors.reduce{_&_} == row_majors.reduce{_|_})
            //   // {
            //   //   throw new Exception(s"Cannot handle NBuf memory with both row- and column-major reads!")
            //   // }
            //   val read_pars = readersOf(sym).map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.length}
            //   val read_head = read_pars.head
            //   if (!(read_pars.map{a => a == read_head}.reduce{_&_})) {
            //     throw new Exception(s"""Cannot handle multiple NBuf readers on ${nameOf(sym).getOrElse("")} if they do not have the same access par! ($read_pars)""")
            //   }
            //   val write_pars = writersOf(sym).map{write => parIndicesOf(write.node).map{ind => quote2D(ind, 0)}.length }
            //   val write_head = write_pars.head
            //   if (!(write_pars.map{a => a == write_head}.reduce{_&_})) {
            //     throw new Exception(s"Cannot handle multiple NBuf writers if they do not have the same access par!")
            //   }
            //   emit(s"""NBufKernelLib ${quote(sym)}_${i} = new NBufKernelLib(this, "${quote(sym)}_${i}",
            //     ${quote(size0)}, ${quote(size1)}, /*size0, size1*/
            //     $ts, ${banks}, ${strides}, ${r.depth}, /*banks, strides, depth*/
            //     ${all_same}, /*all_same access (row_major or col_major)*/
            //     new boolean[] {${row_majors.map{a => a | size1==1}.mkString(",")}}, /*rowmajor read?*/
            //     ${write_head}, ${read_head} /*writepar, readpar*/); // ${nameOf(sym).getOrElse("")}""")
            // }
          }
        }
        emitComment("} Sram_new")
      }

    case Sram_load(EatAlias(sram), addr) =>
      sramLoad(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr)

    case Par_sram_load(EatAlias(sram), addr) =>
      sramLoad(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, true)

    case Sram_store(EatAlias(sram), addr, value, en) =>
      sramStore(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, value)

    case Par_sram_store(EatAlias(sram), addr, value, ens) =>
      sramStore(sym, sram.asInstanceOf[Exp[SRAM[Any]]], addr, value)

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


    case Par_pop_fifo(fifo, en) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, ${quote(en)});""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      val readEn = s"${reader}_ctr_en"
      emit(s"""${quote(fifo)}_readEn <== ${readEn};""")
      emit(s"""DFEVector<DFEVar> ${quote(sym)} = ${quote(fifo)}_rdata;""")

    case Pop_fifo(fifo,en) =>
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

    case ListVector(elems) =>
      // val ts = tpstr(1)(elems(0).tp, implicitly[SourceContext])
      elems(0).tp.erasure.getSimpleName match {
        case "FixedPoint" => 
          emit(s"""val ${quote(sym)} = ${elems.map(quote)} // TODO: Probably incorrect codegen""")
        case "SpatialVector" => 
          emit(s"""val ${quote(sym)} = ${quote(elems(0))} // TODO: Why is there a ListVector node for a vector??""")
        case _ => 
          emit(s"""val ${quote(sym)} = ${elems.map(quote)} // TODO: Probably incorrect codegen""")
      }
      
      // emit(s"""DFEVector<DFEVar> ${quote(sym)} = new DFEVectorType<DFEVar>($ts, ${elems.size}).newInstance(this, Arrays.asList(${elems.map(quote).mkString(",")}));""")

    case _ => super.emitNode(sym, rhs)
  }

}

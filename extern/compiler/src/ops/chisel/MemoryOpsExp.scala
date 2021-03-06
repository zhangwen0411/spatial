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
    case "SpatialVector" => "val"
    case _ => super.remap(m)
  }

  override def consumesMemFifo(node: Exp[Any]) = {
    Console.println(s"who is the parent of ${node} = ${parentOf(node)}")
    parentOf(node) match {
      case Some(parent) => childrenOf(parent).map { n => 
        Console.println(s"  checking child $n")
        n match {
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
  val burstSize = 256
  var nextLMemAddr: Long = 0//burstSize * 1024 * 1024
  def getNextLMemAddr(/*dram_size: Int*/) = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024; //dram_size + (burstSize - dram_size % burstSize)
    addr/burstSize
  }

  var emittedSize = Set.empty[Exp[Any]]

  // var myBuildDir = "" comes from ControllerOps
  override def initializeGenerator(buildDir:String): Unit = {
    emittedSize = Set.empty[Exp[Any]]
    nextLMemAddr = burstSize * 1024 * 1024
    // myBuildDir = buildDir
    super.initializeGenerator(buildDir)
  }

  val srams = Set[Exp[SRAM[Any]]]()
  val regs = Set[Exp[Reg[Any]]]()

  // override def newStream(fileName:String):PrintWriter = {
  //   val path = myBuildDir + java.io.File.separator + fileName + ".scala"
  //   val pw = new PrintWriter(path)
  //   pw
  // }

  // Quote-Exempt method for quoting syms which should
  //    be exempt from kernel-quoting rules.  Used 
  //    in places where codegen secretly creates syms
  //    that are not in the IR and therefore are assumed
  //    to be bound
  def quoteEx(x: Exp[Any]) = {
    quote(x).replace("t.","")
  }

  def emitBufferControlSignals() {
    withStream(newStream("BufferControlSignals")) {
      emit(s"""package app
import templates._
import chisel3._
class BufferControlSignals(t: TopModule) {
// rdone signals for N-Buffers go here""")

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
          val readPorts = readers.filter{reader => instanceIndicesOf(reader, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }
          val writePorts = writers.filter{writer => instanceIndicesOf(writer, mem).contains(i) }.groupBy{a => portsOf(a, mem, i) }
          if (readPorts.isEmpty || writePorts.isEmpty) throw EmptyDuplicateException(mem, i)

          // val numDupDups = if (isSRAM(mem.tp)) {
          //   if (nameOf(mem).getOrElse("") == "mu0Tile" | nameOf(mem).getOrElse("") == "mu1Tile" | nameOf(mem).getOrElse("") == "btheta" | nameOf(mem).getOrElse("") == "sgdmodel") { // Crazy issue 46 witchcraft
          //     d.duplicates
          //   } else {1} //SUPER TODO: Waiting for david's fix for duplication rules!!!!!!
          //   } else {1}


          // val prnt = parentOf(topCtrl).get
          // val allSiblings = childrenOf(prnt) //-- List(prnt).map{ case (c,_) => c}.toSet


          def emitPortConnections(ports: scala.collection.immutable.Set[Int], accesses: List[Access], connect: String, comment: String = "") {
            val controllers = accesses.flatMap{access => topControllerOf(access, mem, i) }.distinct
            val isBuffered = ports.size == 1 && accesses.nonEmpty

            if (isBuffered && controllers.length > 1)
              throw MultipleSwapControllersException(mem, i, accesses, ports.head)
            else if (isBuffered && controllers.isEmpty)
              throw UndefinedSwapControllerException(mem, i, accesses, ports.head)
            else if (isBuffered) {
              val portlist = ports.mkString{","} // TODO: Can probably use ports.head here
              emit(s"""t.${quoteDuplicate(mem, i)}.${connect}(t.${quote(controllers.head.node)}_done, t.${quote(controllers.head.node)}_en, List($portlist)); /*$comment*/""")
            }
          }

          // Get all siblings of read/write ports and match to ports of buf
          val allSiblings = childrenOf(parentOf(readPorts.map{case (_, readers) => readers.flatMap{a => topControllerOf(a,mem,i)}.head}.head.node).get)
          val readSiblings = readPorts.map{case (_,r) => r.flatMap{ a => topControllerOf(a, mem, i)}}.filter{case l => l.length > 0}.map{case all => all.head.node}
          val writeSiblings = writePorts.map{case (_,r) => r.flatMap{ a => topControllerOf(a, mem, i)}}.filter{case l => l.length > 0}.map{case all => all.head.node}

          val writePortsNumbers = writeSiblings.map{ sw => allSiblings.indexOf(sw) }
          val readPortsNumbers = readSiblings.map{ sr => allSiblings.indexOf(sr) }
          val firstActivePort = math.min( readPortsNumbers.min, writePortsNumbers.min )
          val lastActivePort = math.max( readPortsNumbers.max, writePortsNumbers.max )
          val numStagesInbetween = lastActivePort - firstActivePort
          // readPorts.foreach{case (ports, readers) => emitPortConnections(ports, readers, "connectStageCtrl","read")}
          // writePorts.foreach{case (ports, writers) => emitPortConnections(ports, writers, "connectStageCtrl","write") }
          // Console.println(s" THERE ARE ${numStagesInbetween} stages for ${quoteDuplicate(mem,i)}, who reads ${readPortsNumbers} writes ${writePortsNumbers}, from ${firstActivePort} to ${lastActivePort}")
          (0 to numStagesInbetween).foreach { port =>
            val ctrlId = port + firstActivePort
            val node = allSiblings(ctrlId)
            val rd = if (readPortsNumbers.toList.contains(ctrlId)) {"read"} else ""
            val wr = if (writePortsNumbers.toList.contains(ctrlId)) {"write"} else ""
            val empty = if (rd == "" & wr == "") "empty" else ""
            emit(s"""t.${quoteDuplicate(mem,i)}.connectStageCtrl(t.${quote(node)}_done, t.${quote(node)}_en, List(${port})) /*$rd $wr $empty*/""")
          }

          // (0 until numDupDups).foreach { ii =>
          //   emit(s"""${quoteDuplicate(mem,i,ii)}.connectUnwrittenPorts(new int[] {${noWritePorts.mkString(",")}});""")
          //   emit(s"""${quoteDuplicate(mem,i,ii)}.connectUnreadPorts(new int[] {${noReadPorts.mkString(",")}});""")
          //   // emit(s"""${quoteDuplicate(mem,i,ii)}.connectUntouchedPorts(new int[] {}); //new int[] {${inactivePortsNumbers.mkString(",")}});""")
          //   if (writePorts.map{case (ports, writers) => ports.toList.map{a => a}}.filter{ a => a.length > 1 }.toList.length == 0) {
          //     emit(s"""${quoteDuplicate(mem,i,ii)}.connectDummyBroadcast();""")
          //   }
          // }
        }
      }
      emit("}")
    }
    emit("""new BufferControlSignals(this)""")

  }

  override def emitFileFooter() = {
    emitBufferControlSignals()
    withStream(baseStream) {
      emit(s"""// Emit argin reads""")
      // Get each all unique reg strings
      emitted_argins.toList.map{a=>a._2}.distinct.foreach{ a => 
        emit(s"""val ${a} = io.ArgIn.ports(${argInsByName.indexOf(a)})""")
      }

      emitted_argins.toList.foreach {
        case (sym, regStr) =>
          emit(s"""val ${quote(sym)} = $regStr""")
      }
    }
    super.emitFileFooter()
  }

  def getBanking(sram: MemInstance) = {
    val bnks = sram.banking.map(_.banks)
    sram.banking.length match {
      case 1 => bnks(0)
      case 2 => bnks.mkString("List(", ",", ")")
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
      case 1 => s"List(${strds(0)})"
      case 2 => strds.mkString("List(", ",", ")")
      case _ => throw new Exception(s"Can't handle ${sram.banking.length}-D memory!")
    }

  }

  def isBoundSym(x: Sym[Any]) = x match {
    case Def(_) => false // Is not a bound sym
    case _ => true
  }

  def sramLoad(read: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], par: Boolean = false) {
    emit(s"// ---- sram load ----")
    val dups = duplicatesOf(sram)
    val readers = readersOf(sram)
    val reader = readers.find{_.node == read}.get   // Corresponding reader for this read node
    val parent = reader.controlNode
    val b_i = instanceIndicesOf(reader, sram).head    // Instance indices should have exactly one index for reads
    val p = portsOf(read, sram, b_i).head

    val sram_name = s"${quote(sram)}_${b_i}"
    val num_dims = dimsOf(sram).length

    val inds = parIndicesOf(read)
    val rPar = inds.length

    emit(s"""// Assemble multidimR vector
val ${quote(read)}_rVec = Wire(Vec(${rPar}, new multidimR(${num_dims}, 32)))
${quote(read)}_rVec.foreach { r => r.en := ${quote(parent)}_en}""")
    inds.zipWithIndex.foreach{ case(ind,i) => 
      ind.zipWithIndex.foreach { case(wire,j) =>
        emit(s"""${quote(read)}_rVec($i).addr($j) := ${quote(wire)}""")
      }
    }

    emit(s"""${quote(sram_name)}.connectRPort(Vec(${quote(read)}_rVec.toArray), ${p})""")
    if (par) {
      emit(s"""val ${quote(read)} = (0 until ${rPar}).map{i => ${quote(sram_name)}.io.output.data(${rPar}*${p}+i) }""")
    } else {
      emit(s"""val ${quote(read)} = ${quote(sram_name)}.io.output.data(${rPar}*${p})""")
    }


  }

  def sramStore(write: Sym[Any], sram: Exp[SRAM[Any]], addr: Exp[Any], value: Exp[Any], ens: Exp[Any]) {
    emit(s"// ---- sram store ----")
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
    val globalEn = if (isAccum(sram) & isAccumCtrl) {
      writeCtrl match {
        case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
        case Deff(a) => s"${quote(writeCtrl)}_datapath_en /*& ${quote(writeCtrl)}_redLoop_done *//*wtf pipe is $a*/"
        case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*no def node*/"
      }
    } else {
      s"${quote(writeCtrl)}_datapath_en /*old behavior mask*/"
    }

    val dups = allDups.zipWithIndex.filter{dup => instanceIndicesOf(writer,sram).contains(dup._2) }

    val inds = parIndicesOf(write)
    val num_dims = dimsOf(sram).length
    val wPar = inds.length

    if (inds.isEmpty) throw NoParIndicesException(sram, write)

    val Deff(value_type) = value


    emit(s"""// Assemble multidimW vector
val ${quote(write)}_wVec = Wire(Vec(${wPar}, new multidimW(${num_dims}, 32))) """)
    value match {
      case Deff(d:ListVector[_]) => // zip up vector nodes
        emit(s"""
${quote(write)}_wVec.zip(${quote(value)}).foreach {case (w,d) => w.data := d}
${quote(write)}_wVec.zip(${quote(ens)}).foreach {case (w,e) => w.en := e}""")
      case _ => // Otherwise, just connect one thing
        emit(s"""${quote(write)}_wVec(0).data := ${quote(value)}
${quote(write)}_wVec(0).en := ${quote(ens)}""")
    }
    inds.zipWithIndex.foreach{ case(ind,i) => 
      ind.zipWithIndex.foreach { case(wire,j) =>
        emit(s"""${quote(write)}_wVec($i).addr($j) := ${quote(wire)}""")
      }
    }
    dups.foreach{ case (d,i) =>
      val p = portsOf(write, sram, i).mkString(",")
      emit(s"""${quote(sram)}_$i.connectWPort(${quote(write)}_wVec, ${quote(globalEn)}, List(${p})) """)
    }
    
    // if (isAccum(sram) & isAccumCtrl) {
    //   // val offsetStr = quote(writeCtrl) + "_offset"
    //   // val parentPipe = parentOf(sram).getOrElse(throw UndefinedParentException(sram))

    //   val accEn = writeCtrl match {
    //     case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
    //     case Deff(a) => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*wtf pipe is $a*/"
    //     case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done /*no def node*/"
    //   }
    //   dups.foreach {case (dd, ii) =>
    //     val p = portsOf(write, sram, ii).mkString(",")
    //   }
    // }
    // else { // Not accum

    // }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Dram_new(size) =>
        emitComment(s""" Dram_new(${quote(size)}) {""")
        alwaysGen { emit(s"""val ${quote(sym)} = ${getNextLMemAddr()};""") }
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
      val wasInside = insideBlock
      insideBlock = false
      print_stage_prefix(s"Offchip Load",s"",s"${quote(sym)}", true)
      val streamId = memStreamsByName.indexOf(quote(sym))
      withStream(baseStream) {
        emit(s"""val ${quote(sym)} = Module(new MemController(${quote(par)}))""")
        emit(s"""io.MemStreams.outPorts${streamId} := ${quote(sym)}.io.CtrlToDRAM""")
        emit(s"""${quote(sym)}.io.DRAMToCtrl := io.MemStreams.inPorts${streamId} """)
        insideBlock = wasInside
      }
      emit(s"""// ---- Memory Controller (Load) ${quote(sym)} ----
${quote(sym)}_done := ${quote(sym)}.io.CtrlToAccel.cmdIssued
${quote(sym)}.io.AccelToCtrl.enLoad := ${quote(sym)}_en
${quote(sym)}.io.AccelToCtrl.offset := ${quote(ofs)}
${quote(sym)}.io.AccelToCtrl.base := ${quote(mem)}.U
${quote(sym)}.io.AccelToCtrl.pop := ${quote(fifo)}_writeEn
${quote(fifo)}_wdata.zip(${quote(sym)}.io.CtrlToAccel.data).foreach { case (d, p) => d := p }""")

      len match {
        case ConstFix(length) =>
          emit(s"""${quote(sym)}.io.AccelToCtrl.size := ${length}.U""")
        case _ =>
          emit(s"""${quote(sym)}.io.AccelToCtrl.size := ${quote(len)}""")
      }
      emit(s"""${quote(fifo)}_writeEn := ${quote(sym)}.io.CtrlToAccel.valid;""")
      // emit(s"""${quote(fifo)}_wdata := ${quote(fifo)}_rdata;""")
      print_stage_suffix(quote(sym), true)

    case BurstStore(mem, fifo, ofs, len, par) =>
      val wasInside = insideBlock
      insideBlock = false
      print_stage_prefix(s"Offchip Store",s"",s"${quote(sym)}", true)
      val streamId = memStreamsByName.indexOf(quote(sym))
      withStream(baseStream) {
        emit(s"""val ${quote(sym)} = Module(new MemController(${quote(par)}))""")
        emit(s"""io.MemStreams.outPorts${streamId} := ${quote(sym)}.io.CtrlToDRAM""")
        emit(s"""${quote(sym)}.io.DRAMToCtrl := io.MemStreams.inPorts${streamId} """)
        insideBlock = wasInside
      }
      emit(s"""// ---- Memory Controller (Store) ${quote(sym)} ----
${quote(sym)}_done := ${quote(sym)}.io.CtrlToAccel.valid
${quote(sym)}.io.AccelToCtrl.enStore := ${quote(sym)}_en
${quote(sym)}.io.AccelToCtrl.offset := ${quote(ofs)}
${quote(sym)}.io.AccelToCtrl.base := ${quote(mem)}.U
${quote(sym)}.io.AccelToCtrl.data := ${quote(fifo)}_wdata
${quote(sym)}.io.AccelToCtrl.push := ${quote(fifo)}_writeEn
${quote(sym)}_done := ${quote(sym)}.io.CtrlToAccel.doneStore
""")

      len match {
        case ConstFix(length) =>
          emit(s"""${quote(sym)}.io.AccelToCtrl.size := ${length}.U""")
        case _ =>
          emit(s"""${quote(sym)}.io.AccelToCtrl.size := ${quote(len)}""")
      }
      print_stage_suffix(quote(sym), true)

    case Reg_new(init) =>
      val tp = sym.tp.typeArguments(0)

      // TODO: This is known to have a def, so it shouldn't be necessary to use EatAlias here
      val EatAlias(alias) = sym
      if (!regs.contains(alias.asInstanceOf[Sym[Reg[Any]]])) {
        regs += alias.asInstanceOf[Sym[Reg[Any]]]

        withStream(baseStream) {
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
                    emit(s"""val ${quote(sym)}_${i}_accum = Module(new UIntAccum(32,"add")) // TODO: Is first instance always accum?""")
                    emit(s"""val ${quote(sym)}_${i} = ${quote(sym)}_${i}_accum.io.output""")
                  case _ =>
                    val parent = if (parentOf(sym).isEmpty) "top" else quote(parentOf(sym).get) //TODO
                    if (d.depth > 1) {
                      emit(s"""val ${quote(sym)}_${i}_lib = Module(new NBufFF(${d.depth}, 32)) // ${nameOf(sym).getOrElse("")}""")
                    } else {
                      emit(s"""val ${quote(sym)}_${i}_lib = Module(new FF(32)); // ${nameOf(sym).getOrElse("")}""")
                      val readstr = if (parOf(sym) > 1) "readv" else "read"
                      emit(s"""val ${quote(sym)}_$i = ${quote(sym)}_${i}_lib.io.output.data // ${nameOf(sym).getOrElse("")}""")
                      emit(s"""val ${quote(sym)}_${i}_delayed = Wire(UInt(32)) // ${nameOf(sym).getOrElse("")}""")
                    }
                }
              case _ => throw new Exception(s"""Unknown reg type ${regType(sym)}""")
            }
          }
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
        if (!isReduceStarter(sym)) { // Hack to check if this is reduction read

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
              if (!emitted_argins.contains((sym, regStr.replace("t.","")))) {
                emit(s"""// Placing regread ${quote(sym).replace("t.","")} in BaseModule""")
                emitted_argins += ((sym,regStr.replace("t.","")))
              }
            case _ => // Otherwise emit here
              if (!emitted_reglibreads.contains((sym, regStr))) {
                emit(s"""val ${quote(sym)} = $regStr; // ${nameOf(reg).getOrElse("")}""")
                emitted_reglibreads += ((sym, regStr))
              }
          }

        }
        else {
          emit(s"""val ${quote(sym)} = 0.U // TODO: Figure out how to change the final reduction node that uses this reg_read to a no-op in the unrolling xformer""")
        }
      }

    case e@Reg_write(EatAlias(reg), value, en) =>
      // emitComment("Reg_write {")
      emit(s"")

      assert(writersOf(reg).nonEmpty, s"Register ${quote(reg)} is not written by a controller")

      val writer = writersOf(reg).find(_.node == sym).get

      val writeCtrl = writersOf(reg).head.controlNode  // Regs have unique writer which also drives reset
      val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])// Deprecated
      val allDups = duplicatesOf(reg).zipWithIndex
      val dups = allDups.filter{case (dup, i) => instanceIndicesOf(writer, reg).contains(i) }

      regType(reg) match {
        case ArgumentIn => throw new Exception("Cannot write to ArgIn " + quote(reg) + "!")
        case ArgumentOut =>
          if (isAccum(reg)) throw new Exception(s"""ArgOut (${quote(reg)}) cannot be used as an accumulator!""")
          val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done"
          // emitGlobal(s"""var ${quote(reg)} =  $tsb OUTPUT $tse;""")
          // val wasInside = insideBlock
          // insideBlock = false
          val specialPre = if (insideBlock) "t." else ""
          emit(s"""val ${quote(reg).replace("t.","")} = Reg(init = 0.U) // Write to output register""")
          emit(s"""${quote(reg).replace("t.","")} := Mux(${quote(writeCtrl)}_en, ${quote(value)}, ${quote(reg).replace("t.","")})""")
          emit(s"""${specialPre}io.ArgOut.ports(${argOutsByName.indexOf(quote(reg).replace("t.",""))}) := ${quote(reg).replace("t.","")} // ${quote(reg)}""")
          // insideBlock = wasInside
        case _ =>
          if (isAccum(reg)) {
            // Not sure how to decide this now...
            val accEn = writeCtrl match {
              case Deff(_: UnitPipe) => s"${quote(writeCtrl)}_done /* Not sure if this is right */"
              case _ => s"${quote(writeCtrl)}_datapath_en & ${quote(writeCtrl)}_redLoop_done"
            }

            val rstStr = quote(parentOf(reg).get) + "_done"
            writeCtrl match {
              // case p@Def(EatReflect(_:OpForeach | _:UnrolledForeach)) => // Safe to comment this out??
              //   throw new Exception(s"Foreaches may not have accumulators ($reg in $p)")

              case p@Deff(_:OpReduce[_,_] | _:UnrolledReduce[_,_] | _:UnitPipe | _:OpForeach | _:UnrolledForeach) =>
                emit(s"// Write to accumulator register")
                reduceType(reg) match {
                  case Some(fps: ReduceFunction) => 
                    emit(s"// ---- Specialized reduce for $fps ---- ")
                    fps match {
                      case FixPtSum =>
                        emit(s"""${quote(reg)}_0_accum.io.next := ${quote(value)}""")
                        emit(s"""${quote(reg)}_0_accum.io.enable := ${quote(accEn)}""")
                        emit(s"""${quote(reg)}_0_accum.io.reset := Utils.delay(${quote(rstStr)}, 2)""")
                        emit(s"""val ${quoteEx(reg)} = ${quote(reg)}_0_accum.io.output""")
                      case FltPtSum =>
                        emit(s"""// TODO: Specialized accum, set val ${quote(reg)} = specialAccum( + ${quote(value)} ).withClear(${rstStr}).withEnable(${accEn});""")
                      case _ =>
                    }
                    // TODO: I think this is a maxj vestige, but assume duplicate 0 is used for reduction, all others need writes
                    dups.foreach { case (dup, ii) =>
                      val port = portsOf(writer, reg, ii).head
                      if (ii > 0) emit(s"""${quote(reg)}_${ii}_lib.write(${quoteEx(reg)}, Utils.delay(${quote(writeCtrl)}_done, 1), false.B, $port); // ${nameOf(reg).getOrElse("")}""")
                    }
                  case _ =>
                  }
              case _ =>
                emit(s"val ${accEn} = true.B")
            }
          }
          else { // Non-accumulator registers
            dups.foreach{case (dup, ii) =>
              val regname = s"${quote(reg)}_${ii}"
              val port = portsOf(writer, reg, ii).head
              val rstStr = quote(parentOf(reg).get) + "_rst_en"
              // emit(s"""${regname}_lib.write(${quote(value)}, constant.var(true), $rstStr);""")
              val portSelector = if (dup.depth > 1) s"($port)" else ""
              emit(s"""${regname}_lib.io.input.data := ${quote(value)}; // ${nameOf(reg).getOrElse("")} // on port ${portSelector}""")
              emit(s"""${regname}_lib.io.input.enable := ${quote(writeCtrl)}_done // on port ${portSelector}""")
              emit(s"""${regname}_lib.io.input.reset := false.B // on port ${portSelector}""")
            }
          } // End non-accumulator case
      }
      emit(s"")
      // emitComment(s"} Reg_write // regType ${regType(reg)}, numDuplicates = ${allDups.length}")

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
        def quote2D(ind: List[Exp[Any]], i: Int) = if (i >= ind.length) quote(0) else quote(ind(i))
        val read_pars = readersOf(sym).map{read => parIndicesOf(read.node).map{ind => quote2D(ind, 0)}.length}
        val read_head = read_pars.head

        val write_pars = writersOf(sym).map{write => parIndicesOf(write.node).map{ind => quote2D(ind, 0)}.length }
        val write_head = write_pars.head

        val dups = duplicatesOf(sym)
        dups.zipWithIndex.foreach { case (r, i) =>
          val banks = r.banking.map(_.banks).mkString("List(", ",", ")")
          val strides = getStride(r)
          if (isDummy(sym)) {
            emit(s"""DummyMemLib ${quote(sym)}_${i} = new DummyMemLib(this, ${ts}, ${banks}); //dummymem""")
          } else {
            if (r.depth == 1) {
              emit(s"""val ${quote(sym)}_${i} = Module(new SRAM(
          List(${sizes.map(quote).mkString(",")}), 32,
          ${banks}, ${strides}, $numDistinctWriters, 1, 
          ${write_head}, ${read_head})) // ${nameOf(sym).getOrElse("")}""")
            } else if (r.depth >= 2) {
              val numReaders_for_duplicate = readersOf(sym).filter{r => instanceIndicesOf(r, sym).contains(i) }.map{r => parentOf(r.controlNode)}.distinct.length
              emit(s"""val ${quote(sym)}_${i} = Module(new NBufSRAM(
          List(${sizes.map(quote).mkString(",")}), ${r.depth}, 32,
          ${banks}, ${strides}, $numDistinctWriters, ${numReaders_for_duplicate}, 
          ${write_head}, ${read_head})) // ${nameOf(sym).getOrElse("")}""")
            }
          }
        }
        emitComment("} Sram_new")
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
      val depth = bound(size).get.toInt
      val par = duplicates.head.banking.head.banks
      val ts = tpstr(1)(sym.tp.typeArguments.head, implicitly[SourceContext])
      emit(s"""val ${quote(sym)}_wdata = Wire(Vec($par, UInt(32.W))) // $ts""")
      emit(s"""val ${quote(sym)}_readEn = Wire(Bool())""")
      emit(s"""val ${quote(sym)}_writeEn = Wire(Bool())""")
      // if (consumesMemFifo(sym)) {
      //   emit(s"""val ${quote(sym)}_rdata = Wire(Vec($par, UInt(32.W))) // $ts""")
      //   emit(s"""// val ${quote(sym)} included in memory controller""")
      // } else {
      emit(s"""val ${quote(sym)} = Module(new FIFO($par, $par, ${quote(depth)})) // ${nameOf(sym).getOrElse("")}""")
      emit(s"""val ${quote(sym)}_rdata = ${quote(sym)}.io.out""")
      emit(s"""${quote(sym)}.io.in := ${quote(sym)}_wdata""")
      emit(s"""${quote(sym)}.io.pop := ${quote(sym)}_readEn""")
      emit(s"""${quote(sym)}.io.push := ${quote(sym)}_writeEn""")
      // }

    case Par_push_fifo(fifo, value, en, shuffle) =>
      emit(s"""// ---- Fifo Push ----""")
      val writer = quote(writersOf(fifo).head.controlNode)  // Not using 'en' or 'shuffle'
      emit(s"""${quote(fifo)}_writeEn := ${writer}_ctr_en;""")
      emit(s"""${quote(fifo)}_wdata := ${quote(value)};""")


    case Par_pop_fifo(fifo, en) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, ${quote(en)});""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      val readEn = s"${reader}_ctr_en"
      emit(s"""${quote(fifo)}_readEn := ${readEn};""")
      emit(s"""val ${quote(sym)} = ${quote(fifo)}_rdata;""")

    case Pop_fifo(fifo,en) =>
      emit(s"""// DFEVar ${quote(sym)} = Par_pop_fifo(${quote(fifo)}, 1);""")
      val reader = quote(readersOf(fifo).head.controlNode)  // Assuming that each fifo has a unique reader
      emit(s"""${quote(fifo)}_readEn <== ${reader}_ctr_en;""")
      emit(s"""DFEVar ${quote(sym)} = ${quote(fifo)}_rdata(0);""")

    case Vec_apply(vec, idx) =>
      emit(s"""val ${quote(sym)} = ${quote(vec)}(${quote(idx)});""")
      // rTreeMap(sym) match {
      //   case Nil =>
      //     emit(s"""DFEVar ${quote(sym)} = ${quote(vec)}(${quote(idx)});""")
      //   case m =>
      //     emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      // }

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

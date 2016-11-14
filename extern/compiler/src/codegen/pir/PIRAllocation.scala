package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

trait PIRAllocation extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  override val name = "PIR CU Allocation"
  override val eatReflect = true
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  // -- State
  var top: Option[Symbol] = None
  var mapping = mutable.HashMap[Symbol, PCU]()

  // HACK: Skip parallel pipes in PIR gen
  private def parentHack(x: Symbol): Option[Symbol] = parentOf(x) match {
    case Some(pipe@Deff(_:ParallelPipe)) => parentHack(pipe)
    case parentOpt => parentOpt
  }
  private def controllersHack(pipe: Symbol): List[Symbol] = pipe match {
    case Deff(_:ParallelPipe) => childrenOf(pipe).flatMap{child => controllersHack(child)}
    case _ => List(pipe)
  }
  // Give top controller or first controller below which is not a Parallel
  private def topControllerHack(access: Access, ctrl: Controller): Controller = ctrl.node match {
    case pipe@Deff(ParallelPipe(_)) =>
      topControllerHack(access, childContaining(ctrl, access))
    case _ => ctrl
  }

  // ISSUE #2: Assumes linear stage order
  def pipeDependencies(pipe: Symbol): List[Symbol] = parentOf(pipe) match {
    case Some(parent@Deff(_:ParallelPipe)) => pipeDependencies(parent)
    case Some(parent) =>
      val childs = childrenOf(parent).map{child => controllersHack(child) }
      val idx = childs.indexWhere(_ contains pipe )
      if (idx > 0) childs(idx-1)
      else Nil
    case None => Nil
  }

  def addIterators(cu: PCU, cc: Exp[CounterChain], inds: List[List[Exp[Index]]], ens: List[List[Exp[Bit]]]) {
    val cchain = cu.cchains.find(_.name == quote(cc)).getOrElse(throw new Exception(s"Cannot find counterchain $cc in $cu"))
    inds.zipWithIndex.foreach{case (is, i) =>
      is.foreach{index => cu.addReg(index, CounterReg(cchain, i)) }
    }
    ens.zipWithIndex.foreach{case (es, i) =>
      es.foreach{e => cu.addReg(e, ValidReg(cchain, i)) }
    }
  }

  def allocateCChains(cu: PCU, pipe: Symbol) {
    def allocateCounter(start: Symbol, end: Symbol, stride: Symbol) = {
      val min = cu.getOrElse(start){ allocateLocal(start, pipe) }
      val max = cu.getOrElse(end){ allocateLocal(end, pipe) }
      val step = cu.getOrElse(stride){ allocateLocal(stride, pipe) }
      CUCounter(localScalar(min), localScalar(max), localScalar(step))
    }

    parentHack(pipe).foreach{parent => copyIterators(cu, allocateCU(parent)) }

    val Def(rhs) = pipe
    val ccs = syms(rhs).collect{
      case cc@Deff(Counterchain_new(ctrs)) =>
        val counters = ctrs.collect{case Deff(Counter_new(start,end,stride,_)) => allocateCounter(start, end, stride) }
        CChainInstance(quote(cc), counters)
    }
    cu.cchains ++= ccs
  }

  def initCU(cu: PCU, pipe: Symbol) {
    allocateCChains(cu, pipe)
    cu.deps ++= pipeDependencies(pipe).map(allocateCU)
  }

  def allocateComputeUnit(pipe: Symbol): PCU = mapping.getOrElseUpdate(pipe, {
    val Deff(d) = pipe
    debug(s"Allocating CU for $pipe = $d")
    val parent = parentHack(pipe).map(allocateCU)

    val style = pipe match {
      case Deff(_:UnitPipe) => UnitCU
      case Deff(_:Hwblock)  => UnitCU
      case _ if styleOf(pipe) == SequentialPipe && isInnerPipe(pipe) => UnitCU
      case _ => typeToStyle(styleOf(pipe))
    }

    val cu = PseudoComputeUnit(quote(pipe), pipe, style)
    cu.parent = parent
    initCU(cu, pipe)

    pipe match {
      case Deff(e:UnrolledForeach)      => addIterators(cu, e.cc, e.inds, e.valids)
      case Deff(e:UnrolledReduce[_,_])  => addIterators(cu, e.cc, e.inds, e.valids)
      case Deff(_:UnitPipe | _:Hwblock) => cu.cchains += UnitCChain(quote(pipe)+"_unitcc")
      case _ =>
    }
    if (top.isEmpty && parent.isEmpty) top = Some(pipe)

    debug(s"  created CU $cu")

    cu
  })

  def allocateMemoryUnit(pipe: Symbol): PCU = mapping.getOrElseUpdate(pipe, {
    val parent = parentHack(pipe).map(allocateCU)

    val style = pipe match {
      case Deff(_:BurstLoad[_]) => UnitCU
      case Deff(_:BurstStore[_]) => UnitCU
      case Deff(_:Scatter[_]) => StreamCU
      case Deff(_:Gather[_]) => StreamCU
    }

    val cu = PseudoComputeUnit(quote(pipe), pipe, style)
    cu.parent = parent
    pipe match {
      case Deff(_:BurstLoad[_] | _:BurstStore[_]) => cu.cchains += UnitCChain(quote(pipe)+"_unitcc")
      case Deff(e:Scatter[_]) =>
      case Deff(e:Gather[_]) =>
    }

    initCU(cu, pipe)
    cu
  })

  def allocateCU(pipe: Symbol): PCU = pipe match {
    case Deff(_:Hwblock)             => allocateComputeUnit(pipe)
    case Deff(_:UnrolledForeach)     => allocateComputeUnit(pipe)
    case Deff(_:UnrolledReduce[_,_]) => allocateComputeUnit(pipe)
    case Deff(_:UnitPipe)            => allocateComputeUnit(pipe)

    case Deff(_:BurstLoad[_])  => allocateMemoryUnit(pipe)
    case Deff(_:BurstStore[_]) => allocateMemoryUnit(pipe)
    case Deff(_:Scatter[_])    => allocateMemoryUnit(pipe)
    case Deff(_:Gather[_])     => allocateMemoryUnit(pipe)

    case Def(d) => throw new Exception(s"Don't know how to generate CU for\n  $pipe = $d")
  }

  def prescheduleRegisterRead(reg: Symbol, reader: Symbol, pipe: Option[Symbol]) = {
    debug(s"Allocating register read: $reader")
    // Register reads may be used by more than one pipe
    readersOf(reg).filter(_.node == reader).map(_.controlNode).foreach{readCtrl =>
      val isCurrentPipe = pipe.exists(_ == readCtrl)
      val isLocallyWritten = isWrittenInPipe(reg, readCtrl)

      if (!isCurrentPipe || !isLocallyWritten) {
        val readerCU = allocateCU(readCtrl)
        debug(s"  Adding stage $reader of $reg to reader $readerCU")
        readerCU.computeStages += DefStage(reader)
      }
    }
  }

  def allocateWrittenSRAM(writer: Symbol, mem: Symbol, writerCU: PCU, stages: List[PseudoStage]) {
    val srams = readersOf(mem).map{reader =>
      val readerCU = allocateCU(reader.controlNode)
      copyIterators(readerCU, writerCU)

      val sram = allocateMem(mem, reader.node, readerCU)
      if (readerCU == writerCU) {
        sram.vector = Some(LocalVectorBus)
      }
      else {
        val bus = if (writerCU.isUnit) CUScalar(quote(mem)) else CUVector(quote(mem))
        globals += bus
        sram.vector = Some(bus)
      }

      debug(s"  Allocating written SRAM $mem")
      debug(s"    writer   = $writer")
      debug(s"    writerCU = $writerCU")
      debug(s"    readerCU = $readerCU")

      (readerCU, sram)
    }

    if (stages.nonEmpty) {
      debug(s"    Write stages: ")
      stages.foreach{stage => debug(s"      $stage")}

      val groups = srams.groupBy(_._1).mapValues(_.map(_._2))
      for ((readerCU,srams) <- groups if readerCU != writerCU) {
        debug(s"""  Adding write stages to $readerCU for SRAMs: ${srams.mkString(", ")}""")
        readerCU.writeStages(srams) = (writerCU.pipe,stages)
      }
    }
  }
  def allocateReadSRAM(reader: Symbol, mem: Symbol, readerCU: PCU) = {
    val sram = allocateMem(mem, reader, readerCU)

    debug(s"  Allocating read SRAM $mem")
    debug(s"    reader   = $reader")
    debug(s"    readerCU = $readerCU")
    sram
  }

  private def initializeSRAM(sram: CUMemory, mem_in: Symbol, read: Symbol, cu: PCU) {
    val mem = aliasOf(mem_in)
    val reader = readersOf(mem).find(_.node == read).get

    val instIndex = instanceIndicesOf(reader, mem).head
    val instance = duplicatesOf(mem).apply(instIndex)

    // Find first writer corresponding to this reader
    val writers = writersOf(mem).filter{writer => instanceIndicesOf(writer,mem).contains(instIndex) }
    if (writers.length > 1) {
      throw new Exception(s"$mem: $writers: PIR currently cannot handle multiple writers")
    }
    val writer = writers.headOption

    val writerCU = writer.map{w => allocateCU(w.controlNode) }
    val swapWritePipe = writer.flatMap{w => topControllerOf(w, mem, instIndex) }
    val swapReadPipe  = topControllerOf(reader, mem, instIndex)

    val swapWriteCU = (writer, swapWritePipe) match {
      case (Some(write), Some(ctrl)) =>
        val topCtrl = topControllerHack(write, ctrl)
        Some(allocateCU(topCtrl.node))
      case _ => None
    }
    val swapReadCU = swapReadPipe.map{ctrl =>
        val topCtrl = topControllerHack(reader, ctrl)
        allocateCU(topCtrl.node)
    }

    val remoteWriteCtrl = writerCU.flatMap{cu => cu.cchains.find{case _:UnitCChain | _:CChainInstance => true; case _ => false}}
    val remoteSwapWriteCtrl = swapWriteCU.flatMap{cu => cu.cchains.find{case _:UnitCChain | _:CChainInstance => true; case _ => false}}
    val remoteSwapReadCtrl = swapReadCU.flatMap{cu => cu.cchains.find{case _:UnitCChain | _:CChainInstance => true; case _ => false}}

    val readCtrl = cu.cchains.find{case _:UnitCChain | _:CChainInstance => true; case _ => false}
    val writeCtrl = remoteWriteCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }
    val swapWrite = remoteSwapWriteCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }
    val swapRead  = remoteSwapReadCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }

    val writeIter = writeCtrl.flatMap{cc => cu.innermostIter(cc) }
    val readIter  = readCtrl.flatMap{cc => cu.innermostIter(cc) }

    val banking = if (isFIFO(mem.tp)) Strided(1) else {
      val readBanking  = bank(mem, read, readIter)
      val writeBanking = writer.map{w => bank(mem, w.node, writeIter) }.getOrElse(NoBanks)
      mergeBanking(writeBanking, readBanking)
    }

    sram.writeCtrl = writeCtrl
    sram.swapWrite = swapWrite
    sram.swapRead  = swapRead
    sram.banking   = Some(banking)
    sram.bufferDepth = instance.depth
    if (isFIFO(mem.tp)) sram.mode = FIFOMode
  }

  def allocateMem(mem: Symbol, reader: Symbol, cu: PCU): CUMemory = {
//    if (!isBuffer(mem))
//      throw new Exception(s"Cannot allocate SRAM for non-buffer symbol $mem")

    cu.srams.find{sram => sram.mem == mem && sram.reader == reader}.getOrElse{
      val name = s"${quote(mem)}_${quote(reader)}"
      val size = dimsOf(mem).map{case Exact(d) => d}.product.toInt
      val sram = CUMemory(name, size, aliasOf(mem), reader)
      cu.srams += sram
      sram
    }
  }


  def prescheduleStages(pipe: Symbol, func: Block[Any]) {
    val cu = allocateCU(pipe)

    val remotelyAddedStages = cu.computeStages // Stages added prior to traversing this pipe
    val remotelyAddedStms = remotelyAddedStages.flatMap(_.output).flatMap{
      case s: Sym[_] => findDefinition(s)
      case _ => None
    }

    val stms = remotelyAddedStms ++ getStmsInBlock(func)
    val stages = stms.map{case TP(lhs,rhs) => lhs}
    var remoteStages: Set[Exp[Any]] = Set.empty   // Stages to ignore (goes on different CU)

    // HACK: Ignore write address for SRAMs written from popping the result of tile loads
    // (Skipping the vector packing/unpacking nonsense in between)
    def useFifoOnWrite(mem: Exp[Any], value: Exp[Any]): Boolean = value match {
      case Deff(Pop_fifo(fifo,_))     =>
        debug(s"      $value = pop($fifo) [${writersOf(fifo)}]")
        writersOf(fifo).forall{writer => writer.node match {case Deff(_:BurstLoad[_]) => true; case _ => false }}
      case Deff(Par_pop_fifo(fifo,_)) =>
        debug(s"      $value = pop($fifo) [${writersOf(fifo)}]")
        writersOf(fifo).forall{writer => writer.node match {case Deff(_:BurstLoad[_]) => true; case _ => false }}
      case Deff(ListVector(elems))    =>
        debug(s"      $value = vector")
        useFifoOnWrite(mem, elems.head)
      case Deff(Vec_apply(vec,i))     =>
        debug(s"      $value = vec")
        useFifoOnWrite(mem, vec)
      case _ =>
        debug(s"      $value -> no FIFO-on-write")
        //debug(s"Written value is $value = $d: FIFO-on-write disallowed")
        false
    }

    // HACK! Determine start and end bounds for enable on FIFO
    def getFifoBounds(en: Option[Exp[Any]]): (Option[Exp[Any]], Option[Exp[Any]]) = en match {
      case Some( Deff(Bit_And( Deff(FixPt_Leq(start,i1)), Deff(FixPt_Lt(i2,end)) )) ) => (Some(start), Some(end))
      case Some( Deff(Bit_And(x, y))) =>
        val xBnd = getFifoBounds(Some(x))
        val yBnd = getFifoBounds(Some(y))

        if (xBnd._1.isDefined) xBnd else yBnd

      case Some( Deff(ListVector(en) )) => getFifoBounds(Some(en.head))
      case Some( Deff(Vec_apply(vec,i))) => getFifoBounds(Some(vec))
      case _ => (None, None)
    }

    cu.computeStages.clear() // Clear stages so we don't duplicate existing stages

    foreachSymInBlock(func){
      // NOTE: Writers always appear to occur in the associated writer controller
      // However, register reads may appear outside their corresponding controller
      case writer@LocalWriter(writes) if !isControlNode(writer) =>
        val rhs = writer match {case Deff(d) => d; case _ => null }
        debug(s"$writer = $rhs [WRITER]")

        writes.foreach{case (EatAlias(mem), value, indices) =>
          debug(s"    Checking if $mem write can be implemented as FIFO-on-write:")
          val writeAsFIFO = value.exists{v => useFifoOnWrite(mem, v) }

          if ((isBuffer(mem) || isFIFO(mem.tp)) && writeAsFIFO) {
            // This entire section is a hack to support FIFO-on-write for burst loads
            val enable: Option[Exp[Any]] = writer match {
              case Deff(Sram_store(sram,addr,value,en))      => Some(en)
              case Deff(Par_sram_store(sram,addr,value,en))  => Some(en)
              case Deff(Push_fifo(fifo, value, en))          => Some(en)
              case Deff(Par_push_fifo(fifo, values, ens, _)) => Some(ens)
              case _ => None
            }
            val enableComputation = enable.map{e => getScheduleForAddress(stms)(e) }.getOrElse(Nil)
            val enableSyms = enableComputation.map{case TP(s,d) => s}

            val (start,end) = getFifoBounds(enable)

            val startX = start match {case start@Some(Deff(_:Reg_read[_])) => start; case _ => None }
            val endX = end match {case end@Some(Deff(_:Reg_read[_])) => end; case _ => None }

            val remoteWriteStage = FifoOnWriteStage(mem, startX, endX)
            val enStages = startX.map{s => DefStage(s) }.toList ++ endX.map{s => DefStage(s) }.toList

            allocateWrittenSRAM(writer, mem, cu, enStages ++ List(remoteWriteStage))

            val indexComputation = indices.map{is => getScheduleForAddress(stms)(is) }.getOrElse(Nil)
            val indexSyms = indexComputation.map{case TP(s,d) => s}
            remoteStages ++= symsOnlyUsedInWriteAddrOrEn(stms)(func.res, indexSyms ++ enableSyms)
          }
          else if (isBuffer(mem)) {
            val indexComputation = indices.map{is => getScheduleForAddress(stms)(is) }.getOrElse(Nil)
            val indexSyms = indexComputation.map{case TP(s,d) => s }
            val indexStages = indexSyms.map{s => DefStage(s) }
            val flatOpt = indices.map{is => flattenNDAddress(is, dimsOf(mem)) }
            val addr = flatOpt.map(_._1)
            val remoteWriteStage = addr.map{a => WriteAddrStage(mem, a) }
            val addrStages = indexStages ++ flatOpt.map(_._2).getOrElse(Nil) ++ remoteWriteStage

            allocateWrittenSRAM(writer, mem, cu, addrStages)

            val isLocallyRead = isReadInPipe(mem, pipe)
            // Currently have to duplicate if used in both address and compute
            if (indexSyms.nonEmpty && !isLocallyRead) {
              //debug(s"  Checking if symbols calculating ${addr.get} are used in current scope $pipe")
              remoteStages ++= symsOnlyUsedInWriteAddr(stms)(func.res, indexSyms)
            }
          }
        }

      case reader@LocalReader(reads) if !isControlNode(reader) =>
        val rhs = reader match {case Deff(d) => d; case _ => null }
        debug(s"  $reader = $rhs [READER]")

        reads.foreach{case (EatAlias(mem),indices) =>
          if (isReg(mem.tp)) {
            prescheduleRegisterRead(mem, reader, Some(pipe))
            val isLocallyRead = isReadInPipe(mem, pipe, Some(reader))
            val isLocallyWritten = isWrittenInPipe(mem, pipe)
            //debug(s"  isLocallyRead: $isLocallyRead, isLocallyWritten: $isLocallyWritten")
            if (!isLocallyWritten || !isLocallyRead || isInnerAccum(mem)) remoteStages += reader
          }
          else if (isBuffer(mem)) {
            allocateReadSRAM(reader, mem, cu)
          }
        }

      case lhs@Def(rhs) =>
        debug(s"  $lhs = $rhs [OTHER]")
        traverse(lhs.asInstanceOf[Sym[Any]], rhs)
    }

    val localCompute = stages.filter{s => (isPrimitiveNode(s) || isRegisterRead(s) || isGlobal(s) || isVector(s.tp)) && !remoteStages.contains(s) }

    // Sanity check
    val trueComputation = localCompute.filterNot{case Exact(_) => true; case Def(ConstBit(_)) => true; case s => isRegisterRead(s)}
    if (isOuterControl(pipe) && trueComputation.nonEmpty) {
      stageWarn(s"Outer control $pipe has compute stages: ")
      trueComputation.foreach{case lhs@Def(rhs) => stageWarn(s"  $lhs = $rhs")}
    }

    cu.computeStages ++= localCompute.map{s => DefStage(s, isReduce = reduceType(s).isDefined) }
  }

  def prescheduleBurstTransfer(pipe: Symbol, mem: Symbol, ofs: Symbol, len: Symbol, mode: OffchipMemoryMode) = {
    // Ofs and len must either be constants or results of reading registers written in another controller
    val ofsWriter = ofs match {case Deff(Reg_read(reg)) if writersOf(reg).nonEmpty => Some(writersOf(reg).head); case _ => None }
    val lenWriter = len match {case Deff(Reg_read(reg)) if writersOf(reg).nonEmpty => Some(writersOf(reg).head); case _ => None }

    var ofsCUOpt = ofsWriter.map{writer => allocateCU(writer.controlNode)}
    var lenCUOpt = lenWriter.map{writer => allocateCU(writer.controlNode)}

    if (ofsCUOpt.isEmpty && lenCUOpt.isEmpty) {
      val cu = allocateCU(pipe)
      cu.deps = Nil // Both are constants, so no dependencies
      ofsCUOpt = cu
      lenCUOpt = cu
    }
    else if (lenCUOpt.isEmpty && ofsCUOpt.isDefined) lenCUOpt = ofsCUOpt
    else if (lenCUOpt.isDefined && ofsCUOpt.isEmpty) ofsCUOpt = lenCUOpt
    val lenCU = lenCUOpt.get
    val ofsCU = ofsCUOpt.get

    val dram = allocateDRAM(pipe, mem, mode)

    val mcOfs = fresh[Index]
    ofsCU.addReg(mcOfs, ScalarOut(DRAMOffset(dram)))
    val ofsReg = ofs match {case Deff(Reg_read(reg)) => reg; case ofs => ofs }

    val mcLen = fresh[Index]
    lenCU.addReg(mcLen, ScalarOut(DRAMLength(dram)))
    val lenReg = len match {case Deff(Reg_read(reg)) => reg; case len => len }

    ofsCU.computeStages += OpStage(Bypass, List(ofsReg), mcOfs)
    lenCU.computeStages += OpStage(Bypass, List(lenReg), mcLen)

    // HACK- no dependents of ofsCU or lenCU
    mapping.values.foreach{cu =>
      cu.deps = cu.deps.filterNot{dep => dep == ofsCU || dep == lenCU }
    }
  }

  def prescheduleGather(pipe: Symbol, mem: Symbol, local: Symbol, addrs: Symbol, len: Symbol) {
    val cu = allocateCU(pipe)
    val dram = allocateDRAM(pipe, mem, MemGather)

    val n = cu.getOrElse(len){ allocateLocal(len, pipe) }
    val ctr = CUCounter(ConstReg("0i"), localScalar(n), ConstReg("1i"))
    val cc  = CChainInstance(quote(pipe)+"_cc", List(ctr))
    cu.cchains += cc
    val i = CounterReg(cc, 0)
    cu.regs += i


    val addr = allocateReadSRAM(pipe, addrs, cu)
    addr.readAddr = Some(i)

    val addrIn = fresh[Any]
    cu.addReg(addrIn, SRAMReadReg(addr))

    val addrOut = fresh[Any]
    cu.addReg(addrOut, VectorOut(DRAMAddress(dram)))

    cu.computeStages += OpStage(Bypass, List(addrIn), addrOut)

    readersOf(local).foreach{reader =>
      val readerCU = allocateCU(reader.controlNode)
      copyIterators(readerCU, cu)

      val sram = allocateMem(local, reader.node, readerCU)
      sram.mode = FIFOOnWriteMode
      sram.writeAddr = None
      sram.vector = Some(DRAMDataIn(dram))
    }
  }

  def prescheduleScatter(pipe: Symbol, mem: Symbol, local: Symbol, addrs: Symbol, len: Symbol) {
    val cu = allocateCU(pipe)
    val dram = allocateDRAM(pipe, mem, MemScatter)

    val n = cu.getOrElse(len){ allocateLocal(len, pipe) }
    val ctr = CUCounter(ConstReg("0i"), localScalar(n), ConstReg("1i"))
    val cc  = CChainInstance(quote(pipe)+"_cc", List(ctr))
    cu.cchains += cc
    val i = CounterReg(cc, 0)
    cu.regs += i

    val addr = allocateReadSRAM(pipe, addrs, cu)
    val data = allocateReadSRAM(pipe, local, cu)
    addr.readAddr = Some(i)
    data.readAddr = Some(i)

    val addrIn = fresh[Any]
    val dataIn = fresh[Any]
    cu.addReg(addrIn, SRAMReadReg(addr))
    cu.addReg(dataIn, SRAMReadReg(data))

    val addrOut = fresh[Any]
    val dataOut = fresh[Any]
    cu.addReg(addrOut, VectorOut(DRAMAddress(dram)))
    cu.addReg(dataOut, VectorOut(DRAMDataOut(dram)))

    cu.computeStages += OpStage(Bypass, List(addrIn), addrOut)
    cu.computeStages += OpStage(Bypass, List(dataIn), dataOut)
  }


  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reg_read(EatAlias(reg)) if isArgIn(reg) =>
      prescheduleRegisterRead(reg, lhs, None)

    case Hwblock(func) =>
      prescheduleStages(lhs, func)

    case UnitPipe(func) =>
      prescheduleStages(lhs, func)

    case UnrolledForeach(cc, func, inds, vs) =>
      prescheduleStages(lhs, func)

    case UnrolledReduce(cc, accum, func, rFunc, inds, vs, acc, rV) =>
      prescheduleStages(lhs, func)

    case BurstLoad(mem,fifo,ofs,len,p) =>
      prescheduleBurstTransfer(lhs, mem, ofs, len, MemLoad)

    case BurstStore(mem,fifo,ofs,len,p) =>
      prescheduleBurstTransfer(lhs, mem, ofs, len, MemStore)

    case Scatter(mem, local, addrs, len, _, i) =>
      prescheduleScatter(lhs, mem, local, addrs, len)

    case Gather(mem, local, addrs, len, _, i) =>
      prescheduleGather(lhs, mem, local, addrs, len)

    // Something bad happened if these are still in the IR
    case _:OpForeach => throw new Exception(s"Disallowed compact op $lhs = $rhs")
    case _:OpReduce[_,_] => throw new Exception(s"Disallowed compact op $lhs = $rhs")
    case _:OpMemReduce[_,_] => throw new Exception(s"Disallowed compact op $lhs = $rhs")
    case _ => super.traverse(lhs, rhs)
  }


  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    top = None
    mapping.clear()
    globals = Set.empty
    super.preprocess(b)
  }

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    for (cu <- mapping.values) {
      for (sram <- cu.srams) {
        initializeSRAM(sram, sram.mem, sram.reader, cu)
      }
    }

    super.postprocess(b)
  }
}
package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

trait PIRScheduler extends PIRTraversal {
  val IR: SpatialExp with PIRCommonExp
  import IR.{assert => _, _}

  override val name = "PIR Scheduler"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mappingIn  = mutable.HashMap[Symbol, PCU]()
  val mappingOut = mutable.HashMap[Symbol, CU]()

  override def run[A:Manifest](b: Block[A]): Block[A] = {
    super.run(b)
    val cuMapping = mappingIn.keys.map{s => mappingIn(s).asInstanceOf[ACU] -> mappingOut(s).asInstanceOf[ACU] }.toMap

    // Swap dependencies, parents, cchain owners from pcu to cu
    swapCUs(mappingOut.values, cuMapping)
    b
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && mappingIn.contains(lhs))
      schedulePCU(lhs, mappingIn(lhs))
  }

  def schedulePCU(pipe: Symbol, pcu: PCU) {
    val cu = pcu.copyToConcrete()

    if (debugMode) {
      debug(s"\n\n\n")
      debug(s"Scheduling $pipe CU: $pcu")
      for ((s,r) <- cu.regTable) {
        debug(s"  $s -> $r")
      }
      debug(s"  Write stages:")
      for ((k,v) <- pcu.writeStages) {
        debug(s"    Memories: " + k.mkString(", "))
        for (stage <- v._2) debug(s"      $stage")
      }
      debug(s"  Compute stages:")
      for (stage <- pcu.computeStages) debug(s"    $stage")
    }

    val origRegs = cu.regs
    var writeStageRegs = cu.regs

    // --- Schedule write contexts
    for ((srams,grp) <- pcu.writeStages) {
      val writer = grp._1
      val stages = grp._2
      val ctx = WriteContext(cu, writer, srams)
      ctx.init()
      stages.foreach{stage => scheduleStage(stage, ctx) }

      writeStageRegs ++= cu.regs
      cu.regs = origRegs
    }

    // --- Schedule compute context
    val ctx = ComputeContext(cu)
    pcu.computeStages.foreach{stage => scheduleStage(stage, ctx) }
    cu.regs ++= writeStageRegs

    mappingOut += pipe -> cu

    if (debugMode) {
      if (cu.srams.nonEmpty) {
        debug(s"SRAMs: ")
        for (sram <- cu.srams) {
          debug(s"""  $sram [${sram.mode}]""")
          debug(s"""    banking   = ${sram.banking.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    vector    = ${sram.vector.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    writeAddr = ${sram.writeAddr.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    readAddr  = ${sram.readAddr.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    start     = ${sram.writeStart.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    end       = ${sram.writeEnd.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    swapWrite = ${sram.swapWrite.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    swapRead  = ${sram.swapRead.map(_.toString).getOrElse("N/A")}""")
          debug(s"""    writeCtrl = ${sram.writeCtrl.map(_.toString).getOrElse("N/A")}""")
        }
      }

      for (srams <- cu.writeStages.keys) {
        debug(s"Generated write stages ($srams): ")
        cu.writeStages(srams).foreach(stage => debug(s"  $stage"))
      }
      debug("Generated compute stages: ")
      cu.computeStages.foreach(stage => debug(s"  $stage"))

      debug(s"CU global inputs:")
      globalInputs(cu).foreach{in => debug(s"  $in") }
    }
  }

  def scheduleStage(stage: PseudoStage, ctx: CUContext) = stage match {
    case DefStage(lhs@Deff(rhs), isReduce) =>
      //debug(s"""$lhs = $rhs ${if (isReduce) "[REDUCE]" else ""}""")
      if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
      else          mapNodeToStage(lhs,rhs,ctx)

    case WriteAddrStage(mem, addr) =>
      //debug(s"$mem @ $addr [WRITE]")
      writeAddrToStage(mem, addr, ctx)

    case FifoOnWriteStage(mem, start, end) =>
      //debug(s"$mem [WRITE]")
      fifoOnWriteToStage(mem, start, end, ctx)

    case OpStage(op, ins, out, isReduce) =>
      //debug(s"""$out = $op(${ins.mkString(",")}) [OP]""")
      opStageToStage(op, ins, out, ctx, isReduce)
  }

  // If addr is a counter or const, just returns that register back. Otherwise returns address wire
  def allocateAddrReg(sram: CUMemory, addr: Symbol, ctx: CUContext, write: Boolean, local: Boolean = false) = {
    val wire = if (write && local) FeedbackAddrReg(sram)
               else if (write)     WriteAddrWire(sram)
               else                ReadAddrWire(sram)

    val addrReg = ctx.reg(addr)
    propagateReg(addr, addrReg, wire, ctx)
  }

  def writeAddrToStage(mem: Symbol, addr: Symbol, ctx: CUContext, local: Boolean = false) {
    ctx.memories(mem).foreach{sram =>
      sram.writeAddr = Some(allocateAddrReg(sram, addr, ctx, write=true, local=local))
    }
  }
  def fifoOnWriteToStage(mem: Symbol, start: Option[Symbol], end: Option[Symbol], ctx: CUContext) {
    ctx.memories(mem).foreach{sram =>
      if (sram.mode != FIFOMode) sram.mode = FIFOOnWriteMode
      sram.writeAddr = None
      sram.writeCtrl = None
      sram.swapWrite = None

      start match {
        case Some(e) => ctx.reg(e) match {
          case x: LocalScalar => sram.writeStart = Some(x)
          case x => throw new Exception(s"Invalid FIFO-on-write start $x")
        }
        case None => // Do nothing
      }
      end match {
        case Some(e) => ctx.reg(e) match {
          case x: LocalScalar => sram.writeEnd = Some(x)
          case x => throw new Exception(s"Invalid FIFO-on-write start $x")
        }
        case None => // No end
      }
    }
  }

  def bufferWrite(mem: Symbol, value: Symbol, ndAddr: Option[Symbol], ctx: CUContext) {
    if (isReadInPipe(mem, ctx.pipe)) {
      val flatOpt = ndAddr.map{a => flattenNDAddress(a, dimsOf(mem)) }
      val addr = flatOpt.map(_._1)
      val addrStages = flatOpt.map(_._2).getOrElse(Nil)
      addrStages.foreach{stage => scheduleStage(stage, ctx) }
      addr.foreach{a => writeAddrToStage(mem, a, ctx, local=true) }

      // TODO: Should we allow multiple versions of local accumulator?
      ctx.memories(mem).foreach{sram =>
        propagateReg(value, ctx.reg(value), FeedbackDataReg(sram), ctx)
      }
    }
    // Push data out to global bus (even for local accumulators)
    if (ctx.isUnit) {
      val bus = CUScalar(quote(mem))
      globals += bus
      propagateReg(value, ctx.reg(value), ScalarOut(bus), ctx)
    }
    else {
      val bus = CUVector(quote(mem))
      globals += bus
      propagateReg(value, ctx.reg(value), VectorOut(bus), ctx)
    }
  }

  def allocateFifoPop(lhs: Symbol, fifo: Symbol, ctx: CUContext) = writersOf(fifo).head.controlNode match {
    case tx@Deff(e:BurstLoad[_]) =>
      val dram = allocateDRAM(tx, e.mem, MemLoad)
      ctx.addReg(lhs, VectorIn(DRAMDataIn(dram)))

    case x => ctx.memOption(fifo, lhs) match {
      case Some(sram) =>
        ctx.addReg(lhs, SRAMReadReg(sram))

      case None =>
        if (isUnitPipe(x)) {
          val bus = CUScalar(quote(fifo))
          globals += bus
          ctx.addReg(lhs, ScalarIn(bus))
        }
        else {
          val bus = CUVector(quote(fifo))
          globals += bus
          ctx.addReg(lhs, VectorIn(bus))
        }
    }
  }

  def allocateSRAMRead(lhs: Symbol, mem: Symbol, ndAddr: Symbol, ctx: CUContext) {
    val sram = ctx.mem(mem, lhs)
    val (addr, addrStages) = flattenNDAddress(ndAddr, dimsOf(mem))
    addrStages.foreach{stage => scheduleStage(stage, ctx) }
    sram.readAddr = Some(allocateAddrReg(sram, addr, ctx, write=false, local=true))
    ctx.addReg(lhs, SRAMReadReg(sram))
  }

  def allocateFifoPush(fifo: Symbol, value: Symbol, ctx: CUContext) = readersOf(fifo).head.controlNode match {
    case tx@Deff(e:BurstStore[_]) =>
      val dram = allocateDRAM(tx, e.mem, MemStore)
      propagateReg(value, ctx.reg(value), VectorOut(DRAMDataOut(dram)), ctx)

    case _ => bufferWrite(fifo,value,None,ctx)
  }

  // Cases: 1. Inner Accumulator (read -> write)
  //        2. Outer Accumulator (read -> write)
  //        3. Local register but not accumulator (e.g. write -> read)
  //        4. Scalar output (read globally)
  // (1, 2, and 3 are mutually exclusive)
  // - 1: Ignore. Inner reductions are handled differently
  // - 2: Update producer of value to have accumulator as output
  // - 3: Update reg to map to register of value (for later use in reads)
  // - 4: If any of first 3 options, add bypass value to scalar out, otherwise update producer
  def allocateRegWrite(writer: Symbol, reg: Symbol, value: Symbol, ctx: CUContext) {
    val isLocallyRead    = isReadInPipe(reg, ctx.pipe)
    val isLocallyWritten = isWrittenInPipe(reg, ctx.pipe, Some(writer)) // Always true?
    val isInnerAcc = isInnerAccum(reg) && isLocallyRead && isLocallyWritten
    val isOuterAcc = isAccum(reg) && !isInnerAcc && isLocallyRead && isLocallyWritten
    val isRemotelyRead = isReadOutsidePipe(reg, ctx.pipe)

    val Deff(d) = writer

    debug(s"[REG WRITE] $writer = $d")
    debug(s"  Reg = $reg, value = $value")
    debug(s"  localRead:$isLocallyRead, localWrite:$isLocallyWritten, innerAcc:$isInnerAcc, outerAcc:$isOuterAcc, remoteRead:$isRemotelyRead")

    if (isOuterAcc) { // Case 2
      val out = ctx.cu.getOrElse(reg){ allocateLocal(reg, ctx.pipe) }
      propagateReg(value, ctx.reg(value), out, ctx)
    }
    else if (!isInnerAcc) { // Case 3
      ctx.addReg(reg, ctx.reg(value))
    }
    if (isRemotelyRead) {
      // Handle registers for cross-lane accumulation specially
      val start = if (isInnerAcc) ctx.reg(value) else ctx.reg(reg)
      if (isArgOut(reg)) {
        val bus = OutputArg(quote(reg))
        globals += bus
        propagateReg(reg, start, ScalarOut(bus), ctx)
      }
      else if (ctx.isUnit || isInnerAcc) {
        val bus = CUScalar(quote(reg))
        globals += bus
        propagateReg(reg, start, ScalarOut(bus), ctx)
      }
      else {
        val bus = CUVector(quote(reg))
        globals += bus
        propagateReg(reg, start, VectorOut(bus), ctx)
      }
    }
  }


  def mapNodeToStage(lhs: Symbol, rhs: Def[Any], ctx: CUContext) = rhs match {
    // --- Reads
    case Pop_fifo(EatAlias(fifo), en)     => allocateFifoPop(lhs, fifo, ctx)
    case Par_pop_fifo(EatAlias(fifo), en) => allocateFifoPop(lhs, fifo, ctx)

    case Sram_load(EatAlias(mem), addr)     => allocateSRAMRead(lhs, mem, addr, ctx)
    case Par_sram_load(EatAlias(mem), addr) => allocateSRAMRead(lhs, mem, addr, ctx)

    case ListVector(elems) => ctx.addReg(lhs, ctx.reg(elems.head))
    case Vec_apply(vec, idx) =>
      if (idx != 0) throw new Exception("Expected parallelization of 1 in inner loop in PIR gen")
      ctx.addReg(lhs, ctx.reg(vec))

    case Reg_read(EatAlias(reg)) =>
      val input = ctx.cu.getOrElse(reg){ allocateLocal(reg, ctx.pipe, read=Some(lhs)) }
      ctx.addReg(lhs, input)

    // --- Writes
    // Only for the data, not for the address, unless the memory is a local accumulator
    // TODO: Support enables!
    case Push_fifo(EatAlias(fifo), value, en)          => allocateFifoPush(fifo, value, ctx)
    case Par_push_fifo(EatAlias(fifo), values, ens, _) => allocateFifoPush(fifo, values, ctx)

    case Sram_store(EatAlias(mem), addr, value, en)        => bufferWrite(mem,value,Some(addr),ctx)
    case Par_sram_store(EatAlias(mem), addrs, values, ens) => bufferWrite(mem,values,Some(addrs),ctx)

    case Reg_write(EatAlias(reg), value, en) => allocateRegWrite(lhs, reg, value, ctx)

    // --- Constants
    case c if isConstant(lhs) => ctx.cu.getOrElse(lhs){ allocateLocal(lhs, ctx.pipe) }

    // --- All other ops
    // Negation isn't an op in plasticine ISA right now
    case FltPt_Neg(x) => opStageToStage(FltMul, List(Const(-1f), x), lhs, ctx, false)
    case FixPt_Neg(x) => opStageToStage(FixMul, List(Const(-1), x), lhs, ctx, false)

    case d => nodeToOp(d) match {
      case Some(op) =>
        val inputs = syms(rhs)
        opStageToStage(op, inputs, lhs, ctx, false)

      case None => stageWarn(s"No ALU operation known for $lhs = $rhs")
    }
  }

  def reduceNodeToStage(lhs: Symbol, rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) => opStageToStage(op, syms(rhs), lhs, ctx, true)
    case _ => stageWarn(s"No ALU reduce operation known for $lhs = $rhs")
  }

  def opStageToStage(op: PIROp, ins: List[Symbol], out: Symbol, ctx: CUContext, isReduce: Boolean) {
    if (isReduce) {
      // By convention, the inputs to the reduction tree is the first argument to the node
      // This input must be in the previous stage's reduction register
      // Ensure this either by adding a bypass register for raw inputs or changing the output
      // of the previous stage from a temporary register to the reduction register
      debug(s"[REDUCE] $op, ins = $ins, out = $out")

      val input = ins.head
      val accum = aliasOf(ins.last)
      val inputReg = ctx.reg(input)
      val usedInput = propagateReg(input, inputReg, ReduceReg(), ctx)
      val zero = accum match {
        case Deff(Reg_read(reg)) => ConstReg(extractConstant(resetValue(reg)))
        case _ => ConstReg("0l")
      }
      val acc = ReduceReg()
      val stage = ReduceStage(op, zero, ctx.refIn(usedInput), acc)
      ctx.addReg(out, acc)
      ctx.addStage(stage)
    }
    else if (op == Bypass) {
      assert(ins.length == 1)
      propagateReg(ins.head, ctx.reg(ins.head), ctx.reg(out), ctx)
    }
    else {
      val inputRegs = ins.map{in => ctx.reg(in) }
      val isControlStage  = inputRegs.nonEmpty && !inputRegs.exists{reg => !isControl(reg) }
      val hasControlLogic = inputRegs.nonEmpty && inputRegs.exists{reg => isControl(reg) }

      if (isControlStage) {
        val n = ctx.controlStageNum
        val inputs = inputRegs.map{reg => ctx.refIn(reg, n) }
        val output = ctx.cu.getOrElse(out){ ControlReg() }
        val stage = MapStage(op, inputs, List(ctx.refOut(output, n)))
        ctx.addControlStage(stage)
      }
      // HACK: Skip control logic generation for now...
      else if (hasControlLogic && op == ALUMux) {
        val skip = inputRegs.drop(1).find{case _:ConstReg => false; case _ => true}
        ctx.addReg(out, skip.getOrElse(inputRegs.drop(1).head))
      }
      else if (hasControlLogic) {
        throw new Exception("Cannot skip control logic...")
      }
      else {
        val inputs = inputRegs.map{reg => ctx.refIn(reg) }
        val output = ctx.cu.getOrElse(out){ TempReg() }
        val stage = MapStage(op, inputs, List(ctx.refOut(output)))
        ctx.addStage(stage)
      }
    }
  }
}

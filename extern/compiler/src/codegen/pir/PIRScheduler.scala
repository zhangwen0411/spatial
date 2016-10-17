package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait PIRScheduler extends Traversal with PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Scheduler"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cus = HashMap[Exp[Any], ComputeUnit]()

  def allocateCU(pipe: Exp[Any]): ComputeUnit = cus(pipe)

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      scheduleCU(lhs, cus(lhs))
  }

  def scheduleCU(pipe: Exp[Any], cu: ComputeUnit) {
    debug(s"Scheduling $pipe CU: $cu")

    val origRegs = cu.regs
    var writeStageRegs = cu.regs

    // --- Schedule write contexts
    for (srams <- cu.writePseudoStages.keys) {
      val writes = WriteContext(cu, srams)
      scheduleContext(writes)
      writeStageRegs ++= cu.regs // Reset view of registers each time
      cu.regs = origRegs
    }
    // --- Schedule compute context
    val compute = ComputeContext(cu)
    scheduleContext(compute)
    cu.regs ++= writeStageRegs

    for (srams <- cu.writePseudoStages.keys) {
      debug(s"Generated write stages ($srams): ")
      cu.writeStages(srams).foreach(stage => debug(s"  $stage"))
    }
    debug("Generated compute stages: ")
    cu.stages.foreach(stage => debug(s"  $stage"))
  }

  def scheduleContext(ctx: CUContext) {
    debug(s"  Scheduling context $ctx")
    ctx.pseudoStages.foreach {stage => scheduleStage(stage, ctx) }
  }

  def scheduleStage(stage: PseudoStage, ctx: CUContext): Unit = stage match {
    case DefStage(lhs@Deff(rhs), isReduce) =>
      debug(s"""    $lhs = $rhs ${if (isReduce) "[REDUCE]" else ""}""")
      if (isReduce) reduceNodeToStage(lhs,rhs,ctx)
      else          mapNodeToStage(lhs,rhs,ctx)

    case WriteAddrStage(mem, addr) =>
      debug(s"""    $mem @ $addr [WRITE]""")
      writeAddrToStage(mem, addr, ctx)

    case OpStage(op, ins, out, isReduce) =>
      debug(s"""    $out = $op(${ins.mkString(",")}) [OP]""")
      opStageToStage(op, ins, out, ctx, isReduce)
  }


  // Given result register type A, reroute to type B as necessary
  def propagateReg(exp: Exp[Any], a: LocalMem, b: LocalMem, ctx: CUContext) = (a,b) match {
    case (a:ScalarOut, b:ScalarOut) => a
    case (a:VectorOut, b:VectorOut) => a
    case (a:VectorLocal, b:VectorLocal) => a
    case (_:ReduceReg | _:AccumReg, _:ReduceReg | _:AccumReg) => a

    // Propagating from read addr wire to another read addr wire is ok (but should usually never happen)
    case (a:ReadAddrWire, b:ReadAddrWire) => ctx.addOutput(exp,a,b); b
    case (a,b) if !isReadable(a) => throw new Exception(s"Cannot propagate for $exp from output-only $a")
    case (a,b) if !isWritable(b) => throw new Exception(s"Cannot propagate for $exp to input-only $b")

    // TODO: Are these necessary? Do we ever see multiple writes to the same output?
    case (a, ScalarOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorOut(x,_)) => ctx.addOutput(x,a,b); b
    case (a, VectorLocal(x,_)) => ctx.addOutput(x,a,b); b

    case (a, b:TempReg) => a

    // Special cases: don't propagate to write/read wires from counters or constants
    case (_:CounterReg | _:ConstReg, _:WriteAddrWire | _:ReadAddrWire) => a
    // General case for outputs: Don't add mapping for exp to output
    case (a,b) if !isReadable(b) => ctx.addOutput(exp,a,b,add=false); b

    case (a,b) => ctx.addOutput(exp,a,b); b
  }

  // If addr is a counter or const, just returns that register back. Otherwise returns address wire
  def allocateAddrReg(sram: CUMemory, addr: Exp[Any], ctx: CUContext, write: Boolean, local: Boolean = false) = {
    val wire = if (write && local) LocalWriteReg(sram)
               else if (write)     WriteAddrWire(sram)
               else                ReadAddrWire(sram)
    val addrReg = ctx.reg(addr)
    propagateReg(addr, addrReg, wire, ctx)
  }

  // Addresses only, not values
  def writeAddrToStage(mem: Exp[Any], addr: Exp[Any], ctx: CUContext, local: Boolean = false) = {
    ctx.memories(mem).foreach{sram =>
      sram.writeAddr = Some(allocateAddrReg(sram, addr, ctx, write=true, local=local))
    }
  }

  def bufferWrite(mem: Exp[Any], value: Exp[Any], ndAddr: Option[Exp[Any]], ctx: CUContext) {
    if (isReadInPipe(mem, ctx.pipe)) {
      val flatOpt = ndAddr.map{a => flattenNDAddress(a, dimsOf(mem)) }
      val addr = flatOpt.map(_._1)
      val addrStages = flatOpt.map(_._2).getOrElse(Nil)
      addrStages.foreach{stage => scheduleStage(stage, ctx) }
      addr.foreach{a => writeAddrToStage(mem, a, ctx, local=true) }

      // TODO: Should we allow multiple versions of local accumulator?
      ctx.memories(mem).foreach{sram =>
        propagateReg(value, ctx.reg(value), VectorLocal(fresh[Any], sram), ctx)
      }
    }
    if (isReadOutsidePipe(mem, ctx.pipe)) { // Should always be true?
      val vector = allocateGlobal(mem)
      propagateReg(value, ctx.reg(value), VectorOut(fresh[Any], vector), ctx)
    }
  }

  def mapNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = rhs match {
    // --- Reads
    case Pop_fifo(EatAlias(fifo), en) =>
      val vector = allocateGlobal(fifo).asInstanceOf[VectorMem]
      ctx.addReg(lhs, VectorIn(vector))

    case Par_pop_fifo(EatAlias(fifo), en) =>
      val vector = allocateGlobal(fifo).asInstanceOf[VectorMem]
      ctx.addReg(lhs, VectorIn(vector))

    // Create a reference to this SRAM
    case Sram_load(EatAlias(ram), ndAddr) =>
      val sram = ctx.mem(ram,lhs)
      // Insert flat address computation
      val (addr, addrStages) = flattenNDAddress(ndAddr, dimsOf(ram))
      addrStages.foreach{stage => scheduleStage(stage, ctx) }
      sram.readAddr = Some(allocateAddrReg(sram, addr, ctx, write=false, local=true))
      ctx.addReg(lhs, SRAMRead(sram))

    case Par_sram_load(EatAlias(ram), ndAddrs) =>
      val sram = ctx.mem(ram,lhs)
      // Insert flat address computation
      val (addr, addrStages) = flattenNDAddress(ndAddrs, dimsOf(ram))
      addrStages.foreach{stage => scheduleStage(stage, ctx) }
      sram.readAddr = Some(allocateAddrReg(sram, addr, ctx, write=false, local=true))
      ctx.addReg(lhs, SRAMRead(sram))

    case ListVector(elems) => ctx.addReg(lhs, ctx.reg(elems.head))

    case Vec_apply(vec, idx) =>
      if (idx != 0) stageError("Expected parallelization of 1 in inner loop in PIR generation")
      ctx.addReg(lhs, ctx.reg(vec))

    // Register read is not a "true" node in PIR
    // True register reads happen at the beginning of ALU operations
    // So just make this node alias with the register
    case Reg_read(EatAlias(reg)) =>
      val input = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe, read=Some(lhs)) }
      ctx.addReg(lhs, input)

    // --- Writes
    // Only for the values, not the addresses UNLESS these are local accumulators
    // TODO: Filters
    case Push_fifo(EatAlias(fifo),value,en) =>
      bufferWrite(fifo,value,None,ctx)
    case Par_push_fifo(EatAlias(fifo),values,ens,_) =>
      bufferWrite(fifo,values,None,ctx)

    case Sram_store(EatAlias(sram), addr, value, ens) =>
      bufferWrite(sram,value,Some(addr),ctx)
    case Par_sram_store(EatAlias(sram), addrs, values, ens) =>
      bufferWrite(sram,values,Some(addrs),ctx)

    // Cases: 1. Inner Accumulator (read -> write)
    //        2. Outer Accumulator (read -> write)
    //        3. Local register but not accumulator (e.g. write -> read)
    //        4. Scalar output (read globally)
    // (1, 2, and 3 are mutually exclusive)
    // - 1: (Nothing, inner reductions are handled differently)
    // - 2: Update producer of value to have accumulator as output
    // - 3: Update reg to map to register of value (for later use in reads)
    // - 4: If any of first 3 options, add bypass value to scalar out, otherwise update producer
    case Reg_write(EatAlias(reg), value, en) =>
      val isLocallyRead = isReadInPipe(reg, ctx.pipe)
      val isLocallyWritten = isWrittenInPipe(reg, ctx.pipe, Some(lhs)) // Always true?
      val isInnerAcc = isInnerAccum(reg) && isLocallyRead && isLocallyWritten
      val isOuterAcc = isAccum(reg) && !isInnerAcc && isLocallyRead && isLocallyWritten
      val isRemotelyRead = isReadOutsidePipe(reg, ctx.pipe)

      if (isOuterAcc) { // Case 2
        val out = ctx.cu.getOrAddReg(reg){ allocateLocal(reg, ctx.pipe) }
        propagateReg(value, ctx.reg(value), out, ctx)
      }
      else if (!isInnerAcc) { // Case 3
        ctx.addReg(reg, ctx.reg(value)) // Forward refs to reg to value
      }
      if (isRemotelyRead) { // Case 4
        val scalar = allocateGlobal(reg)
        val out = ScalarOut(fresh[Any], scalar)
        if (isInnerAcc)
          propagateReg(reg, ctx.reg(value), out, ctx) // Bypass
        else
          propagateReg(reg, ctx.reg(reg), out, ctx)
      }

    case _ => lhs match {
      case Fixed(_) => ctx.cu.getOrAddReg(lhs){ allocateLocal(lhs, ctx.pipe) }
      case Def(ConstBit(_)) => ctx.cu.getOrAddReg(lhs){ allocateLocal(lhs, ctx.pipe) }

      // Negation isn't an op in the plasticine ISA right now
      case Def(FltPt_Neg(x)) =>
        val in = ctx.reg(x)
        val c = ConstReg("-1f")
        val out = ctx.cu.getOrAddReg(lhs){ TempReg(lhs) }
        val stage = MapStage(FltMul, List(ctx.refIn(in), ctx.refIn(c)), List(ctx.refOut(out)))
        ctx.addStage(stage)

      case Def(FixPt_Neg(x)) =>
        val in = ctx.reg(x)
        val c = ConstReg("-1i")
        val out = ctx.cu.getOrAddReg(lhs){ TempReg(lhs) }
        val stage = MapStage(FixMul, List(ctx.refIn(in), ctx.refIn(c)) , List(ctx.refOut(out)))
        ctx.addStage(stage)

      case _ => nodeToOp(rhs) match {
        case Some(op) =>
          val inputs = syms(rhs)
          opStageToStage(op, inputs, lhs, ctx, false)

        case None => stageWarn(s"No ALU operation known for $lhs = $rhs")
      }
    }
  }

  def reduceNodeToStage(lhs: Exp[Any], rhs: Def[Any], ctx: CUContext) = nodeToOp(rhs) match {
    case Some(op) => opStageToStage(op, syms(rhs), lhs, ctx, true)
    case _ => stageWarn(s"No ALU operation known for $lhs = $rhs")
  }

  // FIXME: Assumes a single node reduction function right now, change to allow multi-node later
  def opStageToStage(op: PIROp, ins: List[Exp[Any]], out: Exp[Any], ctx: CUContext, isReduce: Boolean) = {
    if (isReduce) {
      // By convention, the inputs to the reduction tree is the first argument to the node
      // This input must be in the previous stage's reduction register
      // Ensure this either by adding a bypass register for raw inputs or changing the output
      // of the previous stage from a temporary register to the reduction register
      val input = ins.head
      val accum = ins.last
      val inputReg = ctx.reg(input)
      val usedInput = propagateReg(input, inputReg, ReduceReg(fresh[Any]), ctx)
      val zero = accum match {
        case Deff(Reg_read(acc)) => allocateConst(resetValue(acc))
        case _ => ConstReg("0l")
      }
      val acc = ReduceReg(out)
      val stage = ReduceStage(op, zero, ctx.refIn(usedInput), acc)
      ctx.addReg(out, acc)
      ctx.addStage(stage)
    }
    else {
      val inputRegs = ins.map{in => ctx.reg(in) }
      val isControlStage = inputRegs.nonEmpty && !inputRegs.exists{reg => !isControl(reg)}

      if (isControlStage) {
        val n = ctx.controlStageNum
        val inputs = inputRegs.map{reg => ctx.refIn(reg, n)}
        val output = ctx.cu.getOrAddReg(out){ ControlReg(out) }
        val stage = MapStage(op, inputs, List(ctx.refOut(output, n)))
        ctx.addControlStage(stage)
      }
      else {
        val inputs = inputRegs.map{reg => ctx.refIn(reg) }
        val output = ctx.cu.getOrAddReg(out){ TempReg(out) }
        val stage = MapStage(op, inputs, List(ctx.refOut(output)))
        ctx.addStage(stage)
      }
    }
  }

}

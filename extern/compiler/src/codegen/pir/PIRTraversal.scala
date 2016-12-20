package spatial.compiler.ops
import spatial.compiler._

import scala.virtualization.lms.internal.QuotingExp
import scala.virtualization.lms.util.GraphUtil
import scala.collection.mutable

trait PIRTraversal extends ControllerTools with QuotingExp {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  //override def quote(x: Symbol) = super.quote(aliasOf(x))

  val LANES = 16         // Number of SIMD lanes per CU
  val REDUCE_STAGES = 5  // Number of stages required to reduce across all lanes


  // --- Allocating
  var globals = Set[GlobalComponent]()

  def allocateDRAM(ctrl: Symbol, dram: Symbol, mode: OffchipMemoryMode): MemoryController = {
    val region = OffChip(quote(dram))
    val mc = MemoryController(quote(ctrl), region, mode)
    globals += mc
    globals += region
    mc
  }

  private def allocateReg(reg: Symbol, pipe: Symbol, read: Option[Symbol] = None, write: Option[Symbol] = None): LocalComponent = {
    val isLocallyRead = isReadInPipe(reg, pipe, read)
    val isLocallyWritten = isWrittenInPipe(reg, pipe, write)

    if (isLocallyRead && isLocallyWritten && isInnerAccum(reg)) {
      ReduceReg()
    }
    else if (isLocallyRead && isLocallyWritten && isAccum(reg)) {
      val init = ConstReg(extractConstant(resetValue(reg.asInstanceOf[Exp[Reg[Any]]])))
      AccumReg(init)
    }
    else if (!isLocallyRead) {
      if (isUnitPipe(pipe) || isInnerAccum(reg)) {
        val bus = CUScalar(quote(reg))
        globals += bus
        ScalarOut(bus)
      }
      else {
        val bus = CUVector(quote(reg))
        globals += bus
        VectorOut(bus)
      }
    }
    else if (!isLocallyWritten) {
      if (isWrittenByUnitPipe(reg) || isInnerAccum(reg)) {
        val bus = CUScalar(quote(reg))
        globals += bus
        ScalarIn(bus).asInstanceOf[LocalComponent]  // Weird scala type error here
      }
      else {
        val bus = CUVector(quote(reg))
        globals += bus
        VectorIn(bus).asInstanceOf[LocalComponent]  // Weird scala type error here
      }
    }
    else {
      TempReg()
    }
  }

  def allocateLocal(x: Symbol, pipe: Symbol, read: Option[Symbol] = None,  write: Option[Symbol] = None): LocalComponent = x match {
    case c if isConstant(c) => ConstReg(extractConstant(x))
    case reg@Deff(Argin_new(_)) =>
      val bus = new InputArg(quote(reg))
      globals += bus
      ScalarIn(bus)

    case reg@Deff(Argout_new(_)) =>
      val bus = new OutputArg(quote(reg))
      globals += bus
      ScalarOut(bus)

    case reg@Deff(Reg_new(_))     => allocateReg(reg, pipe, read, write)
    case read@Deff(Reg_read(reg)) => allocateLocal(reg, pipe, Some(read), write)

    case _ => TempReg()
  }

  def foreachSymInBlock(b: Block[Any])(func: Sym[Any] => Unit) {
    focusBlock(b){
      focusExactScope(b){ stms =>
        stms.foreach{ case TP(lhs,rhs) => func(lhs) }
      }
    }
  }

  // HACK: Ignore simple effect scheduling dependencies in remote writes
  def getScheduleForAddress(stms: Seq[Stm])(addr: Symbol) = {
    def mysyms(rhs: Any) = rhs match {
      case d@Reflect(x, u, es) if u.maySimple =>
        val dataDeps = syms(x)
        val schedDeps = syms(es) filter {
          case Def(Reflect(_,uu,_)) => uu != Simple
          case _ => true
        }
        dataDeps ++ schedDeps
      case _ => syms(rhs)
    }
    val scopeIndex = buildScopeIndex(stms)
    def deps(x: Any) = scheduleDepsWithIndex(mysyms(x), scopeIndex)

    GraphUtil.stronglyConnectedComponents[Stm](deps(addr), t => deps(t.rhs)).flatten.reverse
  }

  // Build a schedule as usual, except for depencies on write addresses
  def symsOnlyUsedInWriteAddr(stms: Seq[Stm])(result: Symbol, exps: List[Symbol]) = {
    def mysyms(rhs: Any) = rhs match {
      case rhs: Def[_] => rhs match {
        case LocalWriter(writes) =>
          val addrs = writes.flatMap{case (mem,value,addr) => addr.filterNot{a => a == value }}
          syms(rhs) filterNot (addrs contains _)
        case _ => syms(rhs)
      }
      case _ => syms(rhs)
    }
    val scopeIndex = buildScopeIndex(stms)
    def deps(x: Any) = scheduleDepsWithIndex(mysyms(x), scopeIndex)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(result), t => deps(t.rhs)).flatten

    exps.filterNot{case sym: Sym[_] => xx.exists(_.defines(sym).isDefined) }
  }

  // HACK: Rip apart the block, looking only for true data dependencies and necessary effects dependencies
  def symsOnlyUsedInWriteAddrOrEn(stms: Seq[Stm])(result: Symbol, exps: List[Symbol]) = {
    def mysyms(rhs: Any) = rhs match {
      case d:Def[_] => d match {
        case EatReflect(Sram_store(sram,addr,value,en)) => syms(sram) ++ syms(value) //++ syms(es)
        case EatReflect(Par_sram_store(sram,addr,value,en)) => syms(sram) ++ syms(value) //++ syms(es)
        case EatReflect(Push_fifo(fifo, value, en))          => syms(fifo) ++ syms(value)
        case EatReflect(Par_push_fifo(fifo, values, ens, _)) => syms(fifo) ++ syms(values)
        case Reify(x,u,es) => syms( es.filterNot{case Def(Reflect(d,u,es)) => u.resAlloc; case _ => false})
        case _ => syms(d)
      }
      case _ => syms(rhs)
    }
    val scopeIndex = buildScopeIndex(stms)
    def deps(x: Any) = scheduleDepsWithIndex(mysyms(x), scopeIndex)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(result), t => deps(t.rhs)).flatten

    //xx.reverse.foreach{case TP(s,d) => debug(s"  $s = $d")}

    // Remove all parts of schedule which are used to calculate values
    exps.filterNot{case sym:Sym[_] => xx.exists(_.defines(sym).isDefined) }
  }



  // --- Transformation functions
  def removeComputeStages(cu: CU, remove: Set[Stage]) {
    val ctx = ComputeContext(cu)
    val stages = mutable.ArrayBuffer[Stage]()
    stages ++= ctx.stages
    cu.computeStages.clear()

    stages.foreach{
      case stage@MapStage(op,ins,outs) if !remove.contains(stage) =>
        stage.ins = ins.map{case LocalRef(i,reg) => ctx.refIn(reg) }
        stage.outs = outs.map{case LocalRef(i,reg) => ctx.refOut(reg) }
        ctx.addStage(stage)

      case stage@ReduceStage(op,init,in,acc) if !remove.contains(stage) =>
        ctx.addStage(stage)

      case _ => // This stage is being removed! Ignore it!
    }
  }


  def swapBus(cus: Iterable[CU], orig: GlobalBus, swap: GlobalBus) = cus.foreach{cu =>
    cu.allStages.foreach{stage => swapBus_stage(stage) }
    cu.srams.foreach{sram => swapBus_sram(sram) }
    cu.cchains.foreach{cc => swapBus_cchain(cc) }

    def swapBus_stage(stage: Stage): Unit = stage match {
      case stage@MapStage(op, ins, outs) =>
        stage.ins = ins.map{ref => swapBus_ref(ref) }
        stage.outs = outs.map{ref => swapBus_ref(ref) }
      case stage:ReduceStage => // No action
    }
    def swapBus_ref(ref: LocalRef): LocalRef = ref match {
      case LocalRef(i,reg) => LocalRef(i, swapBus_reg(reg))
    }
    def swapBus_reg(reg: LocalComponent): LocalComponent = (reg,swap) match {
      case (ScalarIn(`orig`),  swap: ScalarBus) => ScalarIn(swap)
      case (ScalarOut(`orig`), swap: ScalarBus) => ScalarOut(swap)
      case (VectorIn(`orig`),  swap: VectorBus) => VectorIn(swap)
      case (VectorOut(`orig`), swap: VectorBus) => VectorOut(swap)

      case (ScalarIn(x), _)  if x != orig => reg
      case (ScalarOut(x), _) if x != orig => reg
      case (VectorIn(x), _)  if x != orig => reg
      case (VectorOut(x), _) if x != orig => reg

      case (_:LocalPort[_], _) => throw new Exception(s"$swap is not a valid replacement for $orig")
      case _ => reg
    }

    def swapBus_writeAddr(addr: WriteAddr): WriteAddr = addr match {
      case reg: LocalComponent => swapBus_reg(reg).asInstanceOf[WriteAddr]
      case _ => addr
    }
    def swapBus_readAddr(addr: ReadAddr): ReadAddr = addr match {
      case reg: LocalComponent => swapBus_reg(reg).asInstanceOf[ReadAddr]
      case _ => addr
    }
    def swapBus_localScalar(sc: LocalScalar): LocalScalar = sc match {
      case reg: LocalComponent => swapBus_reg(reg).asInstanceOf[LocalScalar]
      case _ => sc
    }

    def swapBus_sram(sram: CUMemory): Unit = {
      sram.vector = sram.vector.map{case `orig` => swap; case vec => vec}
      sram.readAddr = sram.readAddr.map{reg => swapBus_readAddr(reg)}
      sram.writeAddr = sram.writeAddr.map{reg => swapBus_writeAddr(reg)}
      sram.writeStart = sram.writeStart.map{reg => swapBus_localScalar(reg)}
      sram.writeEnd = sram.writeEnd.map{reg => swapBus_localScalar(reg)}
    }
    def swapBus_cchain(cchain: CUCChain): Unit = cchain match {
      case cc: CChainInstance => cc.counters.foreach{ctr => swapBus_ctr(ctr)}
      case _ => // No action
    }
    def swapBus_ctr(ctr: CUCounter): Unit = {
      ctr.start = swapBus_localScalar(ctr.start)
      ctr.end = swapBus_localScalar(ctr.end)
      ctr.stride = swapBus_localScalar(ctr.stride)
    }
  }


  def swapCUs(cus: Iterable[CU], mapping: Map[ACU, ACU]): Unit = cus.foreach {cu =>
    cu.cchains.foreach{cchain => swapCU_cchain(cchain) }
    cu.parent = cu.parent.map{parent => mapping.getOrElse(parent,parent) }
    cu.deps = cu.deps.map{dep => mapping.getOrElse(dep, dep) }

    def swapCU_cchain(cchain: CUCChain) = cchain match {
      case cc: CChainCopy => cc.owner = mapping.getOrElse(cc.owner,cc.owner)
      case _ => // No action
    }
  }


  // --- Context for creating/modifying CUs
  abstract class CUContext(val cu: ComputeUnit) {
    private val refs = mutable.HashMap[Symbol,LocalRef]()
    private var readAccums: Set[AccumReg] = Set.empty

    def pipe: Symbol
    def stages: mutable.ArrayBuffer[Stage]
    def addStage(stage: Stage): Unit
    def isWriteContext: Boolean
    def init(): Unit

    def isUnit = cu.isUnit

    def stageNum: Int = stages.count{case stage:MapStage => true; case _ => false} + 1
    def controlStageNum: Int = controlStages.length
    def prevStage: Option[Stage] = stages.lastOption
    def mapStages: Iterator[MapStage] = stages.iterator.collect{case stage:MapStage => stage}

    def controlStages: mutable.ArrayBuffer[Stage] = cu.controlStages
    def addControlStage(stage: Stage): Unit = cu.controlStages += stage

    def addReg(x: Symbol, reg: LocalComponent) {
      debug(s"  $x -> $reg")
      cu.addReg(x, reg)
    }
    def addRef(x: Symbol, ref: LocalRef) { refs += x -> ref }
    def getReg(x: Symbol): Option[LocalComponent] = cu.get(x)
    def reg(x: Symbol): LocalComponent = cu.get(x).getOrElse(throw new Exception(s"No register defined for $x"))

    // Add a stage which bypasses x to y
    def bypass(x: LocalComponent, y: LocalComponent) {
      val stage = MapStage(Bypass, List(refIn(x)), List(refOut(y)))
      addStage(stage)
    }

    def ref(reg: LocalComponent, out: Boolean, stage: Int = stageNum): LocalRef = reg match {
      // If the previous stage computed the read address for this load, use the registered output
      // of the memory directly. Otherwise, use the previous stage
      case SRAMReadReg(sram) =>
        /*debug(s"Referencing SRAM $sram in stage $stage")
        debug(s"  Previous stage: $prevStage")
        debug(s"  SRAM read addr: ${sram.readAddr}")*/
        if (prevStage.isEmpty || sram.mode == FIFOMode)
          LocalRef(-1, reg)
        else {
          if (sram.mode != FIFOMode && sram.readAddr.isDefined) {
            if (prevStage.get.outputMems.contains(sram.readAddr.get))
              LocalRef(-1, reg)
            else
              LocalRef(stage-1,reg)
          }
          else
            throw new Exception(s"No address defined for SRAM $sram")
        }

      case reg: CounterReg if isWriteContext && prevStage.isEmpty =>
        //debug(s"Referencing counter $reg in first write stage")
        LocalRef(-1, reg)

      case reg: AccumReg if isUnreadAccum(reg) =>
        //debug(s"First reference to accumulator $reg in stage $stage")
        readAccums += reg
        LocalRef(stage, reg)
      case _ if out =>
        //debug(s"Referencing output register $reg in stage $stage")
        LocalRef(stage, reg)
      case _ =>
        //debug(s"Referencing input register $reg in stage $stage")
        LocalRef(stage-1, reg)
    }
    def refIn(reg: LocalComponent, stage: Int = stageNum) = ref(reg, out = false, stage)
    def refOut(reg: LocalComponent, stage: Int = stageNum) = ref(reg, out = true, stage)

    def addOutputFor(e: Symbol)(prev: LocalComponent, out: LocalComponent): Unit = addOutput(prev, out, Some(e))
    def addOutput(prev: LocalComponent, out: LocalComponent): Unit = addOutput(prev, out, None)
    def addOutput(prev: LocalComponent, out: LocalComponent, e: Option[Symbol]): Unit = {
      mapStages.find{stage => stage.outputMems.contains(prev) } match {
        case Some(stage) =>
          stage.outs ::= refOut(out, mapStages.indexOf(stage) + 1)
        case None =>
          bypass(prev, out)
      }
      if (e.isDefined) addReg(e.get, out)
      else cu.regs += out // No mapping, only list
    }

    // Get memory in this CU associated with the given reader
    def mem(mem: Symbol, reader: Symbol): CUMemory = {
      memOption(mem,reader).getOrElse(throw new Exception(s"Cannot find sram ($mem,$reader) in cu $cu"))
    }

    def memOption(mem: Symbol, reader: Symbol): Option[CUMemory] = {
      cu.srams.find{sram => sram.mem == mem && sram.reader == reader}
    }

    // A CU can have multiple SRAMs for a given mem symbol, one for each local read
    def memories(mem: Symbol) = readersOf(mem).filter(_.controlNode == cu.pipe).map{reader => this.mem(mem, reader.node) }


    // HACK: Keep track of first read of accum reg (otherwise can use the wrong stage)
    private def isUnreadAccum(reg: LocalComponent) = reg match {
      case reg: AccumReg => !readAccums.contains(reg)
      case _ => false
    }
  }


  case class ComputeContext(override val cu: ComputeUnit) extends CUContext(cu) {
    def stages = cu.computeStages
    def addStage(stage: Stage) { cu.computeStages += stage }
    def isWriteContext = false
    def pipe = cu.pipe
    def init() = {}
  }
  case class WriteContext(override val cu: ComputeUnit, pipe: Symbol, srams: List[CUMemory]) extends CUContext(cu) {
    def init() { cu.writeStages += srams -> mutable.ArrayBuffer[Stage]() }
    def stages = cu.writeStages(srams)
    def addStage(stage: Stage) { cu.writeStages(srams) += stage }
    def isWriteContext = true
  }


  // Given result register type A, reroute to type B as necessary
  def propagateReg(exp: Symbol, a: LocalComponent, b: LocalComponent, ctx: CUContext) = (a,b) match {
    case (a:ScalarOut, b:ScalarOut) => a
    case (a:VectorOut, b:VectorOut) => a
    case (a:FeedbackDataReg, b:FeedbackDataReg) => a
    case (_:ReduceReg | _:AccumReg, _:ReduceReg | _:AccumReg) => a

    // Propagating from read addr wire to another read addr wire is ok (but should usually never happen)
    case (a:ReadAddrWire, b:ReadAddrWire) => ctx.addOutputFor(exp)(a,b); b
    case (a,b) if !isReadable(a) => throw new Exception(s"Cannot propagate for $exp from output-only $a")
    case (a,b) if !isWritable(b) => throw new Exception(s"Cannot propagate for $exp to input-only $b")

    // Prefer reading original over a new temporary register
    case (a, b:TempReg) => a

    // Special cases: don't propagate to write/read wires from counters or constants
    case (_:CounterReg | _:ConstReg, _:WriteAddrWire | _:ReadAddrWire) => a

    // General case for outputs: Don't add mapping for exp to output
    case (a,b) if !isReadable(b) => ctx.addOutput(a,b); b

    // General case for all others: add output + mapping
    case (a,b) => ctx.addOutputFor(exp)(a,b); b
  }

}
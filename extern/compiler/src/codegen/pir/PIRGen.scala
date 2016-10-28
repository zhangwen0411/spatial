package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{ArrayBuffer, HashMap}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import java.io.PrintWriter
import ppl.delite.framework.Config

trait PIRGen extends Traversal with PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR._

  override val name = "PIR Generation"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  lazy val dir = sys.env("PIR_HOME") + "/apps/"
  val app = Config.degFilename.take(Config.degFilename.length - 4)
  val filename = app + ".scala"

  lazy val prescheduler = new PIRScheduleAnalyzer{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val scheduler = new PIRScheduler{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val optimizer = new PIROptimizer{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val splitter  = new PIRSplitter{val IR: PIRGen.this.IR.type = PIRGen.this.IR}
  lazy val retimer   = new PIRRetiming{val IR: PIRGen.this.IR.type = PIRGen.this.IR}

  lazy val dse       = new PIRDSE{val IR: PIRGen.this.IR.type = PIRGen.this.IR}

  lazy val top       = prescheduler.top
  val cus = HashMap[Exp[Any],List[ComputeUnit]]()

  def allocateCU(pipe: Exp[Any]): ComputeUnit = throw new Exception("Cannot allocate CUs during generation")

  var stream: PrintWriter = null
  var indent = 0
  def emit(x: => Any) { stream.println("  "*indent + x) }
  def open(x: => Any) { emit(x); indent += 1 }
  def close(x: => Any) { indent -= 1; emit(x) }

  override def quote(x: Exp[Any]) = x match {
    case Const(_) => quote(allocateConst(x))
    case Param(_) => quote(allocateConst(x))
    case Fixed(_) => quote(allocateConst(x))
    case Def(ConstBit(_)) => quote(allocateConst(x))
    case _ => super.quote(x)
  }

  override def run[A:Manifest](b: Block[A]) = {
    if (SpatialConfig.genCGRA) {
      stream = new PrintWriter(dir + filename)
      emitPIR(b)
      stream.flush()
      stream.close()
    }
    (b)
  }

  def emitPIR(b: Block[Any]) {
    // prescheduling
    prescheduler.run(b)
    val cuMapping = prescheduler.cuMapping
    // scheduling
    scheduler.cus ++= cuMapping.toList
    scheduler.run(b)
    // optimization
    optimizer.globals = (prescheduler.globals ++ scheduler.globals)
    optimizer.cuMapping ++= cuMapping.toList
    optimizer.run(b)
    // dse
    dse.globals = optimizer.globals
    dse.cuMapping ++= optimizer.cuMapping.toList
    dse.run(b)
    // splitting
    splitter.globals = optimizer.globals
    splitter.cuMapping ++= optimizer.cuMapping.toList
    splitter.run(b)
    // retiming
    retimer.globals = splitter.globals
    retimer.cus ++= splitter.cus
    retimer.run(b)
    // gen
    globals = retimer.globals
    cus ++= retimer.cus

    debug("Scheduling complete. Generating...")
    generateHeader()
    generateGlobals()
    traverseBlock(b)
    generateFooter()
  }

  def generateHeader() {
    emit("import pir.graph")
    emit("import pir.graph._")
    emit("import pir.graph.enums._")
    emit("import pir.codegen._")
    emit("import pir.plasticine.config._")
    emit("import pir.Design")
    emit("import pir.misc._")
    emit("import pir.PIRApp")
    emit("")
    open(s"""object ${app}Design extends PIRApp {""")
    emit(s"""override val arch = Config0""")
    open(s"""def main(args: String*)(top:Top) = {""")
    //emit(s"""top = Top()""")
  }

  def generateGlobals() {
    val (mems, dramCtrls) = globals.partition{case DRAMCtrl(_,_,_) => false; case _ => true}
    mems.foreach(emitComponent(_))
    dramCtrls.foreach(emitComponent(_))
  }

  def generateFooter() {
    val args = globals.flatMap{case InputArg(name)=>Some(name); case OutputArg(name)=>Some(name); case _ => None}.mkString(", ")
    val mcs  = globals.flatMap{case DRAMCtrl(name,_,_)=>Some(name); case _ => None}.mkString(", ")
    //emit(s"top.updateFields(List(${cus(top.get).name}), List($args), List($mcs))")
    emit(s"")
    close("}")
    close("}")
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && cus.contains(lhs))
      for (cu <- cus(lhs)) generateCU(lhs, cu)
  }

  def cuDeclaration(cu: ComputeUnit) = {
    val parent = cu.parent.map(_.name).getOrElse("top")
    val deps = cu.deps.map{dep => dep.name }.mkString("List(", ", ", ")")
    val isCtrl = cu.allStages.isEmpty
    cu match {
      case cu: BasicComputeUnit if cu.isUnitCompute =>
        s"""UnitPipeline(name ="${cu.name}", parent=$parent, deps=$deps)"""
      case cu: BasicComputeUnit =>
        s"""${quote(cu.tpe, isCtrl)}(name="${cu.name}", parent=$parent, deps=$deps)"""
      case cu: TileTransferUnit =>
        s"""TileTransfer(name="${cu.name}", parent=$parent, memctrl=${quote(cu.ctrl)}, mctpe=${cu.mode}, deps=$deps, vec=${quote(cu.vec)})"""
    }
  }

  def generateCU(pipe: Exp[Any], cu: ComputeUnit, suffix: String = "") {
    debug(s"Generating CU for $pipe")
    debug(cu.dumpString)

    open(s"val ${cu.name} = ${cuDeclaration(cu)} { implicit CU => ")
    emit(s"val stage0 = CU.emptyStage")
    preallocateRegisters(cu)                // Includes scalar inputs/outputs, temps, accums
    cu.cchains.foreach(emitComponent(_))    // Allocate all counterchains
    cu.srams.foreach(emitComponent(_))      // Allocate all SRAMs
    preallocateWriteRegs(cu)                // Local write addresses

    emitAllStages(cu)

    close("}")
  }

  def quoteInCounter(reg: LocalMem) = reg match {
    case reg:ScalarIn => s"CU.scalarIn(stage0, ${quote(reg)}).out"
    case reg:ConstReg => s"""${quote(reg)}.out"""
    case _ => throw new Exception(s"Disallowed input to counter: $reg")
  }

  def emitComponent(x: Any): Unit = x match {
    case CounterChainCopy(name, owner) =>
      emit(s"""val $name = CounterChain.copy(${owner.name}, "$name")""")

    case cc@CounterChainInstance(name, ctrs) =>
      for (ctr <- ctrs) emitComponent(ctr)
      val ctrList = ctrs.map{_.name}.mkString(", ")
      var decl = s"""val $name = CounterChain(name = "$name", $ctrList)"""
      if (cc.isStreaming) decl += ".isStreaming(true)"
      emit(decl)

    case cc@UnitCounterChain(name) =>
      var decl = s"""val $name = CounterChain(name = "$name", (Const("0i"), Const("1i"), Const("1i")))"""
      if (cc.isStreaming) decl += ".isStreaming(true)"
      emit(decl)

    case CUCounter(name,start,end,stride) =>
      debug(s"Generating counter $x")
      emit(s"""val $name = (${quoteInCounter(start)}, ${quoteInCounter(end)}, ${quoteInCounter(stride)}) // Counter""")

    case sram@CUMemory(sym, size) =>
      debug(s"Generating ${sram.dumpString}")
      val prefix = if (sram.isFIFO) "FIFO" else "SRAM"

      var decl = s"""val ${quote(sym)} = $prefix(size = $size"""
      sram.writeCtrl match {
        case Some(cchain) => decl += s""", writeCtr = ${cchain.name}(0)"""
        case None if sram.isFIFO => // Ok
        case None => throw new Exception(s"No write controller defined for $sram")
      }
      sram.banking match {
        case Some(banking) => decl += s""", banking = $banking"""
        case None => throw new Exception(s"No banking defined for $sram")
      }
      if (sram.bufferDepth > 1 && !sram.isFIFO) {
        val swapRead = sram.swapRead match {
          case Some(cchain) => s"swapRead = ${cchain.name}(0)"
          case None => throw new Exception(s"No swap read controller defined for $sram")
        }
        val swapWrite = sram.swapWrite match {
          case Some(cchain) => s"swapWrite = ${cchain.name}(0)"
          case None => throw new Exception(s"No swap write controller defined for $sram")
        }
        decl += s""", buffering = MultiBuffer(${sram.bufferDepth}, ${swapRead}, ${swapWrite}))"""
      }
      else if (!sram.isFIFO) {
        decl += s""", buffering = SingleBuffer())"""
      }

      sram.vector match {
        case Some(LocalVector) => // Nothing?
        case Some(vec) => decl += s""".wtPort(${quote(vec)})"""
        case None => throw new Exception(s"Memory $sram has no vector defined")
      }
      sram.readAddr match {
        case Some(_:CounterReg | _:ConstReg) => decl += s""".rdAddr(${quote(sram.readAddr.get)})"""
        case Some(_:ReadAddrWire) =>
        case addr if sram.isFIFO => // ok
        case addr => throw new Exception(s"Disallowed memory read address in $sram: $addr")
      }
      sram.writeAddr match {
        case Some(_:CounterReg | _:ConstReg) => decl += s""".wtAddr(${quote(sram.writeAddr.get)})"""
        case Some(_:WriteAddrWire | _:LocalWriteReg) =>
        case addr if sram.isFIFO => // ok
        case addr => throw new Exception(s"Disallowed memory write address in $sram: $addr")
      }
      emit(decl)

    case mem@DRAMCtrl(_,region,mode) => emit(s"val ${quote(mem)} = MemoryController($mode, ${quote(region)})")
    case mem: Offchip   => emit(s"val ${quote(mem)} = OffChip()")
    case mem: InputArg  => emit(s"val ${quote(mem)} = ArgIn()")
    case mem: OutputArg => emit(s"val ${quote(mem)} = ArgOut()")
    case mem: ScalarMem => emit(s"val ${quote(mem)} = Scalar()")
    case mem: VectorMem => emit(s"val ${quote(mem)} = Vector()")
    case _ => throw new Exception(s"Don't know how to generate CGRA component: $x")
  }

  def preallocateRegisters(cu: ComputeUnit) = cu.regs.foreach{
    case reg:TempReg        => emit(s"val ${quote(reg)} = CU.temp")
    case reg@AccumReg(init) => emit(s"val ${quote(reg)} = CU.accum(init = ${quote(init)})")
    case reg:ControlReg if GenControlLogic => emit(s"val ${quote(reg)} = CU.ctrl")
    case _ => // No preallocation
  }

  def preallocateWriteRegs(cu: ComputeUnit) = cu.regs.foreach{
    case reg@LocalWriteReg(mem) => emit(s"val ${quote(reg)} = CU.wtAddr(${quote(mem)})")
    case _ => //nothing
  }

  def quote(sram: CUMemory): String = sram.name
  def quote(mem: GlobalMem): String = mem match {
    case Offchip(name)      => s"${name}_oc"
    case DRAMCtrl(name,_,_) => s"${name}_mc"
    case InputArg(name)     => s"${name}_argin"
    case OutputArg(name)    => s"${name}_argout"
    case ScalarMem(name)    => s"${name}_scalar"
    case VectorMem(name)    => s"${name}_vector"
    case LocalVector        => "local"
  }

  def quote(tpe: ControlType, isCtrl: Boolean) = tpe match {
    case InnerPipe            => "Pipeline"
    case CoarsePipe           => "MetaPipeline"
    case SequentialPipe       => "Sequential"
    case StreamPipe if isCtrl => "StreamController"
    case StreamPipe           => "StreamPipeline"
  }

  def quote(reg: LocalMem): String = reg match {
    case ConstReg(c)             => s"""Const("$c")"""      // Constant
    case CounterReg(cchain, idx) => s"${cchain.name}($idx)" // Counter
    case ValidReg(cchain,idx)    => s"${cchain.name}.valids($idx)"

    case WriteAddrWire(mem) => s"${quote(mem)}.writeAddr"   // Write address wire
    case ReadAddrWire(mem)  => s"${quote(mem)}.readAddr"    // Read address wire
    case LocalWriteReg(mem) => s"wr${reg.id}"               // Local write address register
    case SRAMRead(mem)      => quote(mem)                   // Local vector read

    case reg:ReduceReg      => s"rr${reg.id}"               // Reduction register
    case reg:AccumReg       => s"ar${reg.id}"               // After preallocation
    case reg:TempReg        => s"tr${reg.id}"               // Temporary register
    case reg:ControlReg     => s"cr${reg.id}"               // Control register

    case ScalarIn(glob:InputArg)   => quote(glob)           // Scalar inputs from input arg
    case ScalarIn(glob:ScalarMem)  => quote(glob)           // Scalar inputs from CU
    case ScalarOut(out:OutputArg)  => quote(out)            // Scalar output to output arg
    case ScalarOut(glob:ScalarMem) => quote(glob)           // Output to another CU
    case ScalarOut(mc:DRAMCtrl)    => s"${quote(mc)}.saddr" // Output to memory address

    case VectorIn(glob)            => quote(glob)           // Global vector read
    case VectorLocal(mem)          => quote(mem)            // Local vector write
    case VectorOut(vec: VectorMem) => quote(vec)            // Global vector write

    case _ => throw new Exception(s"Invalid local memory $reg")
  }

  var allocatedReduce: Set[ReduceReg] = Set.empty

  def quote(ref: LocalRef): String = ref match {
    case LocalRef(stage, reg: ConstReg)   => quote(reg)
    case LocalRef(stage, reg: CounterReg) => if (stage >= 0) s"CU.ctr(stage($stage), ${quote(reg)})" else quote(reg)
    case LocalRef(stage, reg: ValidReg)   => quote(reg)

    case LocalRef(stage, wire: WriteAddrWire) => quote(wire)
    case LocalRef(stage, wire: ReadAddrWire)  => quote(wire)
    case LocalRef(stage, reg: LocalWriteReg)  => s"CU.wtAddr(stage($stage), ${quote(reg)})"

    case LocalRef(stage, reg: ReduceReg) if allocatedReduce.contains(reg) => quote(reg)
    case LocalRef(stage, reg: ReduceReg)  => s"CU.reduce(stage($stage))"
    case LocalRef(stage, reg: AccumReg)   => s"CU.accum(stage($stage), ${quote(reg)})"
    case LocalRef(stage, reg: TempReg)    => s"CU.temp(stage($stage), ${quote(reg)})"
    case LocalRef(stage, reg: ControlReg) => s"CU.ctrl(stage($stage), ${quote(reg)})"

    case LocalRef(stage, reg: ScalarIn)  => s"CU.scalarIn(stage($stage), ${quote(reg)})"
    case LocalRef(stage, reg: ScalarOut) => s"CU.scalarOut(stage($stage), ${quote(reg)})"

    case LocalRef(stage, reg: VectorIn)  => s"CU.vecIn(stage($stage), ${quote(reg)})"
    case LocalRef(stage, reg: SRAMRead)    => if (stage >= 0) s"CU.load(stage($stage), ${quote(reg)})" else s"${quote(reg)}.load"
    case LocalRef(stage, reg: VectorLocal) => s"CU.store(stage($stage), ${quote(reg)})"
    case LocalRef(stage, reg: VectorOut)   => s"CU.vecOut(stage($stage), ${quote(reg)})"
  }

  def emitAllStages(cu: ComputeUnit) {
    var i = 1
    var r = 1
    def emitStages(stages: ArrayBuffer[Stage]) = stages.foreach{
      case MapStage(op,inputs,outputs) =>
        val ins = inputs.map(quote(_)).mkString(", ")
        val outs = outputs.map(quote(_)).mkString(", ")
        emit(s"""Stage(stage($i), operands=List($ins), op=$op, results=List($outs))""")
        i += 1

      case ReduceStage(op,init,in,acc) =>
        emit(s"""val (rs$r, ${quote(acc)}) = Stage.reduce(op=$op, init=${quote(init)})""")
        allocatedReduce += acc
        r += 1
    }

    if (cu.stages.nonEmpty || cu.writeStages.exists{case (mem,stages) => stages.nonEmpty}) {
      emit(s"var stage: List[Stage] = Nil")
    }
    if (cu.controlStages.nonEmpty && GenControlLogic) {
      i = 0
      val nCompute = cu.controlStages.length
      emit(s"stage = ControlStages(${nCompute})")
      emitStages(cu.controlStages)
    }
    for ((srams,stages) <- cu.writeStages if stages.nonEmpty) {
      i = 1
      val nWrites  = stages.filter{_.isInstanceOf[MapStage]}.length
      emit(s"stage = stage0 +: WAStages(${nWrites}, ${srams.map(quote(_))})")
      emitStages(stages)
    }
    if (cu.stages.nonEmpty) {
      i = 1
      val nCompute = cu.stages.filter{_.isInstanceOf[MapStage]}.length
      emit(s"stage = stage0 +: Stages(${nCompute})")
      emitStages(cu.stages)
    }
  }
}

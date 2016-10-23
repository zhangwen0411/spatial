package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.{ArrayBuffer, HashMap}

trait PIRScheduleAnalysisExp extends NodeMetadataOpsExp with ReductionAnalysisExp with MemoryAnalysisExp {
  this: SpatialExp =>

  sealed abstract class MemoryMode
  case object MemLoad extends MemoryMode { override def toString() = "TileLoad" }
  case object MemStore extends MemoryMode { override def toString() = "TileStore" }
  case object MemScatter extends MemoryMode { override def toString() = "Scatter" }
  case object MemGather extends MemoryMode { override def toString() = "Gather" }

  // Inter-CU communication
  sealed abstract class GlobalMem
  case class Offchip(name: String) extends GlobalMem
  case class DRAMCtrl(name: String, region: Offchip, mode: MemoryMode) extends GlobalMem
  case class InputArg(name: String) extends GlobalMem
  case class OutputArg(name: String) extends GlobalMem
  case class ScalarMem(name: String) extends GlobalMem
  case class VectorMem(name: String) extends GlobalMem
  case object LocalVector extends GlobalMem


  // Intra-CU communication
  sealed abstract class LocalMem {
    val id = {LocalMem.id += 1; LocalMem.id}

    override def equals(x: Any) = x match {
      case that: LocalMem => this.id == that.id
      case _ => false
    }
  }
  object LocalMem { var id = 0 }

  case class ConstReg(const: String) extends LocalMem {
    override def equals(x: Any) = x match {
      case that: ConstReg => this.const == that.const
      case _ => false
    }
  }
  case class CounterReg(cchain: CUCounterChain, idx: Int) extends LocalMem {
    override def equals(x: Any) = x match {
      case that: CounterReg => this.cchain == that.cchain && this.idx == that.idx
      case _ => false
    }
  }

  case class ControlReg() extends LocalMem { override def toString = s"cr$id" }
  case class ValidReg(cchain: CUCounterChain, idx: Int) extends LocalMem {
    override def equals(x: Any) = x match {
      case that: CounterReg => this.cchain == that.cchain && this.idx == that.idx
      case _ => false
    }
  }

  abstract class LocalSRAMPort(val mem: CUMemory) extends LocalMem {
    override def equals(x: Any) = x match {
      case that: LocalSRAMPort => this.mem == that.mem && this.getClass == that.getClass
      case _ => false
    }
  }
  case class ReadAddrWire(override val mem: CUMemory) extends LocalSRAMPort(mem) {
    override def toString = s"${mem.name}.readAddr"
  }
  case class WriteAddrWire(override val mem: CUMemory) extends LocalSRAMPort(mem) {
    override def toString = s"${mem.name}.writeAddr"
  }
  case class LocalWriteReg(override val mem: CUMemory) extends LocalSRAMPort(mem) {
    override def toString = s"wr${id}"
  }
  case class SRAMRead(override val mem: CUMemory) extends LocalSRAMPort(mem) {
    override def toString = s"${mem.name}.read"
  }
  case class VectorLocal(override val mem: CUMemory) extends LocalSRAMPort(mem) {
    override def toString = s"${mem.name}.feedbackWrite"
  }

  case class ReduceReg() extends LocalMem { override def toString = s"rr$id"}
  case class AccumReg(init: ConstReg) extends LocalMem { override def toString = s"ar$id" }
  case class TempReg() extends LocalMem { override def toString = s"tr$id" }

  abstract class GlobalPort(val glob: GlobalMem) extends LocalMem {
    override def equals(x: Any) = x match {
      case that: GlobalPort => this.glob == that.glob && this.getClass == that.getClass
      case _ => false
    }
  }
  case class ScalarIn (override val glob: GlobalMem) extends GlobalPort(glob)
  case class ScalarOut(override val glob: GlobalMem) extends GlobalPort(glob)
  case class VectorIn (override val glob: VectorMem) extends GlobalPort(glob)
  case class VectorOut(override val glob: GlobalMem) extends GlobalPort(glob)


  def isAccum(mem: LocalMem): Boolean = mem match {
    case _:AccumReg | _:ReduceReg => true
    case _ => false
  }
  def isCUOutput(mem: LocalMem): Boolean = mem match {
    case _:ScalarOut | _:VectorOut => true
    case _ => false
  }
  def isCUInput(mem: LocalMem): Boolean = mem match {
    case _:VectorIn | _:ScalarIn => true
    case _ => false
  }
  def isReadable(mem: LocalMem) = mem match {
    case _:ReadAddrWire | _:WriteAddrWire | _:LocalWriteReg => false
    case _:ScalarOut | _:VectorLocal | _:VectorOut => false
    case _ => true
  }
  def isWritable(mem: LocalMem) = mem match {
    case _:ConstReg | _:CounterReg | _:ScalarIn => false
    case _:VectorIn | _:SRAMRead | _:ValidReg => false
    case _ => true
  }
  def isGlobal(mem: LocalMem) = mem match {
    case _:ScalarIn | _:ScalarOut | _:VectorIn | _:VectorOut => true
    case _ => false
  }
  def isControl(mem: LocalMem) = mem match {
    case _:ControlReg | _:ValidReg => true
    case _ => false
  }

  // Local memory references
  case class LocalRef(stage: Int, reg: LocalMem)

  def isReadOutsidePipe(x: Exp[Any], pipe: Exp[Any], reader: Option[Exp[Any]] = None) = {
    isArgOut(x) || readersOf(x).exists{read => reader.map{filt => read.node == filt}.getOrElse(true) && read.controlNode != pipe }
  }
  // (A) reader exists in this pipe or there are no readers
  def isReadInPipe(x: Exp[Any], pipe: Exp[Any], reader: Option[Exp[Any]] = None) = {
    readersOf(x).isEmpty || readersOf(x).exists{read => reader.map{filt => read.node == filt}.getOrElse(true) && read.controlNode == pipe }
  }
  // Not an input argument, (a) writer exists in this pipe or there are no writers
  def isWrittenInPipe(x: Exp[Any], pipe: Exp[Any], writer: Option[Exp[Any]] = None) = {
    !isArgIn(x) && (writersOf(x).isEmpty || writersOf(x).exists{write => writer.map{filt => write.node == filt}.getOrElse(true) && write.controlNode == pipe })
  }

  // TODO: This is VERY redundant with PIR
  sealed abstract class PIROp
  case object ALUMux extends PIROp { override def toString() = "Mux" }
  case object Bypass extends PIROp
  case object FixAdd extends PIROp
  case object FixSub extends PIROp
  case object FixMul extends PIROp
  case object FixDiv extends PIROp
  case object FixMod extends PIROp
  case object FixLt  extends PIROp
  case object FixLeq extends PIROp
  case object FixEql extends PIROp
  case object FixNeq extends PIROp
  case object FixMin extends PIROp
  case object FixMax extends PIROp

  case object FltAdd extends PIROp
  case object FltSub extends PIROp
  case object FltMul extends PIROp
  case object FltDiv extends PIROp
  case object FltLt  extends PIROp
  case object FltLeq extends PIROp
  case object FltEql extends PIROp
  case object FltNeq extends PIROp
  case object FltExp extends PIROp
  case object FltLog extends PIROp
  case object FltSqrt extends PIROp
  case object FltAbs extends PIROp
  case object FltMin extends PIROp
  case object FltMax extends PIROp

  case object BitAnd extends PIROp
  case object BitOr  extends PIROp

  def nodeToOp(node: Def[Any]): Option[PIROp] = node match {
    case Mux2(_,_,_)    => Some(ALUMux)
    case FixPt_Add(_,_) => Some(FixAdd)
    case FixPt_Sub(_,_) => Some(FixSub)
    case FixPt_Mul(_,_) => Some(FixMul)
    case FixPt_Div(_,_) => Some(FixDiv)
    case FixPt_Mod(_,_) => Some(FixMod)
    case FixPt_Lt(_,_)  => Some(FixLt)
    case FixPt_Leq(_,_) => Some(FixLeq)
    case FixPt_Eql(_,_) => Some(FixEql)
    case FixPt_Neq(_,_) => Some(FixNeq)
    case e: Min2[_] if isFixPtType(e.mT) => Some(FltMin)
    case e: Max2[_] if isFixPtType(e.mT) => Some(FltMax)

    // Float ops currently assumed to be single op
    case FltPt_Add(_,_) => Some(FltAdd)
    case FltPt_Sub(_,_) => Some(FltSub)
    case FltPt_Mul(_,_) => Some(FltMul)
    case FltPt_Div(_,_) => Some(FltDiv)
    case FltPt_Lt(_,_)  => Some(FltLt)
    case FltPt_Leq(_,_) => Some(FltLeq)
    case FltPt_Eql(_,_) => Some(FltEql)
    case FltPt_Neq(_,_) => Some(FltNeq)

    case FltPt_Abs(_)   => Some(FltAbs)
    case FltPt_Exp(_)   => Some(FltExp)
    case FltPt_Log(_)   => Some(FltLog)
    case FltPt_Sqrt(_)  => Some(FltSqrt)
    case e: Min2[_] if isFltPtType(e.mT) => Some(FltMin)
    case e: Max2[_] if isFltPtType(e.mT) => Some(FltMax)

    case Bit_And(_,_)   => Some(BitAnd)
    case Bit_Or(_,_)    => Some(BitOr)
    case _ => None
  }


  // --- Stages prior to scheduling
  sealed abstract class PseudoStage { def output: Option[Exp[Any]] }
  case class DefStage(op: Exp[Any], isReduce: Boolean = false) extends PseudoStage { def output = Some(op) }
  case class OpStage(op: PIROp, inputs: List[Exp[Any]], out: Exp[Any], isReduce: Boolean = false) extends PseudoStage { def output = Some(out) }
  case class WriteAddrStage(mem: Exp[Any], addr: Exp[Any]) extends PseudoStage { def output = None }

  // --- Stages after scheduling
  sealed abstract class Stage {
    def outputMems: List[LocalMem]
    def inputMems: List[LocalMem]
    def inputRefs: List[LocalRef]
    def outputRefs: List[LocalRef]
  }
  case class MapStage(op: PIROp, var ins: List[LocalRef], var outs: List[LocalRef]) extends Stage {
    def outputMems = outs.map(_.reg)
    def inputMems = ins.map(_.reg)
    def inputRefs = ins
    def outputRefs = outs
  }
  case class ReduceStage(op: PIROp, init: LocalMem, in: LocalRef, acc: ReduceReg) extends Stage {
    def outputMems = List(acc)
    def inputMems = List(in.reg, acc)
    def inputRefs = List(in)
    def outputRefs = Nil
  }

  // --- Compute units
  def allocateConst(x: Exp[Any]) = x match {
    case Const(c: Int)    => ConstReg(s"${c}i")
    case Const(c: Long)   => ConstReg(s"${c}l")
    case Const(c: Double) => ConstReg(s"${c}d")
    case Const(c: Float)  => ConstReg(s"${c}f")
    case Param(c: Int)    => ConstReg(s"${c}i")
    case Param(c: Double) => ConstReg(s"${c}d")
    case Param(c: Float)  => ConstReg(s"${c}f")

    // TODO: Not quite correct since bound is a double
    case Fixed(c) if (c.toInt == c)  => ConstReg(s"${c.toInt}i")
    case Fixed(c) if (c.toLong == c) => ConstReg(s"${c.toLong}l")
    case Fixed(c) if (c.toFloat == c) => ConstReg(s"${c.toFloat}f")
    case Fixed(c) => ConstReg(s"${c.toDouble}d")

    case Def(ConstBit(c)) => if (c) ConstReg("1i") else ConstReg("0i")
    case _ => throw new Exception(s"Cannot allocate constant value for $x")
  }

  sealed abstract class ComputeUnit(val name: String, val pipe: Exp[Any], val parent: Option[ComputeUnit]) {
    var cchains: Set[CUCounterChain] = Set.empty
    var srams: Set[CUMemory] = Set.empty
    var regs: Set[LocalMem] = Set.empty
    var deps: Set[ComputeUnit] = Set.empty
    private val regTable = HashMap[Exp[Any], LocalMem]()
    private val expTable = HashMap[LocalMem, List[Exp[Any]]]()

    def addReg(exp: Exp[Any], reg: LocalMem) {
      regs += reg
      regTable += exp -> reg
      if (expTable.contains(reg)) expTable += reg -> (expTable(reg) :+ exp)
      else                        expTable += reg -> List(exp)
    }
    def iterators = regTable.toList.flatMap{case (exp, reg: CounterReg) => Some((exp,reg)); case _ => None}
    def valids    = regTable.toList.flatMap{case (exp, reg: ValidReg) => Some((exp,reg)); case _ => None}

    def innermostIter(cc: CUCounterChain) = {
      val iters = iterators.flatMap{case (e,CounterReg(`cc`,i)) => Some((e,i)); case _ => None}
      if (iters.isEmpty) None  else Some(iters.reduce{(a,b) => if (a._2 > b._2) a else b}._1)
    }

    def get(x: Exp[Any]): Option[LocalMem] = x match {
      case Exact(_) => Some(getOrAddReg(x)(allocateConst(x)))
      case Def(ConstBit(_)) => Some(getOrAddReg(x)(allocateConst(x)))
      case _ => regTable.get(x) match {
        case Some(reg) if regs.contains(reg) => Some(reg)
        case _ => None
      }
    }
    def getOrAddReg(x: Exp[Any])(func: => LocalMem) = regTable.get(x) match {
      case Some(reg) if regs.contains(reg) => reg // On return this mapping if it is valid
      case _ =>
        val reg = x match {
          case Exact(_) => allocateConst(x)
          case Def(ConstBit(_)) => allocateConst(x)
          case _ => func
        }
        addReg(x, reg)
        reg
    }

    var writePseudoStages = HashMap[List[CUMemory], List[PseudoStage]]()
    var computePseudoStages: List[PseudoStage] = Nil
    var writeStages = HashMap[List[CUMemory], ArrayBuffer[Stage]]()
    var stages: ArrayBuffer[Stage] = ArrayBuffer.empty
    var controlStages: ArrayBuffer[Stage] = ArrayBuffer.empty

    def allStages = (writeStages.values.flatten ++ stages).toList

    def dumpString = s"""  cchains = ${cchains.mkString(", ")}
  regs    = ${regs.mkString(", ")}
  srams   = ${srams.mkString(", ")}
  stages  = ${if (stages.isEmpty) "" else stages.mkString("\n    ","\n    ","")}"""
  }

  case class BasicComputeUnit(
    override val name: String,
    override val pipe: Exp[Any],
    override val parent: Option[ComputeUnit],
    val tpe: ControlType
  ) extends ComputeUnit(name,pipe,parent) {
    override def dumpString = s"""BasicComputeUnit($name, $parent, $tpe){
${super.dumpString}
}"""
    override def toString() = s"BasicComputeUnit($name, ${parent.map(_.name)})"

    var isUnitCompute = false
  }

  case class TileTransferUnit(
    override val name: String,
    override val pipe: Exp[Any],
    override val parent: Option[ComputeUnit],
    val ctrl: DRAMCtrl,
    var vec: VectorMem,
    val mode: MemoryMode
  ) extends ComputeUnit(name,pipe,parent) {
    override def dumpString = s"""TileTransferUnit($name, $parent, $ctrl, $mode){
${super.dumpString}
}"""
    override def toString() = s"TileTransferUnit($name, ${parent.map(_.name)}, $ctrl, $mode)"
  }

  // TODO: Parallelism?
  case class CUCounter(name: String, start: LocalMem, end: LocalMem, stride: LocalMem)

  sealed abstract class CUCounterChain(val name: String) {
    var isStreaming = false
  }
  case class CounterChainCopy(override val name: String, owner: ComputeUnit) extends CUCounterChain(name)
  case class CounterChainInstance(override val name: String, ctrs: List[CUCounter]) extends CUCounterChain(name)
  case class UnitCounterChain(override val name: String) extends CUCounterChain(name)

  def memSize(mem: Exp[Any]) = dimsOf(mem).map(dim => bound(dim).get.toInt).fold(1){_*_}

  sealed abstract class SRAMBanking
  case class Strided(stride: Int) extends SRAMBanking
  case class Diagonal(stride1: Int, stride2: Int) extends SRAMBanking
  case object NoBanks extends SRAMBanking { override def toString() = "NoBanking()" }
  case object Duplicated extends SRAMBanking { override def toString = "Duplicated()" }

  case class CUMemory(name: String, size: Int) {
    // These can be recursive... e.g. readAddr = ReadAddrWire(this)
    // TODO: Does this need to be changed?
    var vector: Option[GlobalMem] = None
    var readAddr: Option[LocalMem] = None
    var writeAddr: Option[LocalMem] = None
    var swapWrite: Option[CUCounterChain] = None
    var swapRead: Option[CUCounterChain] = None
    var writeCtrl: Option[CUCounterChain] = None
    var banking: Option[SRAMBanking] = None
    var bufferDepth:Int = 1

    def dumpString = s"""CUMemory($name, $size) {
  vector = $vector
  readAddr = $readAddr
  writeAddr = $writeAddr
}"""
  }

}

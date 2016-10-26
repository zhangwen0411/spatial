package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.{HashMap, ArrayBuffer}

// For bound symbols
trait SubstQuotingExp extends QuotingExp  {
  val IR: SpatialExp
  import IR._
  override def quote(x: Exp[Any]) = super.quote(aliasOf(x))
}

trait PIRCommon extends SubstQuotingExp with ControllerTools {
  val IR: PIRScheduleAnalysisExp with SpatialExp
  import IR.{assert => _, _}

  val GenControlLogic = false
  val EnableSplitting = false


  var globals = Set[GlobalMem]()

  def allocateCU(pipe: Exp[Any]): ComputeUnit

  // HACK: Skip parallel pipes in PIR gen
  def parentOfHack(x: Exp[Any]): Option[Exp[Any]] = parentOf(x) match {
    case Some(pipe@Deff(ParallelPipe(_))) => parentOfHack(pipe)
    case parentOpt => parentOpt
  }


  // Give top controller or first controller below which is not a Parallel
  def topControllerHack(access: Access, ctrl: Controller): Controller = ctrl.node match {
    case pipe@Deff(ParallelPipe(_)) =>
      topControllerHack(access, childContaining(ctrl, access))
    case _ => ctrl
  }


  // TODO: Awkward extension of ComputeUnit. May want to move all this to CU later?
  abstract class CUContext(val cu: ComputeUnit) {
    private val refs = HashMap[Exp[Any],LocalRef]()
    private var readAccums = Set[AccumReg]()

    // HACK: Keep track of first read of accum reg (otherwise can use the wrong stage)
    def isUnreadAccum(reg: LocalMem) = reg match {
      case reg: AccumReg => !readAccums.contains(reg)
      case _ => false
    }

    def pipe: Exp[Any]

    def controlStages: ArrayBuffer[Stage] = cu.controlStages

    def pseudoStages: List[PseudoStage]
    def stages: ArrayBuffer[Stage]
    def addStage(stage: Stage): Unit
    def addControlStage(stage: Stage): Unit = cu.controlStages += stage
    def isWriteContext: Boolean

    def mapStages = stages.flatMap{case stage:MapStage => Some(stage); case _ => None}
    def stageNum = mapStages.length+1
    def controlStageNum = controlStages.length
    def prevStage = stages.lastOption

    def mem(mem: Exp[Any], reader: Exp[Any]) = allocateMem(mem, reader, cu, false)

    // A CU can have multiple SRAMs for a given mem symbol, one for each local read
    def memories(mem: Exp[Any]) = readersOf(mem).filter(_.controlNode == cu.pipe).map{reader => this.mem(mem, reader.node) }

    def addReg(x: Exp[Any], reg: LocalMem) {
      debug(s"Adding register mapping $x -> $reg")
      cu.addReg(x, reg)
    }
    def addRef(x: Exp[Any], ref: LocalRef) { refs += x -> ref }
    def getReg(x: Exp[Any]) = cu.get(x)
    def reg(x: Exp[Any]) = cu.get(x).getOrElse(throw new Exception(s"No register defined for $x"))

    // Add a stage which bypasses x to y
    def bypass(x: LocalMem, y: LocalMem) {
      val stage = MapStage(Bypass, List(refIn(x)), List(refOut(y)))
      addStage(stage)
    }

    def ref(reg: LocalMem, out: Boolean, stage: Int = stageNum): LocalRef = reg match {
      // If the previous stage computed the read address for this load, use the registered output
      // of the memory directly. Otherwise, use the previous stage
      case SRAMRead(mem) =>
        /*debug(s"Referencing SRAM $mem in stage $stage")
        debug(s"  Previous stage: $prevStage")
        debug(s"  SRAM read addr: ${mem.readAddr.get}")*/
        if (!prevStage.isDefined || prevStage.get.outputMems.contains(mem.readAddr.get))
          LocalRef(-1, reg)
        else
          LocalRef(stage-1, reg)

      case reg: CounterReg if isWriteContext && !prevStage.isDefined =>
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
    def refIn(reg: LocalMem, stage: Int = stageNum) = ref(reg, false, stage)
    def refOut(reg: LocalMem, stage: Int = stageNum) = ref(reg, true, stage)

    def addOutputFor(e: Exp[Any])(prev: LocalMem, out: LocalMem) { addOutput(prev, out, Some(e)) }
    def addOutput(prev: LocalMem, out: LocalMem) { addOutput(prev, out, None) }
    def addOutput(prev: LocalMem, out: LocalMem, e: Option[Exp[Any]]) {
      mapStages.find{stage => stage.outputMems.contains(prev) } match {
        case Some(stage) =>
          stage.outs ::= refOut(out, mapStages.indexOf(stage) + 1)
        case None =>
          bypass(prev, out)
      }
      if (e.isDefined) addReg(e.get, out)
      else cu.regs += out // No mapping, only list
    }
    def isUnitCompute = cu match {
      case x:BasicComputeUnit => x.isUnitCompute
      case _ => false
    }
  }

  case class ComputeContext(override val cu: ComputeUnit) extends CUContext(cu) {
    def pseudoStages = cu.computePseudoStages
    def stages = cu.stages
    def addStage(stage: Stage) { cu.stages += stage }
    def isWriteContext = false
    def pipe = cu.pipe
  }
  case class WriteContext(override val cu: ComputeUnit, srams: List[CUMemory]) extends CUContext(cu) {
    cu.writeStages += srams -> ArrayBuffer.empty

    def pseudoStages = cu.writePseudoStages(srams)._2
    def stages = cu.writeStages(srams)
    def addStage(stage: Stage) { cu.writeStages(srams) += stage }
    def isWriteContext = true
    def pipe = cu.writePseudoStages(srams)._1
  }


  // -- Utility functions
  def allMapStages(cu: ComputeUnit): Set[MapStage] = {
    cu.stages.flatMap{case stage: MapStage => Some(stage); case _ => None}.toSet ++
    cu.writeStages.values.flatMap{stages => stages.flatMap{case stage: MapStage => Some(stage); case _ => None}}.toSet
  }

  def scalarIns(cu: ComputeUnit): Set[GlobalMem] = {
    cu.stages.flatMap(_.inputMems).flatMap{case ScalarIn(in) => Some(in); case _ => None}.toSet ++
    cu.srams.flatMap{sram => sram.readAddr.flatMap{case ScalarIn(in) => Some(in); case _ => None}}.toSet ++
    cu.srams.flatMap{sram => sram.writeAddr.flatMap{case ScalarIn(in) => Some(in); case _ => None}}.toSet ++
    cu.cchains.flatMap{
      case CounterChainInstance(_,ctrs) => ctrs.flatMap{case CUCounter(_,start,end,stride) => List(start,end,stride).flatMap{case ScalarIn(in) => Some(in); case _ => None}}
      case _ => Nil
    }.toSet
  }
  def scalarOuts(cu: ComputeUnit): Set[GlobalMem] = cu match {
    case tu: TileTransferUnit => Set.empty
    case cu: BasicComputeUnit =>
      cu.stages.flatMap(_.outputMems).flatMap{case ScalarOut(out) => Some(out); case _ => None }.toSet
    case _ => Set.empty
  }

  def vectorOuts(cu: ComputeUnit): Set[VectorMem] = cu match {
    case tu: TileTransferUnit if tu.mode == MemLoad => Set(tu.vec)
    case cu: BasicComputeUnit =>
      cu.stages.flatMap(_.outputMems).flatMap{case VectorOut(vec: VectorMem) => Some(vec); case _ => None}.toSet
    case _ => Set.empty
  }

  def vectorIns(cu: ComputeUnit): Set[VectorMem] = cu match {
    case tu: TileTransferUnit if tu.mode == MemStore => Set(tu.vec)
    case cu: BasicComputeUnit =>
      cu.stages.flatMap(_.inputMems).flatMap{case VectorIn(vec) => Some(vec); case _ => None}.toSet ++
      cu.srams.flatMap{sram => sram.vector.flatMap{case vec: VectorMem => Some(vec); case _ => None }}.toSet ++
      cu.cchains.flatMap{
        case CounterChainInstance(_,ctrs) => ctrs.flatMap{case CUCounter(_,start,end,stride) => List(start,end,stride).flatMap{case VectorIn(in) => Some(in); case _ => None}}
        case _ => Nil
      }.toSet // TODO: Is it allowed for counterchains to have vector input dimensions?

    case _ => Set.empty
  }


  // Copy iterators from source CPU to dest CPU
  def copyIterators(destCU: ComputeUnit, srcCU: ComputeUnit) = {
    if (destCU != srcCU) {
      val cchainCopies = srcCU.cchains.map{
        case cc@CounterChainCopy(name, owner) => cc -> cc
        case cc@CounterChainInstance(name, ctrs) => cc -> CounterChainCopy(name, srcCU)
        case cc@UnitCounterChain(name) => cc -> CounterChainCopy(name, srcCU)
      }
      val cchainMapping = Map[CUCounterChain,CUCounterChain](cchainCopies.toList:_*)
      destCU.cchains ++= cchainCopies.map(_._2)

      srcCU.iterators.foreach{ case (iter,CounterReg(cchain,idx)) =>
        destCU.addReg(iter, CounterReg(cchainMapping(cchain),idx))
      }
      srcCU.valids.foreach{case (iter, ValidReg(cchain,idx)) =>
        destCU.addReg(iter, ValidReg(cchainMapping(cchain), idx))
      }
      cchainMapping
    }
    else Map.empty[CUCounterChain,CUCounterChain]
  }



  // Create a vector for communication to/from a given memory
  def allocateGlobal(mem: Exp[Any]) = {
    val name = quote(mem)
    val global = mem match {
      case Deff(Dram_new(_))    => Offchip(name)
      case Deff(Argin_new(_))   => InputArg(name)
      case Deff(Argout_new(_))  => OutputArg(name)
      case Deff(Reg_new(_))     => ScalarMem(name)
      case mem if isArgIn(mem)  => InputArg(name)
      case mem if isArgOut(mem) => OutputArg(name)
      case mem if isReg(mem.tp) => ScalarMem(name)
      case _                    => VectorMem(name)
    }
    debug(s"### Adding global for $mem: $global")
    globals += global
    global
  }

  private def allocateReg(reg: Exp[Any], pipe: Exp[Any], read: Option[Exp[Any]] = None, write: Option[Exp[Any]] = None) = {
    val isLocallyRead = isReadInPipe(reg, pipe, read)
    val isLocallyWritten = isWrittenInPipe(reg, pipe, write)
    debug(s"### Allocating register $reg in $pipe (localRead: $isLocallyRead, localWrite: $isLocallyWritten, accum: ${isAccum(reg)}")
    if (isLocallyRead && isLocallyWritten && isInnerAccum(reg)) {
      ReduceReg()
    }
    else if (isLocallyRead && isLocallyWritten && isAccum(reg)) {
      val rst = allocateConst(resetValue(reg.asInstanceOf[Exp[Reg[Any]]]))
      AccumReg(rst)
    }
    else if (!isLocallyRead) { // Always prefer the local register over ScalarOut, if applicable
      val global = allocateGlobal(reg)
      ScalarOut(global)
    }
    else if (!isLocallyWritten) {
      val global = allocateGlobal(reg)
      ScalarIn(global)
    }
    else {
      TempReg()
    }
  }

  def allocateLocal(mem: Exp[Any], pipe: Exp[Any], read: Option[Exp[Any]] = None,  write: Option[Exp[Any]] = None): LocalMem = mem match {
    case Exact(c) => allocateConst(mem)
    case Def(ConstBit(c)) => allocateConst(mem)
    case reg@Deff(Argin_new(init)) =>
      val global = allocateGlobal(reg)
      ScalarIn(global)
    case reg@Deff(Argout_new(init)) => allocateReg(reg, pipe, read, write) // argOuts can be accumulators
    case reg@Deff(Reg_new(init))    => allocateReg(reg, pipe, read, write)

    case reader@Deff(Reg_read(reg)) =>
      debug(s"### Allocating reader $reader of $reg in $pipe")
      allocateLocal(reg, pipe, Some(reader), write)

    case _ => TempReg()
  }

  def isBuffer(mem: Exp[Any]) = isSRAM(mem.tp) // || isFIFO(mem.tp)

  /* How much to bank by in Plasticine:
     - Which dimension (stride) has a predictable, parallelizable access with inner index
       of reader and writer CUs?
   */
  def bank(mem: Exp[Any], access: Exp[Any], iter: Option[Exp[Any]]) = {
    val indices = accessIndicesOf(access)
    val pattern = accessPatternOf(access)
    val allStrides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})
    val strides = if (indices.length == 1) List(allStrides.last) else allStrides

    /*debug(s"  access: $access")
    debug(s"  indices: $indices")
    debug(s"  pattern: $pattern")*/

    def bankFactor(i: Exp[Any]) = if (iter.isDefined && i == iter.get) 16 else 1

    val banking = (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, bankFactor(i))
      case StridedAccess(Exact(a),i)  => StridedBanking(a.toInt*stride, bankFactor(i))
      case OffsetAccess(i,b)          => StridedBanking(stride, bankFactor(i))
      case LinearAccess(i)            => StridedBanking(stride, bankFactor(i))
      case InvariantAccess(b)         => NoBanking // Duplicate in this dimension
      case RandomAccess               => NoBanking // Duplicate in this dimension
    }}
    //debug(s"  banking: $banking")

    val form = banking.find(_.banks > 1).getOrElse(NoBanking)
    form match {
      case StridedBanking(stride, banks) => Strided(stride)
      case NoBanking if iter.isDefined => Duplicated
      case NoBanking => NoBanks
    }
  }
  def matchBanking(bank1: SRAMBanking, bank2: SRAMBanking) = (bank1,bank2) match {
    case (Strided(stride1), Strided(stride2)) if stride1 == stride2 => Strided(stride1)
    case (Strided(stride1), Strided(stride2)) => Diagonal(stride1, stride2)
    case (Duplicated, _) => Duplicated
    case (_, Duplicated) => Duplicated
    case (NoBanks, bank2) => bank2
    case (bank1, NoBanks) => bank1
  }

  private def initializeSRAM(sram: CUMemory, mem_in: Exp[Any], read: Exp[Any], cu: ComputeUnit) {
    // TODO: Assumes we see the mapping prior to any uses
    //debug(s"Creating SRAM for memory $mem_in: ")
    //getProps(mem_in).foreach{m => debug(makeString(m)) }

    val mem = aliasOf(mem_in)
    val reader = readersOf(mem).find{_.node == read}.get

    val instIndex = instanceIndicesOf(reader, mem).head
    val instance = duplicatesOf(mem).apply(instIndex)

    // First writer corresponding to this reader
    val writers = writersOf(mem).filter{writer => instanceIndicesOf(writer, mem).contains(instIndex) }
    assert(writers.length <= 1, "PIR cannot currently handle multiple writers")
    val writer = writers.headOption
    debug(s"Creating SRAM for memory $mem, reader $reader, writer: $writer, cu $cu")

    val writerCU = writer.map{writer => allocateCU(writer.controlNode) }
    val swapWritePipe = writer.flatMap{writer => topControllerOf(writer, mem, instIndex) }
    val swapReadPipe = topControllerOf(reader, mem, instIndex)

    // HACK: Ignore Parallels as top metapipeline controllers
    val swapWriteCU = (writer, swapWritePipe) match {
      case (Some(write), Some(ctrl)) =>
        val topCtrl = topControllerHack(write, ctrl)
        Some(allocateCU(topCtrl.node))
      case _ => None
    }
    val swapReadCU = swapReadPipe.map{ctrl => topControllerHack(reader, ctrl)}.map{ctrl => allocateCU(ctrl.node) }

    /*debug(s"  readerCU: $cu")
    debug(s"  writerCU: $writerCU")
    debug(s"  swapWriteCU: $swapWriteCU")
    debug(s"  swapReadCU: $swapReadCU")*/

    // ASSUMPTION: Each CU originally only instantiates only one counterchain
    val remoteWriteCtrl = writerCU.flatMap{cu => cu.cchains.find{case _:UnitCounterChain | _:CounterChainInstance => true; case _ => false }}
    val remoteSwapWriteCtrl = swapWriteCU.flatMap{cu => cu.cchains.find{case _:UnitCounterChain | _:CounterChainInstance => true; case _ => false }}
    val remoteSwapReadCtrl = swapReadCU.flatMap{cu => cu.cchains.find{case _:UnitCounterChain | _:CounterChainInstance => true; case _ => false }}

    val readCtrl = cu.cchains.find{case _:CounterChainCopy => false; case _ => true}
    val writeCtrl = remoteWriteCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }
    val swapWrite = remoteSwapWriteCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }
    val swapRead  = remoteSwapReadCtrl.flatMap{cc => cu.cchains.find(_.name == cc.name) }

    val writeIter = writeCtrl.flatMap{cc => cu.innermostIter(cc) }
    val readIter = readCtrl.flatMap{cc => cu.innermostIter(cc) }

    //debug(s"  readIter: $readIter")
    //debug(s"  writeIter: $writeIter")

    val readBanking = bank(mem, read, readIter)
    val writeBanking = writer.map{writer => bank(mem, writer.node, writeIter) }.getOrElse(NoBanks)

    //debug(s"  read banking: $readBanking")
    //debug(s"  write banking: $writeBanking")

    val banking = matchBanking(writeBanking, readBanking)

    sram.writeCtrl = writeCtrl
    sram.swapWrite = swapWrite
    sram.swapRead = swapRead
    sram.banking = Some(banking)
    sram.bufferDepth = instance.depth
  }

  def allocateMem(mem: Exp[Any], reader: Exp[Any], cu: ComputeUnit, add: Boolean) = {
    if (!isBuffer(mem))
      throw new Exception(s"Cannot allocate SRAM for non-buffer $mem")

    val name = s"${quote(mem)}_${quote(reader)}"
    //debug(s"### Looking for mem $name in $cu")
    cu.srams.find(_.name == name) match {
      case Some(sram) => sram
      case None if add =>
        val size = memSize(mem)
        val sram = CUMemory(name, size)
        initializeSRAM(sram, mem, reader, cu)
        cu.srams += sram
        sram
      case None => throw new Exception(s"Cannot find sram $name in cu $cu")
    }
  }


  def flattenNDAddress(addr: Exp[Any], dims: List[Exp[Index]]) = addr match {
    case Deff(ListVector(List(Deff(ListVector(indices))))) if indices.nonEmpty => flattenNDIndices(indices, dims)
    case Deff(ListVector(indices)) if indices.nonEmpty => flattenNDIndices(indices, dims)
    case _ => throw new Exception(s"Unsupported address in PIR generation: $addr")
  }
  private def flattenNDIndices(indices: List[Exp[Any]], dims: List[Exp[Index]]) = {
    val cdims = dims.map{case Bound(d) => d.toInt; case _ => throw new Exception("Unable to get bound of memory size") }
    val strides = List.tabulate(dims.length){d =>
      if (d == dims.length - 1) 1.as[Index]
      else cdims.drop(d+1).reduce(_*_).as[Index]
    }
    var partialAddr: Exp[Any] = indices.last
    var addrCompute: List[OpStage] = Nil
    for (i <- dims.length-2 to 0 by -1) { // If dims.length <= 1 this won't run
      val mul = OpStage(FixMul, List(indices(i),strides(i)), fresh[Index])
      val add = OpStage(FixAdd, List(mul.out, partialAddr),  fresh[Index])
      partialAddr = add.out
      addrCompute ++= List(mul,add)
    }
    (partialAddr, addrCompute)
  }

}

package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.virtualization.lms.util.GraphUtil
import scala.collection.mutable.{HashMap,HashSet}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait PIRScheduleAnalyzer extends Traversal with SpatialTraversalTools with PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_by => _, _}

  override val name = "PIR Preschedule Analysis"
  override val eatReflect = true
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  // --- State
  // TODO: Is there always only one CU per pipe?
  var top: Option[Exp[Any]] = None
  var pipes = List[Exp[Any]]()
  val cuMapping = HashMap[Exp[Any], ComputeUnit]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    top = None
    pipes = Nil
    cuMapping.clear()
    globals = Set.empty
    b
  }

  // --- CounterChains
  def inputs(x: Exp[Any]) = x match {
    case Deff(rhs: Product) => rhs.productIterator.filter(_.isInstanceOf[Exp[_]]).toList
    case Def(rhs) => syms(rhs).distinct
    case _ => Nil
  }

  def copyIterators(destCU: ComputeUnit, srcCU: ComputeUnit) {
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
    }
  }
  def addIterators(cu: ComputeUnit, cc: Exp[CounterChain], inds: List[List[Exp[Index]]]) {
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    inds.zipWithIndex.foreach{case (indSet, i) =>
      indSet.foreach{ind => cu.addReg(ind, CounterReg(cchain, i)) }
    }
  }
  def addEnabledIterators(cu: ComputeUnit, cc: Exp[CounterChain], inds: List[List[Exp[Index]]], ens: List[List[Exp[Bit]]]) {
    addIterators(cu, cc, inds)
    val cchain = cu.cchains.find(_.name == quote(cc)).get
    ens.zipWithIndex.foreach{case (enSet, i) =>
      enSet.foreach{en => cu.addReg(en, ValidReg(cchain, i)) }
    }
  }

  def allocateCChains(cu: ComputeUnit, pipe: Exp[Any]) {
    def allocateCounter(ctr: Exp[Counter], start: Exp[Any], end: Exp[Any], stride: Exp[Any]) = {
      val min = cu.getOrAddReg(start){ allocateLocal(start, pipe) }
      val max = cu.getOrAddReg(end){ allocateLocal(end, pipe) }
      val step = cu.getOrAddReg(stride){ allocateLocal(stride, pipe) }
      CUCounter(quote(ctr), min, max, step)
    }

    parentOfHack(pipe).foreach{parent => copyIterators(cu, allocateCU(parent)) }

    val ccs = inputs(pipe).flatMap {
      case cc@Deff(Counterchain_new(ctrs)) =>
        val ctrInsts = ctrs.map{case ctr@Deff(Counter_new(start,end,stride,par)) => allocateCounter(ctr, start, end, stride) }
        Some(CounterChainInstance(quote(cc),ctrInsts))
      case _ => None
    }
    if (ccs.length > 1) {
      debug(s"Compute unit $cu for $pipe has multiple counter chains: ")
      ccs.foreach{cc => debug(s"  $cc") }
    }
    cu.cchains ++= ccs
  }


  // --- Compute Units
  def controllersHack(pipe: Exp[Any]): List[Exp[Any]] = pipe match {
    case Deff(_:ParallelPipe) => childrenOf(pipe).flatMap{child => controllersHack(child)}
    case _ => List(pipe)
  }

  // TODO: This assumes linear stage order. Switch to more general dataflow graph after parallel is removed
  def pipeDependencies(pipe: Exp[Any]): List[Exp[Any]] = parentOf(pipe) match {
    case Some(parent@Deff(_:ParallelPipe)) => pipeDependencies(parent)
    case Some(parent) =>
      val childs = childrenOf(parent).map{child => controllersHack(child) }
      val idx = childs.indexWhere(_ contains pipe )
      val deps = if (idx > 0) childs(idx-1) else Nil
      debug(s"Found deps of $pipe: $deps")
      deps
    case None => Nil
  }

  def initCU[T<:ComputeUnit](cu: T, pipe: Exp[Any]): T = {
    cuMapping(pipe) = cu
    pipes ::= pipe
    allocateCChains(cu, pipe)
    cu.deps ++= pipeDependencies(pipe).map(allocateCU(_))
    debug(s"Allocated CU for control node $pipe: $cu")
    cu
  }

  // Get associated CU (or create a new one if not allocated yet)
  def allocateBasicCU(pipe: Exp[Any]): BasicComputeUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[BasicComputeUnit]
    else {
      debug(s"Allocating CU for $pipe")
      val parent = parentOfHack(pipe).map(allocateCU(_))
      val cu = BasicComputeUnit(quote(pipe), pipe, parent, styleOf(pipe))
      initCU(cu, pipe)
      pipe match {
        case Deff(e:UnrolledForeach)     => addEnabledIterators(cu, e.cc, e.inds, e.valids)
        case Deff(e:UnrolledReduce[_,_]) => addEnabledIterators(cu, e.cc, e.inds, e.valids)
        case Deff(_:UnitPipe | _:Hwblock) =>
          cu.isUnitCompute = isInnerPipe(pipe)
          cu.cchains += UnitCounterChain(quote(pipe)+"_unitCC")

        case _ =>
      }
      if (top.isEmpty && parent.isEmpty) top = Some(pipe)

      cu
    }
  }

  def allocateMemoryCU(pipe: Exp[Any], mem: Exp[Any], vec: Exp[Any], mode: MemoryMode): TileTransferUnit = {
    if (cuMapping.contains(pipe)) cuMapping(pipe).asInstanceOf[TileTransferUnit]
    else {
      debug(s"Allocating CU for $pipe")
      val region = allocateGlobal(mem).asInstanceOf[Offchip]
      val vector = allocateGlobal(vec).asInstanceOf[VectorMem]
      val mc = DRAMCtrl(quote(pipe)+"_mc", region, mode)
      globals += mc
      val parent = parentOfHack(pipe).map(allocateCU(_))
      val cu = TileTransferUnit(quote(pipe), pipe, parent, mc, vector, mode)
      initCU(cu, pipe)
    }
  }

  /**
   * Get or create a CU which corresponds to the given pipe
   **/
  def allocateCU(pipe: Exp[Any]) = pipe match {
    case Deff(_:Hwblock)              => allocateBasicCU(pipe)
    case Deff(_:UnrolledForeach)       => allocateBasicCU(pipe)
    case Deff(_:UnrolledReduce[_,_])   => allocateBasicCU(pipe)
    case Deff(_:UnitPipe)            => allocateBasicCU(pipe)
    case Deff(e:BurstLoad[_])  => allocateMemoryCU(pipe, e.mem, e.fifo, MemLoad)
    case Deff(e:BurstStore[_]) => allocateMemoryCU(pipe, e.mem, e.fifo, MemStore)
    case Def(d) => throw new Exception(s"Don't know how to generate CU for: \n  $pipe = $d")
  }


  def allocateWrittenSRAM(writer: Exp[Any], mem: Exp[Any], writerCU: ComputeUnit, stages: List[PseudoStage]) {
    val srams = readersOf(mem).map{reader =>
      val readerCU = allocateCU(reader.controlNode)
      copyIterators(readerCU, writerCU)
      val isLocallyRead = readerCU == writerCU

      val sram = allocateMem(mem, reader.node, readerCU)
      val vector = if (isLocallyRead) LocalVector else allocateGlobal(mem)
      sram.vector = Some(vector)
      (readerCU, sram)
    }

    if (stages.nonEmpty) {
      val groups = srams.groupBy(_._1).mapValues(_.map(_._2))
      for ((readerCU,srams) <- groups if readerCU != writerCU) {
        debug(s"""Adding write stages to $readerCU for SRAMs: ${srams.mkString(", ")}""")
        readerCU.writePseudoStages += srams -> stages
      }
    }
  }
  def allocateReadSRAM(reader: Exp[Any], mem: Exp[Any], readerCU: ComputeUnit) {
    allocateMem(mem, reader, readerCU)
  }

  def foreachSymInBlock(b: Block[Any])(func: Sym[Any] => Unit) {
    focusBlock(b){
      focusExactScope(b){ stms =>
        stms.foreach{ case TP(lhs,rhs) => func(lhs) }
      }
    }
  }

  def prescheduleRegisterRead(reg: Exp[Any], reader: Exp[Any], pipe: Option[Exp[Any]]) = {
    debug(s"  Register read: $reader")
    // Register reads may be used by more than one pipe
    readersOf(reg).filter(_.node == reader).map(_.controlNode).foreach{readCtrl =>
      val isCurrentPipe = pipe.map(_ == readCtrl).getOrElse(false)
      val isLocallyWritten = isWrittenInPipe(reg, readCtrl)

      if (!isCurrentPipe || !isLocallyWritten) {
        val readerCU = allocateCU(readCtrl)
        debug(s"  Adding read stage $reader of $reg to remote reader $readerCU")
        readerCU.computePseudoStages ++= List(DefStage(reader))
      }
    }
  }

  def prescheduleStages(pipe: Exp[Any], func: Block[Any]) {
    val Def(d) = pipe
    debug(s"Getting states of $pipe = $d")
    val cu = allocateCU(pipe)

    val remotelyAddedStages = cu.computePseudoStages // Stages added prior to traversing this pipe
    val remotelyAddedStms = remotelyAddedStages.flatMap(_.output).flatMap{
      case s: Sym[_] => findDefinition(s)
      case _ => None
    }

    val stms = remotelyAddedStms ++ getStmsInBlock(func)
    val stages = stms.map{case TP(lhs,rhs) => lhs}
    var remoteStages: Set[Exp[Any]] = Set.empty           // Ignore these (goes on different CU)

    def symsOnlyUsedInWriteAddr(exps: List[Exp[Any]]) = {
      // Build a schedule as usual, except for depencies on write addresses
      def mysyms(rhs: Any) = rhs match {
        case rhs: Def[_] => rhs match {
          case LocalWriter(writes) =>
            val addrs = writes.flatMap{case (mem,value,addr) => addr}
            syms(rhs) filterNot (addrs contains _)
          case _ => syms(rhs)
        }
        case _ => syms(rhs)
      }
      val scopeIndex = buildScopeIndex(stms)
      val result = func.res
      val xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(mysyms(result), scopeIndex), t => scheduleDepsWithIndex(mysyms(t.rhs), scopeIndex)).flatten

      debug(s"  Schedule without write addresses:")
      xx.reverse.foreach{case TP(lhs, rhs) => debug(s"    $lhs = $rhs")}

      exps.filterNot{case sym: Sym[_] => xx.exists(_.defines(sym).isDefined) }
    }

    stms.foreach{case TP(lhs, rhs) => debug(s"  $lhs = $rhs")}
    debug(s"Prescheduling $pipe = $d")
    cu.computePseudoStages = Nil // Clear stages so we don't duplicate existing stages

    foreachSymInBlock(func){
      // NOTE: Writers always appear to occur in the associated writer controller
      // However, register reads may appear outside their corresponding controller
      case writer@LocalWriter(writes) if !isControlNode(writer) =>
        debug(s"  $writer [WRITER]")
        writes.foreach{case (EatAlias(mem), value, indices) =>
          if (isBuffer(mem)) {
            val indexComputation = indices.map{is => getSchedule(stms)(is,false) }.getOrElse(Nil)
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
              debug(s"  Checking if symbols calculating ${addr.get} are used in current scope $pipe")
              remoteStages ++= symsOnlyUsedInWriteAddr(indexSyms)
            }
          }
        }

      case reader@LocalReader(reads) if !isControlNode(reader) =>
        debug(s"  $reader [READER]")
        reads.foreach{case (EatAlias(mem),indices) =>
          if (isReg(mem.tp)) {
            prescheduleRegisterRead(mem, reader, Some(pipe))
            val isLocallyRead = isReadInPipe(mem, pipe, Some(reader))
            val isLocallyWritten = isWrittenInPipe(mem, pipe)
            debug(s"  isLocallyRead: $isLocallyRead, isLocallyWritten: $isLocallyWritten")
            if (!isLocallyWritten || !isLocallyRead || isInnerAccum(mem)) remoteStages += reader
          }
          else if (isBuffer(mem)) {
            debug(s"  Local buffer read: $reader")
            allocateReadSRAM(reader, mem, cu)
          }
        }

      case lhs@Def(rhs) =>
        debug(s"  $lhs = $rhs [OTHER]")
        traverse(lhs.asInstanceOf[Sym[Any]], rhs)
    }

    val localCompute = stages.filter{s => (isPrimitiveNode(s) || isRegisterRead(s) || isGlobal(s)) && !remoteStages.contains(s) }

    // Sanity check
    val trueComputation = localCompute.filterNot{case Exact(_) => true; case Def(ConstBit(_)) => true; case s => isRegisterRead(s)}
    if (isOuterControl(pipe) && trueComputation.nonEmpty) {
      stageWarn(s"Outer control $pipe has compute stages: ")
      trueComputation.foreach{case lhs@Def(rhs) => stageWarn(s"  $lhs = $rhs")}
    }

    cu.computePseudoStages ++= localCompute.map{s => DefStage(s, isReduce = reduceType(s).isDefined) }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reg_read(EatAlias(reg)) if isArgIn(reg) =>
      prescheduleRegisterRead(reg, lhs, None)

    case Hwblock(func) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case UnrolledForeach(cc, func, inds, vs) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case UnrolledReduce(cc, accum, func, rFunc, inds, vs, acc, rV) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    case UnitPipe(func) =>
      val cu = allocateCU(lhs)
      prescheduleStages(lhs, func)

    // NOTE: Need to generate offset calculation as a codegen hack right now.
    case BurstLoad(mem,EatAlias(fifo),ofs,len,p) =>
      debug(s"Traversing $lhs = $rhs")
      val cu = allocateCU(lhs).asInstanceOf[TileTransferUnit]
      val lenIn = cu.getOrAddReg(len){ allocateLocal(len, lhs) }
      val ctr = CUCounter(quote(lhs)+"_ctr",ConstReg("0l"),lenIn,ConstReg("1l"))
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(ctr))
      val i = fresh[Index]
      cu.cchains += cc
      cu.addReg(i, CounterReg(cc, 0))
      //allocateWrittenSRAM(lhs, fifo, Some(i), cu, Nil)

      val memAddr = fresh[Index]
      cu.addReg(memAddr, ScalarOut(memAddr, cu.ctrl))
      val ofsCalc = OpStage(FixAdd, List(ofs, i), memAddr)
      cu.computePseudoStages ++= List(ofsCalc)

    case BurstStore(mem,EatAlias(fifo),ofs,len,p) =>
      debug(s"Traversing $lhs = $rhs")
      val cu = allocateCU(lhs).asInstanceOf[TileTransferUnit]
      val lenIn = cu.getOrAddReg(len){ allocateLocal(len, lhs) }
      val ctr = CUCounter(quote(lhs)+"_ctr",ConstReg("0l"),lenIn,ConstReg("1l"))
      val cc = CounterChainInstance(quote(lhs)+"_cc", List(ctr))
      val i = fresh[Index]
      cu.cchains += cc
      cu.addReg(i, CounterReg(cc, 0))
      //allocateReadSRAM(lhs, fifo, Some(i), cu)
      val memAddr = fresh[Index]
      cu.addReg(memAddr, ScalarOut(memAddr, cu.ctrl))
      val ofsCalc = OpStage(FixAdd, List(ofs, i), memAddr)
      cu.computePseudoStages ++= List(ofsCalc)

      // HACK: These are not correct - fix
      //val sram = allocateMem(fifo,lhs,cu)
      //sram.readAddr = cu.get(i)
      //sram.writeAddr = cu.get(i)

    case _ => super.traverse(lhs, rhs)
  }
}

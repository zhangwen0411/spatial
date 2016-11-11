package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable

// PIR operations which need the rest of the Spatial IR mixed in
trait PIRCommonExp extends PIRCommon with NodeMetadataOpsExp with MemoryAnalysisExp with ReductionAnalysisExp {this: SpatialExp =>
  type Symbol = Exp[Any]
  type CUControl = ControlType

  def str(x: Symbol) = x match {
    case Deff(d) => s"$x = $d"
    case _ => s"$x"
  }

  def extractConstant(x: Symbol): String = x match {
    case Const(c: Int)    => s"${c}i"
    case Const(c: Long)   => s"${c}l"
    case Const(c: Double) => s"${c}d"
    case Const(c: Float)  => s"${c}f"
    case Param(c: Int)    => s"${c}i"
    case Param(c: Double) => s"${c}d"
    case Param(c: Float)  => s"${c}f"

    // TODO: Not quite correct since bound is a double
    case Fixed(c) if (c.toInt == c)  => s"${c.toInt}i"
    case Fixed(c) if (c.toLong == c) => s"${c.toLong}l"
    case Fixed(c) if (c.toFloat == c) => s"${c.toFloat}f"
    case Fixed(c) => s"${c.toDouble}d"

    case Def(ConstBit(c)) => if (c) "1i" else "0i"

    case _ => throw new Exception(s"Cannot allocate constant value for $x")
  }

  def isReadInPipe(mem: Symbol, pipe: Symbol, reader: Option[Symbol] = None): Boolean = {
    readersOf(mem).isEmpty || readersOf(mem).exists{read => reader.forall(_ == read.node) && read.controlNode == pipe }
  }
  def isWrittenInPipe(mem: Symbol, pipe: Symbol, writer: Option[Symbol] = None): Boolean = {
    !isArgIn(mem) && (writersOf(mem).isEmpty || writersOf(mem).exists{write => writer.forall(_ == write.node) && write.controlNode == pipe })
  }
  def isWrittenByUnitPipe(mem: Symbol): Boolean = {
    writersOf(mem).headOption.map{writer => isUnitPipe(writer.controlNode)}.getOrElse(true)
  }
  def isReadOutsidePipe(mem: Symbol, pipe: Symbol, reader: Option[Symbol] = None): Boolean = {
    isArgOut(mem) || readersOf(mem).exists{read => reader.forall(_ == read.node) && read.controlNode != pipe }
  }

  def isBuffer(mem: Symbol): Boolean = isSRAM(mem.tp)

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
  def typeToStyle(tpe: ControlType) = tpe match {
    case InnerPipe      => PipeCU
    case CoarsePipe     => MetaPipeCU
    case SequentialPipe => SequentialCU
    case StreamPipe     => StreamCU
  }


  def bank(mem: Symbol, access: Symbol, iter: Option[Symbol]) = {
    //val indices = accessIndicesOf(access)
    val pattern = accessPatternOf(access)
    val strides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})

    def bankFactor(i: Symbol) = if (iter.isDefined && i == iter.get) 16 else 1

    if (pattern.forall(_ == InvariantAccess)) NoBanks
    else {
      val banking = (pattern, strides).zipped.map{case (pattern, stride) => pattern match {
        case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, bankFactor(i))
        case StridedAccess(Exact(a), i) => StridedBanking(a.toInt*stride, bankFactor(i))
        case OffsetAccess(i, b)         => StridedBanking(stride, bankFactor(i))
        case LinearAccess(i)            => StridedBanking(stride, bankFactor(i))
        case InvariantAccess(b)         => NoBanking
        case RandomAccess               => NoBanking
      }}

      val form = banking.find(_.banks > 1).getOrElse(NoBanking)

      form match {
        case StridedBanking(stride,_)    => Strided(stride)
        case NoBanking if iter.isDefined => Duplicated
        case NoBanking                   => NoBanks
      }
    }
  }
  def mergeBanking(bank1: SRAMBanking, bank2: SRAMBanking) = (bank1,bank2) match {
    case (Strided(s1),Strided(s2)) if s1 == s2 => Strided(s1)
    case (Strided(s1),Strided(s2)) => Diagonal(s1, s2)
    case (Duplicated, _) => Duplicated
    case (_, Duplicated) => Duplicated
    case (NoBanks, bank2) => bank2
    case (bank1, NoBanks) => bank1
  }
}

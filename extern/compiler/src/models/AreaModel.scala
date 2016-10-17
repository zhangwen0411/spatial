package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

// Struct for passing around completed FPGA area analysis results
case class FPGAResourceSummary(
  alms: Int = 0,
  regs: Int = 0,
  dsps: Int = 0,
  bram: Int = 0,
  streams: Int = 0
) {
  def <(that: FPGAResourceSummary)  = this.alms <  that.alms && this.regs <  that.regs && this.dsps <  that.dsps && this.bram <  that.bram && this.streams <  that.streams
  def <=(that: FPGAResourceSummary) = this.alms <= that.alms && this.regs <= that.regs && this.dsps <= that.dsps && this.bram <= that.bram && this.streams <= that.streams
  def >(that: FPGAResourceSummary)  = this.alms >  that.alms && this.regs >  that.regs && this.dsps >  that.dsps && this.bram >  that.bram && this.streams >  that.streams
  def >=(that: FPGAResourceSummary) = this.alms >= that.alms && this.regs >= that.regs && this.dsps >= that.dsps && this.bram >= that.bram && this.streams >= that.streams
}

// Class for operating on intermediate FPGA resource counts
case class FPGAResources(
  lut7: Int = 0,
  lut6: Int = 0,
  lut5: Int = 0,
  lut4: Int = 0,
  lut3: Int = 0,
  mem64: Int = 0,
  mem32: Int = 0,
  mem16: Int = 0,
  regs: Int = 0,
  dsps: Int = 0,
  bram: Int = 0,
  mregs: Int = 0,
  streams: Int = 0
) {
  def +(that: FPGAResources) = FPGAResources(
    lut7 = this.lut7 + that.lut7,
    lut6 = this.lut6 + that.lut6,
    lut5 = this.lut5 + that.lut5,
    lut4 = this.lut4 + that.lut4,
    lut3 = this.lut3 + that.lut3,
    mem64 = this.mem64 + that.mem64,
    mem32 = this.mem32 + that.mem32,
    mem16 = this.mem16 + that.mem16,
    regs = this.regs + that.regs,
    dsps = this.dsps + that.dsps,
    bram = this.bram + that.bram,
    mregs = this.mregs + that.mregs,
    streams = this.streams + that.streams
  )

  // HACK: Don't duplicate BRAM in inner loops (bank instead)
  def replicated(x: Int, inner: Boolean) = {
    val bramNew = if (inner) bram else x*bram
    val mregsNew = if (inner) mregs else x*mregs
    FPGAResources(x*lut7,x*lut6,x*lut5,x*lut4,x*lut3,x*mem64,x*mem32,x*mem16,x*regs,x*dsps,bramNew,mregsNew,x*streams)
  }

  def isNonzero: Boolean = lut7 > 0 || lut6 > 0 || lut5 > 0 || lut4 > 0 || lut3 > 0 ||
                           mem64 > 0 || mem32 > 0 || mem16 > 0 || regs > 0 || dsps > 0 || bram > 0 || mregs > 0 || streams > 0

  override def toString() = s"lut3=$lut3, lut4=$lut4, lut5=$lut5, lut6=$lut6, lut7=$lut7, mem16=$mem16, mem32=$mem32, mem64=$mem64, regs=$regs, dsps=$dsps, bram=$bram, mregs=$mregs, streams=$streams"

  def toArray: Array[Int] = Array(lut7,lut6,lut5,lut4,lut3,mem64,mem32,mem16,regs+mregs,dsps,bram)
}

object NoArea extends FPGAResources()


// ISSUE #33: Should get some of this from loading a file rather than hardcoding
// All numbers here are from Stratix V profiling
trait AreaModel extends NodeMetadataOpsExp with MemoryAnalysisExp {
  this: SpatialExp =>

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silenceAreaModel() { silentModel = true }

  /**
   * Returns the area resources for a delay line with the given width (in bits) and length (in cycles)
   * Models delays as registers for short delays, BRAM for long ones
   **/
  def areaOfDelayLine(width: Int, length: Int, par: Int): FPGAResources = {
    //System.out.println(s"Delay line: w = $width x l = $length (${width*length}) ")
    val nregs = width*length
    if (nregs < 256) FPGAResources(regs = nregs*par)
    else             areaOfSRAM(width*par, List(length), List(SimpleInstance))
  }
  def areaOfDelayLine(width: Int, length: Long, par: Int): FPGAResources = {
    if(length > Int.MaxValue) throw new Exception(s"Casting delay line length to Int would result in overflow")
    areaOfDelayLine(width, length.toInt, par)
  }

  private def areaOfMemWord(nbits: Int) = {
    val m64 = nbits/64
    val m32 = (nbits - m64*64)/32
    val m16 = (nbits - m64*64 - m32*32)/16
    FPGAResources(mem64=m64, mem32=m32, mem16=m16)
  }

  private def areaOfArg(nbits: Int) = FPGAResources(regs=nbits) //3*nbits/2)

  // Set to 0 or lower to disable
  val REG_RAM_DEPTH = 5  // Non-inclusive

  /**
   * Area resources required for an SRAM with word size nbits, and with given depth,
   * number of banks, and double buffering
   *
   * (NoBanking, Strided(1)) =>
   * (Strided(1), NoBanking) =>
   **/
  private def areaOfMemInstance(nbits: Int, origDims: List[Int], instance: MemInstance) = {
    // Physical depth for given word size for horizontally concatenated RAMs
    val wordDepth = if      (nbits == 1)  16384
                    else if (nbits == 2)  8192
                    else if (nbits <= 5)  4096
                    else if (nbits <= 10) 2048
                    else if (nbits <= 20) 1024
                    else                  512

    // Number of horizontally concatenated RAMs required to implement given word
    val portWidth = if (nbits > 40) Math.ceil( nbits / 40.0 ).toInt else 1

    val bufferDepth = instance.depth
    val duplicates = instance.duplicates
    val banking = instance.banking

    var d = 0
    // Group dimensions to match diagonal banking groupings
    val dims = banking.map{
      case DiagonalBanking(strides, _) =>
        val dim = origDims.slice(d, d+strides.length).fold(1){_*_}
        d += strides.length
        dim
      case _ =>
        val dim = origDims(d)
        d += 1
        dim
    }

    val banks = banking.map(_.banks)

    val elementsPerBank = dims.zip(banks).map{case (dim,banks) => Math.ceil(dim.toDouble/banks).toInt}.fold(1){_*_}

    val memoryResourcesPerBank = if (elementsPerBank < REG_RAM_DEPTH) FPGAResources(mregs = nbits * elementsPerBank) else {
      val nRAMs = Math.ceil(elementsPerBank/wordDepth).toInt * portWidth
      FPGAResources(bram = nRAMs)
    }

    val nBanks = banks.fold(1){_*_} * duplicates

    val memoryResources = memoryResourcesPerBank.replicated(nBanks, false)

    val controlResourcesPerBank = if (bufferDepth == 1) NoArea else {
      FPGAResources(lut3 = bufferDepth*nbits, regs= bufferDepth*nbits)
    }
    val controlResources = controlResourcesPerBank.replicated(nBanks, false)

    memoryResources + controlResources
  }

  private def areaOfSRAM(nbits: Int, dims: List[Int], instances: List[MemInstance]) = {
    instances.map{inst => areaOfMemInstance(nbits, dims, inst) }.reduce{_+_}
  }

  def areaOfStreamPipe(n: Int) = NoArea // ISSUE #33: Needs characterization

  def areaOfMetaPipe(n: Int) = FPGAResources(
    lut4 = (11*n*n + 45*n)/2 + 105,  // 0.5(11n^2 + 45n) + 105
    regs = (n*n + 3*n)/2 + 35        // 0.5(n^2 + 3n) + 35
  )
  def areaOfSequential(n: Int) = FPGAResources(lut4=7*n+40, regs=2*n+35)


  /**
   * Returns the area of the given expression, e.
   * Note that this does not include the area of any function bodies
   *   inReduce  - in a tight reduce-accumulate loop (generated hardware often optimized for latency)
   *   inHwScope - used to ignore nodes outside the block being implemented in hardware
   **/
  def areaOf(e: Exp[Any], inReduce: Boolean, inHwScope: Boolean) = {
    val area = e match {
      case Def(d) if !inHwScope => areaOfNodeOutOfScope(e, d)
      case Def(d) if inReduce  => areaOfNodeInReduce(e, d)
      case Def(d) if !inReduce => areaOfNode(e, d)
      case _ => NoArea  // Bound args and constants accounted for elsewhere
    }
    //if (area.bram > 0) System.out.println(s"  $e (reduce = $inReduce): $area")

    area
  }


  private def areaOfNodeOutOfScope(s: Exp[Any], d: Def[Any]): FPGAResources = d match {
    case _:Reg_new[_] if regType(s) != Regular => areaOfNode(s, d)
    case Reflect(d,_,_) => areaOfNodeOutOfScope(s,d)
    case _ => NoArea
  }

  /**
   * Returns the area resources for the given node when specialized for tight update cycles
   * Accumulator calculation+update is often generated as a special case to minimize latencies in tight cycles
   **/
  private def areaOfNodeInReduce(s: Exp[Any], d: Def[Any]): FPGAResources = d match {
    case FltPt_Add(_,_) =>
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=1606,mem16=50) // More registers

    case Reflect(d,_,_) => areaOfNodeInReduce(s,d)
    case _ => areaOfNode(s, d)
  }


  def areaOfCounterRegs(lhs: Exp[Any], cchain: Exp[CounterChain]): FPGAResources = {
    if (isMetaPipe(lhs)) {
      val N = nStages(lhs) - 1          // Number of stages needed for delay
      val P = parsOf(cchain).reduce(_+_) // Number of duplications per counter
      FPGAResources(lut3=N*P*32, regs = 4*N*P*32) // ISSUE #33: Hardcoded 32 bit index sizes
    }
    else NoArea
  }

  // HACK: Need better way of checking const multiplications here
  def areaOfConstMult(c: Int, nbits: Int) = {
    FPGAResources(lut3 = nbits, regs = 2*nbits)
  }

  /**
   * Returns the area resources for the given node
   **/
  private def areaOfNode(s: Exp[Any], d: Def[Any]): FPGAResources = s match {
    case Fixed(_) => areaOfMemWord(nbits(s))
    case Exact(_) => areaOfMemWord(nbits(s))
    case _ => d match {
    case ConstBit(_) => FPGAResources(lut3=1,regs=1)
    case ConstFix(_) => areaOfMemWord(nbits(s))
    case ConstFlt(_) => areaOfMemWord(nbits(s))

    // ISSUE #33: Need characterization
    case _:Fifo_new[_] => NoArea
    case _:Push_fifo[_] => NoArea
    case _:Pop_fifo[_] => NoArea
    case _:Count_fifo[_] => NoArea

    // ISSUE #33: Need characterization
    case Cam_new(_,_) => NoArea
    case Cam_load(_,_) => NoArea
    case Cam_store(_,_,_) => NoArea

    // ISSUE #33: Need characterization
    case Argin_new(_) => areaOfArg(nbits(s))
    case Argout_new(_) => areaOfArg(nbits(s))
    case Reg_new(_) => FPGAResources(regs = nbits(s))

    case e@Sram_new(depth, _) =>
      val dims = dimsOf(s).zipWithIndex.map{case (d,i) => bound(d).getOrElse{throw InvalidMemoryDimensionException(i)(mpos(s.pos)) }.toInt }
      areaOfSRAM(nbits(e._mT), dims, duplicatesOf(s))

    // ISSUE #33: Need characterization
    case e@Sram_load(ram, _) =>
      val decode = 0
      val bits = nbits(e.mT)
      FPGAResources(lut3=decode+bits, regs=decode+bits)

    // ISSUE #33: Need characterization
    case e@Sram_store(ram, _, _, _) =>
      val decode = 0
      val bits = nbits(e.mT)
      FPGAResources(lut3=decode+bits, regs=decode+bits)

    case _:Counter_new => FPGAResources(lut3=106,regs=67)
    case _:Counterchain_new => NoArea

    case FixPt_Neg(_)   => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Add(_,_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Sub(_,_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))

    // ISSUE #33: Have to get numbers for non-32 bit multiplies and divides
    case FixPt_Mul(Exact(c),_) => areaOfConstMult(c.toInt, nbits(s)) // HACK
    case FixPt_Mul(_,Exact(c)) => areaOfConstMult(c.toInt, nbits(s)) // HACK
    case FixPt_Mul(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(dsps = 2)

    case FixPt_Div(Exact(_),_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Div(_,Exact(_)) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      if (sign(s)) FPGAResources(lut3=1192,lut5=2,regs=2700)
      else         FPGAResources(lut3=1317,lut5=6,regs=2900)

    case FixPt_Mod(Exact(_),_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Mod(_,Exact(_)) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Mod(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      if (sign(s)) FPGAResources(lut3=1192,lut5=2,regs=2700)
      else         FPGAResources(lut3=1317,lut5=6,regs=2900)

    case FixPt_Lt(_,_)  => FPGAResources(lut4=nbits(s), regs=nbits(s))
    case FixPt_Leq(_,_) => FPGAResources(lut4=nbits(s), regs=nbits(s))
    case FixPt_Neq(_,_) => FPGAResources(lut4=nbits(s)/2, lut5=nbits(s)/8, regs=nbits(s))
    case FixPt_Eql(_,_) => FPGAResources(lut4=nbits(s)/2, lut5=nbits(s)/8, regs=nbits(s))
    case FixPt_And(_,_) => FPGAResources(lut3=nbits(s), regs=nbits(s))
    case FixPt_Or(_,_)  => FPGAResources(lut3=nbits(s), regs=nbits(s))

    // ISSUE #33: Needs characterization
    //case FixPt_Lsh(_,_) => // ??? nbits(s)*nbits(s) ?
    //case FixPt_Rsh(_,_) => // ???

    // ISSUE #33: Floating point for things besides single precision
    case FltPt_Neg(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50)

    case FltPt_Add(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50) // ~372 ALMs, 1 DSP (around 564)

    case FltPt_Sub(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50)

    case FltPt_Mul(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=152,lut4=10,lut5=21,lut6=2,dsps=1,regs=335,mem16=43) // ~76 ALMs, 1 DSP (around 1967)

    case FltPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=2384,lut4=448,lut5=149,lut6=385,lut7=1,regs=3048,mem32=25,mem16=9)

    case FltPt_Lt(a,_)  =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Leq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Neq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Eql(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)


    case Bit_Not(_)   => FPGAResources(lut3=1,regs=1)
    case Bit_And(_,_) => FPGAResources(lut3=1,regs=1)
    case Bit_Or(_,_)  => FPGAResources(lut3=1,regs=1)
    case Bit_Xor(_,_)  => FPGAResources(lut3=1,regs=1)
    case Bit_Xnor(_,_)  => FPGAResources(lut3=1,regs=1)

    case FixPt_Abs(_) => FPGAResources(lut3=nbits(s),regs=nbits(s))
    case FltPt_Abs(_) => FPGAResources(regs=nbits(s)-1)
    case FltPt_Log(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=472,lut4=47,lut5=74,lut6=26,lut7=3,mem16=42,regs=950,dsps=7,bram=3)

    case FltPt_Exp(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=368,lut4=102,lut5=137,lut6=38,mem16=24,regs=670,dsps=5,bram=2)

    case FltPt_Sqrt(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=476,lut4=6,lut5=6,mem32=11,regs=900)

    case Mux2(_,_,_) => FPGAResources(regs = nbits(s))

    case _:Min2[_] | _:Max2[_] =>
      val lt = if (isFixPtType(s.tp)) FPGAResources(lut3 = nbits(s)+1,regs=1) else FPGAResources(lut4=42,lut6=26,regs=34)
      lt + FPGAResources(regs = nbits(s))

    case Convert_fixpt(_) => FPGAResources(regs=nbits(s))
    //case Convert_fltpt(_) => // ???
    case Fixpt_to_fltpt(x) =>
      if (nbits(s) != 32 && nbits(x) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=50,lut6=132,regs=238)

    case Fltpt_to_fixpt(_) =>
      FPGAResources(lut4=160,lut6=96,regs=223+nbits(s))


    // ISSUE #33: New templates - needs recharacterization
    // Tile Store
    case BurstStore(mem,stream,ofs,len,p) =>
      //val nonConstDims = (dimsOf(tt.mem) ++ tt.memOfs).filterNot{case Fixed(_) => true; case _ => false}.length
      //val dsp = if (nonConstDims > 1) 3 else 0
      //val p = parsOf(cc).reduce{_*_}

      //val brm = if (p < 25) 46 + Math.floor(0.75*(p-1)).toInt else if (p < 49) 62 - Math.floor(0.75*(p-24)).toInt else 42

      //System.out.println(s"Tile store $tt: $brm")
      // Old template
      //FPGAResources(lut3=1900,lut4=167,lut5=207,lut6=516,lut7=11,regs=5636,dsps=dsp,bram=46,streams = 1)
      // New template
      FPGAResources(lut3=893,lut4=91,lut5=96,lut6=618,lut7=10, regs=4692, dsps=0, bram=0, streams=1)  // ~1206 ALMs

      //FPGAResources(lut3=378,lut4=38,lut5=58,lut6=569,lut7=4, regs=3878, dsps=dsp, bram=46, streams=1)

    // Tile Load
    case BurstLoad(mem,stream,ofs,len,p) =>
      //val p = parsOf(cc).reduce{_*_}
      //val nonConstDims = (dimsOf(tt.mem) ++ tt.memOfs).filterNot{case Fixed(_) => true; case _ => false}.length
      //val dsp = if (nonConstDims > 1) 4 else 0
      // New template
      // FPGAResources(lut3=453, lut4=60, lut5=131,lut6=522,regs=1377,dsps=dsp,bram=46, streams=1)
      //val brams = 12 - p/8
      // New template
      FPGAResources(lut3=410, lut4=50, lut5=70, lut6=53, regs=920, dsps=0, bram=0, streams=1) // ~353 ALMs

    case _:ParallelPipe => FPGAResources(lut4=9*nStages(s)/2, regs = nStages(s) + 3)

    case e:OpForeach if isInnerPipe(s)  => NoArea
    case e:OpForeach if isStreamPipe(s) => areaOfStreamPipe(nStages(s))
    case e:OpForeach if isMetaPipe(s)   => areaOfMetaPipe(nStages(s)) + areaOfCounterRegs(s, e.cchain)
    case e:OpForeach if isSequential(s) => areaOfSequential(nStages(s))

    case e:OpReduce[_,_] if isInnerPipe(s)  => NoArea
    case e:OpReduce[_,_] if isStreamPipe(s) => areaOfStreamPipe(nStages(s))
    case e:OpReduce[_,_] if isMetaPipe(s)   => areaOfMetaPipe(nStages(s)) + areaOfCounterRegs(s, e.cchain)
    case e:OpReduce[_,_] if isSequential(s) => areaOfSequential(nStages(s))

    case e:OpMemReduce[_,_] if isInnerPipe(s)  => NoArea // Should not exist
    case e:OpMemReduce[_,_] if isStreamPipe(s) => areaOfStreamPipe(nStages(s))
    case e:OpMemReduce[_,_] if isMetaPipe(s)   => areaOfMetaPipe(nStages(s)) + areaOfCounterRegs(s, e.ccOuter)
    case e:OpMemReduce[_,_] if isSequential(s) => areaOfSequential(nStages(s))

    case _:UnitPipe if isInnerPipe(s)  => NoArea
    case _:UnitPipe if isStreamPipe(s) => NoArea // Should not exist
    case _:UnitPipe if isMetaPipe(s)   => NoArea // Should not exist
    case _:UnitPipe if isSequential(s) => areaOfSequential(nStages(s))

    // Nodes with known zero area cost
    case _:Reg_read[_]     => NoArea
    case _:Reg_write[_]    => NoArea
    case _:Reg_reset[_]    => NoArea
    case _:Dram_new[_]     => NoArea
    case _:FieldApply[_]   => NoArea
    case _:DeliteStruct[_] => NoArea
    case _:ListVector[_]   => NoArea

    // Effects
    case Reflect(d,_,_) => areaOfNode(s,d)
    case Reify(_,_,_) => NoArea
    case _ =>
      warn(s"Don't know area for $d")
      NoArea
  }}
}

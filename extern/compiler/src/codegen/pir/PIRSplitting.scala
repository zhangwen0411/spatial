package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet,Queue,ArrayBuffer}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import ppl.delite.framework.Config

// TODO: Add scheduling deps to split CUs
// TODO: Splitting control logic
// TODO: Large (> 1 physical SRAM) buffers
trait PIRSplitter extends Traversal with SplittingOps {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_until => _, _}

  override val name = "PIR Splitting"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val cus = HashMap[Exp[Any], List[ComputeUnit]]()

  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal CU Splitting")
    for ((pipe,cu) <- cuMapping) {
      val splitCUs = splitCU(cu)
      cus += pipe -> splitCUs
    }

    (b)
  }

  val ComputeMax = SplitCost(aIn=16, vIn=4, vOut=2, vLoc=1, comp=6, write=4, read=4, mems=4)
  val UnitMax    = SplitCost(aIn=2,  vIn=4, vOut=1, vLoc=1, comp=6, write=4, read=4, mems=4)

  def splitCU(cu: ComputeUnit): List[ComputeUnit] = cu match {
    case tu: TileTransferUnit => List(tu)
    case cu: BasicComputeUnit if cu.stages.nonEmpty =>
      val max = if (cu.isUnitCompute) UnitMax else ComputeMax
      if (SpatialConfig.enableSplitting) splitComputeCU(cu, max) else List(cu)
    case cu: BasicComputeUnit => List(cu) // Outer control logic (no compute, so no need to split)
  }
}

trait SplittingOps extends PIRCommon {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_until => _, _}

  class SplitException(val msg: String) extends Exception("Unable to split!")

  val EnableArchExploration = true
  val REDUCE_STAGES = 5  // Number of stages required to do a full reduction
  val LANES = 16

  val cuMapping = HashMap[Exp[Any], ComputeUnit]()
  def allocateCU(pipe: Exp[Any]): ComputeUnit = cuMapping(pipe)

  case class SplitStats(
    cus:  Int = 0,
    ccus: Int = 0,
    tus:  Int = 0,
    ucus: Int = 0,
    maxALUs: Int = 0,
    maxMems: Int = 0,
    alus:   Int = 0,
    mems:   Int = 0,
    vecIn:  Int = 0,
    vecOut: Int = 0
  ) {
    def +(that: SplitStats) = SplitStats(
      cus  = this.cus + that.cus,
      ccus = this.ccus + that.ccus,
      tus  = this.tus + that.tus,
      ucus = this.ucus + that.ucus,
      maxALUs = Math.max(this.maxALUs, that.maxALUs),
      maxMems = Math.max(this.maxMems, that.maxMems),
      alus   = this.alus + that.alus,
      mems   = this.mems + that.mems,
      vecIn  = this.vecIn + that.vecIn,
      vecOut = this.vecOut + that.vecOut
    )

    override def toString = s"$cus, $ccus, $tus, $ucus, " +
                            s"$maxALUs, $maxMems, $alus, $mems, $vecIn, $vecOut"

    def heading = "CUs, CCUs, TUs, UCUs, Max ALUs, Max SRAMs, ALUs, SRAMs, VecIn, VecOut"
  }
  def getStats(cu: BasicComputeUnit) = SplitStats(
    cus = 1,
    ucus = if (cu.isUnitCompute) 1 else 0,
    maxALUs = nUsedALUs(cu),
    maxMems = cu.srams.size,
    alus    = nUsedALUs(cu),
    mems    = cu.srams.size,
    vecIn   = vectorIns(cu).size,
    vecOut  = vectorOuts(cu).size
  )
  def nUsedALUs(cu: BasicComputeUnit) = {
    val lanes = if (cu.isUnitCompute) 1 else LANES

    cu.allStages.map{
      case MapStage(op,_,_) if op != Bypass => lanes
      case _:ReduceStage => lanes // ALUs used by reduction tree
      case _ => 0
    }.fold(0){_+_}
  }

  def reportStats(stats: SplitStats) {
    val SplitStats(cus,ccus,tus,ucus,maxALUs,maxMems,alus,mems,vecIn,vecOut) = stats
    debug(s"  cus = $cus, ccus = $ccus, tus = $tus, ucus = $ucus")
    debug(s"  maxALUs = $maxALUs, maxMems = $maxMems")
    debug(s"  alus = $alus, mems = $mems, vecIn = $vecIn, vecOut = $vecOut")
  }


  case class SplitCost(
    aIn:  Int,  // Scalar inputs
    vIn:  Int,  // Vector inputs
    vOut: Int,  // Vector outputs
    vLoc: Int,  // Local vectors (self-writes)
    comp: Int,  // Compute stages
    write: Int, // Write stages
    read: Int,  // Read SRAMs in compute stages
    mems: Int   // Total SRAMs
  ) {
    def >(that: SplitCost) = (this.aIn > that.aIn || this.vIn > that.vIn || this.vOut > that.vOut ||
                              this.vLoc > that.vLoc || this.comp > (that.comp+that.write) ||
                              this.write > that.write || this.read > that.read || this.mems > that.mems)

    def toStat = SplitStats(alus = comp+write, mems = this.mems, vecIn = vIn, vecOut = vOut)
  }


  /**
    CU splitting is graph partitioning, where each partition has a limited number of inputs, outputs, and nodes

    Given a set of stages, we want to find a set of minimum cuts where each partition satisfies these limits

    This is more restricted/complex than the standard graph partitioning problem as nodes and edges cannot be treated uniformly:
    - Reduction stages represent multiple real ALU stages in the architecture (logV + 1 ?)
    - Reduction stages cannot directly consume CU inputs (requires a bypass stage)
    - Reduction stages cannot directly produce CU outputs (requires a bypass stage)
    - Write address computation stages MUST be preserved/copied within consumer CUs
    - Local vector writes: address and data must be available in the same CU

    Nodes must be duplicated/created depending on how the graph is partitioned
    Could model this as conditional costs for nodes in context of their partition?
   **/
  def report(cost: SplitCost) = {
    val SplitCost(aIn, vIn, vOut, vLoc, comp, writes, read, mems) = cost
    debug(s"  aIn = $aIn, vIn = $vIn, vOut = $vOut, vLoc = $vLoc")
    debug(s"  comp = $comp, wrt = $writes, read = $read, mems = $mems")
  }

  case class WriteGroup(mems: List[CUMemory], stages: List[Stage])

  // Mutable compute/write partitioning
  class Partition(write: Map[Int, WriteGroup], compute: ArrayBuffer[Stage]) {
    var wstages = Map[Int, WriteGroup]() ++ write
    val cstages = ArrayBuffer[Stage]() ++ compute
    def nonEmpty = wstages.nonEmpty || cstages.nonEmpty

    def stageGroups = wstages.values.map(_.stages).toList :+ cstages.toList

    def writeStages = wstages.values.flatMap(_.stages).toList
    def memories = wstages.values.flatMap(_.mems).toList

    // Used as a last resort if we're stuck
    def popSRAM(sram: CUMemory, sramOwners: Set[LocalRef])(implicit ctx: ComputeContext) = {
      val owner = sramOwners.find{case SRAMRef(mem) => mem == sram; case _ => false}.get
      val out = owner match {case LocalRef(s, reg) => LocalRef(s+1, reg) }
      val stage = MapStage(Bypass, List(owner), List(out))

      cstages.foreach{
        case stage@MapStage(_,ins,outs) if ins.contains(owner) =>
          stage.ins = ins.map{case LocalRef(s,SRAMRead(`sram`)) => LocalRef(s+1,SRAMRead(sram)); case ref => ref}
        case _ =>
      }

      val (keep,drop) = recomputeWrites(this, sramOwners)
      wstages = keep

      (stage, drop)
    }

    // Remove a compute stage and re-evaluate the write stages needed to support compute
    def popCompute(sramOwners: Set[LocalRef])(implicit ctx: ComputeContext) = {
      val stage = cstages.remove(0)
      debug(s"  drop stage = $stage")
      val oldStages = wstages
      val (keep, drop) = recomputeWrites(this, sramOwners)
      wstages = keep
      (stage, drop)
    }
    def addCompute(drop: (Stage, Map[Int, WriteGroup])) {
      cstages += drop._1
      val keys = wstages.keySet ++ drop._2.keySet
      val joined = keys.map{k => (wstages.get(k), drop._2.get(k)) match {
        case (Some(a),Some(b)) => k -> WriteGroup((a.mems ++ b.mems).distinct, a.stages)
        case (Some(a),None)    => k -> a
        case (None,Some(b))    => k -> b
      }}.toMap
      wstages = joined
    }

    /*
    Remote write stage calculation isn't supported in the architecture

    def canPopWrite() = {
      wstages.exists{case (i,grp) => grp.stages.length > 1 && grp.mems.nonEmpty }
    }

    // Remove a write stage (no recompute needed)
    def popWrite()(implicit ctx: SourceContext) = {
      if (!canPopWrite()) throw new Exception("Just what do you think you're doing Dave?")
      val (longest, size) = wstages.map{case (i,grp) => (i, grp.stages.length) }.reduce{(a,b) => if (a._2 > b._2) a else b}
      val grp = wstages(longest)
      wstages += longest -> WriteGroup(grp.mems, grp.stages.tail)
      longest -> grp.stages.head
    }
    def addWrite(drop: (Int, Stage)) {
      if (wstages.contains(drop._1)) {
        val cur = wstages(drop._1)
        if (!cur.stages.contains(drop._2)) {
          wstages += drop._1 -> WriteGroup(cur.mems, cur.stages ++ List(drop._2))
        }
      }
      else {
        wstages += drop._1 -> WriteGroup(Nil, List(drop._2))
      }
    }*/
  }
  object Partition {
    def empty = new Partition(Map.empty, ArrayBuffer.empty)
  }


  def inputsOf(stage: Stage): List[LocalMem] = {
    stage.inputMems.filterNot(stage.outputMems contains _ ) // Ignore self-cycles
  }
  def outputsOf(stage: Stage) = stage.outputMems

  object SRAMRef {
    def unapply(x: LocalRef): Option[CUMemory] = x match {
      case LocalRef(_, SRAMRead(mem)) => Some(mem)
      case _ => None
    }
  }

  // Handle nested memory reads
  def recomputeWrites(partition: Partition, sramOwners: Set[LocalRef])(implicit ctx: ComputeContext) = {

    def writeStages(inRefs: Set[LocalRef]) = {
      val ownedSRAMs = (inRefs intersect sramOwners).flatMap(SRAMRef.unapply(_))
      //debug(s"Owned SRAMs: $ownedSRAMs")
      partition.wstages.filter{case (i,grp) => grp.mems.exists(ownedSRAMs contains _) }
    }

    var inRefs: Set[LocalRef] = partition.cstages.flatMap(_.inputRefs).toSet

    /*debug(s"    SRAM reads (compute): ")
    inRefs.foreach{
      case ref@SRAMRef(_) =>
        val owner = partition.cstages.find(_.inputRefs.contains(ref)).get
        debug(s"      $ref : $owner")
      case _ =>
    }*/

    var writes = writeStages(inRefs)
    var oldSize = -1
    while (inRefs.size != oldSize) {
      oldSize = inRefs.size
      inRefs ++= writes.values.map(_.stages).flatten.flatMap(_.inputRefs)
      writes = writeStages(inRefs)
    }

    /*debug(s"    SRAM reads (all): ")
    val all = writes.values.flatMap(_.stages) ++ partition.cstages
    inRefs.foreach{
      case ref@SRAMRef(_) =>
        val owner = partition.cstages.find(_.inputRefs.contains(ref)).get
        debug(s"      $ref : $owner")
      case _ =>
    }*/

    val ownedSRAMs = (inRefs intersect sramOwners).flatMap(SRAMRef.unapply(_))
    val keep = partition.wstages.flatMap{case (i, grp) =>
      val keys = grp.mems filter (ownedSRAMs contains _ )
      if (keys.nonEmpty) Some(i -> WriteGroup(keys,grp.stages)) else None
    }
    val drop = partition.wstages.flatMap{case (i, grp) =>
      val keys = grp.mems filterNot (ownedSRAMs contains _)
      if (keys.nonEmpty) Some(i -> WriteGroup(keys,grp.stages)) else None
    }
    debug(s"    owned: " + ownedSRAMs.map(_.name).mkString(", ") )
    /*keep.foreach{case (k,v) =>
      v.mems.foreach{m => debug(s"    [keep] $k -> ${m.name}: ")  }
    }*/
    drop.foreach{case (k,v) =>
      v.mems.foreach{m => debug(s"    [drop] $k -> ${m.name}") }
    }
    (keep, drop)
  }

  def partitionCost(part: Partition, sramOwners: Set[LocalRef], dbg: Boolean = false)(implicit ctx: ComputeContext) = {
    val localGroups = part.stageGroups

    val local  = localGroups.flatten
    val remote = ctx.cu.allStages diff local
    val localInRefs: Set[LocalRef]   = local.flatMap(_.inputRefs).toSet     // All inputs to stages in this partition
    val localOutRefs: Set[LocalRef]  = local.flatMap(_.outputRefs).toSet    // All outputs from stages in this partition
    val remoteInRefs: Set[LocalRef]  = remote.flatMap(_.inputRefs).toSet
    val remoteOutRefs: Set[LocalRef] = remote.flatMap(_.outputRefs).toSet

    val localVectors = localOutRefs.filter{case LocalRef(_,reg:VectorLocal) => true; case _ => false}

    val (trueLocalVectors, remoteAccumVectors) = localVectors.partition{case ref@LocalRef(_,VectorLocal(mem)) => part.memories.contains(mem) }

    // --- Local inputs/outputs
    val localOutRegs = local.flatMap(_.outputMems).toSet
    val localInRegs   = local.flatMap(_.inputMems).toSet

    // --- Remote inputs/outputs
    val remoteInRegs = remote.flatMap(_.inputMems).toSet
    val remoteOutRegs = remote.flatMap(_.outputMems).toSet

    // --- Scalars
    val argIns  = localInRefs.filter{case LocalRef(_,ScalarIn(_:InputArg)) => true; case _ => false}
    //val scalarOuts = localOutRefs.filter{case LocalRef(_,_:ScalarOut) => true; case _ => false} -- not a thing

    // Communication with other virtual CUs
    val origVectorIns  = localInRegs.filter(_.isInstanceOf[VectorIn])
    val origVectorOuts = localOutRegs.filter(_.isInstanceOf[VectorOut])

    // --- SRAMs
    // The first read of each SRAM in a CU requires an input vector, either from a remote SRAM or for the SRAM's write data
    val remoteReadSRAMs = remoteInRefs.flatMap(SRAMRef.unapply(_))
    val sramVectorIns   = getSRAMOwners(localGroups) // Either a true SRAM read or a remote SRAM usage
    val sramVectorOuts  = (sramVectorIns intersect sramOwners).filter{case SRAMRef(mem) => remoteReadSRAMs.contains(mem); case _ => false }

    val sramVectorLocal = trueLocalVectors

    val sramBypassStages = sramVectorOuts.size // Requires bypass from SRAMRead register to VectorOut (may need ALU)

    // --- Others
    val dataVectorIns  = localInRegs intersect remoteOutRegs
    val dataVectorOuts = (localOutRegs intersect remoteInRegs) filterNot (_.isInstanceOf[SRAMRead]) // Bypassed SRAM reads

    val aIn  = argIns.size
    //val sOut = scalarOuts.size
    val vIn  = sramVectorIns.size + dataVectorIns.size + origVectorIns.size
    val vOut = sramVectorOuts.size + dataVectorOuts.size + remoteAccumVectors.size + origVectorOuts.size
    if (dbg) { debug(s"Vector outs: " + (sramVectorOuts ++ dataVectorOuts ++ remoteAccumVectors ++ origVectorOuts).mkString(", ")) }
    val vLocal = sramVectorLocal.size

    // --- Stage costs
    val rawComputeCost = part.cstages.map{
      case stage:MapStage => 1
      case stage@ReduceStage(op,init,in,acc) =>
        val bypassInputCost  = if (localInRegs.contains(in.reg)) 0 else 1 // Needs bypass added at input
        val bypassOutputCost = if (remoteInRegs.contains(acc))   1 else 0 // Needs bypass added at output
        REDUCE_STAGES + bypassOutputCost + bypassInputCost
    }.fold(0){_+_}

    val writeCost = part.writeStages.length
    val mems = part.memories.length // TODO: One virtual memory may cost more than one physical memory!

    // NOTE: Currently no attempt is made to push read stages earlier than unrelated compute. Should we try this prior to splitting?
    val readStages = part.cstages.lastIndexWhere{stage => stage.outputMems.exists(_.isInstanceOf[ReadAddrWire])} + 1

    val compute = rawComputeCost + sramBypassStages

    SplitCost(aIn, vIn, vOut, vLocal, compute, writeCost, readStages, mems)
  }

  // Currently no distinction - splitting always is compute stage splitting
  // def requiresSplitting(cost: SplitCost, max: SplitCost)(implicit ctx: ComputeContext) = cost > max
  /*def requiresComputeSplitting(cost: SplitCost)(implicit ctx: ComputeContext) = {
    val max = if (ctx.isUnitCompute) UnitMax else ComputeMax
    requiresSplitting(cost) && (cost.comp > max.comp || cost.read > max.read)
  }*/

  // Mark the first read of each SRAM as the "owner" of this SRAM
  // ASSUMPTION: References after the first read are done via delayed registers, not direct connections to the SRAM
  // ASSUMPTION: If two reads occur within two different stage groups (e.g. compute and write A, write A and write B, etc.)
  //             these two reads CANNOT occur to the same SRAM
  def getSRAMOwners(stageGroups: List[List[Stage]]): Set[LocalRef] = {

    def sramRefs(x: List[LocalRef]) = x.filter{case SRAMRef(_) => true; case _ => false}

    // Group set of stages into references for each memory
    def groupByMem(stages: List[Stage]): List[List[LocalRef]] = stages.flatMap{stage => sramRefs(stage.inputRefs) }.groupBy(SRAMRef.unapply(_)).values.toList
    // Get reference with lowest stage number
    def getMemOwner(refs: List[LocalRef]): LocalRef = refs.reduce{(refA,refB) => if (refA.stage < refB.stage) refA else refB}

    stageGroups.map{stages: List[Stage] => groupByMem(stages) }.flatMap{mems => mems.map{refs => getMemOwner(refs)}}.toSet
  }


  def splitComputeCU(orig: BasicComputeUnit, archLimits: SplitCost): List[BasicComputeUnit] = {
    debug("\n\n\n")
    debug(s"Splitting CU: $orig")
    debug(s"Compute: ")
    orig.stages.foreach{stage => debug(s"  $stage")}
    implicit val ctx = ComputeContext(orig)

    val writeGroups = Map[Int,WriteGroup]() ++ orig.writeStages.zipWithIndex.map{case ((mems,stages),i) => i -> WriteGroup(mems,stages.toList) }
    debug(s"SRAM write groups: ")
    writeGroups.foreach{case (i,grp) =>
      debug(s"  Group #$i: ${grp.mems}")
      grp.stages.foreach{stage => debug(s"    $stage") }
    }

    val sramOwners = getSRAMOwners(orig.stages.toList +: orig.writeStages.values.map(_.toList).toList)
    debug(s"SRAM owners:")
    sramOwners.foreach{s => debug(s"  $s") }
    orig.srams.foreach{mem =>
      val owner = sramOwners.find{case SRAMRef(`mem`) => true; case _ => false}
      if (!owner.isDefined) {
        throw new Exception(s"CU $orig does not have an owner stage for memory $mem")
      }
    }

    val partitions: ArrayBuffer[Partition] = ArrayBuffer.empty
    var current: Partition = new Partition(writeGroups, orig.stages)
    var remote: Partition = Partition.empty

    // Early termination check
    while (remote.nonEmpty || current.nonEmpty) {
      debug(s"Computing partition ${partitions.length}")
      var cost = partitionCost(current, sramOwners)
      while (cost > archLimits) {
        remote addCompute current.popCompute(sramOwners) // split off a compute stage and associated write stages, if any
        cost = partitionCost(current, sramOwners)
        //report(cost)
      }
      if (current.cstages.isEmpty) {
        val splitMem = remote.memories.find{sram =>
          val ownerOpt = sramOwners.find{case SRAMRef(mem) => mem == sram; case _ => false}

          if (!ownerOpt.isDefined) {
            throw new Exception(s"No SRAM owner found for memory $sram in CU $orig")
          }
          val owner = ownerOpt.get

          !remote.cstages.exists{case MapStage(Bypass,List(`owner`),List(LocalRef(_,SRAMRead(`sram`)))) => true; case _ => false}
        }
        if (splitMem.isDefined) {
          debug(s"Reached a point where SRAM splitting is required. Splitting read for SRAM ${splitMem.get} and retrying")
          current = remote
          remote = Partition.empty
          remote addCompute current.popSRAM(splitMem.get, sramOwners)
        }
        else {
          if (debugMode) {
            debug(s"Failed splitting with remaining stages: ")
            debug(s"  Write stages: ")
            for ((i,grp) <- remote.wstages) {
              debug(s"    Write Group #$i (local = ${grp.mems}):")
              grp.stages.foreach{stage => debug(s"      $stage")}
            }
            debug(s"  Compute stages: ")
            remote.cstages.foreach{stage => debug(s"    $stage") }

            val cost = partitionCost(remote, sramOwners)
            report(cost)

            current.cstages ++= remote.cstages
            current.wstages ++= remote.wstages

            debug("")
            debug("Cost for each split option: ")
            while(remote.nonEmpty) {
              remote.popCompute(sramOwners)
              cost = partitionCost(remote, sramOwners,dbg=true)
              report(cost)
            }
          }


          var errReport = "Write stages:"
          for ((i,grp) <- remote.wstages) {
            errReport += s"\n  Write Group #$i (local = ${grp.mems}):"
            grp.stages.foreach{stage => errReport += s"\n    $stage"}
          }
          errReport += s"\nCompute stages: "
          remote.cstages.foreach{stage => errReport += s"\n    $stage" }
          throw new SplitException(errReport)
        }
      }
      else {
        debug(s"Partition #${partitions.length}")
        report(partitionCost(current, sramOwners))
        debug(s"  Write stages: ")
        for ((i,grp) <- current.wstages) {
          debug(s"    Memories: " + grp.mems.map(_.name).mkString(", "))
          grp.stages.foreach{stage => debug(s"      $stage")}
        }
        debug(s"  Compute stages: ")
        current.cstages.foreach{stage => debug(s"    $stage") }

        partitions += current
        current = remote
        remote = Partition.empty
      }
      // Splitting off write stages is currently disallowed
      /*while (requiresSplitting(cost)) {
        remote addWrite current.popWrite()   // split off a write stage
        cost = partitionCost(current, sramOwners)
        report(cost)
      }*/
    }

    debug(s"SRAM owners:")
    sramOwners.foreach{s => debug(s"  $s") }

    /*partitions.zipWithIndex.foreach{case (p,i) =>
      debug(s"Partition #$i: ")

    }*/
    //val remap = RegRemapping(new HashMap[LocalMem,LocalMem]())
    val parent = if (partitions.length > 1) {
      val parent = BasicComputeUnit(orig.name, orig.pipe, orig.parent, StreamPipe)
      parent.cchains ++= orig.cchains
      parent.deps = orig.deps
      Some(parent)
    }
    else None

    val cus = partitions.reverse.zipWithIndex.map{case (p,i) =>
      schedulePartitioned(orig, p, i, parent)
    }

    if (debugMode) {
      debug(s"Proposed partitioning for CU $orig: ")
      debug(s"Original compute stages: ")
      orig.stages.foreach{stage => debug(s"  $stage")}

      var maxMems = 0
      var maxComp = 0
      var vecIns = 0
      var vecOuts = 0

      cus.reverse.zip(partitions).zipWithIndex.foreach{case ((cu,p),i) =>
        debug(s"Partition #$i: $cu")
        val cost = partitionCost(p, sramOwners)
        debug(s"Cost:")
        report(cost)
        debug(s"Stats:")
        reportStats(getStats(cu))

        if (cost.mems > maxMems) maxMems = cost.mems
        if (cost.comp > maxComp) maxComp = cost.comp
        vecIns += cost.vIn
        vecOuts += cost.vOut
        //debug(s"  Memories: ")
        //cu.srams.foreach{sram => debug(sram.dumpString) }
        debug(s"  Write stages: ")
        for ((mems,stages) <- cu.writeStages) {
          debug(s"    Memories: " + mems.map(_.name).mkString(", "))
          stages.foreach{stage => debug(s"      $stage")}
        }
        debug(s"  Compute stages: ")
        cu.stages.foreach{stage => debug(s"    $stage") }
      }
      /*debug(s"Original:")
      debug(s"${stats.compute} compute")
      debug(s"${stats.mems} memories")
      debug(s"")
      debug(s"Partitioned")
      debug(s"${stats.splitCUs} CUs")
      debug(s"Max compute  / CU: ${stats.maxMemoryPerCU}")
      debug(s"Max memories / CU: ${stats.maxMemoryPerCU}")
      debug(s"Compute  / CU: ${stats.avgComputePerCU}")
      debug(s"Memories / CU: ${stats.avgMemoryPerCU}")
      debug(s"VecIns   / CU: ${stats.avgVecInPerCU}")
      debug(s"VecOuts  / CU: ${stats.avgVecOutPerCU}")*/
    }

    parent.toList ++ cus.toList
  }

  def schedulePartitioned(orig: BasicComputeUnit, part: Partition, i: Int, parent: Option[BasicComputeUnit]): BasicComputeUnit = {
    val cu = BasicComputeUnit(orig.name+"_"+i, orig.pipe, parent, StreamPipe)
    cu.isUnitCompute = orig.isUnitCompute
    cu.writeStages ++= part.wstages.map{case (i,WriteGroup(mems,stages)) => mems -> ArrayBuffer(stages:_*) }

    cu.srams ++= part.memories

    val local  = part.cstages
    val remote = orig.allStages diff part.stageGroups.flatten

    val localOutputs = local.flatMap(_.outputMems).toSet
    val remoteInputs = remote.flatMap(_.inputMems).toSet

    val localInputs   = local.flatMap(_.inputMems).toSet
    val remoteOutputs = remote.flatMap(_.outputMems).toSet

    val ctx = ComputeContext(cu)

    def global(reg: LocalMem, scalar: Boolean): GlobalMem = reg match {
      case ScalarIn(glob)   => glob
      case VectorIn(glob)   => glob
      case ScalarOut(glob)  => glob
      case VectorOut(glob)  => glob
      case VectorLocal(mem) => VectorMem(mem.name+"_glob")
      case SRAMRead(mem)    => if (scalar) ScalarMem(mem.name+"_data") else VectorMem(mem.name+"_data")
      case _ =>
        if (scalar) ScalarMem(reg.id+"_glob") else VectorMem(reg.id+"_glob")
    }
    def globalOut(reg: LocalMem, scalar: Boolean): LocalMem = global(reg,scalar) match {
      case m: VectorMem => VectorOut(m)
      case m: ScalarMem => ScalarOut(m)
      case m: OutputArg => ScalarOut(m)
    }
    def globalIn(reg: LocalMem, scalar: Boolean): LocalMem = global(reg,scalar) match {
      case m: VectorMem => VectorIn(m)
      case m: ScalarMem => ScalarIn(m)
      case m: InputArg  => ScalarIn(m)
    }
    def rerefIn(reg: LocalMem, scalar: Boolean = cu.isUnitCompute): LocalRef = {
      val in = reg match {
        case _:ConstReg | _:CounterReg => reg
        case _:ReduceReg   => if (localOutputs.contains(reg)) reg else globalIn(reg, scalar)
        case _:AccumReg    => if (localOutputs.contains(reg)) reg else globalIn(reg, scalar)
        case SRAMRead(mem) => if (cu.srams.contains(mem)) reg else globalIn(reg, scalar)
        case _             => if (cu.regs.contains(reg)) reg else globalIn(reg, scalar)
      }
      cu.regs += in
      ctx.refIn(in)
    }
    def rerefOut(reg: LocalMem, scalar: Boolean = cu.isUnitCompute): List[LocalRef] = {
      val outs = reg match {
        case ScalarOut(glob)  => List(reg)
        case VectorOut(glob)  => List(reg)
        case SRAMRead(mem)    => List(globalOut(reg,scalar))
        case VectorLocal(mem) => if (cu.srams.contains(mem)) List(reg) else List(globalOut(reg,scalar))
        case _ =>
          val local = if (localInputs.contains(reg)) List(reg) else Nil
          val glob  = if (remoteInputs.contains(reg)) List(globalOut(reg, scalar)) else Nil
          local ++ glob
      }
      cu.regs ++= outs
      outs.map{out => ctx.refOut(out) }
    }

    part.cstages.foreach{
      case MapStage(op, ins, outs) =>
        val inputs = ins.map{in => rerefIn(in.reg) }
        val outputs = outs.flatMap{out => rerefOut(out.reg) }
        ctx.addStage(MapStage(op, inputs, outputs))

      case ReduceStage(op, init, in, acc) =>
        var input = rerefIn(in.reg)
        if (!isAccum(input.reg)) {
          val redReg = ReduceReg()
          val reduce = ctx.refOut(redReg)
          cu.regs += redReg
          ctx.addStage(MapStage(Bypass, List(input), List(reduce)))
          input = ctx.refIn(redReg)
        }
        cu.regs += acc
        ctx.addStage(ReduceStage(op, init, input, acc))

        if (remoteInputs.contains(acc)) {
          val glob = globalOut(acc, true)
          ctx.addStage(MapStage(Bypass, List(ctx.refIn(acc)), List(ctx.refOut(glob))))
        }
    }

    // --- bypass stages for remotely read SRAMs
    val remoteSRAMReads = remoteInputs.flatMap{case SRAMRead(mem) => Some(mem); case _ => None}
    val localVectorOuts = remoteSRAMReads intersect cu.srams
    localVectorOuts.foreach{sram =>
      val reg = SRAMRead(sram)
      val glob = globalOut(reg, cu.isUnitCompute)
      // If we don't already have a bypass stage for this used value, create one now
      if (!cu.stages.flatMap(_.outputMems).contains(glob)) {
        ctx.addStage( MapStage(Bypass, List(ctx.refIn(reg)), List(ctx.refOut(glob))) )
        cu.regs += glob
        cu.regs += reg
      }
    }


    // --- sram vector inputs
    val rescheduledOutputs = cu.stages.flatMap(_.outputMems).toSet

    cu.srams.foreach{sram => sram.vector match {
      case Some(LocalVector) =>
        if (!rescheduledOutputs.exists{case VectorLocal(`sram`) => true; case _ => false})
          sram.vector = Some(VectorMem(sram.name+"_glob"))
      case _ =>
    }}

    // --- counter chains
    if (parent.isDefined) {
      val ctrl = parent.get

      val f = copyIterators(cu, ctrl)
      def tx(cc: CUCounterChain): CUCounterChain = {
        if (f.contains(cc)) f(cc)
        else if (f.values.toList.contains(cc)) cc  // HACK: DSE
        else {
          val mapping = f.map{case (k,v) => s"$k -> $v"}.mkString("\n")
          throw new Exception(s"Attempted to copy counter $cc in CU $ctrl, but no such counter exists.\nMapping:\n$mapping")
        }
      }

      def swap_cchain_Reg(x: LocalMem) = x match {
        case CounterReg(cc,idx) => CounterReg(tx(cc),idx)
        case _ => x
      }
      def swap_cchains_Ref(x: LocalRef) = x match {
        case LocalRef(i, reg) => LocalRef(i, swap_cchain_Reg(reg))
      }

      cu.allStages.foreach{
        case stage@MapStage(_,ins,_) => stage.ins = ins.map{swap_cchains_Ref(_)}
        case _ =>
      }
      cu.srams.foreach{sram =>
        sram.readAddr = sram.readAddr.map{swap_cchain_Reg(_)}
        sram.writeAddr = sram.writeAddr.map{swap_cchain_Reg(_)}
        sram.swapWrite = sram.swapWrite.map{tx(_)}
        sram.swapRead = sram.swapRead.map{tx(_)}
        sram.writeCtrl = sram.writeCtrl.map{tx(_)}
      }
    }
    else {
      cu.tpe = InnerPipe
      cu.cchains ++= orig.cchains
      cu.deps = orig.deps
    }

    // --- control logic
    // TODO




    cu
  }

 /*{
    debug(s"Splitting CU: $cu")


    val stages = cu.stages.toSet



    def requiresSplitting(stages: Set[Stage]) = {

      val nStages = stages.map{case _:MapStage => 1; case _:ReduceStage => REDUCE_STAGES }.fold(0)(_+_)

      debug(s"Stages: ")
      stages.foreach{stage => debug(s"  $stage") }
      debug(s"Scalar inputs: $scalarsIn")
      debug(s"Scalar outputs: $scalarsOut")
      debug(s"Vector inputs: $vectorsIn")
      debug(s"Vector outputs: $vectorsOut")
      debug(s"Vector locals: $vectorsLocal")
      debug(s"Actual stages: $nStages")

      scalarsIn.size > MAX_SCALAR_IN || scalarsOut.size > MAX_SCALAR_OUT || nStages > MAX_STAGES ||
      vectorsIn.size > MAX_VECTOR_IN || vectorsOut.size > MAX_VECTOR_OUT || vectorsLocal.size > MAX_VECTOR_LOCAL
    }


    def getScheduleFor(stages: Set[Stage])(result: Stage*) = {
      val frontier = Queue[Stage](result:_*)
      var visited  = HashSet[Stage](result:_*)
      var schedule = ArrayBuffer[Stage]()

      while (frontier.nonEmpty) {
        val stage = frontier.dequeue()
        schedule += stage
        val deps = inputsOf(stage).flatMap{in => stages.find{stage => outputsOf(stage) contains in}}.filterNot(visited contains _)
        frontier.enqueue(deps:_*)
        visited ++= deps
      }
      schedule.toList
    }

    val stagesSet = cu.stages.toSet

    val schedules = Map(outStages.map{stage => stage -> getScheduleFor(stagesSet)(stage).toSet }:_*)

    val unusedOutputs = HashSet[Stage]() ++ outStages
    val groups = ArrayBuffer[HashSet[Stage]]()
    def curGroup = groups.last
    var groupSchedule: Set[Stage] = Set.empty

    while (unusedOutputs.nonEmpty) {
      if (groups.isEmpty || groupIsFull(curGroup)) {
        val stage = unusedOutputs.head
        groups += HashSet(stage)
        unusedOutputs -= stage
        groupSchedule = schedules(stage)
      }
      else {
        val stage = unusedOutputs.map{stage => (stage, (groupSchedule union schedules(stage)).size)}.reduce{(a,b) =>
            if (a._2 > b._2) a._1 else b._1
        }
        curGroup += stage
        unusedOutputs -= stage
        groupSchedule ++= schedules(stage)
      }
    }



    /*val computedOutputs = HashMap[Stage, List[LocalMem]]()

    def bfs(frontier: List[Stage], live: List[List[LocalMem]]): Unit = if (frontier.nonEmpty) {
      debug(s"Frontier:")
      frontier.zip(live).foreach{case (stage, liveIn) =>
        debug(s"  $stage")
        debug(s"  $liveIn")
        debug("")
        liveIns(stage) = liveIn
      }

      val inputLivePairs = frontier.zip(live).flatMap{case (stage,liveIn) =>
        inputsOf(stage).map{input => input -> (liveIn :+ input).filterNot(outputsOf(stage) contains _) }
      }
      val inputMap = inputLivePairs.groupBy(_._1).mapValues{lists => lists.map(_._2).flatten.distinct }
      debug(s"Input Mapping: ")
      for ((input,lives) <- inputMap) {
        debug(s"$input : (${lives.length}) - $lives ")
      }
      val inputList: List[(LocalMem, List[LocalMem])] = inputMap.toList
      val inputLiveList: List[(Stage, List[LocalMem])] = inputList.flatMap{case (input,liveIn) => cu.stages.find{stage => outputsOf(stage).contains(input) }.map(stage => stage -> liveIn) }
      val newFrontier = inputLiveList.map(_._1)
      val newLiveIns  = inputLiveList.map(_._2)
      bfs(newFrontier, newLiveIns)
    }

    bfs(outStages, liveOuts)*/
  }*/

}

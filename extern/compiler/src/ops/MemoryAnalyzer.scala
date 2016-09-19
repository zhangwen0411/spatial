package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.Traversal
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.util.GraphUtil

import scala.collection.mutable.HashMap

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

/**
  A Spatial program is internally represented memory declarations, primitive operations, and nested state machines.
  Primitive operations currently may only be defined within innermost state machines, with the exception of register reads.
  Loop state machines have associated iterators which correspond to the loop counters.

  Given memory M with set of accesses A, determine the [minimum?] duplication/banking/buffering scheme required to sustain throughput.

  Two accesses a, b in A are concurrent if a != b and their least common ancestor is a pipelined state machine.
  Two accesses a, b in A are sequential if a != b and their least common ancestor is a sequential state machine.

  An access x in A is either a write to M or a read from M. Write accesses have associated data.
  If M is an addressable memory, access x also has an associated multi-dimensional address.

  Accesses with associated multi-dimensional addresses also have an associated access pattern for each dimension.
  These patterns are defined in terms of loop iterators. Currently the only recognized pattern for a single dimension is
  a * i + b
  where a and b are both loop invariant expression (and may be zero).

  (An expression is loop invariant if it can be statically determined to remain constant across all loop iterations)

  ASSUMPTIONS:
  1. Concurrent reads are always allowed.
  2. Concurrent writes are disallowed unless they are to provably distinct addresses.
  3. Time multiplexing sequential accesses is always cheaper than duplicating memories.
 **/

trait MemoryAnalysisExp extends SpatialAffineAnalysisExp with ControlSignalAnalysisExp {
  this: SpatialExp =>

  sealed abstract class Banking(val banks: Int) { def nDims = 1 }
  object Banking {
    def unapply(x: Banking): Option[Int] = Some(x.banks)
  }
  // Optimization for doing things like diagonal banking for a memory accessed
  // both row- and column-wise.
  case class DiagonalBanking(strides: List[Int], override val banks: Int) extends Banking(banks) {
    override def nDims = strides.length
  }
  // Strided banking scheme. Includes simple, 1D case
  case class StridedBanking(stride: Int, override val banks: Int) extends Banking(banks)
  // No banking
  case object NoBanking extends Banking(1)

  /**
    Metadata for duplicated instances of a single coherent scratchpad. When possible,
    address generators should be coalesced to a single instance. However, this is only
    possible when the addresses can be guaranteed to be in lockstep.
  */
  case class MemInstance(depth: Int, duplicates: Int, banking: List[Banking])

  def SimpleInstance = MemInstance(1, 1, List(NoBanking))

  case class MemDuplicates(insts: List[MemInstance]) extends Metadata
  object duplicatesOf {
    def update(e: Exp[Any], m: List[MemInstance]) { setMetadata(e, MemDuplicates(m)) }
    def apply(e: Exp[Any]) = meta[MemDuplicates](e).map(_.insts).getOrElse(Nil)
  }


  /**
    Metadata for determining which memory instance(s) an access should correspond to.
    Needed to preserve mapping after unrolling
  */
  case class MemInstanceIndices(mapping: Map[Exp[Any], Set[Int]]) extends Metadata
  object instanceIndicesOf {
    def update(access: Exp[Any], mem: Exp[Any], idxs: Set[Int]) = instanceIndicesOf.get(access) match {
      case Some(map) =>
        val newMap = map.filterKeys(_ != mem) + (mem -> idxs)
        setMetadata(access, MemInstanceIndices(newMap))
      case None =>
        setMetadata(access, MemInstanceIndices(Map(mem -> idxs)))
    }
    def add(access: Exp[Any], mem: Exp[Any], idx: Int) = instanceIndicesOf.get(access) match {
      case Some(map) if map contains mem =>
        val newMap = map.filterKeys(_ != mem) + (mem -> (map(mem) + idx))
        setMetadata(access, MemInstanceIndices(newMap))
      case Some(map) =>
        val newMap = map.filterKeys(_ != mem) + (mem -> Set(idx))
        setMetadata(access, MemInstanceIndices(newMap))
      case None =>
        setMetadata(access, MemInstanceIndices(Map(mem -> Set(idx))))
    }
    def get(access: Exp[Any]) = meta[MemInstanceIndices](access).map(_.mapping)

    def apply(access: Exp[Any], mem: Exp[Any]) = {
      val mapping = meta[MemInstanceIndices](access).map(_.mapping).getOrElse(throw NoInstanceIndicesException(access, mem))
      mapping.getOrElse(mem, throw NoInstanceIndicesException(access, mem))
    }
  }

  case class MemPortIndex(mapping: Map[Exp[Any], Map[Int,Set[Int]]]) extends Metadata
  object portOf {
    def get(access: Exp[Any]) = meta[MemPortIndex](access).map(_.mapping)
    def apply(access: Exp[Any], mem: Exp[Any], instIdx: Int): Set[Int] = {
      val map = meta[MemPortIndex](access).map(_.mapping).getOrElse(throw NoPortsException(access, mem, instIdx))
      val portMap = map.getOrElse(mem, throw NoPortsException(access, mem, instIdx))
      portMap.getOrElse(instIdx, throw NoPortsException(access, mem, instIdx))
    }
  }
  object portsOf {
    def update(access: Exp[Any], mem: Exp[Any], instIdx: Int, idx: Set[Int]) = portOf.get(access) match {
      case Some(map) if map contains mem =>
        val newMap = map.filterKeys(_ != mem) + (mem -> (map(mem) + (instIdx -> idx)))
        setMetadata(access, MemPortIndex(newMap))
      case Some(map) =>
        val newMap = map + (mem -> Map(instIdx -> idx))
        setMetadata(access, MemPortIndex(newMap))
      case None =>
        setMetadata(access, MemPortIndex(Map(mem -> Map(instIdx -> idx))))
    }
    def update(access: Exp[Any], mem: Exp[Any], ports: Map[Int,Set[Int]]) {
      ports.foreach{case (instIdx, port) => portsOf(access, mem, instIdx) = port }
    }
    def apply(access: Exp[Any]): Map[Exp[Any], Map[Int,Set[Int]]] = {
      meta[MemPortIndex](access).map(_.mapping).getOrElse(Map.empty)
    }
  }

  case class AccessTopController(mapping: Map[Exp[Any], Map[Int,Controller]]) extends Metadata
  object topControllerOf {
    def update(access: Exp[Any], mem: Exp[Any], instIdx: Int, ctrl: Controller) = topControllerOf.get(access) match {
      case Some(map) if map contains mem =>
        val newMap = map.filterKeys(_ != mem) + (mem -> (map(mem) + (instIdx -> ctrl)))
        setMetadata(access, AccessTopController(newMap))
      case Some(map) =>
        val newMap = map + (mem -> Map(instIdx -> ctrl))
        setMetadata(access, AccessTopController(newMap))
      case None =>
        setMetadata(access, AccessTopController(Map(mem -> Map(instIdx -> ctrl))))
    }
    def get(access: Exp[Any]) = meta[AccessTopController](access).map(_.mapping)
    def apply(access: Exp[Any], mem: Exp[Any], instIdx: Int): Option[Controller] = {
      val mapping = meta[AccessTopController](access).map(_.mapping) // Option Map[Exp, Map[Int,Controller]]
      val portMap = mapping.flatMap(_.get(mem))  // Option[Map[Int,Controller]]
      portMap.flatMap(x => x.get(instIdx))  // Option[Controller]
    }
  }
  object topControllersOf {
    def update(access: Exp[Any], mem: Exp[Any], mapping: Option[Map[Int, Controller]]) {
      if (mapping.isDefined) {
        mapping.get.foreach{case (idx, ctrl) => topControllerOf(access, mem, idx) = ctrl }
      }
    }
    def apply(access: Exp[Any], mem: Exp[Any]) = meta[AccessTopController](access).flatMap(_.mapping.get(mem))
  }

  override def mirror[T<:Metadata](m: T, f: Transformer): T = m match {
    case MemInstanceIndices(map) =>
      MemInstanceIndices(map.map{case (mem,idxs) => f(mem) -> idxs }).asInstanceOf[T]

    case MemPortIndex(mapping) =>
      MemPortIndex(mapping.map{case (mem, idxs) => f(mem) -> idxs}).asInstanceOf[T]

    case AccessTopController(mapping) =>
      AccessTopController(mapping.map{case (mem, ctrls) => f(mem) -> ctrls.map{case (idx,ctrl) => idx -> (f(ctrl.node),ctrl.inReduce) }}).asInstanceOf[T]

    case _ => super.mirror(m,f)
  }

}

// Technically doesn't need a real traversal, but nice to have debugging, etc.
trait BankingBase extends Traversal {
  val IR: SpatialExp with MemoryAnalysisExp
  import IR.{assert => _, infix_until => _, _}

  debugMode = SpatialConfig.verbose
  verboseMode = SpatialConfig.verbose

  //var reachingWrites = Map[(Exp[Any], Access), List[Access]]()
  // TODO: Account for "killing" writes, write ordering
  def reachingWrites(mem: Exp[Any], reader: Access) = writersOf(mem)

  def lca(a: Controller, b: Controller) = GraphUtil.leastCommonAncestor[Controller](a, b, {node => parentOf(node)})

  // TODO: how is distance defined when one controller is the child of the other?
  def lcaWithDistance(a: Controller, b: Controller): (Controller, Int) = {
    if (a == b) (a, 0)
    else {
      val (lca, pathA, pathB) = GraphUtil.leastCommonAncestorWithPaths[Controller](a, b, {node => parentOf(node)})
      debug("    lca: " + lca)
      debug("    node A: " + a)
      debug("    node B: " + b)
      debug("    path A: " + pathA.mkString(", "))
      debug("    path B: " + pathB.mkString(", "))
      if (lca.isEmpty) throw NoCommonControllerException(a,b)

      val parent = lca.get

      if (isOuterControl(parent)) {
        val topA = if (a == parent) parent else pathA.head
        val topB = if (b == parent) parent else pathB.head
        val children = parent +: childrenOf(parent)

        debug("    LCA children: " + children.mkString(", "))
        val idxA = children.indexOf(topA) // FIXME: (Issue #2) Metapipe children assumed to be a
        val idxB = children.indexOf(topB) // linear sequence of stages, not an arbitrary graph

        if (idxA < 0) throw LCADistanceException(a, b)
        if (idxB < 0) throw LCADistanceException(b, a)

        (parent, idxB - idxA) // Negative means B comes before A
      }
      else (parent, 0)
    }
  }

  // Defined as the number of coarse-grained pipeline stages between accesses A and B
  // TODO: How is this defined for streaming cases?
  def lcaWithCoarseDistance(a: Access, b: Access): (Controller, Int) = {
    val (lca, dist) = lcaWithDistance(a.controller, b.controller)
    val coarseDistance = if (isMetaPipe(lca)) dist else 0
    (lca, coarseDistance)
  }

  def childContaining(top: Controller, access: Access) = {
    val child = access.controller
    val (lca, pathA, pathB) = GraphUtil.leastCommonAncestorWithPaths[Controller](top,child, {node => parentOf(node)})

    val accessController = pathB.head
    assert(top != accessController, s"[N-Buffering] Top controller of access $access is top ($top)")
    assert(lca.isDefined && top == lca.get, s"[N-Buffering] Top controller for n-buffer access $access was $lca, not given top ($top)")
    accessController
  }

  // Least common multiple of two integers (smallest integer which has integer divisors a and b)
  def lcm(a: Int, b: Int): Int = {
    val bigA = BigInt(a)
    val bigB = BigInt(b)
    (bigA*bigB / bigA.gcd(bigB)).intValue()
  }

  def areConcurrent(a: Access, b: Access) = {
    val ctrl = lca(a.controller, b.controller).get
    isParallel(ctrl.node) || isInnerPipeline(ctrl)
  }
  def arePipelined(a: Access, b: Access) = {
    val ctrl = lca(a.controller, b.controller).get
    isMetaPipe(ctrl) || isStreamPipe(ctrl)
  }

  //def isInLoop(ctrl: Controller): Boolean = isLoop(ctrl.node) || parentOf(ctrl).map(isInLoop).getOrElse(false)

  // O(N^2), but number of accesses is typically small
  def checkAccesses(mem: Exp[Any], access: List[Access])(func: (Access, Access) => Unit) {
    for (i <- 0 until access.length) {
      for (j <- i+1 until access.length) {
        if (access(i) != access(j)) func(access(i), access(j))
      }
    }
  }
  val allowConcurrentReaders = false
  val allowConcurrentWriters = false
  val allowPipelinedReaders  = true
  val allowPipelinedWriters  = true
  val allowMultipleReaders   = true
  val allowMultipleWriters   = true

  // Override to allow concurrent readers
  def checkConcurrentReaders(mem: Exp[Any]): Unit = checkAccesses(mem,readersOf(mem)){(a,b) =>
    if (areConcurrent(a,b)) throw ConcurrentReadersException(mem, a, b)
  }
  // Override to allow concurrent writers
  def checkConcurrentWriters(mem: Exp[Any]): Unit = checkAccesses(mem,writersOf(mem)){(a,b) =>
    if (areConcurrent(a,b)) throw ConcurrentWritersException(mem, a, b)
  }
  // Override to allow concurrent readers
  def checkPipelinedReaders(mem: Exp[Any]): Unit = checkAccesses(mem,readersOf(mem)){(a,b) =>
    if (arePipelined(a,b)) throw PipelinedReadersException(mem, a, b)
  }
  // Override to allow concurrent writers
  def checkPipelinedWriters(mem: Exp[Any]): Unit = checkAccesses(mem,writersOf(mem)){(a,b) =>
    if (arePipelined(a,b)) throw PipelinedWritersException(mem, a, b)
  }
  // Override to allow multiple readers
  def checkMultipleReaders(mem: Exp[Any]): Unit = if (readersOf(mem).length > 1) {
    throw MultipleReadersException(mem, readersOf(mem))
  }
  // Override to allow multiple writers
  def checkMultipleWriters(mem: Exp[Any]): Unit = if (writersOf(mem).length > 1) {
    throw MultipleWritersException(mem, writersOf(mem))
  }

  def bankAccess(mem: Exp[Any], access: Exp[Any]): (List[Banking], Int) = (List(NoBanking), 1)

  // TODO: How to handle non-matching strides? Just don't bank?
  def meetBanking(mem: Exp[Any], a: Banking, b: Banking) = (a,b) match {
    case (StridedBanking(s1,p), StridedBanking(s2,q)) if s1 == s2 => StridedBanking(s1, lcm(p,q))
    case (DiagonalBanking(s1,p), DiagonalBanking(s2,q)) if s1 == s2 => DiagonalBanking(s1, lcm(p,q))
    case (Banking(1), banking) => banking
    case (banking, Banking(1)) => banking
    case _ => throw MismatchedStridesException(mem, a.toString, b.toString)
  }
  def combineBankings(mem: Exp[Any], bankingA: List[Banking], bankingB: List[Banking]) = {
    var banking: List[Banking] = Nil

    val dimsA = bankingA.map(_.nDims).sum
    val dimsB = bankingB.map(_.nDims).sum

    // TODO: Should we try to detect diagonal banking for more than 2 dimensions?
    // TODO: Should we try to detect diagonal banking for non-contiguous dimensions?
    if (dimsA == dimsB) {
      var i_a = 0
      var i_b = 0
      while (i_a < bankingA.length && i_b < bankingB.length) {
        (bankingA(i_a), bankingB(i_b)) match {
          case (a: DiagonalBanking, b: DiagonalBanking) =>
            banking :+= meetBanking(mem, a,b)
            i_a += 1
            i_b += 1
          case (DiagonalBanking(strides,banks), _) =>
            val a = strides.map{stride => StridedBanking(stride, banks) }
            val b = bankingB.slice(i_b, i_b + a.length)
            banking ++= a.zip(b).map{case (a,b) => meetBanking(mem,a,b) }
            i_a += 1
            i_b += a.length
          case (_, DiagonalBanking(strides,banks)) =>
            val b = strides.map{stride => StridedBanking(stride, banks) }
            val a = bankingA.slice(i_a, i_a + b.length)
            banking ++= a.zip(b).map{case (a,b) => meetBanking(mem,a,b) }
            i_a += b.length
            i_b += 1
          case _ if i_a < bankingA.length - 1 && i_b < bankingB.length - 1 =>
            // Special case for creating diagonally banked memories
            (bankingA(i_a),bankingA(i_a+1), bankingB(i_b),bankingB(i_b+1)) match {
              case (Banking(1),StridedBanking(s1,p), StridedBanking(s2,q),Banking(1)) if p > 1 && q > 1 =>
                banking :+= DiagonalBanking(List(s2,s1), lcm(p,q))
                i_a += 2
                i_b += 2

              case (StridedBanking(s1,p),Banking(1), Banking(1),StridedBanking(s2,q)) if p > 1 && q > 1 =>
                banking :+= DiagonalBanking(List(s1,s2), lcm(p,q))
                i_a += 2
                i_b += 2

              case (a,_, b,_) =>
                banking :+= meetBanking(mem,a,b)
                i_a += 1
                i_b += 1
            }
          case (a,b) =>
            banking :+= meetBanking(mem,a,b)
            i_a += 1
            i_b += 1
        }
      } // End while
    }
    else throw MismatchedDimensionsException(mem, dimsA, dimsB)

    banking
  }

  case class InstanceGroup (
    metapipe: Option[Controller],   // Controller if at least some accesses require n-buffering
    accesses: List[Access],         // All accesses within this group
    instance: MemInstance,          // Banking/buffering/duplication information
    ports: Map[Access, Set[Int]],   // Set of ports each access is connected to
    swaps: Map[Access, Controller]  // Swap controller for done signal for n-buffering
  )

  def bankGroup(mem: Exp[Any], writers: List[Access], reader: Option[Access]): InstanceGroup = {
    debug(s"  Banking group:")
    debug(s"    Reader: $reader")
    debug(s"    Writers: ")
    writers.foreach{writer => debug(s"      $writer")}

    val accesses = writers ++ reader
    val bankings = accesses.map{a => bankAccess(mem, a.access)}
    val banking = bankings.map(_._1).reduce{(a,b) => combineBankings(mem,a,b) }
    val duplicates = bankings.map(_._2).reduce{(a,b) => Math.max(a,b) }

    val group = {
      if (accesses.isEmpty) {
        InstanceGroup(None, accesses, MemInstance(1, duplicates, banking), Map.empty, Map.empty)
      }
      else if (writers.isEmpty && reader.isDefined) {
        InstanceGroup(None, accesses, MemInstance(1, duplicates, banking), Map(reader.get -> Set(0)), Map.empty)
      }
      else {
        val anchor = reader.getOrElse(writers.head)
        debug(s"  Anchor: $anchor")
        val lcas = accesses.map{access =>
          val (lca,dist) = lcaWithCoarseDistance(anchor, access)
          debug(s"  access: $access, LCA = $lca, distance = $dist")
          (lca,dist,access)
        }
        // Find accesses which require n-buffering, group by their controller
        val metapipeLCAs = lcas.filter(_._2 != 0).groupBy(_._1).mapValues(_.map(_._3))

        // Hierarchical metapipelining is currently disallowed
        if (metapipeLCAs.keys.size > 1) throw UnsupportedNBufferException(mem, metapipeLCAs)

        val metapipe = metapipeLCAs.keys.headOption

        val minDist = lcas.map(_._2).min

        // Port 0: First stage to write/read
        // Port X: X stage(s) after first stage
        val bufferPorts = Map(lcas.map{grp => grp._3 -> (grp._2 - minDist)}:_*)
        val depth = bufferPorts.values.max + 1

        metapipe match {
          // Metapipelined case
          case Some(ctrl) =>
            // Partition accesses based on whether they're n-buffered or time multiplexed with the n-buffer
            val (nbuf, tmux) = accesses.partition{access =>
              val common = lca(access.controller, ctrl).get
              debug(s"    lca( $access, $ctrl ) = $common ")
              common == ctrl
            }

            val ports = Map((nbuf.map{a => a -> Set(bufferPorts(a)) } ++
                             tmux.map{a => a -> List.tabulate(depth){i=>i}.toSet}
                           ):_*)

            val swaps = Map(nbuf.map{a => a -> childContaining(ctrl, a) }:_*)

            InstanceGroup(metapipe, accesses, MemInstance(depth, duplicates, banking), ports, swaps)

          // Purely time-multiplexed case
          case None =>
            val ports = bufferPorts.map{case (key, port) => key -> Set(port)}

            InstanceGroup(None, accesses, MemInstance(depth, duplicates, banking), ports, Map.empty)
        }
      }
    }

    debug(s"  Depth: ${group.instance.depth}, Duplicates: $duplicates, Banking: " + banking.mkString(", "))
    debug(s"  MetaPipe controller: ${group.metapipe}")
    debug(s"  Buffer Ports: ")
    (0 until group.instance.depth).foreach{port =>
      val portAccesses = accesses.filter{a => group.ports(a).contains(port) }
      debug(s"  #$port: " + portAccesses.mkString(", "))
    }
    group
  }

  def coalesceDuplicates(mem: Exp[Any], instances: List[InstanceGroup]) = instances


  final def bank(mem: Exp[Any]) {
    debug("")
    debug(s"Inferring instances for memory $mem " + nameOf(mem).map{n => "(" + n + ")"}.getOrElse(""))

    val writers = writersOf(mem)
    val readers = readersOf(mem)

    if (writers.isEmpty && !isArgIn(mem))
      stageWarn("Memory " + nameOf(mem).getOrElse("") + s" ($mem) defined here has no writers!")(mpos(mem.pos))

    if (readers.isEmpty && !isArgOut(mem))
      stageWarn("Memory " + nameOf(mem).getOrElse("") + s" ($mem) defined here has no readers!")(mpos(mem.pos))

    if (!allowMultipleReaders)   checkMultipleReaders(mem)
    if (!allowMultipleWriters)   checkMultipleWriters(mem)
    if (!allowConcurrentReaders) checkConcurrentReaders(mem)
    if (!allowConcurrentWriters) checkConcurrentWriters(mem)
    if (!allowPipelinedReaders)  checkPipelinedReaders(mem)
    if (!allowPipelinedWriters)  checkPipelinedWriters(mem)

    val instanceGroups = if (readers.isEmpty) {
      List(bankGroup(mem, writers, None))
    }
    else {
      readers.map{reader =>
        val reaching = reachingWrites(mem, reader)
        bankGroup(mem, reaching, Some(reader))
      }
    }

    val coalescedInsts = coalesceDuplicates(mem, instanceGroups)

    debug("Instances inferred (after coalescing): ")
    instanceGroups.zipWithIndex.foreach{case (InstanceGroup(metapipe, accesses, instance, ports, swaps), i) =>
      debug(s"  #$i Depth: ${instance.depth}, Duplicates: ${instance.duplicates}, Banking: " + instance.banking.mkString(", "))

      accesses.foreach{a =>
        val access = a.access
        instanceIndicesOf.add(access, mem, i)
        portsOf(access, mem, i) = ports(a)

        swaps.get(a) match {
          case Some(swap) =>
            topControllerOf(access, mem, i) = swap
            debug(s"""   - $a (ports: ${ports(a).mkString(", ")}) [swap: $swap]""")
          case None =>
            debug(s"""   - $a (ports: ${ports(a).mkString(", ")})""")
        }
      }
    }

    duplicatesOf(mem) = instanceGroups.map(_.instance)
  }


}

/**
    BRAM BANKING
    stride - number of contiguous elements mapped to the same bank
    banks  - number of independent address generators corresponding to a single vectorized load
    depth  - number of writes that occur before one read (number of blocks needed to be saved)

    val bram = BRAM[Fix](16,32)
    Pipe(N until 1 par P){i =>    // domain of P should be (1,1,1) if this includes random *writes* to bram
      val a = Reg[Idx]
      Pipe { a := ... }
      Pipe(32 until 1 par Q){j => bram(j) = ... }
      Pipe(...)
    }

    Should be allowed to bank (but not duplicate) writes to outer BRAMs -> restriction on params, not banking
    Otherwise need some more complicated coherency protocol
    Also should have a multiFold in Spatial?

    Examples for an RxC scratchpad:
     Access      Vectorization
      (i)             (1)         Strided(1, 1)
     (i,j)           (1,4)        Strided(1, 4)
     (i,j)           (4,1)        Strided(C, 4)
     (i,j)           (4,4)        Strided((C,1), (4,4)) // Does order matter here?
      b(i)            (4)         Duplicate(4)
    (b(i), i)        (4,4)        Duplicate(4), Strided(1, 4)
   (b(i), c(i))      (4,4)        Duplicate(4), Duplicate(1)
   (i,j)&(j,i)    (1,4)&(4,1)     Diagonal((C,1), 4)

    Data (2x2)        Stride 1        Stride 4        Diagonal
                B  0 | 1 | 2 | 3   0 | 1 | 2 | 3   0 | 1 | 2 | 3
                  --------------- --------------- ---------------
     0 1 2 3       0 | 1 | 2 | 3   0   4   8   C   0   1   2   3
     4 5 6 7       4 | 5 | 6 | 7   1   5   9   D   7   4   5   6
     8 9 A B       8 | 9 | A | B   2   6   A   E   A   B   8   9
     C D E F       C | D | E | F   3   7   B   F   D   E   F   C
                   B = addr%N      B = (addr/S)%S  B = (i + j)%S
                   A = addr/N      A =             A =

     Stride 1  Stride 4     Diagonal
   B  0 | 1     0 | 1        0 | 1
     -------   -------      -------
      0   1     0   4        0   1
      2   3     1   5        2   3
      4   5     2   6        7   4
      6   7     3   7        5   6
      8   9     8   C        8   9
      A   B     9   D        A   B
      C   D     A   E        D   E
      E   F     B   F        F   C
  B:   j%N       i%N        (i+j)%N
  A:(i*C+j)/N  (i/N)*C+j      ???
**/
trait BRAMBanking extends BankingBase {
  import IR._

  override def bankAccess(mem: Exp[Any], access: Exp[Any]): (List[Banking], Int) = {
    val indices = accessIndicesOf(access)
    val pattern = accessPatternOf(access)
    val allStrides = constDimsToStrides(dimsOf(mem).map{case Exact(d) => d.toInt})
    val strides = if (indices.length == 1) List(allStrides.last) else allStrides

    val Def(d) = access
    debug(s"")
    debug(s"  access:  $access = $d")
    debug(s"  indices: $indices")
    debug(s"  pattern: $pattern")

    val factors = unrollFactorsOf(access) diff unrollFactorsOf(mem) // Parallelization factors relative to this memory

    debug(s"  factors: ${factors}")

    var used: Map[Exp[Any], Boolean] = Map.empty
    factors.foreach{factor => used += factor -> false}

    def bankFactor(i: Exp[Any]): Int = {
      val factor = parFactorOf(i)
      if (used.contains(factor) && !used(factor)) {
        used += factor -> true
        parOf(i)
      }
      else 1
    }

    val banking = (pattern, indices, strides).zipped.map{ case (pattern, index, stride) => pattern match {
      case AffineAccess(Exact(a),i,b) => StridedBanking(a.toInt*stride, bankFactor(i))
      case StridedAccess(Exact(a),i)  => StridedBanking(a.toInt*stride, bankFactor(i))
      case OffsetAccess(i,b)          => StridedBanking(stride, bankFactor(i))
      case LinearAccess(i)            => StridedBanking(stride, bankFactor(i))
      case InvariantAccess(b)         => NoBanking // Single "bank" in this dimension
      case RandomAccess               => NoBanking // Single "bank" in this dimension
    }}

    val duplicates = factors.filter{factor => !used(factor)}.map{case Exact(p) => p.toInt}.fold(1){_*_}

    debug(s"  Inferred: " + banking.mkString(", ") + s" (duplicates = $duplicates)")
    (banking, duplicates)
  }
}

trait RegisterBanking extends BankingBase {
  import IR._
}

trait FIFOBanking extends BankingBase {
  import IR._

  override val allowConcurrentReaders = false
  override val allowConcurrentWriters = false
  override val allowPipelinedReaders  = false
  override val allowPipelinedWriters  = false
  override val allowMultipleReaders = false
  override val allowMultipleWriters = false

  override def bankAccess(mem: Exp[Any], access: Exp[Any]): (List[Banking], Int) = {
    val banks = (unrollFactorsOf(access) diff unrollFactorsOf(mem)).map{case Exact(p) => p.toInt}.fold(1){_*_}
    (List(StridedBanking(1, banks)), 1)
  }
}


trait MemoryAnalyzer extends Traversal {
  val IR: SpatialExp with MemoryAnalysisExp
  import IR._

  override val name = "Memory Analyzer"
  debugMode = SpatialConfig.verbose
  verboseMode = SpatialConfig.verbose

  lazy val ctrlAnalyzer = new ControlSignalAnalyzer{val IR: MemoryAnalyzer.this.IR.type = MemoryAnalyzer.this.IR}

  lazy val BRAMAnalyzer = new BRAMBanking{val IR: MemoryAnalyzer.this.IR.type = MemoryAnalyzer.this.IR}
  lazy val RegAnalyzer  = new RegisterBanking{val IR: MemoryAnalyzer.this.IR.type = MemoryAnalyzer.this.IR}
  lazy val FIFOAnalyzer = new FIFOBanking{val IR: MemoryAnalyzer.this.IR.type = MemoryAnalyzer.this.IR}

  def run(localMems: List[Exp[Any]]): Unit = localMems.foreach {
    case mem if isBRAM(mem.tp) => BRAMAnalyzer.bank(mem)
    case mem if isFIFO(mem.tp) => FIFOAnalyzer.bank(mem)
    case mem if isRegister(mem.tp) => RegAnalyzer.bank(mem)
    case mem => throw UnsupportedBankingException(mem)
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    ctrlAnalyzer.run(b)
    run(ctrlAnalyzer.localMems)
    b
  }

}

package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal,NestedBlockTraversal}
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.util.GraphUtil

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait NodeMetadataTypesExp extends SpatialMetadataOpsExp {
  this: SpatialExp =>

  def parsOf(e: Exp[Any]): List[Int] = parFactorsOf(e).map{
    case Const(c: Int) => c
    case Param(p: Int) => p
  }
  def parOf(e: Rep[Any]): Int = if (parsOf(e).isEmpty) 1 else parsOf(e).head
}

// TODO: Can these somehow be derived automatically?
trait NodeMetadataOpsExp extends NodeMetadataTypesExp {
  this: SpatialExp =>

  type LocalWrite = (Exp[Any], Option[Exp[Any]], Option[Exp[Any]]) // Memory, optional value, optional address
  type LocalRead  = (Exp[Any], Option[Exp[Any]])                   // Memory, optional address

  /**
   * Controller parent
   * Defines the controller which controls the reset of the given node.
   * Currently defined only for control nodes (readers/writers use WrittenMemsIn)
   **/
  case class Parent(parent: Exp[Any]) extends Metadata
  object parentOf {
    def update(child: Exp[Any], parent: Exp[Any]): Unit = setMetadata(child, Parent(parent))
    def apply(child: Exp[Any]): Option[Exp[Any]] = meta[Parent](child).map(_.parent)
    def apply(child: Controller): Option[Controller] = child.inReduce match {
      case true => Some((child.node, false))
      case false => parentOf(child.node).map{p => (p,false) }
    }
  }

  /**
   * List of writers for a given memory
   **/
  case class Writers(writers: List[Access]) extends Metadata
  object writersOf {
    def update(mem: Exp[Any], writers: List[Access]): Unit = setMetadata(mem, Writers(writers))
    def apply(mem: Exp[Any]): List[Access] = meta[Writers](mem).map(_.writers).getOrElse(Nil)
  }
  /**
   * List of readers for a given memory
   **/
  case class Readers(readers: List[Access]) extends Metadata
  object readersOf {
    def update(mem: Exp[Any], readers: List[Access]): Unit = setMetadata(mem, Readers(readers))
    def apply(mem: Exp[Any]): List[Access] = meta[Readers](mem).map(_.readers).getOrElse(Nil)
  }

  /**
   * List of memories written in a given controller
   **/
  case class WrittenMemsIn(memories: List[Exp[Any]]) extends Metadata
  object writtenIn {
    def update(ctrl: Exp[Any], memories: List[Exp[Any]]): Unit = setMetadata(ctrl, WrittenMemsIn(memories))
    def update(ctrl: Controller, memories: List[Exp[Any]]): Unit = setMetadata(ctrl.node, WrittenMemsIn(memories))
    def apply(ctrl: Exp[Any]) = meta[WrittenMemsIn](ctrl).map(_.memories).getOrElse(Nil)
    def apply(ctrl: Controller) = meta[WrittenMemsIn](ctrl.node).map(_.memories).getOrElse(Nil)
  }

  /**
   * Controller children
   * A list of control nodes inside given (outer) control node.
   **/
  case class Children(children: List[Exp[Any]]) extends Metadata
  object childrenOf {
    def update(ctrl: Exp[Any], children: List[Exp[Any]]): Unit = setMetadata(ctrl, Children(children))
    def apply(ctrl: Exp[Any]): List[Exp[Any]] = meta[Children](ctrl).map(_.children).getOrElse(Nil)
    def apply(ctrl: Controller): List[Controller] = {
      if (!ctrl.inReduce)
        childrenOf(ctrl.node).map{x => (x,false)} :+ ((ctrl.node,true)) // Only include in special cases?
      else
        childrenOf(ctrl.node).map{x => (x,false)} // Shouldn't have children?
    }
  }

  def mirrorCtrl(x: Controller, f: Transformer): Controller = (f(x.node), x.inReduce)
  def mirrorAccess(x: Access, f: Transformer): Access = (f(x.node), mirrorCtrl(x.controller, f))

  /** Metadata mirroring **/
  override def mirror[T<:Metadata](m: T, f: Transformer): T = (m match {
    case Parent(parent) => Parent(f(parent))
    case Writers(writers) => Writers(writers.map{x => mirrorAccess(x, f) })
    case Readers(readers) => Readers(readers.map{x => mirrorAccess(x, f) })
    case WrittenMemsIn(written) => WrittenMemsIn(written.map{x => f(x)})
    case Children(children) => Children(children.map{x => f(x)})
    case _ => super.mirror(m,f)
  }).asInstanceOf[T]


  // Returns written memory, optional value, optional address
  private def writerUnapply(d: Def[Any]): Option[List[LocalWrite]] = d match {
    case EatReflect(Reg_write(reg,value,_))            => Some(LocalWrite(reg,value))
    case EatReflect(Sram_store(sram,addr,value,_))     => Some(LocalWrite(sram,value,addr))
    case EatReflect(Push_fifo(fifo,value,_))           => Some(LocalWrite(fifo,value))
    case EatReflect(Cam_store(cam,key,value))          => Some(LocalWrite(cam,value,key))
    case EatReflect(Par_sram_store(sram,addr,value,_)) => Some(LocalWrite(sram,value,addr))
    case EatReflect(Par_push_fifo(fifo,value,_,_))     => Some(LocalWrite(fifo,value))
    case EatReflect(BurstLoad(mem,fifo,_,_,_))         => Some(LocalWrite(fifo))
    case EatReflect(Gather(mem,local,addrs,_,_,_))     => Some(LocalWrite(local))
    case EatReflect(e: Convolve[_])                    => Some(LocalWrite(e.output))
    case EatReflect(e: ConvLayer[_])                   => Some(LocalWrite(e.output))
    case _ => None
  }

  // Returns read memory, optional address
  private def readerUnapply(d: Def[Any]): Option[List[LocalRead]] = d match {
    case EatReflect(Reg_read(reg))                  => Some(LocalRead(reg))
    case EatReflect(Sram_load(sram,addr))           => Some(LocalRead(sram,addr))
    case EatReflect(Pop_fifo(fifo,_))               => Some(LocalRead(fifo))
    case EatReflect(Cam_load(cam,key))              => Some(LocalRead(cam,key))
    case EatReflect(Par_sram_load(sram,addr))       => Some(LocalRead(sram,addr))
    case EatReflect(Par_pop_fifo(fifo,_))           => Some(LocalRead(fifo))
    case EatReflect(BurstStore(mem,fifo,_,_,_))     => Some(LocalRead(fifo))
    case EatReflect(Gather(mem,local,addrs,_,_,_))  => Some(LocalRead(addrs))
    case EatReflect(Scatter(mem,local,addrs,_,_,_)) => Some(LocalRead(local) ++ LocalRead(addrs))
    case EatReflect(e: ConvLayer[_])                => Some(LocalRead(e.image))
    case _ => None
  }

  def parize(par: Rep[Int]) = par match {
    case Const(c) =>
      val p = param(c)
      domainOf(p) = (c,c,1)
      p
    case p: Param[_] => p.asInstanceOf[Param[Int]]
    case _ => throw InvalidParFactorException(par)
  }

  private def indicesOf(addr: Exp[Any]): List[Exp[Index]] = addr match {
    case Deff(ListVector(indices)) => indices.flatMap{
      case i if isIndexType(i.tp) => Some(i.asInstanceOf[Exp[Index]])
      case _ => None
    }
    case s: Sym[_] if isIndexType(s.tp) => List(s.asInstanceOf[Exp[Index]])
    case _ => Nil
  }
  private def vectorIndicesOf(addr: Exp[Any]): List[List[Exp[Index]]] = addr match {
    case Deff(ListVector(vector)) if vector.isEmpty => Nil
    case Deff(ListVector(vector)) if isIndexType(vector.head.tp) => List(indicesOf(addr))
    case Deff(ListVector(vector)) =>
      val vecs = vector.map(indicesOf(_))
      if (vecs.exists(_.isEmpty)) Nil else vecs // forall or exists?
    case _ => Nil
  }

  // HACK: Only uses first access (assumed that access to multiple mems is with same address)
  def accessIndicesOf(x: Exp[Any]): List[Exp[Index]] = x match {
    case LocalWriter(accesses) => accesses.flatMap(_._3).headOption.map(indicesOf(_)).getOrElse(Nil)
    case LocalReader(accesses) => accesses.flatMap(_._2).headOption.map(indicesOf(_)).getOrElse(Nil)
    case _ => Nil
  }
  // HACK: Only uses first access (assumed that access to multiple mems is with same address)
  def parIndicesOf(x: Exp[Any]): List[List[Exp[Index]]] = x match {
    case LocalWriter(accesses) => accesses.flatMap(_._3).headOption.map(vectorIndicesOf(_)).getOrElse(Nil)
    case LocalReader(accesses) => accesses.flatMap(_._2).headOption.map(vectorIndicesOf(_)).getOrElse(Nil)
    case _ => Nil
  }

  // Parallelization factors associated with this node
  override def parFactors(e: Exp[Any])(implicit ctx: SourceContext): List[Exp[Int]] = e match {
    case Deff(Counterchain_new(ctrs))  => ctrs.flatMap{ctr => parFactors(ctr) }
    case Deff(Counter_new(_,_,_,par))  => List(par)
    case Deff(BurstLoad(_,_,_,_,par))  => List(par)
    case Deff(BurstStore(_,_,_,_,par)) => List(par)
    case Deff(Scatter(_,_,_,_,par,_))  => List(par)
    case Deff(Gather(_,_,_,_,par,_))   => List(par)
    case _ => super.parFactors(e)
  }

  def isAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_new[_])       => true
    case EatReflect(_:Argin_new[_])     => true
    case EatReflect(_:Argout_new[_])    => true
    case EatReflect(_:Sram_new[_])      => true
    case EatReflect(_:Fifo_new[_])      => true
    case EatReflect(_:Cam_new[_,_])     => true
    case EatReflect(_:Dram_new[_])      => true
    case EatReflect(_:Counter_new)      => true
    case EatReflect(_:Counterchain_new) => true
    case EatReflect(_:DeliteStruct[_])  => true
    case EatReflect(_:ListVector[_])    => true
    case _ => false
  }

  // Allocations which can depend on local, dynamic values
  // Should DRAM be included here? Does it matter?
  def isDynamicAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Counter_new)      => true
    case EatReflect(_:Counterchain_new) => true
    case EatReflect(_:DeliteStruct[_])  => true
    case EatReflect(_:ListVector[_])    => true
    case _ => false
  }
  // Allocations which can be directly used in primitive computation
  def isPrimitiveAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:DeliteStruct[_]) => true
    case EatReflect(_:ListVector[_]) => true
    case _ => false
  }

  def isOffChipTransfer(d: Def[Any]): Boolean = d match {
    case EatReflect(_:BurstLoad[_])  => true
    case EatReflect(_:BurstStore[_]) => true
    case EatReflect(_:Gather[_]) => true
    case EatReflect(_:Scatter[_]) => true
    case EatReflect(_:Convolve[_]) => true
    case EatReflect(_:ConvLayer[_]) => true
    case _ => false
  }
  def isOffChipTransfer(e: Exp[Any]): Boolean = e match {
    case Def(d) => isOffChipTransfer(d)
    case _ => false
  }

  def isParallel(d: Def[Any]): Boolean = d match {
    case EatReflect(_:ParallelPipe) => true
    case _ => false
  }
  def isParallel(s: Exp[Any]): Boolean = s match {
    case Def(d) => isParallel(d)
    case _ => false
  }
  def isUnitPipe(s: Exp[Any]): Boolean = s match {
    case Deff(_:UnitPipe) => true
    case _ => false
  }


  def isPipeline(d: Def[Any]): Boolean = d match {
    case EatReflect(_:OpForeach)    => true
    case EatReflect(_:OpReduce[_,_])  => true
    case EatReflect(_:OpMemReduce[_,_]) => true
    case EatReflect(_:UnitPipe)       => true
    case EatReflect(_:Hwblock)         => true
    case EatReflect(_:UnrolledForeach)  => true
    case EatReflect(_:UnrolledReduce[_,_]) => true
    case _ => false
  }
  def isLoop(d: Def[Any]): Boolean = d match {
    case EatReflect(_:UnrolledForeach)  => true
    case EatReflect(_:UnrolledReduce[_,_]) => true
    case EatReflect(_:OpForeach)    => true
    case EatReflect(_:OpReduce[_,_])  => true
    case EatReflect(_:OpMemReduce[_,_]) => true
    case _ => false
  }
  def isLoop(e: Exp[Any]): Boolean = e match {
    case Def(d) => isLoop(d)
    case _ => false
  }

  // --- Syntax sugar

  object LocalWrite {
    def apply(mem: Exp[Any]): List[LocalWrite] = List( (mem, None, None) )
    def apply(mem: Exp[Any], value: Exp[Any]): List[LocalWrite] = List( (mem, Some(value), None) )
    def apply(mem: Exp[Any], value: Exp[Any], addr: Exp[Any]): List[LocalWrite] = List( (mem, Some(value), Some(addr)) )
  }

  object LocalRead {
    def apply(mem: Exp[Any]): List[LocalRead] = List( (mem,None) )
    def apply(mem: Exp[Any], addr: Exp[Any]): List[LocalRead] = List( (mem,Some(addr)) )
  }

  object LocalWriter {
    def unapply(s: Exp[Any]): Option[List[LocalWrite]] = s match {
      case Def(d) => writerUnapply(d)
      case _ => None
    }
    def unapply(d: Def[Any]): Option[List[LocalWrite]] = writerUnapply(d)
  }

  object LocalReader {
    def unapply(s: Exp[Any]): Option[List[LocalRead]] = s match {
      case Def(d) => readerUnapply(d)
      case _ => None
    }
    def unapply(d: Def[Any]): Option[List[LocalRead]] = readerUnapply(d)
  }

  def isWriter(s: Exp[Any]): Boolean = s match {
    case LocalWriter(_) => true
    case _ => false
  }
  def isWriter(d: Def[Any]): Boolean = d match {
    case LocalWriter(_) => true
    case _ => false
  }

  def isReader(s: Exp[Any]): Boolean = s match {
    case LocalReader(_) => true
    case _ => false
  }
  def isReader(d: Def[Any]): Boolean = d match {
    case LocalReader(_) => true
    case _ => false
  }

  def isAccess(s: Exp[Any]) = isReader(s) || isWriter(s)

  // Register reads are currently "special" nodes - neither controllers nor primitives
  def isRegisterRead(s: Exp[Any]): Boolean = s match {
    case Deff(_:Reg_read[_]) => true
    case _ => false
  }

  def isConstant(x: Exp[Any]): Boolean = x match {
    case Const(c) => true
    case Exact(c) => true
    case Def(ConstBit(c)) => true
    case _ => false
  }

  def isPrimitiveNode(s: Exp[Any]): Boolean = s match {
    case Def(Reify(_,_,_)) => false
    case _ => !isControlNode(s) && !isRegisterRead(s) && !isAllocation(s) && !isConstant(s) && !isGlobal(s)
  }

  def isControlNode(s: Exp[Any]): Boolean = s match {
    case Def(d) => isOuterControl(s) || isInnerControl(s)
    case _ => false
  }
  def isOuterControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => isOuterPipeline(s) || isParallel(d)
    case _ => false
  }
  def isOuterControl(s: Controller): Boolean = !s.inReduce && isOuterControl(s.node)

  def isInnerControl(s: Exp[Any]): Boolean = s match {
    case Def(d) => isInnerPipeline(s) || isOffChipTransfer(d)
    case _ => false
  }
  def isInnerControl(s: Controller): Boolean = s.inReduce || isInnerControl(s.node)

  def isOuterPipeline(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) != InnerPipe
    case _ => false
  }
  def isInnerPipeline(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPipeline(d) && styleOf(s) == InnerPipe
    case _ => false
  }
  def isInnerPipeline(s: Controller): Boolean = s.inReduce || isInnerPipeline(s.node)

  def isOuterLoop(s: Exp[Any]): Boolean = s match {
    case Def(d) => isLoop(d) && styleOf(s) != InnerPipe
    case _ => false
  }

  def isInnerLoop(s: Exp[Any]): Boolean = s match {
    case Def(d) => isLoop(d) && styleOf(s) == InnerPipe
    case _ => false
  }

  def isInnerPipe(s: Exp[Any]): Boolean = isInnerControl(s)
  def isInnerPipe(s: Controller): Boolean = s.inReduce || isInnerControl(s.node)

  def isStreamPipe(s: Exp[Any]): Boolean = isOuterControl(s) && styleOf(s) == StreamPipe
  def isStreamPipe(s: Controller): Boolean = !s.inReduce && isStreamPipe(s.node)

  def isMetaPipe(s: Exp[Any]): Boolean = isOuterControl(s) && styleOf(s) == CoarsePipe
  def isMetaPipe(s: Controller): Boolean = !s.inReduce && isMetaPipe(s.node)

  def isSequential(s: Exp[Any]): Boolean = isOuterControl(s) && styleOf(s) == SequentialPipe
  def isSequential(s: Controller): Boolean = !s.inReduce && isSequential(s.node)

  def isParallelizableLoop(e: Exp[Any]) = {
    (isInnerPipe(e) || isMetaPipe(e) || isStreamPipe(e)) && !childrenOf(e).exists(isOffChipTransfer(_))
  }

  def isAllocation(s: Exp[Any]): Boolean = s match {
    case Def(d) => isAllocation(d)
    case _ => false
  }
  def isDynamicAllocation(s: Exp[Any]): Boolean = s match {
    case Def(d) => isDynamicAllocation(d)
    case _ => false
  }
  def isPrimitiveAllocation(s: Exp[Any]): Boolean = s match {
    case Def(d) => isPrimitiveAllocation(d)
    case _ => false
  }

  def isLocalMemory(s: Exp[Any]) = isReg(s.tp) || isSRAM(s.tp) || isFIFO(s.tp) || isCache(s.tp)

  // Checks to see if lhs is dependent on rhs (used for checking for accum. cycles)
  def hasDependency(lhs: Exp[Any], rhs: Exp[Any]): Boolean = {
    def dfs(frontier: List[Exp[Any]]): Boolean = (frontier.map{
      case s if s == rhs => true
      case Def(d) => dfs(syms(d))
      case _ => false
    }).fold(false){_||_}

    lhs match {
      case Def(d) => dfs(syms(d))
      case _ => false
    }
  }
}


// Uses scheduling to get all statements in a given block
trait SpatialTraversalTools extends NestedBlockTraversal {
  val IR: NodeMetadataOpsExp
  import IR._

  def list(x: List[Exp[Any]]) = x.zipWithIndex.foreach{
    case (s@Def(d),i) if isControlNode(s)  => println(s"   $i. [Ctrl] $s = $d")
    case (s@Def(d),i) if isInnerControl(s) => println(s"   $i. [Pipe] $s = $d")
    case (s@Def(d),i) if isAllocation(s)   => println(s"   $i. [Allc] $s = $d")
    case (s@Def(d),i)                      => println(s"   $i. [None] $s = $d")
    case (s,i)                             => println(s"   $i. [None] $s")
  }

  def getStages(blks: Block[Any]*): List[Exp[Any]] = {
    blks.toList.flatMap(b => getStmsInBlock(b)).map{case TP(s,d) => s}
  }
  def getControlNodes(blk: Block[Any]*) = getStages(blk:_*).filter{s => isControlNode(s) }
  def getAllocations(blk: Block[Any]*)  = getStages(blk:_*).filter{s => isAllocation(s) }

  def getLocalReaders(blk: Block[Any]*) = getStages(blk:_*).filter{s => isReader(s)}
  def getLocalWriters(blk: Block[Any]*) = getStages(blk:_*).filter{s => isWriter(s)}
}


// --- Controller helper functions
trait ControllerTools extends Traversal {
  val IR: SpatialExp with NodeMetadataOpsExp
  import IR.{__ifThenElse => _, infix_until => _, assert => _, _}

  /**
   * @returns The least common ancestor (LCA) controller of two controllers
   **/
  def lca(a: Controller, b: Controller) = GraphUtil.leastCommonAncestor[Controller](a, b, {node => parentOf(node)})

  /**
   * Pipeline distance between controllers a and b:
   * If a and b have a least common ancestor which is neither a nor b,
   * this is defined as the dataflow distance between the LCA's children which contain a and b
   * When a and b are equal, the distance is defined as zero.
   *
   * TODO: How is distance defined when the LCA is a xor b?
   * @returns The LCA of a and b and the pipeline distance.
   */
  def lcaWithDistance(a: Controller, b: Controller): (Controller, Int) = {
    if (a == b) (a, 0)
    else {
      val (lca, pathA, pathB) = GraphUtil.leastCommonAncestorWithPaths[Controller](a, b, {node => parentOf(node)})
      if (lca.isEmpty) throw NoCommonControllerException(a,b)

      val parent = lca.get
      //debug(s"lca($a, $b) = $parent [outer = ${isOuterControl(parent)}]")

      if (isOuterControl(parent)) {
        val topA = if (a == parent) parent else pathA.head
        val topB = if (b == parent) parent else pathB.head
        val children = parent +: childrenOf(parent)

        // ISSUE #2: Children assumed to be a linear sequence of stages
        val idxA = children.indexOf(topA)
        val idxB = children.indexOf(topB)

        if (idxA < 0) throw LCADistanceException(a, b)
        if (idxB < 0) throw LCADistanceException(b, a)

        //debug(s"topA = $topA ($idxA), topB = $topB ($idxB), dist = ${idxA - idxB}")

        (parent, idxB - idxA) // Negative means B comes before A
      }
      else (parent, 0)
    }
  }

  /**
   * Coarse-grained pipeline distance between accesses a and b.
   * If the LCA controller of a and b is a metapipeline, the pipeline distance
   * of the respective controllers for a and b. Otherwise zero.
   *
   * // TODO: How is this defined for streaming cases?
   * @returns The LCA of a and b and the coarse-grained pipeline distance
   **/
  def lcaWithCoarseDistance(a: Access, b: Access): (Controller, Int) = {
    val (lca, dist) = lcaWithDistance(a.controller, b.controller)
    val coarseDistance = if (isMetaPipe(lca)) dist else 0
    (lca, coarseDistance)
  }

  /**
   * @returns The child of the top controller which contains the given access.
   * Undefined if the access is not contained within a child of the top controller
   **/
  def childContaining(top: Controller, access: Access) = {
    val child = access.controller
    val (lca, pathA, pathB) = GraphUtil.leastCommonAncestorWithPaths[Controller](top,child, {node => parentOf(node)})

    if (pathB.isEmpty || lca.isEmpty || top != lca.get)
      throw UndefinedChildException(top, access)

    pathB.head
  }

  /**
   * Returns metapipe controller for given accesses
   **/
  def findMetapipe(mem: Exp[Any], readers: List[Access], writers: List[Access]) = {
    val accesses = readers ++ writers
    assert(accesses.nonEmpty)

    val anchor = if (readers.nonEmpty) readers.head else writers.head
    //debug(s"  anchor = $anchor")

    val lcas = accesses.map{access =>
      val (lca,dist) = lcaWithCoarseDistance(anchor, access)
      //debug(s"    lca($anchor, $access) = $lca ($dist)")
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
    val ports = Map(lcas.map{grp => grp._3 -> (grp._2 - minDist)}:_*)

    //debug(s"  metapipe = $metapipe")
    ports.foreach{case (access, port) => debug(s"    - $access : port #$port")}

    (metapipe, ports)
  }
}
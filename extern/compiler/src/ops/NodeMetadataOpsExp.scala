package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal,NestedBlockTraversal}
import scala.virtualization.lms.common.EffectExp

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

  /** Metadata mirroring **/
  override def mirror[T<:Metadata](m: T, f: Transformer): T = (m match {
    case Parent(parent) => Parent(f(parent))
    case Writers(writers) => Writers(writers.map{x => (f(x.access), (f(x.controlNode), x.inReduce)) })
    case Readers(readers) => Readers(readers.map{x => (f(x.access), (f(x.controlNode), x.inReduce)) })
    case WrittenMemsIn(written) => WrittenMemsIn(written.map{x => f(x)})
    case Children(children) => Children(children.map{x => f(x)})
    case _ => super.mirror(m,f)
  }).asInstanceOf[T]


  // Returns written memory, optional value, optional address
  private def writerUnapply(d: Def[Any]): Option[List[LocalWrite]] = d match {
    case EatReflect(Reg_write(reg,value))             => Some(LocalWrite(reg,value))
    case EatReflect(Bram_store(bram,addr,value))      => Some(LocalWrite(bram,value,addr))
    case EatReflect(Push_fifo(fifo,value,_))          => Some(LocalWrite(fifo,value))
    case EatReflect(Cam_store(cam,key,value))         => Some(LocalWrite(cam,value,key))
    case EatReflect(Par_bram_store(bram,addr,value))  => Some(LocalWrite(bram,value,addr))
    case EatReflect(Par_push_fifo(fifo,value,_,_))    => Some(LocalWrite(fifo,value))
    case EatReflect(Offchip_load_cmd(mem,fifo,_,_,_)) => Some(LocalWrite(fifo))
    case EatReflect(Gather(mem,local,addrs,_,_))      => Some(LocalWrite(local))
    case _ => None
  }

  // Returns read memory, optional address
  private def readerUnapply(d: Def[Any]): Option[List[LocalRead]] = d match {
    case EatReflect(Reg_read(reg))                     => Some(LocalRead(reg))
    case EatReflect(Bram_load(bram,addr))              => Some(LocalRead(bram,addr))
    case EatReflect(Pop_fifo(fifo))                    => Some(LocalRead(fifo))
    case EatReflect(Cam_load(cam,key))                 => Some(LocalRead(cam,key))
    case EatReflect(Par_bram_load(bram,addr))          => Some(LocalRead(bram,addr))
    case EatReflect(Par_pop_fifo(fifo,_))              => Some(LocalRead(fifo))
    case EatReflect(Offchip_store_cmd(mem,fifo,_,_,_)) => Some(LocalRead(fifo))
    case EatReflect(Gather(mem,local,addrs,_,_))       => Some(LocalRead(addrs))
    case EatReflect(Scatter(mem,local,addrs,_,_))      => Some(LocalRead(local) ++ LocalRead(addrs))
    case _ => None
  }

  // Parallelization factors associated with this node
  override def parFactors(e: Exp[Any])(implicit ctx: SourceContext): List[Exp[Int]] = e match {
    case Deff(Counterchain_new(ctrs))         => ctrs.flatMap{ctr => parFactors(ctr) }
    case Deff(Counter_new(_,_,_,par))         => List(par)
    case Deff(Offchip_load_cmd(_,_,_,_,par))  => List(par)
    case Deff(Offchip_store_cmd(_,_,_,_,par)) => List(par)
    case Deff(Scatter(_,_,_,_,par))           => List(par)
    case Deff(Gather(_,_,_,_,par))            => List(par)
    case _ => super.parFactors(e)
  }

  def isAllocation(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Reg_new[_])       => true
    case EatReflect(_:Argin_new[_])     => true
    case EatReflect(_:Argout_new[_])    => true
    case EatReflect(_:Bram_new[_])      => true
    case EatReflect(_:Fifo_new[_])      => true
    case EatReflect(_:Cam_new[_,_])     => true
    case EatReflect(_:Offchip_new[_])   => true
    case EatReflect(_:Counter_new)      => true
    case EatReflect(_:Counterchain_new) => true
    case _ => false
  }
  def isOffChipTransfer(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Offchip_load_cmd[_])  => true
    case EatReflect(_:Offchip_store_cmd[_]) => true
    case EatReflect(_:Gather[_]) => true
    case EatReflect(_:Scatter[_]) => true
    case _ => false
  }
  def isOffChipTransfer(e: Exp[Any]): Boolean = e match {
    case Def(d) => isOffChipTransfer(d)
    case _ => false
  }

  def isParallel(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_parallel) => true
    case _ => false
  }
  def isParallel(s: Exp[Any]): Boolean = s match {
    case Def(d) => isParallel(d)
    case _ => false
  }

  def isPipeline(d: Def[Any]): Boolean = d match {
    case EatReflect(_:Pipe_foreach)    => true
    case EatReflect(_:Pipe_fold[_,_])  => true
    case EatReflect(_:Accum_fold[_,_]) => true
    case EatReflect(_:Unit_pipe)       => true
    case EatReflect(_:Hwblock)         => true
    case EatReflect(_:ParPipeForeach)  => true
    case EatReflect(_:ParPipeReduce[_,_]) => true
    case _ => false
  }
  def isLoop(d: Def[Any]): Boolean = d match {
    case EatReflect(_:ParPipeForeach)  => true
    case EatReflect(_:ParPipeReduce[_,_]) => true
    case EatReflect(_:Pipe_foreach)    => true
    case EatReflect(_:Pipe_fold[_,_])  => true
    case EatReflect(_:Accum_fold[_,_]) => true
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

  // Register reads are currently "special" nodes - neither controllers nor primitives
  def isRegisterRead(s: Exp[Any]): Boolean = s match {
    case Deff(_:Reg_read[_]) => true
    case _ => false
  }

  def isConstant(s: Exp[Any]): Boolean = s match {
    case Fixed(_) => true
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
  def isLocalMemory(s: Exp[Any]) = isRegister(s.tp) || isBRAM(s.tp) || isFIFO(s.tp) || isCache(s.tp)

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


object ReductionTreeAnalysis {
  /*
    Calculate delay line costs:
    a. Determine time (in cycles) any given input or internal signal needs to be delayed
    b. Distinguish each delay line as a separate entity

    Is there a concise equation that can capture this? Haven't been able to come up with one.
    E.g.
      8 inputs => perfectly balanced binary tree, no delay paths
      9 inputs => 1 path of length 3
      85 inputs => 3 paths with lengths 2, 1, and 1
  */
  def reductionTreeDelays(nLeaves: Int): List[Long] = {
    if ( (nLeaves & (nLeaves - 1)) == 0) Nil // Specialize for powers of 2
    // Could also have 2^k + 1 case (delay = 1 path of length k)
    else {
      def reduceLevel(nNodes: Int, completePaths: List[Long], currentPath: Long): List[Long] = {
        if (nNodes <= 1) completePaths  // Stop when 1 node is remaining
        else if (nNodes % 2 == 0) {
          // For an even number of nodes, we don't need any delays - all current delay paths end
          val allPaths = completePaths ++ (if (currentPath > 0) List(currentPath) else Nil)
          reduceLevel(nNodes/2, allPaths, 0L)
        }
        // For odd number of nodes, always delay exactly one signal, and keep delaying that signal until it can be used
        else reduceLevel((nNodes-1)/2 + 1, completePaths, currentPath+1)
      }

      reduceLevel(nLeaves, Nil, 0L)
    }
  }

  def reductionTreeHeight(nLeaves: Int): Int = {
    def treeLevel(nNodes: Int, curHeight: Int): Int = {
      if (nNodes <= 1) curHeight
      else if (nNodes % 2 == 0) treeLevel(nNodes/2, curHeight + 1)
      else treeLevel((nNodes - 1)/2 + 1, curHeight + 1)
    }
    treeLevel(nLeaves, 0)
  }
}

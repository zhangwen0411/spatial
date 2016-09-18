package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.collection.mutable.{HashSet,HashMap}

trait ControlSignalAnalysisExp extends NodeMetadataOpsExp {this: SpatialExp => }
trait UnrolledControlSignalAnalysisExp extends ControlSignalAnalysisExp {this: SpatialExp => }

// TODO: This analyzer has a somewhat different pattern of "do x for all nodes" prior to the usual pattern matching traversal rules
// This is a small issue with existing traversal API, as traverse calls itself to eat reflect nodes
// Can this be generalized and added to the standard traversal API? Currently mimicking EatReflect traversal + some extra stuff
// (Probably want to put EatReflect in traverseStm in Traversal instead? But then to add exceptions to this behavior need to override traverseStm and traverse...)

// (1)  Sets parent control nodes of local memories
// (2)  Sets parent control nodes of controllers
// (3)  Sets children control nodes of controllers
// (4)  Sets reader control nodes of locally read memories
// (5)  Sets writer control nodes of locally written memories
// (6)  Flags accumulators
// (7)  Records list of local memories
// (8)  Records set of metapipes
// (9)  Set parallelization factors of memory readers and writers relative to memory
// (10) Sets written set of controllers
// (11) Determines the top controller
trait ControlSignalAnalyzer extends Traversal {
  val IR: SpatialExp with ControlSignalAnalysisExp
  import IR._

  override val name = "Control Signal Analyzer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  // --- State
  var level = 0
  var controller: Option[Controller] = None                     // Parent controller for the current scope
  var pendingReads: Map[Exp[Any],List[Exp[Any]]] = Map.empty    // Memory reads outside of inner pipes
  var pendingExternalReads: Set[Exp[Any]] = Set.empty           // Memory reads outside the hardware block
  var unrollFactors = List[Exp[Int]]()                          // Unrolling factors for the current scope

  // --- Output (to DSE)
  var localMems = List[Exp[Any]]()    // Local memories
  var metapipes = List[Exp[Any]]()    // List of metapipes which can be sequentialized
  var top: Exp[Any] = null

  // During preprocessing, clear out metadata that we append to in order to get rid of stale symbols
  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    localMems = Nil
    metapipes = Nil
    top = null
    controller = None
    for ((s,p) <- metadata) { // TODO: Better way to clear stale data?
      if (meta[Readers](s).isDefined) readersOf(s) = Nil
      if (meta[WrittenMemsIn](s).isDefined) writtenIn(s) = Nil
      if (meta[Writers](s).isDefined) writersOf(s) = Nil
      if (meta[Children](s).isDefined) childrenOf(s) = Nil
    }
    super.preprocess(b)
  }

  // Traverse the scope of the given controller
  def traverseWith(ctrl: Controller)(b: Block[Any]) {
    level += 1
    val prevCtrl = controller
    val prevReads = pendingReads

    controller = Some(ctrl)
    traverseBlock(b)

    controller = prevCtrl
    pendingReads = prevReads
    level -= 1
  }
  // Traverse the scope of the given iterative controller
  def traverseWith(ctrl: Controller, inds: List[Exp[Any]], cc: Rep[CounterChain])(b: Block[Any]) {
    val prevUnrollFactors = unrollFactors
    val factors = parFactorsOf(cc)
    inds.zip(factors).foreach{case (i,p) => parFactorOf(i) = p }

    // ASSUMPTION: Only parallelize by innermost loop currently
    unrollFactors ++= List(parFactors(cc).last) //unrollFactors ++= factors

    traverseWith(ctrl)(b)
    unrollFactors = prevUnrollFactors
  }

  private def dfs(frontier: List[Exp[Any]]): List[Exp[Any]] = {
    frontier.flatMap{
      case Def(d) => dfs(syms(d))
      case bnd => List(bnd)
    }
  }

  /**
   * In general, we assume that primitive nodes are wrapped within controllers for scheduling purposes
   * (The Unit Pipe Transformer inserts these wrappers for users to simplify programs)
   * Here, however, we relax this assumption for memory reads to two cases:
   *  1. The memory read is in an inner pipe: we use the inner pipe as the "Reader" controller
   *  2. The memory read is in an outer pipe: we use all consumers of this read as "Reader" controllers
   *
   * Note that this is required primarily for register reads, which cannot always be wrapped.
   **/
  def appendReader(reader: Exp[Any], ctrl: Controller) = {
    val LocalReader(reads) = reader
    reads.foreach{ case (EatAlias(mem),addr) =>
      readersOf(mem) = readersOf(mem) :+ (reader, ctrl)
    }
  }
  def addPendingReader(reader: Exp[Any]) {
    pendingReads += reader -> List(reader)
    debug(s"Added pending reader: $reader")
  }
  def addPendingExternalReader(reader: Exp[Any]) {
    pendingExternalReads += reader
    debug(s"Added pending external reader: $reader")
  }

  def addReader(reader: Exp[Any], ctrl: Controller) {
    if (isInnerPipe(ctrl))
      appendReader(reader, ctrl)
    else
      addPendingReader(reader)
  }

  def appendWriter(writer: Exp[Any], ctrl: Controller) = {
    val LocalWriter(writes) = writer
    writes.foreach{ case (EatAlias(mem),value,addr) =>

      writersOf(mem) = writersOf(mem) :+ (writer, ctrl)        // (5)
      writtenIn(ctrl) = writtenIn(ctrl) :+ mem                 // (10)
      // This memory is set as an accumulator if it's written value depends on the memory (some read node)
      value.foreach{input => isAccum(mem) = hasDependency(input, mem) }  // (6)
    }
  }

  def addWriter(writer: Exp[Any], ctrl: Controller) {
    if (isInnerPipe(ctrl))
      appendWriter(writer, ctrl)
    else
      throw ExternalMemoryWriteException(writer, ctrl)
  }

  /**
   * Allocations
   **/
  def addAllocation(alloc: Exp[Any], ctrl: Exp[Any]) {
    parentOf(alloc) = ctrl // (1)
    if (isLocalMemory(alloc)) localMems ::= alloc // (7)
  }

  /**
   * Children/Parent Controllers
   **/
  def addChild(child: Exp[Any], ctrl: Exp[Any]) {
    parentOf(child) = ctrl                        // (2)
    childrenOf(ctrl) = childrenOf(ctrl) :+ child  // (3)
  }

  def addCommonControlData(lhs: Exp[Any], rhs: Def[Any]) {
    // Set total unrolling factors of this node's scope + internal unrolling factors in this node
    unrollFactorsOf(lhs) = unrollFactors ++ parFactors(lhs) // (9)

    if (controller.isDefined) {
      // Add pending readers
      val ctrl = controller.get
      val parent = if (isControlNode(lhs)) (lhs,false) else ctrl

      val deps = readSyms(rhs)
      val delayedReads = deps.filter(pendingReads.keySet contains _)
      val readers = delayedReads.flatMap{sym => pendingReads(sym)}

      if (readers.nonEmpty) {
        debug(s"Checking node for dependencies on pending readers:")
        debug(s"  $lhs = $rhs")
        debug("  " + deps.mkString(", "))

        if (isAllocation(lhs)) { // TODO: Other propagaters?
          debug(s"""  Found propagating dep ($lhs) for pending readers (${readers.mkString(",")})""")
          pendingReads += lhs -> readers
        }
        else {
          debug(s"""  Found true dep ($lhs) of pending readers (${readers.mkString(",")})""")
          readers.foreach{reader => appendReader(reader, parent) }
        }
      }

      if (isAllocation(lhs)) addAllocation(lhs, parent.node)    // (1,7)
      if (isReader(lhs)) addReader(lhs, parent)                 // (4)
      if (isWriter(lhs)) addWriter(lhs, parent)                 // (5,6,10)
    }
    else {
      // For input arguments read outside the hardware block
      if (isReader(lhs)) addPendingExternalReader(lhs)
      if (isAllocation(lhs) && (isArgOut(lhs) || isArgIn(lhs))) localMems ::= lhs
    }

    if (isControlNode(lhs)) {
      if (controller.isDefined) addChild(lhs, controller.get.node)     // (2,3)
      else {
        top = lhs                                                      // (11)

        if (pendingExternalReads.nonEmpty) {
          pendingExternalReads.foreach{reader => appendReader(reader, (top,false)) }
        }
      }

      if (isMetaPipe(lhs)) metapipes ::= lhs                           // (8)
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    addCommonControlData(lhs, rhs)

    // Hack: usually would use EatReflect here
    rhs match {
      case Reflect(inner,_,_) => analyze(lhs, inner)
      case _ => analyze(lhs, rhs)
    }
  }

  def analyze(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(blk)       => traverseWith((lhs, false))(blk)
    case Pipe_parallel(blk) => traverseWith((lhs, false))(blk)
    case Unit_pipe(blk)     => traverseWith((lhs, false))(blk)

    case Pipe_foreach(cc,func,inds) =>
      traverseWith((lhs, false), inds, cc)(func)

    case Pipe_fold(cc,a,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) =>
      aliasOf(acc) = a
      traverseWith((lhs, false), inds, cc)(func)
      traverseWith((lhs, false), inds, cc)(rFunc)
      traverseWith((lhs, false), inds, cc)(ld)
      traverseWith((lhs, false), inds, cc)(st)
      isAccum(a) = true                                   // (6)
      parentOf(a) = lhs  // Reset accumulator with reduction

    case Accum_fold(cc1,cc2,a,zero,fA,iFunc,func,ld1,ld2,rFunc,st,inds1,inds2,idx,part,acc,res,rV) =>
      aliasOf(acc) = a
      aliasOf(part) = getBlockResult(func)
      traverseWith((lhs, false), inds1, cc1)(func)
      traverseWith((lhs, true),  inds2, cc2)(rFunc)
      traverseWith((lhs, true),  inds2, cc2)(ld1)
      traverseWith((lhs, true),  inds2, cc2)(ld2)
      traverseWith((lhs, true),  inds2, cc2)(st)
      isAccum(a) = true                                    // (6)
      parentOf(a) = lhs  // Reset accumulator with reduction, not allocation

    case _ => blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
}



// --- Control analysis on unrolled IR
// Expanded version of control signal analysis on the unrolled IR
trait UnrolledControlSignalAnalyzer extends ControlSignalAnalyzer {
  val IR: SpatialExp with ControlSignalAnalysisExp
  import IR._

  override val name = "Control Signal Analyzer [Post-Unrolling]"

  var propagationPairs = List[(Exp[Any],Exp[Any])]()

  def traverseUnrolled(ctrl: Exp[Any], inds: List[List[Exp[Any]]], cc: Rep[CounterChain])(b: Block[Any]) {
    traverseWith((ctrl, false))(b)
  }

  override def analyze(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case ParPipeForeach(cc,func,inds) =>
      traverseUnrolled(lhs, inds, cc)(func)

    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) =>
      aliasOf(acc) = accum
      traverseUnrolled(lhs, inds, cc)(func)
      // rFunc isn't "real" anymore
      isAccum(accum) = true                                 // (6)
      parentOf(accum) = lhs // Reset accumulator with reduction, not allocation

      // TODO: Investigate why all metadata isn't copied properly
      propagationPairs ::= (accum, acc)

    case _ => super.analyze(lhs, rhs)
  }

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    propagationPairs.foreach{case (src,dest) =>
      setProps(dest,getProps(src))
      //getProps(src).foreach{m => debug(s"$src" + makeString(m)) }
      //getProps(dest).foreach{m => debug(s"$dest" + makeString(m)) }
    }
    b
  }

}

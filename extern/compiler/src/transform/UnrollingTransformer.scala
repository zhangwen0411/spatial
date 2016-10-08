package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import spatial.compiler._
import spatial.compiler.ops._

trait UnrollingTransformExp extends ReductionAnalysisExp with UnrolledOpsExp {
  this: SpatialExp =>

  case class UnrolledResult(isIt: Boolean) extends Metadata
  object isReduceResult {
    def update(e: Exp[Any], isIt: Boolean) { setMetadata(e, UnrolledResult(isIt)) }
    def apply(e: Exp[Any]) = meta[UnrolledResult](e).map(_.isIt).getOrElse(false)
  }

  case class ReduceStarter(isIt: Boolean) extends Metadata
  object isReduceStarter {
    def update(e: Exp[Any], isIt: Boolean) { setMetadata(e, ReduceStarter(isIt)) }
    def apply(e: Exp[Any]) = meta[ReduceStarter](e).map(_.isIt).getOrElse(false)
  }

  case class PartOfTree(node: Exp[Any]) extends Metadata
  object rTreeMap {
    def update(e: Exp[Any], node: Exp[Any]) { setMetadata(e, PartOfTree(node)) }
    def apply(e: Exp[Any]) = meta[PartOfTree](e).map(_.node).getOrElse(Nil)
  }
}

trait UnrollingTransformer extends MultiPassTransformer {
  val IR: UnrollingTransformExp with SpatialExp
  import IR.{infix_until => _, Array => _, assert => _, __ifThenElse => _, _}

  override val name = "Unrolling Transformer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  var cloneFuncs: List[Exp[Any] => Unit] = Nil
  def duringClone[T](func: Exp[Any] => Unit)(blk: => T): T = {
    val prevCloneFuncs = cloneFuncs
    cloneFuncs ::= func
    val result = blk
    cloneFuncs = prevCloneFuncs
    result
  }
  def inReduction[T](blk: => T): T = {
    duringClone{e => if (SpatialConfig.genCGRA) reduceType(e) = None }{ blk }
  }
  def withAccessIndices[T](inds: List[Exp[Index]])(blk: => T): T = {
    duringClone{e => if (isAccess(e)) accessIndicesOf(e) = inds }{ blk }
  }

  /**
   * Helper class for unrolling
   * Tracks multiple substitution contexts in 'laneSubst' array
   **/
  case class Unroller(cchain: Exp[CounterChain], inds: List[Exp[Index]], inner: Boolean, unrolledInds: Option[List[List[Sym[Index]]]] = None) {
    // Don't unroll inner loops for CGRA generation
    val Ps = if (inner && SpatialConfig.genCGRA) inds.map{i => 1} else parsOf(cchain)
    val P = Ps.reduce(_*_)
    val N = Ps.length
    val prods = List.tabulate(N){i => Ps.slice(i+1,N).fold(1)(_*_) }
    val indices = unrolledInds.getOrElse{ Ps.map{p => List.fill(p){fresh[Index]}} }

    def size = P

    def parAddr(p: Int) = List.tabulate(N){d => (p / prods(d)) % Ps(d) }

    // Substitution for each duplication "lane"
    val laneSubst = Array.tabulate(P){p =>
      val inds2 = indices.zip(parAddr(p)).map{case (vec, i) => vec(i) }
      withSubstScope(inds.zip(inds2):_*){ subst }
    }

    def inLane[A](i: Int)(block: => A): A = {
      withSubstRules(laneSubst(i)){
        val result = block
        laneSubst(i) = subst
        result
      }
    }

    def map[A](block: Int => A): List[A] = List.tabulate(P){p => inLane(p){ block(p) } }

    def foreach(block: Int => Unit) { map(block) }

    def vectorize[T:Manifest](block: Int => Exp[T]): Exp[Vector[T]] = vector_create_from_list(map(block))

    // --- Each unrolling rule should do at least one of three things:
    // 1. Split a given vector as the substitution for the single original symbol
    def duplicate(s: Sym[Any], d: Def[Any]): List[Exp[Any]] = map{p =>
      val s2 = self_clone(s,d)
      register(s -> s2)
      s2
    }
    // 2. Make later stages depend on the given substitution across all lanes
    // NOTE: This assumes that the node has no meaningful return value (i.e. all are Pipeline or Unit)
    // Bad things can happen here if you're not careful!
    def split[T:Manifest](orig: Sym[Any], vec: Exp[Vector[T]]): List[Exp[T]] = map{p =>
      val element = vec_apply[T](vec, p)
      register(orig -> element)
      element
    }
    // 3. Create an unrolled clone of symbol s for each lane
    def unify(orig: Exp[Any], unrolled: Exp[Any]): List[Exp[Any]] = {
      foreach{p => register(orig -> unrolled) }
      List(unrolled)
    }

    // Same symbol for all lanes
    def isCommon(e: Exp[Any]) = laneSubst.map{p => f(e)}.forall{e2 => e2 == f(e)}
  }

  /**
   * Create duplicates of the given node or special case, vectorized version
   * NOTE: Only can be used within reify scope
   **/
  def unroll[T](lhs: Sym[T], rhs: Def[T], lanes: Unroller)(implicit ctx: SourceContext): List[Exp[Any]] = rhs match {
    // Account for the edge case with FIFO writing
    case EatReflect(e@Push_fifo(fifo@EatAlias(mem), value, en)) if lanes.isCommon(fifo) =>
      debugs(s"Unrolling $lhs = $rhs")
      val values  = lanes.vectorize{p => f(value)}
      val valids  = boundChecks(lanes.cchain, lanes.indices)
      val enables = lanes.vectorize{p => f(en) && valids(p) }
      val parPush = par_push_fifo(f(fifo), values, enables, true)(e._mT,e.__pos)

      setProps(parPush, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      cloneFuncs.foreach{func => func(parPush) }
      lanes.unify(lhs, parPush)

    case EatReflect(e@Pop_fifo(fifo@EatAlias(mem))) if lanes.isCommon(fifo) =>
      debugs(s"Unrolling $lhs = $rhs")
      val parPop = par_pop_fifo(f(fifo), lanes.size)(e._mT,e.__pos)
      dimsOf(parPop) = List(lanes.size.as[Index])
      lenOf(parPop) = lanes.size

      setProps(parPop, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      cloneFuncs.foreach{func => func(parPop) }
      lanes.split(lhs, parPop)(e._mT)

    case EatReflect(e: Cam_load[_,_]) =>
      if (lanes.size > 1) throw ParallelizedCAMOpException(lhs)(mpos(lhs.pos))
      lanes.duplicate(lhs, rhs)
    case EatReflect(e: Cam_store[_,_]) =>
      if (lanes.size > 1) throw ParallelizedCAMOpException(lhs)(mpos(lhs.pos))
      lanes.duplicate(lhs, rhs)

    case EatReflect(e@Sram_store(sram@EatAlias(mem),addr,value)) if lanes.isCommon(sram) =>
      debugs(s"Unrolling $lhs = $rhs")
      val values = lanes.vectorize{p => f(value)}
      val addrs  = lanes.vectorize{p => f(addr)}
      val parStore = par_sram_store(f(sram), addrs, values)(e._mT, e.__pos)

      setProps(parStore, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      parIndicesOf(parStore) = lanes.map{i => accessIndicesOf(lhs).map(f(_)) }
      cloneFuncs.foreach{func => func(parStore) }
      lanes.unify(lhs, parStore)

    case EatReflect(e@Sram_load(sram@EatAlias(mem),addr)) if lanes.isCommon(sram) =>
      debugs(s"Unrolling $lhs = $rhs")
      val addrs = lanes.vectorize{p => f(addr)}
      val parLoad = par_sram_load(f(sram), addrs)(e._mT, e.__pos)
      dimsOf(parLoad) = List(lanes.size.as[Index])
      lenOf(parLoad) = lanes.size

      setProps(parLoad, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      parIndicesOf(parLoad) = lanes.map{i => accessIndicesOf(lhs).map(f(_)) }
      cloneFuncs.foreach{func => func(parLoad) }
      lanes.split(lhs, parLoad)(e._mT)

    case EatReflect(e@Sram_store(sram,addr,value)) =>
      debugs(s"Duplicating $lhs = $rhs")

      val stores = lanes.duplicate(lhs, rhs)
      lanes.foreach{i => parIndicesOf(f(lhs)) = List(accessIndicesOf(lhs).map(f(_))) }
      stores

    case EatReflect(e@Sram_load(sram,addr)) =>
      debugs(s"Duplicating $lhs = $rhs")
      val loads = lanes.duplicate(lhs, rhs)
      loads.foreach{i => parIndicesOf(f(lhs)) = List(accessIndicesOf(lhs).map(f(_))) }
      loads

    case EatReflect(e:OpForeach)  => unrollControllers(lhs,rhs,lanes){ unrollForeachNode(lhs, e) }
    case EatReflect(e:OpReduce[_,_])  => unrollControllers(lhs,rhs,lanes){ unrollReduceNode(lhs, e) }
    case EatReflect(e:OpMemReduce[_,_]) => unrollControllers(lhs,rhs,lanes){ unrollMemReduceNode(lhs, e) }
    case EatReflect(e:Scatter[_])      => unrollControllers(lhs,rhs,lanes){ unrollScatterNode(lhs, e) }
    case EatReflect(e:Gather[_])       => unrollControllers(lhs,rhs,lanes){ unrollGatherNode(lhs, e) }
    case d if isControlNode(lhs)       => unrollControllers(lhs,rhs,lanes){ self_clone(lhs, rhs) }

    case EatReflect(Reg_new(_)) =>
      debugs(s"Duplicating $lhs = $rhs")
      val dups = lanes.duplicate(lhs, rhs)
      debugs(s"  Created registers: ")
      lanes.foreach{p => debug(s"  $p: $lhs -> ${f(lhs)}") }
      dups

    case _ =>
      debugs(s"Duplicating $lhs = $rhs")
      val dups = lanes.duplicate(lhs, rhs)
      dups
  }

  def unrollControllers[T](lhs: Sym[T], rhs: Def[T], lanes: Unroller)(unroll: => Exp[Any]) = {
    debugs(s"Unrolling controller:")
    debugs(s"$lhs = $rhs")
    if (lanes.size > 1) {
      val blk = reifyBlock {
        /*debugs(s"$lhs duplicate 1/${lanes.size}")
        val first = lanes.inLane(0){ unroll }
        (1 until lanes.size).foreach{p => lanes.inLane(p){
          debugs(s"$lhs duplicate ${p+1}/${lanes.size}")
          first match {case Def(rhs2) => self_clone(first.asInstanceOf[Sym[Any]],rhs2); case _ => first }
        }}*/
        // Can't quite use the above method -- e.g. if accumulators are duplicated A -> A', A''
        // There's no rule that says A' -> A''
        // So for now just unrolling the same loop in multiple contexts
        // Is unrolling multiple times more expensive? Doesn't seem like it should be...
        lanes.foreach{p =>
          debugs(s"$lhs duplicate ${p+1}/${lanes.size}")
          unroll
        }
      }
      val parStage = reflectEffect(ParallelPipe(blk), summarizeEffects(blk) andAlso Simple())
      styleOf(parStage) = ForkJoin
      lanes.unify(lhs, parStage)
    }
    else {
      debugs(s"$lhs duplicate 1/1")
      val first = lanes.inLane(0){ unroll }
      lanes.unify(lhs, first)
    }
  }


  /**
   * Create index bound checks
   * NOTE: Only can be used within reify scope
   **/
  def boundChecks2D(cchain: Exp[CounterChain], inds: List[List[Exp[Index]]]): List[List[Exp[Bit]]] = {
    val ccMax = ccMaxes(cchain)
    inds.zip(ccMax).map{case (ind,max) => ind.map{i => i < max}}
  }
  def boundChecks(cchain: Exp[CounterChain], inds: List[List[Exp[Index]]]): List[Exp[Bit]] = boundChecks2D(cchain,inds).flatten

  /*
    Unrolls purely independent loop iterations
    NOTE: The func block should already have been mirrored to update dependencies prior to unrolling
  */
  def unrollMap[A:Manifest](func: Block[A], lanes: Unroller): List[Exp[A]] = {
    val origResult = getBlockResult(func)

    tab += 1
    focusBlock(func){
      focusExactScope(func){ stms =>
        stms.foreach { case TP(s,d) =>
          unroll(s, d, lanes)(mpos(s.pos))
        }
      }
    }
    tab -= 1
    // Get the list of duplicates for the original result of this block
    lanes.map{p => f(origResult).asInstanceOf[Exp[A]] }
  }


  def unrollForeach (
    lhs:    Exp[Any],
    cchain: Exp[CounterChain],
    func:   Block[Unit],
    inds:   List[Sym[Index]]
  )(implicit ctx: SourceContext) = {
    debugs(s"Unrolling foreach $lhs")

    val lanes = Unroller(cchain, inds, isInnerControl(lhs))
    val blk = reifyBlock { unrollMap(func, lanes); () }
    val inds2 = lanes.indices

    val effects = summarizeEffects(blk).star andAlso Simple()
    val lhs2 = reflectEffect(UnrolledForeach(cchain, blk, inds2)(ctx), effects)
    setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))

    val Def(rhs2) = lhs2
    debugs(s"Created foreach $lhs2 = $rhs2")
    lhs2
  }
  def unrollForeachNode(lhs: Sym[Any], rhs: OpForeach) = {
    val OpForeach(cc, func, inds) = rhs
    unrollForeach(lhs, f(cc), func, inds)(rhs.ctx)
  }


  def unrollReduceTree[A:Manifest:Num](
    inputs: List[Exp[A]],     // Symbols to be reduced
    valids: List[Exp[Bit]],   // Data valid bits corresponding to inputs
    zero:   Option[Exp[A]],   // Optional zero value
    rFunc:  Block[A],         // Reduction function
    ld:     Block[A],         // Load function from accumulator
    st:     Block[Unit],      // Store function to accumulator
    rV:     (Sym[A], Sym[A]), // Bound symbols used to reify rFunc
    res:    Sym[A]            // Bound symbol used to reify st
  )(implicit ctx: SourceContext) = {

    def reduce(x: Exp[A], y: Exp[A]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc) }

    val validInputs = zero match {
      case Some(z) => inputs.zip(valids).map{case (in,v) => mux(v, in, z) }
      case None => ReductionWithoutZeroException()(ctx)
    }
    val treeResult = reduceTree(inputs){(x,y) => reduce(x,y) }
    val accValue = inReduction{ inlineBlock(ld) }
    val res2 = reduce(treeResult, accValue)
    isReduceResult(res2) = true
    isReduceStarter(accValue) = true

    inReduction{ withSubstScope(res -> res2){ inlineBlock(st) }}
  }

  def unrollReduce[T,C[T]](
    lhs:    Exp[Any],           // Original pipe symbol
    cchain: Exp[CounterChain],  // Counterchain
    accum:  Exp[C[T]],          // Accumulator (external)
    zero:   Option[Exp[T]],     // Optional identity value for reduction
    fold:   Boolean,            // [Unused]
    iFunc:  Block[Index],       // Address function for accumulator
    ld:     Block[T],           // Load function for accumulator
    st:     Block[Unit],        // Store function for accumulator
    func:   Block[T],           // Map function
    rFunc:  Block[T],           // Reduce function
    inds:   List[Sym[Index]],   // Bound iterators for map loop
    idx:    Sym[Index],         // Bound symbol corresponding to result of iFunc
    acc:    Sym[C[T]],          // Bound symbol corresponding to accum
    res:    Sym[T],             // Bound symbol corresponding to result of rFunc
    rV:     (Sym[T],Sym[T])     // Bound symbols used to reify rFunc
  )(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debugs(s"Unrolling pipe-fold $lhs")
    val lanes = Unroller(cchain, inds, isInnerControl(lhs))
    val inds2 = lanes.indices

    val blk = reifyBlock {
      debugs("Unrolling map")
      val values = unrollMap(func, lanes)(mT)
      val valids = boundChecks(cchain, inds2)

      if (isOuterLoop(lhs)) {
        debugs("Unrolling unit pipe reduce")
        val rblk = reifyBlock {
          val idx2 = inlineBlock(iFunc)
          withSubstScope(idx -> idx2){ unrollReduceTree[T](values, valids, zero, rFunc, ld, st, rV, res) }
        }
        val effects = summarizeEffects(rblk) andAlso Simple()
        val rpipe = reflectEffect(UnitPipe(rblk)(ctx), effects)
        styleOf(rpipe) = InnerPipe
      }
      else {
        debugs("Unrolling inner reduce")
        val idx2 = inlineBlock(iFunc)
        withSubstScope(idx -> idx2) { unrollReduceTree[T](values, valids, zero, rFunc, ld, st, rV, res) }
      }
    }
    val effects = summarizeEffects(blk).star andAlso Simple() andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))
    val lhs2 = reflectEffect(UnrolledReduce(cchain, accum, blk, rFunc, inds2, acc, rV)(ctx,mT,mC), effects)
    setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))

    val Def(rhs2) = lhs2
    debugs(s"Created reduce $lhs2 = $rhs2")
    lhs2
  }
  def unrollReduceNode[T,C[T]](lhs: Sym[Any], rhs: OpReduce[T,C]) = {
    val OpReduce(cc,accum,zero,fold,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs

    // Create a new copy of all the bound symbols that carry over to the unrolled node
    val acc2 = reflectMutableSym(fresh(List(rhs.ctx))(rhs.mC))
    val rV2 = (fresh(List(rhs.ctx))(rhs.mT), fresh(List(rhs.ctx))(rhs.mT))

    // Mirror the corresponding blocks for the new bound symbols
    val iFunc2 = f(iFunc)
    val ld2 = withSubstScope(acc -> acc2){ f(ld)(rhs.mT) }
    val st2 = withSubstScope(acc -> acc2){ f(st) }
    val rFunc2 = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(rFunc)(rhs.mT) }

    unrollReduce[T,C](lhs, f(cc), f(accum), zero.map(f(_)), fold, iFunc2, ld2, st2, func, rFunc2, inds, idx, acc2, res, rV2)(rhs.ctx,rhs.numT,rhs.mT,rhs.mC)
  }



  def unrollMemReduce[T,C[T]](
    lhs:   Exp[Any],          // Original pipe symbol
    ccMap: Exp[CounterChain], // Map counterchain
    ccRed: Exp[CounterChain], // Reduction counterchain
    accum: Exp[C[T]],         // Accumulator (external)
    zero:  Option[Exp[T]],    // Optional identity value for reduction
    fold:  Boolean,           // [Unused]
    iFunc: Block[Index],      // Address function for intermediate values and accumulator
    func:  Block[C[T]],       // Map function
    ldMap: Block[T],          // Load function for intermediate values
    ldAcc: Block[T],          // Load function for accumulator
    rFunc: Block[T],          // Reduction function
    st:    Block[Unit],       // Store function for accumulator
    isMap: List[Sym[Index]],  // Bound iterators for map loop
    isRed: List[Sym[Index]],  // Bound iterators for reduce loop
    idx:   Sym[Index],        // Bound symbol corresponding to result of iFunc
    part:  Sym[C[T]],         // Bound symbol corresponding to result of func
    acc:   Sym[C[T]],         // Bound symbol corresponding to accum
    res:   Sym[T],            // Bound symbol corresponding to result of rFunc
    rV:    (Sym[T],Sym[T])    // Bound symbol used to reify rFunc
  )(implicit ctx: SourceContext, numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    debugs(s"Unrolling accum-fold $lhs")

    def reduce(x: Exp[T], y: Exp[T]) = withSubstScope(rV._1 -> x, rV._2 -> y){ inlineBlock(rFunc)(mT) }

    val mapLanes = Unroller(ccMap, isMap, false)
    val isMap2 = mapLanes.indices
    val partial = getBlockResult(func)

    val blk = reifyBlock {
      debugs(s"[Accum-fold $lhs] Unrolling map")
      val mems = unrollMap(func, mapLanes)
      val mvalids = boundChecks(ccMap, isMap2)

      if (isUnitCounterChain(ccRed)) withSubstScope(acc -> accum) {
        debugs(s"[Accum-fold $lhs] Unrolling unit pipe reduction")
        val rblk = reifyBlock {
          val idx2 = inlineBlock(iFunc)
          withSubstScope(idx -> idx2){
            val values = mems.map{mem => withSubstScope(partial -> mem, part -> mem){
              inReduction{ inlineBlock(ldMap)(mT) }
            }}
            inReduction{ unrollReduceTree[T](values, mvalids, zero, rFunc, ldAcc, st, rV, res) }
          }
        }
        val effects = summarizeEffects(rblk) andAlso Simple()
        val rpipe = reflectEffect(UnitPipe(rblk)(ctx), effects)
        styleOf(rpipe) = InnerPipe
      }
      else {
        debugs(s"[Accum-fold $lhs] Unrolling pipe-reduce reduction")
        tab += 1
        val reduceLanes = Unroller(ccRed, isRed, true)
        val isRed2 = reduceLanes.indices

        val rblk = reifyBlock {
          val rvalids = boundChecks2D(ccRed, isRed2)

          debugs(s"[Accum-fold $lhs] Creating reduction indices")
          val idx2 = reduceLanes.map{mem =>
            val idx2 = inlineBlock(iFunc)
            register(idx -> idx2)
            idx2
          }

          debugs(s"[Accum-fold $lhs] Unrolling map loads")
          val values = mems.map{mem => withSubstScope(partial -> mem, part -> mem){
            val loadLanes = Unroller(ccRed, isRed, true, Some(isRed2))
            loadLanes.foreach{p => register(idx -> idx2(p) ) }
            inReduction{ unrollMap(ldMap, loadLanes)(mT) }
          }}

          debugs(s"[Accum-fold $lhs] Unrolling accum loads")
          val accValues = inReduction{ unrollMap(ldAcc, reduceLanes)(mT) }

          debugs(s"[Accum-fold $lhs] Unrolling reduction trees and cycles")
          reduceLanes.foreach{p =>
            // Valid bit for each index
            val indexValids = rvalids.zip(reduceLanes.parAddr(p)).map{case (vec,j) => vec(j) }
            // Valid bit for this lane (all indices are valid)
            val laneValid = reduceTree(indexValids){(x,y) => x && y}

            debugs(s"Lane #$p:")
            tab += 1
            val inputs = values.map(_.apply(p)) // The pth value of each vector load
            val valids = mvalids.map{mvalid => mvalid && laneValid }

            debug("Valids:")
            valids.foreach{case s@Def(d) => debug(s"  $s = $d")}

            debugs("Inputs:")
            inputs.foreach{case s@Def(d) => debugs(s"  $s = $d") }

            val validInputs = zero match {
              case Some(z) => inputs.zip(valids).map{case (in, v) => mux(v, in, z) }
              case None => throw ReductionWithoutZeroException()(ctx)
            }
            debugs("Valid inputs: ")
            validInputs.foreach{case s@Def(d) => debugs(s"  $s = $d") }

            val accValue = accValues(p)
            val res2 = inReduction {
              val treeResult = reduceTree(validInputs){(x,y) => reduce(x,y) }
              reduce(treeResult, accValue)
            }
            isReduceResult(res2) = true
            isReduceStarter(accValue) = true
            register(res -> res2)

            tab -= 1
          }

          debugs(s"[Accum-fold $lhs] Unrolling accumulator store")
          inReduction{ unrollMap(st, reduceLanes) }
          ()
        }
        val effects = summarizeEffects(rblk).star andAlso Simple()
        val rpipe = reflectEffect(UnrolledForeach(ccRed, rblk, isRed2)(ctx), effects)
        styleOf(rpipe) = InnerPipe
        tab -= 1
      }
    }
    val effects = summarizeEffects(blk) andAlso Simple() andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))
    val lhs2 = reflectEffect(UnrolledReduce(ccMap, accum, blk, rFunc, isMap2, acc, rV)(ctx,mT,mC), effects)
    setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))

    val Def(rhs2) = lhs2
    debugs(s"Created foreach $lhs2 = $rhs2")
    lhs2
  }
  def unrollMemReduceNode[T,C[T]](lhs: Sym[Any], rhs: OpMemReduce[T,C]) = {
    val OpMemReduce(ccMap,ccRed,accum,zero,fold,iFunc,func,ldMap,ldAcc,rFunc,st,isMap,isRed,idx,part,acc,res,rV) = rhs

    // Create a new copy of all the bound symbols that (could) carry over to the unrolled nodes
    val acc2 = reflectMutableSym(fresh(List(rhs.ctx))(rhs.mC))
    val rV2 = (fresh(List(rhs.ctx))(rhs.mT), fresh(List(rhs.ctx))(rhs.mT))

    // Mirror the corresponding blocks for the new bound symbols
    val iFunc2 = f(iFunc)
    val ldAcc2 = withSubstScope(acc -> acc2){ f(ldAcc)(rhs.mT) }
    val st2 = withSubstScope(acc -> acc2){ f(st) }
    val rFunc2 = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(rFunc)(rhs.mT) }

    unrollMemReduce(lhs,f(ccMap),f(ccRed),f(accum),zero.map(f(_)),fold,iFunc2,func,ldMap,ldAcc2,rFunc2,st2,isMap,isRed,idx,part,acc2,res,rV2)(rhs.ctx,rhs.numT,rhs.mT,rhs.mC)
  }

  // TODO: can probably unify this for scatter and gather
  def unrollScatter[T](
    lhs:   Exp[Any],
    mem:   Exp[DRAM[T]],
    local: Exp[SRAM[T]],
    addrs: Exp[SRAM[Index]],
    len:   Exp[Index],
    par:   Int
  )(implicit ctx: SourceContext, mT: Manifest[T]) = {
    val blk = reifyBlock {
      (0 until par).foreach{i =>
        val scatter = reflectWrite(mem)(Scatter(mem,local,addrs,len,Const(1),fresh[Index])(ctx, mT))
        setProps(scatter, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      }
    }
    val parallel = reflectEffect(ParallelPipe(blk), summarizeEffects(blk) andAlso Simple())
    styleOf(parallel) = ForkJoin
  }
  def unrollScatterNode[T](lhs: Sym[Any], rhs: Scatter[T]) = {
    val Scatter(mem, local, addrs, len, Fixed(p), i) = rhs
    unrollScatter(lhs, f(mem), f(local), f(addrs), f(len), p.toInt)(rhs.ctx, rhs.mT)
  }

  def unrollGather[T](
    lhs:   Exp[Any],
    mem:   Exp[DRAM[T]],
    local: Exp[SRAM[T]],
    addrs: Exp[SRAM[Index]],
    len:   Exp[Index],
    par:   Int
  )(implicit ctx: SourceContext, mT: Manifest[T]) = {
    val blk = reifyBlock {
      (0 until par).foreach{i =>
        val scatter = reflectWrite(local)(Gather(mem,local,addrs,len,Const(1),fresh[Index])(ctx, mT))
        setProps(scatter, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
      }
    }
    val parallel = reflectEffect(ParallelPipe(blk), summarizeEffects(blk) andAlso Simple())
    styleOf(parallel) = ForkJoin
  }
  def unrollGatherNode[T](lhs: Sym[Any], rhs: Gather[T]) = {
    val Gather(mem, local, addrs, len, Fixed(p), i) = rhs
    unrollGather(lhs, f(mem), f(local), f(addrs), f(len), p.toInt)(rhs.ctx, rhs.mT)
  }


  override def self_mirror[A](lhs: Sym[A], rhs: Def[A]): Exp[A] = self_clone(lhs, rhs)

  def self_clone[A](lhs: Sym[A], rhs: Def[A]): Exp[A] = {
    debugs(s"Cloning $lhs = $rhs")
    getProps(lhs).foreach{props => props.data.foreach{(k,m) => debugs(" -" + readable(k) + makeString(m)) }}

    // HACK!!! Check whether the result of clone is actually a new symbol. Don't set props if not
    // Assumption: If the symbol we get back from cloning/mirroring had already been created by this
    // point, the mirrored symbol underwent a rewrite rule or CSE. The correct thing to do here is
    // to keep the previously created symbol's metadata, not the mirrored version of lhs's.
    // TBD: Should this be added to the general self_mirror function too?
    val prevVars = IR.nVars

    val lhs2 = clone(lhs, rhs)(mtype(lhs.tp), mpos(lhs.pos))
    val rhs2 = lhs2 match {case Def(d) => d; case _ => null }

    lhs2 match {
      case s: Sym[_] if s.id >= prevVars =>
        setProps(lhs2, mirror(getProps(lhs), f.asInstanceOf[Transformer]))
        cloneFuncs.foreach{func => func(lhs2) }
        debugs(s"Created $lhs2 = $rhs2")
      case s: Sym[_] =>
        debugs(s"Changed to $lhs2 = $rhs2")
      case _ =>
        debugs(s"Changed to $lhs2")
    }
    getProps(lhs2).foreach{props => props.data.foreach{(k,m) => " -" + debugs(readable(k) + makeString(m)) }}

    lhs2
  }

  def cloneInds[I:Manifest](inds: List[List[Sym[I]]]) = inds.map{is => is.map{i => fresh[I] }}

  def clone[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Exp[A] = (rhs match {
    case Reflect(e@UnrolledForeach(cc,b,i), u, es) =>
      val i2 = cloneInds(i)
      val b2 = withSubstScope(i.flatten.zip(i2.flatten):_*){ f(b) }
      reflectMirrored(Reflect(UnrolledForeach(f(cc), b2, i2)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@UnrolledReduce(cc,a,b,rF,i,acc,rV), u, es) =>
      val i2 = cloneInds(i)
      val acc2 = reflectMutableSym(fresh(List(e.ctx))(e.mC))
      val rV2 = (fresh(List(e.ctx))(e.mT), fresh(List(e.ctx))(e.mT))
      val b2 = withSubstScope( (i.flatten.zip(i2.flatten) ++ List(acc -> acc2)):_*) { f(b) }
      val rF2 = withSubstScope(rV._1 -> rV2._1, rV._2 -> rV2._2){ f(rF) }
      reflectMirrored(Reflect(UnrolledReduce(f(cc),f(a),b2,rF2,i2,acc2,rV2)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case EatReflect(e@Sram_store(sram,addr,value)) =>
      val store = mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), ctx)
      parIndicesOf(store) = List(accessIndicesOf(lhs).map(f(_)))
      store

    case EatReflect(e@Sram_load(sram,addr)) =>
      val load = mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), ctx)
      parIndicesOf(load) = List(accessIndicesOf(lhs).map(f(_)))
      load

    case _ => mirror(rhs, f.asInstanceOf[Transformer])(mtype(manifest[A]), ctx)

  }).asInstanceOf[Exp[A]]


  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = lhs match {
    case Deff(e:OpForeach)        => Some(unrollForeachNode(lhs, e))
    case Deff(e:OpReduce[_,_])    => Some(unrollReduceNode(lhs, e))
    case Deff(e:OpMemReduce[_,_]) => Some(unrollMemReduceNode(lhs, e))
    case Deff(e:Scatter[_])       => Some(unrollScatterNode(lhs, e))
    case Deff(e:Gather[_])        => Some(unrollGatherNode(lhs, e))
    case _ => None
  }

}

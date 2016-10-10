package spatial.compiler.ops

import scala.virtualization.lms.common.{ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}
import java.io.{File, PrintWriter}
import scala.collection.mutable.HashMap

import spatial.compiler._
import spatial.compiler.ops._

trait LoweredPipeOpsExp extends ExternPrimitiveTypesExp with MemoryTemplateOpsExp {
  this: SpatialExp =>

  val controller_tree = new PrintWriter(new File("controller_tree.html" ))
  val table_init = """<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""

  def print_stage_prefix(title: String, ctr: String, node: String, hasThingsInside: Boolean = true) {
    controller_tree.write(s"""<TD><font size = "6">$title<br><b>$node</b></font><br><font size = "1">$ctr</font> """)
    if (hasThingsInside) {
      controller_tree.write(s"""<div data-role="collapsible">
      <h4> </h4>${table_init}""")
    }
  }
  def print_stage_suffix(name: String, hadThingsInside: Boolean = true) {
    if (hadThingsInside) {
      controller_tree.write("""</TABLE></div>""")
    }
    controller_tree.write(s"</TD><!-- Close $name -->")
  }

  // --- Nodes
  case class ParPipeForeach(
    cc:   Exp[CounterChain],
    func: Block[Unit],
    inds: List[List[Sym[FixPt[Signed,B32,B0]]]]
  )(implicit val ctx: SourceContext) extends Def[Pipeline]

  case class ParPipeReduce[T,C[T]](
    cc:    Exp[CounterChain],
    accum: Exp[C[T]],
    func:  Block[Unit],
    rFunc: Block[T],
    inds:  List[List[Sym[FixPt[Signed,B32,B0]]]],
    acc:   Sym[C[T]],
    rV:    (Sym[T], Sym[T])
  )(implicit val ctx: SourceContext, val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]

  // --- Internal API

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@ParPipeForeach(cc,func,i) => reflectPure(ParPipeForeach(f(cc),f(func),i)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeForeach(cc,func,i), u, es) => reflectMirrored(Reflect(ParPipeForeach(f(cc),f(func),i)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@ParPipeReduce(cc,a,b,r,i,acc,rV) => reflectPure(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC))(mtype(manifest[A]),pos)
    case Reflect(e@ParPipeReduce(cc,a,b,r,i,acc,rV), u, es) => reflectMirrored(Reflect(ParPipeReduce(f(cc),f(a),f(b),f(r),i,acc,rV)(e.ctx,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => syms(cc) ::: syms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => syms(cc) ::: syms(accum) ::: syms(func) ::: syms(rFunc)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => readSyms(cc) ::: readSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => readSyms(cc) ::: readSyms(accum) ::: readSyms(func) ::: readSyms(rFunc)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ParPipeForeach(cc,func,inds) => freqNormal(cc) ::: freqCold(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => freqNormal(cc) ::: freqNormal(accum) ::: freqCold(func) ::: freqNormal(rFunc)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ParPipeForeach(cc,func,inds) => inds.flatten ::: effectSyms(func)
    case ParPipeReduce(cc,accum,func,rFunc,inds,acc,rV) => inds.flatten ::: effectSyms(func) ::: effectSyms(rFunc) ::: List(acc, rV._1, rV._2)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenLoweredPipeOps extends ScalaGenEffect {
  val IR: LoweredPipeOpsExp with SpatialCodegenOps
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain])(emitBlk: => Unit) = {
    iters.zipWithIndex.foreach{ case (is, i) =>
      stream.println("for( " + quote(cchain) + "_vec" + i + " <- " + quote(cchain) + ".apply(" + i + ".toInt)) {")
      is.zipWithIndex.foreach{ case (iter, j) =>
        stream.println("  val "+quote(iter)+" = " + quote(cchain) + "_vec" + i + ".apply(" + j + ".toInt)")
      }
    }
    emitBlk
    stream.println("}" * iters.length)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      emitValDef(acc, quote(accum))
      emitParallelizedLoop(inds, cchain){ emitBlock(func) }
      emitValDef(sym, "()")

    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenLoweredPipeOps extends MaxJGenControllerTemplateOps {
  val IR: LoweredPipeOpsExp with ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with SpatialCodegenOps with NosynthOpsExp with MemoryAnalysisExp
          with DeliteTransform with VectorOpsExp with SpatialExp with UnrollingTransformExp
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain]) = {
    val Def(EatReflect(Counterchain_new(counters))) = cchain

    iters.zipWithIndex.foreach{ case (is, i) =>
      if (is.size == 1) { // This level is not parallelized, so assign the iter as-is
          emit("DFEVar " + quote(is(0)) + " = " + quote(counters(i)) + ";");
      } else { // This level IS parallelized, index into the counters correctly
        is.zipWithIndex.foreach{ case (iter, j) =>
          emit("DFEVar " + quote(iter) + " = " + quote(counters(i)) + "[" + j + "];")
        }
      }
    }
  }

  override def isConstOrArgOrBnd(x: Exp[Any]) = x match {
    case s@Sym(n) => {
      s match {
        case Deff(ConstFixPt(_,_,_,_)) => true
        case Deff(ConstFltPt(_,_,_)) => true
        case Deff(Reg_read(xx)) => // Only if rhs of exp is argin
          xx match {
            case Deff(Argin_new(_)) => true
            case _ =>  
              if (isReduceStarter(s)) {false} else {true}
          }
        case Deff(_) => false // None
        case _ => true // Is bound
      }
    }
  }

  def addConstOrArgOrBnd(x: Exp[Any], set: Set[Exp[Any]]) = {
    var ret = Set[Exp[Any]]()
    val Deff(dd) = x
    dd match {
      case FltPt_Add(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Add(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FltPt_Mul(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Mul(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Lt(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Leq(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Neq(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Eql(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_And(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Or(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Lsh(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FixPt_Rsh(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FltPt_Lt(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FltPt_Leq(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FltPt_Neq(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case FltPt_Eql(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case Bit_And(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case Bit_Or(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case Bit_Xor(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case Bit_Xnor(a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case Bit_Not(a) => if (isConstOrArgOrBnd(a)) {ret += a}
      case Mux2(sel,a,b) => {if (isConstOrArgOrBnd(a)) {ret += a}; if (isConstOrArgOrBnd(b)) {ret += b}}
      case _ =>
    }
    set ++ ret
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@ParPipeForeach(cchain, func, inds) =>
      controlNodeStack.push(sym)
      emitComment(s"""ParPipeForeach ${quote(sym)} = ParPipeForeach(${quote(cchain)}) {""")
      emit("""{""")

      // Ctr analysis for controller_tree diagram
      val Def(EatReflect(Counterchain_new(diagram_counters))) = cchain
      var ctr_str = diagram_counters.map { ctr =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        s"${quote(start)} until ${quote(end)} by ${quote(step)} par ${quote(par)}"
      }

      var hadThingsInside = true
      styleOf(sym) match {
        case StreamPipe =>
          emitComment(s"""StrmPipe to be emitted""")
          print_stage_prefix(s"Foreach Streampipe",s"${ctr_str}",s"${quote(sym)}")
        case CoarsePipe =>
          emitComment(s"""MPSM to be emitted""")
          print_stage_prefix(s"Foreach Metapipe",s"${ctr_str}",s"${quote(sym)}")
        case InnerPipe =>
          emitComment(s"""PipeSM to be emitted""")
          print_stage_prefix(s"Foreach Innerpipe",s"${ctr_str}",s"${quote(sym)}", false)
          hadThingsInside = false
        case SequentialPipe =>
          emitComment(s"""SeqSM to be emitted""")
          print_stage_prefix(s"Foreach Seqpipe",s"${ctr_str}",s"${quote(sym)}")
        case _ =>
          emitComment(s"""ParPipeForeach style: ${styleOf(sym)}""")
          print_stage_prefix(s"Foreach ${styleOf(sym)}",s"${ctr_str}",s"${quote(sym)}")
      }
      emitController(sym, Some(cchain))
      emit(s"DFEVar ${quote(sym)}_redLoop_done = constant.var(true); // Hack for new fold unrolling...")
      emitParallelizedLoop(inds, cchain)
      emitRegChains(sym, inds.flatten)

      parentOf(sym).get match {
        case e@Deff(ParPipeReduce(_,accum,_,_,_,_,_)) => // If part of reduce, emit custom red kernel
          if (childrenOf(parentOf(sym).get).indexOf(sym) == childrenOf(parentOf(sym).get).length-1) {
            val isKerneledRed = reduceType(accum) match {
              case Some(fps: ReduceFunction) => fps match {
                case FixPtSum => true
                case FltPtSum => true
                case _ => false
              }
            }

            styleOf(sym) match {
              case InnerPipe =>
                if (isKerneledRed) {
                  // Putting reduction tree in its own kernel
                  var inputVecs = Set[Sym[Any]]()
                  var consts_args_bnds_list = Set[Exp[Any]]()
                  var treeResultSyms = Set[Sym[Any]]()
                  focusBlock(func){ // Send reduce tree to separate file
                    focusExactScope(func){ stms =>
                      stms.foreach { case TP(s,d) =>
                        val Deff(dd) = s
                        dd match {
                          case tag @ (Vec_apply(_,_) | FixPt_Mul(_,_) | FixPt_Add(_,_) | FltPt_Mul(_,_) | FltPt_Add(_,_)) =>
                            if (isReduceResult(s)) {
                              val ts = tpstr(1)(s.tp, implicitly[SourceContext])
                              emit(s"DFEVar ${quote(s)} = ${ts}.newInstance(this);")
                              treeResultSyms += s
                            }
                            consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                          case input @ ( Par_bram_load(_,_) | Par_pop_fifo(_,_) | Pop_fifo(_) ) =>
                            inputVecs += s
                          case _ =>
                            consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                        }
                      }
                    }
                  }

                  emitBlock(func)
                  val treeResult = treeResultSyms.map{a=>quote(a)}.toList.sortWith(_<_).mkString(",")
                  val inputVecsStr = inputVecs.map {a => quote(a)}.mkString(",")
                  val trailingArgsStr = consts_args_bnds_list.toList.map {a => quote(a)}.sortWith(_ < _).mkString(",")
                  val should_comma1 = if (inputVecs.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
                  val should_comma2 = if (treeResult != "") {","} else {""} // TODO: Such an ugly way to do this
                  val should_comma3 = if (consts_args_bnds_list.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
                  emit(s"new ${quote(sym)}_reduce_kernel(owner $should_comma1 $inputVecsStr $should_comma2 $treeResult $should_comma3 $trailingArgsStr); // Reduce kernel")
                } else {
                  emitBlock(func)
                }
              case _ =>
                emitBlock(func)
              }
            } else {
              emitBlock(func)
            }
        case _ =>
          emitBlock(func)
      }

      emit("""}""")
      emitComment(s"""} ParPipeForeach ${quote(sym)}""")
      print_stage_suffix(quote(sym), hadThingsInside)
      controlNodeStack.pop

    case e@ParPipeReduce(cchain, accum, func, rFunc, inds, acc, rV) =>
      controlNodeStack.push(sym)
      val Def(EatReflect(Counterchain_new(diagram_counters))) = cchain
      var ctr_str = diagram_counters.map { ctr =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        s"${quote(start)} until ${quote(end)} by ${quote(step)} par ${quote(par)}"
      }

      emitComment(s"""ParPipeReduce ${quote(sym)} = ParPipeReduce(${quote(cchain)}, ${quote(accum)}) {""")
      emit("""{""")
      var hadThingsInside = true
      styleOf(sym) match {
        case CoarsePipe =>
          emitComment(s"""MPSM to be emitted""")
          print_stage_prefix(s"Reduce Metapipe",s"${ctr_str}",s"${quote(sym)}")
        case InnerPipe =>
          emitComment(s"""PipeSM to be emitted""")
          print_stage_prefix(s"Reduce Innerpipe",s"${ctr_str}",s"${quote(sym)}", false)
          hadThingsInside = false
        case SequentialPipe =>
          emitComment(s"""SeqSM to be emitted""")
          print_stage_prefix(s"Reduce Seqpipe",s"${ctr_str}",s"${quote(sym)}")
        case _ =>
          emitComment(s"""ParPipeReduce style: ${styleOf(sym)}""")
          print_stage_prefix(s"Reduce ${styleOf(sym)}",s"${ctr_str}",s"${quote(sym)}")
      }

      // The body of ParPipeReduce uses 'acc' to refer to the accumulator
      // The rest of the world uses 'accum'. Make sure their metadata matches up here
      // FIXME: This should be unnecessary in codegen
      val Def(d) = accum  // CHEATING!
      duplicatesOf(acc) = duplicatesOf(accum)
      readersOf(acc) = readersOf(accum)

      emitComment(s"""ParPipeReduce ${quote(sym)} controller {""")
      emitController(sym, Some(cchain))
      emitComment(s"""} ${quote(sym)} controller""")

      emitComment(s"""ParPipeReduce ${quote(sym)} par loop {""")
      emitParallelizedLoop(inds, cchain)
      emitComment(s"""} ${quote(sym)} par loop""")

      val isKerneledRed = reduceType(accum) match {
        case Some(fps: ReduceFunction) => fps match {
          case FixPtSum => true
          case FltPtSum => true
          case _ => false
        }
      }

      styleOf(sym) match {
        case InnerPipe =>
          if (isKerneledRed) {
            // Putting reduction tree in its own kernel
            var inputVecs = Set[Sym[Any]]()
            var consts_args_bnds_list = Set[Exp[Any]]()
            var treeResult = ""
            focusBlock(func){ // Send reduce tree to separate file
              focusExactScope(func){ stms =>
                stms.foreach { case TP(s,d) =>
                  val Deff(dd) = s
                  dd match {
                    case tag @ (Vec_apply(_,_) | FixPt_Mul(_,_) | FixPt_Add(_,_) | FltPt_Mul(_,_) | FltPt_Add(_,_)) =>
                      if (isReduceResult(s)) {
                        val ts = tpstr(1)(s.tp, implicitly[SourceContext])
                        emit(s"DFEVar ${quote(s)} = ${ts}.newInstance(this);")
                        treeResult = quote(s)
                      }
                      consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                    case input @ ( Par_bram_load(_,_) | Par_pop_fifo(_,_) | Pop_fifo(_) ) =>
                      inputVecs += s
                    case _ =>
                      consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                  }
                }
              }
            }

            emitRegChains(sym, inds.flatten)
            emitComment(s"""ParPipeReduce ${quote(sym)} func block {""")
            emitBlock(func)
            emitComment(s"""} ${quote(sym)} func block""")

            val inputVecsStr = inputVecs.map {a => quote(a)}.mkString(",")
            val trailingArgsStr = consts_args_bnds_list.toList.map {a => quote(a)}.sortWith(_ < _).mkString(",")
            val should_comma1 = if (inputVecs.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
            val should_comma2 = if (treeResult != "") {","} else {""} // TODO: Such an ugly way to do this
            val should_comma3 = if (consts_args_bnds_list.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
            emit(s"new ${quote(sym)}_reduce_kernel(owner $should_comma1 $inputVecsStr $should_comma2 $treeResult $should_comma3 $trailingArgsStr); // Reduce kernel")
          } else {
            emitRegChains(sym, inds.flatten)
            emitComment(s"""ParPipeReduce ${quote(sym)} func block {""")
            emitBlock(func)
            emitComment(s"""} ${quote(sym)} func block""")
          }
        case _ =>
          emitRegChains(sym, inds.flatten)
          emitComment(s"""ParPipeReduce ${quote(sym)} func block {""")
          emitBlock(func)
          emitComment(s"""} ${quote(sym)} func block""")
        }

      val Def(EatReflect(dp)) = accum
      dp match {
        case a@Bram_new(_,_) =>
          // emitNode(accum.asInstanceOf[Sym[Any]], d)
        /* NOT SURE WHAT THIS SECTION DOES! -Matt*/
        case Reg_new(init) =>
        //   (0 until duplicatesOf(accum).size) foreach { i =>
        //     emit(s"""${quote(accum)}_${i}_lib.write(${quote(acc)}_0, constant.var(true), constant.var(false));""")
        //   }
        case _ =>
          throw new Exception(s"""Unknown accum in ParPipeReduce on ${dp}!""")
      }

      emit("""}""")
      emitComment(s"""} ParPipeReduce ${quote(sym)}""")
      print_stage_suffix(quote(sym), hadThingsInside)
      controlNodeStack.pop

    case _ => super.emitNode(sym, rhs)
  }
}

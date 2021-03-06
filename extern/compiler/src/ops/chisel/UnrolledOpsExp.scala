package spatial.compiler.ops

import scala.virtualization.lms.common.{ScalaGenEffect, DotGenEffect, MaxJGenEffect, ChiselGenEffect}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}
import java.io.{File, PrintWriter}
import scala.collection.mutable.HashMap
import ppl.delite.framework.{Config}

import spatial.compiler._
import spatial.compiler.ops._


trait ChiselGenUnrolledOps extends ChiselGenControllerOps {
  val IR: UnrolledOpsExp with ControllerOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with DRAMOpsExp with RegOpsExp with ExternCounterOpsExp
          with SpatialCodegenOps with NosynthOpsExp with MemoryAnalysisExp
          with DeliteTransform with VectorOpsExp with SpatialExp with UnrollingTransformExp
  import IR._

  def emitParallelizedLoop(iters: List[List[Sym[FixPt[Signed,B32,B0]]]], cchain: Exp[CounterChain]) = {
    val Def(EatReflect(Counterchain_new(counters))) = cchain

    iters.zipWithIndex.foreach{ case (is, i) =>
      if (is.size == 1) { // This level is not parallelized, so assign the iter as-is
          emit(quote(is(0)) + " := " + quote(counters(i)) + "(0)");
          withStream(baseStream) {
            emit(s"val " + quote(is(0)) + " = Wire(UInt(32.W))")
          }
      } else { // This level IS parallelized, index into the counters correctly
        is.zipWithIndex.foreach{ case (iter, j) =>
          emit(quote(iter) + " := " + quote(counters(i)) + s"(" + j + ")")
          withStream(baseStream) {
            emit(s"val " + quote(iter) + " = Wire(UInt(32.W))")
          }
        }
      }
    }
  }



  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@UnrolledForeach(cchain, func, inds, vs) =>
      controlNodeStack.push(sym)

      // Ctr analysis for controller_tree diagram
      val Def(EatReflect(Counterchain_new(diagram_counters))) = cchain
      var ctr_str = diagram_counters.map { ctr =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        s"${quote(ctr)}: ${quote(start)} until ${quote(end)} by ${quote(step)} par ${quote(par)}"
      }

      var inner = false
      val style = styleOf(sym) match {
        case StreamPipe => "Stream"
        case CoarsePipe => "Meta"
        case InnerPipe => 
          inner = true
          "Pipe"
        case SequentialPipe => "Seq"
        case _ => s"${styleOf(sym)}"
      }
      print_stage_prefix(s"Foreach $style",s"${ctr_str}",s"${quote(sym)}", inner)
      // emit(s"""//  ---- style ${quote(sym)} (UnrolledForeach(${quote(cchain)})) ----""")

      emitController(sym, Some(cchain))
      // emit(s"DFEVar ${quote(sym)}_redLoop_done = constant.var(true); // Hack for new fold unrolling...")
      emitParallelizedLoop(inds, cchain)
      emitRegChains(sym, inds.flatten)

      parentOf(sym).get match {
        case e@Deff(_:UnrolledReduce[_,_]) => // If part of reduce, emit custom red kernel
          if (childrenOf(parentOf(sym).get).indexOf(sym) == childrenOf(parentOf(sym).get).length-1) {
            styleOf(sym) match {
              case InnerPipe =>
                // Putting reduction tree in its own kernel
                // var inputVecs = Set[Sym[Any]]()
                // var consts_args_bnds_list = Set[Exp[Any]]()
                // var treeResultSyms = Set[Sym[Any]]()
                // focusBlock(func){ // Send reduce tree to separate file
                //   focusExactScope(func){ stms =>
                //     stms.foreach { case TP(s,d) =>
                //       val Deff(dd) = s
                //       dd match {
                //         case tag @ (Vec_apply(_,_) | FixPt_Mul(_,_) | FixPt_Add(_,_) | FltPt_Mul(_,_) | FltPt_Add(_,_)) =>
                //           if (isReduceResult(s)) {
                //             val ts = tpstr(1)(s.tp, implicitly[SourceContext])
                //             emit(s"//val ${quote(s)} = Wire(${ts})")
                //             treeResultSyms += s
                //           }
                //           consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                //         case input @ ( _:Par_sram_load[_] | _:Par_pop_fifo[_] | _:Pop_fifo[_] ) =>
                //           inputVecs += s
                //         case _ =>
                //           consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
                //       }
                //     }
                //   }
                // }

                emit(s"""// ---- UnrolledForeach ${quote(sym)} func block ---- """)
                emitBlock(func)
                // val treeResult = treeResultSyms.map{a=>quote(a)}.toList.sortWith(_ < _).mkString(",")
                // val inputVecsStr = inputVecs.map {a => quote(a)}.mkString(",")
                // val trailingArgsStr = consts_args_bnds_list.toList.map {a => quote(a)}.sortWith(_ < _).mkString(",")
                // val should_comma1 = if (inputVecs.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
                // val should_comma2 = if (treeResult != "") {","} else {""} // TODO: Such an ugly way to do this
                // val should_comma3 = if (consts_args_bnds_list.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
                // emit(s"new ${quote(sym)}_reduce_kernel(owner $should_comma1 $inputVecsStr $should_comma2 $treeResult $should_comma3 $trailingArgsStr); // Reduce kernel")
              case _ =>
                emitBlock(func)
              }
            } else {
              emitBlock(func)
            }
        case _ =>
          emitBlock(func)
      }

      // emit("""}""")
      // emitComment(s"""} UnrolledForeach ${quote(sym)}""")
      print_stage_suffix(quote(sym), inner)
      controlNodeStack.pop

    case e@UnrolledReduce(cchain, accum, func, rFunc, inds, vs, acc, rV) =>
      controlNodeStack.push(sym)
      val Def(EatReflect(Counterchain_new(diagram_counters))) = cchain
      var ctr_str = diagram_counters.map { ctr =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        s"${quote(ctr)}: ${quote(start)} until ${quote(end)} by ${quote(step)} par ${quote(par)}"
      }

      emit(s"""// ---- UnrolledReduce ${quote(sym)} = UnrolledReduce(${quote(cchain)}, ${quote(accum)}) ----""")
      var inner = false
      styleOf(sym) match {
        case CoarsePipe =>
          emitComment(s"""MPSM to be emitted""")
          print_stage_prefix(s"Reduce Metapipe",s"${ctr_str}",s"${quote(sym)}")
        case InnerPipe =>
          emitComment(s"""PipeSM to be emitted""")
          print_stage_prefix(s"Reduce Innerpipe",s"${ctr_str}",s"${quote(sym)}", false)
          inner = true
        case SequentialPipe =>
          emitComment(s"""SeqSM to be emitted""")
          print_stage_prefix(s"Reduce Seqpipe",s"${ctr_str}",s"${quote(sym)}")
        case _ =>
          emitComment(s"""UnrolledReduce style: ${styleOf(sym)}""")
          print_stage_prefix(s"Reduce ${styleOf(sym)}",s"${ctr_str}",s"${quote(sym)}")
      }

      // The body of UnrolledReduce uses 'acc' to refer to the accumulator
      // The rest of the world uses 'accum'. Make sure their metadata matches up here
      // FIXME: This should be unnecessary in codegen
      val Def(d) = accum  // CHEATING!
      duplicatesOf(acc) = duplicatesOf(accum)
      readersOf(acc) = readersOf(accum)

      emit(s"""// ---- UnrolledReduce ${quote(sym)} controller ----""")
      emitController(sym, Some(cchain))

      emit(s"""// ---- UnrolledReduce ${quote(sym)} par loop ----""")
      emitParallelizedLoop(inds, cchain)

      styleOf(sym) match {
        case InnerPipe =>
          // Putting reduction tree in its own kernel
          // var inputVecs = Set[Sym[Any]]()
          // var consts_args_bnds_list = Set[Exp[Any]]()
          // var treeResult = ""
          // focusBlock(func){ // Send reduce tree to separate file
          //   focusExactScope(func){ stms =>
          //     stms.foreach { case TP(s,d) =>
          //       val Deff(dd) = s
          //       dd match {
          //         case tag @ (Vec_apply(_,_) | FixPt_Mul(_,_) | FixPt_Add(_,_) | FltPt_Mul(_,_) | FltPt_Add(_,_)) =>
          //           if (isReduceResult(s)) {
          //             val ts = tpstr(1)(s.tp, implicitly[SourceContext])
          //             emit(s"// val ${quote(s)} = Wire(${ts})")
          //             treeResult = quote(s)
          //           }
          //           consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
          //         case input @ ( _:Par_sram_load[_] | _:Par_pop_fifo[_] | _:Pop_fifo[_] ) =>
          //           inputVecs += s
          //         case _ =>
          //           consts_args_bnds_list = addConstOrArgOrBnd(s, consts_args_bnds_list)
          //       }
          //     }
          //   }
          // }

          emitRegChains(sym, inds.flatten)
          emit(s"""// ---- UnrolledReduce ${quote(sym)} func block ---- """)
          emitBlock(func)

          // val inputVecsStr = inputVecs.map {a => quote(a)}.mkString(",")
          // val trailingArgsStr = consts_args_bnds_list.toList.map {a => quote(a)}.sortWith(_ < _).mkString(",")
          // val should_comma1 = if (inputVecs.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
          // val should_comma2 = if (treeResult != "") {","} else {""} // TODO: Such an ugly way to do this
          // val should_comma3 = if (consts_args_bnds_list.toList.length > 0) {","} else {""} // TODO: Such an ugly way to do this
          // emit(s"new ${quote(sym)}_reduce_kernel(owner $should_comma1 $inputVecsStr $should_comma2 $treeResult $should_comma3 $trailingArgsStr); // Reduce kernel")
        case _ =>
          emitRegChains(sym, inds.flatten)
          emit(s"""// ---- UnrolledReduce ${quote(sym)} func block ----""")
          emitBlock(func)
        }

      val Def(EatReflect(dp)) = accum
      dp match {
        case a@Sram_new(_,_) =>
          // emitNode(accum.asInstanceOf[Sym[Any]], d)
        /* NOT SURE WHAT THIS SECTION DOES! -Matt*/
        case Reg_new(init) =>
        //   (0 until duplicatesOf(accum).size) foreach { i =>
        //     emit(s"""${quote(accum)}_${i}_lib.write(${quote(acc)}_0, constant.var(true), constant.var(false));""")
        //   }
        case _ =>
          throw new Exception(s"""Unknown accum in UnrolledReduce on ${dp}!""")
      }

      print_stage_suffix(quote(sym), inner)
      controlNodeStack.pop

    case _ => super.emitNode(sym, rhs)
  }
}

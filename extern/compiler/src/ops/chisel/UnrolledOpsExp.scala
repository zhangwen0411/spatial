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
      emit(s"""// ---- UnrolledForeach ${quote(sym)} func block ---- """)
      if (inner) {
        withStream(newStream(s"${quote(sym)}UnrolledForeach")) {
          emitBlock(func)
          emit("}}")
        }
      } else {
        emitBlock(func)
      }
      print_stage_suffix(quote(sym), inner)
      controlNodeStack.pop

    case e@UnrolledReduce(cchain, accum, func, rFunc, inds, vs, acc, rV) =>
      controlNodeStack.push(sym)
      val Def(EatReflect(Counterchain_new(diagram_counters))) = cchain
      var ctr_str = diagram_counters.map { ctr =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        s"${quote(ctr)}: ${quote(start)} until ${quote(end)} by ${quote(step)} par ${quote(par)}"
      }

      var inner = false
      styleOf(sym) match {
        case CoarsePipe =>
          print_stage_prefix(s"Reduce Metapipe",s"${ctr_str}",s"${quote(sym)}")
        case InnerPipe =>
          print_stage_prefix(s"Reduce Innerpipe",s"${ctr_str}",s"${quote(sym)}", false)
          inner = true
        case SequentialPipe =>
          print_stage_prefix(s"Reduce Seqpipe",s"${ctr_str}",s"${quote(sym)}")
        case _ =>
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

      emitRegChains(sym, inds.flatten)
      emit(s"""// ---- UnrolledReduce ${quote(sym)} func block ---- """)
      if (inner) {
        withStream(newStream(s"${quote(sym)}UnrolledReduce")) {
          emitBlock(func)
          emit("}}")
        }
      } else {
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

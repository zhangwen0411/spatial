package spatial.compiler.ops

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.virtualization.lms.internal.{Traversal, QuotingExp}

import scala.reflect.{Manifest,SourceContext}
import java.io.{File, PrintWriter}
import sys.process._
import scala.language.postfixOps

import scala.collection.mutable.Set

import ppl.delite.framework.Config

trait DotIRPrinter extends Traversal with QuotingExp {
	val IR: SpatialExp with MemoryAnalysisExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

  override val name = "DotIRPrinter"
  override val eatReflect = true
  debugMode = false

  var inHwScope = false
  var fileNum = 0
  val emittedCtrChain = Set.empty[Exp[Any]]
  val emittedSize = Set.empty[Exp[Any]]

  def alwaysGen(x: => Any) {
    val oldScope = inHwScope
    inHwScope = true
    x
    inHwScope = oldScope
  }

  /* Special case to handle nodes producing HW inputs outside of hardware scope */
  def hackGen(x: Exp[Any]): Unit = x match {
    case Def(EatReflect(_:Reg_new[_])) => // Nothing
    case ConstFix(_) => // Nothing
    case ConstFlt(_) => // Nothing
    case Def(d) if !emittedSize.contains(x) =>
      alwaysGen{ traverse(x.asInstanceOf[Sym[Any]], d) }
      emittedSize += x
      syms(d).foreach{s => hackGen(s) }
    case _ => // Nothing
  }


	var stream:PrintWriter = _
	def newStream(fileName:String):PrintWriter = {
		val path = Config.buildDir + java.io.File.separator + fileName + ".dot"
		val pw = new PrintWriter(path)
		pw
	}

  override def quote(x: Exp[Any]):String = x match {
    case s@Sym(id) => s match {
			case Def(ConstFix(c)) => c.toString
			case Def(ConstFlt(c)) => c.toString
			case _ =>
				var tstr = s.tp.erasure.getSimpleName()
				tstr = tstr.replace("Spatial","")
			  if (isReg(s.tp)) {
					tstr = tstr.replace("Reg", regType(s) match {
						case Regular => "Reg"
						case ArgumentIn => "ArgIn"
						case ArgumentOut => "ArgOut"
					})
			  }
        else if (isPipeline(s.tp)) {
          tstr = tstr.replace("Pipeline", styleOf(s) match {
						case InnerPipe => "Pipe"
            case StreamPipe => "StreamPipe"
						case CoarsePipe => "MetaPipe"
						case SequentialPipe => "Sequential"
					})
				}
        else if (isSRAM(s.tp)) {
          tstr = tstr.replace("SpatialSRAM", "SRAM")
        }

        tstr + nameOf(s).map{n => "_"+n}.getOrElse("") + "_x" + id
		}
    case _ => super.quote(x)
  }

	def emit(str: String):Unit = {
		stream.println(str)
	}
	def emitEdge(x:Exp[Any], y:Exp[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Exp[Any], y:Exp[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ headlabel="${label}" ]""")
	}
	def emitComment(str: String):Unit = {
		stream.println(s"""/* $str */ """)
	}

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    val filename = Config.degFilename.replace(".deg", "")
    stream = newStream(filename + fileNum)
		emittedCtrChain.clear
		emittedSize.clear
    emit("digraph{")
    emit(s"compound=true")
    emit(s"""graph [splines="ortho" clusterrank="local" rankdir = "LR"]""")
    emit(s"edge [arrowsize=$arrowSize penwidth=$edgeThickness]")
    emit(s"""node [fontsize=$fontsize shape=$defaultShape style="filled" fillcolor=$bgcolor ]""")
    emit(s"fontsize=$fontsize")
		b
	}

  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
    emit("}")
    stream.flush()
    stream.close()
    fileNum += 1
		b
	}

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitBlock(y: Block[Any], name:String, label:String, color:String): Unit = {
    emit(s"""subgraph cluster_${name} {""")
    emit(s"""label="${name}" """)
    emit(s"""style="filled" """)
		emit(s"""fillcolor=$color""")
		emit(s"""color=none""")
		emitBlock(y)
		emit(s"""}""")
	}

	def emitValDef(lhs: Exp[Any], rhs: Exp[Any]):Unit = {
		emitValDef(lhs.asInstanceOf[Sym[Any]], quote(rhs))
	}
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
		stream.println(s"""define(`${quote(sym)}', `${rhs}')""")
  }

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    val Deff(Counterchain_new(counters)) = cchain
	  inds.zip(counters).foreach{case (iter, ctr) => emitValDef(iter, ctr) }
  }

  def emitParallelNestedIdx(cchain: Exp[CounterChain], inds: List[List[Sym[FixPt[Signed,B32,B0]]]]) = {
    val Deff(Counterchain_new(counters)) = cchain
    inds.zip(counters).foreach{case (iters, ctr) => iters.foreach{iter => emitValDef(iter, ctr) }}
  }

	def emitCtrChain(cchain: Exp[CounterChain]):Unit = {
		val Def(EatReflect(d)) = cchain
		emitCtrChain(cchain.asInstanceOf[Sym[CounterChain]],
									 d.asInstanceOf[Def[Any]])
	}
	def emitCtrChain(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
	  case e@Counterchain_new(counters) =>
			if (!emittedCtrChain.contains(sym)) {
				emittedCtrChain += sym
    		emit(s"""subgraph cluster_${quote(sym)} {""")
    		emit(s""" label=${quote(sym)} """)
    		emit(s""" style="rounded, filled" """)
    		emit(s""" fillcolor=$counterColor""")
    		counters.foreach{ ctr =>
    		  emit(s"""   ${quote(ctr)}""")
    		}
    		emit("}")
			}
		case _ =>
	}

  private def isDblBuf(sym: Exp[Any]) = duplicatesOf(sym).exists{dup => dup.depth > 1}

  def emitVector(sym: Sym[Any]) = {
    if (isDblBuf(sym)) {
      emit(s"""${quote(sym)} [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="${quote(sym)}"""")
      emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
      emit(s"""fillcolor=$vectorFillColor ]""")
    }
    else {
      emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$vectorFillColor]""")
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = {
    debug(s"[$inHwScope] $lhs = $rhs")
    if (inHwScope) emitHWNode(lhs, rhs)
    else emitOtherNode(lhs, rhs)
  }

  def emitOtherNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(func) =>
      alwaysGen { emitBlock(func) }

    case _:Reg_new[_] if isDblBuf(sym) =>
      emit(s"""${quote(sym)} [margin=0, rankdir="LR", label="{<st> | <ld>}" xlabel="${quote(sym)}" """)
      emit(s"""      shape="record" color=$dblbufBorderColor style="filled" """)
      emit(s"""      fillcolor=$regFillColor ]""")

    case _:Reg_new[_] =>
      emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$regFillColor ]""")

    case _:Argin_new[_] =>
      emit(s"""${quote(sym)} [label="${quote(sym)}" shape="Msquare" style="filled" fillcolor=$regFillColor ]""")

    case _:Argout_new[_] =>
      emit(s"""${quote(sym)} [label="${quote(sym)}" shape="Msquare" style="filled" fillcolor=$regFillColor ]""")

    case Dram_new(size) =>
      if (!emittedSize.contains(size)) hackGen(size)
      var label = s""" "${quote(sym)} """
      if (quote(size).forall(_.isDigit)) {
        label += ", size = " + quote(size)
      }
      else emitEdge(size, sym, "size")
      label += "\""
      emit(s"""${quote(sym)} [label=$label shape="square" fontcolor="white" color="white" style="filled" """)
      emit(s"""               fillcolor=$dramFillColor color=black]""")

    case ConstBit(v) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")
    case ConstFixPt(v,_,_,_) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")
    case ConstFltPt(v,_,_) =>
      emit(s"""${quote(sym)} [label=${quote(v)} style="filled" fillcolor="lightgray" color="none"]""")

    case Tpes_Fix_to_int(v) => emitValDef(sym, quote(v))
    case Tpes_Int_to_fix(v) => emitValDef(sym, quote(v))

    case Reflect(d, u, es) => traverse(sym, d)
    case _ =>
      debug(s"...ignored")
      blocks(rhs).foreach{blk => traverseBlock(blk)}
  }

  def emitHWNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,_) =>
			var l = s""""${quote(sym)}"""
			if (quote(start).forall(_.isDigit)) {
				l += "|start=" + quote(start)
			} else {
				emitEdge(start, sym, "start")
			}
			if (quote(end).forall(_.isDigit)) {
				l += "|end=" + quote(end)
			} else {
				emitEdge(end, sym, "end")
			}
			if (quote(step).forall(_.isDigit)) {
				l += "|step=" + quote(step)
			} else {
				emitEdge(step, sym, "step")
			}
			l += "\""
      emit(s"""${quote(sym)} [ label=$l shape="record" style="filled,rounded"
						color=$counterInnerColor ]""")

	  case e@Counterchain_new(counters) =>
			//TODO: check whether parent of cchain is empty, if is emit ctrchain
			if (parentOf(sym).isEmpty) {
				emitCtrChain(sym, rhs)
			}

		case e@ParallelPipe(func) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""	label = "parallel ${quote(sym)}"""")
      emit(s"""	style = "filled, bold"""")
      emit(s"""	fillcolor = $parallelFillColor""")
      emit(s"""	color = $parallelBorderColor""")
      emitBlock(func)
			emit(s"""}""")

    case e@UnitPipe(func) =>
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s""" label = "pipe ${quote(sym)}"""")
      emit(s""" style = "filled, bold"""")
      emit(s""" fillcolor = $pipeFillColor""")
      emit(s""" color = $pipeBorderColor""")
      emitBlock(func)
      emit(s"""}""")

    case e@OpForeach(cchain, func, inds) =>
			emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
			emitCtrChain(cchain)
      emitBlock(func, quote(sym) + "_foreachFunc", "foreachFunc", foreachFillColor)             // Map function
      emit("}")

    case e@OpReduce(cchain, accum, zero, fA, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
			emitValDef(acc, accum)
      emitNestedIdx(cchain, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
			emit(s"""fillcolor=$pipeFillColor""")
      emitValDef(acc, accum)
			emitCtrChain(cchain)
      emitBlock(func, quote(sym) + "_mapFunc", "mapFunc", mapFillColor)
      emitBlock(ldFunc, quote(sym) + "_ldFunc", "ldFunc", ldFillColor)
      emitValDef(rV._1, getBlockResult(ldFunc))
      emitValDef(rV._2, getBlockResult(func))
      emitBlock(rFunc, quote(sym) + "_reduceFunc", "reduceFunc", reduceFillColor)
      emitValDef(res, getBlockResult(rFunc))
      emitBlock(stFunc, quote(sym) + "_stFunc", "stFunc" , stFillColor)
      emit("}")

    case e@OpMemReduce(ccOuter, ccInner, accum, zero, fA, func, ldPart, ldFunc, rFunc, stFunc, indsOuter, indsInner, part, acc, res, rV) =>
      emitValDef(acc, accum)
      emitNestedIdx(ccOuter, indsOuter)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""  label="${quote(sym)}"""")
      emit(s"""  style="bold, filled" """)
      emit(s"""  fillcolor=$mpFillColor""")
      emit(s"""  color=$mpBorderColor""")
      emitCtrChain(ccOuter)
      if (!isUnitCounterChain(ccInner)) {
        emitCtrChain(ccInner)
        emitNestedIdx(ccInner, indsInner)
      }
      emitBlock(func, quote(sym) + "_mapFunc", "mapFunc", mapFillColor)
      emitValDef(part, getBlockResult(func))
      emitBlock(ldPart, quote(sym) + "_ldPart", "ldPart", ldFillColor)
      emitBlock(ldFunc, quote(sym) + "_ldFunc", "ldFunc", ldFillColor)
      emitValDef(rV._1, getBlockResult(ldPart))
      emitValDef(rV._2, getBlockResult(ldFunc))
      emitBlock(rFunc, quote(sym) + "_reduceFunc", "reduceFunc", reduceFillColor)
      emitValDef(res, getBlockResult(rFunc))
      emitBlock(stFunc, quote(sym) + "_stFund", "stFunc", stFillColor)
      emit("}")

    case Cache_new(offchip) =>
      if (isDblBuf(sym)) {
        emit(s"""${quote(sym)} [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="${quote(sym)}"""")
        emit(s"""               shape="record" color=$dblbufBorderColor  style="filled" """)
        emit(s"""               fillcolor=$cacheFillColor ]""")
      }
      else {
        emit(s"""${quote(sym)} [label="${quote(sym)}" shape="square" style="filled" fillcolor=$cacheFillColor ]""")
      }
      emitEdge(offchip, sym)

    case Cache_load(cache, addr) =>
      emitEdge(addr, cache, "addr")
      emitValDef(sym, cache)

    case Cache_store(cache, addr, value) =>
      emitEdge(addr, cache, "addr")
      emitEdge(value, cache, "data")

    case Fifo_new(size, zero) =>
      val qsym = quote(sym)
      if (isDblBuf(sym)) {
        emit(s"""$qsym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$qsym """")
        emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
        emit(s"""fillcolor=$sramFillColor ]""")
      }
      else {
        emit(s"""$qsym [label="$qsym " shape="square" style="filled" fillcolor=$sramFillColor ]""")
      }
    case Push_fifo(fifo,value,en) =>
      emitEdge(value,fifo, "data")
      emitEdge(en,fifo,"en")

    case Pop_fifo(fifo,en) =>
      emitValDef(sym, fifo)

    case Count_fifo(fifo) =>
      emitValDef(sym, fifo)

		case Sram_new(size, zero) =>
      val qsym = quote(sym)
      if (isDblBuf(sym)) {
      	emit(s"""$qsym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$qsym """")
        emit(s"""shape="record" color=$dblbufBorderColor  style="filled"""")
        emit(s"""fillcolor=$sramFillColor ]""")
      }
      else {
        emit(s"""$qsym [label="$qsym " shape="square" style="filled" fillcolor=$sramFillColor ]""")
      }

    case Sram_load(sram,addr) =>
      emitEdge(addr, sram, "addr")
			emitValDef(sym, sram)

    case Sram_store(sram,addr,value,ens) =>
      emitEdge(addr, sram, "addr")
      emitEdge(value, sram, "data")

    case e@BurstStore(mem,stream,ofs,len,p) =>
      emitEdge(stream, mem, "data")
      emitEdge(ofs, mem, "addr")

    case e@BurstLoad(mem,stream,ofs,len,p) =>
      emitEdge(ofs, mem, "addr")
      emitEdge(mem, stream, "data")

    case Reg_read(reg) =>
      emitValDef(sym, reg)

    case Reg_write(reg, value, en) =>
      emitEdge(value, reg)

    case Fixpt_to_fltpt(x) =>
      emit(s"""${quote(sym)} [ label="fix2flt" ]""")
      emitEdge(x, sym)
    case Fltpt_to_fixpt(x) =>
      emit(s"""${quote(sym)} [ label="flt2fix" ]""")
      emitEdge(x, sym)
    case Convert_fixpt(x) =>
      emit(s"""${quote(sym)} [ label="fix2fix" ]""")
      emitEdge(x, sym)
    case Convert_fltpt(x) =>
      emit(s"""${quote(sym)} [ label="flt2flt" ]""")
      emitEdge(x, sym)

    case FixPt_Neg(a)   => emit(s"""${quote(sym)} [label="neg" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym)
    case FixPt_Add(a,b) => emit(s"""${quote(sym)} [label="+"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Sub(a,b) => emit(s"""${quote(sym)} [label="-"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Mul(a,b) => emit(s"""${quote(sym)} [label="*"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Div(a,b) => emit(s"""${quote(sym)} [label="/"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Mod(a,b) => emit(s"""${quote(sym)} [label="%"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Lt(a,b)  => emit(s"""${quote(sym)} [label="<"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Leq(a,b) => emit(s"""${quote(sym)} [label="<=" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Neq(a,b) => emit(s"""${quote(sym)} [label="!=" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Eql(a,b) => emit(s"""${quote(sym)} [label="==" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_And(a,b) => emit(s"""${quote(sym)} [label="&"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Or(a,b)  => emit(s"""${quote(sym)} [label="|"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Lsh(a,b) => emit(s"""${quote(sym)} [label="<<" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FixPt_Rsh(a,b) => emit(s"""${quote(sym)} [label=">>" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)

    case FltPt_Neg(a)   => emit(s"""${quote(sym)} [label="neg" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym)
    case FltPt_Add(a,b) => emit(s"""${quote(sym)} [label="+"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Sub(a,b) => emit(s"""${quote(sym)} [label="-"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Mul(a,b) => emit(s"""${quote(sym)} [label="*"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Div(a,b) => emit(s"""${quote(sym)} [label="/"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Lt(a,b)  => emit(s"""${quote(sym)} [label="<"  shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Leq(a,b) => emit(s"""${quote(sym)} [label="<=" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Neq(a,b) => emit(s"""${quote(sym)} [label="!=" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)
    case FltPt_Eql(a,b) => emit(s"""${quote(sym)} [label="==" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a, sym); emitEdge(b, sym)

    case Bit_Not(a)    => emit(s"""${quote(sym)} [label="~" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)
    case Bit_And(a,b)  => emit(s"""${quote(sym)} [label="&&" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym); emitEdge(b,sym)
    case Bit_Or(a,b)   => emit(s"""${quote(sym)} [label="||" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym); emitEdge(b,sym)
    case Bit_Xor(a,b)  => emit(s"""${quote(sym)} [label="==" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym); emitEdge(b,sym)
    case Bit_Xnor(a,b) => emit(s"""${quote(sym)} [label="!=" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym); emitEdge(b,sym)

    case FixPt_Abs(a)  => emit(s"""${quote(sym)} [label="abs" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)
    case FltPt_Abs(a)  => emit(s"""${quote(sym)} [label="abs" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)
    case FltPt_Log(a)  => emit(s"""${quote(sym)} [label="log" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)
    case FltPt_Exp(a)  => emit(s"""${quote(sym)} [label="exp" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)
    case FltPt_Sqrt(a) => emit(s"""${quote(sym)} [label="sqrt" shape="square" style="filled" fillcolor="white"]"""); emitEdge(a,sym)

    case Mux2(s,a,b) =>
      emit(s"""${quote(sym)} [label="mux", shape="diamond" style="filled" fillcolor="white"]""")
      emitEdge(s, sym, "sel")
      emitEdge(a, sym, "a")
      emitEdge(b, sym, "b")

    case UnrolledForeach(cc,func,inds,vs) =>
      emitParallelNestedIdx(cc, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
      emit(s"""fillcolor=$pipeFillColor""")
      emitCtrChain(cc)
      emitBlock(func, quote(sym) + "_foreach", "foreach", foreachFillColor)             // Map function
      emit("}")

    case UnrolledReduce(cc,accum,func,rFunc,inds,vs,acc,rV) =>
      emitValDef(acc, accum)
      emitParallelNestedIdx(cc, inds)
      emit(s"""subgraph cluster_${quote(sym)} {""")
      emit(s"""label="${quote(sym)}"""")
      emit(s"""color=$pipeBorderColor""")
      emit(s"""style="bold, filled" """)
      emit(s"""fillcolor=$pipeFillColor""")
      emitCtrChain(cc)
      emitBlock(func, quote(sym) + "_mapreduce", "mapreduce", foreachFillColor)             // Map function
      emit("}")

		case _ => emitOtherNode(sym, rhs)
	}

	val arrowSize = 0.6
	val edgeThickness = 0.5
	val ctrlColor = s""""red""""
	val counterColor = s""""#e8e8e8""""
	val counterInnerColor = s""""gray""""
	val fontsize = 10
	val defaultShape = "square"
	val bgcolor = s""""white""""

	// Pipe Colors
	//val pipeFillColor = "#4FA1DB"
	val pipeFillColor = s""""white""""
	val pipeBorderColor = s""""black""""

	// Block Colors
	val foreachFillColor = s""""#F6EC93""""
	val mapFillColor = s""""#56D9D2""""
	val reduceFillColor = s""""#FE7365""""
	val ldFillColor = s""""#7be58f""""
	val stFillColor = s""""#7be58f""""

	// Metapipeline colors
	val mpFillColor = s""""#4FA1DB""""
	val mpBorderColor = s""""#4FA1DB""""
	val mpStageFillColor = s""""#BADDFF""""
	val mpStageBorderColor = s""""none""""

	// Parallel colors
	//val parallelFillColor = "#4FDBC2"
	val parallelFillColor = s""""white""""
	//val parallelBorderColor = s""""#00AB8C""""
	val parallelBorderColor = s""""black""""
	val parallelStageFillColor = s""""#CCFFF6""""
	val parallelStageBorderColor = s""""none""""

	// Tile transfer colors
	val tileTransFillColor = s""""#FFA500""""

	// Memories
  val vectorFillColor = s""""#8bd645""""
  val fifoFillColor = s""""70C6E6""""
	val sramFillColor = s""""#70C6E6""""
	val cacheFillColor = s""""#B3A582""""
	val dramFillColor = s""""#685643""""
	val regFillColor = s""""#8bd645""""
	val dblbufBorderColor = s""""#4fb0b0""""

}

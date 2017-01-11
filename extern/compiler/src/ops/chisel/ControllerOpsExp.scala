package spatial.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect, MaxJGenFat, ChiselGenEffect, ChiselGenFat}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}
import ppl.delite.framework.{Config}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.HashMap



trait ChiselGenControllerOps extends ChiselGenEffect with ChiselGenFat {
  val IR: UnrolledOpsExp with ControllerOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with DRAMOpsExp with RegOpsExp with ExternCounterOpsExp
          with SpatialCodegenOps with NosynthOpsExp with MemoryAnalysisExp
          with DeliteTransform with VectorOpsExp with SpatialExp with UnrollingTransformExp

 import IR._ //{__ifThenElse => _, Nosynth___ifThenElse => _, __whileDo => _,
             // Forloop => _, println => _ , _}

  def consumesMemFifo(node: Exp[Any]) = {
    childrenOf(parentOf(node).get).map{ n => n match {
        case Deff(BurstLoad(mem, fifo, ofs, len, par)) => true
        case _ => false
      }
    }.reduce{_|_}
  }

  def trashCount(i: Int, node: Exp[Any]) = {
    if (consumesMemFifo(node)) {
      96 - i%96 // TODO: Pass info about word size.  Assume 32-bit for now
    } else {
      0
    }
  }

  def isConstOrArgOrBnd(x: Exp[Any]) = x match {
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
  // HACK alert [TODO Raghu] : This code is duplicated in MaxJManagerGen so that argin and argout
  // have a consistent name. Code is duplicated because MaxJManagerGen is currently
  // a standalone thing that does not have a means to share things.
  // The correct fix is to put common things in a trait that is mixed into both
  // code generators
	var quoteSuffix = HashMap[Sym[Any],HashMap[Sym[Any], String]]()
  override def quote(x: Exp[Any]) = x match {
		case ss@Sym(nn) => {
      val s = if (rwPortAlias.contains(ss)) rwPortAlias(ss) else ss
      val Sym(n) = s
      s match {
        case Def(Argin_new(init)) =>
          s"argin_" + s.tp.erasure.getSimpleName() + n
        case Def(ConstFix(value)) =>
          s"const${value}_" + s.tp.erasure.getSimpleName() + n
        case Def(ConstFlt(value)) =>
          val str = s"$value"
          s"const${str.replace('.', 'p').replace('-', 'n')}_" + s.tp.erasure.getSimpleName() + n
        case _ =>
    			val tstr = s.tp.erasure.getSimpleName().replace("Spatial","")
          val customStr = tstr match {
            case "Pipeline" => styleOf(s) match {
              case CoarsePipe => "metapipe"
              case InnerPipe => "pipe"
              case SequentialPipe => "seq"
              case StreamPipe => "strm"
              case ForkJoin => "parallel"
            }
            case "Register" => regType(s) match {
              case Regular => "reg"
              case ArgumentIn => "argin"
              case ArgumentOut => "argout"
            }

            case _ => tstr
          }
          val suffix = if (controlNodeStack.isEmpty) "" else controlNodeStack.map { c =>
            if (quoteSuffix.contains(c)) {
              val suffixMap = quoteSuffix(c)
              if (suffixMap.contains(x.asInstanceOf[Sym[Any]])) {
                suffixMap(x.asInstanceOf[Sym[Any]])
              } else {
                ""
              }
            } else {
              ""
            }
          }.reduce{_+_}
          val rw_suffix = if (rwPortAlias.contains(ss)) "_rwport" else ""
			    customStr + n + suffix + rw_suffix
        }
		  }
    case _ => super.quote(x)
  }

  /* Set of control nodes which already have their enable signal emitted */
  val enDeclaredSet = Set.empty[Exp[Any]]

  /* Set of control nodes which already have their done signal emitted */
  val doneDeclaredSet = Set.empty[Exp[Any]]

  override def initializeGenerator(buildDir:String): Unit = {
		enDeclaredSet.clear
		doneDeclaredSet.clear
		super.initializeGenerator(buildDir)
	}

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    emit(s"""${maxJPre(sym)} ${quote(sym)} = ${quote(rhs)};""")
  }

  def emitValDef(sym: Sym[Any], exp: Exp[Any]): Unit = {
    emitValDef(sym, quote(exp))
  }

  def emitBlock(y: Block[Any], blockName:String, doNotClose:Boolean = false): Unit = {
    // emit("\n//  ---- Emitting COMPUTATION BLOCK ----\n")
    emit(s"//     ---- Block ${blockName} ----")
    //emit("{")
    emitBlock(y)
    //emit(s"""${if (doNotClose) "" else "}"}""")
    // emitComment(s"} Block ${blockName}")
  }

	def emitNestedIdx(cchain:Exp[CounterChain], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    val Deff(Counterchain_new(counters)) = cchain
	  inds.zipWithIndex.foreach {case (iter, idx) => emitValDef(iter, counters(idx)) }
  }

	def emitRegChains(controller: Sym[Any], inds:List[Sym[FixPt[Signed,B32,B0]]]) = {
    styleOf(controller) match {
      case CoarsePipe =>
        val stages = childrenOf(controller)
        inds.foreach { idx =>
          emit(s"""DblBufReg[] ${quote(idx)}_chain = spatialUtils.getRegChain(
              "${quote(controller)}_${quote(idx)}", ${stages.size}, ${quote(idx)},
              new var[]{${stages.map{s => quote(s)+"_done"}.mkString(",")}});""")
        }
      case _ =>
    }
  }

	var expToArg = HashMap[Exp[Any],Exp[Reg[Any]]]()
	var argToExp = HashMap[Exp[Reg[Any]],Exp[Any]]()
  override def preProcess[A:Manifest](body: Block[A]) = {
    val argInPass = new ChiselArgInPass {
      val IR: ChiselGenControllerOps.this.IR.type = ChiselGenControllerOps.this.IR
    }
    argInPass.run(body)
    expToArg = argInPass.expToArg
    argToExp = argInPass.argToExp

    val regChainPass = new RegChainPass {
      val IR: ChiselGenControllerOps.this.IR.type = ChiselGenControllerOps.this.IR
    }
    regChainPass.run(body)
    quoteSuffix = regChainPass.quoteSuffix
    super.preProcess(body)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    
    
    case Hwblock(func) =>
      val appName = Config.degFilename.dropRight(4)
      controlNodeStack.push(sym)
      controller_tree.write("""<!DOCTYPE html>
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.css">
<script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
<script src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"></script>
</head><body>

  <div data-role="main" class="ui-content">
    <h2>Controller Diagram</h2>
<TABLE BORDER="3" CELLPADDING="10" CELLSPACING="10">"""
      )

      print_stage_prefix(s"Hwblock",s"",s"${quote(sym)}")
			inHwScope = true
      emit(s"""// TopModule for application: $appName""")
			emitComment("Emitting Hwblock dependencies {")
      val hwblockDeps = recursiveDeps(rhs)
      expToArg.keys.filterNot { hwblockDeps.contains(_) } foreach { argToExp -= expToArg(_) }

      val emitted = Set[Exp[Any]]()
      def emitIfUndeclared(e: Exp[Any]) = {
        if (!emitted.contains(e)) {
          val ts = tpstr(parOf(e))(e.tp, implicitly[SourceContext])
          emit(s"""var ${quote(e)} = $ts.newInstance(this);""")
          emitted += e
        }
      }

      hwblockDeps.foreach { s =>
        val Def(d) = s
        // emit(s"""// Dep: ${quote(s)} = $d""")

        if (argToExp.contains(s.asInstanceOf[Sym[Reg[Any]]])) {
          val e = argToExp(s.asInstanceOf[Sym[Reg[Any]]])
          emitIfUndeclared(e)
        }

        if (expToArg.contains(s)) {
          emitIfUndeclared(s)
        }

        d match {
           case Reflect(Dram_new(size),_,_) =>  // Avoid emitting Dram_new here as it would've been emitted already
           case Dram_new(size) =>  // Avoid emitting Dram_new here as it would've been emitted already
           case _ => emitNode(s, d)
         }
      }
			// emitComment(" End Hwblock dependencies }")
      // emitComment("\n//Setup Top Level IO")
      // emitComment(s"quoteSuffix = $quoteSuffix")
      emit(s"""val ${quote(sym)}_en = io.top_en;""")
      emitGlobalWire(s"""${quote(sym)}_done""")
      // emit(s"""io.top_done := ${quote(sym)}_done;""")
      // emit(s"""// Hwblock: childrenOf(${quote(sym)}) = ${childrenOf(sym)}""")
      emitController(sym, None)
      emit(s"""io.top_done := ${quote(sym)}_sm.io.output.done""")
      // emitComment("\n--------------- HW BLOCK ----------------\n")      
      emitBlock(func)
			inHwScope = false
      print_stage_suffix(quote(sym))
      controller_tree.write(s"""  </TABLE>
</body>
</html>""")
      controller_tree.close

      controlNodeStack.pop

      

    case e@Counterchain_new(counters) =>

    case e@OpForeach(cchain, func, inds) =>
      controlNodeStack.push(sym)
      print_stage_prefix(s"OpForeach",s"",s"${quote(sym)}")
      emitController(sym, Some(cchain))
      emitNestedIdx(cchain, inds)
      emitRegChains(sym, inds)
      emitBlock(func, s"${quote(sym)} Foreach")             // Map function
      print_stage_suffix(quote(sym))
      controlNodeStack.pop

    case e@OpReduce(cchain, accum, zero, fA, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
      controlNodeStack.push(sym)
      print_stage_prefix(s"OpReduce","","${quote(sym)}")
      emitController(sym, Some(cchain))
      emitNestedIdx(cchain, inds)
      emitRegChains(sym, inds)
      emitBlock(func, s"${quote(sym)} Foreach")
      emitBlock(ldFunc, s"${quote(sym)} Load")
      emitValDef(rV._1, quote(getBlockResult(ldFunc)))
      emitValDef(rV._2, quote(getBlockResult(func)))
      emitBlock(rFunc, s"${quote(sym)} Reduce")
      emitValDef(res, quote(getBlockResult(rFunc)))
      emitBlock(stFunc, s"${quote(sym)} Store")
      print_stage_suffix(quote(sym))
      controlNodeStack.pop

    
		case e@ParallelPipe(func: Block[Unit]) =>
      controlNodeStack.push(sym)
      print_stage_prefix(s"ParallelPipe",s"",s"${quote(sym)}")
      emitController(sym, None)
      emitBlock(func, s"${quote(sym)} Parallel")
      print_stage_suffix(quote(sym))
      controlNodeStack.pop

		case e@UnitPipe(func: Block[Unit]) =>
      var inner = if (isInnerPipe(sym)) {false} else {true}
      controlNodeStack.push(sym)
      val smStr = styleOf(sym) match {
        case CoarsePipe => s"Metapipe"
        case StreamPipe => "Streampipe"
        case InnerPipe => "Innerpipe"
        case SequentialPipe => s"Seqpipe"
        case ForkJoin => s"Parpipe"
      }

      print_stage_prefix(s"Unit $smStr",s"",s"${quote(sym)}", inner)
      // emit(s"""// Unit pipe writtenIn(${quote(sym)}) = ${writtenIn(sym)}""")
      writtenIn(sym) foreach { s =>
        val Def(d) = s
        // emit(s"""//   ${quote(s)} = $d, isAccum(${quote(s)}) = ${isAccum(s)}""")
      }
      val writesToAccumReg = writtenIn(sym).exists {s => s match {
          case Def(EatReflect(Reg_new(_))) => isAccum(s)
          case _ => false
        }
      }
      if (writesToAccumReg) {
        val acc = writtenIn(sym).filter { s => s match {
            case Def(EatReflect(Reg_new(_))) => isAccum(s)
            case _ => false
          }
        }.head
      }
      emitController(sym, None)

      if (smStr == "Innerpipe") {
        emit(s"""// ---- Begin unit counter for ${quote(sym)} ---- """)
        emit(s"""${quote(sym)}_sm.io.input.ctr_done := Utils.delay(${quote(sym)}_sm.io.output.ctr_en, 1)""")
      }

      if (writesToAccumReg) {

        emit(s"""DFEVar ${quote(sym)}_loopLengthVal = ${quote(sym)}_offset.getDFEVar(this, dfeUInt(9));""")
        emit(s"""Count.Params ${quote(sym)}_redLoopParams = control.count.makeParams(9)
                              .withEnable(${quote(sym)}_datapath_en)
                              .withReset(${quote(sym)}_done)
                              .withMax(${quote(sym)}_loopLengthVal)
                              .withWrapMode(WrapMode.STOP_AT_MAX);
    Counter ${quote(sym)}_redLoopCounter = control.count.makeCounter(${quote(sym)}_redLoopParams);
    DFEVar ${quote(sym)}_redLoop_done = ${quote(sym)}_redLoopCounter.getCount() === ${quote(sym)}_loopLengthVal-1;""")
      }

      emitBlock(func, s"${quote(sym)} Unitpipe")
      // parentOf(sym).get match {
      //   case e@Deff(UnrolledReduce(_,accum,_,_,_,_,_,_)) => // If part of reduce, emit custom red kernel
      //     emitBlock(func, s"${quote(sym)} Unitpipe")
      // }

      print_stage_suffix(quote(sym), inner)
      controlNodeStack.pop


    case _ => super.emitNode(sym,rhs)
  }

  def emitController(sym:Sym[Any], cchain:Option[Exp[CounterChain]]) {
    val smStr = styleOf(sym) match {
			case CoarsePipe => s"Metapipe"
      case StreamPipe => "Stream"
      case InnerPipe => "Pipe"
      case SequentialPipe => s"Sequential"
      case ForkJoin => s"Parallel"
    }
    emit(s"""\n//  ---- Begin ${smStr} ${quote(sym)} Controller ----""")

    /* State Machine Instatiation */
    // IO
    val numIter = if (cchain.isDefined) {
      val Def(EatReflect(Counterchain_new(counters))) = cchain.get
      counters.zipWithIndex.map {case (ctr,i) =>
        val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
        emit(s"""val ${quote(sym)}_level${i}_iters = (${quote(end)} - ${quote(start)}) / (${quote(step)} * ${quote(par)}.U) + Mux(((${quote(end)} - ${quote(start)}) % (${quote(step)} * ${quote(par)}.U) === 0.U), 0.U, 1.U)""")
        s"${quote(sym)}_level${i}_iters"
      }
    } else { 
      List("1.U")
    }

    val constrArg = smStr match {
      case "Pipe" => s"${numIter.length} /*probably don't need*/"
      case "Parallel" => ""
      case _ => childrenOf(sym).length
    }

    emit(s"""val ${quote(sym)}_offset = 3 // TODO: Compute real delays""")
    emit(s"""val ${quote(sym)}_sm = Module(new ${smStr}(${constrArg}))""")
    emit(s"""  ${quote(sym)}_sm.io.input.enable := ${quote(sym)}_en;""")
    emit(s"""  ${quote(sym)}_done := ${quote(sym)}_sm.io.output.done""")
    emit(s"""val ${quote(sym)}_rst_en = ${quote(sym)}_sm.io.output.rst_en""")

    styleOf(sym) match {
      case s @ (CoarsePipe | SequentialPipe) =>
        emit(s"""  ${quote(sym)}_sm.io.input.numIter := (${numIter.mkString(" * ")})""")
      case _ =>
    }

    if (styleOf(sym)!=ForkJoin) {
      if (cchain.isDefined) {
        emitCChainCtrl(sym, cchain.get)
      } else {
        emit(s"""val ${quote(sym)}_datapath_en = ${quote(sym)}_en & ~${quote(sym)}_rst_en;""")
        emit(s"""val ${quote(sym)}_ctr_en = ${quote(sym)}_datapath_en;""")
        // emit(s"""val ${quote(sym)}_ctr_en = ${quote(sym)}_sm.io.output.ctr_en // TODO: Why did we originally generate the 2 lines above??""")
      }
    }

    // val childrenSet = Set[String]()
    // val percentDSet = Set[String]()
    /* Control Signals to Children Controllers */
    if (!isInnerPipe(sym)) {
      emit(s"""//      ---- Begin $smStr ${quote(sym)} Children Signals ----""")
		  childrenOf(sym).zipWithIndex.foreach { case (c, idx) =>
		  	emitGlobalWire(s"""${quote(c)}_done""")
		  	emit(s"""    ${quote(sym)}_sm.io.input.stageDone(${idx}) := ${quote(c)}_done;""")
        emit(s"""    val ${quote(c)}_en = ${quote(sym)}_sm.io.output.stageEnable(${quote(idx)})""")
        // childrenSet += (s"${quote(c)}_en, ${quote(c)}_done")
        // percentDSet += (s"${idx}: %d %d")
		  	enDeclaredSet += c
		  	doneDeclaredSet += c
		  }
    }

    // emit(s"""// debug.simPrintf(${quote(sym)}_en, "pipe ${quote(sym)}: ${percentDSet.toList.mkString(",   ")}\\n", ${childrenSet.toList.mkString(",")});""")

  }

  def emitCChainCtrl(sym: Sym[Any], cchain: Exp[CounterChain]) {
		val Deff(Counterchain_new(counters)) = cchain

    emit(s"""//      ---- Begin counter for ${quote(sym)} ----""")
    /* Reset CounterChain */
    //TODO: support reset of counterchain to sequential and metapipe in templete
    val maxes = counters.map {case ctr =>
      val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
      styleOf(sym) match {
        // case InnerPipe =>
        //   emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_$i", ${quote(end)});""")
        //   // emit(s"""${quote(sym)}_sm.connectInput("sm_trashCnt", constant.var(dfeUInt(32), ${trashCount(bound(end).get.toInt, sym)}));""")
        //   emit(s"""DFEVar ${quote(sym)}_trash_en = ${quote(sym)}_sm.getOutput("trashEn");""")
        //   emit(s"""val ${quote(ctr)}_max_$i = ${quote(sym)}_sm.getOutput("ctr_maxOut_$i");""")
        case ForkJoin => 
          throw new Exception("Cannot have counter chain control logic for fork-join (parallel) controller!")
          "error"
        case _ =>
          quote(end)
      }
    }

    // emit(s"""val ${quote(sym)}_maxes = List(${maxes.map(quote).mkString(",")}) // TODO: Is this variable ever used??""")

    styleOf(sym) match {
      case InnerPipe =>
        emitGlobalWire(s"""${quote(cchain)}_done""")
        doneDeclaredSet += cchain
        emit(s"""${quote(sym)}_sm.io.input.ctr_done := ${quote(cchain)}_done;""")
        emit(s"""val ${quote(sym)}_datapath_en = ${quote(sym)}_sm.io.output.ctr_en;""")
      case ForkJoin => throw new Exception("Cannot have counter chain control logic for fork-join (parallel) controller!")
      case _ => emit(s"""val ${quote(sym)}_datapath_en = ${quote(sym)}_en;""")
    }


    /* Emit CounterChain */
    styleOf(sym) match {
      case InnerPipe =>
        val Def(EatReflect(d)) = sym; d match {
          case n:OpForeach =>
            val writesToAccumRam = writtenIn(sym).exists {s => s match {
                case Def(EatReflect(Sram_new(_,_))) => isAccum(sym)
                case _ => false
              }
            }

            if (writesToAccumRam) {
              val ctrEn = s"${quote(sym)}_datapath_en | ${quote(sym)}_rst_en"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym,
                    Some(s"stream.offset(${quote(sym)}_datapath_en & ${quote(cchain)}_chain.getCounterWrap(${quote(counters.head)}), -${quote(sym)}_offset-1)"))
            } else {
              val ctrEn = s"${quote(sym)}_datapath_en"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            }


          case n:UnrolledForeach =>
            val writesToAccumRam = writtenIn(sym).exists {s => s match {
                case Def(EatReflect(Sram_new(_,_))) => isAccum(sym)
                case _ => false
              }
            }
            if (writesToAccumRam) {
              val ctrEn = s"${quote(sym)}_datapath_en | ${quote(sym)}_rst_en"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym,
                    Some(s"stream.offset(${quote(sym)}_datapath_en & ${quote(cchain)}_chain.getCounterWrap(${quote(counters.head)}), -${quote(sym)}_offset-1)"))
            } else {
              val ctrEn = s"${quote(sym)}_datapath_en"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            }

          case n@UnrolledReduce(cchain, accum, func, rFunc, inds, ens, acc, rV) =>
            emit(s"""val ${quote(sym)}_loopLengthVal = 1.U // TODO: fix this;""")
            emit(s"""val ${quote(sym)}_redLoopCtr = Module(new RedxnCtr());""")
            emit(s"""${quote(sym)}_redLoopCtr.io.input.enable := ${quote(sym)}_datapath_en""")
            emit(s"""${quote(sym)}_redLoopCtr.io.input.max := 5.U //TODO: Really calculate this""")
            // // emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopCtr.addCounter(${stream_offset_guess+1}, 1);""")
            // emit(s"""var ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopCtr.addCounter(${quote(sym)}_loopLengthVal, 1);""")
            emit(s"""var ${quote(sym)}_redLoop_done = ${quote(sym)}_redLoopCtr.io.output.done;""")
            val ctrEn = s"${quote(sym)}_datapath_en & ${quote(sym)}_redLoop_done"
            emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
            val rstStr = s"${quote(sym)}_done"
            emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)


          case n:OpReduce[_,_] =>
			      //TODO : what is this? seems like all reduce supported are specialized
            //  def specializeReduce(r: ReduceTree) = {
            //  val lastGraph = r.graph.takeRight(1)(0)
            //  (lastGraph.nodes.size == 1) & (r.accum.input match {
            //    case in:Add => true
            //    case in:MinWithMetadata => true
            //    case in:MaxWithMetadata => true
            //    case in:Max => true
            //    case _ => false
            //  })
			      val specializeReduce = true;
            if (specializeReduce) {
              val ctrEn = s"${quote(sym)}_datapath_en"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            } else {
              emit(s"""var ${quote(sym)}_loopLengthVal = ${quote(sym)}_offset.getDFEVar(this, dfeUInt(9));""")
              emit(s"""CounterChain ${quote(sym)}_redLoopChain =
		        		control.count.makeCounterChain(${quote(sym)}_datapath_en);""")
              // emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${stream_offset_guess+1}, 1);""")
              emit(s"""var ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${quote(sym)}_loopLengthVal, 1);""")
              emit(s"""var ${quote(sym)}_redLoop_done = stream.offset(${quote(sym)}_redLoopChain.getCounterWrap(${quote(sym)}_redLoopCtr), -1);""")
              val ctrEn = s"${quote(sym)}_datapath_en & ${quote(sym)}_redLoop_done"
              emit(s"""var ${quote(sym)}_ctr_en = $ctrEn; // TODO: Not migrated from maxj yet!!!""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            }
        }
      case CoarsePipe =>
        val ctrEn = s"${quote(childrenOf(sym).head)}_done"
        emit(s"""var ${quote(sym)}_ctr_en = $ctrEn;""")
        val rstStr = s"${quote(sym)}_done"
        emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
      case SequentialPipe =>
        val ctrEn = s"${quote(childrenOf(sym).last)}_done"
        emit(s"""var ${quote(sym)}_ctr_en = $ctrEn;""")
        val rstStr = s"${quote(sym)}_done"
		    emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
    }

  }

  def emitCustomCounterChain(cchain: Exp[CounterChain], en: Option[String], rstStr: Option[String], parent: Exp[Any], done: Option[String]=None) = {
    val sym = cchain
    if (!enDeclaredSet.contains(sym)) {
      emit(s"""val ${quote(sym)}_en = ${en.get};""")
      enDeclaredSet += sym
    }



    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
    val Deff(Counterchain_new(counters)) = cchain

    // Connect maxes
    val maxes = counters.zipWithIndex.map { case (ctr, i) =>
      val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
      quote(end)
      // parentOf(cchain.asInstanceOf[Rep[CounterChain]]) match {
      //   case Some(s) =>
      //     s.tp.erasure.getSimpleName match {  // <- There's got to be a better way
      //     case "Pipeline" => s"${quote(ctr)}_max_$i"
      //     case "SpatialPipeline" => s"${quote(ctr)}_max_$i"
      //     case _ => quote(end)
      //   }
      //   case None => quote(end)
      // }
    }

    // val trashStr = if (false/*consumesMemFifo(sym)*/) {
    //   val ctr = counters(0)
    //   val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
    //   val c = trashCount(bound(end).get.toInt, sym)
    //   s" + ${c}"
    // } else {""}
    emit(s"""val ${quote(sym)}_maxes = List(${maxes.map(m=>s"${quote(m)}").mkString(",")});""")

    // Connect strides
    val strides = counters.zipWithIndex.map { case (ctr, i) =>
      val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
      val Def(d) = step
      d match {
        case n@ConstFix(value) => value
        case n@Tpes_Int_to_fix(v) => v match {
          case c@Const(value) => value
          case c@Param(value) => value
          case _ => throw new Exception(s"""Step is of unhandled node $n, $v""")
        }
        case _ => throw new Exception(s"""Step is of unhandled node $d""")
      }
    }
    emit(s"""val ${quote(sym)}_strides = List(${strides.map(s=>s"${quote(s)}").mkString(",")})""")

    val gap = 0 // Power-of-2 upcasting not supported yet
    val pars = counters.map{parOf(_)}
    val parsList = s"""List(${pars.mkString(",")})"""
    // emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop(\"${quote(sym)}_offset\");""")
    emit(s"""val ${quote(sym)} = Module(new Counter(${parsList}))""")
    emit(s"""${quote(sym)}.io.input.enable := ${quote(sym)}_en
${quote(sym)}.io.input.reset := ${rstStr.get}
val ${quote(sym)}_maxed = ${quote(sym)}.io.output.saturated""")

    if (!doneDeclaredSet.contains(sym)) {
      emit(s"""val ${quote(sym)}_done = ${quote(sym)}.io.output.done""")
      doneDeclaredSet += sym
    } else {
      emit(s"""${quote(sym)}_done := ${quote(sym)}.io.output.done""")
    }

    emit(s"""  ${quote(sym)}.io.input.maxes.zip(${quote(sym)}_maxes).foreach { case (port,max) => port := max }""")
    emit(s"""  ${quote(sym)}.io.input.strides.zip(${quote(sym)}_strides).foreach { case (port,stride) => port := stride.U }""")
    // emit(s"""  ${quote(sym)}.io.input.starts.zip(${quote(sym)}_starts).foreach { (port,start) => port := start }""")
    counters.zipWithIndex.map { case (ctr, i) =>
      val drop = pars.take(i+1).reduce{_+_} - pars(i)
      val take = pars(i)
      emit(s"""val ${quote(ctr)} = ${quote(sym)}.io.output.counts.drop(${drop}).take(${take})""")      
    }

    // Emit the valid bit calculations that don't exist in the IR
    parent match {
      case Deff(UnrolledForeach(_,_,counts,vs)) =>
        vs.zip(counts).zipWithIndex.map { case ((layer,count), i) =>
          layer.zip(count).map { case (v, c) =>
            emit(s"val ${quote(v)} = ${quote(c)} < ${quote(sym)}_maxes(${i})")
          }
        }
      case Deff(UnrolledReduce(_,_,_,_,counts,vs,_,_)) =>
        vs.zip(counts).zipWithIndex.map { case ((layer,count), i) =>
          layer.zip(count).map { case (v, c) =>
            emit(s"val ${quote(v)} = ${quote(c)} < ${quote(sym)}_maxes(${i})")
          }
        }
      case _ =>
    }

  }



}

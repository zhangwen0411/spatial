package spatial.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect, MaxJGenFat}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.HashMap

trait ControllerOpsExp extends ControllerCompilerOps with MemoryOpsExp with ExternCounterOpsExp {
  this: SpatialExp =>

  type Idx = FixPt[Signed,B32,B0]

  // --- Nodes
  case class ParallelPipe(func: Block[Unit])(implicit val ctx: SourceContext) extends Def[Pipeline]
  case class UnitPipe(func: Block[Unit])(implicit val ctx: SourceContext) extends Def[Pipeline]


  case class OpForeach(
    cchain: Exp[CounterChain],  // Loop counter chain
    func:   Block[Unit],        // Foreach function
    inds:   List[Sym[Idx]]      // Loop iterators
  )(implicit val ctx: SourceContext) extends Def[Pipeline]


  case class OpReduce[T,C[T]] (
    // - Inputs
    cchain: Exp[CounterChain],  // Loop counter chain
    accum:  Exp[C[T]],          // Reduction accumulator
    zero: Option[Exp[T]],       // Zero value
    foldAccum: Boolean,         // Act as a fold (true) or a reduce (false)
    // - Reified blocks
    ldFunc: Block[T],           // Accumulator load function (reified with acc, idx)
    stFunc: Block[Unit],        // Accumulator store function (reified with acc, idx, res)
    func:   Block[T],           // Map function
    rFunc:  Block[T],           // Reduction function
    // - Bound args
    inds:   List[Sym[Idx]],     // Loop iterators
    acc:    Sym[C[T]],          // Reduction accumulator (aliases with accum)
    res:    Sym[T],             // Reduction intermediate result (aliases with rFunc.res)
    rV:    (Sym[T], Sym[T])     // Reduction function inputs
  )(implicit val ctx: SourceContext, val memC: Mem[T,C], val numT: Num[T], val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]


  case class OpMemReduce[T,C[T]](
    // - Inputs
    ccOuter: Exp[CounterChain], // Counter chain for map (outer) loop
    ccInner: Exp[CounterChain], // Counter chain for reduce (inner) loop
    accum: Exp[C[T]],           // Reduction accumulator
    zero: Option[Exp[T]],       // Zero value
    foldAccum: Boolean,         // Act as a fold (true) or a reduce (false)
    // - Reified blocks
    func: Block[C[T]],          // Map function
    resLdFunc: Block[T],        // Partial result load function
    ldFunc: Block[T],           // Accumulator load function
    rFunc: Block[T],            // Reduction function
    stFunc: Block[Unit],        // Accumulator store function
    // - Bound args
    indsOuter: List[Sym[Idx]],  // Map (outer) loop iterators
    indsInner: List[Sym[Idx]],  // Reduce (inner) loop iterators
    part: Sym[C[T]],            // Partial result (aliases with func.res)
    acc: Sym[C[T]],             // Reduction accumulator (aliases with accum)
    res: Sym[T],                // Reduction intermediate result (aliases with rFunc.res)
    rV:  (Sym[T], Sym[T])       // Reduction function inputs
  )(implicit val ctx: SourceContext, val memC: Mem[T,C], val numT: Num[T], val mT: Manifest[T], val mC: Manifest[C[T]]) extends Def[Pipeline]



  // --- Internals
  def parallel_pipe(func: => Rep[Unit])(implicit ctx: SourceContext) = {
    val blk = reifyEffects(func)
    val effects = summarizeEffects(blk)
    reflectEffect(ParallelPipe(blk)(ctx), effects andAlso Simple())
  }

  def unit_pipe(func: => Rep[Unit])(implicit ctx: SourceContext) = {
    val blk = reifyEffects(func)
    val effects = summarizeEffects(blk)
    reflectEffect(UnitPipe(blk)(ctx), effects andAlso Simple())
  }

  def foreach_op(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = {
    val inds = List.fill(lenOf(cchain)){ fresh[Idx] } // Arbitrary number of bound args. Awww yeah.
    val blk = reifyEffects( func(indices_create(inds)) )
    reflectEffect(OpForeach(cchain, blk, inds), summarizeEffects(blk).star andAlso Simple())
  }

  def reduce_op[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], zero: Option[Rep[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __num: Num[T], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline]  = {
    // Loop indices
    val is = List.fill(lenOf(cchain)){ fresh[Idx] }
    val inds = indices_create(is)

    // Reified map function
    val mBlk = reifyEffects( func(inds) )

    // Reified load function
    val acc = reflectMutableSym( fresh[C[T]] )  // Has to be mutable since we write to "it"
    setProps(acc, getProps(accum))

    val ldBlk = reifyEffects(__mem.zeroLd(acc, true.asBit))

   // Reified reduction function
    val rV = (fresh[T], fresh[T])
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    // Reified store function
    val res = fresh[T]
    val stBlk = reifyEffects(__mem.zeroSt(acc, res, true.asBit))

    val effects = summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(OpReduce[T,C](cchain, accum, zero, foldAccum, ldBlk, stBlk, mBlk, rBlk, is, acc, res, rV), effects.star)
  }

  private def memreduce_common[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[C[T]], zero: Option[Rep[T]], func: Block[C[T]], isMap: List[Sym[Idx]], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __num: Num[T], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline] = {
    val isRed = List.fill(lenOf(cchainRed)){ fresh[Idx] } // Reduce loop indices
    val indsRed = indices_create(isRed)

    val part = fresh[C[T]]
    setProps(part, getProps(func))
    // Partial result load
    val ldPartBlk = reifyEffects( __mem.ld(part, isRed, true.asBit) )

    val acc = reflectMutableSym( fresh[C[T]] )
    setProps(acc, getProps(accum))
    // Accumulator load
    val ldBlk = reifyEffects( __mem.ld(acc, isRed, true.asBit) )

    val rV = (fresh[T],fresh[T])
    // Reified reduction function
    val rBlk = reifyEffects( rFunc(rV._1, rV._2) )

    val res = fresh[T]
    // Accumulator store function
    val stBlk = reifyEffects( __mem.st(acc, isRed, res, true.asBit) )

    val effects = summarizeEffects(func) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum.asInstanceOf[Sym[C[T]]]))

    reflectEffect(OpMemReduce[T,C](cchain, cchainRed, accum, zero, foldAccum, func, ldPartBlk, ldBlk, rBlk, stBlk, isMap, isRed, part, acc, res, rV), effects.star)
  }


  def memreduce_op[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[C[T]], zero: Option[Rep[T]], func: Rep[Indices] => Rep[C[T]], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __num: Num[T], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline] = {
    val isMap = List.fill(lenOf(cchain)){ fresh[Idx] }   // Map loop indices
    val indsMap = indices_create(isMap)
    val mBlk = reifyEffects( func(indsMap) )             // Reified map function

    memreduce_common(cchain, cchainRed, accum, zero, mBlk, isMap, rFunc, foldAccum)
  }

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@OpForeach(c,func,inds) => reflectPure(OpForeach(f(c),f(func),inds)(e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@OpForeach(c,func,inds), u, es) => reflectMirrored(Reflect(OpForeach(f(c),f(func),inds)(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)

    case e@OpReduce(c,a,z,fA,ld,st,func,rFunc,inds,acc,res,rV) => reflectPure(OpReduce(f(c),f(a),f(z),fA,f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(e.ctx, e.memC, e.numT, e.mT, e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@OpReduce(c,a,z,fA,ld,st,func,rFunc,inds,acc,res,rV), u, es) => reflectMirrored(Reflect(OpReduce(f(c),f(a),f(z),fA,f(ld),f(st),f(func),f(rFunc),inds,acc,res,rV)(e.ctx, e.memC, e.numT, e.mT, e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@OpMemReduce(c1,c2,a,z,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) => reflectPure(OpMemReduce(f(c1),f(c2),f(a),f(z),fA,f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.ctx,e.memC,e.numT, e.mT,e.mC))(mtype(manifest[A]), pos)
    case Reflect(e@OpMemReduce(c1,c2,a,z,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV), u, es) => reflectMirrored(Reflect(OpMemReduce(f(c1),f(c2),f(a),f(z),fA,f(func),f(ld1),f(ld2),f(rFunc),f(st),inds1,inds2,part,acc,res,rV)(e.ctx,e.memC,e.numT,e.mT,e.mC), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@ParallelPipe(blk) => reflectPure(ParallelPipe(f(blk))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@ParallelPipe(blk), u, es) => reflectMirrored(Reflect(ParallelPipe(f(blk))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@UnitPipe(blk) => reflectPure(UnitPipe(f(blk))(e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@UnitPipe(blk), u, es) => reflectMirrored(Reflect(UnitPipe(f(blk))(e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  override def propagate(lhs: Exp[Any], rhs: Def[Any]) = rhs match {
    case OpReduce(c,a,z,fA,ld,st,func,rFunc,inds,acc,res,rV) =>
      /*Console.println(s"$lhs = $rhs")
      Console.println(s"Getting props for $func")
      Console.println(s"block result of $func = ${getBlockResult(func)}")
      Console.println(s"props: ${makeString(getProps(func))}")*/

      setProps(acc, getProps(a))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(func))
      setProps(rV._2, getProps(func))
    case OpMemReduce(c1,c2,a,z,fA,func,ld1,ld2,rFunc,st,inds1,inds2,part,acc,res,rV) =>
      setProps(acc, getProps(a))
      setProps(part, getProps(func))
      setProps(res, getProps(rFunc))
      setProps(rV._1, getProps(ld1))
      setProps(rV._2, getProps(ld2))

    case _ => super.propagate(lhs, rhs)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case e:OpForeach        => syms(e.cchain) ::: syms(e.func)
    case e:OpReduce[_,_]    => syms(e.cchain) ::: syms(e.accum) ::: syms(e.zero) ::: syms(e.func) ::: syms(e.rFunc) ::: syms(e.ldFunc) ::: syms(e.stFunc)
    case e:OpMemReduce[_,_] => syms(e.ccOuter) ::: syms(e.ccInner) ::: syms(e.accum) ::: syms(e.zero) ::: syms(e.func) ::: syms(e.resLdFunc) ::: syms(e.ldFunc) ::: syms(e.rFunc) ::: syms(e.stFunc)
    case e:ParallelPipe     => syms(e.func)
    case e:UnitPipe         => syms(e.func)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e:OpForeach        => readSyms(e.cchain) ::: readSyms(e.func)
    case e:OpReduce[_,_]    => readSyms(e.cchain) ::: readSyms(e.accum) ::: readSyms(e.zero) ::: readSyms(e.func) ::: readSyms(e.rFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.stFunc)
    case e:OpMemReduce[_,_] => readSyms(e.ccOuter) ::: readSyms(e.ccInner) ::: readSyms(e.accum) ::: readSyms(e.zero) ::: readSyms(e.func) ::: readSyms(e.resLdFunc) ::: readSyms(e.ldFunc) ::: readSyms(e.rFunc) ::: readSyms(e.stFunc)
    case e:ParallelPipe     => readSyms(e.func)
    case e:UnitPipe         => readSyms(e.func)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e:OpForeach        => freqCold(e.cchain) ::: freqCold(e.func)
    case e:OpReduce[_,_]    => freqCold(e.func) ::: freqCold(e.rFunc) ::: freqCold(e.ldFunc) ::: freqCold(e.stFunc) ::: freqCold(e.cchain) ::: freqCold(e.accum) ::: freqCold(e.zero)
    case e:OpMemReduce[_,_] => freqNormal(e.ccOuter) ::: freqNormal(e.ccInner) ::: freqNormal(e.accum) ::: freqCold(e.zero) ::: freqNormal(e.func) ::: freqNormal(e.resLdFunc) ::: freqNormal(e.ldFunc) ::: freqNormal(e.rFunc) ::: freqNormal(e.stFunc)
    case e:ParallelPipe     => freqCold(e.func)
    case e:UnitPipe         => freqCold(e.func)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e:OpForeach        => e.inds ::: effectSyms(e.func) ::: effectSyms(e.cchain)
    case e:OpReduce[_,_]    => e.inds ::: List(e.rV._1, e.rV._2, e.acc, e.res) ::: effectSyms(e.cchain) ::: effectSyms(e.func) ::: effectSyms(e.rFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.stFunc) ::: effectSyms(e.accum)
    case e:OpMemReduce[_,_] => e.indsOuter ::: e.indsInner ::: List(e.rV._1, e.rV._2, e.acc, e.res, e.part) ::: effectSyms(e.ccOuter) ::: effectSyms(e.ccInner) ::: effectSyms(e.accum) ::: effectSyms(e.func) ::: effectSyms(e.resLdFunc) ::: effectSyms(e.ldFunc) ::: effectSyms(e.rFunc) ::: effectSyms(e.stFunc)
    case e:ParallelPipe     => effectSyms(e.func)
    case e:UnitPipe         => effectSyms(e.func)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenControllerOps extends ScalaGenEffect {
  val IR: ControllerOpsExp with SpatialCodegenOps
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ParallelPipe(blk) =>
      stream.println(s"val ${quote(sym)} = {")
      emitBlock(blk)
      stream.println("()")
      stream.println("}")

    case UnitPipe(blk) =>
      stream.println(s"val ${quote(sym)} = {")
      emitBlock(blk)
      stream.println("()")
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenControllerOps extends MaxJGenEffect with MaxJGenFat {
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

  var bbd = ""

  def newStream(fileName:String):PrintWriter = {
    val path = bbd + java.io.File.separator + fileName + ".maxj"
    Console.println(s"[StreamWriter] Making newstream $path")
    val pw = new PrintWriter(path)
    pw
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
          val str = s"${value}"
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
    bbd = buildDir
    // Console.println(s"[Initialize] Setting buildDir to $bd")
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
    emitComment(s"Block ${blockName} {")
    emit("{")
    emitBlock(y)
    emit(s"""${if (doNotClose) "" else "}"}""")
    emitComment(s"} Block ${blockName}")
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
              new DFEVar[]{${stages.map{s => quote(s)+"_done"}.mkString(",")}});""")
        }
      case _ =>
    }
  }

	var expToArg = HashMap[Exp[Any],Exp[Reg[Any]]]()
	var argToExp = HashMap[Exp[Reg[Any]],Exp[Any]]()
  override def preProcess[A:Manifest](body: Block[A]) = {
    val argInPass = new MaxJArgInPass {
      val IR: MaxJGenControllerOps.this.IR.type = MaxJGenControllerOps.this.IR
    }
    argInPass.run(body)
    expToArg = argInPass.expToArg
    argToExp = argInPass.argToExp

    val regChainPass = new RegChainPass {
      val IR: MaxJGenControllerOps.this.IR.type = MaxJGenControllerOps.this.IR
    }
    regChainPass.run(body)
    quoteSuffix = regChainPass.quoteSuffix
    super.preProcess(body)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hwblock(func) =>
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
			emitComment("Emitting Hwblock dependencies {")
      val hwblockDeps = recursiveDeps(rhs)
      expToArg.keys.filterNot { hwblockDeps.contains(_) } foreach { argToExp -= expToArg(_) }

      val emitted = Set[Exp[Any]]()
      def emitIfUndeclared(e: Exp[Any]) = {
        if (!emitted.contains(e)) {
          val ts = tpstr(parOf(e))(e.tp, implicitly[SourceContext])
          emit(s"""DFEVar ${quote(e)} = $ts.newInstance(this);""")
          emitted += e
        }
      }

      hwblockDeps.foreach { s =>
        val Def(d) = s
        emit(s"""// Dep: ${quote(s)} = $d""")

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
			emitComment(" End Hwblock dependencies }")
      emitComment(s"quoteSuffix = $quoteSuffix")
      emit(s"""DFEVar ${quote(sym)}_en = top_en;""")
      emitGlobalWire(s"""${quote(sym)}_done""")
      emit(s"""top_done <== ${quote(sym)}_done;""")
      emit(s"""// Hwblock: childrenOf(${quote(sym)}) = ${childrenOf(sym)}""")
      emitController(sym, None)
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
      var hadThingsInside = if (isInnerPipe(sym)) {false} else {true}
      controlNodeStack.push(sym)
      val smStr = styleOf(sym) match {
        case CoarsePipe => s"Metapipe"
        case StreamPipe => "Streampipe"
        case InnerPipe => "Innerpipe"
        case SequentialPipe => s"Seqpipe"
        case ForkJoin => s"Parpipe"
      }

      print_stage_prefix(s"Unit $smStr",s"",s"${quote(sym)}", hadThingsInside)
      emit(s"""// Unit pipe writtenIn(${quote(sym)}) = ${writtenIn(sym)}""")
      writtenIn(sym) foreach { s =>
        val Def(d) = s
        emit(s"""//   ${quote(s)} = $d, isAccum(${quote(s)}) = ${isAccum(s)}""")
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

      print_stage_suffix(quote(sym), hadThingsInside)
      controlNodeStack.pop

    case _ => super.emitNode(sym,rhs)
  }

  def emitController(sym:Sym[Any], cchain:Option[Exp[CounterChain]]) {
    val smStr = styleOf(sym) match {
			case CoarsePipe => s"${quote(sym)}_MPSM"
      case StreamPipe => "StrmSM"
      case InnerPipe => "PipeSM"
      case SequentialPipe => s"${quote(sym)}_SeqSM"
      case ForkJoin => s"${quote(sym)}_ParSM"
    }
    emitComment(s"""${smStr} ${quote(sym)} {""")

    /* State Machine Instatiation */
    // IO
    styleOf(sym) match {
      case InnerPipe =>
        val numCounters = if (cchain.isDefined) {
          val Def(EatReflect(Counterchain_new(counters))) = cchain.get
          counters.size
        } else {
          1
        }
        emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop("${quote(sym)}_offset");""")
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${smStr}(this, $numCounters));""")
        emit(s"""    ${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
        emit(s"""    ${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1-${quote(sym)}_offset);""")

        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
        emitGlobalWire(s"""${quote(sym)}_rst_done""")
        emit(s"""${quote(sym)}_sm.connectInput("rst_done", ${quote(sym)}_rst_done);""")
        emit(s"""${quote(sym)}_rst_done <== stream.offset(${quote(sym)}_rst_en, -${quote(sym)}_offset-1);""")
        if (!cchain.isDefined) {
          // Unit pipe, emit constant 1's wherever required
          emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_0", constant.var(dfeInt(32), 1));""")
          emit(s"""${quote(sym)}_sm.connectInput("ctr_done", stream.offset(${quote(sym)}_sm.getOutput("ctr_en"), -1));""")
        }
      case CoarsePipe =>
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${smStr}(this));""")
        emit(s"""    ${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
        emit(s"""    ${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")
        if (cchain.isDefined) {
          val Def(EatReflect(Counterchain_new(counters))) = cchain.get
          var niter_str = s""
          counters.zipWithIndex.map {case (ctr,i) =>
            val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
            if (i > 0) {
              niter_str += " * "
            }
            niter_str += s"((${quote(end)} - ${quote(start)}) / (${quote(step)} * ${quote(par)}))"
          }
          emit(s"""DFEVar ${quote(sym)}_niter = ${quote(niter_str)};""")
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", stream.offset(${quote(sym)}_niter.cast(dfeUInt(32)),-5)/*makes BFS work*/);""")
        } else {
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", constant.var(dfeUInt(32), 1));""")
        }
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case SequentialPipe =>
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${smStr}(this));""")
        emit(s"""    ${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
        emit(s"""    ${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")
        if (cchain.isDefined) {
          val Def(EatReflect(Counterchain_new(counters))) = cchain.get
          var niter_str = s""
          counters.zipWithIndex.map {case (ctr,i) =>
            val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
            if (i > 0) {
              niter_str += " * "
            }
            niter_str += s"((${quote(end)} - ${quote(start)}) / (${quote(step)} * ${quote(par)}))"
          }
          emit(s"""DFEVar ${quote(sym)}_niter = ${quote(niter_str)};""")
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", stream.offset(${quote(sym)}_niter.cast(dfeUInt(32)),-5)/*makes BFS work*/);""")
        } else {
          emit(s"""${quote(sym)}_sm.connectInput("sm_numIter", constant.var(dfeUInt(32), 1));""")
        }
        emit(s"""DFEVar ${quote(sym)}_rst_en = ${quote(sym)}_sm.getOutput("rst_en");""")
      case ForkJoin =>
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${smStr}(this));""")
        emit(s"""    ${quote(sym)}_sm.connectInput("sm_en", ${quote(sym)}_en);""")
        emit(s"""    ${quote(sym)}_done <== stream.offset(${quote(sym)}_sm.getOutput("sm_done"),-1);""")

    }

    val childrenSet = Set[String]()
    val percentDSet = Set[String]()
    /* Control Signals to Children Controllers */
    if (!isInnerPipe(sym)) {
		  childrenOf(sym).zipWithIndex.foreach { case (c, idx) =>
		  	emitGlobalWire(s"""${quote(c)}_done""")
		  	emit(s"""${quote(sym)}_sm.connectInput("s${idx}_done", ${quote(c)}_done);""")
        emitGlobalWire(s"""${quote(c)}_en""")
        emit(s"""${quote(c)}_en <== ${quote(sym)}_sm.getOutput("s${quote(idx)}_en");""")
        childrenSet += (s"${quote(c)}_en, ${quote(c)}_done")
        percentDSet += (s"${idx}: %d %d")
		  	enDeclaredSet += c
		  	doneDeclaredSet += c
		  }
    }

    emit(s"""// debug.simPrintf(${quote(sym)}_en, "pipe ${quote(sym)}: ${percentDSet.toList.mkString(",   ")}\\n", ${childrenSet.toList.mkString(",")});""")


    if (styleOf(sym)!=ForkJoin) {
      if (cchain.isDefined) {
        emitCChainCtrl(sym, cchain.get)
      } else {
        emit(s"""DFEVar ${quote(sym)}_datapath_en = ${quote(sym)}_en & ~${quote(sym)}_rst_en;""")
        emit(s"""DFEVar ${quote(sym)}_ctr_en = ${quote(sym)}_datapath_en;""")
      }
    }

    emitComment(s"""} ${smStr} ${quote(sym)}""")
  }

  def emitCChainCtrl(sym: Sym[Any], cchain: Exp[CounterChain]) {
		val Deff(Counterchain_new(counters)) = cchain

    /* Reset CounterChain */
    //TODO: support reset of counterchain to sequential and metapipe in templete
    counters.zipWithIndex.map {case (ctr,i) =>
      val Def(EatReflect(Counter_new(start, end, step, par))) = ctr
      styleOf(sym) match {
        case InnerPipe =>
          emit(s"""${quote(sym)}_sm.connectInput("sm_maxIn_$i", ${quote(end)});""")
          // emit(s"""${quote(sym)}_sm.connectInput("sm_trashCnt", constant.var(dfeUInt(32), ${trashCount(bound(end).get.toInt, sym)}));""")
          // emit(s"""DFEVar ${quote(sym)}_trash_en = ${quote(sym)}_sm.getOutput("trashEn");""")
          emit(s"""DFEVar ${quote(ctr)}_max_$i = ${quote(sym)}_sm.getOutput("ctr_maxOut_$i");""")
        case ForkJoin => throw new Exception("Cannot have counter chain control logic for fork-join (parallel) controller!")
        case _ =>
          emit(s"""DFEVar ${quote(ctr)}_max_$i = ${quote(end)};""")
      }
    }

    styleOf(sym) match {
      case InnerPipe =>
        emitGlobalWire(s"""${quote(cchain)}_done""")
        doneDeclaredSet += cchain
        emit(s"""${quote(sym)}_sm.connectInput("ctr_done", ${quote(cchain)}_done);""")
        if (consumesMemFifo(sym)) {
          emit(s"""DFEVar ${quote(sym)}_datapath_en = ${quote(sym)}_sm.getOutput("ctr_en");""")
        } else {
          emit(s"""DFEVar ${quote(sym)}_datapath_en = ${quote(sym)}_sm.getOutput("ctr_en");""")
        }

      case ForkJoin => throw new Exception("Cannot have counter chain control logic for fork-join (parallel) controller!")
      case _ =>
        emit(s"""DFEVar ${quote(sym)}_datapath_en = ${quote(sym)}_en;""")
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
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym,
                    Some(s"stream.offset(${quote(sym)}_datapath_en & ${quote(cchain)}_chain.getCounterWrap(${quote(counters.head)}), -${quote(sym)}_offset-1)"))
            } else {
              val ctrEn = s"${quote(sym)}_datapath_en"
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
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
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym,
                    Some(s"stream.offset(${quote(sym)}_datapath_en & ${quote(cchain)}_chain.getCounterWrap(${quote(counters.head)}), -${quote(sym)}_offset-1)"))
            } else {
              val ctrEn = s"${quote(sym)}_datapath_en"
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            }

          case n@UnrolledReduce(cchain, accum, func, rFunc, inds, ens, acc, rV) =>
            emit(s"""DFEVar ${quote(sym)}_loopLengthVal = ${quote(sym)}_offset.getDFEVar(this, dfeUInt(9));""")
            emit(s"""CounterChain ${quote(sym)}_redLoopChain = control.count.makeCounterChain(${quote(sym)}_datapath_en);""")
            // emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${stream_offset_guess+1}, 1);""")
            emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${quote(sym)}_loopLengthVal, 1);""")
            emit(s"""DFEVar ${quote(sym)}_redLoop_done = stream.offset(${quote(sym)}_redLoopChain.getCounterWrap(${quote(sym)}_redLoopCtr), -1);""")
            val ctrEn = s"${quote(sym)}_datapath_en & ${quote(sym)}_redLoop_done"
            emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
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
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            } else {
              emit(s"""DFEVar ${quote(sym)}_loopLengthVal = ${quote(sym)}_offset.getDFEVar(this, dfeUInt(9));""")
              emit(s"""CounterChain ${quote(sym)}_redLoopChain =
		        		control.count.makeCounterChain(${quote(sym)}_datapath_en);""")
              // emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${stream_offset_guess+1}, 1);""")
              emit(s"""DFEVar ${quote(sym)}_redLoopCtr = ${quote(sym)}_redLoopChain.addCounter(${quote(sym)}_loopLengthVal, 1);""")
              emit(s"""DFEVar ${quote(sym)}_redLoop_done = stream.offset(${quote(sym)}_redLoopChain.getCounterWrap(${quote(sym)}_redLoopCtr), -1);""")
              val ctrEn = s"${quote(sym)}_datapath_en & ${quote(sym)}_redLoop_done"
              emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
              val rstStr = s"${quote(sym)}_done"
              emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
            }
        }
      case CoarsePipe =>
        val ctrEn = s"${quote(childrenOf(sym).head)}_done"
        emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
        val rstStr = s"${quote(sym)}_done"
        emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
      case SequentialPipe =>
        val ctrEn = s"${quote(childrenOf(sym).last)}_done"
        emit(s"""DFEVar ${quote(sym)}_ctr_en = $ctrEn;""")
        val rstStr = s"${quote(sym)}_done"
		    emitCustomCounterChain(cchain, Some(ctrEn), Some(rstStr), sym)
    }

  }

  def emitCustomCounterChain(cchain: Exp[CounterChain], en: Option[String], rstStr: Option[String], parent: Exp[Any], done: Option[String]=None) = {
    val sym = cchain
    emitComment("CustomCounterChain {")

    if (!enDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_en = ${en.get};""")
      enDeclaredSet += sym
    }



    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
    val Deff(Counterchain_new(counters)) = cchain

    // Connect maxes
    val maxes = counters.zipWithIndex.map { case (ctr, i) =>
      val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
      parentOf(cchain.asInstanceOf[Rep[CounterChain]]) match {
        case Some(s) =>
          s.tp.erasure.getSimpleName match {  // <- There's got to be a better way
          case "Pipeline" => s"${quote(ctr)}_max_$i"
          case "SpatialPipeline" => s"${quote(ctr)}_max_$i"
          case _ => quote(end)
        }
        case None => quote(end)
      }
    }

    // val trashStr = if (false/*consumesMemFifo(sym)*/) {
    //   val ctr = counters(0)
    //   val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
    //   val c = trashCount(bound(end).get.toInt, sym)
    //   s" + ${c}"
    // } else {""}
    emit(s"""DFEVar[] ${quote(sym)}_max = {${maxes.map(m=>s"${quote(m)}").mkString(",")}};""")

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
    emit(s"""int[] ${quote(sym)}_strides = {${strides.map(s=>s"${quote(s)}").mkString(",")}};""")

    val gap = 0 // Power-of-2 upcasting not supported yet

    emit(s"""OffsetExpr ${quote(sym)}_offset = stream.makeOffsetAutoLoop(\"${quote(sym)}_offset\");""")
    emit(s"""SMIO ${quote(sym)} = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_CtrSM(owner, ${quote(sym)}_strides)); // gap = ${gap}""")
    emit(s"""${quote(sym)}.connectInput("en", ${quote(sym)}_en);
${quote(sym)}.connectInput("reset", ${rstStr.get});
DFEVar ${quote(sym)}_maxed = ${quote(sym)}.getOutput("saturated");""")

    val doneStr = if (!done.isDefined) {
      s"""stream.offset(${quote(sym)}.getOutput("done"), -1)"""
    } else {
      done.get
    }

    if (!doneDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_done = $doneStr;""")
      doneDeclaredSet += sym
    } else {
      emit(s"""${quote(sym)}_done <== $doneStr;""")
    }

    emit(s"""OffsetExpr ${quote(sym)}_additionalOffset = new OffsetExpr();""")
    counters.zipWithIndex.map { case (ctr, i) =>
      emit(s"""${quote(sym)}.connectInput("max${i}", ${quote(sym)}_max[${i}]);""")
      if (parOf(ctr) == 1) {
        emit(s"""DFEVar ${quote(ctr)} = ${quote(sym)}.getOutput("counter${i}");""")
        // cast(n.ctrs(i)) // Cast if necessary
      } else {
        emit(s"""DFEVector<DFEVar> ${quote(ctr)} = new DFEVectorType<DFEVar>(dfeInt(32), ${parOf(ctr)}).newInstance(this);
${quote(ctr)}[0] <== ${quote(sym)}.getOutput("counter${i}");
for (int i = 0; i < ${parOf(ctr)-1}; i++) {
  ${quote(ctr)}[i+1] <== ${quote(sym)}.getOutput("counter${i}_extension" + i);
}""")
        // (0 until n.par(i)) map {k =>
        //     cast(n.ctrs(i)) // Cast if necessary
        // }
      }
    }

    parent match {
      case Deff(UnrolledForeach(_,_,counts,vs)) => 
        vs.zip(counts).zipWithIndex.map { case ((layer,count), i) => 
          layer.zip(count).map { case (v, c) => 
            emit(s"DFEVar ${quote(v)} = ${quote(c)} < ${quote(sym)}_max[${i}];")
          }
        }
      case Deff(UnrolledReduce(_,_,_,_,counts,vs,_,_)) => 
        vs.zip(counts).zipWithIndex.map { case ((layer,count), i) => 
          layer.zip(count).map { case (v, c) => 
            emit(s"DFEVar ${quote(v)} = ${quote(c)} < ${quote(sym)}_max[${i}];")
          }
        }
      case _ => 
    }

    emitComment("} CustomCounterChain")

  }

	def emitMaxJCounterChain(cchain: Exp[CounterChain], en: Option[String], done: Option[String]=None) = {
		val sym = cchain
    // 'En' and 'done' signal contract: Enable signal is declared here, done signal must be
    // declared before this method is called.
    // Both signals are defined here.
    emitComment("MaxJCounterChain {")
    if (!enDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_en = ${en.get};""")
      enDeclaredSet += sym
    }
    emit(s"""CounterChain ${quote(sym)} = control.count.makeCounterChain(${quote(sym)}_en);""")

    // For Pipes, max must be derived from PipeSM
    // For everyone else, max is as mentioned in the ctr
		val Deff(Counterchain_new(counters)) = cchain
		counters.zipWithIndex.map {case (ctr,i) =>
			val Def(EatReflect(Counter_new(start, end, step, _))) = ctr
			val max = parentOf(cchain.asInstanceOf[Rep[CounterChain]]) match {
				case Some(s) =>
          s.tp.erasure.getSimpleName match {  // <- There's got to be a better way
					case "Pipeline" => s"${quote(ctr)}_max_$i"
					case "SpatialPipeline" => s"${quote(ctr)}_max_$i"
					case _ => quote(end)
				}
				case None => quote(end)
			}

      // Scala - MaxJ Types: A short story
      // MaxJ's counter library expects 'counter max' values for each counter to be of
      // type "dfeUInt()". The output counter object created from both addCounter() and
      // addCountervect() returns a counter of type dfeUInt().
      // Scala treats all fixed point numbers as signed numbers by default. This means that
      // all lifted constants, default types for ArgIns etc are signed numbers. Counter values,
      // therefore, are treated as signed numbers.
      // Temporary fix: The following logic adds typecasts to satisfy both Scala and MaxJ.
      // [TODO] Real fix: Replace MaxJ's counter chain library with our home-grown state machine
      // which offers more flexibility.
      // [TODO]: The following typecasts (and state machines like PipeSM) assume that counters
      // are always 32-bit. This is almost always overkill, and must be fixed.
      val maxType = "dfeUInt(32)"
      val maxWithCast = s"""${max}.cast($maxType)"""
      val counterType = tpstr(parOf(ctr))(ctr.tp, implicitly[SourceContext])
      val counterObject = s"""${quote(ctr)}_obj"""
      val Def(d) = step
      val constStep = d match {
        case n@ConstFix(value) => value
        case n@Tpes_Int_to_fix(v) => v match {
          case c@Const(value) => value
          case c@Param(value) => value
          case _ => throw new Exception(s"""Step is of unhandled node $n, $v""")
        }
        case _ => throw new Exception(s"""Step is of unhandled node $d""")
      }

      if (parOf(ctr) == 1) {
        emit(s"""${maxJPre(ctr)} $counterObject = ${quote(cchain)}.addCounter(${quote(maxWithCast)}, ${quote(constStep)});""")
      } else {
        emit(s"""${maxJPre(ctr)} $counterObject = ${quote(cchain)}.addCounterVect(${quote(parOf(ctr))}, ${quote(maxWithCast)}, ${quote(constStep)});""")
      }
      emit(s"""${maxJPre(ctr)} ${quote(ctr)} = $counterObject.cast($counterType);""")
    }

    val doneStr = if (!done.isDefined) {
        s"stream.offset(${quote(cchain)}.getCounterWrap(${quote(counters(0))}_obj),-1)"
    } else {
      done.get
    }

    if (!doneDeclaredSet.contains(sym)) {
      emit(s"""DFEVar ${quote(sym)}_done = $doneStr;""")
      doneDeclaredSet += sym
    } else {
      emit(s"""${quote(sym)}_done <== $doneStr;""")
  	}

    emitComment("} MaxJCounterChain")
  }


}

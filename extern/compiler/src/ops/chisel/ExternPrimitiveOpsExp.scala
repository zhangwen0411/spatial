package spatial.compiler.ops

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.internal.{Traversal}
import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, CGenEffect, MaxJGenEffect, ChiselGenEffect}
import ppl.delite.framework.transform.{DeliteTransform}
import scala.reflect.{Manifest,SourceContext}

import scala.collection.mutable.HashMap

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._


trait ChiselGenExternPrimitiveOps extends ChiselGenEffect {
  val IR:UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform

  import IR.{infix_until => _, looprange_until => _, println => _, _}

  var emitted_consts: Set[(Exp[Any], Def[Any])] = Set.empty
  var emitted_argins: Set[(Exp[Any], String)] = Set.empty
  var emitted_reglibreads: Set[(Exp[Any], String)] = Set.empty
  var emitted_ands = List() // TODO: HUGE HACK BECAUSE WE DON'T KNOW WHY SYMS GET RE-EMITTED!
  def addEmittedConsts(xs: Exp[Any]*) = xs.foreach {
    case lhs@Def(rhs) => if (!emitted_consts.contains((lhs, rhs))) { emitted_consts += ((lhs, rhs)) }
    case _ =>
  }


  var traversals: List[Traversal{val IR: ChiselGenExternPrimitiveOps.this.IR.type}] = Nil

  lazy val preCodegen = new ChiselPreCodegen {
    val IR: ChiselGenExternPrimitiveOps.this.IR.type = ChiselGenExternPrimitiveOps.this.IR
  }

  override def initializeGenerator(bd:String): Unit = {
    preCodegen.buildDir = bd
    traversals = IR.traversals
    super.initializeGenerator(bd)
  }


  def runTraversals[A:Manifest](b: Block[A]): Block[A] = {
    println("ChiselCodegen: applying transformations")
    var curBlock = b
    println("Traversals:\n\t" + traversals.map(_.name).mkString("\n\t"))

    for (t <- traversals) {
      printlog("  Block before transformation: " + curBlock)
      curBlock = t.run(curBlock)
      printlog("  Block after transformation: " + curBlock)
    }
    println("ChiselCodegen: done transforming")
    (curBlock)
  }

  override def preProcess[A: Manifest](body: Block[A]) = {
    preCodegen.run(body)
    super.preProcess(body)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Min2(a, b) =>
      emit(s"""val ${quote(sym)} = Utils.min(${quote(a)}, ${quote(b)});""")
    case Max2(a, b) =>
      emit(s"""val ${quote(sym)} = Utils.max(${quote(a)}, ${quote(b)});""")
    case ConstFixPt(x,_,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }
    case ConstFltPt(x,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }

    case FixPt_Add(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} + ${quote(b)};""")

    case FixPt_Sub(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} - ${quote(b)};""")

    case FltPt_Add(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} + ${quote(b)};""")

    case FixPt_Div(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")

    case FltPt_Div(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")

    case FixPt_Mul(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")

    case FixPt_Mod(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} % ${quote(b)};""")

    case FltPt_Mul(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")

    case FixPt_Lt(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} < ${quote(b)}""")

    case FixPt_Leq(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} <= ${quote(b)}""")

    case FixPt_Neq(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} =/= ${quote(b)}""")

    case FixPt_Eql(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} === ${quote(b)}""")

    case FixPt_And(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")

    case FixPt_Or(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")

    case FixPt_Lsh(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} << ${quote(b)} ;""")

    case FixPt_Rsh(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} >> ${quote(b)} ;""")

    case FltPt_Lt(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} < ${quote(b)}""")

    case FltPt_Leq(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} <= ${quote(b)};""")

    case FltPt_Neq(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} =/= ${quote(b)};""")

    case FltPt_Eql(a,b) =>
      emit(s"""val ${quote(sym)} = ${quote(a)} === ${quote(b)};""")

    case Bit_Not(a) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ~( ${quote(a)} );""")

    case Bit_And(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")  

    case Bit_Or(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")

    case Bit_Xor(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ${quote(a)} ^ ${quote(b)} ;""")

    case Bit_Xnor(a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = ~ ( ${quote(a)} ^ ${quote(b)} ) ;""")

    case Mux2(sel,a,b) =>
      val pre = chiselPre(sym)
      emit(s"""$pre ${quote(sym)} = Mux(${quote(sel)}, ${quote(a)}, ${quote(b)})""")

    case Tpes_Int_to_fix(x) =>  // Emit this node in MaxJ only if x is a const
      val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      x match {
        case _:Const[_] | _:Param[_] =>
          emit(s"""var ${quote(sym)} = UInt(${quote(x)}) // emit const""")
        case _ =>
          emit(s"""// var $sym = ${quote(x)}.cast($ts) // emit const""")
        }

    case Tpes_String_to_fixpt(x) =>
      emit(s"""// val ${quote(sym)} = UInt(${quote(x)}) // TODO: Why do these nodes exist?""")

    case _ => super.emitNode(sym, rhs)

  }

  override def emitFileFooter() = {
    withStream(baseStream) {
      emit(s"""// Emit consts""")
      emitted_consts.foreach {
        case ((s, d)) =>
          d match {
            case ConstFixPt(x,_,_,_) =>
              val tsb = ctpstrb(parOf(s)) (s.tp, implicitly[SourceContext])
              val tse = ctpstre(parOf(s)) (s.tp, implicitly[SourceContext])
              emit(s"""val ${quote(s)} =  ${x}.U  // emit const""")
            case ConstFltPt(x,_,_) =>
              val tsb = ctpstrb(parOf(s)) (s.tp, implicitly[SourceContext])
              val tse = ctpstre(parOf(s)) (s.tp, implicitly[SourceContext])
              emit(s"""val ${quote(s)} =  ${x}.U  // emit const""")
            case _ =>
              withStream(baseStream) {
                emit(s"""// Can't emit ${quote(s)}""")
              }
            }
        case _ =>
          throw new Exception(s"Cannot match, you did something really wrong")
      }
    }
    super.emitFileFooter()
  }

}

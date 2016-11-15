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

trait SpatialBit
trait FixedPoint[SIGN,INT,FRAC]
trait FloatPoint[SIG,EXP]

trait ExternPrimitiveTypesExp extends ExternPrimitiveTypes with BaseExp {
  type Bit                  = SpatialBit
  type FixPt[SIGN,INT,FRAC] = FixedPoint[SIGN,INT,FRAC]
  type FltPt[SIG,EXP]       = FloatPoint[SIG,EXP]

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint[_,_,_]])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint[_,_]])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[SpatialBit])

  def fixManifest[S:Manifest,I:Manifest,F:Manifest]: Manifest[FixPt[S,I,F]] = manifest[FixedPoint[S,I,F]]
  def fltManifest[G:Manifest,E:Manifest]: Manifest[FltPt[G,E]] = manifest[FloatPoint[G,E]]
  def bitManifest: Manifest[Bit] = manifest[SpatialBit]
}

trait ExternPrimitiveOpsExp extends ExternPrimitiveCompilerOps with ExternPrimitiveTypesExp with TpesOpsExp
  with SpatialMetadataOpsExp with FixPtOpsExp {

  this: SpatialExp =>

  var rwPortAlias = HashMap[Exp[Any],Exp[Any]]()

  case class Min2[T](a: Rep[T],b:Rep[T])(implicit val mT: Manifest[T], val oT: Order[T], val nT: Num[T], val ctx: SourceContext) extends Def[T]
  case class Max2[T](a: Rep[T],b:Rep[T])(implicit val mT: Manifest[T], val oT: Order[T], val nT: Num[T], val ctx: SourceContext) extends Def[T]

  def min2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = reflectPure(Min2(a,b))
  def max2[T:Manifest:Order:Num](a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = reflectPure(Max2(a,b))

  // --- Internal API
  // Shorthand versions for matching on ConstFixPt and ConstFltPt without the manifests
  object ParamFix {
    def unapply(x: Exp[Any]): Option[Param[Int]] = x match {
      case Def(EatReflect(Tpes_Int_to_fix(e: Param[Int]))) => Some(e)
      case _ => None
    }
  }

  object ConstFix {
    def unapply(x: Any): Option[Any] = x match {
      case ConstFixPt(x,_,_,_) => Some(x)
      case Def(ConstFixPt(x,_,_,_)) => Some(x)
      case _ => None
    }
  }
  object ConstFlt {
    def unapply(x: Any): Option[Any] = x match {
      case ConstFltPt(x,_,_) => Some(x)
      case Def(ConstFltPt(x,_,_)) => Some(x)
      case _ => None
    }
  }

  override def nbits[T:Manifest]: Int = manifest[T] match {
    case StructType(_,fields) => fields.map(f => nbits(f._2)).fold(0){_+_}
    case _ => super.nbits[T]
  }

  def nbits(e: Exp[Any]): Int = nbits(e.tp)
  def sign(e: Exp[Any]): Boolean = sign(e.tp)

  def isBits[T:Manifest]: Boolean = manifest[T] match {
    case t if isFltPtType(t) || isFixPtType(t) || isBitType(t) => true
    case StructType(_,fields) => fields.map(f => isBits(f._2)).fold(true){_&&_}
    case _ => false
  }

  def isStaticSize[T:Manifest](x: Rep[T]): Boolean = x match {
    case ConstFix(_) => true
    case ParamFix(_) => true
    case _ => false
  }

  def isIndexType(t: Manifest[_]) = {
    isFixPtType(t) && sign(t) && nbits(t.typeArguments(1)) == 32 && nbits(t.typeArguments(2)) == 0
  }

  // --- Rewrite Rules
  override def globalCheck(__arg0: Rep[Any])(implicit __pos: SourceContext): Boolean = __arg0 match {
    case p: Param[_] => true
    case Const(x) => true
    case _ => super.globalCheck(__arg0)
  }

  private def extractNumericConst[T:Manifest](x: T): Option[Double] = {
    val mD = manifest[Double]
    val mF = manifest[Float]
    val mI = manifest[Int]
    val mL = manifest[Long]

    manifest[T] match {
      case `mI` => Some(x.asInstanceOf[Int].toDouble)
      case `mL` => Some(x.asInstanceOf[Long].toDouble)
      case `mF` => Some(x.asInstanceOf[Float].toDouble)
      case `mD` => Some(x.asInstanceOf[Double])
      case mT   => None
    }
  }

  override def boundOf(x: Rep[Any])(implicit ctx: SourceContext): Option[MBound] = x match {
    case p@Param(x) =>
      extractNumericConst(x)(p.tp).map{c => if (p.isFixed) fixed(c) else exact(c) }

    case Const(c) =>
      extractNumericConst(c)(x.tp).map{b => fixed(b) }

    case _ => super.boundOf(x)
  }


  // TODO: Move to spec later?
  // Rewrite needed for length calculation of vectors, ranges
  override def sub[S:Manifest,I:Manifest,F:Manifest](__arg0: Rep[FixPt[S,I,F]],__arg1: Rep[FixPt[S,I,F]])(implicit __pos: SourceContext,__imp1: Overload2) = {
    (__arg0) match {
      case Def(FixPt_Add(`__arg1`, y: Rep[FixPt[S,I,F]])) => y   // (x + y) - x == y
      case Def(FixPt_Add(y: Rep[FixPt[S,I,F]], `__arg1`)) => y   // (y + x) - x == y
      case _ => super.sub(__arg0, __arg1)(manifest[S],manifest[I],manifest[F],__pos,__imp1)
    }
  }

  // Dual (just for completeness)
  override def add[S:Manifest,I:Manifest,F:Manifest](__arg0: Rep[FixPt[S,I,F]],__arg1: Rep[FixPt[S,I,F]])(implicit __pos: SourceContext,__imp1: Overload2) = {
    (__arg0,__arg1) match {
      case (Def(FixPt_Sub(x: Rep[FixPt[S,I,F]], `__arg1`)), _) => x  // (x - y) + y == x
      case (_, Def(FixPt_Sub(x: Rep[FixPt[S,I,F]], `__arg0`))) => x  // y + (x - y) == x
      case _ => super.add(__arg0, __arg1)(manifest[S],manifest[I],manifest[F],__pos,__imp1)
    }
  }

  override def fix_to_int[S:Manifest,I:Manifest](x: Rep[FixPt[S,I,B0]])(implicit __pos: SourceContext) = x match {
    case Deff(e@Tpes_Int_to_fix(x)) if sign(e._mS) == sign(manifest[S]) && nbits(e._mI) == nbits(manifest[I]) => x.asInstanceOf[Exp[Int]]
    case _ => super.fix_to_int(x)
  }
  override def int_to_fix[S:Manifest,I:Manifest](x: Rep[Int])(implicit ctx: SourceContext) = x match {
    case Deff(e@Tpes_Fix_to_int(x)) if sign(e._mS) == sign(manifest[S]) && nbits(e._mI) == nbits(manifest[I]) => x.asInstanceOf[Exp[FixPt[S,I,B0]]]
    case _ => super.int_to_fix(x)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case EatReflect(e@Min2(a,b)) => reflectPure(Min2(f(a),f(b))(e.mT,e.oT,e.nT,e.ctx))(mtype(manifest[A]),pos)
    case EatReflect(e@Max2(a,b)) => reflectPure(Max2(f(a),f(b))(e.mT,e.oT,e.nT,e.ctx))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CGenExternPrimitiveOps extends CGenEffect {
  val IR: ExternPrimitiveOpsExp with SpatialCodegenOps
  import IR._

  def bitsToStringInt(x: Int) = x match {
    case n: Int if n <= 8 => "8"
    case n: Int if n <= 16 => "16"
    case n: Int if n <= 32 => "32"
    case _ => "64"
  }

  def bitsToFloatType(bits: Int) = bits match {
    case n: Int if n <= 32 => "float"
    case _ => "double"
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialBit" => "bool"
    case "Signed" => ""
    case "Unsign" => "u"
    case "FixedPoint" => remap(m.typeArguments(0)) + "int" + bitsToStringInt(remap(m.typeArguments(1)).toInt + remap(m.typeArguments(2)).toInt) + "_t"
    case "FloatPoint" => bitsToFloatType(remap(m.typeArguments(0)).toInt + remap(m.typeArguments(1)).toInt)
    case bx(n) => n
    case _ => super.remap(m)
  }
}

trait MaxJGenExternPrimitiveOps extends MaxJGenEffect {
  val IR:UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform

  import IR.{infix_until => _, looprange_until => _, println => _, _}

  var emitted_consts: Set[(Exp[Any], Def[Any])] = Set.empty
  var emitted_argins: Set[(Exp[Any], String)] = Set.empty
  var emitted_reglibreads: Set[(Exp[Any], String)] = Set.empty
  def addEmittedConsts(xs: Exp[Any]*) = xs.foreach {
    case lhs@Def(rhs) => if (!emitted_consts.contains((lhs, rhs))) { emitted_consts += ((lhs, rhs)) }
    case _ =>
  }


	var traversals: List[Traversal{val IR: MaxJGenExternPrimitiveOps.this.IR.type}] = Nil

  lazy val preCodegen = new MaxJPreCodegen {
    val IR: MaxJGenExternPrimitiveOps.this.IR.type = MaxJGenExternPrimitiveOps.this.IR
  }

  override def initializeGenerator(bd:String): Unit = {
    preCodegen.buildDir = bd
		traversals = IR.traversals
    super.initializeGenerator(bd)
  }


  def runTraversals[A:Manifest](b: Block[A]): Block[A] = {
    println("MaxJCodegen: applying transformations")
    var curBlock = b
    println("Traversals:\n\t" + traversals.map(_.name).mkString("\n\t"))

    for (t <- traversals) {
      printlog("  Block before transformation: " + curBlock)
      curBlock = t.run(curBlock)
      printlog("  Block after transformation: " + curBlock)
    }
    println("MaxJGodegen: done transforming")
    (curBlock)
  }

  override def preProcess[A: Manifest](body: Block[A]) = {
    preCodegen.run(body)
    super.preProcess(body)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Min2(a, b) =>
      emit(s"""DFEVar ${quote(sym)} = KernelMath.min(${quote(a)}, ${quote(b)});""")
    case Max2(a, b) =>
      emit(s"""DFEVar ${quote(sym)} = KernelMath.max(${quote(a)}, ${quote(b)});""")
    case ConstFixPt(x,_,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }
    case ConstFltPt(x,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }

    case FixPt_Add(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          if (!isReduceResult(sym)) {
            emit(s"""$pre ${quote(sym)} = ${quote(a)} + ${quote(b)};""")
          } else {
            emit(s"""$pre ${quote(sym)} = ${quote(a)}; // ignore ${quote(b)} b/c accumulator""")
          }
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Add(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          if (!isReduceResult(sym)) {
            emit(s"""$pre ${quote(sym)} = ${quote(a)} + ${quote(b)};""")
          } else {
            emit(s"""$pre ${quote(sym)} = ${quote(a)}; // ignore ${quote(b)} b/c accumulator""")
          }
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Div(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Div(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Mul(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Mul(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Lt(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} < ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Leq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} <= ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Neq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} !== {quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Eql(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} === ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_And(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Or(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Lsh(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} << ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Rsh(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} >> ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Lt(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} < ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Leq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} <= ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Neq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} !== ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Eql(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} === ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }


    case Bit_Not(a) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ~( ${quote(a)} );""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_And(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Or(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Xor(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} ^ ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Xnor(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ~ ( ${quote(a)} ^ ${quote(b)} ) ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Mux2(sel,a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(sel)} ? ${quote(a)} : ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Tpes_Int_to_fix(x) =>  // Emit this node in MaxJ only if x is a const
      val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      x match {
        case _:Const[_] | _:Param[_] =>
          withStream(baseStream) {
            emit(s"""var ${quote(sym)} = UInt(${quote(x)})""")
          }
        case _ =>
          withStream(baseStream) {
            emit(s"""// DFEVar $sym = ${quote(x)}.cast($ts)""")
          }
        }

    case _ => super.emitNode(sym, rhs)

  }

  override def emitFileFooter() = {
    emit(s"""// Emit consts""")
    emitted_consts.foreach {
      case ((s, d)) =>
        d match {
          case ConstFixPt(x,_,_,_) =>
            val ts = tpstr(parOf(s)) (s.tp, implicitly[SourceContext])
            withStream(baseStream) {
              emit(s"""DFEVar ${quote(s)} = constant.var( $ts, $x ); """)
            }
          case ConstFltPt(x,_,_) =>
            val ts = tpstr(parOf(s)) (s.tp, implicitly[SourceContext])
            withStream(baseStream) {
              emit(s"""DFEVar ${quote(s)} = constant.var( $ts, $x ); """)
            }
          case _ =>
            withStream(baseStream) {
              emit(s"""// Can't emit ${quote(s)}""")
            }
          }
      case _ =>
        throw new Exception(s"Cannot match, you did something really wrong")
    }
    super.emitFileFooter()
  }

}

trait ChiselGenExternPrimitiveOps extends ChiselGenEffect {
  val IR:UnrollingTransformExp with SpatialExp with MemoryAnalysisExp with DeliteTransform

  import IR.{infix_until => _, looprange_until => _, println => _, _}

  var emitted_consts: Set[(Exp[Any], Def[Any])] = Set.empty
  var emitted_argins: Set[(Exp[Any], String)] = Set.empty
  var emitted_reglibreads: Set[(Exp[Any], String)] = Set.empty
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
      emit(s"""var ${quote(sym)} = KernelMath.min(${quote(a)}, ${quote(b)});""")
    case Max2(a, b) =>
      emit(s"""var ${quote(sym)} = KernelMath.max(${quote(a)}, ${quote(b)});""")
    case ConstFixPt(x,_,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }
    case ConstFltPt(x,_,_) =>
      if (!emitted_consts.contains((sym, rhs))) {
        emitted_consts += ((sym, rhs))
      }

    case FixPt_Add(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          if (!isReduceResult(sym)) {
            emit(s"""$pre ${quote(sym)} = ${quote(a)} + ${quote(b)};""")
          } else {
            emit(s"""$pre ${quote(sym)} = ${quote(a)}; // ignore ${quote(b)} b/c accumulator""")
          }
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Add(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          if (!isReduceResult(sym)) {
            emit(s"""$pre ${quote(sym)} = ${quote(a)} + ${quote(b)};""")
          } else {
            emit(s"""$pre ${quote(sym)} = ${quote(a)}; // ignore ${quote(b)} b/c accumulator""")
          }
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Div(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Div(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} / ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Mul(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FltPt_Mul(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} * ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in ${quote(m)};""")
      }

    case FixPt_Lt(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} < ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Leq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} <= ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Neq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} !== {quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Eql(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} === ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_And(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Or(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Lsh(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} << ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FixPt_Rsh(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} >> ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Lt(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} < ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Leq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} <= ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Neq(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} !== ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case FltPt_Eql(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);""")
          emit(s"""${quote(sym)} <== ${quote(a)} === ${quote(b)};""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }


    case Bit_Not(a) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ~( ${quote(a)} );""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_And(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} & ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Or(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} | ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Xor(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(a)} ^ ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Bit_Xnor(a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ~ ( ${quote(a)} ^ ${quote(b)} ) ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Mux2(sel,a,b) =>
      val pre = maxJPre(sym)
      rTreeMap(sym) match {
        case Nil =>
          emit(s"""$pre ${quote(sym)} = ${quote(sel)} ? ${quote(a)} : ${quote(b)} ;""")
        case m =>
          emit(s"""// ${quote(sym)} already emitted in $m""")
      }

    case Tpes_Int_to_fix(x) =>  // Emit this node in MaxJ only if x is a const
      val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
      x match {
        case _:Const[_] | _:Param[_] =>
          withStream(baseStream) {
            emit(s"""var ${quote(sym)} = UInt(${quote(x)})""")
          }
        case _ =>
          withStream(baseStream) {
            emit(s"""// var $sym = ${quote(x)}.cast($ts)""")
          }
        }

    case _ => super.emitNode(sym, rhs)

  }

  override def emitFileFooter() = {
    emit(s"""// Emit consts""")
    emitted_consts.foreach {
      case ((s, d)) =>
        d match {
          case ConstFixPt(x,_,_,_) =>
            val ts = tpstr(parOf(s)) (s.tp, implicitly[SourceContext])
            withStream(baseStream) {
              emit(s"""var ${quote(s)} = constant.var( $ts, $x ); """)
            }
          case ConstFltPt(x,_,_) =>
            val ts = tpstr(parOf(s)) (s.tp, implicitly[SourceContext])
            withStream(baseStream) {
              emit(s"""var ${quote(s)} = constant.var( $ts, $x ); """)
            }
          case _ =>
            withStream(baseStream) {
              emit(s"""// Can't emit ${quote(s)}""")
            }
          }
      case _ =>
        throw new Exception(s"Cannot match, you did something really wrong")
    }
    super.emitFileFooter()
  }

}


// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenExternPrimitiveOps extends ScalaGenEffect {
  val IR: ExternPrimitiveOpsExp with SpatialIdentifiers
  import IR._

  override def emitDataStructures(path: String) {
    new File(path + deviceTarget).mkdirs()
    val stream = new PrintWriter(new FileWriter(path + deviceTarget + "NumericEmulation.scala"))
    withStream(stream){ emitFileHeader() }
    stream.println(emul)
    stream.close()

    super.emitDataStructures(path)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Min2(x,y) =>
      stream.println(s"val ${quote(sym)} = if (${quote(x)} < ${quote(y)}) ${quote(x)} else ${quote(y)}")
    case Max2(x,y) =>
      stream.println(s"val ${quote(sym)} = if (${quote(x)} > ${quote(y)}) ${quote(x)} else ${quote(y)}")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialBit" => "Boolean"
    case "Signed" => "Signed"
    case "Unsign" => "Unsign"
    case "FixedPoint" => "FixedPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case "FloatPoint" => "FloatPoint[" + m.typeArguments.map(s=> remap(s)).mkString(",") + "]"
    case bx(n) => "B"+n
    case _ => super.remap(m)
  }



// HACK: Have this entire template as a string right now...
  val emul = s"""
import scala.math.BigDecimal.RoundingMode
import scala.reflect.Manifest

trait Signed
trait Unsign

trait B0;  trait B1;  trait B2;  trait B3;  trait B4;  trait B5;  trait B6;  trait B7
trait B8;  trait B9;  trait B10; trait B11; trait B12; trait B13; trait B14; trait B15
trait B16; trait B17; trait B18; trait B19; trait B20; trait B21; trait B22; trait B23
trait B24; trait B25; trait B26; trait B27; trait B28; trait B29; trait B30; trait B31
trait B32; trait B33; trait B34; trait B35; trait B36; trait B37; trait B38; trait B39
trait B40; trait B41; trait B42; trait B43; trait B44; trait B45; trait B46; trait B47
trait B48; trait B49; trait B50; trait B51; trait B52; trait B53; trait B54; trait B55
trait B56; trait B57; trait B58; trait B59; trait B60; trait B61; trait B62; trait B63
trait B64

object NumEmul {
  def sign[T:Manifest] = manifest[T] match {
    case mT if mT == manifest[Signed] => true
    case mT if mT == manifest[Unsign] => false
    case mT => throw new Exception("Unknown type in sign: " + mT.runtimeClass.getSimpleName)
  }

  private lazy val bx = "B([0-9]+)".r
  object BXX {
    def unapply[T](x: Manifest[T]): Option[Int] = x.runtimeClass.getSimpleName match {
      case bx(bits) => Some(bits.toInt)
      case _ => None
    }
  }

  def nbits[T:Manifest] = manifest[T] match {
    case BXX(bits) => bits
    case mT => throw new Exception("Unknown type in nbits: " + mT.runtimeClass.getSimpleName)
  }

  def check(a: FixFormat, b: FixFormat) {
    if (a != b) throw new Exception("Operations on mismatched fixed point representations (" + a + " versus " + b + ") are unsupported")
  }
  def check(a: FloatFormat, b: FloatFormat) {
    if (a != b) throw new Exception("Operations on mismatched floating point representations are unsupported")
  }
}

case class FixFormat(signed: Boolean, m: Int, f: Int) {
  def bits = m + f
  lazy val maxValue = if (signed) (BigInt(1) << (bits-1)) - 1 else (BigInt(1) << bits) - 1
  lazy val minValue = if (signed) -(BigInt(1) << (bits-1))    else BigInt(0)
}

case class FloatFormat(s: Int, e: Int) {
  def bits = s + e
  lazy val maxValue = 0 //TODO
  lazy val minValue = 1 //TODO
}

case class FixedPointRange[S:Manifest,I:Manifest,F:Manifest](start: FixedPoint[S,I,F], end: FixedPoint[S,I,F], step: FixedPoint[S,I,F], par: Int) {
  private val parStep = FixedPoint[S,I,F](par)
  private val fullStep = parStep * step
  private val vecOffsets = Array.tabulate(par){p => FixedPoint[S,I,F](p) * step}

  def foreach(func: (Array[FixedPoint[S,I,F]], Array[Boolean]) => Unit) = {
    var i = start
    while (i < end) {
      val vec = vecOffsets.map{ofs => ofs + i} // Create current vector
      val valids = vec.map{ix => ix < end}     // Valid bits
      func(vec, valids)
      i += fullStep
    }
  }
  def by(s: FixedPoint[S,I,F]) = FixedPointRange[S,I,F](start, end, s, 1)
  def par(p: Int) = FixedPointRange[S,I,F](start, end, step, p)
}

// Defines class for emulating arbitrary fixed point
// Note that all computation is boxed here and done with BigInt for generality. Probably not the best performance
class FixedPoint[S:Manifest,I:Manifest,F:Manifest](private val v: BigInt) {
  import NumEmul._

  private lazy val rep = FixFormat(sign[S],nbits[I],nbits[F])

  def unary_-() = { FixedPoint[S,I,F]( -this.v ) }
  def +(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v + that.v) }
  def -(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v - that.v) }
  def *(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F]( (this.v * that.v) >> rep.f) }
  def /(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F]( (this.v << rep.f) / that.v ) }
  def %(that: FixedPoint[S,I,F]) = {
    if (nbits[F] > 0) throw new Exception("Modulus on non-integer fixed point values currently unsupported")
    FixedPoint[S,I,F]( this.v % that.v )
  }
  def <(that: FixedPoint[S,I,F]) = { this.v < that.v }
  def >(that: FixedPoint[S,I,F]) = { this.v > that.v }
  def <=(that: FixedPoint[S,I,F]) = { this.v <= that.v }
  def >=(that: FixedPoint[S,I,F]) = { this.v >= that.v }
  override def equals(that: Any) = that match {
    case that: FixedPoint[_,_,_] =>
      check(this.rep, that.rep)
      this.v == that.v
    case _ => false
  }
  def &(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v & that.v) }
  def |(that: FixedPoint[S,I,F]) = { FixedPoint[S,I,F](this.v | that.v) }

  def <<[F2:Manifest](that: FixedPoint[S,I,F2]) = {
    if (nbits[F2] > 0) throw new Exception("Cannot shift left by a fractional amount")
    FixedPoint[S,I,F](this.v << that.v.intValue)
  }
  def >>[F2:Manifest](that: FixedPoint[S,I,F2]) = {
    if (nbits[F2] > 0) throw new Exception("Cannot shift right by a fractional amount")
    FixedPoint[S,I,F](this.v >> that.v.intValue)
  }

  def toInt = {
    if (nbits[F] > 0) {
      throw new Exception("Cannot convert fractional fixed point value (FixedPoint[" +
        manifest[S].runtimeClass.getSimpleName + "," + manifest[I].runtimeClass.getSimpleName + manifest[F].runtimeClass.getSimpleName + "]) to Int")
    }
    v.intValue
  }

  def toFloatPoint[G:Manifest,E:Manifest] = {
    val vv = v.abs
    val value = BigDecimal(vv >> rep.f) + (BigDecimal(vv & ((BigInt(1) << rep.f) - 1)) / BigDecimal(BigInt(1) << rep.f))
    FloatPoint[G,E]((if (v < 0) -value else value))
  }
  def changeFormat[S2:Manifest,I2:Manifest,F2:Manifest] = {
    val rep2 = FixFormat(sign[S2],nbits[I2],nbits[F2])
    if (rep2.f > rep.f)
      FixedPoint[S2,I2,F2](v << (rep2.f - rep.f))
    else
      FixedPoint[S2,I2,F2](v >> (rep.f - rep2.f))
  }

  override def toString() = {
    if (rep.f > 0) {
      val vv = v.abs
      val str = (vv >> rep.f).toString + "." + (BigDecimal(vv & ((BigInt(1) << rep.f) - 1)) / BigDecimal(BigInt(1) << rep.f)).toString.split('.').last
      if (v < 0) "-"+str else str
    }
    else v.toString()
  }

  def until(that: FixedPoint[S,I,F]) = {
    check(this.rep, that.rep)
    FixedPointRange(this, that, FixedPoint[S,I,F](1), 1)
  }
}

object FixedPoint {
  import NumEmul._

  def apply[S:Manifest,I:Manifest,F:Manifest](v: Int): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigInt(v) << nbits[F])
  def apply[S:Manifest,I:Manifest,F:Manifest](v: Long): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigInt(v) << nbits[F])
  def apply[S:Manifest,I:Manifest,F:Manifest](v: Float): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))
  def apply[S:Manifest,I:Manifest,F:Manifest](v: Double): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))

  // TODO: Should support arbitrary rounding here, currently always use default (half even)
  def apply[S:Manifest,I:Manifest,F:Manifest](v: BigDecimal): FixedPoint[S,I,F] = {
    FixedPoint[S,I,F](BigInt((v * (1 << nbits[F])).setScale(0,RoundingMode.HALF_EVEN).toString))
  }
  def apply[S:Manifest,I:Manifest,F:Manifest](v: String): FixedPoint[S,I,F] = FixedPoint[S,I,F](BigDecimal(v))
  def apply[S:Manifest,I:Manifest,F:Manifest](v: BigInt): FixedPoint[S,I,F] = {
    var value = v
    val format = FixFormat(sign[S],nbits[I],nbits[F])
    // Emulate overflow and underflow
    // TODO: Write this using modulus instead
    while (value < format.minValue) value = format.maxValue + (value - format.minValue) + 1
    while (value > format.maxValue) value = format.minValue + (value - format.maxValue) - 1
    new FixedPoint[S,I,F](value)
  }

  def abs[S:Manifest,I:Manifest,F:Manifest](f: FixedPoint[S,I,F]) = FixedPoint[S,I,F](f.v.abs)

  def randbnd[S:Manifest,I:Manifest,F:Manifest](f: FixedPoint[S,I,F]) = {
    FixedPoint[S,I,F](BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong(f.v.longValue)))
  }
  def rand[S:Manifest,I:Manifest,F:Manifest] = {
    FixedPoint[S,I,F](BigInt(java.util.concurrent.ThreadLocalRandom.current().nextLong()))
  }
}


// Defines class for emulating arbitrary floating point
class FloatPoint[G:Manifest,E:Manifest](private val v: BigDecimal) {
  import NumEmul._

  private lazy val rep = FloatFormat(nbits[G],nbits[E])

  def unary_-() = { FloatPoint[G,E]( -this.v ) }
  def +(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v + that.v) }
  def -(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v - that.v) }
  def *(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v * that.v) }
  def /(that: FloatPoint[G,E]) = { FloatPoint[G,E](this.v / that.v) }

  def <(that: FloatPoint[G,E]) = { this.v < that.v }
  def >(that: FloatPoint[G,E]) = { this.v > that.v }
  def <=(that: FloatPoint[G,E]) = { this.v <= that.v }
  def >=(that: FloatPoint[G,E]) = { this.v >= that.v }
  override def equals(that: Any) = that match {
    case that: FloatPoint[_,_] =>
      check(this.rep, that.rep)
      this.v == that.v
    case _ => false
  }

  def toFixedPoint[S:Manifest,I:Manifest,F:Manifest]: FixedPoint[S,I,F] = FixedPoint[S,I,F](v)
  def changeFormat[G2:Manifest,E2:Manifest] = FloatPoint[G2,E2](v)

  override def toString() = v.toString()
}

object FloatPoint {
  def apply[G:Manifest,E:Manifest](v: Int): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
  def apply[G:Manifest,E:Manifest](v: Long): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
  def apply[G:Manifest,E:Manifest](v: Float): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
  def apply[G:Manifest,E:Manifest](v: Double): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))
  def apply[G:Manifest,E:Manifest](v: String): FloatPoint[G,E] = FloatPoint[G,E](BigDecimal(v))

  // TODO: Support overflow/underflow and precision
  def apply[G:Manifest,E:Manifest](v: BigDecimal): FloatPoint[G,E] = {
    new FloatPoint[G,E](v)
  }

  def abs[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](f.v.abs)

  // TODO: Just using double precision right now - no default library implementation of these :(
  def log[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.log(f.v.doubleValue))
  def exp[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.exp(f.v.doubleValue))
  def sqrt[G:Manifest,E:Manifest](f: FloatPoint[G,E]) = FloatPoint[G,E](Math.sqrt(f.v.doubleValue))

  def rand[G:Manifest,E:Manifest] = FloatPoint[G,E](java.util.concurrent.ThreadLocalRandom.current().nextDouble())
}
"""
}




package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import ppl.delite.framework.transform.MultiPassTransformer
import scala.collection.mutable.HashMap

import spatial.compiler._
import spatial.compiler.ops._

import scala.collection.mutable.ArrayBuffer

trait UnitPipeTransformExp extends NodeMetadataOpsExp with LoweredPipeOpsExp { this: SpatialExp => }

/**
 * Inserts unit Pipe wrappers for primitive nodes in outer control nodes, along with registers for communication
 *
 * ASSUMPTION: Dynamic allocations (which can depend on local, dynamic values)
 * cannot be read locally. e.g. a counter which is allocated based on the value of a register
 * read cannot have a "counter read" node
 **/
trait UnitPipeTransformer extends MultiPassTransformer with SpatialTraversalTools {
  val IR: UnitPipeTransformExp with SpatialExp
  import IR.{infix_until => _, _}

  override val name = "Unit Pipe Insertion Transformer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  var scope = 0
  def debugs(x: => Any) { debug(".."*scope + x) }

  override def traverseBlock[A](block: Block[A]): Unit = {
    scope += 1
    super.traverseBlock(block)
    scope -= 1
  }

  class Stage(val isControl: Boolean) {
    val allocs: ArrayBuffer[Stm] = ArrayBuffer.empty
    val nodes: ArrayBuffer[Stm] = ArrayBuffer.empty
    val regReads: ArrayBuffer[Stm] = ArrayBuffer.empty

    def dynamicAllocs: ArrayBuffer[Stm] = allocs.filter{
      case TP(s,d) => isDynamicAllocation(d)
      case _ => false
    }
    def staticAllocs: ArrayBuffer[Stm] = allocs.filter{
      case TP(s,d) => !isDynamicAllocation(d)
      case _ => false
    }

    def allocDeps = allocs.flatMap{case TP(s,d) => (syms(d) ++ readSyms(d)) }.toSet
    def deps = nodes.flatMap{case TP(s,d) => (syms(d) ++ readSyms(d)) }.toSet ++ allocDeps

    def dumpStage(i: Int) {
      if (isControl) debugs(s"$i. Control Stage")
      else           debugs(s"$i. Primitive Stage")
      debugs(s"Allocations: ")
      allocs.foreach{case TP(s,d) => debugs(s"..$s = $d [dynamic: ${isDynamicAllocation(d)}]") }
      debugs(s"Nodes:")
      nodes.foreach{case TP(s,d) => debugs(s"..$s = $d") }
      debugs(s"Register reads:")
      regReads.foreach{case TP(s,d) => debugs(s"..$s = $d") }
    }
  }

  // HACK: Have to get a reset value for inserted registers for writing out of unit pipes
  // TODO: Custom records
  private def zero[T](mT: Manifest[T])(implicit ctx: SourceContext): Exp[T] = (mT match {
    case FixPtType(mS,mI,mF) => canFixPtNum(mS,mI,mF).zero
    case FltPtType(mG,mE)    => canFltPtNum(mG,mE).zero
    case mT if isBitType(mT) => canBitNum.zero
    case TupNType(2,List(mA,mB)) => pack((zero(mA),zero(mB)))
    case TupNType(3,List(mA,mB,mC)) => pack((zero(mA),zero(mB),zero(mC)))
    case TupNType(4,List(mA,mB,mC,mD)) => pack((zero(mA),zero(mB),zero(mC),zero(mD)))
    case TupNType(5,List(mA,mB,mC,mD,mE)) => pack((zero(mA),zero(mB),zero(mC),zero(mD),zero(mE)))
    case TupNType(6,List(mA,mB,mC,mD,mE,mF)) => pack((zero(mA),zero(mB),zero(mC),zero(mD),zero(mE),zero(mF)))
    case TupNType(7,List(mA,mB,mC,mD,mE,mF,mG)) => pack((zero(mA),zero(mB),zero(mC),zero(mD),zero(mE),zero(mF),zero(mG)))
    case TupNType(8,List(mA,mB,mC,mD,mE,mF,mG,mH)) => pack((zero(mA),zero(mB),zero(mC),zero(mD),zero(mE),zero(mF),zero(mG),zero(mH)))
    case TupNType(9,List(mA,mB,mC,mD,mE,mF,mG,mH,mI)) => pack((zero(mA),zero(mB),zero(mC),zero(mD),zero(mE),zero(mF),zero(mG),zero(mH),zero(mI)))
    case _ => throw UnknownZeroException(mT)
  }).asInstanceOf[Exp[T]]

  def wrapPrimitives[T:Manifest](blk: Block[T])(implicit ctx: SourceContext): Block[T] = {
    scope += 1

    val blk2 = reifyBlock {
      focusBlock(blk){
        focusExactScope(blk){ stms =>

          // Imperative version (functional version caused ugly scalac crash :( )
          val stages: ArrayBuffer[Stage] = ArrayBuffer.empty
          def curStage = stages.last
          stages += new Stage(true)

          stms foreach { case stm@TP(s,d) =>
            if (isPrimitiveNode(s)) {
              if (curStage.isControl) stages += new Stage(false)
              curStage.nodes += stm
            }
            else if (isRegisterRead(s)) {
              if (!curStage.isControl) curStage.nodes += stm
              curStage.regReads += stm
            }
            else if (isAllocation(s) || isConstant(s) || isGlobal(s)) {
              if (isDynamicAllocation(s) && !curStage.isControl) curStage.nodes += stm
              else curStage.allocs += stm
            }
            else {
              stages += new Stage(true)
              curStage.nodes += stm
            }
          }
          val deps = stages.toList.map{stage => stage.deps }

          if (debugMode) {
            stages.zipWithIndex.foreach{case (stage,i) => stage.dumpStage(i) }
            debug("")
          }

          stages.zipWithIndex.foreach{
            case (stage,i) if !stage.isControl =>
              val calculatedSyms = stage.nodes.map{case TP(s,d) => s}
              val neededSyms = deps.drop(i+1).flatten
              // Determine which symbols escape (can be empty)
              val escapingSyms = calculatedSyms.filter(sym => neededSyms.contains(sym) && !isRegisterRead(sym))
              val (escapingUnits, escapingValues) = escapingSyms.partition{sym => sym.tp == manifest[Unit]}

              debugs("Escaping symbols: " + escapingValues.mkString(", "))
              // Create registers for escaping symbols
              val regs = escapingValues.map{sym =>
                val reg = reg_create(zero(sym.tp)(mpos(sym.pos)))(sym.tp, mpos(sym.pos))
                debugs(s"Created new register $reg for escaping primitive $sym")
                reg
              }
              stage.staticAllocs.foreach{stm => traverseStm(stm) }

              val primBlk = reifyBlock {
                stage.nodes.foreach{stm => traverseStm(stm) } // Mirror each statement

                // Write all escaping symbols to newly created registers
                escapingValues.zip(regs).foreach{case (sym,reg) => reg_write(reg, f(sym), true)(sym.tp, mpos(sym.pos)) }
                ()
              }
              val pipe = reflectEffect(Unit_pipe(primBlk)(ctx), summarizeEffects(primBlk) andAlso Simple())
              styleOf(pipe) = InnerPipe

              // Replace substitutions of original symbol with register reads
              escapingValues.zip(regs).foreach{case (sym,reg) =>
                register(sym -> reg_read(reg)(sym.tp,mpos(sym.pos)))
              }
              // Replace all dependencies on effectful (Unit) symbols with dependencies on newly created Pipe
              escapingUnits.foreach{sym => register(sym -> pipe) }

              // External register reads (outside unit pipe)
              // May cause extra, unused reads (eliminated later)
              stage.regReads.foreach{case TP(s,EatReflect(Reg_read(reg))) =>
                register(s -> reg_read(f(reg))(s.tp,mpos(s.pos)) )
              }

              stage.dynamicAllocs.foreach{stm => traverseStm(stm) }

            case (stage,i) =>
              stage.nodes.foreach{stm => traverseStm(stm) }
              stage.staticAllocs.foreach{stm => traverseStm(stm) }
              stage.regReads.foreach{stm => traverseStm(stm) }
              stage.dynamicAllocs.foreach{stm => traverseStm(stm) }
          }
        }
      }
      f(getBlockResult(blk))
    }
    scope -= 1

    blk2
  }

  def wrapPrimitives_Hwblock(lhs: Sym[Any], rhs: Hwblock)(implicit ctx: SourceContext) = {
    val Hwblock(blk) = rhs
    debugs(s"$lhs = $rhs")
    val wrappedBlk = wrapPrimitives(blk)
    val pipe2 = reflectEffect(Hwblock(wrappedBlk)(ctx), summarizeEffects(blk) andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }
  def wrapPrimitives_UnitPipe(lhs: Sym[Any], rhs: Unit_pipe)(implicit ctx: SourceContext) = {
    val Unit_pipe(func) = rhs
    debugs(s"$lhs = $rhs")
    val wrappedBlk = wrapPrimitives(func)
    val pipe2 = reflectEffect(Unit_pipe(wrappedBlk)(ctx), summarizeEffects(wrappedBlk) andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_PipeForeach(lhs: Sym[Any], rhs: Pipe_foreach)(implicit ctx: SourceContext) = {
    val Pipe_foreach(cchain, func, inds) = rhs
    debugs(s"$lhs = $rhs")
    val wrappedBlk = wrapPrimitives(func)
    val pipe2 = reflectEffect(Pipe_foreach(f(cchain), wrappedBlk, inds)(ctx), summarizeEffects(wrappedBlk).star andAlso Simple())
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_PipeFold[T,C[T]](lhs: Sym[Any], rhs: Pipe_fold[T,C])(implicit ctx: SourceContext, memC: Mem[T,C], numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    val Pipe_fold(cchain,accum,zero,fA,iFunc,ld,st,func,rFunc,inds,idx,acc,res,rV) = rhs
    val accum2 = f(accum)
    debugs(s"$lhs = $rhs")
    val mBlk = wrapPrimitives(func)
    val iBlk = f(iFunc)
    val ldBlk = f(ld)
    val stBlk = f(st)
    val rBlk = f(rFunc)

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(mBlk) andAlso summarizeEffects(ldBlk) andAlso
                  summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum2.asInstanceOf[Sym[C[T]]]))

    val pipe2 = reflectEffect(Pipe_fold(f(cchain),accum2,f(zero),fA,iBlk,ldBlk,stBlk,mBlk,rBlk,inds,idx,acc,res,rV)(ctx,memC,numT,mT,mC), effects.star)
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  def wrapPrimitives_AccumFold[T,C[T]](lhs: Sym[Any], rhs: Accum_fold[T,C])(implicit ctx: SourceContext, memC: Mem[T,C], numT: Num[T], mT: Manifest[T], mC: Manifest[C[T]]) = {
    val Accum_fold(ccOuter,ccInner,accum,zero,fA,iFunc,func,ld1,ld2,rFunc,st,indsOuter,indsInner,idx,part,acc,res,rV) = rhs
    val accum2 = f(accum)
    val iBlk = f(iFunc)
    debugs(s"$lhs = $rhs")
    val mBlk = wrapPrimitives(func)
    val ldPartBlk = f(ld1)
    val ldBlk = f(ld2)
    val rBlk = f(rFunc)
    val stBlk = f(st)

    val effects = summarizeEffects(iBlk) andAlso summarizeEffects(mBlk) andAlso summarizeEffects(ldPartBlk) andAlso
                  summarizeEffects(ldBlk) andAlso summarizeEffects(rBlk) andAlso summarizeEffects(stBlk) andAlso Write(List(accum2.asInstanceOf[Sym[C[T]]]))

    val pipe2 = reflectEffect(Accum_fold(f(ccOuter),f(ccInner),accum2,f(zero),fA,iBlk,mBlk,ldPartBlk,ldBlk,rBlk,stBlk,indsOuter,indsInner,idx,part,acc,res,rV)(ctx,memC,numT,mT,mC), effects.star)
    setProps(pipe2, getProps(lhs))
    pipe2
  }

  override def self_mirror[A](lhs: Sym[A], rhs: Def[A]): Exp[A] = {
    debugs(s"Mirroring $lhs = $rhs")
    val lhs2 = super.self_mirror(lhs, rhs)
    val rhs2 = lhs2 match {case Def(d) => d; case _ => null}
    debugs(s"Created $lhs2 = $rhs2")
    lhs2
  }

  override def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext) = rhs match {
    case EatReflect(Pipe_parallel(func)) => None

    case EatReflect(e: Hwblock) if isOuterControl(lhs) => Some(wrapPrimitives_Hwblock(lhs, e)(e.__pos))
    case EatReflect(e: Unit_pipe) if isOuterControl(lhs) => Some(wrapPrimitives_UnitPipe(lhs, e)(e.__pos))
    case EatReflect(e: Pipe_foreach) if isOuterControl(lhs) => Some(wrapPrimitives_PipeForeach(lhs, e)(e.ctx))
    case EatReflect(e: Pipe_fold[_,_]) if isOuterControl(lhs) => Some(wrapPrimitives_PipeFold(lhs, e)(e.ctx, e.memC, e.numT, e.mT, e.mC))
    case EatReflect(e: Accum_fold[_,_]) if isOuterControl(lhs) => Some(wrapPrimitives_AccumFold(lhs, e)(e.ctx, e.memC, e.numT, e.mT, e.mC))
    case _ => None
  }
}

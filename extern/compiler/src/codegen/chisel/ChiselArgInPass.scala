package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{ChiselCodegen}
import scala.virtualization.lms.internal.{Expressions, Traversal}
import ppl.delite.framework.transform.{DeliteTransform}
import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.HashMap

import ppl.delite.framework.DeliteApplication

trait ChiselArgInPass extends Traversal  {
  val IR: UnrolledOpsExp with ControllerOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with DRAMOpsExp with RegOpsExp with ExternCounterOpsExp
          with ExternPrimitiveOpsExp with SpatialCodegenOps with NosynthOpsExp with DeliteTransform
	import IR._

  override val name = "ChiselArgInPass"

	val expToArg = HashMap[Exp[Any],Exp[Reg[Any]]]()
	val argToExp = HashMap[Exp[Reg[Any]], Exp[Any]]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
		expToArg.clear
    argToExp.clear
		b
	}
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
		b
	}

  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
			traverseNode(sym,rhs)
			super.traverseStm(stm)
		}
    case _ => super.traverseStm(stm)
	}

  def traverseNode(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
    case n@Set_arg(reg, value) =>
      expToArg += value -> reg
      argToExp += reg -> value
    case n@Reflect(d,_,_) =>
      traverseNode(sym, d)
    case _ =>
	}
}

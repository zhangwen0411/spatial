package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait OffChipAnalyzer extends Traversal {
  val IR: SpatialExp
  import IR._

  override val name = "OffChip Analyzer"
  override val eatReflect = true   // Ignore reflect wrappers
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  var softValue: Map[Exp[Reg[Any]], Exp[Any]] = Map.empty
  var offchips: Set[Exp[DRAM[Any]]] = Set.empty

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = rhs match {
    case Set_arg(reg, value) => softValue += reg -> value
    case Dram_new(_) => offchips += lhs.asInstanceOf[Exp[DRAM[Any]]]
    case _ => super.traverse(lhs, rhs)
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    debug("Set input arguments: ")
    for ((reg,value) <- softValue) {
      debug(s"  $reg: $value")
    }
    debug("DRAMs: ")
    for (offchip <- offchips) {
      debug(s"  $offchip")
      val dims = dimsOf(offchip)
      dims.foreach{
        case dim@Def(rhs) => debug(s"    $dim = $rhs")
        case dim => debug(s"    $dim")
      }
    }

    offchips.foreach{offchip =>
      val softDims = dimsOf(offchip).zipWithIndex.map{case (dim,i) => dim match {
        case Deff(Reg_read(reg)) if isArgIn(reg) && softValue.contains(reg) => softValue(reg)
        case dim@Exact(c) => dim
        case _ => throw InvalidOffChipDimensionException(offchip, i)

      }}
      softDimsOf(offchip) = softDims.asInstanceOf[List[Exp[Index]]]
    }
    super.postprocess(b)
  }

}

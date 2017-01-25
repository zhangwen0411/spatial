package spatial.compiler.ops

import scala.virtualization.lms.common.{BaseExp, ScalaGenEffect, CGenEffect, MaxJGenEffect, ChiselGenEffect}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import scala.collection.mutable.Set
import java.io.{File, FileWriter, PrintWriter}
import ppl.delite.framework.transform.{DeliteTransform}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait ChiselGenExternCounterOps extends ChiselGenEffect {
  val IR: ExternCounterOpsExp with SpatialMetadataOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialCounter" => "SpatialCounter"
    case "SpatialCounterChain" => "CounterChain"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Counter_new(start,end,step,par) =>


    case e@Counterchain_new(counters) =>
      // See emitMaxJCounterChain() in PipeTemplateOpsExp.scala

    case _ => super.emitNode(sym,rhs)
  }
}

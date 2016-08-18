package spatial.shared.ops

import scala.virtualization.lms.common.Base
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._

trait ExternCounterTypes {
  type Counter
  type CounterChain

  implicit def counterManifest: Manifest[Counter]
  implicit def counterChainManifest: Manifest[CounterChain]

  def isCounter[T:Manifest]: Boolean
  def isCounterChain[T:Manifest]: Boolean
}

trait ExternCounterOps extends ExternCounterTypes with Base {this: Spatial => }
trait ExternCounterCompilerOps extends ExternCounterOps {
  this: Spatial =>

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext): Rep[Counter]
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain]
}

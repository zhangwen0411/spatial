package spatial.library.classes
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.library._
import spatial.library.classes._


trait ExternCounterWrapper {
  this: SpatialBase with SpatialClasses =>

  type Counter = FixedPointRange[Signed,B32,B0]
  type CounterChain = Array[FixedPointRange[Signed,B32,B0]]

  def counterManifest: Manifest[Counter] = manifest[FixedPointRange[Signed,B32,B0]]
  def counterChainManifest: Manifest[CounterChain] = manifest[Array[FixedPointRange[Signed,B32,B0]]]

  def isCounter[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPointRange[_,_,_]])
  def isCounterChain[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Array[FixedPointRange[_,_,_]]])

  def counter_new(start: Rep[FixPt[Signed,B32,B0]],end: Rep[FixPt[Signed,B32,B0]],step: Rep[FixPt[Signed,B32,B0]], par: Rep[Int])(implicit ctx: SourceContext) = {
    start until end by step
  }
  def counterchain_new(counters: List[Rep[Counter]])(implicit ctx: SourceContext): Rep[CounterChain] = {
    counters.toArray.asInstanceOf[Rep[CounterChain]]
  }

}

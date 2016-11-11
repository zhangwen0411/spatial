package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable


trait PIRSplitter extends PIRSplitting with PIRRetiming {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  override val name = "PIR Splitting"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mappingIn  = mutable.HashMap[Symbol, CU]()
  val mappingOut = mutable.HashMap[Symbol, List[CU]]()

  val ComputeMax = SplitCost(vIn=4, vOut=2, vLoc=1, comp=6, write=4, read=4, mems=4)
  val UnitMax    = SplitCost(vIn=4, vOut=2, vLoc=1, comp=6, write=4, read=4, mems=4)


  override def run[A:Manifest](b: Block[A]) = {
    super.run(b)

    val cuMapping = mappingIn.keys.map{k => mappingIn(k).asInstanceOf[ACU] -> mappingOut(k).head.asInstanceOf[ACU] }.toMap
    swapCUs(mappingOut.values.flatten, cuMapping)
    b
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && mappingIn.contains(lhs))
      mappingOut(lhs) = split(mappingIn(lhs))
  }

  def split(cu: CU): List[CU] = {
    if (cu.allStages.nonEmpty) {
      val max = if (cu.isUnit) UnitMax else ComputeMax
      val others = mappingOut.values.flatten
      val cus = splitCU(cu, max, others)
      retime(cus, others)
      cus
    }
    else List(cu)
  }

}
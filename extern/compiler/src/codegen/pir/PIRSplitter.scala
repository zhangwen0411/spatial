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

  lazy val ComputeMax = SplitCost(
    sIn=SpatialConfig.sIn,
    vIn=SpatialConfig.vIn,
    vOut=SpatialConfig.vOut,
    vLoc=1,
    comp=SpatialConfig.comp,
    write=SpatialConfig.readWrite,
    read=SpatialConfig.readWrite,
    mems=SpatialConfig.mems
  )
  STAGES = 10
  SCALARS_PER_BUS = SpatialConfig.sbus

  override def run[A:Manifest](b: Block[A]) = {
    super.run(b)
    try {
      val cuMapping = mappingIn.keys.map{k => mappingIn(k).asInstanceOf[ACU] -> mappingOut(k).head.asInstanceOf[ACU] }.toMap
      swapCUs(mappingOut.values.flatten, cuMapping)
    }
    catch {case e: SplitException =>
      sys.exit(-1)
    }
    b
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && mappingIn.contains(lhs))
      mappingOut(lhs) = split(mappingIn(lhs))
  }

  def split(cu: CU): List[CU] = {
    if (cu.allStages.nonEmpty) {
      val others = mutable.ArrayBuffer[CU]()
      others ++= mappingOut.values.flatten

      val cus = splitCU(cu, ComputeMax, others)
      retime(cus, others)

      cus.foreach{cu =>
        val cost = getStats(cu, others)
        if (cost.mems > ComputeMax.mems)
          throw new Exception(s"${cu.srams} > ${ComputeMax.mems}, exceeded maximum SRAMs after retiming")

        others += cu
      }

      cus
    }
    else List(cu)
  }

}
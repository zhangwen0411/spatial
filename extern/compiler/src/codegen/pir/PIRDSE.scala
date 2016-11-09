package spatial.compiler.ops
import spatial.compiler._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import ppl.delite.framework.Config
import java.io.PrintStream

trait PIRDSE extends PIRSplitting with PIRRetiming {
  val IR: SpatialExp with PIRCommonExp
  import IR._

  override val name = "Plasticine DSE"
  override val recurse = Always
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  val mappingIn = mutable.HashMap[Symbol, CU]()

  val cus = ArrayBuffer[CU]()

  override def run[A:Manifest](b: Block[A]) = {
    super.run(b)
    dse()
    b
  }
  override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
    if (isControlNode(lhs) && mappingIn.contains(lhs))
      cus += mappingIn(lhs)
  }

  def dse() {
    debug(s"Running design space exploration")
    this.silence()

    val pwd = sys.env("HYPER_HOME")
    val dir = s"${pwd}/data"
    val name = Config.degFilename.replace(".deg","")
    val valid = new PrintStream(s"$dir/${name}.csv")
    val invalid = new PrintStream(s"$dir/${name}_invalid.csv")

    val header = SplitStats()
    valid.println("VIns, Vouts, Compute, Read/Write, SRAMs," + header.heading + ", #ALU,#SRAM,#Vin,#Vout, ALU Util, SRAM Util, VecIn Util, VecOut Util")
    invalid.println("VIns, Vouts, Compute, Read/Write, Mems")

    for (vIns <- 2 to 6) { //2 to 6) {
    for (vOuts <- 1 to 3) {
    for (readWrite <- 1 to 10) {
    for (comps <- 0 to (10-readWrite)) {
    for (mmems <- 1 to vIns) {
      System.out.print(s"vIn=$vIns, vOut=$vOuts, comps=$comps, read/write=$readWrite, mems=$mmems: ")
      var others = ArrayBuffer[CU]()
      val pipe = SplitCost(vIn=vIns, vOut=vOuts, vLoc=1, comp=comps, write=readWrite, read=readWrite, mems=mmems)
      val unit = SplitCost(vIn=vIns, vOut=vOuts, vLoc=1, comp=comps, write=readWrite, read=readWrite, mems=mmems)

      try {
        var nPipes = 0
        var nUnits = 0
        var stats = SplitStats()

        for (orig <- cus) {
          val max = if (orig.isUnit) unit else pipe
          val split = splitCU(orig, max, others)
          retime(split, others)

          for (cu <- split) {
            val cost = getStats(cu, others)
            if (cost.mems > max.mems)
              throw new SplitException(s"$orig requires ${cost.mems} > ${max.mems} SRAMs after retiming")

            stats += cost
            if (cu.allStages.nonEmpty) {
              if (cu.isUnit) nUnits += 1 else nPipes += 1
            }
          }
          others ++= split
        }
        val nALUs = LANES * (nPipes + nUnits) * (comps + readWrite)
        val nMems = mmems * (nPipes + nUnits)
        val nVIns = vIns  * (nPipes + nUnits)
        val nVOut = vOuts * (nPipes + nUnits)

        val aluUtil = stats.alus.toFloat / nALUs
        val memUtil = stats.mems.toFloat / nMems
        val vInUtil = stats.mems.toFloat / nVIns
        val vOutUtil = stats.mems.toFloat / nVOut
        System.out.println("PASS")
        valid.println(s"$vIns, $vOuts, $comps, $readWrite, $mmems, " + stats.toString +
                      s",$nALUs,$nMems,$nVIns,$nVOut, $aluUtil, $memUtil, $vInUtil, $vOutUtil")
      }
      catch {case e:SplitException =>
        System.out.println("FAIL")
        //System.out.println(s"Failing stages:")
        System.out.println(e.msg)
        invalid.println(s"$vIns, $vOuts, $comps, $readWrite, $mmems")
      }
    }}}}}
    valid.close()
    invalid.close()

  }

}
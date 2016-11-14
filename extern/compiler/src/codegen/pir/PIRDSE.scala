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
    invalid.println("Scl/Bus, SIns, VIns, Vouts, Compute, Read/Write, Stages, SRAMs")
    valid.println  ("Scl/Bus, SIns, VIns, Vouts, Compute, Read/Write, Stages, SRAMs," + header.heading +
                    ", #ALU,#SRAM,#Vin,#Vout, ALU Util, SRAM Util, VecIn Util, VecOut Util, " +
                    ", SIn/CU, SOut/CU, VIn/CU, VOut/CU, SIn/Stage, VIn/Stage")

    // Total: ~38,000 combinations...
    var pass = 0
    var fail = 0
    var first: String = ""

    for (vIns <- 2 to 6) { // 5
    for (vOuts <- 1 to 3) { // 3
    for (readWrite <- 1 to 6) { // 7
    for (comps <- 0 to (10-readWrite)) { // 5
    for (mmems <- vIns-2 to vIns) { // 3
    for (sbus <- List(1,2,4)) { // 3
    for (sIns <- 2 to Math.min(vIns*sbus,16) by 2) { // 8  --- implies existence of a vIns*sbus : sIns crossbar (or some other selection mechanism)
      val stages = comps + readWrite
      STAGES = stages
      SCALARS_PER_BUS = sbus

      var others = ArrayBuffer[CU]()
      val pipe = SplitCost(sIn= sIns, vIn=vIns, vOut=vOuts, vLoc=1, comp=comps, write=readWrite, read=readWrite, mems=mmems)
      val unit = SplitCost(sIn= sIns, vIn=vIns, vOut=vOuts, vLoc=1, comp=comps, write=readWrite, read=readWrite, mems=mmems)

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
            others += cu
          }
        }
        val nALUs = LANES * (nPipes + nUnits) * (comps + readWrite)
        val nMems = mmems * (nPipes + nUnits)
        val nVIns = vIns  * (nPipes + nUnits)
        val nVOut = vOuts * (nPipes + nUnits)

        val aluUtil = stats.alus.toFloat / nALUs
        val memUtil = stats.mems.toFloat / nMems
        val vInUtil = stats.mems.toFloat / nVIns
        val vOutUtil = stats.mems.toFloat / nVOut

        val avgSIn  = stats.sclIn.toFloat / (nPipes + nUnits)
        val avgSOut = stats.sclOut.toFloat / (nPipes + nUnits)
        val avgVIn  = stats.vecIn.toFloat / (nPipes + nUnits)
        val avgVOut = stats.vecOut.toFloat / (nPipes + nUnits)

        val sInPerStage = stats.sclIn.toFloat / (stats.alus.toFloat / LANES)
        val vInPerStage = stats.vecIn.toFloat / (stats.alus.toFloat / LANES)

        if (pass == 0) {
          first = s"sbus=$sbus, sIn=$sIns, vIn=$vIns, vOut=$vOuts, comps=$comps, read/write=$readWrite, mems=$mmems"
        }
        pass += 1

        System.out.println(s"sbus=$sbus, sIn=$sIns, vIn=$vIns, vOut=$vOuts, comps=$comps, read/write=$readWrite, mems=$mmems: PASS")
        valid.println(s"$sbus, $sIns, $vIns, $vOuts, $comps, $readWrite, $stages, $mmems, " + stats.toString +
                      s",$nALUs,$nMems,$nVIns,$nVOut, $aluUtil, $memUtil, $vInUtil, $vOutUtil, " +
                      s",$avgSIn,$avgSOut,$avgVIn,$avgVOut, $sInPerStage, $vInPerStage")
      }
      catch {case e:SplitException =>
        fail += 1
        System.out.println(s"sbus=$sbus, sIn=$sIns, vIn=$vIns, vOut=$vOuts, comps=$comps, read/write=$readWrite, mems=$mmems: FAIL")
        System.out.println(e.msg)
        invalid.println(s"$sbus, $sIns, $vIns, $vOuts, $comps, $readWrite, $stages, $mmems")
      }
    }}}}}}}
    valid.close()
    invalid.close()

    System.out.println(s"Pass: $pass (${100.0f * pass.toFloat / (pass + fail)})%")
    System.out.println(s"Fail: $fail (${100.0f * fail.toFloat / (pass + fail)})%")
    System.out.println(s"Smallest: $first")
  }

}
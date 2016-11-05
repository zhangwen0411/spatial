package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.internal.{Traversal, QuotingExp}
import scala.collection.mutable.{HashMap,HashSet,Queue,ArrayBuffer}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import ppl.delite.framework.Config
import java.io.PrintStream

trait PIRDSE extends SplittingOps with RetimingOps {
  val IR: SpatialExp with PIRScheduleAnalysisExp
  import IR.{infix_until => _, _}

  override val name = "Plasticine DSE"
  debugMode = SpatialConfig.debugging || SpatialConfig.pirdebug
  verboseMode = SpatialConfig.verbose || SpatialConfig.pirdebug

  override def allocateCU(pipe: Exp[Any]) = cus(pipe).head

  override def run[A:Manifest](b: Block[A]) = {
    msg(s"Starting traversal Plasticine DSE")
    if (SpatialConfig.enableArchDSE) dse()
    b
  }

  def dse() {
    debug(s"Running design space exploration!")
    this.silence()

    val cus = cuMapping.values.toList
    val nCCUs = cus.collect{case cu: BasicComputeUnit if cu.stages.isEmpty => cu}.length
    val nTUs  = cus.collect{case tu: TileTransferUnit => tu}.length
    val computeCUs = cus.collect{case cu: BasicComputeUnit if cu.stages.nonEmpty => cu}

    val baseStats = SplitStats(cus = nTUs + nCCUs, tus = nTUs, ccus = nCCUs)

    val pwd = sys.env("HYPER_HOME")
    val dir = s"${pwd}/data"
    val name = Config.degFilename.replace(".deg","")
    val valid = new PrintStream(s"$dir/${name}.csv")
    val invalid = new PrintStream(s"$dir/${name}_invalid.csv")

    //valid.println("In  , In   , In     , In        , In  , Sum ,      ,    ,     ,    , Average       ,             ,       ,         ,         ,          ")
    valid.println("VIns, Vouts, Compute, Read/Write, SRAMs," + baseStats.heading + ", #ALU,#SRAM,#Vin,#Vout, ALU Util, SRAM Util, VecIn Util, VecOut Util")
    //invalid.println("In  , In   , In     , In        , In  ")
    invalid.println("VIns, Vouts, Compute, Read/Write, Mems")

    for (vIns <- 2 to 6) { //2 to 6) {
      for (vOuts <- 1 to 3) {
        for (readWrite <- 1 to 10) {
          for (comps <- 0 to (10-readWrite)) {
            for (mmems <- 1 to vIns) {
              System.out.print(s"vIn=$vIns, vOut=$vOuts, comps=$comps, read/write=$readWrite, mems=$mmems: ")

              val pipe = SplitCost(aIn=16, vIn=vIns, vOut=vOuts, vLoc=1, comp=comps, write=readWrite, read=readWrite, mems=mmems)
              val unit = SplitCost(aIn=2,  vIn=vIns, vOut=1, vLoc=1, comp=comps, write=readWrite, read=1, mems=mmems)

              try {
                val cuGrps = computeCUs.map{cu =>
                  val max = if (cu.isUnitCompute) unit else pipe
                  splitComputeCU(cu, max)
                }

                var nPipes = 0
                var nUnits = 0
                var stats = baseStats

                //var maxCU: Option[BasicComputeUnit] = None
                //var maxCU_alus = 0

                for (grp <- cuGrps) {
                  retime(grp)

                  val splitCompute = grp.filterNot(_.allStages.isEmpty)

                  for (cu <- splitCompute) {
                    val cost = getStats(cu)

                    val max = if (cu.isUnitCompute) unit else pipe
                    if (cost.mems > max.mems) {
                      throw new SplitException(s"$cu requires > ${max.mems} SRAMs (after retiming)")
                    }

                    stats += cost

                    /*if (cu.isUnitCompute && cost.alus > maxCU_alus) {
                      maxCU_alus = cost.alus
                      maxCU = Some(cu)
                    }*/

                    if (cu.isUnitCompute) nUnits += 1 else nPipes += 1
                  }
                }

                // Conservative estimate: unit CUs also use the resources of a full CU
                // (in that the resources become otherwise unavailable)
                val nALUs = LANES * (nPipes + nUnits) * (comps + readWrite)
                val nMems = mmems * (nPipes + nUnits)
                val nVIns = vIns  * (nPipes + nUnits)
                val nVOut = vOuts * (nPipes + nUnits)

                val aluUtil = stats.alus.toFloat / nALUs
                val memUtil = stats.mems.toFloat / nMems
                val vInUtil = stats.vecIn.toFloat / nVIns
                val vOutUtil = stats.vecOut.toFloat / nVOut

                val aluCap = (comps + readWrite)

                System.out.println(s"OK")
                /*if (maxCU.isDefined) {
                  val cu = maxCU.get
                  System.out.println(s"$cu")
                  System.out.println(s"  Write stages:")
                  for ((mems,stages) <- cu.writeStages) {
                    stages.foreach{stage => System.out.println(s"      $stage")}
                  }
                  System.out.println(s"  Compute stages: ")
                  cu.stages.foreach{stage => System.out.println(s"    $stage") }
                }*/

                valid.println(s"$vIns, $vOuts, $comps, $readWrite, $mmems, " + stats.toString + s",$nALUs,$nMems,$nVIns,$nVOut, $aluUtil, $memUtil, $vInUtil, $vOutUtil")
              }

              catch {case e: SplitException =>
                System.out.println(s"FAIL")
                System.out.println(s"Failing stages:")
                System.out.println(e.msg)
                invalid.println(s"$vIns, $vOuts, $comps, $readWrite, $mmems")
              }
            }
          }
        }
      }
    }
    valid.close()
    invalid.close()
  }
}
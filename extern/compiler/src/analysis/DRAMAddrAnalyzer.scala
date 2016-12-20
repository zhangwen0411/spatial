package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

object FPGAParameters { 
  val burstSize = 384
}

trait DRAMAddrAnalysisExp extends ControlSignalAnalysisExp with MemoryAnalysisExp {

  this: SpatialExp => 


  case class DRAMAddresses(insts: Long) extends Metadata
  object dramAddr {
    def update(e: Exp[Any], a: Long) { setMetadata(e, DRAMAddresses(a)) }
    def apply(e: Exp[Any]) = meta[DRAMAddresses](e).map(_.insts).getOrElse(0)
  }

}

trait DRAMAddrAnalyzer extends Traversal with ControllerTools {
  val IR: SpatialExp with DRAMAddrAnalysisExp
  import IR._
  import FPGAParameters._

  override val name = "DRAMAddr Analyzer"
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each DRAM memory a 384MB chunk now
  var nextLMemAddr: Long = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr
  }

  def run(memStreams: List[Exp[Any]]): Unit = memStreams.foreach{mem =>
    dramAddr(mem) = getNextLMemAddr()
  }

  val memStreams = List[Sym[Any]]()
  def preGenNodes(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
    case e@BurstStore(mem,_,_,_,_) =>
      memStreams = memStreams :+ mem
    case e@BurstLoad(mem,_,_,_,_) => 
      memStreams = memStreams :+ mem
    case e@Convolve(img,kernel,output,_,_,_,_,_) => 
      memStreams = memStreams :+ img
      memStreams = memStreams :+ kernel
      memStreams = memStreams :+ output
    case e@Scatter(mem,_,_,_,_,_) =>
      memStreams = memStreams :+ mem
    case e@Gather(mem,_,_,_,_,_) =>
      memStreams = memStreams :+ mem
    case Reflect(s, u, effects) =>
     preGenNodes(sym, s)
    // case Reify(s, u, effects) =>
    case _ => {
    }
  }
  
  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
      // Console.println(s"A${sym}");
      preGenNodes(sym,rhs)
      super.traverseStm(stm)
    }
    case _ =>
      super.traverseStm(stm)
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    // Console.println(s"B");
    super.runOnce(b)
    run(memStreams)
    b
  }
}

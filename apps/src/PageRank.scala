import spatial.compiler._
import spatial.library._
import spatial.shared._

object PageRank extends SpatialAppCompiler with PageRankApp
trait PageRankApp extends SpatialApp {
  type Elem = Flt //FixPt[Signed, B16, B16]
  type T = Flt
  type Array[T] = ForgeArray[T]

  val edges_per_page = 6 // Will make this random later



  def pagerank(INpages: Rep[Array[SInt]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INedgeId: Rep[Array[SInt]], INedgeLen: Rep[Array[SInt]], OCiters: Rep[SInt], OCdamp: Rep[SInt]) = {

    val np = 96
    val NE = 9216
    val tileSize = 96 // For now
    val iters = ArgIn[SInt]
    val damp  = ArgIn[SInt]
    val NP    = ArgIn[SInt]
    val OCpages    = DRAM[SInt](np)
    val OCedges    = DRAM[SInt](NE)    // srcs of edges
    val OCcounts   = DRAM[SInt](NE)    // counts for each edge
    val OCedgeId = DRAM[SInt](np) // Start index of edges
    val OCedgeLen = DRAM[SInt](np) // Number of edges for each page
    val OCresult   = DRAM[SInt](np)

    setArg(iters, OCiters)
    setArg(damp, OCdamp)
    setArg(NP, np)
    setMem(OCpages, INpages)
    setMem(OCedges, INedges)
    setMem(OCcounts, INcounts)
    setMem(OCedgeId, INedgeId)
    setMem(OCedgeLen, INedgeLen)

    Accel {
      Sequential(iters by 1){ iter =>
        // val oldPrIdx = iter % 2.as[SInt]
        // val newPrIdx = mux(oldPrIdx == 1, 0.as[SInt], 1.as[SInt])
        Sequential(NP by tileSize) { tid =>
          val currentPR = SRAM[SInt](tileSize)
          val edgesId = SRAM[SInt](tileSize)
          val edgesLen = SRAM[SInt](tileSize)
          Pipe {currentPR := OCpages(tid::tid+tileSize) }
          Pipe {edgesId := OCedgeId(tid :: tid+tileSize) }
          Pipe {edgesLen := OCedgeLen(tid :: tid+tileSize) }
          Sequential(tileSize by 1) { pid =>
            val startId = edgesId(pid)
            val numEdges = edgesLen(pid)

            // Gather edges indices and counts
            val edges = SRAM[SInt](tileSize)
            val counts = SRAM[SInt](tileSize)
            Pipe {edges := OCedges(startId::startId+numEdges) }
            Pipe {counts := OCcounts(startId::startId+numEdges) }

            // Gather pages based on edges
            val gatheredPR = SRAM[SInt](tileSize)
            Pipe {gatheredPR := OCpages(edges, edges_per_page)}

            // Compute new PR
            val pr = Reduce(edges_per_page by 1)(0.as[SInt]){ i => gatheredPR(i) / counts(i) }{_+_}

            // Update PR
            currentPR(pid) = pr.value * damp + (1.as[SInt] - damp)
          }
          OCresult(tid::tid+tileSize) := currentPR
        }
      }
    }
    getMem(OCresult)
  }

  def printArr(a: Rep[Array[SInt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val damp = args(1).to[SInt]
    val NP = 96
    val NE = 18432

    val OCpages = Array.tabulate[SInt](NP){i => 1}
    val OCedges = Array.tabulate(NP){i => Array.tabulate(edges_per_page) {j => j*7}}.flatten
    val OCcounts = Array.tabulate(NP){i => Array.tabulate(edges_per_page) { j => edges_per_page }}.flatten
    val OCedgeId = Array.tabulate(NP) {i => i*edges_per_page } 
    val OCedgeLen = Array.tabulate(NP) { i => edges_per_page }

    val result = pagerank(OCpages, OCedges, OCcounts, OCedgeId, OCedgeLen, iters, damp)
    // println("expected: " + gold.mkString(","))

    printArr(result, "result: ")

  }
}

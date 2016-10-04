import spatial.compiler._
import spatial.library._
import spatial.shared._

object PageRank extends SpatialAppCompiler with PageRankApp
trait PageRankApp extends SpatialApp {
  type Elem = Flt //FixPt[Signed, B16, B16]
  type T = Flt
  type Array[T] = ForgeArray[T]

  lazy val maxNumEdge = param(8)
  lazy val numIter = ArgIn[SInt]
  lazy val damp = ArgIn[Elem]
  val edges_per_page = 6 // Will make this random later



  def pagerank(INpages: Rep[Array[SInt]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INoutBounds: Rep[Array[SInt]], OCiters: Rep[SInt], OCdamp: Rep[SInt]) = {
  
    val np = 96
    val NE = 768
    val tileSize = 96 // For now
    val iters = ArgIn[SInt]
    val damp = ArgIn[SInt]
    val NP = ArgIn[SInt]
    val OCpages = OffChipMem[SInt](np)
    val OCedges = OffChipMem[SInt](NE) // srcs of edges
    val OCcounts = OffChipMem[SInt](np) // number of outbound links for each src in edgeList
    val OCoutBounds = OffChipMem[SInt](np) // [PR iter even, PR iter odd]
    val OCresult = OffChipMem[SInt](np)

    setArg(iters, OCiters)
    setArg(damp, OCdamp)
    setArg(NP, np)
    setMem(OCpages, INpages)
    setMem(OCedges, INedges)
    setMem(OCcounts, INcounts)
    setMem(OCoutBounds, INoutBounds)

    Accel {
      Sequential(iters by 1){ iter =>
        // val oldPrIdx = iter % 2.as[SInt]
        // val newPrIdx = mux(oldPrIdx == 1, 0.as[SInt], 1.as[SInt])
        Sequential(NP by tileSize) { tid =>
          val currentPR = BRAM[SInt](tileSize)
          val lastGatherEdge = Reg[SInt](0)
          val outBoundsTile = BRAM[SInt](tileSize)
          currentPR := OCpages(tid::tid+tileSize, param(1))
          outBoundsTile := OCoutBounds(tid :: tid+tileSize, param(1))
          Sequential(tileSize by 1) { pid =>
            val edgesToGather = BRAM[SInt](tileSize) // Assume no page has more than tileSize # of incoming edges
            val edgeCount = outBoundsTile(pid)
            Pipe(edges_per_page by 1) { i => edgesToGather(i) = i + lastGatherEdge.value }
            lastGatherEdge = Reduce(1 by 1)(0.as[SInt]){ i => edgeCount }{_+_}

            // Gather edges indices
            val edges = BRAM[SInt](tileSize)
            edges := OCedges(edgesToGather,edges_per_page)

            // Gather counts and page ranks with this
            val gatheredPR = BRAM[SInt](tileSize)
            val gatheredCounts = BRAM[SInt](tileSize)
            gatheredPR := OCpages(edges, edges_per_page)
            gatheredCounts := OCcounts(edges, edges_per_page)

            // Compute new PR
            val pr = Reduce(edges_per_page by 1)(0.as[SInt]){ i => gatheredPR(i) / gatheredCounts(i) }{_+_}

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
    val NE = 768

    val OCpages = Array.empty[SInt](NP)
    val OCedges = Array.tabulate(NP){i => Array.tabulate(edges_per_page) {j => j*7}}.flatten
    val OCcounts = Array.tabulate(NP){i => 5}
    val OCoutBounds = Array.tabulate(NP) {i => edges_per_page}

    val result = pagerank(OCpages, OCedges, OCcounts, OCoutBounds, iters, damp)
    // println("expected: " + gold.mkString(","))

    printArr(result, "result: ")

  }
}

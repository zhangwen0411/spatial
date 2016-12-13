import spatial.compiler._
import spatial.library._
import spatial.shared._

object PageRank extends SpatialAppCompiler with PageRankApp // Regression (Sparse) // Args: 1 192 0.5
trait PageRankApp extends SpatialApp {
  type Elem = Flt //FixPt[Signed, B16, B16]
  type T = Flt
  type Array[T] = ForgeArray[T]

/*
                                        0
       _________________________________|__________________________________________________________
      |                   |                 |                  |                 |                 |
      1                   3                 5                  7                 9                 11
   ___|______        _____|____         ____|__          ______|______           |        _________|____________
  |     |    |      |     |    |       |       |        |      |      |          |       |       |        |     |
  2     50   55     4     92   49      150     6        8      10     12        42      110     210      310   311
 _|_    _|_   |    _|_    |   _|_      |     __|_       |      |      |         |        |       |      _|_     |
|   |  |   |  |   |   |   |  |   |     |    |    |      |      |      |         |        |       |     |   |    |
57 100 58 101 140 60 102  99 120 115   13  103  104    105    106    108        43      111     211   300  301  290
                  |
            ______|________
           |   |   |   |   |
           80 131 132 181 235




*/
  val edges_per_page = 16 // Will make this random later
  val margin = 1
  val innerPar = 8
  val outerPar = 1

  def pagerank(INpages: Rep[Array[T]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INedgeId: Rep[Array[SInt]], INedgeLen: Rep[Array[SInt]], OCiters: Rep[SInt], OCdamp: Rep[T], np: Rep[SInt]) = {

    val NE = 92160
    val tileSize = 3072 // For now
    val iters = ArgIn[SInt]
    val NP    = ArgIn[SInt]
    val damp  = ArgIn[T]
    setArg(iters, OCiters)
    setArg(NP, np)
    setArg(damp, OCdamp)

    val op = outerPar (1 -> 3)
    val PX = param(1)
    val ip = innerPar (1 -> 3)

    val OCpages    = DRAM[T](NP)
    val OCedges    = DRAM[SInt](NE)    // srcs of edges
    val OCcounts   = DRAM[SInt](NE)    // counts for each edge
    val OCedgeId = DRAM[SInt](NP) // Start index of edges
    val OCedgeLen = DRAM[SInt](NP) // Number of edges for each page
    // val OCresult   = DRAM[T](np)

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
          val currentPR = SRAM[T](tileSize)
          val initPR = SRAM[T](tileSize)

          val edgesId = SRAM[SInt](tileSize)
          val edgesLen = SRAM[SInt](tileSize)
          Pipe {initPR := OCpages(tid::tid+tileSize) }
          Pipe {edgesId := OCedgeId(tid :: tid+tileSize) }
          Pipe {edgesLen := OCedgeLen(tid :: tid+tileSize) }
          Sequential(tileSize by 1) { pid =>
            val startId = edgesId(pid)
            val numEdges = Reg[SInt](0)
            Pipe{numEdges := edgesLen(pid)}

            def pageRank(id: Rep[SInt]) = {
              mux(id <= pid, initPR(pid), currentPR(pid))
            }

            // Gather edges indices and counts
            val edges = SRAM[SInt](tileSize)
            val counts = SRAM[SInt](tileSize)
            Pipe {edges := OCedges(startId::startId+numEdges.value par ip) }
            Pipe {counts := OCcounts(startId::startId+numEdges.value par ip) }

            // Triage edges based on if they are in current tile or offchip
            val offLoc = SRAM[SInt](tileSize)
            val offAddr = Reg[SInt](-1)
            Sequential(numEdges.value by 1){ i =>
              val addr = edges(i) // Write addr to both tiles, but only inc one addr
              val onchip = addr >= tid && addr < tid+tileSize
              offAddr := offAddr.value + mux(onchip, 0, 1)
              offLoc(i) = mux(onchip, offAddr.value, -1.as[SInt])
            }

            // Set up gather addresses
            val frontierOff = SRAM[SInt](tileSize)
            Sequential(numEdges.value by 1){i =>
              frontierOff(offLoc(i)) = edges(i)
            }

            // Gather offchip ranks
            val gatheredPR = SRAM[T](tileSize)
            Pipe {gatheredPR := OCpages(frontierOff, offAddr.value)} // Can't figure out how to get this to not hang with par

            // // Concat with onchip ranks
            // Pipe(offAddr.value by 1 par PX) { i =>
            //   val appendAddr = onAddr.value + i
            //   Pipe{frontierOn(appendAddr) = gatheredPR(i)}
            // }

            // // Compute new PR
            // val pr = Reduce(numEdges.value by 1 par ip)(0.as[T]){ i => frontierOn(i) / counts(i).to[T] }{_+_}

            // Compute new PR
            val pr = Reduce(numEdges.value by 1)(0.as[T]){ i =>
              val addr = edges(i)
              val off  = offLoc(i)
              val onchipRank = pageRank(addr - tid)

              val offchipRank = gatheredPR(off)

              val rank = mux(off == -1.as[SInt], onchipRank, offchipRank)

              rank / counts(i).to[T]
            }{_+_}
            //val pr = Reduce(numEdges.value by 1)(0.as[T]){ i => frontier(i) / counts(i).to[T] }{_+_}

            // Update PR
            currentPR(pid) = pr.value * damp + (1.as[T] - damp)

            // Reset counts (Plasticine: assume this is done by CUs)
            /*Parallel{
              Pipe{onAddr := 0}
              Pipe{offAddr := 0}
            }*/

          }
          OCpages(tid::tid+tileSize par ip) := currentPR
        }
      }
    }
    getMem(OCpages)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val NP = args(1).to[SInt]
    val damp = args(2).to[T]
    val NE = 18432

    val OCpages = Array.tabulate[T](NP){i => random[T](3)}
    val OCedges = Array.tabulate(NP){i => Array.tabulate(edges_per_page) {j =>
      if (i < edges_per_page) j else i - j}}.flatten
    val OCcounts = Array.tabulate(NP){i => Array.tabulate(edges_per_page) { j => edges_per_page }}.flatten
    val OCedgeId = Array.tabulate(NP) {i => i*edges_per_page }
    val OCedgeLen = Array.tabulate(NP) { i => edges_per_page }

    val result = pagerank(OCpages, OCedges, OCcounts, OCedgeId, OCedgeLen, iters, damp, NP)


    val gold = Array.empty[T](NP)
    // Init
    for (i <- 0 until NP) {
      gold(i) = OCpages(i)
    }

    // Really bad imperative version
    for (ep <- 0 until iters) {
      for (i <- 0 until NP) {
        val numEdges = OCedgeLen(i)
        val startId = OCedgeId(i)
        val iterator = Array.tabulate(numEdges){kk => startId + kk}
        val these_edges = iterator.map{j => OCedges(j)}
        val these_pages = these_edges.map{j => gold(j)}
        val these_counts = these_edges.map{j => OCcounts(j)}
        val pr = these_pages.zip(these_counts){ (p,c) =>
          // println("page " + i + " doing " + p + " / " + c)
          p/c.to[T]
        }.reduce{_+_}
        // println("new pr for " + i + " is " + pr)
        gold(i) = pr*damp + (1.as[T]-damp)
      }
    }

    printArr(gold, "gold: ")
    printArr(result, "result: ")
    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    println("PASS: " + cksum + " (PageRank)")

  }
}

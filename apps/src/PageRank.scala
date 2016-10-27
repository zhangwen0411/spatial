import spatial.compiler._
import spatial.library._
import spatial.shared._

object PageRank extends SpatialAppCompiler with PageRankApp
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
  val edges_per_page = 6 // Will make this random later
  val margin = 1


  def pagerank(INpages: Rep[Array[T]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INedgeId: Rep[Array[SInt]], INedgeLen: Rep[Array[SInt]], OCiters: Rep[SInt], OCdamp: Rep[T]) = {

    val np = 96
    val NE = 9216
    val tileSize = 96 // For now
    val iters = ArgIn[SInt]
    val NP    = ArgIn[SInt]
    val damp  = ArgIn[T]
    val OCpages    = DRAM[T](np)
    val OCedges    = DRAM[SInt](NE)    // srcs of edges
    val OCcounts   = DRAM[SInt](NE)    // counts for each edge
    val OCedgeId = DRAM[SInt](np) // Start index of edges
    val OCedgeLen = DRAM[SInt](np) // Number of edges for each page
    // val OCresult   = DRAM[T](np)

    setArg(iters, OCiters)
    setArg(NP, np)
    setArg(damp, OCdamp)
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
          val edgesId = SRAM[SInt](tileSize)
          val edgesLen = SRAM[SInt](tileSize)
          Pipe {currentPR := OCpages(tid::tid+tileSize) }
          Pipe {edgesId := OCedgeId(tid :: tid+tileSize) }
          Pipe {edgesLen := OCedgeLen(tid :: tid+tileSize) }
          Sequential(tileSize by 1) { pid =>
            val startId = edgesId(pid)
            val numEdges = Reg[SInt](0)
            Pipe{numEdges := edgesLen(pid)}

            // Gather edges indices and counts
            val edges = SRAM[SInt](tileSize)
            val counts = SRAM[SInt](tileSize)
            Pipe {edges := OCedges(startId::startId+numEdges.value) }
            Pipe {counts := OCcounts(startId::startId+numEdges.value) }

            // Triage edges based on if they are in current tile or offchip
            val offAddr = Reg[SInt](0)
            val onAddr = Reg[SInt](0)
            val frontierOff = SRAM[SInt](tileSize)
            val frontierOn = SRAM[T](tileSize)
            Pipe(numEdges.value by 1){ i =>
              val addr = edges(i) // Write addr to both tiles, but only inc one addr
              Pipe{frontierOff(offAddr.value) = addr }
              Pipe{frontierOn(onAddr.value) = currentPR(addr)}
              Pipe{onAddr := mux(i == 0, 0, onAddr.value) + mux(addr >= tid && addr < tid+tileSize, 1, 0)}
              Pipe{offAddr := mux(i == 0, 0, offAddr.value) + mux(addr >= tid && addr < tid+tileSize, 0, 1)}
            }
            // Gather offchip ranks
            val gatheredPR = SRAM[T](tileSize)
            Pipe {gatheredPR := OCpages(frontierOff, offAddr.value)}

            // Concat with onchip ranks
            Pipe(offAddr.value by 1) { i =>
              val appendAddr = onAddr.value + i
              Pipe{frontierOn(appendAddr) = gatheredPR(i)}
            }

            // Compute new PR
            val pr = Reduce(numEdges.value by 1)(0.as[T]){ i => frontierOn(i) / counts(i).to[T] }{_+_}

            // Update PR
            currentPR(pid) = pr.value * damp + (1.as[T] - damp)
          }
          OCpages(tid::tid+tileSize) := currentPR
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
    val OCedges = Array.tabulate(NP){i => Array.tabulate(edges_per_page) {j => j*7}}.flatten
    val OCcounts = Array.tabulate(NP){i => Array.tabulate(edges_per_page) { j => edges_per_page }}.flatten
    val OCedgeId = Array.tabulate(NP) {i => i*edges_per_page } 
    val OCedgeLen = Array.tabulate(NP) { i => edges_per_page }

    val result = pagerank(OCpages, OCedges, OCcounts, OCedgeId, OCedgeLen, iters, damp)


    val gold = Array.empty[T](NP)
    // Init
    for (i <- 0 until NP) {
      gold(i) = OCpages(i)
    }

    // Really bad imperative version
    for (i <- 0 until NP) {
      val numEdges = OCedgeLen(i)
      val startId = OCedgeId(i)
      val iterator = Array.tabulate(numEdges){kk => startId + kk}
      val these_edges = iterator.map{j => OCedges(j)}
      val these_pages = these_edges.map{j => gold(j)}
      val these_counts = these_edges.map{j => OCcounts(j)}
      val pr = these_pages.zip(these_counts){ (p,c) => 
        println("page " + i + " doing " + p + " / " + c)
        p/c.to[T]
      }.reduce{_+_}
      println("new pr for " + i + " is " + pr)
      gold(i) = pr*damp + (1.as[T]-damp)
    }

    printArr(gold, "gold: ")    
    printArr(result, "result: ")
    // val cksum = result.zip(gold.flatten){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    // println("PASS: " + cksum + " (PageRank)")

  }
}

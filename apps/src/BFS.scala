import spatial.compiler._
import spatial.library._
import spatial.shared._

/*
       frontier     ids                  counts           edges
  (ids of nodes   (index in "edges"   (number of edges   (list of
  on next layer)   where this node's   for this node)     edges)
                   cxns starts)
         _            _                   _                _
        | |          | |                 | |              | |
        | |          | |                 | |              | |
        | |          | |                 | |              | |
        | |          | |                 | |              | |
        | |          | |                 | |              | |
        | |          | |                 | |              | |
        |_|          |_|                 |_|              | |
                                                          | |
                                                          | |
                                                          | |
                                                          | |
                                                          | |
                                                          | |
                                                          | |
                                                          |_|
                                                                       
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
object BFS extends SpatialAppCompiler with BFSApp // Regression (Sparse) // Args: 4 5
trait BFSApp extends SpatialApp {
  type Elem = Flt //FixPt[Signed, B16, B16]
  type T = Flt
  type Array[T] = ForgeArray[T]

  val staticTileSize = 1920000
  val tileSize = 46080
  val spacing = 3 // bound for computing offchip mem size
  val innerPar = 8
  val outerPar = 1
  val E = 9600000
  val N = 96000

  def bfs(INnodes: Rep[Array[SInt]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INids: Rep[Array[SInt]], e: Rep[SInt], d: Rep[SInt]) = {

    // val iters = ArgIn[SInt]
    val depth = ArgIn[SInt]
    setArg(depth, d)
    // val E = ArgIn[SInt] // Set to roughly max_edges_per_node * N 
    // setArg(iters, n)
    // setArg(E, e)

    val OCnodes = DRAM[SInt](N)
    val OCedges = DRAM[SInt](E)
    val OCcounts = DRAM[SInt](N)
    val OCids = DRAM[SInt](N)
    val OCresult = DRAM[SInt](N)

    val ip = innerPar (1 -> 2)
    val PX = 1 (1 -> 2)
    val op = outerPar (1 -> 2)

    setMem(OCnodes, INnodes)
    setMem(OCedges, INedges)
    setMem(OCcounts, INcounts)
    setMem(OCids, INids)

    Accel {
      // Sequential(iters by 1) { tile => 
        val frontierNodes = SRAM[SInt](tileSize)
        val frontierCounts = SRAM[SInt](staticTileSize)
        val frontierIds = SRAM[SInt](staticTileSize)
        val currentNodes = SRAM[SInt](tileSize)
        val frontierLevels = SRAM[SInt](tileSize)
        val pieceMem = SRAM[SInt](tileSize)
        val concatReg = Reg[SInt](0)
        val allConcatReg = Reg[SInt](0)
        Parallel {
          Pipe{frontierIds := OCids(0::staticTileSize par ip)}
          Pipe{frontierCounts := OCcounts(0::staticTileSize par ip)}
        }

        Sequential(depth.value by 1) { i => //num layers to scan
          val numEdges = Reg[SInt](1)
          val allNumEdges = Reg[SInt](1)
          // Step through all edges in frontier by tileSize
          /* Divide tile size by average edges per page
             so that we don't overflow
          */
          val stepSize = (tileSize.as[SInt]/5.as[SInt])
          Sequential.fold(allNumEdges.value by stepSize)(allConcatReg) { piece =>
            numEdges := min(allNumEdges.value - piece, stepSize)
            // Grab the number of things that 
            Fold(numEdges.value by 1 par PX, PX)(concatReg, 0.as[SInt]) { k =>
              val nextLen = Reg[SInt]
              val nextId = Reg[SInt]
              val lastLen = Reg[SInt]
              val fetch = Reg[SInt]
              val lastFetch = Reg[SInt]
              Pipe{
                fetch := currentNodes(k)
                lastFetch := currentNodes(k-1)
              }
              Pipe{
                nextId := frontierIds(fetch)
                nextLen := frontierCounts(fetch)
                lastLen := frontierCounts(lastFetch)
              }
              Pipe{pieceMem := OCedges(nextId :: nextId + nextLen.value par ip)}
              Pipe(nextLen.value by 1) { kk => 
                /* Since Fold is a metapipe and we read concatReg before
                   we write to it, this means iter0 and iter1 both read 
                   0 in concatReg.  I.e. we always see the previous iter's
                   value of concatReg, so we should add nextLen to it here
                   if we are not on the first iter (since concatReg is and
                   should be 0)
                */
                val plus = mux(k == 0, 0, lastLen.value)
                frontierNodes(kk + concatReg.value + plus) = pieceMem(kk)
              }
              nextLen
            }{_+_}
            Pipe{concatReg.value by 1 par ip} { kk => currentNodes(kk) = frontierNodes(kk)}
            Pipe(concatReg.value by 1 par ip) { k => frontierLevels(k) = i+1 }
            OCresult(currentNodes, concatReg.value) := frontierLevels
            concatReg
          }{_+_}
          Pipe{allNumEdges := allConcatReg.value}
        }
      // }
    }
    getMem(OCresult)
  }

  def printArr(a: Rep[Array[SInt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    // val layers = 5
    // val nodes = (scala.math.pow(max_edges_per_node, layers) - 1).toInt


    /* OLD VERSION WITH FANCY TREE */
    // val nodes = tileSize
    // val OCedges = Array.empty[SInt](tileSize)
    // OCedges(0) = 1
    // OCedges(1) = 3
    // OCedges(2) = 5
    // OCedges(3) = 7
    // OCedges(4) = 9
    // OCedges(5) = 11
    // OCedges(6) = 2; OCedges(7) = 50; OCedges(8) = 55 // 1 children 6x3
    // OCedges(9) = 57; OCedges(10) = 100 // 2 9x2
    // OCedges(11) = 4; OCedges(12) = 92; OCedges(13) = 49 // 3 11x3
    // OCedges(14) = 60; OCedges(15) = 102 //4 14x2
    // OCedges(16) = 150; OCedges(17) = 6 //5 16x2
    // OCedges(18) = 103; OCedges(19) = 104 //6 18x2
    // OCedges(20) = 8; OCedges(21) = 10; OCedges(22) = 12 //7 20x3
    // OCedges(23) = {105} //8 23x1
    // OCedges(24) = {42} //9 24x1
    // OCedges(25) = {106} //10 25x1
    // OCedges(26) = 110; OCedges(27) = 210; OCedges(28) = 310; OCedges(29) = 311 //11 26x4
    // OCedges(30) = {108} //12 30x1
    // OCedges(31) = {43} //42 31x1
    // OCedges(32) = 120; OCedges(33) = 115 //49 32x2
    // OCedges(34) = 58; OCedges(35) = 101 //50 34x2
    // OCedges(36) = {140} //55 36x1
    // OCedges(37) = {99} //92 37x1
    // OCedges(38) = {111} //110 38x1
    // OCedges(39) = {13} //150 39x1
    // OCedges(40) = {211} //210 40x1
    // OCedges(41) = 300; OCedges(42) = 301 //310 41x2
    // OCedges(43) = {290} //311 43x1
    // OCedges(44) = 80; OCedges(44) = 131; OCedges(45) = 132; OCedges(46) = 181; OCedges(47) = 235 //60 44x5 OOO but don't care

    // val OCids = Array.empty[SInt](tileSize)
    // OCids(0) = 0
    // OCids(1) = 6
    // OCids(2) = 9
    // OCids(3) = 11
    // OCids(4) = 14
    // OCids(5) = 16
    // OCids(6) = 18
    // OCids(7) = 20
    // OCids(8) = 23
    // OCids(9) = 24
    // OCids(10) = 25
    // OCids(11) = 26
    // OCids(12) = 30
    // OCids(42) = 31
    // OCids(49) = 32
    // OCids(50) = 34
    // OCids(55) = 36
    // OCids(60) = 44
    // OCids(92) = 37
    // OCids(110) = 38
    // OCids(150) = 39
    // OCids(210) = 40
    // OCids(310) = 41
    // OCids(311) = 43

    // val OCcounts = Array.empty[SInt](tileSize)
    // OCcounts(0) = 6
    // OCcounts(1) = 3
    // OCcounts(2) = 2
    // OCcounts(3) = 3
    // OCcounts(4) = 2
    // OCcounts(5) = 2
    // OCcounts(6) = 2
    // OCcounts(7) = 3
    // OCcounts(8) = 1
    // OCcounts(9) = 1
    // OCcounts(10) = 1
    // OCcounts(11) = 4
    // OCcounts(12) = 1
    // OCcounts(42) = 1
    // OCcounts(49) = 2
    // OCcounts(50) = 2
    // OCcounts(55) = 1
    // OCcounts(60) = 5
    // OCcounts(92) = 1
    // OCcounts(110) = 1
    // OCcounts(150) = 1
    // OCcounts(210) = 1
    // OCcounts(310) = 2
    // OCcounts(311) = 1


    /* NEW VERSION FOR PERFORMANCE MEASUREMENTS */
    val average_nodes_per_edge = args(0).to[SInt]
    val d = args(1).to[SInt]
    val ed = E //args(1).to[SInt] // Set to roughly max_edges_per_node * N 

    val OCnodes = Array.tabulate(N) {i => 0}
    val OCedges = Array.tabulate(ed){ i => i*2}
    val OCids = Array.tabulate(N)( i => i*spacing)
    val OCcounts = Array.tabulate(N){ i => random[SInt](average_nodes_per_edge-1)*2+1}

    // val bufferedSource = io.Source.fromFile("/tmp/finance.csv")

    val result = bfs(OCnodes, OCedges, OCcounts, OCids, ed, d)
    // val gold = (6*1) + (16*2) + (22*3) + (5*4)
    // println("Cksum: " + gold + " == " + result.reduce{_+_})

    // val cksum = gold == result.reduce{_+_}
    printArr(result, "result: ")
    println("Cksum = " + result.reduce{_+_})
    // println("PASS: " + cksum + " (BFS)")

  }
}

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
*/
object BFS extends SpatialAppCompiler with BFSApp
trait BFSApp extends SpatialApp {
  type Elem = Flt //FixPt[Signed, B16, B16]
  type T = Flt
  type Array[T] = ForgeArray[T]

  val tileSize = 96
  val edges_per_node = 6 // Will make this random later



  def bfs(INnodes: Rep[Array[SInt]], INedges: Rep[Array[SInt]], INcounts: Rep[Array[SInt]], INids: Rep[Array[SInt]], n: Rep[SInt], e: Rep[SInt]) = {

    val OCnodes = DRAM[SInt](n)
    val OCedges = DRAM[SInt](e)
    val OCcounts = DRAM[SInt](n)
    val OCids = DRAM[SInt](n)
    val OCresult = DRAM[SInt](n)

    setMem(OCnodes, INnodes)
    setMem(OCedges, INedges)
    setMem(OCcounts, INcounts)
    setMem(OCids, INids)

    Accel {
      val frontierNodes = SRAM[SInt](tileSize)
      val frontierCounts = SRAM[SInt](tileSize)
      val frontierIds = SRAM[SInt](tileSize)
      val currentNodes = SRAM[SInt](tileSize)
      val frontierLevels = SRAM[SInt](tileSize)
      val pieceMem = SRAM[SInt](tileSize)
      val concatReg = Reg[SInt](0)
      Parallel {
        Pipe{frontierIds := OCids(0::tileSize)}
        Pipe{frontierCounts := OCcounts(0::tileSize)}
      }

      val numEdges = Reg[SInt](1)
      Sequential(2 by 1) { i =>
        Fold(numEdges.value by 1)(concatReg, 0.as[SInt]) { k =>
          val nextLen = Reg[SInt]
          val nextId = Reg[SInt]
          val fetch = currentNodes(k)
          Pipe{nextId := frontierIds(fetch)}
          Pipe{nextLen := frontierCounts(fetch)}
          Pipe{pieceMem := OCedges(nextId :: nextId + nextLen.value)}
          Sequential(nextLen.value by 1) { kk => frontierNodes(kk + concatReg.value) = pieceMem(kk)}
          nextLen
        }{_+_}
        Pipe{numEdges.value by 1} { kk => currentNodes(kk) = (kk)}
        Sequential(concatReg.value by 1) { k => frontierLevels(k) = i+1 }
        OCresult(currentNodes, concatReg.value) := frontierLevels
        Pipe{numEdges := concatReg.value}
      }
      // Sequential(count by 1) { i =>
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
    // val nodes = (scala.math.pow(edges_per_node, layers) - 1).toInt
    val nodes = 96

    val OCnodes = Array.tabulate(nodes) {i => 0}
    val OCedges = Array.tabulate(nodes*edges_per_node){i => i+1}
    val OCcounts = Array.tabulate(nodes) { i => edges_per_node }
    val OCids = Array.tabulate(nodes) { i => i*edges_per_node }
    // val gold = Array.empty[SInt](layers)
    // (0 until nodes) foreach { i =>
    //   gold(i) = Array.tabulate(i) { j => i}.reduce{_*_}}
    val result = bfs(OCnodes, OCedges, OCcounts, OCids, nodes, nodes*edges_per_node)
    // println("expected: " + gold.mkString(","))

    printArr(result, "result: ")

  }
}

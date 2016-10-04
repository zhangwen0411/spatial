// import spatial.compiler._
// import spatial.library._
// import spatial.shared._

// object PageRank extends SpatialAppCompiler with PageRankApp
// trait PageRankApp extends SpatialApp {
//   type Elem = Flt //FixPt[Signed, B16, B16]
//   type Array[T] = ForgeArray[T]

//   lazy val maxNumEdge = param(8)
//   lazy val numIter = ArgIn[SInt]
//   lazy val damp = ArgIn[Elem]



//   def pagerank(OCpages: Rep[Array[T]], OCedges: Rep[Array[SInt]], OCcounts: Rep[Array[SInt], OCoutBounds: Rep[Array[SInt]], OCiters: Rep[SInt], OCdamp: Rep[SInt]) = {
  
//     val NP = OCpages.length
//     val NE = OCedges.length
//     val tileSize = tileSize // For now
//     val iters = ArgIn[SInt]
//     val damp = ArgIn[Elem]
//     val pages = OffChipMem[SInt](NP)
//     val edges = OffChipMem[SInt](NE) // srcs of edges
//     val counts = OffChipMem[SInt](NP) // number of outbound links for each src in edgeList
//     val outBounds = OffChipMem[Elem](NP) // [PR iter even, PR iter odd]
//     val result = OffChipMem[Elem](NP)

//     setArg(iters, OCiters)
//     setArg(damp, OCdamp)
//     setMem(pages, OCpages)
//     setMem(edges, OCedges)
//     setMem(counts, OCcounts)
//     setMem(outBounds, OCoutBounds)

//     Accel {
//       Sequential(iters by 1){ iter =>
//         // val oldPrIdx = iter % 2.as[SInt]
//         // val newPrIdx = mux(oldPrIdx == 1, 0.as[SInt], 1.as[SInt])
//         Sequential(NP by tileSize) { tid =>
//           val currentPR = BRAM[SInt](tileSize)
//           val lastGatherEdge = Reg[SInt](0)
//           val outBoundsTile = BRAM[SInt](tileSize)
//           currentPR := OCpages(tid::tid+tileSize, param(1))
//           outBoundsTile := outBounds(tid :: tid+tileSize, param(1))
//           Sequential(tileSize by 1) { pid =>
//             val edgesToGather = BRAM[Elem](tileSize) // Assume no page has more than tileSize # of incoming edges
//             val edgeCount = outBoundsTile(pid)
//             Pipe(edgeCount by 1 par param(1)) { i => gatherEdges(i) = i}
//             lastGatherEdge = Reduce(1)(0.as[SInt]{ i => edgeCount }

//             // Gather edges indices
//             val edges = BRAM[Elem](tileSize)
//             edges := OCedges(gatherEdges,edgeCount)

//             // Gather counts and page ranks with this
//             val gatheredPR = BRAM[Elem](tileSize)
//             val gatheredCounts = BRAM[Elem](tileSize)
//             gatheredPR := OCpages(edges, edgeCount)
//             gatheredCounts := OCcounts(edges, edgeCount)

//             // Compute new PR
//             val pr = Reduce(edgeCount by 1)(0.as[SInt]){ i => gatheredPR(i) / gatheredCounts(i) }{_+_}

//             // Update PR
//             currentPR(pid) = pr.value * damp + (1 - damp)
//           }  
//         }
//         result(tid::tid+tileSize) := currentPR
//       }




//   def main() {
//     val NI = args(0).to[SInt]
//     val DF = args(1).to[Elem]
//     val maps = loadDirEdgeList("/Users/Yaqi/Documents/hyperdsl/forge/apps/DHDL/graph/testPr.dot", NV, true)
//     val smap = maps(0)
//     val dmap = maps(1)
//     val svl = getVertList(smap, false, true)
//     val dvl = getVertList(dmap, false, true)
//     val del = getEdgeList(dmap)
//     val NE = del.length // Actual number of edges in graph
//     //val NV = svl(0).length/2 // Actual number of vertices in graph
//     val sob = Array.tabulate(NE) { i =>
//       svl(del(i)*2+1)
//     }

//     val result = getMem(pageRank)

//     /* Scala Version */
//     val gold = Array.fill (NV*2)(1.as[Elem]/NV.to[Elem])
//     for (iter <- 0.as[SInt] until NI) {
//       val oldPrIdx = iter % 2.as[SInt]
//       val newPrIdx = (iter + 1.as[SInt]) % 2.as[SInt]
//       def getPr(iv:Rep[SInt]) = gold(iv*2.as[SInt] + oldPrIdx)
//       def setPr(iv:Rep[SInt], p:Rep[Elem]) = gold(iv*2.as[SInt] + newPrIdx) = p
//       for (iv <- 0.as[SInt] until NV) {
//         val pt = dvl(iv*2)
//         val numEdge = dvl(iv*2+1)
//         val sum = if (numEdge == 0.as[SInt]) {
//           0.as[Elem]
//         } else {
//           Array.tabulate(numEdge){ ie =>
//             val e = del(pt + ie)
//             getPr(e) / sob(pt + ie).to[Elem]
//           }.reduce(_+_)
//         }
//         setPr(iv, sum * DF + (1.as[Elem] - DF))
//       }
//     }
//     println("expected: " + gold.mkString(","))
//     println("result: " + result.mkString(","))
//     assert(result == gold)

//   }
// }

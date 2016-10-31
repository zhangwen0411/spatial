import spatial.compiler._
import spatial.library._
import spatial.shared._

object Kmeans extends SpatialAppCompiler with KmeansApp
trait KmeansApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type T = Flt

  val num_cents = 192
  val dim = 384
  val tileSize = 192
  val innerPar = 4
  val outerPar = 1
  val margin = 1
  val K = num_cents
  val D = dim

  lazy val MAXK = num_cents
  lazy val MAXD = dim

  def kmeans(points_in: Rep[Array[T]], numPoints: Rep[SInt], 
    numCents: Rep[SInt], numDims: Rep[SInt], it: Rep[SInt]) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = tileSize (96 -> 96 -> 9600)
    val BD = MAXD
    val PX = 1 (1 -> 1)
    val P0 = outerPar (32 -> 96 -> 192) // Dimensions loaded in parallel
    val P1 = outerPar (1 -> 12)         // Sets of points calculated in parallel
    val P2 = innerPar (1 -> 4 -> 96)    // Dimensions accumulated in parallel (outer)
    val P3 = innerPar (1 -> 4 -> 16)    // Points calculated in parallel
    val PR = innerPar (1 -> 4 -> 96)
    val P4 = innerPar (1 -> 4 -> 96)

    val iters = ArgIn[SInt]
    val N = ArgIn[SInt]
    // val K = ArgIn[SInt]
    // val D = ArgIn[SInt]
    setArg(iters, it)
    setArg(N, numPoints)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](num_cents*dim) // Output centroids
    setMem(points, points_in)

    Accel {
      val cts = SRAM[T](MAXK, MAXD)

      // Load initial centroids (from points)
      cts := points(0::K,0::D par P0)

      val DM1 = D - 1

      Sequential(iters by 1){epoch => 

        val newCents = SRAM[T](MAXK,MAXD)
        // For each set of points
        Pipe(N by BN par param(1)){i =>
          val pts = SRAM[T](BN, BD)
          pts := points(i::i+BN, 0::BD par P0)

          // For each point in this set
          Fold(BN par P3, PR)(newCents, 0.as[T]){pt =>
            // Find the index of the closest centroid
            val minCent = Reduce(K par P4)(pack((0.as[SInt],100000.as[T]))){ct =>
              val dist = Reduce(D par P2)(0.as[T]){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}
              pack((ct, dist.value))
            }{(a,b) =>
              mux(a._2 < b._2, a, b)
            }

            // Store this point to the set of accumulators
            val localCent = SRAM[T](MAXK,MAXD)
            Pipe(K by 1, D par P2){(ct,d) =>
              val elem = mux(d == DM1, 1.as[T], pts(pt, d))
              localCent(ct, d) = mux(ct == minCent.value._1, elem, 0.as[T])
            }
            localCent
          }{_+_} // Add the current point to the accumulators for this centroid
        }

        val centCount = SRAM[T](MAXK)
        Pipe(K by 1){ct => centCount(ct) = newCents(ct,DM1) }

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Pipe(K by 1, D par PX){(ct,d) =>
          cts(ct, d) = newCents(ct,d) / centCount(ct)
        }
        // Flush centroid accumulator
        Pipe(K by 1, D par P2){(ct,d) =>
          newCents(ct,d) = 0.as[T]
        }
      }

      val flatCts = SRAM[T](K*D)
      Pipe(K by 1, D by 1) {(i,j) =>
        flatCts(i*D+j) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par P2) := flatCts
    }

    getMem(centroids)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }
  def printIntArr(a: Rep[Array[SInt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val N = args(1).to[SInt];
    // val KK = num_cents //args(2).to[SInt];
    // val DD = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.as[T] else random[T](10) }}

    // println("points: ")
    // for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    val result = kmeans(pts.flatten, N, K, D, iters)

    // val cts = Array.tabulate(K){i => Array.tabulate(D){d => pts(i,d) }}

    val gold = Array.tabulate(K){i => Array.empty[T](D)}
    val acc = Array.tabulate(K){i => Array.empty[T](D)}
    val ii = Array.tabulate(K) {i=>i}
    val counts = Array.empty[UInt](K)
    for (i <- 0 until K) { for (d <- 0 until D) {
      val row = pts(i)
      gold(i)(d) = row(d)
      acc(i)(d) = 0.as[T]
    }}
    for(epoch <- 0 until iters) {
      // Really bad imperative version
      def dist(p1: Rep[Array[T]], p2: Rep[Array[T]]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)
      for (i <- 0 until N) {
        val pt = pts(i)
        val distances = gold.map{ct => dist(pt, ct) }
        // Issue #21 with metadata SoA forces us to get minIdx in this hacky way
        val minDist = distances.reduce{(a,b) => if (a < b) a else b }
        val minIdx = ii.map{a => if (distances(a) == minDist) a else 0.as[SInt]}.reduce{_+_}
        // printArr(distances, "dst: ")
        // println("mindst " + minDist)
        // printIntArr(ii.map{a => if (distances(a) == minDist) a else 0.as[SInt]}, "id list: ")
        println("point " + i + "matches id: " + minIdx)

        counts(minIdx) = counts(minIdx) + 1
        for (j <- 0 until D) {
          val row = acc(minIdx)
          acc(minIdx)(j) = row(j) + pt(j)
        }

      }

      val actual = acc.zip(counts){(ct,n) => ct.map{p => p / n.to[T] }}
      for (i <- 0 until K) { for (d <- 0 until D) {
        val row = actual(i)
        gold(i)(d) = row(d)
      }}
      // printArr(gold.flatten, "epoch " + epoch + ": ")
    }

    // println("gold:   " + actual.map(a => a).reduce{_+_})
    // println("result: " + result.map(a => a).reduce{_+_})
    printArr(gold.flatten, "gold: ")
    printArr(result, "result: ")

    val cksum = result.zip(gold.flatten){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    println("PASS: " + cksum + " (Kmeans)")
  }

  /*def kmeans_SLOW(points_in: Rep[Array[SInt]], numPoints: Rep[SInt], numCents: Rep[SInt], numDims: Rep[SInt]) = {
    bound(numPoints) = 960000
    bound(numCents) = MAXK
    bound(numDims) = MAXD

    val BN = param(320);  domainOf(BN) = (96, 9600, 96)
    val BD = param(384);  domainOf(BD) = (MAXD, MAXD, MAXD)
    val PX = param(1);    domainOf(PX) = (1,1,1)
    val P0 = param(1);    domainOf(P0) = (1,96,1)     // Dimensions loaded in parallel
    val P1 = param(1);    domainOf(P1) = (1,96,1)     // Points calculated in parallel
    val P2 = param(1);    domainOf(P2) = (1,MAXD,1)   // Dimensions subtracted in parallel
    val P3 = param(1);    domainOf(P3) = (1,MAXD,1)   // Dimensions updated in parallel
    val P4 = param(1);    domainOf(P4) = (1,MAXD,1)   // Dimensions averaged in parallel
    val P5 = param(1);    domainOf(P5) = (1,MAXD,1)   // Dimensions stored in parallel

    val N = ArgIn[SInt]
    val K = ArgIn[SInt]
    val D = ArgIn[SInt]
    setArg(N, numPoints)
    setArg(K, numCents)
    setArg(D, numDims)

    val points = DRAM[SInt](N, D)    // Input points
    val centroids = DRAM[SInt](K, D) // Output centroids
    setMem(points, points_in)

    Accel {
      val cts = SRAM[SInt](MAXK, dTileSize)
      val newCents = SRAM[SInt](MAXK, dTileSize)
      val centCount = SRAM[UInt](MAXK)
      val centsOut = SRAM[SInt](MAXK, dTileSize)

      // Load initial centroids (from points)
      cts := points(0::K,0::dTileSize, P0)

      // For each set of points
      Pipe(N by BN par PX){i =>
        val pts = SRAM[SInt](BN, BD)
        pts := points(i::i+BN, 0::BD, P0)

        // For each point in this set
        Pipe(BN par PX){pt =>
          val minCent = Reg[SInt](0)  // Index of closest centroid
          val minDist = Reg[SInt](-1)  // Distance to closest centroid

          // Find the index of the closest centroid
          Pipe(K by 1 par PX){ct =>
            val dist = Reduce(D par P2)(0.as[SInt]){d => (pts(pt,d) - cts(ct,d)) ** 2 }{_+_}

            Pipe {
              minDist := min(dist.value, minDist.value)
              minCent := mux(minDist.value == dist.value, ct, minCent.value)
            }
          }

          // Add the current point to the accumulators for this centroid
          Parallel {
            Pipe(D par P3){d => newCents(minCent.value,d) = newCents(minCent.value,d) + pts(pt,d) }
            Pipe { centCount(minCent.value) = centCount(minCent.value) + 1 }
          }
        }
      }
      // Average each new centroid
      Pipe(K by 1, D par P4){(ct,d) =>
        centsOut(ct, d) = newCents(ct,d) / centCount(ct).to[SInt]
      }
      // Store the centroids out
      centroids(0::K,0::D,P5) := centsOut
    }

    getMem(centroids)
  }*/

}

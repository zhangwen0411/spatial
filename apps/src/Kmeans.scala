import spatial.compiler._
import spatial.library._
import spatial.shared._

object Kmeans extends SpatialAppCompiler with KmeansApp
trait KmeansApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type T = Flt

  val num_cents = 96
  val dim = 192
  val tileSize = 96
  val innerPar = 1
  val outerPar = 1
  val margin = 1
  lazy val MAXK = num_cents
  lazy val MAXD = dim

  def kmeans(points_in: Rep[Array[T]], numPoints: Rep[SInt], numCents: Rep[SInt], numDims: Rep[SInt], it: Rep[SInt]) = {
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
    val K = ArgIn[SInt]
    val D = ArgIn[SInt]
    setArg(iters, it)
    setArg(N, numPoints)
    setArg(K, numCents)
    setArg(D, numDims)

    val points = DRAM[T](N, D)    // Input points
    val centroids = DRAM[T](K, D) // Output centroids
    setMem(points, points_in)

    Accel {
      val cts = SRAM[T](MAXK, MAXD)

      // Load initial centroids (from points)
      cts := points(0::K,0::D par P0)

      val DM1 = D.value - 1

      Sequential(iters by 1){epoch => 

        val newCents = SRAM[T](MAXK,MAXD)
        // For each set of points
        Fold(N by BN par P1, PR)(newCents, 0.as[T]){i =>
          val pts = SRAM[T](BN, BD)
          pts := points(i::i+BN, 0::BD par P0)

          val centTile = SRAM[T](MAXK, MAXD)
          // For each point in this set
          Fold(BN par P3, PR)(centTile, 0.as[T]){pt =>
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
        }{_+_}

        val centCount = SRAM[T](MAXK)
        Pipe(K by 1 par PX){ct => centCount(ct) = newCents(ct,DM1) }

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Pipe(K by 1, D par PX){(ct,d) =>
          cts(ct, d) = newCents(ct,d) / centCount(ct)
        }
      }
      // Store the centroids out
      centroids(0::K, 0::D par P2) := cts
    }

    getMem(centroids)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val N = args(1).to[SInt];
    val K = num_cents //args(2).to[SInt];
    val D = dim //args(3).to[SInt];

    val pts = Array.tabulate(N){i => Array.tabulate(D){d => if (d == D-1) 1.as[T] else random[T](10) }}

    // println("points: ")
    // for (i <- 0 until N) { println(i.mkString + ": " + pts(i).mkString(", ")) }

    val result = kmeans(pts.flatten, N, K, D, iters)

    // val cts = Array.tabulate(K){i => Array.tabulate(D){d => pts(i,d) }}

    val gold = Array.empty[T](K*D)
    val counts = Array.empty[UInt](K)
    for (i <- 0 until K) { for (d <- 0 until D) {
      val row = pts(i)
      gold(i*D + d) = row(d)
    }}
    // Really bad imperative version
    def dist(p1: Rep[Array[T]], p2: Rep[Array[T]]) = p1.zip(p2){(a,b) => (a - b)**2 }.reduce(_+_)
    // for (i <- 0 until N) {
    //   val pt = pts(i)
    //   val distWithIndex = cts.map{ct => dist(pt, ct) }.zipWithIndex
    //   val minIdx = distWithIndex.reduce{(a,b) => if (a._1 < b._1) a else b }._2

    //   counts(minIdx) = counts(minIdx) + 1
    //   for (j <- 0 until D) {
    //     gold(minIdx)(j) = gold(minIdx).apply(j) + pt(j)
    //   }

    //   println(counts.mkString(", "))
    //   for (x <- 0 until K) { println(gold(x).mkString(", ")) }
    // }
    // val actual = gold.zip(counts){(ct,n) => ct.map{p => p / n.to[T] }}.flatten
    // println("gold:   " + actual.map(a => a).reduce{_+_})
    // println("result: " + result.map(a => a).reduce{_+_})
    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = result.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
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

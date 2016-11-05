import spatial.compiler._
import spatial.library._
import spatial.shared._

object Kmeans extends SpatialAppCompiler with KmeansApp
trait KmeansApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type T = Flt

  val num_cents = 96
  val dim = 96
  val tileSize = 384
  val innerPar = 8
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
    val P0 = innerPar (32 -> 96 -> 192) // Dimensions loaded in parallel
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

    Accel { Sequential {
      val cts = SRAM[T](MAXK, MAXD)

      // Load initial centroids (from points)
      cts := points(0::K,0::D par P0) 

      val DM1 = D - 1

      Sequential(iters by 1){epoch => 

        val newCents = SRAM[T](MAXK,MAXD)
        // For each set of points
        Pipe(N by BN par PX){i =>
          val pts = SRAM[T](BN, BD)
          pts := points(i::i+BN, 0::BD par P0)

          // For each point in this set
          Fold(BN par P1, PR)(newCents, 0.as[T]){pt =>
            // Find the index of the closest centroid
            val minCent = Reduce(K par PX)(pack((0.as[SInt],100000.as[T]))){ct =>
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
        Pipe(K by 1 par PX){ct => centCount(ct) = newCents(ct,DM1) } // Until diagonal banking is allowed

        // Average each new centroid
        // val centsOut = SRAM[T](MAXK, MAXD)
        Pipe(K by 1, D par P0){(ct,d) =>
          cts(ct, d) = newCents(ct,d) / centCount(ct)
        }
        // Flush centroid accumulator
        Pipe(K by 1, D par P0){(ct,d) =>
          newCents(ct,d) = 0.as[T]
        }
      }

      val flatCts = SRAM[T](K*D)
      Pipe(K by 1, D by 1) {(i,j) =>
        flatCts(i*D+j) = cts(i,j)
      }
      // Store the centroids out
      centroids(0::K*D par P2) := flatCts
    }}

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

    // // val cts = Array.tabulate(K){i => Array.tabulate(D){d => pts(i,d) }}

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


}

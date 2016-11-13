import spatial.compiler._
import spatial.library._
import spatial.shared._

// Sparse Matrix Vector multiply
object SMV extends SpatialAppCompiler with SMVApp
trait SMVApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val tileSize = 96
  val innerPar = 1
  val outerPar = 1
  val pp = 3840
  val NNZ = 50
  val margin = 1

  def smv(AC: Rep[Array[SInt]], AD: Rep[Array[T]], S: Rep[Array[SInt]], V: Rep[Array[T]],nn: Rep[SInt]) = {
    val N = ArgIn[SInt]
    setArg(N,nn)

    val aC = DRAM[SInt](pp,NNZ)
    val aD = DRAM[T](pp,NNZ)
    val sizes = DRAM[SInt](pp)
    val v = DRAM[T](pp)
    val out = DRAM[T](N)

    val op = outerPar (1 -> 6)
    val ip = innerPar (1 -> 96)
    val stPar    = innerPar (1 -> 1)

    setMem(aC, AC)
    setMem(aD, AD)
    setMem(sizes, S)
    setMem(v, V)

    Accel {
      Pipe(N by tileSize par op){ rowchunk =>
        val result = SRAM[T](tileSize)
        val tileSizes = SRAM[SInt](tileSize)
        tileSizes := sizes(rowchunk :: rowchunk+tileSize par ip)
        Sequential(tileSize by 1){row =>
          val csrCols = SRAM[SInt](tileSize)
          val csrData = SRAM[T](tileSize)
          val vecGathered = SRAM[T](tileSize)

          // Load dense csr piece
          val len = tileSizes(row)
          val OCROW = (rowchunk+row) // TODO: Issue #47
          csrCols := aC(OCROW, 0 :: len par ip)
          csrData := aD(OCROW, 0 :: len par ip)
          vecGathered := v(csrCols, len)

          val acc = Reduce(len by 1)(0.as[T]) { i =>
            csrData(i) * vecGathered(i)
          }{_+_}

          result(row) = acc.value

        }
      out(rowchunk::rowchunk+tileSize par stPar) := result
      }
    }
    getMem(out)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val maxNNZ = NNZ
    val N = args(0).to[SInt]
    val P = pp

    val AC = Array.tabulate(N){ i => Array.tabulate(maxNNZ) { j => j * 3}}
    val AD = Array.tabulate(N){ i => Array.fill(maxNNZ) {random[T](5) }}
    val S = Array.tabulate(N){ i => maxNNZ }
    val V = Array.tabulate(P){ i => i }

    val result = smv(AC.flatten, AD.flatten, S, V, N)

    val gold = AC.zip(AD) { (col, data) => col.zip(data) {(c, d) =>
      d*V(c)
    }.reduce{_+_}}

    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = result.zip(gold){(a,b) => a - margin < b && a + margin > b}.reduce{_&&_}
    println("PASS: " + cksum + " (SMV)")

  }
}

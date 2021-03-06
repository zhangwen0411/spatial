import spatial.compiler._
import spatial.library._
import spatial.shared._

object MatMult_inner extends SpatialAppCompiler with MatMult_innerApp // Regression (Dense) // Args: 4 192 192
trait MatMult_innerApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val tileSizeM = 8
  val tileSizeN = 192
  val tileSizeP = 192
  val innerPar = 4
  val midPar = 2
  val outerPar = 2
  val memPar = 1

  def MatMult_inner(A: Rep[Array[T]], B: Rep[Array[T]], mm: Rep[SInt], nn: Rep[SInt], pp: Rep[SInt]) = {
    val M = ArgIn[SInt]
    val N = ArgIn[SInt]
    val P = ArgIn[SInt]
    setArg(M,mm)
    setArg(N,nn)
    setArg(P,pp)

    val a = DRAM[T](M, P)
    val b = DRAM[T](P, N)
    val c = DRAM[T](M, N)

    val bm = tileSizeM (1 -> 1536)
    val bn = tileSizeN (96 -> 96 -> 1536)
    val bp = tileSizeP (96 -> 96 -> 1536)

    val op = outerPar (1 -> 6)
    val mp = midPar   (1 -> 96)
    val ip = innerPar (1 -> 96)
    val px = 1 (1 -> 1) // Cannot parallelize accum across k blocks
    val tPar = memPar (1 -> 1)

    setMem(a, A)
    setMem(b, B)

    Accel {
      Pipe(M by bm, (N by bn) par op){(i,j) =>
        val tileC = SRAM[T](bm, bn)
        Pipe((P by bp) par px){k =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          Parallel {
            tileA := a(i::i+bm, k::k+bp par tPar) // Reads M*N*P times
            tileB := b(k::k+bp, j::j+bn par tPar)
          }
          Pipe(bm by 1, (bn by 1) par px){ (ii,jj) =>    // MetaPipe?
            val prod = Reduce((bp by 1) par ip)(0.as[T]){ kk => tileA(ii, kk) * tileB(kk, jj) }{_+_}
            val prev = mux(k == 0, 0.as[T], tileC(ii,jj))
            tileC(ii,jj) = prev + prod.value // Is a unit pipe that should be recognized as accum
          }
        }
        c(i::i+bm, j::j+bn par tPar) := tileC
      }
    }
    getMem(c)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    val P = args(2).to[SInt]

    val a = Array.tabulate(M){ j => Array.tabulate(P){ i => i + j * P} }
    val b = Array.tabulate(P){ j => Array.tabulate(N){ i => i + j * N} }
    // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = MatMult_inner(a.flatten, b.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    println("expected cksum: " + gold.map(a => a).reduce{_+_})
    println("result cksum: " + result.map(a => a).reduce{_+_})

    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MatMult_inner)")

  }
}

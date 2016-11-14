import spatial.compiler._
import spatial.library._
import spatial.shared._

object MatMult_outer extends SpatialAppCompiler with MatMult_outerApp
trait MatMult_outerApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val tileSizeM = 2
  val tileSizeN = 192
  val tileSizeP = 192
  val innerPar = 16
  val midPar = 1
  val outerPar = 2

  def MatMult_outer(A: Rep[Array[T]], B: Rep[Array[T]], C_init: Rep[Array[T]], mm: Rep[SInt], nn: Rep[SInt], pp: Rep[SInt]) = {
    val M = ArgIn[SInt]
    val N = ArgIn[SInt]
    val P = ArgIn[SInt]
    setArg(M,mm)
    setArg(N,nn)
    setArg(P,pp)

    val a = DRAM[T](M, P)
    val b = DRAM[T](P, N)
    val c_init = DRAM[T](M, N)
    val c = DRAM[T](M, N)

    val op = 1 (1 -> 6)
    val mp = 1 (1 -> 96)
    val ip = 1 (1 -> 96)

    val px = 1 (1 -> 1) // Cannot parallelize accum across k blocks

    val bm        = param(4)
    val bn        = param(96)
    val bp        = param(96)

    setMem(a, A)
    setMem(b, B)
    setMem(c, C_init)

    Accel {
      Pipe(M by bm, N by bn par op) { (i,j) =>
        val tileC = SRAM[T](bm, bn)
        Pipe(P by bp par px) { k =>
          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          Parallel {
            tileA := a(i::i+bm, k::k+bp)
            tileB := b(k::k+bp, j::j+bn)
          }
          // Requires tileC NOT to be reset until next j
          Fold(bp by 1 par mp)(tileC, 0.as[T]) { kk =>
            val tileC_partial = SRAM[T](bm,bn)
            Pipe(bm by 1, bn by 1 par ip){ (ii,jj) =>
              tileC_partial(ii,jj) = tileA(ii,kk) * tileB(kk,jj)
            }
            tileC_partial
          }{_+_}
     		}
        c(i::i+bm, j::j+bn) := tileC
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

    val a = Array.fill(M){ Array.fill(P){1} }
    val b = Array.fill(P){ Array.fill(N){1} }
    val c_init = Array.fill(M){ Array.fill(N){0} }
    // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = MatMult_outer(a.flatten, b.flatten, c_init.flatten, M, N, P)

    val gold = Array.tabulate(M){i =>
      val aRow = a(i)
      Array.tabulate(N){j =>
        val bCol = b.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    println("expected cksum: " + gold.map(a => a).reduce{_+_})
    println("result cksum: " + result.map(a => a).reduce{_+_})
    printArr(gold, "Gold: ")
    printArr(result, "Result: ")

    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MatMult_outer)")
   }
 }

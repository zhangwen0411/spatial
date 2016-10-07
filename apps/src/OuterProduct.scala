import spatial.compiler._
import spatial.library._
import spatial.shared._

object OuterProduct extends SpatialAppCompiler with OuterProductApp
trait OuterProductApp extends SpatialApp {
  type T = SInt
  val tileSize1 = 96
  val tileSize2 = 96
  val op = 1
  val ip = 1

  def outerproduct(a: Rep[ForgeArray[T]], b: Rep[ForgeArray[T]]) = {
    val tileSizeA = param(tileSize1);  domainOf(tileSizeA) = (96, 38400, 96)
    val tileSizeB = param(tileSize2);  domainOf(tileSizeB) = (96, 38400, 96)
    val outerPar  = param(op);  domainOf(outerPar) = (1, 4, 1)
    val innerPar  = param(ip);  domainOf(innerPar) = (1, 38400, 1)

    val M = a.length;  bound(M) = 38400
    val N = b.length;  bound(N) = 38400

    val sizeA = ArgIn[SInt]
    val sizeB = ArgIn[SInt]
    setArg(sizeA, M)
    setArg(sizeB, N)

    val vec1 = DRAM[T](sizeA)
    val vec2 = DRAM[T](sizeB)
    val out = DRAM[T](sizeA, sizeB)

    setMem(vec1, a)
    setMem(vec2, b)

    Accel {
      Pipe(sizeA by tileSizeA, sizeB by tileSizeB par outerPar){ (i,j) =>
        val b1 = SRAM[T](tileSizeA)
        val b2 = SRAM[T](tileSizeB)
        val outTile = SRAM[T](tileSizeA, tileSizeB)
        Parallel {
          b1 := vec1(i::i+tileSizeA)
          b2 := vec2(j::j+tileSizeB)
        }
        Pipe(tileSizeA by 1, tileSizeB par innerPar){ (ii,jj) => outTile(ii, jj) = b1(ii) * b2(jj) } // 2

        out(i::i+tileSizeA, j::j+tileSizeB) := outTile
      }
    }
    getMem(out)
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]
    // val a = Array.fill(M)(random[T](100))
    // val b = Array.fill(N)(random[T](100))
    val a = Array.tabulate(M) { i => i }
    val b = Array.fill(N)(1)

    val result = outerproduct(a, b)

    val gold = Array.tabulate(M){i => Array.tabulate(N){j => a(i) * b(j) }}.flatten
    println("expected cksum: " + gold.map(a => a).reduce{_+_})
    println("result cksum:   " + result.map(a => a).reduce{_+_})
    (0 until M*N) foreach { i => assert(result(i) == gold(i)) }

    val cksum = result.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (OuterProduct)")


  }
}

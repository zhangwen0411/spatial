import spatial.compiler._
import spatial.library._
import spatial.shared._

/*
Load data into tile, then accumulate into this tile
*/

object TripleBuf extends SpatialAppCompiler with TripleBufApp
trait TripleBufApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  def TripleBuf(C: Rep[Array[T]], mm: Rep[SInt], nn: Rep[SInt]) = {
    val M = ArgIn[SInt]
    val N = ArgIn[SInt]
    setArg(M,mm)
    setArg(N,nn)
    val bm = param(96)
    val bn = param(96)

    val c = DRAM[T](M, N)
    val d = DRAM[T](M, N)

    setMem(c, C)

    Accel {

      val tileC = SRAM[T](bm, bn)
      Pipe(M by bm, N by bn) { (m,n) =>

        // STAGE 1: Tile load
        tileC := c(m::m+bm, n::n+bn)

        // STAGE 2: Accumulate (Will be DCE in MaxJ)
        Pipe(bn by 1){ i =>
          val sum = Reduce(bn by 1)(0.as[T]) { j =>
            tileC(i,j)
          }{_+_}
        }

        // STAGE 3: Writeback
        d(m::m+bm, n::n+bn) := tileC
      }

    }
    getMem(d)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val M = args(0).to[SInt]
    val N = args(1).to[SInt]

    val init = 1
    val c = Array.fill(M){ Array.fill(N){init} }

    val result = TripleBuf(c.flatten, M, N)

    println("expected cksum: " + M + " * " + N + " * " + init)
    println("result cksum: " + result.map(a => a).reduce{_+_})

  }
}

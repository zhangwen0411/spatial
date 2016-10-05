import spatial.compiler._
import spatial.library._
import spatial.shared._

/*
Load data into tile, then accumulate into this tile
*/

object MultiWr extends SpatialAppCompiler with MultiWrApp
trait MultiWrApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val M = 96
  val N = 96
  def MultiWr(C: Rep[Array[T]], i: Rep[SInt]) = {

    val iters = ArgIn[SInt]
    setArg(iters,i)

    val c = OffChipMem[T](M, N)

    setMem(c, C)

    Accel {

      Sequential(1 by 1) { d =>
        val tileC = BRAM[T](M, N)

        tileC := c(0::M, 0::N, param(1))

        Fold(iters by 1)(tileC, 0.as[T]) { i =>
          val tile_partial = BRAM[T](M,N)
          Pipe(M by 1, N by 1) { (x,y) =>
            tile_partial(x,y) = x+y
          }
          tile_partial
        }{_+_}

        c(0::M, 0::N, param(1)) := tileC
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
    val iters = args(0).to[SInt]

    val init = 1
    val c = Array.fill(M){ Array.fill(N){init} }

    val result = MultiWr(c.flatten, iters)

    val gold = init + iters*(M/2 * ((N-1)*N/2 + (2*M+N-3)*N/2))

    println("expected cksum: " + gold)
    println("result cksum: " + result.map(a => a).reduce{_+_})

    assert(gold == result)
  }
}

import spatial.compiler._
import spatial.library._
import spatial.shared._

object GDA extends SpatialAppCompiler with GDA_App
trait GDA_App extends SpatialApp {
  type T = Flt
  type Array[T] = ForgeArray[T]

  val margin = 1
  val innerPar = 4
  val outerPar = 1
  val MAXC = 96
  val C = MAXC
  val tileSize = 96
  val pLoopPar = 2

  def gda(xCPU: Rep[Array[T]], yCPU: Rep[Array[SInt]], mu0CPU: Rep[Array[T]], mu1CPU: Rep[Array[T]]) = {
    val rTileSize     = tileSize (96 -> 19200)
    val op            = outerPar (1 -> 8)
    val ip            = innerPar (1 -> 12)
    val subLoopPar    = innerPar (1 -> 16)
    val prodLoopPar   = innerPar (1 -> 96)
    val outerAccumPar = innerPar (1 -> 1)

    val rows = yCPU.length;   bound(rows) = 360000
    val cols = mu0CPU.length; bound(cols) = MAXC

    val R = ArgIn[SInt]
    // val C = ArgIn[SInt]
    // setArg(C, cols)
    setArg(R, rows)

    val x     = DRAM[T](R, C)
    val y     = DRAM[SInt](R)
    val mu0   = DRAM[T](C)
    val mu1   = DRAM[T](C)
    val sigma = DRAM[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = SRAM[T](MAXC)
      val mu1Tile = SRAM[T](MAXC)
      Parallel {
        mu0Tile := mu0(0::C par subLoopPar)  // Load mu0
        mu1Tile := mu1(0::C par subLoopPar)  // Load mu1
      }

      val sigmaOut = SRAM[T](MAXC, MAXC)

      Fold(R by rTileSize par op, outerAccumPar)(sigmaOut, 0.as[T]){ r =>
        val gdaYtile = SRAM[SInt](rTileSize)
        val gdaXtile = SRAM[T](rTileSize, MAXC)
        val blk = Reg[SInt]
        Parallel {
          gdaYtile := y(r::r+rTileSize par subLoopPar)
          gdaXtile := x(r::r+rTileSize, 0::C par subLoopPar)  // Load tile of x
          Pipe { blk := min(R.value - r, rTileSize) }
        }

        val sigmaBlk = SRAM[T](MAXC,MAXC)
        Fold(blk par param(1),ip)(sigmaBlk, 0.as[Flt]){rr =>
          val subTile = SRAM[T](MAXC)
          val sigmaTile = SRAM[T](MAXC, MAXC)
          Pipe(C par subLoopPar){ cc =>
            subTile(cc) = gdaXtile(rr,cc) - mux(gdaYtile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Pipe(C by 1, C par ip){ (ii,jj) =>
            sigmaTile(ii,jj) = subTile(ii) * subTile(jj);
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0::C, 0::C par outerAccumPar) := sigmaOut
    }

    getMem(sigma)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }


  def main() {
    val R = args(0).to[SInt]
    // val C = args(0).to[SInt] // TODO: Should be selectable up to maximum

    val x  = Array.fill(R){ Array.fill(C){ random[T](10) }}
    val ys = Array.fill(R){ random[SInt](1) }
    val mu0 = Array.fill(C){ random[T](10) }
    val mu1 = Array.fill(C){ random[T](10) }

    val result = gda(x.flatten, ys, mu0, mu1)

    // val gold = x.zip(ys){ (row, y) =>
    //   val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0){_-_}
    //   Array.tabulate(C){i => Array.tabulate(C){j => sub(i) * sub(j) }}.flatten
    // }.reduce{(a,b) => a.zip(b){_+_}}

    // // println("actual: " + gold.mkString(", "))
    // //println("result: " + result.mkString(", "))
    // // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    // printArr(gold, "gold: ")
    // printArr(result, "result: ")

    // val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    // println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }
}

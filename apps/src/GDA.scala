import spatial.compiler._
import spatial.library._
import spatial.shared._

object GDA extends SpatialAppCompiler with GDA_App
trait GDA_App extends SpatialApp {
  type T = Flt
  type Array[T] = ForgeArray[T]

  val margin = 1
  val innerPar = 2
  val outerPar = 2
  val MAXC = 8
  val tileSize = 4
  val pLoopPar = 1

  def gda(xCPU: Rep[Array[T]], yCPU: Rep[Array[SInt]], mu0CPU: Rep[Array[T]], mu1CPU: Rep[Array[T]]) = {
    val rTileSize     = param(tileSize);  domainOf(rTileSize) = (96, 19200, 1)
    val op            = param(outerPar);  domainOf(op)  = (1, 8, 1)
    val ip            = param(innerPar);  domainOf(ip)  = (1, 12, 1)
    val subLoopPar    = param(innerPar);  domainOf(subLoopPar)    = (1, 16, 1)
    val prodLoopPar   = param(pLoopPar);  domainOf(prodLoopPar)   = (1, 96, 1)
    val outerAccumPar = param(innerPar);  domainOf(outerAccumPar) = (1, 1, 1)

    val rows = yCPU.length;   bound(rows) = 360000
    val cols = mu0CPU.length; bound(cols) = MAXC

    val R = ArgIn[SInt]
    val C = ArgIn[SInt]

    setArg(R, rows)
    setArg(C, cols)

    val x     = OffChipMem[T](R, C)
    val y     = OffChipMem[SInt](R)
    val mu0   = OffChipMem[T](C)
    val mu1   = OffChipMem[T](C)
    val sigma = OffChipMem[T](C, C)

    setMem(x, xCPU)
    setMem(y, yCPU)
    setMem(mu0, mu0CPU)
    setMem(mu1, mu1CPU)

    Accel {
      val mu0Tile = BRAM[T](MAXC)
      val mu1Tile = BRAM[T](MAXC)
      Parallel {
        mu0Tile := mu0(0::C, subLoopPar)  // Load mu0
        mu1Tile := mu1(0::C, subLoopPar)  // Load mu1
      }

      val sigmaOut = BRAM[T](MAXC, MAXC)

      Fold(R by rTileSize par op, outerAccumPar)(sigmaOut, 0.as[T]){ r =>
        val yTile = BRAM[SInt](rTileSize)
        val xTile = BRAM[T](rTileSize, MAXC)
        Parallel {
          yTile := y(r::r+rTileSize, subLoopPar)
          xTile := x(r::r+rTileSize, 0::C, subLoopPar)  // Load tile of x
        }

        val sigmaBlk = BRAM[T](MAXC,MAXC)
        Fold(rTileSize par ip)(sigmaBlk, 0.as[Flt]){rr =>
          val subTile = BRAM[T](MAXC)
          val sigmaTile = BRAM[T](MAXC, MAXC)
          Pipe(C par subLoopPar){ cc =>
            subTile(cc) = xTile(rr,cc) - mux(yTile(rr) == 1, mu1Tile(cc), mu0Tile(cc))
          }
          Pipe(C by 1, C par prodLoopPar){ (ii,jj) =>
            sigmaTile(ii,jj) = subTile(ii) * subTile(jj);
          }
          sigmaTile
        }{_+_}
      }{_+_}

      sigma(0::C, 0::C, prodLoopPar) := sigmaOut
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
    val C = args(1).to[SInt] // TODO: Should be selectable up to maximum

    val x  = Array.fill(R){ Array.fill(C){ random[T](10) }}
    val ys = Array.fill(R){ random[SInt](1) }
    val mu0 = Array.fill(C){ random[T](10) }
    val mu1 = Array.fill(C){ random[T](10) }

    val result = gda(x.flatten, ys, mu0, mu1)

    val gold = x.zip(ys){ (row, y) =>
      val sub = if (y == 1) row.zip(mu1){_-_} else row.zip(mu0){_-_}
      Array.tabulate(C){i => Array.tabulate(C){j => sub(i) * sub(j) }}.flatten
    }.reduce{(a,b) => a.zip(b){_+_}}

    // println("actual: " + gold.mkString(", "))
    //println("result: " + result.mkString(", "))
    // println("Sum of differences: " + gold.zip(result){_-_}.reduce{_+_})
    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    println("PASS: " + cksum  + " (GDA)")

    // assert( result == gold )
  }
}

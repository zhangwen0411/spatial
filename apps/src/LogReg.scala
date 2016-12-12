import spatial.compiler._
import spatial.library._
import spatial.shared._

object LogReg extends SpatialAppCompiler with LogRegApp // Regression (Dense) // Args: 1 384
trait LogRegApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type T = Flt

  val tileSize = 96
  val innerPar = 1
  val outerPar = 1
  val margin = 500
  val dim = 96
  val D = dim

  val A = 1

  def sigmoid(t:Rep[T]) = t//1.as[T]/(exp(-t)+1)

  def logreg(x_in: Rep[Array[T]], y_in: Rep[Array[T]], tt: Rep[Array[T]], n: Rep[SInt], it: Rep[SInt]) = {


    val iters = ArgIn[SInt]
    val N = ArgIn[SInt]
    setArg(iters, it)
    setArg(N, n)

    val BN = tileSize (96 -> 96 -> 9600)
    val PX = 1 (1 -> 1)
    val P1 = innerPar (1 -> 2)
    val P2 = innerPar (1 -> 96)
    val P3 = outerPar (1 -> 96)

    val x = DRAM[T](N, D)
    val y = DRAM[T](N)
    val theta = DRAM[T](D)

    setMem(x, x_in)
    setMem(y, y_in)
    setMem(theta, tt)

    Accel {
      val btheta = SRAM[T](D)

      Sequential(iters by 1) { epoch =>

        Sequential.fold(1 by 1, P2)(btheta){ xx =>
          val gradAcc = SRAM[T](D)
          Pipe(N by BN){ i =>
            val logregX = SRAM[T](BN, D)
            val logregY = SRAM[T](BN)
            Parallel {
              logregX := x(i::i+BN, 0::D par P2)
              logregY := y(i::i+BN par P2)
            }
            Fold(BN par P3, P2)(gradAcc, 0.as[T]){ ii =>
              val pipe2Res = Reg[T]
              val subRam   = SRAM[T](D)

              val dotAccum = Reduce(D par P2)(0.as[T]){ j => logregX(ii,j) * btheta(j) }{_+_}  // read
              Pipe { pipe2Res := (logregY(ii) - sigmoid(dotAccum.value)) }
              Pipe(D par P2) {j => subRam(j) = logregX(ii,j) - pipe2Res.value }
              subRam
            }{_+_}
          }
          gradAcc
        }{(b,g) => b+g*A}

        // Flush gradAcc
        Pipe(D by 1 par P2) { i => gradAcc(i) = 0.as[T]}
      }
      theta(0::D par P2) := btheta // read
    }
    getMem(theta)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val N = args(1).to[SInt]

    val sX = Array.fill(N){ Array.fill(D){ random[T](10.0)} }
    val sY = Array.tabulate(N){ i => i.to[T]}//fill(N)( random[T](10.0) )
    val theta = Array.fill(D) {random[T](1.0) }

    val result = logreg(sX.flatten,sY, theta, N, iters)

    val gold = Array.empty[T](D)
    val ids = Array.tabulate(D){i => i}
    for (i <- 0 until D) {
      gold(i) = theta(i)
    }
    for (i <- 0 until iters) {
      val next = sX.zip(sY) {case (row, y) =>
        // println("sigmoid for " + y + " is " + sigmoid(row.zip(gold){_*_}.reduce{_+_}))
        val sub = y - sigmoid(row.zip(gold){(a,b) =>
          // println("doing " + a + " * " + b + " on row " + y)
          a*b}.reduce{_+_})
        row.map{a =>
          // println("subtraction for " + y + " is " + (a - sub))
          a - sub}
      }.reduce{(a,b) => a.zip(b){_+_}}
      for (i <- 0 until D) {
        gold(i) = gold(i) + next(i)
      }
      // printArr(gold, "gold now")
    }


    printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = result.zip(gold){ (a,b) => a > b-margin && a < b+margin}.reduce{_&&_}
    // println("max err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{Math.max(_,_)})
    // println("mean err: " + result.zip(gold){(a,b) => (a-b)*(a-b)}.reduce{_+_} / D)
    println("PASS: " + cksum  + " (LogReg)")



    /* OptiMl version
    val w = untilconverged(theta, maxIter = 30) { (cur, iter) =>
      val gradient = ((0::x.numRows) { i =>
        x(i)*(y(i) - sigmoid(cur *:* x(i)))
      }).sum
      val z = cur + gradient*alpha
      z
    }
    */

  }
}
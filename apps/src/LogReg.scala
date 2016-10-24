import spatial.compiler._
import spatial.library._
import spatial.shared._

object LogReg extends SpatialAppCompiler with LogRegApp
trait LogRegApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type Elem = Flt
  type T = Flt

  val tileSizeH = 192
  val innerParH = 4
  val outerParH = 2
  val margin = 1

  val A = 1

  def sigmoid(t:Rep[Elem]) = 1.as[Elem]/(exp(-t)+1)

  def logreg(x_in: Rep[Array[Elem]], y_in: Rep[Array[Elem]], tt: Rep[Array[Elem]], n: Rep[SInt], it: Rep[SInt]) = {

    val D = 384

    val iters = ArgIn[SInt]
    val N = ArgIn[SInt]
    setArg(iters, it)
    setArg(N, n)

    val BN = tileSizeH (96 -> 96 -> 9600)
    val PX = 1 (1 -> 1)
    val P0 = outerParH (1 -> 3)
    val P1 = innerParH (1 -> 2)
    val P2 = innerParH (1 -> 96)
    val P3 = 1 (1 -> 96)

    val x = DRAM[Elem](N, D)
    val y = DRAM[Elem](N)
    val theta = DRAM[Elem](D)

    setMem(x, x_in)
    setMem(y, y_in)
    setMem(theta, tt)

    Accel {
      val btheta = SRAM[Elem](D)
      btheta := theta(0::D par P2)

      Sequential(iters by 1) { epoch => 
        val gradAcc = SRAM[Elem](D)
        Fold(N by BN par P0, P1)(gradAcc, 0.as[T]){ i =>
          val xB = SRAM[Elem](BN, D)
          val yB = SRAM[Elem](BN)
          Parallel {
            xB := x(i::i+BN, 0::D par P2)
            yB := y(i::i+BN par P2)
          }
          val gradient = SRAM[Elem](D)
          Fold(BN par P3, P2)(gradient, 0.as[T]){ ii =>
            val pipe2Res = Reg[Elem]
            val subRam   = SRAM[Elem](D)

            val dotAccum = Reduce(D par P2)(0.as[T]){ j => xB(ii,j) * btheta(j) }{_+_}
            Pipe { pipe2Res := (yB(ii) - sigmoid(dotAccum.value)) }
            Pipe(D par P2) {j => subRam(j) = xB(ii,j) - pipe2Res.value }
            subRam
          }{_+_}
        }{_+_}

        Fold (1 by 1 par param(1),P2) (btheta, 0.as[Elem]){ j =>
          gradAcc
        }{case (b,g) => b+g*A}
      }
      theta(0::D par P2) := btheta
    }
    getMem(theta)
  }

  def printArr(a: Rep[Array[Elem]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val iters = args(0).to[SInt]
    val N = args(1).to[SInt]
    val D = 384

    val sX = Array.fill(N){ Array.fill(D){ random[Elem](10.0)} }
    val sY = Array.fill(N)( random[Elem](10.0) )
    val theta = Array.fill(D) {random[Elem](1.0) }

    val result = logreg(sX.flatten,sY, theta, N, iters)

    // val gold = Array.empty[Elem](D)
    val ids = Array.tabulate(D){i => i}
    val all_accums = sX.zip(sY) {case (row, y) => 
      val sub = y - sigmoid(row.zip(theta){_*_}.reduce{_+_})
      row.map{a => a - sub}
    }
    val gold = all_accums.reduce{(a,b) => a.zip(b){_+_}}

    // printArr(gold, "gold: ")
    printArr(result, "result: ")

    val cksum = result.map{ a => a != 0 && a != 1}.reduce{_&&_}
    println("PASS: " + cksum  + " (LogReg) // Just make sure result is something randomish")



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

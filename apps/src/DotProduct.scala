import spatial.compiler._
import spatial.library._
import spatial.shared._

object DotProduct extends SpatialAppCompiler with DotProductApp
trait DotProductApp extends SpatialApp {
  type T = SInt

  val tileSize = 96
  val innerPar = 2
  val outerPar = 2
  type Array[T] = ForgeArray[T]

  def dotproduct(a: Rep[Array[T]], b: Rep[Array[T]]) = {
    val B  = tileSize (96 -> 96 -> 19200)
    val P1 = outerPar (1 -> 6)
    val P2 = innerPar (1 -> 192)
    val P3 = innerPar (1 -> 192)

    val size = a.length
    bound(size) = 1920000

    val N = ArgIn[SInt]
    val out = ArgOut[T]
    setArg(N, size)

    val v1 = DRAM[T](N)
    val v2 = DRAM[T](N)
    setMem(v1, a)
    setMem(v2, b)

    Accel {
      val reg = Reg[T]
      Fold(N by B par P1)(reg, 0.as[T]){ i =>
        val b1 = FIFO[T](B)
        val b2 = FIFO[T](B)
        val bn = Reg[SInt](999)
        Parallel {
          b1 := v1(i::i+B par P3)
          b2 := v2(i::i+B par P3)
          Pipe{ bn := min(N.value - i, B) } // ISSUE #2
        }
        Reduce(bn par P2)(0.as[T]){ii =>
          b1.pop * b2.pop
        }{_+_} // Reg
      }{_+_} // Reg
      Pipe { out := reg }
    }
    getArg(out)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val N = args(0).to[SInt]
    val a = Array.fill(N)(/*random[T](10)*/ 1)
    val b = Array.fill(N)(/*random[T](10)*/ 1)

    // printArr(a, "a")
    // printArr(b, "b")

    val result = dotproduct(a, b)
    val gold = a.zip(b){_*_}.reduce{_+_}
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (DotProduct)")

//    assert(result == gold)
  }
}
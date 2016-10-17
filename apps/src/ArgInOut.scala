import spatial.compiler._
import spatial.library._
import spatial.shared._

object ArgInOut extends SpatialAppCompiler with ArgInOutApp
trait ArgInOutApp extends SpatialApp {

  def main() {

    // Declare SW-HW interface vals
  	val x = ArgIn[SInt]
  	val y = ArgOut[SInt]
    val N = args(0).to[SInt]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      Pipe { y := x + 4 }
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)
  }
}

object StoreTest extends SpatialAppCompiler with StoreTestApp
trait StoreTestApp extends SpatialApp {
  def main() {
    val N = 32

    val src = Array.tabulate[SInt](N) { i => i }

    val dstFPGA = DRAM[SInt](N)
    val srcFPGA = DRAM[SInt](N)

    setMem(srcFPGA, src)

    Accel {
      val f1 = SRAM[SInt](N)
      Pipe{ f1(0) = 1 }
      dstFPGA(0::N) := f1
    }

    val result = getMem(dstFPGA)
    val chksum = src.zip(result){_==_}.reduce{_&&_}
    println("sum: " + chksum)
  }
}
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

import spatial.compiler._
import spatial.library._
import spatial.shared._

/**
 * Odd tests to test the soundness of Spatial regardless of realistic app construction.
 **/

// Testing LMS CSE of (dense) Tiles
object TileCseTest extends SpatialAppCompiler with TileCseTestApp
trait TileCseTestApp extends SpatialApp {
  def main() {
    val dram = OffChipMem[SInt](100, 100)

    setMem(dram, Array.tabulate(100*100){i => i})

    val out  = ArgOut[SInt]
    Accel {
      val bram1 = BRAM[SInt](32, 32)
      val bram2 = BRAM[SInt](16, 16)

      bram1 := dram(4::36,2::34)
      bram2 := dram(16::32,32::48)
      out := bram1(0,0) + bram2(0,0)
    }

    val result = getArg(out)
    assert(result == 2034) // 400 + 2 + 1600 + 32
  }
}

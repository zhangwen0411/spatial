import spatial.compiler._
import spatial.library._
import spatial.shared._

object TileStoreCompiler extends SpatialAppCompiler with TileStoreTest
trait TileStoreTest extends SpatialApp {

  lazy val c = 1
  lazy val p = unit(24)
  lazy val rows = 1000.as[SInt]
  lazy val cols = 1920.as[SInt]

  def stores(v1: Rep[DRAM[Flt]]) {
    val b1 = List.tabulate(c){i => SRAM[Flt](rows, cols) }

    Sequential.foreach(0 until 5000){ i =>
      Parallel {
        b1.foreach{b => v1(0::rows, 0::cols, p) := b }
      }
    }
  }

  def main() {
    val N = args(0).to[SInt]
    val C = args(1).to[SInt]

    bound(N) = 9993600
    bound(C) = 9993600
    val v1 = DRAM[Flt](N, C)

    val vec1 = Array.fill(N)(random[Flt](10))

    setMem(v1, vec1)

    Accel {
      stores(v1)
    }
  }
}

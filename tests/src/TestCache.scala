import spatial.compiler._
import spatial.library._
import spatial.shared._

object TestCacheCompiler extends SpatialAppCompiler with TestCache
object TestCacheInterpreter extends SpatialAppInterpreter with TestCache
trait TestCache extends SpatialApp {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]
    val N = 10

    val v1    = DRAM[Q16](N)
    val v2    = DRAM[Q16](N)
    val v3    = DRAM[Q16](N)
    val outer = ArgOut[Q16]

    val vec1 = Array.fill(N)(random[Q16](N))
    val vec2 = Array.fill(N)(random[Q16](N))
    setMem(v1, vec1)
    setMem(v2, vec2)

    Accel {
      val c1 = Cache[Q16](v1)
      val c2 = Cache[Q16](v2)
      val c3 = Cache[Q16](v3)

      Pipe.reduce(0 until N)(outer){ii => c1(ii) * c2(ii) }{_+_}
      Pipe(N by 1) {ii => c3(ii) = c1(ii) * c2(ii) }
    }

    val goldr = vec1.zip(vec2) {case (e1, e2) => e1*e2}.reduce{_+_}
    val goldm = vec1.zip(vec2) {case (e1, e2) => e1*e2}
    println("outer: " + getArg(outer).mkString + " (should be " + goldr.mkString + ")")
    println("v3: " + getMem(v3).mkString(",") + "\n (should be " + goldm.mkString(",") + ")")
  }
}

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
    val dram = DRAM[SInt](100, 100)

    setMem(dram, Array.tabulate(100*100){i => i})

    val out  = ArgOut[SInt]
    Accel {
      val sram1 = SRAM[SInt](32, 32)
      val sram2 = SRAM[SInt](16, 16)

      sram1 := dram(4::36,2::34)
      sram2 := dram(16::32,32::48)
      out := sram1(0,0) + sram2(0,0)
    }

    val result = getArg(out)
    println("Result =   " + result)
    println("Expected = 2034")
    assert(result == 2034) // 400 + 2 + 1600 + 32
  }
}

object RegWriteTest extends SpatialAppCompiler with RegWriteTestApp
trait RegWriteTestApp extends SpatialApp {
  def main() {
    val in1 = ArgIn[SInt]
    val in2 = ArgIn[Bit]
    val out = ArgOut[SInt]
    setArg(in1, 32)
    setArg(in2, true)
    Accel {
      val reg = Reg[SInt](64)
      reg := mux(in2, in1.value, reg.value)
      out := reg.value
    }
  }
}

object VectorMinTest extends SpatialAppCompiler with VectorMinTestApp
trait VectorMinTestApp extends SpatialApp {
  val B = 16

  def main() {
    val size = 32
    val vec = Array.tabulate(size){i => random[SInt](10) + 16 }

    val N = ArgIn[SInt]
    setArg(N, size)

    val out = ArgOut[SInt]
    val data = DRAM[SInt](N)
    setMem(data, vec)

    Accel {
      val min = Reg[SInt]
      Fold(N by B)(min, 100){i =>
        val fifo = FIFO[SInt](B)
        fifo := data(i::i+B)
        Reduce(B by 1)(100.as[SInt]){j => fifo.pop() }{(a,b) => mux(a < b, a, b) }
      }{(a,b) => mux(a < b, a, b) }

      out := min
    }
    val result = getArg(out)

    println("array = " + vec.mkString(", "))
    println("min = " + result)
  }
}


object ParRegRead extends SpatialAppCompiler with ParRegReadTest
trait ParRegReadTest extends SpatialApp {
  def main() = {
    val P1 = parameter(4)
    val N = ArgIn[SInt]
    val result = ArgOut[SInt]
    setArg(N, 1)

    val n = N.value

    Accel {
      val sum = Fold(12 par P1)(Reg[SInt], 0.as[SInt]){i =>
        val local = Reg[SInt]
        Pipe { local := n }
        local
      }{_+_}
      result := sum.value
    }
    println(getArg(result))
  }
}
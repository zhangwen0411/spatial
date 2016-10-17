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

object LongPipe extends SpatialAppCompiler with LongPipeTest
trait LongPipeTest extends SpatialApp {
  type Array[B] = ForgeArray[B]

  val tileSize = 96
  val outerPar = 1
  val innerPar = 2

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  def CNDF(x: Rep[Flt]) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.as[Flt] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }

  def BlkSchlsEqEuroNoDiv(sptprice: Rep[Flt], strike: Rep[Flt], rate: Rep[Flt],
                          volatility: Rep[Flt], time: Rep[Flt], otype: Rep[UInt]) = {

    val xLogTerm = log( sptprice / strike )
    val xPowerTerm = (volatility ** 2) * 0.5f
    val xNum = (rate + xPowerTerm) * time + xLogTerm
    val xDen = volatility * sqrt(time)

    val xDiv = xNum / (xDen ** 2)
    val nofXd1 = CNDF(xDiv)
    val nofXd2 = CNDF(xDiv - xDen)

    val futureValueX = strike * exp(-rate * time)

    val negNofXd1 = -nofXd1 + 1.0f
    val negNofXd2 = -nofXd2 + 1.0f

    val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    mux(otype == 0, optionPrice2, optionPrice1)
  }

  def blackscholes(
    stypes:      Rep[Array[UInt]],
    sprices:     Rep[Array[Flt]],
    sstrike:     Rep[Array[Flt]],
    srate:       Rep[Array[Flt]],
    svolatility: Rep[Array[Flt]],
    stimes:      Rep[Array[Flt]]
  ): Rep[Array[Flt]] = {
    val B  = tileSize (96 -> 96 -> 19200)
    val OP = outerPar (1 -> 1)
    val IP = innerPar (1 -> 96)

    val size = stypes.length; bound(size) = 9995328

    lazy val N = ArgIn[SInt]
    setArg(N, size)

    val types    = DRAM[UInt](N)
    val prices   = DRAM[Flt](N)
    val strike   = DRAM[Flt](N)
    val rate     = DRAM[Flt](N)
    val vol      = DRAM[Flt](N)
    val times    = DRAM[Flt](N)
    val optprice = DRAM[Flt](N)
    setMem(types, stypes)
    setMem(prices, sprices)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(times, stimes)

    Accel {
      Pipe(N by B par OP) { i =>
        val typeBlk   = SRAM[UInt](B)
        val priceBlk  = SRAM[Flt](B)
        val strikeBlk = SRAM[Flt](B)
        val rateBlk   = SRAM[Flt](B)
        val volBlk    = SRAM[Flt](B)
        val timeBlk   = SRAM[Flt](B)
        val optpriceBlk = SRAM[Flt](B)

        Parallel {
          typeBlk   := types(i::i+B par IP)
          priceBlk  := prices(i::i+B par IP)
          strikeBlk := strike(i::i+B par IP)
          rateBlk   := rate(i::i+B par IP)
          volBlk    := vol(i::i+B par IP)
          timeBlk   := times(i::i+B par IP)
        }

        Pipe(B par IP){ j =>
          val price = BlkSchlsEqEuroNoDiv(priceBlk(j), strikeBlk(j), rateBlk(j), volBlk(j), timeBlk(j), typeBlk(j))
          optpriceBlk(j) = price
        }
        optprice(i::i+B par IP) := optpriceBlk
      }
    }
    getMem(optprice)
  }

  def printArr(a: Rep[ForgeArray[Flt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }
  def main() {
    val N = args(0).to[SInt]

    val types  = Array.fill(N)(random[UInt](2))
    val prices = Array.fill(N)(random[Flt])
    val strike = Array.fill(N)(random[Flt])
    val rate   = Array.fill(N)(random[Flt])
    val vol    = Array.fill(N)(random[Flt])
    val time   = Array.fill(N)(random[Flt])

    val out = blackscholes(types, prices, strike, rate, vol, time)
    printArr(out, "result: ")
  }
}

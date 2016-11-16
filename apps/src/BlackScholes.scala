import spatial.compiler._
import spatial.library._
import spatial.shared._

object BlackScholes extends SpatialAppCompiler with BlackScholesApp
trait BlackScholesApp extends SpatialApp {
  type Array[B] = ForgeArray[B]

  val margin = 0.5f // Validates true if within +/- margin
  val tileSize = 8000
  val outerPar = 1
  val innerPar = 8

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

    /*val inds_array = Array.tabulate(N)(j =>
      // j
      BlkSchlsEqEuroNoDiv(prices(j), strike(j), rate(j), vol(j), time(j), types(j))
    )
    val gold = inds_array*/
    // val gold = (inds_array) map { i =>
    //   val rate = srate(i)
    //   val strike = sstrike(i)
    //   val sptprice = ssptprice(i)
    //   val volatility = svolatility(i)
    //   val time = sotime(i)
    //   val otype = sotype(i)

    //   val xLogTerm = log( sptprice / strike )
    //   val xPowerTerm = (volatility ** 2) * 0.5f
    //   val xNum = (rate + xPowerTerm) * time + xLogTerm
    //   val xDen = volatility * sqrt(time)

    //   val xDiv = xNum / (xDen ** 2)
    //   // CNDF on xDiv
    //   val ax = abs(xDiv)

    //   val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    //   val xK2 = 1.as[Flt] / ((ax * 0.2316419f) + 1.0f)

    //   val xK2_2 = xK2 ** 2
    //   val xK2_3 = xK2_2 * xK2
    //   val xK2_4 = xK2_3 * xK2
    //   val xK2_5 = xK2_4 * xK2

    //   val xLocal_10 = xK2 * 0.319381530f
    //   val xLocal_20 = xK2_2 * -0.356563782f
    //   val xLocal_30 = xK2_3 * 1.781477937f
    //   val xLocal_31 = xK2_4 * -1.821255978f
    //   val xLocal_32 = xK2_5 * 1.330274429f

    //   val xLocal_21 = xLocal_20 + xLocal_30
    //   val xLocal_22 = xLocal_21 + xLocal_31
    //   val xLocal_23 = xLocal_22 + xLocal_32
    //   val xLocal_1 = xLocal_23 + xLocal_10

    //   val xLocal0 = xLocal_1 * xNPrimeofX
    //   val xLocal  = -xLocal0 + 1.0f

    //   mux(xDiv < 0.0f, xLocal0, xLocal)

    //   // val nofXd1 = CNDF(xDiv)
    //   // val nofXd2 = CNDF(xDiv - xDen)

    //   // val futureValueX = strike * exp(-rate * time)

    //   // val negNofXd1 = -nofXd1 + 1.0f
    //   // val negNofXd2 = -nofXd2 + 1.0f

    //   // val optionPrice1 = (sptprice * nofXd1) - (futureValueX * nofXd2)
    //   // val optionPrice2 = (futureValueX * negNofXd2) - (sptprice * negNofXd1)
    //   // mux(otype == 0, optionPrice2, optionPrice1)
    //   // xDiv
    // }
    //printArr(gold, "gold: ")
    printArr(out, "result: ")

    //val cksum = out.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    //println("PASS: " + cksum + " (BlackSholes)")


  }
}

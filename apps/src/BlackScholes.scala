import spatial.compiler._
import spatial.library._
import spatial.shared._

object BlackScholes extends SpatialAppCompiler with BlackScholesApp
trait BlackScholesApp extends SpatialApp {
  // type Array[T] = ForgeArray[T]

  val margin = 0.5f // Validates true if within +/- margin
  val tileSize = 96
  val outerPar = 1
  val innerPar = 2
  lazy val ts = param(tileSize)
  lazy val op = param(outerPar)
  lazy val ip = param(innerPar)
  lazy val numOptions = ArgIn[SInt]

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
    otype:      Rep[DRAM[UInt]],
    sptprice:   Rep[DRAM[Flt]],
    strike:     Rep[DRAM[Flt]],
    rate:       Rep[DRAM[Flt]],
    volatility: Rep[DRAM[Flt]],
    otime:      Rep[DRAM[Flt]],
    optprice:   Rep[DRAM[Flt]]
  ): Rep[Unit] = {

    Pipe((numOptions by ts) par op) { i =>
      val otypeRAM      = SRAM[UInt](ts)
      val sptpriceRAM   = SRAM[Flt](ts)
      val strikeRAM     = SRAM[Flt](ts)
      val rateRAM       = SRAM[Flt](ts)
      val volatilityRAM = SRAM[Flt](ts)
      val otimeRAM      = SRAM[Flt](ts)

      Parallel {
        otypeRAM := otype(i::i+ts, ip)
        sptpriceRAM := sptprice(i::i+ts, ip)
        strikeRAM := strike(i::i+ts, ip)
        rateRAM := rate(i::i+ts, ip)
        volatilityRAM := volatility(i::i+ts, ip)
        otimeRAM := otime(i::i+ts, ip)
      }

      val optpriceRAM = SRAM[Flt](ts)
      Pipe((ts by 1) par ip){ j =>
        val price = BlkSchlsEqEuroNoDiv(sptpriceRAM(j), strikeRAM(j), rateRAM(j), volatilityRAM(j), otimeRAM(j), otypeRAM(j))
        optpriceRAM(j) = price
      }
      optprice(i::i+ts, ip) := optpriceRAM
    }
  }

  def printArr(a: Rep[ForgeArray[Flt]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }
  def main() {
    val N = args(0).to[SInt]

    bound(N) = 9995328
    domainOf(ts) = (96,19200,96)
    domainOf(op) = (1,1,1)
    domainOf(ip) = (1,96,1)

    setArg(numOptions, N)
    val types  = DRAM[UInt](numOptions)
    val prices = DRAM[Flt](numOptions)
    val strike = DRAM[Flt](numOptions)
    val rate   = DRAM[Flt](numOptions)
    val vol    = DRAM[Flt](numOptions)
    val time   = DRAM[Flt](numOptions)
    val optprice = DRAM[Flt](numOptions)

    val sotype      = Array.fill(N)(random[UInt](2))
    val ssptprice   = Array.fill(N)(random[Flt])
    val sstrike     = Array.fill(N)(random[Flt])
    val srate       = Array.fill(N)(random[Flt])
    val svolatility = Array.fill(N)(random[Flt])
    val sotime      = Array.fill(N)(random[Flt])

    setMem(types, sotype)
    setMem(prices, ssptprice)
    setMem(strike, sstrike)
    setMem(rate, srate)
    setMem(vol, svolatility)
    setMem(time, sotime)

    Accel{ blackscholes(types, prices, strike, rate, vol, time, optprice) }

    val out = getMem(optprice)

    val inds_array = Array.tabulate(N)(j =>
      // j
      BlkSchlsEqEuroNoDiv(ssptprice(j), sstrike(j), srate(j), svolatility(j), sotime(j), sotype(j))
    )
    val gold = inds_array
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
    printArr(gold, "gold: ")
    printArr(out, "result: ")

    val cksum = out.zip(gold){ case (o, g) => (g < (o + margin)) && g > (o - margin)}.reduce{_&&_}
    println("PASS: " + cksum + " (BlackSholes)")


  }
}

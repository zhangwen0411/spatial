import spatial.compiler._
import spatial.library._
import spatial.shared._

/*

optiQL
trait TPCHQ6Trait extends TPCHBaseTrait {
  val queryName = "Q6"

  def query() = {
    val lineItems = loadLineItems()
    tic(lineItems.size)

    //FIXME: infix_&& fails to resolve automatically
    val q = lineItems Where (l => infix_&&(l.l_shipdate >= Date("1994-01-01"), infix_&&(l.l_shipdate < Date("1995-01-01"), infix_&&(l.l_discount >= 0.05, infix_&&(l.l_discount <= 0.07, l.l_quantity < 24)))))
    val revenue = q.Sum(l => l.l_extendedprice * l.l_discount)

    toc(revenue)
    println(revenue)
  }
}


SQL:
SELECT
    sum(l_extendedprice * l_discount) as revenue
FROM
    lineitem
WHERE
    l_shipdate >= date '1994-01-01'
    AND l_shipdate < date '1994-01-01' + interval '1' year
    AND l_discount between 0.06 - 0.01 AND 0.06 + 0.01
    AND l_quantity < 24;

*/

object TPCHQ6 extends SpatialAppCompiler with TPCHQ6_App
trait TPCHQ6_App extends SpatialApp {
  type FT = SInt
  type Array[T] = ForgeArray[T]

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val tileSize = 96
  val outerPar = 2
  val innerPar = 2

  def tpchq6(datesIn: Rep[Array[UInt]], quantsIn: Rep[Array[UInt]], disctsIn: Rep[Array[FT]], pricesIn: Rep[Array[FT]]): Rep[FT] = {
    val dataSize = ArgIn[SInt]
    setArg(dataSize, datesIn.length)


    val dates  = DRAM[UInt](dataSize)
    val quants = DRAM[UInt](dataSize)
    val discts = DRAM[FT](dataSize)
    val prices = DRAM[FT](dataSize)
    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE
    val out = ArgOut[FT]

    val ts = param(tileSize);   domainOf(ts) = (96,192000,96)
    val op = param(outerPar);    domainOf(op) = (1,6,1)
    val ip = param(innerPar);    domainOf(ip) = (1,384,1)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn
      val maxDate = maxDateIn

      val acc = Reg[FT]
      Fold(dataSize by ts par op)(acc, 0.as[FT]){ i =>
        val datesTile  = SRAM[UInt](ts)
        val quantsTile = SRAM[UInt](ts)
        val disctsTile = SRAM[FT](ts)
        val pricesTile = SRAM[FT](ts)
        Parallel {
          datesTile  := dates(i::i+ts, ip)
          quantsTile := quants(i::i+ts, ip)
          disctsTile := discts(i::i+ts, ip)
          pricesTile := prices(i::i+ts, ip)
        }
        Reduce(ts par ip)(0.as[FT]){ j =>
          val date  = datesTile(j)
          val disct = disctsTile(j)
          val quant = quantsTile(j)
          val price = pricesTile(j)
          val valid = date > minDate && date < maxDate && disct >= MIN_DISC && disct <= MAX_DISC && quant < 24
          mux(valid, price * disct, 0.0f)
        }{_+_}
      }{_+_}
      Pipe {out := acc}
    }
    getArg(out)
  }

  def printArr(a: Rep[Array[Bit]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }


  def main() {
    val N = args(0).to[SInt]

    val dates  = Array.fill(N){random[UInt](20) + 65}
    val quants = Array.fill(N){random[UInt](25) }
    // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // val prices = Array.fill(N){random[FT] * 1000f}
    val discts = Array.fill(N){random[FT] / 100000}
    val prices = Array.fill(N){random[FT] / 100000}

    val result = tpchq6(dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE  &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC  && discts(i) <= MAX_DISC}
    printArr(conds, "conds: ")

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.0f.as[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (TPCHQ6)")
  }
}

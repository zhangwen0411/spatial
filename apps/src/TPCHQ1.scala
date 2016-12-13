/*import spatial.compiler._
import spatial.library._
import spatial.shared._

object TPCHQ1 extends SpatialAppCompiler with TPCHQ1_App
trait TPCHQ1_App extends SpatialApp {
  type Array[T] = ForgeArray[T]

  val MIN_DATE = 0
  val MAX_DATE = 9999
  val MIN_DISC = 0
  val MAX_DISC = 9999
  val tileSize = 4800
  val outerPar = 2
  val innerPar = 2

  val nKeys = 65536 // Yep. (128 BRAMs)

  def tpchq1(
    flagsIn:  Rep[Array[SInt]], // Char in OptiQL code
    statusIn: Rep[Array[SInt]], // Char in OptiQL code
    datesIn:  Rep[Array[SInt]], // Struct in OptiQL code
    quantsIn: Rep[Array[Flt]],  // Double in OptiQL code
    disctsIn: Rep[Array[Flt]],  // Double in OptiQL code
    pricesIn: Rep[Array[Flt]],  // Double in OptiQL code
    taxesIn:  Rep[Array[Flt]]   // Double in OptiQL code
  ): Rep[Array[Flt]] = {

    val N = ArgIn[SInt]
    setArg(N, datesIn.length)

    val flags  = DRAM[SInt](N)
    val status = DRAM[SInt](N)

    val dates  = DRAM[SInt](N)
    val quants = DRAM[Flt](N)
    val discts = DRAM[Flt](N)
    val prices = DRAM[Flt](N)
    val taxes  = DRAM[Flt](N)

    val sumQtyOut       = DRAM[Flt](nKeys)
    val sumPriceOut     = DRAM[Flt](nKeys)
    val sumDiscPriceOut = DRAM[Flt](nKeys)
    val sumChargeOut    = DRAM[Flt](nKeys)
    val sumDisctOut     = DRAM[Flt](nKeys)
    val countOut        = DRAM[Flt](nKeys)

    val minDateIn = MIN_DATE
    val maxDateIn = MAX_DATE

    val T  = param(tileSize);   domainOf(T) = (96,9600,96)
    val OP = param(outerPar);   domainOf(OP) = (1,1,1)
    val IP = param(innerPar);   domainOf(IP) = (1,384,1)

    setMem(status, statusIn)
    setMem(flags, flagsIn)

    setMem(dates, datesIn)
    setMem(quants, quantsIn)
    setMem(discts, disctsIn)
    setMem(prices, pricesIn)

    Accel {
      val minDate = minDateIn.as[SInt]
      val maxDate = maxDateIn.as[SInt]

      val sumQty       = SRAM[Flt](nKeys)
      val sumPrice     = SRAM[Flt](nKeys)
      val sumDiscPrice = SRAM[Flt](nKeys)
      val sumCharge    = SRAM[Flt](nKeys)
      val sumDisct     = SRAM[Flt](nKeys)
      val count        = SRAM[Flt](nKeys)

      Foreach(N by T par OP){i =>
        val flagsTile  = SRAM[SInt](T)
        val statusTile = SRAM[SInt](T)
        val datesTile  = SRAM[Flt](T)
        val quantsTile = SRAM[Flt](T)
        val disctsTile = SRAM[Flt](T)
        val pricesTile = SRAM[Flt](T)
        val taxesTile  = SRAM[Flt](T)

        // Load all the daters
        Parallel {
          flagsTile  := flags(i::i+T, IP)
          statusTile := status(i::i+T, IP)
          datesTile  := dates(i::i+T, IP)
          quantsTile := quants(i::i+T, IP)
          disctsTile := discts(i::i+T, IP)
          pricesTile := prices(i::i+T, IP)
          taxesTile  := taxes(i::i+T, IP)
        }

        // Do the things
        Pipe(T by 1 par IP){ii =>
          val key = Reg[SInt]
          val disctPrice = Reg[Flt]
          Parallel {
            Pipe{ key := (flagsTile(ii) << 8) + (statusTile(ii)) }

            Pipe{ disctPrice := pricesTile(ii) * (1.0f - disctsTile(ii)) }

            Pipe{  }
          }

        }



      }

      val acc = Reg[FT]
      Fold(N by T par OP)(acc, 0.as[Flt]){ i =>


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


  def main() {
    val N = args(0).to[SInt]
    val max_buckets = args(1).to[SInt]
    // l_returnflag,
    // l_linestatus,
    // sum(l_quantity) as sum_qty,
    // sum(l_extendedprice) as sum_base_price,
    // sum(l_extendedprice * (1 - l_discount)) as sum_disc_price,
    // sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)) as sum_charge,
    // avg(l_quantity) as avg_qty,
    // avg(l_extendedprice) as avg_price,
    // avg(l_discount) as avg_disc,
    // count(*) as count_order

    val returnflags = Array.fill(N){random[SInt](max_buckets)}
    val linestatuses = Array.fill(N){random[SInt](max_buckets)}
    val count = Array.tabulate(N){ i => i }
    val dates  = Array.fill(N){random[SInt](20) + 65}
    val quants = Array.fill(N){random[SInt](25) }
    // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
    // val prices = Array.fill(N){random[FT] * 1000f}
    val discts = Array.fill(N){random[FT] * 10 + 1}
    val prices = Array.fill(N){random[FT] * 1000}

    val result = tpchq1(returnflags, linestatuses, dates, quants, discts, prices)

    // --- software version
    val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE &&
                                       quants(i) < 24 && discts(i) >= MIN_DISC && discts(i) <= MAX_DISC }

    val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.0f.as[FT] }.reduce{_+_}

    println("expected " + gold)
    println("result " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (TPCHQ1)")
  }
}*/

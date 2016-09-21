// import spatial.compiler._
// import spatial.library._
// import spatial.shared._

// /*
// SELECT
//     l_returnflag,
//     l_linestatus,
//     sum(l_quantity) as sum_qty,
//     sum(l_extendedprice) as sum_base_price,
//     sum(l_extendedprice * (1 - l_discount)) as sum_disc_price,
//     sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)) as sum_charge,
//     avg(l_quantity) as avg_qty,
//     avg(l_extendedprice) as avg_price,
//     avg(l_discount) as avg_disc,
//     count(*) as count_order
// FROM
//     lineitem
// WHERE
//     l_shipdate <= date '1998-12-01' - interval '90' day
// GROUP BY
//     l_returnflag,
//     l_linestatus
// ORDER BY
//     l_returnflag,
//     l_linestatus;
// */

// object TPCHQ1 extends SpatialAppCompiler with TPCHQ1_App
// trait TPCHQ1_App extends SpatialApp {
//   type FT = SInt
//   type Array[T] = ForgeArray[T]

//   val MIN_DATE = 0
//   val MAX_DATE = 9999
//   val MIN_DISC = 0
//   val MAX_DISC = 9999
//   val tileSize = 96
//   val outerPar = 2
//   val innerPar = 2

//   def tpchq1(flagsIn: Rep[Array[SInt]], statusIn: Rep[Array[SInt]], datesIn: Rep[Array[SInt]], quantsIn: Rep[Array[SInt]], disctsIn: Rep[Array[FT]], pricesIn: Rep[Array[FT]], linestatusesIn: Rep[Array[FT]]): Rep[FT] = {
//     val dataSize = ArgIn[SInt]
//     setArg(dataSize, datesIn.length)


//     val dates  = OffChipMem[SInt](dataSize)
//     val quants = OffChipMem[SInt](dataSize)
//     val discts = OffChipMem[FT](dataSize)
//     val prices = OffChipMem[FT](dataSize)
//     val linestatuses = OffChipMem[FT](dataSize)
//     val minDateIn = MIN_DATE
//     val maxDateIn = MAX_DATE
//     val out = ArgOut[FT]

//     val ts = param(tileSize);   domainOf(ts) = (96,192000,96)
//     val op = param(outerPar);    domainOf(op) = (1,6,1)
//     val ip = param(innerPar);    domainOf(ip) = (1,384,1)

//     setMem(dates, datesIn)
//     setMem(quants, quantsIn)
//     setMem(discts, disctsIn)
//     setMem(prices, pricesIn)
//     setMem(linestatuses, linestatusesIn)

//     Accel {
//       val minDate = minDateIn
//       val maxDate = maxDateIn

//       val acc = Reg[FT]
//       Fold(dataSize by ts par op)(acc, 0.as[FT]){ i =>
//         val datesTile  = BRAM[SInt](ts)
//         val quantsTile = BRAM[SInt](ts)
//         val disctsTile = BRAM[FT](ts)
//         val pricesTile = BRAM[FT](ts)
//         val linestatusesTile = BRAM[SInt](ts)
//         Parallel {
//           datesTile  := dates(i::i+ts, ip)
//           quantsTile := quants(i::i+ts, ip)
//           disctsTile := discts(i::i+ts, ip)
//           pricesTile := prices(i::i+ts, ip)
//           linestatusesTile := linestatuses(i::i+ts, ip)
//         }
//         Reduce(ts par ip)(0.as[FT]){ j =>
//           val date  = datesTile(j)
//           val disct = disctsTile(j)
//           val quant = quantsTile(j)
//           val price = pricesTile(j)
//           val valid = date > minDate && date < maxDate && disct >= MIN_DISC && disct <= MAX_DISC && quant < 24
//           mux(valid, price * disct, 0.0f)
//         }{_+_}
//       }{_+_}
//       Pipe {out := acc}
//     }
//     getArg(out)
//   }


//   def main() {
//     val N = args(0).to[SInt]
//     val max_buckets = args(1).to[SInt]
//     // l_returnflag,
//     // l_linestatus,
//     // sum(l_quantity) as sum_qty,
//     // sum(l_extendedprice) as sum_base_price,
//     // sum(l_extendedprice * (1 - l_discount)) as sum_disc_price,
//     // sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)) as sum_charge,
//     // avg(l_quantity) as avg_qty,
//     // avg(l_extendedprice) as avg_price,
//     // avg(l_discount) as avg_disc,
//     // count(*) as count_order

//     val returnflags = Array.fill(N){random[SInt](max_buckets)}
//     val linestatuses = Array.fill(N){random[SInt](max_buckets)}
//     val count = Array.tabulate(N){ i => i }
//     val dates  = Array.fill(N){random[SInt](20) + 65}
//     val quants = Array.fill(N){random[SInt](25) }
//     // val discts = Array.fill(N){random[FT] * 0.05f + 0.02f}
//     // val prices = Array.fill(N){random[FT] * 1000f}
//     val discts = Array.fill(N){random[FT] * 10 + 1}
//     val prices = Array.fill(N){random[FT] * 1000}

//     val result = tpchq1(returnflags, linestatuses, dates, quants, discts, prices)

//     // --- software version
//     val conds = Array.tabulate(N){i => dates(i) > MIN_DATE && dates(i) < MAX_DATE &&
//                                        quants(i) < 24 && discts(i) >= MIN_DISC && discts(i) <= MAX_DISC }

//     val gold = Array.tabulate(N){i => if (conds(i)) prices(i) * discts(i) else 0.0f.as[FT] }.reduce{_+_}

//     println("expected " + gold)
//     println("result " + result)
    
//     val cksum = gold == result
//     println("PASS: " + cksum + " (TPCHQ1)")
//   }
// }

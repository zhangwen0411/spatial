// See LICENSE.txt for license details.
package templates

import chisel3._

/**
 * Counter: 1-dimensional counter. Counts upto 'max', each time incrementing
 * by 'stride', beginning at zero.
 * @param w: Word width
 */
class Counter(val par: Int) extends Module {
  val io = IO(new Bundle {
    val max      = UInt(32.W).asInput
    val stride   = UInt(32.W).asInput
    val gap      = UInt(32).asInput
    val out      = Vec(par, UInt(32.W).asOutput)
    val reset  = Bool().asInput
    val enable = Bool().asInput
    val saturate = Bool().asInput
    val done   = Bool().asOutput
    val debug  = UInt(32.W).asOutput
  })

  val base = Module(new FF(32))
  val init = UInt(0)
  base.io.init := init
  base.io.enable := io.reset | io.enable

  val count = base.io.out
  val newval = count + (io.stride * UInt(par)) + io.gap
  val isMax = newval >= io.max
  val next = Mux(isMax, Mux(io.saturate, count, init), newval)
  io.debug := newval + (io.stride * UInt(par-1)) + io.gap
  base.io.in := Mux(io.reset, init, next)

  (0 until par).foreach { i => io.out(i) := count + UInt(i)*io.stride }
  io.done := io.enable & isMax
}

// class CounterReg(val w: Int) extends Module {
//   val io = new Bundle {
//     val data = new Bundle {
//       val max      = UInt(INPUT,  w)
//       val stride   = UInt(INPUT,  w)
//       val out      = UInt(OUTPUT, w)
//     }
//     val control = new Bundle {
//       val reset = Bool(INPUT)
//       val enable = Bool(INPUT)
//       val saturate = Bool(INPUT)
//       val done   = Bool(OUTPUT)
//     }
//   }

//   // Register the inputs
//   val maxReg = Module(new FF(w))
//   maxReg.io.control.enable := Bool(true)
//   maxReg.io.data.in := io.data.max
//   val max = maxReg.io.data.out

//   val strideReg = Module(new FF(w))
//   strideReg.io.control.enable := Bool(true)
//   strideReg.io.data.in := io.data.stride
//   val stride = strideReg.io.data.out

//   val rstReg = Module(new FF(1))
//   rstReg.io.control.enable := Bool(true)
//   rstReg.io.data.in := io.control.reset
//   val rst = rstReg.io.data.out

//   val enableReg = Module(new FF(1))
//   enableReg.io.control.enable := Bool(true)
//   enableReg.io.data.in := io.control.enable
//   val enable = enableReg.io.data.out

//   val saturateReg = Module(new FF(1))
//   saturateReg.io.control.enable := Bool(true)
//   saturateReg.io.data.in := io.control.saturate
//   val saturate = saturateReg.io.data.out

//   // Instantiate counter
//   val counter = Module(new Counter(w))
//   counter.io.data.max := max
//   counter.io.data.stride := stride
//   counter.io.control.enable := enable
//   counter.io.control.reset := rst
//   counter.io.control.enable := enable
//   counter.io.control.saturate := saturate

//   // Register outputs
//   val outReg = Module(new FF(w))
//   outReg.io.control.enable := Bool(true)
//   outReg.io.data.in := counter.io.data.out
//   io.data.out := outReg.io.data.out
//   val doneReg = Module(new FF(1))
//   doneReg.io.control.enable := Bool(true)
//   doneReg.io.data.in := counter.io.control.done
//   io.control.done := doneReg.io.data.out
// }


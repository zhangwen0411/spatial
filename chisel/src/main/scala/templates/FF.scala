package templates

import chisel3._

/**
 * FF: Flip-flop with the ability to set enable and init
 * value as IO
 * @param w: Word width
 */

class FFIO(val w: Int) extends Bundle {
    val in   = UInt(w.W).asInput
    val init = UInt(w.W).asInput
    val enable = Bool().asInput
    val out  = UInt(w.W).asOutput
}

class FF(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Reg(init = io.init)
  ff := Mux(io.enable, io.in, ff)
  io.out := ff
}

class FFNoInit(val w: Int) extends Module {
  val io = IO(new FFIO(w))

  val ff = Module(new FF(w))
  ff.io.in := io.in
  ff.io.enable := io.enable
  ff.io.init := UInt(0, w.W)
  io.out := ff.io.out
}

class TFF() extends Module {
  val io = IO(new Bundle {
    val out = Bool().asOutput
    val enable = Bool().asInput
  })

  val ff = Reg(init = Bool(false))
  ff := Mux(io.enable, ~ff, ff)
  io.out := ff
}



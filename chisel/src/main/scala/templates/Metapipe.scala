// See LICENSE.txt for license details.
package templates

import chisel3._

import scala.collection.mutable.HashMap

class Metapipe(val n: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
      val numIter = UInt(32).asInput
      val stageDone = Vec(n, Bool().asInput)
    }
    val output = new Bundle {
      val done = Bool().asOutput
      val stageEnable = Vec(n, Bool().asOutput)
      val debug1 = UInt(32).asOutput
      val debug2 = UInt(32).asOutput
      val debug3 = UInt(32).asOutput
    }
  })

  // 0: INIT, 1: RESET, 2..2+n-1: stages, n: DONE
  val initState = 0
  val resetState = 1
  val fillState = resetState + 1
  val steadyState = fillState + n - 1
  val drainState = steadyState + 1
  val doneState = drainState+n-1

  val stateFF = Module(new FF(32))
  stateFF.io.input.enable := Bool(true) // TODO: Do we need this line?
  stateFF.io.input.init := UInt(0)
  val state = stateFF.io.output.data

  // Counter for num iterations
  val maxFF = Module(new FF(32))
  maxFF.io.input.enable := io.input.enable
  maxFF.io.input.data := io.input.numIter
  val max = maxFF.io.output.data

  val ctr = Module(new Counter(1))
  ctr.io.input.enable := io.input.enable & io.input.stageDone(0)
  ctr.io.input.reset := (state === UInt(doneState))
  ctr.io.input.saturate := Bool(true)
  ctr.io.input.max := max
  ctr.io.input.stride := UInt(1)

  val cycsSinceDone = Module(new Counter(1))
  cycsSinceDone.io.input.enable := ctr.io.output.extendedDone
  cycsSinceDone.io.input.reset := (state === UInt(doneState))
  cycsSinceDone.io.input.saturate := Bool(true)
  cycsSinceDone.io.input.max := UInt(n)
  cycsSinceDone.io.input.stride := UInt(1)

  val doneClear = Reg(init = UInt(0))
  val doneFF = List.tabulate(n) { i =>
    val ff = Module(new SRFF())
    ff.io.input.set := io.input.stageDone(i)
    ff.io.input.asyn_reset := doneClear
    ff
  }
  val doneMask = doneFF.map { _.io.output.data }


  // // Provide default value for enable and doneClear
  // io.output.stageEnable.foreach { _ := UInt(0) }
  // doneClear := UInt(0)

  when(io.input.enable) {
    when(state === UInt(initState)) {   // INIT -> RESET
      stateFF.io.input.data := UInt(resetState)
    }.elsewhen (state === UInt(resetState)) {  // RESET -> FILL
      stateFF.io.input.data := UInt(fillState)
    }.elsewhen (state < UInt(steadyState)) {  // FILL -> STEADY
      io.output.debug1 := state
      io.output.debug2 := cycsSinceDone.io.output.count(0)
      io.output.debug3 := ctr.io.output.saturated
      for ( i <- fillState until steadyState) {
        val fillStateID = i - fillState
        when((state === UInt(i))) {
          io.output.stageEnable.zip(doneMask).take(fillStateID+1).foreach { 
            case (en, done) => 
              en := ~done// & (i.U >= cycsSinceDone.io.output.count(0))
          }
          io.output.stageEnable.drop(fillStateID+1).foreach { en => en := UInt(0) }
          val doneTree = doneMask.take(fillStateID+1).reduce {_&_}
          doneClear := doneTree

          when (doneTree === 1.U) {
            stateFF.io.input.data := UInt(i+1)
          }.otherwise {
            stateFF.io.input.data := state
          }
        }
      }
    }.elsewhen (state === UInt(steadyState)) {  // STEADY
      io.output.stageEnable.zip(doneMask).foreach { case (en, done) => en := ~done }

      val doneTree = doneMask.reduce {_&_}
      doneClear := doneTree
      when (doneTree === 1.U) {
        when(ctr.io.output.count(0) === (max - UInt(1))) {
          stateFF.io.input.data := UInt(drainState)
        }.otherwise {
          stateFF.io.input.data := state
        }
      }.otherwise {
        stateFF.io.input.data := state
      }
    }.elsewhen (state < UInt(doneState)) {   // DRAIN
      for ( i <- drainState until doneState) {
        val drainStateID = i - drainState
        when (state === UInt(i)) {
          io.output.stageEnable.zip(doneMask).takeRight(n - drainStateID - 1).foreach { case (en, done) => en := ~done }
          io.output.stageEnable.dropRight(n - drainStateID - 1).foreach { en => en := UInt(0) }

          val doneTree = doneMask.takeRight(n - drainStateID - 1).reduce {_&_}
          doneClear := doneTree
          when (doneTree === 1.U) {
            stateFF.io.input.data := UInt(i+1)
          }.otherwise {
            stateFF.io.input.data := state
          }
        }
      }
    }.elsewhen (state === UInt(doneState)) {  // DONE
      stateFF.io.input.data := UInt(initState)
    }.otherwise {
      stateFF.io.input.data := state
    }
  }.otherwise {
    (0 until n).foreach { i => io.output.stageEnable(i) := Bool(false) }
    stateFF.io.input.data := UInt(initState)
  }

  // Output logic
  io.output.done := state === UInt(doneState)
}





// See LICENSE.txt for license details.
package templates

import chisel3._

import scala.collection.mutable.HashMap

class Sequential(val n: Int) extends Module {
  val io = IO(new Bundle {
    val input = new Bundle {
      val enable = Bool().asInput
      val numIter = UInt(32).asInput
      val stageDone = Vec(n, Bool().asInput)
    }
    val output = new Bundle {
      val done = Bool().asOutput
      val stageEnable = Vec(n, Bool().asOutput)
    }
  })

  // 0: INIT, 1: RESET, 2..2+n-1: stages, n: DONE
  val initState = 0
  val resetState = 1
  val firstState = resetState + 1
  val doneState = firstState + n
  val lastState = doneState - 1

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
  ctr.io.input.enable := io.input.enable & io.input.stageDone(lastState-2) // TODO: Is this wrong? It still works...  
  ctr.io.input.reset := (state === UInt(doneState))
  ctr.io.input.saturate := Bool(false)
  ctr.io.input.max := max
  ctr.io.input.stride := UInt(1)
  val iter = ctr.io.output.count(0)

  // Next state logic
//  val nextStateMux = Module(new MuxNOH(n+3, 1))
//  val states = Vec.tabulate(n+3) { i => UInt(i) }
//  val muxSel = Vec.tabulate(n+3) { i =>
//    if (i == initState) {  // INIT enable logic
//     ~io.enable | (io.enable & (state === UInt(doneState)))
//    } else if (i == resetState) { // RESET logic
//      io.enable & (state === UInt(initState))
//    } else if (i == firstState) { // First state
//      io.enable &
//          ((state === UInt(resetState)) |
//           ((state === UInt(lastState)) & io.input.stageDone.last & ~ctr.io.done))
//    } else if (i < 2+n) { // Worker state logic
//      io.enable & (state === UInt(i-2-1)) & io.input.stageDone(i-2-1)
//    } else { // Done state logic
//      io.enable & (state === UInt(lastState)) & ctr.io.done
//    }
//  }
//  nextStateMux.io.ins := states
//  nextStateMux.io.sel := muxSel
  when(io.input.enable) {
    when(state === UInt(initState)) {
      stateFF.io.input.data := UInt(resetState)
    }.elsewhen (state === UInt(resetState)) {
      stateFF.io.input.data := UInt(firstState)
    }.elsewhen (state < UInt(lastState)) {

      // // Safe but expensive way
      // val doneStageId = (0 until n).map { i => // Find which stage got done signal
      //   Mux(io.input.stageDone(i), UInt(i+1), UInt(0)) 
      // }.reduce {_+_}
      // when(state === (doneStageId + 1.U)) {
      //   stateFF.io.input.data := doneStageId + 2.U
      // }.otherwise {
      //   stateFF.io.input.data := state
      // }

      // Less safe but cheap way
      val aStageIsDone = io.input.stageDone.reduce { _ | _ } // TODO: Is it safe to assume children behave properly?
      when(aStageIsDone) {
        stateFF.io.input.data := state + 1.U
      }.otherwise {
        stateFF.io.input.data := state
      }
      
    }.elsewhen (state === UInt(lastState)) {
      when(io.input.stageDone(lastState-2)) {
        when(ctr.io.output.done) {
          stateFF.io.input.data := UInt(doneState)
        }.otherwise {
          stateFF.io.input.data := UInt(firstState)
        }
      }.otherwise {
        stateFF.io.input.data := state
      }

    }.elsewhen (state === UInt(doneState)) {
      stateFF.io.input.data := UInt(initState)
    }.otherwise {
      stateFF.io.input.data := state
    }
  }.otherwise {
    stateFF.io.input.data := UInt(initState)
  }
//  stateFF.io.input.data := nextStateMux.io.out

  // Output logic
  io.output.done := state === UInt(doneState)
  io.output.stageEnable.zipWithIndex.foreach { case (en, i) => en := (state === UInt(i+2)) }
}

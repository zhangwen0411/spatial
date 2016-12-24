// // See LICENSE.txt for license details.
// package templates

// import chisel3._

// import scala.collection.mutable.HashMap

// class Sequential(val n: Int) extends Module {
//   val io = IO(new Bundle {
//     val enable = Bool(INPUT)
//     val done = Bool(OUTPUT)
//     val numIter = UInt(INPUT, width=32)
//     val stageEnable = Vec(n, Bool().asOutput)
//     val stageDone = Vec(n, Bool().asInput)
//   }

//   // 0: INIT, 1: RESET, 2..2+n-1: stages, n: DONE
//   val initState = 0
//   val resetState = 1
//   val firstState = resetState + 1
//   val doneState = firstState + n
//   val lastState = doneState - 1

//   val stateFF = Module(new FF(32))
//   stateFF.io.enable := Bool(true) // TODO: Do we need this line?
//   stateFF.io.init := UInt(0)
//   val state = stateFF.io.out

//   // Counter for num iterations
//   val maxFF = Module(new FF(32))
//   maxFF.io.enable := io.enable
//   maxFF.io.in := io.numIter
//   val max = maxFF.io.out

//   val ctr = Module(new Counter(32))
//   ctr.io.enable := io.enable & io.stageDone(lastState-2)
//   ctr.io.reset := (state === UInt(doneState))
//   ctr.io.saturate := Bool(false)
//   ctr.io.max := max
//   ctr.io.stride := UInt(1)
//   val iter = ctr.io.out

//   // Next state logic
// //  val nextStateMux = Module(new MuxNOH(n+3, 1))
// //  val states = Vec.tabulate(n+3) { i => UInt(i) }
// //  val muxSel = Vec.tabulate(n+3) { i =>
// //    if (i == initState) {  // INIT enable logic
// //     ~io.enable | (io.enable & (state === UInt(doneState)))
// //    } else if (i == resetState) { // RESET logic
// //      io.enable & (state === UInt(initState))
// //    } else if (i == firstState) { // First state
// //      io.enable &
// //          ((state === UInt(resetState)) |
// //           ((state === UInt(lastState)) & io.stageDone.last & ~ctr.io.done))
// //    } else if (i < 2+n) { // Worker state logic
// //      io.enable & (state === UInt(i-2-1)) & io.stageDone(i-2-1)
// //    } else { // Done state logic
// //      io.enable & (state === UInt(lastState)) & ctr.io.done
// //    }
// //  }
// //  nextStateMux.io.ins := states
// //  nextStateMux.io.sel := muxSel
//   when(io.enable) {
//     when(state === UInt(initState)) {
//       stateFF.io.in := UInt(resetState)
//     }.elsewhen (state === UInt(resetState)) {
//       stateFF.io.in := UInt(firstState)
//     }.elsewhen (state < UInt(lastState)) {
//       when((state === UInt(2)) & io.stageDone(0)) {
//         stateFF.io.in := UInt(3)
//       }.elsewhen((state === UInt(3)) & io.stageDone(1)) {
//         stateFF.io.in := UInt(4)
//       }.elsewhen((state === UInt(4)) & io.stageDone(2)) {
//         stateFF.io.in := UInt(5)
//       }.elsewhen((state === UInt(5)) & io.stageDone(3)) {
//         stateFF.io.in := UInt(6)
//       }.elsewhen((state === UInt(6)) & io.stageDone(4)) {
//         stateFF.io.in := UInt(7)
//       }.otherwise {
//         stateFF.io.in := state
//       }
//     }.elsewhen (state === UInt(lastState)) {
//       when(io.stageDone(lastState-2)) {
//         when(ctr.io.done) {
//           stateFF.io.in := UInt(doneState)
//         }.otherwise {
//           stateFF.io.in := UInt(firstState)
//         }
//       }.otherwise {
//         stateFF.io.in := state
//       }

//     }.elsewhen (state === UInt(doneState)) {
//       stateFF.io.in := UInt(initState)
//     }.otherwise {
//       stateFF.io.in := state
//     }
//   }.otherwise {
//     stateFF.io.in := UInt(initState)
//   }
// //  stateFF.io.in := nextStateMux.io.out

//   // Output logic
//   io.done := state === UInt(doneState)
//   io.stageEnable.zipWithIndex.foreach { case (en, i) => en := (state === UInt(i+2)) }
// }

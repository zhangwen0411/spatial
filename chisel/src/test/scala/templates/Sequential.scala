// // See LICENSE.txt for license details.
// package templates

// import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}

// class SequentialTests(c: Sequential) extends PeekPokeTester(c) {
//   val numIter = 5
//   val stageIterCount = List.tabulate(c.numInputs) { i => math.abs(rnd.nextInt) % 10 + 1}
//   println(s"stageIterCount: $stageIterCount")

//   def executeStage(s: Int) {
//     val numCycles = stageIterCount(s)
//     // println(s"[stage $s] Executing for $numCycles")
//     step(numCycles)
//     // println(s"[stage $s] Done")
//     poke(c.io.stageDone(s), 1)
//     step(1)
//     poke(c.io.stageDone(s), 0)
//   }

//   def handleStageEnables = {
//     val stageEnables = c.io.stageEnable.map { peek(_).toInt }
//     val activeStage = stageEnables.indexOf(1)
//     println(s"active stage $activeStage")
//     if (activeStage != -1) executeStage(activeStage)
//   }

//   // Start
//   poke(c.io.numIter, numIter)
//   poke(c.io.enable, 1)

//   var done = peek(c.io.done).toInt
//   var numCycles = 0
//   while ((done != 1) & (numCycles < 100)) {
//     handleStageEnables
//     done = peek(c.io.done).toInt
//     step(1)
//     numCycles += 1
//   }
// }

// class SequentialTester extends ChiselFlatSpec {
//   behavior of "Sequential"
//   backends foreach {backend =>
//     it should s"correctly add randomly generated numbers $backend" in {
//       Driver(() => new Sequential(8))(c => new SequentialTests(c)) should be (true)
//     }
//   }
// }

// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import org.scalatest.Assertions._

class SequentialTests(c: Sequential) extends PeekPokeTester(c) {
  val numIter = 5
  val latencies = (0 until c.n).map { i => math.abs(rnd.nextInt(10)) + 2 } 
  latencies.map { a => println("latency of stage = " + a)}
  val timeout = 500

  def executeStage(s: Int) {
    val numCycles = latencies(s)
    // println(s"[stage $s] Executing for $numCycles")
    step(numCycles)
    // println(s"[stage $s] Done")
    poke(c.io.input.stageDone(s), 1)
    step(1)
    poke(c.io.input.stageDone(s), 0)
  }

  def handleStageEnables = {
    val stageEnables = c.io.output.stageEnable.map { peek(_).toInt }
    val activeStage = stageEnables.indexOf(1)
    // println(s"active stage $activeStage")
    if (activeStage != -1) executeStage(activeStage)
  }

  // Start
  poke(c.io.input.numIter, numIter)
  poke(c.io.input.enable, 1)

  var done = peek(c.io.output.done).toInt
  var numCycles = 0
  while ((done != 1) & (numCycles < timeout)) {
    handleStageEnables
    done = peek(c.io.output.done).toInt
    step(1)
    numCycles += 1
  }
  if ( (numCycles > timeout) | (numCycles < 2) ) {
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  expect(c.io.output.done, 0)

  poke(c.io.input.enable, 0)
  step(5)
  done = peek(c.io.output.done).toInt
  poke(c.io.input.enable, 1)
  numCycles = 0
  while ((done != 1) & (numCycles < timeout)) {
    handleStageEnables
    done = peek(c.io.output.done).toInt
    step(1)
    numCycles += 1
  }
  if ( (numCycles > timeout) | (numCycles < 2) ) {
    expect(c.io.output.done, 999) // TODO: Figure out how to "expect" signals that are not hw IO
  }
  expect(c.io.output.done, 0)

}

class SequentialTester extends ChiselFlatSpec {
  behavior of "Sequential"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Sequential(10))(c => new SequentialTests(c)) should be (true)
    }
  }
}

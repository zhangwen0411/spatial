// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * Counter test harness
 */
class CounterTests(c: Counter) extends PeekPokeTester(c) {
  val saturationVal = 100  // Based on counter width

  val max = saturationVal
  val stride = 6
  val saturate = 1
  val gap = 0

  step(1)
  reset(1)

  poke(c.io.max, max)
  poke(c.io.gap, gap)
  poke(c.io.stride, stride)
  poke(c.io.enable, 1)
  poke(c.io.saturate, saturate)
  poke(c.io.reset, 0)

  var numEnabledCycles = 0
  var expectedCount = 0
  var expectedDone = 0
  def testOneStep() = {
    step(1)
    numEnabledCycles += 1
    val (count, done) = saturate match {
      case 1 =>
        val count = if (numEnabledCycles * (gap + stride*(c.par-1)) < max) (numEnabledCycles * (gap + stride*(c.par))) else (max - max % (gap + stride*(c.par)))
        val done = if (count + c.par*(stride-1) + gap > max) 1 else 0
        (count, done)
      case 0 =>
        val count = 0 // WRONG
        val done = 0 // WRONG
        (count, done)
    }
    (0 until c.par).foreach { i => 
      val a = peek(c.io.out(i))
      println("expect count " + a + " to be " + (count + i*stride) + " from " + numEnabledCycles + " * (" + gap + " + " + stride + " * " + c.par + " -1)")
      expect(c.io.out(i), count + (i * stride)) }
    expect(c.io.done, done)
    expectedCount = count
    expectedDone = done
  }

  println("[reset = 0, enable = 1]")
  (0 until c.par).foreach { i => expect(c.io.out(i), expectedCount + i * stride) }
  expect(c.io.done, expectedDone)

  for (i <- 1 until (5)) {
    testOneStep()
  }

  // println("[reset = 1, enable = 1]")
  // poke(c.io.reset, 1)
  // numEnabledCycles = 0
  // // (0 until c.par).foreach { i => expect(c.io.out(i), expectedCounts.last + i * stride)}
  // expect(c.io.done, 1)
  // for (i <- 0 until 5) {
  //   step(1)
  //   (0 until c.par).foreach { i => expect(c.io.out(i), 0 + i * stride) }
  //   expect(c.io.done, 0)
  // }

  // println("[reset = 1, enable = 0]")
  // poke(c.io.enable, 0)
  // for (i <- 0 until 5) {
  //   step(1)
  //   (0 until c.par).foreach { i => expect(c.io.out(i), 0 + i * stride) }
  //   expect(c.io.done, 0)
  // }

  // println("[reset = 0, enable = 1]")
  // poke(c.io.reset, 0)
  // poke(c.io.enable, 1)
  // (0 until c.par).foreach { i => expect(c.io.out(i), 0 + i * stride) }
  // expect(c.io.done, 0)
  // for (i <- 1 until 5) {
  //   testOneStep()
  // }
  // println("[reset = 0, enable = 0]")
  // poke(c.io.enable, 0)
  // for (i <- 0 until 5) {
  //   step(1)
  //   (0 until c.par).foreach { i => expect(c.io.out(i), expectedCount + i * stride) }
  //   expect(c.io.done, expectedDone)
  // }

  // println("[reset = 0, enable = 1]")
  // poke(c.io.enable, 1)
  // (0 until c.par).foreach { i => expect(c.io.out(i), expectedCount + i * stride) }
  // expect(c.io.done, expectedDone)
  // for (i <- 1 until max+10) {
  //   testOneStep()
  // }
}

class CounterTester extends ChiselFlatSpec {
  behavior of "Counter"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Counter(2))(c => new CounterTests(c)) should be (true)
    }
  }
}

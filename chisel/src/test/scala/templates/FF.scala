// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * FF test harness
 */
class FFTests(c: FF) extends PeekPokeTester(c) {
  val initval = 10
  poke(c.io.input.init, initval)
  step(1)
  reset(1)
  expect(c.io.output.data, initval)

  val numCycles = 15
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.output.data)
    poke(c.io.input.data, i)
    poke(c.io.input.enable, newenable)
    step(1)
    if (newenable == 1) expect(c.io.output.data, i) else expect(c.io.output.data, oldout)
  }
}

class FFNoInitTests(c: FFNoInit) extends PeekPokeTester(c) {
  step(1)
  reset(1)

  val numCycles = 15
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.output.data)
    poke(c.io.input.data, i)
    poke(c.io.input.enable, newenable)
    step(1)
    if (newenable == 1) expect(c.io.output.data, i) else expect(c.io.output.data, oldout)
  }
}

class TFFTests(c: TFF) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  expect(c.io.output.data, 0)
  val numCycles = 20
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.output.data)
    poke(c.io.input.enable, newenable)
    step(1)
    val now = peek(c.io.output.data)
    // Stupid hack because peeking a boolean returns a BigInt which cannot be sliced
    //  and ~0 = -1 and ~1 = -2
    if (newenable == 1 & oldout == 1) {
      expect(c.io.output.data, 0)
    } else if (newenable == 1 & oldout == 0) {
      expect(c.io.output.data, 1)      
    } else {
      expect(c.io.output.data, oldout)
    }
  }
}

class FFTester extends ChiselFlatSpec {
  behavior of "FF"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new FF(32))(c => new FFTests(c)) should be (true)
    }
  }
}

class FFNoInitTester extends ChiselFlatSpec {
  behavior of "FFNoInit"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new FFNoInit(32))(c => new FFNoInitTests(c)) should be (true)
    }
  }
}

class TFFTester extends ChiselFlatSpec {
  behavior of "TFF"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new TFF())(c => new TFFTests(c)) should be (true)
    }
  }
}

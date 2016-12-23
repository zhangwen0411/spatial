// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


/**
 * FF test harness
 */
class FFTests(c: FF) extends PeekPokeTester(c) {
  val initval = 10
  poke(c.io.init, initval)
  step(1)
  reset(1)
  expect(c.io.out, initval)

  val numCycles = 15
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.out)
    poke(c.io.in, i)
    poke(c.io.enable, newenable)
    step(1)
    if (newenable == 1) expect(c.io.out, i) else expect(c.io.out, oldout)
  }
}

class FFNoInitTests(c: FFNoInit) extends PeekPokeTester(c) {
  step(1)
  reset(1)

  val numCycles = 15
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.out)
    poke(c.io.in, i)
    poke(c.io.enable, newenable)
    step(1)
    if (newenable == 1) expect(c.io.out, i) else expect(c.io.out, oldout)
  }
}

class TFFTests(c: TFF) extends PeekPokeTester(c) {
  step(1)
  reset(1)
  expect(c.io.out, 0)
  val numCycles = 20
  for (i <- 0 until numCycles) {
    val newenable = rnd.nextInt(2)
    val oldout = peek(c.io.out)
    poke(c.io.enable, newenable)
    step(1)
    val now = peek(c.io.out)
    // Stupid hack because peeking a boolean returns a BigInt which cannot be sliced
    //  and ~0 = -1 and ~1 = -2
    if (newenable == 1 & oldout == 1) {
      expect(c.io.out, 0)
    } else if (newenable == 1 & oldout == 0) {
      expect(c.io.out, 1)      
    } else {
      expect(c.io.out, oldout)
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

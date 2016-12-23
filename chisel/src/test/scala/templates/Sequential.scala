// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}

class SequentialTests(c: Sequential) extends PeekPokeTester(c) {
  for (t <- 0 until 4) {
    val rnd0 = rnd.nextInt(c.n)
    val rnd1 = rnd.nextInt(c.n)
    val rnd2 = rnd.nextInt(1)

    poke(c.io.A, rnd0)
    poke(c.io.B, rnd1)
    poke(c.io.Cin, rnd2)
    step(1)
    val rsum = rnd0 + rnd1 + rnd2
    val mask = BigInt("1"*c.n, 2)
    expect(c.io.Sum, rsum &  mask)
    expect(c.io.Cout, rsum % 1)
  }
}

class SequentialTester extends ChiselFlatSpec {
  behavior of "Sequential"
  backends foreach {backend =>
    it should s"correctly add randomly generated numbers $backend" in {
      Driver(() => new Sequential(8))(c => new SequentialTests(c)) should be (true)
    }
  }
}

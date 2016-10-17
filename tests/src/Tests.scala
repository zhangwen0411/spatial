import spatial.compiler._
import spatial.library._
import spatial.shared._

object TestCompiler extends SpatialAppCompiler with Test
object TestInterpreter extends SpatialAppInterpreter with Test
trait Test extends SpatialApp {

  def main() {
    type Q16 = FixPt[Signed, B16, B16]
    type A = SInt

    val N = 10
    val T = 5
    val v1 = DRAM[A](N)
    val v2 = DRAM[A](N)
    //val out = DRAM[A](N, N)

    val vec1 = Array.fill(N)(random[A](10))
    setMem(v1, vec1)

    Accel {
      Pipe(N by T) { i =>
        val b1 = SRAM[A](T)
        b1 := v1(i::i+T)
        v2(i::i+T) := b1
      }
    }

  }
}

// Reported bug where zero wasn't being propagated for registers
object Test3Compiler extends SpatialAppCompiler with Test3
trait Test3 extends SpatialApp {
  def main() {
    val xin = 5
    val T = param(4)
    val P = param(2)
    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      Sequential(T by T){ i =>
        val b1 = SRAM[SInt](T)
        Pipe(T par P){ ii =>
          b1(ii) = x.value * ii
        }
        out := Reduce(T par P)(0){ii => b1(ii) }{_+_}
      }
      ()
    }
    getArg(out)
  }
}

object FilterTestCompiler extends SpatialAppCompiler with FilterTest
trait FilterTest extends SpatialApp {
  def main() {
    val N = args(0).to[SInt]
    val B = param(10)

    val data = DRAM[SInt](N)
    val size = ArgIn[SInt]
    val sum = ArgOut[SInt]

    val vec = Array.tabulate(N){i => i}

    println(vec.mkString(","))

    setArg(size, N)
    setMem(data, vec)

    Accel {
      Pipe.fold(size by B par unit(1))(sum){i =>
        val tile = SRAM[SInt](B)
        val fifo = FIFO[SInt](B)
        tile := data(i::i+B)

        Pipe(B by 1 par unit(2)){j =>
          val x = tile(j)
          fifo.push(x, x % 2 == 0)
        }

        val innerSum = Reg[SInt]
        Pipe.reduce(fifo.count by 1 par unit(2))(innerSum){j =>
          fifo.pop()
        }{_+_}
      }{_+_}
    }
    val result = getArg(sum)
    println("result = " + result)
  }
}


// TODO: Eventually this should be banked properly (mem should be duplicated but banked?)
object UnrollTest1Compiler extends SpatialAppCompiler with UnrollTest1
trait UnrollTest1 extends SpatialApp {
  def main() {
    val N = 16
    val out = ArgOut[SInt]

    Accel {
      out := Reduce(N by 1 par unit(2))(0){i =>
        val mem = SRAM[SInt](32)
        mem(i)
      }{_+_}
    }
  }
}

object BankingTest0Compiler extends SpatialAppCompiler with BankingTest0
trait BankingTest0 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(i, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest1Compiler extends SpatialAppCompiler with BankingTest1
trait BankingTest1 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          // Contrived example:
          // Total parallelization relative to memory is 16, but par of j is only 8
          mem(j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest2Compiler extends SpatialAppCompiler with BankingTest2
trait BankingTest2 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(0, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest3Compiler extends SpatialAppCompiler with BankingTest3
trait BankingTest3 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(j, j)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest4Compiler extends SpatialAppCompiler with BankingTest4
trait BankingTest4 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(i, i)
        }{_+_}
      }{_+_}
    }
  }
}

object BankingTest5Compiler extends SpatialAppCompiler with BankingTest5
trait BankingTest5 extends SpatialAppCompiler {
  def main() {
    Accel {
      val mem = SRAM[SInt](8, 8)
      val out = Reg[SInt]
      Fold(8 by 1 par unit(2))(out, 0){i =>
        Reduce(8 by 1 par unit(8))(0){j =>
          mem(j, i)
        }{_+_}
      }{_+_}
    }
  }
}

// Previously having issue with primitive operations being code motioned out of Accel
// despite the fact that all
object LiftTestCompiler extends SpatialAppCompiler with LiftTest
trait LiftTest extends SpatialApp {
  type Array[T] = ForgeArray[T]

  def simpleseq(xin: Rep[SInt], yin: Rep[SInt]) = {
    val innerPar = param("innerPar", 1); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val x = ArgIn[SInt]
    val y = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)
    setArg(y, yin)

    Accel {
      val b1 = SRAM[SInt](tileSize)
      Sequential (tileSize by tileSize) { i =>
        Pipe.foreach(tileSize par innerPar) { ii =>
          b1(ii) = x.value * ii
        }
        Pipe { out := b1(y) }
      }
      ()
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]
    val y = args(unit(1)).to[SInt]

    val result = simpleseq(x, y)

    val b1 = Array.tabulate[SInt](96) { i => x * i }
    val gold = b1(y)
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object LiftTest2Compiler extends SpatialAppCompiler with LiftTest2
trait LiftTest2 extends SpatialApp {
  def main() {
    val tileSize = param(32)
    val out = ArgOut[SInt]
    Accel {
      out := 16.as[SInt] * tileSize
    }
    println(getArg(out))
  }
}


object LiftTest3Compiler extends SpatialAppCompiler with LiftTest3
trait LiftTest3 extends SpatialApp {
  def main() {
    val T = param(32)
    val P = param(16)
    val out = ArgOut[SInt]
    Accel {
      out := 32.as[SInt] * T * P
    }
    println(getArg(out))
  }
}

object ParLoadCompiler extends SpatialAppCompiler with ParLoadTest
trait ParLoadTest extends SpatialApp {
  def main() {
    val P1 = param(4)
    val P2 = param(2)
    val P3 = param(2)
    val mem = DRAM[SInt](32,32)
    val out = ArgOut[SInt]
    Accel {
      val sram = SRAM[SInt](32,32)
      sram := mem(0::32, 0::32, P1)

      Fold(32 par P2)(out, 0.as[SInt]){i =>
        Reduce(32 par P3)(0.as[SInt]){j =>
          sram(i,j)
        }{_+_}
      }{_+_}
      ()
    }
    println(getArg(out))
  }
}

import spatial.compiler._
import spatial.library._
import spatial.shared._

// 1
object SimpleSequentialTest extends SpatialAppCompiler with SimpleSequential
trait SimpleSequential extends SpatialApp {
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
      Pipe(tileSize par innerPar){ ii =>
        b1(ii) = x.value * ii
      }
      Pipe { out := b1(y) }
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

// 2
object ArgInOutTest extends SpatialAppCompiler with ArgInOut
trait ArgInOut extends SpatialApp {

  def main() {
    val N = 8
  	val x = ArgIn[SInt]
  	val y = ArgOut[SInt]
    setArg(x, N)
    Accel {
      Pipe { y := x + 4 }
    }
    val result = getArg(y)
    println("result = " + result)
  }
}

// 3
object DeviceMemcpyTest extends SpatialAppCompiler with DeviceMemcpy
trait DeviceMemcpy extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]

  val N = 192
  def memcpyViaFPGA(srcHost: Rep[Array[T]]) = {
    val fpgamem = DRAM[SInt](N)
    setMem(fpgamem, srcHost)

  	val y = ArgOut[SInt]
    Accel { Pipe { y := 10 } }

    getMem(fpgamem)
  }

  def main() {
    val arraySize = N
    val c = args(unit(0)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i*c }
    val dst = memcpyViaFPGA(src)

    println("src")
    (0 until arraySize) foreach { i => print(src(i) + " ") }
    println("dst")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

//    println("dst"); println(dst.mkString(" "))
  }

}

// 4
object SimpleTileLoadStoreTest extends SpatialAppCompiler with SimpleTileLoadStore
trait SimpleTileLoadStore extends SpatialApp {
  type T = SInt
  val N = 192
  type Array[T] = ForgeArray[T]
  def simpleLoadStore(srcHost: Rep[Array[T]], value: Rep[SInt]) = {
    val loadPar = param("loadPar", 1); domainOf(loadPar) = (1, 1, 1)
    val storePar = param("storePar", 1); domainOf(storePar) = (1, 1, 1)
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val srcFPGA = DRAM[SInt](N)
    val dstFPGA = DRAM[SInt](N)
    setMem(srcFPGA, srcHost)

  	val size = ArgIn[SInt]
  	val x = ArgIn[SInt]
    setArg(x, value)
    setArg(size, N)
    Accel {
      val b1 = SRAM[SInt](tileSize)
      Sequential(size by tileSize) { i =>
        b1 := srcFPGA(i::i+tileSize)

        val b2 = SRAM[SInt](tileSize)
        Pipe (tileSize by 1) { ii =>
          b2(ii) = b1(ii) * x
        }

        dstFPGA(i::i+tileSize) := b2
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = N
    val value = args(unit(0)).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = simpleLoadStore(src, value)

    val gold = src.map { _ * value }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

// 5
object FifoLoadTest extends SpatialAppCompiler with FifoLoad
trait FifoLoad extends SpatialApp {
  type T = SInt
  val N = 192

  type Array[T] = ForgeArray[T]
  def fifoLoad(srcHost: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val srcFPGA = DRAM[SInt](N)
    val dstFPGA = DRAM[SInt](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Sequential {
        f1 := srcFPGA(0::tileSize)
        val b1 = SRAM[SInt](tileSize)
        Pipe(tileSize by 1) { i =>
          b1(i) = f1.pop
        }
        dstFPGA(0::tileSize) := b1
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = N

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = fifoLoad(src)

    val gold = src.map { i => i }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

// 6
object ParFifoLoadTest extends SpatialAppCompiler with ParFifoLoad
trait ParFifoLoad extends SpatialApp {
  type T = SInt
  val N = 192

  type Array[T] = ForgeArray[T]
  def parFifoLoad(src1: Rep[Array[T]], src2: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val src1FPGA = DRAM[T](N)
    val src2FPGA = DRAM[T](N)
    val in = ArgIn[T]
    val out = ArgOut[T]
    setArg(in, N)
    setMem(src1FPGA, src1)
    setMem(src2FPGA, src2)

    Accel {
      val f1 = FIFO[T](tileSize)
      val f2 = FIFO[T](tileSize)
      Pipe (in by tileSize) { i =>
        Parallel {
          f1 := src1FPGA(i::i+tileSize)
          f2 := src2FPGA(i::i+tileSize)
        }
        val accum = Reduce(tileSize by 1)(0.as[T]) { i =>
          f1.pop() * f2.pop()
        }{_+_}
        Pipe { out := accum }
      }
      ()
    }
    getArg(out)
  }

  def main() {
    val arraySize = args(unit(0)).to[SInt]

    val src1 = Array.tabulate[SInt](arraySize) { i => i }
    val src2 = Array.tabulate[SInt](arraySize) { i => i*2 }
    val out = parFifoLoad(src1, src2)

//    val gold = ((arraySize - 96 until arraySize)) map { i => src1(i) * src2(i) }.reduce{_+_}
//    println(s"out = " + out + ",gold = " + gold)
    println(s"out = " + out)

//    assert(out == gold)
  }
}

// 7
object FifoLoadStoreTest extends SpatialAppCompiler with FifoLoadStore
trait FifoLoadStore extends SpatialApp {
  type T = SInt
  val N = 192

  type Array[T] = ForgeArray[T]
  def fifoLoadStore(srcHost: Rep[Array[T]]) = {
    val tileSize = param("tileSize", 96); domainOf(tileSize) = (96, 96, 96)

    val srcFPGA = DRAM[SInt](N)
    val dstFPGA = DRAM[SInt](N)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Parallel {
        Sequential {
          f1 := srcFPGA(0::tileSize)
          dstFPGA(0::tileSize) := f1
        }
        Pipe(tileSize by 1) { i => }
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = N

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = fifoLoadStore(src)

    val gold = src.map { i => i }

    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    (0 until arraySize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

// 8
object SimpleReduceTest extends SpatialAppCompiler with SimpleReduce
trait SimpleReduce extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]

  val N = 96.as[SInt]

  def simpleReduce(xin: Rep[SInt]) = {
    val P = param(8)

    val x = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)

    Accel {
      out := Reduce(N by 1)(0.as[T]) { ii =>
        x.value * ii
      }{_+_}
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[SInt]

    val result = simpleReduce(x)

    val gold = Array.tabulate[SInt](N){i => x * i}.reduce{_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

object SimpleUnitTest extends SpatialAppCompiler with SimpleUnit
trait SimpleUnit extends SpatialApp {
  val N = 96.as[SInt]

  final val inv_sqrt_2xPI = 0.39894228040143270286f

  def CNDF(x: Rep[Flt]) = {
    val ax = abs(x)

    val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
    val xK2 = 1.as[Flt] / ((ax * 0.2316419f) + 1.0f)

    val xK2_2 = xK2 ** 2
    val xK2_3 = xK2_2 * xK2
    val xK2_4 = xK2_3 * xK2
    val xK2_5 = xK2_4 * xK2

    val xLocal_10 = xK2 * 0.319381530f
    val xLocal_20 = xK2_2 * -0.356563782f
    val xLocal_30 = xK2_3 * 1.781477937f
    val xLocal_31 = xK2_4 * -1.821255978f
    val xLocal_32 = xK2_5 * 1.330274429f

    val xLocal_21 = xLocal_20 + xLocal_30
    val xLocal_22 = xLocal_21 + xLocal_31
    val xLocal_23 = xLocal_22 + xLocal_32
    val xLocal_1 = xLocal_23 + xLocal_10

    val xLocal0 = xLocal_1 * xNPrimeofX
    val xLocal  = -xLocal0 + 1.0f

    mux(x < 0.0f, xLocal0, xLocal)
  }

  def simpleUnit(xin: Rep[Flt]) = {
    val x = ArgIn[Flt]
    val out = ArgOut[Flt]
    setArg(x, xin)

    Accel {
      out := CNDF(x)
    }
    getArg(out)
  }

  def main() {
    val x = args(unit(0)).to[Flt]

    val result = simpleUnit(x)
    println(result)
  }
}

// 9
object NiterTest extends SpatialAppCompiler with Niter
trait Niter extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96


  def nIterTest(len: Rep[SInt]) = {
    val innerPar = param("innerPar", 8); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)
    bound(len) = 9216

    val N = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(N, len)

    Accel {
      Sequential {
        Sequential (N by tileSize) { i =>
          val accum = Reduce (tileSize par innerPar)(0.as[T]) { ii =>
            i + ii
          } {_+_}
          Pipe { out := accum }
        }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val len = args(unit(0)).to[SInt]

    val result = nIterTest(len)

    val b1 = Array.tabulate[SInt](len) { i => i }

    val gold = b1.reduce {_+_} - ((len-constTileSize) * (len-constTileSize-1))/2
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

// 10
object SimpleFoldTest extends SpatialAppCompiler with SimpleFold
trait SimpleFold extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96
  val N = 192


  def simple_fold(src: Rep[Array[T]]) = {
    val innerPar = param("innerPar", 8); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param("tileSize", constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)
    val len = src.length; bound(len) = 9216

    val N = ArgIn[T]
    val out = ArgOut[T]
    setArg(N, len)

    val v1 = DRAM[T](N)
    setMem(v1, src)

    Accel {
      Sequential {
        val accum = Reg[T]
        Fold (N by tileSize)(accum, 0.as[T]) { i =>
          val b1 = SRAM[T](tileSize)
          b1 := v1(i::i+tileSize)
          Reduce (tileSize par innerPar)(0.as[T]) { ii =>
            b1(ii)
          } {_+_}
        } {_+_}
        Pipe { out := accum }
      }
      ()
    }

    getArg(out)
  }

  def main() {
    val len = N

    val src = Array.tabulate[T](len) { i => i }
    val result = simple_fold(src)

    val gold = src.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)
    assert(result == gold)
  }
}

// 11
object Memcpy2DTest extends SpatialAppCompiler with Memcpy2D
trait Memcpy2D extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val R = 96
  val C = 96

  def memcpy_2d(src: Rep[ForgeArray[T]], rows: Rep[SInt], cols: Rep[SInt]) = {
    val tileDim1 = param(2);
    val tileDim2 = param(96);  domainOf(tileDim2) = (96, 96, 96)

    val rowsIn = rows
    val colsIn = cols

    val srcFPGA = DRAM[T](rows, cols)
    val dstFPGA = DRAM[T](rows, cols)

    // Transfer data and start accelerator
    setMem(srcFPGA, src)

    Accel {
      Sequential(rowsIn by tileDim1, colsIn by tileDim2) { (i,j) =>
        val tile = SRAM[T](tileDim1, tileDim2)
        tile := srcFPGA(i::i+tileDim1, j::j+tileDim2)
        dstFPGA (i::i+tileDim1, j::j+tileDim2) := tile
      }
    }
    getMem(dstFPGA)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val rows = R
    val cols = C
    val src = Array.tabulate(rows*cols) { i => i }

    val dst = memcpy_2d(src, rows, cols)

    printArr(src, "src:")
    printArr(dst, "dst:")
    (0 until rows*cols) foreach { i => assert(dst(i) == src(i)) }
  }
}

// 12
object BlockReduce1DTest extends SpatialAppCompiler with BlockReduce1D
trait BlockReduce1D extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val N = 192

  val tileSize = 96

  def blockreduce_1d(src: Rep[ForgeArray[T]], size: Rep[SInt]) = {

    val sizeIn = ArgIn[SInt]; setArg(sizeIn, size)

    val srcFPGA = DRAM[T](size)
    val dstFPGA = DRAM[T](tileSize)

    setMem(srcFPGA, src)

    Accel {
      val accum = SRAM[T](tileSize)
      Fold (sizeIn by tileSize)(accum, 0.as[T]) { i  =>
        val tile = SRAM[T](tileSize)
        tile := srcFPGA(i::i+tileSize)
        tile
      }{_+_}
      dstFPGA (0::tileSize) := accum
    }
    getMem(dstFPGA)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val size = N
    val src = Array.tabulate(size) { i => i }

    val dst = blockreduce_1d(src, size)

//    val gold = Array.tabulate(tileSize) { i =>
////      {for(ii <- i until size by tileSize)  yield src(ii) }.reduce{_+_}
//      (i until size by tileSize).map { ii => src(ii) }.reduce{_+_}
//    }

    printArr(src, "src:")
    printArr(dst, "dst:")
//    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}



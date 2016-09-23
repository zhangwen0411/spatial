import spatial.compiler._
import spatial.library._
import spatial.shared._

object SimpleSequential extends SpatialAppCompiler with SimpleSequentialApp // Args: 5 8
trait SimpleSequentialApp extends SpatialApp {
  type Array[T] = ForgeArray[T]

  def simpleseq(xin: Rep[SInt], yin: Rep[SInt]) = {
    val innerPar = param(4); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param(96); domainOf(tileSize) = (96, 96, 96)

    val x = ArgIn[SInt]
    val y = ArgIn[SInt]
    val out = ArgOut[SInt]
    setArg(x, xin)
    setArg(y, yin)

    Accel {
      val b1 = BRAM[SInt](tileSize)
      Pipe(tileSize by 1 par innerPar){ ii =>
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
    val cksum = result == gold
    println("PASS: " + cksum + " (SimpleSeq)")

  }
}


// 3
object DeviceMemcpy extends SpatialAppCompiler with DeviceMemcpyApp // Args: 5
trait DeviceMemcpyApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]

  val N = 192
  def memcpyViaFPGA(srcHost: Rep[Array[T]]) = {
    val fpgamem = OffChipMem[SInt](N)
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

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(src(i) + " ") }
    println("Got out: ")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(src){_ == _}.reduce{_&&_}
    println("PASS: " + cksum  + " (DeviceMemcpy)")





//    println("dst"); println(dst.mkString(" "))
  }

}

object SimpleTileLoadStore extends SpatialAppCompiler with SimpleTileLoadStoreApp // Args: 960 5
trait SimpleTileLoadStoreApp extends SpatialApp {
  type T = SInt
  val N = 192
  type Array[T] = ForgeArray[T]
  def simpleLoadStore(srcHost: Rep[Array[T]], value: Rep[SInt]) = {
    val loadPar = param(1); domainOf(loadPar) = (1, 1, 1)
    val storePar = param(1); domainOf(storePar) = (1, 1, 1)
    val tileSize = param(96); domainOf(tileSize) = (96, 96, 96)

    val srcFPGA = OffChipMem[SInt](N)
    val dstFPGA = OffChipMem[SInt](N)
    setMem(srcFPGA, srcHost)

  	val size = ArgIn[SInt]
  	val x = ArgIn[SInt]
    setArg(x, value)
    setArg(size, N)
    Accel {
      val b1 = BRAM[SInt](tileSize)
      Sequential(size by tileSize) { i =>
        b1 := srcFPGA(i::i+tileSize)

        val b2 = BRAM[SInt](tileSize)
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

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("Got out: ")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (SimpleTileLoadStore)")
  }
}

// 5
object FifoLoad extends SpatialAppCompiler with FifoLoadApp // Args: 960
trait FifoLoadApp extends SpatialApp {
  type T = SInt

  type Array[T] = ForgeArray[T]
  def fifoLoad(srcHost: Rep[Array[T]], N: Rep[SInt]) = {
    val tileSize = param(96); domainOf(tileSize) = (96, 96, 96)

  	val size = ArgIn[SInt]
  	setArg(size, N)

    val srcFPGA = OffChipMem[SInt](size)
    val dstFPGA = OffChipMem[SInt](size)
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Sequential(size by tileSize) { i =>
        f1 := srcFPGA(i::i + tileSize)
        val b1 = BRAM[SInt](tileSize)
        Pipe(tileSize by 1) { i =>
          b1(i) = f1.pop
        }
        dstFPGA(i::i + tileSize) := b1
      }
      ()
    }
    getMem(dstFPGA)
  }

  def main() {
    val arraySize = args(0).to[SInt]

    val src = Array.tabulate[SInt](arraySize) { i => i }
    val dst = fifoLoad(src, arraySize)

    val gold = src.map { i => i }

    println("Sent in: ")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("Got out:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (FifoLoadTest)")


  }
}

// 6
object ParFifoLoad extends SpatialAppCompiler with ParFifoLoadApp // Args: 960
trait ParFifoLoadApp extends SpatialApp {
  type T = SInt

  type Array[T] = ForgeArray[T]
  def parFifoLoad(src1: Rep[Array[T]], src2: Rep[Array[T]], in: Rep[SInt]) = {
    val tileSize = param(96); domainOf(tileSize) = (96, 96, 96)

    val N = ArgIn[T]
    setArg(N, in)

    val src1FPGA = OffChipMem[T](N)
    val src2FPGA = OffChipMem[T](N)
    val out = ArgOut[T]
    setMem(src1FPGA, src1)
    setMem(src2FPGA, src2)

    Accel {
      val f1 = FIFO[T](tileSize)
      val f2 = FIFO[T](tileSize)
      Pipe (N by tileSize) { i =>
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
    val out = parFifoLoad(src1, src2, arraySize)

    val sub1_for_check = Array.tabulate[SInt](arraySize-96) {i => i}
    val sub2_for_check = Array.tabulate[SInt](arraySize-96) {i => i*2}

    // val gold = src1.zip(src2){_*_}.zipWithIndex.filter( (a:Int, i:Int) => i > arraySize-96).reduce{_+_}
    val gold = src1.zip(src2){_*_}.reduce{_+_} - sub1_for_check.zip(sub2_for_check){_*_}.reduce(_+_)
	println(s"gold = " + gold)
    println(s"out = " + out)

    val cksum = out == gold
    println("PASS: " + cksum + " (ParFifoLoad)")
  }
}

object FifoLoadStore extends SpatialAppCompiler with FifoLoadStoreApp // Args:
trait FifoLoadStoreApp extends SpatialApp {
  type T = SInt
  val N = 192

  type Array[T] = ForgeArray[T]
  def fifoLoadStore(srcHost: Rep[Array[T]]) = {
    val tileSize = N

    val srcFPGA = OffChipMem[SInt](N)
    val dstFPGA = OffChipMem[SInt](N)
    val dummyOut = ArgOut[SInt]
    setMem(srcFPGA, srcHost)

    Accel {
      val f1 = FIFO[SInt](tileSize)
      Parallel {
        Sequential {
          f1 := srcFPGA(0::tileSize)
          dstFPGA(0::tileSize) := f1
        }
        Pipe(tileSize by 1) { i =>
          dummyOut := i
        }
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

    println("gold:")
    (0 until arraySize) foreach { i => print(gold(i) + " ") }
    println("")
    println("dst:")
    (0 until arraySize) foreach { i => print(dst(i) + " ") }
    println("")

    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (FifoLoadStore)")

  }
}

object SimpleReduce extends SpatialAppCompiler with SimpleReduceApp // Args: 72
trait SimpleReduceApp extends SpatialApp {
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

    val cksum = gold == result
    println("PASS: " + cksum + " (SimpleReduce)")
  }
}

// object SimpleUnitTest extends SpatialAppCompiler with SimpleUnit // Args:
// trait SimpleUnit extends SpatialApp {
//   val N = 96.as[SInt]

//   final val inv_sqrt_2xPI = 0.39894228040143270286f

//   def CNDF(x: Rep[Flt]) = {
//     val ax = abs(x)

//     val xNPrimeofX = exp((ax ** 2) * -0.05f) * inv_sqrt_2xPI
//     val xK2 = 1.as[Flt] / ((ax * 0.2316419f) + 1.0f)

//     val xK2_2 = xK2 ** 2
//     val xK2_3 = xK2_2 * xK2
//     val xK2_4 = xK2_3 * xK2
//     val xK2_5 = xK2_4 * xK2

//     val xLocal_10 = xK2 * 0.319381530f
//     val xLocal_20 = xK2_2 * -0.356563782f
//     val xLocal_30 = xK2_3 * 1.781477937f
//     val xLocal_31 = xK2_4 * -1.821255978f
//     val xLocal_32 = xK2_5 * 1.330274429f

//     val xLocal_21 = xLocal_20 + xLocal_30
//     val xLocal_22 = xLocal_21 + xLocal_31
//     val xLocal_23 = xLocal_22 + xLocal_32
//     val xLocal_1 = xLocal_23 + xLocal_10

//     val xLocal0 = xLocal_1 * xNPrimeofX
//     val xLocal  = -xLocal0 + 1.0f

//     mux(x < 0.0f, xLocal0, xLocal)
//   }

//   def simpleUnit(xin: Rep[Flt]) = {
//     val x = ArgIn[Flt]
//     val out = ArgOut[Flt]
//     setArg(x, xin)

//     Accel {
//       out := CNDF(x)
//     }
//     getArg(out)
//   }

//   def main() {
//     val x = args(unit(0)).to[Flt]

//     val result = simpleUnit(x)
//     println(result)
//   }
// }

object Niter extends SpatialAppCompiler with NiterApp // Args: 9216
trait NiterApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96


  def nIterTest(len: Rep[SInt]) = {
    val innerPar = param(8); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param(constTileSize); domainOf(constTileSize) = (constTileSize, constTileSize, constTileSize)
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

    val cksum = gold == result
    println("PASS: " + cksum + " (Niter)")
  }
}

object SimpleFold extends SpatialAppCompiler with SimpleFoldApp // Args: 1920
trait SimpleFoldApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val constTileSize = 96
  val N = 192

  def simple_fold(src: Rep[Array[T]]) = {
    val innerPar = param(8); domainOf(innerPar) = (1, 1, 1)
    val tileSize = param(constTileSize); domainOf(tileSize) = (constTileSize, constTileSize, constTileSize)
    val len = src.length; bound(len) = 9216

    val N = ArgIn[T]
    val out = ArgOut[T]
    setArg(N, len)

    val v1 = OffChipMem[T](N)
    setMem(v1, src)

    Accel {
      Sequential {
        val accum = Reg[T]
        Fold (N by tileSize)(accum, 0.as[T]) { i =>
          val b1 = BRAM[T](tileSize)
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
    val len = args(unit(0)).to[SInt]

    val src = Array.tabulate[T](len) { i => i }
    val result = simple_fold(src)

    val gold = src.reduce {_+_}
    println("expected: " + gold)
    println("result:   " + result)

    val cksum = result == gold
    println("PASS: " + cksum + " (SimpleFold)")
  }
}

object Memcpy2D extends SpatialAppCompiler with Memcpy2DApp // Args:
trait Memcpy2DApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val R = 96
  val C = 96

  def memcpy_2d(src: Rep[ForgeArray[T]], rows: Rep[SInt], cols: Rep[SInt]) = {
    val tileDim1 = param(96);
    val tileDim2 = param(96);  domainOf(tileDim2) = (96, 96, 96)

    val rowsIn = rows
    val colsIn = cols

    val srcFPGA = OffChipMem[T](rows, cols)
    val dstFPGA = OffChipMem[T](rows, cols)

    // Transfer data and start accelerator
    setMem(srcFPGA, src)

    Accel {
      Sequential(rowsIn by tileDim1, colsIn by tileDim2) { (i,j) =>
        val tile = BRAM[T](tileDim1, tileDim2)
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

    val cksum = dst.zip(src){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (MemCpy2D)")

  }
}

object BlockReduce1D extends SpatialAppCompiler with BlockReduce1DApp // Args: 1920
trait BlockReduce1DApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val N = 1920

  val tileSize = 96

  def blockreduce_1d(src: Rep[ForgeArray[T]], size: Rep[SInt]) = {

    val sizeIn = ArgIn[SInt]; setArg(sizeIn, size)

    val srcFPGA = OffChipMem[T](sizeIn)
    val dstFPGA = OffChipMem[T](tileSize)

    setMem(srcFPGA, src)

    Accel {
      val accum = BRAM[T](tileSize)
      Fold (sizeIn by tileSize)(accum, 0.as[T]) { i  =>
        val tile = BRAM[T](tileSize)
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
    val size = args(0).to[SInt]
    // val size = N
    val src = Array.tabulate(size) { i => i }

    val dst = blockreduce_1d(src, size)

    val iters = size/tileSize
    val first = ((iters*(iters-1))/2)*tileSize

    val gold = Array.tabulate(tileSize) { i => first + i*iters }

    printArr(gold, "src:")
    printArr(dst, "dst:")
    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (BlockReduce1D)")

//    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object UnalignedLd extends SpatialAppCompiler with UnalignedLdApp // Args:
trait UnalignedLdApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val N = 1920

  val numCols = 155
  val paddedCols = 192

  def unaligned_1d(src: Rep[ForgeArray[T]]) = {

    val srcFPGA = OffChipMem[T](paddedCols)
    val acc = ArgOut[T]

    setMem(srcFPGA, src)

    Accel {
      val mem = BRAM[T](numCols)
      Pipe { mem := srcFPGA(0::numCols) }
      acc := Reduce(numCols by 1)(0.as[T]) { i => mem(i) }{_+_}
    }
    getArg(acc)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    // val size = args(unit(0)).to[SInt]
    val size = paddedCols
    val src = Array.tabulate(size) { i => i }

    val dst = unaligned_1d(src)

    val gold = Array.tabulate(numCols) { i => i }.reduce{_+_}

    println("src:" + gold)
    println("dst:" + dst)
    val cksum = gold == dst
    println("PASS: " + cksum + " (UnalignedLd)")

//    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}

object BlockReduce2D extends SpatialAppCompiler with BlockReduce2DApp // Args: 960 960
trait BlockReduce2DApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val N = 1920

  val tileSize = 96

  def blockreduce_2d(src: Rep[ForgeArray[T]], rows: Rep[SInt], cols: Rep[SInt]) = {

    val rowsIn = ArgIn[SInt]; setArg(rowsIn, rows)
    val colsIn = ArgIn[SInt]; setArg(colsIn, cols)

    val srcFPGA = OffChipMem[T](rowsIn,colsIn)
    val dstFPGA = OffChipMem[T](tileSize,tileSize)

    setMem(srcFPGA, src)

    Accel {
      val accum = BRAM[T](tileSize,tileSize)
      Fold (rowsIn by tileSize, colsIn by tileSize)(accum, 0.as[T]) { (i,j)  =>
        val tile = BRAM[T](tileSize,tileSize)
        tile := srcFPGA(i::i+tileSize, j::j+tileSize, param(1))
        tile
      }{_+_}
      dstFPGA (0::tileSize, 0::tileSize) := accum
    }
    getMem(dstFPGA)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    val numRows = args(0).to[SInt]
    val numCols = args(1).to[SInt]
    val src = Array.tabulate(numRows) { i => Array.tabulate(numCols) { j => i*numCols + j} }

    val dst = blockreduce_2d(src.flatten, numRows, numCols)

    val numBlocks = numRows*numCols/(tileSize*tileSize)
    val first = ((numBlocks*(numBlocks-1))/2)*tileSize*tileSize
    val gold = Array.tabulate(tileSize*tileSize) { i => first + i*numBlocks }
    printArr(gold, "src:")
    printArr(dst, "dst:")
    // dst.zip(gold){_==_} foreach {println(_)}
    val cksum = dst.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (BlockReduce2D)")

//    (0 until tileSize) foreach { i => assert(dst(i) == gold(i)) }
  }
}


object ScatterGather extends SpatialAppCompiler with ScatterGatherApp // Args: 
trait ScatterGatherApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val N = 1920

  val tileSize = 384
  val maxNumAddrs = 1536
  val offchip_dataSize = maxNumAddrs*6

  def scattergather(addrs: Rep[ForgeArray[T]], offchip_data: Rep[ForgeArray[T]], size: Rep[SInt], dataSize: Rep[SInt]) = {

    val srcAddrs = OffChipMem[T](maxNumAddrs)
    val gatherData = OffChipMem[T](offchip_dataSize)
    val scatterResult = OffChipMem[T](offchip_dataSize)

    setMem(srcAddrs, addrs)
    setMem(gatherData, offchip_data)

    Accel {
      val addrs = BRAM[T](maxNumAddrs)
      Sequential (maxNumAddrs by tileSize) { i => 
        val gathered = BRAM[T](maxNumAddrs)
        Pipe {addrs := srcAddrs(i::i + tileSize, param(1))}
        Pipe {gathered := gatherData(addrs, tileSize)}
        Pipe {scatterResult(addrs, tileSize) := gathered}
      }
    }
      
    getMem(scatterResult)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {
    // val size = args(unit(0)).to[SInt]
    val size = maxNumAddrs
    val dataSize = offchip_dataSize
    val addrs = Array.tabulate[SInt](size) { i => 
      // i*2 // for debug
      if (i == 5) 199 else if (i == 6) offchip_dataSize-2 else if (i == 7) 191 else if (i==8) 203
        else if (i == 9) 381 else if (i == 10) offchip_dataSize-97 else if (i == 15) 97
        else if (i == 16) 11 else if (i == 17) 99 else if (i == 18) 245
        else if (i == 94) 3 else if (i == 95) 1 else if (i == 83) 101 
        else if (i == 70) 203 else if (i == 71) (offchip_dataSize-1) else i*2
    }
    val offchip_data = Array.fill(dataSize) {random[SInt](dataSize)}
    // val offchip_data = Array.tabulate (dataSize) { i => i}

    val received = scattergather(addrs, offchip_data, size, dataSize)

    // printArr(addrs, "addrs: ")
    // (0 until dataSize) foreach { i => println(i + " match? " + (addrs.map{a => a==i}.reduce{_||_}) ) }
    val gold = Array.tabulate(dataSize) { i => __ifThenElse(addrs.map{a => a==i}.reduce{_||_}, offchip_data(i), 0) } // Staging ifthenelse

    printArr(gold, "gold:")
    printArr(received, "received:")
    val cksum = received.zip(gold){_ == _}.reduce{_&&_}
    println("PASS: " + cksum + " (ScatterGather)")

  }
}


object InOutArg extends SpatialAppCompiler with InOutArgApp // Args: 7
trait InOutArgApp extends SpatialApp {

  def main() {

    // Declare SW-HW interface vals
    val x = ArgIn[SInt]
    val y = ArgOut[SInt]
    val N = args(0).to[SInt]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      Pipe { y := x + 4 }
    }


    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)

    val cksum = gold == result
    println("PASS: " + cksum + " (InOutArg)")
  }
}

object MultiplexedWriteTest extends SpatialAppCompiler with MultiplexedWriteApp
trait MultiplexedWriteApp extends SpatialApp {
  type Array[T] = ForgeArray[T]

  val tileSize = 16

  def main() = {
    val N = 1024
    val T = param(tileSize)
    val P = param(4)
    val I = 5.as[SInt]
    val weights = OffChipMem[SInt](N)
    val inputs  = OffChipMem[SInt](N)
    Accel {
      val wt = BRAM[SInt](T)
      val in = BRAM[SInt](T)
      Sequential(N by T){i =>
        wt := weights(i::i+T)
        in := inputs(i::i+T)

        // Some math nonsense (definitely not a correct implementation of anything)
        Pipe(I by 1){x =>
          Pipe(T par P){j => wt(j) = wt(j) - ((wt(j) - in(j))/in(j)) }
          val sum = Reduce(T par P)(0.as[SInt]){j => wt(j) }{_+_}
          Pipe(T par P){j => wt(j) = (wt(j) * tileSize) / sum }

          weights(i::i+T) := wt
        }
      }

    }
  }
}


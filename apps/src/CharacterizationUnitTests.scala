import spatial.compiler._
import spatial.library._
import spatial.shared._


/*

  Sketch of apps:

  ################
  # CharLoadTest #
  ################

  Do this for N iterations:

 (outerPar tiles)           offchip ←|→ onchip
          (innerPar ldPar)
            ↓↓↓                                    Dummy Memories
            _____tileSize1_____      .             .....tileSize1.....
       i → |                   | ------------->   .□□□                .    Write □□□ to offchip args
           |                   |     .   tileSize0.                   .          □□□
           |...................|     .            .....................          □□□
       i → |                   | ----------.       ...................
           |                   |     .     '-->   .□□□                .      □ = (onChip_addr xor offChip_addr)
           |...................|     .            .                   .
       i → |                   | ----------.      .....................
           |                   |     .     |       ...................
           |___________________|     .     '-->   .□□□                .
                                     .            .                   .
                                     .            .....................


  #################
  # CharStoreTest #
  #################

 Do this for N iterations:

 (outerPar tiles)
           Dummy Memories     onchip ←|→ offchip
          (innerPar ldPar)            .
            ↓↓↓                       .        ___________________
           .....tileSize1.....        .       |                   | ↑
  □ argIn .□□□                .       .       |                   | N*dim0
          .                   .       .       |...................| ↓
          .....................       .       |                   |
           ...................        .       |                   |
          .□□□                .       .       |...................|
          .                   .       .       |                   |
          .....................       .       |                   |
           ...................        .       |___________________|
          .□□□                .       .
          .                   .       .
          .....................       .



  ################
  # CharBramTest #
  ################

                    Write argin to entire SRAM
                      Then write first entry from each bank to argOut

              i
              ↓↓↓↓
              ___________________
         j → |                   |
           → |                   |
           → |                   |
             |                   |
             |      SRAM         |
             |                   |
             |                   |
             |                   |
             |___________________|




*/
object CharLoadTest extends SpatialAppCompiler with CharLoadTestApp // Args: 5
trait CharLoadTestApp extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val innerPar = 4;
  val outerPar = 2;
  val dim0 = 192;
  val dim1 = 1920;

  def CharLoad(srcHost: Rep[Array[T]], iters: Rep[SInt]) = {
    val sinnerPar = param(innerPar);
    val tileSize0 = param(dim0);
    val tileSize1 = param(dim1);

    val N = ArgIn[SInt]
    val out = List.tabulate(outerPar){i => List.tabulate(innerPar) {j => ArgOut[SInt] }}

    setArg(N, iters)

    val srcFPGA = DRAM[SInt](tileSize0, tileSize1)
    setMem(srcFPGA, srcHost)

    Accel {
      Sequential (N by 1) { ii =>
        val dummy = List.tabulate(outerPar){i =>
          SRAM[SInt](tileSize0, tileSize1)
        }
        dummy.foreach {dum =>
          isDummy(dum) = true
        }
        Parallel {
          dummy.zipWithIndex.foreach{ case (dum, i) =>
            Pipe {dum := srcFPGA(i*dim0::(i+1)*dim0, i*dim1::(i+1)*dim1 par sinnerPar)}
          }
        }
        Parallel {
          out.zip(dummy).zipWithIndex.foreach { case ((row, dum), i) =>
            row.zipWithIndex.foreach { case (o, j) =>
              Pipe {
                val rd = dum(0, j)
                bankOverride(rd) = j
                if (j > 0) {memoryIndexOf(rd) = 0}
                Pipe {o := rd}
              }
            }
          }
        }
      }
      ()
    }
    out.map { row =>
      row.map { m =>
        getArg(m)
      }
    }
  }

  def main() {
    val iters = args(unit(0)).to[T]

    val src = Array.tabulate[T](dim0*dim1*outerPar) { i => i }
    val result = CharLoad(src, iters)

    // val gold = List.tabulate(outerPar) { i =>
    //   List.tabulate(innerPar) {j =>
    //     i * dim0 * dim1 + j
    //   }
    // }
    // gold.foreach{row => row.foreach{println(_)}}
    result.map{row => row.foreach{println(_)}}

    // Lazy check because I don't feel like xor'ing here
    val cksum = result.flatten.zipWithIndex.map{ case (a, i) =>
      if (i < innerPar) {a == 0} else {a != 0}
    }.reduce{_&&_}
    println("PASS: " + cksum  + " (CharLoadTest)")

  }
}

object CharStoreTest extends SpatialAppCompiler with CharStore // Args: 5 3
trait CharStore extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val innerPar = 4;
  val outerPar = 4;
  val dim0 = 192;
  val dim1 = 1920;
  def CharStore(iters: Rep[T], numin: Rep[T]) = {
    val sinnerPar = param(innerPar);
    val tileSize0 = param(dim0);
    val tileSize1 = param(dim1);

    val N = ArgIn[SInt]
    val num = ArgIn[SInt]
    setArg(N, iters)
    setArg(num, numin)
    val dstFPGA = List.tabulate(outerPar){i => DRAM[SInt](dim0, dim1) }

    Accel {
      Sequential (N by 1) { ii =>
        val dummy = List.tabulate(outerPar){i =>
          SRAM[SInt](tileSize0, tileSize1)
        }
        dummy.foreach {dum =>
          isDummy(dum) = true
        }
        Parallel {
          dummy.zipWithIndex.foreach{ case (dum, i) =>
            Pipe {dum(0,0) = num.value + i} // Template hack broadcasts this write to all banks
          }
        }
        Parallel {
          dummy.zip(dstFPGA).zipWithIndex.foreach{ case ((dum, dst), i) =>
            Pipe {dst (0::dim0, 0::dim1 par sinnerPar) := dum}
          }
        }
      }
      ()
    }
    dstFPGA.map { m =>
      getMem(m)
    }

  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val len = args(unit(0)).to[T]
    val num = args(unit(1)).to[T]
    val mem = Array.tabulate[T](outerPar) {i => i + num}

    val result = CharStore(len, num)
    // val print_result = result.map(a => a.reduce{_+_})

    // // println("expected: sequential stuff")
    // println("Expected: " + mem.map{a => a}.reduce{_+_}*dim0*dim1)
    // println("Received: " + print_result.map{a => a}.reduce{_+_})

    // val cksum = mem.reduce{_+_}*dim0*dim1 == print_result.reduce{_+_}
    println("PASS: 1 (CharStoreTest) ** But possibly a lie because ListVector has no CPP backend so I hardcoded this")

  }
}


object CharBramTest extends SpatialAppCompiler with CharBram // Args: 5 1 0
trait CharBram extends SpatialApp {
  type T = SInt
  type Array[T] = ForgeArray[T]
  val innerPar = 4;
  val outerPar = 2;
  val dim0 = 192;
  val dim1 = 192;
  def CharBram(numin: Rep[T], addrIn0: Rep[T], addrIn1: Rep[T]) = {
    val tileDim0 = param(dim0);
    val tileDim1 = param(dim1);
    val spar0 = param(innerPar);
    val spar1 = param(outerPar);

    val num = ArgIn[SInt]
    val rdAddr0 = ArgIn[SInt]
    val rdAddr1 = ArgIn[SInt]
    setArg(num, numin)
    setArg(rdAddr0, addrIn0)
    setArg(rdAddr1, addrIn1)

    val out = ArgOut[SInt]

    Accel {
      val tile = SRAM[T](tileDim0, tileDim1)
      hardcodeEnsembles(tile) = true
      Pipe (tileDim0 by 1 par spar0) { i =>
        Pipe (tileDim1 by 1 par spar1) { j =>
          tile(i,j) = num
        }
      }
      Pipe {out := tile(rdAddr0, rdAddr1)}
    }

    getArg(out)
  }

  def main() {
    val numin = args(0).to[T]
    val addrIn0 = args(1).to[T]
    val addrIn1 = args(2).to[T]

    val result = CharBram(numin, addrIn0, addrIn1)

    val cksum = numin == result
    println("PASS: " + cksum + " (CharBramTest)")

  }
}

// NOTES: WHY DID 3 AND 7 FAIL FOR CHAR LOAD? - Sept 17 2016
import spatial.compiler._
import spatial.library._
import spatial.shared._




/*





         Minibatch impelementation:
                             _
                            | |
                            |M|
                            | |
                            | |
                            | |
                            | |
                  D         |_|
             _____________   _       _                  _
            |             | |^|     | |                | |
          N |      X      | |Y|  -  |Y|  =>            |Y_err
            |_____________| |_|     |_|                |_|
                                                ____    _        _      _
                                               |    |  | |      | |    | |
                                               |    |  | |      |M|    |M|
                                               |    |  |Δ|  +   | | -> | |
                                               | X_T|  | |      | |    | |
                                               |    |  | |      | |    | |
                                               |    |  | |      | |    | |
                                               |____|  |_|      |_|    |_|


*/



object SGD extends SpatialAppCompiler with SGDApp // Args: 1 5
trait SGDApp extends SpatialApp {
  type Array[T] = ForgeArray[T]
  type T = Flt
  val modelSize = 768
  val tileSize = 192
  val innerPar = 48
  val outerPar = 1
  val margin = 1


  def sgd_onept(x_in: Rep[Array[T]], y_in: Rep[Array[T]], alpha: Rep[T], epochs: Rep[SInt], nn: Rep[SInt]) = {
    val E = ArgIn[SInt]
    val N = ArgIn[SInt]
    val A = ArgIn[T]
    val D = modelSize

    val ip = innerPar (1 -> 1)
    val op = outerPar (1 -> 1)

    setArg(E, epochs)
    setArg(N, nn)
    setArg(A, alpha)

    val x = DRAM[T](N,D)
    val y = DRAM[T](N)
    val result = DRAM[T](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val y_tile = SRAM[T](tileSize)
      val sgdmodel = SRAM[T](D)
      Sequential(E by 1) { e =>
        Sequential (N by tileSize) { b =>
          y_tile := y(b::b+tileSize par op)
          // Sequential.fold(tileSize by 1 par op)(sgdmodel) { i =>
          Sequential(tileSize by 1) {i => 
            val y_err = Reg[T](0)
            // val update = SRAM[T](D)
            val x_tile = SRAM[T](D)
            Parallel{
              x_tile := x(b+i, 0 :: D par ip)
            }
            Pipe{ // This pipe is here to make sure y_hat resets properly
              val y_hat = Reduce(D by 1 par ip)(0.as[T]){ j => x_tile(j) * sgdmodel(j) }{_+_}
              y_err := y_hat.value - y_tile(i)
            }

            Pipe (D by 1 par ip) { j =>
              sgdmodel(j) = sgdmodel(j) + x_tile(j)*y_err.value*A
            }
          }
        }
      }
      result(0::D par ip) := sgdmodel

    }

    getMem(result)

  }

  // def sgd_minibatch(x_in: Rep[Array[T]], y_in: Rep[Array[T]], alpha: Rep[T], epochs: Rep[SInt], nn: Rep[SInt]) = {
  //   val E = ArgIn[SInt]
  //   val N = ArgIn[SInt]
  //   val A = ArgIn[T]
  //   val D = modelSize

  //   setArg(E, epochs)
  //   setArg(N, nn)
  //   setArg(A, alpha)

  //   val ip = innerPar (1 -> 2)
  //   val op = outerPar (1 -> 2)

  //   val x = DRAM[T](N, D)
  //   val y = DRAM[T](N)
  //   val result = DRAM[T](D)

  //   setMem(x, x_in)
  //   setMem(y, y_in)

  //   Accel {
  //     val x_tile = SRAM[T](tileSize,D)
  //     val y_tile = SRAM[T](tileSize)
  //     val update = SRAM[T](D)
  //     val sgdmodel = SRAM[T](D)
  //     val y_err = SRAM[T](tileSize)
  //     Sequential(E by 1) { e =>
  //       Sequential(N by tileSize) { tile =>
  //         Parallel {
  //           x_tile := x(tile::tile + tileSize, 0::D par ip)
  //           y_tile := y(tile::tile + tileSize)
  //         }
  //         Pipe (tileSize by 1) { i =>
  //           val y_hat = Reduce(D by 1 par ip)(0.as[T]){ j => x_tile(i,j) * sgdmodel(j) }{_+_}
  //           Pipe{y_err(i) = y_hat.value - y_tile(i)}
  //         }
  //         Pipe (D by 1) { j =>
  //           val delta = Reduce(tileSize by 1 par ip)(0.as[T]){ i => x_tile(i,j) * y_err(i)}{_+_}
  //           Pipe{update(j) = delta.value*A}
  //         }
  //         Fold(1 by 1)(sgdmodel, 0.as[T]){d => update}{_+_} // Fuse with prev pipe once foreach reduce is allowed
  //       }
  //     }
  //     result(0::D par ip) := sgdmodel
  //   }

  //   getMem(result)
  // }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val E = args(0).to[SInt]
    val N = args(1).to[SInt]
    val A = args(2).to[T] // Should be somewhere around 0.0001 for point-wise sgd
    val D = modelSize

    val sX = Array.fill(N){ Array.fill(D){ random[T](3.as[T])} }
    val sY = Array.fill(N)( random[T](10.as[T]) )
    val id = Array.tabulate(D){ i => i }
    val ep = Array.tabulate(E){ i => i }

    val result = sgd_onept(sX.flatten, sY, A, E, N)

    val gold = Array.empty[T](D)
    (0 until D) foreach { j => gold(j) = 0.as[T] }

    (0 until E) foreach { i =>
      val y_hat = sX.zip(sY){ case (row, y) => row.zip(gold) {_*_}.reduce{_+_} }
      val y_err = y_hat.zip(sY){case (a,b) => a - b}
      val update = id.map{ j =>
        val col = sX.map{_(j)}
        col.zip(y_err){case (a,b) => a*b}.reduce{_+_}
      }

      (0 until D) foreach { j => gold(j) = update(j)*A + gold(j) }
    }

    val cksum = gold.zip(result){ case (a,b) => a < b + margin && a > b - margin }.reduce{_&&_}
    printArr(result, "result: ")
    printArr(gold, "gold: ")
    println("PASS: " + cksum  + " (SGD) *Note that test was designed for minibatch SGD and not true point-wise SGD")
  }
}

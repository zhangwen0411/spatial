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
  type T = SInt
  val numPoints = 96
  val modelSize = 96
  val margin = 1


  // def sgd_onept(x_in: Rep[Array[T]], y_in: Rep[Array[T]], alpha: Rep[T], epochs: Rep[SInt]) = {
  //   val A = ArgIn[T]
  //   val E = ArgIn[SInt]
  //   val N = numPoints
  //   val D = modelSize

  //   setArg(A, alpha)
  //   setArg(E, epochs)

  //   val x = OffChipMem[T](N*D)
  //   val y = OffChipMem[T](N)
  //   val result = OffChipMem[T](D)

  //   setMem(x, x_in)
  //   setMem(y, y_in)

  //   Accel {
  //     val x_tile = BRAM[T](D)
  //     val y_tile = BRAM[T](N)
  //     val update = BRAM[T](D)
  //     val model = BRAM[T](D)
  //     val y_err = Reg[T](0)
  //     y_tile := y(0::D, param(1))
  //     Sequential(E by 1) { e =>
  //       Pipe (N by 1) { i =>
  //         x_tile := x(i*D :: (i+1)*D, param(1))

  //         val y_hat = Reduce(D by 1)(0.as[T]){ j => x_tile(j) * model(j) }{_+_}
  //         y_err := y_hat.value - y_tile(i)

  //         Pipe (D by 1) { j =>
  //           model(j) = model(j) + y_err.value*A
  //         }
  //       }
  //     }
  //     result(0::D, param(1)) := model

  //   }

  //   getMem(result)
  
  // }

  def sgd_minibatch(x_in: Rep[Array[T]], y_in: Rep[Array[T]], alpha: Rep[T], epochs: Rep[SInt]) = {
    val A = ArgIn[T]
    val E = ArgIn[SInt]
    val N = numPoints
    val D = modelSize

    setArg(A, alpha)
    setArg(E, epochs)

    val x = OffChipMem[T](N, D)
    val y = OffChipMem[T](N)
    val result = OffChipMem[T](D)

    setMem(x, x_in)
    setMem(y, y_in)

    Accel {
      val x_tile = BRAM[T](N,D)
      val y_tile = BRAM[T](N)
      val update = BRAM[T](D)
      val model = BRAM[T](D)
      val y_err = BRAM[T](N)
      Sequential(E by 1) { e =>
        Parallel {
          Pipe {x_tile := x(0::N, 0::D, param(1))}
          y_tile := y(0::D, param(1))
        }
        Pipe (N by 1) { i =>
          val y_hat = Reduce(D by 1)(0.as[T]){ j => x_tile(i,j) * model(j) }{_+_}
          y_err(i) = y_hat.value - y_tile(i)
        }
        Pipe (D by 1) { j =>
          val delta = Reduce(N by 1)(0.as[T]){ i => x_tile(i,j) * y_err(i)}{_+_}
          update(j) = delta.value*A
        }
        Fold(1 by 1)(model, 0.as[T]){d => update}{_+_} // Fuse with prev pipe once foreach reduce is allowed
      }
      result(0::D, param(1)) := model

    }

    getMem(result)
  

  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() {
    val A = args(0).to[T]
    val E = args(1).to[SInt]
    val N = numPoints
    val D = modelSize

    val sX = Array.fill(N){ Array.fill(D){ random[T](10.as[T])} }
    val sY = Array.fill(N)( random[T](10.as[T]) )
    val id = Array.tabulate(D){ i => i }
    val ep = Array.tabulate(E){ i => i }

    val result = sgd_minibatch(sX.flatten, sY, A, E)

    val gold = Array.empty[T](D)
    (0 until D) foreach { j => gold(j) = 0 }

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
    println("PASS: " + cksum  + " (SGD)")
  }
}

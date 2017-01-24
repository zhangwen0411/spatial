// import spatial.compiler._
// import spatial.library._
// import spatial.shared._


// object EdgeDetectorFifoConv1D extends SpatialAppCompiler with EdgeDetectorFifoConv1DApp // Regression (Dense) // Args: 384
// trait EdgeDetectorFifoConv1DApp extends SpatialApp {
//   type T = SInt
//   type Array[T] = ForgeArray[T]


//   def fifoconv1d(data: Rep[Array[T]]): Rep[T] = {
//     val margin = 0.05 // Allowed percent error
//     val size = 1152
//     val window = 5
//     val amplitude = 10

//     val signal  = DRAM[T](size)
//     val out = ArgOut[T]

//     setMem(signal, data)

//     Accel {
//       // Declare memories
//       val edge1 = Reg[T]
//       val edge2 = Reg[T]
//       val fifo = FIFO[T](size)
//       val running_sum = Reg[T]
//       val running_err = Reg[T]
//       val start = Reg[T]
//       val end = Reg[T]
//       val active = FIFO[T](window)

//       // Load data into fifo (ultimately a stream interface)
//       fifo := signal(0::size)

//       // // Fill active window
//       // val init_sum = Reduce(window by 1) { i => 
//       //   val enq = fifo.pop()
//       //   active.push(enq)
//       //   enq
//       // }
//       // running_sum := init_sum.value()

//       sequential.fold(size by 1)((running_sum, running_err), (0.as[T], 0.as[T]) { i => 
//         // Get elements to enq and deq from window
//         val enq = fifo.pop()
//         val deq = active.pop()
//         active.push(enq)

//         // Compute what these enq and deq elements do to running sums
//         val sum_update = enq - deq
//         val err_update = (enq - running_mean.value())^2 - (deq - running_mean.value())^2

//         // Compute new statistics
//         val running_mean = running_sum.value() / window.as[T]
//         val running_std = running_err.value() / window.as[T]

//         // Update results with statistics, if they meet criteria
//         start := Mux(running_std > amplitude/2, i + window, start.value())
//         end := Mux(running_std > amplitude/2, i + window, end.value())

//         // Return values to update running sums with
//         (sum_update, err_update)
//       }

//       out := (start.value() + end.value()) / 2
//     }
//     getArg(out)
//   }

//   def printArr(a: Rep[Array[Bit]], str: String = "") {
//     println(str)
//     (0 until a.length) foreach { i => print(a(i) + " ") }
//     println("")
//   }


//   def main() {
//     val N = args(0).to[T]
//     val data = (0 until size).map{ i => 
//       if ( i < size/3 | i > size*2/3) 1 // + noise
//       else amplitude // + noise
//     }

//     val result = fifoconv1d(data)

//     val gold = size/2

//     println("expected " + gold)
//     println("result " + result)

//     val lowerBound = gold - gold*margin
//     val upperBound = gold + gold*margin
//     val cksum = (result < upperBound && result > lowerBound)
//     println("PASS: " + cksum + " (EdgeDetectorFifoConv1D)")
//   }
// }


// object EdgeDetectorRegression1D extends SpatialAppCompiler with EdgeDetectorRegression1DApp // Regression (Dense) // Args: 384
// trait EdgeDetectorRegression1DApp extends SpatialApp {
//   type T = SInt
//   type Array[T] = ForgeArray[T]


//   def regression1d(data: Rep[Array[T]]): Rep[T] = {
//     val margin = 0.05 // Allowed percent error
//     val size = 1152
//     val window = 5
//     val amplitude = 10

//     val signal  = DRAM[T](size)
//     val out = ArgOut[T]

//     val epochs = 10
//     val degree = 6

//     setMem(signal, data)

//     Accel {
//       // Declare memories
//       val edge1 = Reg[T]
//       val edge2 = Reg[T]
//       val data = SRAM[T](size)
//       val model = SRAM[T](degree)

//       // Load data into sram
//       fifo := signal(0::size)

//       Pipe(epochs by 1) {
//         // TODO: Regression to fit model to data
//       }

//       out := (start.value() + end.value()) / 2
//     }
//     getArg(out)
//   }

//   def printArr(a: Rep[Array[Bit]], str: String = "") {
//     println(str)
//     (0 until a.length) foreach { i => print(a(i) + " ") }
//     println("")
//   }


//   def main() {
//     val N = args(0).to[T]
//     val data = (0 until size).map{ i => 
//       if ( i < size/3 | i > size*2/3) 1 // + noise
//       else amplitude // + noise
//     }

//     val result = regression1d(data)

//     val gold = size/2

//     println("expected " + gold)
//     println("result " + result)

//     val lowerBound = gold - gold*margin
//     val upperBound = gold + gold*margin
//     val cksum = (result < upperBound && result > lowerBound)
//     println("PASS: " + cksum + " (EdgeDetectorRegression1D)")
//   }
// }


// object EdgeDetectorLBConv2D extends SpatialAppCompiler with EdgeDetectorLBConv2DApp // Regression (Dense) // Args: 384
// trait EdgeDetectorLBConv2DApp extends SpatialApp {
//   type T = SInt
//   type Array[T] = ForgeArray[T]


//   def LBConv(data: Rep[Array[T]]): Rep[T] = {
//     val margin = 0.05 // Allowed percent error
//     val N = 1024
//     val M = 1024
//     val window = 5
//     val amplitude = 10
//     val threshold = 1

//     val signal  = DRAM[T](M,N)
//     val out = ArgOut[T]

//     setMem(signal, data)

//     Accel {
//       // Declare memories
//       val edgeN = Reg[T]
//       val edgeS = Reg[T]
//       val edgeE = Reg[T]
//       val edgeW = Reg[T]
//       val image = SRAM[T](M,N)
//       val kernelN = SRAM[T](window,window)
//       val kernelS = SRAM[T](window,window)
//       val kernelE = SRAM[T](window,window)
//       val kernelW = SRAM[T](window,window)

//       val lb = LineBuffer[T](window, N)

//       Pipe(M - window by 1) { ii => 
//         lb.enq(signal(0::N)) // load a row

//         Pipe(N - window by 1){ jj => 
//           val sr = ShiftReg(window, window)

//           sr := lb(0 :: window)

//           val resultN = Reduce(window by 1, window by 1){(i,j) => sr(i,j)*kernelN(i,j) }{_+_}
//           val resultS = Reduce(window by 1, window by 1){(i,j) => sr(i,j)*kernelS(i,j) }{_+_}
//           val resultE = Reduce(window by 1, window by 1){(i,j) => sr(i,j)*kernelE(i,j) }{_+_}
//           val resultW = Reduce(window by 1, window by 1){(i,j) => sr(i,j)*kernelW(i,j) }{_+_}
//           edgeN := Mux(resultN, ii, edgeN.value())
//           edgeS := Mux(resultS, ii, edgeS.value())
//           edgeE := Mux(resultE, jj, edgeE.value())
//           edgeW := Mux(resultW, jj, edgeW.value())
//         }
//       }           
//     out := (((edgeN.value() + edgeS.value()) / 2),((edgeE.value() + edgeW.value()) / 2)) 
//     }
//     getArg(out)
//   }

//   def printArr(a: Rep[Array[Bit]], str: String = "") {
//     println(str)
//     (0 until a.length) foreach { i => print(a(i) + " ") }
//     println("")
//   }


//   def main() {
//     val N = args(0).to[T]
//     val data = (0 until size).map{ i => 
//       if ( i < size/3 | i > size*2/3) 1 // + noise
//       else amplitude // + noise
//     }

//     val result = LBConv(data)

//     val gold = size/2

//     println("expected " + gold)
//     println("result " + result)

//     val lowerBound = gold - gold*margin
//     val upperBound = gold + gold*margin
//     val cksum = (result < upperBound && result > lowerBound)
//     println("PASS: " + cksum + " (EdgeDetectorLBConv2D)")
//   }
// }
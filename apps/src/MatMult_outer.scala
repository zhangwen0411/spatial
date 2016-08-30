import spatial.compiler._
 import spatial.library._
 import spatial.shared._

 object MatMult_outer extends SpatialAppCompiler with MatMult_outerApp
 trait MatMult_outerApp extends SpatialApp {
   type T = SInt //FixPt[Signed,B16,B16]
   type Array[T] = ForgeArray[T]

   def MatMult_outer(A: Rep[Array[T]], B: Rep[Array[T]], mm: Rep[SInt], nn: Rep[SInt], pp: Rep[SInt]) = {
     val M = ArgIn[SInt]
     val N = ArgIn[SInt]
     val P = ArgIn[SInt]
     setArg(M,mm)
     setArg(N,nn)
     setArg(P,pp)

     val a = OffChipMem[T](M, P) 
     val b = OffChipMem[T](P, N) 
     val c = OffChipMem[T](M, N)

     val bm        = param(2) 
     val bn        = param(96) 
     val bp        = param(96) 

     setMem(a, A)
     setMem(b, B)

	Accel {

		Pipe(M by bm, N by bn) { (i,j) =>
       		Pipe(P by bp) { k =>
              val tileA = BRAM[T](bm, bp)
              val tileB = BRAM[T](bp, bn)
              val tileC = BRAM[T](bm, bn)
              Parallel {
                tileA := a(i::i+bm, k::k+bp, param(1))
                tileB := b(k::k+bp, j::j+bn, param(1))
              }
              Fold(bp by 1)(tileC, 0.as[T]) { kk =>
                val tileC_partial = BRAM[T](bm,bn)
                Pipe(bm by 1, bn by 1){ (ii,jj) => 
                 tileC_partial(ii,jj) = tileA(ii,kk) * tileB(kk,jj)
                }
                tileC_partial
              }{_+_}
              c(i::i+bm, j::j+bn, param(1)) := tileC
       		}
       	}

     }
     getMem(c)
   }

   def printArr(a: Rep[Array[T]], str: String = "") {
     println(str)
     (0 until a.length) foreach { i => print(a(i) + " ") }
     println("")
   }

   def main() = {
     val M = args(0).to[SInt]
     val N = args(1).to[SInt]
     val P = args(2).to[SInt]

     val a = Array.fill(M){ Array.fill(P){1} }
     val b = Array.fill(P){ Array.fill(N){1} }
     // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
     // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

     val result = MatMult_outer(a.flatten, b.flatten, M, N, P)

     val gold = Array.tabulate(M){i =>
       val aRow = a(i)
       Array.tabulate(N){j =>
         val bCol = b.map{row => row(j)}
         aRow.zip(bCol){_*_}.reduce{_+_}
       }
     }.flatten

     println("expected cksum: " + gold.map(a => a).reduce{_+_})
     println("result cksum: " + result.map(a => a).reduce{_+_})

     assert(gold == result)
   }
 }

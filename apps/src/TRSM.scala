import spatial.compiler._
import spatial.library._
import spatial.shared._


/*
  Sketch of app:


    ################
    # Inner Kernel #
    ################
                       diag_index                              
                     __↓_______        ___________________
                    |\         |      |                   |
            diag_index\        |  diag_index      B       |
                   ↳|  o   L   |     ↳|===================|
                 ↱  |   \      |      |                   |
                 |  |    \     |      |                   |
                 |  |     \    |      |                   |
             len {  |      \   |      |                   |
                 |  |       \  |      |                   |
                 |  |        \ |      |                   |
                 ↳  |_________\|      |___________________|
                                               
         __N_       ___K___       
        |    |     |       |
      N |    |   N |       |                                 
        |____|     |_______|                               
  aligned_N  .     .       .                                                                     
        .    .     .       .                                  
        .    .     .       .                                        
        ......     .........                   

                  *Make DRAMs match big rectangles so                                 
                     maxj doesn't complain, but only fill                              
                     and load valid stuff                                  
                                                      

                                                      

    ##################
    # MatMult Kernel #
    ##################
                                       LDB                               
                                      _____________________________________                             
                                     |       |                             |                              
                                     |       |                             |                              
                                     |       |                             |                              
                                     |       |                             |                              
                                     |_______|_____________________________|                              
                                                                   
                                                                   
                                                   LDC          
                ____________          _____________________________________
               |            |        |                                     |
      LDA      |            |        |                                     |
               |            |        |                                     |
               |            |        |                                     |     
               |____________|        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |     
               |            |        |                                     |
               |            |        |                                     |
               |            |        |                                     |
               |            |        |                                     |
               |            |        |                                     |
               |            |        |                                     |
               |____________|        |_____________________________________|



                        LDC          
                _____________________________________
               |                                     |
               |  C                                  |
               |                                     |
               |                            N        |        
               |               idb↴__________________|        
               |                  | B                |        
               |                K |                  |        
               |          ida     |                  |        
               |           ↳ _____|__________________|        
               |            | A   | C                |        
               |            |     |                  |        
               |            |     |                  |
               |            |     |                  |
               |          M |     |                  |
               |            |     |                  |
               |            |     |                  |
               |            |     |                  |
               |____________|_____|__________________|



*/




object TRSM extends SpatialAppCompiler with TRSMApp
trait TRSMApp extends SpatialApp {
  type T = Flt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val N = 4   // N < k usually
  val K = 8
  val aligned_N = 96
  val margin = 1



  def rank1update(L: Rep[SRAM[T]], diag_index: Rep[SInt], len: Rep[Reg[SInt]], B: Rep[SRAM[T]], K:Rep[SInt]) = {
    Sequential(len.value by 1) { i => 
      Sequential(K by 1) { j => 
        val update_row = diag_index + 1 + i
        val data = B(update_row,j) - L(update_row, diag_index)*B(diag_index, j)
        sram_store_nd(B, List(update_row,j), data, Some(len.value != 0))
        // B(update_row,j) = B(update_row,j) - L(update_row, diag_index)*B(diag_index, j)
      }
    }
  }

  def SGEMM(M: Rep[SInt], N: Rep[SInt], K: Rep[SInt], alpha: Rep[T], 
    A: Rep[SRAM[T]], LDA: Rep[SInt], B: Rep[SRAM[T]], LDB: Rep[SInt],
    beta: Rep[T], C: Rep[SRAM[T]], LDC: Rep[SInt]) = {

    // ALTERNATIVE 1: By outer products, direct access
    Sequential(K by 1) { k => 
      Pipe(LDA by 1, LDB by 1) { (i,j) =>
        C(i,j) = beta*C(i,j) + alpha*A(i,k)*B(k,j)
      }
    }

    // ALTERNATIVE 2: By outer products, block reduce
    val C_part = SRAM[T](LDA,LDB)
    Pipe(LDA by 1, LDB by 1){ (i,j) => C_part(i,j) = C(i,j) }
    Fold(K by 1)(C_part, 0.as[T]) { k => 
      val update = SRAM[T](LDA,LDB)
      Pipe(LDA by 1, LDB by 1) { (i,j) => 
        update(i,j) = alpha*A(i,k)*B(j,k)
      }
      update
    }{ (C_tile, update_tile) => C_tile*beta + update_tile }
    Pipe(LDA by 1, LDB by 1){ (i,j) => C(i,j) = C_part(i,j) }


    // ALTERNATIVE 3: By inner products, direct access
    Sequential(LDA by 1, LDB by 1) { (i,j) => 
      val update = Reduce(K by 1)(0.as[T]) { k => A(i,k)*B(k,j) }{_+_}
      C(i,j) = beta*C(i,j) + update*alpha
    }

  }

  def trsm(B: Rep[Array[T]], L: Rep[Array[T]]) = {

    val OCB    = DRAM[T](aligned_N,K)
    val OCL    = DRAM[T](aligned_N,N)
    val OCX    = DRAM[T](aligned_N*K)
    setMem(OCB, B)
    setMem(OCL, L)


    Accel {
      Sequential{
        val B = SRAM[T](N,K)
        val L = SRAM[T](N,N)
        val X = SRAM[T](aligned_N*K)
        Parallel{
          B := OCB(0::N, 0::K)
          L := OCL(0::N, 0::N)
        }
        Sequential(N by 1) { diag_index => 
          val lambda = L(diag_index,diag_index)
          Sequential(K by 1) { k => B(diag_index,k) = B(diag_index,k) / lambda }
          // Rank 1 update (outer product, subtraction accumulator)
          val len = Reg[SInt]
          Pipe{len := N.as[SInt] - diag_index - 1}
          rank1update(L, diag_index, len, B, K)
        }
        // Pack result to 1D, to avoid burst alignment issues
        Pipe(N by 1) { i => Pipe(K by 1) { j => X(i*K + j) = B(i,j)}}
        Pipe{OCX(0::aligned_N*K) := X}        
      }
    }
    getMem(OCX)
  }

  def printArr(a: Rep[Array[T]], numel:Int, str: String = "") {
    println(str)
    (0 until numel) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {

    val B = Array.fill(aligned_N){ Array.fill(K){ random[T](1)} }
    val L = Array.tabulate(aligned_N){ i => Array.tabulate(N){ j =>
      if (j > i) 0.as[T] else random[T](1)
    }}

    val result = trsm(B.flatten, L.flatten)

    printArr(B.flatten, N*K, "B: ")
    printArr(L.flatten, N*N, "L: ")
    printArr(result, N*K, "X: ")

    val X_check = Array.tabulate(N){ i => Array.tabulate(K) { j => result(i*K + j) }}
    val L_check = Array.tabulate(N){ i => Array.tabulate(N) { j => 
      val row = L(i)
      row(j)}}
    val B_check = Array.tabulate(N){ i => Array.tabulate(K) { j =>
      val row = B(i)
      row(j) }}.flatten
    val B_computed = Array.tabulate(N){i =>
      val aRow = L_check(i)
      Array.tabulate(K){j =>
        val bCol = X_check.map{row => row(j)}
        aRow.zip(bCol){_*_}.reduce{_+_}
      }
    }.flatten

    val cksum = B_check.zip(B_computed){ (a,b) => a > b - margin && a < b + margin}.reduce{_&&_}
    println("PASS: " + cksum + " (TRSM)")
  }
}

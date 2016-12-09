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
                                               
 


    ##################
    # MatMult Kernel #
    ##################
                              ld_edge_c          
                _____________________________________
               |                                     |
               |  C                                  |
               |                                     |
               |                      dim_b          |        
               |               ldb __________________|        
               |            lda   | B                |        
               |                  |                  |        
               |          dim_a   |                  |        
               |             _____|__________________|        
               |            | A   | C                |        
               |            |     |                  |        
               |            |     |                  |
               |            |     |                  |
               |            |     |                  |
               |            |     |                  |
               |            |     |                  |
               |            |     |                  |
               |____________|_____|__________________|



*/




object TRSM extends SpatialAppCompiler with TRSMApp
trait TRSMApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  val N = 8   // N < k usually
  val K = 16


  def rank1update(L: Rep[SRAM[T]], diag_index: Rep[SInt], len: Rep[Reg[SInt]], B: Rep[SRAM[T]], K:Rep[SInt]) = {
    Sequential(len.value by 1) { i => 
      Sequential(K by 1) { j => 
        val update_row = diag_index + 1 + i
        B(update_row,j) = B(update_row,j) - L(update_row, diag_index)*B(diag_index, j)
      }
    }
  }

  def trsm(L: Rep[Array[T]], B: Rep[Array[T]]) = {

    val OCB    = DRAM[T](N,K)
    val OCL    = DRAM[T](N,N)
    val OCX    = DRAM[T](N,K)
    setMem(OCB, B)
    setMem(OCL, L)


    Accel {
      val L = SRAM[T](N,N)
      val B = SRAM[T](N,K)
      Parallel{
        L := OCL(0::N, 0::N)
        B := OCB(0::N, 0::K)
      }
      Sequential(N by 1) { diag_index => 
        val lambda = L(diag_index,diag_index)
        Pipe(K by 1) { k => B(diag_index,k) = B(diag_index,k) / lambda }
        // Rank 1 update (outer product, subtraction accumulator)
        val len = Reg[SInt]
        Pipe{len := N.as[SInt] - diag_index - 1}
        rank1update(L, diag_index, len, B, K)
      }
      Pipe{OCX(0::N, 0::K) := B}
    }
    getMem(OCX)
  }

  def printArr(a: Rep[Array[T]], str: String = "") {
    println(str)
    (0 until a.length) foreach { i => print(a(i) + " ") }
    println("")
  }

  def main() = {

    val B = Array.fill(N){ Array.fill(K){ random[T](100)} }
    val L = Array.tabulate(N){ i => Array.tabulate(N){ j =>
      if (j > i) 0.as[T] else random[T](100)
    }}

    val result = trsm(B.flatten, L.flatten)

    printArr(B.flatten, "B: ")
    printArr(L.flatten, "L: ")
    printArr(result, "X: ")
  }
}

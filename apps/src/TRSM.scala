// import spatial.compiler._
// import spatial.library._
// import spatial.shared._


// /*
//   Sketch of app:


//     ################
//     # Inner Kernel #
//     ################
//                        diag_index                              
//                      __↓_______        ___________________
//                     |\         |      |                   |
//             diag_index\        |  diag_index      B       |
//                    ↳|  o   L   |     ↳|===================|
//                  ↱  |   \      |      |                   |
//                  |  |    \     |      |                   |
//                  |  |     \    |      |                   |
//              len {  |      \   |      |                   |
//                  |  |       \  |      |                   |
//                  |  |        \ |      |                   |
//                  ↳  |_________\|      |___________________|
                                               
//          __N_       ___K___       
//         |    |     |       |
//       N |    |   N |       |                                 
//         |____|     |_______|                               
//   full_N     .     .       .                                                                     
//         .    .     .       .                                  
//         .    .     .       .                                        
//         ......     .........                   

//                   *Make DRAMs match big rectangles so                                 
//                      maxj doesn't complain, but only fill                              
//                      and load valid stuff                                  
                                                      

                                                      

//     ##################
//     # MatMult Kernel #
//     ##################


//                 Horizontal Blocking
//                 _______________     _________LDB___________     
//                |               |   |                       |    
//                |               |   |                       |    
//                |      id0      |   |                       |    
//                |__LDA__↓       |   |_______________________|    
//                |_______|_\     |  inner_N__________________|    
//                |               |   |                       |    
//                |               |   |                       |    
//                |_______________|   |_______________________|    


                                                                       
//                 Vertical Blocking                                                       
                                                                       
                                                                       
//                 _______________     _________LDB___________                                                                     
//                |               |   |                       |                                                                                                        
//                |               |   |                       |                                                                                                        
//                |               |   |                       |                                                                                                        
//                |  id0,1        |   |_______________________|                                       
//                |    ↳|_\       |  K|_______________________|                                       
//                |     | |       |   |                       |                                       
//                |   LDA |       |   |                       |                                                                   
//                |_____|_|_______|   |_______________________|                                              
                                                                                 


// */




// object TRSM extends SpatialAppCompiler with TRSMApp
// trait TRSMApp extends SpatialApp {
//   type T = Flt //FixPt[Signed,B16,B16]
//   type Array[T] = ForgeArray[T]

//   def blockedGEMMH(id0: Rep[SInt], L: Rep[SRAM[T]],
//     B: Rep[SRAM[T]], LDB: Rep[SInt],
//     K: Rep[SInt]) = {

//     Sequential(id0 by 1) { k => 
//       Sequential(K by 1, LDB by 1) { (i,j) => 
//         val Laddr0 = id0 + i
//         val data = B(Laddr0,j) - L(Laddr0,k)*B(k,j)
//         sram_store_nd(B, List(Laddr0,j), data, Some(id0 !=0))
//         // B(Laddr0, j) = B(Laddr0,j) - L(Laddr0,k)*B(k,j)
//       }
//     }

//   }

//   def rank1update(L: Rep[SRAM[T]], diag_index: Rep[SInt], len: Rep[Reg[SInt]], 
//     B: Rep[SRAM[T]], K:Rep[SInt]) = {
//     Sequential(len.value by 1) { i => 
//       Sequential(K by 1) { j => 
//         val update_row = diag_index + 1 + i
//         val data = B(update_row,j) - L(update_row, diag_index)*B(diag_index, j)
//         sram_store_nd(B, List(update_row,j), data, Some(len.value != 0))
//         // B(update_row,j) = B(update_row,j) - L(update_row, diag_index)*B(diag_index, j)
//       }
//     }
//   }

//   def blockedGEMMV(id0: Rep[SInt], id1: Rep[SInt],
//     L: Rep[SRAM[T]], B: Rep[SRAM[T]], LDA: Rep[SInt], LDB: Rep[SInt],
//     K: Rep[SInt]) = {

//     Sequential(K by 1) { k => 
//       Pipe(LDA by 1, LDB by 1) { (i,j) =>
//         val Laddr0 = id0 + i
//         val Baddr0 = id0 - K + k
//         val Laddr1 = id1 + k
//         B(Laddr0, j) = B(Laddr0,j) - L(Laddr0,Laddr1)*B(Baddr0,j)
//       }
//     }
//   }

//   def SGEMM(M: Rep[SInt], N: Rep[SInt], K: Rep[SInt], alpha: Rep[T], 
//     A: Rep[SRAM[T]], LDA: Rep[SInt], B: Rep[SRAM[T]], LDB: Rep[SInt],
//     beta: Rep[T], C: Rep[SRAM[T]], LDC: Rep[SInt]) = {

//     // ALTERNATIVE 1: By outer products, direct access
//     Sequential(K by 1) { k => 
//       Pipe(LDA by 1, LDB by 1) { (i,j) =>
//         C(i,j) = beta*C(i,j) + alpha*A(i,k)*B(k,j)
//       }
//     }

//     // ALTERNATIVE 2: By outer products, block reduce
//     val C_part = SRAM[T](LDA,LDB)
//     Pipe(LDA by 1, LDB by 1){ (i,j) => C_part(i,j) = C(i,j) }
//     Fold(K by 1)(C_part, 0.as[T]) { k => 
//       val update = SRAM[T](LDA,LDB)
//       Pipe(LDA by 1, LDB by 1) { (i,j) => 
//         update(i,j) = alpha*A(i,k)*B(j,k)
//       }
//       update
//     }{ (C_tile, update_tile) => C_tile*beta + update_tile }
//     Pipe(LDA by 1, LDB by 1){ (i,j) => C(i,j) = C_part(i,j) }


//     // ALTERNATIVE 3: By inner products, direct access
//     Sequential(LDA by 1, LDB by 1) { (i,j) => 
//       val update = Reduce(K by 1)(0.as[T]) { k => A(i,k)*B(k,j) }{_+_}
//       C(i,j) = beta*C(i,j) + alpha*update
//     }

//   }


//   val inner_N = 8   // inner_N < k usually
//   val full_N = 136
//   val full_K = 96
//   val aligned_N = 192
//   val margin = 1

//   def trsm(B: Rep[Array[T]], L: Rep[Array[T]]) = {

//     val OCB    = DRAM[T](aligned_N,full_K)
//     val OCL    = DRAM[T](aligned_N,full_N)
//     val OCX    = DRAM[T](aligned_N*full_K)
//     setMem(OCB, B)
//     setMem(OCL, L)


//     Accel { Sequential{
//       val B = SRAM[T](full_N,full_K)
//       val L = SRAM[T](full_N,full_N)
//       val X = SRAM[T](aligned_N*full_K)
//       Parallel{
//         B := OCB(0::full_N, 0::full_K)
//         L := OCL(0::full_N, 0::full_N)
//       }
//       Sequential(full_N by inner_N) { diag_tile =>
//         // Horizontal Blocking
//         val id0 = diag_tile
//         val LDA = diag_tile
//         blockedGEMMH(id0, L, B, full_K, inner_N)

//         Sequential(inner_N by 1) { diag_index => 
//           val diag_addr = diag_index + diag_tile
//           val lambda = L(diag_addr, diag_addr)
//           Sequential(full_K by 1) { k => B(diag_addr,k) = B(diag_addr,k) / lambda }
//           // Rank 1 update (outer product, subtraction accumulator)
//           val len = Reg[SInt]
//           Pipe{len := inner_N.as[SInt] - diag_index - 1}
//           rank1update(L, diag_addr, len, B, full_K)
//         }

//         // // Vertical Blocking
//         // val id0 = diag_tile + inner_N
//         // val LDA = full_N.as[SInt] - diag_tile - inner_N.as[SInt]
//         // blockedGEMMV(id0, diag_tile, L,B, LDA, full_K, inner_N) // TODO: Can be rewritten messily to outerprod B rows as they finish

//       }
//       // Pack result to 1D, to avoid burst alignment issues
//       Pipe(full_N by 1) { i => Pipe(full_K by 1) { j => X(i*full_K + j) = B(i,j)}}
//       Pipe{OCX(0::aligned_N*full_K) := X}        
//     }}
//     getMem(OCX)
//   }

//   def printArr(a: Rep[Array[T]], numel:Int, str: String = "") {
//     println(str)
//     (0 until numel) foreach { i => print(a(i) + " ") }
//     println("")
//   }

//   def main() = {

//     val B = Array.fill(aligned_N){ Array.fill(full_K){ random[T](1)} }
//     val L = Array.tabulate(aligned_N){ i => Array.tabulate(full_N){ j =>
//       if (j > i) 0.as[T] 
//       else if (j == i) random[T](10)
//       else random[T](0.5)
//     }}

//     val result = trsm(B.flatten, L.flatten)

//     printArr(B.flatten, full_N*full_K, "B: ")
//     printArr(L.flatten, full_N*full_N, "L: ")
//     printArr(result, full_N*full_K, "X: ")

//     val X_check = Array.tabulate(full_N){ i => Array.tabulate(full_K) { j => result(i*full_K + j) }}
//     val L_check = Array.tabulate(full_N){ i => Array.tabulate(full_N) { j => 
//       val row = L(i)
//       row(j)}}
//     val B_check = Array.tabulate(full_N){ i => Array.tabulate(full_K) { j =>
//       val row = B(i)
//       row(j) }}.flatten
//     val B_computed = Array.tabulate(full_N){i =>
//       val aRow = L_check(i)
//       Array.tabulate(full_K){j =>
//         val bCol = X_check.map{row => row(j)}
//         aRow.zip(bCol){_*_}.reduce{_+_}
//       }
//     }.flatten

//     val cksum = B_check.zip(B_computed){ (a,b) => a > b - margin && a < b + margin}.reduce{_&&_}
//     println("PASS: " + cksum + " (TRSM)")
//   }
// }

import spatial.compiler._
import spatial.library._
import spatial.shared._

/*

	Sketch of app:


    ###########
    # LAYER 1 #
    ###########
                                              N
                          (k) →        ___________________
                                      |                   |
                                   kc |                   |
                                      |     tileB         |
                                      |___________________|
                               P      |                   |
                                      |                   |
                                      |                   |
                                      |                   |     
                                      |                   |
          (k)                         |___________________|
          ↓                        x
                      P          
          ___kc_________________       ___________________
         |         |            |     |                   |
         |         |            |     |                   |
         |         |            |     |                   |
  M      |         |            |     |                   |
         |         |            |     |                   |
         |         |            |     |                   |
         |         |            |     |                   |
         | tileA   |            |     | tileC             |
         |_________|____________|     |___________________|
          
                
                         
             
    ###########
    # LAYER 2 #
    ##################
        # outer pipe #                     
        ##############                              N
                                       ___________________
                                      |                   |
                                    kc|                   |
                                      |     tileB         |
                                      |___________________|
                                  P   .                   .
                                      .                   .
                                      .                   .
                                      .                   .     
                                      .                   .
                                      .....................
                               x
(local_m)
     ↳   ___kc____..............       ___________________
        |         |            .      |                   |
     mc | tileA_ip|            .      |                   |
        |         |            .      |                   |
  M     |_________|            .      |                   |
        |         |            .      |                   |
        |         |            .      |                   |
        |         |            .      |                   |
        |         |            .      | tileC             |
        |_________|.............      |___________________|


    ###########
    # LAYER 2 #
    ##################
        # inner pipe #                     
        ##############
                                  (local_n)
                                      ↓
                                       _nr _______________
                                      |   |               |
                                    kc|tileB_pj           |
                                      |   |               |
                                      |___|_______________|
                                      .                   .
                                      .                   .
                                      .                   .
                                      .                   .     
                                      .                   .
                                      .....................
                        x
                                 (local_m,local_n)
         ___kc____ ............       ↓___________________
        |         |            .     ↳|   |               |
     mc | tileA_ip|            .      |tileC_acc          |
        |         |            .      |   |               |
  M     |_________|            .      |___|               |
        .         .            .      |                   |
        .         .            .      |                   |
        .         .            .      |                   |
        .         .            .      |                   |
        ........................      |___________________|


    ###########
    # LAYER 3 #
    ###########
                                 

                                             (compute_n)
                                     ↓ ↓
                                      _nr ................
                        (compute_k) →|o o|               .
                                   kc|   |               .
                                     |   |               .
                                     |___|................
                                     .                   .
                                     .                   .
    (compute_m)                      .                   .
    |                                .                   .     
    | (accum_m)                      .                   .
    |  |                             .....................
    |  | (compute_k)           x
    |  | ↓     
    |  ↳ ____kc___ ............      ___ ................
    ↳   |o        |            .    |o o|               .
    ↳   |o        |            .    |o o|               .
      mc|o        |            .    |   |               .
  M     |o        |            .    |   |               .
        .‾‾‾‾‾‾‾‾‾.            .    .‾‾‾                .
        .         .            .    .                   .
        .         .            .    .                   .
        .         .            .    .                   .
        ........................    .....................


*/

object GEMM extends SpatialAppCompiler with GEMMApp
trait GEMMApp extends SpatialApp {
  type T = SInt //FixPt[Signed,B16,B16]
  type Array[T] = ForgeArray[T]

  def GEMM(A: Rep[Array[T]], B: Rep[Array[T]], C: Rep[Array[T]], mm: Rep[SInt], nn: Rep[SInt], pp: Rep[SInt]) = {
    // val M = ArgIn[SInt]
    // val N = ArgIn[SInt]
    // val P = ArgIn[SInt]
    // setArg(M,mm)
    // setArg(N,nn)
    // setArg(P,pp)
    val M = 96
    val N = 96
    val P = 96


    val a = OffChipMem[T](M, P) 
    val b = OffChipMem[T](P, N) 
    val c = OffChipMem[T](M, N)
    levelOf(a) = 0
    levelOf(b) = 0
    levelOf(c) = 0

// lets solve  A(480x96)x B(96x480)= C (480x480)


    val n_r        = param(96) // rank-1 updated width/height
    // We could have two n_r but we assume we do square rank-1 updated

    val m_c        = param(96) // blockram number of A matrix rows in each tile
    val k_c        = param(96) // number of B rows/A Columns  in each tile
    val n_r_par    = param(1)

    setMem(a, A)
    setMem(b, B)
    setMem(c, C)

    Accel {


      // **LAYER 1** ON-CHIP MEMORY 
      val tileC = BRAM[T](M, N)
      // TODO: Let tileC load from offchip here, because it currently complains about multiple writers
      // tileC := c(0::M, 0::N, param(1));
      //loaded C into the chip

      Pipe(P by k_c) { k =>
        val tileA = BRAM[T](M, k_c)
        val tileB = BRAM[T](k_c, N)
        levelOf(tileA) = 1
        levelOf(tileB) = 1
        Parallel {
          tileA := a(0::M, k::k+k_c, param(1))
          tileB := b(k::k+k_c, 0::N, param(1))
        }
        // Loaded On-chip memory


        // **LAYER 2** LOCAL-LEVEL STORE
        //      *outer*
        Pipe(M by m_c) { local_m =>
          val tileA_ip = BRAM[T](m_c, k_c)
          val tileB_pj = BRAM[T](k_c, n_r) 

          // DATA MOVEMENT
          Pipe(m_c by 1, k_c by 1) { (copy_m, copy_k) => tileA_ip(copy_m, copy_k) = tileA(local_m + copy_m, copy_k) }
          // Loaded local store level of tileA_ip

          // **LAYER 2** LOCAL-LEVEL STORE
          //      *inner*
          Pipe(N by n_r) { local_n =>
            // DATA MOVEMENT
            Pipe(k_c by 1, n_r by 1) { (copy_k, copy_n) => tileB_pj(copy_k, copy_n) = tileB(copy_k, local_n + copy_n) }
            // Loaded local store level of tileB_pj


            // **LAYER 3** ACCUMULATOR (REGISTER LEVEL)     
            val tileC_prev = BRAM[T](n_r,n_r)
            val tileC_acc = BRAM[T](n_r,n_r) // TODO: Merge these three double buffers into a triple buffer?
            // val tileC_acc_post = BRAM[T](n_r,n_r)
            Pipe(m_c by n_r){ accum_m =>
              // DATA MOVEMENT
              Pipe(n_r by 1, n_r by 1) { (copy_m, copy_n) => 
                // TODO: Use tileC_acc as accumulator, which doesn't work now because multiple writers to it...
                // tileC_acc(copy_m, copy_n) = tileC(local_m + accum_m + copy_m, local_n + copy_n) 
                tileC_prev(copy_m, copy_n) = tileC(local_m + accum_m + copy_m, local_n + copy_n) 
                // TODO: Handle corner cases if triple buffered
              }


              Fold(k_c by 1)(tileC_acc, 0.as[T]) { compute_k =>
              	val tileC_partial = BRAM[T](n_r,n_r)
                Pipe(n_r by 1, n_r by 1) { (compute_m, compute_n) => 
                  tileC_partial(compute_m, compute_n) = tileA_ip(compute_m, compute_k) * tileB_pj(compute_k, compute_n) + tileC_prev(compute_k, compute_n)
                }
                tileC_partial
              }{_+_}

              // DATA MOVEMENT
              Pipe(n_r by 1, n_r by 1) { (copy_m, copy_n) =>
                tileC(local_m + accum_m + copy_m, local_n + copy_n) = tileC_acc(copy_m, copy_n)
              }
            }
          }
        }
      }
      c(0::M, 0::N, param(1)) := tileC

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
    val c = Array.fill(M){ Array.fill(N){0} }
    // val a = Array.fill(M){ Array.fill(P){random[T](100)} }
    // val b = Array.fill(P){ Array.fill(N){random[T](100)} }

    val result = GEMM(a.flatten, b.flatten, c.flatten, M, N, P)

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

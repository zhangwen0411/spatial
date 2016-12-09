import spatial.compiler._
import spatial.library._
import spatial.shared._

object ConvTest extends SpatialAppCompiler with ConvApp
trait ConvApp extends SpatialApp {
  type Array[T] = ForgeArray[T]

  def conv(sImg: Rep[Array[Flt]], sKernel: Rep[Array[Flt]]) = {
    // TODO: The convolution node is the same granularity / level 
    // of abstraction as TensorFlow's conv2D, which
    //  "Computes a 2-D convolution given 4-D input and filter tensors."
    // Currently the node reads inputs from and writes inputs to DRAM.
    // In the future the node will use SRAM as input/output so that
    // subsequent layers do not need to read from DRAM, and then to
    // support banking these DRAMs and SRAMs can be multi-dimensional.
    // For now the DRAM is 1-dimensional.
    val img = DRAM[Flt](224*224*3)
    val kernel = DRAM[Flt](11*11*3*96)
    val outImg = DRAM[Flt](54*54*96)

    setMem(img, sImg)
    setMem(kernel, sKernel)

    Accel {
      // Should use this one (output dim = 96) but then interrupt never happens? App finishes but no interrupt
      // convolve(img, kernel, outImg, List(224,224,3), List(11,11,3,96), List(4,4), List(unit(11),unit(11)))
      convolve(img, kernel, outImg, List(224,224,3), List(11,11,3,1), List(4,4), List(unit(11),unit(11)))
    }

    getMem(outImg)
  }

  def main() {
  
    val sImg = Array.tabulate(224*224*3){i =>
                if      (i <  224*224*3* 1/4) (0.1).as[Flt]
                else if (i <  224*224*3* 2/4) (0.2).as[Flt]
                else if (i <  224*224*3* 3/4) (0.3).as[Flt]
                else    (0.4).as[Flt]
               }
    val sKernel = Array.tabulate(11*11*3*96){i =>
                  (0.1).as[Flt]
               }

    val result = conv(sImg, sKernel)

    // for (k <- 0 until 96){
    for (k <- 0 until 1){
      for (j <- 0 until 54){
        for (i <- 0 until 54){
          print(result(k*54*54 + j*54 + i) + " ")
        }
        println("")
      }
      println("----------------------------------------")
    }
  }
}

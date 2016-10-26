import spatial.compiler._
import spatial.library._
import spatial.shared._

object ConvTest extends SpatialAppCompiler with ConvApp
trait ConvApp extends SpatialApp {
  type Array[T] = ForgeArray[T]

  def conv(sImg: Rep[Array[SInt]], sKernel: Rep[Array[SInt]]) = {
    val img = DRAM[SInt](10,10)
    val kernel = DRAM[SInt](3,3)
    val outImg = DRAM[SInt](10,10)

    setMem(img, sImg)
    setMem(kernel, sKernel)

    Accel {
      val out = SRAM[SInt](10,10)
      convolve(img, kernel, out, List(1,1), List(unit(1),unit(1)))

      outImg(0::10,0::10) := out
    }

    getMem(outImg)
  }

  def main() {
    val sImg = Array.tabulate(10){i =>
               Array.tabulate(10){j =>
                 if (j > 5) 9 else 0
               }}
    val sKernel = Array.tabulate(3){i =>
                  Array.tabulate(3){j =>
                    if      (i == 0 && j == 0) -1
                    else if (i == 1 && j == 0) -2
                    else if (i == 2 && j == 0) -1
                    else if (i == 0 && j == 2)  1
                    else if (i == 1 && j == 2)  2
                    else if (i == 2 && j == 2)  1
                    else 0
                  }}

    val result = conv(sImg.flatten, sKernel.flatten)

    for (i <- 0 until 10){
      for (j <- 0 until 10){
        println(result(i*10+j))
      }
    }
  }
}

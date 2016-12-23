// See LICENSE.txt for license details.
package templates

import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import utils.TutorialRunner

object Launcher {
  val templates = Map(
      "FF" -> { (backendName: String) =>
        Driver(() => new FF(32), "verilator") {
          (c) => new FFTests(c)
        }
      },
      "FFNoInit" -> { (backendName: String) =>
        Driver(() => new FFNoInit(32), "verilator") {
          (c) => new FFNoInitTests(c)
        }
      },
      "TFF" -> { (backendName: String) =>
        Driver(() => new TFF(), "verilator") {
          (c) => new TFFTests(c)
        }
      },
      "Parallel" -> { (backendName: String) =>
        Driver(() => new Parallel(3), "verilator") {
          (c) => new ParallelTests(c)
        }
      },
      "Sequential" -> { (backendName: String) =>
        Driver(() => new Sequential(8), "verilator") {
          (c) => new SequentialTests(c)
        }
      }
  )
  def main(args: Array[String]): Unit = {
    TutorialRunner(templates, args)
  }
}


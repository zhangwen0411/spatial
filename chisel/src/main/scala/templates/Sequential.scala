// See LICENSE.txt for license details.
package templates

import chisel3._
import chisel3.util._

//A n-bit Sequential with carry in and carry out
class Sequential(val n:Int) extends Module {
  val io = IO(new Bundle {
    val A    = Input(UInt(n.W))
    val B    = Input(UInt(n.W))
    val Cin  = Input(UInt(1.W))
    val Sum  = Output(UInt(n.W))
    val Cout = Output(UInt(1.W))
  })
  //create a vector of FullSequentials
  val FAs   = Vec.fill(n)(Module(new FullAdder()).io)
  val carry = Wire(Vec(n+1, UInt(1.W)))
  val sum   = Wire(Vec(n, Bool()))

  //first carry is the top level carry in
  carry(0) := io.Cin

  //wire up the ports of the full Sequentials
  for (i <- 0 until n) {
    FAs(i).a := io.A(i)
    FAs(i).b := io.B(i)
    FAs(i).cin := carry(i)
    carry(i+1) := FAs(i).cout
    sum(i) := FAs(i).sum.toBool()
  }
  io.Sum := Cat(sum.reverse)
  io.Cout := carry(n)
}

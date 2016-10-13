package spatial.library.classes

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}

import spatial.shared._
import spatial.shared.ops._
import spatial.library._
import spatial.library.classes._

trait ControllerWrapper {
  this: SpatialBase with SpatialClasses =>

  private def loop(cchain: Rep[CounterChain], idx: Int, indices: List[FixPt[Signed,B32,B0]], func: Rep[Indices] => Rep[Unit]): Rep[Unit] = {
    val ctr = cchain(idx)
    if (idx >= cchain.length - 1) {
      for (i <- ctr) { func(indices_create(indices :+ i.head)) }
    }
    else {
      for (i <- ctr) { loop(cchain, idx+1, indices :+ i.head, func) }
    }
  }
  private def loopList(cchain: Rep[CounterChain], idx: Int, indices: List[FixPt[Signed,B32,B0]], func: List[Rep[FixPt[Signed,B32,B0]]] => Rep[Unit]): Rep[Unit] = {
    val ctr = cchain(idx)
    if (idx >= cchain.length - 1) {
      for (i <- ctr) { func(indices :+ i.head) }
    }
    else {
      for (i <- ctr) { loopList(cchain, idx+1, indices :+ i.head, func) }
    }
  }

  def parallel_pipe(func: => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = func
  def unit_pipe(func: => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = func

  def foreach_op(cchain: Rep[CounterChain], func: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Pipeline] = {
    loop(cchain, 0, Nil, func)
  }

  def reduce_op[T,C[T]](cchain: Rep[CounterChain], accum: Rep[C[T]], zero: Option[Rep[T]], func: Rep[Indices] => Rep[T], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __num: Num[T], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline]  = {
    loop(cchain, 0, Nil, {i: Rep[Indices] =>
      __mem.zeroSt(accum, rFunc(__mem.zeroLd(accum, true), func(i)), true)
    })
  }

  def memreduce_op[T,C[T]](cchain: Rep[CounterChain], cchainRed: Rep[CounterChain], accum: Rep[C[T]], zero: Option[Rep[T]], func: Rep[Indices] => Rep[C[T]], rFunc: (Rep[T],Rep[T]) => Rep[T], foldAccum: Boolean = true)(implicit ctx: SourceContext, __mem: Mem[T,C], __num: Num[T], __mT: Manifest[T], __mC: Manifest[C[T]]): Rep[Pipeline] = {
    var first = true
    loop(cchain, 0, Nil, {i =>
      val part = func(i)

      loopList(cchainRed, 0, Nil, {j =>
        if (first && !foldAccum)
          __mem.st(accum, j, __mem.ld(part, j, true), true)
        else
          __mem.st(accum, j, rFunc(__mem.ld(part, j, true), __mem.ld(accum, j, true)), true)
      })
      first = false
    })
  }

}

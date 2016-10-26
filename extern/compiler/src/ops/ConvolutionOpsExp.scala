package spatial.compiler.ops

import scala.virtualization.lms.common.{ScalaGenEffect, MaxJGenEffect, MaxJGenFat}
import scala.reflect.{Manifest,SourceContext}
import java.io.{File, FileWriter, PrintWriter}

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

trait ConvolutionOpsExp extends ControllerOpsExp with NodeMetadataOpsExp {
  this: SpatialExp =>

  // --- Nodes
  case class Convolve[T:Manifest](
    image:   Rep[DRAM[T]],
    kernel:  Rep[DRAM[T]],
    output:  Rep[SRAM[T]],
    strides: List[Int],
    pars:    List[Rep[Int]],
    inds:    List[Sym[Idx]]
  ) extends Def[Unit] { val mT = manifest[T] }

  case class ConvLayer[T:Manifest](
    image:   Rep[SRAM[T]],
    kernel:  Rep[DRAM[T]],  // TODO: Should this still be offchip?
    output:  Rep[SRAM[T]],
    strides: List[Int],
    pars:    List[Rep[Int]],
    inds:    List[Sym[Idx]]
  ) extends Def[Unit] { val mT = manifest[T] }

  def convolve[T:Manifest](image: Rep[DRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    val ps = pars.map(parize(_))
    val inds = List.tabulate(dimsOf(output).length){i => fresh[Idx] }

    reflectWrite(output)(Convolve(image,kernel,output,strides,ps,inds))
  }

  def convLayer[T:Manifest](image: Rep[SRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    val ps = pars.map(parize(_))
    // TODO: Are dimensions of input and output always the same?
    val inds = List.tabulate(dimsOf(output).length){i => fresh[Idx] }
    reflectWrite(output)(ConvLayer(image,kernel,output,strides,ps,inds))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(e@Convolve(m,k,o,s,p,i), u, es) => reflectMirrored(Reflect(Convolve(f(m),f(k),f(o),s,f(p),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)
    case Reflect(e@ConvLayer(m,k,o,s,p,i), u, es) => reflectMirrored(Reflect(ConvLayer(f(m),f(k),f(o),s,f(p),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e:Convolve[_]  => Nil
    case e:ConvLayer[_] => Nil
    case _ => super.aliasSyms(e)
  }
}


trait ScalaGenConvolutionOps extends ScalaGenEffect {
  val IR: ConvolutionOpsExp with SpatialCodegenOps
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Convolve(img, kernel, output, strides, pars, inds) =>
      //val imgDims = dimsOf(img)
      //val kerDims = dimsOf(kernel)



    case _:ConvLayer[_] => throw new Exception("What are you doing dave?")
    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenConvolutionOps extends MaxJGenEffect with MaxJGenFat {
  val IR: ConvolutionOpsExp with SpatialCodegenOps
  import IR._

  // STEFAN STUFF GOES HERE

}

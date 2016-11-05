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
    output:  Rep[DRAM[T]],
    img_dims: List[Int],
    k_dims: List[Int],
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

  def convolve[T:Manifest](image: Rep[DRAM[T]], kernel: Rep[DRAM[T]], output: Rep[DRAM[T]], img_dims: List[Int], k_dims: List[Int], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    val ps = pars.map(parize(_))
    val inds = List.tabulate(dimsOf(output).length){i => fresh[Idx] }

    reflectWrite(output)(Convolve(image,kernel,output,img_dims,k_dims,strides,ps,inds))
  }

  def convLayer[T:Manifest](image: Rep[SRAM[T]], kernel: Rep[DRAM[T]], output: Rep[SRAM[T]], strides: List[Int], pars: List[Rep[Int]])(implicit ctx: SourceContext): Rep[Unit] = {
    val ps = pars.map(parize(_))
    // TODO: Are dimensions of input and output always the same?
    val inds = List.tabulate(dimsOf(output).length){i => fresh[Idx] }
    reflectWrite(output)(ConvLayer(image,kernel,output,strides,ps,inds))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(e@Convolve(m,k,o,id,kd,s,p,i), u, es) => reflectMirrored(Reflect(Convolve(f(m),f(k),f(o),id,kd,s,f(p),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)
    case Reflect(e@ConvLayer(m,k,o,s,p,i), u, es) => reflectMirrored(Reflect(ConvLayer(f(m),f(k),f(o),s,f(p),i)(e.mT), mapOver(f,u), f(es)))(mtype(manifest[A]),pos)
    case _ => super.mirror(e,f)
  }

  override def syms(e: Any) = e match {
    case e: Convolve[_]  => syms(List(e.image,e.kernel,e.output)) ++ syms(e.pars)
    case e: ConvLayer[_] => syms(List(e.image,e.kernel,e.output)) ++ syms(e.pars)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: Convolve[_]  => readSyms(List(e.image,e.kernel,e.output)) ++ readSyms(e.pars)
    case e: ConvLayer[_] => readSyms(List(e.image,e.kernel,e.output)) ++ readSyms(e.pars)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: Convolve[_]  => freqNormal(e.image) ++ freqNormal(e.kernel) ++ freqNormal(e.output) ++ freqNormal(e.pars)
    case e: ConvLayer[_] => freqNormal(e.image) ++ freqNormal(e.kernel) ++ freqNormal(e.output) ++ freqNormal(e.pars)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: Convolve[_]  => e.inds
    case e: ConvLayer[_] => e.inds
    case _ => super.boundSyms(e)
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
    case Convolve(img, kernel, output, img_dims, k_dims, strides, pars, inds) =>
      //val imgDims = dimsOf(img)
      //val kerDims = dimsOf(kernel)



    case _:ConvLayer[_] => throw new Exception("What are you doing dave?")
    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenConvolutionOps extends MaxJGenEffect with MaxJGenFat {
  val IR: ConvolutionOpsExp with SpatialCodegenOps with UnrolledOpsExp 
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case Convolve(img, kernel, output, img_dims, k_dims, strides, pars, inds) =>
    
      // Determine parameters
      
      assert(img_dims.length > 1)
      assert(k_dims.length > 1)
      
      assert(img_dims.length < 5)
      assert(k_dims.length < 5)
      assert(strides.length < 3)
      
      assert(k_dims(0) == k_dims(1))
      
      val inH = img_dims(0)
      val inW = img_dims(1)
      val k = k_dims(0)
      val p = pars(0)
      val s = strides(0)

      var inD = 1;
      var outD = 1;
      
      if (strides.length == 2) {
        assert(strides(0) == strides(1))
      }
      
      if (img_dims.length == 2) {
        assert(k_dims.length == 2)
      }
      else if (img_dims.length == 3) {
        assert(k_dims.length > 2)
        assert(img_dims(2) == k_dims(2))
        inD = img_dims(2)
      }
      else if (img_dims.length == 4) {
        assert(img_dims(3) == 1) // Batch size 1 only for now
      }
      
      if (k_dims.length == 4) {
        outD = k_dims(3)
      }
      
      var outH = (inH + 2*0 - k)/s + 1
      var outW = (inW + 2*0 - k)/s + 1

      // Get DRAM sizes too now
      // These should be adjust(inH*inW, 384) and adjust(outH*outW, 384) repectively
      // Could do that calculation here but it is done inside app instead
      
      val img_DRAM_dims = dimsOf(img)
      assert(img_DRAM_dims.length == 1)
      // val img_DRAM_sizes = img_DRAM_dims.map{dim => bound(dim).get.toInt}
      val img_DRAM_size = bound(img_DRAM_dims(0)).get.toInt
      
      val kernel_DRAM_dims = dimsOf(kernel)
      assert(kernel_DRAM_dims.length == 1)
      // val kernel_DRAM_sizes = kernel_DRAM_dims.map{dim => bound(dim).get.toInt}
      val kernel_DRAM_size = bound(kernel_DRAM_dims(0)).get.toInt
      
      val output_DRAM_dims = dimsOf(output)
      assert(output_DRAM_dims.length == 1)
      // val output_DRAM_sizes = output_DRAM_dims.map{dim => bound(dim).get.toInt}
      val output_DRAM_size = bound(output_DRAM_dims(0)).get.toInt
    
      // HTML (see published/Spatial/controller_tree.html)
      print_stage_prefix(s"Convolution",s"",s"${quote(sym)}", false)
      
      emitComment(s"""

-------------------------------------------------------
Emitting Convolution Block with parameters:

- inH   =  ${inH }
- inW   =  ${inW }
- k     =  ${k   }
- par   =  ${quote(p)}
- s     =  ${s   }
- inD   =  ${inD }
- outD  =  ${outD}
- outH  =  ${outH}
- outW  =  ${outW}

- img DRAM:          ${quote(img)}
- kernel DRAM:       ${quote(kernel)}
- output DRAM:       ${quote(output)}
- img DRAM size      ${img_DRAM_size}
- kernel DRAM size   ${kernel_DRAM_size}
- output DRAM size   ${output_DRAM_size}

-------------------------------------------------------""")

      emit(raw"""//{
/*
  public TopKernel(KernelParameters parameters, int k, int p, int s, 
        int inH, int inW, int inD, int outD, int outH, int outW, boolean print_debug) {
    
    super(parameters);
*/
    
    // Begin convolution:

    int k = ${k};
    int p = ${quote(p)};
    int s = ${s};
    int inH = ${inH};
    int inW = ${inW};
    int inD = ${inD};
    int outD = ${outD};
    int outH = ${outH};
    int outW = ${outW};
    boolean print_debug = false;

    // Read parameters from host    
    // For now these are scalars (rather than constants) but can be constants, see
    // comment in manager
    // Edit: I made these DFEVar constants, could just be integers
    DFEVar real_input_len =   constant.var(dfeUInt(32), ${img_DRAM_size}); ; // io.scalarInput("realInLen", dfeUInt(32)); // SHADJIS TODO: optimize bit width
    DFEVar real_k_len =       constant.var(dfeUInt(32), ${kernel_DRAM_size}); ; // io.scalarInput("realKLen", dfeUInt(32)); // SHADJIS TODO: optimize bit width
    DFEVar real_output_len =  constant.var(dfeUInt(32), ${output_DRAM_size}); ; // io.scalarInput("realOutLen", dfeUInt(32)); // SHADJIS TODO: optimize bit width

    // Global FSM which keeps track of global state
    DFEVar new_row_in_FMEM = dfeBool().newInstance(this);
    DFEVar burst_adjusted_DRAM_read_en = dfeBool().newInstance(this);
    DFEVar done_processing_psums = dfeBool().newInstance(this);
    DFEVar datapath_enable = dfeBool().newInstance(this);
    DFEVar new_ifmap = dfeBool().newInstance(this);
    DFEVar last_ifmap = dfeBool().newInstance(this);
    DFEVar convlayer_done = dfeBool().newInstance(this);
    DFEVar first_ifmap = dfeBool().newInstance(this);
    DFEVar linebuf_read_enable = dfeBool().newInstance(this);
    SMIO sm = addStateMachine("globalFSM", new GlobalFSM(this, k, s, outH, inD, outD));
    sm.connectInput("new_row_in_FMEM", new_row_in_FMEM.cast(dfeBool()));
    sm.connectInput("burst_adjusted_DRAM_read_en", burst_adjusted_DRAM_read_en.cast(dfeBool()));
    sm.connectInput("done_processing_psums", stream.offset(done_processing_psums.cast(dfeBool()), -1));//SHADJIS TODO: undef cycle 0 but ok since not used in state 0
    datapath_enable <== sm.getOutput("datapath_enable");
                        //    stream.offset(sm.getOutput("datapath_enable"), -1);
    
    // SHADJIS TODO: I use stream offsets everywhere of -1 to eliminate MaxJ combinational loops,
    // but in some cases (e.g. linebuf_read_enable below) it is possible to refactor the code to
    // eliminate the 1 cycle delay. For linebuf_read_enable, can refactor the counters now in the 
    // LineBuffer lib to instead be inside the FSM and this eliminates the 1 cycle delay.
    // Doing that also means that the 1st cycle lb_read is high, so we can save that 1 cycle too (minor).
    // Recall this loop exists because we have an enable lb enable which enables the counter, and the
    // wrap of that enable (new row in fmem) which can set lb enable low in the FSM.
    // More comments:
    // - Another solution would be to have new row in fmem go high 1 cycle sooner (wrap - 1), but
    //   delay it 1 cycle. That way new row in fmem goes high, the next cycle lb enable is still high, 
    //   but 1 cycle later it goes low. Now instead I just made lb enable go low 1 cycle early (see 
    //   comment in global FSM)
    // - Also this is still undefined in cycle 0, so can make a counter up to 1 and have it hold its max
    //   value (like in shift reg lib). Currently I didn't add it yet so sometimes we will DRAM in cycle 0
    //   and others we won't, but doesn't matter because we only go to next global FSM state once this
    //   has been high a certain # cycles (i.e. it will just take 1 cycle extra if it randomly starts low)
    //   But we could always make this high in 1st cycle by adding a counter that counts e.g. 0 1 1 1
    linebuf_read_enable <== //sm.getOutput("linebuf_read_enable");
                            stream.offset(sm.getOutput("linebuf_read_enable"), -1);// SHADJIS TODO: undef cycle 0 (add 0 1 1 1 counter)
    new_ifmap <== sm.getOutput("new_ifmap");
                            // stream.offset(sm.getOutput("new_ifmap"), -1);// SHADJIS TODO: undef cycle 0 (add 0 1 1 1 counter)
    last_ifmap <== sm.getOutput("last_ifmap");
    convlayer_done <== sm.getOutput("convlayer_done");
    first_ifmap <== sm.getOutput("first_ifmap");
    
    ${quote(sym)}_done <== convlayer_done;
    
    // Kernel to read from LMEM
    // SHADJIS TODO: for now we assume stride perfectly divides row size, see ReaderKernelLib.maxj
    // for an explanation
    DFEVar us_data = dfeRawBits(32*s).newInstance(this); // us = "upstream"
    // DFEVar us_data = dfeFloat(8,24).newInstance(this); // us = "upstream"
    DFEVar DRAM_read_en = dfeBool().newInstance(this);
    DRAM_read_en <== linebuf_read_enable;
    burst_adjusted_DRAM_read_en <== (new ReaderKernel(this, inH*inW*inD/s, s)).doIt("${quote(img)}_${quote(sym)}_in", real_input_len/s /* i.e. multiple of 384 */, DRAM_read_en, us_data);
    
    // Read filter weights from LMEM too
    Count.Params paramsReadK = control.count.makeParams(MathUtils.bitsToAddress(k*k))
      .withMax(k*k)
      .withReset(new_ifmap)
      .withWrapMode(WrapMode.STOP_AT_MAX);
    Counter countReadK = control.count.makeCounter(paramsReadK);
    DFEVar k_data = dfeFloat(8,24).newInstance(this);
    DFEVar DRAM_k_read_en = countReadK.getCount() < k*k;
    DFEVar[] kernel = new DFEVar[k*k]; // Store into shift register
    kernel[0] = dfeFloat(8,24).newInstance(this);
    kernel[0] = Reductions.streamHold(k_data, DRAM_k_read_en);
    for(int i=1; i<k*k; i++) {
      kernel[i] = dfeFloat(8,24).newInstance(this);
      kernel[i] = Reductions.streamHold(stream.offset(kernel[i-1], -1), DRAM_k_read_en);
    }
///////////////////////////////
if (print_debug)
{
debug.simPrintf("\n\nCount=%d, Enable=%d, real_k_len=%d\n", countReadK.getCount(), DRAM_k_read_en, real_k_len);
    for (int i=0; i<k*k; ++i) {
      debug.simPrintf("%f    ",kernel[i]);
    }
debug.simPrintf("\n%f\n\n", k_data);
}
///////////////////////////////
    new ReaderKernelFloat(this, k*k*inD*outD).doIt("${quote(kernel)}_${quote(sym)}_in", real_k_len /* i.e. multiple of 384 */, DRAM_k_read_en, k_data);
    
    // Kernel to move from us_data into line buffer
    // Returns array of BRAMs, each holding 1 row
    int nBuffers = k + s; // SHADJIS TODO: This assumes stride < k, otherwise would be k + min(s,k)
    LineBuffer lb = new LineBuffer(this, inW, nBuffers, s, print_debug);
    Memory<DFEVar>[] linebuffer = lb.doIt(linebuf_read_enable, new_row_in_FMEM, us_data, new_ifmap);

    // Kernel to read line buffers and produce partial sums
    DFEVar DRAM_write_en = dfeBool().newInstance(this);
    ShiftReg sr = new ShiftReg(this, inW, nBuffers, s, p, k, outW, inD, outD, outH*outW, print_debug);
    DFEVar sum = sr.doIt(linebuffer, datapath_enable, done_processing_psums, DRAM_write_en, kernel, last_ifmap, first_ifmap, new_ifmap);
    
    // Write result back to LMEM
    new WriterKernel(this, outH*outW*outD).doIt("${quote(output)}_${quote(sym)}_out", real_output_len /* i.e. multiple of 384 */, DRAM_write_en, sum);
    
///////////////////////////////////////////////
if (print_debug)
{
    // Debugging
    DFEVar tick = control.count.simpleCounter(32,100000000);
    debug.simPrintf("%d: lineb_r_en=%d, new_ifmap=%d, last_ifmap=%d, convlayer_done=%d, new_row_in_FMEM=%d, datap_enable=%d, done_proc_psums=%d, DRAM_r_en=%d, DRAM_r_en_B=%d, DRAM_w_en=%d, out=%f,", tick, linebuf_read_enable, new_ifmap, last_ifmap, convlayer_done, new_row_in_FMEM, datapath_enable, done_processing_psums, DRAM_read_en, burst_adjusted_DRAM_read_en, DRAM_write_en, sum);
    debug.simPrintf("us_data=");
    // SHADJIS TODO: Replace this loop with a single %f once input data is a float again
    for (int i=0; i<s; ++i) {
      debug.simPrintf("%f,",us_data.slice(i*32, 32).cast(dfeFloat(8, 24)));
    }
    debug.simPrintf("\n==================================================\n");
}
else
{
    //DFEVar tick = control.count.simpleCounter(32,100000000);
    //DFEVar tick_interval = control.count.makeCounter(control.count.makeParams(32).withMax(100)).getWrap();
    //debug.simPrintf(tick_interval, "%d\n", tick+1);
}
///////////////////////////////////////////////
//}
""")

      emitComment(s"""-------------------------------------------------------
End of Convolution
-------------------------------------------------------

""")
      print_stage_suffix(quote(sym), false)

    case _ => super.emitNode(sym,rhs)
  }

}

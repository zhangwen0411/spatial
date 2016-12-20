package spatial.compiler.ops
import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.Set
import scala.reflect.SourceContext
import java.io.{PrintWriter}

trait MaxJManagerGen {
	val IR:SpatialExp with DRAMAddrAnalysisExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

	var stream:PrintWriter = _
	def emit(str: String):Unit = {
		stream.println(str)
	}

  def quote(x: Exp[Any]) = x match {
		case s@Sym(n) => {
			val tstr = s.tp.erasure.getSimpleName().replace("Spatial","")
      val customStr = tstr match {
        case "Pipeline" => styleOf(s) match {
          case CoarsePipe => "metapipe"
          case InnerPipe => "pipe"
          case StreamPipe => "strm"
          case SequentialPipe => "seq"
          case ForkJoin => "parallel"
        }
        case "Register" => regType(s) match {
          case Regular => "reg"
          case ArgumentIn => "argin"
          case ArgumentOut => "argout"
        }
        case _ => tstr
      }
			customStr + n
		}
    case _ => ""
  }
//  def quote(x: Exp[Any]):String = x match {
//		case s@Sym(n) =>
//      val tp = s.tp.erasure.getSimpleName().replace("Spatial", "")
//      tp + n
//    case _ => ""
//  }

  /**
   * The following three methods are largely duplicated in CGenMemoryOps.
   * The only differences are that BlockRAM and DRAM aren't remapped in the Manager,
   * and 'float' and 'double' types are remapped to capitalized versions (non-caps
   * versions are reserved keywords)
   * must ideally share the remap methods from CCodegen
   */
  private def bitsToIntType(bits: Int) = {
    if (bits <= 8) "8"
    else if (bits <= 16) "16"
    else if (bits <= 32) "32"
    else "64"
  }

  private def bitsToFloatType(bits: Int) = {
    if (bits <= 32) "FLOAT"
    else "DOUBLE"
  }

  def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "SpatialReg" => remap(m.typeArguments(0))
    case "SpatialBit" => "uint8_t"
    case "Signed" => ""
    case "Unsign" => "u"
    case "FixedPoint" => remap(m.typeArguments(0)) + "int" + bitsToIntType(remap(m.typeArguments(1)).toInt + remap(m.typeArguments(2)).toInt) + "_t"
    case "FloatPoint" => bitsToFloatType(remap(m.typeArguments(0)).toInt + remap(m.typeArguments(1)).toInt)
    case bx(n) => n
    case _ => throw new Exception(s"""No remap rule in MaxJManagerGen for ${m.erasure.getSimpleName}""")
  }



  val mImportPrefix = "com.maxeler.maxcompiler.v2"
  val streamClock = 150
  val memClock = 400
  val mImports = List(
    "build.EngineParameters",
    "kernelcompiler.Kernel",
    "managers.standard.Manager",
    "managers.standard.Manager.MemAccessPattern",
    "managers.standard.Manager.IOType",
    "managers.engine_interfaces.EngineInterface",
    "managers.engine_interfaces.EngineInterface.Direction",
    "managers.engine_interfaces.InterfaceParam",
    "managers.engine_interfaces.CPUTypes",
    "managers.custom.CustomManager",
    "managers.custom.DFELink",
    "managers.custom.blocks.KernelBlock",
    "managers.custom.stdlib.DebugLevel",
    "managers.BuildConfig",
    "managers.custom.stdlib.MemoryControllerConfig",
    "kernelcompiler.KernelConfiguration",
    "kernelcompiler.KernelConfiguration.OptimizationOptions",
    "kernelcompiler.KernelConfiguration.OptimizationOptions.OptimizationTechnique"
  )

  val oldMemImports = List(
    "managers.custom.stdlib.MemoryControlGroup"
    )

  val newMemImports = List(
    "managers.custom.stdlib.LMemCommandGroup",
    "managers.custom.stdlib.LMemInterface"
  )

  val mPreamble =
	s"""
	class TopManager extends CustomManager {
	  private static final CPUTypes int8_t =    CPUTypes.INT8;
	  private static final CPUTypes int16_t =    CPUTypes.INT16;
	  private static final CPUTypes int32_t =    CPUTypes.INT32;
	  private static final CPUTypes int64_t =    CPUTypes.INT64;
	  private static final CPUTypes uint8_t =    CPUTypes.UINT8;
	  private static final CPUTypes uint16_t =    CPUTypes.UINT16;
	  private static final CPUTypes uint32_t =    CPUTypes.UINT32;
	  private static final CPUTypes uint64_t =    CPUTypes.UINT64;
	  private static final CPUTypes FLOAT = CPUTypes.FLOAT;
	  private static final CPUTypes DOUBLE = CPUTypes.DOUBLE;
	"""

	  val mEpilogue =
	s"""
	  public static void main(String[] args) {
	    TopManager m = new TopManager(new EngineParameters(args));

	    BuildConfig c = new BuildConfig(BuildConfig.Level.FULL_BUILD);
	    c.setBuildEffort(BuildConfig.Effort.HIGH);
	    c.setEnableTimingAnalysis(true);
	    m.setBuildConfig(c);

	    m.createSLiCinterface(interfaceRead("readLMem"));
	    m.createSLiCinterface(interfaceWrite("writeLMem"));
	    m.createSLiCinterface(interfaceDefault());
	    m.build();
	  }
	}
	"""

	val mReadIntf =
	s"""
	  // CPU -> LMEM (read interface)
	  private static EngineInterface interfaceRead(String name) {
	    EngineInterface ei = new EngineInterface(name);
	    InterfaceParam size = ei.addParam("size", uint32_t);
	    InterfaceParam start = ei.addParam("start", uint32_t);
	    InterfaceParam sizeInBytes = size;

	    // Stop the kernel from running
	    ei.setScalar("TopKernel", "en", 0);

	    // Setup address map and access pattern
	    ei.setLMemLinear("fromlmem", start, sizeInBytes);
	    ei.setStream("tocpu", uint8_t, sizeInBytes);
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val mWriteIntf =
	s"""
	  // LMEM -> CPU (write interface)
	  private static EngineInterface interfaceWrite(String name) {
	    EngineInterface ei = new EngineInterface(name);
	    InterfaceParam size = ei.addParam("size", uint32_t);
	    InterfaceParam start = ei.addParam("start", uint32_t);
	    InterfaceParam sizeInBytes = size;

	    // Stop the kernel from running
	    ei.setScalar("TopKernel", "en", 0);

	    // Setup address map and access pattern
	    ei.setLMemLinear("tolmem", start, sizeInBytes);
	    ei.setStream("fromcpu", uint8_t, sizeInBytes);
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val mDefaultIntfPreamble =
	s"""
	  // Interface to run DFE (default interface)
	  private static EngineInterface interfaceDefault() {
	    EngineInterface ei = new EngineInterface();
	    ei.setTicks("TopKernel", Long.MAX_VALUE);
	    ei.setScalar("TopKernel", "en", 1);
	"""

	val mDefaultIntfEpilogue =
	s"""
	    ei.setLMemInterruptOn("intrStream");
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val cpuIntfOld =
	s"""
	    // Setup CPU <-> FPGA stream
	    DFELink fromcpu = addStreamFromCPU("fromcpu");
	    DFELink tocpu = addStreamToCPU("tocpu");
	    DFELink fromlmem = addStreamFromOnCardMemory("fromlmem", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);
	    DFELink tolmem = addStreamToOnCardMemory("tolmem", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);
	    tolmem <== fromcpu;
	    tocpu <== fromlmem;
	"""
	val cpuIntfNew =
	s"""
	    // Setup CPU <-> FPGA stream
	    DFELink fromcpu = addStreamFromCPU("fromcpu");
	    DFELink tocpu = addStreamToCPU("tocpu");
	    DFELink fromlmem = addStreamFromLMem("fromlmem", LMemCommandGroup.MemoryAccessPattern.LINEAR_1D);
	    DFELink tolmem = addStreamToLMem("tolmem", LMemCommandGroup.MemoryAccessPattern.LINEAR_1D);
	    tolmem <== fromcpu;
	    tocpu <== fromlmem;
	"""

	val intrIntfOld =
	s"""
	    // Setup interrupt stream
	    DFELink intrStream = addStreamToOnCardMemory("intrStream", k.getOutput("intrCmd"));
	    intrStream <== k.getOutput("intrStream");
	"""
	val intrIntfNew =
	s"""
	    // Setup interrupt stream
	    DFELink intrStream = addStreamToLmem("intrStream", k.getOutput("intrCmd"));
	    intrStream <== k.getOutput("intrStream");
	"""
	//val intrIntf = if (Config.newMemAPI) intrIntfNew else intrIntfOld
	val intrIntf = intrIntfOld
	//val cpuIntf = if (Config.newMemAPI) cpuIntfNew else cpuIntfOld
	val cpuIntf = cpuIntfOld

	val mConstructorPreamble =
	s"""
	  TopManager(EngineParameters engineParameters) {
	    super(engineParameters);

	    // Disable stream status blocks
	    DebugLevel debugLevel = new DebugLevel();
	    debugLevel.setHasStreamStatus(false);
	    debug.setDebugLevel(debugLevel);

	    // Setup stream clock and memory clock
	    config.setDefaultStreamClockFrequency($streamClock);
	    config.setOnCardMemoryFrequency(LMemFrequency.MAX4MAIA_$memClock);
	    config.setEnableAddressGeneratorsInSlowClock(true);

    // Allow non-multiple transitions for parallel tile loaders: may affect area
    config.setAllowNonMultipleTransitions(true);

	    // Setup memory controller clock and config
	//    MemoryControllerConfig mem_cfg = new MemoryControllerConfig();
	////    mem_cfg.setBurstSize(4); //MAX3: 4 = 4*384 bits, 8 = 8*384 bits
	//    mem_cfg.setDataReadFIFOExtraPipelineRegInFabric(true);
	//    mem_cfg.setDataFIFOExtraPipelineRegInFabric(true); //timing-23may
	//    //mem_cfg.setDataFIFOPrimitiveWidth(5*72);
	//    config.setMemoryControllerConfig(mem_cfg);

	    // Create a KernelConfiguration object that sets the OptimizationTechnique
	    // to optimize for area, which is the default in the 2014.1 compiler
	    // TODO: This causes build failures with MaxJ during source annotation. Investigate why
	    KernelConfiguration kernelConfig = getCurrentKernelConfig();
	    kernelConfig.warnings.setWarningBehaviour(KernelConfiguration.WarningOptions.Warning.ALL,
	    	KernelConfiguration.WarningOptions.WarningBehaviour.IGNORE);
	    // kernelConfig.optimization.setOptimizationTechnique(OptimizationTechnique.AREA);
	    // KernelBlock k = addKernel(new TopKernel(makeKernelParameters("TopKernel", kernelConfig)));

	    KernelBlock k = addKernel(new TopKernel(makeKernelParameters("TopKernel")));

	    $cpuIntf

	    $intrIntf
	"""

val mConstructorEpilogue =
s"""
  }
"""

  def emitImports() = {
    mImports.foreach(i => emit(s"import $mImportPrefix.$i;"))
    //if (Config.newMemAPI) {
      oldMemImports.foreach { i => emit(s"import $mImportPrefix.$i;") }
    //} else {
      //oldMemImports.map { i => emit(s"import $mImportPrefix.$i;") }
    //}
  }

  def emitConstructor(memStreams: List[Sym[Any]]) = {
    emit(mConstructorPreamble)
    emit("    // Setup LMEM -> DFE streams (input streams to DFE)")
    emit("    // Setup DFE -> LMEM (output streams from DFE)")
    memStreams.foreach{
      case tt@Def(EatReflect(BurstStore(mem,stream,ofs,len,p))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        emit(s"""// BurstStore $streamName""")
        emit(s"""    DFELink ${streamName}_out = addStreamToOnCardMemory("${streamName}_out", k.getOutput("${streamName}_out_cmd"));""")
        emit(s"""    ${streamName}_out <== k.getOutput("${streamName}_out");""")

      case tt@Def(EatReflect(BurstLoad(mem,stream,ofs,len,p))) =>
     	  val streamName = s"${quote(mem)}_${quote(tt)}"
        emit(s"""// BurstLoad $streamName""")
        emit(s"""    DFELink ${streamName}_in = addStreamFromOnCardMemory("${streamName}_in", k.getOutput("${streamName}_in_cmd"));""")
        emit(s"""    k.getInput("${streamName}_in") <== ${streamName}_in;""")
      case tt@Def(EatReflect(Convolve(img, kernel, output, img_dims, k_dims, strides, pars, inds))) =>
      
     	  val streamName1 = s"${quote(img)}_${quote(tt)}"
        emit(s"""// DRAM Load $streamName1""")
        // emit(s"""    DFELink ${streamName1}_in = addStreamFromOnCardMemory("${streamName1}_in", k.getOutput("${streamName1}_in_cmd"));""")
        emit(s"""    DFELink ${streamName1}_in = addStreamFromOnCardMemory("${streamName1}_in", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);""")
        emit(s"""    k.getInput("${streamName1}_in") <== ${streamName1}_in;""")

     	  val streamName2 = s"${quote(kernel)}_${quote(tt)}"
        emit(s"""// DRAM Load $streamName2""")
        // emit(s"""    DFELink ${streamName2}_in = addStreamFromOnCardMemory("${streamName2}_in", k.getOutput("${streamName2}_in_cmd"));""")
        emit(s"""    DFELink ${streamName2}_in = addStreamFromOnCardMemory("${streamName2}_in", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);""")
        emit(s"""    k.getInput("${streamName2}_in") <== ${streamName2}_in;""")

        val streamName3 = s"${quote(output)}_${quote(tt)}"
        emit(s"""// DRAM Store $streamName3""")
        // emit(s"""    DFELink ${streamName3}_out = addStreamToOnCardMemory("${streamName3}_out", k.getOutput("${streamName3}_out_cmd"));""")
        emit(s"""    DFELink ${streamName3}_out = addStreamToOnCardMemory("${streamName3}_out", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);""")
        emit(s"""    ${streamName3}_out <== k.getOutput("${streamName3}_out");""")

      case tt@Def(EatReflect(Scatter(mem,local,addrs,len,par,_))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        emit(s"""// Scatter $streamName""")
  	    val p = childrenOf(parentOf(tt).get).length
        val i = childrenOf(parentOf(tt).get).indexOf(tt)
        emit(s"""    DFELink ${streamName}_out_rd_${i} = addStreamFromOnCardMemory("${streamName}_out_rd_${i}", k.getOutput("${streamName}_out_rd_cmd_${i}"));""")
        emit(s"""    k.getInput("${streamName}_out_rd_${i}") <== ${streamName}_out_rd_${i};""")
        emit(s"""    DFELink ${streamName}_out_$i = addStreamToOnCardMemory("${streamName}_out_$i", k.getOutput("${streamName}_out_cmd_$i"));""")
        emit(s"""    ${streamName}_out_$i <== k.getOutput("${streamName}_out_$i");""")
      case tt@Def(EatReflect(Gather(mem,local,addrs,len,_,i))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        val p = childrenOf(parentOf(tt).get).length
        val i = childrenOf(parentOf(tt).get).indexOf(tt)
        emit(s"""// Gather $streamName""")
        emit(s"""    DFELink ${streamName}_in_$i = addStreamFromOnCardMemory("${streamName}_in_$i", k.getOutput("${streamName}_in_cmd_$i"));""")
        emit(s"""    k.getInput("${streamName}_in_$i") <== ${streamName}_in_$i;""")

    }
    emit(mConstructorEpilogue)
  }

  def emitRWInterface() = {
    emit(mReadIntf)
    emit(mWriteIntf)
  }

  def emitDefaultInterface(argInOuts: List[Sym[Reg[_]]], memStreams: List[Sym[Any]]) = {
    emit(mDefaultIntfPreamble)
    argInOuts.foreach { a =>
			regType(a) match {
				case Regular =>
				case ArgumentIn =>
					val ts =  remap(a.tp)
      		emit(s"""    InterfaceParam ${quote(a)} = ei.addParam("${quote(a)}", ${ts});""")
      		emit(s"""    ei.setScalar("TopKernel", "${quote(a)}", ${quote(a)});""")
        case ArgumentOut =>
      		emit(s"""    ei.unignoreScalar("TopKernel", "${quote(a)}");""")
			}
    }
    memStreams.foreach{
      case tt@Def(EatReflect(Convolve(img, kernel, output, img_dims, k_dims, strides, pars, inds))) =>
      
     	  val streamName1 = s"${quote(img)}_${quote(tt)}_in"
     	  val streamName2 = s"${quote(kernel)}_${quote(tt)}_in"
        val streamName3 = s"${quote(output)}_${quote(tt)}_out"

        var outD = 1;
        if (k_dims.length == 4) {
          outD = k_dims(3)
        }

        val img_DRAM_dims = dimsOf(img)
        val img_DRAM_size = bound(img_DRAM_dims(0)).get.toInt
        
        val kernel_DRAM_dims = dimsOf(kernel)
        val kernel_DRAM_size = bound(kernel_DRAM_dims(0)).get.toInt
        
        val output_DRAM_dims = dimsOf(output)
        val output_DRAM_size = bound(output_DRAM_dims(0)).get.toInt

        emit(s"""     // Convolution currently uses linear memory regions""")
        emit(s"""     ei.setLMemLinearWrapped("${streamName1}",  ${dramAddr(img   )},  ${img_DRAM_size*4},  ${img_DRAM_size*4*outD},  0);""")
        emit(s"""     ei.setLMemLinear("${streamName2}",         ${dramAddr(kernel)},  ${kernel_DRAM_size*4});""")
        emit(s"""     ei.setLMemLinear("${streamName3}",         ${dramAddr(output)},  ${output_DRAM_size*4});
""")

    case _ =>

    }
    emit(s"""      ei.unignoreScalar("TopKernel", "cycles");""")
    emit(mDefaultIntfEpilogue)
  }

  def emitManager(stream:PrintWriter, argInOuts:List[Sym[Reg[_]]], memStreams:List[Sym[Any]]) = {
		this.stream = stream
    initPass()
    //println(s"""tileTransfers: """)
		//tileTsfs.foreach { tt => println(quote(tt)) }
    //println(s"""argIns and argOuts: """)
		//argInOuts.foreach { a => println(quote(a)) }
    emitConstructor(memStreams)
    emitRWInterface()
    emitDefaultInterface(argInOuts, memStreams)

    finPass()
  }

  def initPass() = {
    emit("package engine;")
    emitImports()
    emit(mPreamble)
  }

  def finPass() = {
    emit(mEpilogue)
  }


}

package spatial.compiler.ops
import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.Set
import scala.reflect.SourceContext
import java.io.{PrintWriter}

trait ChiselManagerGen {
	val IR:SpatialExp
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
    case _ => throw new Exception(s"""No remap rule in ChiselManagerGen for ${m.erasure.getSimpleName}""")
  }



  // val mImportPrefix = "com.maxeler.maxcompiler.v2"
  val streamClock = 150
  val memClock = 400
  val mImports = List(
    "Chisel._"
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
	class TopManager extends Module {
		// Empty for now
	"""

	  val mEpilogue =
	s"""
	}
	"""

	val mReadIntf =
	s"""
	"""

	val mWriteIntf =
	s"""
	"""

	val mDefaultIntfPreamble =
	s"""
	"""

	val mDefaultIntfEpilogue =
	s"""
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
	"""

val mConstructorEpilogue =
s"""
  }
"""

  def emitImports() = {
    mImports.foreach(i => emit(s"import $i"))
    //if (Config.newMemAPI) {
      // oldMemImports.foreach { i => emit(s"import $mImportPrefix.$i;") }
    //} else {
      //oldMemImports.map { i => emit(s"import $mImportPrefix.$i;") }
    //}
  }

  def emitConstructor(memStreams: Set[Sym[Any]]) = {
    // emit(mConstructorPreamble)
    // emit("    // Setup LMEM -> DFE streams (input streams to DFE)")
    // emit("    // Setup DFE -> LMEM (output streams from DFE)")
    memStreams.foreach{
      case tt@Def(EatReflect(BurstStore(mem,stream,ofs,len,p))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        // emit(s"""// BurstStore $streamName""")
        // emit(s"""    DFELink ${streamName}_out = addStreamToOnCardMemory("${streamName}_out", k.getOutput("${streamName}_out_cmd"));""")
        // emit(s"""    ${streamName}_out <== k.getOutput("${streamName}_out");""")

      case tt@Def(EatReflect(BurstLoad(mem,stream,ofs,len,p))) =>
     	  val streamName = s"${quote(mem)}_${quote(tt)}"
        // emit(s"""// BurstLoad $streamName""")
        // emit(s"""    DFELink ${streamName}_in = addStreamFromOnCardMemory("${streamName}_in", k.getOutput("${streamName}_in_cmd"));""")
        // emit(s"""    k.getInput("${streamName}_in") <== ${streamName}_in;""")
      case tt@Def(EatReflect(Scatter(mem,local,addrs,len,par,_))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        // emit(s"""// Scatter $streamName""")
	    val p = childrenOf(parentOf(tt).get).length
	    val i = childrenOf(parentOf(tt).get).indexOf(tt)
        // emit(s"""    DFELink ${streamName}_out_rd_${i} = addStreamFromOnCardMemory("${streamName}_out_rd_${i}", k.getOutput("${streamName}_out_rd_cmd_${i}"));""")
        // emit(s"""    k.getInput("${streamName}_out_rd_${i}") <== ${streamName}_out_rd_${i};""")
        // emit(s"""    DFELink ${streamName}_out_$i = addStreamToOnCardMemory("${streamName}_out_$i", k.getOutput("${streamName}_out_cmd_$i"));""")
        // emit(s"""    ${streamName}_out_$i <== k.getOutput("${streamName}_out_$i");""")
      case tt@Def(EatReflect(Gather(mem,local,addrs,len,_,i))) =>
     	val streamName = s"${quote(mem)}_${quote(tt)}"
	    val p = childrenOf(parentOf(tt).get).length
	    val i = childrenOf(parentOf(tt).get).indexOf(tt)
        // emit(s"""// Gather $streamName""")
        // emit(s"""    DFELink ${streamName}_in_$i = addStreamFromOnCardMemory("${streamName}_in_$i", k.getOutput("${streamName}_in_cmd_$i"));""")
        // emit(s"""    k.getInput("${streamName}_in_$i") <== ${streamName}_in_$i;""")

    }
    // emit(mConstructorEpilogue)
  }

  def emitRWInterface() = {
    // emit(mReadIntf)
    // emit(mWriteIntf)
  }

  def emitDefaultInterface(argInOuts: Set[Sym[Reg[_]]]) = {
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
    emit(s"""    ei.unignoreScalar("TopKernel", "cycles");""")
    emit(mDefaultIntfEpilogue)
  }

  def emitManager(stream:PrintWriter, argInOuts:Set[Sym[Reg[_]]], memStreams:Set[Sym[Any]]) = {
		this.stream = stream
    initPass()
    //println(s"""tileTransfers: """)
		//tileTsfs.foreach { tt => println(quote(tt)) }
    //println(s"""argIns and argOuts: """)
		//argInOuts.foreach { a => println(quote(a)) }
    emitConstructor(memStreams)
    emitRWInterface()
    emitDefaultInterface(argInOuts)

    finPass()
  }

  def initPass() = {
    // emit("package engine;")
    emitImports()
    emit(mPreamble)
  }

  def finPass() = {
    emit(mEpilogue)
  }


}

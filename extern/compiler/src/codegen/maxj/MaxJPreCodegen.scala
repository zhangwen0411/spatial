package spatial.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{MaxJCodegen}
import scala.virtualization.lms.internal.{Traversal}
import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._
import scala.collection.mutable.Set

import ppl.delite.framework.DeliteApplication

trait MaxJPreCodegen extends Traversal  {
	val IR:SpatialExp with MemoryAnalysisExp with UnrollingTransformExp with ExternPrimitiveOpsExp with ReductionAnalysisExp with DRAMAddrAnalysisExp

	import IR.{infix_until => _, looprange_until => _, println => _, _}

	var buildDir:String = _

  //debugMode = false
  override val name = "MaxJPreCodegen"

	lazy val maxJManagerGen = new MaxJManagerGen {
		val IR: MaxJPreCodegen.this.IR.type = MaxJPreCodegen.this.IR
	}


  def quote(x: Exp[Any]) = x match {
    case s@Sym(n) => {
      s match {
        case Def(Argin_new(init)) =>
          s"argin_" + s.tp.erasure.getSimpleName() + n
        case Def(ConstFix(value)) =>
          s"const${value}_" + s.tp.erasure.getSimpleName() + n
        case Def(ConstFlt(value)) =>
          val str = s"${value}"
          s"const${str.replace('.', 'p').replace('-', 'n')}_" + s.tp.erasure.getSimpleName() + n
        case _ =>
          maxJManagerGen.quote(x)
      }}
    case _ =>
      maxJManagerGen.quote(x)
  }

	var argInOuts  = List[Sym[Reg[_]]]()
  val memStreams = List[Sym[Any]]()

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
		argInOuts = List()
		memStreams = List()
		b
	}
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
		withStream(newStream("MaxJManager")) {
			maxJManagerGen.emitManager(stream, argInOuts, memStreams)
		}
		b
	}

	def newStream(fileName:String):PrintWriter = {
		val path = buildDir + java.io.File.separator + fileName + ".maxj"
		val pw = new PrintWriter(path)
		pw
	}

	var stream:PrintWriter = _

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush; stream.close; stream = save }
  }


	def emit(str: String):Unit = {
		stream.println(str)
	}

  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
			preGenNodes(sym,rhs)
			super.traverseStm(stm)
		}
    case _ =>
      super.traverseStm(stm)
	}

  def preGenNodes(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
    case e@Hwblock(func) =>
			withStream(newStream("sequential_" + quote(sym))) {
				emitSeqSM(quote(sym), childrenOf(sym).length)
			}
		case e@ParallelPipe(func: Block[Unit]) =>
			withStream(newStream("parallel_" + quote(sym))) {
				emitParallelSM(quote(sym), childrenOf(sym).length)
			}
    case e@OpForeach(cchain, func, inds) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case CoarsePipe =>
					withStream(newStream("metapipe_" + quote(sym))) {
    				emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case InnerPipe =>
				case SequentialPipe =>
					withStream(newStream("sequential_" + quote(sym))) {
    				emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}
    case e@OpReduce(cchain, accum, zero, foldAccum, ldFunc, stFunc, func, rFunc, inds, acc, res, rV) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {

				case CoarsePipe =>
					withStream(newStream("metapipe_" + quote(sym))) {
    				emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case InnerPipe =>
				case SequentialPipe =>
					withStream(newStream("sequential_" + quote(sym))) {
    				emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}

    case e@UnrolledForeach(cc, func, inds, vs) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case CoarsePipe =>
					withStream(newStream("metapipe_" + quote(sym))) {
    				emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case InnerPipe =>
          parentOf(sym).get match {
            case e@Deff(UnrolledReduce(_,accum,_,_,_,_,_,_)) => // If part of reduce, emit custom red kernel
              if (childrenOf(parentOf(sym).get).indexOf(sym) == childrenOf(parentOf(sym).get).length-1) {
                reduceType(accum) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                    case FltPtSum =>
                    case _ => // Do not specialize, turn off all metadata for reduction
                      focusBlock(func){focusExactScope(func){stms =>stms.map { case TP(s,d) =>
                        isReduceStarter(s) = false
                        isReduceResult(s) = false}}}
                  }
                  case _ => 
                    Console.println(s"WARNING: Unknown reduce type of accum $accum")
                }
              }
            case _ =>
          }
				case SequentialPipe =>
					withStream(newStream("sequential_" + quote(sym))) {
    				emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}
    case e@UnrolledReduce(cchain, accum, func, rFunc, inds, vs, acc, rV) =>
      styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
        case SequentialPipe =>
          withStream(newStream("sequential_" + quote(sym))) {
            emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
          }
        case _ =>
          withStream(newStream("metapipe_" + quote(sym))) {
            emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
          }
      }
      styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
        case InnerPipe =>
                reduceType(accum) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                      // withStream(newStream(s"${quote(sym)}_reduce_kernel")) {
                        // emitReduction(sym, rhs)
                      // }                
                    case FltPtSum =>
                      // withStream(newStream(s"${quote(sym)}_reduce_kernel")) {
                        // emitReduction(sym, rhs)
                      // }                
                    case _ => // Do not specialize, turn off all metadata for reduction
                      focusBlock(func){focusExactScope(func){stms =>stms.map { case TP(s,d) =>
                        isReduceStarter(s) = false
                        isReduceResult(s) = false}}}
                  }
                  case _ => 
                    Console.println(s"WARNING: Unknown reduce type of accum $accum")
                }
        case _ =>
      }

    case e@UnitPipe(func) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case CoarsePipe =>
					withStream(newStream("metapipe_" + quote(sym))) {
            emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case InnerPipe =>
          parentOf(sym).get match {
            case e@Deff(UnrolledReduce(_,accum,_,_,_,_,acc,_)) => // If part of reduce, emit custom red kernel
              if (childrenOf(parentOf(sym).get).indexOf(sym) == childrenOf(parentOf(sym).get).length-1) {
                reduceType(accum) match {
                  case Some(fps: ReduceFunction) => fps match {
                    case FixPtSum =>
                    case FltPtSum =>
                    case _ => // Do not specialize, turn off all metadata for reduction
                      focusBlock(func){focusExactScope(func){stms =>stms.map { case TP(s,d) =>
                        isReduceStarter(s) = false
                        isReduceResult(s) = false}}}
                  }
                  case _ => 
                    Console.println(s"WARNING: Unknown reduce type of accum $accum")
                }
              }
            case _ =>
          }
				case SequentialPipe =>
          withStream(newStream("sequential_" + quote(sym))) {
            emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}

    case e@Counter_new(start,end,step,par) =>
      withStream(newStream("counter_" + quote(sym))) {
        emitCtrSM(quote(sym), List(parOf(sym)), 0, 1)
      }
    case e@Counterchain_new(counters) =>
      val pars = counters.map { ctr => parOf(ctr) }
      val gaps = counters.map { ctr => 0 }
      withStream(newStream("counter_" + quote(sym))) {
        emitCtrSM(quote(sym), pars, 0, counters.length)
      }

		case e:Argin_new[_] => argInOuts = argInOuts :+ sym.asInstanceOf[Sym[Reg[_]]]
    case e:Argout_new[_] => argInOuts = argInOuts :+ sym.asInstanceOf[Sym[Reg[_]]]

    case e@BurstStore(_,fifo,_,_,_) =>
      memLdFifos += fifo
      memStreams = memStreams :+ sym
    case e@BurstLoad(_,fifo,_,_,_) => 
      memLdFifos += fifo
      memStreams = memStreams :+ sym
    case e@Convolve(img,kernel,output,_,_,_,_,_) => 
      memStreams = memStreams :+ sym
      // argInOuts = argInOuts :+ sym.asInstanceOf[Sym[Reg[_]]]
    case _:Scatter[_] => memStreams = memStreams :+ sym
    case _:Gather[_] => memStreams = memStreams :+ sym

    case Sram_new(size, zero) =>
      val dups = duplicatesOf(sym)
      dups.zipWithIndex.foreach { case (d, i) =>
        val readers = readersOf(sym)
        if (d.depth > 2) {
          val numReaders_for_this_duplicate = readers.filter{r => instanceIndicesOf(r, sym).contains(i) }.length
          withStream(newStream("bram_" + quote(sym) + "_" + i)) {
            emitDblBufSM(quote(sym) + "_" + i, numReaders_for_this_duplicate)
          }
          // withStream(newStream("bram_" + quote(sym) + "_" + i)) {
          //   emitNBufSM(quote(sym) + "_" + i, d.depth)
          // }
        }
      }

    // case Reg_new(init) =>
    //   val duplicates = duplicatesOf(sym)
    //   duplicates.zipWithIndex.foreach { case (d, i) =>
    //     if (d.depth > 2) {
    //       withStream(newStream("nbuf_" + quote(sym) + "_" + i)) {
    //         emitNBufSM(quote(sym), i, d.depth)
    //       }
    //     }
    //   }



    case Reflect(s, u, effects) =>
      preGenNodes(sym, s)
    case Reify(s, u, effects) =>
		case _ => {
			//println("tp:" + sym.tp.erasure.getSimpleName() + "rhs:" + rhs)
		}
	}

  def emitCtrSM(name: String, par: List[Int], gap: Int, numCtrs: Int) = {
    stream.println("""
package engine;
import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;""")

  val smName = name
  stream.println(s"""class ${smName}_CtrSM extends KernelStateMachine {""")

  stream.println(s"""// ** VISUALIZATION FOR COUNTER **""")
  par.zipWithIndex.map { case (p, i) =>
    stream.println(s"""//    ctr${i}: ${(0 until p).map{_ => "o"}.mkString(" ")}""")
  }


  stream.println(s"""
    // States
    enum States {
      COUNT,
      SATURATED
    }

    // State IO
    private final DFEsmOutput[] count;""")
  // TODO: Not tested for parallelism in higher level counters
  for (i <- 0 until par.size){
    if (par(i) > 1){
      stream.println(s"""    private final DFEsmOutput[] counter${i}_extension;""")
    }
  }
  stream.println(s"""
    private final DFEsmOutput saturated;
    private final DFEsmOutput done;
    private final DFEsmInput en;
    private final DFEsmInput reset;
    private final DFEsmInput[] max;
    private final int[] strides;
    // Gap between the end of one array of count to the start of the next.
    //   This is useful for padding non-powerof2-banked SRAMs to the next highest pwr of 2 banks
    // i.e- gap = 32, stride = 1, par = 96 would do this:
    // cycle1: count = [0, 1, ..., 94, 95]
    // cycle2: count = [128, 129, ..., 222, 223]
    private final int gap;
    private final int[] ff_extensions;

    // State storage
    private final DFEsmStateValue[] countFF;""")
  if (numCtrs > 1) {stream.println(s"""    private final DFEsmStateValue[] gapBumpFF;""")}
  stream.println(s"""    private final DFEsmStateEnum<States> stateFF;
    """)

  stream.println(s"""
    // Initialize state machine in constructor
    // NOTE: strides is a constructor argument and max is an io.input
    //       because old DHDL specifies max as CombNodes and strides as
    //       ints so this was the easiest way to make this CtrSM
    public ${smName}_CtrSM(KernelLib owner, int[] s) {
      super(owner);
      // Declare all types required to wire the state machine together
      DFEsmValueType numberType = dfeInt(32);
      DFEsmValueType wireType = dfeBool();

      strides = s;
      gap = ${gap};
      ff_extensions = new int[${par.size}];
      // Define state machine IO
      count = new DFEsmOutput[${numCtrs}];
      max = new DFEsmInput[${numCtrs}];
      for (int i = 0; i < ${numCtrs}; i++) {
        count[i] = io.output("counter" + i, numberType);
        max[i] = io.input("max" + i, numberType);
      }""")
  for (j <- 0 until par.size){
    if (par(j) > 1){
      stream.println(s"""      // Par extension for ${j}
      ff_extensions[${j}] = ${par(j)-1} * strides[${j}];
      counter${j}_extension = new DFEsmOutput[${par(j)-1}];
      for (int i = 0; i < ${par(j)-1}; i++) {
        counter${j}_extension[i] = io.output("counter${j}_extension" + i, numberType);
      }""")
    } else {
      stream.println(s"""      ff_extensions[${j}] = 0;""")
    }
  }

  stream.println(s"""
      saturated = io.output("saturated", wireType);
      done = io.output("done", wireType);
      en = io.input("en", wireType);
      reset = io.input("reset", wireType);

      // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.COUNT);
      countFF = new DFEsmStateValue[${numCtrs}];
      for (int i = 0; i < ${numCtrs}; i++) {
        countFF[i] = state.value(numberType, 0);
      }""")
  if (numCtrs > 1) {
    stream.println(s"""      gapBumpFF = new DFEsmStateValue[${numCtrs-1}];
      for (int i = 0; i < ${numCtrs-1}; i++) {
        gapBumpFF[i] = state.value(numberType, ${par(par.size-1)});
      }""")
  }
  stream.println(s"""    }
  """)

  stream.println(s"""
    @Override
    protected void nextState() {
      IF (reset) {""")

  for(i <- 0 until numCtrs) {
    stream.println(s"""        countFF[${i}].next <== 0;""")
  }

  stream.println(s"""        stateFF.next <== States.COUNT;
      } ELSE {
          SWITCH(stateFF) {
            CASE(States.COUNT) {
              IF(en) {
                IF (""")

  // Generate saturated case
  for(i <- 0 until numCtrs) {
    if(i == numCtrs - 1){
      stream.println(s"""                  (countFF[${i}] + ff_extensions[${i}] >= max[${i}] - strides[${i}])) {""")
    } else {
      stream.println(s"""                  (countFF[${i}] + ff_extensions[${i}] >= max[${i}] - strides[${i}]) &""")
    }
  }

  stream.println(s"""                stateFF.next <== States.SATURATED;""")

  // Generated wrap cases
  for(i <- 1 to numCtrs-1) {
    stream.println(s"""                } ELSE { IF (""")
    for(j <- numCtrs-1 to i by -1){
      if (j == i){
        stream.println(s"""                  (countFF[${j}] + ff_extensions[${j}] >= max[${j}] - strides[${i}])) {""")
      } else {
        stream.println(s"""                  (countFF[${j}] + ff_extensions[${j}] >= max[${j}] - strides[${i}]) &""")
      }
    }
    for(j <- numCtrs-1 to i by -1){
      stream.println(s"""                countFF[${j}].next <== 0;""")
    }
    stream.println(s"""                IF (countFF[${i-1}] + ff_extensions[${i-1}] === gapBumpFF[${i-1}] - strides[${i-1}]) {
                  countFF[${i-1}].next <== countFF[${i-1}] + ${par(i-1)} * strides[${i-1}] + gap;
                  gapBumpFF[${i-1}].next <== gapBumpFF[${i-1}] + gap + ${par(par.size-1)};
                } ELSE {
                  countFF[${i-1}].next <== countFF[${i-1}] + ${par(i-1)} * strides[${i-1}];
                }""")
  }
  stream.println(s"""                } ELSE { // innermost ctr
                countFF[${numCtrs-1}].next <== countFF[${numCtrs-1}] + ${par(numCtrs-1)} * strides[${numCtrs-1}] + gap;""")
  for(j <- 0 until numCtrs){
    stream.print(s"""}""")
  }
  stream.println(s"""           } ELSE {
              stateFF.next <== States.COUNT;
            }
          }""")

  stream.println(s"""          CASE(States.SATURATED) {
            stateFF.next <== States.SATURATED;
          }
        }
      }
    }

  @Override
    protected void outputFunction() {
        """)

  for(i <- 0 until numCtrs) {
    stream.println(s"""      count[${i}] <== countFF[${i}];""")
  }
  for (j <- 0 until par.size){
    if (par(j) > 1){
      stream.println(s"""      for (int i = 0; i < ${par(j)-1}; i++) {
        counter${j}_extension[i] <== countFF[${j}] + strides[${j}] * (i+1);
      }""")
    }
  }


  stream.println(s"""
      saturated <== 0;
      done <== 0;
      SWITCH(stateFF){
        CASE(States.COUNT){
          IF(en) {

            IF (""")

  for(i <- 0 until numCtrs) {
    if(i == numCtrs - 1){
      stream.println(s"""               (countFF[${i}] + ff_extensions[${i}] >= max[${i}] - strides[${i}]) & max[${i}] !== 0) {""")
    } else {
      stream.println(s"""               (countFF[${i}] + ff_extensions[${i}] >= max[${i}] - strides[${i}]) & max[${i}] !== 0 &""")
    }
  }

  stream.println(s"""             saturated <== 1;
             done <== 1;
            }
          }
        }
        CASE(States.SATURATED){
          IF (en) {
            done <== 1;
          }
          saturated <== 1;
        }
      }
    }
}""")







  }

  def emitDblBufSM(name: String, numReaders: Int) = {
  stream.println(s"""
  package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;

  class ${name}_DblBufSM extends KernelStateMachine {

    // States
    enum States {
      W, R, RW, SWAP
    }

    // State IO
    private final DFEsmInput w_done;
    private final DFEsmOutput curBuf;""");

    for(i <- 0 until numReaders) {
  stream.println(s"""
  private final DFEsmInput r_done_$i;
  """)
    }

  stream.println(s"""
    // State storage
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue curBufFF;

    private final DFEsmStateValue[] rdoneBitVectorFF;
    private final DFEsmValue allRdone;

    // Initialize state machine in constructor
    public ${name}_DblBufSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      w_done = io.input("w_done", wireType);
  """)

  for(i <- 0 until numReaders) {
      stream.println(s"""
        r_done_${i} = io.input("r_done_${i}", wireType);
      """)
    }


  stream.println(s"""
      curBuf = io.output("curBuf", wireType);

      // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.W);
      curBufFF = state.value(wireType, 0);

      rdoneBitVectorFF = new DFEsmStateValue[$numReaders];
      for (int i = 0; i < $numReaders; i++) {
        rdoneBitVectorFF[i] = state.value(wireType, 0);
      }
""")

   stream.println(s"""allRdone = ${(0 until numReaders) map ("rdoneBitVectorFF["+_+"]") mkString(" & ")};""")


  stream.println(s"""
    }

    private void resetBitVector() {
      for (int i=0; i<$numReaders; i++) {
        rdoneBitVectorFF[i].next <== 0;
      }
    }



  @Override
  protected void nextState() {""")

  (0 until numReaders) map { i =>
    stream.println(s"""
      IF (r_done_$i) {
        rdoneBitVectorFF[$i].next <== 1;
      }""")
  }
  stream.println(s"""
    SWITCH(stateFF) {
      CASE(States.W) {
        IF (w_done) {
          stateFF.next <== States.SWAP;
        }
      }
      CASE(States.RW) {
        IF (allRdone & w_done) {
          stateFF.next <== States.SWAP;
        } ELSE {
          IF (allRdone) {
          stateFF.next <== States.W;
          } ELSE {
            IF (w_done) {
              stateFF.next <== States.R;
            }
          }
        }
      }
      CASE(States.R) {
        IF (allRdone) {
          stateFF.next <== States.SWAP;
        }
      }
      CASE(States.SWAP) {
        curBufFF.next <== ~curBufFF;
        stateFF.next <== States.RW;
        resetBitVector();
      }
      OTHERWISE {
        stateFF.next <== stateFF;
      }
    }
  }

  @Override
  protected void outputFunction() {
    curBuf <== curBufFF;
  }
  }
  """)
  }

  private def stateTextSeq(state: Int, N: Int) = {
    val condStr = s"bitVector[ $state ]"
    val max = N-1

    emit(s"""IF($condStr) {
      resetBitVector();""")
    if (state == max) {
      emit(s"""
      counterFF.next <== counterFF + 1;
      IF (counterFF >= sizeFF-1 | sizeFF === 0 /*hack for when reg inits to 0*/) {
        stateFF.next <== States.DONE;
      } ELSE {
        stateFF.next <== States.S0;
      }""")
      emit("}")
    } else {
      emit(s"stateFF.next <== States.S${state+1};")
      emit("}")
    }
  }

  def emitSeqSM(name: String, numStates: Int):Unit = {
		if (numStates==0) {
			emit(s"""//Number of stages = 0 for ${name}. Nothing is emitted""")
			return
		}
    emit("""
package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;
""")

  val smName = name
  val states = (0 until numStates).map(List(_)).toList
  emit(s"""class ${smName}_SeqSM extends KernelStateMachine {""")

  val stateNames = states.map(stateStr(_))
  emit(s"""
    // States
    enum States {
      INIT,
      RSET,
      ${stateNames.reduce(_ + ",\n" + _) + ",\nDONE"}
    }
  """)

  emit("""
    // State IO
    private final DFEsmOutput sm_done;
//    private final DFEsmOutput sm_last;
    private final DFEsmInput sm_en;
    private final DFEsmInput sm_numIter;
    private final DFEsmOutput rst_en;
  """)

  for(i <- 0 until numStates) {
    emit(s"""
    private final DFEsmInput s${i}_done;
    private final DFEsmOutput s${i}_en;
    """)
  }

  emit(s"""
    // State storage
    private final DFEsmStateValue sizeFF;
//    private final DFEsmStateValue lastFF;
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue counterFF;
    private final DFEsmStateValue rstCounterFF;
    private final DFEsmStateValue[] bitVector;

    private final int numStates = ${numStates};
    private final int rstCycles = 10; // <-- hardcoded
    // Initialize state machine in constructor
    public ${smName}_SeqSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      sm_done = io.output("sm_done", wireType);
//      sm_last = io.output("sm_last", wireType);
      sm_en = io.input("sm_en", wireType);
      sm_numIter = io.input("sm_numIter", counterType);
      rst_en = io.output("rst_en", wireType);
  """)

  for(i <- 0 until numStates) {
    emit(s"""
      s${i}_done = io.input("s${i}_done", wireType);
      s${i}_en = io.output("s${i}_en", wireType);
    """)
  }

  emit("""
    // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.INIT);
      counterFF = state.value(counterType, 0);
      rstCounterFF = state.value(counterType, 0);
      sizeFF = state.value(counterType, 0);
//      lastFF = state.value(wireType, 0);

      // Bitvector keeps track of which kernels have finished execution
      // This is a useful hardware synchronization structure to keep
      // track of which kernels have executed/finished execution
      bitVector = new DFEsmStateValue[numStates];
      for (int i=0; i<numStates; i++) {
        bitVector[i] = state.value(wireType, 0);
      }
    }

    private void resetBitVector() {
      for (int i=0; i<numStates; i++) {
        bitVector[i].next <== 0;
      }
    }
      """)

  emit(s"""
    @Override
    protected void nextState() {
      IF(sm_en) {
        // State-agnostic update logic for bitVector
    """)
  for(i <- 0 until numStates) {
    emit(s"""
        IF (s${i}_done) {
          bitVector[$i].next <== 1;
        }""")
  }

  emit(s"""
        SWITCH(stateFF) {
          CASE (States.INIT) {
            sizeFF.next <== sm_numIter;
            stateFF.next <== States.RSET;
            counterFF.next <== 0;
            rstCounterFF.next <== 0;
//            lastFF.next <== 0;
          }

          CASE (States.RSET) {
            rstCounterFF.next <== rstCounterFF + 1;
            sizeFF.next <== sm_numIter;
            IF (rstCounterFF === rstCycles) {
              stateFF.next <== States.S0;
            } ELSE {
              stateFF.next <== States.RSET;
            }
          }
          """)

  for(i <- 0 until states.size) {
    val state = states(i)
    val name = stateNames(i)
    emit(s"""
          CASE (States.${name}) {""")
      stateTextSeq(state(0), numStates)
    emit(s"""
          }""")
  }

  emit(s"""
         CASE (States.DONE) {
           resetBitVector();
           stateFF.next <== States.INIT;
         }

         OTHERWISE {
           stateFF.next <== stateFF;
         }
        }
      }
    }""")

  emit(s"""
  @Override
    protected void outputFunction() {
      sm_done <== 0;
      rst_en <== 0;
//      sm_last <== 0;
      """)

  for (i <- 0 until numStates) {
    emit(s"""
      s${i}_en <== 0;""")
  }

  emit(s"""
     IF (sm_en) {
//        IF (counterFF >= sizeFF-1  | sizeFF === 0 /*hack for when reg inits to 0*/) {
//          sm_last <== 1;
//        } ELSE {
//          sm_last <== 0;
//        }
       SWITCH(stateFF) {
            CASE (States.RSET) {
              rst_en <== 1;
            }""")
        for(i <- 0 until states.size) {
          val state = states(i)
          val name = stateNames(i)
          emit(s"""
            CASE (States.$name) {""")
             for (s <- state) {
               emit(s"""s${s}_en <== ~(bitVector[$s] | s${s}_done);""")
             }
          emit(s"""
                }""")
        }

        emit(s"""
          CASE (States.DONE) {
            sm_done <== 1;
          }""")

  emit("""
      }
    }
  }
}
  """)

  }

  private def stateStr(state:List[Int]) = {
    "S" + state.map( _.toString).reduce(_+_)
  }

	def emitParallelSM(name: String, numParallel: Int):Unit = {
		if (numParallel==0) {
			emit(s"""//Number of parallel stages = 0 for ${name}. Nothing is emitted""")
			return
		}
		emit(s"""
			package engine;
			import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
			import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
			import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;

			class ${name}_ParSM extends KernelStateMachine {

				// States
				enum States {
					INIT,
					RUN,
					DONE
				}

				// State IO
				private final DFEsmOutput sm_done;
				private final DFEsmInput sm_en;""");

		for(i <- 0 until numParallel) {
			emit(s"""
				private final DFEsmInput s${i}_done;
				private final DFEsmOutput s${i}_en;
				""")
		}

		emit(s"""
			// State storage
			private final DFEsmStateEnum<States> stateFF;
			private final DFEsmStateValue[] bitVector;

			private final int numParallel = $numParallel;
			// Initialize state machine in constructor
			public ${name}_ParSM(KernelLib owner) {
				super(owner);

				// Declare all types required to wire the state machine together
				DFEsmValueType counterType = dfeUInt(32);
				DFEsmValueType wireType = dfeBool();
				// Define state machine IO
				sm_done = io.output("sm_done", wireType);
				sm_en = io.input("sm_en", wireType);
				""")
		for(i <- 0 until numParallel) {
			emit(s"""
				s${i}_done = io.input("s${i}_done", wireType);
				s${i}_en = io.output("s${i}_en", wireType);
				""")
		}

		emit(s"""
			// Define state storage elements and initial state
			stateFF = state.enumerated(States.class, States.INIT);

			bitVector = new DFEsmStateValue[numParallel];
			for (int i=0; i<numParallel; i++) {
				bitVector[i] = state.value(wireType, 0);
			}
			}

			private void resetBitVector() {
				for (int i=0; i<numParallel; i++) {
					bitVector[i].next <== 0;
				}
			}

			@Override
			protected void nextState() {
				IF(sm_en) {
					""")

		for(i <- 0 until numParallel) {
			emit(s"""
				IF (s${i}_done) {
					bitVector[$i].next <== 1;
				}""")
		}

		emit(s"""
			SWITCH(stateFF) {
				CASE (States.INIT) {
					stateFF.next <== States.RUN;
				}
				""")

		emit(s"""
			CASE (States.RUN) {""")
				val condStr = (0 until numParallel).map("bitVector[" + _ + "]").reduce(_ + " & " + _)
				emit(s"""
					IF($condStr) {
						resetBitVector();
						stateFF.next <== States.DONE;
					}
			}

			CASE (States.DONE) {
				resetBitVector();
				stateFF.next <== States.INIT;
			}
			OTHERWISE {
				stateFF.next <== stateFF;
			}
			}
				}
			}
			""")

				emit("""
					@Override
					protected void outputFunction() {
						sm_done <== 0;""")
				for (i <- 0 until numParallel) {
					emit(s"""
						s${i}_en <== 0;""")
				}

				emit("""
					IF (sm_en) {
						SWITCH(stateFF) {
							CASE(States.RUN) {""")
								for (i <- 0 until numParallel) {
									emit(s"""s${i}_en <== ~(bitVector[${i}] | s${i}_done);""")
								}
								emit(s"""
							}
							CASE(States.DONE) {
								sm_done <== 1;
							}
						}
					}
					}
			}""")
	}

  private def getStates(N: Int) = {
    val l = 0.until(N).toList
    val lb_total = ListBuffer[List[Int]]()
    for (i <- 0 until l.size) {
         val lb = ListBuffer[List[Int]]()
         lb.append(List(i))
         for (j <- i+1 until l.size) {
           lb.append((lb.last ++ List(j)))
         }
         lb_total.appendAll(lb)
    }
    lb_total.toList
  }

  private def stateText(state: List[Int], N: Int) = {
    val condStr = state.map("bitVector[" + _ + "]").reduce(_ + " & " + _)
    val max = N-1

    emit(s"""IF($condStr) {
      resetBitVector();""")
    if (state.size == 1 && state.max == max && !state.contains(0)) {
      emit("  stateFF.next <== States.DONE;")
    } else {
      if (state.contains(0)) {
        emit("  counterFF.next <== counterFF + 1;")
        emit("  IF (counterFF >= sizeFF-1  | sizeFF === 0 /*hack for when reg inits to 0*/) {")
        stream.print("    stateFF.next <== States.")
        if (state.max == max) {
          if (state.size == 1) {  // Only state 0
            stream.print("DONE")
          } else {
            stream.print(stateStr(state.drop(1)))
          }
        } else {
          stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
        }
          emit(";")
          emit("  } ELSE {")
        stream.print("    stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state)) else stream.print(stateStr(state ++ List(state.max+1)))
          emit(";")
          emit("  }")
      } else {
        stream.print("stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state.drop(1))) else stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
        emit(";")
      }
    }
    emit("}")
  }

  def emitMPSM(name: String, numStates: Int):Unit = {
		if (numStates==0) {
			emit(s"""//Number of stages = 0 for ${name}. Nothing is emitted""")
			return
		}
    emit("""
package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;
""")

  val smName = name
  val states = getStates(numStates)
  emit(s"""class ${smName}_MPSM extends KernelStateMachine {""")


  // val stateNames = states.map("S" + _.map( _.toString).reduce(_+_))
  val stateNames = states.map(stateStr(_))
  emit(s"""
    // States
    enum States {
      INIT,
      RSET,
      ${stateNames.reduce(_ + ",\n      " + _) + ",\nDONE"}
    }
  """)

  emit("""

    // State IO
    private final DFEsmOutput sm_done;
    private final DFEsmOutput sm_last;
    private final DFEsmInput sm_en;
    private final DFEsmInput sm_numIter;
    private final DFEsmOutput rst_en;
  """)

  for(i <- 0 until numStates) {
    emit(s"""
    private final DFEsmInput s${i}_done;
    private final DFEsmOutput s${i}_en;
    """)
  }

  emit(s"""
    // State storage
    private final DFEsmStateValue sizeFF;
    private final DFEsmStateValue lastFF;
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue counterFF;
    private final DFEsmStateValue rstCounterFF;
    private final DFEsmStateValue[] bitVector;

    private final int numStates = ${numStates};
    private final int rstCycles = 10; // <-- hardcoded
    // Initialize state machine in constructor
    public ${smName}_MPSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      sm_done = io.output("sm_done", wireType);
      sm_last = io.output("sm_last", wireType);
      sm_en = io.input("sm_en", wireType);
      sm_numIter = io.input("sm_numIter", counterType);
      rst_en = io.output("rst_en", wireType);
  """)

  for(i <- 0 until numStates) {
    emit(s"""
      s${i}_done = io.input("s${i}_done", wireType);
      s${i}_en = io.output("s${i}_en", wireType);
    """)
  }

  emit("""
    // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.INIT);
      counterFF = state.value(counterType, 0);
      rstCounterFF = state.value(counterType, 0);
      sizeFF = state.value(counterType, 0);
      lastFF = state.value(wireType, 0);

      // Bitvector keeps track of which kernels have finished execution
      // This is a useful hardware synchronization structure to keep
      // track of which kernels have executed/finished execution
      bitVector = new DFEsmStateValue[numStates];
      for (int i=0; i<numStates; i++) {
        bitVector[i] = state.value(wireType, 0);
      }
    }

    private void resetBitVector() {
      for (int i=0; i<numStates; i++) {
        bitVector[i].next <== 0;
      }
    }
      """)

  emit(s"""
    @Override
    protected void nextState() {
      IF(sm_en) {
        // State-agnostic update logic for bitVector
    """)
  for(i <- 0 until numStates) {
    emit(s"""
        IF (s${i}_done) {
          bitVector[$i].next <== 1;
        }""")
  }

emit("""
        IF (counterFF === sizeFF-2) {
          lastFF.next <== 1;
        }""")

  emit(s"""
        SWITCH(stateFF) {
          CASE (States.INIT) {
            sizeFF.next <== sm_numIter;
            stateFF.next <== States.RSET;
            counterFF.next <== 0;
            rstCounterFF.next <== 0;
            lastFF.next <== 0;
          }

          CASE (States.RSET) {
            rstCounterFF.next <== rstCounterFF + 1;
            sizeFF.next <== sm_numIter;
            IF (rstCounterFF === rstCycles) {
              stateFF.next <== States.S0;
            } ELSE {
              stateFF.next <== States.RSET;
            }
          }
          """)

  for(i <- 0 until states.size) {
    val state = states(i)
    val name = stateNames(i)
    emit(s"""
          CASE (States.${name}) {""")
      stateText(state, numStates)
    emit(s"""
          }""")
  }

  emit(s"""
         CASE (States.DONE) {
           resetBitVector();
           stateFF.next <== States.INIT;
         }

         OTHERWISE {
           stateFF.next <== stateFF;
         }
        }
      }
    }""")

  emit(s"""
  @Override
    protected void outputFunction() {
      sm_done <== 0;
      sm_last <== 0;
      rst_en <== 0;
      """)

  for (i <- 0 until numStates) {
    emit(s"""
      s${i}_en <== 0;""")
  }

  emit(s"""
     IF (sm_en) {
        IF (counterFF >= sizeFF-1  | sizeFF === 0 /*hack for when reg inits to 0*/) {
          sm_last <== 1;
        } ELSE {
          sm_last <== 0;
        }
       SWITCH(stateFF) {
            CASE (States.RSET) {
              rst_en <== 1;
            }""")

        for(i <- 0 until states.size) {
          val state = states(i)
          val name = stateNames(i)
          emit(s"""
            CASE (States.$name) {""")
             for (s <- state) {
               emit(s"""s${s}_en <== ~(bitVector[$s] | s${s}_done);""")
             }
          emit(s"""
                }""")
        }

        emit(s"""
          CASE (States.DONE) {
            sm_done <== 1;
          }""")

  emit("""
      }
    }
  }
  }
  """)
  }

}

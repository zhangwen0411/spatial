package ppl.dsl.forge
package dsls
package spatial

import core.{ForgeApplication,ForgeApplicationRunner}
import scala.virtualization.lms.internal.GenericCodegen

object SpatialDSLRunner extends ForgeApplicationRunner with SpatialDSL

@dsl
trait SpatialDSL extends ForgeApplication
  with SpatialMath with SpatialMisc with PrimitiveTypes with Memories with Vectors
  with Controllers with SpatialMetadata with Enums with Sugar with TupleJunk
  with GlobalAnalysis
  with BoundAnalysis {

  override def dslName = "Spatial"
  override def dslAuthor = "Stanford PPL"
  override def dslVersion = "0.1"
  override def useReps = false

  override def addREPLOverride = false
  override def clearTraversals = true

  lazy val S = tpePar("S")
  lazy val I = tpePar("I")
  lazy val F = tpePar("F")
  lazy val G = tpePar("G")
  lazy val E = tpePar("E")

  object UnstagedNumerics {
    lazy val prims = List(SInt, SLong, SFloat, SDouble)
    lazy val types = List("Int", "Long", "Float", "Double")

    def foreach(func: (Rep[DSLType],String) => Unit) = {
      for (i <- 0 until prims.length) { func(prims(i),types(i)) }
    }
  }

  /***
   * Spatial is a domain-specific language for describing hardware datapaths. A Spatial program describes a dataflow graph consisting of various kinds of nodes
   * connected to each other by data dependencies. Each node in a Spatial program corresponds to a architectural template. Spatial is represented
   * in-memory as a parameterized, hierarchical dataflow graph.
   *
   * Templates in Spatial capture parallelism, locality, and access pattern information at multiple levels. This dramatically simplifies coarse-grained pipelining and
   * enables us to explicitly capture and represent a large space of designs which other tools cannot capture.
   * Every template is parameterized. A specific hardware design point is instantiated from a Spatial description by instantiating all the templates in the design with concrete
   * parameter values passed to the program. Spatial heavily uses metaprogramming, so these values are passed in as arguments to the Spatial program. The
   * generated design instance is represented internally as a graph that can be analyzed to provide estimates of metrics such as area and cycle count. The parameters
   * used to create the design instance can be automatically generated by a design space exploration tool.
   ***/
  def specification() = {
    // No fusion in Spatial (for now)
    disableFusion()

    val T = tpePar("T")
    val K = tpePar("K")
    val V = tpePar("V")

    // --- Primitive Types
    /** Bit represents a single bit, equivalent to a Boolean **/
    val Bit = tpe("Bit")
    /** FixPt[S,I,F] represents an arbitrary precision fixed point representation.
     * FixPt values may be signed or unsigned. Negative values, if applicable, are represented
     * in twos complement.
     *
     * The type parameters for FixPt are:
     *
     * +---+-----------------------------------+-----------------+
     * | S | Signed or unsigned representation | (Signed/Unsign) |
     * +---+-----------------------------------+-----------------+
     * | I | Number of integer bits            | (B0 - B64)      |
     * +---+-----------------------------------+-----------------+
     * | F | Number of fractional bits         | (B0 - B64)      |
     * +---+-----------------------------------+-----------------+
     *
     * Note that numbers of bits use the B- prefix as integers cannot be used as type parameters in Scala
     **/
    val FixPt = tpe("FixPt", (S,I,F))

    /** FltPt[G,E] represents an arbitrary precision, IEEE-754-like representation.
     * FltPt values are always assumed to be signed.
     *
     * The type parameters for FltPt are:
     *
     * +---+------------------------------------------------+---------------+
     * | G | Number of significand bits, including sign bit | (B2 - B64)    |
     * +---+------------------------------------------------+---------------+
     * | E | Number of exponent bits                        | (B1 - B64)    |
     * +---+------------------------------------------------+---------------+
     *
     * Note that numbers of bits use the B- prefix as integers cannot be used as type parameters in Scala
     **/
    val FltPt = tpe("FltPt", (G,E))      // significand and exponent
    primitiveTypes :::= List(Bit, FixPt, FltPt)

    // --- Type parameters
    val Signed = tpe("Signed", stage=compile)
    val Unsign = tpe("Unsign", stage=compile)
    val B = (0 to 64).map{i => tpe("B" + i, stage=compile) } // B0 - B64

    // --- Common Type Aliases
    /** Signed 32 bit integer **/
    val SInt32 = tpeAlias("SInt", FixPt(Signed, B(32), B(0)))  // Note: This is not a scala Int, this is a signed int!
    /** Signed 32 bit integer (indexing) **/
    val Index  = tpeAlias("Index", FixPt(Signed, B(32), B(0)))
    /** Unsigned 32 bit integer **/
    val UInt32 = tpeAlias("UInt", FixPt(Unsign, B(32), B(0)))
    /** IEEE-754 half precision **/
    val Half   = tpeAlias("Half", FltPt(B(11), B(5)))
    /** IEEE-754 single precision **/
    val Flt    = tpeAlias("Flt",  FltPt(B(24), B(8)))
    /** IEEE-754 double precision **/
    val Dbl    = tpeAlias("Dbl",  FltPt(B(53), B(11)))

    // --- Memory Types
    /**
     * DRAMs are pointers to locations in the accelerator's main memory to dense multi-dimensional arrays. They are the primary form of communication
     * of data between the host and the accelerator. Data may be loaded to and from the accelerator in contiguous chunks (Tiles), by random access addresses,
     * or by gather operations (SparseTiles).
     **/
    val DRAM = tpe("DRAM", T)
    /**
     * A Tile describes a continguous slice of a DRAM memory which can be loaded onto the accelerator for processing or which can be updated
     * with results once computation is complete.
     **/
    val Tile = tpe("Tile", T)
    /**
     * A SparseTile describes a sparse section of a DRAM memory which can be loaded onto the accelerator using a gather operation, or which can
     * be updated using a scatter operation.
     **/
    val SparseTile = tpe("SparseTile", T)
    /**
     * SRAMs are on-chip scratchpads with fixed size. SRAMs can be specified as multi-dimensional, but the underlying addressing
     * in hardware is always flat. The contents of SRAMs are currently persistent across loop iterations, even when they are declared in an inner scope.
     * SRAMs can have an arbitrary number of readers but only one writer. This writer may be an element-based store or a load from a DRAM memory.
     **/
    val SRAM = tpe("SRAM", T)

    /**
     * FIFOs are on-chip scratchpads with additional control logic for address-less push/pop operations. FIFOs can have an arbitrary number of readers
     * but only one writer.
     **/
    val FIFO = tpe("FIFO", T)

    /**
     * CAMs (content addressable memories) are used for associative key-value stores.
     **/
    val CAM = tpe("CAM", (K,V))

    /**
     * Caches are on-chip caches for a specific off-chip memory/data structure. Caches allow loading
     * with multi-dimentional address, whose dimensions are inherited from the cached off-chip memories.
     * The multi-dimentional address is converted to a single flat address in hardware. The
     * addressing scheme is word-based flat indexing of the offchip memory. Cache allows loading of a
     * single element at a time. During a miss, a cache automatically loads from its off-chip memory
     * and stalls the pipeline, and resumes pipeline when loading is complete.
     **/
    val Cache = tpe("Cache", T)
    /**
     * Reg defines a hardware register used to hold a scalar value. Regs have an optional name (primarily used for debugging) and reset value.
     * The default reset value for a Reg is the numeric zero value for it's specified type.
     * Regs can have an arbitrary number of readers but can only have one writer. By default, Regs are reset based upon the controller that they
     * are defined within. A Reg defined within a Pipe, for example, is reset at the beginning of each iteration of that Pipe.
     **/
    val Reg = tpe("Reg", T)
    /**
     * Vector defines a fixed size collection of scalar values.
     **/
    val Vector = tpe("Vector", T)

    primitiveStructs :::= List(DRAM, SRAM, FIFO, CAM, Reg, Vector, Cache)

    // --- State Machine Types
    /** Counter is a single hardware counter with an associated minimum, maximum, step size, and parallelization factor.
     * By default, the parallelization factor is assumed to be a design parameter. Counters can be chained together using
     * CounterChain, but this is typically done implicitly when creating controllers.
     **/
    val Counter = tpe("Counter")
    /**
     * CounterChain describes a set of chained hardware counters, where a given counter increments only when the counter
     * below it wraps around. Order is specified as outermost to innermost.
     */
    val CounterChain = tpe("CounterChain")
    val Pipeline  = tpe("Pipeline")
    primitiveStructs :::= List(Counter, CounterChain)
    primitiveTypes :::= List(Pipeline)

    // --- Other Types
    val Indices   = tpe("Indices")
    val LoopRange = tpe("LoopRange")
    val Range     = tpe("Range")
    primitiveTypes :::= List(Indices)

    val RangeWildcard = tpe("RangeWildcard", stage = compile)
    identifier (RangeWildcard) ("*")


    // --- Enums
    val ControlType = tpe("ControlType", stage=compile)
    val RegType     = tpe("RegType", stage=compile)

    noInfixList :::= List(":=", "**", "as", "to", "rst")

    // Scala.scala imports
    importTuples()
    importStrings()

    // DSL spec imports
    importSugar()
    importPrimitiveTypes()
    importEnums()
    importMetadata()

    importSpatialMath()
    importMemories()
    importVectors()
    importControllers()

    importSpatialMisc()
    importTupleTypeClassInstances()

    // Traversals: Generic traversal for debugging, etc.
    val DotPrinter            = traversal("DotIRPrinter", isExtern=true)
    val Printer               = traversal("SpatialPrinter", isExtern=true)
    val PrinterLast           = traversal("SpatialPrinterLast", isExtern=true)
    val StructurePrint        = analyzer("Structure", isExtern=true)

    // Analyzers: Set metadata
    val NameAnalyzer          = traversal("NameAnalyzer", isExtern=true)
    val DSE                   = traversal("DSE", isExtern=true)
    val OffChipAnalyzer       = traversal("OffChipAnalyzer", isExtern=true)
    val LevelAnalyzer         = analyzer("PipeLevel", isExtern=true)
    val StageAnalyzer         = analyzer("Stage", isExtern=true)
    val GlobalAnalyzer        = analyzer("Global")
    val ControlSignalAnalyzer = analyzer("ControlSignal", isExtern=true)
    val UnrolledControl       = analyzer("UnrolledControlSignal", isExtern=true)
    val SpatialAffineAnalysis = analyzer("SpatialAffine", isExtern=true)
    val BoundAnalyzer         = analyzer("Bound", isIterative=false)
    val MemoryAnalyzer        = analyzer("Memory", isExtern=true)
    val BufferAnalyzer        = analyzer("Buffer", isExtern=true)
    val AreaAnalyzer          = analyzer("Area", isExtern=true)
    val LatencyAnalyzer       = analyzer("Latency", isExtern=true)
    val OpsAnalyzer           = analyzer("Ops", isExtern=true)
    val ReductionAnalyzer     = analyzer("Reduction", isExtern=true)
    val ParameterAnalyzer     = analyzer("Parameter",isExtern=true)

    // Transformers: Change or mirror IR
    val UnitPipeTransformer   = transformer("UnitPipe", isExtern=true)
    val RegisterCleanup       = transformer("RegisterCleanup", isExtern=true)
    val Unrolling             = transformer("Unrolling", isExtern=true)
    val ConstantFolding       = traversal("ConstantFolding", isExtern=true)
    val RewriteTransformer    = traversal("RewriteTransformer", isExtern=true)

    // CGRA stuff
    val PIRScheduling     = analyzer("PIRSchedule", isExtern=true)
    val PIRGen            = traversal("PIRGen", isExtern=true) // Technically a codegen
    val PlasticineLatency = traversal("PlasticineLatencyAnalyzer", isExtern=true)

    // --- Transformer ordering notes
    // Controller metadata doesn't follow dataflow order! Explicitly run control signal analyzer after transformers

    importGlobalAnalysis()
    importBoundAnalysis()

    schedule(Printer)
    schedule(NameAnalyzer)          // Symbol names
    schedule(LevelAnalyzer)         // Initial sanity checks and pipe style annotation fixes
    schedule(BoundAnalyzer)         // Bound analysis prior to constant folding
    schedule(ConstantFolding)       // Constant folding prior to DSE
    schedule(GlobalAnalyzer)        // Values computed outside of all controllers

    // --- Unit Pipe Insertion
    schedule(Printer)
    schedule(UnitPipeTransformer)   // Wrap primitives in outer pipes
    schedule(Printer)
    schedule(GlobalAnalyzer)        // Values computed outside of all controllers (TODO: Needed again?)

    // --- Pre-DSE analysis
    schedule(OffChipAnalyzer)       // Check dimensions of offchip memories
    schedule(StageAnalyzer)         // Get number of stages in each control node
    schedule(SpatialAffineAnalysis) // Access patterns
    schedule(Printer)
    schedule(MemoryAnalyzer)        // Get used readers/writers of each memory
    schedule(Printer)
    schedule(RegisterCleanup)       // Remove unused registers/writes/reads created in unit pipe insertion
    schedule(Printer)
    //schedule(DotPrinter)            // Graph prior to unrolling

    // --- Design Space Exploration
    schedule(DSE)                   // Design space exploration. Runs a host of other analyses:
                                    // Pre-DSE:
                                    //   Bound analyzer (for finding constants)
                                    //   Parameter analyzer (for calculating design space)
                                    //   Control signal analyzer (needed for others)
                                    // During DSE:
                                    //   Bound analyzer (to propagate new constant values)
                                    //   Memory analyzer (for banking/buffering of memories)
                                    //   Contention analyzer (to estimate contention in main memory)
                                    //   Area analyzer (to estimate area)
                                    //   Latency analyzer (to estimate runtime)
                                    // Post-DSE:
                                    //   Contention analyzer (to finalize contention estimates)

    // --- Post-DSE Constants
    schedule(ParamFinalizer)        // Finalize all parameters in preparation for constant folding
    schedule(BoundAnalyzer)         // Constant propagation in metadata
    schedule(ConstantFolding)       // Constant folding
    schedule(GlobalAnalyzer)        // Add "global" annotations for newly created symbols after folding

    // --- Post-DSE Analysis
    schedule(MemoryAnalyzer)        // Memory analyzer (to finalize banking/buffering)
    schedule(Printer)
    schedule(AreaAnalyzer)          // Area estimation
    schedule(OpsAnalyzer)           // Instructions, FLOPs, etc. Also runs latency estimates
    schedule(PlasticineLatency)     // Plasticine latency

    // --- Design Elaboration
    schedule(ReductionAnalyzer)     // Reduce/accumulator specialization
    schedule(Unrolling)             // Pipeline unrolling
    schedule(Printer)
    schedule(RewriteTransformer)
    schedule(Printer)

    // --- Final analysis
    schedule(BufferAnalyzer)        // Top controllers for n-buffers
    // schedule(DotPrinter)            // Graph after unrolling
    schedule(PrinterLast)
    schedule(StructurePrint)
    schedule(PIRGen)

    // External groups
    extern(grp("Memory"), targets = List($cala, cpp, maxj), withTypes = true)
    extern(grp("Controller"), targets = List($cala, cpp, maxj))
    extern(grp("Unrolled"), targets = List($cala, maxj))

    extern(grp("ExternCounter"), targets = List($cala, cpp, maxj), withTypes = true)
    extern(metadata("ExternPrimitive"), targets = List($cala, cpp, maxj), withTypes = true)
    extern(metadata("NodeMetadata"), targets = Nil, withTypes = true)
    extern(metadata("Name"), targets = Nil, withTypes = true)
    extern(metadata("SpatialExceptions"), targets = Nil)
		()
	}
}

package spatial.shared.ops
import scala.reflect.{Manifest,SourceContext}

import scala.virtualization.lms.common.Base
import spatial.shared.ops._
import spatial.shared._

trait SpatialExceptionsOps extends Base {this: Spatial => }
trait SpatialExceptionsCompilerOps extends SpatialExceptionsOps with NodeMetadataTypes with MiscInternalOps {
  this: Spatial =>

  def name(x: Rep[Any]): String = nameOf(x).map(x => x + " ").getOrElse("") + s"($x)"
  def name(x: Manifest[_]): String = x.runtimeClass.getSimpleName match {
    case "SpatialReg" => "Reg"
    case "SpatialCAM" => "CAM"
    case "SpatialSRAM" => "SRAM"
    case "SpatialFIFO" => "FIFO"
    case "SpatialDRAM" => "DRAM"
    case "SpatialCache" => "Cache"
    case "SpatialVector" => "Vector"
    case "SpatialPipeline" => "Pipe"
    case tp => tp
  }
  def context(x: Rep[Any]) = implicitly[SourceContext]
  def repType(x: Rep[Any]) = name(manifest[Any])

  /** Internal compiler bugs, asserts, etc. **/
  abstract class SpatialException(id: Int, msg: String) extends Exception(s"Internal exception #$id: " + msg)

  case class NoInstanceIndicesException(access: Rep[Any], mem: Rep[Any]) extends
  SpatialException(0, s"No instance indices defined for access $access on ${name(mem)}")

  case class NoPortsException(access: Rep[Any], mem: Rep[Any], idx: Option[Int]) extends
  SpatialException(1, s"No ports defined for access $access on ${name(mem)}" + idx.map{i => s", port $i"}.getOrElse(""))

  case class NoCommonControllerException(a: Controller, b: Controller) extends
  SpatialException(2, s"No common controller found between $a and $b when calculating the LCA")

  case class LCADistanceException(a: Controller, b: Controller) extends
  SpatialException(3, s"Access $a was not found along the path to the LCA when calculating distance between $a and $b")

  case class ExternalMemoryWriteException(writer: Rep[Any], ctrl: Controller) extends
  SpatialException(4, s"Memory writer $writer defined outside inner pipe in $ctrl")

  case class UnknownZeroException(tp: Manifest[_])(implicit ctx: SourceContext) extends
  SpatialException(5, s"Primitive expressions with unrecognized type: ${name(tp)}")

  case class EmptyDuplicateException(mem: Rep[Any], i: Int) extends
  SpatialException(6, s"Duplicate $i of ${name(mem)} has no readers and/or writers")

  case class UndefinedChildException(top: Controller, access: Access) extends
  SpatialException(7, s"childContaining(access = $access, top = $top) is not defined because no child of top contains access")

  case class MultipleSwapControllersException(mem: Rep[Any], inst: Int, accesses: List[Access], port: Int) extends
  SpatialException(8, s"Port $port of buffered ${name(mem)} has multiple defined done control signals")

  case class UndefinedSwapControllerException(mem: Rep[Any], inst: Int, accesses: List[Access], port: Int) extends
  SpatialException(9, s"Port $port of buffered ${name(mem)} has accesses but has no defined done control signals")

  case class NoParIndicesException(mem: Rep[Any], access: Rep[Any]) extends
  SpatialException(10, s"${name(mem)} access $access has no defined par indices")

  case class UndefinedParentException(mem: Rep[Any]) extends
  SpatialException(11, s"${name(mem)} does not have a defined parent")

  case class UnknownLibraryManifest(tp: Manifest[_]) extends
  SpatialException(12, s"Cannot determine memory template type of ${name(tp)} during library runtime")


  /** Exceptions during code generation **/
  abstract class CodegenException(id: Int, msg: String) extends SpatialException(id, msg)

  // May want to move this to unsupported exceptions?
  case class MultipleWriteControllersException(mem: Rep[Any], writers: List[Access]) extends
  CodegenException(100, s"""${name(mem)} appears to have writers enabled by the same parent:
  ${writers.mkString("  \n")}""")

  case class UnknownParentControllerException(mem: Rep[Any], access: Rep[Any], parent: Rep[Any]) extends
  CodegenException(101, s"""${name(mem)} access $access has an unknown controller $parent""")

  case class UnknownAccumControllerException(mem: Rep[Any], access: Rep[Any], parent: Rep[Any]) extends
  CodegenException(102, s"""${name(mem)} access $access has an unknown accumulator controller $parent""")


  /** True user / input program errors **/
  abstract class UserException(id: Int, msg: String, console: => Unit) extends Exception(s"Error #$id: " + msg) {
    console
  }

  case class ConcurrentReadersException(mem: Rep[Any], a: Access, b: Access) extends
  UserException(0, s"Disallowed concurrent readers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has multiple concurrent readers: ")(context(mem))
    stageError(s"  First reader defined here ($a)")(context(a.node))
    stageError(s"  Second reader defined here ($b)")(context(b.node))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class ConcurrentWritersException(mem: Rep[Any], a: Access, b: Access) extends
  UserException(1, s"Disallowed concurrent writers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has multiple concurrent writers: ")(context(mem))
    stageError(s"  First writer defined here ($a)")(context(a.node))
    stageError(s"  Second writer defined here ($b)")(context(b.node))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class PipelinedReadersException(mem: Rep[Any], a: Access, b: Access) extends
  UserException(2, s"Disallowed pipelined readers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has pipelined readers: ")(context(mem))
    stageError(s"  First reader defined here ($a)")(context(a.node))
    stageError(s"  Second reader defined here ($b)")(context(b.node))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class PipelinedWritersException(mem: Rep[Any], a: Access, b: Access) extends
  UserException(3, s"Disallowed pipelined writers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has pipelined writers: ")(context(mem))
    stageError(s"  First writer defined here ($a)")(context(a.node))
    stageError(s"  Second writer defined here ($b)")(context(b.node))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class MultipleReadersException(mem: Rep[Any], readers: List[Access]) extends
  UserException(4, s"Disallowed multiple readers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has multiple readers.")(context(mem))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class MultipleWritersException(mem: Rep[Any], writers: List[Access]) extends
  UserException(5, s"Disallowed multiple writers for ${name(mem)}", {
    stageError(s"${name(mem)} defined here has multiple writers.")(context(mem))
    stageError(s"This is disallowed on memories of type ${repType(mem)}.")(context(mem))
  })

  case class MismatchedStridesException(mem: Rep[Any], a: String, b: String) extends
  UserException(6, s"Disallowed mismatched access patterns for ${name(mem)}: $a, $b", {
    stageError(s"${name(mem)} defined here is accessed with incompatible access patterns:")(context(mem))
    stageError(s"  Access Pattern 1: $a")
    stageError(s"  Access Pattern 2: $b")
    stageError(s"This is currently unsupported.")
  })

  case class MismatchedDimensionsException(mem: Rep[Any], dimA: Int, dimB: Int) extends
  UserException(7, s"Unsupported mismatched dimensions ($dimA, $dimB) for ${name(mem)}", {
    stageError(s"${name(mem)} defined here is treated as both ${dimA}D and ${dimB}D. This is currently unsupported.")(context(mem))
  })

  case class ZeroIndicesException(mem: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(8, s"Access to ${name(mem)} must use at least one index", {
    stageError(s"Access to ${name(mem)} must use at least one index")(ctx)
  })

  case class ZeroDimensionsException(tp: String)(implicit ctx: SourceContext) extends
  UserException(9, s"Cannot create $tp with zero dimensions", {
    stageError(s"Cannot create $tp with zero dimensions")(ctx)
  })

  case class EmptyReductionTreeException()(implicit ctx: SourceContext) extends
  UserException(11, s"Cannot create reduction tree from an empty list", {
    stageError("Cannot create a reduction tree from an empty list")(ctx)
  })

  // ArgIns / ArgOuts
  case class InvalidGetArgRegisterException(reg: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(12, s"Invalid argument to getArg: $reg. Can only get value of ArgOut registers", {
    stageError(s"Invalid argument to getArg: $reg. Can only get value of ArgOut registers")(ctx)
  })
  case class InvalidSetArgRegisterException(reg: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(13, s"Invalid argument to setArg: $reg. Can only set value of ArgIn registers", {
    stageError(s"Invalid argument to setArg: $reg. Can only set value of ArgIn registers")(ctx)
  })
  case class ArgInWriteException(reg: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(14, s"Writing to ArgIn registers is disallowed.", {
    stageError(s"Cannot write to input argument ${name(reg)}")(ctx)
  })

  case class DimensionMismatchException(mem: Rep[Any], dims: Int, inds: List[Rep[Any]])(implicit ctx: SourceContext) extends
  UserException(15, s"Invalid number of indices used to access ${name(mem)}: Expected ${dims}, got ${inds.length}", {
    stageError(s"Invalid number of indices used to access ${name(mem)}: Expected ${dims}, got ${inds.length}")(ctx)
  })

  // Vector
  case class EmptyVectorException()(implicit ctx: SourceContext) extends
  UserException(16, s"Cannot create an empty Vector", {
    stageError("Cannot create an empty Vector")(ctx)
  })
  case class InvalidVectorSliceException(vec: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(17, s"Slice of ${name(vec)} exceeds length of original Vector", {
    stageError(s"Slice of ${name(vec)} exceeds length of original Vector")(ctx)
  })
  case class InvalidVectorApplyException(vec: Rep[Any], i: Int)(implicit ctx: SourceContext) extends
  UserException(18, s"Invalid apply index $i for ${name(vec)}", {
    stageError(s"Invalid apply index $i for ${name(vec)}")(ctx)
  })


  case class InvalidMemoryDimensionException(dim: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(10, s"Invalid memory dimension $dim. Only constants and DSE parameters can be used as on-chip memory dimensions", {
    stageError("Invalid memory dimension. Only constants and DSE parameters are allowed as dimensions of on-chip memories")(ctx)
  })
  case class InvalidParFactorException(p: Rep[Int])(implicit ctx: SourceContext) extends
  UserException(19, s"Invalid parallelization factor $p. Counter parallelization must be a DSE parameter or a constant", {
    stageError(s"Invalid parallelization factor $p. Counter parallelization must be a DSE parameter or a constant")(ctx)
  })
  case class InvalidOffChipDimensionException(mem: Rep[Any], i: Int)(implicit ctx: SourceContext) extends
  UserException(20, s"Invalid dimension $i for ${name(mem)}. Dimensions for off-chip memories must be constants or input arguments.", {
    stageError(s"Invalid dimension $i for ${name(mem)}. Dimensions for off-chip memories must be constants or input arguments.")(context(mem))
  })

  // Controller/primitive nesting
  case class PrimitivesInParallelException(parallel: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(21, s"Parallel controller $parallel must not have any primitive nodes", {
    stageError(s"Parallel controller must not have any primitive nodes")(ctx)
  })
  case class EmptyParallelException(parallel: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(22, s"Parallel controller $parallel must have at least one control node", {
    stageError(s"Parallel controller must have at least one control node")(ctx)
  })
  case class ControlInReductionException(reduce: Rep[Any])(implicit ctx: SourceContext) extends
  UserException(23, s"Reduction function of $reduce contains control nodes. This is currently disallowed", {
    stageError(s"Reduction function defined here contains control nodes. This is currently disallowed")(ctx)
  })


  /** Currently unsupported features which may be added later **/
  abstract class UnsupportedException(id: Int, msg: String, console: => Unit) extends UserException(id, msg, console)

  case class UnsupportedSparseDimensionalityException(mem: Rep[Any], dims: Int)(implicit ctx: SourceContext) extends
  UnsupportedException(1000, s"Multi-dimensional address list in sparse offchip accesses is currently unsupported.", {
    stageError(s"Must provide flattened list of addresses for scatter and gather. Dimensionality of given address ${name(mem)} is ${dims}")(ctx)
  })

  case class UnsupportedBankingException(mem: Rep[Any]) extends
  UnsupportedException(1001, s"Banking for memories of type ${repType(mem)} is not yet supported", {
    stageError(s"Banking for memories of type ${repType(mem)} is not yet supported")(context(mem))
  })

  case class UnsupportedPowException(pow: Int)(implicit ctx: SourceContext) extends
  UnsupportedException(1002, s"Power less than 1 is currently unsupported", {
    stageError(s"Power less than 1 is currently unsupported")(ctx)
  })

  case class UnsupportedRandomException(tp: Manifest[_])(implicit ctx: SourceContext) extends
  UnsupportedException(1003, s"No random implementation yet exists for type ${name(tp)}", {
    stageError(s"No random implementation yet exists for type ${name(tp)}")(ctx)
  })

  case class UnsupportedCastException(in: Manifest[_], out: Manifest[_])(implicit ctx: SourceContext) extends
  UnsupportedException(1004, s"Unsupported cast from type ${name(in)} to type ${name(out)}", {
    stageError(s"Unsupported cast from type ${name(in)} to type ${name(out)}")(ctx)
  })

  case class UnsupportedNBufferException(mem: Rep[Any], grps: Map[Controller, List[Access]]) extends
  UnsupportedException(1005, s"${name(mem)} requires nested buffering, but this is currently unsupported", {
    stageError(s"${name(mem)} defined here requires nested buffering:")(context(mem))
    for ((lca,accesses) <- grps) {
      stageError(s"Controller: $lca")(context(lca.node))
      accesses.foreach{a => stageError(s"  Access: $a")(context(a.node)) }
    }
    stageError("Nested buffering is currently not supported.")(context(mem))
  })

  case class ReductionWithoutZeroException()(implicit ctx: SourceContext) extends
  UnsupportedException(1006, s"Reduction without zero is not yet supported", {
    stageError(s"Reduction without explicit zero value is not yet supported")(ctx)
  })

  case class ParallelizedCAMOpException(op: Rep[Any])(implicit ctx: SourceContext) extends
  UnsupportedException(1007, s"Cannot parallelize CAM operation $op", {
    stageError(s"Parallelizing CAM operations is currently unsupported")(ctx)
  })

}

package spatial.compiler.ops

import spatial.shared._
import spatial.shared.ops._
import spatial.compiler._
import spatial.compiler.ops._

import scala.virtualization.lms.internal.{Traversal, QuotingExp}

import scala.reflect.{Manifest,SourceContext}
import java.io.{File, PrintWriter}
import sys.process._
import scala.language.postfixOps

import scala.collection.mutable.Set

import ppl.delite.framework.Config
import ppl.delite.framework.analysis.IRPrinter

trait SpatialPrinter extends IRPrinter {
  import IR._
  debugMode = SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose
}

trait SpatialPrinterLast extends IRPrinter {
  import IR._
  debugMode = SpatialConfig.debugginglast || SpatialConfig.debugging
  verboseMode = SpatialConfig.verbose
}

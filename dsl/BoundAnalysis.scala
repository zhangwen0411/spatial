package ppl.dsl.forge
package dsls
package spatial

// Not for general use, only for checking expected number of iterations of loops
trait BoundAnalysis {
  this: SpatialDSL =>

  def importBoundAnalysis() = {
    val BoundAnalyzer = analyzer("Bound")
    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Tpes = lookupGrp("Tpes")
    val Ctrl = lookupGrp("BasicCtrl")
    val Tst  = lookupGrp("Nosynth")
    val Lifts = lookupGrp("ConstLifts")
    val Reg = lookupTpe("Reg")

    val BoundAnalysisRules = withAnalyzer(BoundAnalyzer)
    BoundAnalysisRules {
      analyze(Lifts, "constFixPt") using rule ${ bound(lhs) = fixed(implicitly[Numeric[T]].toDouble($0)) }
      analyze(Lifts, "constFltPt") using rule ${ bound(lhs) = fixed(implicitly[Numeric[T]].toDouble($0)) }

      analyze(Tpes, "int_to_fix") using rule ${ bound(lhs) = boundOf($0) }

      // TODO: These could actually be structs! Handle using normal propagation instead
      //analyze(Reg, "reg_new") using rule ${ bound(lhs) = bound($0).get }
      //analyze(Reg, "argin_new") using rule ${ bound(lhs) = bound($0).get }
      //analyze(Reg, "argout_new") using rule ${ bound(lhs) = bound($0).get }
      analyze(Reg, "reg_read") using rule ${
        //debug("Setting bound of " + lhs + " to " + bound($0))
        bound(lhs) = boundOf($0)
      }

      // FIXME: Not terribly accurate..
      analyze(Reg, "reg_write") using rule ${
        if (boundOf($0).isDefined && boundOf($1).isDefined) {
          val max = Math.max(bound($0).get,bound($1).get)
          //debug("Setting bound of " + $0 + " to " + max)
          bound($0) = max
        }
        else {
          //debug("Setting bound of " + $0 + " to " + boundOf($1))
          bound($0) = boundOf($1)
        }
      }

      analyze(Tst, "set_arg") using rule ${
        if (boundOf($0).isDefined && boundOf($1).isDefined)
          bound($0) = Math.max(bound($0).get,bound($1).get)
        else
          bound($0) = boundOf($1)
      }

      // NOTE: Assumes values are non-negative (i.e. max(x * y) could actually be min(x) * min(y) for neg. values)
      // Only for use with index calculation right now
      analyze(FixPt, "add") using pattern((${Fixed(x)},${Fixed(y)}) -> ${ bound(lhs) = fixed(x + y) })
      analyze(FixPt, "add") using pattern((${Fixed(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x + y) })
      analyze(FixPt, "add") using pattern((${Exact(x)},${Fixed(y)}) -> ${ bound(lhs) = exact(x + y) })
      analyze(FixPt, "add") using pattern((${Exact(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x + y) })
      analyze(FixPt, "add") using pattern((${Bound(x)},${Bound(y)}) -> ${ bound(lhs) = x + y })

      analyze(FixPt, "sub") using pattern((${Fixed(x)},${Fixed(y)}) -> ${ bound(lhs) = fixed(x - y) })
      analyze(FixPt, "sub") using pattern((${Fixed(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x - y) })
      analyze(FixPt, "sub") using pattern((${Exact(x)},${Fixed(y)}) -> ${ bound(lhs) = exact(x - y) })
      analyze(FixPt, "sub") using pattern((${Exact(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x - y) })
      analyze(FixPt, "sub") using pattern((${Bound(x)},${Exact(y)}) -> ${ bound(lhs) = x - y })

      analyze(FixPt, "mul") using pattern((${Fixed(x)},${Fixed(y)}) -> ${ bound(lhs) = fixed(x * y) })
      analyze(FixPt, "mul") using pattern((${Fixed(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x * y) })
      analyze(FixPt, "mul") using pattern((${Exact(x)},${Fixed(y)}) -> ${ bound(lhs) = exact(x * y) })
      analyze(FixPt, "mul") using pattern((${Exact(x)},${Exact(y)}) -> ${ bound(lhs) = exact(x * y) })
      analyze(FixPt, "mul") using pattern((${Bound(x)},${Bound(y)}) -> ${ bound(lhs) = x * y })

      analyze(FixPt, "div") using pattern((${Fixed(x)},${Fixed(y)}) -> ${ bound(lhs) = fixed(Math.ceil(x / y)) })
      analyze(FixPt, "div") using pattern((${Fixed(x)},${Exact(y)}) -> ${ bound(lhs) = exact(Math.ceil(x / y)) })
      analyze(FixPt, "div") using pattern((${Exact(x)},${Fixed(y)}) -> ${ bound(lhs) = exact(Math.ceil(x / y)) })
      analyze(FixPt, "div") using pattern((${Exact(x)},${Exact(y)}) -> ${ bound(lhs) = exact(Math.ceil(x / y)) })
      analyze(FixPt, "div") using pattern((${Bound(x)},${Exact(y)}) -> ${ bound(lhs) = Math.ceil(x / y) })
    }
  }

}

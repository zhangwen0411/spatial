package spatial.compiler.ops

object SpatialConfig {
  import ppl.delite.framework.Config._

  var enableDSE: Boolean = getProperty("spatial.dse", "false") != "false"
  var genCGRA: Boolean = getProperty("spatial.cgra", "false") != "false"
  var debugging: Boolean = getProperty("spatial.debugging", "false") != "false"
  var debugginglast: Boolean = getProperty("spatial.debugginglast", "false") != "false"
  var latency: Boolean = getProperty("spatial.latency", "false") != "false"
  var verbose: Boolean = getProperty("spatial.verbose", "false") != "false" || debugging
  var pirdebug: Boolean = getProperty("spatial.pirdebug", "false") != "false"
  var loudModels: Boolean = getProperty("spatial.loudmodels", "false") != "false"
  var enableArchDSE: Boolean = getProperty("spatial.pdse", "false") != "false"
  var enableSplitting: Boolean = getProperty("spatial.split", "true") != "false"


  // Plasticine limits
  var sIn: Int = getProperty("plasticine.sIn", "8").toInt
  var sbus: Int = getProperty("plasticine.sbus", "4").toInt
  var vIn: Int = getProperty("plasticine.vIn", "4").toInt
  var vOut: Int = getProperty("plasticine.vOut", "1").toInt
  var comp: Int = getProperty("plasticine.comp", "8").toInt
  var readWrite: Int = getProperty("plasticine.rw", "4").toInt
  var mems: Int = getProperty("plasticine.mems", "4").toInt

  var checkBounds: Boolean = getProperty("plasticine.bnds", "false") != "false"
}

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
}

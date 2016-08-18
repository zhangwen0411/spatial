package ppl.dsl.forge
package dsls
package spatial

trait Enums {
  this: SpatialDSL =>

  def importEnums () = {
    /* Reg Type Enum */
    val RegType = lookupTpe("RegType", stage=compile)
    identifier (RegType) ("ArgumentIn")
    identifier (RegType) ("ArgumentOut")
    identifier (RegType) ("Regular")

    /* Controller style enum */
    val ControlType = lookupTpe("ControlType", stage=compile)
    identifier (ControlType) ("InnerPipe")
    identifier (ControlType) ("StreamPipe")
    identifier (ControlType) ("CoarsePipe")
    identifier (ControlType) ("SequentialPipe")
    identifier (ControlType) ("ForkJoin")
  }

}

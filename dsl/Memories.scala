package ppl.dsl.forge
package dsls
package spatial

@dsl
trait Memories extends Regs with BRAMs with FIFOs with CAMs with OffChip with Caches {
  this: SpatialDSL =>

  object TMem extends TypeClassSignature {
    def name = "Mem"
    def prefix = "_m"
    def wrapper = None
  }

  def importMemories() {
    importMemOps()
    importRegs()
    importBRAMs()
    importFIFOs()
    importCAMs()
    importOffChip()
    importTiles()
    importSparseTiles()
    importCaches()
  }

  // Type class for local memories which can be used as accumulators in reductions
  def importMemOps() {
    val T = tpePar("T")       // data type
    val C = hkTpePar("C", T)  // memory type

    val CounterChain = lookupTpe("CounterChain")
    val Indices      = lookupTpe("Indices")
    val Idx          = lookupAlias("Index")

    val Mem = tpeClass("Mem", TMem, (T, C))
    infix (Mem) ("ld", (T,C), (C, Idx) :: T)
    infix (Mem) ("st", (T,C), (C, Idx, T) :: MUnit, effect = write(0))
    infix (Mem) ("zeroIdx", (T,C), C :: Indices)
    infix (Mem) ("flatIdx", (T,C), (C, Indices) :: Idx)
    infix (Mem) ("iterator", (T,C), (C, SList(MInt)) :: CounterChain)
    infix (Mem) ("empty", (T,C), C :: C, TNum(T))
  }
}

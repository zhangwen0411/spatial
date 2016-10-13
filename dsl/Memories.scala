package ppl.dsl.forge
package dsls
package spatial

@dsl
trait Memories extends Regs with SRAMs with FIFOs with CAMs with DRAMs with Caches {
  this: SpatialDSL =>

  object TMem extends TypeClassSignature {
    def name = "Mem"
    def prefix = "_m"
    def wrapper = None
  }

  def importMemories() {
    importMemOps()
    importRegs()
    importCAMs()
    importSRAMs()
    importFIFOs()
    importDRAMs()
    importTiles()
    importSparseTiles()
    importCaches()
  }

  // Type class for local memories which can be used as accumulators in reductions
  def importMemOps() {
    val T = tpePar("T")       // data type
    val C = hkTpePar("C", T)  // memory type

    val CounterChain = lookupTpe("CounterChain")
    val Bit          = lookupTpe("Bit")
    val Idx          = lookupAlias("Index")

    val Mem = tpeClass("Mem", TMem, (T, C))
    infix (Mem) ("ld", (T,C), (C, SList(Idx), Bit) :: T)
    infix (Mem) ("st", (T,C), (C, SList(Idx), T, Bit) :: MUnit, effect = write(0))
    infix (Mem) ("zeroLd", (T,C), (C, Bit) :: T)
    infix (Mem) ("zeroSt", (T,C), (C, T, Bit) :: MUnit, effect = write(0))
    infix (Mem) ("iterator", (T,C), (C, SList(MInt)) :: CounterChain)
    infix (Mem) ("empty", (T,C), (C) :: C, TNum(T))
  }
}

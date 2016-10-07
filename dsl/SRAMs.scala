package ppl.dsl.forge
package dsls
package spatial

@dsl
trait SRAMs {
  this: SpatialDSL =>

  // ISSUE #32: Should we support a SRAM reset API? How to time this properly? Should this be a composite which creates a Pipe?
  def importSRAMs() {
    val T = tpePar("T")
    val SRAM         = lookupTpe("SRAM")
    val Tile         = lookupTpe("Tile")
    val SparseTile   = lookupTpe("SparseTile")
    val Indices      = lookupTpe("Indices")
    val Range        = lookupTpe("Range")
    val MVector      = lookupTpe("Vector")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")

    // --- Nodes
    val sram_new = internal (SRAM) ("sram_new", T, (("size", Idx), ("zero", T)) :: SRAM(T), effect = mutable)
    val sram_load = internal (SRAM) ("sram_load", T, (("sram", SRAM(T)), ("addr", Idx)) :: T, aliasHint = aliases(Nil))
    val sram_store = internal (SRAM) ("sram_store", T, (("sram", SRAM(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    //val sram_reset = internal (SRAM) ("sram_reset", T, (("sram", SRAM(T)), ("zero", T)) :: MUnit, effect = write(0))

    // --- Internals
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_load_nd", T, (SRAM(T), SList(Idx)) :: T) implements composite ${
      if ($1.length < 1) throw ZeroIndicesException($0)
      val addr = calcAddress($1, dimsOf($0))
      val ld = sram_load($0, addr)

      if ($1.length == 1 && dimsOf($0).length > 1 && accessIndicesOf($1.head).nonEmpty)
        accessIndicesOf(ld) = accessIndicesOf($1.head)
      else
        accessIndicesOf(ld) = $1
      ld
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_store_nd", T, (SRAM(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      if ($1.length < 1) throw ZeroIndicesException($0)
      val addr = calcAddress($1, dimsOf($0))
      val st = sram_store($0, addr, $2)

      if ($1.length == 1 && dimsOf($0).length > 1 && accessIndicesOf($1.head).nonEmpty)
        accessIndicesOf(st) = accessIndicesOf($1.head)
      else
        accessIndicesOf(st) = $1
      st
    }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_zero_idx", T, SRAM(T) :: Indices) implements composite ${ indices_create(List.fill(dimsOf($0).length){0.as[Index]}) }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_calc_addr", T, (SRAM(T), Indices) :: Idx) implements composite ${ calcAddress($1.toList, dimsOf($0)) }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_iterator", T, (SRAM(T), SList(MInt)) :: CounterChain) implements composite ${
      val dims = dimsOf($0)
      val pars = List.fill(dims.length - $1.length)(param(1)) ++ $1
      val ctrs = dims.zip(pars).map{case (d,p) => Counter(min = 0, max = d, step = 1, par = p) }
      CounterChain(ctrs:_*)
    }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_empty", T, SRAM(T) :: SRAM(T), TNum(T)) implements composite ${ SRAM[T](dimsOf($0):_*) }

    val Mem = lookupTpeClass("Mem").get
    val SramMem = tpeClassInst("SramMem", T, TMem(T, SRAM(T)))
    infix (SramMem) ("ld", T, (SRAM(T), Idx) :: T) implements composite ${ sram_load_nd($0, List($1)) }
    infix (SramMem) ("st", T, (SRAM(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($0, List($1), $2) }
    infix (SramMem) ("zeroIdx", T, (SRAM(T)) :: Indices) implements composite ${ sram_zero_idx($0) }
    infix (SramMem) ("flatIdx", T, (SRAM(T), Indices) :: Idx) implements composite ${ sram_calc_addr($0, $1) }
    infix (SramMem) ("iterator", T, (SRAM(T), SList(MInt)) :: CounterChain) implements composite ${ sram_iterator($0, $1) }
    infix (SramMem) ("empty", T, SRAM(T) :: SRAM(T), TNum(T)) implements composite ${ sram_empty($0) }


    // --- API
    /** Creates a SRAM with given dimensions. Dimensions must be statically known signed integers (constants or parameters).
     * @param dims
     **/
    static (SRAM) ("apply", T, (varArgs(Idx)) :: SRAM(T), TNum(T)) implements composite ${
      if ($0.length < 1) throw ZeroDimensionsException("SRAM")
      $0.foreach{dim => if (!isStaticSize(dim)) throw InvalidMemoryDimensionException(dim) }

      val sram = sram_new[T](productTree($0.toList), zero[T])
      dimsOf(sram) = $0.toList
      sram
    }

    val SRAM_API = withTpe(SRAM)
    SRAM_API {
      /** Creates a read from this SRAM at the given multi-dimensional address. Number of indices given can either be 1 or the
       * same as the number of dimensions that the SRAM was declared with.
       * @param ii: multi-dimensional address
       **/
      infix ("apply") (varArgs(Idx) :: T) implements composite ${ sram_load_nd($self, $1.toList) }

      /*infix ("load") (Range :: MVector(T)) implements composite ${
        sramLoadVector($self, List($1.start), $1.len, param(1))
      }
      infix ("load") ((Idx, Range) :: MVector(T)) implements composite ${
        sramLoadVector($self, List($1,$2.start), $2.len, param(1))
      }
      infix ("load") ((Idx, Idx, Range) :: MVector(T)) implements composite ${
        sramLoadVector($self, List($1,$3.start), $3.len, param(1))
      }*/

      /* Store */
      /** Creates a write to this SRAM at the given 1D address.
       * @param i: 1D address
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1), $2) }
      /** Creates a write to this SRAM at the given 2D address. The SRAM must have initially been declared as 2D.
       * @param i: row index
       * @param j: column index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1, $2), $3) }
      /** Creates a write to this SRAM at the given 3D address. The SRAM must have initially been declared as 3D.
       * @param i: row index
       * @param j: column index
       * @param k: page index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1, $2, $3), $4) }

      /** Creates a write to this SRAM at the given multi-dimensional address. The number of indices given can either be 1 or the
       * same as the number of dimensions that the SRAM was declared with.
       * @param ii: multi-dimensional index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, $1.toList, $2) }

      /** @nodoc - SRAM reset is not yet well defined **/
      //infix ("rst") (Nil :: MUnit, TNum(T), effect = write(0)) implements composite ${ sram_reset($self, zero[T]) }

      /** Stores a Tile of a DRAM to this SRAM.
       * @param tile
       **/
      infix (":=") (Tile(T) :: MUnit, TNum(T), effect = write(0)) implements redirect ${ copyTile($1, $self, false) }

      /*infix (":=") (MVector(T) :: MUnit, effect = write(0)) implements composite ${
        sramStoreVector($self, dimsOf($self).map(dim => 0.as[Index]), $1, param(1))
      }*/

      /** Gathers the values from the supplied SparseTile into this SRAM
       * @param tile
       **/
      infix (":=") (SparseTile(T) :: MUnit, effect = write(0)) implements redirect ${ copySparse($1, $self, false) }
    }

    // --- Scala Backend
    impl (sram_new)   (codegen($cala, ${ Array.fill($size.toInt)($zero) })) // $t[T] refers to concrete type in IR
    impl (sram_load)  (codegen($cala, ${ $sram.apply($addr.toInt) }))
    impl (sram_store) (codegen($cala, ${ $sram.update($addr.toInt, $value) }))

    // --- C++ Backend
    impl (sram_new) (codegen(cpp, ${new $t[T]($size) }))

    // --- MaxJ Backend
    // See MemoryOpsExp in extern/compiler/src/ops

    // --- Unrolled nodes
    val load = internal (SRAM) ("par_sram_load", T, (("sram", SRAM(T)), ("addr", MVector(Idx))) :: MVector(T), aliasHint = aliases(Nil))
    val store = internal (SRAM) ("par_sram_store", T, (("sram", SRAM(T)), ("addr", MVector(Idx)), ("value", MVector(T))) :: MUnit, effect = write(0), aliasHint = aliases(Nil))

    impl (load) (codegen($cala, ${
      $addr.map{addr => if (addr.toInt < $sram.length) $sram(addr.toInt) else $sram(0) }
    }))

    impl (store) (codegen($cala, ${
      $value.zip($addr).foreach{ case (v,a) => if (a.toInt < $sram.length) $sram(a.toInt) = v }
    }))

  }
}

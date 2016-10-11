package ppl.dsl.forge
package dsls
package spatial

@dsl
trait SRAMs {
  this: SpatialDSL =>

  // ISSUE #32: Should we support a SRAM reset API? How to time this properly? Should this be a composite which creates a Pipe?
  def importSRAMs() {
    val T = tpePar("T")
    val Bit          = lookupTpe("Bit")
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

    // --- Internals
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_load_nd", T, (SRAM(T), SList(Idx)) :: T) implements composite ${
      if ($1.length != dimsOf($0).length) throw MismatchedDimensionsException($0, $1.length, dimsOf($0).length)
      sram_load($0, vectorize($1))
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_store_nd", T, (SRAM(T), SList(Idx), T, SOption(Bit)) :: MUnit, effect = write(0)) implements composite ${
      if ($1.length != dimsOf($0).length) throw MismatchedDimensionsException($0, $1.length, dimsOf($0).length)
      sram_store($0, vectorize($1), $2, $3.getOrElse(true.asBit))
    }

    /** @nodoc -- only used for Mem typeclass instance **/
    direct (SRAM) ("sram_zero_idx", T, SRAM(T) :: SList(Idx)) implements composite ${ List.fill(dimsOf($0).length){0.as[Index]} }

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
    infix (SramMem) ("ld", T, (SRAM(T), SList(Idx)) :: T) implements composite ${
      sram_load_nd($0, $1)
    }
    infix (SramMem) ("st", T, (SRAM(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      sram_store_nd($0, $1, $2, None)
    }
    infix (SramMem) ("zeroLd", T, (SRAM(T)) :: T) implements composite ${
      sram_load_nd($0, sram_zero_idx($0))
    }
    infix (SramMem) ("zeroSt", T, (SRAM(T), T) :: MUnit, effect = write(0)) implements composite ${
      sram_store_nd($0, sram_zero_idx($0), $1, None)
    }
    infix (SramMem) ("iterator", T, (SRAM(T), SList(MInt)) :: CounterChain) implements composite ${
      sram_iterator($0, $1)
    }
    infix (SramMem) ("empty", T, SRAM(T) :: SRAM(T), TNum(T)) implements composite ${
      sram_empty($0)
    }


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

      /* Store */
      /** Creates a write to this SRAM at the given 1D address.
       * @param i: 1D address
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1), $2, None) }
      /** Creates a write to this SRAM at the given 2D address. The SRAM must have initially been declared as 2D.
       * @param i: row index
       * @param j: column index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1, $2), $3, None) }
      /** Creates a write to this SRAM at the given 3D address. The SRAM must have initially been declared as 3D.
       * @param i: row index
       * @param j: column index
       * @param k: page index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, List($1, $2, $3), $4, None) }

      /** Creates a write to this SRAM at the given multi-dimensional address. The number of indices given can either be 1 or the
       * same as the number of dimensions that the SRAM was declared with.
       * @param ii: multi-dimensional index
       * @param x: element to be stored to SRAM
       **/
      infix ("update") ((SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${ sram_store_nd($self, $1.toList, $2, None) }

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

      infix ("par") (MInt :: SRAM(T)) implements composite ${
        tilePar($self) = $1
        $self
      }
    }

    // --- Scala Backend
    impl (sram_new)   (codegen($cala, ${ Array.fill($size.toInt)($zero) })) // $t[T] refers to concrete type in IR

    // --- C++ Backend
    impl (sram_new) (codegen(cpp, ${new $t[T]($size) }))

    // --- MaxJ Backend
    // See MemoryOpsExp in extern/compiler/src/ops

  }
}

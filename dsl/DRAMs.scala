package ppl.dsl.forge
package dsls
package spatial

@dsl
trait DRAMs {
  this: SpatialDSL =>

  def importDRAMs() {
    val T = tpePar("T")

    val DRAM       = lookupTpe("DRAM")
    val Tile       = lookupTpe("Tile")
    val SparseTile = lookupTpe("SparseTile")
    val SRAM       = lookupTpe("SRAM")
    val FIFO       = lookupTpe("FIFO")
    val Range      = lookupTpe("Range")
    val MVector    = lookupTpe("Vector")
    val Idx        = lookupAlias("Index")

    // --- Nodes
    val dram_new   = internal (DRAM) ("dram_new", T, ("size", Idx) :: DRAM(T), effect = mutable)

    // --- Internal
    val dram_create = internal (DRAM) ("dram_create", T, SList(Idx) :: DRAM(T), TNum(T)) implements composite ${
      if ($0.length < 1) throw ZeroDimensionsException("DRAM")
      val offchip = dram_new[T](productTree($0.toList))
      dimsOf(offchip) = $0.toList
      offchip
    }

    // --- API
    /** Creates a reference to a multi-dimensional array in main memory with given dimensions
     * @param dims
     **/
    static (DRAM) ("apply", T, (varArgs(Idx)) :: DRAM(T), TNum(T)) implements composite ${ dram_create[T]($0.toList) }

    // Offer multiple versions of tile select since implicit cast from signed int to range isn't working
    val DRAM_API = withTpe(DRAM)
    DRAM_API {
      /** Creates a reference to a 1D Tile of this 1D DRAM which can be loaded into local memory.
       * @param cols
       **/
      infix ("apply") (Range :: Tile(T)) implements composite ${ tile_create($self, List($1)) }
      /** Creates a reference to a 2D Tile of this 2D DRAM which can be loaded into local memory.
       * @param rows
       * @param cols
       **/
      infix ("apply") ((Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2)) }
      /** Creates a reference to a 3D Tile of this 3D DRAM which can be loaded into local memory.
       * @param rows
       * @param cols
       * @param pages
       **/
      infix ("apply") ((Range,Range,Range) :: Tile(T)) implements composite ${ tile_create($self, List($1,$2,$3)) }

      // 2D -> 1D
      /** Creates a reference to a 1D row Tile of this 2D DRAM
       * @param row
       * @param cols
       **/
      infix ("apply") ((Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2)) }
      /** Creates a reference to a 1D column Tile of this 2D DRAM
       * @param rows
       * @param col
       **/
      infix ("apply") ((Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2))) }

      // 3D -> 2D
      /** Creates a reference to a 2D column/page Tile of this 3D DRAM
       * @param row
       * @param cols
       * @param pages
       **/
      infix ("apply") ((Idx, Range, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, $3)) }
      /** Creates a reference to a 2D row/page Tile of this 3D DRAM
       * @param rows
       * @param col
       * @param pages
       **/
      infix ("apply") ((Range, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), $3)) }
      /** Creates a reference to a 2D row/column Tile of this 3D DRAM
       * @param rows
       * @param cols
       * @param page
       **/
      infix ("apply") ((Range, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, $2, unitRange($3))) }

      // 3D -> 1D
      /** Creates a reference to a 1D page Tile of this 3D DRAM
       * @param row
       * @param col
       * @param pages
       **/
      infix ("apply") ((Idx, Idx, Range) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), unitRange($2), $3)) }
      /** Creates a reference to a 1D column Tile of this 3D DRAM
       * @param row
       * @param cols
       * @param page
       **/
      infix ("apply") ((Idx, Range, Idx) :: Tile(T)) implements composite ${ tile_create($self, List(unitRange($1), $2, unitRange($3))) }
      /** Creates a reference to a 1D row Tile of this 3D DRAM
       * @param rows
       * @param col
       * @param page
       **/
      infix ("apply") ((Range, Idx, Idx) :: Tile(T)) implements composite ${ tile_create($self, List($1, unitRange($2), unitRange($3))) }

      /** Sets up a sparse gather from this DRAM using *size* addresses from the supplied SRAM
       * @param addrs: SRAM with addresses to load
       * @param size: the number of addresses
       **/
      infix ("apply") ((SRAM(Idx), Idx) :: SparseTile(T)) implements composite ${ stile_create($self, $1, $2) }

      /** Sets up a sparse gather from this DRAM using all addresses from the supplied SRAM
       * @param addrs: SRAM with addresses to load
       **/
      infix ("apply") (SRAM(Idx) :: SparseTile(T)) implements composite ${ stile_create($self, $1, sizeOf($1)) }
    }

    // --- Scala Backend
    impl (dram_new) (codegen($cala, ${ new Array[$t[T]]($size.toInt) }))

    // --- C++ Backend
    impl (dram_new) (codegen(cpp, ${new $t[T]($size) }))

    // --- MaxJ Backend
    //dram_new (extern)
  }


  def importTiles() {
    val T = tpePar("T")

    val Tile  = lookupTpe("Tile")
    val DRAM  = lookupTpe("DRAM")
    val SRAM  = lookupTpe("SRAM")
    val FIFO  = lookupTpe("FIFO")
    val Range = lookupTpe("Range")

    data(Tile, ("_target", DRAM(T)))
    internal (Tile) ("tile_new", T, DRAM(T) :: Tile(T)) implements allocates(Tile, ${$0})
    internal (Tile) ("tile_create", T, (DRAM(T), SList(Range)) :: Tile(T)) implements composite ${
      if (dimsOf($0).length != $1.length) throw DimensionMismatchException($0, dimsOf($0).length, $1)
      val tile = tile_new($0)
      rangesOf(tile) = $1
      tile
    }
    internal.infix (Tile) ("mem", T, Tile(T) :: DRAM(T)) implements getter(0, "_target")


    /** Creates a store from the given on-chip SRAM to this Tile of off-chip memory
     * @param sram
     **/
    infix (Tile) (":=", T, (Tile(T), SRAM(T)) :: MUnit, TNum(T), effect = write(0)) implements redirect ${ copyTile($0, $1, true) }
    infix (Tile) (":=", T, (Tile(T), FIFO(T)) :: MUnit, TNum(T), effect = write(0)) implements redirect ${ streamTile($0, $1, true) }

    direct (Tile) ("streamTile", T, (("tile", Tile(T)), ("fifo", FIFO(T)), ("store", SBoolean)) :: MUnit, TNum(T), effect = simple) implements composite ${
      val mem = $tile.mem
      val offsets = rangesOf($tile).map(_.start)
      val tileDims = rangesOf($tile).map(_.len)

      val p = tilePar(rangesOf(tile).last).getOrElse(param(1))
      val len = tileDims.last

      val px = param(1); domainOf(px) = (1,1,1)

      if (tileDims.length > 1) {
        Pipe(CounterChain(tileDims.take(tileDims.length - 1).map{d => Counter(min = 0, max = d, step = 1, par = px) }:_*)){inds =>
          val indices = inds.toList :+ 0.as[Index]
          val memOfs = calcAddress(offsets.zip(indices).map{case (a,b) => a + b}, dimsOf(mem))

          if ($store) { burst_store(mem, $fifo, memOfs, len, p) }
          else        { burst_load(mem, $fifo, memOfs, len, p) }
        }
      }
      else {
        Pipe {  // TODO: Is this needed?
          val memOfs = calcAddress(offsets, dimsOf(mem))
          if ($store) { burst_store(mem, $fifo, memOfs, len, p) }
          else        { burst_load(mem, $fifo, memOfs, len, p) }
        }
      }
    }

    /** @nodoc - not actually a user-facing method for now **/
    direct (Tile) ("copyTile", T, (("tile",Tile(T)), ("local",SRAM(T)), ("store", SBoolean)) :: MUnit, TNum(T), effect = simple) implements composite ${
      val mem      = $tile.mem
      val offsets  = rangesOf($tile).map(_.start)
      val tileDims = rangesOf($tile).map(_.len)
      val unitDims = rangesOf($tile).map(isUnit(_))

      val p = tilePar(rangesOf($tile).last).getOrElse(param(1))
      val len = tileDims.last

      val fifo = FIFO[T](512) // TODO: How to determine FIFO depth?

      val px = param(1); domainOf(px) = (1,1,1)


      if (tileDims.length > 1) {
        Pipe(CounterChain(tileDims.take(tileDims.length - 1).map{d => Counter(min = 0, max = d, step = 1, par = px) }:_*)){inds =>
          val indices = inds.toList :+ 0.as[Index]
          val localOfs = indices.zip(unitDims).flatMap{case (i,isUnitDim) => if (!isUnitDim) Some(i) else None}
          val memOfs = calcAddress(offsets.zip(indices).map{case (a,b) => a + b}, dimsOf(mem))

          if ($store) {
            Pipe(len par p){i =>
              val localAddr = localOfs.take(localOfs.length - 1) :+ (localOfs.last + i)
              fifo.push($local(localAddr:_*))
            }

            burst_store(mem, fifo, memOfs, len, p)
          }
          else {
            // val el_per_burst = 384*8/nbits(manifest[T]) // Get number of elements in a burst
            // val start_bound = memOfs % el_per_burst // Figure out number of elements to ignore before we get desired data
            // val memOfs_downcast = memOfs - start_bound // Figure out burst-aligned memOfs
            // val end_bound = start_bound + len // Figure out number of elements before we should ignore again
            // val len_upcast = (end_bound - (end_bound % el_per_burst)) + el_per_burst // Upcast memory request to nearest burst alignment

            burst_load(mem, fifo, memOfs/*_downcast*/, len/*_upcast*/, p)

            Pipe(len/*_upcast*/ par p){i =>
              val localAddr = localOfs.take(localOfs.length - 1) :+ (localOfs.last + i)
              $local(localAddr/*,en = i > start_bound & i < end_bound*/) = fifo.pop()
            }
          }
        }
      }
      else {
        Pipe {
          val memOfs = calcAddress(offsets, dimsOf(mem))
          if ($store) {
            Pipe(len par p){i => fifo.push($local(i)) }
            burst_store(mem, fifo, memOfs, len, p)
          }
          else {
            // val el_per_burst = 384*8/nbits(manifest[T]) // Get number of elements in a burst
            // val start_bound = memOfs % el_per_burst // Figure out number of elements to ignore before we get desired data
            // val memOfs_downcast = memOfs - start_bound // Figure out burst-aligned memOfs
            // val end_bound = start_bound + len // Figure out number of elements before we should ignore again
            // val len_upcast = (end_bound - (end_bound % el_per_burst)) + el_per_burst // Upcast memory request to nearest burst alignment

            burst_load(mem, fifo, memOfs/*_downcast*/, len/*_upcast*/, p)

            Pipe(len/*_upcast*/ par p){i => $local(i/*, en = i > start_bound & i < end_bound*/) = fifo.pop() }
          }
        }
      }
    }
  }

  def importSparseTiles() {
    val T = tpePar("T")

    val SparseTile = lookupTpe("SparseTile")
    val DRAM       = lookupTpe("DRAM")
    val SRAM       = lookupTpe("SRAM")
    val Idx = lookupAlias("Index")

    data(SparseTile, ("_target", DRAM(T)), ("_addrs", SRAM(Idx)), ("_len", Idx))
    internal (SparseTile) ("stile_new", T, (DRAM(T), SRAM(Idx), Idx) :: SparseTile(T)) implements allocates(SparseTile, ${$0}, ${$1}, ${$2})
    internal (SparseTile) ("stile_create", T, (DRAM(T), SRAM(Idx), Idx) :: SparseTile(T)) implements composite ${
      if (dimsOf($1).length != 1) throw UnsupportedSparseDimensionalityException($1, dimsOf($1).length)
      stile_new($0, $1, $2)
    }
    internal.infix (SparseTile) ("mem", T, SparseTile(T) :: DRAM(T)) implements getter(0, "_target")
    internal.infix (SparseTile) ("addr", T, SparseTile(T) :: SRAM(Idx)) implements getter(0, "_addrs")
    internal.infix (SparseTile) ("len", T, SparseTile(T) :: Idx) implements getter(0, "_len")

    /** Creates a store from the given on-chip SRAM to this SparseTile of off-chip memory
     * @param sram
     **/
    infix (SparseTile) (":=", T, (SparseTile(T), SRAM(T)) :: MUnit, effect = write(0)) implements redirect ${ copySparse($0, $1, true) }

    direct (SparseTile) ("copySparse", T, (("tile",SparseTile(T)), ("local",SRAM(T)), ("isScatter", SBoolean)) :: MUnit, effect = simple) implements composite ${
      val mem   = $tile.mem
      val addrs = $tile.addr
      val len   = $tile.len
      val p     = tilePar(addrs).getOrElse(param(1))

      if ($isScatter) { scatter(mem, $local, addrs, len, p) }
      else            { gather(mem, $local, addrs, len, p) }
    }

  }

}

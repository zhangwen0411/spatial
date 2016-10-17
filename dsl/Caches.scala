package ppl.dsl.forge
package dsls
package spatial

@dsl
trait Caches {
  this: SpatialDSL =>

  def importCaches() {
    val T = tpePar("T")
    val Cache   = lookupTpe("Cache")
    val Tile    = lookupTpe("Tile")
    val DRAM    = lookupTpe("DRAM")
    val Idx     = lookupAlias("Index")
    val Indices = lookupTpe("Indices")

    // --- Nodes
    val cache_new = internal (Cache) ("cache_new", T, ("offchip", DRAM(T)) :: Cache(T), effect = mutable)
    val cache_load = internal (Cache) ("cache_load", T, (("cache", Cache(T)), ("addr", Idx)) :: T)
    val cache_store = internal (Cache) ("cache_store", T, (("cache", Cache(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    //ISSUE #32: cache_flush?

    // --- Internals

    /** @nodoc **/
    direct (Cache) ("cache_load_nd", T, (Cache(T), SList(Idx)) :: T) implements composite ${
      val addr = calcAddress($1, dimsOf($0))
      cache_load($0, addr)
    }
    /** @nodoc **/
    direct (Cache) ("cache_store_nd", T, (Cache(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      val addr = calcAddress($1, dimsOf($0))
      cache_store($0, addr, $2)
    }

    /** @nodoc **/
    direct (Cache) ("cache_calc_addr", T, (Cache(T), Indices) :: Idx) implements composite ${ calcAddress($1.toList, dimsOf($0)) }

    // Mem type class is primarily used for accumulator reductions. Using a cache for this is not well defined right now
    /*val Mem = lookupTpeClass("Mem").get
    val CacheMem = tpeClassInst("CacheMem", T, TMem(T, Cache(T)))
    infix (CacheMem) ("ld", T, (Cache(T), Idx) :: T) implements composite ${ cache_load_nd($0, List($1)) }
    infix (CacheMem) ("st", T, (Cache(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($0, List($1), $2) }
    infix (CacheMem) ("flatIdx", T, (Cache(T), Indices) :: Idx) implements composite ${ cache_calc_addr($0, $1) }*/

    // --- API
    /** Creates a Cache with target DRAM. Dimensions is inherited from DRAM
     * @param name
     * @param offchip
     **/
    static (Cache) ("apply", T, DRAM(T) :: Cache(T), TNum(T)) implements composite ${
      val cache = cache_new[T]($0)
      dimsOf(cache) = dimsOf($0)
      cache
    }

    val Cache_API = withTpe(Cache)
    Cache_API {
      /* Load */
      /** Creates a read from this Cache at the given multi-dimensional address. Number of indices given can either be 1 or the
       * same as the number of dimensions that the cached DRAM was declared with.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param ii: multi-dimensional address
       **/
      infix ("apply") ((Idx, varArgs(Idx)) :: T) implements composite ${ cache_load_nd($self, $1 +: $2.toList) }

      /* Store */
      /** Creates a write to this Cache at the given 1D address.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: 1D address
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1), $2) }
      /** Creates a write to this Cache at the given 2D address. The cached DRAM must have initially been declared as 2D.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: row index
       * @param j: column index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1, $2), $3) }
      /** Creates a write to this Cache at the given 3D address. The cached DRAM must have initially been declared as 3D.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: row index
       * @param j: column index
       * @param k: page index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1, $2, $3), $4) }
      /** Creates a write to this Cache at the given multi-dimensional address. The number of indices given can either be 1 or the
       * same as the number of dimensions that the cached DRAM was declared with.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param ii: multi-dimensional index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((SSeq(Idx), T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, $1.toList, $2) }

    }

    // --- Scala Backend
    impl (cache_new)   (codegen($cala, ${ $offchip }))
    impl (cache_load)  (codegen($cala, ${ $cache.apply($addr.toInt) }))
    impl (cache_store) (codegen($cala, ${ $cache.update($addr.toInt, $value) }))
  }
}

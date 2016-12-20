package ppl.dsl.forge
package dsls
package spatial

@dsl
trait FIFOs {
  this: SpatialDSL =>

  // TODO: Writing/reading at specific addresses from FIFO?
  def importFIFOs() {
    val T = tpePar("T")
    val FIFO         = lookupTpe("FIFO")
    val Tile         = lookupTpe("Tile")
    val SparseTile   = lookupTpe("SparseTile")
    val Indices      = lookupTpe("Indices")
    val Range        = lookupTpe("Range")
    val MVector      = lookupTpe("Vector")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")
    val Bit          = lookupTpe("Bit")

    // --- Nodes
    val fifo_new = internal (FIFO) ("fifo_new", T, (("size", Idx), ("zero", T)) :: FIFO(T), effect = mutable)
    val fifo_load = internal (FIFO) ("fifo_load", T, (("fifo", FIFO(T)), ("addr", Idx)) :: T, aliasHint = aliases(Nil))
    val fifo_store = internal (FIFO) ("fifo_store", T, (("fifo", FIFO(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))

    val fifo_push = internal (FIFO) ("push_fifo", T, (("fifo", FIFO(T)), ("value",T), ("en", Bit)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val fifo_pop  = internal (FIFO) ("pop_fifo", T, (("fifo", FIFO(T)), ("en", Bit)) :: T, aliasHint = aliases(Nil))
    val fifo_count = internal (FIFO) ("count_fifo", T, ("fifo", FIFO(T)) :: Idx, aliasHint = aliases(Nil))

    // --- Internal
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (FIFO) ("popFIFO", T, (FIFO(T), Bit) :: T) implements composite ${
      pop_fifo($0, $1)
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (FIFO) ("pushFIFO", T, (FIFO(T), T, Bit) :: MUnit, effect = write(0)) implements composite ${
      push_fifo($0, $1, $2)
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (FIFO) ("fifo_iterator", T, (FIFO(T), SList(MInt)) :: CounterChain) implements composite ${
      val dims = dimsOf($0)
      val pars = List.fill(dims.length - $1.length)(param(1)) ++ $1
      val ctrs = dims.zip(pars).map{case (d,p) => Counter(min = 0, max = d, step = 1, par = p) }
      CounterChain(ctrs:_*)
    }
    /** @nodoc -- only used for Mem typeclass instance **/
    direct (FIFO) ("fifo_empty", T, FIFO(T) :: FIFO(T), TNum(T)) implements composite ${ FIFO[T](dimsOf($0).head) }


    val Mem = lookupTpeClass("Mem").get
    val FifoMem = tpeClassInst("FifoMem", T, TMem(T, FIFO(T)))
    infix (FifoMem) ("ld", T, (FIFO(T), SList(Idx), Bit) :: T) implements composite ${
      popFIFO($0, $2)
    }
    infix (FifoMem) ("st", T, (FIFO(T), SList(Idx), T, Bit) :: MUnit, effect = write(0)) implements composite ${
      pushFIFO($0, $2, $3)
    }
    infix (FifoMem) ("zeroLd", T, (FIFO(T), Bit) :: T) implements composite ${
      popFIFO($0, $1)
    }
    infix (FifoMem) ("zeroSt", T, (FIFO(T), T, Bit) :: MUnit, effect = write(0)) implements composite ${
      pushFIFO($0, $1, $2)
    }
    infix (FifoMem) ("iterator", T, (FIFO(T), SList(MInt)) :: CounterChain) implements composite ${
      fifo_iterator($0, $1)
    }
    infix (FifoMem) ("empty", T, FIFO(T) :: FIFO(T), TNum(T)) implements composite ${
      fifo_empty($0)
    }

    // --- API
    /** Creates a FIFO with given size. Size must be a statically known signed integer (constant or parameter).
     * @param size
     **/
    static (FIFO) ("apply", T, (Idx) :: FIFO(T), TNum(T)) implements composite ${
      if (!isStaticSize($0)) InvalidMemoryDimensionException($0)
      val fifo = fifo_new[T]($0, zero[T])
      dimsOf(fifo) = List($0)
      fifo
    }

    val FIFO_API = withTpe(FIFO)
    FIFO_API {
      /** Creates a push (write) port to this FIFO
       * @param value: the value to be pushed into the FIFO
       **/
      infix ("push") (T :: MUnit, effect = write(0)) implements composite ${ push_fifo($self, $1, true.asBit) }

      /** Creates a push (write) port to this FIFO with a write enable
       * @param value: the value to be pushed into the FIFO
       * @param wren: write enable
       **/
      infix ("push") ((T, Bit) :: MUnit, effect = write(0)) implements composite ${ push_fifo($self, $1, $2) }

      /** Creates a pop (read) port to this FIFO **/
      infix ("pop") (Nil :: T) implements composite ${ pop_fifo($self, true.asBit) }
      infix ("pop") (Bit :: T) implements composite ${ pop_fifo($self, $1)}

      infix ("count") (Nil :: Idx) implements composite ${ count_fifo($self) }

      /** Streams a Tile of a DRAM memory to this FIFO.
       * @param tile
       **/
      infix (":=") (Tile(T) :: MUnit, TNum(T), effect = write(0)) implements redirect ${ copyTile($1, $self, false) }
    }

    // --- Scala Backend
    impl (fifo_new)   (codegen($cala, ${ scala.collection.mutable.Queue.fill(0.toInt)($zero) }))
    impl (fifo_load)  (codegen($cala, ${ $fifo.apply($addr.toInt) }))
    impl (fifo_store) (codegen($cala, ${ $fifo.update($addr.toInt, $value) }))
    impl (fifo_push)  (codegen($cala, ${ if ($en) $fifo.enqueue($value); () }))
    impl (fifo_pop)   (codegen($cala, ${ if ($en) $fifo.dequeue() else $fifo.front }))
    impl (fifo_count) (codegen($cala, ${ FixedPoint[Signed,B32,B0]($fifo.length) }))


    // --- Unrolled nodes
    val push = internal (FIFO) ("par_push_fifo", T, (("fifo", FIFO(T)), ("value",MVector(T)), ("en", MVector(Bit)), ("shuffle", SBoolean)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val pop  = internal (FIFO) ("par_pop_fifo", T, (("fifo", FIFO(T)), ("en", MVector(Bit))) :: MVector(T), aliasHint = aliases(Nil))

    impl (push) (codegen($cala, ${
      $value.zip($en).foreach{ case (v,e) => if (e) $fifo.enqueue(v) }
    }))

    impl (pop) (codegen($cala, ${
      // Assumes there's at least one element
      val first = $fifo.front
      $en.map{e => if ($fifo.nonEmpty && e) $fifo.dequeue() else first }
    }))

  }
}

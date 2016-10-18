package ppl.dsl.forge
package dsls
package spatial

@dsl
trait Regs {
  this: SpatialDSL =>

  // ISSUE #32: Better / more correct way of exposing register reset?
  def importRegs() {
    val T = tpePar("T")
    val Reg          = lookupTpe("Reg")
    val FixPt        = lookupTpe("FixPt")
    val FltPt        = lookupTpe("FltPt")
    val Bit          = lookupTpe("Bit")
    val RegType      = lookupTpe("RegType", stage=compile)
    val Indices      = lookupTpe("Indices")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")

    // --- Nodes
    val reg_new    = internal (Reg) ("reg_new", T, ("init", T) :: Reg(T), effect = mutable)
    val argin_new  = internal (Reg) ("argin_new", T, ("init", T) :: Reg(T), effect = mutable)
    val argout_new = internal (Reg) ("argout_new", T, ("init", T) :: Reg(T), effect = mutable)
    val reg_read   = internal (Reg) ("reg_read", T, ("reg", Reg(T)) :: T, aliasHint = aliases(Nil))
    val reg_write  = internal (Reg) ("reg_write", T, (("reg", Reg(T)), ("value", T), ("en", Bit)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val reg_reset  = internal (Reg) ("reg_reset", T, ("reg", Reg(T)) :: MUnit, effect = write(0))

    // --- Internals
    /** @nodoc **/
    direct (Reg) ("reg_create", T, (T) :: Reg(T), effect = mutable) implements composite ${
      val reg = reg_new[T](init = $0)
      regType(reg) = Regular
      resetValue(reg) = $0
      reg
    }

    /** @nodoc **/
    direct (Reg) ("readReg", T, ("reg", Reg(T)) :: T) implements composite ${ reg_read($0) }
    /** @nodoc **/
    direct (Reg) ("writeReg", T, (("reg", Reg(T)), ("value", T), ("en", Bit)) :: MUnit, effect = write(0)) implements composite ${ reg_write($0, $1, $2) }

    /** @nodoc **/
    direct (Reg) ("reg_zero_idx", Nil, Nil :: Indices) implements composite ${ indices_create(List(0.as[Index])) }

    val Mem = lookupTpeClass("Mem").get
    val RegMem = tpeClassInst("RegMem", T, TMem(T, Reg(T)))
    infix (RegMem) ("ld", T, (Reg(T), SList(Idx), Bit) :: T) implements composite ${
      readReg($0)
    }
    infix (RegMem) ("st", T, (Reg(T), SList(Idx), T, Bit) :: MUnit, effect = write(0)) implements composite ${
      writeReg($0, $2, $3)
    }
    infix (RegMem) ("zeroLd", T, (Reg(T), Bit) :: T) implements composite ${
      readReg($0)
    }
    infix (RegMem) ("zeroSt", T, (Reg(T), T, Bit) :: MUnit, effect = write(0)) implements composite ${
      writeReg($0, $1, $2)
    }
    infix (RegMem) ("iterator", T, (Reg(T), SList(MInt)) :: CounterChain) implements composite ${
      CounterChain(Counter(max=1))
    }
    infix (RegMem) ("empty", T, Reg(T) :: Reg(T), TNum(T)) implements composite ${
      reg_create[T](zero[T])
    }

    // --- API
    /* Reg */
    /** Creates a register with type T **/
    static (Reg) ("apply", T, Nil :: Reg(T), TNum(T)) implements composite ${ reg_create[T](zero[T]) }

    UnstagedNumerics.foreach{ (ST,_) =>
      /** Creates an unnamed register with type T and given reset value **/
      static (Reg) ("apply", T, ("reset", ST) :: Reg(T), TNum(T)) implements composite ${ reg_create[T]($reset.as[T]) }
    }
    /** Creates an unnamed input argument from the host CPU **/
    direct (Reg) ("ArgIn", T, Nil :: Reg(T), TNum(T)) implements composite ${
      val rst = zero[T]
      val argIn = argin_new[T](init = rst)
      regType(argIn) = ArgumentIn
      resetValue(argIn) = rst
      argIn
    }
    /** Creats an unnamed output argument to the host CPU **/
    direct (Reg) ("ArgOut", T, Nil :: Reg(T), TNum(T)) implements composite ${
      val rst = zero[T]
      val argOut = argout_new[T](init = rst)
      regType(argOut) = ArgumentOut
      resetValue(argOut) = rst
      argOut
    }

    val Reg_API = withTpe(Reg)
    Reg_API {
      /** Reads the current value of this register **/
      infix ("value") (Nil :: T) implements redirect ${ readReg($self) }
      /** Creates a writer to this Reg. Note that Regs and ArgOuts can only have one writer, while ArgIns cannot have any **/
      infix (":=") (("x",T) :: MUnit, effect = write(0)) implements composite ${
        if (regType($self) == ArgumentIn) throw ArgInWriteException($self)
        writeReg($self, $1, true)
      }
      /** @nodoc - User register reset is not yet well-defined **/
      infix ("rst") (Nil :: MUnit, effect = write(0)) implements composite ${ reg_reset($self) }
    }

    /** Enables implicit reading from fixed point type Regs **/
    fimplicit (Reg) ("regFixToFix", (S,I,F), Reg(FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ readReg($0) }
    /** Enables implicit reading from floating point type Regs **/
    fimplicit (Reg) ("regFltToFlt", (G,E), Reg(FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ readReg($0) }
    /** Enables implicit reading from bit type Regs **/
    fimplicit (Reg) ("regBitToBit", Nil, Reg(Bit) :: Bit) implements redirect ${ readReg($0) }



    // --- Scala Backend
    impl (reg_new)    (codegen($cala, ${ Array($init) }))
    impl (argin_new)  (codegen($cala, ${ Array($init) }))
    impl (argout_new) (codegen($cala, ${ Array($init) }))
    impl (reg_read)   (codegen($cala, ${ $reg.apply(0) }))
    impl (reg_write)  (codegen($cala, ${ if ($en) $reg.update(0, $value) }))
    impl (reg_reset)  (codegen($cala, ${
      @ val init = resetValue($reg)
      $reg.update(0, $init)
    }))

    // --- C++ Backend
    impl (reg_new)   (codegen(cpp, ${
      @ val tpname = remap(sym.tp.typeArguments(0))
      new $tpname {$init}
    }))
    impl (argin_new) (codegen(cpp, ${
      @ val tpname = remap(sym.tp.typeArguments(0))
      new $tpname {$init}
    }))
    impl (argout_new) (codegen(cpp, ${
      @ val tpname = remap(sym.tp.typeArguments(0))
      new $tpname {$init}
    }))
    impl (reg_read)  (codegen(cpp, ${ *$reg }))
    impl (reg_write)  (codegen(cpp, ${ if ($en) { *$reg = $value ; } }))
    impl (reg_reset) (codegen(cpp, ${
      @ val init = resetValue($reg)
      *$reg = $init
    }))

    // --- MaxJ Backend
    //reg_new (extern)

    //reg_write (extern)
    impl (reg_reset) (codegen(maxj, ${
    }))

    // -- Chisel Backend
    impl (reg_reset) (codegen(chisel, ${
    }))
  }
}

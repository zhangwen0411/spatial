# Spatial
Spatial is a Forge DSL for programming reconfigurable hardware from a parameterized, high level abstraction.

Prerequisites
=============

* Java JDK: http://www.oracle.com/technetwork/java/javase/downloads/index.html
* Scala SBT: http://www.scala-sbt.org
* **[Optional]** Sphinx: http://www.sphinx-doc.org/en/stable/ (autodocumentation)


Installation
============

Building Spatial requires the `spatial` branch of the hyperdsl (https://github.com/stanford-ppl/hyperdsl) project. Hyperdsl is itself composed of three submodules: Forge, Delite, and LMS, all of which also have a `spatial` branch.

To setup hyperdsl, do the following:

    git clone https://github.com/stanford-ppl/hyperdsl.git
    cd hyperdsl
    git checkout spatial
    git submodule update --init

Set the following environment variables, requried for sbt and hyperdsl scripts:

    HYPER_HOME: hyperdsl repository home directory
    LMS_HOME: virtualization-lms-core repository home directory
    DELITE_HOME: Delite repository home directory
    FORGE_HOME: Forge repository home directory
    JAVA_HOME: JDK home directory
    SPATIAL_HOME: Spatial repository home directory

*init-env.sh* in hyperdsl sets the sensible defaults for all of these paths except `JAVA_HOME` for the current session. Add these variables to your login shell's startup script to avoid having to manually set these each session.

Now, clone Spatial into hyperdsl as follows:

    git clone https://github.com/stanford-ppl/spatial.git
    cd spatial && make

The setup script will prompt you for the `SPATIAL_HOME` environment variable. Press enter to use the current directory. To skip this message in the future, set this variable in your login shell's startup script.


**[Optional]** To track the most recent commits relevant to Spatial on each submodule of hyperdsl:

    cd $DELITE_HOME && git checkout spatial
    cd $FORGE_HOME && git checkout spatial
    cd $LMS_HOME && git checkout spatial


Introduction to Spatial
=======================
Now that you have cloned all of the code and set all of your environment variables, let's look at how to write, build, and run your first Spatial app!

### a) Spatial App Structure
All Spatial programs have a few basic components. The following code example shows each of those components.

```scala
// 1. The Scala object which can be compiled and staged
object MyAppCompiler extends SpatialAppCompiler with MyApp

// 2. The SpatialApp trait provides all of the user syntax
trait MyApp extends SpatialApp {

  // 3. This method is called on program startup
  def main() {
    // 4. These are input arguments from the command line
    val n = args(0).to[SInt]
    val b = args(1).to[Flt]

    // 5. Declare some scalar values on the interface between hardware and software
    val in  = ArgIn[SInt]
    val out = ArgOut[SInt]

    // 6. Set input arguments using software values
    setArg(in, n)

    // 7. Declare memories in the accelerator's global memory
    val data = OffChipMem[SInt](in)
    val outputs = OffChipMem[SInt](in)

    // 8. Everything within this scope
    Accel {
      // Hardware accelerator code goes here!
    }

    // 9. Extract results from accelerator
    val scalar = getArg(out)
    val vector = getMem(outputs)

    // 10. Do validation, further processing, etc.
    ...

  }
}
```


Because Spatial is a DSL for programming reconfigurable *hardware*, we will begin with the hardware equivalent of "Hello, World."  In this app, the hardware reads some numeric argument from an off-chip source and then echoes it back to an off-chip destination.

You can find the source code (shown in the next section for convenience) for this app here:

    $SPATIAL_HOME/apps/src/ArgInOutTest.scala


### b) Hello, World!
```scala
object HelloWorldCompiler extends SpatialAppCompiler with HelloWorld
trait HelloWorld extends SpatialApp {
  def main() {
    // Declare SW-HW interface vals
    val x = ArgIn[SInt]
    val y = ArgOut[SInt]
    val N = args(0).to[SInt]

    // Connect SW vals to HW vals
    setArg(x, N)

    // Create HW accelerator
    Accel {
      Pipe { y := x + 4 }
    }

    // Extract results from accelerator
    val result = getArg(y)

    // Create validation checks and debug code
    val gold = N + 4
    println("expected: " + gold)
    println("result: " + result)
  }
}
  ```
Spatial apps are divided into two parts: the portion of code that runs on the host CPU and the portion of code that gets generated as an accelerator.  The entirety of the app exists inside of `Main()`, and the subset of code inside of the `Accel{}` block is the hardware part of the app.

In ArgInOut, we start with three declarations above the `Accel{}` block.  First, we declare x to be an `ArgIn` of type `SInt`.  Then, we declare y to be an `ArgOut` of type `SInt`.  Finally, we declare `N` to be one of the command-line input arguments at run-time by setting it equal to `args(0)`.  We must also explicitly cast this argument to a Spatial type by appending `.as[SInt]`.  In addition to ArgIns and ArgOuts, there is a type, `DRAM`, which represents a 1-D or 2-D array that the accelerator can read from and write to.

Now that we have both a val that represents an ArgIn and another val which reads some value from the command-line at runtime, we must connect the two with `setArg(<HW val>, <SW val>)`.  Similarly, we can connect a DRAM to an array with `setMem(<HW array>, <SW array>)`.

Next, we specify the `Accel{}` block.  In this particular app, we simply want to add the number 4 to whatever input argument is read in.  To do this, we create a `Pipe` that consists of this primitive addition operation and writes the result to an ArgOut register with `:=`.  In later sections, you will learn what other operations and building blocks Spatial exposes to the developer.

After the `Accel{}` block, we return to the host code section of an app that will interact with the result generated by the hardware.  Specifically, we start by assigning the ArgOut register to a SW variable with `getArg(<HW val>)`.  Similarly, we can assign a DRAM to a SW val with `getMem(<HW array>)`.

Finally, we add any debug and validation code to check if the accelerator is performing as expected.  In this example, we compute the result we expect the hardware to give, and then println both this number and the number we actually got.

### b) Compiling, Synthesizing, and Testing

Now that you have a complete Spatial app, you will want to compile and run it.  Currently, there are three available targets; Scala, Dot, and MaxJ.

**NOTE: Any time you change an app, you must remake Spatial with:**

    cd ${SPATIAL_HOME} && make

#### i) Scala
Targetting Scala is the quickest way to simulate your app and test for accuracy.
**Ask David**

#### ii) Dot
Targetting Dot is a good way to create a dataflow graph of your accelerator in GraphViz format.
**Ask Yaqi**

#### iii) MaxJ
Targetting MaxJ will let you generate a MaxJ app that you can either simulate through Maxeler's framework or synthesize and run on a real FPGA.

Run the following commands to make your app:

    cd ${SPATIAL_HOME}/published/Spatial && bin/spatial ArgInOut --maxj --cpp

This will create the directory `out/` in `$SPATIAL_HOME/published/Spatial` that contains the generated code.  You must have the --cpp flag on even to generate MaxJ because this causes Delite to generate all of the necessary host code for the app.

In order to test the MaxJ app, you must copy this `out/` directory to a machine with MaxCompiler 2014.1 or later:

    cp -r $SPATIAL_HOME/published/Spatial/out <maxcompiler_server>

If you want to test the code for functionality, you can force MaxCompiler to generate a CPP simulation of the app.  If you want to generate a bitstream to test the app on a Stratix V, you can get MaxCompiler to synthesize the entire design.  A build for simulation will take time on the order of minutes, while a build for synthesis will take time on the order of hours.  A simulation will ensure functionality but not runtime.

To build for simulation, run the following:

    cd ${SPATIAL_HOME}/published/Spatial/out && make clean sim
    bash run.sh <arguments>

To build for synthesis, run the following:

    cd ${SPATIAL_HOME}/published/Spatial/out && make clean dfe
    bash run_fpga.sh <arguments>


The Spatial Programming Model
=============================
Coming Soon!
**Raghu**


Design Space Exploration with Spatial
=====================================
Coming Soon!
**David**


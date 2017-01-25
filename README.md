Unit: [![Build Status](https://travis-ci.org/mattfel1/UnitTracker.svg?branch=master)](https://github.com/stanford-ppl/spatial/wiki/chiselBranch-chiselTest-Regression-Tests-Status) ----
Dense: [![Build Status](https://travis-ci.org/mattfel1/DenseTracker.svg?branch=master)](https://github.com/stanford-ppl/spatial/wiki/chiselBranch-chiselTest-Regression-Tests-Status) ----
Sparse: [![Build Status](https://travis-ci.org/mattfel1/SparseTracker.svg?branch=master)](https://github.com/stanford-ppl/spatial/wiki/chiselBranch-chiselTest-Regression-Tests-Status) ----
Characterization: [![Build Status](https://travis-ci.org/mattfel1/CharacterizationTracker.svg?branch=master)](https://github.com/stanford-ppl/spatial/wiki/chiselBranch-chiselTest-Regression-Tests-Status)


# Spatial
Spatial is a Forge DSL for programming reconfigurable hardware from a parameterized, high level abstraction.

Prerequisites
=============

* Java JDK: http://www.oracle.com/technetwork/java/javase/downloads/index.html
* Scala SBT: http://www.scala-sbt.org
* **[Optional]** Sphinx: http://www.sphinx-doc.org/en/stable/ (autodocumentation)


Installation
============

Start by setting the following environment variables, required for sbt.  You may need to customize some of them, such as JAVA_HOME. You can either set them in your bashrc or in your current shell:

    export HYPER_HOME = ${HOME}/hyperdsl                        # hyperdsl repository home directory
    export LMS_HOME = ${HYPER_HOME}/virtualization-lms-core     # virtualization-lms-core repository home directory
    export DELITE_HOME = ${HYPER_HOME}/delite                   # Delite repository home directory
    export FORGE_HOME = ${HYPER_HOME}/forge                     # Forge repository home directory
    export JAVA_HOME = /usr/bin/                                # JDK home directory
    export SPATIAL_HOME = ${HYPER_HOME}/spatial                 # Spatial repository home directory

Building Spatial requires the `spatial` branch of the hyperdsl (https://github.com/stanford-ppl/hyperdsl) project. Hyperdsl is itself composed of three submodules: Forge, Delite, and LMS, all of which also have a `spatial` branch.

To setup hyperdsl, do the following:

    cd ${HOME}
    git clone https://github.com/stanford-ppl/hyperdsl.git
    cd hyperdsl
    git checkout spatial
    cd $DELITE_HOME && git checkout spatial
    cd $FORGE_HOME && git checkout spatial
    cd $LMS_HOME && git checkout spatial
    cd $HYPER_HOME

*init-env.sh* in hyperdsl sets the sensible defaults for all of these paths except `JAVA_HOME` for the current session. Add these variables to your login shell's startup script to avoid having to manually set these each session.

Now, clone Spatial into your hyperdsl directory as follows:

    git clone https://github.com/stanford-ppl/spatial.git
    cd spatial && make

**[Optional]** To get the latest version that may not be stable, switch your spatial repository to the `chisel` branch. ([status](https://github.com/stanford-ppl/spatial/wiki/chiselBranch-chiselTest-Regression-Tests-Status)) 



Introduction to Spatial
=======================
Now that you have cloned all of the code and set all of your environment variables, let's look at how to write, build, and run your first Spatial app!

### a) Spatial App Structure
All Spatial programs have a few basic components. The following code example shows each of those components.

```scala
// 0. Imports
import spatial.compiler._
import spatial.library._
import spatial.shared._

// 1. The Scala object which can be compiled and staged
object MyApp extends SpatialAppCompiler with MyAppCompiler

// 2. The SpatialApp trait provides all of the user syntax
trait MyAppCompiler extends SpatialApp {

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
    val data = DRAM[SInt](in)
    val outputs = DRAM[SInt](in)

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
import spatial.compiler._
import spatial.library._
import spatial.shared._

object HelloWorld extends SpatialAppCompiler with HelloWorldCompiler
trait HelloWorldCompiler extends SpatialApp {
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

You should edit and place apps inside of your `${SPATIAL_HOME}/apps/src/` directory.  **Any time you change an app, you must remake Spatial with:**

    cd ${SPATIAL_HOME} && make

Once you have a complete Spatial app, you will want to compile and run it.  Currently, there are four available targets; Scala, Chisel, Dot, and MaxJ.

#### i) Scala
Targetting Scala is the quickest way to simulate your app and test for accuracy.  It also exposes println to code that exists inside the `Accel` block with string interpolation arguments (s"").  You should use this backend if you are debugging things at the algorithm level.  In order to compile and simulate for the Scala backend, run:

    cd ${SPATIAL_HOME}/published/Spatial
    bin/spatial --test <app name> <arguments>

The "<app name>" refers to the name of the `Object`.  The "<arguments>" should be a space-separated list.  This command will automatically create the Scala backend and execute it, ultimately providing you with the results for the app.

#### ii) Dot
Targetting Dot is a good way to create a dataflow graph of your accelerator in GraphViz format.
**Ask Yaqi for more**

#### iii) Chisel
Targetting Chisel will let you compile your app down into Berkeley's chisel language, which eventually compiles down to Verilog.  It also allows you to debug your app at the clock-cycle resolution. In order to compile with the Chisel backend, run the following:

    cd ${SPATIAL_HOME}/published/Spatial
    bin/spatial --chisel <app name>

This will create a folder in `${SPATIAL_HOME}/published/Spatial` called `out` where it dumps all of the generated C++ (hostcode) and chisel (accelerator) files.  You can see the generated code in the `out/chisel/src/kernels/TopModule.scala` file to get a sense of what your Accel compiled into.  In order to compile this generated code into something you can test or target an FPGA with, do the following:
    
    cd out
    make sim
    bash run.sh <arguments>

This will turn the Chisel code into verilog, which then gets turned into C++ through verilator.  It also compiles the Spatial-generated C++.  Finally, the run.sh script executes the entire application with communication between the hardware and CPU and returns the result.

Aditionally, you can see the waveform from the test in the `test_run_dir/app.Launcher####` folder, with the `.vcd` extension. 

In order to get oriented with the state machines in your app, you can look at a generated file in the published directory, `${SPATIAL_HOME}/published/Spatial/controller_tree_<app name>.html`.  If you open it with a browser, you will see expandable boxes that show the control nodes in your application and where they fall in the hierarchy.  From here, you can look at the waveform with some information about how the state machines are connected.  See the next section on **The Spatial Programming Model** for more information about control flow and the philosophy behind Spatial apps.

#### iv) MaxJ
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


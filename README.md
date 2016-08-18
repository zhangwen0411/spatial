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

To setup hyperdsl after cloning:

    cd hyperdsl
    git checkout spatial
    git submodule update --init

Set the following environment variables, requried for sbt and hyperdsl scripts:

    HYPER_HOME: hyperdsl repository home directory
    LMS_HOME: virtualization-lms-core repository home directory
    DELITE_HOME: Delite repository home directory
    FORGE_HOME: Forge repository home directory
    JAVA_HOME: JDK home directory

`init-env.sh` in hyperdsl sets the sensible defaults for all of these paths except JAVA_HOME for the current session. Add these variables to your login shell's startup script to avoid having to manually set these each session.

Now, clone Spatial to the directory of your choice and type:

    cd spatial && make

The setup script will prompt you for the `SPATIAL_HOME` environment variable. Press enter to use the current directory. To skip this message in the future, set this variable in your login shell's startup script.


**[Optional]** To track the most recent commits relevant to Spatial on each submodule of hyperdsl:

    cd $DELITE_HOME && git checkout spatial
    cd $FORGE_HOME && git checkout spatial
    cd $LMS_HOME && git checkout spatial




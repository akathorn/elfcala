# Embedding Logical Framework in Scala

## About

The goal of this project is to define an Embedded Domain Specific Language in
Scala to encode and verify deductive systems, such as programming languages
and logics. The main inspiration comes from the [Twelf
system](http://http://twelf.org/wiki/Main_Page), the most successful
implementation of the Edinburgh Logical Framework.

The code includes a Twelf "backend", that can be used to check signatures by
using Twelf instead of the more limited typechecking provided in Scala.


## Compiling and testing

This project uses [sbt](http://www.scala-sbt.org/), to compile the code run
`sbt compile`, and to test run `sbt test`


## Configuring the Twelf backend

To configure the Twelf backend, the binary `twelf-sever` has to be accessible
in the directory of the project. The recommended way to achieve this is by
creating a symbolic link: from the directory containing this file, run the
command `ln -s /path/to/twelf-server`.


# Refactored-Computing-Machine (is the surprisingly accurate auto-generated name that GitHub gave this)

# Background
This is an interpreter for a Java-C-Ish language, written in Racket. It was created for EECS 345 at CWRU, and with plenty of documentation we're making it public here in the hopes that it can be helpful to anyone else learning Racket or Functional Programming in general. It's released under an MIT license, so go wild.

# Usage
The interpreter is called on a file using the `interpret` function, like `(interpret "program.javacish")`. Examples are given at the top of the `Interpreter3Core.rkt` file.

# Structure
The `Programs` folder contains a suite of example programs used as test cases.
The `Interpreter3State.rkt` file implements functions for manipulating the program's runtime state.
The `functionParser.rkt` and `lex.rkt` files implement all of the text parsing. We did not write them.
The `Interpreter3Core.rkt` file implements the rest of the interpreter, including all of the execution.

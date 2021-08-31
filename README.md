# Language

Take a look at the tests to see examples
of valid and invalid programs.

# Running the compiler

Use the Makefile to compile the compiler.
It's written in Haskell and compiles to
LLVM.
Sample usage:

    ./latc tests/good/core001.lat
    lli tests/good/core001.bc  # run the bytecode
    cat tests/good/core001.ll  # inspect the LLVM IR

# Directory structure

- grammar -- grammar files, in particular the main file with the grammar `Latte.cf`
- src -- compiler code
- src/static_analyser -- compiler frontend
- src/code_generator -- compiler backend

The tests as well as the grammar are based on the [course website](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/latte.html).

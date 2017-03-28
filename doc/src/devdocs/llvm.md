# Working with LLVM

This is not a replacement for the LLVM documentation, but a collection of tips for working on
LLVM for Julia.

## Overview of Julia to LLVM Interface

Julia statically links in LLVM by default. Build with `USE_LLVM_SHLIB=1` to link dynamically.

The code for lowering Julia AST to LLVM IR or interpreting it directly is in directory `src/`.

| File                | Description                                                |
|:------------------- |:---------------------------------------------------------- |
| `builtins.c`        | Builtin functions                                          |
| `ccall.cpp`         | Lowering `ccall`                                           |
| `cgutils.cpp`       | Lowering utilities, notably for array and tuple accesses   |
| `codegen.cpp`       | Top-level of code generation, pass list, lowering builtins |
| `debuginfo.cpp`     | Tracks debug information for JIT code                      |
| `disasm.cpp`        | Handles native object file and JIT code diassembly         |
| `gf.c`              | Generic functions                                          |
| `intrinsics.cpp`    | Lowering intrinsics                                        |
| `llvm-simdloop.cpp` | Custom LLVM pass for `@simd`                               |
| `sys.c`             | I/O and operating system utility functions                 |

Some of the `.cpp` files form a group that compile to a single object.

The difference between an intrinsic and a builtin is that a builtin is a first class function
that can be used like any other Julia function.  An intrinsic can operate only on unboxed data,
and therefore its arguments must be statically typed.

### Alias Analysis

Julia currently uses LLVM's [Type Based Alias Analysis](http://llvm.org/docs/LangRef.html#tbaa-metadata).
To find the comments that document the inclusion relationships, look for `static MDNode*` in
`src/codegen.cpp`.

The `-O` option enables LLVM's [Basic Alias Analysis](http://llvm.org/docs/AliasAnalysis.html#the-basicaa-pass).

## Building Julia with a different version of LLVM

The default version of LLVM is specified in `deps/Versions.make`. You can override it by creating
a file called `Make.user` in the top-level directory and adding a line to it such as:

```
LLVM_VER = 3.5.0
```

Besides the LLVM release numerals, you can also use `LLVM_VER = svn` to bulid against the latest
development version of LLVM.

## Passing options to LLVM

You can pass options to LLVM using *debug* builds of Julia.  To create a debug build, run `make debug`.
 The resulting executable is `usr/bin/julia-debug`. You can pass LLVM options to this executable
via the environment variable `JULIA_LLVM_ARGS`. Here are example settings using `bash` syntax:

  * `export JULIA_LLVM_ARGS = -print-after-all` dumps IR after each pass.
  * `export JULIA_LLVM_ARGS = -debug-only=loop-vectorize` dumps LLVM `DEBUG(...)` diagnostics for
    loop vectorizer *if* you built Julia with `LLVM_ASSERTIONS=1`. Otherwise you will get warnings
    about "Unknown command line argument". Counter-intuitively, building Julia with `LLVM_DEBUG=1`
    is *not* enough to dump `DEBUG` diagnostics from a pass.

## Improving LLVM optimizations for Julia

Improving LLVM code generation usually involves either changing Julia lowering to be more friendly
to LLVM's passes, or improving a pass.

If you are planning to improve a pass, be sure to read the [LLVM developer policy](http://llvm.org/docs/DeveloperPolicy.html).
The best strategy is to create a code example in a form where you can use LLVM's `opt` tool to
study it and the pass of interest in isolation.

1. Create an example Julia code of interest.
2. Use `JULIA_LLVM_ARGS = -print-after-all` to dump the IR.
3. Pick out the IR at the point just before the pass of interest runs.
4. Strip the debug metadata and fix up the TBAA metadata by hand.

The last step is labor intensive.  Suggestions on a better way would be appreciated.

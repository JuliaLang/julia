# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

@AGENTS.md

## Architecture Overview

Julia is a high-level, high-performance dynamic language. The repository contains the runtime, compiler, and standard library all in one tree.

### Boot and Build Process

The system image (`sys.so`/`sys.dylib`) bundles precompiled Base and stdlib code. Building with `make -j` compiles the C/C++ runtime, then runs Julia to create the system image. Changes to `base/` or `stdlib/` are baked into the sysimage at build time - use Revise to test without rebuilding.

### Runtime (`src/`)

Key C/C++ files:
- `julia.h` / `julia_internal.h` - Core type definitions and internal APIs
- `codegen.cpp` - LLVM IR generation from Julia IR; `cgutils.cpp` has codegen helpers
- `gf.c` - Method dispatch (generic functions, method tables, specialization)
- `typemap.c` - Type-based method lookup tables
- `subtype.c` - Subtyping algorithm
- `builtins.c` - Built-in functions (core intrinsics callable from Julia)
- `datatype.c` - DataType layout and construction
- `ast.c` - Frontend parsing (calls into flisp parser in `src/flisp/`)
- `task.c` - Task (coroutine) implementation and switching
- `threading.c` - Multi-threading runtime support
- `gc-stock.c` / `gc-mmtk.c` - Two GC backends behind `gc-interface.h`; stock is the default concurrent mark-sweep collector
- `signal-handling.c` / `signals-*.c` - Platform-specific signal handling
- `staticdata.c` - System image serialization/deserialization
- `aotcompile.cpp` - Ahead-of-time compilation and pkgimage support
- `interpreter.c` - Fallback interpreter for non-compiled code
- `jitlayers.cpp` - JIT compilation pipeline (LLVM ORC)
- `processor_*.cpp` - CPU feature detection and multi-versioning

### Compiler (`Compiler/src/`)

The Julia compiler is a separate module that can be swapped. Key files:
- `abstractinterpretation.jl` - Type inference engine (abstract interpretation)
- `tfuncs.jl` - Transfer functions (type inference rules for builtins)
- `typelattice.jl` / `typelimits.jl` - Type lattice operations and widening
- `optimize.jl` - Optimization passes on Julia IR
- `ssair/` - SSA-form intermediate representation

### Base Library (`base/`)

`base/boot.jl` defines the lowest-level types and primitives (bootstrapped by the runtime). `base/Base.jl` is the main entry point that includes everything else. The include order matters - files are loaded sequentially during sysimage build.

### Test Organization (`test/`)

Test files mirror base module names (e.g., `test/arrayops.jl` tests array operations). `test/choosetests.jl` defines the test suite groupings used by CI.

## Common Build and Test Commands

```bash
make -j                          # Build Julia (needed after src/ changes)
make debug                       # Build debug version (usr/bin/julia-debug)
make test                        # Run default test suite
make testall                     # Run all tests
make test-arrayops               # Run a specific test file
make test-revise-arrayops        # Run test using Revise (no rebuild needed for base/ changes)
JULIA_TEST_FAILFAST=1 make test-revise-foo  # Fail fast on first error
make -C src analyze-gf           # Run static analysis on src/gf.c
make -C doc doctest=true revise=true        # Run doctests
make fix-whitespace              # Fix whitespace issues before committing
```

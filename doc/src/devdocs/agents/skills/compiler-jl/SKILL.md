---
name: compiler-jl
description: Develop and test Julia's Compiler.jl. Use when modifying Compiler/, debugging compiler behavior, or activating stdlib Compiler for reflection/native-codegen experiments instead of using sysimage Base.Compiler.
---

# Compiler.jl development

Use this when modifying `Compiler/` source, tests, or package metadata.

## Compiler modules in Julia

During bootstrap, `Compiler/` is built into the sysimage as `Base.Compiler`.
That in-sysimage compiler is what Julia normally uses for native compilation.
Because it is baked into the sysimage, changes made to `Compiler/` after the
sysimage was built are not reflected in `Base.Compiler` unless Julia is rebuilt.

`Compiler/` can also be loaded as a stdlib package, producing a separate
`Compiler` module from `Base.Compiler`. For day-to-day development, testing, and
debugging of post-sysimage source changes, prefer this stdlib `Compiler` module
over trying to replace `Base.Compiler` with Revise. It is usually reliable except
for failures caused by compatibility with the `src/` runtime or other
sysimage/native-codegen integration issues.

When investigating a runtime compatibility issue, `make -C src` can update the
runtime faster than rebuilding the full Julia sysimage. Rebuild Julia when the
updated `Compiler/` code needs to be baked into `Base.Compiler` or when checking
sysimage/native-codegen integration.

## Experimenting with Compiler as a stdlib

Run the in-tree Julia runtime with the `Compiler` project:

```sh
./usr/bin/julia --project=Compiler
```

### Reflection backend

For reflection-oriented debugging, activate the stdlib `Compiler` module:

```julia
using InteractiveUtils, Compiler
@activate Compiler
```

`@activate Compiler` is equivalent to `@activate Compiler[:reflection]`: Base and
InteractiveUtils reflection utilities such as `code_typed`, `@code_typed`, and
`code_warntype` will use the loaded stdlib `Compiler` module. This is useful for
trying compiler changes without rebuilding the sysimage.

### Native compilation backend

If you specifically need to exercise the loaded stdlib `Compiler` as the native
compilation backend, use:

```julia
using InteractiveUtils, Compiler
@activate Compiler[:codegen]
```

This is broader than reflection activation: it makes the runtime invoke the
loaded compiler for compilation requests. Use it only when debugging native
compilation/codegen behavior, and expect failures here to involve sysimage,
runtime, native-codegen, or compiler-cache interactions.

For ordinary debugging, prefer reflection activation. Use `[:codegen]` only when
you specifically need to test the native compilation backend; for final
integration checks, rebuild Julia and test the in-sysimage `Base.Compiler`.

## Testing Compiler as a stdlib package

Run package tests from the repository root with the in-tree Julia runtime:

```sh
./usr/bin/julia --project=Compiler -e 'using Pkg; Pkg.test()'
```

To run a single test file, include it directly under a `Test.@testset`:

```sh
./usr/bin/julia --project=Compiler -e 'using Test; @testset "inline" include("Compiler/test/inline.jl")'
```

Most `Compiler/test/` files include `Compiler/test/setup_Compiler.jl`, which
activates or imports the appropriate `Compiler` module for this workflow.

## Testing `Base.Compiler`

Use the top-level test entry point when the goal is to test the `Base.Compiler`
that is baked into the sysimage:

```sh
./usr/bin/julia test/runtests.jl Compiler
```

If you changed `Compiler/` and need those changes reflected in `Base.Compiler`,
rebuild Julia first so the sysimage contains the updated compiler code.

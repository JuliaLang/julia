# System Image Building

## [Building the Julia system image](@id Building-the-Julia-system-image)

Julia ships with a preparsed system image containing the contents of the `Base` module, named
`sys.ji`. This file is also precompiled into a shared library called `sys.{so,dll,dylib}` on
as many platforms as possible, so as to give vastly improved startup times. On systems that do
not ship with a precompiled system image file, one can be generated from the source files shipped
in Julia's `DATAROOTDIR/julia/base` folder.

This operation is useful for multiple reasons. A user may:

  * Build a precompiled shared library system image on a platform that did not ship with one, thereby
    improving startup times.
  * Modify `Base`, rebuild the system image and use the new `Base` next time Julia is started.
  * Build a system image with packages embedded into the startup environment, by running a script
    that loads them on top of an existing system image with `--output-o`.

The [`PackageCompiler.jl` package](https://github.com/JuliaLang/PackageCompiler.jl) contains convenient
wrapper functions to automate this process.

## Coverage instrumentation

Set `JULIA_COVERAGE_IMAGES=1` in `Make.user` before building Julia to compile
`hit`-mode coverage counters into the system image and the bundled package
images. Coverage runs in `hit` mode then reuse the native code instead of
recompiling it in each process. This configuration is intended for coverage
CI: the images retain instrumentation overhead even when coverage is disabled,
and cached inference effects can differ from those of an ordinary build.

Like `--check-bounds`, coverage uses the system image as built, without
invalidating its native code at startup. For newly compiled and interpreted
code, `@<path>` uses the same instrumentation policy as `user`: Base and Core
code are excluded. The path filters reports, not compilation. Compatible image
counters can still report Base and Core locations under the selected path;
relative Base filenames are resolved against the installed Base source directory.
Complete coverage of Base requires suitable instrumented images and `all` for
newly compiled Base code.
An instrumented image carries counters for every statement, and the requested
scope selects which of them contribute to the report. The counters contribute
only when their mode can serve the request: count counters can serve hit mode,
but hit counters cannot serve count mode. An ordinary system image has no
counters. Package images are selected separately by their instrumentation flags.
Allocation tracking retains its per-method recompilation fallback because
images carry no allocation counters. Image generation does not record coverage
hits for its own precompile workload.

## [System image optimized for multiple microarchitectures](@id sysimg-multi-versioning)

The system image can be compiled simultaneously for multiple CPU microarchitectures
under the same instruction set architecture (ISA). Multiple versions of the same function
may be created with minimal dispatch points inserted into shared functions
in order to take advantage of different ISA extensions or other microarchitecture features.
The version that offers the best performance will be selected automatically at runtime
based on available CPU features.

### Specifying multiple system image targets

A multi-microarchitecture system image can be enabled by passing multiple targets
during system image compilation. This can be done either with the [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) make option
or with the `-C` command line option when running the compilation command manually.
Multiple targets are separated by `;` in the option string.
The syntax for each target is a CPU name followed by multiple features separated by `,`.
All features supported by LLVM are supported and a feature can be disabled with a `-` prefix.
(`+` prefix is also allowed and ignored to be consistent with LLVM syntax).
Additionally, a few special features are supported to control the function cloning behavior.

!!! note
    It is good practice to specify either `clone_all` or `base(<n>)` for every target apart from the first one. This makes it explicit which targets have all functions cloned, and which targets are based on other targets. If this is not done, the default behavior is to not clone every function, and to use the first target's function definition as the fallback when not cloning a function.

1. `clone_all`

    By default, only functions that are the most likely to benefit from
    the microarchitecture features will be cloned.
    When `clone_all` is specified for a target, however,
    **all** functions in the system image will be cloned for the target.
    The negative form `-clone_all` can be used to prevent the built-in
    heuristic from cloning all functions.

2. `base(<n>)`

    Where `<n>` is a placeholder for a non-negative number (e.g. `base(0)`, `base(1)`).
    By default, a partially cloned (i.e. not `clone_all`) target will use functions
    from the default target (first one specified) if a function is not cloned.
    This behavior can be changed by specifying a different base with the `base(<n>)` option.
    The `n`th target (0-based) will be used as the base target instead of the default (`0`th) one.
    The base target has to be either `0` or another `clone_all` target.
    Specifying a non-`clone_all` target as the base target will cause an error.

3. `opt_size`

    This causes the function for the target to be optimized for size when there isn't a significant
    runtime performance impact. This corresponds to `-Os` GCC and Clang option.

4. `min_size`

    This causes the function for the target to be optimized for size that might have
    a significant runtime performance impact. This corresponds to `-Oz` Clang option.

As an example, at the time of this writing, the following string is used in the creation of
the official `x86_64` Julia binaries downloadable from julialang.org:

```
generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1);x86-64-v4,-rdrnd,base(1)
```

This creates a system image with four separate targets; one for a generic `x86_64`
processor, one with a `sandybridge` ISA (explicitly excluding `xsaveopt`) that explicitly
clones all functions, and two targeting the `haswell` and `x86-64-v4` ISAs, based off of
the `sandybridge` version, both excluding `rdrnd`. When a Julia implementation loads the
generated sysimg, it will check the host processor for matching CPU capability flags,
enabling the highest ISA level possible. Note that the base level (`generic`) requires
the `cx16` instruction, which is disabled in some virtualization software and must be
enabled for the `generic` target to be loaded. Alternatively, a sysimg could be generated
with the target `generic,-cx16` for greater compatibility, however note that this may cause
performance and stability problems in some code.

### Implementation overview

CPU and feature tables come from the [cpufeatures](https://github.com/JuliaLang/cpufeatures)
library. A build-time generator extracts tables from LLVM's TableGen data and emits
standalone C headers (`generated/target_tables_{x86_64,aarch64,riscv64}.h`).

The runtime library (`libtarget_parsing`) handles target string parsing, host CPU
detection, and serialization.

  - `src/processor.cpp`: parses target strings, detects the host, resolves targets,
    and serializes/deserializes target data for system images.
  - `src/llvm-multiversioning.cpp`: clones functions based on target feature diffs.
    Functions with loops or CPU queries are always cloned; SIMD, math, float16, and
    bfloat16 functions are cloned when the target adds those capabilities.

### Upgrading LLVM

When Julia's LLVM version changes, regenerate the cpufeatures headers:

```
cd cpufeatures
cmake -B build -DCMAKE_PREFIX_PATH=<path-to-new-llvm>
cmake --build build --target gen_target_tables
build/gen_target_tables x86_64-linux-gnu > generated/target_tables_x86_64.h
build/gen_target_tables aarch64-linux-gnu > generated/target_tables_aarch64.h
build/gen_target_tables riscv64-linux-gnu > generated/target_tables_riscv64.h
```

Commit the updated headers and update Julia's cpufeatures dependency. A
`static_assert` in `src/processor.cpp` checks `TARGET_TABLES_LLVM_VERSION_MAJOR`
against Julia's LLVM version, so mismatches are caught at build time.

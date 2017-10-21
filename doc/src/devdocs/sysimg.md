# System Image Building

## Building the Julia system image

Julia ships with a preparsed system image containing the contents of the `Base` module, named
`sys.ji`.  This file is also precompiled into a shared library called `sys.{so,dll,dylib}` on
as many platforms as possible, so as to give vastly improved startup times.  On systems that do
not ship with a precompiled system image file, one can be generated from the source files shipped
in Julia's `DATAROOTDIR/julia/base` folder.

This operation is useful for multiple reasons.  A user may:

  * Build a precompiled shared library system image on a platform that did not ship with one, thereby
    improving startup times.
  * Modify `Base`, rebuild the system image and use the new `Base` next time Julia is started.
  * Include a `userimg.jl` file that includes packages into the system image, thereby creating a system
    image that has packages embedded into the startup environment.

Julia now ships with a script that automates the tasks of building the system image, wittingly
named `build_sysimg.jl` that lives in `DATAROOTDIR/julia/`.  That is, to include it into a current
Julia session, type:

```julia
include(joinpath(JULIA_HOME, Base.DATAROOTDIR, "julia", "build_sysimg.jl"))
```

This will include a `build_sysimg` function:

```@docs
BuildSysImg.build_sysimg
```

Note that this file can also be run as a script itself, with command line arguments taking the
place of arguments passed to the `build_sysimg` function.  For example, to build a system image
in `/tmp/sys.{so,dll,dylib}`, with the `core2` CPU instruction set, a user image of `~/userimg.jl`
and `force` set to `true`, one would execute:

```
julia build_sysimg.jl /tmp/sys core2 ~/userimg.jl --force
```

## System image optimized for multiple microarchitectures

The system image can be compiled simultaneously for multiple CPU microarchitectures
under the same instruction set architecture (ISA). Multiple versions of the same function
may be created with minimum dispatch point inserted into shared functions
in order to take advantage of different ISA extensions or other microarchitecture features.
The version that offers the best performance will be selected automatically at runtime
based on available features.

### Specifying multiple system image targets

Multi-microarch system image can be enabled by passing multiple targets
during system image compilation. This can be done either with the `JULIA_CPU_TARGET` make option
or with the `-C` command line option when running the compilation command manually.
Multiple targets are separated by `;` in the option.
The syntax for each target is a CPU name followed by multiple features separated by `,`.
All features supported by LLVM is supported and a feature can be disabled with a `-` prefix.
(`+` prefix is also allowed and ignored to be consistent with LLVM syntax).
Additionally, two special features are supported to control the function cloning behavior.

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
    Specifying a non default `clone_all` target as the base target will cause an error.

### Implementation overview

This is a brief overview of different part involved in the implementation.
See code comments for each components for more implementation details.

1. System image compilation

    The parsing and cloning decision are done in `src/processor*`.
    We currently support cloning of function based on the present of loops, simd instructions,
    or other math operations (e.g. fastmath, fma, muladd).
    This information is passed on to `src/llvm-multiversioning.cpp` which does the actual cloning.
    In addition to doing the cloning and insert dispatch slots
    (see comments in `MultiVersioning::runOnModule` for how this is done),
    the pass also generates metadata so that the runtime can load and initialize the
    system image correctly.
    A detail description of the metadata is available in `src/processor.h`.

2. System image loading

    The loading and initialization of the system image is done in `src/processor*` by
    parsing the metadata saved during system image generation.
    Host feature detection and selection decision are done in `src/processor_*.cpp`
    depending on the ISA. The target selection will prefer exact CPU name match,
    larger vector register size, and larget number of features.
    An overview of this process is in `src/processor.cpp`.

# Profiling package precompilation

Most of the work of precompiling a package happens in a short-lived worker process that the
precompilation driver spawns, not in the session you typed `Pkg.precompile()` into. Profiling
that session therefore shows you the driver waiting on its children and nothing about where
the time actually went. This page describes how to sample the workers themselves with the
[`Profile`](@ref lib-profiling) stdlib.

For zone-level timing of the same processes, such as how long inference or image generation
took as labelled regions rather than as sampled stacks, see
[Profiling package precompilation with Tracy](@ref).

## Collecting a profile

The driver can turn on sampling for the workers it spawns:

```julia
using Base.Precompilation
precompilepkgs(; profile = true)                       # every package
precompilepkgs(; profile = "Makie")                    # just this one
precompilepkgs(; profile = ["Makie", "CairoMakie"])    # or these
precompilepkgs(; profile = true, profile_dir = "/tmp/profiles")
```

Each selected worker writes two files into the profile directory, named after the package and
the worker's process id. The directory defaults to a fresh temporary one and is printed when
the run starts.

The same thing can be driven from the environment, which is useful when the precompilation is
started by something other than a direct `precompilepkgs` call, such as `Pkg.precompile()`,
`Pkg.add`, or a package being loaded for the first time:

```
JULIA_PRECOMPILE_PROFILE=/tmp/profiles julia --project -e 'using Pkg; Pkg.precompile()'
```

Every worker spawned under that variable profiles itself, so this is the blunt instrument;
the `profile` keyword above is usually what you want.

[`JULIA_PRECOMPILE_PROFILE_DELAY`](@ref JULIA_PRECOMPILE_PROFILE_DELAY) and
[`JULIA_PRECOMPILE_PROFILE_NSAMPLES`](@ref JULIA_PRECOMPILE_PROFILE_NSAMPLES) set the sampling
period in seconds and the buffer size in samples. They default to the same values as
`Profile.init`, a 1 ms period and a 10 million entry buffer, which is about 80 MB per worker.
A warning is printed if the buffer fills.

## Reading a profile

The worker writes `<pkg>-<pid>.profdata`, the raw sample buffer, and `<pkg>-<pid>.profsyms`,
the stack frames its instruction pointers resolve to. The two are separate because an
instruction pointer only means something in the process that recorded it, so the worker has to
resolve its own symbols, while everything else is better done later in a session that can load
`Profile`.

`contrib/read_precompile_profile.jl` turns the pair back into the arguments the `Profile`
reporting functions take:

```julia
include("contrib/read_precompile_profile.jl")
data, lidict = read_precompile_profile("/tmp/profiles/Makie-64321")
Profile.print(stdout, data, lidict; C = true, format = :flat, sortedby = :count)
```

Keep `C = true`. A precompile worker spends much of its time inside the runtime and LLVM, and
those frames disappear from the report without it.

The same `data` and `lidict` can be handed to any package that consumes `Profile` output, such
as `ProfileView`, `PProf` or `FlameGraphs`, which is usually an easier way to read a worker's
profile than the textual tree.

## What the profile covers

Sampling starts when the worker begins loading the package's dependencies and stops after the
package image has been written, so it covers dependency loading, lowering, type inference,
code generation, and image generation. Two things are outside it:

  * The driver process, including the dependency resolution and the linker and `dsymutil`
    invocations that run after the worker exits.
  * Workers for packages that were already up to date, which are never spawned.

If a package is precompiled with pkgimages disabled, the profile stops at the end of `include`
instead, because the hook it dumps from only runs when native code is emitted.

## Caveats

  * Resolving symbols happens in the worker after sampling stops, and shows up in the worker's
    reported total time. Wall-clock numbers from a profiled run are not comparable with an
    unprofiled one; take timings separately.
  * The buffer is allocated per worker, so profiling every package in a large environment with
    many parallel workers costs memory. Prefer naming the packages you care about.
  * Sampling perturbs what it measures. Treat the shape of the profile as the result, not the
    absolute times.

# Optimized Julia builds

This directory builds Julia with profile-guided optimization (PGO), ThinLTO and,
where supported, BOLT. It is what the nightly `*opt` builds use.

The build runs in stages:

0. a stage-0 toolchain (clang, lld and the LLVM tools, downloaded from
   BinaryBuilder; BOLT is built from source, as BinaryBuilder has none),
1. an instrumented Julia, whose system image and package image builds are the
   workload that the PGO profile is collected from,
2. the optimized Julia, built with that profile and ThinLTO, and
3. with BOLT: an instrumentation, training and rewriting pass over `libLLVM`,
   `libjulia-internal` and `libjulia-codegen`.

## Building

```bash
make -C contrib/optimized all
```

That runs the complete flow and leaves the result in `optimized.build`. It takes
a while: LLVM is built twice and Julia two or three times.

The stages are also available individually, mainly to collect profile data from a
workload of your own in between (`cd` into this directory first):

```bash
make stage1                       # instrumented build; profiles land in profiles/
./pgo-instrumented.build/julia my-workload.jl
make top                          # top 50 functions of the merged profile
make stage2                       # optimized build
make bolt-originals bolt-instrument
./optimized.build/julia my-workload.jl
make bolt-merge bolt              # rewrite the libraries
```

`make clean-profiles` drops the collected profiles, e.g. to profile only your own
workload rather than the build of Julia itself. `make clean` removes the stage
stamps so that the next build redoes the stages incrementally.

`make delete-originals` removes the pre-BOLT copies of the rewritten libraries,
and `make restore-originals` puts them back in place, undoing the rewrite.

## Knobs

| Variable            | Default                               | Meaning                                      |
| ------------------- | ------------------------------------- | -------------------------------------------- |
| `USE_BOLT`          | 1 on Linux x86-64 and AArch64, else 0 | run the BOLT stages                          |
| `USE_PGO`           | 1                                     | build and profile stage 1, optimize with it  |
| `USE_LTO`           | 1                                     | build stage 2 with ThinLTO                   |
| `STAGE1_CPU_TARGET` | `generic`                             | `JULIA_CPU_TARGET` of the instrumented build |
| `STAGE0_BUILD`      | `toolchain`                           | stage-0 build directory                      |
| `STAGE1_BUILD`      | `pgo-instrumented.build`              | stage-1 build directory                      |
| `STAGE2_BUILD`      | `optimized.build`                     | stage-2 build directory                      |

Other variables are passed through to the staged builds, so
`make all JULIA_CPU_TARGET=...` works as usual. CI points `STAGE2_BUILD` at the
checkout, so that the optimized tree is the one it packages, and asks
`make print-profile-artifacts` for the profile data to archive.

## Platform support

PGO and ThinLTO work wherever the stage-0 toolchain does. BOLT only rewrites ELF
binaries, and only for x86-64 and AArch64, so it is off by default elsewhere: the
macOS and Windows builds are PGO+LTO only.

DO NOT STRIP THE RESULTING SHARED LIBRARIES when BOLT was used,
<https://github.com/llvm/llvm-project/issues/56738>. If you really need to, try
adding `-use-gnu-stack` to `BOLT_ARGS`.

The BOLT rewrite does not align code for huge pages, since the regular build does
not either; that keeps the shared libraries a few MB smaller.

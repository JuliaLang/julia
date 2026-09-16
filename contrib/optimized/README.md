# Optimized Julia builds

This directory builds Julia with profile-guided optimization (PGO), ThinLTO and,
where supported, BOLT. It replaces `contrib/pgo-lto`, `contrib/bolt` and
`contrib/pgo-lto-bolt`.

```sh
make -C contrib/optimized -j8 all
```

The result is in `contrib/optimized/optimized.build`. The build first downloads
clang, lld and LLVM tools from BinaryBuilder, and builds BOLT from source if
needed. It then builds an instrumented Julia, using the system image and package
image builds as the PGO workload, and builds Julia again with the resulting
profile and ThinLTO. With BOLT enabled, it also instruments `libLLVM`,
`libjulia-internal` and `libjulia-codegen`, builds the system image and package
images against them, and rewrites the libraries using that profile.

## Options

| Variable | Default | Meaning |
| --- | --- | --- |
| `USE_BOLT` | 1 on Linux x86-64 and AArch64, else 0 | Run the BOLT stages |
| `USE_PGO` | 1 | Build and profile stage 1, then optimize with it |
| `USE_LTO` | 1 | Build stage 2 with ThinLTO |
| `LTO_JOBS` | 8 on 32-bit targets, else the linker's default | ThinLTO backend threads per link (Linux only) |
| `STAGE1_CPU_TARGET` | `generic` | CPU target of the instrumented build |
| `STAGE0_BUILD` | `$(CURDIR)/toolchain` | Toolchain build directory |
| `STAGE1_BUILD` | `$(CURDIR)/pgo-instrumented.build` | Instrumented build directory |
| `STAGE2_BUILD` | `$(CURDIR)/optimized.build` | Optimized build directory |

For PGO+LTO alone, set `USE_BOLT=0`. For BOLT alone, set `USE_PGO=0 USE_LTO=0`;
this skips stage 1. Command-line build variables are passed to the staged builds,
including `JULIA_CPU_TARGET` for stage 2. Use absolute paths when overriding the
build directories. `STAGE2_BUILD` can also point at the source checkout for
packaging there. `make print-profile-artifacts` prints the profile globs to
archive, relative to the source root.

Stages use stamp files to resume completed builds. They do not track changes to
options: use fresh build directories and remove the stamps with `make clean`
when changing optimization settings. `make clean` only removes stamps and the
merged PGO profile; it does not clean compiled objects or undo a BOLT rewrite.

## Custom workloads

Run the stages separately to collect additional profile data. From this directory,
with PGO enabled:

```sh
make stage1
# Optional: make clean-pgo-profiles to discard the build's PGO workload.
./pgo-instrumented.build/julia my-workload.jl
make top                          # inspect the merged PGO profile
make stage2
```

If BOLT is enabled, stage 2 stops after building the libraries. Complete the
training build before running your own workload:

```sh
make bolt-train
# Optional: make clean-bolt-profiles to discard the build's BOLT workload.
./optimized.build/julia my-workload.jl
make bolt                         # merge profiles and rewrite the libraries
```

Collect BOLT profiles before the final rewrite, while the libraries are still
instrumented. The separate cleanup targets preserve the other optimization's
profiles; `make clean-profiles` clears both. New or updated raw profiles cause
the corresponding merge to run again.

`make restore-originals` restores the libraries saved before BOLT and preserves
their mtimes. A subsequent `make bolt` reapplies the rewrite. Once satisfied,
`make delete-originals` removes those saved copies before packaging; restoring
or rewriting again then requires rebuilding the libraries.

## Platforms

The flow supports PGO+ThinLTO on macOS and Windows x86-64. On Windows it uses
BinaryBuilder's Clang and lld's MinGW driver with an MSYS2 mingw64 sysroot.
Stage 0 places the matching support DLLs beside the tools; the instrumented
stage supplies Clang's profile runtime to Julia's direct linker invocations.
The runtime export map is applied through a COFF export definition. A discovery
link identifies exports from the objects and archive members actually used;
the final link exports the matching names and explicitly exported symbols.

Windows i686 optimized builds are not supported. Its native BinaryBuilder
toolchain is 32-bit, making address space a constraint for ThinLTO links of
libLLVM; supporting it would require a separate cross-toolchain setup.
BOLT defaults to Linux x86-64 and AArch64, where it can rewrite ELF libraries.
It cannot rewrite Windows PE/COFF binaries. An optimization setting does not
replace validation on the target platform.

Do not strip shared libraries rewritten by BOLT; see
<https://github.com/llvm/llvm-project/issues/56738>. The rewrite uses regular page
alignment, as does the normal Julia build.

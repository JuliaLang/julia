# Julia + MMTk

There has been quite a lot of effort to refactor the GC code inside Julia to support external GCs. The first step to enable using different GC algorithms for Julia was the design and implementation of a [GC interface](https://docs.google.com/document/d/1v0jtSrIpdEDNOxj5S9g1jPqSpuAkNWhr_T8ToFC9RLI/edit?usp=sharing). To drive that interface, we added support for building Julia with [MMTk](https://www.mmtk.io) (Memory Management Toolkit). Using Julia + MMTk enables testing different GC implementations, allowing developers to choose a specific implementation when building Julia from source. The connection between Julia and MMTk is done via a *binding*, which links the language runtime with MMTk core. The mmtk-julia binding is written in Rust and can be found in [this repository](https://github.com/mmtk/mmtk-julia).

> [!NOTE]
> Using a different GC requires building Julia from source. It is not possible to switch implementations at runtime. To see what version of the GC is currently being used, run `versioninfo()` from the Julia REPL and it should show the version under `GC: ...`.

## Building Julia with MMTk

There are 3 different ways of building Julia with MMTk: building from source using a fixed release of the binding, checking out a custom version in the mmtk-julia [repository](https://github.com/mmtk/mmtk-julia) or using a precompiled binary from Julia's BinaryBuilder. The easiest way is to use the BinaryBuilder binary. First, to enable MMTk as a third-party GC, set the variable `WITH_THIRD_PARTY_GC` to `mmtk`. Then, for example, to use the Immix as the GC, simply set the variable `MMTK_PLAN=Immix` and build Julia as usual.

There are different configurations supported by the following variables, which can be set in a `Make.user` file or as an environment variable. Note that at this time, setting `MMTK_PLAN=StickyImmix` (to use a generational version of Immix) or `MMTK_MOVING=1` (to enable object movement) will likely cause segmentation faults or other build failures, since we have not added support for these configurations yet. Setting `MMTK_BUILD=debug` will force a debug build of the binding, which will print some logging information that can be used to find errors that are specific to MMTk.

| Variable      |       |        |
|---------------|--------------|---------------|
| `MMTK_PLAN`     | Immix        | StickyImmix   |
| `MMTK_MOVING`   | 0            | 1             |
| `MMTK_BUILD`    | release      | debug         |

Note that when setting only `MMTK_PLAN`, then the default is to do a non-moving, release build.

### Building mmtk-julia from source

It is also possible to build the binding from source. To do so, set the variable `USE_BINARYBUILDER_MMTK_JULIA=0` and the latest release version of the binding will be downloaded and built as part of building Julia. Note that this requires an installation of the rust toolchain.

It is also possible to build a custom version of binding by checking it out from the [git repository](https://github.com/mmtk/mmtk-julia) and setting a variable named `MMTK_JULIA_DIR` as the path that contains the binding.

For more information on building Julia with MMTk, please refer to the [README](https://github.com/mmtk/mmtk-julia/blob/master/README.md) file in the binding repo.

### I've got a build error when building Julia with MMTk, what should I do?

If you try to build Julia with MMTk and get an error it is likely due to a change to Julia that has not been yet propagated to the binding or to the code in Julia that is specific to MMTk. Some changes include:

(1) **Changing the memory layout of objects in Julia**. The binding relies on automatically generated Rust FFI bindings from Julia code. These files are generated using a crate named [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen). To regenerate those files, check out the latest version of the `mmtk-julia` binding, set the variable `JULIA_PATH` to the path of the Julia version you are trying to build and run `make regen-bindgen-ffi` from the directory containing the binding. This should delete the current version of the FFI bindings and generate a new version based on the Julia code from `JULIA_PATH`.

(2) **Changing the root objects passed to the GC**. Julia passes a set of objects to the GC as roots in the function [gc_mark_roots](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2846). At the moment, this set needs to be consistent between both the Stock GC and MMTk (in the function [`jl_gc_scan_vm_specific_roots`](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-mmtk.c#L496)).

(3) **Changing how objects are scanned**. MMTk uses the same strategy to find references in Julia objects as the stock GC (see [gc_mark_outrefs](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2227C19-L2227C34)). Changing the logic from this function should be reflected in the Rust code in the binding that [scan Julia objects](https://github.com/mmtk/mmtk-julia/blob/c9e046baf3a0d52fe75d6c8b28f6afd69b045d95/mmtk/src/julia_scanning.rs#L68).

If your case is not included in one of the alternatives above, please create an issue in the Julia repository tagging it with the `GC: MMTK` label.

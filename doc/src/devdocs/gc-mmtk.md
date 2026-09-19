# Julia + MMTk

There has been quite a lot of effort to refactor the GC code inside Julia to support external GCs. The first step to enable using different GC algorithms for Julia was the design and implementation of a [GC interface](https://docs.google.com/document/d/1v0jtSrIpdEDNOxj5S9g1jPqSpuAkNWhr_T8ToFC9RLI/edit?usp=sharing). To drive that interface, we added support for building Julia with [MMTk](https://www.mmtk.io) (Memory Management Toolkit). Using Julia + MMTk enables testing different GC implementations, allowing developers to choose a specific implementation when building Julia from source. The connection between Julia and MMTk is done via a *binding*, which links the language runtime with MMTk core. The MMTk integration lives in `src/gc-mmtk`, and the Rust binding lives in `src/gc-mmtk/mmtk_julia`.

> [!NOTE]
> Using a different GC requires building Julia from source. It is not possible to switch implementations at runtime. To see what version of the GC is currently being used, run `versioninfo()` from the Julia REPL and it should show the version under `GC: ...`.

## Building Julia with MMTk

Julia builds MMTk from the in-tree integration in `src/gc-mmtk`. To enable MMTk as a third-party GC, set the variable `WITH_THIRD_PARTY_GC` to `mmtk`. Then, for example, to use Immix as the GC, set `MMTK_PLAN=Immix` and build Julia as usual.

There are different configurations supported by the following variables, which can be set in a `Make.user` file or as an environment variable. Setting `MMTK_BUILD_MODE=debug` will force a debug build of the binding, which will print some logging information that can be used to find errors that are specific to MMTk.

| Variable          | Default      | Alternative           |
|-------------------|--------------|-----------------------|
| `MMTK_PLAN`       | StickyImmix  | Immix                 |
| `MMTK_BUILD_MODE` | release      | debug                 |
| `MMTK_MOVING`     | 0            | 1 (not supported yet) |

Note that when setting only `MMTK_PLAN`, then the default is to do a non-moving (`MMTK_MOVING=0`), release (`MMTK_BUILD_MODE=release`) build.

The mmtk-julia binding is built from source as part of the Julia build. This requires an installation of the Rust toolchain.

### I've got a build error when building Julia with MMTk, what should I do?

If you try to build Julia with MMTk and get an error it is likely due to a change to Julia that has not been yet propagated to the binding or to the code in Julia that is specific to MMTk. Some changes include:

(1) **Changing the root objects passed to the GC**. Julia passes a set of objects to the GC as roots in the function [gc_mark_roots](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2846). At the moment, this set needs to be consistent between both the Stock GC and MMTk (in the function `jl_gc_scan_vm_specific_roots` in `src/gc-mmtk/gc-mmtk.c`).

(2) **Changing how objects are scanned**. MMTk uses the same strategy to find references in Julia objects as the stock GC (see [gc_mark_outrefs](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2227C19-L2227C34)). Changing the logic from this function should be reflected in the Rust function `scan_julia_object` in `src/gc-mmtk/mmtk_julia/src/julia_scanning.rs`.

If your case is not included in one of the alternatives above, please create an issue in the Julia repository tagging it with the `GC: MMTK` label.

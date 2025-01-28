# Julia Garbage Collector (GC) Internals

## Introduction

Julia implements a garbage collector (GC) to automate dynamic memory management. Julia's GC is:

- **Mark-sweep**: the object graph is traced starting from a root-set (e.g., global variables and local variables on the stack) to determine the set of live objects.
- **Non-moving**: objects are not relocated to a different memory address.
- **Parallel**: multiple threads can be used during the marking and sweeping phases.
- **Partially concurrent**: the runtime provides an option to scavenge pool-allocated memory blocks (e.g., call `madvise` on these blocks on Linux) concurrently with Julia user code.
- **Generational**: objects are partitioned into generations according to how many collection cycles they've survived. Younger generations are collected more often.
- **Mostly precise**: Julia optionally supports conservative stack scanning for users who inter-operate with foreign languages like C.

## Allocation

Julia uses two types of allocators, depending on the size of the allocation request.

### Small Object Allocation

Sufficiently small objects, up to 2k bytes, are allocated through a per-thread free-list pool allocator.

Julia's pool allocator often has better runtime performance than `libc` `malloc` for small allocations. Additionally, using a custom pool allocator enables a few optimizations during the sweeping phase (e.g., concurrent scavenging).

The pool allocator segregates objects on different size classes. Each large memory block (16k bytes) managed by the pool allocator only contains objects belonging to the same size class.

Each pool-allocated memory block is paired with a metadata structure containing information such as whether the block has live objects at all, the number of free memory slots in the block, the offsets to the first and last objects in the block, etc. This metadata is used to aggregate statistics such as number of objects freed during a collection cycle. It's also used to optimize the sweeping phase of the GC: blocks that have no live objects whatsoever don't need to be linearly scanned during the sweeping phase.

Julia's pool allocator stores memory blocks into different global lock-free lists depending on whether the block has been mapped but never accessed (`page_pool_clean`),  whether the page has been lazily swept and it's waiting to be scavenged by a background GC thread (`page_pool_lazily_freed`), or whether the page has been scavenged (`page_pool_freed`).

 The pool allocator uses this partitioning of blocks to implement a tiered allocation discipline. When it requests a fresh memory block, it will:

- Try to claim a block from `page_pool_lazily_freed`, which contains blocks that were empty during the last stop-the-world phase, but haven't been madvised by a concurrent scavenger GC thread yet.

- If it failed to claim a block from `page_pool_lazily_freed`, it will try to claim a block from `page_pool_clean`, which contains blocks mapped on a previous block allocation request but never accessed.

- If it failed to claim a block from `page_pool_clean` and from `page_pool_lazily_freed`, it will try to claim a block from `page_pool_freed`, which contains blocks already scavenged by a concurrent scavenger GC thread and whose underlying virtual address can be recycled.

- If it failed in all of the attempts mentioned above, it will map a batch of operating system pages, partition them into memory blocks, claim one block for itself, and insert the remaining blocks into `page_pool_clean`.

![Diagram of tiered pool allocation](./img/gc-tiered-allocation.jpg)

### Large Object Allocation

Sufficiently large objects, above the 2k byte threshold mentioned in the previous section, are allocated through `libc` `malloc`. Large allocations are typically less performance-critical than small allocations, as they occur less frequently.

Although Julia currently uses `libc` `malloc`, it also supports pre-loading other dynamic memory allocators (e.g., `jemalloc`).

## Marking and Generational Collection

Julia’s mark phase is implemented through a parallel depth-first-search that traverses the object graph to determine which objects are alive.

Julia stores age information for its generational GC in the object header: the lowest two bits of an object’s header store a mark bit, set when an object is marked, and an age bit, set when the object is promoted. Because Julia’s GC is non-moving, object age information can’t be only determined through the object's memory address, such as in GC implementations that allocate young objects in certain memory regions and relocate them to other memory regions during object promotion.

Generational collection is implemented through sticky bits: objects are only pushed to the mark-stack, and therefore traced, if their mark-bits have not been set. When objects reach the oldest generation, their mark-bits aren't reset during a quick sweep, so these objects aren't traced during a subsequent mark phase. A full sweep, however, resets the mark-bits of all objects, so all of them are traced in a subsequent collection.

When the mutator is running, a write barrier intercepts field writes and pushes an object’s address into a per-thread remembered set if the reference crosses generations. Objects in this remembered set are then traced during the next mark phase.

## Sweeping

If a memory block managed by the pool allocator contains at least one live object, the sweeping phase creates a free-list from its dead objects; if it doesn't, then the block is scavenged and its underlying physical memory might be returned to the operating system through, for instance, `madvise` on Linux.

The linear scan of memory blocks that have at least one live object can be run with multiple threads. If concurrent page sweeping is enabled through the flag `--gcthreads=X,1` the GC scavenges memory blocks concurrently with the mutator.

During the stop-the-world phase of the collector, memory blocks containing no live objects are initially pushed into the `page_pool_lazily_freed`. The background scavenger thread is then woken up and removes blocks from `page_pool_lazily_freed`, scavenges them (e.g., `madvise` on Linux), and inserts them into `page_pool_freed`. `page_pool_lazily_freed` is also shared with mutator threads. This can improve performance in some applications because in allocation-heavy multithreaded workloads, mutator threads often avoid a page fault during allocation, which happens by accessing a freshly mapped operating system page or a madvised page, by directly allocating a block from `page_pool_lazily_freed`. In these workloads, the scavenger thread also needs to scavenge fewer blocks, since some have already been claimed by the mutators.

## Memory Accounting

The GC determines the heap size by adding the number of bytes in-use by pool-allocated memory blocks and bytes in-use by objects allocated through the large allocator. Previously, we measured the heap size by adding up the bytes for live objects, but not live memory blocks. This definition ignores fragmentation, which can lead to inaccurate GC decisions.

## GC Trigger Heuristics

Julia's GC heuristics are based on `MemBalancer` (https://dl.acm.org/doi/10.1145/3563323). They decide when to trigger a collection and which (quick or full) collection to trigger. The heuristics adjust the number of bytes the mutator can allocate before triggering a collection cycle by measuring metrics such as allocation rate, freeing rate, and current heap size.

Independently of allocation rates, freeing rates, or GC times, Julia will always trigger full collections if the heap size exceeds 80% of a memory upper bound specified through `--heap-size-hint` or determined by reading system information.

## Julia + MMTk

There has been quite a lot of effort to refactor the GC code inside Julia to support external GCs. The first step to enable using different GC algorithms for Julia was the design and implementation of a [GC interface](https://docs.google.com/document/d/1v0jtSrIpdEDNOxj5S9g1jPqSpuAkNWhr_T8ToFC9RLI/edit?usp=sharing). To drive that interface, we added support for building Julia with [MMTk](https://www.mmtk.io). MMTk is a memory management toolkit providing language implementers with a framework to implement flexible and performant GCs. The flexibility comes from the fact that it is possible to switch implementations fairly easily. MMTk supports state-of-the-art high-performance implementations that are continuously added and maintained in the core part of the framework. MMTk is under active development and has been used by other programming languages such as [Java](https://github.com/mmtk/mmtk-openjdk) and [Ruby](https://github.com/ruby/mmtk). To support a language, it is necessary to implement an *MMTk binding*, which contains the code that connects the language to [mmtk-core](https://github.com/mmtk/mmtk-core). The mmtk-julia binding can be found in [this repository](https://github.com/mmtk/mmtk-julia).

> [!NOTE]
> Using a different GC requires building Julia from source. It is not possible to switch implementations at runtime. To see what version of the GC is currently being used, run `versioninfo()` from the Julia REPL and it should show the version under `GC: ...`.

### Building Julia with MMTk

There are 3 different ways of building Julia with MMTk. The first is to simply use the BinaryBuilder version of the mmtk-julia binding. There are different configurations supported by the following variables, which can be set in a `Make.user` file or as an environment variable.

| Variable      |       |        |
|---------------|--------------|---------------|
| `MMTK_PLAN`     | Immix        | StickyImmix   |
| `MMTK_MOVING`   | 0            | 1             |
| `MMTK_BUILD`    | release      | debug         |

If only `MMTK_PLAN` is set, then the default is currently do to a non-moving release build.

> [!IMPORTANT]
> While the binding supports building all versions above, we have only integrated non-moving Immix into Julia. Support for the other versions should be added in the near future.

#### Building mmtk-julia from source

It is also possible to build the binding from source. To do so, set the variable `USE_BINARYBUILDER_MMTK_JULIA=0` and the latest release version of the binding will be downloaded and built as part of building Julia. Note that this requires an installation of the rust toolchain.

It is also possible to build a custom version of binding by checking it out from the [git repository](https://github.com/mmtk/mmtk-julia) and setting a variable named `MMTK_JULIA_DIR` to the path containing the binding.

For more information on building Julia with MMTk, please refer to the [README](https://github.com/mmtk/mmtk-julia/blob/master/README.md) file in the binding repo.

#### I've got a build error when building Julia with MMTk, what should I do?

If you try to build Julia with MMTk and get an error it is likely due to a change to Julia that has not been yet propagated to the binding, or to the code in Julia that is specific to MMTk. Some changes include:

(1) **Changing the memory layout of objects in Julia**. The binding relies on automatically generated Rust FFI bindings from Julia code. These files are generated using a crate named [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen). To regenerate those files, check out the latest version of the `mmtk-julia` binding, set the variable `JULIA_PATH` to the path of the Julia version you are trying to build and run `make regen-bindgen-ffi` from the directory containing the binding. This should delete the current version of the FFI bindings and generate a new version based on the Julia code from `JULIA_PATH`.

(2) **Changing the root objects passed to the GC**. Julia passes a set of objects to the GC as roots in the function [gc_mark_roots](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2846). At the moment, this set needs to be consistent between both the Stock GC and MMTk (in the function [`jl_gc_scan_vm_specific_roots`](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-mmtk.c#L496)).

(3) **Changing how objects are scanned**. MMTk uses the same strategy to find references in Julia objects as the stock GC (see [gc_mark_outrefs](https://github.com/JuliaLang/julia/blob/fbe865657942da7d73cc02f76064f9ba9cdef56c/src/gc-stock.c#L2227C19-L2227C34)). Changing the logic from this function should be reflected in the Rust code in the binding that [scan Julia objects](https://github.com/mmtk/mmtk-julia/blob/c9e046baf3a0d52fe75d6c8b28f6afd69b045d95/mmtk/src/julia_scanning.rs#L68).

If your case is not included in one of the alternatives above, please create an issue in the Julia repository tagging it with the `GC: MMTK` label.

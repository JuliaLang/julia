# Garbage Collection in Julia

## Introduction

Julia has a non-moving, partially concurrent, parallel, generational and mostly precise mark-sweep collector (an interface
for conservative stack scanning is provided as an option for users who wish to call Julia from C).

## Allocation

Julia uses two types of allocators, the size of the allocation request determining which one is used. Objects up to 2k
bytes are allocated on a per-thread free-list pool allocator, while objects larger than 2k bytes are allocated through libc
malloc.

Julia’s pool allocator partitions objects on different size classes, so that a memory page managed by the pool allocator
(which spans 4 operating system pages on 64bit platforms) only contains objects of the same size class. Each memory
page from the pool allocator is paired with some page metadata stored on per-thread lock-free lists. The page metadata contains information such as whether the page has live objects at all, number of free slots, and offsets to the first and last objects in the free-list contained in that page. These metadata are used to optimize the collection phase: a page which has no live objects at all may be returned to the operating system without any need of scanning it, for example.

While a page that has no objects may be returned to the operating system, its associated metadata is permanently
allocated and may outlive the given page. As mentioned above, metadata for allocated pages are stored on per-thread lock-free
lists. Metadata for free pages, however, may be stored into three separate lock-free lists depending on whether the page has been mapped but never accessed (`page_pool_clean`), or whether the page has been lazily sweeped and it's waiting to be madvised by a background GC thread (`page_pool_lazily_freed`), or whether the page has been madvised (`page_pool_freed`).

Julia's pool allocator follows a "tiered" allocation discipline. When requesting a memory page for the pool allocator, Julia will:

- Try to claim a page from `page_pool_lazily_freed`, which contains pages which were empty on the last stop-the-world phase, but not yet madvised by a concurrent sweeper GC thread.

- If it failed claiming a page from `page_pool_lazily_freed`, it will try to claim a page from `the page_pool_clean`, which contains pages which were mmaped on a previous page allocation request but never accessed.

- If it failed claiming a page from `pool_page_clean` and from `page_pool_lazily_freed`, it will try to claim a page
  from `page_pool_freed`, which contains pages which have already been madvised by a concurrent sweeper GC thread and whose underlying virtual address can be recycled.

- If it failed in all of the attempts mentioned above, it will mmap a batch of pages, claim one page for itself, and
  insert the remaining pages into `page_pool_clean`.

![Diagram of tiered pool allocation](./img/gc-tiered-allocation.jpg)

## Marking and Generational Collection

Julia’s mark phase is implemented through a parallel iterative depth-first-search over the object graph. Julia’s collector is non-moving, so object age information can’t be determined through the memory region in which the object resides alone, but has to be somehow encoded in the object header or on a side table. The lowest two bits of an object’s header are used to store, respectively, a mark bit that is set when an object is scanned during the mark phase and an age bit for the generational collection.

Generational collection is implemented through sticky bits: objects are only pushed to the mark-stack, and therefore
traced, if their mark-bits are not set. When objects reach the oldest generation, their mark-bits are not reset during
the so-called "quick-sweep", which leads to these objects not being traced in a subsequent mark phase. A "full-sweep",
however, causes the mark-bits of all objects to be reset, leading to all objects being traced in a subsequent mark phase.
Objects are promoted to the next generation during every sweep phase they survive. On the mutator side, field writes
are intercepted through a write barrier that pushes an object’s address into a per-thread remembered set if the object is
in the last generation, and if the object at the field being written is not. Objects in this remembered set are then traced
during the mark phase.

## Sweeping

Sweeping of object pools for Julia may fall into two categories: if a given page managed by the pool allocator contains at least one live object, then a free-list must be threaded through its dead objects; if a given page contains no live objects at all, then its underlying physical memory may be returned to the operating system through, for instance, the use of madvise system calls on Linux.

The first category of sweeping is parallelized through work-stealing. For the second category of sweeping, if concurrent page sweeping is enabled through the flag `--gcthreads=X,1` we perform the madvise system calls in a background sweeper thread, concurrently with the mutator threads. During the stop-the-world phase of the collector, pool allocated pages which contain no live objects are initially pushed into the `pool_page_lazily_freed`. The background sweeping thread is then woken up and is responsible for removing pages from `pool_page_lazily_freed`, calling madvise on them, and inserting them into `pool_page_freed`. As described above, `pool_page_lazily_freed` is also shared with mutator threads. This implies that on allocation-heavy multithreaded workloads, mutator threads would often avoid a page fault on allocation (coming from accessing a fresh mmaped page or accessing a madvised page) by directly allocating from a page in `pool_page_lazily_freed`, while the background sweeper thread needs to madvise a reduce number of pages given some of them were already claimed by the mutators.

## Heuristics

GC heuristics tune the GC by changing the size of the allocation interval between garbage collections.

The GC heuristics measure how big the heap size is after a collection and set the next collection according to the algorithm described by https://dl.acm.org/doi/10.1145/3563323, in summary, it argues that the heap target should have a square root relationship with the live heap, and that it should also be scaled by how fast the GC is freeing objects and how fast the mutators are allocating. The heuristics measure the heap size by counting the number of pages that are in use and the objects that use malloc. Previously we measured the heap size by counting the alive objects, but that doesn't take into account fragmentation which could lead to bad decisions, that also meant that we used thread local information (allocations) to make decisions about a process wide (when to GC), measuring pages means the decision is global.

The GC will do full collections when the heap size reaches 80% of the maximum allowed size.

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

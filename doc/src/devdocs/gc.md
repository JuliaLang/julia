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

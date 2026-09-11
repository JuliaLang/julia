# The cost of regions that a program does not use

A julia with the region runtime, on a program that never opens a window,
runs the stock collector unchanged. It is not free. This document says what
such a program pays, in memory and in time, in absolute and in relative
terms, and which switches take which part of the cost out.

The timing numbers below repeat the M1, M2 and M13 tables of
[`MEASUREMENTS.md`](MEASUREMENTS.md). That run was on an idle machine of 32
CPUs: the unit costs inside an isolated cpuset partition on a tickless core,
over 25 paired rounds; the collection rows on the whole machine, over 10
paired rounds. A bracket is a percentile bootstrap interval at 95 % over the
rounds. [`results/tables.py`](results/tables.py) rewrites the tables of
[`MEASUREMENTS.md`](MEASUREMENTS.md) from the data files, and the rows here
follow by hand. The sizes come from a C program compiled against the headers
of the two builds (`sizeof`) and from `size` and `readelf` on the two
binaries; they do not depend on the machine's load.

The two binaries are the ones of [`MEASUREMENTS.md`](MEASUREMENTS.md):
**vanilla**, built from the base commit `8f33e09afe` (`v1.13.0-rc4`) with
nothing else changed, and
**regions**, this branch, in a process that never opens a window.

## What the cost is made of

1. **A static cost, once per process.** More code in the runtime, and a
   larger system image. The system image grows because every managed
   pointer store of compiled Julia code carries a guard: one load of a
   global flag and one predicted branch.
2. **A structural cost, per thread and per pool page.** A few hundred bytes
   in each thread heap, and eight bytes more in the metadata of each 16 KB
   pool page. Nothing per object and nothing per task. No heap memory is
   allocated for regions until a first window opens: the region table of a
   thread heap is 64 `NULL` pointers until then.
3. **A dynamic cost, per operation.** A flag test on each pointer store and
   one on each construction of an object with boxed children, one
   indirection on each pool allocation, and one relaxed load per object in
   the mark loop. Each is a fraction of a nanosecond.

## Memory

| Item | Vanilla | Regions, unused | Delta | Relative |
| --- | --- | --- | --- | --- |
| Thread heap struct, `jl_thread_heap_t` | 1,496 B | 2,104 B | +608 B per thread | +13 % of the 5 KB thread-local state; 0.01 % of an 8 MB thread stack |
| Task struct, `jl_task_t` | 224 B | 224 B | 0 | the two region bytes sit in existing padding |
| Page metadata, `jl_gc_pagemeta_t` | 40 B | 48 B | +8 B per 16 KB page | +0.05 % of the pool heap; 512 KB on a 1 GB pool heap |
| Region state on the heap | — | 0 B | 0 | 64 `NULL` pointers until a first window; about 1.5 KB per region a heap then uses |
| Runtime static data, `.bss` | — | +67 KB | +67 KB | 64 KB is the block table of the heap reserve, of which about 8 KB is ever touched |
| Runtime code, `libjulia-internal` `.text` | 2,667 KB | 2,708 KB | +41 KB | +1.5 % |
| System image code, `sys.so` `.text` | 16,762,424 B | 17,338,504 B | +576,080 B | +3.4 %; the store guards in the compiled code of Base |
| System image file, `sys.so` | 198.8 MB | 200.2 MB | +1.4 MB | +0.7 % |

The thread heap gains the region table (512 B), the pool pointer of the
current region (8 B), the two tree masks (16 B), the child counts (64 B), and
three bytes of window state with their padding. The page metadata gains the
region tag (1 B) and the chain link (8 B), and the struct grows by one
alignment step. The system image is mapped, not read: only the pages the
program touches count, but the guards sit inside the hot code and dilute the
instruction cache by their share of the text.

## Time

| Operation | Vanilla | Regions, no window | Delta | Relative |
| --- | --- | --- | --- | --- |
| Pointer store with a write barrier | 0.321 ns | 0.406 ns | +0.085 ns [0.084, 0.086] | +27 % of the barrier |
| Construction of an object with two pointer fields and two fresh children (three allocations) | 7.01 ns | 7.82 ns | +0.823 ns [0.767, 0.860] | +12 %; the three allocations, see below |
| Construction of the same object with two shared children (one allocation) | 3.60 ns | 3.93 ns | +0.335 ns [0.320, 0.351] | +9 %; the allocation and a flag check |
| A boxed copy of an immutable value with two pointer fields | 3.90 ns | 4.14 ns | +0.243 ns [0.220, 0.266] | +6 %; the allocation, and the field checks inside it |
| Pool allocation | 2.15 ns | 2.43 ns | +0.283 ns [0.277, 0.291] | +13 % |
| Stock mark, one collection, one thread | 65.6 ms | 66.8 ms | +1.14 ms [0.89, 1.43] | +1.7 % |
| A full collection, 32 threads, 16 GC threads, 1 GB live | 83.5 ms | 88.9 ms | +5.4 ms | +7 % [3 %, 8 %]; see the note below |
| Stock sweep | — | one byte test per page | not measurable | — |
| Each stock collection, the three region brackets | — | 64 pointer tests per heap | nanoseconds | — |
| Task switch | — | one byte compare and one byte store | nanoseconds | — |
| Finalizer registration | — | one page-metadata lookup | tens of nanoseconds, on a rare path | — |
| GCBenchmarks, end to end (M1) | 1.00 | 0.92 to 1.02 | six of nine cross 1.00 | one benchmark 2 % slower |

Where each cost comes from:

- **The store.** The lowered write barrier loads `jl_gc_region_barrier_on`
  and branches on it. The flag is 0 until a first window, and the branch is
  predicted. The C runtime and the builtins run the same test in
  `jl_gc_wb` ([`src/gc-wb-stock.h`](../../src/gc-wb-stock.h)).
- **The construction.** Vanilla emits no write barrier for the fields of a
fresh object, because a fresh object is young. This branch emits the region
check for the boxed children of a fresh object through a second intrinsic,
`julia.region_write_barrier` ([`src/cgutils.cpp`](../../src/cgutils.cpp),
[`src/llvm-late-gc-lowering.cpp`](../../src/llvm-late-gc-lowering.cpp)): one
call per object that names every boxed child, lowered to the flag load, the
branch, and a cold call per child; no generational part. The +1.0 ns of the
first construction row is not this check: the row allocates two children and
the object, and each allocation pays the +0.28 ns of the allocation row. The
second row makes the same object from two shared children and pays one
allocation and the check. The third row boxes an immutable value with two
pointer fields: the runtime copies the fields into the fresh box and checks
each one, and the delta stays under the delta of a plain allocation, so the
check is inside the spread of the row.
- **The allocation.** `jl_gc_small_alloc_inner` decodes its pool offset to
  an index and addresses the pools through `active_pools`, one dependent
  load from the thread-local state, in place of a fixed offset. Then
  `maybe_collect` tests `current_region != 0`, one byte load and a
  predicted branch.
- **The mark.** The mark loops read `jl_gc_region_census_target` once per
  object array and pass it down; the claim tests it per object. The load is
  relaxed and the branch is predicted. On one thread that costs 1.7 % of the
  mark, established over 25 paired rounds.
- **The collection on the whole machine.** The cost is not flat in the
  thread count. A full collection over a live set that every thread builds
  costs 2 % more with the region runtime at one thread and **7 % more at 32
  threads with 16 GC threads** (M13), and the mark alone goes from 58.7 to
  62.5 ms there. Six probe builds attribute it, and the attribution is in
  [`HISTORY.md`](HISTORY.md):

  - The **page metadata**, 8 bytes larger per 16 KB page, costs nothing:
    vanilla padded by the same 8 bytes measures 0.996 [0.971, 1.04] at 30
    threads.
  - The **thread heap**, 608 bytes larger, explains 1.02 [0.999, 1.03]. Its
    fields are appended, so every field of the stock collector keeps the
    offset it has in a build without regions.
  - The **census filter** is not the cause. A build with
    `JL_NO_REGION_ALLOC` folds it to a constant and still costs 1.04
    [1.02, 1.06], where a plain rebuild of the same source - the control -
    costs 1.06 [1.05, 1.07].
  - The **region code as a whole** is not the cause either: with both
    defines off, so no store barrier, no region allocation path and no
    filter, the collection still costs 1.05 [1.02, 1.08].

  A profile says what kind of cost it is. The region build executes 5.7 %
  more instructions at the same instructions per cycle, and every extra
  cycle is inside the collector: it executes more, it does not stall more.
  The arithmetic of the mark is about 15 instructions per marked object.
  They are the collector's own region code, which no define removes: the
  corpse check of `gc_setmark_pool`, one test per marked object; the census
  parameter of the mark tree, one test per slot; the region test of the page
  sweep, one per page; and the three brackets of a collection. Each is worth
  about one point, which is the resolution of this measurement.
- **The sweep.** The page sweep tests `region_n != 0` once per page and
  skips a region page. No region page exists.
- **The collection.** Three brackets of a stock collection walk the region
  table of every heap: clear the stock marks of region pages, mark the
  region finalizer lists, and the finalizer epilogue. Each is 64 pointer
  tests per heap that all find `NULL`.
- **The task switch.** `ctx_switch` compares the region of the task that
  comes in with the current one, and stores the current one into the task
  that goes out.

## The judgment

- **Memory:** noise. 608 bytes per thread and 0.05 % of the pool heap are
  below anything a user can observe.
- **Code size:** noticeable, acceptable. A 3 % larger system image text is
  the size of a medium feature of Base. A core developer notes it and asks
  for the build switch, which exists: `JL_NO_REGION_STORE_BARRIER` below.
- **Time:** small but not zero, and on the two paths that the core
developers guard most: the write barrier and the allocation fast path. A 13 %
change on an allocation microbenchmark starts a discussion in a pull request,
even when the application benchmarks stay inside noise. The residual is 0.085
ns per store, the same check once per construction of an object with boxed
children, and 0.283 ns per allocation. Every one of those intervals is tight
over 25 paired rounds; none of them is noise. Those are the price of a feature
that is off, and the allocation is the largest of them: `active_pools` is one
dependent load from the thread-local state in place of a fixed offset, and no
switch short of `JL_NO_REGION_ALLOC` removes it.
- **Mark and sweep:** 1.7 % on a serial mark is at the edge of what a
  collector developer accepts without a switch, and **7 % on a 32-thread
  collection is past it**. That number is the one an upstream review will
  ask about first, and this document cannot yet say which of the two
  candidates carries it. `JL_NO_REGION_ALLOC` below folds the census
  branches out of the mark loops and is the tool that would answer it.

In one sentence: unused regions cost a few hundred bytes per thread, 3 % of
the image code, a tenth of a nanosecond per pointer store, 0.3 ns per
allocation, and up to 7 % of a collection on the whole machine; the memory
is free, the code size is acceptable, and the collection on many threads is
the one number an upstream review will ask about.

## What was tried, and what it cost

Four ways to cut the collection cost were built and measured against the
control. None was accepted, and each was reverted the day it was measured.

| Candidate | Result |
| --- | --- |
| Give the page metadata its 8 bytes back, by moving the region page chain into the region state | Dropped before it was written: the padding probe shows those bytes are free |
| Specialize the mark loops, so the stock path compiles with the census filter folded to 0 | Built, tested, and the fold confirmed in the disassembly. No change at 30 threads, +11 KB of text. The compiler already unswitches a branch on a loop-invariant value |
| Put the region table behind one pointer, so the thread heap grows by 8 bytes instead of 608 | Three points **worse**: 1.09 [1.06, 1.10] against a control of 1.06 [1.05, 1.07] |
| Shrink the thread heap another way | Not attempted: the fields are appended already, and the padding probe explains only 1.02 of the 1.06 |

The measurement cannot resolve the checks that remain, because each is worth
about a point and a point is its noise floor. A finer instrument - one
process marking one fixed object graph, with and without a single check -
would be needed before removing any of them, and each removal would cost a
diagnostic or a mechanism.

## The switches and their effect on the cost

There is no `julia` command-line option and no environment variable for the
regions. The runtime has two kinds of switch: two build defines, decided
when julia is compiled, and three runtime entries, called from a program.
The build defines are the ones that change what an unused runtime costs.

### Build defines

A define goes through `CPPFLAGS` in `Make.user`, which
[`src/Makefile`](../../src/Makefile) adds to every C and C++ compile of the
runtime:

```
CPPFLAGS += -DJL_NO_REGION_STORE_BARRIER
```

A change of `CPPFLAGS` alone does not recompile the objects that exist. Run
`make -C src clean` first, then `make`. The system image is rebuilt with
the runtime, because the lowered write barrier is part of the compiler.

| Define | What it turns off | What it removes from the table above | What stays |
| --- | --- | --- | --- |
| `JL_NO_REGION_STORE_BARRIER` | The escape barrier, in the compiler (`llvm-late-gc-lowering.cpp`, `cgutils.cpp`) and in the C-side hooks (`gc-wb-stock.h`). A pointer store and a construction compile exactly as vanilla compiles them. | The store guard (0.09 ns per store), the construction guard (the same check once per object with boxed children), the store guards of the system image (0.6 MB of `.text`). | The allocation indirection, the mark filter, the page tag, the structs. Rule 4 of the model is gone, and rule 3 rests on the static checker [`tools/region_check.jl`](tools/region_check.jl) alone: a window can open, but no escape is ever seen. |
| `JL_NO_REGION_ALLOC` | The region pools. `jl_gc_small_alloc_inner` takes the stock pool by its fixed offset, `jl_gc_region_set` refuses with `EINVAL`, and the census filter of the mark loops is the constant 0, which folds the census branches out. | The allocation indirection and the `maybe_collect` test (0.3 ns per allocation), the mark filter (1 % of the mark). | The store guard and the construction guard, the page tag, the structs. No window opens, so no region state is ever made and no census runs. |
| Both | Everything per object. | Every row of the time table except the sweep, the brackets and the task switch. | One region tag per page, which the page allocator writes and the sweep reads; a store and a compare of the region fields at a task switch; the 608 bytes per thread heap and the 8 bytes per page, because the structs do not change; the runtime code that no path reaches. |

The region tests fail on both builds by design: every window refused on the
one, no escape ever seen on the other. A build with a define is a build for
measurement or for a deployment that wants the stock collector alone. It is
not a build that runs the demonstrators.

### Runtime entries

Three entries change what a program pays. None of them costs anything in a
program that never opens a window; they are listed so that the whole set of
switches is in one place.

| Entry | Effect | Cost when set |
| --- | --- | --- |
| `jl_gc_region_census_threshold(pages)` | A window on a region that holds at least `pages` pages gets a census of the open region in place of a stock collection. 0 turns it off. | Inside a window, `maybe_collect` reads the threshold and, when it is on, the page count of the current region: one load and one compare more per allocation in a window. Nothing outside a window. |
| `jl_gc_region_set_debug(on)` | With reporting on, the root check of a reset names the objects it finds. The check itself always runs. | A print per found reference, at a reset that refuses. Nothing on any other path. |
| `jl_gc_heap_reserve(bytes)` | Prefaults `bytes` of pool heap so that a later allocation never faults. | The page faults move from the allocation to the call. The reserve keeps the memory mapped and touched for the life of the process. Without the call, nothing. |

The stock options of `julia` are unchanged by the regions. `--gcthreads`
sets the threads of the stock mark and sweep, which a census does not use;
`--heap-size-hint` bounds the stock heap, which a region's pages count
toward.

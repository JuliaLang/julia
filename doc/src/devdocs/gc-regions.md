# GC regions

A region is a numbered set of pool pages with its own allocation cursors. A
thread allocates into region `n` while a window on `n` is open, and frees every
object of the region at once with a reset, without a trace. The stock collector
implements the regions (`src/gc-regions.c`, `src/gc-regions.h`) in a build with
`make WITH_GC_REGIONS=1`. Without the flag, every change to an existing file is
inside an `#ifdef` of the flag or is a hook that expands to no code, and the
runtime compiles to the same code, object for object. A program that opens no
window runs on the stock collector unchanged.

The design is for a program that repeats one unit of work many times, whose
garbage dies at the end of each unit, and which cannot afford a tracing
collection at a moment it does not choose, such as a discrete event simulator
with a time budget per event. It opens a window before the unit and resets the
region after it; the reset frees every object the unit allocated in constant
time per page. Every object outside a window is a stock object, and the stock
collector runs with regions present.

## The model and the rule

Every managed object belongs to exactly one region for its whole lifetime.
Region 0 is the stock heap; regions 1 to `JL_GC_MAX_REGIONS - 1` are the regions
a program can open. There is no promotion and no migration. The regions form a
tree of lifetimes: the parent of a region lives at least as long as the region,
and the default tree is the chain `0 <- 1 <- 2 <- ...` ([The tree](@ref gc-regions-tree)).
**The one rule:** a managed reference `a -> b` is legal when the region of `b`
is the region of `a` or one of its ancestors; a reference from an older region
into a younger one is an escape. So no object outside a region references into
it, except a younger descendant, and once the descendants are reset, a reset of
the region frees every object of it without a trace.

The rules the runtime keeps: every pool allocation goes into the region of the
open window of the thread, the compiler's own allocations included, except a
buffer that replaces the buffer of an object that exists; every managed pointer
store checks the one rule while a region is in use, and a store that breaks it
quarantines the region of the child until the next stock collection; a region is
reset only when no execution root references into it and no live child region
exists; a collection of one region takes its roots from the execution roots of
every task and from the younger regions, nothing else. The store check is what
makes the design different from a generational collector: that barrier records
a store, this one rejects a store.

Two facts carry the rest of the runtime. **The runtime's own objects are
region 0**: inference, compilation, a dispatch cache miss and a type
instantiation run in a region-0 zone (`gf.c`, `jltypes.c`); a binding and its
partition are made under a borrow of region 0 (`module.c`), the exception stack
of a task under a borrow of the task's region (`rtutils.c`), a wait entry in
region 0 (`task.c`, `base/cancellation.jl`). **The barrier check runs at managed
stores only**: a store from C, an `unsafe_store!` and a pointer kept by a
foreign library are not checked, and such a region object dangles at the reset.

## The barrier

The compiler emits the escape barrier at every managed pointer store of a boxed
child, before the generational write barrier
(`src/llvm-final-gc-lowering-stock.cpp`): one load of the flag
`jl_gc_region_barrier_on` and a branch. The first window of the process arms
the flag, and it stays armed. At the fields of a fresh object, where no
generational barrier is needed, codegen emits `julia.region_write_barrier`, the
guard alone, once for every boxed child and for the pointer fields of every
inline field; its parent is `nocapture`, so that alloc-opt can still elide the
object. `jl_gc_wb` in `src/gc-wb-stock.h` runs the same
check for the runtime's stores. Armed, the store calls
`jl_gc_region_wb(parent, child)`, which reads the region of the child from its
page tag; a child of region `cr` under a parent of region `pr` is legal when
`cr` is `pr` or an ancestor of it. An illegal store is reported in one
`REGION-ESCAPE` line and quarantines `cr`: the store stays, a reset and a
census of `cr` return `EQUARANTINED`, and the next stock collection hands the
pages of `cr` to the stock collector, where the escaped object and the other
objects of the region live and die as ordinary objects. The region is usable
again after that collection.

A bulk copy (`copyto!` and `copy` of a `Memory` with references,
`jl_svec_copy`, the store of an inline immutable with pointer fields) checks
the pair (destination, source) first: every element of a legal source is legal,
so the copy costs one check; when the pair check fails, the elements are
checked one by one. A copy of an inline value whose bytes are not a heap object
checks its pointer fields through `jl_gc_multi_wb_fresh` (`src/gc-interface.h`),
the fourth barrier annotation; the three that exist carry the region check too.

## A window, a borrow, a replacement buffer

A window (`jl_gc_region_set`) belongs to the calling task: the runtime saves
and restores it at a task switch, counts it, and pins the task to its thread,
because a region's pages live in the thread heap; a bad number, a quarantined
region and a thread that runs finalizers get a refusal code. A window is the
lifetime scope of a unit of work. A borrow (`jl_gc_region_borrow`) installs a region for
the next allocations of the thread and nothing else, for one allocation that
must land where another object lives: keep it short, do not yield inside one,
give it back in a `finally`. `jl_gc_region_suspend` is the borrow of region 0.

A container that grows replaces the buffer behind an object that exists, and
the new buffer takes the region of that object, whatever window is open;
otherwise a `push!` to a long-lived vector inside a window would store a
younger buffer into an older array, an escape that quarantines the region. The region
comes from the container, never from the old buffer, because an empty container
shares one permanent empty `Memory` of region 0. Base applies the rule in
`base/gcregions.jl` (`with_region_of`, `memory_for`) at every array growth,
`rehash!` of a `Dict`, `empty!` of an `IdDict`, `push!` of an `IdSet` and the
growth of an `IOBuffer`; the runtime in `jl_array_grow_end` and
`jl_idtable_rehash`; a container written elsewhere with `@in_region_of` of
`contrib/memory-regions/regions.jl`. The rule covers the buffer, not the
elements: a region object stored into a long-lived container is an escape.

## The reset

`jl_gc_region_reset` runs four phases: the preconditions (a valid region, no
window on it, no finalizer run on this thread, no quarantine, no live child);
the finalizers of the region, in rounds until the list is empty; the quarantine
read again, because a finalizer can escape; the root check and the free in one
stop-the-world pause. The root check marks from the execution roots of every
thread with the region filter and returns `EROOT` when a reference into the
region exists: the barrier covers the heap, the root check covers the stacks.
A Julia frame roots a local until the frame ends, so build and use the region's
objects in one function and reset after it returned. When several threads
reset at once, a reset retries the safepoint and returns `ERACE` after many
lost attempts. `jl_gc_region_reset_global` runs the same check over
every heap's instance of the region in its own pause. `jl_gc_region_unsafe_reset`
frees with no pause and no scan; a reference from a stack slot or a parked task
then dangles, and the next collection reports `CORPSE` and aborts.

## The API

The entries are `ccall` targets that take region numbers, whose meaning belongs
to the program; `contrib/memory-regions/regions.jl` wraps them, and
`Base.GC_REGIONS` is true in a build with them.

| Entry | Returns |
|:--|:--|
| `jl_gc_region_set(n)` | Open a window on region `n` on the calling task; `n = 0` closes it. Returns the region that was current, or a refusal code. |
| `jl_gc_region_current()` | The region of the open window, 0 when none. |
| `jl_gc_region_reset(n)` | Free every object of region `n` on the calling thread's heap after the root check. Returns the pages the region held, or a refusal code cast to `uint64_t`. |
| `jl_gc_region_unsafe_reset(n)` | The same with no check and no pause. |
| `jl_gc_region_reset_global(n)` | Free region `n` on every heap at once, with the world stopped. |
| `jl_gc_region_borrow(n)`, `jl_gc_region_unborrow(lent)` | Install region `n` for the next allocations of this thread; give back the region it replaced. |
| `jl_gc_region_declare_parent(child, parent)`, `jl_gc_region_parent_of(child)` | Declare and read an edge of the tree. |
| `jl_gc_region_collect(n)`, `jl_gc_region_collect_coop(n)` | The stop-the-world and the cooperative census of region `n`: free its dead objects, keep the live ones. Returns the cells freed, or a refusal code. |
| `jl_gc_region_census_threshold(pages)` | The page count of the open region that triggers a census from the allocator; 0 never. |
| `jl_gc_region_of(v)`, `jl_gc_region_pages(n)`, `jl_gc_region_quarantined(n)` | The region of an object; the pages of a region on this heap; 1 when an escape quarantined the region. |
| `jl_gc_region_stat(i)` | A field of the last census: 0 total ns, 1 stop-the-world ns, 2 mark ns, 3 sweep ns, 4 live cells, 5 freed cells, 6 pages walked, 7 pages freed wholesale. |
| `jl_gc_region_set_debug(on)`, `jl_gc_region_check(n)`, `jl_gc_region_verify(n)` | Name the roots a refused reset found; run the root check alone; walk the page chains for consistency. |

The refusal codes are negative: `EINVAL` (-1) a bad region number or tree edge,
`EBUSY` (-2) the region is current, a window is open, or this heap runs region
finalizers, `ERACE` (-3) the safepoint race was lost, `EUNSAFE` (-4) another
thread runs managed code (cooperative census), `EQUARANTINED` (-5), `EFINALIZERS`
(-6) finalizers are pending (cooperative census), `ECHILD` (-7) a live child
region, `EROOT` (-8) an execution root references the region.

## The window, the tree, the census

The runtime saves a window with its task and restores it at the switch
(`src/task.c`), closes it when the task ends, and starts a new task without
one. A stock collection runs with region 0 installed on every thread, the open
windows parked; so does a finalizer list, and no window opens on the thread
until it returns. The runtime's own work runs in region 0: inference and
compilation, the cache-miss path of a dynamic dispatch and of a type
instantiation run with the window closed and reopened
(`jl_gc_region_zone_enter`, `jl_gc_region_zone_leave`); a cache hit costs
nothing. The lazily initialized
state of Base (`OncePerProcess`, `OncePerThread`) is made with the window
suspended, so the task stays pinned while it may park on a lock.

A program declares another tree with `jl_gc_region_declare_parent` before the
regions are used: a parent's number is smaller than its child's. Two leaves
over a shared trunk are isolated from each other; a trunk two threads share is
reset with the global reset. A region is live between a window on it and its
reset; a reset or a census of a region with a live child returns `ECHILD`.

A census is a tracing collection of one region: the mark runs from the
execution roots of every task with the census filter set, which claims only
objects of the region, and the sweep covers only the pages of the region. The
stop-the-world census returns `EFINALIZERS` with pending region finalizers;
the cooperative census runs them after its sweep and needs every other thread
parked GC-safe. Both return `EBUSY` while a window is open. `jl_gc_region_census_threshold` makes the
page claim of a window run a census of the open region past that many pages,
which bounds a window whose garbage dies inside the window. A finalizer
registered on a region object goes to the list of the region: the reset runs
them all, the census the dead ones. A `Memory` with malloc'd data allocated in
a region is freed with the region. An object larger than the pool limit is a
big object of region 0. An allocation in a region counts in `gc_num.allocd`,
and nothing ever subtracts a region object from `Base.gc_live_bytes()`.

## What changes for whom

| Build and program | What the regions cost |
|:--|:--|
| A build without `WITH_GC_REGIONS` | Nothing: the same code, object for object. |
| A build with the flag, a program that opens no window | The flag load and branch at a managed pointer store; the census filter read at a claim of the mark; a region tag per page; a pointer per thread heap per region. |
| A program that opens windows | The rules below, the barrier call at a store while the flag is armed, and the reset's root check. |

## Discipline and limits

- Reset from a frame that names none of the region's objects, and close every
  window on the region first. Catch an exception inside the window; one that
  leaves it is a root into the region at the reset.
- Open a window inside a function, not at top level: a window at top level
  covers the next top-level statement, and a definition there stores a region
  object into a binding of region 0, an escape; the line numbers of the next
  statement escape the same way.
- Do not block inside a window: a waiting task keeps the region live and stays
  pinned to its thread. The wait entry is a region-0 object linked from the
  task, and the object waited on must be a region-0 object too.
- Make tasks outside the window and open a window inside them; do not capture
  a region object in a task closure (hand it over as a raw pointer under
  `GC.@preserve`).
- `WeakRef` on a region object throws, and so does an image write inside a
  window; `finalize(o)` on a region object does nothing; do not hand a region
  object to C and let it keep the pointer.
- A region's pages belong to one thread heap and never return to the operating
  system or to the stock pool, so `gc_heap_stats.heap_size` counts them, parked
  or in use. A quarantine lasts until the next stock collection, which hands
  the pages of the region to the stock collector; nothing is retained.
- Compilation runs in region 0: a method compiled for the first time inside a
  window costs its compile in the stock heap. A borrow is thread state: after
  a yield inside one, the task keeps the borrowed region until the borrow ends.
- `WITH_GC_REGIONS=1` and a third-party GC exclude each other; Linux x86-64 is the platform
  the design was exercised on. The demonstrators, the benchmarks, the
  measurements and the history of the design are outside this tree, on the
  branch [`gc-regions-master-evidence`](https://github.com/levy/julia/tree/gc-regions-master-evidence/contrib/memory-regions)
  of the fork: this tree plus `bench/`, `demo/`, `tools/`, `results/`,
  `MEASUREMENTS.md`, `COST.md` and `HISTORY.md` under `contrib/memory-regions`.

## Files

| File | Content |
|:--|:--|
| `src/gc-regions.h`, `src/gc-regions.c` | The API, the refusal codes, the hooks the runtime calls (no code without the flag), the window, the reset, the tree, the census, the barrier. |
| `src/gc-tls-stock.h`, `src/gc-stock.h`, `src/julia_threads.h` | The per-heap region table and masks, the page tag, the task's window. |
| `src/gc-stock.c`, `src/gc-common.c`, `src/gc-pages.c` | The allocation into the active pools, the census filter in the mark, the sweep that skips region pages, the finalizer and malloc'd lists, the collection brackets. |
| `src/gc-interface.h`, `src/gc-wb-stock.h`, `src/codegen.cpp`, `src/cgutils.cpp`, `src/intrinsics.cpp`, `src/llvm-final-gc-lowering-stock.cpp`, the LLVM passes | The escape barrier in the runtime and in the compiler. |
| `src/gf.c`, `src/jltypes.c`, `src/module.c`, `src/rtutils.c`, `src/task.c`, `src/array.c`, `src/builtins.c`, `src/staticdata.c` | The region-0 zones and borrows of the runtime, the window of a task, the refusal of an image write. |
| `base/gcregions.jl` | `Base.GC_REGIONS` and every hook of Base; each the plain call without the flag. |
| `contrib/memory-regions/`, `test/gc/regions_*.jl` | The Julia face and its README; the tests, run by `test/gc.jl` when `Base.GC_REGIONS`. |

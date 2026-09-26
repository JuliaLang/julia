# Memory regions

A second way to free memory in Julia, beside the stock collector: allocate
the objects of one unit of work in a **region**, and free the whole region in
one act, with no mark and no sweep.

```julia
@with_region 1 begin        # everything allocated here belongs to region 1
    handle_one_event(state)
end
region_reset(1)             # every object of that unit is gone, at once
```

The stock collector still runs, still owns everything outside a region, and
never traces a region's pages. A program that opens no window costs one
predicted branch per pointer store. The regions exist in a julia built with
`make WITH_GC_REGIONS=1`; `Base.GC_REGIONS` says whether a build has them,
and a build without the flag is the stock runtime, object for object. A
build with `WITH_GC_REGION_BARRIER=0` on top has the regions without the
escape barrier: no store is checked, for a program validated with it on;
`Base.GC_REGION_BARRIER` says which build this is.

## Why you might want it

The stock collector chooses when to pause. That is right for most programs
and wrong for a loop with a deadline: a discrete-event simulation that drives
hardware, a control loop, a frame, a request with a latency budget.

| The problem | What a region does |
| --- | --- |
| A pause of milliseconds arrives at a moment your loop did not choose. | You free at a moment you choose, and freeing costs the pages, not the objects. |
| You know the garbage of one unit of work dies at the end of that unit. The collector does not know it. | You say it: the unit's objects go in a region, and the reset frees them. |
| The collector's work grows with the live set it traces. | A reset traces nothing. Its cost is the pages of the region. |

What it does **not** do: it does not make a program faster in general, it
does not replace the collector, and it does not remove your duty to know what
your program allocates. Where the discarded allocation per unit of work is
small, the region model loses on wall time. The evidence branch named at
the end shows that crossover, not only the wins.

## The model, for a program

Five ideas, and nothing else:

| Idea | What it is |
| --- | --- |
| **Region** | A number from 1 to 63. Region 0 is the ordinary heap. A region is a set of pool pages, not a type and not a container. |
| **Window** | A scope. While a window on region `n` is open, everything the **calling task** allocates lands in region `n`. |
| **Reset** | Frees every object of a region at once, after a check that no stack references into the region; a reference found returns `EROOT`. `unsafe_region_reset` skips the check and its stop-the-world pause, at the cost of a few pointer swaps; a reference left behind dangles. |
| **The one rule** | An object in a region may reference objects of its **own region or an older one**. Region 0 is the oldest. A store that breaks the rule is caught. |
| **Lifetime tree** | The default order is `0 <- 1 <- 2 <- ...`: region 1 outlives region 2. Declare another tree, and two leaves become isolated from each other. |

**What happens if you break the rule.** The write barrier reports the store
in one line that names both objects and **quarantines** the region: its
reset and its censuses return `EQUARANTINED` until the next stock collection,
which hands the region's pages to the stock collector, where its objects live
and die like any other. You lose the region until that collection, never
memory safety and never memory.

**What you must not do.** The five that catch people:

- Do not keep a reference to a region object past the reset. The reset checks
  the stacks and returns `EROOT`, not a crash.
- Do not open a window at top level. A definition inside a window makes its
  method, type or binding in the region, and the store into region 0 is an
  escape.
- Do not capture a region object in a task closure. The closure is stored
  into the queues of the scheduler, region-0 objects. Pass a raw pointer or
  an index.
- Do not block inside a window. An open window keeps the region live and the
  task on its thread.
- Do not hand a region object to C and let it keep the pointer. The barrier
  check runs at managed stores only.

## The Julia API

The face is [`regions.jl`](regions.jl), a thin `ccall` wrapper. There is no
`Base` API: `include` the file and write `using .Regions`.

**Allocate**

| Call | Does |
| --- | --- |
| `@with_region n body` | Runs `body` with region `n` current; restores the previous region however the body leaves. |
| `region_set(n) -> previous` | The raw form. `n = 0` closes the window. |
| `region_current() -> n` | The region of the open window, 0 for none. |
| `@in_region_of container body` | Allocates the body's objects **where `container` lives**, not where the window points. For a replacement buffer of a container of your own. |
| `region_of(x) -> n` | The region an object lives in. |

**Free**

| Call | Does |
| --- | --- |
| `region_reset(n) -> pages` | Runs the region's finalizers, checks the execution roots, stops the world, frees. Returns the pages freed, or a negative code. |
| `unsafe_region_reset(n)` | The same free with no check and no pause. A reference left behind dangles. With a barrier-less build it is the fully trusted mode. |
| `region_reset_global(n)` | Frees region `n` on every thread heap at once, with the world stopped. For a region several threads filled. |
| `region_quarantined(n) -> Bool` | Has an escape quarantined this region? |

**Keep a region alive**

| Call | Does |
| --- | --- |
| `region_collect(n)` | A census: frees the dead cells of one region and keeps the live ones. Stops the world. |
| `region_collect_coop(n)` | The same, with no stop, when every other thread is parked. |
| `region_census_threshold!(pages)` | Past `pages`, the page claim of a window runs a census of the open region. |
| `region_pages(n)` | The pages the region holds on this heap. |

**Shape and machine**

| Call | Does |
| --- | --- |
| `region_parent!(child, parent)`, `region_tree!(parents)`, `region_parent_of(child)` | Declare and read the lifetime tree, before any of the regions is used. |
| `region_check(n)`, `region_debug(on)` | The reset's root check as a query, and extra reporting. |

A refusal is a negative return code, not an exception:

| Code | Means |
| --- | --- |
| −1 `EINVAL` | A bad region number, or a build without regions. |
| −2 `EBUSY` | The region is current, a window is open, or finalizers run here. |
| −5 `EQUARANTINED` | An escape quarantined the region. |
| −6 `EFINALIZERS` | Finalizers are pending; run a census first. |
| −7 `ECHILD` | A child region is live; reset the child first. |
| −8 `EROOT` | An execution root still points into the region. |

## Examples

**1. Scratch that dies every round.** The common case.

```julia
include("contrib/memory-regions/regions.jl"); using .Regions

const SCRATCH = 1

@noinline function step!(state)          # a frame, an event, a request
    @with_region SCRATCH begin
        work = [transform(x) for x in state.input]   # the temporaries of this step
        state.total += sum(work)                     # a number: no reference into the region
    end
end

for _ in 1:1_000_000
    step!(state)
    region_reset(SCRATCH)                # frees the temporaries of the step in one act
end
```

The reset must not run in a frame that still names one of the region's
objects. A Julia frame roots a local until the frame ends, so build and use
the objects in a function and reset after it returns.

**2. Read the answer of the reset.**

```julia
r = reinterpret(Int64, region_reset(SCRATCH))
if r < 0
    r == -8 && @warn "something still points into the region"
    r == -5 && @error "an escape quarantined it; the next collection takes it over"
else
    @info "freed" pages=r
end
```

**3. A container of your own that grows inside a window.**

```julia
mutable struct RingBuffer
    data::Memory{Float64}
end

function grow!(rb::RingBuffer)
    new = @in_region_of rb Memory{Float64}(undef, 2 * length(rb.data))
    copyto!(new, rb.data)
    rb.data = new          # legal: the buffer lives where the container lives
end
```

Without `@in_region_of` the new buffer lands in the open window's region, an
older container holds a younger object, and the barrier quarantines the
region for an ordinary `push!`. Base does this for its own containers
already; this is the same rule for yours.

**4. One leaf per worker, over a shared trunk.**

```julia
region_parent!(2, 1); region_parent!(3, 1)      # leaves 2 and 3 under trunk 1
@with_region 1 begin
    trunk = build_shared_model()                # lives as long as the run
end
Threads.@threads :static for w in 1:2
    leaf = w + 1
    for round in 1:1000
        run_round!(leaf, pointer_from_objref(trunk))   # not the object itself
        region_reset(leaf)
    end
end
region_reset_global(1)
```

Leaves 2 and 3 are siblings: neither may reference the other, and both may
reference the trunk. A worker takes the trunk as a raw pointer, because a
task closure that captured it would be a region-0 object that holds a
region-1 reference.

**5. A region that must live on.** One unit of work can be long and make
garbage inside itself. A census reclaims the dead cells without closing the
window:

```julia
region_census_threshold!(256)   # past 256 pages, a census of the open region
@with_region SEARCH begin
    explore(tree)               # allocates and discards inside the window
end
region_reset(SEARCH)
```

## How it works, in outline

| Mechanism | What it does |
| --- | --- |
| **The page tag** | Every pool page carries the region that claimed it; the stock sweep skips a tagged page. |
| **The window** | One pointer switch in the thread heap: the allocation fast path addresses another set of pool cursors. |
| **The barrier** | Every managed pointer store loads one flag. Armed, the barrier compares the page tags of the parent and the child, and quarantines the child's region when the store breaks the rule. |
| **The reset** | Runs the region's finalizers, then parks the page chain on the free list of the region; no object is touched. The checked entry stops the world and scans the execution roots of every thread first, and returns `EROOT` when one references into the region. |
| **The census** | A mark from the execution roots with a filter that claims only objects of the region, then a sweep of that region's pages. |
| **The tree** | Each region carries a bitset of its ancestors; the barrier test is one shift and one bit test. |

The stock collector is unchanged: it traces its heap, marks the region
objects it reaches without moving or freeing them, and frees the stock
objects that only region objects referenced.

## Where the evidence lives

The measurements, the benchmarks, the demonstrators, the cost study and the
history of this work are not part of this tree. They are on the branch
[`gc-regions-master-evidence`](https://github.com/levy/julia/tree/gc-regions-master-evidence/contrib/memory-regions)
of the fork: this tree plus `bench/`, `demo/`, `tools/`, `results/`,
`MEASUREMENTS.md`, `COST.md` and `HISTORY.md` in this directory, with the
scripts that made every number and the data they made. The first line of the
work, on release-1.13, is the branch `gc-regions-wip` of the same fork.

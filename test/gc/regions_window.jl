# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

const SIM = 1
const EVENT = 2
const RESERVE = 3

# Every entry refuses a region number outside 1 to MAX_REGIONS - 1 without
# touching any region's state.
function bad_region_numbers()
    for n in (-1, MAX_REGIONS, 99)
        check("set($n) refuses", region_set(n) == EINVAL)
        check("set($n) leaves region 0 current", region_current() == 0)
        check("reset($n) refuses", code(region_reset(n)) == EINVAL)
        check("reset_global($n) refuses", code(region_reset_global(n)) == EINVAL)
        check("collect($n) refuses", region_collect(n) == EINVAL)
        check("coop($n) refuses", region_collect_coop(n) == EINVAL)
        check("quarantined($n) is 0", quarantined(n) == 0)
        check("pages($n) is 0", region_pages(n) == 0)
    end
end

# The region that is current cannot be reset or censused, by any of the
# three census-like entries, and a global reset refuses while any window
# is open anywhere.
function current_region_refusals()
    region_set(SIM)
    check("reset of the current region refuses", code(region_reset(SIM)) == EBUSY)
    check("collect of the current region refuses", region_collect(SIM) == EBUSY)
    check("coop of the current region refuses", region_collect_coop(SIM) == EBUSY)
    check("reset_global with a window open refuses", code(region_reset_global(EVENT)) == EBUSY)
    region_set(0)
    check("region 1 is not quarantined by the refusal checks", quarantined(SIM) == 0)
end

# A heap makes the state of a region on the first window onto it. A valid
# region that no window ever opened on this heap has no state here, and
# every entry answers for it as for an empty region: nothing to reset,
# nothing to collect, nothing to check, and its place in the default chain.
const UNOPENED = 5

function no_window_defaults()
    check("set(0) with no window returns 0", region_set(0) == 0)
    check("reset of an untouched region returns 0", code(region_reset(RESERVE)) == 0)
    n = UNOPENED
    check("pages of a never-opened region is 0", region_pages(n) == 0)
    check("reset of a never-opened region returns 0", code(region_reset(n)) == 0)
    check("reset_global of a never-opened region returns 0", code(region_reset_global(n)) == 0)
    check("collect of a never-opened region refuses", region_collect(n) == EINVAL)
    check("coop of a never-opened region refuses", region_collect_coop(n) == EINVAL)
    check("check of a never-opened region finds nothing", region_check(n) == 0)
    check("verify of a never-opened region finds nothing", region_verify(n) == 0)
    check("a never-opened region is not quarantined", quarantined(n) == 0)
    check("a never-opened region keeps its chain parent", parent_of(n) == n - 1)
end

# jl_gc_region_set returns the region that was current, so a chain of
# opens and a close read back as the sequence of switches it made.
@noinline alloc_in_current() = Ref(1)

function set_returns_previous_and_reports_current()
    check("opening region 1 returns the previous region 0", region_set(SIM) == 0)
    check("region 1 is now current", region_current() == SIM)
    a = escape(alloc_in_current())
    check("an object allocated inside the window is in region 1", a == SIM)
    check("opening region 2 inside region 1 returns region 1", region_set(EVENT) == SIM)
    check("region 2 is now current", region_current() == EVENT)
    b = escape(alloc_in_current())
    check("an object allocated inside the nested window is in region 2", b == EVENT)
    check("closing the window returns region 2", region_set(0) == EVENT)
    check("no region is current after the close", region_current() == 0)
    c = escape(alloc_in_current())
    check("an object allocated after the close is in region 0", c == 0)
    check("region 1 is not quarantined", quarantined(SIM) == 0)
    check("region 2 is not quarantined", quarantined(EVENT) == 0)
end

# A region that holds objects reports a positive page count, and its reset
# both reclaims the pages and reports zero held afterward. In the default
# chain region 2 is the child of region 1: a live child refuses the
# parent's reset, so the child resets first.
function reset_holds_pages_then_chain()
    check("region 2 holds pages", region_pages(EVENT) > 0)
    check("the parent refuses its reset while the child is live", code(region_reset(SIM)) == ECHILD)
    check("region 1 still holds its pages after the refusal", region_pages(SIM) > 0)
    r2 = region_reset(EVENT)
    check("reset of a region that holds objects returns a positive page count", code(r2) > 0)
    check("region 2 holds no pages after its reset", region_pages(EVENT) == 0)
    check("region 1 holds pages", region_pages(SIM) > 0)
    r1 = region_reset(SIM)
    check("reset of region 1 returns a positive page count", code(r1) > 0)
    check("region 1 holds no pages after its reset", region_pages(SIM) == 0)
end

# A window belongs to the task that opened it: it survives a yield that
# parks the task, and it makes the task sticky to its thread for as long
# as it stays open. The handshake below stores no pointer (a Base.Event
# carries none), so it cannot itself trip the escape barrier.
function interleave()
    e1, e2 = Base.Event(), Base.Event()
    got = zeros(Int, 3)
    tA = Threads.@spawn begin
        was_sticky = current_task().sticky
        region_set(EVENT)
        check("opening a window makes the task sticky", current_task().sticky)
        a = Ref(1); got[1] = region_of(a)
        notify(e1); wait(e2)              # park the window; let B run
        b = Ref(2); got[2] = region_of(b)
        region_set(0)
        check("closing the window restores the previous stickiness", current_task().sticky == was_sticky)
        a[] + b[]
    end
    tB = Threads.@spawn begin
        wait(e1)                          # A's window is parked now
        c = Ref(3); got[3] = region_of(c)
        notify(e2)
        c[]
    end
    fetch(tA); fetch(tB)
    check("A allocates in its window before the yield", got[1] == EVENT)
    check("A allocates in its window after the yield", got[2] == EVENT)
    check("B allocates outside A's window", got[3] == 0)
end

# A window survives being spawned across every available thread, and each
# task's allocations keep landing in its own region across repeated yields.
function spawn_stress()
    n = Threads.nthreads() * 8
    ok = Threads.Atomic{Int}(0)
    Threads.@sync for k in 1:n
        Threads.@spawn begin
            region_set(EVENT)
            r = Ref(k)
            for _ in 1:4
                yield()
                region_of(r) == EVENT || error("window lost across yield")
                escape(Ref(0))         # allocate inside the window
            end
            region_set(0)
            Threads.atomic_add!(ok, 1)
        end
    end
    check("every spawned window survived its yields", ok[] == n)
end

function task_window_cases()
    interleave()
    spawn_stress()
    check("the event region is not quarantined", quarantined(EVENT) == 0)
    r = code(region_reset(EVENT))
    check("the event region resets (got $r)", r >= 0)
end

@noinline work(i) = (tmp = [Float64(i), 2.0, 3.0]; w = collect(1.0:4.0); sum(tmp) + sum(w))

# A reset every iteration, on a region that quiesces to zero pages between
# uses, must never mishandle the empty case, and no reset may refuse.
@noinline function reset_every_iteration(n)
    acc = 0.0
    refusals = 0
    for i in 1:n
        region_set(SIM)
        acc += work(i)
        region_set(0)
        refusals += refused(region_reset(SIM))
    end
    return acc, refusals
end

function reset_loop_case()
    reset_every_iteration(1000)          # warm up before the timed run
    GC.gc()
    r, refusals = reset_every_iteration(1_000_000)
    GC.gc()
    check("no reset of the loop refused (got $refusals)", refusals == 0)
    check("the reset loop accumulates the exact sum", r == 5.000155e11)
    check("region verify finds no inconsistency after the loop", region_verify(SIM) == 0)
end

# A window with nothing allocated inside it needs no reset: opening and
# closing it repeatedly must stay cheap and leave no pages behind.
@noinline function swap_only(n)
    acc = 0.0
    for i in 1:n
        region_set(SIM)
        acc += work(i)
        region_set(0)
    end
    return acc
end

function swap_only_case()
    swap_only(1000)                      # warm up before the timed run
    GC.gc()
    r = swap_only(1_000_000)
    GC.gc()
    check("the swap-only loop accumulates the exact sum", r == 5.000155e11)
    check("region verify finds no inconsistency after the loop", region_verify(SIM) == 0)
    check("a window with no allocation holds no pages", region_pages(SIM) == 0)
end

# A window opened at top level would leave the compiler's own allocations
# for the next top-level statement inside it; wrapping the allocation in
# its own function keeps the window's contents to just the array this
# case means to put there.
@noinline function make_live_array()
    region_set(SIM)
    a = [1.0, 2.0, 3.0]
    region_set(0)
    return a
end

function live_object_survives_gc()
    a = make_live_array()
    GC.gc(); GC.gc()
    check("a live region object survives two collections", sum(a) == 6.0)
    check("the object is still reported in its region", region_of(a) == SIM)
    check("region verify finds no inconsistency", region_verify(SIM) == 0)
    r = code(region_reset(SIM))
    check("the region resets (got $r)", r >= 0)
end

# Code that runs for the first time inside a window makes the runtime
# allocate on the code's behalf: a method compiles; a dynamic dispatch on a
# signature the method cache has not seen builds the argument tuple type
# and a cache entry; a type first instantiated at run time goes into the
# type cache. All of that belongs to region 0, whatever window is open, or
# the runtime's own tables would hold references into the region and the
# barrier would quarantine it. Four dynamic first-time paths, none warmed
# before the window opens, next to a static call as the control.
struct FirstStatic; x::Int; end
struct FirstDynamic; x::Int; end
struct FirstLatest; x::Int; end
struct FirstParam; x::Int; end
struct FirstTuple; x::Int; end
@noinline first_static(v) = v.x + 1
@noinline first_dynamic(v) = v.x * 2
@noinline first_latest(v) = v.x - 3

@noinline function first_time_code_in_window()
    region_set(SIM)
    r = first_static(FirstStatic(1))                    # static: inferred with its caller
    boxed = Any[FirstDynamic(2), FirstParam, FirstTuple(4)]
    r += first_dynamic(boxed[1])                        # dynamic dispatch on a signature not in the cache
    r += Base.invokelatest(first_latest, FirstLatest(3))
    r += length(Vector{boxed[2]}(undef, 2))             # Vector{FirstParam}, instantiated now
    r += (boxed[3], 5)[2]                               # Tuple{FirstTuple, Int}, instantiated now
    region_set(0)
    return r
end

function first_time_code_stays_in_region_0()
    r = first_time_code_in_window()
    check("the first-time calls compute their sum", r == 13)
    check("first-time code inside a window does not quarantine the region", quarantined(SIM) == 0)
    check("the region resets after first-time code ran inside it", !refused(region_reset(SIM)))
end

# Base keeps lazily initialized state - a value made once per process or
# once per thread, in a table that outlives every window - and makes it on
# behalf of whatever task first needs it, inside that task's window when
# it holds one. The case that reaches every program is the scheduler's:
# the first idle wait on a thread makes the thread's scheduler task and
# its sticky work queue. All of it belongs to region 0, or the table would
# reference into the region and the barrier would quarantine it.
const lazy_per_thread = Base.OncePerThread{Base.RefValue{Int}}() do
    Ref(1)
end
const lazy_per_process = Base.OncePerProcess{Base.RefValue{Int}}() do
    Ref(2)
end

@noinline function lazy_state_in_window()
    region_set(SIM)
    a = lazy_per_thread()
    b = lazy_per_process()
    s = Base.get_sched_task()
    q = Base.workqueue_for(Threads.threadid())
    r = Ref(3)                                          # the window still allocates here
    region_set(0)
    return region_of(a), region_of(b), region_of(s), region_of(q), region_of(r)
end

function lazy_state_stays_in_region_0()
    ra, rb, rs, rq, rr = lazy_state_in_window()
    check("a per-thread value first made inside a window is in region 0", ra == 0)
    check("a per-process value first made inside a window is in region 0", rb == 0)
    check("the thread's scheduler task is in region 0", rs == 0)
    check("the thread's sticky work queue is in region 0", rq == 0)
    check("the window allocates in its region after the lazy state was made", rr == SIM)
    check("lazy state made inside a window does not quarantine the region", quarantined(SIM) == 0)
    check("the region resets after lazy state was made inside it", !refused(region_reset(SIM)))
end

# A lookup of a global name that no binding exists for yet makes the
# binding and, lazily, its partition. Both are stored into stock tables (the
# module's binding vector, the binding itself), so the runtime makes them in
# region 0 whatever window is open; otherwise the store is an escape and the
# region is quarantined by a lookup. The name is built at run time, so the
# compiler cannot resolve the binding when it compiles the function.
@noinline function lookup_of_new_name_in_window(stem)
    region_set(SIM)
    name = Symbol(stem, "_seen_first_inside_a_window")
    r = try
        getglobal(@__MODULE__, name)
        :defined
    catch e
        e isa UndefVarError ? :undefined : :other
    end
    region_set(0)
    return r, name
end

function runtime_binding_stays_in_region_0()
    r, name = lookup_of_new_name_in_window("never")
    check("the new name is undefined", r == :undefined)
    b = ccall(:jl_get_binding, Any, (Any, Any), @__MODULE__, name)
    check("a binding made inside a window is in region 0", region_of(b) == 0)
    check("its partition is in region 0", region_of(b.partitions) == 0)
    check("a lookup of a new name inside a window does not quarantine the region", quarantined(SIM) == 0)
    check("the region resets after the lookup", !refused(region_reset(SIM)))
    Core.eval(@__MODULE__, :(global $name = 7))
    # The definition is in a newer world than this function; read it there.
    check("the binding takes a value after the reset",
          Base.invokelatest(getglobal, @__MODULE__, name) == 7)
end

# The first throw on a task makes the task's exception stack, a buffer the
# task keeps and every later throw reuses. Made inside a window, it would be
# a region buffer held by a region-0 task: an escape at the first throw
# inside a window, and a quarantine. A fresh task has no exception stack
# yet, so the case throws for the first time inside its window. The task is
# sticky, so its window and its buffer are on this thread's heap.
@noinline function first_throw_in_window()
    region_set(SIM)
    caught = try
        throw(ErrorException("the first throw inside a window"))
    catch e
        e isa ErrorException
    end
    region_set(0)
    return caught
end

function first_throw_stays_in_region_0()
    t = Task(first_throw_in_window)
    schedule(t)
    check("the task caught its first throw inside the window", fetch(t) === true)
    check("the first throw inside a window does not quarantine the region", quarantined(SIM) == 0)
    check("the region resets after the first throw inside it", !refused(region_reset(SIM)))
end

# The reserve claims page blocks up front so a later allocation inside a
# region never first-touch-faults; a second call must be a no-op, not an
# error, and a window can still allocate normally afterward.
@noinline function fill_with_refs(n)
    region_set(RESERVE)
    v = Vector{Any}(undef, n)
    for i in 1:n
        v[i] = Ref(i)
    end
    region_set(0)
    return v
end

# The vector and its refs stay in this frame, which has returned before the
# reset runs: the reset checks the execution roots, and a local that still
# names a region object refuses it.
@noinline function every_ref_is_in_the_region(n)
    v = fill_with_refs(n)
    return all(x -> region_of(x) == RESERVE, v)
end

function heap_reserve_then_window()
    r1 = heap_reserve(64 << 20)
    check("the reserve returns a byte count, not a refusal", !refused(r1))
    r2 = heap_reserve(64 << 20)
    check("a second reserve call also returns without error", !refused(r2))
    check("every ref landed in the window's region", every_ref_is_in_the_region(10_000))
    check("the window resets without a refusal", !refused(reset_via_call(RESERVE)))
end

bad_region_numbers()
current_region_refusals()
no_window_defaults()
set_returns_previous_and_reports_current()
reset_holds_pages_then_chain()
task_window_cases()
reset_loop_case()
swap_only_case()
live_object_survives_gc()
first_time_code_stays_in_region_0()
lazy_state_stays_in_region_0()
runtime_binding_stays_in_region_0()
first_throw_stays_in_region_0()
heap_reserve_then_window()

finish("regions_window")

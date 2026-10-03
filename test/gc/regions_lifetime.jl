# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

const SIM = 1
const EVENT = 2

mutable struct Payload
    x::Int
end
const seen = Int[]        # region 0; each finalizer records its own tag here

@noinline function register_four_finalizers()
    region_set(EVENT)
    for i in 1:4
        p = Payload(i)
        finalizer(q -> push!(seen, q.x), p)
    end
    region_set(0)
end

function reset_runs_every_finalizer()
    empty!(seen)
    register_four_finalizers()
    r = region_reset(EVENT)
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    check("the reset ran every finalizer, on a whole object", sort(seen) == [1, 2, 3, 4])
end

@noinline function register_one_live_and_three_dead()
    region_set(SIM)
    live = Payload(100)
    finalizer(q -> push!(seen, q.x), live)
    for i in 10:12
        p = Payload(i)
        finalizer(q -> push!(seen, q.x), p)
    end
    region_set(0)
    freed = region_collect_coop(SIM)
    return freed, live.x
end

function census_runs_only_the_dead()
    # The stock collector must not run its own finalizer pass while this
    # case asserts exactly which finalizers the cooperative census ran, so
    # it is held off for the duration of the case.
    GC.enable(false)
    empty!(seen)
    freed, livex = register_one_live_and_three_dead()
    check("the cooperative census ran (got $freed)", freed >= 0)
    check("the census ran the dead finalizers only", sort(seen) == [10, 11, 12])
    # The census keeps a finalized object for one more cycle; the next
    # census frees it and runs nothing new.
    freed2 = region_collect_coop(SIM)
    check("the next census freed the finalized objects (got $freed2)", freed2 >= 3)
    check("the live object stayed whole", livex == 100)
    r = region_reset(SIM)
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    check("the reset then ran the survivor's finalizer", sort(seen) == [10, 11, 12, 100])
    GC.enable(true)
end

const RESULT = Int[]

# A closure that captures an array is not a singleton: each call makes a
# fresh box. The box is made where the closure is first passed as Any --
# inside the window, when the closure reaches `finalizer` unboxed (a
# region object whose captured array is itself a region-0 object), or
# outside it, when the closure is stored into a `Vector{Any}` first (a
# region-0 object that only the region's finalizer list references).
# Both shapes must survive a stock collection before the region resets.
@noinline function make_closure(tag)
    cap = [tag, tag + 1]
    return x -> push!(RESULT, sum(cap))
end
@noinline function make_gc_closure(tag)
    cap = [tag, tag + 1]
    return x -> (GC.gc(); churn(); push!(RESULT, sum(cap)))
end

@noinline function register_finalizers(fs...)
    region_set(SIM)
    for f in fs
        o = Ref(1)
        finalizer(f, o)
        escape(o)
    end
    region_set(0)
    nothing                           # every object here is already dead
end

function closures_survive_stock_collections()
    empty!(RESULT)
    register_finalizers(make_closure(10), Any[make_closure(20)]...)
    GC.gc(); GC.gc()                  # only the region's finalizer list references the closures now
    churn()
    GC.gc(false); GC.gc()             # a young pass right after a full one
    churn()
    r = region_reset(SIM)
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    check("both closure shapes survived the collections (got $RESULT)", sort(RESULT) == [21, 41])
end

# The reset runs a region's finalizer list in reverse registration order.
# An entry not yet run must survive a collection that an earlier entry in
# the same reset triggers.
function reset_runs_finalizers_in_reverse_order()
    empty!(RESULT)
    register_finalizers(make_closure(100), make_closure(200), make_gc_closure(300))
    r = region_reset(SIM)
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    check("the entries ran in reverse order, and the ones not yet run survived a collection a finalizer triggered",
          RESULT == [601, 401, 201])
end

const global_reset_finalizer_ran = Ref(0)
@noinline function setup_pending_finalizer()
    region_set(SIM)
    o = Ref(1)
    finalizer(x -> (global_reset_finalizer_ran[] += 1), o)
    r = escape(o)                      # o escapes: the finalizer is registered, not inlined
    region_set(0)
    return r
end

function reset_global_refuses_with_pending_finalizers()
    global_reset_finalizer_ran[] = 0
    check("the object is in its region", setup_pending_finalizer() == SIM)
    r = region_reset_global(SIM)
    check("reset_global refuses while a finalizer is pending (code $(code(r)))", code(r) == EFINALIZERS)
    check("the finalizer did not run", global_reset_finalizer_ran[] == 0)
    freed = region_collect_coop(SIM)
    check("the cooperative census ran (got $freed)", freed >= 0)
    check("the census ran the pending finalizer", global_reset_finalizer_ran[] == 1)
    r2 = region_reset_global(SIM)
    check("reset_global then succeeds (code $(code(r2)))", code(r2) >= 0)
    check("the finalizer ran exactly once", global_reset_finalizer_ran[] == 1)
end

# A finalizer can register a finalizer on another object of the region. The
# reset takes the list again until it stays empty, so the second one runs in
# the same reset, before the free. The frame of the registering function
# has returned before the reset: nothing else roots the two objects.
const chain_seen = Int[]
@noinline function register_chained_finalizers()
    region_set(EVENT)
    a = Payload(1)
    b = Payload(2)
    finalizer(a) do q
        push!(chain_seen, q.x)
        finalizer(p -> push!(chain_seen, p.x), b)   # registered while the reset runs the list
    end
    escape(a)
    region_set(0)
    return nothing
end

function reset_runs_a_finalizer_registered_by_a_finalizer()
    empty!(chain_seen)
    register_chained_finalizers()
    r = region_reset(EVENT)
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    check("both finalizers ran, the second one registered by the first", chain_seen == [1, 2])
end

# A finalizer that registers itself again every round would keep the reset
# in its finalizer phase forever. The phase is bounded: the reset refuses
# with EFINALIZERS and leaves the region whole, and the next reset takes
# the list up again. The relay stops after 100 rounds, so the second reset
# drains it and frees.
const relay_rounds = Ref(0)
function relay(q)
    relay_rounds[] += 1
    relay_rounds[] < 100 && finalizer(relay, q)
end

@noinline function register_relay()
    region_set(EVENT)
    q = Payload(3)
    finalizer(relay, q)
    escape(q)
    region_set(0)
    return nothing
end

function reset_bounds_a_finalizer_that_registers_forever()
    relay_rounds[] = 0
    register_relay()
    r = region_reset(EVENT)
    check("the reset refuses a relay that outlives its bound (code $(code(r)))", code(r) == EFINALIZERS)
    check("the reset ran a bounded number of rounds (ran $(relay_rounds[]))", relay_rounds[] == 64)
    r2 = region_reset(EVENT)
    check("the next reset drains the relay and frees (code $(code(r2)))", code(r2) >= 0)
    check("the relay ran to its end (ran $(relay_rounds[]))", relay_rounds[] == 100)
    check("the region is not quarantined", quarantined(EVENT) == 0)
end

const SINK = Any[]
mutable struct Handle
    x::Int
end
@noinline function make_finalizable()
    h = Handle(7)
    finalizer(h) do h
        push!(SINK, Ref(h.x))          # allocates, and stores into region 0
    end
    escape(h)                          # h escapes: the finalizer is registered, not inlined
    nothing
end

# A stock finalizer runs with region 0 installed, whatever window the
# thread holds, so an allocation it makes -- and a store of that
# allocation into region-0 state -- must never touch the open region.
@noinline function stock_finalizer_inside_a_window()
    empty!(SINK)
    make_finalizable()
    region_set(SIM)
    GC.gc()                            # the handle dies; the finalizer runs after the collection
    for _ in 1:3                       # the finalizer may run at a later safepoint
        isempty(SINK) || break
        GC.gc()
    end
    region_set(0)
    check("the finalizer ran", length(SINK) >= 1)
    check("the finalizer allocated in region 0", all(r -> region_of(r) == 0, SINK))
    check("the open region is not quarantined by a finalizer it never saw run", quarantined(SIM) == 0)
    r = region_reset(SIM)
    check("the reset succeeded (got $(code(r)))", code(r) >= 0)
end

# The consumer keeps the compiler from deleting a dead allocation outright.
@noinline consume_malloced(v) = (v[1] = 0.0; nothing)

# The memory of a 32 KB vector is malloc'd, not pooled: the reset must free
# it, or the churn below leaks 640 MB.
@noinline function malloced_churn(iters)
    refusals = 0
    for i in 1:iters
        region_set(SIM)
        v = Vector{Float64}(undef, 4096)
        v[1] = i; v[end] = i
        consume_malloced(v)
        region_set(0)
        refusals += refused(region_reset(SIM))
    end
    return refusals
end

function reset_frees_mallocd_data()
    malloced_churn(10)                        # warm up before the RSS is sampled
    GC.gc()
    rss0 = Sys.maxrss()
    refusals = malloced_churn(20_000)
    check("no reset of the churn refused (got $refusals)", refusals == 0)
    check("the reset frees malloc'd data (RSS bound)", Sys.maxrss() - rss0 < 100e6)
    GC.gc()                                   # a full collection after the churn finds no dangling memory
end

# A global cannot hold a region object, so the live and dead memories here
# are frame locals of the function that runs the census.

@noinline function malloced_census_inner()
    region_set(SIM)
    keep = [Vector{Float64}(undef, 1024) for _ in 1:8]
    for (i, v) in enumerate(keep); fill!(v, i); end
    for _ in 1:64; consume_malloced(Vector{Float64}(undef, 1024)); end   # dead
    region_set(0)
    freed = region_collect_coop(SIM)
    live_ok = all(keep[i][7] == i for i in 1:8)
    return freed, live_ok
end

function census_frees_dead_mallocd_memories()
    GC.enable(false)
    freed, live_ok = malloced_census_inner()
    check("the census freed the dead memories (got $freed)", freed >= 64)
    check("the live memories kept their data", live_ok)
    r = region_reset(SIM)          # the frame that referenced the survivors is gone now
    check("the reset succeeded (code $(code(r)))", code(r) >= 0)
    GC.enable(true)
end

mutable struct Box
    v::Int
end
@noinline make_in_region(n) = (region_set(n); x = Box(7); region_set(0); x)

# The region object stays in this frame, which has returned before the reset
# runs: the reset checks the execution roots, and a local that still names a
# region object refuses it.
@noinline function try_weakref_in_its_own_frame()
    x = make_in_region(SIM)
    threw = false
    message = ""
    try
        WeakRef(x)
    catch e
        threw = true
        message = sprint(showerror, e)
    end
    return (threw, message)
end

function weakref_to_region_object_is_refused()
    threw, message = try_weakref_in_its_own_frame()
    check("a WeakRef to a region object is refused", threw)
    check("the error message mentions region", occursin("region", message))
    check("the refused WeakRef did not quarantine the region", quarantined(SIM) == 0)
    r = code(reset_via_call(SIM))
    check("the region still resets (got $r)", r >= 0)
end

mutable struct Rec
    x::Int
end
@noinline consume_rec(v) = (v[1] = 0.0; nothing)

@noinline function stock_gc_body()
    region_set(SIM)
    keep = [Rec(i) for i in 1:1000]
    region_set(0)

    # Collections with a live region and an OPEN window: the window, and
    # everything already allocated into it, must survive them, and the
    # window itself must stay the current region throughout.
    region_set(EVENT)
    scratch = [Rec(-i) for i in 1:100]
    GC.gc(false)
    check1 = region_current() == EVENT
    GC.gc(true)
    check2 = region_current() == EVENT
    ok_scratch = all(scratch[i].x == -i for i in 1:100)
    region_set(0)
    event_reset_code = code(region_reset(EVENT))

    # Collections between windows: region-0 garbage is reclaimed while a
    # region lives, and the region's own objects stay whole.
    pauses0 = Base.gc_num().pause
    for _ in 1:50
        consume_rec(Vector{Float64}(undef, 4096))   # region-0 garbage
    end
    GC.gc(false)
    GC.gc(true)
    pauses1 = Base.gc_num().pause
    ok_keep1 = all(keep[i].x == i for i in 1:1000)

    # The census after those collections must be exact: kill half the
    # records, then count the frees; a stale mark would corrupt this.
    for i in 1:2:1000
        keep[i] = Rec(10_000 + i)               # 500 dead records
        i % 100 == 1 && GC.gc(false)            # force the mid-loop case
    end
    freed = region_collect_coop(SIM)
    ok_keep2 = all(i -> keep[i].x == (isodd(i) ? 10_000 + i : i), 1:1000)
    return check1, check2, ok_scratch, event_reset_code, pauses1 - pauses0, ok_keep1, freed, ok_keep2
end

function stock_collections_coexist_with_live_regions()
    check1, check2, ok_scratch, event_reset_code, pauses, ok_keep1, freed, ok_keep2 = stock_gc_body()
    check("the window stays current across a young collection inside it", check1)
    check("the window stays current across a full collection inside it", check2)
    check("the open-window scratch survived the collections", ok_scratch)
    check("the event region reset succeeded (code $event_reset_code)", event_reset_code >= 0)
    check("the collections actually ran (got $pauses)", pauses >= 2)
    check("the live records survived the collections", ok_keep1)
    check("the census after stock collections is exact (got $freed)", freed >= 500)
    check("the records after the census are whole", ok_keep2)
    r = region_reset(SIM)
    check("the region resets after the census (code $(code(r)))", code(r) >= 0)
end

# A forced full collection right after a young one runs two passes.
# Region pages are never swept by the stock collector, so a region object
# marked in the first pass must not read as already-visited in the
# second: its region-0 children must still be traversed and kept alive.
mutable struct Holder
    a::Vector{Int}
end
@noinline function make_holder_with(cap)
    region_set(SIM)
    h = Holder(cap)
    region_set(0)
    return h
end

@noinline function second_pass_survives_alternating_collections()
    cap = [10, 11]
    h = make_holder_with(cap)
    cap = nothing
    check("the holder is in its region", region_of(h) == SIM)
    check("the array is in region 0", region_of(h.a) == 0)
    for i in 1:6
        GC.gc(i % 2 == 0)                  # quick, full, quick, full, quick, full
        churn(200_000)                     # reuse whatever the collection freed
        check("the array survived collection $i", sum(h.a) == 21)
    end
    h = nothing
    check("the reset succeeded", code(region_reset(SIM)) >= 0)
end

reset_runs_every_finalizer()
census_runs_only_the_dead()
closures_survive_stock_collections()
reset_runs_finalizers_in_reverse_order()
reset_global_refuses_with_pending_finalizers()
reset_runs_a_finalizer_registered_by_a_finalizer()
reset_bounds_a_finalizer_that_registers_forever()
stock_finalizer_inside_a_window()
reset_frees_mallocd_data()
census_frees_dead_mallocd_memories()
weakref_to_region_object_is_refused()
stock_collections_coexist_with_live_regions()
second_pass_survives_alternating_collections()

finish("regions_lifetime")

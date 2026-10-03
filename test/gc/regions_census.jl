# This file is a part of Julia. License is MIT: https://julialang.org/license

# The census: it frees the dead objects of a region and keeps the live
# ones, whether it stops the world or runs cooperatively on one thread.
# It must see a task's stack, a finalizer's own allocations during the
# census that runs it, and a stack root the optimizer pins across a
# safepoint; and it must leave the stock collector's state as it found
# it: no mark bit on an object outside the region, no entry in a remset.
# No case in this script quarantines a region: every case ends with a
# clean reset.
include(joinpath(@__DIR__, "regions_api.jl"))

# ---- collect and coop refuse a bad region number and the current region ----

@noinline function census_bad_number_refusals()
    for n in (-1, MAX_REGIONS, 99)
        check("collect($n) refuses with EINVAL", region_collect(n) == EINVAL)
        check("coop($n) refuses with EINVAL", region_collect_coop(n) == EINVAL)
    end
end

@noinline function census_current_region_refusal()
    region_set(1)
    check("collect of the current region refuses", region_collect(1) == EBUSY)
    check("coop of the current region refuses", region_collect_coop(1) == EBUSY)
    region_set(0)
    check("region 1 not quarantined", quarantined(1) == 0)
    check("the empty window resets", !refused(region_reset(1)))
end

# ---- a census refuses while a child region is live ----
# A child holds legal references into its parent, and the census filter
# drops the child's objects at the claim: a census of the parent would
# never mark a parent object that only the child references, and would
# free it. The reset refuses this case with ECHILD; so must every census
# entry, and the allocator's census of the open region must skip.

@noinline function census_build_child_holder()
    region_set(1)
    a = [Ref(i) for i in 1:1000]
    region_set(2)
    b = Ref(a)                        # a region-2 object, the only holder of a
    region_set(0)
    return b
end

@noinline function census_live_child_refusal()
    b = census_build_child_holder()
    check("collect of a region with a live child refuses", region_collect(1) == ECHILD)
    check("coop of a region with a live child refuses", region_collect_coop(1) == ECHILD)
    # The allocator's census of the open region: region 1 is open again
    # while its child 2 is live, and the page count passes the threshold.
    census_threshold!(4)
    region_set(1)
    junk = [Ref(-1) for _ in 1:20_000]
    region_set(0)
    census_threshold!(0)
    check("the parent's objects survive (sum $(sum(x[] for x in b[])))",
          sum(x[] for x in b[]) == 500_500)
    check("the junk is intact", all(j -> j[] == -1, junk))
    check("regions 1 and 2 not quarantined", quarantined(1) == 0 && quarantined(2) == 0)
    check("reset of the parent refuses while the child is live", code(region_reset(1)) == ECHILD)
    return nothing
end

# The frame that held the child object is gone; the child resets, and the
# parent is collectable and resettable again.
@noinline function census_live_child_cleanup()
    check("the child resets", !refused(region_reset(2)))
    check("the parent collects once the child is gone", region_collect(1) >= 0)
    check("the parent resets", !refused(region_reset(1)))
end

# ---- the census must not leave a mark on an object outside the region ----
# The stock mark loop marks the LAST pointer field of an object in place,
# without a push. A census must route that field through its filter too:
# a region-0 array reached only through the last field of a region object
# keeps its mark bits. A stale mark would make the next stock collection
# skip the array's scan and free the elements under it.

mutable struct CensusNode
    a::Int
    v::Vector{Base.RefValue{Int}}     # the last pointer field
end

@noinline function census_make_holder(v)
    region_set(1)
    node = CensusNode(1, v)           # a region-1 object with a region-0 last field
    region_set(0)
    return node
end

@noinline function census_last_field_leak()
    GC.gc()
    v = [Ref(i) for i in 1:50_000]    # young: allocated after the collection
    expected = sum(r[] for r in v)
    node = census_make_holder(v)
    freed = region_collect(1)         # the census walks node, then node.v
    check("the census ran (got $freed)", freed >= 0)
    GC.gc(false)                      # a young collection: the elements must survive
    churn()                           # reuse whatever was freed
    GC.gc()
    churn()
    check("the elements of the last field survived", sum(r[] for r in node.v) == expected)
    check("the array is the same object", node.v === v)
    check("region 1 not quarantined", quarantined(1) == 0)
    check("region 1 resets", !refused(region_reset(1)))
end

# ---- the census must not leave an entry in a remset ----
# The stock task scan re-adds an old task to the remset of the marking
# thread. A census scans every task but marks none of them; a task it
# left in a remset enters the next stock collection as a remset object,
# whose scan sets no page metadata, and the sweep frees the task's page
# as empty. The object at risk is the root task of a GC mark thread: it
# sits alone on its page, so no other mark keeps the page. The run_gctest
# matrix supplies the mark threads (JULIA_NUM_GC_THREADS). The fault shows
# as a crash in a later collection, once churn reuses the freed page; that
# collection can be one of the next case.

@noinline function census_leaves_remsets(census)
    GC.gc()                           # every task is old
    node = census_make_holder(Ref{Int}[])
    freed = census(1)
    check("the census ran (got $freed)", freed >= 0)
    GC.gc(false)                      # the remset entries enter this collection
    churn()                           # reuse the pages the sweep freed
    GC.gc()
    churn()
    GC.gc()                           # a freed root task dies here
    check("the region object is intact", node.a == 1)
    check("region 1 not quarantined", quarantined(1) == 0)
    check("region 1 resets", !refused(region_reset(1)))
end

# ---- the census records every task it meets, however many there are ----
# 5000 young tasks park with a heap array on their own stack. A census
# marks each task once to scan its stack; a task the census fails to
# record is skipped by the next stock collection, which then frees what
# its stack references.

const CENSUS_NTASKS = 5000
const census_park = Base.Event()
const census_results = fill(false, CENSUS_NTASKS)

@noinline function census_worker(k)
    arr = [Ref(k * j) for j in 1:32]     # referenced from this stack only
    s0 = sum(r[] for r in arr)
    wait(census_park)
    census_results[k] = sum(r[] for r in arr) == s0
    nothing
end

@noinline function census_scratch_in_region()
    region_set(1)
    scratch = [Ref(i) for i in 1:1000]
    region_set(0)
    return length(scratch)
end

function census_many_tasks()
    GC.gc()
    GC.enable(false)                 # the tasks stay young until the census
    tasks = [Threads.@spawn census_worker(k) for k in 1:CENSUS_NTASKS]
    for _ in 1:3
        yield()                      # every worker reaches wait(census_park)
    end
    GC.enable(true)
    census_scratch_in_region()
    freed = region_collect(1)        # the census walks the 5000 task stacks
    check("the census ran (got $freed)", freed >= 0)
    GC.gc(); GC.gc()                 # a skipped task loses its array here
    churn()                          # reuse the freed cells
    notify(census_park)
    foreach(wait, tasks)
    check("every task's array survived", all(census_results))
    check("region 1 not quarantined", quarantined(1) == 0)
    check("region 1 resets", !refused(region_reset(1)))
end

# ---- a finalizer the cooperative census runs may itself allocate ----
# The allocation can trigger a stock collection; that collection must see
# the whole heap, not the census's region filter. The census runs a dead
# object's finalizer after its sweep, with the filter off, on an object
# it kept marked for one more cycle so the finalizer's target is valid.

const census_fin_ran = Ref(0)

@noinline function census_setup_finalizer()
    region_set(1)
    o = Ref(1)
    finalizer(x -> (churn(4_000_000); census_fin_ran[] += 1), o)
    r = escape(o)                        # o escapes: the finalizer is registered, not inlined away
    region_set(0)
    return r
end

function census_coop_finalizer_alloc()
    check("the object is in region 1", census_setup_finalizer() == 1)
    live = [rand(100) for _ in 1:2000]
    sums = sum.(live)
    freed = region_collect_coop(1)
    check("the census ran (got $freed)", freed >= 0)
    check("the finalizer ran", census_fin_ran[] == 1)
    GC.gc()
    churn()
    check("the heap is intact", sum.(live) == sums)
    check("the next census frees the finalized object", region_collect_coop(1) >= 1)
    check("region 1 not quarantined", quarantined(1) == 0)
    check("region 1 resets", !refused(region_reset(1)))
end

# ---- a spawned task parks with its window closed; a lost cooperative
#      race retries ----
# The cooperative census refuses with EUNSAFE while another thread runs
# managed code, and the caller retries at its next boundary. On one
# thread the parked task shares the census's own thread heap, which is
# the discriminating case; with more threads the task's records live on
# another heap and the census never touches them.

mutable struct CensusRec
    x::Int
end

@noinline function census_park_task_setup()
    # Initialize region 1 on this thread too, so the census's return is
    # meaningful whether or not the spawned task lands on this thread.
    region_set(1)
    region_set(0)
end

@noinline function census_spawn_parked(parked, release)
    region_set(1)
    recs = [CensusRec(i) for i in 1:100]
    region_set(0)
    notify(parked)                       # park: window closed, refs live on this task's stack
    wait(release)
    all(recs[i].x == i for i in 1:100)
end

function census_parked_task()
    census_park_task_setup()
    parked, release = Base.Event(), Base.Event()
    tA = Threads.@spawn census_spawn_parked(parked, release)
    wait(parked)
    freed = EUNSAFE
    for _ in 1:100
        freed = region_collect_coop(1)
        freed != EUNSAFE && break
        sleep(0.01)
    end
    check("the census ran (got $freed)", freed >= 0)
    notify(release)
    check("the parked task's records survived the census", fetch(tA))
    check("region 1 not quarantined", quarantined(1) == 0)
    check("region 1 resets", !refused(region_reset(1)))
end

# ---- the debug check: a reset refuses while an execution root still
#      references into the region ----
# With the check on, a finalizer registered on a region object still goes
# to the region's own list and runs at the reset; and a reset refuses
# while a stack root is pinned into the region, succeeding once the root
# goes out of scope.

const census_debug_fin_ran = Ref(0)
@noinline census_debug_count_fin(x) = (census_debug_fin_ran[] += 1; nothing)

mutable struct CensusThing
    x::Int
end

@noinline function census_finalizer_on_region()
    region_set(1)
    t = CensusThing(1)
    escape(t)                            # materialize t on the heap inside the window,
                                          # so the allocation is not sunk past the close
    finalizer(census_debug_count_fin, t)
    region_set(0)
    nothing
end

@noinline function census_hold_and_try()
    region_set(1)
    obj = Ref(42)
    rof = escape(obj)                    # a boxing use: obj must exist as a heap object
    region_set(0)
    r = GC.@preserve obj region_reset(1) # obj is pinned as a root across the reset
    v = obj[]
    return rof, r, v
end

function census_debug_reset_precondition()
    region_debug!(1)

    fin_before = census_debug_fin_ran[]
    census_finalizer_on_region()
    r1 = region_reset(1)
    check("the region finalizer runs at the reset",
          !refused(r1) && census_debug_fin_ran[] == fin_before + 1)

    rof, r2, v = census_hold_and_try()
    check("the object lives in the region", rof == 1)
    check("the reset refuses while the root is pinned", code(r2) == EROOT && v == 42)
    check("the reset succeeds once the root is gone", !refused(region_reset(1)))
    check("the debug check reports no live references now", region_check(1) == 0)

    region_debug!(0)
end

# ---- the growth bound: a census armed at a low threshold keeps a churn's
#      dead pool objects from piling up inside one long window ----
# Each round replaces the last round's batch; only a small accumulator on
# the stack needs to survive. Disarmed, the region holds every dead batch
# until the reset, so it grows with the whole run. Armed at a low page
# threshold, the allocator's own maybe-census call reclaims the dead
# batches as they die, so the region stays near the threshold instead.

mutable struct CensusSm
    v::Int
end

@noinline function census_bound_churn(rounds, per)
    region_set(1)
    acc = 0
    peak = 0
    for r in 1:rounds
        batch = Vector{CensusSm}(undef, per)   # the previous batch is now dead
        @inbounds for i in 1:per
            batch[i] = CensusSm((r * i) & 0xffff)
        end
        s = 0
        @inbounds for i in 1:per
            s += batch[i].v
        end
        acc += s
        p = region_pages(1)
        p > peak && (peak = p)
    end
    region_set(0)
    check("region 1 not quarantined", quarantined(1) == 0)
    region_reset(1)
    return acc, peak
end

const CENSUS_BOUND_ROUNDS = 40000
const CENSUS_BOUND_PER = 256           # 256 pool objects per batch, 40000 batches

function census_growth_bound()
    census_bound_churn(100, CENSUS_BOUND_PER)   # warm: churn compiles at region 0 first

    census_threshold!(0)                        # disarmed: the region holds every batch
    acc_off, peak_off = census_bound_churn(CENSUS_BOUND_ROUNDS, CENSUS_BOUND_PER)

    census_threshold!(64)                        # armed at ~1 MB of pages
    acc_on, peak_on = census_bound_churn(CENSUS_BOUND_ROUNDS, CENSUS_BOUND_PER)
    freed = region_stat(5)                       # cells freed by the armed run's last census
    census_threshold!(0)

    check("same accumulator both ways", acc_off == acc_on)
    check("the armed census bounds the region", peak_on < peak_off ÷ 4)
    check("the armed census keeps it near the threshold", peak_on < 4000)
    check("the armed run's census freed cells", freed > 0)
end

# ---- driver: the cases run in order, each leaving region 1 clean ----

census_bad_number_refusals()
census_current_region_refusal()
census_live_child_refusal()
census_live_child_cleanup()
census_last_field_leak()
census_leaves_remsets(region_collect)
census_leaves_remsets(region_collect_coop)
census_many_tasks()
census_coop_finalizer_alloc()
census_parked_task()
census_debug_reset_precondition()
census_growth_bound()

finish("regions_census")

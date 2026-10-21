# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

# The safety of the reset itself.
#
# 1. The reset checks the execution roots and refuses one that references
#    into the region. The unsafe entry does not check.
# 2. A finalizer of the region can quarantine the region while the reset
#    runs it, and the reset must not free the pages after that.
# 3. A task that dies inside a window must not leave the window open for
#    the life of the process.
# 4. A window on a quarantined region is refused, so a program that ignores
#    the printed line fails at its next window instead of at its memory
#    limit.

const ROOTED = 1
const FINAL = 2
const DEAD = 3
const BURNED = 4
const PAUSED = 5          # the stop-the-world entries with the collector disabled
const LOCKED = 6          # a reset with finalizers to run, under a lock

# The reset must run as a real call with the reference live across it, or
# the compiler keeps the reference in a register the precise scan cannot
# see. `reset_via_call` in regions_api.jl is the @noinline call.
@noinline function checked_reset_refuses_under_a_root()
    region_set(ROOTED)
    r = Ref(42)
    region_set(0)
    # The object must be allocated for real: one the compiler can prove does
    # not escape is scalarized, and then the region holds nothing to find.
    check("the object lives in the region", escape(r) == ROOTED)
    res = reset_via_call(ROOTED)
    check("the checked reset refuses while an execution root points into the region",
          code(res) == EROOT)
    check("the object the root names still reads", r[] == 42)
    return nothing
end

@noinline function the_same_region_resets_once_the_root_is_gone()
    res = reset_via_call(ROOTED)
    check("the reset succeeds once no root points into the region", !refused(res))
end

# The unsafe entry frees without the check. The region is empty here, so
# the two entries agree; the case fixes that the entry exists and returns a
# page count.
@noinline function the_unsafe_reset_frees()
    region_set(ROOTED)
    local last
    for i in 1:1000
        last = Ref(i)
    end
    escape(last)
    region_set(0)
    res = unsafe_reset_via_call(ROOTED)
    check("the unsafe reset frees the region", !refused(res))
end

# A finalizer of the region runs during the reset, with the barrier armed.
# One that publishes its own object quarantines the region after the reset
# passed its own quarantine test. The reset must read the mark again and
# keep the pages.
const SINK = Any[]

@noinline function register_escaping_finalizer(n)
    region_set(n)
    p = Ref(7)
    finalizer(q -> (push!(SINK, q); nothing), p)
    region_set(0)
    escape(p)
    return nothing
end

function the_reset_refuses_after_its_finalizer_escaped()
    register_escaping_finalizer(FINAL)
    GC.gc()
    res = reset_via_call(FINAL)
    check("the reset refuses after its own finalizer quarantined the region",
          code(res) == EQUARANTINED)
    check("the finalizer ran and published the object", length(SINK) == 1)
    check("the published object still reads", SINK[1][] == 7)
    check("the region is quarantined", quarantined(FINAL) == 1)
    GC.gc()
    check("the collection hands the region over", quarantined(FINAL) == 0)
    check("the published object reads after the hand-over", SINK[1][] == 7)
    check("the published object is a stock object now", region_of(SINK[1]) == 0)
    check("the reset of the handed-over region succeeds", !refused(reset_via_call(FINAL)))
end

# A task that opens a window and dies inside it must not hold the window
# count. The exception object is a region object stored into the task, so
# the region quarantines as well; the count is what this case tests.
function a_dead_task_leaves_no_window()
    region_set(BURNED)
    escape(Ref(1))
    region_set(0)
    t = Task(() -> (region_set(DEAD); error("the task dies inside its window")))
    t.sticky = true
    schedule(t)
    try
        wait(t)
    catch
    end
    res = region_collect(BURNED)
    check("a census is not refused after a task died inside its window", res != EBUSY)
end

function a_window_on_a_quarantined_region_is_refused()
    check("the region of the dead task is quarantined", quarantined(DEAD) == 1)
    check("a window on a quarantined region is refused",
          region_set(DEAD) == EQUARANTINED)
    check("no window opened", region_current() == 0)
end

# The stop-the-world entries (census, checked reset, declare, check) run
# with the collector disabled, as the loops of a region program do, and
# they leave the disable counter as they found it. (They add the saved
# count back rather than store it, so a GC.enable of another thread in
# between keeps its effect; that race can not be forced here.)
@noinline function paused_fill()
    region_set(PAUSED)
    objs = [Ref(i) for i in 1:2000]
    s = sum(r[] for r in objs)
    region_set(0)
    return s
end
function the_entries_run_with_the_collector_disabled()
a_reset_with_finalizers_refuses_under_a_lock()
    GC.enable(false)
    paused_fill()
    check("a census runs with the collector disabled", region_collect(PAUSED) >= 0)
    check("the collector stays disabled after the census", GC.enable(true) == false)
    GC.enable(false)
    check("a check runs with the collector disabled", region_check(PAUSED) == 0)
    check("the collector stays disabled after the check", GC.enable(true) == false)
    GC.enable(false)
    check("a checked reset runs with the collector disabled", reset_via_call(PAUSED) < 2^62)
    check("the collector stays disabled after the reset", GC.enable(true) == false)
    GC.enable(false)
    # Other regions of this script hold live objects, so the declare refuses
    # with ECHILD; that test runs inside the pause, so the pause took place.
    code = declare_parent(PAUSED + 1, PAUSED)
    check("a declare runs its pause with the collector disabled (code $code)", code == 0 || code == ECHILD)
    check("the collector stays disabled after the declare", GC.enable(true) == false)
    GC.enable(true)
end

# The finalizers of a reset run Julia code, so a reset with finalizers to
# run refuses under a lock the caller holds (a ReentrantLock inhibits
# finalizers while it is held), as the stock collector defers finalizers in
# that state; the same reset succeeds once the lock is released.
# The object with the finalizer is made in one function and registered in
# another, and the finalizer has an effect: the compiler removes an
# effect-free finalizer, and inlines the finalizer of an object whose
# allocation it sees.
const LOCKED_RAN = Ref(0)
@noinline make_locked_vector() = (region_set(LOCKED); v = [Ref(1)]; region_set(0); v)
@noinline register_finalizer!(v) = (finalizer(_ -> (LOCKED_RAN[] += 1), v[1]); nothing)
@noinline function locked_fill()
    v = make_locked_vector()
    register_finalizer!(v)
    return v[1][]
end
function a_reset_with_finalizers_refuses_under_a_lock()
    locked_fill()
    l = ReentrantLock()
    r = @lock l reset_via_call(LOCKED)
    check("a reset with finalizers to run refuses under a lock (code $(code(r)))", code(r) == EBUSY)
    check("the finalizer did not run under the lock", LOCKED_RAN[] == 0)
    r = reset_via_call(LOCKED)
    check("the same reset succeeds without the lock (code $(code(r)))", !refused(r))
    check("the finalizer ran at the reset", LOCKED_RAN[] == 1)
end

checked_reset_refuses_under_a_root()
the_entries_run_with_the_collector_disabled()
the_same_region_resets_once_the_root_is_gone()
the_unsafe_reset_frees()
if Base.GC_REGION_BARRIER
    the_reset_refuses_after_its_finalizer_escaped()
end
a_dead_task_leaves_no_window()
if Base.GC_REGION_BARRIER
    a_window_on_a_quarantined_region_is_refused()
else
    println("regions_safety: the escape cases are skipped, this build has no region barrier")
end

finish("regions_safety")

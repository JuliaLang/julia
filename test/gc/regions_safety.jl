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

checked_reset_refuses_under_a_root()
the_same_region_resets_once_the_root_is_gone()
the_unsafe_reset_frees()
the_reset_refuses_after_its_finalizer_escaped()
a_dead_task_leaves_no_window()
a_window_on_a_quarantined_region_is_refused()

finish("regions_safety")

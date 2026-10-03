# This file is a part of Julia. License is MIT: https://julialang.org/license

# Two threads use the same region number, each on its own heap: the
# per-thread leaf pattern. A checked reset on one heap marks from the roots
# of every thread, so it marks the other thread's region objects as well.
# It must clear those marks before it returns: a mark that stays makes the
# other thread's next checked reset refuse falsely, and makes its next
# census stop at the marked object, so the census never marks the children
# of that object and frees them under a live parent.
include(joinpath(@__DIR__, "regions_api.jl"))

const N = 1
const A_READY = Threads.Atomic{Int}(0)
const B_DONE = Threads.Atomic{Int}(0)

@noinline function spin_until(flag)
    while flag[] == 0
        GC.safepoint()
        ccall(:jl_cpu_pause, Cvoid, ())
    end
end

# A holds region objects rooted on its frame while B runs a checked reset
# of B's own instance of the region. Then A closes its window and works on.
@noinline function heaps_hold(mode)
    region_set(N)
    objs = [Ref(Ref(i)) for i in 1:3000]
    check("A's objects are in region $N", region_of(objs[1]) == N)
    Threads.atomic_add!(A_READY, 1)
    spin_until(B_DONE)
    s = 0
    for r in objs; s += r[][]; end
    check("A's objects survive B's reset (sum $s)", s == 4_501_500)
    if mode == :census
        # Give each outer a new inner. A census that does not walk the
        # outer, because B's scan left it marked, never marks the new inner.
        for r in objs; r[] = Ref(r[][] + 1); end
        region_set(0)
        freed = region_collect(N)
        check("A's census runs after B's reset (code $freed)", freed >= 3000)
        # Reuse the freed cells: a freed inner takes a -1, and its parent
        # reads it.
        region_set(N)
        junk = [Ref(-1) for _ in 1:6000]
        region_set(0)
        s2 = 0
        for r in objs; s2 += r[][]; end
        check("the new inners survive A's census (sum $s2)", s2 == s + length(objs))
        check("the junk is intact", all(j -> j[] == -1, junk))
    else
        region_set(0)
    end
    return s
end

@noinline function heaps_fill()
    region_set(N)
    x = [Ref(i) for i in 1:100]
    s = sum(r[] for r in x)
    region_set(0)
    return s
end

function heaps_worker(w, mode)
    if w == 1
        heaps_hold(mode)
        # The frame of heaps_hold is gone: nothing roots A's objects.
        check("A's check finds no root after B's reset ($mode)", region_check(N) == 0)
        r = reset_via_call(N)
        check("A's checked reset succeeds after B's ($mode, code $(code(r)))", !refused(r))
    else
        spin_until(A_READY)
        heaps_fill()
        r = reset_via_call(N)
        check("B's checked reset succeeds ($mode, code $(code(r)))", !refused(r))
        Threads.atomic_add!(B_DONE, 1)
    end
end

function heaps_round(mode)
    A_READY[] = 0
    B_DONE[] = 0
    Threads.@threads :static for w in 1:2
        heaps_worker(w, mode)
    end
    check("no quarantine after the $mode round", quarantined(N) == 0)
end

# A borrow brings a region number to a heap that never opened a window on
# it: B grows a vector that A made in the child of N, so B's heap takes
# pages of the child. The child must be live on B's heap from that moment,
# or B's reset of the parent does not see it. A keeps the vector on its
# frame and B reaches it through its address: no region-0 object stores a
# pointer to it, so the barrier stays quiet and the borrow is the only
# region event on B's heap.
const CHILD = N + 1
const LENT_PTR = Threads.Atomic{UInt}(0)
const B_GREW = Threads.Atomic{Int}(0)
const A_DROPPED = Threads.Atomic{Int}(0)
const B_RESET = Threads.Atomic{Int}(0)

@noinline function heaps_lend(v)
    LENT_PTR[] = UInt(pointer_from_objref(v))
    spin_until(B_GREW)
    return sum(v)
end

@noinline function heaps_make_and_lend()
    region_set(CHILD)
    v = Int[1]
    region_set(0)
    return heaps_lend(v)
end

# B's use of the lent vector, in a frame of its own so that B roots the
# vector no longer when it returns. Returns the region of the grown buffer.
@noinline function heaps_grow_lent()
    v = unsafe_pointer_to_objref(Ptr{Cvoid}(LENT_PTR[]))::Vector{Int}
    for i in 2:100
        push!(v, i)                       # grows: the borrow of CHILD on B's heap
    end
    return region_of(v.ref.mem)
end

@noinline function heaps_touch(n)
    region_set(n)
    x = Ref(1)
    region_set(0)
    return x[]
end

function heaps_borrow_worker(w)
    if w == 1
        s = heaps_make_and_lend()
        check("A reads the vector that B grew (sum $s)", s == 5050)
        # The frame of heaps_make_and_lend is gone: nothing roots the vector.
        Threads.atomic_add!(A_DROPPED, 1)
        spin_until(B_RESET)
        r = reset_via_call(CHILD)
        check("A's checked reset of its child instance succeeds (code $(code(r)))", !refused(r))
    else
        heaps_touch(N)                    # N is live on B's heap, with no child yet
        spin_until(LENT_PTR)
        got = heaps_grow_lent()
        check("B's growth of A's vector does not quarantine", quarantined(CHILD) == 0)
        check("the grown buffer is in the child region", got == CHILD)
        check("B's heap holds pages of the child", region_pages(CHILD) > 0)
        r = region_reset(N)
        check("B's reset of the parent refuses while B's borrowed buffer lives (code $(code(r)))", code(r) == ECHILD)
        Threads.atomic_add!(B_GREW, 1)
        spin_until(A_DROPPED)
        check("B's check finds no root into the child", region_check(CHILD) == 0)
        r = reset_via_call(CHILD)
        check("B's checked reset of the child succeeds (code $(code(r)))", !refused(r))
        check("B's heap holds no pages of the child after its reset", region_pages(CHILD) == 0)
        r = reset_via_call(N)
        check("B's reset of the parent succeeds once the child is gone (code $(code(r)))", !refused(r))
        Threads.atomic_add!(B_RESET, 1)
    end
end

function heaps_borrow_round()
    LENT_PTR[] = 0
    B_GREW[] = 0
    A_DROPPED[] = 0
    B_RESET[] = 0
    Threads.@threads :static for w in 1:2
        heaps_borrow_worker(w)
    end
    check("no quarantine after the borrow round", quarantined(CHILD) == 0 && quarantined(N) == 0)
    r = region_reset_global(N)
    check("the global reset of the parent succeeds after the round (code $(code(r)))", !refused(r))
end

if Threads.nthreads() < 2
    println("regions_heaps: skipped, needs two threads")
else
    heaps_round(:reset)
    heaps_round(:census)
    heaps_borrow_round()
    finish("regions_heaps")
end

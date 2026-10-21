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
# region event on B's heap. A reset of the parent refuses while a child
# instance lives on any heap, so both threads reset their child first.
const CHILD = N + 1
const LENT_PTR = Threads.Atomic{UInt}(0)
const B_GREW = Threads.Atomic{Int}(0)
const A_DROPPED = Threads.Atomic{Int}(0)
const B_RESET = Threads.Atomic{Int}(0)
const A_CHILD_RESET = Threads.Atomic{Int}(0)

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
        Threads.atomic_add!(A_CHILD_RESET, 1)
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
        # A's instance of the child still lives, on A's heap: it may hold
        # legal references into B's instance of the parent, so the parent
        # still refuses.
        r = reset_via_call(N)
        check("B's reset of the parent refuses while A's child instance lives (code $(code(r)))", code(r) == ECHILD)
        Threads.atomic_add!(B_RESET, 1)
        spin_until(A_CHILD_RESET)
        r = reset_via_call(N)
        check("B's reset of the parent succeeds once the child is gone on every heap (code $(code(r)))", !refused(r))
    end
end

function heaps_borrow_round()
    LENT_PTR[] = 0
    B_GREW[] = 0
    A_DROPPED[] = 0
    B_RESET[] = 0
    A_CHILD_RESET[] = 0
    Threads.@threads :static for w in 1:2
        heaps_borrow_worker(w)
    end
    check("no quarantine after the borrow round", quarantined(CHILD) == 0 && quarantined(N) == 0)
    r = region_reset_global(N)
    check("the global reset of the parent succeeds after the round (code $(code(r)))", !refused(r))
end

# A child region live on another heap holds legal references into this
# heap's instance of the parent: A makes x in N, B stores x into y, an object
# of CHILD on B's heap (a child may reference its parent). A's checked reset
# of N must refuse with ECHILD while B's child instance lives, although A's
# own heap has no child, and must succeed once B reset its child. B reaches x
# through its address, as in the borrow round, so no region-0 object stores a
# pointer to it.
const X_PTR = Threads.Atomic{UInt}(0)
const B_STORED = Threads.Atomic{Int}(0)
const A_REFUSED = Threads.Atomic{Int}(0)
const B_CHILD_RESET = Threads.Atomic{Int}(0)

# x is a vector, as the lent object of the borrow round is: the compiler
# keeps a Ref whose only uses are pointer_from_objref and a read on the
# stack, and B would then receive the address of a stack slot.
@noinline function cross_make_and_lend()
    region_set(N)
    x = Int[4242]
    region_set(0)
    X_PTR[] = UInt(pointer_from_objref(x))
    spin_until(B_STORED)
    return x[1]
end

# B's store of the object behind `ptr` into an object of the child, in a
# frame of its own: when it returns, B roots that object no longer, and only
# y's field references it. The first call compiles the function's callees;
# a compile inside the window allocates in the region on paths the zones do
# not cover, so B warms the function up on a region-0 object first.
@noinline function cross_store(ptr)
    region_set(CHILD)
    y = Ref{Any}(nothing)
    y[] = unsafe_pointer_to_objref(Ptr{Cvoid}(ptr))
    region_set(0)
    return y
end

@noinline cross_warm_store(dummy) = cross_store(UInt(pointer_from_objref(dummy)))[] === dummy

@noinline function cross_warm()
    dummy = Int[0]
    ok = cross_warm_store(dummy)          # y lives in that frame alone
    r = reset_via_call(CHILD)
    return ok && !refused(r)
end

function heaps_cross_child_worker(w)
    if w == 1
        v = cross_make_and_lend()
        check("A made x in N (value $v)", v == 4242)
        # The frame of cross_make_and_lend is gone: nothing on A roots x.
        r = reset_via_call(N)
        check("A's checked reset refuses while B's child instance lives (code $(code(r)))", code(r) == ECHILD)
        Threads.atomic_add!(A_REFUSED, 1)
        spin_until(B_CHILD_RESET)
        r = reset_via_call(N)
        check("A's checked reset succeeds once B's child is gone (code $(code(r)))", !refused(r))
    else
        check("B's warm-up of the store compiled it outside the window", cross_warm())
        spin_until(X_PTR)
        y = cross_store(X_PTR[])
        check("the store of a parent object into the child did not quarantine", quarantined(CHILD) == 0 && quarantined(N) == 0)
        Threads.atomic_add!(B_STORED, 1)
        spin_until(A_REFUSED)
        check("x is whole after A's refused reset (value $(y[][1]))", y[][1] == 4242)
        y = nothing
        r = reset_via_call(CHILD)
        check("B's checked reset of its child succeeds (code $(code(r)))", !refused(r))
        Threads.atomic_add!(B_CHILD_RESET, 1)
    end
end

function heaps_cross_child_round()
    X_PTR[] = 0
    B_STORED[] = 0
    A_REFUSED[] = 0
    B_CHILD_RESET[] = 0
    Threads.@threads :static for w in 1:2
        heaps_cross_child_worker(w)
    end
    check("no quarantine after the cross-child round", quarantined(CHILD) == 0 && quarantined(N) == 0)
end

# The cooperative census of A marks from A's roots alone, and its claim
# accepts region objects of every heap: through a cross-heap reference it
# marks B's objects too, and it sweeps only A's heap. The marks it leaves on
# B's heap must be cleared before it returns, or B's next check counts them
# as roots and B's next stock mark stops at them. B parks in a channel wait
# while A runs the census, as the cooperative census asks.
const B_PTR = Threads.Atomic{UInt}(0)
const COOP_DONE = Channel{Int}(1)

@noinline function coop_make_b()
    region_set(N)
    b = Int[7]
    region_set(0)
    return UInt(pointer_from_objref(b))
end

# A's object in N references B's object in N, on another heap; the census
# runs in this frame, where `a` is a root, so it marks a and through a, b.
@noinline function coop_store_and_census(ptr)
    region_set(N)
    a = Any[]
    push!(a, unsafe_pointer_to_objref(Ptr{Cvoid}(ptr)))
    region_set(0)
    # EUNSAFE until the other thread is parked in its wait: retry, bounded.
    deadline = time_ns() + 20_000_000_000
    freed = region_collect_coop(N)
    while freed == EUNSAFE && time_ns() < deadline
        GC.safepoint()
        ccall(:jl_cpu_pause, Cvoid, ())
        freed = region_collect_coop(N)
    end
    return (freed, (a[1]::Vector{Int})[1])
end

@noinline function coop_warm()
    dummy = Int[0]
    freed, v = coop_store_and_census(UInt(pointer_from_objref(dummy)))
    return freed >= 0 && v == 0
end

# The main thread is the one that idles in the event loop, and it idles in
# the unsafe state, so a cooperative census from a worker never sees it
# parked: the census runs on the main task, and B is a task of the default
# pool, parked in a channel wait while the census runs.
function heaps_coop_round()
    B_PTR[] = 0
    check("the warm-up of the cooperative round ran", coop_warm())
    r = reset_via_call(N)
    check("the reset after the warm-up succeeds (code $(code(r)))", !refused(r))
    b_task = Threads.@spawn begin
        B_PTR[] = coop_make_b()
        take!(COOP_DONE)                  # parked: the census of A runs now
        c = region_check(N)
        check("B's check finds no root into N after A's census (count $c)", c == 0)
    end
    spin_until(B_PTR)
    freed, v = coop_store_and_census(B_PTR[])
    check("A's cooperative census ran with B parked (code $freed)", freed >= 0)
    check("A reads B's object through its own (value $v)", v == 7)
    # The frame of coop_store_and_census is gone: nothing roots a or b.
    put!(COOP_DONE, 1)
    wait(b_task)
    check("no quarantine after the cooperative round", quarantined(N) == 0)
    r = region_reset_global(N)
    check("the global reset after the cooperative round succeeds (code $(code(r)))", !refused(r))
end

if Threads.nthreads() < 2
    println("regions_heaps: skipped, needs two threads")
else
    heaps_round(:reset)
    heaps_round(:census)
    heaps_borrow_round()
    heaps_cross_child_round()
    heaps_coop_round()
    finish("regions_heaps")
end

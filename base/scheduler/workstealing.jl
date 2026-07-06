# This file is a part of Julia. License is MIT: https://julialang.org/license

# A work-stealing task scheduler in the shape shared by Go and Tokio: a
# bounded FIFO ring of task references per worker, a per-thread LIFO slot
# for the most recently enqueued task, steal-half, and a per-threadpool
# injection queue for cross-pool enqueues and ring overflow.
#
# The ring is Tokio's `queue.rs` (derived from Go's runq). The head packs
# two UInt32 counters with steal <= real: the live queue is [real, tail),
# and [steal, real) is being copied out by a thief. A thief claims by
# advancing `real` (leaving `steal` behind) and finishes by snapping
# `steal` forward, so steal == real means no steal is in progress. The
# owner pushes at `tail` (no RMW) and pops at `real` (one CAS, no fence);
# capacity is checked against `steal` so claimed slots are not reused.
# Consumed slots are always nulled so the queue never pins a dead task.

module Workstealing

using ..Threads: SpinLock
using ..Scheduler: cong

const RING_CAP = UInt32(256)          # power of two
const RING_MASK = RING_CAP - UInt32(1)
const LIFO_CAP = 0x03                 # max consecutive polls served from the LIFO slot

pack(steal::UInt32, real::UInt32) = (UInt64(steal) << 32) | UInt64(real)
steal_of(h::UInt64) = (h >> 32) % UInt32
real_of(h::UInt64) = h % UInt32
slot(i::UInt32) = Int(i & RING_MASK) + 1

# Unbounded overflow/injection queue, one per threadpool, striped over
# independently locked FIFO segments so off-pool producers and the pool's
# consumers don't serialize on one lock. Each stripe's `n` is a lock-free
# emptiness hint; the fences in jl_task_get_next / jl_wakeup_threadpool
# make reading it without the lock sound (as with partr's heaps).
const INJECT_STRIPES = 16

mutable struct InjectStripe
    const lock::SpinLock
    const tasks::Vector{Task}
    @atomic n::Int
    InjectStripe() = new(SpinLock(), Vector{Task}(), 0)
end

struct InjectQueue
    stripes::Memory{InjectStripe}
    function InjectQueue()
        stripes = Memory{InjectStripe}(undef, INJECT_STRIPES)
        for i in 1:INJECT_STRIPES
            stripes[i] = InjectStripe()
        end
        return new(stripes)
    end
end

function stripe_push!(s::InjectStripe, t::Task)
    push!(s.tasks, t)
    @atomic :release s.n = length(s.tasks)
    return nothing
end

function inject_push!(q::InjectQueue, t::Task)
    start = Int(cong(UInt32(INJECT_STRIPES)))
    for p in 0:(INJECT_STRIPES - 1)
        s = q.stripes[(start + p - 1) % INJECT_STRIPES + 1]
        if trylock(s.lock)
            stripe_push!(s, t)
            unlock(s.lock)
            return nothing
        end
    end
    s = q.stripes[start]
    @lock s.lock stripe_push!(s, t)
    return nothing
end

function inject_pushbatch!(q::InjectQueue, ts::Vector{Task})
    s = q.stripes[Int(cong(UInt32(INJECT_STRIPES)))]
    @lock s.lock begin
        append!(s.tasks, ts)
        @atomic :release s.n = length(s.tasks)
    end
    return nothing
end

function inject_isempty(q::InjectQueue)
    for i in 1:INJECT_STRIPES
        (@atomic :acquire q.stripes[i].n) == 0 || return false
    end
    return true
end

# Field groups sit on separate cache lines (head / tail / LIFO slot /
# owner-only counters / read-shared constants), so e.g. an owner bumping
# `tick` never invalidates the `buffer` pointer thieves are reading.
mutable struct Ring
    @atomic head::UInt64                  # (steal | real), see file header
    const _pad1::NTuple{15,UInt64}
    @atomic tail::UInt32                  # owner-only writer
    const _pad2::NTuple{15,UInt64}
    @atomic lifo::Union{Task,Nothing}
    const _pad3::NTuple{15,UInt64}
    lifo_streak::UInt8                    # owner-only
    tick::UInt32                          # owner-only, fairness counter
    const _pad4::NTuple{15,UInt64}
    const tpid::Int8
    const tp_idx::Int32                   # position within the pool's Rings
    const buffer::AtomicMemory{Union{Task,Nothing}}
    function Ring(tpid::Int8, tp_idx::Int=0)
        buf = AtomicMemory{Union{Task,Nothing}}(undef, Int(RING_CAP))
        for i in 1:Int(RING_CAP)
            Core.memoryrefset!(memoryref(buf, i), nothing, :monotonic, false)
        end
        pad = ntuple(_ -> UInt64(0), Val(15))  # Val: infers as NTuple{15,UInt64}, keeps --trim happy
        return new(UInt64(0), pad, UInt32(0), pad, nothing, pad, 0x00, UInt32(0), pad, tpid, tp_idx % Int32, buf)
    end
end

@assert fieldoffset(Ring, :tail) - fieldoffset(Ring, :head) >= 64
@assert fieldoffset(Ring, :lifo) - fieldoffset(Ring, :tail) >= 64
@assert fieldoffset(Ring, :lifo_streak) - fieldoffset(Ring, :lifo) >= 64
@assert fieldoffset(Ring, :tpid) - fieldoffset(Ring, :lifo_streak) >= 64

getslot(r::Ring, idx::Int) = Core.memoryrefget(memoryref(r.buffer, idx), :monotonic, false)
setslot!(r::Ring, val::Union{Task,Nothing}, idx::Int) =
    Core.memoryrefset!(memoryref(r.buffer, idx), val, :monotonic, false)

# Owner-only: push `t` at the tail; on a full ring overflow half of it (plus
# `t`) to the injection queue, or just `t` if a thief is mid-steal.
function push_local!(r::Ring, t::Task)
    tail = @atomic :monotonic r.tail
    while true
        h = @atomic :acquire r.head
        s = steal_of(h)
        if tail - s < RING_CAP
            setslot!(r, t, slot(tail))
            @atomic :release r.tail = tail + UInt32(1)
            return nothing
        end
        rl = real_of(h)
        if s != rl
            # Full and a steal is in progress: overflow this one task.
            inject_push!(inject_for(r.tpid), t)
            return nothing
        end
        push_overflow!(r, t, rl, tail) && return nothing
        # a thief claimed between our load and the overflow CAS; retry
    end
end

# Owner-only: claim the first half of the ring as if stealing from ourselves,
# then move it (plus `t`) to the injection queue.
function push_overflow!(r::Ring, t::Task, head::UInt32, tail::UInt32)
    n = RING_CAP >> 1
    prev = pack(head, head)
    next = pack(head + n, head + n)
    (@atomicreplace :acquire_release :monotonic r.head prev => next).success || return false
    batch = Vector{Task}(undef, Int(n) + 1)
    for i in UInt32(0):(n - UInt32(1))
        idx = slot(head + i)
        batch[Int(i) + 1] = getslot(r, idx)::Task
        setslot!(r, nothing, idx)
    end
    batch[Int(n) + 1] = t
    inject_pushbatch!(inject_for(r.tpid), batch)
    return true
end

# Pop a batch proportional to stripe length (Go's `size/nprocs + 1`), capped
# by free space in `r`, under one lock acquisition. The first task is
# returned; the rest go onto `r` outside the lock (push_local! can overflow
# back into this queue — pushing under the lock would self-deadlock).
# Stripes are trylock-probed from a random start; a nonempty-but-locked
# stripe is retried blocking at the end so work cannot be missed.
function stripe_popbatch!(s::InjectStripe, free::Int, nthreads::Int)
    len = length(s.tasks)
    len == 0 && return nothing
    k = max(1, min(len, len ÷ nthreads + 1, free + 1, Int(RING_CAP >> 1)))
    b = Vector{Task}(undef, k)
    for i in 1:k
        b[i] = popfirst!(s.tasks)
    end
    @atomic :release s.n = length(s.tasks)
    return b
end

function inject_popbatch!(q::InjectQueue, r::Ring, nthreads::Int)
    tail = @atomic :monotonic r.tail
    h = @atomic :acquire r.head
    free = Int(RING_CAP - (tail - steal_of(h)))
    start = Int(cong(UInt32(INJECT_STRIPES)))
    batch = nothing
    busy = 0
    for p in 0:(INJECT_STRIPES - 1)
        i = (start + p - 1) % INJECT_STRIPES + 1
        s = q.stripes[i]
        (@atomic :acquire s.n) == 0 && continue
        if trylock(s.lock)
            batch = stripe_popbatch!(s, free, nthreads)
            unlock(s.lock)
            batch === nothing || break
        elseif busy == 0
            busy = i
        end
    end
    if batch === nothing && busy != 0
        s = q.stripes[busy]
        batch = @lock s.lock stripe_popbatch!(s, free, nthreads)
    end
    batch === nothing && return nothing
    for i in 2:length(batch)
        push_local!(r, batch[i])
    end
    return batch[1]
end

# Owner-only: pop from the head (FIFO). One CAS, no seq-cst fence.
function pop_local!(r::Ring)
    while true
        h = @atomic :acquire r.head
        s = steal_of(h)
        rl = real_of(h)
        tail = @atomic :monotonic r.tail
        rl == tail && return nothing
        nrl = rl + UInt32(1)
        next = s == rl ? pack(nrl, nrl) : pack(s, nrl)
        if (@atomicreplace :acquire_release :monotonic r.head h => next).success
            idx = slot(rl)
            t = getslot(r, idx)::Task
            setslot!(r, nothing, idx)
            return t
        end
    end
end

# Any thread: steal half of `src`'s ring (capped by free space in `dst`, the
# thief's own ring). The rest of the batch is copied raw into `dst` and
# published with one tail store, keeping the exclusive [claim, finish)
# window short — only one thief may be inside it per ring.
function steal_from!(src::Ring, dst::Ring)
    dtail = @atomic :monotonic dst.tail
    dh = @atomic :acquire dst.head
    dfree = RING_CAP - (dtail - steal_of(dh))
    spins = 0
    while true
        h = @atomic :acquire src.head
        s = steal_of(h)
        rl = real_of(h)
        if s != rl
            # Another thief is mid-steal; wait out its short copy window
            # rather than reporting a non-empty ring as empty.
            (spins += 1) > 64 && return nothing
            ccall(:jl_cpu_pause, Cvoid, ())
            continue
        end
        tail = @atomic :acquire src.tail
        n = tail - rl
        n == UInt32(0) && return nothing
        n = n - (n >> 1)                  # half, rounded up
        n = min(n, dfree + UInt32(1))     # 1 returned + at most dfree parked
        next = pack(s, rl + n)            # leave `steal` behind to mark in-progress
        (@atomicreplace :acquire_release :monotonic src.head h => next).success || continue
        idx = slot(rl)
        first_t = getslot(src, idx)::Task
        setslot!(src, nothing, idx)
        for i in UInt32(1):(n - UInt32(1))
            idx = slot(rl + i)
            ti = getslot(src, idx)::Task
            setslot!(src, nothing, idx)
            setslot!(dst, ti, slot(dtail + i - UInt32(1)))
        end
        # Finish: snap `steal` forward to the current `real`, releasing the
        # nulled slots for reuse by the owner's pushes.
        while true
            h2 = @atomic :monotonic src.head
            done = pack(real_of(h2), real_of(h2))
            (@atomicreplace :acquire_release :monotonic src.head h2 => done).success && break
        end
        n > UInt32(1) && (@atomic :release dst.tail = dtail + (n - UInt32(1)))
        return first_t
    end
end

# Owner-only: stash `t` in the LIFO slot; a displaced occupant goes to the ring.
function lifo_push!(r::Ring, t::Task)
    old = @atomicswap :acquire_release r.lifo = t
    old === nothing || push_local!(r, old::Task)
    return nothing
end

lifo_pop!(r::Ring) = @atomicswap :acquire_release r.lifo = nothing

function lifo_steal!(r::Ring)
    t = @atomic :acquire r.lifo
    t === nothing && return nothing
    return (@atomicreplace :acquire_release :monotonic r.lifo t => nothing).success ? t : nothing
end

# One Memory{Ring} per threadpool, indexed by position within the pool,
# plus one injection queue per pool. The rings Memory is replaced under the
# lock and read racily (grow-only, as with partr's heaps).
const Rings = [Memory{Ring}(undef, 0), Memory{Ring}(undef, 0)]
const Rings_lock = SpinLock()
const Injects = [InjectQueue(), InjectQueue()]

inject_for(tpid::Int8) = Injects[Int(tpid) + 1]

function rings_for(tpid::Int8)
    rs = Rings[Int(tpid) + 1]
    n = Int(Threads._nthreads_in_pool(tpid))
    length(rs) == n && return rs
    return grow_rings!(tpid, n)
end

@noinline function grow_rings!(tpid::Int8, n::Int)
    @lock Rings_lock begin
        rs = Rings[Int(tpid) + 1]
        length(rs) == n && return rs
        new = Memory{Ring}(undef, n)
        for i in 1:n
            new[i] = i <= length(rs) ? rs[i] : Ring(tpid, i)
        end
        Rings[Int(tpid) + 1] = new
        return new
    end
end

# The current thread's ring and its pool's rings, or nothing for threads
# outside the pools (foreign/adopted/GC), which have no local queue.
function own_ring_and_pool()
    tid = Threads.threadid()
    tpid = ccall(:jl_threadpoolid, Int8, (Int16,), tid - 1)
    tpid == Int8(-1) && return nothing
    rs = rings_for(tpid)
    off = tpid == Int8(0) ? 0 : Int(Threads._nthreads_in_pool(Int8(0)))
    tp_tid = tid - off
    1 <= tp_tid <= length(rs) || return nothing
    return rs[tp_tid], rs
end

# Claim the task for this thread; if it is claimed elsewhere (e.g. queued
# twice), park it in its pool's injection queue for its owner to find.
function tryclaim(t::Task)
    if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, Threads.threadid() - 1) == 0
        # The task is pinned to another thread — typically that thread's
        # current task, parked hosting the thread-sleep logic in wait() — so
        # only that thread can run it. Deliver it to that thread's sticky
        # workqueue and wake it directly: re-parking it in the shared queues
        # lets other dequeuers bounce it around (and a pool wake is gated by
        # our own spinner slot), cycling it through the injection queue
        # indefinitely while the one thread able to run it sleeps.
        tid = Threads.threadid(t)
        if tid > 0
            push!(Base.workqueue_for(tid), t)
            ccall(:jl_wakeup_thread, Cint, (Int16,), (tid - 1) % Int16)
        else
            # became claimable in the meantime; requeue for anyone
            tpid = ccall(:jl_get_task_threadpoolid, Int8, (Any,), t)
            inject_push!(inject_for(tpid), t)
            ccall(:jl_wakeup_threadpool, Cvoid, (Int8,), tpid)
        end
        return nothing
    end
    return t
end

function enqueue!(t::Task)
    tpid = ccall(:jl_get_task_threadpoolid, Int8, (Any,), t)
    rp = own_ring_and_pool()
    if rp !== nothing && rp[1].tpid == tpid
        lifo_push!(rp[1], t)
    else
        inject_push!(inject_for(tpid), t)
    end
    return nothing
end

function dequeue!()
    rp = own_ring_and_pool()
    rp === nothing && return nothing
    r, rs = rp
    nt = length(rs)
    inj = inject_for(r.tpid)
    r.tick += UInt32(1)
    # Fairness: periodically serve the injection queue first so tasks that
    # keep respawning each other cannot starve injected work (61 as in Go:
    # prime, to avoid resonating with application periodicity).
    if r.tick % UInt32(61) == UInt32(0)
        t = inject_popbatch!(inj, r, nt)
        t === nothing || (c = tryclaim(t); c === nothing || return c)
    end
    if r.lifo_streak < LIFO_CAP
        t = lifo_pop!(r)
        if t !== nothing
            c = tryclaim(t::Task)
            if c !== nothing
                r.lifo_streak += 0x01
                return c
            end
        end
    end
    r.lifo_streak = 0x00
    t = pop_local!(r)
    t === nothing || (c = tryclaim(t); c === nothing || return c)
    # The ring is empty; take the LIFO slot even if it was streak-capped.
    t = lifo_pop!(r)
    t === nothing || (c = tryclaim(t::Task); c === nothing || return c)
    t = inject_popbatch!(inj, r, nt)
    t === nothing || (c = tryclaim(t); c === nothing || return c)
    # Steal; how many threads search at once is bounded by the runtime's
    # spinner accounting in jl_task_get_next.
    if nt > 1
        t = steal_sweep!(rs, r, nt)
        t === nothing || return t
    end
    return nothing
end

# One sweep over all victims (Tokio's shape): no available task can be
# missed before the thread sleeps, yet each ring is probed at most once per
# dequeue. Starting at the thief's own successor fans concurrent thieves
# out over distinct victims. A victim's LIFO slot — its owner's hottest
# cache line — is only raided when its ring yields nothing. Returns a
# claimed task, or nothing.
function steal_sweep!(rs::Memory{Ring}, r::Ring, nt::Int)
    start = Int(r.tp_idx) % nt + 1
    for k in 0:(nt - 1)
        victim = rs[(start + k - 1) % nt + 1]
        victim === r && continue
        t = steal_from!(victim, r)
        t === nothing && (t = lifo_steal!(victim))
        t === nothing || (c = tryclaim(t::Task); c === nothing || return c)
    end
    return nothing
end

function checktaskempty()
    tid = Threads.threadid()
    tpid = ccall(:jl_threadpoolid, Int8, (Int16,), tid - 1)
    tpid == Int8(-1) && return true
    inject_isempty(inject_for(tpid)) || return false
    rs = rings_for(tpid)
    for i in 1:length(rs)
        r = rs[i]
        h = @atomic :acquire r.head
        tail = @atomic :acquire r.tail
        real_of(h) == tail || return false
        (@atomic :acquire r.lifo) === nothing || return false
    end
    return true
end

end

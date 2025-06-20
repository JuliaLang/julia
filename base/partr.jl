# This file is a part of Julia. License is MIT: https://julialang.org/license

module Partr

using ..Threads: SpinLock, maxthreadid, threadid

# a task minheap
mutable struct taskheap
    const lock::SpinLock
    const tasks::Vector{Task}
    @atomic ntasks::Int32
    @atomic priority::UInt16
    taskheap() = new(SpinLock(), Vector{Task}(undef, 256), zero(Int32), typemax(UInt16))
end


# multiqueue minheap state
const heap_d = UInt32(8)
const heaps = [Vector{taskheap}(undef, 0), Vector{taskheap}(undef, 0)]
const heaps_lock = [SpinLock(), SpinLock()]


"""
    cong(max::UInt32)

Return a random UInt32 in the range `1:max` except if max is 0, in that case return 0.
"""
cong(max::UInt32) = iszero(max) ? UInt32(0) : rand_ptls(max) + UInt32(1) #TODO: make sure users don't use 0 and remove this check

get_ptls_rng() = ccall(:jl_get_ptls_rng, UInt64, ())

set_ptls_rng(seed::UInt64) = ccall(:jl_set_ptls_rng, Cvoid, (UInt64,), seed)

"""
    rand_ptls(max::UInt32)

Return a random UInt32 in the range `0:max-1` using the thread-local RNG
state. Max must be greater than 0.
"""
Base.@assume_effects :removable :inaccessiblememonly :notaskstate function rand_ptls(max::UInt32)
    rngseed = get_ptls_rng()
    val, seed = rand_uniform_max_int32(max, rngseed)
    set_ptls_rng(seed)
    return val % UInt32
end

# This implementation is based on OpenSSLs implementation of rand_uniform
# https://github.com/openssl/openssl/blob/1d2cbd9b5a126189d5e9bc78a3bdb9709427d02b/crypto/rand/rand_uniform.c#L13-L99
# Comments are vendored from their implementation as well.
# For the original developer check the PR to swift https://github.com/apple/swift/pull/39143.

# Essentially it boils down to incrementally generating a fixed point
# number on the interval [0, 1) and multiplying this number by the upper
# range limit.  Once it is certain what the fractional part contributes to
# the integral part of the product, the algorithm has produced a definitive
# result.
"""
    rand_uniform_max_int32(max::UInt32, seed::UInt64)

Return a random UInt32 in the range `0:max-1` using the given seed.
Max must be greater than 0.
"""
Base.@assume_effects :total function rand_uniform_max_int32(max::UInt32, seed::UInt64)
    if max == UInt32(1)
        return UInt32(0), seed
    end
    # We are generating a fixed point number on the interval [0, 1).
    # Multiplying this by the range gives us a number on [0, upper).
    # The high word of the multiplication result represents the integral part
    # This is not completely unbiased as it's missing the fractional part of the original implementation but it's good enough for our purposes
    seed = UInt64(69069) * seed + UInt64(362437)
    prod = (UInt64(max)) * (seed % UInt32) # 64 bit product
    i = prod >> 32 % UInt32 # integral part
    return i % UInt32, seed
end



function multiq_sift_up(heap::taskheap, idx::Int32)
    while idx > Int32(1)
        parent = (idx - Int32(2)) ÷ heap_d + Int32(1)
        if heap.tasks[idx].priority < heap.tasks[parent].priority
            t = heap.tasks[parent]
            heap.tasks[parent] = heap.tasks[idx]
            heap.tasks[idx] = t
            idx = parent
        else
            break
        end
    end
end


function multiq_sift_down(heap::taskheap, idx::Int32)
    if idx <= heap.ntasks
        for child = (heap_d * idx - heap_d + 2):(heap_d * idx + 1)
            child = Int(child)
            child > length(heap.tasks) && break
            if isassigned(heap.tasks, child) &&
                    heap.tasks[child].priority < heap.tasks[idx].priority
                t = heap.tasks[idx]
                heap.tasks[idx] = heap.tasks[child]
                heap.tasks[child] = t
                multiq_sift_down(heap, Int32(child))
            end
        end
    end
end

function multiq_size(tpid::Int8)
    nt = UInt32(Threads._nthreads_in_pool(tpid))
    tp = tpid + 1
    tpheaps = heaps[tp]
    heap_c = UInt32(2)
    heap_p = UInt32(length(tpheaps))

    if heap_c * nt <= heap_p
        return heap_p
    end

    @lock heaps_lock[tp] begin
        heap_p = UInt32(length(tpheaps))
        nt = UInt32(Threads._nthreads_in_pool(tpid))
        if heap_c * nt <= heap_p
            return heap_p
        end

        heap_p += heap_c * nt
        newheaps = Vector{taskheap}(undef, heap_p)
        copyto!(newheaps, tpheaps)
        for i = (1 + length(tpheaps)):heap_p
            newheaps[i] = taskheap()
        end
        heaps[tp] = newheaps
    end

    return heap_p
end

function multiq_insert(task::Task, priority::UInt16)
    tpid = ccall(:jl_get_task_threadpoolid, Int8, (Any,), task)
    @assert tpid > -1
    heap_p = multiq_size(tpid)
    tp = tpid + 1

    task.priority = priority

    rn = cong(heap_p)
    tpheaps = heaps[tp]
    while !trylock(tpheaps[rn].lock)
        rn = cong(heap_p)
    end

    heap = tpheaps[rn]
    if heap.ntasks >= length(heap.tasks)
        resize!(heap.tasks, length(heap.tasks) * 2)
    end

    ntasks = heap.ntasks + Int32(1)
    @atomic :monotonic heap.ntasks = ntasks
    heap.tasks[ntasks] = task
    multiq_sift_up(heap, ntasks)
    priority = heap.priority
    if task.priority < priority
        @atomic :monotonic heap.priority = task.priority
    end
    unlock(heap.lock)

    return true
end

function multiq_deletemin()
    local rn1::UInt32

    tid = Threads.threadid()
    tp = ccall(:jl_threadpoolid, Int8, (Int16,), tid-1) + 1
    if tp == 0 # Foreign thread
        return nothing
    end
    tpheaps = heaps[tp]

    @label retry
    GC.safepoint()
    heap_p = UInt32(length(tpheaps))
    for i = UInt32(0):heap_p
        if i == heap_p
            return nothing
        end
        rn1 = cong(heap_p)
        rn2 = cong(heap_p)
        prio1 = tpheaps[rn1].priority
        prio2 = tpheaps[rn2].priority
        if prio1 > prio2
            prio1 = prio2
            rn1 = rn2
        elseif prio1 == prio2 && prio1 == typemax(UInt16)
            continue
        end
        if trylock(tpheaps[rn1].lock)
            if prio1 == tpheaps[rn1].priority
                break
            end
            unlock(tpheaps[rn1].lock)
        end
    end

    @assert @isdefined(rn1) "Assertion to tell the compiler about the definedness of this variable"

    heap = tpheaps[rn1]
    task = heap.tasks[1]
    if ccall(:jl_set_task_tid, Cint, (Any, Cint), task, tid-1) == 0
        unlock(heap.lock)
        @goto retry
    end
    ntasks = heap.ntasks
    @atomic :monotonic heap.ntasks = ntasks - Int32(1)
    heap.tasks[1] = heap.tasks[ntasks]
    Base._unsetindex!(heap.tasks, Int(ntasks))
    prio1 = typemax(UInt16)
    if ntasks > 1
        multiq_sift_down(heap, Int32(1))
        prio1 = heap.tasks[1].priority
    end
    @atomic :monotonic heap.priority = prio1
    unlock(heap.lock)

    return task
end

function multiq_check_empty()
    tid = Threads.threadid()
    tp = ccall(:jl_threadpoolid, Int8, (Int16,), tid-1) + 1
    if tp == 0 # Foreign thread
        return true
    end
    for i = UInt32(1):length(heaps[tp])
        if heaps[tp][i].ntasks != 0
            return false
        end
    end
    return true
end

end

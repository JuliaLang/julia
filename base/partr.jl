# This file is a part of Julia. License is MIT: https://julialang.org/license

module Partr

using ..Threads: SpinLock, nthreads

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
global heaps::Vector{taskheap} = Vector{taskheap}(undef, 0)
const heaps_lock = SpinLock()
global cong_unbias::UInt32 = typemax(UInt32)


cong(max::UInt32, unbias::UInt32) = ccall(:jl_rand_ptls, UInt32, (UInt32, UInt32), max, unbias) + UInt32(1)

function unbias_cong(max::UInt32)
    return typemax(UInt32) - ((typemax(UInt32) % max) + UInt32(1))
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


function multiq_size()
    heap_c = UInt32(2)
    heap_p = UInt32(length(heaps))
    nt = UInt32(nthreads())

    if heap_c * nt <= heap_p
        return heap_p
    end

    @lock heaps_lock begin
        heap_p = UInt32(length(heaps))
        nt = UInt32(nthreads())
        if heap_c * nt <= heap_p
            return heap_p
        end

        heap_p += heap_c * nt
        newheaps = Vector{taskheap}(undef, heap_p)
        copyto!(newheaps, heaps)
        for i = (1 + length(heaps)):heap_p
            newheaps[i] = taskheap()
        end
        global heaps = newheaps
        global cong_unbias = unbias_cong(heap_p)
    end

    return heap_p
end


function multiq_insert(task::Task, priority::UInt16)
    task.priority = priority

    heap_p = multiq_size()
    rn = cong(heap_p, cong_unbias)
    while !trylock(heaps[rn].lock)
        rn = cong(heap_p, cong_unbias)
    end

    heap = heaps[rn]
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
    local rn1, rn2
    local prio1, prio2

    @label retry
    GC.safepoint()
    heap_p = UInt32(length(heaps))
    for i = UInt32(0):heap_p
        if i == heap_p
            return nothing
        end
        rn1 = cong(heap_p, cong_unbias)
        rn2 = cong(heap_p, cong_unbias)
        prio1 = heaps[rn1].priority
        prio2 = heaps[rn2].priority
        if prio1 > prio2
            prio1 = prio2
            rn1 = rn2
        elseif prio1 == prio2 && prio1 == typemax(UInt16)
            continue
        end
        if trylock(heaps[rn1].lock)
            if prio1 == heaps[rn1].priority
                break
            end
            unlock(heaps[rn1].lock)
        end
    end

    heap = heaps[rn1]
    task = heap.tasks[1]
    tid = Threads.threadid()
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
    for i = UInt32(1):length(heaps)
        if heaps[i].ntasks != 0
            return false
        end
    end
    return true
end

end

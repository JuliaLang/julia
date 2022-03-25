# This file is a part of Julia. License is MIT: https://julialang.org/license

module Partr

using ..Threads: SpinLock

# a task heap
mutable struct taskheap
    const lock::SpinLock
    const tasks::Vector{Task}
    @atomic ntasks::Int32
    @atomic priority::UInt16
    taskheap() = new(SpinLock(), Vector{Task}(undef, tasks_per_heap), zero(Int32), typemax(UInt16))
end

# multiqueue parameters
const heap_d = UInt32(8)
const heap_c = UInt32(2)

# size of each heap
const tasks_per_heap = Int32(65536) # TODO: this should be smaller by default, but growable!

# the multiqueue's heaps
global heaps::Vector{taskheap}
global heap_p::UInt32 = 0

# unbias state for the RNG
global cong_unbias::UInt32 = 0


cong(max::UInt32, unbias::UInt32) = ccall(:jl_rand_ptls, UInt32, (UInt32, UInt32), max, unbias) + UInt32(1)

function unbias_cong(max::UInt32)
    return typemax(UInt32) - ((typemax(UInt32) % max) + UInt32(1))
end


function multiq_init(nthreads)
    global heap_p = heap_c * nthreads
    global cong_unbias = unbias_cong(UInt32(heap_p))
    global heaps = Vector{taskheap}(undef, heap_p)
    for i = UInt32(1):heap_p
        heaps[i] = taskheap()
    end
    nothing
end


function sift_up(heap::taskheap, idx::Int32)
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


function sift_down(heap::taskheap, idx::Int32)
    if idx <= heap.ntasks
        for child = (heap_d * idx - heap_d + Int32(2)):(heap_d * idx + Int32(1))
            child > tasks_per_heap && break
            if isassigned(heap.tasks, child) &&
                    heap.tasks[child].priority < heap.tasks[idx].priority
                t = heap.tasks[idx]
                heap.tasks[idx] = heap.tasks[child]
                heap.tasks[child] = t
                sift_down(heap, child)
            end
        end
    end
end


function multiq_insert(task::Task, priority::UInt16)
    task.priority = priority

    rn = cong(heap_p, cong_unbias)
    while !trylock(heaps[rn].lock)
        rn = cong(heap_p, cong_unbias)
    end

    if heaps[rn].ntasks >= tasks_per_heap
        unlock(heaps[rn].lock)
        # multiq insertion failed, increase #tasks per heap
        return false
    end

    ntasks = heaps[rn].ntasks + Int32(1)
    @atomic :monotonic heaps[rn].ntasks = ntasks
    heaps[rn].tasks[ntasks] = task
    sift_up(heaps[rn], ntasks)
    priority = heaps[rn].priority
    if task.priority < priority
        @atomic :monotonic heaps[rn].priority = task.priority
    end
    unlock(heaps[rn].lock)
    return true
end


function multiq_deletemin()
    local rn1, rn2
    local prio1, prio2

    @label retry
    GC.safepoint()
    for i = UInt32(1):heap_p
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

    task = heaps[rn1].tasks[1]
    tid = Threads.threadid()
    if ccall(:jl_set_task_tid, Cint, (Any, Cint), task, tid-1) == 0
        unlock(heaps[rn1].lock)
        @goto retry
    end
    ntasks = heaps[rn1].ntasks
    @atomic :monotonic heaps[rn1].ntasks = ntasks - Int32(1)
    heaps[rn1].tasks[1] = heaps[rn1].tasks[ntasks]
    Base._unsetindex!(heaps[rn1].tasks, Int(ntasks))
    prio1 = typemax(UInt16)
    if ntasks > 1
        sift_down(heaps[rn1], Int32(1))
        prio1 = heaps[rn1].tasks[1].priority
    end
    @atomic :monotonic heaps[rn1].priority = prio1
    unlock(heaps[rn1].lock)

    return task
end


function multiq_check_empty()
    for i = UInt32(1):heap_p
        if heaps[i].ntasks != 0
            return false
        end
    end
    return true
end

end

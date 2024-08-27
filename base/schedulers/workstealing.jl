# This file is a part of Julia. License is MIT: https://julialang.org/license

module Workstealing

# Expected interface for a work-stealing queue:
#    push!(queue, task)
#    pop!(queue) # Only legal if you are the queues owner.
#    steal!(queue)
include("schedulers/CLL.jl")
include("schedulers/CDLL.jl")

# Threadpool utilities
function cur_threadpoolid()
    return ccall(:jl_cur_threadpoolid, Int8, ()) + 1
end

function cur_threadpool_tid()
    return ccall(:jl_cur_threadpool_tid, Int16, ()) + 1
end

function get_task_tpid(task::Task)
    return ccall(:jl_get_task_threadpoolid, Int8, (Any,), task) + 1
end

# Logic for threadpools:
# Each thread has a global thread id, always called tid and unique per thread.
# Accesed via Threads.threadid() for a thread and Threads.threadid(task) for a task.

# Each threadpool has an id, called tpid and unique per threadpool.
# Accessed via cur_threadpoolid() for the current thread and task_tpid for a task.

# Each thread also has a threadpool_tid, called tp_tid, its use is to index into the array of queues for the threadpool.
# Accessed via cur_threadpool_tid() for the current thread. Or by checking if it's in the Threads.threadpooltids array.
# It's calculated by doing Threads.threadid() - Threads.threadpooltids(tpid)[1], though we store in the thread ptls for performance.

# The calls return 1 based indexed numbers so threadpool 1 is :interactive and 2 is :default
# When a thread has either a tp_tid of 0 or a tpid of 0 it means that they aren't associated with a threadpool and should be inserted in the index 1 of the tasks tpid


function release_copyto!(dest::AtomicMemory{T}, src::AbstractArray{T,1}) where T
    Base._checkaxs(axes(dest), axes(src))
    for i in eachindex(src)
        @atomic :monotonic dest[i] = src[i]
    end
    Core.Intrinsics.atomic_fence(:release)
    return dest
end

make_atomic(x::AbstractArray{T,1}) where {T} = release_copyto!(AtomicMemory{T}(undef, size(x)), x)

const QueueModule = ConcurrentList
const Queue = QueueModule.Queue{Task}
const Queues_lock = Threads.SpinLock()
global Queues::AtomicMemory{Memory{Queue}} = make_atomic([Memory{Queue}([Queue()]) for _ in 1:Threads.nthreadpools()]) # One array of queues per threadpool

function queue_for(tp_tid::Int, tpid::Int)
    @assert tp_tid >= 0
    qs = @atomic :monotonic Queues[tpid]
    if (tp_tid == 0)
        queue_index = 1 # We always have a queue for someone that isn't us to push to
    else
        queue_index = tp_tid + 1
    end
    if length(qs) >= queue_index && isassigned(qs, queue_index)
        return qs[queue_index]
    end
    # slow path to allocate it
    # TODO: outline this
    l = Queues_lock
    @lock l begin
        qs = @atomic :monotonic Queues[tpid]
        if length(qs) < queue_index
            nt = Threads._nthreads_in_pool(Int8(tpid - 1)) + 1
            @assert queue_index <= nt
            new_q = copyto!(typeof(qs)(undef, length(qs) + nt - 1), qs)
            qs = new_q
            @atomic :monotonic Queues[tpid] = new_q
        end
        if !isassigned(qs, queue_index)
            qs[queue_index] = Queue()
        end
        return qs[queue_index]
    end
end

function enqueue!(t::Task)
    task_tpid = get_task_tpid(t)
    thread_tpid = cur_threadpoolid()
    if task_tpid == thread_tpid
        push!(queue_for(Int(cur_threadpool_tid()), Int(thread_tpid)), t)
    else
        push!(queue_for(0, Int(task_tpid)), t)
    end
    return nothing
end

function dequeue!()
    tpid = cur_threadpoolid()
    tp_tid = cur_threadpool_tid()
    tid = Threads.threadid()
    q = queue_for(Int(tp_tid), Int(tpid))
    t = pop!(q) # Check own queue first
    if t !== nothing
        if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
            push!(q, t) # Is there a way to avoid popping the same unrunnable task over and over?
            ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
        else
            return t
        end
    end
    t = attempt_steal!(Int(tp_tid), Int(tpid)) # Otherwise try to steal from others
    return t
end

function attempt_steal!(tp_tid::Int, tpid::Int)
    tid = Threads.threadid()
    nt = Threads._nthreads_in_pool(Int8(tpid - 1))
    for _ in 1:(4*nt) # Try to steal 4x nthread times
        tp_tid2 = Base.Scheduler.cong(UInt32(nt + 1)) - 1 # From 0 to nt since queue_for uses 0 for the foreign queue
        tp_tid == tp_tid2 && continue
        t = QueueModule.steal!(queue_for(Int(tp_tid2), Int(tpid))) #TODO: Change types of things to avoid the convert
        if t !== nothing
            if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
                push!(queue_for(0, Int(get_task_tpid(t))), t)
                ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
            else
                return t
            end
        end
    end
    for i in 0:(nt) # Try to steal from other threads round robin
        t = QueueModule.steal!(queue_for(Int(i), Int(tpid))) #TODO: Change types of things to avoid the convert
        if t !== nothing
            if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
                push!(queue_for(0, Int(get_task_tpid(t))), t)
                ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
            else
                return t
            end
        end
    end
    return nothing
end

function checktaskempty()
    qs = @atomic :monotonic Queues[cur_threadpoolid()]
    for i in eachindex(qs)
        if isassigned(qs, i)
            q = qs[i]
            if !isempty(q)
                return false
            end
        end
    end
    return true
end

end

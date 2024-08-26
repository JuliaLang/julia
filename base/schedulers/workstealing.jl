# This file is a part of Julia. License is MIT: https://julialang.org/license

module Workstealing

# Expected interface for a work-stealing queue:
#    push!(queue, task)
#    pop!(queue) # Only legal if you are the queues owner.
#    steal!(queue)
include("schedulers/CLL.jl")
include("schedulers/CDLL.jl")

const QueueModule = ConcurrentList
const Queue = QueueModule.Queue{Task}
const Queues_lock = Threads.SpinLock()
global Queues::Memory{Queue} = Memory{Queue}([Queue()])
function queue_for(tid::Int)
    qs = Queues
    if length(qs) >= tid && isassigned(qs, tid)
        return @inbounds qs[tid]
    end
    # slow path to allocate it
    # TODO: outline this
    @assert tid > 0
    l = Queues_lock
    @lock l begin
        qs = Queues
        if length(qs) < tid
            nt = Threads.maxthreadid()
            @assert tid <= nt
            global Queues = qs = copyto!(typeof(qs)(undef, length(qs) + nt - 1), qs)
        end
        if !isassigned(qs, tid)
            @inbounds qs[tid] = Queue()
        end
        return @inbounds qs[tid]
    end
end

function enqueue!(t::Task)
    # TODO: threadpools?
    push!(queue_for(Threads.threadid()), t)
    return nothing
end

function dequeue!()
    tid = Threads.threadid()
    q = queue_for(Threads.threadid())
    t = pop!(q) # Check own queue first
    if t !== nothing
        if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
            push!(q, t) # Is there a way to avoid popping the same unrunnable task over and over?
            ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
        else
            return t
        end
    end
    t = attempt_steal!() # Otherwise try to steal from others
    return t
end

function attempt_steal!()
    nt = Threads.maxthreadid()
    tid = Threads.threadid()
    for _ in 1:(4*nt) # Try to steal 4x nthread times
        tid2 = Base.Scheduler.cong(UInt32(nt))
        tid == tid2 && continue
        t = QueueModule.steal!(queue_for(Int(tid2))) #TODO: Change types of things to avoid the convert
        if t !== nothing
            if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
                push!(queue_for(tid), t)
                ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
            else
                return t
            end
        end
    end
    for i in 1:(nt) # Try to steal from other threads round robin
        t = QueueModule.steal!(queue_for(Int(i))) #TODO: Change types of things to avoid the convert
        if t !== nothing
            if ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1) == 0
                push!(queue_for(tid), t)
                ccall(:jl_wakeup_thread, Cvoid, (Int16,), (Threads.threadid(t) - 1) % Int16)
            else
                return t
            end
        end
    end
    return nothing
end

function checktaskempty()
    qs = Queues
    for i in eachindex(qs)
        q = @inbounds qs[i]
        if !isempty(q)
            return false
        end
    end
    return true
end

end

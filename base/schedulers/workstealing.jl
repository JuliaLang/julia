# This file is a part of Julia. License is MIT: https://julialang.org/license

module Workstealing

# Expected interface for a work-stealing queue:
#    push!(queue, task)
#    pop!(queue) # Only legal if you are the queues owner.
#    steal!(queue)
include("schedulers/CLL.jl")

const QueueModule = CLL
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
    q = queue_for(Threads.threadid())
    t = pop!(q) # Check own queue first
    t !== nothing && return t
    return attempt_steal!() # Otherwise try to steal from others
end

function attempt_steal!()
    nt = Threads.maxthreadid()
    tid = Threads.threadid()
    for _ in 1:(4*nt) # Try to steal 4x nthread times
        tid2 = Base.Scheduler.cong(UInt32(nt))
        tid == tid2 && continue
        t = QueueModule.steal!(queue_for(tid2))
        t !== nothing && return t
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

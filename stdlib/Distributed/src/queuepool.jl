export QueuePool, close, push!, take!
import Base: close, push!, take!

"""
    QueuePool

A pool of workers that asynchronously execute jobs passed to them, with
facilities for both in-order and out-of-order taking of results.  This
construct is intended to act like an asynchronous `pmap()`, allowing streaming
parallel operations to dynamically add jobs to a work queue and take results
with a minimum of fuss. To use, construct a `QueuePool`, submit work items with
`push!()`, and get output through `take!()`.  Example usage:

```julia
using Distributed

workers = addprocs(Sys.CPU_THREADS-1)

using Statistics
@everywhere function do_work(x, y)
    # Be sure to import Statistics for `mean()`
    z = mean((x .+ y).^2)
    sleep(1 + sum(z))
    return z
end

# Create QueuePool to call `do_work()` on every submitted job
qp = QueuePool(workers, do_work; setup=:(using Statistics))

# Queue many jobs (they will immediately start processing)
for idx in 1:10000
    push!(qp, randn(10, 10), randn(10))
end

# pull results out, first asking for specific job IDs.  Note that if we were to
# request a job ID too far into the future, this would pause interminably, as
# the worker threads would run up against the default `queue_size` limit of 128,
# refusing to calculate further results until an earlier result is taken, so
# if taking specific results is important, be sure to that the `queue_size` is
# set large enough to guarantee that all results will be takeable.
take!(qp, 1)
take!(qp, 10)
take!(qp, 100)

# Next, just pull out whatever results are available quickest:
[take!(qp) for idx in 1:222]

# Finally, take a result from far in the future, but timing out if it doesn't
# show up within 10 seconds:
try
    take!(qp, 9999; timeout=10.0)
catch
    println("Could not take jod_id 9999!")
end
```
"""
mutable struct QueuePool
    # The worker PIDs
    workers::Vector{Int}

    # The setup code we execute on the workers
    setup::Expr

    # Channels for communication
    queued_jobs::RemoteChannel
    results::RemoteChannel
    kill_switch::RemoteChannel

    # The ID of the next job to be submitted
    next_job::Int

    # Buffer space where we store results for out-of-order execution
    results_buffer::Dict{Int,Any}
end

"""
    QueuePool(workers, proc_func; setup = nothing, queue_size=128)

Construct a QueuePool that wraps the `workers` worker processes, each executing
`proc_func()` on items submitted to the `QueuePool` via the `push!()`
method. If the quoted expression `setup` is given, it will be executed once on
each worker before `proc_func` is ever called.  Queued jobs can be added
without limit, however the workers will process results only up to the given
`queue_size` limit; if more than `queue_size` results are waiting to be
taken, further result calculation will be blocked.
"""
function QueuePool(workers::Vector, proc_func::Function; setup::Expr=quote nothing end, queue_size::Int=128)
    # Create our QueuePool
    qp = QueuePool(
        workers,
        setup,
        RemoteChannel(() -> Channel{Tuple}(Inf)),
        RemoteChannel(() -> Channel{Tuple}(queue_size)),
        RemoteChannel(() -> Channel{Bool}(1)),
        1,
        Dict{Int,Any}(),
    )

    # Launch workers, running the `worker_task` with a handle to this QueuePool object
    # and the processing function that will be called within the worker loop.
    for id in workers
        remote_do(worker_task, id, qp, proc_func)
    end

    # Set a finalizer to put the kill switch onto workers if `qp` gets GC'd
    finalizer(close, qp)

    # Return the queuepool
    return qp
end


"""
    close(qp::QueuePool)

Close the given `QueuePool`, breaking all workers out of their `worker_task`.
"""
function close(qp::QueuePool)
    # Tell the worker processes to quit out
    put!(qp.kill_switch, true)
    return nothing
end

function worker_task(qp::QueuePool, proc_func)
    # Run the setup on this worker
    Core.eval(Main, qp.setup)

    # Loop unless we're burning this whole queue pool down
    while !isready(qp.kill_switch)
        local job_id, args, y

        # Grab the next queued job from the master
        try
            job_id, args = take!(qp.queued_jobs)
        catch e
            # If we can't `take!()`, zoom to the kill switch check
            if isa(e, InvalidStateException)
                continue
            end
            rethrow(e)
        end

        try
            # Push x through proc_func to get y
            y = proc_func(args...)
        catch e
            if isa(e, InterruptException)
                rethrow(e)
            end
            # Just skip bad processing runs
            continue
        end

        # Push the result onto qp.results
        put!(qp.results, (job_id, y))
    end
end

"""
    push!(qp::QueuePool, args...)

Push a new job onto the QueuePool, returning the associated job id with this
job, for future usage with `take!(qp, job_id)`.  `args...` will be passed
to the `proc_func` within `qp`.
"""
function push!(qp::QueuePool, args...)
    job_id = qp.next_job
    qp.next_job += 1

    put!(qp.queued_jobs, (job_id, args))
    return job_id
end

"""
    take!(qp::QueuePool; timeout = Inf)

Return a result from the QueuePool, regardless of order.  By default, will wait
for forever; set `timeout` to a value in seconds to time out and throw an error
if a value does not arrive.
"""
function take!(qp::QueuePool; timeout::Number = Inf)
    # Wait until we either have a result within results_buffer,
    # are ready to pull something out of qp.results, or we timeout.
    ret = timedwait(
        () -> !isempty(qp.results_buffer) || isready(qp.results),
        Float64(timeout);
        pollint=0.1,
    )

    if ret == :error
        error("Timeout within take!()")
    end

    if isempty(qp.results_buffer)
        # Don't even bother sticking it onto the results_buffer,
        # just return it immediately.
        job_id, result = take!(qp.results)
        return result
    end

    # Take the first thing off of the results_buffer
    return pop!(qp.results_buffer).second
end

"""
    take!(qp::QueuePool, job_id::Int; timeout = Inf)

Return a result from the QueuePool, in specific order.  By default, will wait
for forever; set `timeout` to a value in seconds to time out and throw an error
if a value does not arrive.
"""
function take!(qp::QueuePool, job_id::Int; timeout::Number = Inf)
    t_horizon = time() + Float64(timeout)
    while !haskey(qp.results_buffer, job_id)
        # Wait until a new result is available
        ret = timedwait(() -> isready(qp.results), t_horizon - time(); pollint=0.1)
        if ret == :timed_out
            error("Timeout within take!()")
        end

        # Push it onto results_buffer
        new_job_id, result = take!(qp.results)
        qp.results_buffer[new_job_id] = result
    end

    # Return the relevant entry from the results_buffer
    return pop!(qp.results_buffer, job_id)
end

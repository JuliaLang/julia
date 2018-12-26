export QueuePool, close, push_job!, fetch_result
import Base: close

"""
    QueuePool

A pool of workers that asynchronously execute jobs passed to them, with
facilities for both in-order and out-of-order result fetching.  This construct
is intended to act like an asynchronous `pmap()`, allowing streaming parallel
operations to dynamically add jobs to a work queue and fetch results with a
minimum of fuss. To use, construct a `QueuePool`, submit work items with
`push_job!()`, and get output through `fetch_result()`.  Example usage:

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
    push_job!(qp, randn(10, 10), randn(10))
end

# pull results out, first asking for specific job IDs.  Note that if we were to
# request a job ID too far into the future, this would pause interminably, as
# the worker threads would run up against the default `queue_size` limit of 128,
# refusing to calculate further results until an earlier result is fetched, so
# if fetching specific results is important, be sure to that the `queue_size` is
# set large enough to guarantee that all results will be fetchable.
fetch_result(qp, 1)
fetch_result(qp, 10)
fetch_result(qp, 100)

# Next, just pull out whatever results are available quickest:
[fetch_result(qp) for idx in 1:222]

# Finally, fetch a result from far in the future, but timing out if it doesn't
# show up within 10 seconds:
try
    fetch_result(qp, 9999; timeout=10.0)
catch
    println("Could not fetch jod_id 9999!")
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
`proc_func()` on items submitted to the `QueuePool` via the `push_job!()`
method. If the quoted expression `setup` is given, it will be executed once on
each worker before `proc_func` is ever called.  Queued jobs can be added
without limit, however the workers will process results only up to the given
`queue_size` limit; if more than `queue_size` results are waiting to be
fetched, further result calculation will be blocked.
"""
function QueuePool(workers::Vector{Int}, proc_func::Function; setup::Expr=quote nothing end, queue_size::Int=128)
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

    # Return the queuepool
    return qp
end

"""
    QueuePool(num_workers, proc_func, setup = nothing, queue_size=128)

A variant of the `QueuePool` constructor that takes the number of workers to be
created rather than already-created worker IDs.
"""
function QueuePool(num_workers::Int, args...; kwargs...)
    # Auto-create the workers
    workers = addprocs(num_workers)

    return QueuePool(workers, args...; kwargs...)
end

"""
    close(qp::QueuePool)

Close the given `QueuePool`, destroying all queue workers
"""
function close(qp::QueuePool)
    # Tell the worker processes to die
    put!(qp.kill_switch, true)

    # Wait for the workers to descend into the long, dark sleep
    rmprocs(qp.workers...; waitfor=10)
end

function worker_task(qp::QueuePool, proc_func)
    # Tell the workers to include this file and whatever other setup they need,
    # so that they can communicate with us and complete their tasks.
    #include(@__FILE__)
    Core.eval(Main, qp.setup)

    # Loop unless we're burning this whole queue pool down
    while !isready(qp.kill_switch)
        # Grab the next queued job from the master
        local job_id, args
        try
            job_id, args = take!(qp.queued_jobs)
        catch e
            # If we can't `take!()`, zoom to the kill switch check
            if isa(e, InvalidStateException)
                continue
            end
            rethrow(e)
        end

        local y
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
    try_buffer_result!(qp::QueuePool)

Does a nonblocking read of the next result from the QueuePool into our result
buffer.  If no result is available, returns `nothing` immediately.
"""
function try_buffer_result!(qp::QueuePool)
    if isready(qp.results)
        job_id, result = take!(qp.results)
        qp.results_buffer[job_id] = result
        return job_id
    end
    return
end

# Check to see if it's `nothing` and `yield()` if it is.
function try_buffer_result!(qp::QueuePool, t_start::Number, timeout::Nothing)
    if try_buffer_result!(qp) == nothing
        # No new results available, so just yield
        yield()
    end
end

# Check to see if we've broken through our timeout
function try_buffer_result!(qp::QueuePool, t_start::Number, timeout::Number)
    try_buffer_result!(qp, t_start, nothing)

    if (time() - t_start) > timeout
        error("timeout within fetch_result")
    end
end



"""
    push_job!(qp::QueuePool, args...)

Push a new job onto the QueuePool, returning the associated job id with this
job, for future usage with `fetch_result(qp, job_id)`.  `args...` will be passed
to the `proc_func` within `qp`.
"""
function push_job!(qp::QueuePool, args...)
    job_id = qp.next_job
    qp.next_job += 1

    put!(qp.queued_jobs, (job_id, args))
    return job_id
end

"""
    fetch_result(qp::QueuePool; timeout = nothing)

Return a result from the QueuePool, regardless of order.  By default, will wait
for forever; set `timeout` to a value in seconds to time out and throw an error
if a value does not arrive.
"""
function fetch_result(qp::QueuePool; timeout = nothing)
    # If we don't have any results buffered, then pull one in
    t_start = time()
    while isempty(qp.results_buffer)
        try_buffer_result!(qp, t_start, timeout)
    end
    return pop!(qp.results_buffer).second
end

"""
    fetch_result(qp::QueuePool, job_id::Int; timeout = nothing)

Return a result from the QueuePool, in specific order.  By default, will wait
for forever; set `timeout` to a value in seconds to time out and throw an error
if a value does not arrive.
"""
function fetch_result(qp::QueuePool, job_id::Int; timeout=nothing)
    # Keep accumulating results until we get the job_id we're interested in.
    t_start = time()
    while !haskey(qp.results_buffer, job_id)
        try_buffer_result!(qp, t_start, timeout)
    end
    return pop!(qp.results_buffer, job_id)
end

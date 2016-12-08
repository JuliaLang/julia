# This file is a part of Julia. License is MIT: http://julialang.org/license

type BatchProcessingError <: Exception
    data
    ex
end

"""
    pgenerate([::WorkerPool], f, c...) -> iterator

Apply `f` to each element of `c` in parallel using available workers and tasks.

For multiple collection arguments, apply `f` elementwise.

Results are returned in order as they become available.

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](@ref)
for details.
"""
function pgenerate(p::WorkerPool, f, c)
    if length(p) == 0
        return AsyncGenerator(f, c; ntasks=()->nworkers(p))
    end
    batches = batchsplit(c, min_batch_count = length(p) * 3)
    return Iterators.flatten(AsyncGenerator(remote(p, b -> asyncmap(f, b)), batches))
end
pgenerate(p::WorkerPool, f, c1, c...) = pgenerate(p, a->f(a...), zip(c1, c...))
pgenerate(f, c) = pgenerate(default_worker_pool(), f, c)
pgenerate(f, c1, c...) = pgenerate(a->f(a...), zip(c1, c...))

"""
    pmap([::AbstractWorkerPool], f, c...; distributed=true, batch_size=1, on_error=nothing, retry_n=0, retry_max_delay=DEFAULT_RETRY_MAX_DELAY, retry_on=DEFAULT_RETRY_ON) -> collection

Transform collection `c` by applying `f` to each element using available
workers and tasks.

For multiple collection arguments, apply `f` elementwise.

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](@ref)
for details.

If a worker pool is not specified, all available workers, i.e., the default worker pool
is used.

By default, `pmap` distributes the computation over all specified workers. To use only the
local process and distribute over tasks, specify `distributed=false`.
This is equivalent to [`asyncmap`](@ref).

`pmap` can also use a mix of processes and tasks via the `batch_size` argument. For batch sizes
greater than 1, the collection is split into multiple batches, which are distributed across
workers. Each such batch is processed in parallel via tasks in each worker. The specified
`batch_size` is an upper limit, the actual size of batches may be smaller and is calculated
depending on the number of workers available and length of the collection.

Any error stops pmap from processing the remainder of the collection. To override this behavior
you can specify an error handling function via argument `on_error` which takes in a single argument, i.e.,
the exception. The function can stop the processing by rethrowing the error, or, to continue, return any value
which is then returned inline with the results to the caller.

Failed computation can also be retried via `retry_on`, `retry_n`, `retry_max_delay`, which are passed through
to `retry` as arguments `retry_on`, `n` and `max_delay` respectively. If batching is specified, and an entire batch fails,
all items in the batch are retried.

The following are equivalent:

* `pmap(f, c; distributed=false)` and `asyncmap(f,c)`
* `pmap(f, c; retry_n=1)` and `asyncmap(retry(remote(f)),c)`
* `pmap(f, c; retry_n=1, on_error=e->e)` and `asyncmap(x->try retry(remote(f))(x) catch e; e end, c)`
"""
function pmap(p::AbstractWorkerPool, f, c; distributed=true, batch_size=1, on_error=nothing,
                                           retry_n=0,
                                           retry_max_delay=DEFAULT_RETRY_MAX_DELAY,
                                           retry_on=DEFAULT_RETRY_ON,
                                           # deprecated keyword args:
                                           err_retry=nothing, err_stop=nothing, pids=nothing)
    #15409
    if err_retry !== nothing
        depwarn("err_retry is deprecated, use pmap(retry(f), c...).", :pmap)
        if err_retry == true
            f = retry(f)
        end
    end
    if pids !== nothing
        depwarn("pids is deprecated, use pmap(::WorkerPool, f, c...).", :pmap)
        p = WorkerPool(pids)
    end
    if err_stop !== nothing
        depwarn("err_stop is deprecated, use pmap(f, c...; on_error = error_handling_func).", :pmap)
        if err_stop === false
            on_error = e->e
        end
    end

    f_orig = f
    # Don't do remote calls if there are no workers.
    if (length(p) == 0) || (length(p) == 1 && fetch(p.channel) == myid())
        distributed = false
    end

    # Don't do batching if not doing remote calls.
    if !distributed
        batch_size = 1
    end

    # If not batching, do simple remote call.
    if batch_size == 1
        if distributed
            f = remote(p, f)
        end

        if retry_n > 0
            f = wrap_retry(f, retry_on, retry_n, retry_max_delay)
        end
        if on_error !== nothing
            f = wrap_on_error(f, on_error)
        end

        return asyncmap(f, c; ntasks=()->nworkers(p))
    else
        # During batch processing, We need to ensure that if on_error is set, it is called
        # for each element in error, and that we return as many elements as the original list.
        # retry, if set, has to be called element wise and we will do a best-effort
        # to ensure that we do not call mapped function on the same element more than retry_n.
        # This guarantee is not possible in case of worker death / network errors, wherein
        # we will retry the entire batch on a new worker.
        if (on_error !== nothing) || (retry_n > 0)
            f = wrap_on_error(f, (x,e)->BatchProcessingError(x,e); capture_data=true)
        end
        f = wrap_batch(f, p, on_error)
        results = asyncmap(f, c; ntasks=()->nworkers(p), batch_size=batch_size)

        # handle error processing....
        if (on_error !== nothing) || (retry_n > 0)
            process_batch_errors!(p, f_orig, results, on_error, retry_on, retry_n, retry_max_delay)
        end

        return results
    end
end

pmap(p::AbstractWorkerPool, f, c1, c...; kwargs...) = pmap(p, a->f(a...), zip(c1, c...); kwargs...)
pmap(f, c; kwargs...) = pmap(default_worker_pool(), f, c; kwargs...)
pmap(f, c1, c...; kwargs...) = pmap(a->f(a...), zip(c1, c...); kwargs...)

function wrap_on_error(f, on_error; capture_data=false)
    return x -> begin
        try
            f(x)
        catch e
            if capture_data
                on_error(x, e)
            else
                on_error(e)
            end
        end
    end
end

wrap_retry(f, retry_on, n, max_delay) = retry(f, retry_on; n=n, max_delay=max_delay)

function wrap_batch(f, p, on_error)
    f = asyncmap_batch(f)
    return batch -> begin
        try
            remotecall_fetch(f, p, batch)
        catch e
            if on_error !== nothing
                return Any[BatchProcessingError(batch[i], e) for i in 1:length(batch)]
            else
                rethrow(e)
            end
        end
    end
end

asyncmap_batch(f) = batch -> asyncmap(x->f(x...), batch)

function process_batch_errors!(p, f, results, on_error, retry_on, retry_n, retry_max_delay)
    # Handle all the ones in error in another pmap, with batch size set to 1
    if (on_error !== nothing) || (retry_n > 0)
        reprocess = []
        for (idx, v) in enumerate(results)
            if isa(v, BatchProcessingError)
                push!(reprocess, (idx,v))
            end
        end

        if length(reprocess) > 0
            errors = [x[2] for x in reprocess]
            exceptions = [x.ex for x in errors]
            if (retry_n > 0) && all([retry_on(ex) for ex in exceptions])
                retry_n = retry_n - 1
                error_processed = pmap(p, f, [x.data for x in errors];
                                                    on_error=on_error,
                                                    retry_on=retry_on,
                                                    retry_n=retry_n,
                                                    retry_max_delay=retry_max_delay)
            elseif on_error !== nothing
                error_processed = map(on_error, exceptions)
            else
                throw(CompositeException(exceptions))
            end

            for (idx, v) in enumerate(error_processed)
                results[reprocess[idx][1]] = v
            end
        end
    end
    nothing
end

"""
    head_and_tail(c, n) -> head, tail

Returns `head`: the first `n` elements of `c`;
and `tail`: an iterator over the remaining elements.

```jldoctest
julia> a = 1:10
1:10

julia> b, c = Base.head_and_tail(a, 3)
([1,2,3],Base.Iterators.Rest{UnitRange{Int64},Int64}(1:10,4))

julia> collect(c)
7-element Array{Any,1}:
  4
  5
  6
  7
  8
  9
 10
```
"""
function head_and_tail(c, n)
    head = Vector{eltype(c)}(n)
    s = start(c)
    i = 0
    while i < n && !done(c, s)
        i += 1
        head[i], s = next(c, s)
    end
    return resize!(head, i), Iterators.rest(c, s)
end

"""
    batchsplit(c; min_batch_count=1, max_batch_size=100) -> iterator

Split a collection into at least `min_batch_count` batches.

Equivalent to `partition(c, max_batch_size)` when `length(c) >> max_batch_size`.
"""
function batchsplit(c; min_batch_count=1, max_batch_size=100)
    if min_batch_count < 1
        throw(ArgumentError("min_batch_count must be ≥ 1, got $min_batch_count"))
    end

    if max_batch_size < 1
        throw(ArgumentError("max_batch_size must be ≥ 1, got $max_batch_size"))
    end

    # Split collection into batches, then peek at the first few batches
    batches = Iterators.partition(c, max_batch_size)
    head, tail = head_and_tail(batches, min_batch_count)

    # If there are not enough batches, use a smaller batch size
    if length(head) < min_batch_count
        batch_size = max(1, div(sum(length, head), min_batch_count))
        return Iterators.partition(collect(Iterators.flatten(head)), batch_size)
    end

    return Iterators.flatten((head, tail))
end

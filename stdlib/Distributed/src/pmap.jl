# This file is a part of Julia. License is MIT: https://julialang.org/license

struct BatchProcessingError <: Exception
    data
    ex
end

"""
    pgenerate([::WorkerPool], f, c...) -> iterator

Apply `f` to each element of `c` in parallel using available workers and tasks.

For multiple collection arguments, apply `f` elementwise.

Results are returned in order as they become available.

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](@ref code-availability)
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
    pmap(f, [::AbstractWorkerPool], c...; distributed=true, batch_size=1, on_error=nothing, retry_delays=[], retry_check=nothing) -> collection

Transform collection `c` by applying `f` to each element using available
workers and tasks.

For multiple collection arguments, apply `f` elementwise.

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](@ref code-availability) for details.

If a worker pool is not specified, all available workers, i.e., the default worker pool
is used.

By default, `pmap` distributes the computation over all specified workers. To use only the
local process and distribute over tasks, specify `distributed=false`.
This is equivalent to using [`asyncmap`](@ref). For example,
`pmap(f, c; distributed=false)` is equivalent to `asyncmap(f,c; ntasks=()->nworkers())`

`pmap` can also use a mix of processes and tasks via the `batch_size` argument. For batch sizes
greater than 1, the collection is processed in multiple batches, each of length `batch_size` or less.
A batch is sent as a single request to a free worker, where a local [`asyncmap`](@ref) processes
elements from the batch using multiple concurrent tasks.

Any error stops `pmap` from processing the remainder of the collection. To override this behavior
you can specify an error handling function via argument `on_error` which takes in a single argument, i.e.,
the exception. The function can stop the processing by rethrowing the error, or, to continue, return any value
which is then returned inline with the results to the caller.

Consider the following two examples. The first one returns the exception object inline,
the second a 0 in place of any exception:
```julia-repl
julia> pmap(x->iseven(x) ? error("foo") : x, 1:4; on_error=identity)
4-element Array{Any,1}:
 1
  ErrorException("foo")
 3
  ErrorException("foo")

julia> pmap(x->iseven(x) ? error("foo") : x, 1:4; on_error=ex->0)
4-element Array{Int64,1}:
 1
 0
 3
 0
```

Errors can also be handled by retrying failed computations. Keyword arguments `retry_delays` and
`retry_check` are passed through to [`retry`](@ref) as keyword arguments `delays` and `check`
respectively. If batching is specified, and an entire batch fails, all items in
the batch are retried.

Note that if both `on_error` and `retry_delays` are specified, the `on_error` hook is called
before retrying. If `on_error` does not throw (or rethrow) an exception, the element will not
be retried.

Example: On errors, retry `f` on an element a maximum of 3 times without any delay between retries.
```julia
pmap(f, c; retry_delays = zeros(3))
```

Example: Retry `f` only if the exception is not of type [`InexactError`](@ref), with exponentially increasing
delays up to 3 times. Return a `NaN` in place for all `InexactError` occurrences.
```julia
pmap(f, c; on_error = e->(isa(e, InexactError) ? NaN : rethrow()), retry_delays = ExponentialBackOff(n = 3))
```
"""
function pmap(f, p::AbstractWorkerPool, c; distributed=true, batch_size=1, on_error=nothing,
                                           retry_delays=[], retry_check=nothing)
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
        if on_error !== nothing
            f = wrap_on_error(f, on_error)
        end

        if distributed
            f = remote(p, f)
        end

        if length(retry_delays) > 0
            f = wrap_retry(f, retry_delays, retry_check)
        end

        return asyncmap(f, c; ntasks=()->nworkers(p))
    else
        # During batch processing, We need to ensure that if on_error is set, it is called
        # for each element in error, and that we return as many elements as the original list.
        # retry, if set, has to be called element wise and we will do a best-effort
        # to ensure that we do not call mapped function on the same element more than length(retry_delays).
        # This guarantee is not possible in case of worker death / network errors, wherein
        # we will retry the entire batch on a new worker.

        handle_errors = ((on_error !== nothing) || (length(retry_delays) > 0))

        # Unlike the non-batch case, in batch mode, we trap all errors and the on_error hook (if present)
        # is processed later in non-batch mode.
        if handle_errors
            f = wrap_on_error(f, (x,e)->BatchProcessingError(x,e); capture_data=true)
        end

        f = wrap_batch(f, p, handle_errors)
        results = asyncmap(f, c; ntasks=()->nworkers(p), batch_size=batch_size)

        # process errors if any.
        if handle_errors
            process_batch_errors!(p, f_orig, results, on_error, retry_delays, retry_check)
        end

        return results
    end
end

pmap(f, p::AbstractWorkerPool, c1, c...; kwargs...) = pmap(a->f(a...), p, zip(c1, c...); kwargs...)
pmap(f, c; kwargs...) = pmap(f, default_worker_pool(), c; kwargs...)
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

function wrap_retry(f, retry_delays, retry_check)
    retry(delays=retry_delays, check=retry_check) do x
        try
            f(x)
        catch e
            rethrow(extract_exception(e))
        end
    end
end

function wrap_batch(f, p, handle_errors)
    f = asyncmap_batch(f)
    return batch -> begin
        try
            remotecall_fetch(f, p, batch)
        catch e
            if handle_errors
                return Any[BatchProcessingError(b, e) for b in batch]
            else
                rethrow()
            end
        end
    end
end

asyncmap_batch(f) = batch -> asyncmap(x->f(x...), batch)
extract_exception(e) = isa(e, RemoteException) ? e.captured.ex : e


function process_batch_errors!(p, f, results, on_error, retry_delays, retry_check)
    # Handle all the ones in error in another pmap, with batch size set to 1
    reprocess = []
    for (idx, v) in enumerate(results)
        if isa(v, BatchProcessingError)
            push!(reprocess, (idx,v))
        end
    end

    if length(reprocess) > 0
        errors = [x[2] for x in reprocess]
        exceptions = [x.ex for x in errors]
        state = iterate(retry_delays)
        state !== nothing && (state = state[2])
        if (length(retry_delays) > 0) &&
                (retry_check === nothing || all([retry_check(state,ex)[2] for ex in exceptions]))
            # BatchProcessingError.data is a tuple of original args
            error_processed = pmap(x->f(x...), p, [x.data for x in errors];
                    on_error = on_error, retry_delays = collect(retry_delays)[2:end], retry_check = retry_check)
        elseif on_error !== nothing
            error_processed = map(on_error, exceptions)
        else
            throw(CompositeException(exceptions))
        end

        for (idx, v) in enumerate(error_processed)
            results[reprocess[idx][1]] = v
        end
    end
    nothing
end

"""
    head_and_tail(c, n) -> head, tail

Return `head`: the first `n` elements of `c`;
and `tail`: an iterator over the remaining elements.

```jldoctest
julia> b, c = Distributed.head_and_tail(1:10, 3)
([1, 2, 3], Base.Iterators.Rest{UnitRange{Int64},Int64}(1:10, 3))

julia> collect(c)
7-element Array{Int64,1}:
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
    head = Vector{eltype(c)}(undef, n)
    n == 0 && return (head, c)
    i = 1
    y = iterate(c)
    y === nothing && return (resize!(head, 0), ())
    head[i] = y[1]
    while i < n
        y = iterate(c, y[2])
        y === nothing && return (resize!(head, i), ())
        i += 1
        head[i] = y[1]
    end
    return head, Iterators.rest(c, y[2])
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

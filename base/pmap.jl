# This file is a part of Julia. License is MIT: http://julialang.org/license


"""
    pgenerate([::WorkerPool], f, c...) -> iterator

Apply `f` to each element of `c` in parallel using available workers and tasks.

For multiple collection arguments, apply f elementwise.

Results are returned in order as they become available.

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](:ref:`Code Availability
and Loading Packages <man-parallel-computing-code-availability>`)
for details.
"""
function pgenerate(p::WorkerPool, f, c)
    if length(p) == 0
        return asyncgenerate(f, c)
    end
    batches = batchsplit(c, min_batch_count = length(p) * 3)
    return flatten(asyncgenerate(remote(p, b -> asyncmap(f, b)), batches))
end

pgenerate(p::WorkerPool, f, c1, c...) = pgenerate(p, a->f(a...), zip(c1, c...))

pgenerate(f, c) = pgenerate(default_worker_pool(), f, c...)
pgenerate(f, c1, c...) = pgenerate(a->f(a...), zip(c1, c...))



"""
    pmap([::WorkerPool], f, c...) -> collection

Transform collection `c` by applying `f` to each element using available
workers and tasks.

For multiple collection arguments, apply f elementwise.

Note that `err_retry=true` and `err_stop=false` are deprecated,
use `pmap(retry(f), c)` or `pmap(@catch(f), c)` instead
(or to retry on a different worker, use `asyncmap(retry(remote(f)), c)`).

Note that `f` must be made available to all worker processes; see
[Code Availability and Loading Packages](:ref:`Code Availability
and Loading Packages <man-parallel-computing-code-availability>`)
for details.
"""
pmap(p::WorkerPool, f, c...) = collect(pgenerate(p, f, c...))

# TODO: deprecated.jl defines pmap(f, c...; kw...) to support old kw args.
#       When that is retierd it should be replaced by somthing like this:
#
#       pmap(f, c...) = pmap(default_worker_pool(), f, c...)



"""
    batchsplit(c; min_batch_count=1, max_batch_size=100) -> iterator

Split a collection into at least `min_batch_count` batches.

Equivalent to `partition(c, max_batch_size)` when `length(c) >> max_batch_size`.
"""
function batchsplit(c; min_batch_count=1, max_batch_size=100)

    @assert min_batch_count > 0
    @assert max_batch_size > 1

    # Split collection into batches, then peek at the first few batches...
    batches = partition(c, max_batch_size)
    head, tail = head_and_tail(batches, min_batch_count)

    # If there are not enough batches, use a smaller batch size...
    if length(head) < min_batch_count
        batch_size = max(1, div(sum(length, head), min_batch_count))
        return partition(flatten(head), batch_size)
    end

    return flatten((head, tail))
end

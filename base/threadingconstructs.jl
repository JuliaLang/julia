# This file is a part of Julia. License is MIT: https://julialang.org/license

export threadid, nthreads, @threads, @spawn,
       threadpool, nthreadpools

public Condition, threadpoolsize, ngcthreads

"""
    Threads.threadid([t::Task])::Int

Get the ID number of the current thread of execution, or the thread of task
`t`. The master thread has ID `1`.

# Examples
```julia-repl
julia> Threads.threadid()
1

julia> Threads.@threads for i in 1:4
          println(Threads.threadid())
       end
4
2
5
4

julia> Threads.threadid(Threads.@spawn "foo")
2
```

!!! note
    The thread that a task runs on may change if the task yields, which is known as [`Task Migration`](@ref man-task-migration).
    For this reason in most cases it is not safe to use `threadid([task])` to index into, say, a vector of buffers or stateful
    objects.
"""
threadid() = Int(ccall(:jl_threadid, Int16, ())+1)

# lower bound on the largest threadid()
"""
    Threads.maxthreadid()::Int

Get a lower bound on the number of threads (across all thread pools) available
to the Julia process, with atomic-acquire semantics. The result will always be
greater than or equal to [`threadid()`](@ref) as well as `threadid(task)` for
any task you were able to observe before calling `maxthreadid`.
"""
maxthreadid() = Int(Core.Intrinsics.atomic_pointerref(cglobal(:jl_n_threads, Cint), :acquire))

"""
    Threads.nthreads(:default | :interactive)::Int

Get the current number of threads within the specified thread pool. The threads in `:interactive`
have id numbers `1:nthreads(:interactive)`, and the threads in `:default` have id numbers in
`nthreads(:interactive) .+ (1:nthreads(:default))`.

See also `BLAS.get_num_threads` and `BLAS.set_num_threads` in the [`LinearAlgebra`](@ref
man-linalg) standard library, and `nprocs()` in the [`Distributed`](@ref man-distributed)
standard library and [`Threads.maxthreadid()`](@ref).
"""
nthreads(pool::Symbol) = threadpoolsize(pool)

function _nthreads_in_pool(tpid::Int8)
    p = unsafe_load(cglobal(:jl_n_threads_per_pool, Ptr{Cint}))
    return Int(unsafe_load(p, tpid + 1))
end

function _tpid_to_sym(tpid::Int8)
    if tpid == 0
        return :interactive
    elseif tpid == 1
        return :default
    elseif tpid == -1
        return :foreign
    else
        throw(ArgumentError(LazyString("Unrecognized threadpool id ", tpid)))
    end
end

function _sym_to_tpid(tp::Symbol)
    if tp === :interactive
        return Int8(0)
    elseif tp === :default
        return Int8(1)
    elseif tp == :foreign
        return Int8(-1)
    else
        throw(ArgumentError(LazyString("Unrecognized threadpool name `", tp, "`")))
    end
end

"""
    Threads.threadpool(tid = threadid())::Symbol

Return the specified thread's threadpool; either `:default`, `:interactive`, or `:foreign`.
"""
function threadpool(tid = threadid())
    tpid = ccall(:jl_threadpoolid, Int8, (Int16,), tid-1)
    return _tpid_to_sym(tpid)
end

"""
    Threads.threadpooldescription(tid = threadid())::String

Return the specified thread's threadpool name with extended description where appropriate.
"""
function threadpooldescription(tid = threadid())
    threadpool_name = threadpool(tid)
    if threadpool_name == :foreign
        # TODO: extend tls to include a field to add a description to a foreign thread and make this more general
        n_others = nthreads(:interactive) + nthreads(:default)
        # Assumes GC threads come first in the foreign thread pool
        if tid > n_others && tid <= n_others + ngcthreads()
            return "foreign: gc"
        end
    end
    return string(threadpool_name)
end

"""
    Threads.nthreadpools()::Int

Return the number of threadpools currently configured.
"""
nthreadpools() = Int(unsafe_load(cglobal(:jl_n_threadpools, Cint)))

"""
    Threads.threadpoolsize(pool::Symbol = :default)::Int

Get the number of threads available to the default thread pool (or to the
specified thread pool).

See also: `BLAS.get_num_threads` and `BLAS.set_num_threads` in the
[`LinearAlgebra`](@ref man-linalg) standard library, and `nprocs()` in the
[`Distributed`](@ref man-distributed) standard library.
"""
function threadpoolsize(pool::Symbol = :default)
    if pool === :default || pool === :interactive
        tpid = _sym_to_tpid(pool)
    elseif pool == :foreign
        error("Threadpool size of `:foreign` is indeterminant")
    else
        error("invalid threadpool specified")
    end
    return _nthreads_in_pool(tpid)
end

"""
    threadpooltids(pool::Symbol)

Return a vector of IDs of threads in the given pool.
"""
function threadpooltids(pool::Symbol)
    ni = _nthreads_in_pool(Int8(0))
    if pool === :interactive
        return collect(1:ni)
    elseif pool === :default
        return collect(ni+1:ni+_nthreads_in_pool(Int8(1)))
    else
        error("invalid threadpool specified")
    end
end

"""
    Threads.ngcthreads()::Int

Return the number of GC threads currently configured.
This includes both mark threads and concurrent sweep threads.
"""
ngcthreads() = Int(unsafe_load(cglobal(:jl_n_gcthreads, Cint))) + 1

function threading_run(fun, static)
    ccall(:jl_enter_threaded_region, Cvoid, ())
    n = threadpoolsize()
    tid_offset = threadpoolsize(:interactive)
    tasks = Vector{Task}(undef, n)
    try
        for i = 1:n
            t = Task(() -> fun(i)) # pass in tid
            t.sticky = static
            if static
                ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid_offset + i-1)
            else
                # TODO: this should be the current pool (except interactive) if there
                # are ever more than two pools.
                _result = ccall(:jl_set_task_threadpoolid, Cint, (Any, Int8), t, _sym_to_tpid(:default))
                @assert _result == 1 "_result != 1"
            end
            tasks[i] = t
            schedule(t)
        end
        for i = 1:n
            Base._wait(tasks[i])
        end
    finally
        ccall(:jl_exit_threaded_region, Cvoid, ())
    end
    failed_tasks = filter!(istaskfailed, tasks)
    if !isempty(failed_tasks)
        throw(CompositeException(map(TaskFailedException, failed_tasks)))
    end
end

# Helper to generate threading run code with schedule checking
function _threading_run_expr(schedule)
    quote
        if $(schedule === :greedy || schedule === :dynamic || schedule === :default)
            threading_run(threadsfor_fun, false)
        elseif ccall(:jl_in_threaded_region, Cint, ()) != 0 # :static
            error("`@threads :static` cannot be used concurrently or nested")
        else # :static
            threading_run(threadsfor_fun, true)
        end
    end
end

function _threadsfor(iter, lbody, schedule)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    esc_range = esc(range)
    func = if schedule === :greedy
        greedy_func(esc_range, lidx, lbody)
    else
        default_func(esc_range, lidx, lbody)
    end
    quote
        local threadsfor_fun
        $func
        $(_threading_run_expr(schedule))
        nothing
    end
end

function _threadsfor_multi_iterator(body, iterators, condition, schedule, dims, result_type)
    vars = [iter.args[1] for iter in iterators]
    ranges = [iter.args[2] for iter in iterators]

    tuple_var = gensym("iter_tuple")
    assignments = [:($(vars[i]) = $(tuple_var)[$i]) for i in 1:length(vars)]
    # Use let blocks so destructured variables are local to each iteration,
    # avoiding data races when multiple threads execute the body concurrently.
    new_body = Expr(:let, Expr(:block, assignments...), body)
    new_condition = if condition === true
        true
    else
        Expr(:let, Expr(:block, assignments...), condition)
    end

    product_expr = :(Iterators.product($(ranges...)))
    synthetic_iter = :($(tuple_var) = $(product_expr))

    return _threadsfor_single_iterator(new_body, synthetic_iter, new_condition, schedule, dims; result_type)
end

function _threadsfor_comprehension(gen::Expr, schedule, result_type=nothing)
    @assert gen.head === :generator

    body = gen.args[1]

    # Check if the second arg is a filter (handles both single and multi-loop with filters)
    iter_or_filter = gen.args[2]
    if isa(iter_or_filter, Expr) && iter_or_filter.head === :filter
        condition = iter_or_filter.args[1]
        iterators = iter_or_filter.args[2:end]

        if length(iterators) == 1
            return _threadsfor_single_iterator(body, iterators[1], condition, schedule; result_type)
        else
            return _threadsfor_multi_iterator(body, iterators, condition, schedule, nothing, result_type)
        end
    elseif length(gen.args) > 2
        iterators = gen.args[2:end]
        ranges = [iter.args[2] for iter in iterators]
        # Use axes to preserve offset index spaces (e.g. OffsetArrays)
        dims_expr = :(tuple($([:(axes($(esc(r)), 1)) for r in ranges]...)))
        return _threadsfor_multi_iterator(body, iterators, true, schedule, dims_expr, result_type)
    else
        return _threadsfor_single_iterator(body, iter_or_filter, true, schedule; result_type)
    end
end

function _threadsfor_single_iterator(body, iterator, condition, schedule, dims=nothing; result_type=nothing)
    lidx = iterator.args[1]
    range = iterator.args[2]
    esc_range = esc(range)
    esc_lidx = esc(lidx)
    esc_body = esc(body)
    esc_condition = condition === true ? true : esc(condition)

    # Fast path: no filter and not greedy — pre-allocate and write directly
    if condition === true && schedule !== :greedy
        return _threadsfor_comprehension_fast(esc_range, esc_lidx, esc_body, schedule, dims, result_type)
    end

    func = if schedule === :greedy
        greedy_comprehension_func(esc_range, esc_lidx, esc_body, esc_condition)
    else
        default_comprehension_func(esc_range, esc_lidx, esc_body, esc_condition)
    end

    result_expr = if schedule === :greedy
        # Greedy: collect values in arrival order (no ordering guarantee)
        if result_type !== nothing
            esc_result_type = esc(result_type)
            quote
                close(result_channel)
                vals = collect(result_channel)
                isempty(vals) ? $esc_result_type[] : $esc_result_type[v for v in vals]
            end
        else
            quote
                close(result_channel)
                collect(result_channel)
            end
        end
    else
        # Default/static/dynamic: sort by index to preserve iteration order
        if result_type !== nothing
            esc_result_type = esc(result_type)
            quote
                close(result_channel)
                pairs = collect(result_channel)
                if isempty(pairs)
                    $esc_result_type[]
                else
                    sort!(pairs, by=first)
                    $esc_result_type[p[2] for p in pairs]
                end
            end
        else
            quote
                close(result_channel)
                pairs = collect(result_channel)
                if isempty(pairs)
                    []
                else
                    sort!(pairs, by=first)
                    [p[2] for p in pairs]
                end
            end
        end
    end

    # If dims is provided, reshape the result to match original comprehension dimensions
    if dims !== nothing
        result_expr = quote
            let flat_result = $result_expr
                reshape(flat_result, $(dims))
            end
        end
    end

    quote
        local threadsfor_fun
        local result_channel = $func
        $(_threading_run_expr(schedule))
        $result_expr
    end
end

# Fast path for non-filtered, non-greedy comprehensions: pre-allocate and write directly.
# Non-AbstractArray iterators (e.g. Iterators.flatten) are collected into a Vector
# because the parallel work distribution indexes into items with r[i].
# AbstractArrays and Tuples are used directly to preserve their index space (e.g. OffsetArrays).
function _threadsfor_comprehension_fast(esc_range, esc_lidx, esc_body, schedule, dims, result_type)
    work_dist = _work_distribution_code()
    wrap_final = dims !== nothing ? (x -> :(reshape($x, $dims))) : identity

    if result_type !== nothing
        # Typed path: pre-allocate with known element type
        esc_result_type = esc(result_type)
        return quote
            let iter = $esc_range
            local items = iter isa Union{Tuple, AbstractArray} ? iter : collect(iter)
            local niter = length(items)
            local result = similar(Vector{$esc_result_type}, axes(items))
            if niter > 0
                let items = items, result = result
                local threadsfor_fun
                function threadsfor_fun(tid = 1; onethread = false)
                    # Reads: items, tid, onethread. Defines: r, loop_first, loop_last.
                    $work_dist
                    for i = loop_first:loop_last
                        local $esc_lidx = @inbounds r[i]
                        @inbounds result[i] = $esc_body
                    end
                end
                $(_threading_run_expr(schedule))
                end
            end
            $(wrap_final(:(result)))
            end
        end
    else
        # Untyped path: evaluate first element to determine result type,
        # then fill in parallel with the body expression inlined directly
        # in the closure. This avoids boxing that occurs when calling a
        # lambda whose return type is a Union across closure boundaries.
        return quote
            let iter = $esc_range
            local items = iter isa Union{Tuple, AbstractArray} ? iter : collect(iter)
            local niter = length(items)
            if niter == 0
                $(wrap_final(:(similar(Vector{Any}, axes(items)))))
            else
                local _skip = firstindex(items)
                local $esc_lidx = @inbounds items[_skip]
                local _probe_val = $esc_body
                local result = similar(Vector{typeof(_probe_val)}, axes(items))
                @inbounds result[_skip] = _probe_val
                if niter > 1
                    local _npool = threadpoolsize()
                    local _widen_buffers = [Pair{Int,Any}[] for _ in 1:_npool]
                    let items = items, result = result, _widen_buffers = _widen_buffers,
                        _skip = _skip
                    local threadsfor_fun
                    function threadsfor_fun(tid = 1; onethread = false)
                        # Reads: items, tid, onethread. Defines: r, loop_first, loop_last.
                        $work_dist
                        local _T = eltype(result)
                        local _my_widen = _widen_buffers[tid]
                        for i = loop_first:loop_last
                            i == _skip && continue
                            local $esc_lidx = @inbounds r[i]
                            local _val = $esc_body
                            if _val isa _T
                                @inbounds result[i] = _val
                            else
                                push!(_my_widen, i => _val)
                            end
                        end
                    end
                    $(_threading_run_expr(schedule))
                    end
                    result = Base.setindices_widen_up_to(result, _widen_buffers)
                end
                $(wrap_final(:(result)))
            end
            end
        end
    end
end


function greedy_func(itr, lidx, lbody)
    quote
        let c = Channel{eltype($itr)}(threadpoolsize(), spawn=true) do ch
            for item in $itr
                put!(ch, item)
            end
        end
        function threadsfor_fun(tid)
            for item in c
                local $(esc(lidx)) = item
                $(esc(lbody))
            end
        end
        end
    end
end

function greedy_comprehension_func(itr, esc_lidx, esc_body, esc_condition)
    quote
        let c = Channel{eltype($itr)}(threadpoolsize(), spawn=true) do ch
            for item in $itr
                put!(ch, item)
            end
        end
        result_channel = Channel{Any}(Inf)

        function threadsfor_fun(tid)
            for item in c
                local $esc_lidx = item
                if $esc_condition
                    put!(result_channel, $esc_body)
                end
            end
        end
        result_channel
        end
    end
end

# Helper function to generate work distribution code
function _work_distribution_code()
    quote
        r = items # Load into local variable
        lenr = length(r)
        # divide loop iterations among threads
        if onethread
            tid = 1
            len, rem = lenr, 0
        else
            len, rem = divrem(lenr, threadpoolsize())
        end
        # not enough iterations for all the threads?
        if len == 0
            if tid > rem
                return
            end
            len, rem = 1, 0
        end
        # compute this thread's iterations
        loop_first = firstindex(r) + ((tid-1) * len)
        loop_last = loop_first + len - 1
        # distribute remaining iterations evenly
        if rem > 0
            if tid <= rem
                loop_first = loop_first + (tid-1)
                loop_last = loop_last + tid
            else
                loop_first = loop_first + rem
                loop_last = loop_last + rem
            end
        end
    end
end

function default_func(itr, lidx, lbody)
    work_dist = _work_distribution_code()
    quote
        let items = $itr
        function threadsfor_fun(tid = 1; onethread = false)
            # Reads: items, tid, onethread. Defines: r, loop_first, loop_last.
            $work_dist
            for i = loop_first:loop_last
                local $(esc(lidx)) = @inbounds r[i]
                $(esc(lbody))
            end
        end
        end
    end
end

function default_comprehension_func(itr, esc_lidx, esc_body, esc_condition)
    work_dist = _work_distribution_code()
    quote
        let iter = $itr
        # Collect non-indexable iterators for random access (r[i]) in work distribution
        items = iter isa AbstractArray ? iter : collect(iter)
        # Channel uses Tuple{Int, Any} because the output type of the body expression
        # is not known at macro expansion time.
        result_channel = Channel{Tuple{Int, Any}}(Inf)

        function threadsfor_fun(tid = 1; onethread = false)
            # Reads: items, tid, onethread. Defines: r, loop_first, loop_last.
            $work_dist
            for i = loop_first:loop_last
                local $esc_lidx = @inbounds r[i]
                if $esc_condition
                    put!(result_channel, (i, $esc_body))
                end
            end
        end
        result_channel  # Return channel so results can be collected after threading_run
        end
    end
end

"""
    Threads.@threads [schedule] for ... end
    Threads.@threads [schedule] [expr for ... end]
    Threads.@threads [schedule] T[expr for ... end]

A macro to execute a `for` loop or array comprehension in parallel. The iteration space is distributed to
coarse-grained tasks. This policy can be specified by the `schedule` argument. The
execution of the loop waits for the evaluation of all iterations.

For `for` loops, the macro executes the loop body in parallel but does not return a value.
For array comprehensions, the macro executes the comprehension in parallel and returns
the collected results as an array.

Tasks spawned by `@threads` are scheduled on the `:default` threadpool. This means that
`@threads` will not use threads from the `:interactive` threadpool, even if called from
the main thread or from a task in the interactive pool. The `:default` threadpool is
intended for compute-intensive parallel workloads.

See also: [`@spawn`](@ref Threads.@spawn) and
`pmap` in [`Distributed`](@ref man-distributed).
For more information on threadpools, see the chapter on [threadpools](@ref man-threadpools).

# Extended help

## Semantics

Unless stronger guarantees are specified by the scheduling option, the loop executed by
`@threads` macro have the following semantics.

The `@threads` macro executes the loop body in an unspecified order and potentially
concurrently. It does not specify the exact assignments of the tasks and the worker threads.
The assignments can be different for each execution. The loop body code (including any code
transitively called from it) must not make any assumptions about the distribution of
iterations to tasks or the worker thread in which they are executed. The loop body for each
iteration must be able to make forward progress independent of other iterations and be free
from data races. As such, invalid synchronizations across iterations may deadlock while
unsynchronized memory accesses may result in undefined behavior.

For example, the above conditions imply that:

- A lock taken in an iteration *must* be released within the same iteration.
- Communicating between iterations using blocking primitives like `Channel`s is incorrect.
- Write only to locations not shared across iterations (unless a lock or atomic operation is
  used).
- Unless the `:static` schedule is used, the value of [`threadid()`](@ref Threads.threadid)
  may change even within a single iteration. See [`Task Migration`](@ref man-task-migration).

## Schedulers

Without the scheduler argument, the exact scheduling is unspecified and varies across Julia
releases. Currently, `:dynamic` is used when the scheduler is not specified.

!!! compat "Julia 1.5"
    The `schedule` argument is available as of Julia 1.5.

### `:dynamic` (default)

`:dynamic` scheduler executes iterations dynamically to available worker threads. Current
implementation assumes that the workload for each iteration is uniform. However, this
assumption may be removed in the future.

This scheduling option is merely a hint to the underlying execution mechanism. However, a
few properties can be expected. The number of `Task`s used by `:dynamic` scheduler is
bounded by a small constant multiple of the number of available worker threads
([`Threads.threadpoolsize()`](@ref)). Each task processes contiguous regions of the
iteration space. Thus, `@threads :dynamic for x in xs; f(x); end` is typically more
efficient than `@sync for x in xs; @spawn f(x); end` if `length(xs)` is significantly
larger than the number of the worker threads and the run-time of `f(x)` is relatively
smaller than the cost of spawning and synchronizing a task (typically less than 10
microseconds).

!!! compat "Julia 1.8"
    The `:dynamic` option for the `schedule` argument is available and the default as of Julia 1.8.

### `:greedy`

`:greedy` scheduler spawns up to [`Threads.threadpoolsize()`](@ref) tasks, each greedily working on
the given iterated values as they are produced. As soon as one task finishes its work, it takes
the next value from the iterator. Work done by any individual task is not necessarily on
contiguous values from the iterator. The given iterator may produce values forever, only the
iterator interface is required (no indexing).

This scheduling option is generally a good choice if the workload of individual iterations
is not uniform/has a large spread.

!!! compat "Julia 1.11"
    The `:greedy` option for the `schedule` argument is available as of Julia 1.11.

### `:static`

`:static` scheduler creates one task per thread and divides the iterations equally among
them, assigning each task specifically to each thread. In particular, the value of
[`threadid()`](@ref Threads.threadid) is guaranteed to be constant within one iteration.
Specifying `:static` is an error if used from inside another `@threads` loop or from a
thread other than 1.

!!! note
    `:static` scheduling exists for supporting transition of code written before Julia 1.3.
    In newly written library functions, `:static` scheduling is discouraged because the
    functions using this option cannot be called from arbitrary worker threads.

## Examples

### For loops

To illustrate of the different scheduling strategies, consider the following function
`busywait` containing a non-yielding timed loop that runs for a given number of seconds.

```julia-repl
julia> function busywait(seconds)
            tstart = time_ns()
            while (time_ns() - tstart) / 1e9 < seconds
            end
        end

julia> @time begin
            Threads.@spawn busywait(5)
            Threads.@threads :static for i in 1:Threads.threadpoolsize()
                busywait(1)
            end
        end
6.003001 seconds (16.33 k allocations: 899.255 KiB, 0.25% compilation time)

julia> @time begin
            Threads.@spawn busywait(5)
            Threads.@threads :dynamic for i in 1:Threads.threadpoolsize()
                busywait(1)
            end
        end
2.012056 seconds (16.05 k allocations: 883.919 KiB, 0.66% compilation time)
```

The `:dynamic` example takes 2 seconds since one of the non-occupied threads is able
to run two of the 1-second iterations to complete the for loop.

### Array comprehensions

The `@threads` macro also supports array comprehensions, which return the collected results.
Array comprehensions preserve element order for `:static` and `:dynamic` (default) scheduling.
The `:greedy` scheduler does not guarantee element order, since tasks consume work items as
they become available. Multi-dimensional comprehensions preserve the dimensions of the
original comprehension (e.g., `[f(i,j) for i in 1:n, j in 1:m]` returns an `n×m` matrix).
Typed comprehensions (`T[expr for ...]`) are also supported and return an array with the
specified element type.

For non-filtered comprehensions with non-`:greedy` scheduling, a fast path is used that
pre-allocates the result array and writes directly by index, avoiding Channel overhead.

!!! tip "Performance tip"
    For best performance, use typed comprehensions (`T[expr for ...]`) when the element type
    is known. Untyped comprehensions infer the type from the first result; if later results
    have incompatible types, a type-widening path is used which may be slower.

!!! warning
    The body expression of a threaded comprehension may execute on any thread and may
    migrate between threads. Do not rely on [`threadid()`](@ref Threads.threadid) or
    [`task_local_storage()`](@ref) returning consistent values within the body expression.

```julia-repl
julia> Threads.@threads [i^2 for i in 1:5] # Simple comprehension
5-element Vector{Int64}:
   1
   4
   9
  16
  25

julia> Threads.@threads [i^2 for i in 1:5 if iseven(i)] # Filtered comprehension
2-element Vector{Int64}:
   4
  16

julia> Threads.@threads [i + j for i in 1:3, j in 1:3] # Multiple loops
3×3 Matrix{Int64}:
 2  3  4
 3  4  5
 4  5  6

julia> Threads.@threads Float64[i^2 for i in 1:5] # Typed comprehension
5-element Vector{Float64}:
  1.0
  4.0
  9.0
 16.0
 25.0
```

When the iterator doesn't have a known length, such as a channel, the `:greedy` scheduling
option can be used.
```julia-repl
julia> c = Channel(5, spawn=true) do ch
           foreach(i -> put!(ch, i), 1:5)
       end;

julia> Threads.@threads :greedy [i^2 for i in c if iseven(i)]
2-element Vector{Int64}:
  4
 16

julia> # Non-indexable iterators are also supported
       Threads.@threads [i for i in Iterators.flatten([1:3, 4:6])]
6-element Vector{Int64}:
 1
 2
 3
 4
 5
 6
```
"""
macro threads(args...)
    na = length(args)
    if na == 2
        sched, ex = args
        if sched isa QuoteNode
            sched = sched.value
        elseif sched isa Symbol
            # for now only allow quoted symbols
            sched = nothing
        end
        if sched !== :static && sched !== :dynamic && sched !== :greedy
            throw(ArgumentError("unsupported schedule argument in @threads"))
        end
    elseif na == 1
        sched = :default
        ex = args[1]
    else
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    if isa(ex, Expr) && (ex.head === :comprehension || ex.head === :typed_comprehension)
        # Handle array comprehensions (typed and untyped)
        if ex.head === :typed_comprehension
            return _threadsfor_comprehension(ex.args[2], sched, ex.args[1])
        else
            return _threadsfor_comprehension(ex.args[1], sched)
        end
    elseif isa(ex, Expr) && ex.head === :for
        # Handle for loops
        if !(ex.args[1] isa Expr && ex.args[1].head === :(=))
            throw(ArgumentError("nested outer loops are not currently supported by @threads"))
        end
        return _threadsfor(ex.args[1], ex.args[2], sched)
    else
        throw(ArgumentError("@threads requires a `for` loop or comprehension expression"))
    end
end

function _spawn_set_thrpool(t::Task, tp::Symbol)
    tpid = _sym_to_tpid(tp)
    if tpid == -1 || _nthreads_in_pool(tpid) == 0
        tpid = _sym_to_tpid(:default)
    end
    _result = ccall(:jl_set_task_threadpoolid, Cint, (Any, Int8), t, tpid)
    @assert _result == 1 "_result != 1"
    nothing
end

"""
    Threads.@spawn [:default|:interactive|:samepool] expr

Create a [`Task`](@ref) and [`schedule`](@ref) it to run on any available
thread in the specified threadpool: `:default`, `:interactive`, or `:samepool`
to use the same as the caller. `:default` is used if unspecified. The task is
allocated to a thread once one becomes available. To wait for the task to
finish, call [`wait`](@ref) on the result of this macro, or call
[`fetch`](@ref) to wait and then obtain its return value.

Values can be interpolated into `@spawn` via `\$`, which copies the value
directly into the constructed underlying closure. This allows you to insert
the _value_ of a variable, isolating the asynchronous code from changes to
the variable's value in the current task.

!!! note
    The thread that the task runs on may change if the task yields, therefore `threadid()` should not
    be treated as constant for a task. See [`Task Migration`](@ref man-task-migration), and the broader
    [multi-threading](@ref man-multithreading) manual for further important caveats.
    See also the chapter on [threadpools](@ref man-threadpools).

!!! compat "Julia 1.3"
    This macro is available as of Julia 1.3.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.

!!! compat "Julia 1.9"
    A threadpool may be specified as of Julia 1.9.

!!! compat "Julia 1.12"
    The same threadpool may be specified as of Julia 1.12.

# Examples
```julia-repl
julia> t() = println("Hello from ", Threads.threadid());

julia> tasks = fetch.([Threads.@spawn t() for i in 1:4]);
Hello from 1
Hello from 1
Hello from 3
Hello from 4
```
"""
macro spawn(args...)
    tp = QuoteNode(:default)
    na = length(args)
    if na == 2
        ttype, ex = args
        if ttype isa QuoteNode
            ttype = ttype.value
            if !in(ttype, (:interactive, :default, :samepool))
                throw(ArgumentError(LazyString("unsupported threadpool in @spawn: ", ttype)))
            end
            tp = QuoteNode(ttype)
        else
            tp = ttype
        end
    elseif na == 1
        ex = args[1]
    else
        throw(ArgumentError("wrong number of arguments in @spawn"))
    end

    letargs = Base._lift_one_interp!(ex)

    thunk = Base.replace_linenums!(:(()->($(esc(ex)))), __source__)
    var = esc(Base.sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            task.sticky = false
            local tp = $(esc(tp))
            if tp == :samepool
                tp = Threads.threadpool()
            end
            _spawn_set_thrpool(task, tp)
            if $(Expr(:islocal, var))
                put!($var, task)
            end
            schedule(task)
            task
        end
    end
end

# This is a stub that can be overloaded for downstream structures like `Channel`
function foreach end

# Scheduling traits that can be employed for downstream overloads
abstract type AbstractSchedule end
struct StaticSchedule <: AbstractSchedule end
struct FairSchedule <: AbstractSchedule end

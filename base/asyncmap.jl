# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Iterators: Enumerate

"""
    asyncmap(f, c...; ntasks=0, batch_size=nothing)

Uses multiple concurrent tasks to map `f` over a collection (or multiple
equal length collections). For multiple collection arguments, `f` is
applied elementwise.

The output is guaranteed to be the same order as the elements of the collection(s) `c`.

`ntasks` specifies the number of tasks to run concurrently.
Depending on the length of the collections, if `ntasks` is unspecified,
up to 100 tasks will be used for concurrent mapping.

`ntasks` can also be specified as a zero-arg function. In this case, the
number of tasks to run in parallel is checked before processing every element and a new
task started if the value of `ntasks_func` is greater than the current number
of tasks.

If `batch_size` is specified, the collection is processed in batch mode. `f` must
then be a function that must accept a `Vector` of argument tuples and must
return a vector of results. The input vector will have a length of `batch_size` or less.

The following examples highlight execution in different tasks by returning
the `objectid` of the tasks in which the mapping function is executed.

First, with `ntasks` undefined, each element is processed in a different task.
```julia-repl
julia> tskoid() = objectid(current_task());

julia> asyncmap(x->tskoid(), 1:5)
5-element Vector{UInt64}:
 0x6e15e66c75c75853
 0x440f8819a1baa682
 0x9fb3eeadd0c83985
 0xebd3e35fe90d4050
 0x29efc93edce2b961

julia> length(unique(asyncmap(x->tskoid(), 1:5)))
5
```

With `ntasks=2` all elements are processed in 2 tasks.
```julia-repl
julia> asyncmap(x->tskoid(), 1:5; ntasks=2)
5-element Vector{UInt64}:
 0x027ab1680df7ae94
 0xa23d2f80cd7cf157
 0x027ab1680df7ae94
 0xa23d2f80cd7cf157
 0x027ab1680df7ae94

julia> length(unique(asyncmap(x->tskoid(), 1:5; ntasks=2)))
2
```

With `batch_size` defined, the mapping function needs to be changed to accept an array
of argument tuples and return an array of results. `map` is used in the modified mapping
function to achieve this.
```julia-repl
julia> batch_func(input) = map(x->string("args_tuple: ", x, ", element_val: ", x[1], ", task: ", tskoid()), input)
batch_func (generic function with 1 method)

julia> asyncmap(batch_func, 1:5; ntasks=2, batch_size=2)
5-element Vector{String}:
 "args_tuple: (1,), element_val: 1, task: 9118321258196414413"
 "args_tuple: (2,), element_val: 2, task: 4904288162898683522"
 "args_tuple: (3,), element_val: 3, task: 9118321258196414413"
 "args_tuple: (4,), element_val: 4, task: 4904288162898683522"
 "args_tuple: (5,), element_val: 5, task: 9118321258196414413"
```
"""
function asyncmap(f, c...; ntasks=0, batch_size=nothing)
    src = isone(length(c)) ? c[1] : zip(c...)
    if haslength(src)
        n = length(src)
        results = Vector{Any}(undef, n)
    else
        results = Dict{Int,Any}()
    end

    foreach(identity, AsyncCollector(f, results, c...; ntasks=ntasks, batch_size=batch_size))

    if results isa Dict
        v = Any[results[i] for i in 1:length(results)]
    else
        v = results
    end
    return _asyncmap_result(v, src)
end

# Narrow element type and reshape to try to match what `map` would return.
_asyncmap_result(v, c1::Tuple) = Tuple(map(identity, v))
_asyncmap_result(v, c1::AbstractArray) = collect_similar(c1, Generator(identity, reshape(v, axes(c1))))
_asyncmap_result(v, c1) = _asyncmap_result(v, c1, IteratorSize(c1))
_asyncmap_result(v, c1, ::HasShape) = collect(Generator(identity, reshape(v, axes(c1))))
_asyncmap_result(v, _, _) = map(identity, v)

batch_size_err_str(batch_size) = string("batch_size must be specified as a positive integer. batch_size=", batch_size)
function verify_batch_size(batch_size)
    if batch_size === nothing
        return batch_size
    elseif isa(batch_size, Number)
        batch_size = Int(batch_size)
        batch_size < 1 && throw(ArgumentError(batch_size_err_str(batch_size)))
        return batch_size
    else
        throw(ArgumentError(batch_size_err_str(batch_size)))
    end
end


function verify_ntasks(iterable, ntasks)
    if !((isa(ntasks, Number) && (ntasks >= 0)) || isa(ntasks, Function))
        err = string("ntasks must be specified as a positive integer or a 0-arg function. ntasks=", ntasks)
        throw(ArgumentError(err))
    end

    if ntasks == 0
        if haslength(iterable)
            ntasks = max(1,min(100, length(iterable)))
        else
            ntasks = 100
        end
    end
    return ntasks
end


function setup_chnl_and_tasks(exec_func, ntasks, batch_size=nothing)
    if isa(ntasks, Function)
        nt = ntasks()::Int
        # start at least one worker task.
        if nt == 0
            nt = 1
        end
    else
        nt = ntasks::Int
    end

    # Use an unbuffered channel for communicating with the worker tasks. In the event
    # of an error in any of the worker tasks, the channel is closed. This
    # results in the `put!` in the driver task failing immediately.
    chnl = Channel(0)
    worker_tasks = Task[]
    foreach(_ -> start_worker_task!(worker_tasks, exec_func, chnl, batch_size), 1:nt)
    yield()
    return (chnl, worker_tasks)
end

function start_worker_task!(worker_tasks, exec_func, chnl, batch_size=nothing)
    t = @async begin
        retval = nothing

        try
            if isa(batch_size, Number)
                while isopen(chnl)
                    # The mapping function expects an array of input args, as it processes
                    # elements in a batch.
                    batch_collection=Any[]
                    n = 0
                    for exec_data in chnl
                        push!(batch_collection, exec_data)
                        n += 1
                        (n == batch_size) && break
                    end
                    if n > 0
                        exec_func(batch_collection)
                    end
                end
            else
                for exec_data in chnl
                    exec_func(exec_data...)
                end
            end
        catch e
            close(chnl)
            retval = capture_exception(e, catch_backtrace())
        end
        retval
    end
    push!(worker_tasks, t)
end

# Special handling for some types.
function asyncmap(f, s::AbstractString; kwargs...)
    s2 = Vector{Char}(undef, length(s))
    asyncmap!(f, s2, s; kwargs...)
    return String(s2)
end

mutable struct AsyncCollector
    f
    results
    enumerator::Enumerate
    ntasks
    batch_size
    nt_check::Bool     # check number of tasks on every iteration

    AsyncCollector(f, r, en::Enumerate, ntasks, batch_size) = new(f, r, en, ntasks, batch_size, isa(ntasks, Function))
end

"""
    AsyncCollector(f, results, c...; ntasks=0, batch_size=nothing) -> iterator

Return an iterator which applies `f` to each element of `c` asynchronously
and collects output into `results`.

Keyword args `ntasks` and `batch_size` have the same behavior as in
[`asyncmap`](@ref). If `batch_size` is specified, `f` must
be a function which operates on an array of argument tuples.

!!! note
    `iterate(::AsyncCollector, state) -> (nothing, state)`. A successful return
    from `iterate` indicates that the next element from the input collection is
    being processed asynchronously. It blocks until a free worker task becomes
    available.

!!! note
    `for _ in AsyncCollector(f, results, c...; ntasks=1) end` is equivalent to
    `map!(f, results, c...)`.
"""
function AsyncCollector(f, results, c...; ntasks=0, batch_size=nothing)
    AsyncCollector(f, results, enumerate(zip(c...)), ntasks, batch_size)
end

mutable struct AsyncCollectorState
    chnl::Channel
    worker_tasks::Vector{Task}
    exec_func
    batch_size
    enum_state      # enumerator state
end

function iterate(itr::AsyncCollector)
    itr.ntasks = verify_ntasks(itr.enumerator, itr.ntasks)
    itr.batch_size = verify_batch_size(itr.batch_size)

    if itr.batch_size !== nothing
        exec_func = batch -> begin
            indices = map(x -> x[1], batch)
            batched_args = map(x -> x[2], batch)
            res = itr.f(batched_args)
            foreach(((j, v),) -> (itr.results[indices[j]] = v), enumerate(res))
        end
    else
        exec_func = (i, args) -> (itr.results[i] = itr.f(args...))
    end

    chnl, worker_tasks = setup_chnl_and_tasks(exec_func, itr.ntasks, itr.batch_size)
    return iterate(itr, AsyncCollectorState(chnl, worker_tasks, exec_func, itr.batch_size, nothing))
end

function wait_done(itr::AsyncCollector, state::AsyncCollectorState)
    close(state.chnl)

    # wait for all tasks to finish
    foreach(x->(v=fetch(x); isa(v, Exception) && throw(v)), state.worker_tasks)
    empty!(state.worker_tasks)
end

function iterate(itr::AsyncCollector, state::AsyncCollectorState)
    if itr.nt_check && (length(state.worker_tasks) < itr.ntasks())
        start_worker_task!(state.worker_tasks, state.exec_func, state.chnl, state.batch_size)
    end

    # Get index and mapped function arguments from enumeration iterator.
    y = !isnothing(state.enum_state) ?
        iterate(itr.enumerator, state.enum_state) :
        iterate(itr.enumerator)
    if isnothing(y)
        wait_done(itr, state)
        return nothing
    end
    (i, args), state.enum_state = y
    try
        put!(state.chnl, (i, args))
    catch
        # Prefer throwing a worker exception over the put! failure.
        for t in state.worker_tasks
            v = fetch(t)
            isa(v, Exception) && throw(v)
        end
        rethrow()
    end

    return (nothing, state)
end

"""
    AsyncGenerator(f, c...; ntasks=0, batch_size=nothing) -> iterator

Apply `f` to each element of `c` using at most `ntasks` asynchronous tasks.

Keyword args `ntasks` and `batch_size` have the same behavior as in
[`asyncmap`](@ref). If `batch_size` is specified, `f` must
be a function which operates on an array of argument tuples.

!!! note
    `collect(AsyncGenerator(f, c...; ntasks=1))` is equivalent to
    `map(f, c...)`.
"""
mutable struct AsyncGenerator
    collector::AsyncCollector
end

function AsyncGenerator(f, c...; ntasks=0)
    AsyncGenerator(AsyncCollector(f, Dict{Int,Any}(), c...; ntasks=ntasks))
end

mutable struct AsyncGeneratorState
    i::Int
    collector_done::Bool
    collector_state::Union{AsyncCollectorState, Nothing}
end

function iterate(itr::AsyncGenerator, state::AsyncGeneratorState=AsyncGeneratorState(0, false, nothing))
    state.i += 1

    results_dict = itr.collector.results
    while !state.collector_done && !haskey(results_dict, state.i)
        y = !isnothing(state.collector_state) ?
            iterate(itr.collector, state.collector_state) :
            iterate(itr.collector)
        if isnothing(y)
            # All input consumed; async tasks may still be in flight.
            state.collector_done = true
            break
        end
        _, state.collector_state = y
    end
    state.collector_done && isempty(results_dict) && return nothing
    r = results_dict[state.i]
    delete!(results_dict, state.i)

    return (r, state)
end

IteratorSize(::Type{AsyncGenerator}) = SizeUnknown()
IteratorEltype(::Type{AsyncGenerator}) = EltypeUnknown()

"""
    asyncmap!(f, results, c...; ntasks=0, batch_size=nothing)

Like [`asyncmap`](@ref), but stores output in `results` rather than
returning a collection.

$(_DOCS_ALIASING_WARNING)
"""
function asyncmap!(f, r, c1, c...; ntasks=0, batch_size=nothing)
    foreach(identity, AsyncCollector(f, r, c1, c...; ntasks=ntasks, batch_size=batch_size))
    r
end

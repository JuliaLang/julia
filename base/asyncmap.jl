# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Iterators: Enumerate

"""
    asyncmap(f, c...; ntasks=0, batch_size=nothing)

Uses multiple concurrent tasks to map `f` over a collection (or multiple
equal length collections). For multiple collection arguments, `f` is
applied elementwise.

`ntasks` specifies the number of tasks to run concurrently.
Depending on the length of the collections, if `ntasks` is unspecified,
up to 100 tasks will be used for concurrent mapping.

`ntasks` can also be specified as a zero-arg function. In this case, the
number of tasks to run in parallel is checked before processing every element and a new
task started if the value of `ntasks_func` is less than the current number
of tasks.

If `batch_size` is specified, the collection is processed in batch mode. `f` must
then be a function that must accept a `Vector` of argument tuples and must
return a vector of results. The input vector will have a length of `batch_size` or less.

# Exception handling

Individual exceptions thrown by `f` will be wrapped in a `TaskFailedException`.
As multiple tasks are used, more than one exception may be thrown. Exceptions
are combined into a `CompositeException`. Even if only a single exception is
thrown, it is still wrapped in a `CompositeException`.

However, when an exception is thrown `asyncmap` will fail-fast. Any remaining
iterations, which are not already in progress, will be cancelled. If you need
`asyncmap` to be error resistant then wrap the body of 'f' in a `try... catch`
statement. Below is one possible approach to error handling:

```
julia> result = asyncmap(1:2) do x
           try
               iseven(x) ? x : error("foo")
           catch e
               CapturedException(e, catch_backtrace())
           end
       end
2-element Array{Any,1}:
  CapturedException(ErrorException("foo"), Any[(error(::String) at error.jl:33, 1), ...])
 2
```

# Examples

The following examples highlight execution in different tasks by returning
the `objectid` of the tasks in which the mapping function is executed.

First, with `ntasks` undefined, each element is processed in a different task.
```
julia> tskoid() = objectid(current_task());

julia> asyncmap(x->tskoid(), 1:5)
5-element Array{UInt64,1}:
 0x6e15e66c75c75853
 0x440f8819a1baa682
 0x9fb3eeadd0c83985
 0xebd3e35fe90d4050
 0x29efc93edce2b961

julia> length(unique(asyncmap(x->tskoid(), 1:5)))
5
```

With `ntasks=2` all elements are processed in 2 tasks.
```
julia> asyncmap(x->tskoid(), 1:5; ntasks=2)
5-element Array{UInt64,1}:
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
```
julia> batch_func(input) = map(x->string("args_tuple: ", x, ", element_val: ", x[1], ", task: ", tskoid()), input)
batch_func (generic function with 1 method)

julia> asyncmap(batch_func, 1:5; ntasks=2, batch_size=2)
5-element Array{String,1}:
 "args_tuple: (1,), element_val: 1, task: 9118321258196414413"
 "args_tuple: (2,), element_val: 2, task: 4904288162898683522"
 "args_tuple: (3,), element_val: 3, task: 9118321258196414413"
 "args_tuple: (4,), element_val: 4, task: 4904288162898683522"
 "args_tuple: (5,), element_val: 5, task: 9118321258196414413"
```

!!! note
    The tasks created by `asyncmap` are executed in a single OS thread co-operatively. Consequently,
    `asyncmap` is beneficial only when the mapping function involves any I/O - disk, network, remote
    worker invocation, etc. See [`Threads`](@ref) and `Distributed` for alternatives.

"""
function asyncmap(f, c...; ntasks=0, batch_size=nothing)
    return async_usemap(f, c...; ntasks=ntasks, batch_size=batch_size)
end

function async_usemap(f, c...; ntasks=0, batch_size=nothing)
    ntasks = verify_ntasks(c[1], ntasks)
    batch_size = verify_batch_size(batch_size)

    if batch_size !== nothing
        exec_func = batch -> begin
            # extract the Refs from the input tuple
            batch_refs = map(x->x[1], batch)

            # and the args tuple....
            batched_args = map(x->x[2], batch)

            results = f(batched_args)
            foreach(x -> (batch_refs[x[1]].x = x[2]), enumerate(results))
        end
    else
        exec_func = (r,args) -> (r.x = f(args...))
    end
    chnl, err_chnl, worker_tasks = setup_chnl_and_tasks(exec_func, ntasks, batch_size)
    return wrap_n_exec_twice(chnl, err_chnl, worker_tasks, ntasks, exec_func, c...)
end

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
        chklen = IteratorSize(iterable)
        if (chklen isa HasLength) || (chklen isa HasShape)
            ntasks = max(1,min(100, length(iterable)))
        else
            ntasks = 100
        end
    end
    return ntasks
end

function wrap_n_exec_twice(chnl, err_chnl, worker_tasks, ntasks, exec_func, c...)
    # The driver task, creates a Ref object and writes it and the args tuple to
    # the communication channel for processing by a free worker task.
    push_arg_to_channel = (x...) -> (r=Ref{Any}(nothing); put!(chnl,(r,x));r)

    if isa(ntasks, Function)
        map_f = (x...) -> begin
            # check number of tasks every time, and start one if required.
            # number_tasks > optimal_number is fine, the other way around is inefficient.
            if length(worker_tasks) < ntasks()
                start_worker_task!(worker_tasks, exec_func, chnl, err_chnl)
            end
            push_arg_to_channel(x...)
        end
    else
        map_f = push_arg_to_channel
    end
    maptwice(map_f, chnl, err_chnl, worker_tasks, c...)
end

function maptwice(wrapped_f, chnl, err_chnl, worker_tasks, c...)
    # first run, returns a collection of Refs
    asyncrun = try
        map(wrapped_f, c...)
    catch ex
        # the work channel was closed early and we have the error
        ex isa InvalidStateException && isready(err_chnl) || rethrow()
    finally
        close(chnl)
    end

    # wait for all worker tasks to finish. We try not to throw
    # task errors here because they would be in task creation
    # order.
    try
        @sync foreach(t -> @sync_add(t), worker_tasks)
    catch
        isready(err_chnl) || rethrow()
    finally
        close(err_chnl)
    end

    # throw task errors in chronological order
    if isready(err_chnl)
        exs = [TaskFailedException(task) for task in err_chnl]
        throw(CompositeException(exs))
    end

    if isa(asyncrun, Ref)
        # scalar case
        return asyncrun.x
    else
        # second run, extract values from the Refs and return
        return map(ref->ref.x, asyncrun)
    end
end

function setup_chnl_and_tasks(exec_func, ntasks, batch_size=nothing)
    if isa(ntasks, Function)
        nt = ntasks()
        # start at least one worker task.
        if nt == 0
            nt = 1
        end
    else
        nt = ntasks
    end

    # Use an unbuffered channel for communicating with the worker tasks. In the event
    # of an error in any of the worker tasks, the channel is closed. This
    # results in the `put!` in the driver task failing immediately.
    chnl = Channel(0)
    err_chnl = Channel(nt)
    worker_tasks = []
    foreach(_ -> start_worker_task!(worker_tasks, exec_func, chnl, err_chnl, batch_size), 1:nt)
    yield()
    return (chnl, err_chnl, worker_tasks)
end

start_worker_task(exec_func, chnl) = foreach(exec_data -> exec_func(exec_data...), chnl)
start_worker_task(exec_func, chnl, ::Nothing) = start_worker_task(exec_func, chnl)
start_worker_task(exec_func, chnl, batch_size) = while isopen(chnl)
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

function start_worker_task!(worker_tasks, exec_func, chnl, err_chnl, batch_size=nothing)
    t = @async begin
        try
            start_worker_task(exec_func, chnl, batch_size)
        catch
            put!(err_chnl, current_task())
            close(chnl)
            rethrow()
        end
    end
    push!(worker_tasks, t)
end

# Special handling for some types.
function asyncmap(f, s::AbstractString; kwargs...)
    s2 = Vector{Char}(undef, length(s))
    asyncmap!(f, s2, s; kwargs...)
    return String(s2)
end

# map on a single BitArray returns a BitArray if the mapping function is boolean.
function asyncmap(f, b::BitArray; kwargs...)
    b2 = async_usemap(f, b; kwargs...)
    if eltype(b2) == Bool
        return BitArray(b2)
    end
    return b2
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
    err_chnl::Channel
    worker_tasks::Array{Task,1}
    enum_state      # enumerator state
    AsyncCollectorState(chnl::Channel, err_chnl::Channel, worker_tasks::Vector) =
        new(chnl, err_chnl, convert(Vector{Task}, worker_tasks))
end

function iterate(itr::AsyncCollector)
    itr.ntasks = verify_ntasks(itr.enumerator, itr.ntasks)
    itr.batch_size = verify_batch_size(itr.batch_size)
    if itr.batch_size !== nothing
        exec_func = batch -> begin
            # extract indices from the input tuple
            batch_idxs = map(x->x[1], batch)

            # and the args tuple....
            batched_args = map(x->x[2], batch)

            results = f(batched_args)
            foreach(x -> (itr.results[batch_idxs[x[1]]] = x[2]), enumerate(results))
        end
    else
        exec_func = (i,args) -> (itr.results[i]=itr.f(args...))
    end

    chnl, err_chnl, worker_tasks = setup_chnl_and_tasks(itr.ntasks, itr.batch_size) do i, args
        itr.results[i]=itr.f(args...)
    end

    return iterate(itr, AsyncCollectorState(chnl, err_chnl, worker_tasks))
end

function wait_done(itr::AsyncCollector, state::AsyncCollectorState)
    close(state.chnl)

    # wait for all tasks to finish
    @sync foreach(t -> @sync_add(t), state.worker_tasks)
    empty!(state.worker_tasks)

    close(state.err_chnl)
    isready(state.err_chnl) && throw(CompositeException(collect(state.err_chnl)))
end

function iterate(itr::AsyncCollector, state::AsyncCollectorState)
    if itr.nt_check && (length(state.worker_tasks) < itr.ntasks())
        start_worker_task!(state.worker_tasks, itr.f, state.chnl)
    end

    # Get index and mapped function arguments from enumeration iterator.
    y = isdefined(state, :enum_state) ?
        iterate(itr.enumerator, state.enum_state) :
        iterate(itr.enumerator)
    if y === nothing
        wait_done(itr, state)
        return nothing
    end
    (i, args), state.enum_state = y

    try
        put!(state.chnl, (i, args))
    catch ex
        put!(state.err_chnl, ex)
        wait_done(itr, state)
        rethrow() # Should never reach here
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
    collector_state::AsyncCollectorState
    AsyncGeneratorState(i::Int) = new(i, false)
end

function iterate(itr::AsyncGenerator, state::AsyncGeneratorState=AsyncGeneratorState(0))
    state.i += 1

    results_dict = itr.collector.results
    while !state.collector_done && !haskey(results_dict, state.i)
        y = isdefined(state, :collector_state) ?
            iterate(itr.collector, state.collector_state) :
            iterate(itr.collector)
        if y === nothing
            # `check_done` waits for async tasks to finish. if we do not have the index
            # we are looking for, it is an error.
            state.collector_done = true
            break;
        end
        _, state.collector_state = y
    end
    state.collector_done && isempty(results_dict) && return nothing
    r = results_dict[state.i]
    delete!(results_dict, state.i)

    return (r, state)
end

# pass-through iterator traits to the iterable
# on which the mapping function is being applied
IteratorSize(::Type{AsyncGenerator}) = SizeUnknown()
IteratorEltype(::Type{AsyncGenerator}) = EltypeUnknown()
size(itr::AsyncGenerator) = size(itr.collector.enumerator)
length(itr::AsyncGenerator) = length(itr.collector.enumerator)

"""
    asyncmap!(f, results, c...; ntasks=0, batch_size=nothing)

Like [`asyncmap`](@ref), but stores output in `results` rather than
returning a collection.
"""
function asyncmap!(f, r, c1, c...; ntasks=0, batch_size=nothing)
    foreach(identity, AsyncCollector(f, r, c1, c...; ntasks=ntasks, batch_size=batch_size))
    r
end

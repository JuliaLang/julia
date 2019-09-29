# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Iterators: Enumerate

struct AbortMapException <: Exception end

"""
    asyncmap(f, c...; ntasks=0, batch_size=nothing)

Uses multiple concurrent tasks to map `f` over a collection (or multiple
equal length collections). For multiple collection arguments, `f` is
applied elementwise.

`ntasks` specifies the maximum number of tasks permitted to run
concurrently. the default is 100.

`ntasks` can also be specified as a zero-arg function. This is called before
each task is scheduled (each iteration) to be used as the new
maximum. Changing the maximum will not effect tasks that have already been
scheduled, but it may effect the scheduling of new tasks.

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

First, with `ntasks` undefined, all three iterations can be started at once.
```
julia> asyncmap(1:3) do i
         println("before sleep \$i")
         sleep(1/i)
         println("after sleep \$i")
         i
       end
before sleep 1
before sleep 2
before sleep 3
after sleep 3
after sleep 2
after sleep 1
3-element Array{Int64,1}:
 1
 2
 3
```

With `ntasks=2` the third iteration must wait until the second
completes. However it can still start and finish before the first completes.
```
julia> asyncmap(1:3; ntasks=2) do i
         println("before sleep \$i")
         sleep(1/i)
         println("after sleep \$i")
         i
       end
before sleep 1
before sleep 2
after sleep 2
before sleep 3
after sleep 3
after sleep 1
3-element Array{Int64,1}:
 1
 2
 3
```

With `batch_size` defined, the mapping function needs to be changed to accept an array
of argument tuples and return an array of results. `map` is used in the modified mapping
function to achieve this.
```
julia> asyncmap(1:5; ntasks=2, batch_size=2) do input
         map(input) do x
           string("args_tuple: ", x, ", element_val: ", x[1], ", ", current_task())
         end
       end
5-element Array{String,1}:
 "args_tuple: 1, element_val: 1, Task (runnable) @0x00007f6629271390"
 "args_tuple: 2, element_val: 2, Task (runnable) @0x00007f6629271390"
 "args_tuple: 3, element_val: 3, Task (runnable) @0x00007f6629271600"
 "args_tuple: 4, element_val: 4, Task (runnable) @0x00007f6629271600"
 "args_tuple: 5, element_val: 5, Task (runnable) @0x00007f6629271870"
```

!!! note
    The tasks created by `asyncmap` are executed in a single OS thread co-operatively. Consequently,
    `asyncmap` is beneficial only when the mapping function involves any I/O - disk, network, remote
    worker invocation, etc. See [`Threads`](@ref) and `Distributed` for alternatives.

"""
asyncmap(f, c...; kwargs...) = do_asyncmap(f, c...; kwargs...)

# This goes to great lengths to use `map` internally so that we do not have
# to reimplement all of map's special behaviours.
function do_asyncmap(f, c...; ntasks=0, batch_size=nothing)
    ntasks = verify_ntasks(c[1], ntasks)
    batch_size = verify_batch_size(batch_size)

    res = if batch_size === nothing
        do_asyncmap(f, c, ntasks)
    else
        do_asyncmap(f, c, ntasks, batch_size)
    end

    if res isa Ref
        # scalar case: map(f, x...) = f(x...)
        res.x
    else
        map(ref -> ref.x, res)
    end
end

mutable struct AsyncmapCtx
    tasks::Vector{Task}
    ntasks::Function
    concur::Int
    echnl::Channel{Task}
end

AsyncmapCtx(ntasks) =
    AsyncmapCtx([], ntasks, 0, Channel{Task}(typemax(Int)))

# Schedule a task to map f(xs). This will block if too many
# tasks are concurrently active. Callers should catch
# AbortMapException.
function do_asyncmap_task!(f::Function, s::AsyncmapCtx, xs...)
    t = @task try
        f(xs...)
    catch
        put!(s.echnl, current_task())
        rethrow()
    finally
        s.concur -= 1
    end

    while s.concur >= s.ntasks() && !isready(s.echnl)
        yield()
    end

    isready(s.echnl) && throw(AbortMapException());

    s.concur += 1
    schedule(t)
    push!(s.tasks, t)
end

function wait_done(s::AsyncmapCtx)
    foreach(_wait, s.tasks)
    close(s.echnl)

    isready(s.echnl) &&
        throw(CompositeException(TaskFailedException.(s.echnl)))
end

function do_asyncmap(f, c::Tuple, ntasks::Function)
    s = AsyncmapCtx(ntasks)

    res = try
        map(c...) do x...
            r = Ref{Any}(nothing)

            do_asyncmap_task!(s) do
                r.x = f(x...)
            end

            r
        end
    catch ex
        ex isa AbortMapException || rethrow()
    end

    wait_done(s)

    res
end

function do_asyncmap(f, c::Tuple, ntasks::Function, batch_size)
    s = AsyncmapCtx(ntasks)
    local batch_in
    local batch_out

    function new_batch()
        batch_in = sizehint!(Vector{Any}(), batch_size)
        batch_out = sizehint!(Vector{Ref{Any}}(), batch_size)
    end

    exec_batch(args, res) = for (i, x) in enumerate(f(args))
        res[i].x = x
    end

    new_batch()
    res = try
        rs = map(c...) do x
            r = Ref{Any}(undef)

            push!(batch_in, x)
            push!(batch_out, r)

            if length(batch_in) >= batch_size
                do_asyncmap_task!(exec_batch, s, batch_in, batch_out)
                new_batch()
            end

            r
        end

        do_asyncmap_task!(exec_batch, s, batch_in, batch_out)

        rs
    catch ex
        ex isa AbortMapException || rethrow()
    end

    wait_done(s)

    res
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


function verify_ntasks(iterable, ntasks)::Function
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

    ntasks isa Number ? () -> ntasks : ntasks
end

# Special handling for some types.
function asyncmap(f, s::AbstractString; kwargs...)
    s2 = Vector{Char}(undef, length(s))
    asyncmap!(f, s2, s; kwargs...)
    return String(s2)
end

# map on a single BitArray returns a BitArray if the mapping function is boolean.
function asyncmap(f, b::BitArray; kwargs...)
    b2 = do_asyncmap(f, b; kwargs...)
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
    ctx::AsyncmapCtx
    batch::Vector{Any}
    enum_state      # enumerator state

    AsyncCollectorState(ntasks::Function) = new(AsyncmapCtx(ntasks), [])
end

function iterate(itr::AsyncCollector)
    itr.ntasks = verify_ntasks(itr.enumerator, itr.ntasks)
    itr.batch_size = verify_batch_size(itr.batch_size)

    return iterate(itr, AsyncCollectorState(itr.ntasks))
end

function iterate(itr::AsyncCollector, state::AsyncCollectorState)
    y = isdefined(state, :enum_state) ?
        iterate(itr.enumerator, state.enum_state) :
        iterate(itr.enumerator)

    if y === nothing
        wait_done(state.ctx)
        return nothing
    end

    (i, args), state.enum_state = y

    try
        if itr.batch_size in (1, nothing)
            do_asyncmap_task!(state.ctx, i, args) do i, args
                itr.results[i] = itr.f(args...)
            end
        else
            push!(state.batch, args)

            if length(state.batch) >= itr.batch_size
                do_asyncmap_task!(state.ctx, state.batch) do batch
                    for (j, res) in enumerate(itr.f(batch))
                        itr.results[i - length(batch) + j] = res
                    end
                end

                state.batch = []
            end
        end
    catch ex
        ex isa AbortMapException || rethrow()
        wait_done()
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

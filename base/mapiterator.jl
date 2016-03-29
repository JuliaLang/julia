# This file is a part of Julia. License is MIT: http://julialang.org/license


"""
    AsyncMapIterator(f, results, c...; ntasks=100) -> iterator

Apply f to each element of c using at most 100 asynchronous tasks.
For multiple collection arguments, apply f elementwise.
Results are asynchronously stored in "results" collection
(the iterator returns nothing).

Note: `for task in AsyncMapIterator(f, results, c...)` end is equivalent to
`map!(f, results, c...)`.
"""
type AsyncMapIterator
    f
    results
    arg_enum::Enumerate
    ntasks::Int
end

function AsyncMapIterator(f, results, c...; ntasks=0)
    if ntasks == 0
        ntasks = 100
    end
    AsyncMapIterator(f, results, enumerate(zip(c...)), ntasks)
end


type AsyncMapState
    enum_state
    active_count::Int
    task_done::Condition
    done::Bool
end


# Busy if the maximum number of concurrent tasks is running.
function isbusy(itr::AsyncMapIterator, state::AsyncMapState)
    state.active_count == itr.ntasks
end


# Wait for @async task to end.
wait(state::AsyncMapState) = wait(state.task_done)


# Open a @sync block and initialise iterator state.
function start(itr::AsyncMapIterator)
    sync_begin()
    AsyncMapState(start(itr.arg_enum),  0, Condition(), false)
end

# Close @sync block when iterator is done.
function done(itr::AsyncMapIterator, state::AsyncMapState)
    if !state.done && done(itr.arg_enum, state.enum_state)
        state.done = true
        sync_end()
    end
    return state.done
end

function next(itr::AsyncMapIterator, state::AsyncMapState)

    # Wait if the maximum number of concurrent tasks are already running...
    while isbusy(itr, state)
        wait(state)
    end

    # Get index and mapped function arguments from enumeration iterator...
    (i, args), state.enum_state = next(itr.arg_enum, state.enum_state)

    # Execute function call and save result asynchronously...
    @async begin
        itr.results[i] = itr.f(args...)
        state.active_count -= 1
        notify(state.task_done, nothing)
    end

    # Count number of concurrent tasks...
    state.active_count += 1

    return (nothing, state)
end



"""
    StreamMapIterator(f, c...; ntasks=100) -> iterator

Apply f to each element of c using at most 100 asynchronous tasks.
For multiple collection arguments, apply f elementwise.
The iterator returns results as the become available.
Note: `collect(StreamMapIterator(f, c...; ntasks=1))` is equivalent to
`map(f, c...)`.
"""
type StreamMapIterator
    async_itr::AsyncMapIterator
end

function StreamMapIterator(f, c...; ntasks=0)
    StreamMapIterator(AsyncMapIterator(f, Dict{Int,Any}(), c...; ntasks=ntasks))
end


type StreamMapState
    i::Int
    async_state::AsyncMapState
end


start(itr::StreamMapIterator) = StreamMapState(0, start(itr.async_itr))

# Done when source async iterator is done and all results have been consumed.
function done(itr::StreamMapIterator, state::StreamMapState)
    done(itr.async_itr, state.async_state) && isempty(itr.async_itr.results)
end

# Pump the source async iterator if it is not already busy...
function pump_source(itr::StreamMapIterator, state::StreamMapState)
    if !isbusy(itr.async_itr, state.async_state) &&
       !done(itr.async_itr, state.async_state)
        ignored, state.async_state = next(itr.async_itr, state.async_state)
        return true
    else
        return false
    end
end

function next(itr::StreamMapIterator, state::StreamMapState)

    state.i += 1

    results = itr.async_itr.results
    while !haskey(results, state.i)

        # Wait for results to become available...
        if !pump_source(itr,state) && !haskey(results, state.i)
            wait(state.async_state)
        end
    end
    r = results[state.i]
    delete!(results, state.i)

    return (r, state)
end

iteratorsize(::Type{StreamMapIterator}) = SizeUnknown()

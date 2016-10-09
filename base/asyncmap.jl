# This file is a part of Julia. License is MIT: http://julialang.org/license


"""
    AsyncCollector(f, results, c...; ntasks=0) -> iterator

Apply `f` to each element of `c` using at most `ntasks` asynchronous
tasks.
If `ntasks` is unspecified, uses `max(100, nworkers())` tasks.
For multiple collection arguments, apply `f` elementwise.
Output is collected into `results`.

Note: `next(::AsyncCollector, state) -> (nothing, state)`

Note: `for task in AsyncCollector(f, results, c...) end` is equivalent to
`map!(f, results, c...)`.
"""
type AsyncCollector
    f
    results
    enumerator::Enumerate
    max_tasks::Function
    task_chnl::Channel{Tuple{Int, Any}}             # to communicate with the tasks

    AsyncCollector(f, r, en::Enumerate, mt::Function, c::Channel) = new(f, r, en, mt, c)
end

function AsyncCollector(f, results, c...; ntasks=0)
    if isa(ntasks, Integer) && ntasks == 0
        ntasks = max(nworkers(), 100)
        max_tasks = ()->ntasks
    elseif isa(ntasks, Integer)
        max_tasks = ()->ntasks
    elseif isa(ntasks, Function)
        max_tasks = ntasks
    else
        throw(ArgumentError("ntasks must be an Integer or a zero-arg function returning the maximum number of tasks allowed."))
    end
    AsyncCollector(f, results, enumerate(zip(c...)), max_tasks, Channel{Tuple{Int, Any}}(typemax(Int)))
end

type AsyncCollectorState
    enum_state
    active_count::Int
    item_done::Condition
    done::Bool
    in_error::Bool
    nfree::Int                  # number of free tasks
end


isbusy(itr::AsyncCollector, state::AsyncCollectorState) = (state.nfree == 0)

# Wait for @async task to end.
wait(state::AsyncCollectorState) = wait(state.item_done)

function start_collector_task(itr::AsyncCollector, state::AsyncCollectorState)
    t = @async begin
        try
            for (i, args) in itr.task_chnl
                state.nfree -= 1

                itr.results[i] = itr.f(args...)
                notify(state.item_done, nothing)

                state.nfree += 1
            end
        catch e
            # The in_error flag causes done() to end the iteration early and call sync_end().
            # sync_end() then re-throws "e" in the main task.
            state.in_error = true
            clear_collector_channel(itr)
            notify(state.item_done, nothing)

            rethrow(e)
        end
    end

    state.active_count += 1
    t
end

function clear_collector_channel(itr::AsyncCollector)
    try
        # empty out the channel and close it, ignore any errors in doing this
        while isready(itr.task_chnl)
            take!(itr.task_chnl)
        end
        close(itr.task_chnl)
    catch
    end
    nothing
end

# Open a @sync block and initialise iterator state.
function start(itr::AsyncCollector)
    sync_begin()

    state = AsyncCollectorState(start(itr.enumerator),  0, Condition(), false, false, 0)

    for _ in 1:itr.max_tasks()
        start_collector_task(itr, state)
        state.nfree += 1
    end
    state
end

# Close @sync block when iterator is done.
function done(itr::AsyncCollector, state::AsyncCollectorState)
    if state.in_error
        @assert !isopen(itr.task_chnl)  # Channel should have been cleared and closed in the async task.

        sync_end()

        # state.in_error is only being set in the @async block (and an error thrown),
        # which in turn should have been caught and thrown by the sync_end() call above.
        # Control should not come here.
        @assert false "Error should have been captured and thrown previously."
    end

    if !state.done && done(itr.enumerator, state.enum_state)
        state.done = true
        close(itr.task_chnl)
        sync_end()
    end
    return state.done
end

function next(itr::AsyncCollector, state::AsyncCollectorState)
    # start a task if required.
    # Note: Shouldn't need to check on every iteration. Do this at a periodic interval?
    if state.active_count < itr.max_tasks()
        start_collector_task(itr, state)
    end

    while isbusy(itr, state)
        wait(state)
        if state.in_error
            # Stop processing immediately on error.
            return (nothing, state)
        end
    end

    # Get index and mapped function arguments from enumeration iterator.
    (i, args), state.enum_state = next(itr.enumerator, state.enum_state)
    put!(itr.task_chnl, (i, args))

    return (nothing, state)
end

"""
    AsyncGenerator(f, c...; ntasks=0) -> iterator

Apply `f` to each element of `c` using at most `ntasks` asynchronous tasks.
If `ntasks` is unspecified, uses `max(100, nworkers())` tasks.
For multiple collection arguments, apply f elementwise.
Results are returned by the iterator as they become available.
Note: `collect(AsyncGenerator(f, c...; ntasks=1))` is equivalent to
`map(f, c...)`.
"""
type AsyncGenerator
    collector::AsyncCollector
end

function AsyncGenerator(f, c...; ntasks=0)
    AsyncGenerator(AsyncCollector(f, Dict{Int,Any}(), c...; ntasks=ntasks))
end


type AsyncGeneratorState
    i::Int
    async_state::AsyncCollectorState
end


start(itr::AsyncGenerator) = AsyncGeneratorState(0, start(itr.collector))

# Done when source async collector is done and all results have been consumed.
function done(itr::AsyncGenerator, state::AsyncGeneratorState)
    done(itr.collector, state.async_state) && isempty(itr.collector.results)
end

# Pump the source async collector if it is not already busy.
function pump_source(itr::AsyncGenerator, state::AsyncGeneratorState)
    if !isbusy(itr.collector, state.async_state) &&
       !done(itr.collector, state.async_state)
        ignored, state.async_state = next(itr.collector, state.async_state)
        return true
    else
        return false
    end
end

function next(itr::AsyncGenerator, state::AsyncGeneratorState)
    state.i += 1

    results = itr.collector.results
    while !haskey(results, state.i)

        # Wait for results to become available.
        if !pump_source(itr,state) && !haskey(results, state.i)
            wait(state.async_state)
        end
    end
    r = results[state.i]
    delete!(results, state.i)

    return (r, state)
end

iteratorsize(::Type{AsyncGenerator}) = SizeUnknown()


"""
    asyncmap(f, c...) -> collection

Transform collection `c` by applying `@async f` to each element.

For multiple collection arguments, apply f elementwise.
"""
asyncmap(f, c...) = collect(AsyncGenerator(f, c...))


"""
    asyncmap!(f, c)

In-place version of `asyncmap()`.
"""
asyncmap!(f, c) = (for x in AsyncCollector(f, c, c) end; c)


"""
    asyncmap!(f, results, c...)

Like `asyncmap()`, but stores output in `results` rather returning a collection.
"""
asyncmap!(f, r, c1, c...) = (for x in AsyncCollector(f, r, c1, c...) end; r)

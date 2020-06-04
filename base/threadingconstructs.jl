# This file is a part of Julia. License is MIT: https://julialang.org/license

export threadid, nthreads, @threads

"""
    Threads.threadid()

Get the ID number of the current thread of execution. The master thread has ID `1`.
"""
threadid() = Int(ccall(:jl_threadid, Int16, ())+1)

# Inclusive upper bound on threadid()
"""
    Threads.nthreads()

Get the number of threads available to the Julia process. This is the inclusive upper bound
on [`threadid()`](@ref).
"""
nthreads() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

function threading_run(func)
    ccall(:jl_enter_threaded_region, Cvoid, ())
    n = nthreads()
    tasks = Vector{Task}(undef, n)
    for i = 1:n
        t = Task(func)
        t.sticky = true
        ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, i-1)
        tasks[i] = t
        schedule(t)
    end
    try
        for i = 1:n
            wait(tasks[i])
        end
    finally
        ccall(:jl_exit_threaded_region, Cvoid, ())
    end
end



function _threadsfor(a, body, schedule)
    rang = map(x-> x.args[2], a) #extract the ranges from the expression
    lidx = map(x-> x.args[1], a) #extract the variable names from the expression
    rangelen = length(rang)
    #create variables to store information about each iterator
    starts = [Meta.parse("start__" * string(x)) for x in lidx]
    ends = [Meta.parse("end__" * string(x)) for x in lidx]
    ranges = [Meta.parse("range__" * string(x)) for x in lidx]
    values = [Meta.parse("val__" * string(x)) for x in lidx]
    #create expressions initializing these variables
    initranges = [:($(ranges[x]) = range[$x]) for x in 1:rangelen]
    initstarts = ((x, y) -> :($x = firstindex($y) ) ).(starts, ranges)
    initends = ((x, y) -> :($x = lastindex($y) ) ).(ends, ranges)
    initstartingindecies = ((x, y) -> :($x += $y)).(values, starts)
    #updates to variables
    updatevalues = [:( $(esc(lidx[x])) = @inbounds $(ranges[x])[$(values[x])] ) for x in 1:rangelen]
    initvalues = [ :($(values[1]) = (tid - 1) * len + min(tid - 1, rem))] #initial calculation of the first dimension
    for x in 2:rangelen
        push!(initvalues, :(($(values[x]), $(values[x - 1])) = divrem($(values[x - 1]), length($(ranges[x - 1]))))) #initial calculation for every dimension
    end
    checks = :() #checks are not necessary when there is only one variable
    for x in rangelen:-1:2
        checks =
        quote
            if $(values[x - 1]) > $(ends[x - 1]) #check the bounds of the values, starting from the end, if a bound is crossed, reset the previous value, increase the current one, and check the next
                $(values[x - 1]) = $(starts[x - 1]) #set value to firstindex() of that range
                $(values[x]) += 1
                $checks
                $(updatevalues[x]) #only update variable if it changes
            end
        end
    end
    quote
        local threadsfor_fun
        let range = [$(esc.(rang)...)]
            totallength = reduce(*, length.(range))
            function threadsfor_fun(onethread=false)
                #load into local variables
                $(initranges...)
                tlen = totallength
                #calculate the iteration length for the current thread
                if onethread
                    tid = 1
                    len, rem = tlen, 0
                else
                    tid = threadid()
                    len, rem = divrem(tlen, nthreads())
                end
                if len == 0 && rem < tid #no iterations available for this thread
                    return
                end
                #inits
                $(initstarts...)
                $(initends...)
                $(initvalues...)
                $(initstartingindecies...)
                #distribute the remainder across the threads
                if tid <= rem
                    len += 1
                end
                $(updatevalues...) #set all variables to their initial values, they will be updated later in the loop
                $(values[begin]) -=1 #reduce code duplication by "omitting" the first increment
                for i in 1:len
                    $(values[1]) += 1
                    $checks
                    $(updatevalues[1])
                    $(esc(body))
                end
            end
        end
        if threadid() != 1 || ccall(:jl_in_threaded_region, Cint, ()) != 0
            $(if schedule === :static
              :(error("`@threads :static` can only be used from thread 1 and not nested"))
              else
              # only use threads when called from thread 1, outside @threads
              :(Base.invokelatest(threadsfor_fun, true))
              end)
        else
            threading_run(threadsfor_fun)
        end
        nothing
    end
end

"""
    Threads.@threads [schedule] for ... end

A macro to parallelize a `for` loop to run with multiple threads. Splits the iteration
space among multiple tasks and runs those tasks on threads according to a scheduling
policy.
A barrier is placed at the end of the loop which waits for all tasks to finish
execution.

The `schedule` argument can be used to request a particular scheduling policy.
The only currently supported value is `:static`, which creates one task per thread
and divides the iterations equally among them. Specifying `:static` is an error
if used from inside another `@threads` loop or from a thread other than 1.

The default schedule (used when no `schedule` argument is present) is subject to change.

!!! compat "Julia 1.5"
    The `schedule` argument is available as of Julia 1.5.
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
        if sched !== :static
            throw(ArgumentError("unsupported schedule argument in @threads"))
        end
    elseif na == 1
        sched = :default
        ex = args[1]
    else
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    if !(isa(ex, Expr) && ex.head === :for)
        throw(ArgumentError("@threads requires a `for` loop expression"))
    end
    if !(ex.args[1] isa Expr && ex.args[1].head === :(=))
        return _threadsfor(ex.args[1].args, ex.args[2], sched)
    end
    return _threadsfor([ex.args[1]], ex.args[2], sched)
end

"""
    Threads.@spawn expr

Create and run a [`Task`](@ref) on any available thread. To wait for the task to
finish, call [`wait`](@ref) on the result of this macro, or call [`fetch`](@ref)
to wait and then obtain its return value.

Values can be interpolated into `@spawn` via `\$`, which copies the value directly into the
constructed underlying closure. This allows you to insert the _value_ of a variable,
isolating the aysnchronous code from changes to the variable's value in the current task.

!!! note
    See the manual chapter on threading for important caveats.

!!! compat "Julia 1.3"
    This macro is available as of Julia 1.3.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.
"""
macro spawn(expr)
    letargs = Base._lift_one_interp!(expr)

    thunk = esc(:(()->($expr)))
    var = esc(Base.sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            task.sticky = false
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

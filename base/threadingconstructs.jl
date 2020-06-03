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



function _threadsfor_nested(a, body, schedule)
    rang = map(x-> x.args[2], a) #extract the ranges from the expression
    lidx = map(x-> x.args[1], a) #extract the variable names from the expression
    acce = [ :( @inbounds range[($x)][currentiteration[($x)]]  ) for x in eachindex(rang)] #for each range create an expression that accesses it with the variable `currentiteration` declared later
    lidx = ((x, y) -> :( $(esc(x)) = $(y))).(lidx, acce) #join all expressions together, setting the variables to their corresponding `currentiteration`
    rangelen = length(rang)
    loops = (rangelen == 1) ? nothing :  quote #emulate the outer nested loop
        inv = lr
        @inbounds while currentiteration[inv] > lidx[inv]
            currentiteration[inv] = fidx[inv]
            inv -= 1
            currentiteration[inv] += 1
        end
    end
    quote
        local threadsfor_fun
        let
            range = [$(esc.(rang)...)]
            #calculate variables which are the same across the threads
            totallength = reduce(*, length.(range))
            firstindecies = firstindex.(range)
            lastindecies = lastindex.(range)
            function threadsfor_fun(onethread=false)
                #Load into local variables
                lr = $rangelen
                r = range
                tlen = totallength
                fidx = firstindecies
                lidx = lastindecies
                #calculate the iteration length for the current thread
                if onethread
                    tid = 1
                    len, rem = tlen, 0
                else
                    tid = threadid()
                    len, rem = divrem(tlen, nthreads())
                end
                currentiteration = fill(0, lr) #operation on lengths not, so set everything to 0
                currentiteration[end] = (tid - 1) * len + min(tid - 1, rem) #calculate the index
                #convert the index to lr-dimensional starting coordinate
                @inbounds for cur in lr:-1:2
                    if currentiteration[cur] == 0
                        break
                    end
                    currentiteration[cur - 1], currentiteration[cur] = divrem(currentiteration[cur], length(r[cur]) )
                end
                #from now on operations on indices
                currentiteration .+= fidx
                #distribute the remainder across the threads
                if tid <= rem
                    len += 1
                end
                #run this thread's iterations
                for i in 1:len
                    $(loops)
                    $(lidx...)
                    $(esc(body))
                    #increment currentiteration
                    currentiteration[end] += 1
                end
            end
        end
        if threadid() != 1
            $(
            if schedule === :static
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



function _threadsfor(iter, lbody, schedule)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    quote
        local threadsfor_fun
        let range = $(esc(range))
        function threadsfor_fun(onethread=false)
            r = range # Load into local variable
            lenr = length(r)
            # divide loop iterations among threads
            if onethread
                tid = 1
                len, rem = lenr, 0
            else
                tid = threadid()
                len, rem = divrem(lenr, nthreads())
            end
            # not enough iterations for all the threads?
            if len == 0
                if tid > rem
                    return
                end
                len, rem = 1, 0
            end
            # compute this thread's iterations
            f = firstindex(r) + ((tid-1) * len)
            l = f + len - 1
            # distribute remaining iterations evenly
            if rem > 0
                if tid <= rem
                    f = f + (tid-1)
                    l = l + tid
                else
                    f = f + rem
                    l = l + rem
                end
            end
            # run this thread's iterations
            for i = f:l
                local $(esc(lidx)) = @inbounds r[i]
                $(esc(lbody))
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
        return _threadsfor_nested(ex.args[1].args, ex.args[2], sched)
    end
    return _threadsfor(ex.args[1], ex.args[2], sched)
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

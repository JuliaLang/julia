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

function _threadsfor(iter_stmt, lbody)
    loopvar   = iter_stmt.args[1]
    iter      = iter_stmt.args[2]
    rng = gensym(:rng)
    out = quote
        @sync for $rng in $(Iterators.partition)($iter, $(length)($iter) ÷ $(nthreads)())
            Base.Threads.@spawn begin
                for $loopvar in $rng
                    $lbody
                end
            end
        end
    end
    esc(out)
end

"""
    Threads.@threads

A macro to parallelize a for-loop to run with multiple threads. This spawns up to [`nthreads() + 1`](@ref)
threads, splits the iteration space amongst them, and iterates in parallel.
A barrier is placed at the end of the loop which waits for all the threads to finish
execution, and the loop returns.
"""
macro threads(args...)
    na = length(args)
    if na != 1
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    ex = args[1]
    if !isa(ex, Expr)
        throw(ArgumentError("need an expression argument to @threads"))
    end
    if ex.head === :for
        if ex.args[1] isa Expr && ex.args[1].head === :(=)
            return _threadsfor(ex.args[1], ex.args[2])
        else
            throw(ArgumentError("nested outer loops are not currently supported by @threads"))
        end
    else
        throw(ArgumentError("unrecognized argument to @threads"))
    end
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
    This feature is currently considered experimental.

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
                push!($var, task)
            end
            schedule(task)
            task
        end
    end
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads())

Similar to `foreach(f, channel)`, but iteration over `channel` and calls to
`f` are split across `ntasks` Tasks spawned by `Threads.@spawn`. This function
will wait for all internally spawned tasks to complete before returning.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
function Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads(),
                         schedule=:fair)
    apply = schedule === :static ? (f, x) -> f(x) : _waitspawn
    stop = Threads.Atomic{Bool}(false)
    @sync for _ in 1:ntasks
        Threads.@spawn try
            for item in channel
                apply(f, item)
                # do `stop[] && break` after `f(item)` to avoid losing `item`.
                # this isn't super comprehensive since a task could still get
                # stuck on `take!` at `for item in channel`. We should think
                # about a more robust mechanism to avoid dropping items. See also:
                # https://github.com/JuliaLang/julia/pull/34543#discussion_r422695217
                stop[] && break
            end
        catch
            stop[] = true
            rethrow()
        end
    end
    return nothing
end

# Avoid registering the task in outer @sync:
_waitspawn(f, x) = wait(Threads.@spawn f(x))

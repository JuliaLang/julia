# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads())

Similar to `foreach(f, channel)`, but iteration over `channel` and calls to
`f` are split across `ntasks` Tasks spawned by `Threads.@spawn`. This function
will wait for all internally spawned tasks to complete before returning.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
function Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads())
    stop = Threads.Atomic{Bool}(false)
    @sync for _ in 1:ntasks
        Threads.@spawn try
            for item in channel
                _waitspawn(f, item)
                stop[] && break  # do this _after_ f(item) to avoid loosing `item`
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

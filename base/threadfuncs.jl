# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads(),
                    schedule=:fair)

Similar to `foreach(f, channel)`, but iteration over `channel` and calls to
`f` are split across `ntasks` tasks spawned by `Threads.@spawn`. This function
will wait for all internally spawned tasks to complete before returning.

If `schedule` is `:fair`, `Threads.foreach` will attempt to spawn tasks in a
manner that enables Julia's scheduler to more freely load-balance work items across
threads. This approach generally has higher per-item overhead, but may perform
better than the `:static` schedule in concurrence with other multithreaded
workloads.

If `schedule` is `:static`, `Threads.foreach` will spawn tasks in a manner that
incurs lower per-item overhead than the `:fair` schedule, but is less amenable
to load-balancing. This approach thus may be more suitable for fine-grained,
uniform workloads, but may perform worse than the `:fair` schedule in
concurrence with other multithreaded workloads.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
function Threads.foreach(f, channel::Channel; ntasks=Threads.nthreads(),
                         schedule=:fair)
    if schedule === :fair
        apply = _waitspawn
    elseif schedule === :static
        apply = (f, x) -> f(x)
    else
        throw(ArgumentError("`schedule` must be either `:fair` or `:static`"))
    end
    stop = Threads.Atomic{Bool}(false)
    @sync for _ in 1:ntasks
        Threads.@spawn try
            for item in channel
                $apply(f, item)
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

# avoid registering the task within `Threads.foreach`'s outer `@sync`
_waitspawn(f, x) = wait(Threads.@spawn f(x))

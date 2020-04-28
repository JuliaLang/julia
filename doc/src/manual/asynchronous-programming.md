# Asynchronous Programming

Julia's parallel programming platform uses [Tasks (aka Coroutines)](@ref man-tasks) to switch among multiple computations.
To express an order of execution between lightweight threads communication primitives are necessary.
Julia offers `Channel(func::Function, ctype=Any, csize=0, taskref=nothing)` that creates a new task from `func`,
binds it to a new channel of type `ctype` and size `csize` and schedule the task.
`Channels` can serve as a way to communicate between tasks, as `Channel{T}(sz::Int)` creates a buffered channel of type `T` and size `sz`.
Whenever code performs a communication operation like [`fetch`](@ref) or [`wait`](@ref),
the current task is suspended and a scheduler picks another task to run.
A task is restarted when the event it is waiting for completes.

For many problems, it is not necessary to think about tasks directly. However, they can be used
to wait for multiple events at the same time, which provides for *dynamic scheduling*. In dynamic
scheduling, a program decides what to compute or where to compute it based on when other jobs
finish. This is needed for unpredictable or unbalanced workloads, where we want to assign more
work to processes only when they finish their current tasks.

## Channels

The section on [`Task`](@ref)s in [Control Flow](@ref) discussed the execution of multiple functions in
a co-operative manner. [`Channel`](@ref)s can be quite useful to pass data between running tasks, particularly
those involving I/O operations.

Examples of operations involving I/O include reading/writing to files, accessing web services,
executing external programs, etc. In all these cases, overall execution time can be improved if
other tasks can be run while a file is being read, or while waiting for an external service/program
to complete.

A channel can be visualized as a pipe, i.e., it has a write end and a read end :

  * Multiple writers in different tasks can write to the same channel concurrently via [`put!`](@ref)
    calls.
  * Multiple readers in different tasks can read data concurrently via [`take!`](@ref) calls.
  * As an example:

    ```julia
    # Given Channels c1 and c2,
    c1 = Channel(32)
    c2 = Channel(32)

    # and a function `foo` which reads items from c1, processes the item read
    # and writes a result to c2,
    function foo()
        while true
            data = take!(c1)
            [...]               # process data
            put!(c2, result)    # write out result
        end
    end

    # we can schedule `n` instances of `foo` to be active concurrently.
    for _ in 1:n
        @async foo()
    end
    ```
  * Channels are created via the `Channel{T}(sz)` constructor. The channel will only hold objects
    of type `T`. If the type is not specified, the channel can hold objects of any type. `sz` refers
    to the maximum number of elements that can be held in the channel at any time. For example, `Channel(32)`
    creates a channel that can hold a maximum of 32 objects of any type. A `Channel{MyType}(64)` can
    hold up to 64 objects of `MyType` at any time.
  * If a [`Channel`](@ref) is empty, readers (on a [`take!`](@ref) call) will block until data is available.
  * If a [`Channel`](@ref) is full, writers (on a [`put!`](@ref) call) will block until space becomes available.
  * [`isready`](@ref) tests for the presence of any object in the channel, while [`wait`](@ref)
    waits for an object to become available.
  * A [`Channel`](@ref) is in an open state initially. This means that it can be read from and written to
    freely via [`take!`](@ref) and [`put!`](@ref) calls. [`close`](@ref) closes a [`Channel`](@ref).
    On a closed [`Channel`](@ref), [`put!`](@ref) will fail. For example:

    ```julia-repl
    julia> c = Channel(2);

    julia> put!(c, 1) # `put!` on an open channel succeeds
    1

    julia> close(c);

    julia> put!(c, 2) # `put!` on a closed channel throws an exception.
    ERROR: InvalidStateException("Channel is closed.",:closed)
    Stacktrace:
    [...]
    ```

  * [`take!`](@ref) and [`fetch`](@ref) (which retrieves but does not remove the value) on a closed
    channel successfully return any existing values until it is emptied. Continuing the above example:

    ```julia-repl
    julia> fetch(c) # Any number of `fetch` calls succeed.
    1

    julia> fetch(c)
    1

    julia> take!(c) # The first `take!` removes the value.
    1

    julia> take!(c) # No more data available on a closed channel.
    ERROR: InvalidStateException("Channel is closed.",:closed)
    Stacktrace:
    [...]
    ```

A `Channel` can be used as an iterable object in a `for` loop, in which case the loop runs as
long as the `Channel` has data or is open. The loop variable takes on all values added to the
`Channel`. The `for` loop is terminated once the `Channel` is closed and emptied.

For example, the following would cause the `for` loop to wait for more data:

```julia-repl
julia> c = Channel{Int}(10);

julia> foreach(i->put!(c, i), 1:3) # add a few entries

julia> data = [i for i in c]
```

while this will return after reading all data:

```julia-repl
julia> c = Channel{Int}(10);

julia> foreach(i->put!(c, i), 1:3); # add a few entries

julia> close(c);                    # `for` loops can exit

julia> data = [i for i in c]
3-element Array{Int64,1}:
 1
 2
 3
```

Consider a simple example using channels for inter-task communication. We start 4 tasks to process
data from a single `jobs` channel. Jobs, identified by an id (`job_id`), are written to the channel.
Each task in this simulation reads a `job_id`, waits for a random amount of time and writes back
a tuple of `job_id` and the simulated time to the results channel. Finally all the `results` are
printed out.

```julia-repl
julia> const jobs = Channel{Int}(32);

julia> const results = Channel{Tuple}(32);

julia> function do_work()
           for job_id in jobs
               exec_time = rand()
               sleep(exec_time)                # simulates elapsed time doing actual work
                                               # typically performed externally.
               put!(results, (job_id, exec_time))
           end
       end;

julia> function make_jobs(n)
           for i in 1:n
               put!(jobs, i)
           end
       end;

julia> n = 12;

julia> @async make_jobs(n); # feed the jobs channel with "n" jobs

julia> for i in 1:4 # start 4 tasks to process requests in parallel
           @async do_work()
       end

julia> @elapsed while n > 0 # print out results
           job_id, exec_time = take!(results)
           println("$job_id finished in $(round(exec_time; digits=2)) seconds")
           global n = n - 1
       end
4 finished in 0.22 seconds
3 finished in 0.45 seconds
1 finished in 0.5 seconds
7 finished in 0.14 seconds
2 finished in 0.78 seconds
5 finished in 0.9 seconds
9 finished in 0.36 seconds
6 finished in 0.87 seconds
8 finished in 0.79 seconds
10 finished in 0.64 seconds
12 finished in 0.5 seconds
11 finished in 0.97 seconds
0.029772311
```

The current version of Julia multiplexes all tasks onto a single OS thread. Thus, while tasks
involving I/O operations benefit from parallel execution, compute bound tasks are effectively
executed sequentially on a single OS thread. Future versions of Julia may support scheduling of
tasks on multiple threads, in which case compute bound tasks will see benefits of parallel execution
too.

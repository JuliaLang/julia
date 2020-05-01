# [Multi-Threading (Experimental)](@id man-multithreading)

In addition to tasks Julia natively supports multi-threading.
Note that this section is experimental and the interfaces may change in the future.

## Starting Julia with multi-threading

By default, Julia starts up with a single thread of execution. This can be verified by using the
command [`Threads.nthreads()`](@ref):

```julia-repl
julia> Threads.nthreads()
1
```

The number of threads Julia starts up with is controlled either by using the
`-t`/`--threads` command line argument or by using the
[`JULIA_NUM_THREADS`](@ref JULIA_NUM_THREADS) environment variable. When both are
specified, then `-t`/`--threads` takes precedence.

!!! compat "Julia 1.5"
    The `-t`/`--threads` command line argument requires at least Julia 1.5.
    In older versions you must use the environment variable instead.

Lets start Julia with 4 threads:

```bash
$ julia --threads 4
```

Let's verify there are 4 threads at our disposal.

```julia-repl
julia> Threads.nthreads()
4
```

But we are currently on the master thread. To check, we use the function [`Threads.threadid`](@ref)

```julia-repl
julia> Threads.threadid()
1
```

!!! note
    If you prefer to use the environment variable you can set it as follows in
    Bash (Linux/macOS):
    ```bash
    export JULIA_NUM_THREADS=4
    ```
    C shell on Linux/macOS, CMD on Windows:
    ```bash
    set JULIA_NUM_THREADS=4
    ```
    Powershell on Windows:
    ```powershell
    $env:JULIA_NUM_THREADS=4
    ```
    Note that this must be done *before* starting Julia.

!!! note
    The number of threads specified with `-t`/`--threads` is propagated to worker processes
    that are spawned using the `-p`/`--procs` or `--machine-file` command line options.
    For example, `julia -p2 -t2` spawns 1 main process with 2 worker processes, and all
    three processes have 2 threads enabled. For more fine grained control over worker
    threads use [`addprocs`](@ref) and pass `-t`/`--threads` as `exeflags`.

## The `@threads` Macro

Let's work a simple example using our native threads. Let us create an array of zeros:

```jldoctest
julia> a = zeros(10)
10-element Array{Float64,1}:
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
 0.0
```

Let us operate on this array simultaneously using 4 threads. We'll have each thread write its
thread ID into each location.

Julia supports parallel loops using the [`Threads.@threads`](@ref) macro. This macro is affixed
in front of a `for` loop to indicate to Julia that the loop is a multi-threaded region:

```julia-repl
julia> Threads.@threads for i = 1:10
           a[i] = Threads.threadid()
       end
```

The iteration space is split amongst the threads, after which each thread writes its thread ID
to its assigned locations:

```julia-repl
julia> a
10-element Array{Float64,1}:
 1.0
 1.0
 1.0
 2.0
 2.0
 2.0
 3.0
 3.0
 4.0
 4.0
```

Note that [`Threads.@threads`](@ref) does not have an optional reduction parameter like [`@distributed`](@ref).

## Atomic Operations

Julia supports accessing and modifying values *atomically*, that is, in a thread-safe way to avoid
[race conditions](https://en.wikipedia.org/wiki/Race_condition). A value (which must be of a primitive
type) can be wrapped as [`Threads.Atomic`](@ref) to indicate it must be accessed in this way.
Here we can see an example:

```julia-repl
julia> i = Threads.Atomic{Int}(0);

julia> ids = zeros(4);

julia> old_is = zeros(4);

julia> Threads.@threads for id in 1:4
           old_is[id] = Threads.atomic_add!(i, id)
           ids[id] = id
       end

julia> old_is
4-element Array{Float64,1}:
 0.0
 1.0
 7.0
 3.0

julia> ids
4-element Array{Float64,1}:
 1.0
 2.0
 3.0
 4.0
```

Had we tried to do the addition without the atomic tag, we might have gotten the
wrong answer due to a race condition. An example of what would happen if we didn't
avoid the race:

```julia-repl
julia> using Base.Threads

julia> nthreads()
4

julia> acc = Ref(0)
Base.RefValue{Int64}(0)

julia> @threads for i in 1:1000
          acc[] += 1
       end

julia> acc[]
926

julia> acc = Atomic{Int64}(0)
Atomic{Int64}(0)

julia> @threads for i in 1:1000
          atomic_add!(acc, 1)
       end

julia> acc[]
1000
```

!!! note
    Not *all* primitive types can be wrapped in an `Atomic` tag. Supported types
    are `Int8`, `Int16`, `Int32`, `Int64`, `Int128`, `UInt8`, `UInt16`, `UInt32`,
    `UInt64`, `UInt128`, `Float16`, `Float32`, and `Float64`. Additionally,
    `Int128` and `UInt128` are not supported on AAarch32 and ppc64le.

## Side effects and mutable function arguments

When using multi-threading we have to be careful when using functions that are not
[pure](https://en.wikipedia.org/wiki/Pure_function) as we might get a wrong answer.
For instance functions that have their
[name ending with `!`](@ref bang-convention)
by convention modify their arguments and thus are not pure. However, there are
functions that have side effects and their name does not end with `!`.

## @threadcall

All I/O tasks, timers, REPL commands, etc are multiplexed onto a single OS thread via an event
loop. A patched version of libuv ([http://docs.libuv.org/en/v1.x/](http://docs.libuv.org/en/v1.x/))
provides this functionality. Yield points provide for co-operatively scheduling multiple tasks
onto the same OS thread. I/O tasks and timers yield implicitly while waiting for the event to
occur. Calling [`yield`](@ref) explicitly allows for other tasks to be scheduled.

Thus, a task executing a [`ccall`](@ref) effectively prevents the Julia scheduler from executing any other
tasks till the call returns. This is true for all calls into external libraries. Exceptions are
calls into custom C code that call back into Julia (which may then yield) or C code that calls
`jl_yield()` (C equivalent of [`yield`](@ref)).

Note that while Julia code runs on a single thread (by default), libraries used by Julia may launch
their own internal threads. For example, the BLAS library may start as many threads as there are
cores on a machine.

The [`@threadcall`](@ref) macro addresses scenarios where we do not want a [`ccall`](@ref) to block the main Julia
event loop. It schedules a C function for execution in a separate thread. A threadpool with a
default size of 4 is used for this. The size of the threadpool is controlled via environment variable
`UV_THREADPOOL_SIZE`. While waiting for a free thread, and during function execution once a thread
is available, the requesting task (on the main Julia event loop) yields to other tasks. Note that
`@threadcall` does not return till the execution is complete. From a user point of view, it is
therefore a blocking call like other Julia APIs.

It is very important that the called function does not call back into Julia, as it will segfault.

`@threadcall` may be removed/changed in future versions of Julia.

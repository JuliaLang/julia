# Parallel Computing

For newcomers to multi-threading and parallel computing it can be useful to first appreciate
the different levels of parallelism offered by Julia. We can divide them in three main categories :

1. Asyncronous Programming with Julia Tasks (Co-routines)
2. Multi-Threading
3. Multi-Core and Distributed Processing

We will first consider Julia [Tasks (aka Coroutines)](@ref man-tasks) and other modules that rely on the Julia runtime library, that allow us to suspend and resume computations with full control of inter-`Tasks` communication without having to manually interface with the operating system's scheduler.
Julia also supports communication between `Tasks` through operations like [`wait`](@ref) and [`fetch`](@ref).
Communication and data synchronization is managed through [`Channel`](@ref)s, which are the conduits
that provide inter-`Tasks` communication.

Julia also supports [experimental multi-threading](@ref man-multithreading), where execution is forked and an anonymous function is run across all
threads.
Known as the fork-join approach, parallel threads execute independently, and must ultimately be joined in Julia's main thread to allow serial execution to continue.
Multi-threading is supported using the [`Base.Threads`](@ref lib-multithreading) module that is still considered experimental, as Julia is
not yet fully thread-safe. In particular segfaults seem to occur during I/O operations and task switching.
As an up-to-date reference, keep an eye on [the issue tracker](https://github.com/JuliaLang/julia/issues?q=is%3Aopen+is%3Aissue+label%3Amultithreading).
Multi-Threading should only be used if you take into consideration global variables, locks and
atomics, all of which are explained later.

In the end we will present Julia's approach to distributed and parallel computing. With scientific computing
in mind, Julia natively implements interfaces to distribute a process across multiple cores or machines.
Also we will mention useful external packages for distributed programming like `MPI.jl` and `DistributedArrays.jl`.

# Parallel Computing

Julia supports four main categories of features for concurrent and parallel programming:

1. **Asynchronous "tasks", or coroutines**:

    Julia Tasks allow suspending and resuming computations
    for I/O, event handling, producer-consumer processes, and similar patterns.
    Tasks can synchronize through operations like [`wait`](@ref) and [`fetch`](@ref), and
    communicate via [`Channel`](@ref)s. While strictly not parallel computing by themselves,
    Julia lets you schedule `Task`s on several threads.

2. **Multi-threading**:

    [Multi-threading](@ref man-multithreading) functionality builds on tasks by allowing them to run simultaneously
    on more than one thread or CPU core, sharing memory. This is usually the easiest way 
    to get parallelism on one's PC or on a single large multi-core server.

3. **Distributed computing**:

    Finally, distributed computing runs multiple processes with separate memory spaces,
    potentially on different machines. This functionality is provided by the `Distributed` 
    standard library as well as external packages like [`MPI.jl`](https://github.com/JuliaParallel/MPI.jl) and 
    [`DistributedArrays.jl`](https://github.com/JuliaParallel/DistributedArrays.jl).

4. **GPU computing**:

    The Julia GPU compiler provides the ability to run Julia code natively on GPUs. There
    is a rich ecosystem of Julia packages that target GPUs. The [JuliaGPU.org](https://juliagpu.org)
    website provides a list of capabilities, supported GPUs, related packages and documentation.
    

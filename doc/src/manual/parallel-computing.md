# Parallel Computing

Julia supports three main categories of features for concurrent and parallel programming:

1. Asynchronous "tasks", or coroutines
    Julia Tasks allow suspending and resuming computations
    for I/O, event handling, producer-consumer processes, and similar patterns.
    Tasks can synchronize through operations like [`wait`](@ref) and [`fetch`](@ref), and
    communicate via [`Channel`](@ref)s.

2. Multi-threading
    Multi-threading functionality builds on tasks by allowing them to run simultaneously
    on more than one thread or CPU core, sharing memory.

3. Distributed computing
    Finally, distributed computing runs multiple processes with separate memory spaces,
    potentially on different machines.
    This functionality is provided by the `Distributed` standard library as well as
    external packages like `MPI.jl` and `DistributedArrays.jl`.

4. GPU computing
    The Julia GPU compiler provides the ability to run Julia code natively on GPUs. There
    is a rich ecosystem of Julia packages that target GPUs. The [JuliaGPU.org](https://juliagpu.org)
    website provides a list of capabilities, supported GPUs, related packages and documentation.
    

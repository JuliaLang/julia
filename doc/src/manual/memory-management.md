# [Memory Management and Garbage Collection](@id man-memory-management)

Julia uses automatic memory management through its built-in garbage collector (GC). This section provides an overview of how Julia manages memory and how you can configure and optimize memory usage for your applications.

## [Garbage Collection Overview](@id man-gc-overview)

Julia features a garbage collector with the following characteristics:

* **Non-moving**: Objects are not relocated in memory during garbage collection
* **Generational**: Younger objects are collected more frequently than older ones
* **Parallel and partially concurrent**: The GC can use multiple threads and run concurrently with your program
* **Mostly precise**: The GC accurately identifies object references for pure Julia code, and it provides conservative scanning APIs for users calling Julia from C

The garbage collector automatically reclaims memory used by objects that are no longer reachable from your program, freeing you from manual memory management in most cases.

## [Memory Architecture](@id man-memory-architecture)

Julia uses a two-tier allocation strategy:

* **Small objects** (currently ≤ 2032 bytes but may change): Allocated using a fast per-thread pool allocator
* **Large objects** : Allocated directly through the system's `malloc`

This hybrid approach optimizes for both allocation speed and memory efficiency, with the pool allocator providing fast allocation for the many small objects typical in Julia programs.

## [System Memory Requirements](@id man-system-memory)

### Swap Space

Julia's garbage collector is designed with the expectation that your system has adequate swap space configured. The GC uses heuristics that assume it can allocate memory beyond physical RAM when needed, relying on the operating system's virtual memory management.

If your system has limited or no swap space, you may experience out-of-memory errors during garbage collection. In such cases, you can use the `--heap-size-hint` option to limit Julia's memory usage.

### Memory Hints

You can provide a hint to Julia about the maximum amount of memory to use:

```bash
julia --heap-size-hint=4G  # To set the hint to ~4GB
julia --heap-size-hint=50% # or to 50% of physical memory
```

The `--heap-size-hint` option tells the garbage collector to trigger collection more aggressively when approaching the specified limit. This is particularly useful in:

* Containers with memory limits
* Systems without swap space
* Shared systems where you want to limit Julia's memory footprint

You can also set this via the `JULIA_HEAP_SIZE_HINT` environment variable:

```bash
export JULIA_HEAP_SIZE_HINT=2G
julia
```

## [Multithreaded Garbage Collection](@id man-gc-multithreading)

Julia's garbage collector can leverage multiple threads to improve performance on multi-core systems.

### GC Thread Configuration

By default, Julia uses multiple threads for garbage collection:

* **Mark threads**: Used during the mark phase to trace object references (default: 1, which is shared with the compute thread if there is only one, otherwise half the number of compute threads)
* **Sweep threads**: Used for concurrent sweeping of freed memory (default: 0, disabled)

You can configure GC threading using:

```bash
julia --gcthreads=4,1  # 4 mark threads, 1 sweep thread
julia --gcthreads=8    # 8 mark threads, 0 sweep threads
```

Or via environment variable:

```bash
export JULIA_NUM_GC_THREADS=4,1
julia
```

### Recommendations

For compute-intensive workloads:

* Use multiple mark threads (the default configuration is usually appropriate)
* Consider enabling concurrent sweeping with 1 sweep thread for allocation-heavy workloads

For memory-intensive workloads:

* Enable concurrent sweeping to reduce GC pauses
* Monitor GC time using `@time` and adjust thread counts accordingly

## [Monitoring and Debugging](@id man-gc-monitoring)

### Basic Memory Monitoring

Use the `@time` macro to see memory allocation and GC overhead:

```julia
julia> @time some_computation()
  2.123456 seconds (1.50 M allocations: 58.725 MiB, 17.17% gc time)
```

### GC Logging

Enable detailed GC logging to understand collection patterns:

```julia
julia> GC.enable_logging(true)
julia> # Run your code
julia> GC.enable_logging(false)
```

This logs each garbage collection event with timing and memory statistics.

### Manual GC Control

While generally not recommended, you can manually trigger garbage collection:

```julia
GC.gc()          # Force a garbage collection
GC.enable(false) # Disable automatic GC (use with caution!)
GC.enable(true)  # Re-enable automatic GC
```

**Warning**: Disabling GC can lead to memory exhaustion. Only use this for specific performance measurements or debugging.

## [Performance Considerations](@id man-gc-performance)

### Reducing Allocations

The best way to minimize GC impact is to reduce unnecessary allocations:

* Use in-place operations when possible (e.g., `x .+= y` instead of `x = x + y`)
* Pre-allocate arrays and reuse them
* Avoid creating temporary objects in tight loops
* Consider using `StaticArrays.jl` for small, fixed-size arrays

### Memory-Efficient Patterns

* Avoid global variables that change type
* Use `const` for global constants

### Profiling Memory Usage

For detailed guidance on profiling memory allocations and identifying performance bottlenecks, see the [Profiling](@ref man-profiling) section.

## [Advanced Configuration](@id man-gc-advanced)

### Integration with System Memory Management

Julia works best when:

* The system has adequate swap space (recommended: 2x physical RAM)
* Virtual memory is properly configured
* Other processes leave sufficient memory available
* Container memory limits are set appropriately with `--heap-size-hint`

## [Troubleshooting Memory Issues](@id man-gc-troubleshooting)

### High GC Overhead

If garbage collection is taking too much time:

1. **Reduce allocation rate**: Focus on algorithmic improvements
2. **Adjust GC threads**: Experiment with different `--gcthreads` settings
3. **Use concurrent sweeping**: Enable background sweeping with `--gcthreads=N,1`
4. **Profile memory patterns**: Identify allocation hotspots and optimize them

### Memory Leaks

While Julia's GC prevents most memory leaks, issues can still occur:

* **Global references**: Avoid holding references to large objects in global variables
* **Closures**: Be careful with closures that capture large amounts of data
* **C interop**: Ensure proper cleanup when interfacing with C libraries

For more detailed information about Julia's garbage collector internals, see the Garbage Collection section in the Developer Documentation.

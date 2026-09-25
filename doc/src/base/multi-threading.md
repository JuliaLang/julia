# [Multi-Threading](@id lib-multithreading)

```@docs
Base.Threads.@threads
Base.Threads.foreach
Base.Threads.@spawn
Base.Threads.threadid
Base.Threads.maxthreadid
Base.Threads.nthreads
Base.Threads.threadpool
Base.Threads.nthreadpools
Base.Threads.threadpoolsize
Base.Threads.ngcthreads
```

See also [Multi-Threading](@ref man-multithreading).

## Atomic operations

```@docs
atomic
```

```@docs
Base.@atomic
Base.@atomicswap
Base.@atomicreplace
Base.@atomiconce
Base.AtomicMemory
```

There are also optional memory ordering parameters for the `unsafe` set of functions, that
select the C/C++-compatible versions of these atomic operations, if that parameter is specified to
[`unsafe_load`](@ref), [`unsafe_store!`](@ref), [`unsafe_swap!`](@ref), [`unsafe_replace!`](@ref), and [`unsafe_modify!`](@ref).

### Atomic memory fences

```@docs
Base.Threads.atomic_fence
Base.Threads.atomic_fence_heavy
Base.Threads.atomic_fence_light
```

### Atomic values (`Threads.Atomic`)

!!! note
    [`Threads.Atomic`](@ref) is a standalone, [`Ref`](@ref)-like atomic cell. Like `Ref`, it is a
    useful building block and is not going to be removed, but an `@atomic` field of a mutable struct
    is usually preferable when you have the choice. The `Threads.atomic_*` functions below predate
    the [`@atomic`](@ref Base.@atomic) macros and still work, but the macros are the recommended way
    to operate on an atomic cell; see
    [The `@atomic` reference interface](@ref man-atomic-reference) for how to translate them.

!!! warning
    Storing into a `Threads.Atomic` with the plain `a[] = v` form is deprecated (since uses such as
    `a[] += 1` look atomic but are not); use `@atomic a[] = v` instead.

```@docs
Base.Threads.Atomic
Base.Threads.atomic_cas!
Base.Threads.atomic_xchg!
Base.Threads.atomic_add!
Base.Threads.atomic_sub!
Base.Threads.atomic_and!
Base.Threads.atomic_nand!
Base.Threads.atomic_or!
Base.Threads.atomic_xor!
Base.Threads.atomic_max!
Base.Threads.atomic_min!
```

## ccall using a libuv threadpool (Experimental)

```@docs
Base.@threadcall
```

## Low-level synchronization primitives

These building blocks are used to create the regular synchronization objects.

```@docs
Base.Threads.AbstractSpinLock
Base.Threads.SpinLock
Base.Threads.PaddedSpinLock
```

## Task metrics (Experimental)

```@docs
Base.Experimental.task_metrics
Base.Experimental.task_running_time_ns
Base.Experimental.task_wall_time_ns
```

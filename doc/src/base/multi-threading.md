# [Multi-Threading](@id lib-multithreading)

```@docs
Base.Threads.@threads
Base.Threads.foreach
Base.Threads.@spawn
Base.Threads.threadid
Base.Threads.nthreads
```

## Synchronization

```@docs
Base.Threads.Condition
Base.Threads.Event
```

See also [Synchronization](@ref lib-task-sync).

## Atomic operations

!!! warning

    The API for atomic operations has not yet been finalized and is likely to change.

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
Base.Threads.atomic_fence
```

## ccall using a threadpool (Experimental)

```@docs
Base.@threadcall
```

## Low-level synchronization primitives

These building blocks are used to create the regular synchronization objects.

```@docs
Base.Threads.SpinLock
```

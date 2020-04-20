# [Multi-Threading](@id lib-multithreading)

This experimental interface supports Julia's multi-threading capabilities. Types and functions
described here might (and likely will) change in the future.

```@docs
Base.Threads.threadid
Base.Threads.nthreads
Base.Threads.@threads
Base.Threads.@spawn
```

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

# Low-level synchronization primitives

These building blocks are used to create the regular synchronization objects.

```@docs
Base.Threads.SpinLock
```


# Multi-Threading

This experimental interface supports Julia's multi-threading capabilities. Types and functions
described here might (and likely will) change in the future.

```@docs
Base.Threads.threadid
Base.Threads.nthreads
Base.Threads.@threads
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

## Synchronization Primitives

```@docs
Base.Threads.AbstractLock
Base.lock
Base.unlock
Base.trylock
Base.islocked
Base.ReentrantLock
Base.Threads.Mutex
Base.Threads.SpinLock
Base.Threads.RecursiveSpinLock
Base.Semaphore
Base.acquire
Base.release
```


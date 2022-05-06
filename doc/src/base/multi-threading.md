# [Multi-Threading](@id lib-multithreading)

```@docs
Base.Threads.@threads
Base.Threads.foreach
Base.Threads.@spawn
Base.Threads.threadid
Base.Threads.nthreads
Base.Threads.threadpool
Base.Threads.nthreadpools
```

See also [Multi-Threading](@ref man-multithreading).

## Atomic operations

```@docs
Base.@atomic
Base.@atomicswap
Base.@atomicreplace
```

!!! note

    The following APIs are fairly primitive, and will likely be exposed through an `unsafe_*`-like wrapper.

```
Core.Intrinsics.atomic_pointerref(pointer::Ptr{T}, order::Symbol) --> T
Core.Intrinsics.atomic_pointerset(pointer::Ptr{T}, new::T, order::Symbol) --> pointer
Core.Intrinsics.atomic_pointerswap(pointer::Ptr{T}, new::T, order::Symbol) --> old
Core.Intrinsics.atomic_pointermodify(pointer::Ptr{T}, function::(old::T,arg::S)->T, arg::S, order::Symbol) --> old
Core.Intrinsics.atomic_pointerreplace(pointer::Ptr{T}, expected::Any, new::T, success_order::Symbol, failure_order::Symbol) --> (old, cmp)
```

!!! warning

    The following APIs are deprecated, though support for them is likely to remain for several releases.

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

## ccall using a libuv threadpool (Experimental)

```@docs
Base.@threadcall
```

## Low-level synchronization primitives

These building blocks are used to create the regular synchronization objects.

```@docs
Base.Threads.SpinLock
```

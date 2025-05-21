# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: setindex!, getindex, unsafe_convert
import .Base.Sys: ARCH, WORD_SIZE

export
    Atomic,
    atomic_cas!,
    atomic_xchg!,
    atomic_add!, atomic_sub!,
    atomic_and!, atomic_nand!, atomic_or!, atomic_xor!,
    atomic_max!, atomic_min!,
    atomic_fence

"""
    Threads.Atomic{T}

Holds a reference to an object of type `T`, ensuring that it is only
accessed atomically, i.e. in a thread-safe manner.

New atomic objects can be created from a non-atomic values; if none is
specified, the atomic object is initialized with zero.

Atomic objects can be accessed using the `[]` notation:

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> x[] = 1
1

julia> x[]
1
```

Atomic operations use an `atomic_` prefix, such as [`atomic_add!`](@ref),
[`atomic_xchg!`](@ref), etc.
"""
mutable struct Atomic{T}
    @atomic value::T
    Atomic{T}() where {T} = new(zero(T))
    Atomic{T}(value) where {T} = new(value)
end

Atomic() = Atomic{Int}()

const LOCK_PROFILING = Atomic{Int}(0)
lock_profiling(state::Bool) = state ? atomic_add!(LOCK_PROFILING, 1) : atomic_sub!(LOCK_PROFILING, 1)
lock_profiling() = LOCK_PROFILING[] > 0

const LOCK_CONFLICT_COUNT = Atomic{Int}(0);
inc_lock_conflict_count() = atomic_add!(LOCK_CONFLICT_COUNT, 1)

"""
    Threads.atomic_cas!(x::Atomic{T}, cmp::T, newval::T) where T

Atomically compare-and-set `x`

Atomically compares the value in `x` with `cmp`. If equal, write
`newval` to `x`. Otherwise, leaves `x` unmodified. Returns the old
value in `x`. By comparing the returned value to `cmp` (via `===`) one
knows whether `x` was modified and now holds the new value `newval`.

For further details, see LLVM's `cmpxchg` instruction.

This function can be used to implement transactional semantics. Before
the transaction, one records the value in `x`. After the transaction,
the new value is stored only if `x` has not been modified in the mean
time.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_cas!(x, 4, 2);

julia> x
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_cas!(x, 3, 2);

julia> x
Base.Threads.Atomic{Int64}(2)
```
"""
function atomic_cas! end

"""
    Threads.atomic_xchg!(x::Atomic{T}, newval::T) where T

Atomically exchange the value in `x`

Atomically exchanges the value in `x` with `newval`. Returns the **old**
value.

For further details, see LLVM's `atomicrmw xchg` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_xchg!(x, 2)
3

julia> x[]
2
```
"""
function atomic_xchg! end

"""
    Threads.atomic_add!(x::Atomic{T}, val::T) where T <: ArithmeticTypes

Atomically add `val` to `x`

Performs `x[] += val` atomically. Returns the **old** value. Not defined for
`Atomic{Bool}`.

For further details, see LLVM's `atomicrmw add` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_add!(x, 2)
3

julia> x[]
5
```
"""
function atomic_add! end

"""
    Threads.atomic_sub!(x::Atomic{T}, val::T) where T <: ArithmeticTypes

Atomically subtract `val` from `x`

Performs `x[] -= val` atomically. Returns the **old** value. Not defined for
`Atomic{Bool}`.

For further details, see LLVM's `atomicrmw sub` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_sub!(x, 2)
3

julia> x[]
1
```
"""
function atomic_sub! end

"""
    Threads.atomic_and!(x::Atomic{T}, val::T) where T

Atomically bitwise-and `x` with `val`

Performs `x[] &= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw and` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_and!(x, 2)
3

julia> x[]
2
```
"""
function atomic_and! end

"""
    Threads.atomic_nand!(x::Atomic{T}, val::T) where T

Atomically bitwise-nand (not-and) `x` with `val`

Performs `x[] = ~(x[] & val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw nand` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(3)
Base.Threads.Atomic{Int64}(3)

julia> Threads.atomic_nand!(x, 2)
3

julia> x[]
-3
```
"""
function atomic_nand! end

"""
    Threads.atomic_or!(x::Atomic{T}, val::T) where T

Atomically bitwise-or `x` with `val`

Performs `x[] |= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw or` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_or!(x, 7)
5

julia> x[]
7
```
"""
function atomic_or! end

"""
    Threads.atomic_xor!(x::Atomic{T}, val::T) where T

Atomically bitwise-xor (exclusive-or) `x` with `val`

Performs `x[] \$= val` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw xor` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_xor!(x, 7)
5

julia> x[]
2
```
"""
function atomic_xor! end

"""
    Threads.atomic_max!(x::Atomic{T}, val::T) where T

Atomically store the maximum of `x` and `val` in `x`

Performs `x[] = max(x[], val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw max` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(5)
Base.Threads.Atomic{Int64}(5)

julia> Threads.atomic_max!(x, 7)
5

julia> x[]
7
```
"""
function atomic_max! end

"""
    Threads.atomic_min!(x::Atomic{T}, val::T) where T

Atomically store the minimum of `x` and `val` in `x`

Performs `x[] = min(x[], val)` atomically. Returns the **old** value.

For further details, see LLVM's `atomicrmw min` instruction.

# Examples
```jldoctest
julia> x = Threads.Atomic{Int}(7)
Base.Threads.Atomic{Int64}(7)

julia> Threads.atomic_min!(x, 5)
7

julia> x[]
5
```
"""
function atomic_min! end

#const nand = (~) ∘ (&) # ComposedFunction generated very poor code quality
nand(x, y) = ~(x & y)

getindex(x::Atomic) = @atomic :acquire x.value
setindex!(x::Atomic, v) = (@atomic :release x.value = v; x)
atomic_cas!(x::Atomic, cmp, new) = (@atomicreplace :acquire_release :acquire x.value cmp => new).old
atomic_add!(x::Atomic, v) = (@atomic :acquire_release x.value + v).first
atomic_sub!(x::Atomic, v) = (@atomic :acquire_release x.value - v).first
atomic_and!(x::Atomic, v) = (@atomic :acquire_release x.value & v).first
atomic_or!(x::Atomic, v) = (@atomic :acquire_release x.value | v).first
atomic_xor!(x::Atomic, v) = (@atomic :acquire_release x.value ⊻ v).first
atomic_nand!(x::Atomic, v) = (@atomic :acquire_release x.value nand v).first
atomic_xchg!(x::Atomic, v) = (@atomicswap :acquire_release x.value = v)
atomic_min!(x::Atomic, v) = (@atomic :acquire_release x.value min v).first
atomic_max!(x::Atomic, v) = (@atomic :acquire_release x.value max v).first

"""
    Threads.atomic_fence()

Insert a sequential-consistency memory fence

Inserts a memory fence with sequentially-consistent ordering
semantics. There are algorithms where this is needed, i.e. where an
acquire/release ordering is insufficient.

This is likely a very expensive operation. Given that all other atomic
operations in Julia already have acquire/release semantics, explicit
fences should not be necessary in most cases.

For further details, see LLVM's `fence` instruction.
"""
atomic_fence() = Core.Intrinsics.atomic_fence(:sequentially_consistent)

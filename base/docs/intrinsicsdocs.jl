# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Core.IR

The `Core.IR` module exports the IR object model.
"""
Core.IR

"""
    Core.IntrinsicFunction <: Core.Builtin <: Function

The `Core.IntrinsicFunction` function define some basic primitives for what defines the
abilities and behaviors of a Julia program
"""
Core.IntrinsicFunction

"""
    Core.Intrinsics

The `Core.Intrinsics` module holds the `Core.IntrinsicFunction` objects.
"""
Core.Intrinsics

"""
    Core.memoryrefnew(::GenericMemory)
    Core.memoryrefnew(::GenericMemoryRef, index::Int, [boundscheck::Bool])

Return a `GenericMemoryRef` for a `GenericMemory`. See [`memoryref`](@ref).

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.
"""
Core.memoryrefnew

"""
    Core..memoryrefoffset(::GenericMemoryRef)

Return the offset index that was used to construct the `MemoryRef`. See [`memoryref`](@ref).

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.
"""
Core.memoryrefoffset

"""
    Core.memoryrefget(::GenericMemoryRef, ordering::Symbol, boundscheck::Bool)

Return the value stored at the `MemoryRef`, throwing a `BoundsError` if the `Memory` is empty. See `ref[]`.
The memory ordering specified must be compatible with the `isatomic` parameter.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.
"""
Core.memoryrefget

"""
    Core.memoryrefset!(::GenericMemoryRef, value, ordering::Symbol, boundscheck::Bool)

Store the value to the `MemoryRef`, throwing a `BoundsError` if the `Memory` is empty. See `ref[] = value`.
The memory ordering specified must be compatible with the `isatomic` parameter.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.
"""
Core.memoryrefset!

"""
    Core.memoryref_isassigned(::GenericMemoryRef, ordering::Symbol, boundscheck::Bool)

Return whether there is a value stored at the `MemoryRef`, returning false if the `Memory`
is empty. See [`isassigned(::Base.RefValue)`](@ref), [`Core.memoryrefget`](@ref).
The memory ordering specified must be compatible with the `isatomic` parameter.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.
"""
Core.memoryref_isassigned

"""
    Core.memoryrefswap!(::GenericMemoryRef, value, ordering::Symbol, boundscheck::Bool)

Atomically perform the operations to simultaneously get and set a `MemoryRef` value.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.

See also [`swapproperty!`](@ref Base.swapproperty!) and [`Core.memoryrefset!`](@ref).
"""
Core.memoryrefswap!

"""
    Core.memoryrefmodify!(::GenericMemoryRef, op, value, ordering::Symbol, boundscheck::Bool)::Pair

Atomically perform the operations to get and set a `MemoryRef` value after applying
the function `op`.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.

See also [`modifyproperty!`](@ref Base.modifyproperty!) and [`Core.memoryrefset!`](@ref).
"""
Core.memoryrefmodify!

"""
    Core.memoryrefreplace!(::GenericMemoryRef, expected, desired,
                           success_order::Symbol, fail_order::Symbol=success_order, boundscheck::Bool) -> (; old, success::Bool)

Atomically perform the operations to get and conditionally set a `MemoryRef` value.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.

See also [`replaceproperty!`](@ref Base.replaceproperty!) and [`Core.memoryrefset!`](@ref).
"""
Core.memoryrefreplace!

"""
    Core.memoryrefsetonce!(::GenericMemoryRef, value,
                           success_order::Symbol, fail_order::Symbol=success_order, boundscheck::Bool) -> success::Bool

Atomically perform the operations to set a `MemoryRef` to
a given value, only if it was previously not set.

!!! compat "Julia 1.11"
    This function requires Julia 1.11 or later.

See also [`setpropertyonce!`](@ref Base.replaceproperty!) and [`Core.memoryrefset!`](@ref).
"""
Core.memoryrefsetonce!

"""
    Core.Intrinsics.atomic_pointerref(pointer::Ptr{T}, order::Symbol) --> T

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.

See [`unsafe_load`](@ref Base.unsafe_load).
"""
Core.Intrinsics.atomic_pointerref

"""
    Core.Intrinsics.atomic_pointerset(pointer::Ptr{T}, new::T, order::Symbol) --> pointer

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.

See [`unsafe_store!`](@ref Base.unsafe_store!).
"""
Core.Intrinsics.atomic_pointerset

"""
    Core.Intrinsics.atomic_pointerswap(pointer::Ptr{T}, new::T, order::Symbol) --> old

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.

See [`unsafe_swap!`](@ref Base.unsafe_swap!).
"""
Core.Intrinsics.atomic_pointerswap

"""
    Core.Intrinsics.atomic_pointermodify(pointer::Ptr{T}, function::(old::T,arg::S)->T, arg::S, order::Symbol) --> old

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.

See [`unsafe_modify!`](@ref Base.unsafe_modify!).
"""
Core.Intrinsics.atomic_pointermodify

"""
    Core.Intrinsics.atomic_pointerreplace(pointer::Ptr{T}, expected::Any, new::T, success_order::Symbol, failure_order::Symbol) --> (old, cmp)

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.

See [`unsafe_replace!`](@ref Base.unsafe_replace!).
"""
Core.Intrinsics.atomic_pointerreplace

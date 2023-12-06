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
    Core.memoryref(::GenericMemory)
    Core.memoryref(::GenericMemoryRef, index::Int, [boundscheck::Bool])

Return a `GenericMemoryRef` for a `GenericMemory`. See [`MemoryRef`](@ref).
"""
Core.memoryref

"""
    Core..memoryrefoffset(::GenericMemoryRef)

Return the offset index that was used to construct the `MemoryRef`. See [`Core.memoryref`](@ref).
"""
Core.memoryrefoffset

"""
    Core.memoryrefget(::GenericMemoryRef, ordering::Symbol, boundscheck::Bool)

Return the value stored at the `MemoryRef`, throwing a `BoundsError` if the `Memory` is empty. See `ref[]`.
The memory ordering specified must be compatible with the `isatomic` parameter.
"""
Core.memoryrefget

"""
    Core.memoryrefset!(::GenericMemoryRef, value, ordering::Symbol, boundscheck::Bool)

Store the value to the `MemoryRef`, throwing a `BoundsError` if the `Memory` is empty. See `ref[] = value`.
The memory ordering specified must be compatible with the `isatomic` parameter.
"""
Core.memoryrefset!

"""
    Core.memoryref_isassigned(::GenericMemoryRef, ordering::Symbol, boundscheck::Bool)

Return whether there is a value stored at the `MemoryRef`, returning false if the `Memory`
is empty. See [`isassigned(::Base.RefValue)`](@ref), [`Core.memoryrefget`](@ref).
The memory ordering specified must be compatible with the `isatomic` parameter.
"""
Core.memoryref_isassigned

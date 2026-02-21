# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Ptr{T}

A memory address referring to data of type `T`.  However, there is no guarantee that the
memory is actually valid, or that it actually represents data of the specified type.
"""
Ptr

## converting pointers to an appropriate unsigned ##

"""
    C_NULL

The C null pointer constant, sometimes used when calling external code.
"""
const C_NULL = bitcast(Ptr{Cvoid}, 0)

# TODO: deprecate these conversions. C doesn't even allow them.

# pointer to integer
convert(::Type{T}, x::Ptr) where {T<:Integer} = T(UInt(x))::T

# integer to pointer
convert(::Type{Ptr{T}}, x::Union{Int,UInt}) where {T} = Ptr{T}(x)

# pointer to pointer
convert(::Type{Ptr{T}}, p::Ptr{T}) where {T} = p
convert(::Type{Ptr{T}}, p::Ptr) where {T} = bitcast(Ptr{T}, p)::Ptr{T}

# object to pointer (when used with ccall)

"""
    unsafe_convert(T, x)

Convert `x` to a C argument of type `T`
where the input `x` must be the return value of `cconvert(T, ...)`.

In cases where [`convert`](@ref) would need to take a Julia object
and turn it into a `Ptr`, this function should be used to define and perform
that conversion.

Be careful to ensure that a Julia reference to `x` exists as long as the result of this
function will be used. Accordingly, the argument `x` to this function should never be an
expression, only a variable name or field reference. For example, `x=a.b.c` is acceptable,
but `x=[a,b,c]` is not.

The `unsafe` prefix on this function indicates that using the result of this function after
the `x` argument to this function is no longer accessible to the program may cause undefined
behavior, including program corruption or segfaults, at any later time.

See also [`cconvert`](@ref)
"""
function unsafe_convert end

# convert strings to String etc. to pass as pointers
cconvert(::Type{Ptr{UInt8}}, s::AbstractString) = String(s)
cconvert(::Type{Ptr{Int8}}, s::AbstractString) = String(s)
unsafe_convert(::Type{Ptr{UInt8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), x)
unsafe_convert(::Type{Ptr{Int8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Int8}, (Any,), x)

cconvert(::Type{<:Ptr}, a::Array) = getfield(a, :ref)
unsafe_convert(::Type{Ptr{S}}, a::AbstractArray{T}) where {S,T} = convert(Ptr{S}, unsafe_convert(Ptr{T}, a))
unsafe_convert(::Type{Ptr{T}}, a::Array{T}) where {T} = unsafe_convert(Ptr{T}, a.ref)
unsafe_convert(::Type{Ptr{T}}, a::AbstractArray{T}) where {T} = error("conversion to pointer not defined for $(typeof(a))")
# TODO: add this deprecation to give a better error:
# cconvert(::Type{<:Ptr}, a::AbstractArray) = error("conversion to pointer not defined for $(typeof(a))")
# unsafe_convert(::Type{Ptr{T}}, a::AbstractArray{T}) where {T} = error("missing call to cconvert for call to unsafe_convert for AbstractArray")

cconvert(::Type{<:Ptr}, a::GenericMemory) = a
unsafe_convert(::Type{Ptr{Cvoid}}, a::GenericMemory{T}) where {T} = getfield(a, :ptr)
unsafe_convert(::Type{Ptr{T}}, a::GenericMemory) where {T} = convert(Ptr{T}, getfield(a, :ptr))

function unsafe_convert(::Type{Ptr{Cvoid}}, a::GenericMemoryRef{<:Any,T,Core.CPU}) where {T}
    mem = getfield(a, :mem)
    offset = getfield(a, :ptr_or_offset)
    MemT = typeof(mem)
    arrayelem = datatype_arrayelem(MemT)
    elsz = datatype_layoutsize(MemT)
    isboxed = 1; isunion = 2
    if arrayelem == isunion || elsz == 0
        offset = UInt(offset) * elsz
        offset += unsafe_convert(Ptr{Cvoid}, mem)
    end
    return offset
end
unsafe_convert(::Type{Ptr{T}}, a::GenericMemoryRef) where {T} = convert(Ptr{T}, unsafe_convert(Ptr{Cvoid}, a))

# unsafe pointer to array conversions
"""
    unsafe_wrap(Array, pointer::Ptr{T}, dims; own = false)

Wrap a Julia `Array` object around the data at the address given by `pointer`,
without making a copy.  The pointer element type `T` determines the array
element type. `dims` is either an integer (for a 1d array) or a tuple of the array dimensions.
`own` optionally specifies whether Julia should take ownership of the memory,
calling `free` on the pointer when the array is no longer referenced.

This function is labeled "unsafe" because it will crash if `pointer` is not
a valid memory address to data of the requested length. Unlike [`unsafe_load`](@ref)
and [`unsafe_store!`](@ref), the programmer is responsible also for ensuring that the
underlying data is not accessed through two arrays of different element type, similar
to the strict aliasing rule in C.
"""
function unsafe_wrap(::Union{Type{Array},Type{Array{T}},Type{Array{T,N}}},
                     p::Ptr{T}, dims::NTuple{N,Int}; own::Bool = false) where {T,N}
    ccall(:jl_ptr_to_array, Array{T,N}, (Any, Ptr{Cvoid}, Any, Int32),
          Array{T,N}, p, dims, own)
end
function unsafe_wrap(::Union{Type{Array},Type{Array{T}},Type{Array{T,1}}},
                     p::Ptr{T}, d::Integer; own::Bool = false) where {T}
    ccall(:jl_ptr_to_array_1d, Array{T,1},
          (Any, Ptr{Cvoid}, Csize_t, Cint), Array{T,1}, p, d, own)
end
function unsafe_wrap(::Union{Type{GenericMemory{kind,<:Any,Core.CPU}},Type{GenericMemory{kind,T,Core.CPU}}},
                     p::Ptr{T}, dims::Tuple{Int}; own::Bool = false) where {kind,T}
    ccall(:jl_ptr_to_genericmemory, Ref{GenericMemory{kind,T,Core.CPU}},
          (Any, Ptr{Cvoid}, Csize_t, Cint), GenericMemory{kind,T,Core.CPU}, p, dims[1], own)
end
function unsafe_wrap(::Union{Type{GenericMemory{kind,<:Any,Core.CPU}},Type{GenericMemory{kind,T,Core.CPU}}},
                     p::Ptr{T}, d::Integer; own::Bool = false) where {kind,T}
    ccall(:jl_ptr_to_genericmemory, Ref{GenericMemory{kind,T,Core.CPU}},
          (Any, Ptr{Cvoid}, Csize_t, Cint), GenericMemory{kind,T,Core.CPU}, p, d, own)
end
unsafe_wrap(Atype::Union{Type{Array},Type{Array{T}},Type{Array{T,N}},Type{GenericMemory{kind,<:Any,Core.CPU}},Type{GenericMemory{kind,T,Core.CPU}}} where {kind},
            p::Ptr{T}, dims::NTuple{N,<:Integer}; own::Bool = false) where {T,N} =
    unsafe_wrap(Atype, p, convert(Tuple{Vararg{Int}}, dims), own = own)


"""
    unsafe_load(p::Ptr{T}, i::Integer=1)
    unsafe_load(p::Ptr{T}, order::Symbol)
    unsafe_load(p::Ptr{T}, i::Integer, order::Symbol)

Load a value of type `T` from the address of the `i`th element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1]`. Optionally, an atomic memory ordering can
be provided.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Like C, the programmer is responsible for ensuring
that referenced memory is not freed or garbage collected while invoking this function.
Incorrect usage may segfault your program or return garbage answers. Unlike C, dereferencing
memory region allocated as different type may be valid provided that the types are compatible.

!!! compat "Julia 1.10"
     The `order` argument is available as of Julia 1.10.

See also: [`atomic`](@ref)
"""
unsafe_load(p::Ptr, i::Integer=1) = pointerref(p, Int(i), 1)
unsafe_load(p::Ptr, order::Symbol) = atomic_pointerref(p, order)
function unsafe_load(p::Ptr, i::Integer, order::Symbol)
    unsafe_load(p + (elsize(typeof(p)) * (Int(i) - 1)), order)
end

"""
    unsafe_store!(p::Ptr{T}, x, i::Integer=1)
    unsafe_store!(p::Ptr{T}, x, order::Symbol)
    unsafe_store!(p::Ptr{T}, x, i::Integer, order::Symbol)

Store a value of type `T` to the address of the `i`th element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1] = x`. Optionally, an atomic memory ordering
can be provided.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Like C, the programmer is responsible for ensuring
that referenced memory is not freed or garbage collected while invoking this function.
Incorrect usage may segfault your program. Unlike C, storing memory region allocated as
different type may be valid provided that the types are compatible.

!!! compat "Julia 1.10"
     The `order` argument is available as of Julia 1.10.

See also: [`atomic`](@ref)
"""
unsafe_store!(p::Ptr{Any}, @nospecialize(x), i::Integer=1) = pointerset(p, x, Int(i), 1)
unsafe_store!(p::Ptr{T}, x, i::Integer=1) where {T} = pointerset(p, convert(T,x), Int(i), 1)
unsafe_store!(p::Ptr{T}, x, order::Symbol) where {T} = atomic_pointerset(p, x isa T ? x : convert(T,x), order)
function unsafe_store!(p::Ptr, x, i::Integer, order::Symbol)
    unsafe_store!(p + (elsize(typeof(p)) * (Int(i) - 1)), x, order)
end

"""
    unsafe_modify!(p::Ptr{T}, op, x, [order::Symbol])::Pair

These atomically perform the operations to get and set a memory address after applying
the function `op`. If supported by the hardware (for example, atomic increment), this may be
optimized to the appropriate hardware instruction, otherwise its execution will be
similar to:

    y = unsafe_load(p)
    z = op(y, x)
    unsafe_store!(p, z)
    return y => z

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Like C, the programmer is responsible for ensuring
that referenced memory is not freed or garbage collected while invoking this function.
Incorrect usage may segfault your program.

!!! compat "Julia 1.10"
     This function requires at least Julia 1.10.

See also: [`modifyproperty!`](@ref Base.modifyproperty!), [`atomic`](@ref)
"""
function unsafe_modify!(p::Ptr, op, x, order::Symbol=:not_atomic)
    return atomic_pointermodify(p, op, x, order)
end

"""
    unsafe_replace!(p::Ptr{T}, expected, desired,
                   [success_order::Symbol[, fail_order::Symbol=success_order]]) -> (; old, success::Bool)

These atomically perform the operations to get and conditionally set a memory address to
a given value. If supported by the hardware, this may be optimized to the appropriate
hardware instruction, otherwise its execution will be similar to:

    y = unsafe_load(p, fail_order)
    ok = y === expected
    if ok
        unsafe_store!(p, desired, success_order)
    end
    return (; old = y, success = ok)

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Like C, the programmer is responsible for ensuring
that referenced memory is not freed or garbage collected while invoking this function.
Incorrect usage may segfault your program.

!!! compat "Julia 1.10"
     This function requires at least Julia 1.10.

See also: [`replaceproperty!`](@ref Base.replaceproperty!), [`atomic`](@ref)
"""
function unsafe_replace!(p::Ptr{T}, expected, desired, success_order::Symbol=:not_atomic, fail_order::Symbol=success_order) where {T}
    @inline
    xT = desired isa T ? desired : convert(T, desired)
    return atomic_pointerreplace(p, expected, xT, success_order, fail_order)
end
function unsafe_replace!(p::Ptr{Any}, @nospecialize(expected), @nospecialize(desired), success_order::Symbol=:not_atomic, fail_order::Symbol=success_order)
    return atomic_pointerreplace(p, expected, desired, success_order, fail_order)
end

"""
    unsafe_swap!(p::Ptr{T}, x, [order::Symbol])

These atomically perform the operations to simultaneously get and set a memory address.
If supported by the hardware, this may be optimized to the appropriate hardware
instruction, otherwise its execution will be similar to:

    y = unsafe_load(p)
    unsafe_store!(p, x)
    return y

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Like C, the programmer is responsible for ensuring
that referenced memory is not freed or garbage collected while invoking this function.
Incorrect usage may segfault your program.

!!! compat "Julia 1.10"
     This function requires at least Julia 1.10.

See also: [`swapproperty!`](@ref Base.swapproperty!), [`atomic`](@ref)
"""
function unsafe_swap!(p::Ptr{Any}, x, order::Symbol=:not_atomic)
    return atomic_pointerswap(p, x, order)
end
function unsafe_swap!(p::Ptr{T}, x, order::Symbol=:not_atomic) where {T}
    @inline
    xT = x isa T ? x : convert(T, x)
    return atomic_pointerswap(p, xT, order)
end

# convert a raw Ptr to an object reference, and vice-versa
"""
    unsafe_pointer_to_objref(p::Ptr)

Convert a `Ptr` to an object reference. Assumes the pointer refers to a valid heap-allocated
Julia object. If this is not the case, undefined behavior results, hence this function is
considered "unsafe" and should be used with care.

See also [`pointer_from_objref`](@ref).
"""
unsafe_pointer_to_objref(x::Ptr) = ccall(:jl_value_ptr, Any, (Ptr{Cvoid},), x)

"""
    pointer_from_objref(x)

Get the memory address of a Julia object as a `Ptr`. The existence of the resulting `Ptr`
will not protect the object from garbage collection, so you must ensure that the object
remains referenced for the whole time that the `Ptr` will be used.

This function may not be called on immutable objects, since they do not have
stable memory addresses.

See also [`unsafe_pointer_to_objref`](@ref).
"""
function pointer_from_objref(@nospecialize(x))
    @inline
    ismutable(x) || error("pointer_from_objref cannot be used on immutable objects")
    ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), x)
end

## limited pointer arithmetic & comparison ##

isequal(x::Ptr, y::Ptr) = (x === y)
isless(x::Ptr{T}, y::Ptr{T}) where {T} = x < y

==(x::Ptr, y::Ptr) = UInt(x) == UInt(y)
<(x::Ptr,  y::Ptr) = UInt(x) < UInt(y)
-(x::Ptr,  y::Ptr) = UInt(x) - UInt(y)

+(x::Ptr, y::Integer) = add_ptr(x, (y % UInt) % UInt)
-(x::Ptr, y::Integer) = sub_ptr(x, (y % UInt) % UInt)
+(x::Integer, y::Ptr) = y + x

unsigned(x::Ptr) = UInt(x)
signed(x::Ptr) = Int(x)

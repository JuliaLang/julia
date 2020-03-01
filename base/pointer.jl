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
convert(::Type{T}, x::Ptr) where {T<:Integer} = T(UInt(x))

# integer to pointer
convert(::Type{Ptr{T}}, x::Union{Int,UInt}) where {T} = Ptr{T}(x)

# pointer to pointer
convert(::Type{Ptr{T}}, p::Ptr{T}) where {T} = p
convert(::Type{Ptr{T}}, p::Ptr) where {T} = bitcast(Ptr{T}, p)

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

unsafe_convert(::Type{Ptr{UInt8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), x)
unsafe_convert(::Type{Ptr{Int8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Int8}, (Any,), x)
unsafe_convert(::Type{Ptr{UInt8}}, s::String) = convert(Ptr{UInt8}, pointer_from_objref(s)+sizeof(Int))
unsafe_convert(::Type{Ptr{Int8}}, s::String) = convert(Ptr{Int8}, pointer_from_objref(s)+sizeof(Int))
# convert strings to String etc. to pass as pointers
cconvert(::Type{Ptr{UInt8}}, s::AbstractString) = String(s)
cconvert(::Type{Ptr{Int8}}, s::AbstractString) = String(s)

unsafe_convert(::Type{Ptr{T}}, a::Array{T}) where {T} = ccall(:jl_array_ptr, Ptr{T}, (Any,), a)
unsafe_convert(::Type{Ptr{S}}, a::AbstractArray{T}) where {S,T} = convert(Ptr{S}, unsafe_convert(Ptr{T}, a))
unsafe_convert(::Type{Ptr{T}}, a::AbstractArray{T}) where {T} = error("conversion to pointer not defined for $(typeof(a))")

# unsafe pointer to array conversions
"""
    unsafe_wrap(Array, pointer::Ptr{T}, dims; own = false)

Wrap a Julia `Array` object around the data at the address given by `pointer`,
without making a copy.  The pointer element type `T` determines the array
element type. `dims` is either an integer (for a 1d array) or a tuple of the array dimensions.
`own` optionally specifies whether Julia should take ownership of the memory,
calling `free` on the pointer when the array is no longer referenced.

This function is labeled "unsafe" because it will crash if `pointer` is not
a valid memory address to data of the requested length.
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
unsafe_wrap(Atype::Type, p::Ptr, dims::NTuple{N,<:Integer}; own::Bool = false) where {N} =
    unsafe_wrap(Atype, p, convert(Tuple{Vararg{Int}}, dims), own = own)

"""
    unsafe_load(p::Ptr{T}, i::Integer=1)

Load a value of type `T` from the address of the `i`th element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1]`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may segfault your program or return
garbage answers, in the same manner as C.
"""
unsafe_load(p::Ptr, i::Integer=1) = pointerref(p, Int(i), 1)

"""
    unsafe_store!(p::Ptr{T}, x, i::Integer=1)

Store a value of type `T` to the address of the `i`th element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1] = x`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may corrupt or segfault your
program, in the same manner as C.
"""
unsafe_store!(p::Ptr{Any}, @nospecialize(x), i::Integer=1) = pointerset(p, x, Int(i), 1)
unsafe_store!(p::Ptr{T}, x, i::Integer=1) where {T} = pointerset(p, convert(T,x), Int(i), 1)

# convert a raw Ptr to an object reference, and vice-versa
"""
    unsafe_pointer_to_objref(p::Ptr)

Convert a `Ptr` to an object reference. Assumes the pointer refers to a valid heap-allocated
Julia object. If this is not the case, undefined behavior results, hence this function is
considered "unsafe" and should be used with care.

See also: [`pointer_from_objref`](@ref).
"""
unsafe_pointer_to_objref(x::Ptr) = ccall(:jl_value_ptr, Any, (Ptr{Cvoid},), x)

"""
    pointer_from_objref(x)

Get the memory address of a Julia object as a `Ptr`. The existence of the resulting `Ptr`
will not protect the object from garbage collection, so you must ensure that the object
remains referenced for the whole time that the `Ptr` will be used.

This function may not be called on immutable objects, since they do not have
stable memory addresses.

See also: [`unsafe_pointer_to_objref`](@ref).
"""
function pointer_from_objref(@nospecialize(x))
    @_inline_meta
    typeof(x).mutable || error("pointer_from_objref cannot be used on immutable objects")
    ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), x)
end

## limited pointer arithmetic & comparison ##

isequal(x::Ptr, y::Ptr) = (x === y)
isless(x::Ptr{T}, y::Ptr{T}) where {T} = x < y

==(x::Ptr, y::Ptr) = UInt(x) == UInt(y)
<(x::Ptr,  y::Ptr) = UInt(x) < UInt(y)
-(x::Ptr,  y::Ptr) = UInt(x) - UInt(y)

+(x::Ptr, y::Integer) = oftype(x, add_ptr(UInt(x), (y % UInt) % UInt))
-(x::Ptr, y::Integer) = oftype(x, sub_ptr(UInt(x), (y % UInt) % UInt))
+(x::Integer, y::Ptr) = y + x

unsigned(x::Ptr) = UInt(x)
signed(x::Ptr) = Int(x)

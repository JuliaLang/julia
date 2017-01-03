# This file is a part of Julia. License is MIT: http://julialang.org/license

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
const C_NULL = box(Ptr{Void}, 0)

# pointer to integer
convert{T<:Union{Int,UInt}}(::Type{T}, x::Ptr) = box(T, unbox(Ptr{Void},x))
convert{T<:Integer}(::Type{T}, x::Ptr) = convert(T,convert(UInt, x))

# integer to pointer
convert{T}(::Type{Ptr{T}}, x::UInt) = box(Ptr{T},unbox(UInt,UInt(x)))
convert{T}(::Type{Ptr{T}}, x::Int) = box(Ptr{T},unbox(Int,Int(x)))

# pointer to pointer
convert{T}(::Type{Ptr{T}}, p::Ptr{T}) = p
convert{T}(::Type{Ptr{T}}, p::Ptr) = box(Ptr{T}, unbox(Ptr{Void},p))

# object to pointer (when used with ccall)
unsafe_convert(::Type{Ptr{UInt8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), x)
unsafe_convert(::Type{Ptr{Int8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Int8}, (Any,), x)
unsafe_convert(::Type{Ptr{UInt8}}, s::String) = convert(Ptr{UInt8}, pointer_from_objref(s)+sizeof(Int))
unsafe_convert(::Type{Ptr{Int8}}, s::String) = convert(Ptr{Int8}, pointer_from_objref(s)+sizeof(Int))
# convert strings to String etc. to pass as pointers
cconvert(::Type{Ptr{UInt8}}, s::AbstractString) = String(s)
cconvert(::Type{Ptr{Int8}}, s::AbstractString) = String(s)

unsafe_convert{T}(::Type{Ptr{T}}, a::Array{T}) = ccall(:jl_array_ptr, Ptr{T}, (Any,), a)
unsafe_convert{S,T}(::Type{Ptr{S}}, a::AbstractArray{T}) = convert(Ptr{S}, unsafe_convert(Ptr{T}, a))
unsafe_convert{T}(::Type{Ptr{T}}, a::AbstractArray{T}) = error("conversion to pointer not defined for $(typeof(a))")

# unsafe pointer to array conversions
"""
    unsafe_wrap(Array, pointer::Ptr{T}, dims, own=false)

Wrap a Julia `Array` object around the data at the address given by `pointer`,
without making a copy.  The pointer element type `T` determines the array
element type. `dims` is either an integer (for a 1d array) or a tuple of the array dimensions.
`own` optionally specifies whether Julia should take ownership of the memory,
calling `free` on the pointer when the array is no longer referenced.

This function is labelled "unsafe" because it will crash if `pointer` is not
a valid memory address to data of the requested length.
"""
function unsafe_wrap{T,N}(::Union{Type{Array},Type{Array{T}},Type{Array{T,N}}},
                          p::Ptr{T}, dims::NTuple{N,Int}, own::Bool=false)
    ccall(:jl_ptr_to_array, Array{T,N}, (Any, Ptr{Void}, Any, Int32),
          Array{T,N}, p, dims, own)
end
function unsafe_wrap{T}(::Union{Type{Array},Type{Array{T}},Type{Array{T,1}}},
                        p::Ptr{T}, d::Integer, own::Bool=false)
    ccall(:jl_ptr_to_array_1d, Array{T,1},
          (Any, Ptr{Void}, Csize_t, Cint), Array{T,1}, p, d, own)
end
unsafe_wrap{N,I<:Integer}(Atype::Type, p::Ptr, dims::NTuple{N,I}, own::Bool=false) =
    unsafe_wrap(Atype, p, convert(Tuple{Vararg{Int}}, dims), own)

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
unsafe_store!(p::Ptr{Any}, x::ANY, i::Integer=1) = pointerset(p, x, Int(i), 1)
unsafe_store!{T}(p::Ptr{T}, x, i::Integer=1) = pointerset(p, convert(T,x), Int(i), 1)

# convert a raw Ptr to an object reference, and vice-versa
"""
    unsafe_pointer_to_objref(p::Ptr)

Convert a `Ptr` to an object reference. Assumes the pointer refers to a valid heap-allocated
Julia object. If this is not the case, undefined behavior results, hence this function is
considered "unsafe" and should be used with care.
"""
unsafe_pointer_to_objref(x::Ptr) = ccall(:jl_value_ptr, Any, (Ptr{Void},), x)

"""
    pointer_from_objref(x)

Get the memory address of a Julia object as a `Ptr`. The existence of the resulting `Ptr`
will not protect the object from garbage collection, so you must ensure that the object
remains referenced for the whole time that the `Ptr` will be used.
"""
pointer_from_objref(x::ANY) = ccall(:jl_value_ptr, Ptr{Void}, (Any,), x)
data_pointer_from_objref(x::ANY) = pointer_from_objref(x)::Ptr{Void}

eltype{T}(::Type{Ptr{T}}) = T

## limited pointer arithmetic & comparison ##

==(x::Ptr, y::Ptr) = UInt(x) == UInt(y)
isless(x::Ptr, y::Ptr) = isless(UInt(x), UInt(y))
-(x::Ptr, y::Ptr) = UInt(x) - UInt(y)

+(x::Ptr, y::Integer) = oftype(x, (UInt(x) + (y % UInt) % UInt))
-(x::Ptr, y::Integer) = oftype(x, (UInt(x) - (y % UInt) % UInt))
+(x::Integer, y::Ptr) = y + x

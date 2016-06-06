# This file is a part of Julia. License is MIT: http://julialang.org/license

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
unsafe_convert(::Type{Ptr{UInt8}}, s::String) = unsafe_convert(Ptr{UInt8}, s.data)
unsafe_convert(::Type{Ptr{Int8}}, s::String) = convert(Ptr{Int8}, unsafe_convert(Ptr{UInt8}, s.data))
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
    ccall(:jl_ptr_to_array_1d, Vector{T},
          (Any, Ptr{Void}, Csize_t, Cint), Array{T,1}, p, d, own)
end
unsafe_wrap{N,I<:Integer}(Atype::Type, p::Ptr, dims::NTuple{N,I}, own::Bool=false) =
    unsafe_wrap(Atype, p, convert(Tuple{Vararg{Int}}, dims), own)

unsafe_load(p::Ptr, i::Integer=1) = pointerref(p, Int(i), 1)
unsafe_store!(p::Ptr{Any}, x::ANY, i::Integer=1) = pointerset(p, x, Int(i), 1)
unsafe_store!{T}(p::Ptr{T}, x, i::Integer=1) = pointerset(p, convert(T,x), Int(i), 1)

# unsafe pointer to string conversions (don't make a copy, unlike unsafe_string)
# (Cstring versions are in c.jl)
"""
    unsafe_wrap(String, p::Ptr{UInt8}, [length,] own=false)

Wrap a pointer `p` to an array of bytes in a `String` object,
interpreting the bytes as UTF-8 encoded characters *without making a
copy*. The optional `length` argument indicates the length in bytes of
the pointer's data; if it is omitted, the data is assumed to be
NUL-terminated.  The `own` argument optionally specifies whether Julia
should take ownership of the memory, calling `free` on the pointer
when the array is no longer referenced.

This function is labelled "unsafe" because it will crash if `p` is not
a valid memory address to data of the requested length.

See also [`unsafe_string`](:func:`unsafe_string`), which takes a pointer
and makes a copy of the data.
"""
unsafe_wrap(::Type{String}, p::Union{Ptr{UInt8},Ptr{Int8}}, len::Integer, own::Bool=false) =
    ccall(:jl_array_to_string, Ref{String}, (Any,),
          ccall(:jl_ptr_to_array_1d, Vector{UInt8}, (Any, Ptr{UInt8}, Csize_t, Cint),
                Vector{UInt8}, p, len, own))
unsafe_wrap(::Type{String}, p::Union{Ptr{UInt8},Ptr{Int8}}, own::Bool=false) =
    unsafe_wrap(String, p, ccall(:strlen, Csize_t, (Ptr{UInt8},), p), own)

# convert a raw Ptr to an object reference, and vice-versa
unsafe_pointer_to_objref(x::Ptr) = ccall(:jl_value_ptr, Any, (Ptr{Void},), x)
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

# This file is a part of Julia. License is MIT: http://julialang.org/license

## converting pointers to an appropriate unsigned ##

const C_NULL = box(Ptr{Void}, 0)

# pointer to integer
convert{T<:Union(Int,UInt)}(::Type{T}, x::Ptr) = box(T, unbox(Ptr,x))
convert{T<:Integer}(::Type{T}, x::Ptr) = convert(T,convert(UInt, x))

# integer to pointer
convert{T}(::Type{Ptr{T}}, x::UInt) = box(Ptr{T},unbox(UInt,UInt(x)))
convert{T}(::Type{Ptr{T}}, x::Int) = box(Ptr{T},unbox(Int,Int(x)))

# pointer to pointer
convert{T}(::Type{Ptr{T}}, p::Ptr{T}) = p
convert{T}(::Type{Ptr{T}}, p::Ptr) = box(Ptr{T}, unbox(Ptr,p))

# object to pointer (when used with ccall)
unsafe_convert(::Type{Ptr{UInt8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), x)
unsafe_convert(::Type{Ptr{Int8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Int8}, (Any,), x)
unsafe_convert(::Type{Ptr{UInt8}}, s::ByteString) = unsafe_convert(Ptr{UInt8}, s.data)
unsafe_convert(::Type{Ptr{Int8}}, s::ByteString) = convert(Ptr{Int8}, unsafe_convert(Ptr{UInt8}, s.data))
# convert strings to ByteString etc. to pass as pointers
cconvert(::Type{Ptr{UInt8}}, s::AbstractString) = bytestring(s)
cconvert(::Type{Ptr{Int8}}, s::AbstractString) = bytestring(s)

unsafe_convert{T}(::Type{Ptr{T}}, a::Array{T}) = ccall(:jl_array_ptr, Ptr{T}, (Any,), a)
unsafe_convert(::Type{Ptr{Void}}, a::Array) = ccall(:jl_array_ptr, Ptr{Void}, (Any,), a)

# unsafe pointer to array conversions
pointer_to_array(p, d::Integer, own=false) = pointer_to_array(p, (d,), own)
function pointer_to_array{T,N}(p::Ptr{T}, dims::NTuple{N,Int}, own::Bool=false)
    ccall(:jl_ptr_to_array, Array{T,N}, (Any, Ptr{Void}, Any, Int32),
          Array{T,N}, p, dims, own)
end
function pointer_to_array{T,N}(p::Ptr{T}, dims::NTuple{N,Integer}, own::Bool=false)
    i = 1
    for d in dims
        if !(0 <= d <= typemax(Int))
            throw(ArgumentError("Array dimension must be 0 ≤ dim ≤ $(typemax(Int)), got $d for dimension $i"))
        end
        i += 1
    end
    pointer_to_array(p, convert(Tuple{Vararg{Int}}, dims), own)
end
unsafe_load(p::Ptr,i::Integer) = pointerref(p, Int(i))
unsafe_load(p::Ptr) = unsafe_load(p, 1)
unsafe_store!(p::Ptr{Any}, x::ANY, i::Integer) = pointerset(p, x, Int(i))
unsafe_store!{T}(p::Ptr{T}, x, i::Integer) = pointerset(p, convert(T,x), Int(i))
unsafe_store!{T}(p::Ptr{T}, x) = pointerset(p, convert(T,x), 1)

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

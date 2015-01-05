## converting pointers to an appropriate unsigned ##

const C_NULL = box(Ptr{Void}, 0)

# pointer to integer
convert(::Type{UInt}, x::Ptr) = box(UInt, unbox(Ptr,x))
convert{T<:Integer}(::Type{T}, x::Ptr) = convert(T,unsigned(x))

# integer to pointer
convert{T}(::Type{Ptr{T}}, x::Integer) = box(Ptr{T},unbox(UInt,uint(x)))

# pointer to pointer
convert{T}(::Type{Ptr{T}}, p::Ptr{T}) = p
convert{T}(::Type{Ptr{T}}, p::Ptr) = box(Ptr{T}, unbox(Ptr,p))

# object to pointer
convert(::Type{Ptr{UInt8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), x)
convert(::Type{Ptr{Int8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Int8}, (Any,), x)
convert(::Type{Ptr{UInt8}}, s::ByteString) = convert(Ptr{UInt8}, s.data)
convert(::Type{Ptr{Int8}}, s::ByteString) = convert(Ptr{Int8}, s.data)

convert{T}(::Type{Ptr{T}}, a::Array{T}) = ccall(:jl_array_ptr, Ptr{T}, (Any,), a)
convert(::Type{Ptr{Void}}, a::Array) = ccall(:jl_array_ptr, Ptr{Void}, (Any,), a)

# note: these definitions don't mean any AbstractArray is convertible to
# pointer. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = convert(Ptr{T},x)
pointer{T}(x::AbstractArray{T}, i::Integer) = convert(Ptr{T},x) + (i-1)*elsize(x)

# unsafe pointer to array conversions
pointer_to_array(p, d::Integer, own=false) = pointer_to_array(p, (d,), own)
function pointer_to_array{T,N}(p::Ptr{T}, dims::NTuple{N,Int}, own::Bool=false)
    ccall(:jl_ptr_to_array, Array{T,N}, (Any, Ptr{Void}, Any, Int32),
          Array{T,N}, p, dims, own)
end
function pointer_to_array{T,N}(p::Ptr{T}, dims::NTuple{N,Integer}, own::Bool=false)
    for d in dims
        if !(0 <= d <= typemax(Int))
            error("invalid Array dimensions")
        end
    end
    pointer_to_array(p, convert((Int...), dims), own)
end
unsafe_load(p::Ptr,i::Integer) = pointerref(p, int(i))
unsafe_load(p::Ptr) = unsafe_load(p, 1)
unsafe_store!(p::Ptr{Any}, x::ANY, i::Integer) = pointerset(p, x, int(i))
unsafe_store!{T}(p::Ptr{T}, x, i::Integer) = pointerset(p, convert(T,x), int(i))
unsafe_store!{T}(p::Ptr{T}, x) = pointerset(p, convert(T,x), 1)

# convert a raw Ptr to an object reference, and vice-versa
unsafe_pointer_to_objref(p::Ptr) = pointertoref(unbox(Ptr{Void},p))
pointer_from_objref(x::Any) = ccall(:jl_value_ptr, Ptr{Void}, (Any,), x)

integer(x::Ptr) = convert(UInt, x)
unsigned(x::Ptr) = convert(UInt, x)

eltype{T}(::Ptr{T}) = T

## limited pointer arithmetic & comparison ##

==(x::Ptr, y::Ptr) = uint(x) == uint(y)
isless(x::Ptr, y::Ptr) = isless(uint(x), uint(y))
-(x::Ptr, y::Ptr) = uint(x) - uint(y)

+(x::Ptr, y::Integer) = oftype(x, (uint(x) + (y % UInt) % UInt))
-(x::Ptr, y::Integer) = oftype(x, (uint(x) - (y % UInt) % UInt))
+(x::Integer, y::Ptr) = y + x

## converting pointers to an appropriate unsigned ##

const C_NULL = box(Ptr{Void}, 0)

# pointer to integer
convert(::Type{Uint}, x::Ptr) = box(Uint, unbox(Ptr,x))
convert{T<:Integer}(::Type{T}, x::Ptr) = convert(T,unsigned(x))

# integer to pointer
convert{T}(::Type{Ptr{T}}, x::Integer) = box(Ptr{T},unbox(Uint,uint(x)))

# pointer to pointer
convert{T}(::Type{Ptr{T}}, p::Ptr{T}) = p
convert{T}(::Type{Ptr{T}}, p::Ptr) = box(Ptr{T}, unbox(Ptr,p))

# object to pointer
convert(::Type{Ptr{Uint8}}, x::Symbol) = ccall(:jl_symbol_name, Ptr{Uint8}, (Any,), x)
convert{T}(::Type{Ptr{T}}, a::Array) = ccall(:jl_array_ptr, Ptr{T}, (Any,), a)
convert(::Type{Ptr{Uint8}}, s::ByteString) = convert(Ptr{Uint8}, s.data)

pointer{T}(::Type{T}, x::Uint) = convert(Ptr{T}, x)
pointer{T}(::Type{T}, x::Ptr) = convert(Ptr{T}, x)
# note: these definitions don't mean any AbstractArray is convertible to
# pointer. they just map the array element type to the pointer type for
# convenience in cases that work.
pointer{T}(x::AbstractArray{T}) = convert(Ptr{T},x)
pointer{T}(x::AbstractArray{T}, i::Int) = convert(Ptr{T},x) + (i-1)*sizeof(T)

# unsafe pointer to array conversions
pointer_to_array(p, dims) = pointer_to_array(p, dims, false)
function pointer_to_array{T,N}(p::Ptr{T}, dims::NTuple{N,Int}, own::Bool)
    ccall(:jl_ptr_to_array, Array{T,N}, (Any, Ptr{T}, Any, Int32),
          Array{T,N}, p, dims, own)
end
unsafe_ref(p::Ptr,i::Integer) = pointerref(p, int(i))
unsafe_ref(p::Ptr) = unsafe_ref(p, 1)
unsafe_assign(p::Ptr{Any}, x::ANY, i::Integer) = pointerset(p, x, int(i))
unsafe_assign{T}(p::Ptr{T}, x, i::Integer) = pointerset(p, convert(T, x), int(i))
unsafe_assign{T}(p::Ptr{T}, x) = unsafe_assign(p, convert(T,x), 1)

# convert a raw Ptr to an object reference, and vice-versa
unsafe_pointer_to_objref(p::Ptr) = pointertoref(unbox(Ptr{Void},p))
pointer_from_objref(x::Any) = ccall(:jl_value_ptr, Ptr{Void}, (Any,), x)

integer(x::Ptr) = convert(Uint, x)
unsigned(x::Ptr) = convert(Uint, x)

@eval sizeof(::Type{Ptr}) = $(div(WORD_SIZE,8))
@eval sizeof{T}(::Type{Ptr{T}}) = $(div(WORD_SIZE,8))
eltype{T}(::Ptr{T}) = T

## limited pointer arithmetic & comparison ##

isequal(x::Ptr, y::Ptr) = uint(x) == uint(y)
-(x::Ptr, y::Ptr) = uint(x) - uint(y)

+{T}(x::Ptr{T}, y::Integer) = pointer(T, uint(uint(x) + y))
-{T}(x::Ptr{T}, y::Integer) = pointer(T, uint(uint(x) - y))
+(x::Integer, y::Ptr) = y + x

## system word size ##

WORD_SIZE = ccall(:jl_word_size, Int32, ())

## converting pointers to an appropriate uint ##

if WORD_SIZE == 64
    typealias PtrInt Uint64
    typealias Long Int64
    typealias Ulong Uint64
else
    typealias PtrInt Uint32
    typealias Long Int32
    typealias Ulong Uint32
end

# pointer to int
convert(::Type{PtrInt}, x::Ptr) = box(PtrInt,unbox(PtrInt,x))
convert{T<:Int}(::Type{T}, x::Ptr) = convert(T,uint(x))

# int to pointer
convert{T}(::Type{Ptr{T}}, x::PtrInt) = box(Ptr{T},unbox(PtrInt,x))

# pointer to pointer
convert{T}(::Type{Ptr{T}}, p::Ptr{T}) = p
convert{T}(::Type{Ptr{T}}, p::Ptr) = box(Ptr{T},unbox(PtrInt,p))

# object to pointer
convert(::Type{Ptr{Uint8}}, x::Symbol) =
    ccall(:jl_symbol_name, Ptr{Uint8}, (Any,), x)
convert(::Type{Ptr{Void}}, a::Array) =
    ccall(:jl_array_ptr, Ptr{Void}, (Any,), a)
convert{T}(::Type{Ptr{T}}, a::Array{T}) =
    convert(Ptr{T}, convert(Ptr{Void}, a))

pointer{T}(::Type{T}, x::PtrInt) = convert(Ptr{T}, x)
pointer{T}(x::Array{T}) = convert(Ptr{T},x)

uint(x::Ptr) = convert(PtrInt, x)
ptrint(x) = convert(PtrInt, x)
long(x) = convert(Long, x)
ulong(x) = convert(Ulong, x)

@eval sizeof(::Type{Ptr}) = $div(WORD_SIZE,8)

## limited pointer arithmetic & comparison ##

isequal(x::Ptr, y::Ptr) = uint(x) == uint(y)
 -(x::Ptr, y::Ptr) = uint(x) - uint(y)

+{T}(x::Ptr{T}, y::Int) = pointer(T, uint(x) + uint(y))
-{T}(x::Ptr{T}, y::Int) = pointer(T, uint(x) - uint(y))

+(x::Int, y::Ptr) = y + x

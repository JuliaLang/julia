## system word size ##

WORD_SIZE = ccall(:jl_word_size, Int32, ())

## converting pointers to an appropriate uint ##

if WORD_SIZE == 64
    typealias PtrInt Uint64
else
    typealias PtrInt Uint32
end

convert(::Type{PtrInt}, x::Ptr) = box(PtrInt,unbox(PtrInt,x))
convert{T}(::Type{Ptr{T}}, x::PtrInt) = box(Ptr{T},unbox(PtrInt,x))
convert{T<:Int}(::Type{T}, x::Ptr) = convert(T,uint(x))

pointer{T}(::Type{T}, x::PtrInt) = convert(Ptr{T}, x)
pointer{T}(x::Array{T}) = convert(Ptr{T},x)

uint(x::Ptr) = convert(PtrInt, x)

@eval sizeof{T}(::Type{Ptr{T}}) = $div(WORD_SIZE,8)

## limited pointer arithmetic & comparison ##

== (x::Ptr, y::Ptr) = uint(x) == uint(y)
(-)(x::Ptr, y::Ptr) = uint(x) - uint(y)

(+){T}(x::Ptr{T}, y::Int) = pointer(T, uint(x) + uint(y))
(-){T}(x::Ptr{T}, y::Int) = pointer(T, uint(x) - uint(y))

(+)(x::Int, y::Ptr) = y + x

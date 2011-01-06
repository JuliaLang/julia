## system word size ##

WORD_SIZE = ccall(:jl_word_size, Int32, ())

## converting pointers to an appropriate uint ##

if WORD_SIZE == 64
    typealias PtrInt Uint64
else
    typealias PtrInt Uint32
end

convert(::Type{PtrInt}, x::Ptr) = box(PtrInt,unbox(PtrInt,x))

uint(x::Ptr) = convert(PtrInt, x)

convert{T<:Int}(::Type{T}, x::Ptr) = convert(T,uint(x))

pointer{T}(x::Array{T}) = convert(Ptr{T},x)

@eval sizeof{T}(::Type{Ptr{T}}) = $div(WORD_SIZE,8)

## pointer subtraction & comparison ##

(-)(x::Ptr, y::Ptr) = uint(x) - uint(y)
==(x::Ptr, y::Ptr) = uint(x) == uint(y)

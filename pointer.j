## system word size ##

WORD_SIZE = ccall(dlsym(JuliaDLHandle,"jl_word_size"), Int32, ())

## converting pointers to an appropriate uint ##

POINTER_INT_TYPE = WORD_SIZE == 64 ? Uint64 : Uint32

convert(::Type{POINTER_INT_TYPE}, x::Ptr) =
    box(POINTER_INT_TYPE,unbox(POINTER_INT_TYPE,x))

uint(x::Ptr) = convert(POINTER_INT_TYPE, x)

convert{T<:Int}(::Type{T}, x::Ptr) = convert(T,uint(x))

pointer{T}(x::Array{T}) = convert(Ptr{T},x)

sizeof{T}(::Type{Ptr{T}}) = WORD_SIZE

## pointer subtraction & comparison ##

(-)(x::Ptr, y::Ptr) = uint(x) - uint(y)
==(x::Ptr, y::Ptr) = uint(x) == uint(y)

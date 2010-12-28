## system word size ##

WORD_SIZE = ccall(dlsym(JuliaDLHandle,"jl_word_size"), Int32, ())

## pointer conversions ##

POINTER_INT_TYPE = WORD_SIZE == 64 ? Uint64 : Uint32

convert(::Type{POINTER_INT_TYPE}, x::Ptr) =
    box(POINTER_INT_TYPE,unbox(POINTER_INT_TYPE,x))

uint(x::Ptr) = convert(POINTER_INT_TYPE, x)

convert{T<:Int}(::Type{T}, x::Ptr) = convert(T,uint(x))

pointer{T}(x::Array{T}) = convert(Ptr{T},x)

## pointer promotions ##

promote_rule(::Type{Ptr}, T::Type{Int8})   = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Uint8})  = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Int16})  = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Uint16}) = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Char})   = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Int32})  = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, T::Type{Uint32}) = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, I::Type{Int64})  = promote_type(POINTER_INT_TYPE,T)
promote_rule(::Type{Ptr}, I::Type{Uint64}) = promote_type(POINTER_INT_TYPE,T)

## pointer arithmetic ##

-(x::Ptr) = -uint(x)
~(x::Ptr) = ~uint(x)
(+)(x::Ptr, y::Ptr) = uint(x) + uint(y)
(-)(x::Ptr, y::Ptr) = uint(x) - uint(y)
(*)(x::Ptr, y::Ptr) = uint(x) * uint(y)
div(x::Ptr, y::Ptr) = div(uint(x), uint(y))
fld(x::Ptr, y::Ptr) = div(uint(x), uint(y))
rem(x::Ptr, y::Ptr) = rem(uint(x), uint(y))
mod(x::Ptr, y::Ptr) = rem(uint(x), uint(y))
(&)(x::Ptr, y::Ptr) = uint(x) & uint(y)
(|)(x::Ptr, y::Ptr) = uint(x) | uint(y)
($)(x::Ptr, y::Ptr) = uint(x) $ uint(y)
(<<)(x::Ptr, y::Int32) = uint(x) << y
(>>)(x::Ptr, y::Int32) = uint(x) >>> y
(>>>)(x::Ptr, y::Int32) = uint(x) >>> y
==(x::Ptr, y::Ptr) = uint(x) == uint(y)
< (x::Ptr, y::Ptr) = uint(x) <  uint(y)

## traits ##

sizeof{T}(::Type{Ptr{T}}) = WORD_SIZE

# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic operations on numbers ##

isinteger(x::Integer) = true

size(x::Number) = ()
size(x::Number,d) = convert(Int,d)<1 ? throw(BoundsError()) : 1
eltype{T<:Number}(::Type{T}) = T
ndims(x::Number) = 0
ndims{T<:Number}(::Type{T}) = 0
length(x::Number) = 1
endof(x::Number) = 1
getindex(x::Number) = x
getindex(x::Number, i::Integer) = i == 1 ? x : throw(BoundsError())
function getindex(x::Number, I::Integer...)
    for i in I
        i == 1 || throw(BoundsError())
    end
    x
end
getindex(x::Number, I::Real...) = getindex(x, to_index(I)...)
first(x::Number) = x
last(x::Number) = x

divrem(x,y) = (div(x,y),rem(x,y))
fldmod(x,y) = (fld(x,y),mod(x,y))
signbit(x::Real) = x < 0
sign(x::Real) = ifelse(x < 0, oftype(x,-1), ifelse(x > 0, one(x), x))
sign(x::Unsigned) = ifelse(x > 0, one(x), x)
abs(x::Real) = ifelse(signbit(x), -x, x)
abs2(x::Real) = x*x
copysign(x::Real, y::Real) = ifelse(signbit(x)!=signbit(y), -x, x)

conj(x::Real) = x
transpose(x::Number) = x
ctranspose(x::Number) = conj(x)
inv(x::Number) = one(x)/x
angle(z::Real) = atan2(zero(z), z)

widemul(x::Number, y::Number) = widen(x)*widen(y)

start(x::Number) = false
next(x::Number, state) = (x, true)
done(x::Number, state) = state
isempty(x::Number) = false
in(x::Number, y::Number) = x == y

map(f, x::Number) = f(x)

zero(x::Number) = oftype(x,0)
zero{T<:Number}(::Type{T}) = convert(T,0)
one(x::Number)  = oftype(x,1)
one{T<:Number}(::Type{T}) = convert(T,1)

factorial(x::Number) = gamma(x + 1) # fallback for x not Integer

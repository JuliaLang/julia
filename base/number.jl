# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic operations on numbers ##

isinteger(x::Integer) = true

"""
    iszero(x)

Return `true` if `x == zero(x)`; if `x` is an array, this checks whether
all of the elements of `x` are zero.
"""
iszero(x) = x == zero(x) # fallback method

# it is sometimes convenient, when writing generic code,
# to be able to iterate over numbers as 1-element collections (#7903)
first(x::Number) = x
last(x::Number) = x
start(::Number) = false
next(x::Number, state) = (x, true)
done(::Number, state) = state
isempty(::Number) = false
in(x::Number, y::Number) = x == y
length(::Number) = 1
eltype{T<:Number}(::Type{T}) = T
iteratorsize{T<:Number}(::Type{T}) = HasLength()
copy(x::Number) = x

"""
    divrem(x, y)

The quotient and remainder from Euclidean division. Equivalent to `(div(x,y), rem(x,y))` or
`(x÷y, x%y)`.

```jldoctest
julia> divrem(3,7)
(0,3)

julia> divrem(7,3)
(2,1)
```
"""
divrem(x,y) = (div(x,y),rem(x,y))

"""
    fldmod(x, y)

The floored quotient and modulus after division. Equivalent to `(fld(x,y), mod(x,y))`.
"""
fldmod(x,y) = (fld(x,y),mod(x,y))
signbit(x::Real) = x < 0

"""
    sign(x)

Return zero if `x==0` and ``x/|x|`` otherwise (i.e., ±1 for real `x`).
"""
sign(x::Number) = x == 0 ? x/abs(one(x)) : x/abs(x)
sign(x::Real) = ifelse(x < 0, oftype(x,-1), ifelse(x > 0, one(x), x))
sign(x::Unsigned) = ifelse(x > 0, one(x), x)
abs(x::Real) = ifelse(signbit(x), -x, x)
abs2(x::Real) = x*x
flipsign(x::Real, y::Real) = ifelse(signbit(y), -x, x)
copysign(x::Real, y::Real) = ifelse(signbit(x)!=signbit(y), -x, x)

conj(x::Real) = x
transpose(x::Number) = x
ctranspose(x::Number) = conj(x)
inv(x::Number) = one(x)/x
angle(z::Real) = atan2(zero(z), z)

"""
    widemul(x, y)

Multiply `x` and `y`, giving the result as a larger type.

```jldoctest
julia> widemul(Float32(3.), 4.)
1.200000000000000000000000000000000000000000000000000000000000000000000000000000e+01
```
"""
widemul(x::Number, y::Number) = widen(x)*widen(y)

map(f, x::Number, ys::Number...) = f(x, ys...)

zero(x::Number) = oftype(x,0)
zero{T<:Number}(::Type{T}) = convert(T,0)
one(x::Number)  = oftype(x,1)
one{T<:Number}(::Type{T}) = convert(T,1)

_default_type(::Type{Number}) = Int

"""
    factorial(n)

Factorial of `n`.  If `n` is an `Integer`, the factorial is computed as an
integer (promoted to at least 64 bits).  Note that this may overflow if `n` is not small,
but you can use `factorial(big(n))` to compute the result exactly in arbitrary precision.
If `n` is not an `Integer`, `factorial(n)` is equivalent to [`gamma(n+1)`](@ref).
"""
factorial(x::Number) = gamma(x + 1) # fallback for x not Integer

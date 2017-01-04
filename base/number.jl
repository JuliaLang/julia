# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic operations on numbers ##
"""
    isinteger(x) -> Bool

Test whether `x` or all its elements are numerically equal to some integer.

```jldoctest
julia> isinteger(4.0)
true

julia> isinteger([1; 2; 5.5])
false
```
"""
isinteger(x::Integer) = true

"""
    iszero(x)

Return `true` if `x == zero(x)`; if `x` is an array, this checks whether
all of the elements of `x` are zero.
"""
iszero(x) = x == zero(x) # fallback method

size(x::Number) = ()
size(x::Number,d) = convert(Int,d)<1 ? throw(BoundsError()) : 1
indices(x::Number) = ()
indices(x::Number,d) = convert(Int,d)<1 ? throw(BoundsError()) : OneTo(1)
eltype{T<:Number}(::Type{T}) = T
ndims(x::Number) = 0
ndims{T<:Number}(::Type{T}) = 0
length(x::Number) = 1
endof(x::Number) = 1
iteratorsize{T<:Number}(::Type{T}) = HasShape()

getindex(x::Number) = x
function getindex(x::Number, i::Integer)
    @_inline_meta
    @boundscheck i == 1 || throw(BoundsError())
    x
end
function getindex(x::Number, I::Integer...)
    @_inline_meta
    @boundscheck all([i == 1 for i in I]) || throw(BoundsError())
    x
end
getindex(x::Number, I::Real...) = getindex(x, to_indexes(I...)...)
first(x::Number) = x
last(x::Number) = x
copy(x::Number) = x  # some code treats numbers as collection-like

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

"""
    abs2(x)

Squared absolute value of `x`.
"""
abs2(x::Real) = x*x

"""
    flipsign(x, y)

Return `x` with its sign flipped if `y` is negative. For example `abs(x) = flipsign(x,x)`.
"""
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

start(x::Number) = false
next(x::Number, state) = (x, true)
done(x::Number, state) = state
isempty(x::Number) = false
in(x::Number, y::Number) = x == y

map(f, x::Number, ys::Number...) = f(x, ys...)

"""
    zero(x)

Get the additive identity element for the type of `x` (`x` can also specify the type itself).
"""
zero(x::Number) = oftype(x,0)
zero{T<:Number}(::Type{T}) = convert(T,0)

"""
    one(x)

Get the multiplicative identity element for the type of `x` (`x` can also specify the type
itself). For matrices, returns an identity matrix of the appropriate size and type.
"""
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

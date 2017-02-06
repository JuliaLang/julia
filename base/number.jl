# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic operations on numbers ##
"""
    isinteger(x) -> Bool

Test whether `x` is numerically equal to some integer.

```jldoctest
julia> isinteger(4.0)
true
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
first(x::Number) = x
last(x::Number) = x
copy(x::Number) = x  # some code treats numbers as collection-like

"""
    divrem(x, y)

The quotient and remainder from Euclidean division. Equivalent to `(div(x,y), rem(x,y))` or
`(x÷y, x%y)`.

```jldoctest
julia> divrem(3,7)
(0, 3)

julia> divrem(7,3)
(2, 1)
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
sign(x::Number) = x == 0 ? x/abs(oneunit(x)) : x/abs(x)
sign(x::Real) = ifelse(x < 0, oftype(one(x),-1), ifelse(x > 0, one(x), typeof(one(x))(x)))
sign(x::Unsigned) = ifelse(x > 0, one(x), oftype(one(x),0))
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
    one(T::type)

Return a multiplicative identity for `x`: a value such that
`one(x)*x == x*one(x) == x`.  Alternatively `one(T)` can
take a type `T`, in which case `one` returns a multiplicative
identity for any `x` of type `T`.

If possible, `one(x)` returns a value of the same type as `x`,
and `one(T)` returns a value of type `T`.  However, this may
not be the case for types representing dimensionful quantities
(e.g. time in days), since the multiplicative
identity must be dimensionless.  In that case, `one(x)`
should return an identity value of the same precision
(and shape, for matrices) as `x`.

If you want a quantity that is of the same type as `x`, or of type `T`,
even if `x` is dimensionful, use [`oneunit`](@ref) instead.
```jldoctest
julia> one(3.7)
1.0

julia> one(Int)
1

julia> one(Dates.Day(1))
1
```
"""
one{T<:Number}(::Type{T}) = convert(T,1)
one{T<:Number}(x::T) = one(T)
# note that convert(T, 1) should throw an error if T is dimensionful,
# so this fallback definition should be okay.

"""
    oneunit(x::T)
    oneunit(T::Type)

Returns `T(one(x))`, where `T` is either the type of the argument or
(if a type is passed) the argument.  This differs from [`one`](@ref) for
dimensionful quantities: `one` is dimensionless (a multiplicative identity)
while `oneunit` is dimensionful (of the same type as `x`, or of type `T`).

```jldoctest
julia> oneunit(3.7)
1.0

julia> oneunit(Dates.Day)
1 day
```
"""
oneunit{T}(x::T) = T(one(x))
oneunit{T}(::Type{T}) = T(one(T))

_default_type(::Type{Number}) = Int

"""
    factorial(n)

Factorial of `n`.  If `n` is an `Integer`, the factorial is computed as an
integer (promoted to at least 64 bits).  Note that this may overflow if `n` is not small,
but you can use `factorial(big(n))` to compute the result exactly in arbitrary precision.
If `n` is not an `Integer`, `factorial(n)` is equivalent to [`gamma(n+1)`](@ref).
"""
factorial(x::Number) = gamma(x + 1) # fallback for x not Integer

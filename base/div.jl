# This file is a part of Julia. License is MIT: https://julialang.org/license

# Div is truncating by default

"""
    div(x, y, r::RoundingMode=RoundToZero)

The quotient from Euclidean division. Computes x/y, rounded to an integer according
to the rounding mode `r`. In other words, the quantity

    round(x/y,r)

without any intermediate rounding.

See also: [`fld`](@ref), [`cld`](@ref) which are special cases of this function

# Examples:
```jldoctest
julia> div(4, 3, RoundDown) # Matches fld(4, 3)
1
julia> div(4, 3, RoundUp) # Matches cld(4, 3)
2
julia> div(5, 2, RoundNearest)
2
julia> div(5, 2, RoundNearestTiesAway)
3
julia> div(-5, 2, RoundNearest)
-2
julia> div(-5, 2, RoundNearestTiesAway)
-3
julia> div(-5, 2, RoundNearestTiesUp)
-2
```
"""
div(x, y, r::RoundingMode)

div(a, b) = div(a, b, RoundToZero)

"""
    rem(x, y, r::RoundingMode=RoundToZero)

Compute the remainder of `x` after integer division by `y`, with the quotient rounded
according to the rounding mode `r`. In other words, the quantity

    x - y*round(x/y,r)

without any intermediate rounding.

- if `r == RoundNearest`, then the result is exact, and in the interval
  ``[-|y|/2, |y|/2]``. See also [`RoundNearest`](@ref).

- if `r == RoundToZero` (default), then the result is exact, and in the interval
  ``[0, |y|)`` if `x` is positive, or ``(-|y|, 0]`` otherwise. See also [`RoundToZero`](@ref).

- if `r == RoundDown`, then the result is in the interval ``[0, y)`` if `y` is positive, or
  ``(y, 0]`` otherwise. The result may not be exact if `x` and `y` have different signs, and
  `abs(x) < abs(y)`. See also [`RoundDown`](@ref).

- if `r == RoundUp`, then the result is in the interval `(-y,0]` if `y` is positive, or
  `[0,-y)` otherwise. The result may not be exact if `x` and `y` have the same sign, and
  `abs(x) < abs(y)`. See also [`RoundUp`](@ref).

"""
rem(x, y, r::RoundingMode)

# TODO: Make these primitive and have the two-argument version call these
rem(x, y, ::RoundingMode{:ToZero}) = rem(x,y)
rem(x, y, ::RoundingMode{:Down}) = mod(x,y)
rem(x, y, ::RoundingMode{:Up}) = mod(x,-y)

"""
    fld(x, y)

Largest integer less than or equal to `x/y`. Equivalent to `div(x, y, RoundDown)`.

See also: [`div`](@ref)

# Examples
```jldoctest
julia> fld(7.3,5.5)
1.0
```
"""
fld(a, b) = div(a, b, RoundDown)

"""
    cld(x, y)

Smallest integer larger than or equal to `x/y`. Equivalent to `div(x, y, RoundUp)`.

See also: [`div`](@ref)

# Examples
```jldoctest
julia> cld(5.5,2.2)
3.0
```
"""
cld(a, b) = div(a, b, RoundUp)

# divrem
"""
    divrem(x, y, r::RoundingMode=RoundToZero)

The quotient and remainder from Euclidean division.
Equivalent to `(div(x,y,r), rem(x,y,r))`. Equivalently, with the the default
value of `r`, this call is equivalent to `(x÷y, x%y)`.

# Examples
```jldoctest
julia> divrem(3,7)
(0, 3)

julia> divrem(7,3)
(2, 1)
```
"""
divrem(x, y) = divrem(x, y, RoundToZero)
function divrem(a, b, r::RoundingMode)
    if r == RoundToZero
        # For compat. Remove in 2.0.
        (div(a, b), rem(a, b))
    elseif r === RoundDown
        # For compat. Remove in 2.0.
        (fld(a, b), mod(a, b))
    else
        (div(a, b, r), rem(a, b, r))
    end
end
function divrem(x::Integer, y::Integer, rnd::typeof(RoundNearest))
    (q, r) = divrem(x, y)
    if x >= 0
        if y >= 0
            r >=        (y÷2) + (isodd(y) | iseven(q)) ? (q+true, r-y) : (q, r)
        else
            r >=       -(y÷2) + (isodd(y) | iseven(q)) ? (q-true, r+y) : (q, r)
        end
    else
        if y >= 0
            r <= -signed(y÷2) - (isodd(y) | iseven(q)) ? (q-true, r+y) : (q, r)
        else
            r <=        (y÷2) - (isodd(y) | iseven(q)) ? (q+true, r-y) : (q, r)
        end
    end
end
function divrem(x::Integer, y::Integer, rnd:: typeof(RoundNearestTiesAway))
    (q, r) = divrem(x, y)
    if x >= 0
        if y >= 0
            r >=        (y÷2) + isodd(y) ? (q+true, r-y) : (q, r)
        else
            r >=       -(y÷2) + isodd(y) ? (q-true, r+y) : (q, r)
        end
    else
        if y >= 0
            r <= -signed(y÷2) - isodd(y) ? (q-true, r+y) : (q, r)
        else
            r <=        (y÷2) - isodd(y) ? (q+true, r-y) : (q, r)
        end
    end
end
function divrem(x::Integer, y::Integer, rnd::typeof(RoundNearestTiesUp))
    (q, r) = divrem(x, y)
    if x >= 0
        if y >= 0
            r >=        (y÷2) + isodd(y) ? (q+true, r-y) : (q, r)
        else
            r >=       -(y÷2) + true     ? (q-true, r+y) : (q, r)
        end
    else
        if y >= 0
            r <= -signed(y÷2) - true     ? (q-true, r+y) : (q, r)
        else
            r <=        (y÷2) - isodd(y) ? (q+true, r-y) : (q, r)
        end
    end
end

"""
    fldmod(x, y)

The floored quotient and modulus after division. A convenience wrapper for
`divrem(x, y, RoundDown)`. Equivalent to `(fld(x,y), mod(x,y))`.
"""
fldmod(x,y) = divrem(x, y, RoundDown)

# We definite generic rounding methods for other rounding modes in terms of
# RoundToZero.
function div(x::Signed, y::Unsigned, ::typeof(RoundDown))
    (q, r) = divrem(x, y)
    q - (signbit(x) & (r != 0))
end
function div(x::Unsigned, y::Signed, ::typeof(RoundDown))
    (q, r) = divrem(x, y)
    q - (signbit(y) & (r != 0))
end

function div(x::Signed, y::Unsigned, ::typeof(RoundUp))
    (q, r) = divrem(x, y)
    q + (!signbit(x) & (r != 0))
end
function div(x::Unsigned, y::Signed, ::typeof(RoundUp))
    (q, r) = divrem(x, y)
    q + (!signbit(y) & (r != 0))
end

function div(x::Integer, y::Integer, rnd::Union{typeof(RoundNearest),
                                              typeof(RoundNearestTiesAway),
                                              typeof(RoundNearestTiesUp)})
    divrem(x,y,rnd)[1]
end

# For bootstrapping purposes, we define div for integers directly. Provide the
# generic signature also
div(a::T, b::T, ::typeof(RoundToZero)) where {T<:Union{BitSigned, BitUnsigned64}} = div(a, b)
div(a::Bool, b::Bool, r::RoundingMode) = div(a, b)
# Prevent ambiguities
for rm in (RoundUp, RoundDown, RoundToZero)
    @eval div(a::Bool, b::Bool, r::$(typeof(rm))) = div(a, b)
end
function div(x::Bool, y::Bool, rnd::Union{typeof(RoundNearest),
                                        typeof(RoundNearestTiesAway),
                                        typeof(RoundNearestTiesUp)})
    div(x, y)
end
fld(a::T, b::T) where {T<:Union{Integer,AbstractFloat}} = div(a, b, RoundDown)
cld(a::T, b::T) where {T<:Union{Integer,AbstractFloat}} = div(a, b, RoundUp)
div(a::Int128, b::Int128, ::typeof(RoundToZero)) = div(a, b)
div(a::UInt128, b::UInt128, ::typeof(RoundToZero)) = div(a, b)
rem(a::Int128, b::Int128, ::typeof(RoundToZero)) = rem(a, b)
rem(a::UInt128, b::UInt128, ::typeof(RoundToZero)) = rem(a, b)

# These are kept for compatibility with external packages overriding fld/cld.
# In 2.0, packages should extend div(a,b,r) instead, in which case, these can
# be removed.
fld(x::Real, y::Real) = div(promote(x,y)..., RoundDown)
cld(x::Real, y::Real) = div(promote(x,y)..., RoundUp)
fld(x::Signed, y::Unsigned) = div(x, y, RoundDown)
fld(x::Unsigned, y::Signed) = div(x, y, RoundDown)
cld(x::Signed, y::Unsigned) = div(x, y, RoundUp)
cld(x::Unsigned, y::Signed) = div(x, y, RoundUp)
fld(x::T, y::T) where {T<:Real} = throw(MethodError(div, (x, y, RoundDown)))
cld(x::T, y::T) where {T<:Real} = throw(MethodError(div, (x, y, RoundUp)))

# Promotion
function div(x::Real, y::Real, r::RoundingMode)
    typeof(x) === typeof(y) && throw(MethodError(div, (x, y, r)))
    if r == RoundToZero
        # For compat. Remove in 2.0.
        div(promote(x, y)...)
    else
        div(promote(x, y)..., r)
    end
end

# Integers
# fld(x,y) == div(x,y) - ((x>=0) != (y>=0) && rem(x,y) != 0 ? 1 : 0)
div(x::T, y::T, ::typeof(RoundDown)) where {T<:Unsigned} = div(x,y)
function div(x::T, y::T, ::typeof(RoundDown)) where T<:Integer
    d = div(x, y, RoundToZero)
    return d - (signbit(x ⊻ y) & (d * y != x))
end

# cld(x,y) = div(x,y) + ((x>0) == (y>0) && rem(x,y) != 0 ? 1 : 0)
function div(x::T, y::T, ::typeof(RoundUp)) where T<:Unsigned
    d = div(x, y, RoundToZero)
    return d + (d * y != x)
end
function div(x::T, y::T, ::typeof(RoundUp)) where T<:Integer
    d = div(x, y, RoundToZero)
    return d + (((x > 0) == (y > 0)) & (d * y != x))
end

# Real
# NOTE: C89 fmod() and x87 FPREM implicitly provide truncating float division,
# so it is used here as the basis of float div().
div(x::T, y::T, r::RoundingMode) where {T<:AbstractFloat} = convert(T,round((x-rem(x,y,r))/y))
rem(x::T, y::T, ::typeof(RoundUp)) where {T<:AbstractFloat} = convert(T,x-y*ceil(x/y))

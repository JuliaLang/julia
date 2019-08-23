# Div is truncating by default
div(a, b) = div(a, b, RoundToZero)

"""
    fld(x, y)

Largest integer less than or equal to `x/y`.

# Examples
```jldoctest
julia> fld(7.3,5.5)
1.0
```
"""
fld(a, b) = div(a, b, RoundDown)

"""
    cld(x, y)

Smallest integer larger than or equal to `x/y`.

# Examples
```jldoctest
julia> cld(5.5,2.2)
3.0
```
"""
cld(a, b) = div(a, b, RoundUp)

# We definite generic rounding methods for other rounding modes in terms of
# RoundToZero.
div(x::Signed, y::Unsigned, ::typeof(RoundDown)) = div(x, y, RoundToZero) - (signbit(x) & (rem(x, y) != 0))
div(x::Unsigned, y::Signed, ::typeof(RoundDown)) = div(x, y, RoundToZero) - (signbit(y) & (rem(x, y) != 0))

div(x::Signed, y::Unsigned, ::typeof(RoundUp)) = div(x, y, RoundToZero) + (!signbit(x) & (rem(x, y) != 0))
div(x::Unsigned, y::Signed, ::typeof(RoundUp)) = div(x, y, RoundToZero) + (!signbit(y) & (rem(x, y) != 0))

# For bootstrapping purposes, we define div for integers directly. Provide the
# generic signature also
div(a::T, b::T, ::typeof(RoundToZero)) where {T<:Union{BitSigned, BitUnsigned64}} = div(a, b)
div(a::Bool, b::Bool, r::RoundingMode) = div(a, b)

# For compatibility
fld(a::T, b::T) where {T<:Integer} = div(a, b, RoundDown)
cld(a::T, b::T) where {T<:Integer} = div(a, b, RoundDown)

# Promotion
div(x::Real, y::Real, r::RoundingMode) = div(promote(x, y)..., r)

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
div(x::T, y::T, ::typeof(RoundDown)) where {T<:Real} = convert(T,round((x-mod(x,y))/y))

div(x::T, y::T, ::typeof(RoundUp)) where {T<:Real} = convert(T,round((x-modCeil(x,y))/y))
#rem(x::T, y::T) where {T<:Real} = convert(T,x-y*trunc(x/y))
#mod(x::T, y::T) where {T<:Real} = convert(T,x-y*floor(x/y))
modCeil(x::T, y::T) where {T<:Real} = convert(T,x-y*ceil(x/y))

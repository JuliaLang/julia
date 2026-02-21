# This file is a part of Julia. License is MIT: https://julialang.org/license

# Div is truncating by default

"""
    div(x, y, r::RoundingMode=RoundToZero)

The quotient from Euclidean (integer) division. Computes `x / y`, rounded to
an integer according to the rounding mode `r`. In other words, the quantity

    round(x / y, r)

without any intermediate rounding.

!!! compat "Julia 1.4"
    The three-argument method taking a `RoundingMode` requires Julia 1.4 or later.

See also [`fld`](@ref) and [`cld`](@ref), which are special cases of this function.

!!! compat "Julia 1.9"
    `RoundFromZero` requires at least Julia 1.9.

# Examples:
```jldoctest
julia> div(4, 3, RoundToZero) # Matches div(4, 3)
1
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
julia> div(4, 3, RoundFromZero)
2
julia> div(-4, 3, RoundFromZero)
-2
```
!!! note "Floating-point numbers"
    Accurate results for floating-point arguments are only guaranteed when the
    mathematical value ``\\frac{x}{y}`` is within the range of exactly representable
    integers for the given floating-point type, that is, when `eps(x/y) ≤ 1`, or
    in other words, given `a = div(x, y)`, when `abs(a) < maxintfloat(a)`.

    Because `div(x, y)` implements strict truncated rounding based on the quotient
    and remainder of the Euclidean division, and because the binary representation
    of floating-point numbers (in most cases) only approximates the decimal
    representation we use, unintuitive situations can arise. For example:
    ```jldoctest
    julia> div(6.0, 0.1)
    59.0
    julia> 6.0 / 0.1
    60.0
    julia> 6.0 / big(0.1)
    59.99999999999999666933092612453056361837965690217069245739573412231113406246995
    ```
    What is happening here is that the binary representation of the `Float64`
    number written as `0.1` is slightly larger than the numerical value ``0.1``
    (just like `0.3333333333333333` is less than ``1/3`` in decimal), while `6.0`
    represents the number ``6`` precisely. Therefore the mathematical result of
    `6.0` divided by (the `Float64` representation of) `0.1` is slightly less
    than ``60``. The result of the floating-point division is rounded to precisely
    `60.0`, but `div(6.0, 0.1, RoundToZero)` takes account of the quotient and
    (here non-zero) remainder of the Euclidean division, so the result is `59.0`.

    See also [`rem`](@ref), [`divrem`](@ref).
"""
div(x, y, r::RoundingMode)

div(a, b) = div(a, b, RoundToZero)

"""
    rem(x, y, r::RoundingMode=RoundToZero)

Compute the remainder of `x` after integer division by `y`, with the quotient rounded
according to the rounding mode `r`. In other words, the quantity

    x - y * round(x / y, r)

without any intermediate rounding.

- if `r == RoundNearest`, then the result is exact, and in the interval
  ``[-|y| / 2, |y| / 2]``. See also [`RoundNearest`](@ref).

- if `r == RoundToZero` (default), then the result is exact, and in the interval
  ``[0, |y|)`` if `x` is positive, or ``(-|y|, 0]`` otherwise. See also [`RoundToZero`](@ref).

- if `r == RoundDown`, then the result is in the interval ``[0, y)`` if `y` is positive, or
  ``(y, 0]`` otherwise. The result may not be exact if `x` and `y` have different signs, and
  `abs(x) < abs(y)`. See also [`RoundDown`](@ref).

- if `r == RoundUp`, then the result is in the interval ``(-y, 0]`` if `y` is positive, or
  ``[0, -y)`` otherwise. The result may not be exact if `x` and `y` have the same sign, and
  `abs(x) < abs(y)`. See also [`RoundUp`](@ref).

- if `r == RoundFromZero`, then the result is in the interval ``(-y, 0]`` if `y` is positive, or
  ``[0, -y)`` otherwise. The result may not be exact if `x` and `y` have the same sign, and
  `abs(x) < abs(y)`. See also [`RoundFromZero`](@ref).

!!! compat "Julia 1.9"
    `RoundFromZero` requires at least Julia 1.9.

# Examples:
```jldoctest
julia> x = 9; y = 4;

julia> x % y  # same as rem(x, y)
1

julia> x ÷ y  # same as div(x, y)
2

julia> x == div(x, y) * y + rem(x, y)
true
```
"""
rem(x, y, r::RoundingMode)

# TODO: Make these primitive and have the two-argument version call these
rem(x, y, ::RoundingMode{:ToZero}) = rem(x, y)
rem(x, y, ::RoundingMode{:Down}) = mod(x, y)
rem(x, y, ::RoundingMode{:Up}) = mod(x, -y)
rem(x, y, r::RoundingMode{:Nearest}) = x - y * div(x, y, r)
rem(x::Integer, y::Integer, r::RoundingMode{:Nearest}) = divrem(x, y, r)[2]

function rem(x, y, ::typeof(RoundFromZero))
    signbit(x) == signbit(y) ? rem(x, y, RoundUp) : rem(x, y, RoundDown)
end

"""
    fld(x, y)

Largest integer less than or equal to `x / y`. Equivalent to `div(x, y, RoundDown)`.

See also [`div`](@ref), [`cld`](@ref), [`fld1`](@ref).

# Examples
```jldoctest
julia> fld(7.3, 5.5)
1.0

julia> fld.(-5:5, 3)'
1×11 adjoint(::Vector{Int64}) with eltype Int64:
 -2  -2  -1  -1  -1  0  0  0  1  1  1
```
!!! note "Floating-point numbers"
    Accurate results for floating-point arguments are only guaranteed when the
    mathematical value ``\\frac{x}{y}`` is within the range of exactly representable
    integers for the given floating-point type, that is, when `eps(x/y) ≤ 1`, or
    in other words, given `a = fld(x, y)`, when `abs(a) < maxintfloat(a)`.

    Because `fld(x, y)` implements strict floored rounding based on the quotient
    and remainder of the Euclidean division, and because the binary representation
    of floating-point numbers (in most cases) only approximates the decimal
    representation we use, unintuitive situations can arise. For example:
    ```jldoctest
    julia> fld(6.0, 0.1)
    59.0
    julia> 6.0 / 0.1
    60.0
    julia> 6.0 / big(0.1)
    59.99999999999999666933092612453056361837965690217069245739573412231113406246995
    ```
    What is happening here is that the binary representation of the `Float64`
    number written as `0.1` is slightly larger than the numerical value ``0.1``
    (just like `0.3333333333333333` is less than ``1/3`` in decimal), while `6.0`
    represents the number ``6`` precisely. Therefore the mathematical result of
    `6.0` divided by (the `Float64` representation of) `0.1` is slightly less
    than ``60``. The result of the floating-point division is rounded to precisely
    `60.0`, but `fld(6.0, 0.1)` takes account of the quotient and (here non-zero)
    remainder of the Euclidean division, so the result is `59.0`.

    See also [`rem`](@ref), [`divrem`](@ref).
"""
fld(a, b) = div(a, b, RoundDown)

"""
    cld(x, y)

Smallest integer larger than or equal to `x / y`. Equivalent to `div(x, y, RoundUp)`.

See also [`div`](@ref), [`fld`](@ref).

# Examples
```jldoctest
julia> cld(5.5, 2.2)
3.0

julia> cld.(-5:5, 3)'
1×11 adjoint(::Vector{Int64}) with eltype Int64:
 -1  -1  -1  0  0  0  1  1  1  2  2
```
!!! note "Floating-point numbers"
    Accurate results for floating-point arguments are only guaranteed when the
    mathematical value ``\\frac{x}{y}`` is within the range of exactly representable
    integers for the given floating-point type, that is, when `eps(x/y) ≤ 1`, or
    in other words, given `a = cld(x, y)`, when `abs(a) < maxintfloat(a)`.

    Because `cld(x, y)` implements strict ceiled rounding based on the quotient
    and remainder of the Euclidean division, and because the binary representation
    of floating-point numbers (in most cases) only approximates the decimal
    representation we use, unintuitive situations can arise. For example:
    ```jldoctest
    julia> cld(3.0, 0.3)
    11.0
    julia> 3.0 / 0.3
    10.0
    julia> 3.0 / big(0.3)
    10.00000000000000037007434154171886050337904945061778828900298697586147515340753
    ```
    What is happening here is that the binary representation of the `Float64`
    number written as `0.3` is slightly less than the numerical value ``0.3``
    (just like `0.3333333333333333` is less than ``1/3`` in decimal), while `3.0`
    represents the number ``3`` precisely. Therefore the mathematical result of
    `3.0` divided by (the `Float64` representation of) `0.3` is slightly larger
    than ``10``. The result of the floating-point division is rounded to precisely
    `10.0`, but `cld(3.0, 0.3)` takes account of the quotient and (here non-zero)
    remainder of the Euclidean division, so the result is `11.0`.

    See also [`rem`](@ref), [`divrem`](@ref).
"""
cld(a, b) = div(a, b, RoundUp)

# divrem
"""
    divrem(x, y, r::RoundingMode=RoundToZero)

The quotient and remainder from Euclidean division.
Equivalent to `(div(x, y, r), rem(x, y, r))`. Equivalently, with the default
value of `r`, this call is equivalent to `(x ÷ y, x % y)`.

See also [`fldmod`](@ref), [`cld`](@ref).

# Examples
```jldoctest
julia> divrem(3, 7)
(0, 3)

julia> divrem(7, 3)
(2, 1)
```
"""
divrem(x, y) = divrem(x, y, RoundToZero)


function divrem(a, b, r::RoundingMode)
    if r === RoundToZero
        # For compat. Remove in 2.0.
        (div(a, b), rem(a, b))
    elseif r === RoundDown
        # For compat. Remove in 2.0.
        (fld(a, b), mod(a, b))
    else
        (div(a, b, r), rem(a, b, r))
    end
end
# avoids calling rem for Integers-Integers (all modes),
# a - d * b not precise for Floats - AbstractFloat, AbstractIrrational.
# Rationals are still slower
function divrem(a::Integer, b::Integer, r::Union{typeof(RoundUp),
                                                typeof(RoundDown),
                                                typeof(RoundToZero)})
    if r === RoundToZero
        # For compat. Remove in 2.0.
        d = div(a, b)
        (d, a - d * b)
    elseif r === RoundDown
        # For compat. Remove in 2.0.
        d = fld(a, b)
        (d, a - d * b)
    elseif r === RoundUp
        # For compat. Remove in 2.0.
        d = div(a, b, r)
        (d, a - d * b)
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

function divrem(x, y, ::typeof(RoundFromZero))
    signbit(x) == signbit(y) ? divrem(x, y, RoundUp) : divrem(x, y, RoundDown)
end

"""
    fldmod(x, y)

The floored quotient and modulus after division. A convenience wrapper for
`divrem(x, y, RoundDown)`. Equivalent to `(fld(x, y), mod(x, y))`.

See also [`fld`](@ref), [`cld`](@ref), [`fldmod1`](@ref).
"""
fldmod(x, y) = divrem(x, y, RoundDown)

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
    divrem(x, y, rnd)[1]
end

function div(x::Integer, y::Integer, ::typeof(RoundFromZero))
    signbit(x) == signbit(y) ? div(x, y, RoundUp) : div(x, y, RoundDown)
end

# For bootstrapping purposes, we define div for integers directly. Provide the
# generic signature also
div(a::T, b::T, ::typeof(RoundToZero)) where {T<:Union{BitSigned, BitUnsigned}} = div(a, b)
div(a::Bool, b::Bool, r::RoundingMode) = div(a, b)
# Prevent ambiguities
for rm in (RoundUp, RoundDown, RoundToZero, RoundFromZero)
    @eval div(a::Bool, b::Bool, r::$(typeof(rm))) = div(a, b)
end
function div(x::Bool, y::Bool, rnd::Union{typeof(RoundNearest),
                                        typeof(RoundNearestTiesAway),
                                        typeof(RoundNearestTiesUp)})
    div(x, y)
end
fld(a::T, b::T) where {T<:Union{Integer,AbstractFloat}} = div(a, b, RoundDown)
cld(a::T, b::T) where {T<:Union{Integer,AbstractFloat}} = div(a, b, RoundUp)

# These are kept for compatibility with external packages overriding fld / cld.
# In 2.0, packages should extend div(a, b, r) instead, in which case, these can
# be removed.
fld(x::Real, y::Real) = div(promote(x, y)..., RoundDown)
cld(x::Real, y::Real) = div(promote(x, y)..., RoundUp)
fld(x::Signed, y::Unsigned) = div(x, y, RoundDown)
fld(x::Unsigned, y::Signed) = div(x, y, RoundDown)
cld(x::Signed, y::Unsigned) = div(x, y, RoundUp)
cld(x::Unsigned, y::Signed) = div(x, y, RoundUp)
fld(x::T, y::T) where {T<:Real} = throw(MethodError(div, (x, y, RoundDown)))
cld(x::T, y::T) where {T<:Real} = throw(MethodError(div, (x, y, RoundUp)))

# Promotion
function div(x::Real, y::Real, r::RoundingMode)
    typeof(x) === typeof(y) && throw(MethodError(div, (x, y, r)))
    if r === RoundToZero
        # For compat. Remove in 2.0.
        div(promote(x, y)...)
    else
        div(promote(x, y)..., r)
    end
end

# Integers
# fld(x, y) == div(x, y) - ((x >= 0) != (y >= 0) && rem(x, y) != 0 ? 1 : 0)
div(x::T, y::T, ::typeof(RoundDown)) where {T<:Unsigned} = div(x, y)
function div(x::T, y::T, ::typeof(RoundDown)) where T<:Integer
    d = div(x, y, RoundToZero)
    return d - (signbit(x ⊻ y) & (d * y != x))
end

# cld(x, y) = div(x, y) + ((x > 0) == (y > 0) && rem(x, y) != 0 ? 1 : 0)
function div(x::T, y::T, ::typeof(RoundUp)) where T<:Unsigned
    d = div(x, y, RoundToZero)
    return d + (d * y != x)
end
function div(x::T, y::T, ::typeof(RoundUp)) where T<:Integer
    d = div(x, y, RoundToZero)
    return d + (((x > 0) == (y > 0)) & (d * y != x))
end

# Floats
# NB. If eps(x/y) > 1, x/y rounds to an unsafe integer which can't be floored
# or ceiled if it needs to since x/y ± 1 is not representable.
# @see https://github.com/JuliaLang/julia/issues/49450#issuecomment-3694946121
div(x::T, y::T, r::RoundingMode) where {T<:AbstractFloat} = round(x / y - rem(x, y, r) / y)

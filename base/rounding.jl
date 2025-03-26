# This file is a part of Julia. License is MIT: https://julialang.org/license

module Rounding

let fenv_consts = Array{Cint,1}(undef, 9)
    ccall(:jl_get_fenv_consts, Cvoid, (Ptr{Cint},), fenv_consts)
    global const JL_FE_INEXACT = fenv_consts[1]
    global const JL_FE_UNDERFLOW = fenv_consts[2]
    global const JL_FE_OVERFLOW = fenv_consts[3]
    global const JL_FE_DIVBYZERO = fenv_consts[4]
    global const JL_FE_INVALID = fenv_consts[5]

    global const JL_FE_TONEAREST = fenv_consts[6]
    global const JL_FE_UPWARD = fenv_consts[7]
    global const JL_FE_DOWNWARD = fenv_consts[8]
    global const JL_FE_TOWARDZERO = fenv_consts[9]
end

export
    RoundingMode, RoundNearest, RoundToZero, RoundUp, RoundDown, RoundFromZero,
    RoundNearestTiesAway, RoundNearestTiesUp,
    rounding, setrounding,
    get_zero_subnormals, set_zero_subnormals

## rounding modes ##
"""
    RoundingMode

A type used for controlling the rounding mode of floating point operations (via
[`rounding`](@ref)/[`setrounding`](@ref) functions), or as
optional arguments for rounding to the nearest integer (via the [`round`](@ref)
function).

Currently supported rounding modes are:

- [`RoundNearest`](@ref) (default)
- [`RoundNearestTiesAway`](@ref)
- [`RoundNearestTiesUp`](@ref)
- [`RoundToZero`](@ref)
- [`RoundFromZero`](@ref)
- [`RoundUp`](@ref)
- [`RoundDown`](@ref)

!!! compat "Julia 1.9"
    `RoundFromZero` requires at least Julia 1.9. Prior versions support
    `RoundFromZero` for `BigFloat`s only.
"""
struct RoundingMode{T} end

"""
    RoundNearest

The default rounding mode. Rounds to the nearest integer, with ties (fractional values of
0.5) being rounded to the nearest even integer.
"""
const RoundNearest = RoundingMode{:Nearest}()

"""
    RoundToZero

[`round`](@ref) using this rounding mode is an alias for [`trunc`](@ref).
"""
const RoundToZero = RoundingMode{:ToZero}()

"""
    RoundUp

[`round`](@ref) using this rounding mode is an alias for [`ceil`](@ref).
"""
const RoundUp = RoundingMode{:Up}()

"""
    RoundDown

[`round`](@ref) using this rounding mode is an alias for [`floor`](@ref).
"""
const RoundDown = RoundingMode{:Down}()

"""
    RoundFromZero

Rounds away from zero.

!!! compat "Julia 1.9"
    `RoundFromZero` requires at least Julia 1.9. Prior versions support
    `RoundFromZero` for `BigFloat`s only.

# Examples
```jldoctest
julia> BigFloat("1.0000000000000001", 5, RoundFromZero)
1.06
```
"""
const RoundFromZero = RoundingMode{:FromZero}()

"""
    RoundNearestTiesAway

Rounds to nearest integer, with ties rounded away from zero (C/C++
[`round`](@ref) behaviour).
"""
const RoundNearestTiesAway = RoundingMode{:NearestTiesAway}()

"""
    RoundNearestTiesUp

Rounds to nearest integer, with ties rounded toward positive infinity (Java/JavaScript
[`round`](@ref) behaviour).
"""
const RoundNearestTiesUp = RoundingMode{:NearestTiesUp}()

# Rounding mode predicates. TODO: better names

# Overload these for other rounding modes
rounds_to_nearest(::RoundingMode) = false
rounds_to_nearest(::RoundingMode{:Nearest}) = true
rounds_to_nearest(::RoundingMode{:NearestTiesUp}) = true
rounds_to_nearest(::RoundingMode{:NearestTiesAway}) = true
rounds_away_from_zero(::RoundingMode{:Up},   sign_bit::Bool) = !sign_bit
rounds_away_from_zero(::RoundingMode{:Down}, sign_bit::Bool) = sign_bit
rounds_away_from_zero(::RoundingMode{:FromZero}, ::Bool) = true
rounds_away_from_zero(::RoundingMode{:ToZero},   ::Bool) = false
tie_breaker_is_to_even(::RoundingMode{:Nearest}) = true
tie_breaker_is_to_even(::RoundingMode{:NearestTiesUp}) = false
tie_breaker_is_to_even(::RoundingMode{:NearestTiesAway}) = false
tie_breaker_rounds_away_from_zero(::RoundingMode{:NearestTiesUp}, sign_bit::Bool) = !sign_bit
tie_breaker_rounds_away_from_zero(::RoundingMode{:NearestTiesAway},       ::Bool) = true

rounds_to_nearest(t::Tuple{Any,Bool}) = rounds_to_nearest(first(t))
rounds_away_from_zero(t::Tuple{Any,Bool}) = rounds_away_from_zero(t...)
tie_breaker_is_to_even(t::Tuple{Any,Bool}) = tie_breaker_is_to_even(first(t))
tie_breaker_rounds_away_from_zero(t::Tuple{Any,Bool}) = tie_breaker_rounds_away_from_zero(t...)

struct FinalBit end
struct RoundBit end
struct StickyBit end

function correct_rounding_requires_increment(x, rounding_mode, sign_bit::Bool)
    r = (rounding_mode, sign_bit)
    f = let y = x
        (z::Union{FinalBit,RoundBit,StickyBit}) -> y(z)::Bool
    end
    if rounds_to_nearest(r)
        if f(RoundBit())
            if f(StickyBit())
                true
            else
                if tie_breaker_is_to_even(r)
                    f(FinalBit())
                else
                    tie_breaker_rounds_away_from_zero(r)::Bool
                end
            end
        else
            false
        end
    else
        if rounds_away_from_zero(r)
            if f(RoundBit())
                true
            else
                f(StickyBit())
            end
        else
            false
        end
    end::Bool
end

to_fenv(::RoundingMode{:Nearest}) = JL_FE_TONEAREST
to_fenv(::RoundingMode{:ToZero}) = JL_FE_TOWARDZERO
to_fenv(::RoundingMode{:Up}) = JL_FE_UPWARD
to_fenv(::RoundingMode{:Down}) = JL_FE_DOWNWARD

function from_fenv(r::Integer)
    if r == JL_FE_TONEAREST
        return RoundNearest
    elseif r == JL_FE_DOWNWARD
        return RoundDown
    elseif r == JL_FE_UPWARD
        return RoundUp
    elseif r == JL_FE_TOWARDZERO
        return RoundToZero
    else
        throw(ArgumentError("invalid rounding mode code: $r"))
    end
end

"""
    setrounding(T, mode)

Set the rounding mode of floating point type `T`, controlling the rounding of basic
arithmetic functions ([`+`](@ref), [`-`](@ref), [`*`](@ref),
[`/`](@ref) and [`sqrt`](@ref)) and type conversion. Other numerical
functions may give incorrect or invalid values when using rounding modes other than the
default [`RoundNearest`](@ref).

Note that this is currently only supported for `T == BigFloat`.

!!! warning

    This function is not thread-safe. It will affect code running on all threads, but
    its behavior is undefined if called concurrently with computations that use the
    setting.
"""
setrounding(T::Type, mode)

"""
    rounding(T)

Get the current floating point rounding mode for type `T`, controlling the rounding of basic
arithmetic functions ([`+`](@ref), [`-`](@ref), [`*`](@ref), [`/`](@ref)
and [`sqrt`](@ref)) and type conversion.

See [`RoundingMode`](@ref) for available modes.
"""
:rounding

setrounding_raw(::Type{<:Union{Float32,Float64}}, i::Integer) = ccall(:jl_set_fenv_rounding, Int32, (Int32,), i)
rounding_raw(::Type{<:Union{Float32,Float64}}) = ccall(:jl_get_fenv_rounding, Int32, ())

rounding(::Type{T}) where {T<:Union{Float32,Float64}} = from_fenv(rounding_raw(T))

"""
    setrounding(f::Function, T, mode)

Change the rounding mode of floating point type `T` for the duration of `f`. It is logically
equivalent to:

    old = rounding(T)
    setrounding(T, mode)
    f()
    setrounding(T, old)

See [`RoundingMode`](@ref) for available rounding modes.
"""
function setrounding(f::Function, ::Type{T}, rounding::RoundingMode) where T
    old_rounding_raw = rounding_raw(T)
    setrounding(T,rounding)
    try
        return f()
    finally
        setrounding_raw(T,old_rounding_raw)
    end
end
function setrounding_raw(f::Function, ::Type{T}, rounding) where T
    old_rounding_raw = rounding_raw(T)
    setrounding_raw(T,rounding)
    try
        return f()
    finally
        setrounding_raw(T,old_rounding_raw)
    end
end


# Should be equivalent to:
#   setrounding(Float64,r) do
#       convert(T,x)
#   end
# but explicit checks are currently quicker (~20x).
# Assumes conversion is performed by rounding to nearest value.

# To avoid ambiguous dispatch with methods in mpfr.jl:
(::Type{T})(x::Real, r::RoundingMode) where {T<:AbstractFloat} = _convert_rounding(T,x,r)::T

_convert_rounding(::Type{T}, x::Real, r::RoundingMode{:Nearest}) where {T<:AbstractFloat} = convert(T,x)::T
function _convert_rounding(::Type{T}, x::Real, r::RoundingMode{:Down}) where T<:AbstractFloat
    y = convert(T,x)::T
    y > x ? prevfloat(y) : y
end
function _convert_rounding(::Type{T}, x::Real, r::RoundingMode{:Up}) where T<:AbstractFloat
    y = convert(T,x)::T
    y < x ? nextfloat(y) : y
end
function _convert_rounding(::Type{T}, x::Real, r::RoundingMode{:ToZero}) where T<:AbstractFloat
    y = convert(T,x)::T
    if x > 0.0
        y > x ? prevfloat(y) : y
    else
        y < x ? nextfloat(y) : y
    end
end
function _convert_rounding(::Type{T}, x::Real, r::RoundingMode{:FromZero}) where T<:AbstractFloat
    y = convert(T,x)::T
    if x < 0.0
        y > x ? prevfloat(y) : y
    else
        y < x ? nextfloat(y) : y
    end
end


# Default definitions

"""
    set_zero_subnormals(yes::Bool)::Bool

If `yes` is `false`, subsequent floating-point operations follow rules for IEEE arithmetic
on subnormal values ("denormals"). Otherwise, floating-point operations are permitted (but
not required) to convert subnormal inputs or outputs to zero. Returns `true` unless
`yes==true` but the hardware does not support zeroing of subnormal numbers.

`set_zero_subnormals(true)` can speed up some computations on some hardware. However, it can
break identities such as `(x-y==0) == (x==y)`.

!!! warning

    This function only affects the current thread.
"""
set_zero_subnormals(yes::Bool) = ccall(:jl_set_zero_subnormals,Int32,(Int8,),yes)==0

"""
    get_zero_subnormals()::Bool

Return `false` if operations on subnormal floating-point values ("denormals") obey rules
for IEEE arithmetic, and `true` if they might be converted to zeros.

!!! warning

    This function only affects the current thread.
"""
get_zero_subnormals() = ccall(:jl_get_zero_subnormals,Int32,())!=0

end #module
using .Rounding

"""
    round([T,] x, [r::RoundingMode])
    round(x, [r::RoundingMode]; digits::Integer=0, base = 10)
    round(x, [r::RoundingMode]; sigdigits::Integer, base = 10)

Rounds the number `x`.

Without keyword arguments, `x` is rounded to an integer value, returning a value of type
`T`, or of the same type of `x` if no `T` is provided. An [`InexactError`](@ref) will be
thrown if the value is not representable by `T`, similar to [`convert`](@ref).

If the `digits` keyword argument is provided, it rounds to the specified number of digits
after the decimal place (or before if negative), in base `base`.

If the `sigdigits` keyword argument is provided, it rounds to the specified number of
significant digits, in base `base`.

The [`RoundingMode`](@ref) `r` controls the direction of the rounding; the default is
[`RoundNearest`](@ref), which rounds to the nearest integer, with ties (fractional values
of 0.5) being rounded to the nearest even integer. Note that `round` may give incorrect
results if the global rounding mode is changed (see [`rounding`](@ref)).

When rounding to a floating point type, will round to integers representable by that type
(and Inf) rather than true integers. Inf is treated as one ulp greater than the
`floatmax(T)` for purposes of determining "nearest", similar to [`convert`](@ref).

# Examples
```jldoctest
julia> round(1.7)
2.0

julia> round(Int, 1.7)
2

julia> round(1.5)
2.0

julia> round(2.5)
2.0

julia> round(pi; digits=2)
3.14

julia> round(pi; digits=3, base=2)
3.125

julia> round(123.456; sigdigits=2)
120.0

julia> round(357.913; sigdigits=4, base=2)
352.0

julia> round(Float16, typemax(UInt128))
Inf16

julia> floor(Float16, typemax(UInt128))
Float16(6.55e4)
```

!!! note
    Rounding to specified digits in bases other than 2 can be inexact when
    operating on binary floating point numbers. For example, the [`Float64`](@ref)
    value represented by `1.15` is actually *less* than 1.15, yet will be
    rounded to 1.2. For example:

    ```jldoctest
    julia> x = 1.15
    1.15

    julia> big(1.15)
    1.149999999999999911182158029987476766109466552734375

    julia> x < 115//100
    true

    julia> round(x, digits=1)
    1.2
    ```

# Extensions

To extend `round` to new numeric types, it is typically sufficient to define `Base.round(x::NewType, r::RoundingMode)`.
"""
function round end

"""
    trunc([T,] x)
    trunc(x; digits::Integer= [, base = 10])
    trunc(x; sigdigits::Integer= [, base = 10])

`trunc(x)` returns the nearest integral value of the same type as `x` whose absolute value
is less than or equal to the absolute value of `x`.

`trunc(T, x)` converts the result to type `T`, throwing an `InexactError` if the truncated
value is not representable a `T`.

Keywords `digits`, `sigdigits` and `base` work as for [`round`](@ref).

To support `trunc` for a new type, define `Base.round(x::NewType, ::RoundingMode{:ToZero})`.

See also: [`%`](@ref rem), [`floor`](@ref), [`unsigned`](@ref), [`unsafe_trunc`](@ref).

# Examples
```jldoctest
julia> trunc(2.22)
2.0

julia> trunc(-2.22, digits=1)
-2.2

julia> trunc(Int, -2.22)
-2
```
"""
function trunc end

"""
    floor([T,] x)
    floor(x; digits::Integer= [, base = 10])
    floor(x; sigdigits::Integer= [, base = 10])

`floor(x)` returns the nearest integral value of the same type as `x` that is less than or
equal to `x`.

`floor(T, x)` converts the result to type `T`, throwing an `InexactError` if the floored
value is not representable a `T`.

Keywords `digits`, `sigdigits` and `base` work as for [`round`](@ref).

To support `floor` for a new type, define `Base.round(x::NewType, ::RoundingMode{:Down})`.
"""
function floor end

"""
    ceil([T,] x)
    ceil(x; digits::Integer= [, base = 10])
    ceil(x; sigdigits::Integer= [, base = 10])

`ceil(x)` returns the nearest integral value of the same type as `x` that is greater than or
equal to `x`.

`ceil(T, x)` converts the result to type `T`, throwing an `InexactError` if the ceiled
value is not representable as a `T`.

Keywords `digits`, `sigdigits` and `base` work as for [`round`](@ref).

To support `ceil` for a new type, define `Base.round(x::NewType, ::RoundingMode{:Up})`.
"""
function ceil end

trunc(x; kws...) = round(x, RoundToZero; kws...)
floor(x; kws...) = round(x, RoundDown; kws...)
 ceil(x; kws...) = round(x, RoundUp; kws...)
round(x; kws...) = round(x, RoundNearest; kws...)

trunc(::Type{T}, x) where T = round(T, x, RoundToZero)
floor(::Type{T}, x) where T = round(T, x, RoundDown)
 ceil(::Type{T}, x) where T = round(T, x, RoundUp)
round(::Type{T}, x) where T = round(T, x, RoundNearest)

round(::Type{T}, x, r::RoundingMode) where T = _round_convert(T, round(x, r), x, r)
_round_convert(::Type{T}, x_integer, x, r) where T = convert(T, x_integer)

round(x::Integer, r::RoundingMode) = x

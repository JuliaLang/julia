# This file is a part of Julia. License is MIT: http://julialang.org/license

module Rounding

let fenv_consts = Vector{Cint}(9)
    ccall(:jl_get_fenv_consts, Void, (Ptr{Cint},), fenv_consts)
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
- `RoundFromZero` (`BigFloat` only)
- [`RoundUp`](@ref)
- [`RoundDown`](@ref)
"""
immutable RoundingMode{T} end

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

const RoundFromZero = RoundingMode{:FromZero}() # mpfr only

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
default `RoundNearest`.

Note that this may affect other types, for instance changing the rounding mode of `Float64`
will change the rounding mode of `Float32`. See [`RoundingMode`](@ref) for
available modes.

!!! warning

    This feature is still experimental, and may give unexpected or incorrect values.
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

setrounding_raw{T<:Union{Float32,Float64}}(::Type{T},i::Integer) = ccall(:fesetround, Int32, (Int32,), i)
rounding_raw{T<:Union{Float32,Float64}}(::Type{T}) = ccall(:fegetround, Int32, ())

setrounding{T<:Union{Float32,Float64}}(::Type{T},r::RoundingMode) = setrounding_raw(T,to_fenv(r))
rounding{T<:Union{Float32,Float64}}(::Type{T}) = from_fenv(rounding_raw(T))

"""
    setrounding(f::Function, T, mode)

Change the rounding mode of floating point type `T` for the duration of `f`. It is logically
equivalent to:

    old = rounding(T)
    setrounding(T, mode)
    f()
    setrounding(T, old)

See [`RoundingMode`](@ref) for available rounding modes.

!!! warning

    This feature is still experimental, and may give unexpected or incorrect values. A
    known problem is the interaction with compiler optimisations, e.g.

        julia> setrounding(Float64,RoundDown) do
                   1.1 + 0.1
               end
        1.2000000000000002

    Here the compiler is *constant folding*, that is evaluating a known constant
    expression at compile time, however the rounding mode is only changed at runtime, so
    this is not reflected in the function result. This can be avoided by moving constants
    outside the expression, e.g.

        julia> x = 1.1; y = 0.1;

        julia> setrounding(Float64,RoundDown) do
                   x + y
               end
        1.2
"""
function setrounding{T}(f::Function, ::Type{T}, rounding::RoundingMode)
    old_rounding_raw = rounding_raw(T)
    setrounding(T,rounding)
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
(::Type{T}){T<:AbstractFloat}(x::Real,r::RoundingMode) = _convert_rounding(T,x,r)

_convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Nearest}) = convert(T,x)
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Down})
    y = convert(T,x)
    y > x ? prevfloat(y) : y
end
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:Up})
    y = convert(T,x)
    y < x ? nextfloat(y) : y
end
function _convert_rounding{T<:AbstractFloat}(::Type{T},x::Real,r::RoundingMode{:ToZero})
    y = convert(T,x)
    if x > 0.0
        y > x ? prevfloat(y) : y
    else
        y < x ? nextfloat(y) : y
    end
end

"""
    set_zero_subnormals(yes::Bool) -> Bool

If `yes` is `false`, subsequent floating-point operations follow rules for IEEE arithmetic
on subnormal values ("denormals"). Otherwise, floating-point operations are permitted (but
not required) to convert subnormal inputs or outputs to zero. Returns `true` unless
`yes==true` but the hardware does not support zeroing of subnormal numbers.

`set_zero_subnormals(true)` can speed up some computations on some hardware. However, it can
break identities such as `(x-y==0) == (x==y)`.
"""
set_zero_subnormals(yes::Bool) = ccall(:jl_set_zero_subnormals,Int32,(Int8,),yes)==0

"""
    get_zero_subnormals() -> Bool

Returns `false` if operations on subnormal floating-point values ("denormals") obey rules
for IEEE arithmetic, and `true` if they might be converted to zeros.
"""
get_zero_subnormals() = ccall(:jl_get_zero_subnormals,Int32,())!=0

end #module

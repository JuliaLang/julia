# This file is a part of Julia. License is MIT: https://julialang.org/license

## floating-point functions ##

copysign(x::Float64, y::Float64) = copysign_float(x, y)
copysign(x::Float32, y::Float32) = copysign_float(x, y)
copysign(x::Float32, y::Real) = copysign(x, Float32(y))
copysign(x::Float64, y::Real) = copysign(x, Float64(y))

flipsign(x::Float64, y::Float64) = bitcast(Float64, xor_int(bitcast(UInt64, x), and_int(bitcast(UInt64, y), 0x8000000000000000)))
flipsign(x::Float32, y::Float32) = bitcast(Float32, xor_int(bitcast(UInt32, x), and_int(bitcast(UInt32, y), 0x80000000)))
flipsign(x::Float32, y::Real) = flipsign(x, Float32(y))
flipsign(x::Float64, y::Real) = flipsign(x, Float64(y))

signbit(x::Float64) = signbit(bitcast(Int64, x))
signbit(x::Float32) = signbit(bitcast(Int32, x))
signbit(x::Float16) = signbit(bitcast(Int16, x))

"""
    maxintfloat(T=Float64)

The largest consecutive integer that is exactly represented in the given floating-point type `T`
(which defaults to `Float64`).

That is, `maxintfloat` returns the smallest positive integer `n` such that `n+1`
is *not* exactly representable in the type `T`.
"""
maxintfloat(::Type{Float64}) = 9007199254740992.
maxintfloat(::Type{Float32}) = Float32(16777216.)
maxintfloat(::Type{Float16}) = Float16(2048f0)
maxintfloat(x::T) where {T<:AbstractFloat} = maxintfloat(T)

"""
    maxintfloat(T, S)

The largest consecutive integer representable in the given floating-point type `T` that
also does not exceed the maximum integer representable by the integer type `S`.  Equivalently,
it is the minimum of `maxintfloat(T)` and [`typemax(S)`](@ref).
"""
maxintfloat(::Type{S}, ::Type{T}) where {S<:AbstractFloat, T<:Integer} = min(maxintfloat(S), S(typemax(T)))
maxintfloat() = maxintfloat(Float64)

isinteger(x::AbstractFloat) = (x - trunc(x) == 0)

"""
    round([T,] x, [r::RoundingMode])
    round(x, [digits; base = 10])

Rounds `x` to an integer value according to the provided
[`RoundingMode`](@ref), returning a value of the same type as `x`. When not
specifying a rounding mode the global mode will be used
(see [`rounding`](@ref)), which by default is round to the nearest integer
([`RoundNearest`](@ref) mode), with ties (fractional values of 0.5) being
rounded to the nearest even integer.

# Examples
```jldoctest
julia> round(1.7)
2.0

julia> round(1.5)
2.0

julia> round(2.5)
2.0
```

The optional [`RoundingMode`](@ref) argument will change how the number gets
rounded.

`round(T, x, [r::RoundingMode])` converts the result to type `T`, throwing an
[`InexactError`](@ref) if the value is not representable.

`round(x, digits)` rounds to the specified number of digits after the decimal place (or
before if negative). `round(x, digits, base = base)` rounds using a base other than 10.

# Examples
```jldoctest
julia> round(pi, 2)
3.14

julia> round(pi, 3, base = 2)
3.125
```

!!! note
    Rounding to specified digits in bases other than 2 can be inexact when
    operating on binary floating point numbers. For example, the [`Float64`](@ref)
    value represented by `1.15` is actually *less* than 1.15, yet will be
    rounded to 1.2.

    # Examples
    ```jldoctest; setup = :(using Printf)
    julia> x = 1.15
    1.15

    julia> @sprintf "%.20f" x
    "1.14999999999999991118"

    julia> x < 115//100
    true

    julia> round(x, 1)
    1.2
    ```

See also [`signif`](@ref) for rounding to significant digits.
"""
round(T::Type, x)
round(x::Real, ::RoundingMode{:ToZero}) = trunc(x)
round(x::Real, ::RoundingMode{:Up}) = ceil(x)
round(x::Real, ::RoundingMode{:Down}) = floor(x)
# C-style round
function round(x::AbstractFloat, ::RoundingMode{:NearestTiesAway})
    y = trunc(x)
    ifelse(x==y,y,trunc(2*x-y))
end
# Java-style round
function round(x::AbstractFloat, ::RoundingMode{:NearestTiesUp})
    y = floor(x)
    ifelse(x==y,y,copysign(floor(2*x-y),x))
end
round(::Type{T}, x::AbstractFloat, r::RoundingMode) where {T<:Integer} = trunc(T,round(x,r))

# adapted from Matlab File Exchange roundsd: http://www.mathworks.com/matlabcentral/fileexchange/26212
# for round, og is the power of 10 relative to the decimal point
# for signif, og is the absolute power of 10
# digits and base must be integers, x must be convertable to float

function _signif_og(x, digits, base)
    if base == 10
        e = floor(log10(abs(x)) - digits + 1.)
        og = oftype(x, exp10(abs(e)))
    elseif base == 2
        e = exponent(abs(x)) - digits + 1.
        og = oftype(x, exp2(abs(e)))
    else
        e = floor(log(base, abs(x)) - digits + 1.)
        og = oftype(x, float(base) ^ abs(e))
    end
    return og, e
end

"""
    signif(x, digits; base = 10)

Rounds (in the sense of [`round`](@ref)) `x` so that there are `digits` significant digits, under a
base `base` representation, default 10.

# Examples
```jldoctest
julia> signif(123.456, 2)
120.0

julia> signif(357.913, 4, base = 2)
352.0
```
"""
function signif(x::Real, digits::Integer; base::Integer = 10)
    digits < 1 && throw(DomainError(digits, "`digits` cannot be less than 1."))

    x = float(x)
    (x == 0 || !isfinite(x)) && return x
    og, e = _signif_og(x, digits, base)
    if e >= 0 # for numeric stability
        r = round(x/og)*og
    else
        r = round(x*og)/og
    end
    !isfinite(r) ? x : r
end

for f in (:round, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x::Real, digits::Integer; base::Integer = 10)
            x = float(x)
            og = convert(eltype(x),base)^digits
            r = ($f)(x * og) / og

            if !isfinite(r)
                if digits > 0
                    return x
                elseif x > 0
                    return $(:ceil == f ? :(convert(eltype(x), Inf)) : :(zero(x)))
                elseif x < 0
                    return $(:floor == f ? :(-convert(eltype(x), Inf)) : :(-zero(x)))
                else
                    return x
                end
            end
            return r
        end
    end
end

# isapprox: approximate equality of numbers
"""
    isapprox(x, y; rtol::Real=atol>0 ? 0 : √eps, atol::Real=0, nans::Bool=false, norm::Function)

Inexact equality comparison: `true` if `norm(x-y) <= max(atol, rtol*max(norm(x), norm(y)))`. The
default `atol` is zero and the default `rtol` depends on the types of `x` and `y`. The keyword
argument `nans` determines whether or not NaN values are considered equal (defaults to false).

For real or complex floating-point values, if an `atol > 0` is not specified, `rtol` defaults to
the square root of [`eps`](@ref) of the type of `x` or `y`, whichever is bigger (least precise).
This corresponds to requiring equality of about half of the significand digits. Otherwise,
e.g. for integer arguments or if an `atol > 0` is supplied, `rtol` defaults to zero.

`x` and `y` may also be arrays of numbers, in which case `norm` defaults to `vecnorm` but
may be changed by passing a `norm::Function` keyword argument. (For numbers, `norm` is the
same thing as `abs`.) When `x` and `y` are arrays, if `norm(x-y)` is not finite (i.e. `±Inf`
or `NaN`), the comparison falls back to checking whether all elements of `x` and `y` are
approximately equal component-wise.

The binary operator `≈` is equivalent to `isapprox` with the default arguments, and `x ≉ y`
is equivalent to `!isapprox(x,y)`.

Note that `x ≈ 0` (i.e., comparing to zero with the default tolerances) is
equivalent to `x == 0` since the default `atol` is `0`.  In such cases, you should either
supply an appropriate `atol` (or use `norm(x) ≤ atol`) or rearrange your code (e.g.
use `x ≈ y` rather than `x - y ≈ 0`).   It is not possible to pick a nonzero `atol`
automatically because it depends on the overall scaling (the "units") of your problem:
for example, in `x - y ≈ 0`, `atol=1e-9` is an absurdly small tolerance if `x` is the
[radius of the Earth](https://en.wikipedia.org/wiki/Earth_radius) in meters,
but an absurdly large tolerance if `x` is the
[radius of a Hydrogen atom](https://en.wikipedia.org/wiki/Bohr_radius) in meters.


# Examples
```jldoctest
julia> 0.1 ≈ (0.1 - 1e-10)
true

julia> isapprox(10, 11; atol = 2)
true

julia> isapprox([10.0^9, 1.0], [10.0^9, 2.0])
true

julia> 1e-10 ≈ 0
false

julia> isapprox(1e-10, 0, atol=1e-8)
true
```
"""
function isapprox(x::Number, y::Number; atol::Real=0, rtol::Real=rtoldefault(x,y,atol), nans::Bool=false)
    x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= max(atol, rtol*max(abs(x), abs(y)))) || (nans && isnan(x) && isnan(y))
end

const ≈ = isapprox
"""
    x ≉ y

This is equivalent to `!isapprox(x,y)` (see [`isapprox`](@ref)).
"""
≉(args...; kws...) = !≈(args...; kws...)

# default tolerance arguments
rtoldefault(::Type{T}) where {T<:AbstractFloat} = sqrt(eps(T))
rtoldefault(::Type{<:Real}) = 0
function rtoldefault(x::Union{T,Type{T}}, y::Union{S,Type{S}}, atol::Real) where {T<:Number,S<:Number}
    rtol = max(rtoldefault(real(T)),rtoldefault(real(S)))
    return atol > 0 ? zero(rtol) : rtol
end

# fused multiply-add

"""
    fma(x, y, z)

Computes `x*y+z` without rounding the intermediate result `x*y`. On some systems this is
significantly more expensive than `x*y+z`. `fma` is used to improve accuracy in certain
algorithms. See [`muladd`](@ref).
"""
function fma end

fma_libm(x::Float32, y::Float32, z::Float32) =
    ccall(("fmaf", libm_name), Float32, (Float32,Float32,Float32), x, y, z)
fma_libm(x::Float64, y::Float64, z::Float64) =
    ccall(("fma", libm_name), Float64, (Float64,Float64,Float64), x, y, z)
fma_llvm(x::Float32, y::Float32, z::Float32) = fma_float(x, y, z)
fma_llvm(x::Float64, y::Float64, z::Float64) = fma_float(x, y, z)
# Disable LLVM's fma if it is incorrect, e.g. because LLVM falls back
# onto a broken system libm; if so, use openlibm's fma instead
# 1.0000305f0 = 1 + 1/2^15
# 1.0000000009313226 = 1 + 1/2^30
# If fma_llvm() clobbers the rounding mode, the result of 0.1 + 0.2 will be 0.3
# instead of the properly-rounded 0.30000000000000004; check after calling fma
if (Sys.ARCH != :i686 && fma_llvm(1.0000305f0, 1.0000305f0, -1.0f0) == 6.103609f-5 &&
    (fma_llvm(1.0000000009313226, 1.0000000009313226, -1.0) ==
     1.8626451500983188e-9) && 0.1 + 0.2 == 0.30000000000000004)
    fma(x::Float32, y::Float32, z::Float32) = fma_llvm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_llvm(x,y,z)
else
    fma(x::Float32, y::Float32, z::Float32) = fma_libm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_libm(x,y,z)
end
function fma(a::Float16, b::Float16, c::Float16)
    Float16(fma(Float32(a), Float32(b), Float32(c)))
end

# This is necessary at least on 32-bit Intel Linux, since fma_llvm may
# have called glibc, and some broken glibc fma implementations don't
# properly restore the rounding mode
Rounding.setrounding_raw(Float32, Rounding.JL_FE_TONEAREST)
Rounding.setrounding_raw(Float64, Rounding.JL_FE_TONEAREST)

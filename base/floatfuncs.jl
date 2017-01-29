# This file is a part of Julia. License is MIT: http://julialang.org/license

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

maxintfloat(::Type{Float64}) = 9007199254740992.
maxintfloat(::Type{Float32}) = Float32(16777216.)
maxintfloat(::Type{Float16}) = Float16(2048f0)
maxintfloat{T<:AbstractFloat}(x::T)  = maxintfloat(T)
maxintfloat() = maxintfloat(Float64)

isinteger(x::AbstractFloat) = (x - trunc(x) == 0)

num2hex(x::Float16) = hex(bitcast(UInt16, x), 4)
num2hex(x::Float32) = hex(bitcast(UInt32, x), 8)
num2hex(x::Float64) = hex(bitcast(UInt64, x), 16)

function hex2num(s::AbstractString)
    if length(s) <= 4
        return bitcast(Float16, parse(UInt16, s, 16))
    end
    if length(s) <= 8
        return bitcast(Float32, parse(UInt32, s, 16))
    end
    return bitcast(Float64, parse(UInt64, s, 16))
end

"""
    round([T,] x, [digits, [base]], [r::RoundingMode])

Rounds `x` to an integer value according to the provided
[`RoundingMode`](@ref), returning a value of the same type as `x`. When not
specifying a rounding mode the global mode will be used
(see [`rounding`](@ref)), which by default is round to the nearest integer
([`RoundNearest`](@ref) mode), with ties (fractional values of 0.5) being
rounded to the nearest even integer.

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
before if negative). `round(x, digits, base)` rounds using a base other than 10.

```jldoctest
julia> round(pi, 2)
3.14

julia> round(pi, 3, 2)
3.125
```

!!! note
    Rounding to specified digits in bases other than 2 can be inexact when
    operating on binary floating point numbers. For example, the `Float64`
    value represented by `1.15` is actually *less* than 1.15, yet will be
    rounded to 1.2.

    ```jldoctest
    julia> x = 1.15
    1.15

    julia> @sprintf "%.20f" x
    "1.14999999999999991118"

    julia> x < 115//100
    true

    julia> round(x, 1)
    1.2
    ```
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
round{T<:Integer}(::Type{T}, x::AbstractFloat, r::RoundingMode) = trunc(T,round(x,r))

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

function signif(x::Real, digits::Integer, base::Integer=10)
    digits < 1 && throw(DomainError())

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
        function ($f)(x::Real, digits::Integer, base::Integer=10)
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
    isapprox(x, y; rtol::Real=sqrt(eps), atol::Real=0, nans::Bool=false)

Inexact equality comparison: `true` if `norm(x-y) <= atol + rtol*max(norm(x), norm(y))`. The
default `atol` is zero and the default `rtol` depends on the types of `x` and `y`. The keyword
argument `nans` determines whether or not NaN values are considered equal (defaults to false).

For real or complex floating-point values, `rtol` defaults to
`sqrt(eps(typeof(real(x-y))))`. This corresponds to requiring equality of about half of the
significand digits. For other types, `rtol` defaults to zero.

`x` and `y` may also be arrays of numbers, in which case `norm` defaults to `vecnorm` but
may be changed by passing a `norm::Function` keyword argument. (For numbers, `norm` is the
same thing as `abs`.) When `x` and `y` are arrays, if `norm(x-y)` is not finite (i.e. `±Inf`
or `NaN`), the comparison falls back to checking whether all elements of `x` and `y` are
approximately equal component-wise.

The binary operator `≈` is equivalent to `isapprox` with the default arguments, and `x ≉ y`
is equivalent to `!isapprox(x,y)`.
"""
function isapprox(x::Number, y::Number; rtol::Real=rtoldefault(x,y), atol::Real=0, nans::Bool=false)
    x == y || (isfinite(x) && isfinite(y) && abs(x-y) <= atol + rtol*max(abs(x), abs(y))) || (nans && isnan(x) && isnan(y))
end

const ≈ = isapprox
≉(args...; kws...) = !≈(args...; kws...)

# default tolerance arguments
rtoldefault{T<:AbstractFloat}(::Type{T}) = sqrt(eps(T))
rtoldefault{T<:Real}(::Type{T}) = 0
rtoldefault{T<:Number,S<:Number}(x::Union{T,Type{T}}, y::Union{S,Type{S}}) = max(rtoldefault(real(T)),rtoldefault(real(S)))

# fused multiply-add
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

## floating-point functions ##

abs(x::Float64) = boxf64(abs_float64(unbox64(x)))
abs(x::Float32) = boxf32(abs_float32(unbox32(x)))

isnan(x::Float) = (x != x)
isnan(x::Real) = isnan(float(x))
isnan(x::Integer) = false

isinf(x::Float) = (abs(x) == Inf)
isinf(x::Real) = isinf(float(x))
isinf(x::Integer) = false

isfinite(x::Float) = (x-x == 0)
isfinite(x::Real) = isfinite(float(x))
isfinite(x::Integer) = true

copysign(x::Float64, y::Float64) = boxf64(copysign_float64(unbox64(x),unbox64(y)))
copysign(x::Float32, y::Float32) = boxf32(copysign_float32(unbox32(x),unbox32(y)))
copysign(x::Float32, y::Real) = copysign(x, float32(y))
copysign(x::Float64, y::Real) = copysign(x, float64(y))
@vectorize_2arg Real copysign

signbit(x::Float64) = signbit(reinterpret(Int64,x))
signbit(x::Float32) = signbit(reinterpret(Int32,x))

maxintfloat(::Type{Float64}) = 9007199254740992.
maxintfloat(::Type{Float32}) = float32(16777216.)
maxintfloat{T<:Float}(x::T)  = maxintfloat(T)
maxintfloat() = maxintfloat(Float64)

integer_valued(x::Float) = (trunc(x)==x)&isfinite(x)

sqrt(x::Real) = sqrt(float(x))
sin(x::Real) = sin(float(x))
cos(x::Real) = cos(float(x))

num2hex(x::Float32) = int2str(boxui32(unbox32(x)),16, 8)
num2hex(x::Float64) = int2str(boxui64(unbox64(x)),16,16)

function hex2num(s::String)
    if length(s) <= 8
        return boxf32(unbox32(parse_int(Int32, s, 16)))
    end
    return boxf64(unbox64(parse_int(Int64, s, 16)))
end

@vectorize_1arg Real iround
@vectorize_1arg Real itrunc
@vectorize_1arg Real ifloor
@vectorize_1arg Real iceil

@vectorize_1arg Number abs
@vectorize_1arg Number abs2
@vectorize_1arg Number angle

## floating-point functions ##

abs(x::Float64) = boxf64(abs_float64(unbox64(x)))
abs(x::Float32) = boxf32(abs_float32(unbox32(x)))

isnan(x::Float) = (x != x)
isnan(x::Real) = isnan(float(x))
isnan(x::Int) = false

isinf(x::Float) = (abs(x) == Inf)
isinf(x::Real) = isinf(float(x))
isinf(x::Int) = false

isfinite(x::Float) = (x-x == 0)
isfinite(x::Real) = isfinite(float(x))
isfinite(x::Int) = true

copysign(x::Float64, y::Float64) = boxf64(copysign_float64(unbox64(x),unbox64(y)))
copysign(x::Float32, y::Float32) = boxf32(copysign_float32(unbox32(x),unbox32(y)))
copysign(x::Float32, y::Real) = copysign(x, float32(y))
copysign(x::Float64, y::Real) = copysign(x, float64(y))
@vectorize_2arg Real copysign

signbit(x::Float64) = copysign(1.0, x)
signbit(x::Float32) = copysign(float32(1.0), x)

exponent(x::Float64) = ccall(:double_exponent, Int32, (Float64,), x)
exponent(x::Float32) = ccall(:float_exponent,  Int32, (Float32,), x)
mantissa(x::Float64) = ccall(:double_mantissa, Float64, (Float64,), x)
mantissa(x::Float32) = ccall(:float_mantissa,  Float32, (Float32,), x)

integer_valued(x::Float64) = (trunc(x)==x && abs(x)<=9007199254740992.)
integer_valued(x::Float32) = (trunc(x)==x && abs(x)<=float32(16777216.))

cmp(x::Float, y::Float) = sign(x-y)

#sqrt(x::Float64) = boxf64(sqrt_float(unbox64(x)))
#sqrt(x::Float32) = boxf32(sqrt_float(unbox32(x)))
^(x::Float64, p::Int32) = boxf64(powi_float(unbox64(x),unbox32(p)))
^(x::Float32, p::Int32) = boxf32(powi_float(unbox32(x),unbox32(p)))

sqrt(x::Real) = sqrt(float(x))
sin(x::Real) = sin(float(x))
cos(x::Real) = cos(float(x))

num2hex(x::Float32) = uint2str(boxui32(unbox32(x)),16, 8)
num2hex(x::Float64) = uint2str(boxui64(unbox64(x)),16,16)

function hex2num(s)
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

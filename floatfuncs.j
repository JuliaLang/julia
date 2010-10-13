## floating-point functions ##

rem(x::Float, y::Float) = remainder(x, y)

signbit(x::Float) = (x < 0 ? -1 : (x > 0 ? 1 : (1.0/x < 0 ? -1 : +1)))
signbit(x::Float64) = (boxsi64(unbox64(x)) < int64(0) ? -1 : +1)
signbit(x::Float32) = (boxsi32(unbox32(x)) < int32(0) ? -1 : +1)

exponent(x::Float64) = ccall(dlsym(JuliaDLHandle,"double_exponent"),
                             Int32, (Float64,), x)
exponent(x::Float32) = ccall(dlsym(JuliaDLHandle,"float_exponent"),
                             Int32, (Float32,), x)
mantissa(x::Float64) = ccall(dlsym(JuliaDLHandle,"double_mantissa"),
                             Float64, (Float64,), x)
mantissa(x::Float32) = ccall(dlsym(JuliaDLHandle,"float_mantissa"),
                             Float32, (Float32,), x)

integer_valued(x::Float64) = (trunc(x)==x && abs(x)<=9007199254740992.)
integer_valued(x::Float32) = (trunc(x)==x && abs(x)<=float32(16777216.))

sqrt(x::Float64) = boxf64(sqrt_float(unbox64(x)))
sqrt(x::Float32) = boxf32(sqrt_float(unbox32(x)))
sin(x::Float64) = boxf64(sin_float(unbox64(x)))
sin(x::Float32) = boxf32(sin_float(unbox32(x)))
cos(x::Float64) = boxf64(cos_float(unbox64(x)))
cos(x::Float32) = boxf32(cos_float(unbox32(x)))
^(x::Float64, p::Int32) = boxf64(powi_float(unbox64(x),unbox32(p)))
^(x::Float32, p::Int32) = boxf32(powi_float(unbox32(x),unbox32(p)))

function hypot(x::Real, y::Real)
    x = abs(x)
    y = abs(y)
    if x > y
        r = y/x
        return x*sqrt(1+r*r)
    end
    if y == 0
        return convert(typeof(x),0)
    end
    r = x/y
    return y*sqrt(1+r*r)
end

num2hex(x::Float32) = uint2str(boxui32(unbox32(x)),16,16)
num2hex(x::Float64) = uint2str(boxui64(unbox64(x)),16,16)

function hex2num(s)
    if length(s) <= 8
        return boxf32(unbox32(parse_int(Int32, s, 16)))
    end
    return boxf64(unbox64(parse_int(Int64, s, 16)))
end

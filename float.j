Float32.convert(x::Int8)    = boxf32(sitofp32(unbox8(x)))
Float32.convert(x::Int16)   = boxf32(sitofp32(unbox16(x)))
Float32.convert(x::Int32)   = boxf32(sitofp32(unbox32(x)))
Float32.convert(x::Int64)   = boxf32(sitofp32(unbox64(x)))
Float32.convert(x::Uint8)   = boxf32(uitofp32(unbox8(x)))
Float32.convert(x::Uint16)  = boxf32(uitofp32(unbox16(x)))
Float32.convert(x::Uint32)  = boxf32(uitofp32(unbox32(x)))
Float32.convert(x::Uint64)  = boxf32(uitofp32(unbox64(x)))
Float32.convert(x::Float64) = boxf32(fptrunc32(unbox64(x)))

Float64.convert(x::Int8)    = boxf64(sitofp64(unbox8(x)))
Float64.convert(x::Int16)   = boxf64(sitofp64(unbox16(x)))
Float64.convert(x::Int32)   = boxf64(sitofp64(unbox32(x)))
Float64.convert(x::Int64)   = boxf64(sitofp64(unbox64(x)))
Float64.convert(x::Uint8)   = boxf64(uitofp64(unbox8(x)))
Float64.convert(x::Uint16)  = boxf64(uitofp64(unbox16(x)))
Float64.convert(x::Uint32)  = boxf64(uitofp64(unbox32(x)))
Float64.convert(x::Uint64)  = boxf64(uitofp64(unbox64(x)))
Float64.convert(x::Float32) = boxf64(fpext64(unbox32(x)))

float32(x::Scalar) = Float32.convert(x)
float64(x::Scalar) = Float64.convert(x)
truncate(x::Real) = int32(x)

(+)(x::Float64, y::Float64) = boxf64(add_float(unbox64(x), unbox64(y)))
(-)(x::Float64, y::Float64) = boxf64(sub_float(unbox64(x), unbox64(y)))
(*)(x::Float64, y::Float64) = boxf64(mul_float(unbox64(x), unbox64(y)))
(/)(x::Float64, y::Float64) = boxf64(div_float(unbox64(x), unbox64(y)))

(-)(x::Float64) = boxf64(neg_float(unbox64(x)))

<=(x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y)) || eq_float(unbox64(x),unbox64(y))
< (x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y))
> (x::Float64, y::Float64) = lt_float(unbox64(y),unbox64(x))
>=(x::Float64, y::Float64) = (x>y) || eq_float(unbox64(x),unbox64(y))
==(x::Float64, y::Float64) = eq_float(unbox64(x),unbox64(y))

!=(x::Float64, y::Float64) = ne_float(unbox64(x),unbox64(y))

< (x::Float64, y::Int32) = x < Float64.convert(y)
> (x::Float64, y::Int32) = x > Float64.convert(y)

Inf = 1/0
NaN = 0/0

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
        return x*sqrt(1.0+r*r)
    end
    if y == 0.0
        return 0.0
    end
    r = x/y
    return y*sqrt(1.0+r*r)
end

## conversions ##

convert(::Type{Float32}, x::Int8)    = boxf32(sitofp32(unbox8(x)))
convert(::Type{Float32}, x::Int16)   = boxf32(sitofp32(unbox16(x)))
convert(::Type{Float32}, x::Int32)   = boxf32(sitofp32(unbox32(x)))
convert(::Type{Float32}, x::Int64)   = boxf32(sitofp32(unbox64(x)))
convert(::Type{Float32}, x::Uint8)   = boxf32(uitofp32(unbox8(x)))
convert(::Type{Float32}, x::Uint16)  = boxf32(uitofp32(unbox16(x)))
convert(::Type{Float32}, x::Uint32)  = boxf32(uitofp32(unbox32(x)))
convert(::Type{Float32}, x::Uint64)  = boxf32(uitofp32(unbox64(x)))
convert(::Type{Float32}, x::Float64) = boxf32(fptrunc32(unbox64(x)))

convert(::Type{Float64}, x::Int8)    = boxf64(sitofp64(unbox8(x)))
convert(::Type{Float64}, x::Int16)   = boxf64(sitofp64(unbox16(x)))
convert(::Type{Float64}, x::Int32)   = boxf64(sitofp64(unbox32(x)))
convert(::Type{Float64}, x::Int64)   = boxf64(sitofp64(unbox64(x)))
convert(::Type{Float64}, x::Uint8)   = boxf64(uitofp64(unbox8(x)))
convert(::Type{Float64}, x::Uint16)  = boxf64(uitofp64(unbox16(x)))
convert(::Type{Float64}, x::Uint32)  = boxf64(uitofp64(unbox32(x)))
convert(::Type{Float64}, x::Uint64)  = boxf64(uitofp64(unbox64(x)))
convert(::Type{Float64}, x::Float32) = boxf64(fpext64(unbox32(x)))

float32(x::Scalar) = convert(Float32, x)
float64(x::Scalar) = convert(Float64, x)
truncate(x::Real) = int32(x)

## promotions ##

promote_rule(::Type{Float64}, ::Type{Float32} ) = Float64

promote_rule(::Type{Float32}, ::Type{Int8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Int16}) = Float32
promote_rule(::Type{Float32}, ::Type{Int32}) = Float64
promote_rule(::Type{Float32}, ::Type{Int64}) = Float64 # TODO: should be Float128 or BigFloat
promote_rule(::Type{Float64}, ::Type{Int8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Int16}) = Float64
promote_rule(::Type{Float64}, ::Type{Int32}) = Float64
promote_rule(::Type{Float64}, ::Type{Int64}) = Float64 # TODO: should be Float128 or BitFloat

## basic arithmetic ##

(-)(x::Float32) = boxf32(neg_float(unbox32(x)))
(-)(x::Float64) = boxf64(neg_float(unbox64(x)))
(+)(x::Float32, y::Float32) = boxf32(add_float(unbox32(x), unbox32(y)))
(+)(x::Float64, y::Float64) = boxf64(add_float(unbox64(x), unbox64(y)))
(-)(x::Float32, y::Float32) = boxf32(sub_float(unbox32(x), unbox32(y)))
(-)(x::Float64, y::Float64) = boxf64(sub_float(unbox64(x), unbox64(y)))
(*)(x::Float32, y::Float32) = boxf32(mul_float(unbox32(x), unbox32(y)))
(*)(x::Float64, y::Float64) = boxf64(mul_float(unbox64(x), unbox64(y)))
(/)(x::Float32, y::Float32) = boxf32(div_float(unbox32(x), unbox32(y)))
(/)(x::Float64, y::Float64) = boxf64(div_float(unbox64(x), unbox64(y)))

## floating point comparisons ##

==(x::Float32, y::Float32) = eq_float(unbox32(x),unbox32(y))
==(x::Float64, y::Float64) = eq_float(unbox64(x),unbox64(y))
< (x::Float32, y::Float32) = lt_float(unbox32(x),unbox32(y))
< (x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y))

## floating point constants ##

Inf = 1/0
NaN = -(0/0)

## floating point functions ##

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
    if y == 0
        return convert(typeof(x),0)
    end
    r = x/y
    return y*sqrt(1.0+r*r)
end

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
truncate(x::Float32) = convert(Int32, x)
truncate(x::Float64) = convert(Int64, x)

## promotions ##

promote_rule(::Type{Float64}, ::Type{Float32} ) = Float64

promote_rule(::Type{Float32}, ::Type{Int8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Int16}) = Float32
promote_rule(::Type{Float32}, ::Type{Int32}) = Float64
promote_rule(::Type{Float32}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Int8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Int16}) = Float64
promote_rule(::Type{Float64}, ::Type{Int32}) = Float64
promote_rule(::Type{Float64}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float32}, ::Type{Uint8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Uint16}) = Float32
promote_rule(::Type{Float32}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float32}, ::Type{Uint64}) = Float64# TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Uint8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Uint16}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint64}) = Float64# TODO: should be Float80

## traits ##

eps(::Type{Float32}) = float32(1.1920928e-7)
eps(::Type{Float64}) = 2.2204460492503131e-16
typemin(::Type{Float32}) = float32(1.175494351e-38)
typemax(::Type{Float32}) = float32(3.402823466e+38)
typemin(::Type{Float64}) = 2.2250738585072014e-308
typemax(::Type{Float64}) = 1.7976931348623157e+308

sizeof(x::Float32) = 4
sizeof(x::Float64) = 8

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
!=(x::Float32, y::Float32) = ne_float(unbox32(x),unbox32(y))
!=(x::Float64, y::Float64) = ne_float(unbox64(x),unbox64(y))
# negating a comparison is not ok for floats
!=(x::Float, y::Float) = (!=)(promote(x,y)...)
< (x::Float32, y::Float32) = lt_float(unbox32(x),unbox32(y))
< (x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y))
<=(x::Float32, y::Float32) = le_float(unbox32(x),unbox32(y))
<=(x::Float64, y::Float64) = le_float(unbox64(x),unbox64(y))
>=(x::Float32, y::Float32) = ge_float(unbox32(x),unbox32(y))
>=(x::Float64, y::Float64) = ge_float(unbox64(x),unbox64(y))
<=(x::Float, y::Float) = (<=)(promote(x,y)...)
>=(x::Float, y::Float) = (>=)(promote(x,y)...)

## floating point constants ##

Inf = 1/0
NaN = -(0/0)

## floating point functions ##

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

pi() = 3.14159265358979323846
pi(x) = pi()
pi(::Union(Float64, Type{Float64})) = 3.14159265358979323846
pi(::Union(Float32, Type{Float32})) = float32(3.14159265358979323846)

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

num2hex(x::Float32) = lpad(uint2str(boxui32(unbox32(x)),16),16,"0"[1])
num2hex(x::Float64) = lpad(uint2str(boxui64(unbox64(x)),16),16,"0"[1])

function hex2num(s)
    if length(s) <= 8
        return boxf32(unbox32(parse_int(Int32, s, 16)))
    end
    return boxf64(unbox64(parse_int(Int64, s, 16)))
end

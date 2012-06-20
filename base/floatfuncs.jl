## floating-point functions ##

abs(x::Float64) = box(Float64,abs_float(unbox(Float64,x)))
abs(x::Float32) = box(Float32,abs_float(unbox(Float32,x)))

isnan(x::Float) = (x != x)
isnan(x::Real) = isnan(float(x))
isnan(x::Integer) = false

isinf(x::Float) = (abs(x) == Inf)
isinf(x::Real) = isinf(float(x))
isinf(x::Integer) = false

isfinite(x::Float) = (x-x == 0)
isfinite(x::Real) = isfinite(float(x))
isfinite(x::Integer) = true

copysign(x::Float64, y::Float64) = box(Float64,copysign_float(unbox(Float64,x),unbox(Float64,y)))
copysign(x::Float32, y::Float32) = box(Float32,copysign_float(unbox(Float32,x),unbox(Float32,y)))
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

num2hex(x::Float32) = hex(box(Uint32,unbox(Float32,x)),8)
num2hex(x::Float64) = hex(box(Uint64,unbox(Float64,x)),16)

function hex2num(s::String)
    if length(s) <= 8
        return box(Float32,unbox(Int32,parse_hex(Int32,s)))
    end
    return box(Float64,unbox(Int64,parse_hex(Int64,s)))
end

@vectorize_1arg Real iround
@vectorize_1arg Real itrunc
@vectorize_1arg Real ifloor
@vectorize_1arg Real iceil

@vectorize_1arg Number abs
@vectorize_1arg Number abs2
@vectorize_1arg Number angle

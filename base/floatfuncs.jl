## floating-point functions ##

abs(x::Float64) = box(Float64,abs_float(unbox(Float64,x)))
abs(x::Float32) = box(Float32,abs_float(unbox(Float32,x)))

isnan(x::FloatingPoint) = (x != x)
isnan(x::Real) = isnan(float(x))
isnan(x::Integer) = false

isinf(x::FloatingPoint) = (abs(x) == Inf)
isinf(x::Real) = isinf(float(x))
isinf(x::Integer) = false

isfinite(x::FloatingPoint) = (x-x == 0)
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
maxintfloat{T<:FloatingPoint}(x::T)  = maxintfloat(T)
maxintfloat() = maxintfloat(Float64)

integer_valued(x::FloatingPoint) = (trunc(x)==x)&isfinite(x)

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

@vectorize_1arg Real isnan
@vectorize_1arg Real isinf
@vectorize_1arg Real isfinite

# adapted from Matlab File Exchange roundsd: http://www.mathworks.com/matlabcentral/fileexchange/26212
# for round, og is the power of 10 relative to the decimal point
# for signif, og is the absolute power of 10
# digits and base must be integers, x must be convertable to float

function _signif_og(x, digits, base)
    if base == 10
        10. ^ floor(log10(abs(x)) - digits + 1.)
    elseif base == 2
        2. ^ floor(log2(abs(x)) - digits + 1.)
    else
        float(base) ^ floor(log2(abs(x))/log2(base) - digits + 1.)
    end
end
_round_og(digits, base) = float(base) ^ (- digits)

function signif(x, digits::Integer, base::Integer)
    if digits < 0
        throw(DomainError())
    end
    og = _signif_og(float(x), digits, base)
    round(float(x)/og) * og
end
signif(x, digits) = signif(x, digits, 10)

for f in (:round, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x, digits::Integer, base::Integer)
            og = _round_og(digits, base)
            ($f)(float(x) / og) * og
        end
        ($f)(x, digits) = ($f)(x, digits, 10)
    end
end

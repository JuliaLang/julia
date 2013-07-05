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

isinteger(x::FloatingPoint) = (trunc(x)==x)&isfinite(x)
isfloat64(x::Number) = float64(x) == x
isfloat64(::Float64) = true
isfloat64(::Float32) = true

## precision, as defined by the effective number of bits in the mantissa ##
get_precision(::Float32) = 24
get_precision(::Float64) = 53

num2hex(x::Float32) = hex(box(Uint32,unbox(Float32,x)),8)
num2hex(x::Float64) = hex(box(Uint64,unbox(Float64,x)),16)

function hex2num(s::String)
    if length(s) <= 8
        return box(Float32,unbox(Int32,parseint(Int32,s,16)))
    end
    return box(Float64,unbox(Int64,parseint(Int64,s,16)))
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

function signif(x, digits::Integer, base::Integer=10)
    if digits < 0
        throw(DomainError())
    end
    if x==0 || !isfinite(x)
        return x
    end
    og = _signif_og(float(x), digits, base)
    round(float(x)/og) * og
end

_round_og(digits, base) = float(base) ^ digits

for f in (:round, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x, digits::Integer, base::Integer=10)
            og = _round_og(digits, base)
            ($f)(float(x) * og) / og
        end
    end
end

# isapprox: Tolerant comparison of floating point numbers
function isapprox(x::FloatingPoint, y::FloatingPoint; rtol::Real=rtoldefault(x,y), atol::Real=atoldefault(x,y))
    (isinf(x) || isinf(y)) ? x == y : abs(x-y) <= atol + rtol.*max(abs(x), abs(y))
end

# promotion of non-floats
isapprox(x::Real, y::FloatingPoint; rtol::Real=rtoldefault(x, y), atol::Real=atoldefault(x, y)) = isapprox(promote(x, y)...; rtol=rtol, atol=atol)
isapprox(x::FloatingPoint, y::Real; rtol::Real=rtoldefault(x, y), atol::Real=atoldefault(x, y)) = isapprox(promote(x, y)...; rtol=rtol, atol=atol)

# other real numbers
isapprox(x::Real, y::Real; rtol::Real=0, atol::Real=0) = abs(x-y) <= atol

# complex numbers
isapprox(z::Complex, w::Complex; rtol::Real=rtoldefault(abs(z), abs(w)), atol::Real=atoldefault(abs(z), abs(w))) = abs(z-w) <= atol + rtol*max(abs(z), abs(w))

# real-complex combinations
isapprox(x::Real, z::Complex; rtol::Real=rtoldefault(x, abs(z)), atol::Real=atoldefault(x, abs(z))) = isapprox(complex(x), z; rtol=rtol, atol=atol)
isapprox(z::Complex, x::Real; rtol::Real=rtoldefault(x, abs(z)), atol::Real=atoldefault(x, abs(z))) = isapprox(complex(x), z; rtol=rtol, atol=atol)

# default tolerance arguments
rtoldefault(x::FloatingPoint, y::FloatingPoint) = cbrt(max(eps(x), eps(y)))
atoldefault(x::FloatingPoint, y::FloatingPoint) = sqrt(max(eps(x), eps(y)))

# promotion of non-floats
for fun in (:rtoldefault, :atoldefault)
    @eval begin
        ($fun)(x::Real, y::FloatingPoint) = ($fun)(promote(x,y)...)
        ($fun)(x::FloatingPoint, y::Real) = ($fun)(promote(x,y)...)
    end
end

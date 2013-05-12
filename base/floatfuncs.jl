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

function signif(x, digits::Integer, base::Integer)
    if digits < 0
        throw(DomainError())
    end
    og = _signif_og(float(x), digits, base)
    round(float(x)/og) * og
end
signif(x, digits) = signif(x, digits, 10)

_round_og(digits, base) = float(base) ^ digits

for f in (:round, :ceil, :floor, :trunc)
    @eval begin
        function ($f)(x, digits::Integer, base::Integer)
            og = _round_og(digits, base)
            ($f)(float(x) * og) / og
        end
        ($f)(x, digits) = ($f)(x, digits, 10)
    end
end



typealias RealOrComplexFloat{T<:FloatingPoint} Union(FloatingPoint, Complex{T})

export isapprox, isapproxn

# isapprox: Tolerant comparison of floating point numbers
function isapprox(x::FloatingPoint, y::FloatingPoint; rtol::Real=rtoldefault(x,y), atol::Real=atoldefault(x,y))
    (isinf(x) || isinf(y)) ? x == y : abs(x-y) <= atol + rtol.*max(abs(x), abs(y))
end
# isapproxn: Like isapprox, but with nan==nan
function isapproxn(x::FloatingPoint, y::FloatingPoint; rtol::Real=rtoldefault(x,y), atol::Real=atoldefault(x,y)) 
    (isnan(x) && isnan(y)) || isapprox(x, y, rtol=rtol, atol=atol)
end

# here we create methods for both functions for a wide variety of input arguments.
# the goal is to cover isapprox(x::Number, y::Number) with optional arguments for tolerances, as well as support for arrays
for fun in [:isapprox, :isapproxn]
    @eval begin
        # complex floats
        ($fun){T1<:FloatingPoint, T2<:FloatingPoint}(x::Complex{T1}, y::Complex{T2}; 
                                                     rrtol::Real=rtoldefault(real(x), real(y)), ratol::Real=atoldefault(real(x), real(y)),
                                                     irtol::Real=rtoldefault(imag(x), imag(y)), iatol::Real=atoldefault(imag(x), imag(y))) = 
            ($fun)(real(x), real(y); rtol=rrtol, atol=ratol) && ($fun)(imag(x), imag(y); rtol=irtol, atol=iatol)

        # real-complex combinations of floats
        ($fun){T1<:FloatingPoint, T2<:FloatingPoint}(x::T1, z::Complex{T2};  
                                                     rrtol::Real=rtoldefault(x, real(z)), ratol::Real=atoldefault(x, real(z)),
                                                     irtol::Real=rtoldefault(x, imag(z)), iatol::Real=atoldefault(x, imag(z))) = 
            ($fun)(x, real(z); rtol=rrtol, atol=ratol) && ($fun)(imag(z), 0; rtol=irtol, atol=iatol)
        ($fun){T1<:FloatingPoint, T2<:FloatingPoint}(z::Complex{T1}, x::T2;  
                                                     rrtol::Real=rtoldefault(x, real(z)), ratol::Real=atoldefault(x, real(z)),
                                                     irtol::Real=rtoldefault(x, imag(z)), iatol::Real=atoldefault(x, imag(z))) = 
            ($fun)(x, z; rrtol=rrtol, ratol=ratol, irtol=irtol, iatol=iatol)

        # Catch-all with promotion
        ($fun)(x::FloatingPoint, y::Real; rtol::Real=rtoldefault(x,x), atol::Real=atoldefault(x,x)) = ($fun)(promote(x,y)...; rtol=rtol, atol=atol)
        ($fun)(x::Real, y::FloatingPoint; rtol::Real=rtoldefault(y,y), atol::Real=atoldefault(y,y)) = ($fun)(y,x; rtol=rtol, atol=atol)
    end
end

# isapprox for real non-floats
isapprox(x::Real, y::Real) = isequal(x, y)
isapprox(x::Real, y::Real; tol::Real=0) = abs(x-y) <= tol

# isapprox for complex non-floats
isapprox(z::Complex, w::Complex) = isequal(real(z), real(w)) && isequal(imag(z), imag(w))
isapprox(z::Complex, w::Complex; rtol::Real=0, itol::Real=0) = isapprox(real(z), real(w); tol=rtol) && isapprox(imag(z), imag(w); tol=itol)

# isapprox for real-complex combinations of non-floats
isapprox(z::Complex, x::Real) = isequal(real(z), x) && isequal(imag(z), 0)
isapprox(x::Real, z::Complex) = isapprox(z,x)
isapprox(z::Complex, x::Real; rtol=0, itol=0) = isapprox(real(z), x; tol=rtol) && isapprox(imag(z), 0; tol=itol)
isapprox(x::Real, z::Complex; rtol=0, itol=0) = isapprox(z,x; rtol=rtol, itol=itol)

rtoldefault(x::FloatingPoint, y::FloatingPoint) = cbrt(max(!isnan(x) ? eps(x) : eps(one(x)), !isnan(y) ? eps(y) : eps(one(y))))
atoldefault(x::FloatingPoint, y::FloatingPoint) = sqrt(max(!isnan(x) ? eps(x) : eps(one(x)), !isnan(y) ? eps(y) : eps(one(y))))

for fun in [:rtoldefault, :atoldefault]
    @eval begin
        ($fun)(x::Real, y::FloatingPoint) = ($fun)(promote(x,y)...)
        ($fun)(x::FloatingPoint, y::Real) = ($fun)(y,x)
    end
end

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


typealias RealNonFloats{T<:Union(Integer,Rational)} Union(Integer, Rational{T})
typealias NonFloats Union(RealNonFloats, Complex{RealNonFloats})
typealias RealOrComplexFloat{T<:FloatingPoint} Union(FloatingPoint, Complex{T})


# isapprox: Tolerant comparison of floating point numbers
function isapprox(x::FloatingPoint, y::FloatingPoint, rtol::Real, atol::Real)
    (isinf(x) || isinf(y)) ? x == y : abs(x-y) <= atol + rtol.*max(abs(x), abs(y))
end
# isapproxn: Like isapprox, but with nan==nan
isapproxn(x::FloatingPoint, y::FloatingPoint, rtol::Real, atol::Real) = (isnan(x) && isnan(y)) || isapprox(x, y, rtol, atol)
# isequaln: Like isequal, but with nan==nan
isequaln(x::FloatingPoint, y::FloatingPoint) = (isnan(x) && isnan(y)) || isequal(x,y)


# isapprox for real non-floats
# isapproxn is not relevant for integers, because they cannot be nan
isapprox(x::RealNonFloats, y::RealNonFloats) = isequal(x, y)
isapprox(x::RealNonFloats, y::RealNonFloats, tol::Real) = abs(x-y) <= tol

# isapprox for complex non-floats
isapprox{T1<:RealNonFloats, T2<:RealNonFloats}(z::Complex{T1}, w::Complex{T2}, tol::Integer=0) = 
    isapprox(real(z), real(w), tol) && isapprox(imag(z), imag(w), tol)

# here we create methods for both functions for a wide variety of input arguments.
# the goal is to cover isapprox(x::Number, y::Number) with optional arguments for tolerances, as well as support for arrays
for fun in [:isapprox, :isapproxn]
    @eval begin
        # Complex floats
        ($fun){T1<:FloatingPoint, T2<:FloatingPoint}(x::Complex{T1}, y::Complex{T2}, rtol::Real, atol::Real) = 
            ($fun)(real(x), real(y), rtol, atol) && ($fun)(imag(x), imag(y), rtol, atol)

        # One real, one complex
        ($fun)(x::Real, z::Complex, rtol::Real, atol::Real) = ($fun)(x, real(z), rtol, atol) && ($fun)(imag(z), 0, rtol, atol)
        ($fun)(z::Complex, x::Real, rtol::Real, atol::Real) = ($fun)(x, z, rtol, atol)

        # array versions
        ($fun){T<:Number}(X::AbstractArray{T}, y::Number, args...) = all(map(x -> ($fun)(x, y, args...), X))
        ($fun){T<:Number}(x::Number, Y::AbstractArray{T}, args...) = ($fun)(Y, x, args...)
        ($fun){T1<:Number, T2<:Number}(X::AbstractArray{T1}, Y::AbstractArray{T2}, args...) = 
            (size(X) == size(Y)) && all(map((x, y) -> ($fun)(x, y, args...), X, Y))

        # default tolerances
        ($fun)(x::RealOrComplexFloat, y::RealOrComplexFloat) = ($fun)(x, y, defaulttols(x, y)...)
        ($fun){T<:RealOrComplexFloat}(X::AbstractArray{T}, y::RealOrComplexFloat) = ($fun)(X, y, defaulttols(X, y)...)
        ($fun){T<:RealOrComplexFloat}(x::RealOrComplexFloat, Y::AbstractArray{T}) = ($fun)(Y, x, defaulttols(Y, x)...)
        ($fun){T1<:RealOrComplexFloat, T2<:RealOrComplexFloat}(X::AbstractArray{T1}, Y::AbstractArray{T2}) = ($fun)(X, Y,defaulttols(X, Y)...)

        # Catch-all with promotion
        ($fun)(x::Number, y::Number) = ($fun)(promote(x,y)..., defaulttols(x,y)...)
        ($fun)(x::Number, y::Number, rtol::Real, atol::Real) = ($fun)(promote(x,y)..., rtol, atol)
    end
end

isequaln{T1<:FloatingPoint, T2<:FloatingPoint}(z::Complex{T1}, w::Complex{T2}) = isequaln(real(z), real(w)) && isequaln(imag(z), imag(w))
isequaln{T1<:RealOrComplexFloat, T2<:RealOrComplexFloat}(X::AbstractArray{T1}, Y::AbstractArray{T2}) = all(map((x,y) -> isequaln(x,y), X, Y))

typealias FloatOrDerivative{T<:FloatingPoint} Union(T,Complex{T},AbstractArray{T},AbstractArray{Complex{T}},Rational{T},AbstractArray{Rational{T}})

function defaulttols{T1<:FloatingPoint, T2<:FloatingPoint}(z::FloatOrDerivative{T1}, w::FloatOrDerivative{T2})
    epsilon = max(eps(T1), eps(T2))
    return cbrt(epsilon), sqrt(epsilon)
end
defaulttols{T<:FloatingPoint}(x::NonFloats, y::FloatOrDerivative{T}) = defaulttols(y, y)
defaulttols{T<:FloatingPoint}(x::FloatOrDerivative{T}, y::NonFloats) = defaulttols(y, x)

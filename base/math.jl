libopenlibm = dlopen("libopenlibm")

module Math

import Base.*

export cbrt, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot, 
       sech, csch, coth, asech, acsch, acoth, sinc, cosc, 
       cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       radians2degrees, degrees2radians,
       log, log2, log10, log1p, logb, exp, exp2, expm1, 
       sqrt, square, erf, erfc, ceil, floor, trunc, round, lgamma,
       hypot, gamma, lfact, max, min, ilogb, ldexp, frexp,
       modf, ^

# non-type specific math functions

clamp(x::Real, lo::Real, hi::Real) = (x > hi ? hi : (x < lo ? lo : x))

sinc(x::Number) = x==0 ? one(x)  : (pix=pi*x; oftype(x,sin(pix)/pix))
cosc(x::Number) = x==0 ? zero(x) : (pix=pi*x; oftype(x,cos(pix)/x-sin(pix)/(pix*x)))

radians2degrees(z::Real) = oftype(z, (180/pi) * z)
degrees2radians(z::Real) = oftype(z, (pi/180) * z)

for (finv, f) in ((:sec, :cos), (:csc, :sin), (:cot, :tan),
                  (:sech, :cosh), (:csch, :sinh), (:coth, :tanh))
    @eval begin
        ($finv)(z) = 1 ./ (($f)(z))
    end
end
    
for (fa, fainv) in ((:asec, :acos), (:acsc, :asin), (:acot, :atan),
                    (:asech, :acosh), (:acsch, :asinh), (:acoth, :atanh))
    @eval begin
        ($fa)(y) = ($fainv)(1 ./ y)
    end
end

for (fd, f) in ((:sind, :sins), (:cosd, :cos), (:tand, :tan),
                (:secd, :sec), (:cscd, :csc), (:cotd, :cot))
    @eval begin
        ($fd)(z) = ($f)(degrees2radians(z))
    end
end

for (fd, f) in ((:asin, :asin), (:acosd, :acos), (:atand, :atan),
                (:asecd, :asec), (:acscd, :acsc), (:acotd, :acot))
    @eval begin
        ($fd)(y) = degrees2radians(($f)(z))
    end
end

log(b,x) = log(x)/log(b)

function hypot(x::Real, y::Real)
    x = abs(x)
    y = abs(y)
    if x > y
        r = y/x
        return x*sqrt(one(r)+r*r)
    end
    if y == 0
        return sqrt(y)  # to give same type as other cases
    end
    r = x/y
    return y*sqrt(one(r)+r*r)
end

square(x::Number) = x*x

# type specific math functions

for f in (:cbrt, :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan, 
          :asinh, :acosh, :atanh, :log, :log2, :log10, :exp, :erf, :erfc, :lgamma, :sqrt, :exp2)
    @eval begin
        ($f)(x::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

for f in (:log1p, :logb, :expm1, :ceil, :floor, :trunc, :round, :significand) # :rint, :nearbyint
    @eval begin
        ($f)(x::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Real $f
    end
end

atan2(x::Real, y::Real) = atan2(float64(x), float64(y))

hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

for f in (:atan2, :hypot)
    @eval begin
        ($f)(x::Float64, y::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32, Float32), x, y)
        @vectorize_2arg Number $f
    end
end

gamma(x::Float64) = ccall(dlsym(Base.libopenlibm, :tgamma),  Float64, (Float64,), x)
gamma(x::Float32) = float32(gamma(float64(x)))
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

lfact(x::Real) = (x<=1 ? zero(x) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

max(x::Float64, y::Float64) = ccall(dlsym(Base.libopenlibm, :fmax),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall(dlsym(Base.libopenlibm, :fmaxf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall(dlsym(Base.libopenlibm, :fmin),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall(dlsym(Base.libopenlibm, :fminf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

function ilogb(x::Float64)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(Base.libopenlibm,:ilogb), Int32, (Float64,), x))
end
function ilogb(x::Float32)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(Base.libopenlibm,:ilogbf), Int32, (Float32,), x))
end
@vectorize_1arg Real ilogb

ldexp(x::Float64,e::Int) = ccall(dlsym(Base.libopenlibm, :ldexp),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall(dlsym(Base.libopenlibm, :ldexpf), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize ldexp

begin
    local exp::Array{Int32,1} = zeros(Int32,1)
    global frexp
    function frexp(x::Float64)
        s = ccall(dlsym(Base.libopenlibm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(x::Float32)
        s = ccall(dlsym(Base.libopenlibm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(A::Array{Float64})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(Base.libopenlibm,:frexp), Float64, (Float64, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
    function frexp(A::Array{Float32})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(Base.libopenlibm,:frexpf), Float32, (Float32, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
end

modf(x) = rem(x,one(x)), trunc(x)

^(x::Float64, y::Float64) = ccall(dlsym(Base.libopenlibm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(Base.libopenlibm, :powf), Float32, (Float32,Float32), x, y)

^(x::Float64, y::Integer) = x^float64(y)
^(x::Float32, y::Integer) = x^float32(y)

end # module

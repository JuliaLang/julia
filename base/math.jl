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

sec(z) = 1 ./cos(z)
csc(z) = 1 ./sin(z)
cot(z) = 1 ./tan(z)
asec(y) = acos(1 ./y)
acsc(y) = asin(1 ./y)
acot(y) = atan(1 ./y)
sech(z) = 1 ./cosh(z)
csch(z) = 1 ./sinh(z)
coth(z) = 1 ./tanh(z)
asech(y) = acosh(1 ./y)
acsch(y) = asinh(1 ./y)
acoth(y) = atanh(1 ./y)

sinc(x::Number) = x==0 ? one(x)  : (pix=pi*x; oftype(x,sin(pix)/pix))
cosc(x::Number) = x==0 ? zero(x) : (pix=pi*x; oftype(x,cos(pix)/x-sin(pix)/(pix*x)))

radians2degrees(z::Real) = oftype(z, (180/pi) * z)
degrees2radians(z::Real) = oftype(z, (pi/180) * z)

cosd(z) = cos(degrees2radians(z))
cotd(z) = cot(degrees2radians(z))
cscd(z) = csc(degrees2radians(z))
secd(z) = sec(degrees2radians(z))
sind(z) = sin(degrees2radians(z))
tand(z) = tan(degrees2radians(z))

acosd(y) = radians2degrees(acos(y))
acotd(y) = radians2degrees(acot(y))
acscd(y) = radians2degrees(acsc(y))
asecd(y) = radians2degrees(asec(y))
asind(y) = radians2degrees(asin(y))
atand(y) = radians2degrees(atan(y))

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

macro func_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro func_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro func_1arg_int(T,f,name...)
    if length(name)>0
        fname = name[1]
    else
        fname = f
    end
    quote
        $(esc(fname))(x::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Int32, (Float64,), x)
        $(esc(fname))(x::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Int32, (Float32,), x)
        @vectorize_1arg $T $fname
    end
end

macro func_2arg(T,f)
    quote
        $(esc(f))(x::Float64, y::Float64) = ccall(dlsym(Base.libopenlibm,$(string(f))), Float64, (Float64, Float64,), x, y)
        $(esc(f))(x::Float32, y::Float32) = ccall(dlsym(Base.libopenlibm,$(string(f,"f"))), Float32, (Float32, Float32), x, y)
        @vectorize_2arg $T $f
    end
end

@func_1arg_float Number cbrt
@func_1arg_float Number sin
@func_1arg_float Number cos
@func_1arg_float Number tan
@func_1arg_float Number sinh
@func_1arg_float Number cosh
@func_1arg_float Number tanh
@func_1arg_float Number asin
@func_1arg_float Number acos
@func_1arg_float Number atan
@func_1arg_float Number asinh
@func_1arg_float Number acosh
@func_1arg_float Number atanh
@func_1arg_float Number log
@func_1arg_float Number log2
@func_1arg_float Number log10
@func_1arg_float Real   log1p
@func_1arg_float Real   logb
@func_1arg_float Number exp
@func_1arg_float Real   expm1
@func_1arg_float Number erf
@func_1arg_float Number erfc
@func_1arg_float Real   ceil
@func_1arg_float Real   floor
#@func_1arg_float Real   rint
@func_1arg_float Number lgamma

@func_1arg_float Number sqrt
@func_1arg_float Number exp2
#@func_1arg_float Real   nearbyint
@func_1arg_float Real   trunc
@func_1arg_float Real   round

@func_2arg Number atan2
atan2(x::Real, y::Real) = atan2(float64(x), float64(y))
@func_2arg Number hypot
hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

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

#@func_1arg_int Real lrint
#@func_1arg_int Real lround iround
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

@func_1arg_float Real significand

ldexp(x::Float64,e::Int) = ccall(dlsym(Base.libopenlibm, :ldexp),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall(dlsym(Base.libopenlibm, :ldexpf), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize does not do the right thing for these argument types
#@vectorize_2arg Real ldexp

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
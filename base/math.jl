module Math

export sin, cos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot, 
       sech, csch, coth, asech, acsch, acoth, sinc, cosc, 
       cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       radians2degrees, degrees2radians,
       log, log2, log10, log1p, logb, exp, exp2, expm1, 
       cbrt, sqrt, square, erf, erfc, erfcx, erfi, dawson,
       ceil, floor, trunc, round, significand, 
       lgamma, hypot, gamma, lfact, max, min, ilogb, ldexp, frexp,
       clamp, modf, ^, 
       airy, airyai, airyprime, airyaiprime, airybi, airybiprime,
       besselj0, besselj1, besselj, bessely0, bessely1, bessely,
       hankelh1, hankelh2, besseli, besselk, besselh,
       beta, lbeta, eta, zeta, digamma

import Base.log, Base.exp, Base.sin, Base.cos, Base.tan, Base.sinh, Base.cosh,
       Base.tanh, Base.asin, Base.acos, Base.atan, Base.asinh, Base.acosh,
       Base.atanh, Base.sqrt, Base.log2, Base.log10, Base.max, Base.min,
       Base.ceil, Base.floor, Base.trunc, Base.round, Base.^

import Intrinsics.nan_dom_err

# non-type specific math functions

clamp(x::Real, lo::Real, hi::Real) = (x > hi ? hi : (x < lo ? lo : x))
clamp{T<:Real}(x::AbstractArray{T,1}, lo::Real, hi::Real) = [clamp(xx, lo, hi) for xx in x]
clamp{T<:Real}(x::AbstractArray{T,2}, lo::Real, hi::Real) =
    [clamp(x[i,j], lo, hi) for i in 1:size(x,1), j in 1:size(x,2)]
clamp{T<:Real}(x::AbstractArray{T}, lo::Real, hi::Real) =
    reshape([clamp(xx, lo, hi) for xx in x], size(x))

sinc(x::Number) = x==0 ? one(x)  : (pix=pi*x; oftype(x,sin(pix)/pix))
cosc(x::Number) = x==0 ? zero(x) : (pix=pi*x; oftype(x,cos(pix)/x-sin(pix)/(pix*x)))

radians2degrees(z::Real) = oftype(z, (180/pi) * z)
degrees2radians(z::Real) = oftype(z, (pi/180) * z)
radians2degrees(z::Integer) = oftype(float(z), (180/pi) * z)
degrees2radians(z::Integer) = oftype(float(z), (pi/180) * z)

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

for (fd, f) in ((:sind, :sin), (:cosd, :cos), (:tand, :tan),
                (:secd, :sec), (:cscd, :csc), (:cotd, :cot))
    @eval begin
        ($fd)(z) = ($f)(degrees2radians(z))
    end
end

for (fd, f) in ((:asind, :asin), (:acosd, :acos), (:atand, :atan),
                (:asecd, :asec), (:acscd, :acsc), (:acotd, :acot))
    @eval begin
        ($fd)(y) = degrees2radians(($f)(y))
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

const libm = Base.libm_name
const openlibm_extras = "libopenlibm-extras"

# functions with no domain error
for f in (:cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :exp2)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Integer) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

# functions that return NaN on non-NaN argument for domain error
for f in (:sin, :cos, :tan, :asin, :acos, :acosh, :atanh, :log, :log2, :log10,
          :lgamma, :sqrt, :log1p)
    @eval begin
        ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)),libm), Float64, (Float64,), x), x)
        ($f)(x::Float32) = nan_dom_err(ccall(($(string(f,"f")),libm), Float32, (Float32,), x), x)
        ($f)(x::Integer) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

for f in (:logb, :expm1, :significand)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Integer) = ($f)(float(x))
        @vectorize_1arg Real $f
    end
end

for f in (:ceil, :trunc) # :rint, :nearbyint
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        @vectorize_1arg Real $f
    end
end

round(x::Float32) = ccall((:roundf, libm), Float32, (Float32,), x)
@vectorize_1arg Real round

floor(x::Float32) = ccall((:floorf, libm), Float32, (Float32,), x)
@vectorize_1arg Real floor

atan2(x::Real, y::Real) = atan2(float64(x), float64(y))

hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

for f in (:atan2, :hypot)
    @eval begin
        ($f)(x::Float64, y::Float64) = ccall(($(string(f)),libm), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32, Float32), x, y)
        @vectorize_2arg Number $f
    end
end

gamma(x::Float64) = nan_dom_err(ccall((:tgamma,libm),  Float64, (Float64,), x), x)
gamma(x::Float32) = float32(gamma(float64(x)))
gamma(x::Integer) = gamma(float(x))
@vectorize_1arg Number gamma

lfact(x::Real) = (x<=1 ? zero(float(x)) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

max(x::Float64, y::Float64) = ccall((:fmax,libm),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall((:fmaxf,libm), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall((:fmin,libm),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall((:fminf,libm), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

function ilogb(x::Float64)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogb,libm), Int32, (Float64,), x))
end
function ilogb(x::Float32)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogbf,libm), Int32, (Float32,), x))
end
@vectorize_1arg Real ilogb

ldexp(x::Float64,e::Int) = ccall((:ldexp,libm),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall((:ldexpf,libm), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize ldexp

begin
    local exp::Array{Int32,1} = zeros(Int32,1)
    global frexp
    function frexp(x::Float64)
        s = ccall((:frexp,libm), Float64, (Float64, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(x::Float32)
        s = ccall((:frexpf,libm), Float32, (Float32, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(A::Array{Float64})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:length(A)
            f[i] = ccall((:frexp,libm), Float64, (Float64, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
    function frexp(A::Array{Float32})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:length(A)
            f[i] = ccall((:frexpf,libm), Float32, (Float32, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
end

modf(x) = rem(x,one(x)), trunc(x)

^(x::Float64, y::Float64) = ccall((:pow,libm),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall((:powf,libm), Float32, (Float32,Float32), x, y)

^(x::Float64, y::Integer) = x^float64(y)
^(x::Float32, y::Integer) = x^float32(y)

# special functions

besselj0(x::Float64) = ccall((:j0,openlibm_extras),  Float64, (Float64,), x)
besselj0(x::Float32) = ccall((:j0f,openlibm_extras), Float32, (Float32,), x)
@vectorize_1arg Real besselj0
besselj1(x::Float64) = ccall((:j1,openlibm_extras),  Float64, (Float64,), x)
besselj1(x::Float32) = ccall((:j1f,openlibm_extras), Float32, (Float32,), x)
@vectorize_1arg Real besselj1

bessely0(x::Float64) = ccall((:y0,openlibm_extras),  Float64, (Float64,), x)
bessely0(x::Float32) = ccall((:y0f,openlibm_extras), Float32, (Float32,), x)
@vectorize_1arg Real bessely0
bessely1(x::Float64) = ccall((:y1,openlibm_extras),  Float64, (Float64,), x)
bessely1(x::Float32) = ccall((:y1f,openlibm_extras), Float32, (Float32,), x)
@vectorize_1arg Real bessely1

let
    const ai::Array{Float64,1} = Array(Float64,2)
    const ae::Array{Int32,1} = Array(Int32,2)
global airy
function airy(k::Int, z::Complex128)
    id = int32(k==1 || k==3)
    if k == 0 || k == 1
        ccall((:zairy_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z),
              &id, &1,
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        return complex(ai[1],ai[2])
    elseif k == 2 || k == 3
        ccall((:zbiry_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z),
              &id, &1,
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        return complex(ai[1],ai[2])
    else
        error("airy: invalid argument")
    end
end
end

airy(z) = airy(0,z)
@vectorize_1arg Number airy
airyprime(z) = airy(1,z)
@vectorize_1arg Number airyprime
airyai(z) = airy(0,z)
@vectorize_1arg Number airyai
airyaiprime(z) = airy(1,z)
@vectorize_1arg Number airyaiprime
airybi(z) = airy(2,z)
@vectorize_1arg Number airybi
airybiprime(z) = airy(3,z)
@vectorize_1arg Number airybiprime

airy(k::Number, x::FloatingPoint) = oftype(x, real(airy(k, complex(x))))
airy(k::Number, x::Integer) = airy(k, float(x))
airy(k::Number, z::Complex64) = complex64(airy(k, complex128(z)))
airy(k::Number, z::Complex) = airy(convert(Int,k), complex128(z))
@vectorize_2arg Number airy

let
    const cy::Array{Float64,1} = Array(Float64,2)
    const ae::Array{Int32,1} = Array(Int32,2)
    const wrk::Array{Float64,1} = Array(Float64,2)

    function _besselh(nu::Float64, k::Integer, z::Complex128)
        ccall((:zbesh_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &k, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besseli(nu::Float64, z::Complex128)
        ccall((:zbesi_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselj(nu::Float64, z::Complex128)
        ccall((:zbesj_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselk(nu::Float64, z::Complex128)
        ccall((:zbesk_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _bessely(nu::Float64, z::Complex128)
        ccall((:zbesy_,openlibm_extras), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(wrk,1),
              pointer(wrk,2), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    global besselh
    function besselh(nu::Float64, k::Integer, z::Complex128)
        if nu < 0
            s = (k == 1) ? 1 : -1
            return _besselh(-nu, k, z) * exp(-s*nu*im*pi)
        end
        return _besselh(nu, k, z)
    end

    global besseli
    function besseli(nu::Float64, z::Complex128)
        if nu < 0
            return _besseli(-nu,z) - 2_besselk(-nu,z)sin(pi*nu)/pi
        else
            return _besseli(nu, z)
        end
    end

    global besselj
    function besselj(nu::Float64, z::Complex128)
        if nu < 0
            return _besselj(-nu,z)cos(pi*nu) + _bessely(-nu,z)sin(pi*nu)
        else
            return _besselj(nu, z)
        end
    end

    function besselj(nu::Integer, x::FloatingPoint)
        if x == 0
            return (nu == 0) ? one(x) : zero(x)
        end
        if nu < 0
            nu = -nu
            x = -x
        end
        ans = _besselj(float64(nu), complex128(abs(x)))
        if (x < 0) && isodd(nu)
            ans = -ans
        end
        oftype(x, real(ans))
    end

    global besselk
    besselk(nu::Float64, z::Complex128) = _besselk(abs(nu), z)

    global bessely
    function bessely(nu::Float64, z::Complex128)
        if nu < 0
            return _bessely(-nu,z)cos(pi*nu) - _besselj(-nu,z)sin(pi*nu)
        else
            return _bessely(nu, z)
        end
    end
end

besselh(nu, z) = besselh(nu, 1, z)
besselh(nu::Real, k::Integer, z::Complex64) = complex64(besselh(float64(nu), k, complex128(z)))
besselh(nu::Real, k::Integer, z::Complex) = besselh(float64(nu), k, complex128(z))
besselh(nu::Real, k::Integer, x::Real) = besselh(float64(nu), k, complex128(x))
@vectorize_2arg Number besselh

besseli(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
besseli(nu::Real, z::Complex) = besseli(float64(nu), complex128(z))
besseli(nu::Real, x::Real) = besseli(float64(nu), complex128(x))
@vectorize_2arg Number besseli

function besselj(nu::FloatingPoint, x::FloatingPoint)
    ans = besselj(float64(nu), complex128(x))
    (x > 0) ? oftype(x, real(ans)) : ans
end

besselj(nu::Real, z::Complex64) = complex64(besselj(float64(nu), complex128(z)))
besselj(nu::Real, z::Complex) = besselj(float64(nu), complex128(z))
besselj(nu::Integer, x::Integer) = besselj(nu, float(x))
@vectorize_2arg Number besselj

besselk(nu::Real, z::Complex64) = complex64(besselk(float64(nu), complex128(z)))
besselk(nu::Real, z::Complex) = besselk(float64(nu), complex128(z))
besselk(nu::Real, x::Real) = besselk(float64(nu), complex128(x))
@vectorize_2arg Number besselk

bessely(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
bessely(nu::Real, z::Complex) = bessely(float64(nu), complex128(z))
bessely(nu::Real, x::Real) = bessely(float64(nu), complex128(x))
@vectorize_2arg Number bessely

hankelh1(nu, z) = besselh(nu, 1, z)
@vectorize_2arg Number hankelh1

hankelh2(nu, z) = besselh(nu, 2, z)
@vectorize_2arg Number hankelh2


function angle_restrict_symm(theta)
    P1 = 4 * 7.8539812564849853515625e-01
    P2 = 4 * 3.7748947079307981766760e-08
    P3 = 4 * 2.6951514290790594840552e-15

    y = 2*floor(theta/(2*pi))
    r = ((theta - y*P1) - y*P2) - y*P3
    if (r > pi)
        r -= (2*pi)
    end
    return r
end

const clg_coeff = [76.18009172947146,
                   -86.50532032941677,
                   24.01409824083091,
                   -1.231739572450155,
                   0.1208650973866179e-2,
                   -0.5395239384953e-5]

function clgamma_lanczos(z)
    sqrt2pi = 2.5066282746310005
    
    y = x = z
    temp = x + 5.5
    zz = log(temp)
    zz = zz * (x+0.5)
    temp -= zz
    ser = complex(1.000000000190015, 0)
    for j=1:6
        y += 1.0
        zz = clg_coeff[j]/y
        ser += zz
    end
    zz = sqrt2pi*ser / x
    return log(zz) - temp
end

function lgamma(z::Complex)
    if real(z) <= 0.5
        a = clgamma_lanczos(1-z)
        b = log(sin(pi * z))
        logpi = 1.14472988584940017
        z = logpi - b - a
    else
        z = clgamma_lanczos(z)
    end
    complex(real(z), angle_restrict_symm(imag(z)))
end

gamma(z::Complex) = exp(lgamma(z))

# Translation of psi.c from cephes
const digamma_EUL = 0.57721566490153286061
const digamma_coefs = [8.33333333333333333333e-2,-2.10927960927960927961e-2, 7.57575757575757575758e-3,
                      -4.16666666666666666667e-3, 3.96825396825396825397e-3,-8.33333333333333333333e-3,
                       8.33333333333333333333e-2]

function digamma(x::Float64)
    negative = false
    nz = 0.0

    if x <= 0.0
        negative = true
        q = x
        p = floor(q)
        if p == q
            return NaN
        end

        nz = q - p
        if nz != 0.5
            if nz > 0.5
                p += 1.0
                nz = q - p
            end
            nz = pi / tan(pi * nz)
        else
            nz = 0.0
        end
        x = 1.0 - x
    end

    if x <= 10.0 && x == floor(x)
        y = 0.0
        for i = 1:x-1
            y += 1.0 / i
        end
        y -= digamma_EUL

        if negative
            y -= nz
        end
        return y
    end

    w = 0.0
    while x < 10.0
        w += 1.0 / x
        x += 1.0
    end

    if x < 1.0e17
        z = 1.0 / (x*x)
        y = digamma_coefs[1]
        for j = 2:7
            y = y*z + digamma_coefs[j]
        end
        y *= z
    else
        y = 0.0
    end

    y = log(x) - 0.5/x - y - w

    if negative
        y -= nz
    end

    return y
end
digamma(x::Float32) = float32(digamma(float64(x)))
digamma(x::Real) = digamma(float64(x))
@vectorize_1arg Real digamma

beta(x::Number, w::Number) = exp(lgamma(x)+lgamma(w)-lgamma(x+w))
lbeta(x::Number, w::Number) = lgamma(x)+lgamma(w)-lgamma(x+w)
@vectorize_2arg Number beta
@vectorize_2arg Number lbeta

const eta_coeffs =
    [.99999999999999999997,
     -.99999999999999999821,
     .99999999999999994183,
     -.99999999999999875788,
     .99999999999998040668,
     -.99999999999975652196,
     .99999999999751767484,
     -.99999999997864739190,
     .99999999984183784058,
     -.99999999897537734890,
     .99999999412319859549,
     -.99999996986230482845,
     .99999986068828287678,
     -.99999941559419338151,
     .99999776238757525623,
     -.99999214148507363026,
     .99997457616475604912,
     -.99992394671207596228,
     .99978893483826239739,
     -.99945495809777621055,
     .99868681159465798081,
     -.99704078337369034566,
     .99374872693175507536,
     -.98759401271422391785,
     .97682326283354439220,
     -.95915923302922997013,
     .93198380256105393618,
     -.89273040299591077603,
     .83945793215750220154,
     -.77148960729470505477,
     .68992761745934847866,
     -.59784149990330073143,
     .50000000000000000000,
     -.40215850009669926857,
     .31007238254065152134,
     -.22851039270529494523,
     .16054206784249779846,
     -.10726959700408922397,
     .68016197438946063823e-1,
     -.40840766970770029873e-1,
     .23176737166455607805e-1,
     -.12405987285776082154e-1,
     .62512730682449246388e-2,
     -.29592166263096543401e-2,
     .13131884053420191908e-2,
     -.54504190222378945440e-3,
     .21106516173760261250e-3,
     -.76053287924037718971e-4,
     .25423835243950883896e-4,
     -.78585149263697370338e-5,
     .22376124247437700378e-5,
     -.58440580661848562719e-6,
     .13931171712321674741e-6,
     -.30137695171547022183e-7,
     .58768014045093054654e-8,
     -.10246226511017621219e-8,
     .15816215942184366772e-9,
     -.21352608103961806529e-10,
     .24823251635643084345e-11,
     -.24347803504257137241e-12,
     .19593322190397666205e-13,
     -.12421162189080181548e-14,
     .58167446553847312884e-16,
     -.17889335846010823161e-17,
     .27105054312137610850e-19]

function eta(z::Union(Float64,Complex128))
    if z == 0
        return oftype(z, 0.5)
    end
    re, im = reim(z)
    if im==0 && re < 0 && re==round(re/2)*2
        return zero(z)
    end
    reflect = false
    if re < 0.5
        z = 1-z
        reflect = true
    end
    s = zero(z)
    for n = length(eta_coeffs):-1:1
        c = eta_coeffs[n]
        p = n^-z
        s += c * p
    end
    if reflect
        z2 = 2.0^z
        b = 2.0 - (2.0*z2)
        f = z2 - 2
        piz = pi^z
        
        b = b/f/piz
        
        return s * gamma(z) * b * cos(pi/2*z)
    end
    return s
end

eta(x::Integer) = eta(float64(x))
eta(x::Real)    = oftype(x,eta(float64(x)))
eta(z::Complex) = oftype(z,eta(complex128(z)))
@vectorize_1arg Number eta

function zeta(z::Number)
    zz = 2^z
    eta(z) * zz/(zz-2)
end
@vectorize_1arg Number zeta

if WORD_SIZE == 64
# TODO: complex return only on 64-bit for now
for f in (:erf, :erfc, :erfcx, :erfi, :Dawson)
    fname = (f === :Dawson) ? :dawson : f
    @eval begin
        ($fname)(z::Complex128) = complex128(ccall(($(string("Faddeeva_",f)),openlibm_extras), ComplexPair{Float64}, (ComplexPair{Float64}, Float64), z, zero(Float64)))
        ($fname)(z::Complex64) = complex64(ccall(($(string("Faddeeva_",f)),openlibm_extras), ComplexPair{Float64}, (ComplexPair{Float64}, Float64), complex128(z), float64(eps(Float32))))
        ($fname)(z::Complex) = ($fname)(complex128(z))
    end
end
end
for f in (:erfcx, :erfi, :Dawson)
    fname = (f === :Dawson) ? :dawson : f
    @eval begin
        ($fname)(x::Float64) = ccall(($(string("Faddeeva_",f,"_re")),openlibm_extras), Float64, (Float64,), x)
        ($fname)(x::Float32) = float32(ccall(($(string("Faddeeva_",f,"_re")),openlibm_extras), Float64, (Float64,), float64(x)))
        ($fname)(x::Integer) = ($fname)(float(x))
        @vectorize_1arg Number $fname
    end
end

end # module

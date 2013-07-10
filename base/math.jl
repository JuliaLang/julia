module Math

export sin, cos, tan, sinh, cosh, tanh, asin, acos, atan,
       asinh, acosh, atanh, sec, csc, cot, asec, acsc, acot, 
       sech, csch, coth, asech, acsch, acoth, sinc, cosc, 
       cosd, cotd, cscd, secd, sind, tand,
       acosd, acotd, acscd, asecd, asind, atand, atan2,
       radians2degrees, degrees2radians,
       log, log2, log10, log1p, exponent, exp, exp2, exp10, expm1,
       cbrt, sqrt, erf, erfc, erfcx, erfi, dawson,
       ceil, floor, trunc, round, significand, 
       lgamma, hypot, gamma, lfact, max, min, ldexp, frexp,
       clamp, modf, ^, 
       airy, airyai, airyprime, airyaiprime, airybi, airybiprime,
       besselj0, besselj1, besselj, bessely0, bessely1, bessely,
       hankelh1, hankelh2, besseli, besselk, besselh,
       beta, lbeta, eta, zeta, polygamma, invdigamma, digamma, trigamma,
       erfinv, erfcinv

import Base: log, exp, sin, cos, tan, sinh, cosh, tanh, asin,
             acos, atan, asinh, acosh, atanh, sqrt, log2, log10,
             max, min, ceil, floor, trunc, round, ^, exp2, exp10

import Core.Intrinsics.nan_dom_err

# non-type specific math functions

clamp(x::Real, lo::Real, hi::Real) = (x > hi ? hi : (x < lo ? lo : x))
clamp{T<:Real}(x::AbstractArray{T,1}, lo::Real, hi::Real) = [clamp(xx, lo, hi) for xx in x]
clamp{T<:Real}(x::AbstractArray{T,2}, lo::Real, hi::Real) =
    [clamp(x[i,j], lo, hi) for i in 1:size(x,1), j in 1:size(x,2)]
clamp{T<:Real}(x::AbstractArray{T}, lo::Real, hi::Real) =
    reshape([clamp(xx, lo, hi) for xx in x], size(x))

sinc(x::Number) = x==0 ? one(x)  : (pix=pi*x; oftype(x,sin(pix)/pix))
sinc(x::Integer) = x==0 ? one(x) : zero(x)
sinc{T<:Integer}(x::Complex{T}) = sinc(complex(float(real(x)),float(imag(x))))
@vectorize_1arg Number sinc
cosc(x::Number) = x==0 ? zero(x) : (pix=pi*x; oftype(x,cos(pix)/x-sin(pix)/(pix*x)))
cosc(x::Integer) = cosc(float(x))
cosc{T<:Integer}(x::Complex{T}) = cosc(complex(float(real(x)),float(imag(x))))
@vectorize_1arg Number cosc

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
        ($fd)(y) = radians2degrees(($f)(y))
    end
end

log(b,x) = log(x)/log(b)

function hypot(x::Real, y::Real)
    x = abs(x)
    y = abs(y)
    if x < y
        x, y = y, x
    end
    if x == 0
        r = y/one(x)
    else
        r = y/x
    end
    x * sqrt(one(r)+r*r)
end

# type specific math functions

const libm = Base.libm_name
const openlibm_extras = "libopenlibm-extras"

# functions with no domain error
for f in (:cbrt, :sinh, :cosh, :tanh, :atan, :asinh, :exp, :erf, :erfc, :exp2, :expm1)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

# TODO: GNU libc has exp10 as an extension; should openlibm?
exp10(x::Float64) = 10.0^x
exp10(x::Float32) = 10.0f0^x
exp10(x::Integer) = exp10(float(x))
@vectorize_1arg Number exp10

# functions that return NaN on non-NaN argument for domain error
for f in (:sin, :cos, :tan, :asin, :acos, :acosh, :atanh, :log, :log2, :log10,
          :lgamma, :sqrt, :log1p)
    @eval begin
        ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)),libm), Float64, (Float64,), x), x)
        ($f)(x::Float32) = nan_dom_err(ccall(($(string(f,"f")),libm), Float32, (Float32,), x), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end

for f in (:ceil, :trunc, :significand) # :rint, :nearbyint
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

atan2(x::Real, y::Real) = atan2(float(x), float(y))

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
gamma(x::Float32) = nan_dom_err(ccall((:tgammaf,libm),  Float32, (Float32,), x), x)
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

function lgamma_r(x::Float64)
    signp = Array(Int32, 1)
    y = ccall((:lgamma_r,libm),  Float64, (Float64, Ptr{Int32}), x, signp)
    return y, signp[1]
end
function lgamma_r(x::Float32)
    signp = Array(Int32, 1)
    y = ccall((:lgamma_r,libm),  Float32, (Float32, Ptr{Int32}), x, signp)
    return y, signp[1]
end
lgamma_r(x::Real) = lgamma_r(float(x))

lfact(x::Real) = (x<=1 ? zero(float(x)) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

max(x::Float64, y::Float64) = ccall((:fmax,libm),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall((:fmaxf,libm), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall((:fmin,libm),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall((:fminf,libm), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

function exponent(x::Float64)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogb,libm), Int32, (Float64,), x))
end
function exponent(x::Float32)
    if x==0 || !isfinite(x)
        throw(DomainError())
    end
    int(ccall((:ilogbf,libm), Int32, (Float32,), x))
end
@vectorize_1arg Real exponent

ldexp(x::Float64,e::Int) = ccall((:scalbn,libm),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall((:scalbnf,libm), Float32, (Float32,Int32), x, int32(e))
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
airy(k::Number, x::Real) = airy(k, float(x))
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

# Derivatives of the digamma function
function psifn(x::Float64, n::Int, kode::Int, m::Int)
# Translated from http://www.netlib.org/slatec/src/dpsifn.f
# Note: Underflow handling at 380 in original is skipped
    const nmax = 100
    ans = Array(Float64, m)
#-----------------------------------------------------------------------
#             bernoulli numbers
#-----------------------------------------------------------------------
    const b =                         [1.00000000000000000e+00,
              -5.00000000000000000e-01,1.66666666666666667e-01,
              -3.33333333333333333e-02,2.38095238095238095e-02,
              -3.33333333333333333e-02,7.57575757575757576e-02,
              -2.53113553113553114e-01,1.16666666666666667e+00,
              -7.09215686274509804e+00,5.49711779448621554e+01,
              -5.29124242424242424e+02,6.19212318840579710e+03,
              -8.65802531135531136e+04,1.42551716666666667e+06,
              -2.72982310678160920e+07,6.01580873900642368e+08,
              -1.51163157670921569e+10,4.29614643061166667e+11,
              -1.37116552050883328e+13,4.88332318973593167e+14,
              -1.92965793419400681e+16]
    trm = Array(Float64, 22)
    trmr = Array(Float64, 100)
#***first executable statement  dpsifn
    if x <= 0.0 throw(DomainError()) end
    if n < 0 error("n must be non-negative") end
    if kode < 1 | kode > 2 error("kode must be one or two") end
    if m < 1 error("m must be larger than one") end
    mm = m
    const nx = min(-exponent(realmin(Float64)) + 1, exponent(realmax(Float64)))
    const r1m5 = log10(2)
    const r1m4 = Base.eps(Float64) * 0.5
    const wdtol = max(r1m4, 0.5e-18)
#-----------------------------------------------------------------------
#     elim = approximate exponential over and underflow limit
#-----------------------------------------------------------------------
    const elim = 2.302*(nx*r1m5 - 3.0)
    xln = log(x)
    nn = n + mm - 1
    fn = nn
    t = (fn + 1)*xln
#-----------------------------------------------------------------------
#     overflow and underflow test for small and large x
#-----------------------------------------------------------------------
    if abs(t) > elim
        if t <= 0.0 error("n too large") end
        error("Overflow, x too small or n+m-1 too large or both")
    end
    if x < wdtol
        ans[1] = x^(-n - 1)
        if mm != 1
            k = 1
            for i = 2:mm
                ans[k + 1] = ans[k]/x
                k += 1
            end
        end
        if n != 0 return ans end
        if kode == 2 ans[1] = ans[1] + xln end
        return ans
    end
#-----------------------------------------------------------------------
#     compute xmin and the number of terms of the series, fln+1
#-----------------------------------------------------------------------
    rln = r1m5 * get_precision(x)
    rln = min(rln, 18.06)
    fln = max(rln, 3.0) - 3.0
    yint = 3.50 + 0.40*fln
    slope = 0.21 + fln*(0.0006038*fln + 0.008677)
    xm = yint + slope*fn
    mx = itrunc(xm) + 1
    xmin = mx
    if n != 0
        xm = -2.302*rln - min(0.0,xln)
        arg = xm/n
        arg = min(0.0,arg)
        eps = exp(arg)
        xm = 1.0 - eps
        if abs(arg) < 1.0e-3 xm = -arg end
        fln = x*xm/eps
        xm = xmin - x
        if (xm > 7.0) & (fln < 15.0)
            nn = itrunc(fln) + 1
            np = n + 1
            t1 = (n + 1)*xln
            t = exp(-t1)
            s = t
            den = x
            for i = 1:nn
                den += 1.0
                trm[i] = den^(-np)
                s += trm[i]
            end
            ans[1] = s
            if n == 0
                if kode == 2 ans[1] = s + xln end
            end
            if mm == 1 return ans end
#-----------------------------------------------------------------------
#     generate higher derivatives, j.gt.n
#-----------------------------------------------------------------------
            tol = wdtol/5.0
            for j = 2:mm
                t = t/x
                s = t
                tols = t*tol
                den = x
                for i = 1:nn
                    den += 1.0
                    trm[i] = trm[i]/den
                    s += trm[i]
                    if trm[i] < tols break end
                end
                ans[j] = s
            end
            return ans
        end
    end
    
    xdmy = x
    xdmln = xln
    xinc = 0.0
    if x < xmin
        nx = itrunc(x)
        xinc = xmin - nx
        xdmy = x + xinc
        xdmln = log(xdmy)
    end
#-----------------------------------------------------------------------
#     generate w(n+mm-1,x) by the asymptotic expansion
#-----------------------------------------------------------------------
    t = fn*xdmln
    t1 = xdmln + xdmln
    t2 = t + xdmln
    tk = max(abs(t), abs(t1), abs(t2))
    if tk > elim error("Underflow") end
    tss = exp(-t)
    tt = 0.5/xdmy
    t1 = tt
    tst = wdtol*tt
    if nn != 0 t1 = tt + 1.0/fn end
    rxsq = 1.0/(xdmy*xdmy)
    ta = 0.5*rxsq
    t = (fn + 1)*ta
    s = t*b[3]
    if abs(s) >= tst
        tk = 2.0
        for k = 4:22
            t = t*((tk + fn + 1)/(tk + 1.0))*((tk + fn)/(tk + 2.0))*rxsq
            trm[k] = t*b[k]
            if abs(trm[k]) < tst break end
            s += trm[k]
            tk += 2.0
        end
    end
    s = (s + t1)*tss
    while true
        if xinc != 0.0
#-----------------------------------------------------------------------
#     backward recur from xdmy to x
#-----------------------------------------------------------------------
            nx = itrunc(xinc)
            np = nn + 1
            if nx > nmax error("n too large") end
            if nn == 0 break end
            xm = xinc - 1.0
            fx = x + xm
#-----------------------------------------------------------------------
#     this loop should not be changed. fx is accurate when x is small
#-----------------------------------------------------------------------
            for i = 1:nx
                trmr[i] = fx^(-np)
                s += trmr[i]
                xm -= 1.0
                fx = x + xm
            end
        end
        ans[mm] = s
        if fn == 0
            if kode != 2
                ans[1] = s - xdmln
                return ans
            end
            if xdmy == x return ans end
            xq = xdmy/x
            ans[1] = s - log(xq)
            return ans
        end
#-----------------------------------------------------------------------
#     generate lower derivatives, j.lt.n+mm-1
#-----------------------------------------------------------------------
        if mm == 1 return ans end
        for j = 2:mm
            fn -= 1
            tss *= xdmy
            t1 = tt
            if fn != 0 t1 = tt + 1.0/fn end
            t = (fn + 1)*ta
            s = t*b[3]
            if abs(s) >= tst
                tk = 4 + fn
                for k = 4:22 #110
                    trm[k] = trm[k]*(fn + 1)/tk
                    if abs(trm[k]) < tst break end
                    s += trm[k]
                    tk += 2.0
                end
            end
            s = (s + t1)*tss
            if xinc != 0.0
                if fn == 0 break end
                xm = xinc - 1.0
                fx = x + xm
                for i = 1:nx
                    trmr[i] = trmr[i]*fx
                    s += trmr[i]
                    xm -= 1.0
                    fx = x + xm
                end
            end
            mx = mm - j + 1
            ans[mx] = s
            if fn == 0
                if kode != 2
                    ans[1] = s - xdmln
                    return ans
                end
                if xdmy == x return ans end
                xq = xdmy/x
                ans[1] = s - log(xq)
                return ans
            end
        end
        if fn == 0 break end
        return ans
    end
#-----------------------------------------------------------------------
#     recursion for n = 0
#-----------------------------------------------------------------------
    for i = 1:nx
        s += 1.0/(x + nx - i)
    end
    if kode != 2
        ans[1] = s - xdmln
        return ans
    end
    if xdmy == x return ans end
    xq = xdmy/x
    ans[1] = s - log(xq)
    return ans
end
polygamma(k::Int, x::Float64) = (2rem(k,2) - 1)*psifn(x, k, 1, 1)[1]/gamma(k + 1)
polygamma(k::Int, x::Float32) = float32(polygamma(k, float64(x)))
polygamma(k::Int, x::Real) = polygamma(k, float64(x))

digamma(x::Real) = polygamma(0, x)
@vectorize_1arg Real digamma

trigamma(x::Real) = polygamma(1, x)
@vectorize_1arg Real trigamma

# Inverse digamma function
#
# Implementation of fixed point algorithm described in
#  "Estimating a Dirichlet distribution" by Thomas P. Minka, 2000
function invdigamma(y::Float64)
    # Closed form initial estimates
    if y >= -2.22
        x_old = exp(y) + 0.5
        x_new = x_old
    else
        x_old = -1.0 / (y - digamma(1.0))
        x_new = x_old
    end

    # Fixed point algorithm
    delta = Inf
    iteration = 0
    while delta > 1e-12 && iteration < 25
        iteration += 1
        x_new = x_old - (digamma(x_old) - y) / trigamma(x_old)
        delta = abs(x_new - x_old)
        x_old = x_new
    end

    return x_new
end
invdigamma(x::Float32) = float32(invdigamma(float64(x)))
invdigamma(x::Real) = invdigamma(float64(x))
@vectorize_1arg Real invdigamma

function beta(x::Number, w::Number)
    yx, sx = lgamma_r(x)
    yw, sw = lgamma_r(w)
    yxw, sxw = lgamma_r(x+w)
    return copysign(exp(yx + yw - yxw), sx*sw*sxw)
end
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
        ($fname)(z::Complex128) = complex128(ccall(($(string("Faddeeva_",f)),openlibm_extras), Complex{Float64}, (Complex{Float64}, Float64), z, zero(Float64)))
        ($fname)(z::Complex64) = complex64(ccall(($(string("Faddeeva_",f)),openlibm_extras), Complex{Float64}, (Complex{Float64}, Float64), complex128(z), float64(eps(Float32))))
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

# evaluate p[1] + x * (p[2] + x * (....)), i.e. a polynomial via Horner's rule
macro horner(x, p...)
    ex = p[end]
    for i = length(p)-1:-1:1
        ex = :($(p[i]) + $x * $ex)
    end
    ex
end

# Compute the inverse of the error function: erf(erfinv(x)) == x, 
# using the rational approximants tabulated in:
#     J. M. Blair, C. A. Edwards, and J. H. Johnson, "Rational Chebyshev 
#     approximations for the inverse of the error function," Math. Comp. 30,
#     pp. 827--830 (1976).
#         http://dx.doi.org/10.1090/S0025-5718-1976-0421040-7 
#         http://www.jstor.org/stable/2005402
function erfinv(x::Float64)
    a = abs(x)
    if a >= 1.0
        if x == 1.0
            return inf(Float64)
        elseif x == -1.0
            return -inf(Float64)
        end
        throw(DomainError())
    elseif a <= 0.75 # Table 17 in Blair et al.
        t = x*x - 0.5625
        return x * @horner(t, 0.16030_49558_44066_229311e2, 
                             -0.90784_95926_29603_26650e2,
                              0.18644_91486_16209_87391e3,
                             -0.16900_14273_46423_82420e3,
                              0.65454_66284_79448_7048e2,
                             -0.86421_30115_87247_794e1,
                              0.17605_87821_39059_0) /
                   @horner(t, 0.14780_64707_15138_316110e2,
                             -0.91374_16702_42603_13936e2,
                              0.21015_79048_62053_17714e3,
                             -0.22210_25412_18551_32366e3,
                              0.10760_45391_60551_23830e3,
                             -0.20601_07303_28265_443e2,
                              0.1e1)
    elseif a <= 0.9375 # Table 37 in Blair et al.
        t = x*x - 0.87890625
        return x * @horner(t, -0.15238_92634_40726_128e-1,
                               0.34445_56924_13612_5216,
                              -0.29344_39867_25424_78687e1,
                               0.11763_50570_52178_27302e2,
                              -0.22655_29282_31011_04193e2,
                               0.19121_33439_65803_30163e2,
                              -0.54789_27619_59831_8769e1,
                               0.23751_66890_24448) /
                   @horner(t, -0.10846_51696_02059_954e-1,
                               0.26106_28885_84307_8511,
                              -0.24068_31810_43937_57995e1,
                               0.10695_12997_33870_14469e2,
                              -0.23716_71552_15965_81025e2,
                               0.24640_15894_39172_84883e2,
                              -0.10014_37634_97830_70835e2,
                               0.1e1)
    else # Table 57 in Blair et al.
        t = 1.0 / sqrt(-log(1.0 - a))
        return @horner(t, 0.10501_31152_37334_38116e-3,
                          0.10532_61131_42333_38164_25e-1,
                          0.26987_80273_62432_83544_516,
                          0.23268_69578_89196_90806_414e1,
                          0.71678_54794_91079_96810_001e1,
                          0.85475_61182_21678_27825_185e1,
                          0.68738_08807_35438_39802_913e1,
                          0.36270_02483_09587_08930_02e1,
                          0.88606_27392_96515_46814_9) / 
              (copysign(t, x) *
               @horner(t, 0.10501_26668_70303_37690e-3,
                          0.10532_86230_09333_27531_11e-1,
                          0.27019_86237_37515_54845_553,
                          0.23501_43639_79702_53259_123e1,
                          0.76078_02878_58012_77064_351e1,
                          0.11181_58610_40569_07827_3451e2,
                          0.11948_78791_84353_96667_8438e2,
                          0.81922_40974_72699_07893_913e1,
                          0.40993_87907_63680_15361_45e1,
                          0.1e1))
    end
end

function erfinv(x::Float32)
    a = abs(x)
    if a >= 1.0f0
        if x == 1.0f0
            return inf(Float32)
        elseif x == -1.0f0
            return -inf(Float32)
        end
        throw(DomainError())
    elseif a <= 0.75f0 # Table 10 in Blair et al.
        t = x*x - 0.5625f0
        return x * @horner(t, -0.13095_99674_22f2,
                               0.26785_22576_0f2,
                              -0.92890_57365f1) /
                   @horner(t, -0.12074_94262_97f2,
                               0.30960_61452_9f2,
                              -0.17149_97799_1f2,
                               0.1f1)
    elseif a <= 0.9375f0 # Table 29 in Blair et al.
        t = x*x - 0.87890625f0
        return x * @horner(t, -0.12402_56522_1f0,
                               0.10688_05957_4f1,
                              -0.19594_55607_8f1,
                               0.42305_81357f0) /
                   @horner(t, -0.88276_97997f-1,
                               0.89007_43359f0,
                              -0.21757_03119_6f1,
                               0.1f1)
    else # Table 50 in Blair et al.
        t = 1.0f0 / sqrt(-log(1.0f0 - a))
        return @horner(t, 0.15504_70003_116f0,
                          0.13827_19649_631f1,
                          0.69096_93488_87f0,
                         -0.11280_81391_617f1,
                          0.68054_42468_25f0,
                         -0.16444_15679_1f0) /
              (copysign(t, x) *
               @horner(t, 0.15502_48498_22f0,
                          0.13852_28141_995f1,
                          0.1f1))
    end
end

erfinv(x::Integer) = erfinv(float(x))
@vectorize_1arg Real erfinv

# Inverse complementary error function: use Blair tables for y = 1-x, 
# exploiting the greater accuracy of y (vs. x) when y is small.
function erfcinv(y::Float64)
    if y > 0.0625
        return erfinv(1.0 - y)
    elseif y <= 0.0
        if y == 0.0
            return inf(Float64)
        end
        throw(DomainError())
    elseif y >= 1e-100 # Table 57 in Blair et al.
        t = 1.0 / sqrt(-log(y))
        return @horner(t, 0.10501_31152_37334_38116e-3,
                          0.10532_61131_42333_38164_25e-1,
                          0.26987_80273_62432_83544_516,
                          0.23268_69578_89196_90806_414e1,
                          0.71678_54794_91079_96810_001e1,
                          0.85475_61182_21678_27825_185e1,
                          0.68738_08807_35438_39802_913e1,
                          0.36270_02483_09587_08930_02e1,
                          0.88606_27392_96515_46814_9) / 
              (t *
               @horner(t, 0.10501_26668_70303_37690e-3,
                          0.10532_86230_09333_27531_11e-1,
                          0.27019_86237_37515_54845_553,
                          0.23501_43639_79702_53259_123e1,
                          0.76078_02878_58012_77064_351e1,
                          0.11181_58610_40569_07827_3451e2,
                          0.11948_78791_84353_96667_8438e2,
                          0.81922_40974_72699_07893_913e1,
                          0.40993_87907_63680_15361_45e1,
                          0.1e1))
    else # Table 80 in Blair et al.
        t = 1.0 / sqrt(-log(y))
        return @horner(t, 0.34654_29858_80863_50177e-9,
                          0.25084_67920_24075_70520_55e-6,
                          0.47378_13196_37286_02986_534e-4,
                          0.31312_60375_97786_96408_3388e-2,
                          0.77948_76454_41435_36994_854e-1,
                          0.70045_68123_35816_43868_271e0,
                          0.18710_42034_21679_31668_683e1,
                          0.71452_54774_31351_45428_3e0) /
          (t * @horner(t, 0.34654_29567_31595_11156e-9,
                          0.25084_69079_75880_27114_87e-6,
                          0.47379_53129_59749_13536_339e-4,
                          0.31320_63536_46177_68848_0813e-2,
                          0.78073_48906_27648_97214_733e-1,
                          0.70715_04479_95337_58619_993e0,
                          0.19998_51543_49112_15105_214e1,
                          0.15072_90269_27316_80008_56e1,
                          0.1e1))
    end
end

function erfcinv(y::Float32)
    if y > 0.0625f0
        return erfinv(1.0f0 - y)
    elseif y <= 0.0f0
        if y == 0.0f0
            return inf(Float32)
        end
        throw(DomainError())
    else # Table 50 in Blair et al.
        t = 1.0f0 / sqrt(-log(y))
        return @horner(t, 0.15504_70003_116f0,
                          0.13827_19649_631f1,
                          0.69096_93488_87f0,
                         -0.11280_81391_617f1,
                          0.68054_42468_25f0,
                         -0.16444_15679_1f0) /
        (t *
         @horner(t, 0.15502_48498_22f0,
                    0.13852_28141_995f1,
                    0.1f1))
    end
end

erfcinv(x::Integer) = erfcinv(float(x))
@vectorize_1arg Real erfcinv

end # module

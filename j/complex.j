## generic complex number definitions ##

abstract ComplexNum <: Number

iscomplex(x::ComplexNum) = true
iscomplex(x) = false

real_valued(z::ComplexNum)    = (imag(z) == 0)
integer_valued(z::ComplexNum) = (real_valued(z) && integer_valued(real(z)))

real(x::Real) = x
imag(x::Real) = convert(typeof(x), 0)

function show(c::ComplexNum)
    show(real(c))
    i = imag(c)
    if signbit(i) == -1
        i = -i
        print(" - ")
    else
        print(" + ")
    end
    show(i)
    print("im")
end

complex(re::Array, im::Array ) = reshape([ complex(re[i],im[i]) | i=1:numel(re) ], size(re))
complex(re::Array, im::Real  ) = reshape([ complex(re[i],im   ) | i=1:numel(re) ], size(re))
complex(re::Real , im::Array ) = reshape([ complex(re   ,im[i]) | i=1:numel(im) ], size(im))


## packed complex float types ##

bitstype 128 Complex128 <: ComplexNum

function complex128(r::Float64, i::Float64)
    box(Complex128,
        or_int(shl_int(zext_int(Complex128,unbox64(i)),unbox32(64)),
               zext_int(Complex128,unbox64(r))))
end

complex128(r::Real, i::Real) = complex128(float64(r),float64(i))

real(c::Complex128) = boxf64(trunc64(unbox(Complex128,c)))
imag(c::Complex128) = boxf64(trunc64(ashr_int(unbox(Complex128,c),
                                              unbox32(64))))

pi(z::Complex128) = pi(Float64)
pi(::Type{Complex128}) = pi(Float64)

convert(::Type{Complex128}, x::Real) = complex128(x,zero(x))
convert(::Type{Complex128}, z::ComplexNum) = complex128(real(z),imag(z))

promote_rule(::Type{Complex128}, ::Type{Float64}) = Complex128
promote_rule(::Type{Complex128}, ::Type{Float32}) = Complex128
promote_rule{S<:Real}(::Type{Complex128}, ::Type{S}) =
    (P = promote_type(Float64,S);
     is(P,Float64) ? Complex128 : Complex{P})


bitstype 64 Complex64 <: ComplexNum

function complex64(r::Float32, i::Float32)
    box(Complex64,
        or_int(shl_int(zext_int(Complex64,unbox32(i)),unbox32(32)),
               zext_int(Complex64,unbox32(r))))
end

complex64(r::Real, i::Real) = complex64(float32(r),float32(i))

real(c::Complex64) = boxf32(trunc32(unbox(Complex64,c)))
imag(c::Complex64) = boxf32(trunc32(ashr_int(unbox(Complex64,c),
                                             unbox32(32))))

pi(z::Complex64) = pi(Float32)
pi(::Type{Complex64}) = pi(Float32)

convert(::Type{Complex64}, x::Real) = complex64(x,zero(x))
convert(::Type{Complex64}, z::ComplexNum) = complex64(real(z),imag(z))

promote_rule(::Type{Complex64}, ::Type{Float64}) = Complex128
promote_rule(::Type{Complex64}, ::Type{Float32}) = Complex64
promote_rule{S<:Real}(::Type{Complex64}, ::Type{S}) =
    (P = promote_type(Float32,S);
     is(P,Float64) ? Complex128 :
     is(P,Float32) ? Complex64  : Complex{P})
promote_rule(::Type{Complex128}, ::Type{Complex64}) = Complex128


complex(x::Float64, y::Float64) = complex128(x, y)
complex(x::Float32, y::Float32) = complex64(x, y)
complex(x::Float, y::Float) = complex(promote(x,y)...)
complex(x::Float, y::Real) = complex(promote(x,y)...)
complex(x::Real, y::Float) = complex(promote(x,y)...)
complex(x::Float) = complex(x, zero(x))

im = complex128(0,1)


## complex with arbitrary component type ##

type Complex{T<:Real} <: ComplexNum
    re::T
    im::T
end
Complex(x::Real, y::Real) = Complex(promote(x,y)...)
Complex(x::Real) = Complex(x, zero(x))

real(z::Complex) = z.re
imag(z::Complex) = z.im

convert{T<:Real}(::Type{Complex{T}}, x::T) = Complex(x, convert(T,0))
convert{T<:Real}(::Type{Complex{T}}, x::Real) = Complex(convert(T,x), convert(T,0))
convert{T<:Real}(::Type{Complex{T}}, z::ComplexNum) = Complex(convert(T,real(z)),convert(T,imag(z)))

promote_rule{T<:Real}(::Type{Complex{T}}, ::Type{T}) = Complex{T}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{S}) = Complex{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{Complex{S}}) = Complex{promote_type(T,S)}
promote_rule{T<:Real}(::Type{Complex{T}}, ::Type{Complex128}) =
    (P = promote_type(Float64,T);
     is(P,Float64) ? Complex128 : Complex{P})
promote_rule{T<:Real}(::Type{Complex{T}}, ::Type{Complex64}) =
    (P = promote_type(Float32,T);
     is(P,Float64) ? Complex128 :
     is(P,Float32) ? Complex64  : Complex{P})

complex(x, y) = Complex(x, y)
complex(x) = Complex(x)

pi{T}(z::Complex{T}) = pi(T)
pi{T}(::Type{Complex{T}}) = pi(T)


## functions of complex numbers ##

==(z::ComplexNum, w::ComplexNum) = (real(z) == real(w) && imag(z) == imag(w))
==(z::ComplexNum, x::Real) = (real(z)==x && imag(z)==0)
==(x::Real, z::ComplexNum) = (real(z)==x && imag(z)==0)

isequal(z::ComplexNum, w::ComplexNum) =
    isequal(real(z),real(w)) && isequal(imag(z),imag(w))

hash(z::ComplexNum) = bitmix(hash(real(z)),hash(imag(z)))

conj(z::ComplexNum) = complex(real(z),-imag(z))
norm(z::ComplexNum) = square(real(z)) + square(imag(z))
abs(z::ComplexNum)  = hypot(real(z), imag(z))
inv(z::ComplexNum)  = conj(z)/norm(z)

-(z::ComplexNum) = complex(-real(z), -imag(z))
+(z::ComplexNum, w::ComplexNum) = complex(real(z) + real(w), imag(z) + imag(w))
-(z::ComplexNum, w::ComplexNum) = complex(real(z) - real(w), imag(z) - imag(w))
*(z::ComplexNum, w::ComplexNum) = complex(real(z) * real(w) - imag(z) * imag(w),
                                          real(z) * imag(w) + imag(z) * real(w))

/(z::Number, w::ComplexNum) = z*inv(w)
/(z::ComplexNum, x::Real) = complex(real(z)/x, imag(z)/x)

function /(a::ComplexNum, b::ComplexNum)
    are = real(a); aim = imag(a); bre = real(b); bim = imag(b)
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (1 + r*r)
        complex((are*r + aim)/den, (aim*r - are)/den)
    else
        r = bim / bre
        den = bre * (1 + r*r)
        complex((are + aim*r)/den, (aim - are*r)/den)
    end
end

function /(a::Real, b::ComplexNum)
    bre = real(b); bim = imag(b)
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (1 + r*r)
        complex(a*r/den, -a/den)
    else
        r = bim / bre
        den = bre * (1 + r*r)
        complex(a/den, -a*r/den)
    end
end

function sqrt(z::ComplexNum)
    r = sqrt(0.5(hypot(real(z),imag(z))+abs(real(z))))
    if real(z) >= 0
        return complex(r, 0.5*imag(z)/r)
    end
    return complex(0.5*abs(imag(z))/r, imag(z) >= 0 ? r : -r)
end

cis(theta::Real) = complex(cos(theta),sin(theta))
function cis(z::ComplexNum)
    v = 1/exp(imag(z))
    complex(v*cos(real(z)), v*sin(real(z)))
end

arg(z::ComplexNum) = atan2(imag(z), real(z))

function sin(z::ComplexNum)
    u = exp(imag(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    complex(u*sin(real(z)), v*cos(real(z)))
end

function cos(z::ComplexNum)
    u = exp(imag(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    complex(u*cos(real(z)), -v*sin(real(z)))
end

function log(z::ComplexNum)
    ar = abs(real(z))
    ai = abs(imag(z))
    if ar < ai
        r = ar/ai
        re = log(ai) + 0.5*log1p(r*r)
    else
        r = ai/ar
        re = log(ar) + 0.5*log1p(r*r)
    end
    complex(re, atan2(imag(z), real(z)))
end

function exp(z::ComplexNum)
    er = exp(real(z))
    complex(er*cos(imag(z)), er*sin(imag(z)))
end

^(x::Int, p::Float) = ^(promote(x,p)...)

^(z::ComplexNum, p::ComplexNum) = ^(promote(z,p)...)

^(z::Real, p::ComplexNum) = ^(promote(z,p)...)

^(z::ComplexNum, p::Float) = ^(promote(z,p)...)

function ^{T<:ComplexNum}(z::T, p::T)
    realp = real(p)
    if imag(p) == 0
        if realp == 0
            return one(z)
        elseif realp == 1
            return z
        elseif realp == 2
            return z*z
        elseif realp == 0.5
            return sqrt(z)
        end
    end
    r = abs(z)
    rp = r^realp
    realz = real(z)
    if imag(p) == 0
        ip = truncate(realp)
        if ip == realp
            # integer multiples of pi/2
            if imag(z) == 0 && realz < 0
                return complex(isodd(ip) ? -rp : rp, 0)
            elseif realz == 0 && imag(z) < 0
                if isodd(ip)
                    return complex(0, isodd(div(ip-1,2)) ? rp : -rp)
                else
                    return complex(isodd(div(ip,2)) ? -rp : rp, 0)
                end
            elseif realz == 0 && imag(z) > 0
                if isodd(ip)
                    return complex(0, isodd(div(ip-1,2)) ? -rp : rp)
                else
                    return complex(isodd(div(ip,2)) ? -rp : rp, 0)
                end
            end
        else
            dr = realp*2
            ip = truncate(dr)
            # 1/2 multiples of pi
            if ip == dr && imag(z) == 0
                if realz < 0
                    return complex(0, isodd(div(ip-1,2)) ? -rp : rp)
                elseif realz >= 0
                    return complex(rp, 0)
                end
            end
        end
    end
    theta = atan2(imag(z), realz)
    ntheta = realp*theta
    if imag(p) != 0
        rp = rp*exp(-imag(p)*theta)
        ntheta = ntheta + imag(p)*log(r)
    end
    complex(rp*cos(ntheta), rp*sin(ntheta))
end

function tan(z::ComplexNum)
    u = exp(imag(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    sinre = sin(real(z))
    cosre = cos(real(z))
    d = cosre*cosre + v*v
    complex(sinre*cosre/d, u*v/d)
end

function asin(z::ComplexNum)
    re = 1 - (real(z)*real(z) - imag(z)*imag(z))
    im = -2real(z)*imag(z)
    x = sqrt(complex(re,im))
    re = real(x) - imag(z)
    im = imag(x) + real(z)
    complex(atan2(im, re), -log(hypot(re, im)))
end

function acos(z::ComplexNum)
    re = 1 - (real(z)*real(z) - imag(z)*imag(z))
    im = -2real(z)*imag(z)
    x = sqrt(complex(re,im))
    re = real(z) - imag(x)
    im = imag(z) + real(x)
    complex(atan2(im, re), -log(hypot(re, im)))
end

function atan(z::ComplexNum)
    xsq = real(z)*real(z)
    ysq = imag(z)*imag(z)
    m1y = 1-imag(z)
    yp1 = 1+imag(z)
    m1ysq = m1y*m1y
    yp1sq = yp1*yp1
    complex(0.5(atan2(real(z),m1y) - atan2(-real(z),yp1)),
            0.25*log((yp1sq + xsq)/(xsq + m1ysq)))
end

function sinh(z::ComplexNum)
    u = exp(real(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    complex(v*cos(imag(z)), u*sin(imag(z)))
end

function cosh(z::ComplexNum)
    u = exp(real(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    complex(u*cos(imag(z)), v*sin(imag(z)))
end

function tanh(z::ComplexNum)
    cosim = cos(imag(z))
    u = exp(real(z))
    v = 1/u
    u = 0.5(u+v)
    v = u-v
    d = cosim*cosim + v*v
    complex(u*v/d, sin(imag(z))*cosim/d)
end

asinh(z::ComplexNum) = log(z + sqrt(z*z + 1))
acosh(z::ComplexNum) = log(z + sqrt(z*z - 1))
atanh(z::ComplexNum) = log(sqrt((1+z)/(1-z)))

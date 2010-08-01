struct Complex{T<:Real} <: Number
    re::T
    im::T

    Complex(x::Real, y::Real) = new(x, y)
    Complex(x::Real) = new(x, convert(typeof(x),0))
end

convert{T}(::Type{Complex{T}}, x::T) = Complex(x, convert(T,0))
convert{T}(::Type{Complex{T}}, x::Real) = Complex(convert(T,x), convert(T,0))
convert{T}(::Type{Complex{T}}, z::Complex) = Complex(convert(T,z.re),convert(T,z.im))

promote_rule{T}(::Type{Complex{T}}, ::Type{Real{T}}) = Complex{T}
promote_rule{T,S}(::Type{Complex{T}}, ::Type{Real{S}}) = Complex{promote_type(T,S)}
promote_rule{T,S}(::Type{Complex{T}}, ::Type{Complex{S}}) = Complex{promote_type(T,S)}

function print(c::Complex)
    print(re(c))
    i = im(c)
    if sign(i) == -1
        i = -i
        print(" - ")
    else
        print(" + ")
    end
    print(i)
    print("i")
end

iscomplex(x::Complex) = true
iscomplex(x) = false

re(z::Complex) = z.re
im(z::Complex) = z.im

conj(z::Complex) = Complex(z.re,-z.im)
norm(z::Complex) = z.re*z.re + z.im*z.im
abs(z::Complex) = hypot(z.re, z.im)
inv(z::Complex) = conj(z)/norm(z)

(-)(z::Complex) = Complex(-z.re, -z.im)
(+)(z::Complex, w::Complex) = Complex(z.re + w.re, z.im + w.im)
(-)(z::Complex, w::Complex) = Complex(z.re - w.re, z.im - w.im)
(*)(z::Complex, w::Complex) = Complex(z.re*w.re - z.im*w.im,
                                      z.re*w.im + z.im*w.re)

==(z::Complex, w::Complex) = (z.re == w.re && z.im == w.im)

(/)(z::Number, w::Complex) = z*inv(w)
(/)(z::Complex, x::Real) = Complex(z.re/x, z.im/x)

function (/)(a::Complex, b::Complex)
    are = a.re; aim = a.im; bre = b.re; bim = b.im
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (1 + r*r)
        Complex((are*r + aim)/den, (aim*r - are)/den)
    else
        r = bim / bre
        den = bre * (1 + r*r)
        Complex((are + aim*r)/den, (aim - are*r)/den)
    end
end

function (/)(a::Real, b::Complex)
    bre = b.re; bim = b.im
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (1 + r*r)
        Complex(a*r/den, -a/den)
    else
        r = bim / bre
        den = bre * (1 + r*r)
        Complex(a/den, -a*r/den)
    end
end

function sqrt(z::Complex)
    r = sqrt(0.5*(hypot(z.re,z.im)+abs(z.re)))
    if z.re >= 0
        return Complex(r, 0.5*z.im/r)
    end
    return Complex(0.5*abs(z.im)/r, z.im >= 0 ? r : -r)
end

cis(theta::Real) = Complex(cos(theta),sin(theta))
function cis(z::Complex)
    v = 1/exp(z.im)
    Complex(v cos(z.re), v sin(z.re))
end

arg(z::Complex) = atan2(z.im, z.re)

function sin(z::Complex)
    u = exp(z.im)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    Complex(u sin(z.re), v cos(z.re))
end

function cos(z::Complex)
    u = exp(z.im)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    Complex(u cos(z.re), -v sin(z.re))
end

function log(z::Complex)
    ar = abs(z.re)
    ai = abs(z.im)
    if ar < ai
        r = ar/ai
        re = log(ai) + 0.5 log1p(r*r)
    else
        r = ai/ar
        re = log(ar) + 0.5 log1p(r*r)
    end
    Complex(re, atan2(z.im, z.re))
end

function exp(z::Complex)
    er = exp(z.re)
    Complex(er cos(z.im), er sin(z.im))
end

(^)(x::Union(Int8,Uint8,Int16,Uint16,Int32,Uint32), p::Float) = float64(x)^p

function (^)(x::Float, p::Float)
    if x >= 0
        return pow(x, p)
    end
    if p == 0.5
        return sqrt(Complex(x))
    end
    return Complex(x)^Complex(p)
end

function (^){T}(z::Complex{T}, p::Complex)
    if p.im == 0
        if p.re == 0
            return convert(T,1)
        elseif p.re == 1
            return z
        elseif p.re == 2
            return z*z
        elseif p.re == 0.5
            return sqrt(z)
        end
    end
    r = abs(z)
    rp = r^p.re
    if p.im == 0
        ip = truncate(p.re)
        if ip == p.re
            # integer multiples of pi/2
            if z.im == 0 && z.re < 0
                return Complex(isodd(ip) ? -rp : rp, convert(T,0))
            elseif z.re == 0 && z.im < 0
                if isodd(ip)
                    return Complex(convert(T,0), isodd(div(ip-1,2)) ? rp : -rp)
                else
                    return Complex(isodd(div(ip,2)) ? -rp : rp, convert(T,0))
                end
            elseif z.re == 0 && z.im > 0
                if isodd(ip)
                    return Complex(convert(T,0), isodd(div(ip-1,2)) ? -rp : rp)
                else
                    return Complex(isodd(div(ip,2)) ? -rp : rp, convert(T,0))
                end
            end
        else
            dr = p.re*2
            ip = truncate(dr)
            # 1/2 multiples of pi
            if ip == dr && z.im == 0
                if z.re < 0
                    return Complex(convert(T,0), isodd(div(ip-1,2)) ? -rp : rp)
                elseif z.re >= 0
                    return Complex(rp, 0)
                end
            end
        end
    end
    theta = atan2(z.im, z.re)
    ntheta = p.re theta
    if p.im != 0
        rp = rp exp(-p.im theta)
        ntheta = ntheta + p.im log(r)
    end
    Complex(rp cos(ntheta), rp sin(ntheta))
end

function (^)(z::Real, p::Complex)
    if p.im == 0
        return z^p.re
    end
    (^)(promote(z,p)...)
end

function (^)(z::Complex, p::Float)
    if z.im == 0
        return z.re^p
    end
    (^)(promote(z,p)...)
end

function tan(z::Complex)
    u = exp(z.im)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    sinre = sin(z.re)
    cosre = cos(z.re)
    d = cosre cosre + v v
    Complex(sinre*cosre/d, u v/d)
end

function asin(z::Complex)
    re = 1 - (z.re*z.re - z.im*z.im)
    im = -2z.re*z.im
    x = sqrt(Complex(re,im))
    re = x.re - z.im
    im = x.im + z.re
    Complex(atan2(im, re), -log(hypot(re, im)))
end

function acos(z::Complex)
    re = 1 - (z.re*z.re - z.im*z.im)
    im = -2z.re*z.im
    x = sqrt(Complex(re,im))
    re = z.re - x.im
    im = z.im + x.re
    Complex(atan2(im, re), -log(hypot(re, im)))
end

function atan(z::Complex)
    xsq = z.re*z.re
    ysq = z.im*z.im
    m1y = 1-z.im
    yp1 = 1+z.im
    m1ysq = m1y*m1y
    yp1sq = yp1*yp1
    Complex(0.5*(atan2(z.re,m1y) - atan2(-z.re, yp1)),
            0.25*log((yp1sq + xsq)/(xsq + m1ysq)))
end

function sinh(z::Complex)
    y = exp(z.re)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    Complex(v cos(z.im), u sin(z.im))
end

function cosh(z::Complex)
    u = exp(z.re)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    Complex(u cos(z.im), v sin(z.im))
end

function tanh(z::Complex)
    cosim = cos(z.im)
    u = exp(z.re)
    v = 1/u
    u = 0.5*(u+v)
    v = u-v
    d = cosim*cosim + v*v
    Complex(u*v/d, sin(z.im)*cosim/d)
end

asinh(z::Complex) = log(z + sqrt(z*z + 1))
acosh(z::Complex) = log(z + sqrt(z*z - 1))
atanh(z::Complex) = log(sqrt((1+z)/(1-z)))

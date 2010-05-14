struct Complex{T} <: Number
    re::Real{T}
    im::Real{T}
end

convert{T}(::Type{Complex{T}}, x::T) = Complex(x, convert(T,0))
convert{T}(::Type{Complex{T}}, x::Real) = Complex(convert(T,x), convert(T,0))
convert{T}(::Type{Complex{T}}, z::Complex) = Complex(convert(T,z.re),convert(T,z.im))

promote_table{T,S}(::Type{Complex{T}}, ::Type{Real{S}}) = Complex{promote_type(T,S)}
promote_table{T,S}(::Type{Complex{T}}, ::Type{Complex{S}}) = Complex{promote_type(T,S)}

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

re(z::Complex) = z.re
im(z::Complex) = z.im

conj(z::Complex) = Complex(z.re,-z.im)
norm(z::Complex) = z.re*z.re + z.im*z.im
abs(z::Complex) = hypot(z.re, z.im)
inv(z::Complex) = conj(z)/norm(z)

(-)(z::Complex) = Complex(-z.re, -z.im)

(/)(z::Complex, x::Real) = Complex(z.re/x, z.im/x)

(+){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re + w.re, z.im + w.im)
(+){T}(z::Complex{T}, w::T)          = Complex(z.re + w   , z.im)
(+){T}(w::T, z::Complex{T})          = Complex(w + z.re   , z.im)

(-){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re - w.re, z.im - w.im)
(-){T}(z::Complex{T}, w::T)          = Complex(z.re - w   , z.im)
(-){T}(w::T, z::Complex{T})          = Complex(w - z.re   , -z.im)

(*){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re*w.re - z.im*w.im,
                                               z.re*w.im + z.im*w.re)
(*)(z::Complex, w::Real) = Complex(z.re*w, z.im*w)
(*)(w::Real, z::Complex) = Complex(w*z.re, w*z.im)

(/)(z::Number, w::Complex) = z*inv(w)

=={T}(z::Complex{T}, w::Complex{T})  = (z.re == w.re && z.im == w.im)
=={T}(z::Complex{T}, w::T)           = (z.re == w    && z.im == 0)
=={T}(w::T, z::Complex{T})           = (z.re == w    && z.im == 0)

function sqrt(z::Complex)
    r = sqrt((hypot(z.re, z.im)+abs(z.re))*0.5)
    if z.re >= 0.0
        return Complex(r, z.im/r*0.5)
    end
    return Complex(abs(z.im)/r*0.5, z.im >= 0.0 ? r : -r)
end

cis(theta::Real) = Complex(cos(theta),sin(theta))
function cis(z::Complex)
    v = 1/exp(z.im)
    Complex(v*cos(z.re), v*sin(z.re))
end

arg(z::Complex) = atan2(z.im, z.re)

function sin(z::Complex)
    u = exp(z.im)
    v = 1/u
    u = (u+v)*0.5
    v = u-v
    Complex(u*sin(z.re), v*cos(z.re))
end

function cos(z::Complex)
    u = exp(z.im)
    v = 1/u
    u = (u+v)*0.5
    v = u-v
    Complex(u*cos(z.re), -v*sin(z.re))
end

function log(z::Complex)
    ar = abs(z.re)
    ai = abs(z.im)
    if ar < ai
        r = ar/ai
        re = log(ai) + 0.5*log1p(r*r)
    else
        r = ai/ar
        re = log(ar) + 0.5*log1p(r*r)
    end
    Complex(re, atan2(z.im, z.re))
end

function exp(z::Complex)
    er = exp(z.re)
    Complex(er*cos(z.im), er*sin(z.im))
end

function (^){T}(x::Float{T}, p::Float{T})
    if x >= 0
        return pow(x, p)
    end
    r = convert(Complex{T},x)^convert(Complex{T},p)
    if r.im == 0
        return r.re
    end
    r
end

function (^)(z::Complex, p::Complex)
    r = abs(z)
    r_n = r^p.re
    theta = atan2(z.re, z.im)
    ntheta = p.re*theta
    re = r_n*cos(ntheta)
    im = r_n*sin(ntheta)
    if (p.im == 0)
        return Complex(re, im)
    end
    iz = cis(Complex(p.im*log(r), p.im*theta))
    return Complex(re,im)*iz
end

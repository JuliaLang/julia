immutable Complex{T<:Real} <: Number
    re::T
    im::T
end
Complex(x::Real, y::Real) = Complex(promote(x,y)...)
Complex(x::Real) = Complex(x, zero(x))

typealias Complex128 Complex{Float64}
typealias Complex64  Complex{Float32}
typealias ComplexPair Complex

real(z::Complex) = z.re
imag(z::Complex) = z.im
real(x::Real) = x
imag(x::Real) = zero(x)

convert{T<:Real}(::Type{Complex{T}}, x::Real) =
    Complex{T}(convert(T,x), convert(T,0))
convert{T<:Real}(::Type{Complex{T}}, z::Complex{T}) = z
convert{T<:Real}(::Type{Complex{T}}, z::Complex) =
    Complex{T}(convert(T,real(z)),convert(T,imag(z)))

convert{T<:Real}(::Type{T}, z::Complex) = (imag(z)==0 ? convert(T,real(z)) :
                                           throw(InexactError()))

promote_rule{T<:Real}(::Type{Complex{T}}, ::Type{T}) = Complex{T}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{S}) =
    Complex{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{Complex{S}}) =
    Complex{promote_type(T,S)}

complex(x, y) = Complex(x, y)
complex(x) = Complex(x)

complex128(r::Float64, i::Float64) = Complex{Float64}(r, i)
complex128(r::Real, i::Real) = complex128(float64(r),float64(i))
complex128(z) = complex128(real(z), imag(z))
complex64(r::Float32, i::Float32) = Complex{Float32}(r, i)
complex64(r::Real, i::Real) = complex64(float32(r),float32(i))
complex64(z) = complex64(real(z), imag(z))

iscomplex(x::Complex) = true
iscomplex(x::Number) = false

real_valued{T<:Real}(z::Complex{T}) = imag(z) == 0
integer_valued(z::Complex) = real_valued(z) && integer_valued(real(z))

isfinite(z::Complex) = isfinite(real(z)) && isfinite(imag(z))
reim(z) = (real(z), imag(z))

function complex_show(io::IO, z::Complex, compact::Bool)
    r, i = reim(z)
    if isnan(r) || isfinite(i)
        compact ? showcompact(io,r) : show(io,r)
        if signbit(i)==1 && !isnan(i)
            i = -i
            print(io, compact ? "-" : " - ")
        else
            print(io, compact ? "+" : " + ")
        end
        compact ? showcompact(io, i) : show(io, i)
        if !(isa(i,Integer) || isa(i,Rational) ||
             isa(i,FloatingPoint) && isfinite(i))
            print(io, "*")
        end
        print(io, "im")
    else
        print(io, "complex(",r,",",i,")")
    end
end
show(io::IO, z::Complex) = complex_show(io, z, false)
showcompact(io::IO, z::Complex) = complex_show(io, z, true)

function read{T<:Real}(s::IO, ::Type{Complex{T}})
    r = read(s,T)
    i = read(s,T)
    Complex{T}(r,i)
end
function write(s::IO, z::Complex)
    write(s,real(z))
    write(s,imag(z))
end


## singleton type for imaginary unit constant ##

type ImaginaryUnit <: Number end
const im = ImaginaryUnit()

iscomplex(::ImaginaryUnit) = true

convert{T<:Real}(::Type{Complex{T}}, ::ImaginaryUnit) = Complex{T}(zero(T),one(T))
convert(::Type{Complex}, ::ImaginaryUnit) = Complex(real(im),imag(im))

real(::ImaginaryUnit) = int32(0)
imag(::ImaginaryUnit) = int32(1)

promote_rule{T<:Complex}(::Type{ImaginaryUnit}, ::Type{T}) = T
promote_rule{T<:Real}(::Type{ImaginaryUnit}, ::Type{T}) = Complex{T}


## generic functions of complex numbers ##

convert(::Type{Complex}, z::Complex) = z
convert(::Type{Complex}, x::Real) = complex(x)

==(z::Complex, w::Complex) = real(z) == real(w) && imag(z) == imag(w)
==(z::Complex, x::Real) = real_valued(z) && real(z) == x
==(x::Real, z::Complex) = real_valued(z) && real(z) == x

isequal(z::Complex, w::Complex) = isequal(real(z),real(w)) && isequal(imag(z),imag(w))
isequal(z::Complex, x::Real) = real_valued(z) && isequal(real(z),x)
isequal(x::Real, z::Complex) = real_valued(z) && isequal(real(z),x)

hash(z::Complex) = (r = hash(real(z)); real_valued(z) ? r : bitmix(r,hash(imag(z))))

eps(z::Complex) = eps(abs(z))

conj(z::Complex) = complex(real(z),-imag(z))
abs(z::Complex)  = hypot(real(z), imag(z))
abs2(z::Complex) = real(z)*real(z) + imag(z)*imag(z)
inv(z::Complex)  = conj(z)/abs2(z)
sign(z::Complex) = z/abs(z)

-(z::Complex) = complex(-real(z), -imag(z))
+(z::Complex, w::Complex) = complex(real(z) + real(w), imag(z) + imag(w))
-(z::Complex, w::Complex) = complex(real(z) - real(w), imag(z) - imag(w))
*(z::Complex, w::Complex) = complex(real(z) * real(w) - imag(z) * imag(w),
                                    real(z) * imag(w) + imag(z) * real(w))
*(x::Real, z::Complex) = complex(x * real(z), x * imag(z))
*(z::Complex, x::Real) = complex(x * real(z), x * imag(z))

# multiplying by im is common
*(z::ImaginaryUnit, w::ImaginaryUnit) = complex(-imag(z), real(z))
*(z::ImaginaryUnit, x::Real)    = complex(zero(x), x)
*(x::Real, z::ImaginaryUnit)    = complex(zero(x), x)
*(z::ImaginaryUnit, w::Complex) = complex(-imag(w), real(w))
*(w::Complex, z::ImaginaryUnit) = complex(-imag(w), real(w))

/(z::Number, w::Complex) = z*inv(w)
/(z::Complex, x::Real) = complex(real(z)/x, imag(z)/x)

function /(a::Complex, b::Complex)
    are = real(a); aim = imag(a); bre = real(b); bim = imag(b)
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (one(r) + r*r)
        complex((are*r + aim)/den, (aim*r - are)/den)
    else
        r = bim / bre
        den = bre * (one(r) + r*r)
        complex((are + aim*r)/den, (aim - are*r)/den)
    end
end

function /(a::Real, b::Complex)
    bre = real(b); bim = imag(b)
    abr = abs(bre)
    abi = abs(bim)
    if abr <= abi
        r = bre / bim
        den = bim * (one(r) + r*r)
        complex(a*r/den, -a/den)
    else
        r = bim / bre
        den = bre * (one(r) + r*r)
        complex(a/den, -a*r/den)
    end
end

function sqrt(z::Complex)
    rz = float(real(z))
    iz = float(imag(z))
    r = sqrt((hypot(rz,iz)+abs(rz))/2)
    if r == 0
        return complex(zero(iz), iz)
    end
    if rz >= 0
        return complex(r, iz/r/2)
    end
    return complex(abs(iz)/r/2, iz >= 0 ? r : -r)
end

cis(theta::Real) = complex(cos(theta),sin(theta))
function cis(z::Complex)
    v = 1/exp(imag(z))
    complex(v*cos(real(z)), v*sin(real(z)))
end

angle(z::Complex) = atan2(imag(z), real(z))

function sin(z::Complex)
    u = exp(imag(z))
    v = 1/u
    rz = real(z)
    u = (u+v)/2
    v = u-v
    complex(u*sin(rz), v*cos(rz))
end

function cos(z::Complex)
    u = exp(imag(z))
    v = 1/u
    rz = real(z)
    u = (u+v)/2
    v = u-v
    complex(u*cos(rz), -v*sin(rz))
end

function log(z::Complex)
    ar = abs(real(z))
    ai = abs(imag(z))
    if ar < ai
        r = ar/ai
        re = log(ai) + log1p(r*r)/2
    else
        if ar == 0
            re = -inv(ar)
        else
            r = ai/ar
            re = log(ar) + log1p(r*r)/2
        end
    end
    complex(re, atan2(imag(z), real(z)))
end

log10(z::Complex) = log(z)/oftype(real(z),2.302585092994046)
log2(z::Complex) = log(z)/oftype(real(z),0.6931471805599453)

function exp(z::Complex)
    er = exp(real(z))
    complex(er*cos(imag(z)), er*sin(imag(z)))
end

function ^{T<:Complex}(z::T, p::T)
    realp = real(p); imagp = imag(p)
    realz = real(z); imagz = imag(z)
    r = abs(z)
    rp = r^realp
    theta = atan2(imagz, realz)
    ntheta = realp*theta
    if imagp != 0 && r != 0
        rp = rp*exp(-imagp*theta)
        ntheta = ntheta + imagp*log(r)
    end
    cosntheta = cos(ntheta)
    sinntheta = sin(ntheta)
    re, im = rp*cosntheta, rp*sinntheta
    if isinf(rp)
        if isnan(re)
            re = copysign(zero(re), cosntheta)
        end
        if isnan(im)
            im = copysign(zero(im), sinntheta)
        end
    end

    # apply some corrections to force known zeros
    if imagp == 0
        ip = itrunc(realp)
        if ip == realp
            if imagz == 0
                im = copysign(zero(im), im)
            elseif realz == 0
                if isodd(ip)
                    re = copysign(zero(re), re)
                else
                    im = copysign(zero(im), im)
                end
            end
        else
            dr = realp*2
            ip = itrunc(dr)
            if ip == dr && imagz == 0
                if realz < 0
                    re = copysign(zero(re), re)
                else
                    im = copysign(zero(im), im)
                end
            end
        end
    end

    complex(re, im)
end

function tan(z::Complex)
    u = exp(imag(z))
    v = 1/u
    u = (u+v)/2
    v = u-v
    sinre = sin(real(z))
    cosre = cos(real(z))
    d = cosre*cosre + v*v
    complex(sinre*cosre/d, u*v/d)
end

function asin(z::Complex)
    re = 1 - (real(z)*real(z) - imag(z)*imag(z))
    im = -2real(z)*imag(z)
    x = sqrt(complex(re,im))
    re = real(x) - imag(z)
    im = imag(x) + real(z)
    complex(atan2(im, re), -log(hypot(re, im)))
end

function acos(z::Complex)
    re = 1 - (real(z)*real(z) - imag(z)*imag(z))
    im = -2real(z)*imag(z)
    x = sqrt(complex(re,im))
    re = real(z) - imag(x)
    im = imag(z) + real(x)
    complex(atan2(im, re), -log(hypot(re, im)))
end

function atan(z::Complex)
    xsq = real(z)*real(z)
    ysq = imag(z)*imag(z)
    m1y = 1-imag(z)
    yp1 = 1+imag(z)
    m1ysq = m1y*m1y
    yp1sq = yp1*yp1
    complex((atan2(real(z),m1y) - atan2(-real(z),yp1))/2,
            log((yp1sq + xsq)/(xsq + m1ysq))/4)
end

function sinh(z::Complex)
    u = exp(real(z))
    v = 1/u
    u = (u+v)/2
    v = u-v
    complex(v*cos(imag(z)), u*sin(imag(z)))
end

function cosh(z::Complex)
    u = exp(real(z))
    v = 1/u
    u = (u+v)/2
    v = u-v
    complex(u*cos(imag(z)), v*sin(imag(z)))
end

function tanh(z::Complex)
    cosim = cos(imag(z))
    u = exp(real(z))
    v = 1/u
    u = (u+v)/2
    v = u-v
    d = cosim*cosim + v*v
    complex(u*v/d, sin(imag(z))*cosim/d)
end

asinh(z::Complex) = log(z + sqrt(z*z + 1))
acosh(z::Complex) = log(z + sqrt(z*z - 1))
atanh(z::Complex) = log(sqrt((1+z)/(1-z)))

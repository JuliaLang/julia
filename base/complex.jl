## generic complex number definitions ##

abstract Complex{T<:Real} <: Number

iscomplex(x::Complex) = true
iscomplex(x::Number) = false

real_valued{T<:Real}(z::Complex{T}) = imag(z) == 0
integer_valued(z::Complex) = real_valued(z) && integer_valued(real(z))

real(x::Real) = x
imag(x::Real) = zero(x)

isfinite(z::Complex) = isfinite(real(z)) && isfinite(imag(z))
reim(z) = (real(z), imag(z))

function complex_show(io, z::Complex, compact::Bool)
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

convert{T<:Real}(::Type{T}, z::Complex) = (imag(z)==0 ? convert(T,real(z)) :
                                           throw(InexactError()))

## packed complex float types ##

bitstype 128 Complex128 <: Complex{Float64}

function complex128(r::Float64, i::Float64)
    box(Complex128,
        or_int(shl_int(zext_int(Complex128,unbox(Float64,i)), 64),
               zext_int(Complex128,unbox(Float64,r))))
end

complex128(r::Real, i::Real) = complex128(float64(r),float64(i))
complex128(z) = complex128(real(z), imag(z))

real(c::Complex128) = box(Float64,trunc_int(Int64,c))
imag(c::Complex128) = box(Float64,trunc_int(Int64,ashr_int(c, 64)))

convert(::Type{Complex128}, x::Real) = complex128(x,0)
convert(::Type{Complex128}, z::Complex128) = z
convert(::Type{Complex128}, z::Complex) = complex128(real(z),imag(z))

promote_rule(::Type{Complex128}, ::Type{Float64}) = Complex128
promote_rule(::Type{Complex128}, ::Type{Float32}) = Complex128
promote_rule{S<:Integer}(::Type{Complex128}, ::Type{S}) = Complex128
promote_rule{S<:Real}(::Type{Complex128}, ::Type{S}) =
    (P = promote_type(Float64,S);
     is(P,Float64) ? Complex128 : ComplexPair{P})

function read(s, ::Type{Complex128})
    r = read(s,Float64)
    i = read(s,Float64)
    complex128(r,i)
end
function write(s::IO, z::Complex128)
    write(s,real(z))
    write(s,imag(z))
end

sizeof(::Type{Complex128}) = 16

bitstype 64 Complex64 <: Complex{Float32}

function complex64(r::Float32, i::Float32)
    box(Complex64,
        or_int(shl_int(zext_int(Complex64,unbox(Float32,i)), 32),
               zext_int(Complex64,unbox(Float32,r))))
end

complex64(r::Real, i::Real) = complex64(float32(r),float32(i))
complex64(z) = complex64(real(z), imag(z))

real(c::Complex64) = box(Float32,trunc_int(Int32,c))
imag(c::Complex64) = box(Float32,trunc_int(Int32,ashr_int(c, 32)))

convert(::Type{Complex64}, x::Real) = complex64(x,0)
convert(::Type{Complex64}, z::Complex64) = z
convert(::Type{Complex64}, z::Complex) = complex64(real(z),imag(z))

promote_rule(::Type{Complex64}, ::Type{Float64}) = Complex128
promote_rule(::Type{Complex64}, ::Type{Float32}) = Complex64
promote_rule{S<:Integer}(::Type{Complex64}, ::Type{S}) = Complex64
promote_rule{S<:Real}(::Type{Complex64}, ::Type{S}) =
    (P = promote_type(Float32,S);
     is(P,Float64) ? Complex128 :
     is(P,Float32) ? Complex64  : ComplexPair{P})
promote_rule(::Type{Complex128}, ::Type{Complex64}) = Complex128

function read(s, ::Type{Complex64})
    r = read(s,Float32)
    i = read(s,Float32)
    complex64(r,i)
end
function write(s, z::Complex64)
    write(s,real(z))
    write(s,imag(z))
end

sizeof(::Type{Complex64}) = 8

complex(x::Float64, y::Float64) = complex128(x, y)
complex(x::Float32, y::Float32) = complex64(x, y)
complex(x::FloatingPoint, y::FloatingPoint) = complex(promote(x,y)...)
complex(x::FloatingPoint, y::Real) = complex(promote(x,y)...)
complex(x::Real, y::FloatingPoint) = complex(promote(x,y)...)
complex(x::FloatingPoint) = complex(x, zero(x))


## complex with arbitrary component type ##

type ComplexPair{T<:Real} <: Complex{T}
    re::T
    im::T
end
ComplexPair(x::Real, y::Real) = ComplexPair(promote(x,y)...)
ComplexPair(x::Real) = ComplexPair(x, zero(x))

real(z::ComplexPair) = z.re
imag(z::ComplexPair) = z.im

convert{T<:Real}(::Type{ComplexPair{T}}, x::Real) =
    ComplexPair(convert(T,x), convert(T,0))
convert{T<:Real}(::Type{ComplexPair{T}}, z::ComplexPair{T}) = z
convert{T<:Real}(::Type{ComplexPair{T}}, z::Complex) =
    ComplexPair(convert(T,real(z)),convert(T,imag(z)))

promote_rule{T<:Real}(::Type{ComplexPair{T}}, ::Type{T}) =
    ComplexPair{T}
promote_rule{T<:Real,S<:Real}(::Type{ComplexPair{T}}, ::Type{S}) =
    ComplexPair{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{ComplexPair{T}}, ::Type{ComplexPair{S}}) =
    ComplexPair{promote_type(T,S)}
promote_rule{T<:Real}(::Type{ComplexPair{T}}, ::Type{Complex128}) =
    (P = promote_type(Float64,T);
     is(P,Float64) ? Complex128 : ComplexPair{P})
promote_rule{T<:Real}(::Type{ComplexPair{T}}, ::Type{Complex64}) =
    (P = promote_type(Float32,T);
     is(P,Float64) ? Complex128 : is(P,Float32) ? Complex64  : ComplexPair{P})

complex(x, y) = ComplexPair(x, y)
complex(x) = ComplexPair(x)


## singleton type for imaginary unit constant ##

type ImaginaryUnit <: Complex{Int32}; end
const im = ImaginaryUnit()

convert{T<:Real}(::Type{ComplexPair{T}}, ::ImaginaryUnit) =
    ComplexPair(zero(T),one(T))
convert(::Type{Complex128}, ::ImaginaryUnit) = complex128(0,1)
convert(::Type{Complex64},  ::ImaginaryUnit) = complex64(0,1)

real(::ImaginaryUnit) = int32(0)
imag(::ImaginaryUnit) = int32(1)

promote_rule{T<:Complex}(::Type{ImaginaryUnit}, ::Type{T}) = T
promote_rule{T<:Real}(::Type{ImaginaryUnit}, ::Type{T}) = ComplexPair{T}
promote_rule(::Type{ImaginaryUnit}, ::Type{Float64}) = Complex128
promote_rule(::Type{ImaginaryUnit}, ::Type{Float32}) = Complex64


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

immutable Imaginary{T<:Real} <: Number
    im::T
end
const im = Imaginary(true)

immutable Complex{T<:Real} <: Number
    re::T
    im::T
end
Complex(x::Real, y::Real) = Complex(promote(x,y)...)
Complex(x::Real) = Complex(x, zero(x))
Complex(i::Imaginary) = Complex(zero(imag(i)), imag(i))

typealias Complex128 Complex{Float64}
typealias Complex64  Complex{Float32}
typealias Complex32  Complex{Float16}

sizeof(::Type{Complex128}) = 16
sizeof(::Type{Complex64}) = 8
sizeof(::Type{Complex32}) = 4

real(z::Complex) = z.re
imag(z::Complex) = z.im
real(x::Real) = x
imag(x::Real) = zero(x)

convert{T<:Real}(::Type{Complex{T}}, z::Complex{T}) = z
convert{T<:Real}(::Type{Complex{T}}, x::Real) = Complex{T}(x,0)
convert{T<:Real}(::Type{Complex{T}}, z::Complex) = Complex{T}(real(z),imag(z))
convert{T<:Real}(::Type{T}, z::Complex) =
    imag(z)==0 ? convert(T,real(z)) : throw(InexactError())

complex(x, y) = Complex(x, y)
complex(x) = Complex(x)
complex(z::Complex) = z

complex128(r::Float64, i::Float64) = Complex{Float64}(r, i)
complex128(r::Real, i::Real) = complex128(float64(r),float64(i))
complex128(z) = complex128(real(z), imag(z))
complex64(r::Float32, i::Float32) = Complex{Float32}(r, i)
complex64(r::Real, i::Real) = complex64(float32(r),float32(i))
complex64(z) = complex64(real(z), imag(z))
complex32(r::Float16, i::Float16) = Complex{Float16}(r, i)
complex32(r::Real, i::Real) = complex32(float16(r),float16(i))
complex32(z) = complex32(real(z), imag(z))

for fn in _numeric_conversion_func_names
    @eval $fn(z::Complex) = complex($fn(real(z)),$fn(imag(z)))
end

sizeof{T<:Real}(::Type{Imaginary{T}}) = sizeof(T)
sizeof{T<:Real}(::Type{Complex{T}}) = 2*sizeof(T)

real(i::Imaginary) = zero(i.im)
imag(i::Imaginary) = i.im
real(z::Complex) = z.re
imag(z::Complex) = z.im
real(x::Real) = x
imag(x::Real) = zero(x)
reim(x::Number) = (real(x), imag(x))

isreal(x::Number) = imag(x) == 0
isimag(x::Number) = real(x) == 0
isinteger(z::Union(Imaginary,Complex)) = isreal(z) && isinteger(real(z))

isfinite(z::Imaginary) = isfinite(imag(z))
isfinite(z::Complex) = isfinite(real(z)) && isfinite(imag(z))

function imaginary_show(io::IO, i::Imaginary, compact::Bool)
    compact ? showcompact(io,i.im) : show(io,i.im)
    if !(isa(i.im,Integer) || isa(i.im,FloatingPoint) && isfinite(i.im))
        print(io, "*")
    end
    print(io, "im")
end
imaginary_show(io::IO, i::Imaginary{Bool}, compact::Bool) =
    print(io, i.im ? "im" : "false*im")
show(io::IO, i::Imaginary) = imaginary_show(io, i, false)
showcompact(io::IO, i::Imaginary) = imaginary_show(io, i, true)

function complex_show(io::IO, z::Complex, compact::Bool)
    r, i = reim(z)
    compact ? showcompact(io,r) : show(io,r)
    if signbit(i)==1 && !isnan(i)
        i = -i
        print(io, compact ? "-" : " - ")
    else
        print(io, compact ? "+" : " + ")
    end
    compact ? showcompact(io,i) : show(io,i)
    if !(isa(i,Integer) && !isa(i,Bool) || isa(i,FloatingPoint) && isfinite(i))
        print(io, "*")
    end
    print(io, "im")
end
show(io::IO, z::Complex) = complex_show(io, z, false)
showcompact(io::IO, z::Complex) = complex_show(io, z, true)

read{T<:Real}(s::IO, ::Type{Imaginary{T}}) = Imaginary{T}(read(s,T))
read{T<:Real}(s::IO, ::Type{Complex{T}}) = Complex{T}(read(s,T),read(s,T))

write(s::IO, i::Imaginary) = write(s,i.im)
write(s::IO, z::Complex) = write(s,real(z),imag(z))

convert{T<:Real}(::Type{Imaginary{T}}, i::Imaginary) = Imaginary{T}(imag(i))

convert{T<:Real}(::Type{Complex{T}}, x::Real)      = Complex{T}(real(x), zero(T))
convert{T<:Real}(::Type{Complex{T}}, i::Imaginary) = Complex{T}(zero(T), imag(i))
convert{T<:Real}(::Type{Complex{T}}, z::Complex)   = Complex{T}(real(z), imag(z))

convert{T<:Real}(::Type{T}, z::Union(Imaginary,Complex)) =
    isreal(z) ? convert(T,real(z)) : throw(InexactError())

promote_rule{T<:Real,S<:Real}(::Type{Imaginary{T}}, ::Type{Imaginary{S}}) =
    Imaginary{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Imaginary{T}}, ::Type{S}) =
    Complex{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{S}) =
    Complex{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{Imaginary{S}}) =
    Complex{promote_type(T,S)}
promote_rule{T<:Real,S<:Real}(::Type{Complex{T}}, ::Type{Complex{S}}) =
    Complex{promote_type(T,S)}

convert(::Type{Complex}, z::Complex)   = z
convert(::Type{Complex}, x::Real)      = Complex(x)
convert(::Type{Complex}, i::Imaginary) = Complex(i)

==(z::Complex,   w::Complex  ) = (real(z) == real(w)) & (imag(z) == imag(w))
==(z::Complex,   x::Real     ) = isreal(z) && real(z) == real(x)
==(z::Complex,   i::Imaginary) = isimag(z) && imag(z) == imag(i)
==(x::Real,      z::Complex  ) = isreal(z) && real(z) == real(x)
==(i::Imaginary, z::Complex  ) = isimag(z) && imag(z) == imag(i)

isequal(i::Imaginary, j::Imaginary) = isequal(imag(i),imag(j))
isequal(z::Complex, w::Complex) = isequal(real(z),real(w)) & isequal(imag(z),imag(w))

hash(i::Imaginary) = hash(complex(i))
hash(z::Complex) = bitmix(hash(real(z)),hash(imag(z)))

conj{T<:Real}(i::Imaginary{T}) = Imaginary{T}(-imag(i))
conj{T<:Real}(z::Complex{T}) = Complex{T}(real(z),-imag(z))

abs(i::Imaginary)  = abs(imag(i))
abs(z::Complex)  = hypot(real(z), imag(z))

abs2(i::Imaginary) = abs2(imag(i))
abs2(z::Complex) = real(z)*real(z) + imag(z)*imag(z)

inv(i::Imaginary) = Imaginary(-inv(imag(i)))
inv(z::Complex)  = conj(z)/abs2(z)

sign{T<:Real}(i::Imaginary{T}) = Imaginary{T}(sign(imag(i)))
sign(z::Complex) = z/abs(z)

-(i::Imaginary) = Imaginary(-imag(i))
-(z::Complex) = Complex(-real(z), -imag(z))

+(i::Imaginary, j::Imaginary) = Imaginary(imag(i) + imag(j))
-(i::Imaginary, j::Imaginary) = Imaginary(imag(i) - imag(j))
*(i::Imaginary, j::Imaginary) = -(imag(i) * imag(j))

+(z::Complex, w::Complex) = Complex(real(z) + real(w), imag(z) + imag(w))
-(z::Complex, w::Complex) = Complex(real(z) - real(w), imag(z) - imag(w))
*(z::Complex, w::Complex) = Complex(real(z) * real(w) - imag(z) * imag(w),
                                    real(z) * imag(w) + imag(z) * real(w))

+(x::Real, i::Imaginary) = Complex(x, imag(i))
+(i::Imaginary, x::Real) = Complex(x, imag(i))
-(x::Real, i::Imaginary) = Complex(x, -imag(i))
-(i::Imaginary, x::Real) = Complex(-x, imag(i))
*(x::Real, i::Imaginary) = Imaginary(x * imag(i))
*(i::Imaginary, x::Real) = Imaginary(x * imag(i))

+(z::Complex, x::Real) = Complex(x + real(z), imag(z))
+(x::Real, z::Complex) = Complex(x + real(z), imag(z))
-(z::Complex, x::Real) = Complex(real(z) - x, imag(z))
-(x::Real, z::Complex) = Complex(x - real(z), -imag(z))
*(z::Complex, x::Real) = Complex(x * real(z), x * imag(z))
*(x::Real, z::Complex) = Complex(x * real(z), x * imag(z))

+(z::Complex, i::Imaginary) = Complex(real(z), imag(i) + imag(z))
+(i::Imaginary, z::Complex) = Complex(real(z), imag(i) + imag(z))
-(z::Complex, i::Imaginary) = Complex(real(z), imag(z) - imag(i))
-(i::Imaginary, z::Complex) = Complex(-real(z), imag(i) - imag(z))
*(z::Complex, i::Imaginary) = imag(i) * Complex(-imag(z), real(z))
*(i::Imaginary, z::Complex) = imag(i) * Complex(-imag(z), real(z))

/(i::Imaginary, x::Real) = Imaginary(imag(i)/x)
/(x::Real, i::Imaginary) = Imaginary(-x/imag(i))
/(i::Imaginary, j::Imaginary) = imag(i)/imag(j)

/(z::Complex, x::Real) = Complex(real(z)/x, imag(z)/x)
/(z::Complex, i::Imaginary) = Complex(imag(z)/imag(i), -real(z)/imag(i))
/(x::Union(Real,Imaginary), z::Complex) = x*inv(z)

function /(a::Complex, b::Complex)
    are = real(a); aim = imag(a); bre = real(b); bim = imag(b)
    if abs(bre) <= abs(bim)
        if isinf(bre) && isinf(bim)
            r = sign(bre)/sign(bim)
        else
            r = bre / bim
        end
        den = bim + r*bre
        complex((are*r + aim)/den, (aim*r - are)/den)
    else
        if isinf(bre) && isinf(bim)
            r = sign(bim)/sign(bre)
        else
            r = bim / bre
        end
        den = bre + r*bim
        complex((are + aim*r)/den, (aim - are*r)/den)
    end
end

inv{T<:Union(Float16,Float32)}(z::Complex{T}) =
    oftype(z, conj(complex128(z))/abs2(complex128(z)))

# robust complex division for double precision
# the first step is to scale variables if appropriate ,then do calculations
# in a way that avoids over/underflow (subfuncs 1 and 2), then undo the scaling.
# scaling variable s and other techniques
# based on arxiv.1210.4539 
#             a + i*b
#  p + i*q = ---------
#             c + i*d
function /(z::Complex128, w::Complex128)
    a, b = reim(z); c, d = reim(w)
    half = 0.5
    two = 2.0
    ab = max(abs(a), abs(b))
    cd = max(abs(c), abs(d))
    ov = realmax(a)
    un = realmin(a)
    ϵ = eps(Float64)
    bs = two/(ϵ*ϵ)
    s = 1.0
    ab >= half*ov  && (a=half*a; b=half*b; s=two*s ) # scale down a,b
    cd >= half*ov  && (c=half*c; d=half*d; s=s*half) # scale down c,d
    ab <= un*two/ϵ && (a=a*bs; b=b*bs; s=s/bs      ) # scale up a,b
    cd <= un*two/ϵ && (c=c*bs; d=d*bs; s=s*bs      ) # scale up c,d
    abs(d)<=abs(c) ? ((p,q)=robust_cdiv1(a,b,c,d)  ) : ((p,q)=robust_cdiv1(b,a,d,c); q=-q)
    return Complex128(p*s,q*s) # undo scaling
end
function robust_cdiv1(a::Float64, b::Float64, c::Float64, d::Float64)
    r = d/c
    t = 1.0/(c+d*r)
    p = robust_cdiv2(a,b,c,d,r,t)
    q = robust_cdiv2(b,-a,c,d,r,t)
    return p,q
end
function robust_cdiv2(a::Float64, b::Float64, c::Float64, d::Float64, r::Float64, t::Float64)
    if r != 0
        br = b*r
        return (br != 0 ? (a+br)*t : a*t + (b*t)*r)
    else
        return (a + d*(b/c)) * t
    end
end

function inv(w::Complex128)
    c, d = reim(w)
    half = 0.5
    two = 2.0
    cd = max(abs(c), abs(d))
    ov = realmax(c)
    un = realmin(c)
    ϵ = eps(Float64)
    bs = two/(ϵ*ϵ)
    s = 1.0
    cd >= half*ov  && (c=half*c; d=half*d; s=s*half) # scale down c,d
    cd <= un*two/ϵ && (c=c*bs; d=d*bs; s=s*bs      ) # scale up c,d
    if abs(d)<=abs(c)
        r = d/c
        t = 1.0/(c+d*r)
        p = t
        q = -r * t
    else
        c, d = d, c
        r = d/c
        t = 1.0/(c+d*r)
        p = r * t
        q = -t
    end
    return Complex128(p*s,q*s) # undo scaling
end

function ssqs{T<:FloatingPoint}(x::T, y::T)
    k::Int = 0
    ρ = x*x + y*y
    if !isfinite(ρ) && (isinf(x) || isinf(y))
        ρ = convert(T, Inf)
    elseif isinf(ρ) || (ρ==0 && (x!=0 || y!=0)) || ρ<nextfloat(zero(T))/(2*eps(T)^2)
        m::T = max(abs(x), abs(y))
        k = m==0 ? m : exponent(m)
        xk, yk = ldexp(x,-k), ldexp(y,-k)
        ρ = xk*xk + yk*yk
    end
    ρ, k
end

function sqrt{T<:FloatingPoint}(z::Complex{T})
    x, y = reim(z)
    if x==y==0
        return complex(zero(x),y)
    end
    ρ, k::Int = ssqs(x, y)
    if isfinite(x) ρ=ldexp(abs(x),-k)+sqrt(ρ) end
    if isodd(k)
        k = div(k-1,2)
    else
        k = div(k,2)-1
        ρ += ρ
    end
    ρ = ldexp(sqrt(ρ),k) #sqrt((abs(z)+abs(x))/2) without over/underflow
    ξ = ρ
    η = y
    if ρ != 0
        if isfinite(η) η=(η/ρ)/2 end
        if x<0
            ξ = abs(η)
            η = copysign(ρ,y)
        end
    end
    complex(ξ,η)
end
sqrt(z::Complex) = sqrt(float(z))

# function sqrt(z::Complex)
#     rz = float(real(z))
#     iz = float(imag(z))
#     r = sqrt((hypot(rz,iz)+abs(rz))/2)
#     if r == 0
#         return complex(zero(iz), iz)
#     end
#     if rz >= 0
#         return complex(r, iz/r/2)
#     end
#     return complex(abs(iz)/r/2, copysign(r,iz))
# end

cis(theta::Real) = complex(cos(theta),sin(theta))
function cis(z::Complex)
    v = 1/exp(imag(z))
    complex(v*cos(real(z)), v*sin(real(z)))
end

angle(z::Complex) = atan2(imag(z), real(z))

function log{T<:FloatingPoint}(z::Complex{T})
    const T0::T  = 0.7071067811865475
    const T1::T  = 1.25
    const T2::T  = 3
    const ln2::T = 0.6931471805599453
    x, y = reim(z)
    ρ, k = ssqs(x,y)
    ax = abs(x)
    ay = abs(y)
    if ax < ay
        θ, β = ax, ay
    else
        θ, β = ay, ax
    end
    if k==0 && T0 < β && (β <= T1 || ρ < T2)
        ρρ = log1p((β-1)*(β+1)+θ*θ)/2
    else
        ρρ = log(ρ)/2 + k*ln2
    end
    complex(ρρ, angle(z))
end
log(z::Complex) = log(float(z))

# function log(z::Complex)
#     ar = abs(real(z))
#     ai = abs(imag(z))
#     if ar < ai
#         r = ar/ai
#         re = log(ai) + log1p(r*r)/2
#     else
#         if ar == 0
#             re = isnan(ai) ? ai : -inv(ar)
#         elseif isinf(ai)
#             re = oftype(ar,Inf)
#         else
#             r = ai/ar
#             re = log(ar) + log1p(r*r)/2
#         end
#     end
#     complex(re, angle(z))
# end

log10(z::Complex) = log(z)/oftype(real(z),2.302585092994046)
log2(z::Complex) = log(z)/oftype(real(z),0.6931471805599453)

function exp(z::Union(Imaginary,Complex))
    zr, zi = reim(z)
    if isfinite(zr) && !isfinite(zi) return complex(oftype(zr, NaN), oftype(zi, NaN)) end
    if zr==Inf && zi==0 return complex(zr, zi) end
    if zr==-Inf && !isfinite(zi) return complex(-zero(zr), copysign(zero(zi), zi)) end
    if zr==Inf && !isfinite(zi) return complex(-zr, oftype(zr, NaN)) end
    if isnan(zr) return complex(zr, zi==0 ? zi : zr) end
    er = zr==Inf ? zr : exp(zr)
    wr = er*(isfinite(zi) ? cos(zi) : zi)
    wi = er*(isfinite(zi) ? sin(zi) : zi)
    complex(wr, wi)
end

function ^{T<:FloatingPoint}(z::Complex{T}, p::Complex{T})
    if p==2 #square
        zr, zi = reim(z)
        x = (zr-zi)*(zr+zi)
        y = 2zr*zi
        if isnan(x)
            if isinf(y)
                x = copysign(zero(T),zr)
            elseif isinf(zi)
                x = convert(T,-Inf)
            elseif isinf(zr)
                x = convert(T,Inf)
            end
        elseif isnan(y) && isinf(x)
            y = copysign(zero(T), y)
        end
        complex(x,y)
    elseif z!=0
        if p!=0 && isinteger(p)
            rp = real(p)
            if rp < 0
                return power_by_squaring(inv(z), convert(Integer, -rp))
            else
                return power_by_squaring(z, convert(Integer, rp))
            end
        end
        exp(p*log(z))
    elseif p!=0 #0^p
        zero(z) #CHECK SIGNS
    else #0^0
        zer = copysign(zero(T),real(p))*copysign(zero(T),imag(z))
        complex(one(T), zer)
    end
end

function exp2{T}(z::Complex{T})
    er = exp2(real(z))
    theta = imag(z) * log(convert(T, 2))
    complex(er*cos(theta), er*sin(theta))
end

function exp10{T}(z::Complex{T})
    er = exp10(real(z))
    theta = imag(z) * log(convert(T, 10))
    complex(er*cos(theta), er*sin(theta))
end

function ^{T<:Complex}(z::T, p::T)
    if isinteger(p)
        rp = real(p)
        if rp < 0
            return power_by_squaring(inv(float(z)), convert(Integer, -rp))
        else
            return power_by_squaring(float(z), convert(Integer, rp))
        end
    end
    pr, pim = reim(p)
    zr, zi = reim(z)
    r = abs(z)
    rp = r^pr
    theta = atan2(zi, zr)
    ntheta = pr*theta
    if pim != 0 && r != 0
        rp = rp*exp(-pim*theta)
        ntheta = ntheta + pim*log(r)
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
    if pim == 0
        ip = itrunc(pr)
        if ip == pr
            if zi == 0
                im = copysign(zero(im), im)
            elseif zr == 0
                if isodd(ip)
                    re = copysign(zero(re), re)
                else
                    im = copysign(zero(im), im)
                end
            end
        else
            dr = pr*2
            ip = itrunc(dr)
            if ip == dr && zi == 0
                if zr < 0
                    re = copysign(zero(re), re)
                else
                    im = copysign(zero(im), im)
                end
            end
        end
    end

    complex(re, im)
end

^(z::Complex, n::Bool) = n ? z : one(z)
^(z::Complex, n::Integer) = z^complex(n)

^{T<:FloatingPoint}(z::Complex{T}, n::Bool) = n ? z : one(z)  # to resolve ambiguity
^{T<:Integer}(z::Complex{T}, n::Bool) = n ? z : one(z)        # to resolve ambiguity

^{T<:FloatingPoint}(z::Complex{T}, n::Integer) =
    n>=0 ? power_by_squaring(z,n) : power_by_squaring(inv(z),-n)
^{T<:Integer}(z::Complex{T}, n::Integer) = power_by_squaring(z,n) # DomainError for n<0

function sin(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0 return complex(zr, zi) end
    if isnan(zr) && !isfinite(zi) return complex(zr, zi) end
    if !isfinite(zr) && zi == 0 return complex(oftype(zr, NaN), zi) end
    if !isfinite(zr) && isfinite(zi) return complex(oftype(zr, NaN), oftype(zi, NaN)) end
    if !isfinite(zr) && !isfinite(zi) return complex(zr, oftype(zi, NaN)) end
    complex(sin(zr)*cosh(zi), cos(zr)*sinh(zi))
end

function cos(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0
        return complex(isnan(zi) ? zi : oftype(zi, Inf),
                       isnan(zi) ? zr : zr*-sign(zi))
    end
    if !isfinite(zr) && isinf(zi)
        return complex(oftype(zr, Inf), oftype(zi, NaN))
    end
    if isinf(zr)
        return complex(oftype(zr, NaN), zi==0 ? -copysign(zi, zr) : oftype(zi, NaN))
    end
    if isnan(zr) && zi==0 return complex(zr, abs(zi)) end
    complex(cos(zr)*cosh(zi), -sin(zr)*sinh(zi))
end

function tan(z::Complex)
    zr, zi = reim(z)
    w = tanh(complex(-zi, zr))
    complex(imag(w), -real(w))
end

function asin(z::Complex)
    zr, zi = reim(z)
    if isinf(zr) && isinf(zi)
        return complex(copysign(pi/4, zr),zi)
    elseif isnan(zi) && isinf(zr)
        return complex(zi, oftype(zr, Inf))
    end
    ξ = zr == 0       ? zr :
        !isfinite(zr) ? pi/2*sign(zr) :
        atan2(zr, real(sqrt(1-z)*sqrt(1+z)))
    η = asinh(copysign(imag(sqrt(conj(1-z))*sqrt(1+z)), imag(z)))
    complex(ξ,η)
end

function acos{T<:FloatingPoint}(z::Complex{T})
    zr, zi = reim(z)
    if isnan(zr)
        if isinf(zi) return complex(zr, -zi)
        else         return complex(zr, zr) end
    elseif isnan(zi)
        if isinf(zr) return complex(zi, abs(zr))
        elseif zr==0 return complex(pi/2, zi)
        else         return complex(zi, zi) end
    elseif zr==zi==0
        return complex(pi/2, -zi)
    elseif zr==Inf && zi===0.0
        return complex(zi, -zr)
    elseif zr==-Inf && zi===-0.0
        return complex(oftype(zi, pi), -zr)
    end
    ξ = 2*atan2(real(sqrt(1-z)), real(sqrt(1+z)))
    η = asinh(imag(sqrt(conj(1+z))*sqrt(1-z)))
    if isinf(zr) && isinf(zi) ξ -= pi/4 * sign(zr) end
    complex(ξ,η)
end
acos(z::Complex) = acos(float(z))

function atan(z::Complex)
    w = atanh(complex(-imag(z),real(z)))
    complex(imag(w),-real(w))
end

function sinh(z::Complex)
    zr, zi = reim(z)
    if isinf(zr) && isinf(zi) return complex(zr, oftype(zi, NaN)) end
    w = sin(complex(zi, zr))
    complex(imag(w),real(w))
end

function cosh(z::Complex)
    zr, zi = reim(z)
    if isnan(zr) && zi==0 return complex(zr, zi) end
    cos(complex(-zi,zr))
end

function tanh{T<:FloatingPoint}(z::Complex{T})
    const Ω = prevfloat(typemax(T))
    ξ, η = reim(z)
    if isnan(ξ) && η==0 return complex(ξ, η) end
    if 4*abs(ξ) > asinh(Ω) #Overflow?
        complex(copysign(one(T),ξ),
                copysign(zero(T),η*(isfinite(η) ? sin(2*abs(η)) : one(η))))
    else
        t = tan(η)
        β = 1+t*t #sec(η)^2
        s = sinh(ξ)
        ρ = sqrt(1 + s*s) #cosh(ξ)
        if isinf(t)
            complex(ρ/s,1/t)
        else
            complex(β*ρ*s,t)/(1+β*s*s)
        end
    end
end
tanh(z::Complex) = tanh(float(z))

function asinh(z::Complex)
    w = asin(complex(-imag(z),real(z)))
    complex(imag(w),-real(w))
end

function acosh(z::Complex)
    zr, zi = reim(z)
    if isnan(zr) || isnan(zi)
        if isinf(zr) || isinf(zi)
            return complex(oftype(zr, Inf), oftype(zi, NaN))
        else
            return complex(oftype(zr, NaN), oftype(zi, NaN))
        end
    elseif zr==-Inf && zi===-0.0 #Edge case is wrong - WHY?
        return complex(Inf, -pi)
    end
    ξ = asinh(real(sqrt(conj(z-1))*sqrt(z+1)))
    η = 2atan2(imag(sqrt(z-1)),real(sqrt(z+1)))
    if isinf(zr) && isinf(zi)
        η -= pi/4 * sign(zi) * sign(zr)
    end
    complex(ξ, η)
end

function atanh{T<:FloatingPoint}(z::Complex{T})
    const Ω = prevfloat(typemax(T))
    const θ = sqrt(Ω)/4
    const ρ = 1/θ
    x, y = reim(z)
    ax = abs(x)
    ay = abs(y)
    if ax > θ || ay > θ #Prevent overflow
        if isnan(y)
            if isinf(x)
                return complex(copysign(zero(x),x), y)
            else
                return complex(real(1/z), y)
            end
        end
        if isinf(y)
            return complex(copysign(zero(x),x), copysign(pi/2, y))
        end
        return complex(real(1/z), copysign(pi/2, y))
    elseif ax==1
        if y == 0
            ξ = copysign(oftype(x,Inf),x)
            η = zero(y)
        else
            ym = ay+ρ
            ξ = log(sqrt(sqrt(4+y*y))/sqrt(ym))
            η = copysign(pi/2+atan(ym/2), y)/2
        end
    else #Normal case
        ysq = (ay+ρ)^2
        if x == 0
            ξ = x
        else
            ξ = log1p(4x/((1-x)^2 + ysq))/4
        end
        η = angle(complex((1-x)*(1+x)-ysq, 2y))/2
    end
    complex(ξ, η)
end
atanh(z::Complex) = atanh(float(z))

function lexcmp(a::Complex, b::Complex)
    c = cmp(real(a), real(b))
    c == 0 || return c
    cmp(imag(a), imag(b))
end

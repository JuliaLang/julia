immutable Complex{T<:Real} <: Number
    re::T
    im::T
end
Complex(x::Real, y::Real) = Complex(promote(x,y)...)
Complex(x::Real) = Complex(x, zero(x))

typealias Complex128 Complex{Float64}
typealias Complex64  Complex{Float32}

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
complex(z::Complex) = z

complex128(r::Float64, i::Float64) = Complex{Float64}(r, i)
complex128(r::Real, i::Real) = complex128(float64(r),float64(i))
complex128(z) = complex128(real(z), imag(z))
complex64(r::Float32, i::Float32) = Complex{Float32}(r, i)
complex64(r::Real, i::Real) = complex64(float32(r),float32(i))
complex64(z) = complex64(real(z), imag(z))

for fn in (:int,:integer,:signed,:int8,:int16,:int32,:int64,:int128,
           :uint,:unsigned,:uint8,:uint16,:uint32,:uint64,:uint128,
           :float,:float32,:float64)
    @eval $fn(z::Complex) = complex($fn(real(z)),$fn(imag(z)))
end

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

function sqrt{T<:FloatingPoint}(z::Complex{T})
    ρ,k::Int=ssqs(z)
    x,y=reim(z)
    if isfinite(x) ρ=ldexp(abs(x),-k)+sqrt(ρ) end
    if isodd(k)
        k = (k-1)/2
    else
        k = k/2-1
        ρ+=ρ
    end
    ρ=ldexp(sqrt(ρ),k) #sqrt((abs(z)+abs(x))/2) without over/underflow
    ξ=ρ
    η=y
    if ρ != zero(ρ)
        if isfinite(η) η=(η/ρ)/2 end
        if x<0
            ξ=abs(η)
            η=copysign(ρ,y)
        end
    end
    complex(ξ,η)
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
    return complex(abs(iz)/r/2, copysign(r,iz))
end

cis(theta::Real) = complex(cos(theta),sin(theta))
function cis(z::Complex)
    v = 1/exp(imag(z))
    complex(v*cos(real(z)), v*sin(real(z)))
end

angle(z::Complex) = atan2(imag(z), real(z))

function sin(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0 return complex(zr, zi) end
    if isnan(zr) && !isfinite(zi) return complex(zr, zi) end
    if !isfinite(zr) && zi == 0 return complex(oftype(zr, NaN), zi) end
    if !isfinite(zr) && isfinite(zi) return complex(oftype(zr, NaN), oftype(zi, NaN)) end
    if !isfinite(zr) && !isfinite(zi) return complex(zr, oftype(zi, NaN)) end
    wr = sin(zr)*cosh(zi)
    wi = cos(zr)*sinh(zi)
    complex(wr, wi)
end

function cos(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0 
        return complex(isnan(zi) ? zi : oftype(zi, Inf),
                       isnan(zi) ? zr : zr*-sign(zi)) end
    if !isfinite(zr) && isinf(zi)
        return complex(oftype(zr, Inf), oftype(zi, NaN)) end
    if isinf(zr)
        return complex(oftype(zr, NaN), zi==0 ? -copysign(zi, zr) : oftype(zi, NaN)) end
    if isnan(zr) && zi==0 return complex(zr, abs(zi)) end
    wr =  cos(zr)*cosh(zi)
    wi = -sin(zr)*sinh(zi)
    complex(wr, wi)
end

function ssqs{T}(z::Complex{T})
    k::Int=0
    ρ=abs2(z)
    x, y=reim(z)
    if !isfinite(ρ) && (isinf(x) || isinf(y))
        ρ=convert(T, Inf)
    elseif isinf(ρ) || (ρ==zero(ρ) && (x!=zero(x) || y!=zero(y))) || ρ<nextfloat(zero(T))/(2*eps(T)^2)
        z::T=max(abs(x), abs(y))
        k= z==0 ? z : exponent(z)
        ρ=ldexp(x,-k)^2+ldexp(y,-k)^2
    end
    ρ, k
end

function log{T<:FloatingPoint}(z::Complex{T})
    const T0::T = convert(T, 0.7071067811865475)
    const T1::T = convert(T, 1.25)
    const T2::T = convert(T, 3)
    const ln2::T= convert(T, 0.6931471805599453)
    ρ, k=ssqs(z)
    x, y=reim(z)
    ax = abs(x)
    ay = abs(y)
    β=max(ax, ay)
    θ=min(ax, ay)
    if k==zero(k) && T0 < β && (β <= T1 || ρ < T2)
        ρρ=log1p((β-1)*(β+1)+θ*θ)/2
    else
        ρρ=log(ρ)/2 + k*ln2
    end
    θθ = angle(z)
    complex(ρρ, θθ)
end

function log(z::Complex)
    ar = abs(real(z))
    ai = abs(imag(z))
    if ar < ai
        r = ar/ai
        re = log(ai) + log1p(r*r)/2
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
                x=copysign(zero(T),zr)
            elseif isinf(zi)
                x=-Inf
            elseif isinf(zr)
                x=Inf
            end
        elseif isnan(y) && isinf(x)
            y=copysign(zero(T), y)
        end
        complex(x,y)
    elseif z!=zero(z)
        exp(p*log(z))
    elseif p!=zero(p) #0^p
        zero(z) #CHECK SIGNS
    else #0^0
        zer=copysign(zero(T),real(p))*copysign(zero(T),imag(z))
        complex(one(T), zer)
    end
end

function ^{T<:Complex}(z::T, p::T)
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


function tan(z::Complex)
    zr, zi = reim(z)
    w=tanh(complex(-zi, zr))
    complex(imag(w), -real(w))
end

function asin(z::Complex)
    zr, zi= reim(z)
    if isinf(zr) && isinf(zi)
        return complex(copysign(pi/4, zr),zi) 
    elseif isnan(zi) && isinf(zr) return complex(zi, oftype(zr, Inf)) end
    ξ= zr==0 ? zr : !isfinite(zr) ? pi/2*sign(zr) : atan2(zr, real(sqrt(1-z)*sqrt(1+z)))
    η=asinh(copysign(imag(sqrt(conj(1-z))*sqrt(1+z)), imag(z)))
    complex(ξ,η)
end
 
function acos(z::Complex)
    zr, zi=reim(z)
    if isnan(zr)
        if isinf(zi) return complex(zr, -zi)
        else return complex(zr, zr) end
    elseif isnan(zi)
        if isinf(zr) return complex(zi, abs(zr))
        elseif zr==0 return complex(pi/2, zi)
        else return complex(zi, zi) end
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
 
function atan(z::Complex)
    w = atanh(complex(-imag(z),real(z)))
    complex(imag(w),-real(w))
end

function sinh(z::Complex)
    zr, zi = reim(z)
    if isinf(zr) && isinf(zi)
        return complex(zr, oftype(zi, NaN)) end
    w = sin(complex(zi, zr))
    complex(imag(w),real(w))
end

function cosh(z::Complex)
    zr, zi = reim(z)
    if isnan(zr) && zi==0 return complex(zr, zi) end
    cos(complex(-zi,zr))
end

function tanh{T}(z::Complex{T})
    const Ω=prevfloat(typemax(T))
    ξ=real(z)
    η=imag(z)
    if isnan(ξ) && η==0 return complex(ξ, η) end
    if 4*abs(ξ) > asinh(Ω) #Overflow?
        complex(copysign(one(T),ξ), copysign(zero(T),η*(isfinite(η) ? sin(2*abs(η)) : 1)))
    else
        t = tan(η)
        β = 1+t^2 #sec(η)^2
        s = sinh(ξ)
        ρ = sqrt(1 + s^2) #cosh(ξ)
        if isinf(t)
            complex(ρ/s,1/t)
        else
            complex(β*ρ*s,t)/(1+β*s^2)
        end
    end
end

function asinh(z::Complex)
    w=asin(complex(-imag(z),real(z)))
    complex(imag(w),-real(w))
end

function acosh(z::Complex)
    zr, zi=reim(z)
    if isnan(zr) || isnan(zi)
        if isinf(zr) || isinf(zi)
            return complex(oftype(zr, Inf), oftype(zi, NaN))
        else
            return complex(oftype(zr, NaN), oftype(zi, NaN))
        end
    elseif zr==-Inf && zi===-0.0 #Edge case is wrong - WHY?
        return complex(Inf, -pi)
    end
    ξ=asinh(real(sqrt(conj(z-1))*sqrt(z+1)))
    η=2atan2(imag(sqrt(z-1)),real(sqrt(z+1)))
    if isinf(zr) && isinf(zi)
        η -= pi/4 * sign(zi) * sign(zr) end
    complex(ξ, η)
end

function atanh{T}(z::Complex{T})
    const Ω=prevfloat(typemax(T))
    const θ=sqrt(Ω)/4
    const ρ=1/θ
    x=real(z)
    y=imag(z)
    if x > θ || abs(y) > θ #Prevent overflow
        return complex(copysign(pi/2, y), real(1/z))
    elseif x==one(x)
        ym=abs(y)+ρ
        ξ=log(sqrt(sqrt(4+y^2))/sqrt(ym))
        η=copysign(pi/2+atan(ym/2), y)/2
    else #Normal case
        ysq=(abs(y)+ρ)^2
        ξ=log1p(4x/((1-x)^2 + ysq))/4
        η=angle(complex(((1-x)*(1+x)-ysq)/2, y))
    end
    complex(ξ, η)
end


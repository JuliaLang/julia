type Rational{T<:Int} <: Real
    num::T
    den::T

    function Rational(num::T, den::T)
        if num == 0 && den == 0
            error("invalid rational: 0//0")
        end
        g = gcd(den, num)
        num = div(num, g)
        den = div(den, g)
        new(num, den)
    end
end
Rational{T<:Int}(n::T, d::T) = Rational{T}(n,d)
Rational(n::Int, d::Int) = Rational(promote(n,d)...)
Rational(n::Int) = Rational(n,one(n))

//(n::Int, d::Int) = Rational(n,d)
//(x::Rational, y::Int) = x.num // (x.den*y)
//(x::Int, y::Rational) = (x*y.den) // y.num
//(x::Complex, y::Real) = complex(real(x)//y, imag(x)//y)
//(x::Real, y::Complex) = x*y'//real(y*y')

function //(x::Complex, y::Complex)
    xy = x*y'
    yy = real(y*y')
    complex(real(xy)//yy, imag(xy)//yy)
end

function show(x::Rational)
    if isinf(x)
        print(x.num > 0 ? "Inf" : "-Inf")
    else
        show(num(x)); print("//"); show(den(x))
    end
end

convert{T<:Int}(::Type{Rational{T}}, x::Rational) = Rational(convert(T,x.num),convert(T,x.den))
convert{T<:Int}(::Type{Rational{T}}, x::Int) = Rational(convert(T,x), convert(T,1))

function convert{T<:Int}(::Type{Rational{T}}, x::Float, tol::Real)
    if isnan(x);       return zero(T)//zero(T); end
    if x < typemin(T); return -one(T)//zero(T); end
    if typemax(T) < x; return  one(T)//zero(T); end
    y = x
    a = d = one(T)
    b = c = zero(T)
    while true
        f = convert(T,round(y)); y -= f
        a, b, c, d = f*a+c, f*b+d, a, b
        if y == 0 || abs(a/b-x) <= tol
            return a//b
        end
        y = 1/y
    end
end
convert{T<:Int}(rt::Type{Rational{T}}, x::Float) = convert(rt,x,0)

convert{T<:Float}(::Type{T}, x::Rational) = convert(T,x.num)/convert(T,x.den)
convert{T<:Int}(::Type{T}, x::Rational) = div(convert(T,x.num),convert(T,x.den))

promote_rule{T<:Int}(::Type{Rational{T}}, ::Type{T}) = Rational{T}
promote_rule{T<:Int,S<:Int}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T<:Int,S<:Int}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T<:Int,S<:Float}(::Type{Rational{T}}, ::Type{S}) = promote_type(T,S)

num(x::Int) = x
den(x::Int) = one(x)
num(x::Rational) = x.num
den(x::Rational) = x.den

sign(x::Rational) = sign(x.num)
signbit(x::Rational) = signbit(x.num)
copysign(x::Rational, y::Real) = copysign(x.num,y) // x.den
copysign(x::Rational, y::Rational) = copysign(x.num,y.num) // x.den

isnan(x::Rational) = false
isinf(x::Rational) = x.den == 0
isfinite(x::Rational) = x.den != 0

typemin{T<:Int}(::Type{Rational{T}}) = -one(T)//zero(T)
typemax{T<:Int}(::Type{Rational{T}}) = one(T)//zero(T)

hash(x::Rational) = bitmix(hash(x.num),hash(x.den))

-(x::Rational) = (-x.num) // x.den
+(x::Rational, y::Rational) = (x.num*y.den + x.den*y.num) // (x.den*y.den)
-(x::Rational, y::Rational) = (x.num*y.den - x.den*y.num) // (x.den*y.den)
*(x::Rational, y::Rational) = (x.num*y.num) // (x.den*y.den)
/(x::Rational, y::Rational) = (x.num*y.den) // (x.den*y.num)
/(x::Rational, z::ComplexPair) = inv(z/x)

==(x::Rational, y::Rational) = x.den == y.den  && x.num == y.num
==(x::Rational, y::Int     ) = x.den == one(x.den) && x.num == y
==(x::Int     , y::Rational) = y == x

< (x::Rational, y::Rational) = x.den == y.den ? x.num < y.num : x.num*y.den < x.den*y.num
< (x::Rational, y::Int     ) = x.num < x.den*y
< (x::Int     , y::Rational) = x*y.den < y.num

<=(x::Rational, y::Rational) = x.den == y.den ? x.num <= y.num : x.num*y.den <= x.den*y.num
<=(x::Rational, y::Int     ) = x.num <= x.den*y
<=(x::Int     , y::Rational) = x*y.den <= y.num

div(x::Rational, y::Rational) = div(x.num*y.den, x.den*y.num)
div(x::Rational, y::Real    ) = div(x.num, x.den*y)
div(x::Real    , y::Rational) = div(x*y.den, y.num)

fld(x::Rational, y::Rational) = fld(x.num*y.den, x.den*y.num)
fld(x::Rational, y::Real    ) = fld(x.num, x.den*y)
fld(x::Real    , y::Rational) = fld(x*y.den, y.num)

rational(x::Real) = rational(x, 0)
rational(x::Rational, tol::Real) = x
rational(x::Int) = x // one(x)
rational(x::Int, tol::Real) = x // one(x)
rational(x::Float32, tol::Real) = convert(Rational{Int32}, x, tol)
rational(x::Float64, tol::Real) = convert(Rational{Int64}, x, tol)
rational(z::Complex) = complex(rational(real(z)), rational(imag(z)))
rational(z::Complex, tol::Real) =
    (tol /= sqrt(2); complex(rational(real(z), tol), rational(imag(z), tol)))

int(x::Rational) = div(x.num, x.den)
float(x::Rational) = x.num/x.den

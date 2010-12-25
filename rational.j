struct Rational{T<:Int} <: Real
    num::T
    den::T

    function Rational{T<:Int}(num::T, den::T)
        if num == den == 0
            z = zero(num)
            return new(z, z)
        end
        g = gcd(den, num)
        num = div(num, g)
        den = div(den, g)
        new(num, den)
    end

    Rational(n::Int, d::Int) = Rational(promote(n,d)...)
    Rational(n::Int) = new(n, one(n))
end

//(n::Int, d::Int) = Rational(n,d)
//(x::Rational, y::Int) = x.num // (x.den*y)
//(x::Int, y::Rational) = (x*y.den) // y.num

function show(x::Rational)
    show(num(x))
    print("//")
    show(den(x))
end

convert{T}(::Type{Rational{T}}, x::T) = Rational(x, convert(T,1))
convert{T}(::Type{Rational{T}}, x::Int) = Rational(convert(T,x), convert(T,1))
convert{T}(::Type{Rational{T}}, x::Rational) = Rational(convert(T,x.num),convert(T,x.den))
convert{T<:Float}(::Type{T}, x::Rational) = convert(T,x.num)/convert(T,x.den)
convert{T<:Int}(::Type{T}, x::Rational) = div(convert(T,x.num),convert(T,x.den))

promote_rule{T<:Int}(::Type{Rational{T}}, ::Type{T}) = Rational{T}
promote_rule{T,S<:Int}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T,S}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T,S<:Float}(::Type{Rational{T}}, ::Type{S}) = promote_type(T,S)

num(x::Rational) = x.num
den(x::Rational) = x.den
sign(x::Rational) = sign(x.num)
signbit(x::Rational) = signbit(x.num)
copysign(x::Rational, y::Real) = copysign(x.num,y) // x.den
copysign(x::Rational, y::Rational) = copysign(x.num,y.num) // x.den

(-)(x::Rational) = (-x.num) // x.den
(+)(x::Rational, y::Rational) = (x.num*y.den + x.den*y.num) // (x.den*y.den)
(-)(x::Rational, y::Rational) = (x.num*y.den - x.den*y.num) // (x.den*y.den)
(*)(x::Rational, y::Rational) = (x.num*y.num) // (x.den*y.den)
(/)(x::Rational, y::Rational) = (x.num*y.den) // (x.den*y.num)

==(x::Rational, y::Rational) = (x.num == y.num && x.den == y.den)
!=(x::Rational, y::Rational) = (x.num != y.num || x.den != y.den)
<=(x::Rational, y::Rational) = (x.num*y.den <= y.num*x.den)
< (x::Rational, y::Rational) = (x.num*y.den < y.num*x.den)
>=(x::Rational, y::Rational) = y <= x
> (x::Rational, y::Rational) = y < x

div(x::Rational, y::Rational) = div(x.num*y.den, x.den*y.num)
div(x::Real    , y::Rational) = div(x*y.den, y.num)
div(x::Rational, y::Real    ) = div(x.num, x.den*y)

fld(x::Rational, y::Rational) = fld(x.num*y.den, x.den*y.num)
fld(x::Real    , y::Rational) = fld(x*y.den, y.num)
fld(x::Rational, y::Real    ) = fld(x.num, x.den*y)

int(x::Rational) = div(x.num, x.den)
float(x::Rational) = x.num/x.den

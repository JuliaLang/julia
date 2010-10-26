struct Rational{T<:Int} <: Real{Rational}
    num::T
    den::T
end

convert{T}(::Type{Rational{T}}, x::T) = Rational(x, convert(T,1))
convert{T}(::Type{Rational{T}}, x::Int) = Rational(convert(T,x), convert(T,1))
convert{T}(::Type{Rational{T}}, x::Rational) = Rational(convert(T,x.num),convert(T,x.den))
convert{T}(::Type{Float{T}}, x::Rational) = convert(T,x.num)/convert(T,x.den)

promote_rule{T}(::Type{Rational{T}}, ::Type{Int{T}}) = Rational{T}
promote_rule{T,S}(::Type{Rational{T}}, ::Type{Int{S}}) = Rational{promote_type(T,S)}
promote_rule{T,S}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T,S}(::Type{Rational{T}}, ::Type{Float{S}}) = promote_type(T,S)

function //{T}(num::T, den::T)
    if den == 0
        error("//: division by zero")
    end
    g = gcd(den, num)
    num = div(num, g)
    den = div(den, g)
    Rational(num, den)
end

//(num, den) = //(promote(num, den)...)
//{T<:Int}(x::Rational{T}, y::T) = x.num // (x.den*y)
//{T<:Int}(x::T, y::Rational{T}) = (x*y.den) // y.num

function show(x::Rational)
    show(num(x))
    print("//")
    show(den(x))
end

num(x::Rational) = x.num
den(x::Rational) = x.den
sign(x::Rational) = sign(x.num)*sign(x.den)

(-)(x::Rational) = (-x.num) // x.den
(+)(x::Rational, y::Rational) = (x.num*y.den + x.den*y.num) // (x.den*y.den)
(-)(x::Rational, y::Rational) = (x.num*y.den - x.den*y.num) // (x.den*y.den)
(*)(x::Rational, y::Rational) = (x.num*y.num) // (x.den*y.den)
(/)(x::Rational, y::Rational) = (x.num*y.den) // (x.den*y.num)

div(x::Rational, y::Rational) = div(x.num*y.den, x.den*y.num)
div(x::Real    , y::Rational) = div(x*y.den, y.num)
div(x::Rational, y::Real    ) = div(x.num, x.den*y)

==(x::Rational, y::Rational) = (x.num*y.den == y.num*x.den)
< (x::Rational, y::Rational) = (x.num*y.den <  y.num*x.den)

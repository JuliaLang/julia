type Rational[`T] < Real
    num::T
    den::T
end

function print(x::Rational)
    print(num(x))
    print("/")
    print(den(x))
end

function rational(num::`T, den::`T)
    g = gcd(num, den)
    num = div(num, g)
    den = div(den, g)
    num = sign(den) * num
    den = sign(den) * den
    return new(Rational[T], num, den)
end

num(x::Rational) = x.num
den(x::Rational) = x.den

(-)(x::Rational) = rational(-x.num, x.den)

(+)(x::Rational, y::Rational) = rational(x.num*y.den + x.den*y.num, x.den*y.den)
(-)(x::Rational, y::Rational) = rational(x.num*y.den - x.den*y.num, x.den*y.den)
(*)(x::Rational, y::Rational) = rational(x.num*y.num, x.den*y.den)
(/)(x::Rational, y::Rational) = rational(x.num*y.den, x.den*y.num)

conversion x::Int32-->Rational
    return rational(x,1)
end

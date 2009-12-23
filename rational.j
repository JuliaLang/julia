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
(+)(x::Rational, y::Int) = (x + rational(y, 1))
(+)(y::Int, x::Rational) = (rational(y, 1) + x)

(-)(x::Rational, y::Rational) = rational(x.num*y.den - x.den*y.num, x.den*y.den)
(-)(x::Rational, y::Int) = (x - rational(y, 1))
(-)(y::Int, x::Rational) = (rational(y, 1) - x)

(*)(x::Rational, y::Rational) = rational(x.num*y.num, x.den*y.den)
(*)(x::Rational, y::Int) = rational(x.num*y, x.den)
(*)(y::Int, x::Rational) = rational(y*x.num, x.den)

(/)(x::Rational, y::Rational) = rational(x.num*y.den, x.den*y.num)
(/)(x::Rational, y::Int)      = rational(x.num, x.den*y)
(/)(y::Int, x::Rational)      = rational(y*x.den, x.num)

==(x::Rational, y::Rational) = (x.num == y.num && x.den == y.den)
==(x::Rational, y::Int)      = (x.num == y     && x.den == 1)
==(y::Int, x::Rational)      = (x.num == y     && x.den == 1)

double(x::Rational) = double(x.num)/double(x.den)

conversion x::Int32-->Rational
    return rational(x,1)
end

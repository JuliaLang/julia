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

function num(x::Rational)
    return x.num
end

function den(x::Rational)
    return x.den
end

function (+)(x::Rational, y::Rational)
    return rational(x.num*y.den + x.den*y.num, x.den*y.den)
end

function (-)(x::Rational, y::Rational)
    return rational(x.num*y.den - x.den*y.num, x.den*y.den)
end

function -(x::Rational)
    return rational(-x.num, x.den)
end

function *(x::Rational, y::Rational)
    return rational(x.num*y.num, x.den*y.den)
end

function /(x::Rational, y::Rational)
    return rational(x.num*y.den, x.den*y.num)
end

conversion x::Int32-->Rational
    return rational(x,1)
end

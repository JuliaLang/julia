type Rational[`T] < Real
    num::T
    den::T
end

function reduce(x::Rational)
    g = gcd(x.num, x.den)
    x.num = div(x.num, g)
    x.den = div(x.den, g)
    x.num = sign(x.den) * x.num
    x.den = sign(x.den) * x.den
    return x
end

function print(x::Rational)
    reduce(x)
    print(num(x))
    print("/")
    print(den(x))
end

function rational(num::`T, den::`T)
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

conversion x::Rational-->Double
    return double(x.num/x.den)
end

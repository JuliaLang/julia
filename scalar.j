isscalar(x::Scalar) = true
isscalar(x) = false

size(x::Scalar) = ()
ndims(x::Scalar) = 0
numel(x::Scalar) = 1
length(x::Scalar) = 1

sign(x::Real) = (x < 0 ? -1 : (x > 0 ? +1 : 0))
signbit(x::Real) = (x < 0 ? -1 : +1)
signbit(x::Float) = (x < 0 ? -1 : (x > 0 ? 1 : (1.0/x < 0 ? -1 : +1)))

abs(x::Real) = (x < 0 ? -x : x)

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(transpose(x))

max(x::Real, y::Real) = x > y ? x : y
min(x::Real, y::Real) = x < y ? x : y
sum(x::Number, y::Number) = x + y
prod(x::Number, y::Number) = x * y
all(x::Scalar, y::Scalar) = x && y ? true : false
any(x::Scalar, y::Scalar) = x || y ? true : false

start(a::Real) = a
next(a::Real, i) = (a, a+1)
done(a::Real, i) = (i > a)
isempty(a::Real) = false

clamp(x::Real, lo::Real, hi::Real) = (x > hi ? hi : (x < lo ? lo : x))

sec(z) = 1 ./cos(z)
csc(z) = 1 ./sin(z)
cot(z) = 1 ./tan(z)
asec(y) = acos(1 ./y)
acsc(y) = asin(1 ./y)
acot(y) = atan(1 ./y)
sech(z) = 1 ./cosh(z)
csch(z) = 1 ./sinh(z)
coth(z) = 1 ./tanh(z)
asech(y) = acosh(1 ./y)
acsch(y) = asinh(1 ./y)
acoth(y) = atanh(1 ./y)

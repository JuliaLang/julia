isscalar(x::Scalar) = true
isscalar(x) = false

size(x::Scalar) = ()
ndims(x::Scalar) = 0
numel(x::Scalar) = 1
length(x::Scalar) = 1

sign(x::Scalar) = (x < 0 ? -1 : (x > 0 ? +1 : 0))
signbit(x::Scalar) = (x < 0 ? -1 : +1)
signbit(x::Float) = (x < 0 ? -1 : (x > 0 ? 1 : (1.0/x < 0 ? -1 : +1)))

abs(x::Scalar) = (x < 0 ? -x : x)

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(transpose(x))

max(x::Scalar, y::Scalar) = x > y ? x : y
max(a::Number, b::Number) = a > b ? a : b
min(x::Scalar, y::Scalar) = x < y ? x : y
min(a::Number, b::Number) = a < b ? a : b
sum(x::Scalar, y::Scalar) = x + y
prod(x::Scalar, y::Scalar) = x * y
all(x::Scalar, y::Scalar) = x && y ? true : false
any(x::Scalar, y::Scalar) = x || y ? true : false

start(a::Scalar) = a
next(a::Scalar, i) = (a, a+1)
done(a::Scalar, i) = (i > a)

clamp(x::Scalar, lo::Scalar, hi::Scalar) = (x > hi ? hi : (x < lo ? lo : x))

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

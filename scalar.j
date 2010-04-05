isscalar(x::Scalar) = true
isscalar(x) = false

size(x::Scalar) = buffer()
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
min(x::Scalar, y::Scalar) = x < y ? x : y
sum(x::Scalar, y::Scalar) = x + y
prod(x::Scalar, y::Scalar) = x * y
all(x::Scalar, y::Scalar) = x && y ? true : false
any(x::Scalar, y::Scalar) = x || y ? true : false

start(a::Scalar) = a
next(a::Scalar, i) = (a, a+1)
done(a::Scalar, i) = (i > a)

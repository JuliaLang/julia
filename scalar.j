## generic operations on scalars ##

isscalar(x::Scalar) = true
isscalar(x) = false

isreal(x::Real) = true
isreal(x) = false

isinteger(x::Int) = true
isinteger(x) = false

integer_valued(x::Int) = true
real_valued(x::Real) = true

size(x::Scalar) = ()
ndims(x::Scalar) = 0
numel(x::Scalar) = 1
length(x::Scalar) = 1
ref(x::Scalar) = x

signbit(x::Real) = x < 0 ? int8(-1) : int8(1)
sign(x::Real) = x < 0 ? int8(-1) : x > 0 ? int8(1) : int8(0)
abs(x::Real) = x < 0 ? -x : x

conj(x::Number) = x
transpose(x::Scalar) = x
ctranspose(x::Number) = conj(transpose(x))

max(x::Scalar)  = x
min(x::Scalar)  = x
sum(x::Scalar)  = x
prod(x::Scalar) = x

max(x::Real, y::Real) = x > y ? x : y
min(x::Real, y::Real) = x < y ? x : y
sum(x::Number, y::Number) = x + y
prod(x::Number, y::Number) = x * y

start(a::Real) = a
next(a::Real, i) = (a, a+1)
done(a::Real, i) = (i > a)

isempty(a::Scalar) = false

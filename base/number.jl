## generic operations on numbers ##

isreal(x::Number) = false
isreal(x::Real) = true

isinteger(x::Number) = false
isinteger(x::Integer) = true

real_valued(x::Real) = true
integer_valued(x::Integer) = true

isbool(x::Number) = false
isbool(x::Bool) = true
const islogical = isbool

size(x::Number) = ()
eltype(x::Number) = typeof(x)
eltype{T<:Number}(::Type{T}) = T
ndims(x::Number) = 0
ndims{T<:Number}(::Type{T}) = 0
length(x::Number) = 1
ref(x::Number) = x

signbit(x::Real) = int(x < 0)
sign(x::Real) = x < 0 ? -one(x) : x > 0 ? one(x) : x
abs(x::Real) = x < 0 ? -x : x
abs2(x::Real) = x*x

conj(x::Real) = x
transpose(x::Number) = x
ctranspose(x::Number) = conj(x)
inv(x::Number) = one(x)/x
angle(z::Real) = atan2(zero(z), z)

# TODO: should we really treat numbers as iterable?
start(a::Real) = a
next(a::Real, i) = (a, a+1)
done(a::Real, i) = (i > a)
isempty(a::Number) = false
contains(s::Number, n::Number) = (s == n)

reinterpret{T<:Real}(::Type{T}, x::Real) = box(T,x)

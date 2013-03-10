## generic operations on numbers ##

isreal(x::Number) = false
isreal(x::Real) = true

isinteger(x::Number) = false
isinteger(x::Integer) = true

real_valued(x::Real) = true
integer_valued(x::Integer) = true

isbool(x::Number) = false
isbool(x::Bool) = true

size(x::Number) = ()
size(x::Number,d) = convert(Int,d)<1 ? throw(BoundsError()) : 1
eltype(x::Number) = typeof(x)
eltype{T<:Number}(::Type{T}) = T
ndims(x::Number) = 0
ndims{T<:Number}(::Type{T}) = 0
length(x::Number) = 1
endof(x::Number) = 1
getindex(x::Number) = x
getindex(x::Number, i::Integer) = i == 1 ? x : throw(BoundsError())
getindex(x::Number, i::Real) = getindex(x, to_index(i))
first(x::Number) = x
last(x::Number) = x

signbit(x::Real) = int(x < 0)
sign(x::Real) = x < 0 ? -one(x) : x > 0 ? one(x) : x
abs(x::Real) = x < 0 ? -x : x
abs2(x::Real) = x*x

conj(x::Real) = x
transpose(x::Number) = x
ctranspose(x::Number) = conj(x)
inv(x::Number) = one(x)/x
angle(z::Real) = atan2(zero(z), z)

start(x::Number) = false
next(x::Number, state) = (x, true)
done(x::Number, state) = state
isempty(x::Number) = false
contains(x::Number, y::Number) = x == y

reinterpret{T<:Real}(::Type{T}, x::Real) = box(T,x)

map(f, x::Number) = f(x)

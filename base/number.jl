## generic operations on numbers ##

isreal(x::Real) = true
isinteger(x::Integer) = true

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

divrem(x,y) = (div(x,y),rem(x,y))
signbit(x::Real) = int(x < 0)
sign(x::Real) = ifelse(x < 0, oftype(x,-1), ifelse(x > 0, one(x), x))
abs(x::Real) = ifelse(x < 0, -x, x)
abs2(x::Real) = x*x
copysign(x::Real, y::Real) = ifelse(signbit(x)!=signbit(y), -x, x)

conj(x::Real) = x
transpose(x::Number) = x
ctranspose(x::Number) = conj(x)
inv(x::Number) = one(x)/x
angle(z::Real) = atan2(zero(z), z)

widemul(x::Number, y::Number) = widen(x)*widen(y)

start(x::Number) = false
next(x::Number, state) = (x, true)
done(x::Number, state) = state
isempty(x::Number) = false
in(x::Number, y::Number) = x == y

reinterpret{T<:Real}(::Type{T}, x::Real) = box(T,x)

map(f::Callable, x::Number) = f(x)

const _numeric_conversion_func_names =
    (:int,:integer,:signed,:int8,:int16,:int32,:int64,:int128,
     :uint,:unsigned,:uint8,:uint16,:uint32,:uint64,:uint128,
     :float,:float16,:float32,:float64)

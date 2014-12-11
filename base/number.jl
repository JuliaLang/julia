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
getindex(x::Number, I::Integer...) = all([i == 1 for i in I]) ? x : throw(BoundsError())
getindex(x::Number, I::Real...) = getindex(x, to_index(I)...)
first(x::Number) = x
last(x::Number) = x

divrem(x,y) = (div(x,y),rem(x,y))
signbit(x::Real) = x < 0
sign(x::Real) = ifelse(x < 0, oftype(x,-1), ifelse(x > 0, one(x), x))
sign(x::Unsigned) = ifelse(x > 0, one(x), x)
abs(x::Real) = ifelse(signbit(x), -x, x)
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

map(f::Callable, x::Number) = f(x)

zero(x::Number) = oftype(x,0)
zero{T<:Number}(::Type{T}) = convert(T,0)
one(x::Number)  = oftype(x,1)
one{T<:Number}(::Type{T}) = convert(T,1)

const _numeric_conversion_func_names =
    (:int,:integer,:signed,:int8,:int16,:int32,:int64,:int128,
     :uint,:unsigned,:uint8,:uint16,:uint32,:uint64,:uint128,
     :float,:float16,:float32,:float64,
     :big)

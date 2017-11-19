# This file is a part of Julia. License is MIT: https://julialang.org/license

# Missing, missing and ismissing are defined in essentials.jl

show(io::IO, x::Missing) = print(io, "missing")

"""
    MissingException(msg)

Exception thrown when a [`missing`](@ref) value is encountered in a situation
where it is not supported. The error message, in the `msg` field
may provide more specific details.
"""
struct MissingException <: Exception
    msg::AbstractString
end

showerror(io::IO, ex::MissingException) =
    print(io, "MissingException: ", ex.msg)

promote_rule(::Type{Missing}, ::Type{T}) where {T} = Union{T, Missing}
promote_rule(::Type{Union{S,Missing}}, ::Type{T}) where {T,S} = Union{promote_type(T, S), Missing}
promote_rule(::Type{Any}, ::Type{T}) where {T} = Any
promote_rule(::Type{Any}, ::Type{Missing}) = Any
promote_rule(::Type{Missing}, ::Type{Any}) = Any
promote_rule(::Type{Missing}, ::Type{Missing}) = Missing

convert(::Type{Union{T, Missing}}, x) where {T} = convert(T, x)
# To print more appropriate message than "T not defined"
convert(::Type{Missing}, x) = throw(MethodError(convert, (Missing, x)))
convert(::Type{Missing}, ::Missing) = missing

# Comparison operators
==(::Missing, ::Missing) = missing
==(::Missing, ::Any) = missing
==(::Any, ::Missing) = missing
# To fix ambiguity
==(::Missing, ::WeakRef) = missing
==(::WeakRef, ::Missing) = missing
isequal(::Missing, ::Missing) = true
isequal(::Missing, ::Any) = false
isequal(::Any, ::Missing) = false
<(::Missing, ::Missing) = missing
<(::Missing, ::Any) = missing
<(::Any, ::Missing) = missing
isless(::Missing, ::Missing) = false
isless(::Missing, ::Any) = false
isless(::Any, ::Missing) = true

# Unary operators/functions
for f in (:(!), :(+), :(-), :(identity), :(zero),
          :(abs), :(abs2), :(sign),
          :(acos), :(acosh), :(asin), :(asinh), :(atan), :(atanh),
          :(sin), :(sinh), :(cos), :(cosh), :(tan), :(tanh),
          :(exp), :(exp2), :(expm1), :(log), :(log10), :(log1p),
          :(log2), :(exponent), :(sqrt), :(gamma), :(lgamma),
          :(iseven), :(ispow2), :(isfinite), :(isinf), :(isodd),
          :(isinteger), :(isreal), :(isnan), :(isempty),
          :(iszero), :(transpose), :(float))
    @eval Math.$(f)(::Missing) = missing
end

zero(::Type{Union{T, Missing}}) where {T} = zero(T)
# To prevent StackOverflowError
zero(::Type{Any}) = throw(MethodError(zero, (Any,)))

# Binary operators/functions
for f in (:(+), :(-), :(*), :(/), :(^),
          :(div), :(mod), :(fld), :(rem), :(min), :(max))
    @eval begin
        # Scalar with missing
        ($f)(::Missing, ::Missing) = missing
        ($f)(d::Missing, x::Number) = missing
        ($f)(d::Number, x::Missing) = missing
    end
end

# Rounding and related functions
for f in (:(ceil), :(floor), :(round), :(trunc))
    @eval begin
        ($f)(::Missing, digits::Integer=0, base::Integer=0) = missing
        ($f)(::Type{>:Missing}, ::Missing) = missing
        ($f)(::Type{T}, ::Missing) where {T} =
            throw(MissingException("cannot convert a missing value to type $T"))
    end
end

# to avoid ambiguity warnings
(^)(::Missing, ::Integer) = missing

# Bit operators
(&)(::Missing, ::Missing) = missing
(&)(a::Missing, b::Bool) = ifelse(b, missing, false)
(&)(b::Bool, a::Missing) = ifelse(b, missing, false)
(&)(::Missing, ::Integer) = missing
(&)(::Integer, ::Missing) = missing
(|)(::Missing, ::Missing) = missing
(|)(a::Missing, b::Bool) = ifelse(b, true, missing)
(|)(b::Bool, a::Missing) = ifelse(b, true, missing)
(|)(::Missing, ::Integer) = missing
(|)(::Integer, ::Missing) = missing
xor(::Missing, ::Missing) = missing
xor(a::Missing, b::Bool) = missing
xor(b::Bool, a::Missing) = missing
xor(::Missing, ::Integer) = missing
xor(::Integer, ::Missing) = missing

*(d::Missing, x::AbstractString) = missing
*(d::AbstractString, x::Missing) = missing

function float(A::AbstractArray{Union{T, Missing}}) where {T}
    U = typeof(float(zero(T)))
    convert(AbstractArray{Union{U, Missing}}, A)
end
float(A::AbstractArray{Missing}) = A
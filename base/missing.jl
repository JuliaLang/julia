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

nonmissingtype(::Type{Union{T, Missing}}) where {T} = T
nonmissingtype(::Type{Missing}) = Union{}
nonmissingtype(::Type{T}) where {T} = T
nonmissingtype(::Type{Any}) = Any

promote_rule(::Type{Missing}, ::Type{T}) where {T} = Union{T, Missing}
promote_rule(::Type{Union{S,Missing}}, ::Type{T}) where {T,S} = Union{promote_type(T, S), Missing}
promote_rule(::Type{Any}, ::Type{T}) where {T} = Any
promote_rule(::Type{Any}, ::Type{Missing}) = Any
promote_rule(::Type{Missing}, ::Type{Any}) = Any
promote_rule(::Type{Missing}, ::Type{Missing}) = Missing

convert(::Type{Union{T, Missing}}, x) where {T} = convert(T, x)
# To fix ambiguities
convert(::Type{Missing}, ::Missing) = missing
convert(::Type{Union{Nothing, Missing}}, x::Union{Nothing, Missing}) = x
convert(::Type{Union{Nothing, Missing}}, x) =
    throw(MethodError(convert, (Union{Nothing, Missing}, x)))
# To print more appropriate message than "T not defined"
convert(::Type{Missing}, x) = throw(MethodError(convert, (Missing, x)))

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
for f in (:(!), :(+), :(-), :(identity), :(zero), :(one), :(oneunit),
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

for f in (:(Base.zero), :(Base.one), :(Base.oneunit))
    @eval function $(f)(::Type{Union{T, Missing}}) where T
        T === Any && throw(MethodError($f, (Any,)))  # To prevent StackOverflowError
        $f(T)
    end
end

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

"""
    skipmissing(itr)

Return an iterator over the elements in `itr` skipping [`missing`](@ref) values.

Use [`collect`](@ref) to obtain an `Array` containing the non-`missing` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove missings while preserving dimensions
of the input.

# Examples
```jldoctest
julia> sum(skipmissing([1, missing, 2]))
3

julia> collect(skipmissing([1, missing, 2]))
2-element Array{Int64,1}:
1
2

julia> collect(skipmissing([1 missing; 2 missing]))
2-element Array{Int64,1}:
1
2

```
"""
skipmissing(itr) = SkipMissing(itr)

struct SkipMissing{T}
    x::T
end
iteratorsize(::Type{<:SkipMissing}) = SizeUnknown()
iteratoreltype(::Type{SkipMissing{T}}) where {T} = iteratoreltype(T)
eltype(itr::SkipMissing) = nonmissingtype(eltype(itr.x))
function Base.iterate(itr::SkipMissing, state...)
    y = iterate(itr.x, state...)
    while y !== nothing && y[1] isa Missing
        y = iterate(itr.x, y[2])
    end
    y
end
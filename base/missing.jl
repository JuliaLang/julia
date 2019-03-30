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

for U in (:Nothing, :Missing)
    @eval begin
        promote_rule(::Type{$U}, ::Type{T}) where {T} = Union{T, $U}
        promote_rule(::Type{Union{S,$U}}, ::Type{Any}) where {S} = Any
        promote_rule(::Type{Union{S,$U}}, ::Type{T}) where {T,S} = Union{promote_type(T, S), $U}
        promote_rule(::Type{Any}, ::Type{$U}) = Any
        promote_rule(::Type{$U}, ::Type{Any}) = Any
        # This definition is never actually used, but disambiguates the above definitions
        promote_rule(::Type{$U}, ::Type{$U}) = $U
    end
end
promote_rule(::Type{Union{Nothing, Missing}}, ::Type{Any}) = Any
promote_rule(::Type{Union{Nothing, Missing}}, ::Type{T}) where {T} =
    Union{Nothing, Missing, T}
promote_rule(::Type{Union{Nothing, Missing, S}}, ::Type{Any}) where {S} = Any
promote_rule(::Type{Union{Nothing, Missing, S}}, ::Type{T}) where {T,S} =
    Union{Nothing, Missing, promote_type(T, S)}

convert(::Type{Union{T, Missing}}, x::Union{T, Missing}) where {T} = x
convert(::Type{Union{T, Missing}}, x) where {T} = convert(T, x)
# To fix ambiguities
convert(::Type{Missing}, ::Missing) = missing
convert(::Type{Union{Nothing, Missing}}, x::Union{Nothing, Missing}) = x
convert(::Type{Union{Nothing, Missing, T}}, x::Union{Nothing, Missing, T}) where {T} = x
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
isapprox(::Missing, ::Missing; kwargs...) = missing
isapprox(::Missing, ::Any; kwargs...) = missing
isapprox(::Any, ::Missing; kwargs...) = missing

# Unary operators/functions
for f in (:(!), :(~), :(+), :(-), :(zero), :(one), :(oneunit),
          :(isfinite), :(isinf), :(isodd),
          :(isinteger), :(isreal), :(isnan),
          :(iszero), :(transpose), :(adjoint), :(float), :(conj),
          :(abs), :(abs2), :(iseven), :(ispow2),
          :(real), :(imag), :(sign))
    @eval ($f)(::Missing) = missing
end
for f in (:(Base.zero), :(Base.one), :(Base.oneunit))
    @eval ($f)(::Type{Missing}) = missing
    @eval function $(f)(::Type{Union{T, Missing}}) where T
        T === Any && throw(MethodError($f, (Any,)))  # To prevent StackOverflowError
        $f(T)
    end
end

# Binary operators/functions
for f in (:(+), :(-), :(*), :(/), :(^), :(div), :(mod), :(fld), :(rem))
    @eval begin
        # Scalar with missing
        ($f)(::Missing, ::Missing) = missing
        ($f)(::Missing, ::Number)  = missing
        ($f)(::Number,  ::Missing) = missing
    end
end

min(::Missing, ::Missing) = missing
min(::Missing, ::Any)     = missing
min(::Any,     ::Missing) = missing
max(::Missing, ::Missing) = missing
max(::Missing, ::Any)     = missing
max(::Any,     ::Missing) = missing

# Rounding and related functions
round(::Missing, ::RoundingMode=RoundNearest; sigdigits::Integer=0, digits::Integer=0, base::Integer=0) = missing
round(::Type{>:Missing}, ::Missing, ::RoundingMode=RoundNearest) = missing
round(::Type{T}, ::Missing, ::RoundingMode=RoundNearest) where {T} =
    throw(MissingException("cannot convert a missing value to type $T: use Union{$T, Missing} instead"))
round(::Type{T}, x::Any, r::RoundingMode=RoundNearest) where {T>:Missing} = round(nonmissingtype(T), x, r)
# to fix ambiguities
round(::Type{T}, x::Rational, r::RoundingMode=RoundNearest) where {T>:Missing} = round(nonmissingtype(T), x, r)
round(::Type{T}, x::Rational{Bool}, r::RoundingMode=RoundNearest) where {T>:Missing} = round(nonmissingtype(T), x, r)

# Handle ceil, floor, and trunc separately as they have no RoundingMode argument
for f in (:(ceil), :(floor), :(trunc))
    @eval begin
        ($f)(::Missing; sigdigits::Integer=0, digits::Integer=0, base::Integer=0) = missing
        ($f)(::Type{>:Missing}, ::Missing) = missing
        ($f)(::Type{T}, ::Missing) where {T} =
            throw(MissingException("cannot convert a missing value to type $T: use Union{$T, Missing} instead"))
        ($f)(::Type{T}, x::Any) where {T>:Missing} = $f(nonmissingtype(T), x)
        # to fix ambiguities
        ($f)(::Type{T}, x::Rational) where {T>:Missing} = $f(nonmissingtype(T), x)
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
The returned object can be indexed using indices of `itr` if the latter is indexable.
Indices corresponding to missing values are not valid: they are skipped by [`keys`](@ref)
and [`eachindex`](@ref), and a `MissingException` is thrown when trying to use them.

Use [`collect`](@ref) to obtain an `Array` containing the non-`missing` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove missings while preserving dimensions
of the input.

# Examples
```jldoctest
julia> x = skipmissing([1, missing, 2])
Base.Skip{Missing,Array{Union{Missing, Int64},1}}(Union{Missing, Int64}[1, missing, 2])

julia> sum(x)
3

julia> x[1]
1

julia> x[2]
ERROR: MissingException: the value at index (2,) is of type Missing
[...]

julia> argmax(x)
3

julia> collect(keys(x))
2-element Array{Int64,1}:
 1
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
skipmissing(itr) = skip(Missing, itr)

"""
    coalesce(x, y...)

Return the first value in the arguments which is not equal to [`missing`](@ref),
if any. Otherwise return `missing`.

# Examples

```jldoctest
julia> coalesce(missing, 1)
1

julia> coalesce(1, missing)
1

julia> coalesce(nothing, 1)  # returns `nothing`

julia> coalesce(missing, missing)
missing
```
"""
function coalesce end

coalesce() = missing
coalesce(x::Missing, y...) = coalesce(y...)
coalesce(x::Any, y...) = x

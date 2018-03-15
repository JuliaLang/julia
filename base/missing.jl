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
        promote_rule(::Type{Union{S,$U}}, ::Type{T}) where {T,S} = Union{promote_type(T, S), $U}
        promote_rule(::Type{Any}, ::Type{$U}) = Any
        promote_rule(::Type{$U}, ::Type{Any}) = Any
        promote_rule(::Type{$U}, ::Type{$U}) = U
    end
end
promote_rule(::Type{Union{Nothing, Missing}}, ::Type{Any}) = Any
promote_rule(::Type{Union{Nothing, Missing}}, ::Type{T}) where {T} =
    Union{Nothing, Missing, T}

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
for f in (:(!), :(~), :(+), :(-), :(identity), :(zero), :(one), :(oneunit),
          :(abs), :(abs2), :(sign), :(real), :(imag),
          :(acos), :(acosh), :(asin), :(asinh), :(atan), :(atanh),
          :(sin), :(sinh), :(cos), :(cosh), :(tan), :(tanh),
          :(exp), :(exp2), :(expm1), :(log), :(log10), :(log1p),
          :(log2), :(Math.exponent), :(sqrt), :(Math.gamma), :(Math.lgamma),
          :(iseven), :(ispow2), :(isfinite), :(isinf), :(isodd),
          :(isinteger), :(isreal), :(isnan),
          :(iszero), :(transpose), :(adjoint), :(float), :(conj))
    @eval $(f)(::Missing) = missing
end

for f in (:(Base.zero), :(Base.one), :(Base.oneunit))
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
for f in (:(ceil), :(floor), :(round), :(trunc))
    @eval begin
        ($f)(::Missing, digits::Integer=0, base::Integer=0) = missing
        ($f)(::Type{>:Missing}, ::Missing) = missing
        ($f)(::Type{T}, ::Missing) where {T} =
            throw(MissingException("cannot convert a missing value to type $T: use Union{$T, Missing} instead"))
        ($f)(::Type{T}, x::Any) where {T>:Missing} = $f(nonmissingtype(T), x)
        # to fix ambiguities
        ($f)(::Type{T}, x::Rational) where {T>:Missing} = $f(nonmissingtype(T), x)
        ($f)(::Type{T}, x::Rational{Bool}) where {T>:Missing} = $f(nonmissingtype(T), x)
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
IteratorSize(::Type{<:SkipMissing}) = SizeUnknown()
IteratorEltype(::Type{SkipMissing{T}}) where {T} = IteratorEltype(T)
eltype(::Type{SkipMissing{T}}) where {T} = nonmissingtype(eltype(T))
# Fallback implementation for general iterables: we cannot access a value twice,
# so after finding the next non-missing element in start() or next(), we have to
# pass it in the iterator state, which introduces a type instability since the value
# is missing if the input does not contain any non-missing element.
@inline function Base.start(itr::SkipMissing)
    s = start(itr.x)
    v = missing
    @inbounds while !done(itr.x, s) && v isa Missing
        v, s = next(itr.x, s)
    end
    (v, s)
end
@inline Base.done(itr::SkipMissing, state) = ismissing(state[1]) && done(itr.x, state[2])
@inline function Base.next(itr::SkipMissing, state)
    v1, s = state
    v2 = missing
    @inbounds while !done(itr.x, s) && v2 isa Missing
        v2, s = next(itr.x, s)
    end
    (v1, (v2, s))
end
# Optimized implementation for AbstractArray, relying on the ability to access x[i] twice:
# once in done() to find the next non-missing entry, and once in next() to return it.
# This works around the type instability problem of the generic fallback.
@inline function _next_nonmissing_ind(x::AbstractArray, s)
    idx = eachindex(x)
    @inbounds while !done(idx, s)
        i, new_s = next(idx, s)
        x[i] isa Missing || break
        s = new_s
    end
    s
end
@inline Base.start(itr::SkipMissing{<:AbstractArray}) =
    _next_nonmissing_ind(itr.x, start(eachindex(itr.x)))
@inline Base.done(itr::SkipMissing{<:AbstractArray}, state) =
    done(eachindex(itr.x), state)
@inline function Base.next(itr::SkipMissing{<:AbstractArray}, state)
    i, state = next(eachindex(itr.x), state)
    @inbounds v = itr.x[i]::eltype(itr)
    (v, _next_nonmissing_ind(itr.x, state))
end

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
for f in (:(!), :(~), :(+), :(-), :(identity), :(zero), :(one), :(oneunit),
          :(isfinite), :(isinf), :(isodd),
          :(isinteger), :(isreal), :(isnan),
          :(iszero), :(transpose), :(adjoint), :(float), :(conj),
          :(abs), :(abs2), :(iseven), :(ispow2),
          :(real), :(imag), :(sign))
    @eval ($f)(::Missing) = missing
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

function iterate(itr::SkipMissing, state...)
    y = iterate(itr.x, state...)
    y === nothing && return nothing
    item, state = y
    while item === missing
        y = iterate(itr.x, state)
        y === nothing && return nothing
        item, state = y
    end
    item, state
end

# Optimized mapreduce implementation
# The generic method is faster when !(eltype(A) >: Missing) since it does not need
# additional loops to identify the two first non-missing values of each block
mapreduce(f, op, itr::SkipMissing{<:AbstractArray}) =
    _mapreduce(f, op, IndexStyle(itr.x), eltype(itr.x) >: Missing ? itr : itr.x)

function _mapreduce(f, op, ::IndexLinear, itr::SkipMissing{<:AbstractArray})
    A = itr.x
    local ai
    inds = LinearIndices(A)
    i = first(inds)
    ilast = last(inds)
    while i <= ilast
        @inbounds ai = A[i]
        ai === missing || break
        i += 1
    end
    i > ilast && return mapreduce_empty(f, op, eltype(itr))
    a1 = ai
    i += 1
    while i <= ilast
        @inbounds ai = A[i]
        ai === missing || break
        i += 1
    end
    i > ilast && return mapreduce_first(f, op, a1)
    # We know A contains at least two non-missing entries: the result cannot be nothing
    something(mapreduce_impl(f, op, itr, first(inds), last(inds)))
end

_mapreduce(f, op, ::IndexCartesian, itr::SkipMissing) = mapfoldl(f, op, itr)

mapreduce_impl(f, op, A::SkipMissing, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

# Returns nothing when the input contains only missing values, and Some(x) otherwise
@noinline function mapreduce_impl(f, op, itr::SkipMissing{<:AbstractArray},
                                  ifirst::Integer, ilast::Integer, blksize::Int)
    A = itr.x
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        if a1 === missing
            return nothing
        else
            return Some(mapreduce_first(f, op, a1))
        end
    elseif ifirst + blksize > ilast
        # sequential portion
        local ai
        i = ifirst
        while i <= ilast
            @inbounds ai = A[i]
            ai === missing || break
            i += 1
        end
        i > ilast && return nothing
        a1 = ai::eltype(itr)
        i += 1
        while i <= ilast
            @inbounds ai = A[i]
            ai === missing || break
            i += 1
        end
        i > ilast && return Some(mapreduce_first(f, op, a1))
        a2 = ai::eltype(itr)
        i += 1
        v = op(f(a1), f(a2))
        @simd for i = i:ilast
            @inbounds ai = A[i]
            if ai !== missing
                v = op(v, f(ai))
            end
        end
        return Some(v)
    else
        # pairwise portion
        imid = (ifirst + ilast) >> 1
        v1 = mapreduce_impl(f, op, itr, ifirst, imid, blksize)
        v2 = mapreduce_impl(f, op, itr, imid+1, ilast, blksize)
        if v1 === nothing && v2 === nothing
            return nothing
        elseif v1 === nothing
            return v2
        elseif v2 === nothing
            return v1
        else
            return Some(op(something(v1), something(v2)))
        end
    end
end

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

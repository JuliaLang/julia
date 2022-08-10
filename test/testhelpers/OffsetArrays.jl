# This file is a part of Julia. License is MIT: https://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

# OffsetArrays v1.11.2
# No compat patch and docstrings
module OffsetArrays

using Base: tail, @propagate_inbounds
using Base: IdentityUnitRange

export OffsetArray, OffsetMatrix, OffsetVector

const IIUR = IdentityUnitRange{<:AbstractUnitRange{<:Integer}}

########################################################################################################
# axes.jl
########################################################################################################

struct IdOffsetRange{T<:Integer,I<:AbstractUnitRange{T}} <: AbstractUnitRange{T}
    parent::I
    offset::T

    function IdOffsetRange{T,I}(r::I, offset::T) where {T<:Integer,I<:AbstractUnitRange{T}}
        _bool_check(T, r, offset)
        new{T,I}(r, offset)
    end

    #= This method is necessary to avoid a StackOverflowError in IdOffsetRange{T,I}(r::IdOffsetRange, offset::Integer).
    The type signature in that method is more specific than IdOffsetRange{T,I}(r::I, offset::T),
    so it ends up calling itself if I <: IdOffsetRange.
    =#
    function IdOffsetRange{T,IdOffsetRange{T,I}}(r::IdOffsetRange{T,I}, offset::T) where {T<:Integer,I<:AbstractUnitRange{T}}
        _bool_check(T, r, offset)
        new{T,IdOffsetRange{T,I}}(r, offset)
    end
end

function _bool_check(::Type{Bool}, r, offset)
    # disallow the construction of IdOffsetRange{Bool, UnitRange{Bool}}(true:true, true)
    if offset && (first(r) || last(r))
        throw(ArgumentError("values = $r and offset = $offset can not produce a boolean range"))
    end
    return nothing
end
_bool_check(::Type, r, offset) = nothing

# Construction/coercion from arbitrary AbstractUnitRanges
function IdOffsetRange{T,I}(r::AbstractUnitRange, offset::Integer = 0) where {T<:Integer,I<:AbstractUnitRange{T}}
    rc, o = offset_coerce(I, r)
    return IdOffsetRange{T,I}(rc, convert(T, o+offset)::T)
end
function IdOffsetRange{T}(r::AbstractUnitRange, offset::Integer = 0) where T<:Integer
    rc = convert(AbstractUnitRange{T}, r)::AbstractUnitRange{T}
    return IdOffsetRange{T,typeof(rc)}(rc, convert(T, offset)::T)
end
IdOffsetRange(r::AbstractUnitRange{T}, offset::Integer = 0) where T<:Integer =
    IdOffsetRange{T,typeof(r)}(r, convert(T, offset)::T)

# Coercion from other IdOffsetRanges
IdOffsetRange{T,I}(r::IdOffsetRange{T,I}) where {T<:Integer,I<:AbstractUnitRange{T}} = r
function IdOffsetRange{T,I}(r::IdOffsetRange, offset::Integer = 0) where {T<:Integer,I<:AbstractUnitRange{T}}
    rc, offset_rc = offset_coerce(I, r.parent)
    return IdOffsetRange{T,I}(rc, convert(T, r.offset + offset + offset_rc)::T)
end
IdOffsetRange{T}(r::IdOffsetRange{T}) where {T<:Integer} = r
function IdOffsetRange{T}(r::IdOffsetRange, offset::Integer = 0) where T<:Integer
    return IdOffsetRange{T}(r.parent, r.offset + offset)
end
IdOffsetRange(r::IdOffsetRange) = r

# Constructor to make `show` round-trippable
function IdOffsetRange(; values::AbstractUnitRange{<:Integer}, indices::AbstractUnitRange{<:Integer})
    length(values) == length(indices) || throw(ArgumentError("values and indices must have the same length"))
    offset = first(indices) - 1
    return IdOffsetRange(values .- offset, offset)
end

# Conversions to an AbstractUnitRange{Int} (and to an OrdinalRange{Int,Int} on Julia v"1.6") are necessary
# to evaluate CartesianIndices for BigInt ranges, as their axes are also BigInt ranges
Base.AbstractUnitRange{T}(r::IdOffsetRange) where {T<:Integer} = IdOffsetRange{T}(r)

# TODO: uncomment these when Julia is ready
# # Conversion preserves both the values and the indices, throwing an InexactError if this
# # is not possible.
# Base.convert(::Type{IdOffsetRange{T,I}}, r::IdOffsetRange{T,I}) where {T<:Integer,I<:AbstractUnitRange{T}} = r
# Base.convert(::Type{IdOffsetRange{T,I}}, r::IdOffsetRange) where {T<:Integer,I<:AbstractUnitRange{T}} =
#     IdOffsetRange{T,I}(convert(I, r.parent), r.offset)
# Base.convert(::Type{IdOffsetRange{T,I}}, r::AbstractUnitRange) where {T<:Integer,I<:AbstractUnitRange{T}} =
#     IdOffsetRange{T,I}(convert(I, r), 0)

offset_coerce(::Type{Base.OneTo{T}}, r::Base.OneTo) where T<:Integer = convert(Base.OneTo{T}, r), 0
function offset_coerce(::Type{Base.OneTo{T}}, r::AbstractUnitRange) where T<:Integer
    o = first(r) - 1
    return Base.OneTo{T}(last(r) - o), o
end
# function offset_coerce(::Type{Base.OneTo{T}}, r::IdOffsetRange) where T<:Integer
#     rc, o = offset_coerce(Base.OneTo{T}, r.parent)

# Fallback, specialze this method if `convert(I, r)` doesn't do what you need
offset_coerce(::Type{I}, r::AbstractUnitRange) where I<:AbstractUnitRange =
    convert(I, r)::I, 0

@inline Base.parent(r::IdOffsetRange) = r.parent
@inline Base.axes(r::IdOffsetRange) = (Base.axes1(r),)
@inline Base.axes1(r::IdOffsetRange) = IdOffsetRange(Base.axes1(r.parent), r.offset)
@inline Base.unsafe_indices(r::IdOffsetRange) = (Base.axes1(r),)
@inline Base.length(r::IdOffsetRange) = length(r.parent)
@inline Base.isempty(r::IdOffsetRange) = isempty(r.parent)
Base.reduced_index(i::IdOffsetRange) = typeof(i)(first(i):first(i))
# Workaround for #92 on Julia < 1.4
Base.reduced_index(i::IdentityUnitRange{<:IdOffsetRange}) = typeof(i)(first(i):first(i))
for f in [:firstindex, :lastindex]
    @eval @inline Base.$f(r::IdOffsetRange) = $f(r.parent) + r.offset
end
for f in [:first, :last]
    # coerce the type to deal with values that get promoted on addition (eg. Bool)
    @eval @inline Base.$f(r::IdOffsetRange) = eltype(r)($f(r.parent) + r.offset)
end

# Iteration for an IdOffsetRange
@inline Base.iterate(r::IdOffsetRange, i...) = _iterate(r, i...)
# In general we iterate over the parent term by term and add the offset.
# This might have some performance degradation when coupled with bounds-checking
# See https://github.com/JuliaArrays/OffsetArrays.jl/issues/214
@inline function _iterate(r::IdOffsetRange, i...)
    ret = iterate(r.parent, i...)
    ret === nothing && return nothing
    return (eltype(r)(ret[1] + r.offset), ret[2])
end
# Base.OneTo(n) is known to be exactly equivalent to the range 1:n,
# and has no specialized iteration defined for it,
# so we may add the offset to the range directly and iterate over the result
# This gets around the performance issue described in issue #214
# We use the helper function _addoffset to evaluate the range instead of broadcasting
# just in case this makes it easy for the compiler.
@inline _iterate(r::IdOffsetRange{<:Integer, <:Base.OneTo}, i...) = iterate(_addoffset(r.parent, r.offset), i...)

@inline function Base.getindex(r::IdOffsetRange, i::Integer)
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    @boundscheck checkbounds(r, i)
    @inbounds eltype(r)(r.parent[i - r.offset] + r.offset)
end

# Logical indexing following https://github.com/JuliaLang/julia/pull/31829
#= Helper function to perform logical indxeing for boolean ranges
The code implemented is a branch-free version of the following:

    range(first(s) ? first(r) : last(r), length=Int(last(s)))

See https://github.com/JuliaArrays/OffsetArrays.jl/pull/224#discussion_r595635143

Logical indexing does not preserve indices, unlike other forms of vector indexing
=#
@inline function _getindex(r, s::AbstractUnitRange{Bool})
    range(first(r) * first(s) + last(r) * !first(s), length=Int(last(s)))
end
@inline function _getindex(r, s::StepRange{Bool})
    range(first(r) * first(s) + last(r) * !first(s), step = oneunit(step(s)), length=Int(last(s)))
end
@inline function _getindex(r, s::AbstractUnitRange)
    @inbounds rs = r.parent[_subtractoffset(s, r.offset)] .+ r.offset
    _indexedby(rs, axes(s))
end
@inline function _getindex(r, s::StepRange)
    rs = @inbounds r.parent[s .- r.offset] .+ r.offset
    _indexedby(rs, axes(s))
end

for T in [:AbstractUnitRange, :StepRange]
    @eval @inline function Base.getindex(r::IdOffsetRange, s::$T{<:Integer})
        @boundscheck checkbounds(r, s)
        return _getindex(r, s)
    end
end

# These methods are necessary to avoid ambiguity
for R in [:IIUR, :IdOffsetRange]
    @eval @inline function Base.getindex(r::IdOffsetRange, s::$R)
        @boundscheck checkbounds(r, s)
        return _getindex(r, s)
    end
end

# offset-preserve broadcasting
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(-), r::IdOffsetRange{T}, x::Integer) where T =
    IdOffsetRange{T}(r.parent .- x, r.offset)
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(+), r::IdOffsetRange{T}, x::Integer) where T =
    IdOffsetRange{T}(r.parent .+ x, r.offset)
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(+), x::Integer, r::IdOffsetRange{T}) where T =
    IdOffsetRange{T}(x .+ r.parent, r.offset)

Base.show(io::IO, r::IdOffsetRange) = print(io, IdOffsetRange, "(values=",first(r), ':', last(r),", indices=",first(eachindex(r)),':',last(eachindex(r)), ")")

# Optimizations
@inline Base.checkindex(::Type{Bool}, inds::IdOffsetRange, i::Real) = Base.checkindex(Bool, inds.parent, i - inds.offset)

########################################################################################################
# origin.jl
########################################################################################################

struct Origin{T<:Union{Tuple{Vararg{Int}}, Int}}
    index::T
end
Origin(I::Tuple{Vararg{Int}}) = Origin{typeof(I)}(I)
Origin(I::Tuple{Vararg{Number}}) = Origin(map(Int, I))
Origin(I::CartesianIndex) = Origin(Tuple(I))
Origin(I::Number...) = Origin(I)
# Origin(0) != Origin((0, )) but they work the same with broadcasting
Origin(n::Number) = Origin{Int}(Int(n))

Base.Broadcast.broadcastable(o::Origin) = Ref(o)

_showidx(index::Integer) = "(" * string(index) * ")"
_showidx(index::Tuple) = string(index)
Base.show(io::IO, o::Origin) = print(io, "Origin", _showidx(o.index))

########################################################################################################
# utils.jl
########################################################################################################

### Low-level utilities ###

_indexoffset(r::AbstractRange) = first(r) - 1
_indexoffset(i::Integer) = 0
_indexlength(r::AbstractRange) = length(r)
_indexlength(i::Integer) = Int(i)
_indexlength(i::Colon) = Colon()

# utility methods used in reshape
# we don't use _indexlength in this to avoid converting the arguments to Int
_checksize(ind::Integer, s) = ind == s
_checksize(ind::AbstractUnitRange, s) = length(ind) == s

_toaxis(i::Integer) = Base.OneTo(i)
_toaxis(i) = i

_strip_IdOffsetRange(r::IdOffsetRange) = parent(r)
_strip_IdOffsetRange(r) = r

_offset(axparent::AbstractUnitRange, ax::AbstractUnitRange) = first(ax) - first(axparent)
_offset(axparent::AbstractUnitRange, ::Union{Integer, Colon}) = 1 - first(axparent)

_offsets(A::AbstractArray) = map(ax -> first(ax) - 1, axes(A))
_offsets(A::AbstractArray, B::AbstractArray) = map(_offset, axes(B), axes(A))

abstract type AxisConversionStyle end
struct SingleRange <: AxisConversionStyle end
struct TupleOfRanges <: AxisConversionStyle end

AxisConversionStyle(::Type) = SingleRange()
AxisConversionStyle(::Type{<:CartesianIndices}) = TupleOfRanges()

_convertTupleAbstractUnitRange(x) = _convertTupleAbstractUnitRange(AxisConversionStyle(typeof(x)), x)
_convertTupleAbstractUnitRange(::SingleRange, x) = (convert(AbstractUnitRange{Int}, x),)
_convertTupleAbstractUnitRange(::TupleOfRanges, x) = convert(Tuple{Vararg{AbstractUnitRange{Int}}}, x)

_toAbstractUnitRanges(t::Tuple) = (_convertTupleAbstractUnitRange(first(t))..., _toAbstractUnitRanges(tail(t))...)
_toAbstractUnitRanges(::Tuple{}) = ()

# ensure that the indices are consistent in the constructor
_checkindices(A::AbstractArray, indices, label) = _checkindices(ndims(A), indices, label)
function _checkindices(N::Integer, indices, label)
    throw_argumenterror(N, indices, label) = throw(ArgumentError(label*" $indices are not compatible with a $(N)D array"))
    N == length(indices) || throw_argumenterror(N, indices, label)
end

@inline _indexedby(r::AbstractVector, ax::Tuple{Any}) = _indexedby(r, ax[1])
@inline _indexedby(r::AbstractUnitRange{<:Integer}, ::Base.OneTo) = no_offset_view(r)
@inline _indexedby(r::AbstractUnitRange{Bool}, ::Base.OneTo) = no_offset_view(r)
@inline _indexedby(r::AbstractVector, ::Base.OneTo) = no_offset_view(r)
@inline function _indexedby(r::AbstractUnitRange{<:Integer}, ax::AbstractUnitRange)
    of = convert(eltype(r), first(ax) - 1)
    IdOffsetRange(_subtractoffset(r, of), of)
end
@inline _indexedby(r::AbstractUnitRange{Bool}, ax::AbstractUnitRange) = OffsetArray(r, ax)
@inline _indexedby(r::AbstractVector, ax::AbstractUnitRange) = OffsetArray(r, ax)

# These functions are equivalent to the broadcasted operation r .- of
# However these ensure that the result is an AbstractRange even if a specific
# broadcasting behavior is not defined for a custom type
@inline _subtractoffset(r::AbstractUnitRange, of) = UnitRange(first(r) - of, last(r) - of)
@inline _subtractoffset(r::AbstractRange, of) = range(first(r) - of, stop = last(r) - of, step = step(r))

# similar to _subtractoffset, except these evaluate r .+ of
@inline _addoffset(r::AbstractUnitRange, of) = UnitRange(first(r) + of, last(r) + of)
@inline _addoffset(r::AbstractRange, of) = range(first(r) + of, stop = last(r) + of, step = step(r))

_contiguousindexingtype(r::AbstractUnitRange{<:Integer}) = r

_of_eltype(::Type{T}, M::AbstractArray{T}) where {T} = M
_of_eltype(T, M::AbstractArray) = map(T, M)

# filter the arguments to reshape to check if there are any ranges
# If not, we may pop the parent array
_filterreshapeinds(t::Tuple{AbstractUnitRange, Vararg{Any}}) = t
_filterreshapeinds(t::Tuple) = _filterreshapeinds(tail(t))
_filterreshapeinds(t::Tuple{}) = t
_popreshape(A::AbstractArray, ax::Tuple{Vararg{Base.OneTo}}, inds::Tuple{}) = no_offset_view(A)
_popreshape(A::AbstractArray, ax, inds) = A

########################################################################################################
# OffsetArrays.jl
########################################################################################################

# Technically we know the length of CartesianIndices but we need to convert it first, so here we
# don't put it in OffsetAxisKnownLength.
const OffsetAxisKnownLength = Union{Integer,AbstractUnitRange}
const OffsetAxis = Union{OffsetAxisKnownLength,Colon}
const ArrayInitializer = Union{UndefInitializer,Missing,Nothing}

## OffsetArray
struct OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
    @inline function OffsetArray{T, N, AA}(parent::AA, offsets::NTuple{N, Int}; checkoverflow = true) where {T, N, AA<:AbstractArray{T,N}}
        # allocation of `map` on tuple is optimized away
        checkoverflow && map(overflow_check, axes(parent), offsets)
        new{T, N, AA}(parent, offsets)
    end
end

const OffsetVector{T,AA<:AbstractVector{T}} = OffsetArray{T,1,AA}

const OffsetMatrix{T,AA<:AbstractMatrix{T}} = OffsetArray{T,2,AA}

# checks if the offset may be added to the range without overflowing
function overflow_check(r::AbstractUnitRange, offset::Integer)
    Base.hastypemax(eltype(r)) || return nothing
    # This gives some performance boost https://github.com/JuliaLang/julia/issues/33273
    throw_upper_overflow_error(val) = throw(OverflowError("offset should be <= $(typemax(Int) - val) corresponding to the axis $r, received an offset $offset"))
    throw_lower_overflow_error(val) = throw(OverflowError("offset should be >= $(typemin(Int) - val) corresponding to the axis $r, received an offset $offset"))

    # With ranges in the picture, first(r) might not necessarily be < last(r)
    # we therefore use the min and max of first(r) and last(r) to check for overflow
    firstlast_min, firstlast_max = minmax(first(r), last(r))

    if offset > 0 && firstlast_max > typemax(Int) - offset
        throw_upper_overflow_error(firstlast_max)
    elseif offset < 0 && firstlast_min < typemin(Int) - offset
        throw_lower_overflow_error(firstlast_min)
    end
    return nothing
end

# Tuples of integers are treated as offsets
# Empty Tuples are handled here
@inline function OffsetArray(A::AbstractArray, offsets::Tuple{Vararg{Integer}}; kw...)
    _checkindices(A, offsets, "offsets")
    OffsetArray{eltype(A), ndims(A), typeof(A)}(A, offsets; kw...)
end

# These methods are necessary to disallow incompatible dimensions for
# the OffsetVector and the OffsetMatrix constructors
for (FT, ND) in ((:OffsetVector, :1), (:OffsetMatrix, :2))
    @eval @inline function $FT(A::AbstractArray{<:Any,$ND}, offsets::Tuple{Vararg{Integer}}; kw...)
        _checkindices(A, offsets, "offsets")
        OffsetArray{eltype(A), $ND, typeof(A)}(A, offsets; kw...)
    end
    FTstr = string(FT)
    @eval @inline function $FT(A::AbstractArray, offsets::Tuple{Vararg{Integer}}; kw...)
        throw(ArgumentError($FTstr*" requires a "*string($ND)*"D array"))
    end
end

## OffsetArray constructors
for FT in (:OffsetArray, :OffsetVector, :OffsetMatrix)
    # Nested OffsetArrays may strip off the wrapper and collate the offsets
    # empty tuples are handled here
    @eval @inline function $FT(A::OffsetArray, offsets::Tuple{Vararg{Int}}; checkoverflow = true)
        _checkindices(A, offsets, "offsets")
        # ensure that the offsets may be added together without an overflow
        checkoverflow && map(overflow_check, axes(A), offsets)
        I = map(+, _offsets(A, parent(A)), offsets)
        $FT(parent(A), I, checkoverflow = false)
    end
    @eval @inline function $FT(A::OffsetArray, offsets::Tuple{Integer,Vararg{Integer}}; kw...)
        $FT(A, map(Int, offsets); kw...)
    end

    # In general, indices get converted to AbstractUnitRanges.
    # CartesianIndices{N} get converted to N ranges
    @eval @inline function $FT(A::AbstractArray, inds::Tuple{Any,Vararg{Any}}; kw...)
        $FT(A, _toAbstractUnitRanges(to_indices(A, axes(A), inds)); kw...)
    end

    # convert ranges to offsets
    @eval @inline function $FT(A::AbstractArray, inds::Tuple{AbstractUnitRange,Vararg{AbstractUnitRange}}; kw...)
        _checkindices(A, inds, "indices")
        # Performance gain by wrapping the error in a function: see https://github.com/JuliaLang/julia/issues/37558
        throw_dimerr(lA, lI) = throw(DimensionMismatch("supplied axes do not agree with the size of the array (got size $lA for the array and $lI for the indices"))
        lA = size(A)
        lI = map(length, inds)
        lA == lI || throw_dimerr(lA, lI)
        $FT(A, map(_offset, axes(A), inds); kw...)
    end

    @eval @inline $FT(A::AbstractArray, inds...; kw...) = $FT(A, inds; kw...)
    @eval @inline $FT(A::AbstractArray; checkoverflow = false) = $FT(A, ntuple(zero, Val(ndims(A))), checkoverflow = checkoverflow)

    @eval @inline $FT(A::AbstractArray, origin::Origin; checkoverflow = true) = $FT(A, origin.index .- first.(axes(A)); checkoverflow = checkoverflow)
end

(o::Origin)(A::AbstractArray) = OffsetArray(no_offset_view(A), o)
Origin(A::AbstractArray) = Origin(first.(axes(A)))

# conversion-related methods
@inline OffsetArray{T}(M::AbstractArray, I...; kw...) where {T} = OffsetArray{T,ndims(M)}(M, I...; kw...)

@inline function OffsetArray{T,N}(M::AbstractArray{<:Any,N}, I...; kw...) where {T,N}
    M2 = _of_eltype(T, M)
    OffsetArray{T,N}(M2, I...; kw...)
end
@inline OffsetArray{T,N}(M::OffsetArray{T,N}, I...; kw...) where {T,N} = OffsetArray(M, I...; kw...)
@inline OffsetArray{T,N}(M::AbstractArray{T,N}, I...; kw...) where {T,N} = OffsetArray{T,N,typeof(M)}(M, I...; kw...)

@inline OffsetArray{T,N,A}(M::AbstractArray{<:Any,N}, I...; kw...) where {T,N,A<:AbstractArray{T,N}} = OffsetArray{T,N,A}(M, I; kw...)
@inline function OffsetArray{T,N,A}(M::AbstractArray{<:Any,N}, I::NTuple{N,Int}; checkoverflow = true) where {T,N,A<:AbstractArray{T,N}}
    checkoverflow && map(overflow_check, axes(M), I)
    Mv = no_offset_view(M)
    MvA = convert(A, Mv)::A
    Iof = map(+, _offsets(M), I)
    OffsetArray{T,N,A}(MvA, Iof, checkoverflow = false)
end
@inline function OffsetArray{T, N, AA}(parent::AbstractArray{<:Any,N}, offsets::NTuple{N, Integer}; kw...) where {T, N, AA<:AbstractArray{T,N}}
    OffsetArray{T, N, AA}(parent, map(Int, offsets)::NTuple{N,Int}; kw...)
end
@inline function OffsetArray{T,N,A}(M::AbstractArray{<:Any,N}, I::Tuple{AbstractUnitRange,Vararg{AbstractUnitRange}}; kw...) where {T,N,A<:AbstractArray{T,N}}
    _checkindices(M, I, "indices")
    # Performance gain by wrapping the error in a function: see https://github.com/JuliaLang/julia/issues/37558
    throw_dimerr(lA, lI) = throw(DimensionMismatch("supplied axes do not agree with the size of the array (got size $lA for the array and $lI for the indices"))
    lM = size(M)
    lI = map(length, I)
    lM == lI || throw_dimerr(lM, lI)
    OffsetArray{T,N,A}(M, map(_offset, axes(M), I); kw...)
end
@inline function OffsetArray{T,N,A}(M::AbstractArray{<:Any,N}, I::Tuple; kw...) where {T,N,A<:AbstractArray{T,N}}
    OffsetArray{T,N,A}(M, _toAbstractUnitRanges(to_indices(M, axes(M), I)); kw...)
end
@inline function OffsetArray{T,N,A}(M::AbstractArray{<:Any,N}; kw...) where {T,N,A<:AbstractArray{T,N}}
    Mv = no_offset_view(M)
    MvA = convert(A, Mv)::A
    OffsetArray{T,N,A}(MvA, _offsets(M); kw...)
end
@inline OffsetArray{T,N,A}(M::A; checkoverflow = false) where {T,N,A<:AbstractArray{T,N}} = OffsetArray{T,N,A}(M, ntuple(zero, Val(N)); checkoverflow = checkoverflow)

Base.convert(::Type{T}, M::AbstractArray) where {T<:OffsetArray} = M isa T ? M : T(M)

@inline AbstractArray{T,N}(M::OffsetArray{S,N}) where {T,S,N} = OffsetArray{T}(M)

# array initialization
@inline function OffsetArray{T,N}(init::ArrayInitializer, inds::Tuple{Vararg{OffsetAxisKnownLength}}; kw...) where {T,N}
    _checkindices(N, inds, "indices")
    AA = Array{T,N}(init, map(_indexlength, inds))
    OffsetArray{T, N, typeof(AA)}(AA, map(_indexoffset, inds); kw...)
end
@inline function OffsetArray{T, N}(init::ArrayInitializer, inds::Tuple; kw...) where {T, N}
    OffsetArray{T, N}(init, _toAbstractUnitRanges(inds); kw...)
end
@inline OffsetArray{T,N}(init::ArrayInitializer, inds...; kw...) where {T,N} = OffsetArray{T,N}(init, inds; kw...)

@inline OffsetArray{T}(init::ArrayInitializer, inds::NTuple{N, OffsetAxisKnownLength}; kw...) where {T,N} = OffsetArray{T,N}(init, inds; kw...)
@inline function OffsetArray{T}(init::ArrayInitializer, inds::Tuple; kw...) where {T}
    OffsetArray{T}(init, _toAbstractUnitRanges(inds); kw...)
end
@inline OffsetArray{T}(init::ArrayInitializer, inds...; kw...) where {T} = OffsetArray{T}(init, inds; kw...)

Base.IndexStyle(::Type{OA}) where {OA<:OffsetArray} = IndexStyle(parenttype(OA))
parenttype(::Type{OffsetArray{T,N,AA}}) where {T,N,AA} = AA
parenttype(A::OffsetArray) = parenttype(typeof(A))

Base.parent(A::OffsetArray) = A.parent

# TODO: Ideally we would delegate to the parent's broadcasting implementation, but that
#       is currently broken in sufficiently many implementation, namely RecursiveArrayTools, DistributedArrays
#       and StaticArrays, that it will take concentrated effort to get this working across the ecosystem.
#       The goal would be to have `OffsetArray(CuArray) .+ 1 == OffsetArray{CuArray}`.
# Base.Broadcast.BroadcastStyle(::Type{<:OffsetArray{<:Any, <:Any, AA}}) where AA = Base.Broadcast.BroadcastStyle(AA)

@inline Base.size(A::OffsetArray) = size(parent(A))

@inline Base.axes(A::OffsetArray) = map(IdOffsetRange, axes(parent(A)), A.offsets)
@inline Base.axes(A::OffsetArray, d) = d <= ndims(A) ? IdOffsetRange(axes(parent(A), d), A.offsets[d]) : IdOffsetRange(axes(parent(A), d))
@inline Base.axes1(A::OffsetArray{T,0}) where {T} = IdOffsetRange(axes(parent(A), 1))  # we only need to specialize this one

# Utils to translate a function to the parent while preserving offsets
unwrap(x) = x, identity
unwrap(x::OffsetArray) = parent(x), data -> OffsetArray(data, x.offsets, checkoverflow = false)
function parent_call(f, x)
    parent, wrap_offset = unwrap(x)
    wrap_offset(f(parent))
end

Base.similar(A::OffsetArray, ::Type{T}, dims::Dims) where T =
    similar(parent(A), T, dims)
function Base.similar(A::AbstractArray, ::Type{T}, shape::Tuple{OffsetAxisKnownLength,Vararg{OffsetAxisKnownLength}}) where T
    # strip IdOffsetRanges to extract the parent range and use it to generate the array
    new_shape = map(_strip_IdOffsetRange, shape)
    # route through _similar_axes_or_length to avoid a stack overflow if map(_strip_IdOffsetRange, shape) === shape
    # This tries to use new_shape directly in similar if similar(A, T, ::typeof(new_shape)) is defined
    # If this fails, it calls similar(A, T, map(_indexlength, new_shape)) to use the size along each axis
    # to generate the new array
    P = _similar_axes_or_length(A, T, new_shape, shape)
    return OffsetArray(P, map(_offset, axes(P), shape))
end
Base.similar(::Type{A}, sz::Tuple{Vararg{Int}}) where {A<:OffsetArray} = similar(Array{eltype(A)}, sz)
function Base.similar(::Type{T}, shape::Tuple{OffsetAxisKnownLength,Vararg{OffsetAxisKnownLength}}) where {T<:AbstractArray}
    new_shape = map(_strip_IdOffsetRange, shape)
    P = _similar_axes_or_length(T, new_shape, shape)
    OffsetArray(P, map(_offset, axes(P), shape))
end
# Try to use the axes to generate the parent array type
# This is useful if the axes have special meanings, such as with static arrays
# This method is hit if at least one axis provided to similar(A, T, axes) is an IdOffsetRange
# For example this is hit when similar(A::OffsetArray) is called,
# which expands to similar(A, eltype(A), axes(A))
_similar_axes_or_length(A, T, ax, ::Any) = similar(A, T, ax)
_similar_axes_or_length(AT, ax, ::Any) = similar(AT, ax)
# Handle the general case by resorting to lengths along each axis
# This is hit if none of the axes provided to similar(A, T, axes) are IdOffsetRanges,
# and if similar(A, T, axes::AX) is not defined for the type AX.
# In this case the best that we can do is to create a mutable array of the correct size
_similar_axes_or_length(A, T, ax::I, ::I) where {I} = similar(A, T, map(_indexlength, ax))
_similar_axes_or_length(AT, ax::I, ::I) where {I} = similar(AT, map(_indexlength, ax))

# reshape accepts a single colon
Base.reshape(A::AbstractArray, inds::OffsetAxis...) = reshape(A, inds)
function Base.reshape(A::AbstractArray, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}})
    AR = reshape(no_offset_view(A), map(_indexlength, inds))
    O = OffsetArray(AR, map(_offset, axes(AR), inds))
    return _popreshape(O, axes(AR), _filterreshapeinds(inds))
end

# Reshaping OffsetArrays can "pop" the original OffsetArray wrapper and return
# an OffsetArray(reshape(...)) instead of an OffsetArray(reshape(OffsetArray(...)))
# Short-circuit for AbstractVectors if the axes are compatible to get around the Base restriction
# to 1-based vectors
function _reshape(A::AbstractVector, inds::Tuple{OffsetAxis})
    @noinline throw_dimerr(ind::Integer) = throw(
        DimensionMismatch("parent has $(size(A,1)) elements, which is incompatible with length $ind"))
    @noinline throw_dimerr(ind) = throw(
        DimensionMismatch("parent has $(size(A,1)) elements, which is incompatible with indices $ind"))
    _checksize(first(inds), size(A,1)) || throw_dimerr(first(inds))
    A
end
_reshape(A, inds) = _reshape2(A, inds)
_reshape2(A, inds) = reshape(A, inds)
# avoid a stackoverflow by relegating to the parent if no_offset_view returns an offsetarray
_reshape2(A::OffsetArray, inds) = reshape(parent(A), inds)
_reshape_nov(A, inds) = _reshape(no_offset_view(A), inds)

Base.reshape(A::OffsetArray, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}}) =
    OffsetArray(_reshape(parent(A), inds), map(_toaxis, inds))
# And for non-offset axes, we can just return a reshape of the parent directly
Base.reshape(A::OffsetArray, inds::Tuple{Union{Integer,Base.OneTo},Vararg{Union{Integer,Base.OneTo}}}) = _reshape_nov(A, inds)
Base.reshape(A::OffsetArray, inds::Dims) = _reshape_nov(A, inds)
Base.reshape(A::OffsetVector, ::Colon) = A
Base.reshape(A::OffsetVector, ::Tuple{Colon}) = A
Base.reshape(A::OffsetArray, ::Colon) = reshape(A, (Colon(),))
Base.reshape(A::OffsetArray, inds::Union{Int,Colon}...) = reshape(A, inds)
Base.reshape(A::OffsetArray, inds::Tuple{Vararg{Union{Int,Colon}}}) = _reshape_nov(A, inds)
# The following two additional methods for Colon are added to resolve method ambiguities to
# Base: https://github.com/JuliaLang/julia/pull/45387#issuecomment-1132859663
Base.reshape(A::OffsetArray, inds::Colon) = _reshape_nov(A, inds)
Base.reshape(A::OffsetArray, inds::Tuple{Colon}) = _reshape_nov(A, inds)

# permutedims in Base does not preserve axes, and can not be fixed in a non-breaking way
# This is a stopgap solution
Base.permutedims(v::OffsetVector) = reshape(v, (1, axes(v, 1)))

Base.fill(v, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(similar(Array{typeof(v)}, inds), v)
Base.zeros(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(similar(Array{T}, inds), zero(T))
Base.ones(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(similar(Array{T}, inds), one(T))
Base.trues(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(similar(BitArray, inds), true)
Base.falses(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(similar(BitArray, inds), false)

Base.zero(A::OffsetArray) = parent_call(zero, A)
Base.fill!(A::OffsetArray, x) = parent_call(Ap -> fill!(Ap, x), A)

## Indexing

# Note this gets the index of the parent *array*, not the index of the parent *range*
# Here's how one can think about this:
#   Δi = i - first(r)
#   i′ = first(r.parent) + Δi
# and one obtains the result below.
parentindex(r::IdOffsetRange, i) = i - r.offset

@propagate_inbounds Base.getindex(A::OffsetArray{<:Any,0})  = A.parent[]

@inline function Base.getindex(A::OffsetArray{<:Any,N}, I::Vararg{Int,N}) where N
    @boundscheck checkbounds(A, I...)
    J = map(parentindex, axes(A), I)
    @inbounds parent(A)[J...]
end

@propagate_inbounds Base.getindex(A::OffsetArray{<:Any,N}, c::Vararg{Colon,N}) where N =
    parent_call(x -> getindex(x, c...), A)

# With one Colon we use linear indexing.
# In this case we may forward the index to the parent, as the information about the axes is lost
# The exception to this is with OffsetVectors where the axis information is preserved,
# but that case is handled by getindex(::OffsetArray{<:Any,N}, ::Vararg{Colon,N})
@propagate_inbounds Base.getindex(A::OffsetArray, c::Colon) = A.parent[:]

@inline function Base.getindex(A::OffsetVector, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[parentindex(Base.axes1(A), i)]
end
@propagate_inbounds Base.getindex(A::OffsetArray, i::Int)  = parent(A)[i]

@inline function Base.setindex!(A::OffsetArray{T,N}, val, I::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, I...)
    J = map(parentindex, axes(A), I)
    @inbounds parent(A)[J...] = val
    A
end

@inline function Base.setindex!(A::OffsetVector, val, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[parentindex(Base.axes1(A), i)] = val
    A
end
@propagate_inbounds function Base.setindex!(A::OffsetArray, val, i::Int)
    parent(A)[i] = val
    A
end

@inline Base.iterate(a::OffsetArray, i...) = iterate(parent(a), i...)

Base.in(x, A::OffsetArray) = in(x, parent(A))
Base.copy(A::OffsetArray) = parent_call(copy, A)

Base.strides(A::OffsetArray) = strides(parent(A))
Base.elsize(::Type{OffsetArray{T,N,A}}) where {T,N,A} = Base.elsize(A)
@inline Base.unsafe_convert(::Type{Ptr{T}}, A::OffsetArray{T}) where {T} = Base.unsafe_convert(Ptr{T}, parent(A))

# For fast broadcasting: ref https://discourse.julialang.org/t/why-is-there-a-performance-hit-on-broadcasting-with-offsetarrays/32194
Base.dataids(A::OffsetArray) = Base.dataids(parent(A))
Broadcast.broadcast_unalias(dest::OffsetArray, src::OffsetArray) = parent(dest) === parent(src) ? src : Broadcast.unalias(dest, src)

### Special handling for AbstractRange
const OffsetRange{T} = OffsetVector{T,<:AbstractRange{T}}
const OffsetUnitRange{T} = OffsetVector{T,<:AbstractUnitRange{T}}

Base.step(a::OffsetRange) = step(parent(a))

Base.checkindex(::Type{Bool}, inds::AbstractUnitRange, or::OffsetRange) = Base.checkindex(Bool, inds, parent(or))

# Certain special methods for linear indexing with integer ranges (or OffsetRanges)
# These may bypass the default getindex(A, I...) pathway if the parent types permit this
# For example AbstractUnitRanges and Arrays have special linear indexing behavior defined

# If both the arguments are offset, we may unwrap the indices to call (::OffsetArray)[::AbstractRange{Int}]
@propagate_inbounds function Base.getindex(A::OffsetArray, r::OffsetRange{Int})
    _indexedby(A[parent(r)], axes(r))
end
# If the indices are offset, we may unwrap them and pass the parent to getindex
@propagate_inbounds function Base.getindex(A::AbstractRange, r::OffsetRange{Int})
    _indexedby(A[parent(r)], axes(r))
end

# An OffsetUnitRange might use the rapid getindex(::Array, ::AbstractUnitRange{Int}) for contiguous indexing
@propagate_inbounds function Base.getindex(A::Array, r::OffsetUnitRange{Int})
    B = A[_contiguousindexingtype(parent(r))]
    OffsetArray(B, axes(r), checkoverflow = false)
end

# Linear Indexing of OffsetArrays with AbstractUnitRanges may use the faster contiguous indexing methods
@inline function Base.getindex(A::OffsetArray, r::AbstractUnitRange{Int})
    @boundscheck checkbounds(A, r)
    # nD OffsetArrays do not have their linear indices shifted, so we may forward the indices provided to the parent
    @inbounds B = parent(A)[_contiguousindexingtype(r)]
    _indexedby(B, axes(r))
end
@inline function Base.getindex(A::OffsetVector, r::AbstractUnitRange{Int})
    @boundscheck checkbounds(A, r)
    # OffsetVectors may have their linear indices shifted, so we subtract the offset from the indices provided
    @inbounds B = parent(A)[_subtractoffset(r, A.offsets[1])]
    _indexedby(B, axes(r))
end

# This method added mainly to index an OffsetRange with another range
@inline function Base.getindex(A::OffsetVector, r::AbstractRange{Int})
    @boundscheck checkbounds(A, r)
    @inbounds B = parent(A)[_subtractoffset(r, A.offsets[1])]
    _indexedby(B, axes(r))
end

# In general we would pass through getindex(A, I...) which calls to_indices(A, I) and finally to_index(I)
# An OffsetUnitRange{Int} has an equivalent IdOffsetRange with the same values and axes,
# something similar also holds for OffsetUnitRange{BigInt}
# We may replace the former with the latter in an indexing operation to obtain a performance boost
@inline function Base.to_index(r::OffsetUnitRange{<:Union{Int,BigInt}})
    of = first(axes(r,1)) - 1
    IdOffsetRange(_subtractoffset(parent(r), of), of)
end

@inline function _boundscheck_index_retaining_axes(r, s)
    @boundscheck checkbounds(r, s)
    @inbounds pr = r[UnitRange(s)]
    _indexedby(pr, axes(s))
end
@inline _boundscheck_return(r, s) = (@boundscheck checkbounds(r, s); s)

for OR in [:IIUR, :IdOffsetRange]
    for R in [:StepRange, :StepRangeLen, :LinRange, :UnitRange]
        @eval @inline Base.getindex(r::$R, s::$OR) = _boundscheck_index_retaining_axes(r, s)
    end

    # this method is needed for ambiguity resolution
    @eval @inline function Base.getindex(r::StepRangeLen{T,<:Base.TwicePrecision,<:Base.TwicePrecision}, s::$OR) where T
        _boundscheck_index_retaining_axes(r, s)
    end
end
Base.getindex(r::Base.OneTo, s::IdOffsetRange) = _boundscheck_index_retaining_axes(r, s)

# These methods are added to avoid ambiguities with Base.
# The ones involving Base types should be ported to Base and version-limited here
@inline Base.getindex(r::IdentityUnitRange, s::IIUR) = _boundscheck_return(r, s)
@inline Base.getindex(r::IdentityUnitRange, s::IdOffsetRange) = _boundscheck_return(r, s)
if IdentityUnitRange !== Base.Slice
    @inline Base.getindex(r::Base.Slice, s::IIUR) = _boundscheck_return(r, s)
    @inline Base.getindex(r::Base.Slice, s::IdOffsetRange) = _boundscheck_return(r, s)
end

# eltype conversion
# This may use specialized map methods for the parent
Base.map(::Type{T}, O::OffsetArray) where {T} = parent_call(x -> map(T, x), O)
Base.map(::Type{T}, r::IdOffsetRange) where {T<:Real} = _indexedby(map(T, UnitRange(r)), axes(r))
if eltype(IIUR) === Int
    # This is type-piracy, but there is no way to convert an IdentityUnitRange to a non-Int type in Base
    Base.map(::Type{T}, r::IdentityUnitRange) where {T<:Real} = _indexedby(map(T, UnitRange(r)), axes(r))
end

# mapreduce is faster with an IdOffsetRange than with an OffsetUnitRange
# We therefore convert OffsetUnitRanges to IdOffsetRanges with the same values and axes
function Base.mapreduce(f, op, A1::OffsetUnitRange{<:Integer}, As::OffsetUnitRange{<:Integer}...; kw...)
    As = (A1, As...)
    ofs = map(A -> first(axes(A,1)) - 1, As)
    AIds = map((A, of) -> IdOffsetRange(_subtractoffset(parent(A), of), of), As, ofs)
    mapreduce(f, op, AIds...; kw...)
end

# Optimize certain reductions that treat an OffsetVector as a list
for f in [:minimum, :maximum, :extrema, :sum]
    @eval Base.$f(r::OffsetRange) = $f(parent(r))
end

function Base.show(io::IO, r::OffsetRange)
    show(io, r.parent)
    print(io, " with indices ", UnitRange(axes(r, 1)))
end
Base.show(io::IO, ::MIME"text/plain", r::OffsetRange) = show(io, r)


### Some mutating functions defined only for OffsetVector ###

Base.resize!(A::OffsetVector, nl::Integer) = (resize!(A.parent, nl); A)
Base.push!(A::OffsetVector, x...) = (push!(A.parent, x...); A)
Base.pop!(A::OffsetVector) = pop!(A.parent)
Base.append!(A::OffsetVector, items) = (append!(A.parent, items); A)
Base.empty!(A::OffsetVector) = (empty!(A.parent); A)

# These functions keep the summary compact
function Base.inds2string(inds::Tuple{Vararg{Union{IdOffsetRange, IdentityUnitRange{<:IdOffsetRange}}}})
    Base.inds2string(map(UnitRange, inds))
end
Base.showindices(io::IO, ind1::IdOffsetRange, inds::IdOffsetRange...) = Base.showindices(io, map(UnitRange, (ind1, inds...))...)

function Base.showarg(io::IO, @nospecialize(a::OffsetArray), toplevel)
    print(io, "OffsetArray(")
    Base.showarg(io, parent(a), false)
    Base.showindices(io, axes(a)...)
    print(io, ')')
    if toplevel
        print(io, " with eltype ", eltype(a))
    end
end

function Base.replace_in_print_matrix(A::OffsetArray{<:Any,2}, i::Integer, j::Integer, s::AbstractString)
    J = map(parentindex, axes(A), (i,j))
    Base.replace_in_print_matrix(parent(A), J..., s)
end
function Base.replace_in_print_matrix(A::OffsetArray{<:Any,1}, i::Integer, j::Integer, s::AbstractString)
    ip = parentindex(axes(A,1), i)
    Base.replace_in_print_matrix(parent(A), ip, j, s)
end

no_offset_view(A::OffsetArray) = no_offset_view(parent(A))
no_offset_view(a::Array) = a
no_offset_view(i::Number) = i
no_offset_view(A::AbstractArray) = _no_offset_view(axes(A), A)
_no_offset_view(::Tuple{}, A::AbstractArray{T,0}) where T = A
_no_offset_view(::Tuple{Base.OneTo, Vararg{Base.OneTo}}, A::AbstractArray) = A
# the following method is needed for ambiguity resolution
_no_offset_view(::Tuple{Base.OneTo, Vararg{Base.OneTo}}, A::AbstractUnitRange) = A
_no_offset_view(::Any, A::AbstractArray) = OffsetArray(A, Origin(1))
_no_offset_view(::Any, A::AbstractUnitRange) = UnitRange(A)

#####
# center/centered
# These two helpers are deliberately not exported; their meaning can be very different in
# other scenarios and will be very likely to cause name conflicts if exported.
#####
function center(A::AbstractArray, r::RoundingMode=RoundDown)
    map(axes(A)) do inds
        round(Int, (length(inds)-1)/2, r) + first(inds)
    end
end

centered(A::AbstractArray, cp::Dims=center(A)) = OffsetArray(A, .-cp)

centered(A::AbstractArray, i::CartesianIndex) = centered(A, Tuple(i))

# we may pass the searchsorted* functions to the parent, and wrap the offset
for f in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    _safe_f = Symbol("_safe_" * String(f))
    @eval function $_safe_f(v::OffsetArray, x, ilo, ihi, o::Base.Ordering)
        offset = firstindex(v) - firstindex(parent(v))
        $f(parent(v), x, ilo - offset, ihi - offset, o) .+ offset
    end
    @eval Base.$f(v::OffsetVector, x, ilo::T, ihi::T, o::Base.Ordering) where T<:Integer =
        $_safe_f(v, x, ilo, ihi, o)
end

##
# Deprecations
##

# This is a bad API design as it introduces counter intuitive results (#250)
@deprecate centered(A::AbstractArray, r::RoundingMode) OffsetArray(A, .-center(A, r)) false


end # module

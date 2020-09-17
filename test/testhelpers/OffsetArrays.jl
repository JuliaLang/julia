# This file is a part of Julia. License is MIT: https://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OffsetArrays # OffsetArrays@1.0.0 without compat and docstrings

using Base: Indices, tail, @propagate_inbounds
@static if !isdefined(Base, :IdentityUnitRange)
    const IdentityUnitRange = Base.Slice
else
    using Base: IdentityUnitRange
end

export OffsetArray, OffsetVector

struct IdOffsetRange{T<:Integer,I<:AbstractUnitRange{T}} <: AbstractUnitRange{T}
    parent::I
    offset::T
end
IdOffsetRange(r::AbstractUnitRange{T}, offset::Integer) where T =
    IdOffsetRange{T,typeof(r)}(r, convert(T, offset))

@inline Base.axes(r::IdOffsetRange) = (Base.axes1(r),)
@inline Base.axes1(r::IdOffsetRange) = IdOffsetRange(Base.axes1(r.parent), r.offset)
@inline Base.unsafe_indices(r::IdOffsetRange) = (r,)
@inline Base.length(r::IdOffsetRange) = length(r.parent)

function Base.iterate(r::IdOffsetRange)
    ret = iterate(r.parent)
    ret === nothing && return nothing
    return (ret[1] + r.offset, ret[2])
end
function Base.iterate(r::IdOffsetRange, i) where T
    ret = iterate(r.parent, i)
    ret === nothing && return nothing
    return (ret[1] + r.offset, ret[2])
end

@inline Base.first(r::IdOffsetRange) = first(r.parent) + r.offset
@inline Base.last(r::IdOffsetRange) = last(r.parent) + r.offset

@propagate_inbounds Base.getindex(r::IdOffsetRange, i::Integer) = r.parent[i - r.offset] + r.offset
@propagate_inbounds function Base.getindex(r::IdOffsetRange, s::AbstractUnitRange{<:Integer})
    return r.parent[s .- r.offset] .+ r.offset
end

Base.show(io::IO, r::IdOffsetRange) = print(io, first(r), ':', last(r))

# Optimizations
@inline Base.checkindex(::Type{Bool}, inds::IdOffsetRange, i::Real) = Base.checkindex(Bool, inds.parent, i - inds.offset)

## OffsetArray
struct OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
OffsetVector{T,AA<:AbstractArray} = OffsetArray{T,1,AA}

## OffsetArray constructors

offset(axparent::AbstractUnitRange, ax::AbstractUnitRange) = first(ax) - first(axparent)
offset(axparent::AbstractUnitRange, ax::Integer) = 1 - first(axparent)

function OffsetArray(A::AbstractArray{T,N}, offsets::NTuple{N,Int}) where {T,N}
    OffsetArray{T,N,typeof(A)}(A, offsets)
end
OffsetArray(A::AbstractArray{T,0}, offsets::Tuple{}) where T =
    OffsetArray{T,0,typeof(A)}(A, ())

OffsetArray(A::AbstractArray{T,N}, offsets::Vararg{Int,N}) where {T,N} =
    OffsetArray(A, offsets)
OffsetArray(A::AbstractArray{T,0}) where {T} = OffsetArray(A, ())

const ArrayInitializer = Union{UndefInitializer, Missing, Nothing}
OffsetArray{T,N}(init::ArrayInitializer, inds::Indices{N}) where {T,N} =
    OffsetArray(Array{T,N}(init, map(indexlength, inds)), map(indexoffset, inds))
OffsetArray{T}(init::ArrayInitializer, inds::Indices{N}) where {T,N} = OffsetArray{T,N}(init, inds)
OffsetArray{T,N}(init::ArrayInitializer, inds::Vararg{AbstractUnitRange,N}) where {T,N} = OffsetArray{T,N}(init, inds)
OffsetArray{T}(init::ArrayInitializer, inds::Vararg{AbstractUnitRange,N}) where {T,N} = OffsetArray{T,N}(init, inds)

# OffsetVector constructors
OffsetVector(A::AbstractVector, offset) = OffsetArray(A, offset)
OffsetVector{T}(init::ArrayInitializer, inds::AbstractUnitRange) where {T} = OffsetArray{T}(init, inds)

function OffsetArray(A::AbstractArray{T,N}, inds::NTuple{N,AbstractUnitRange}) where {T,N}
    axparent = axes(A)
    lA = map(length, axparent)
    lI = map(length, inds)
    lA == lI || throw(DimensionMismatch("supplied axes do not agree with the size of the array (got size $lA for the array and $lI for the indices"))
    OffsetArray(A, map(offset, axparent, inds))
end
OffsetArray(A::AbstractArray{T,N}, inds::Vararg{AbstractUnitRange,N}) where {T,N} =
    OffsetArray(A, inds)

# avoid a level of indirection when nesting OffsetArrays
function OffsetArray(A::OffsetArray, inds::NTuple{N,AbstractUnitRange}) where {N}
    OffsetArray(parent(A), inds)
end
OffsetArray(A::OffsetArray{T,0}, inds::Tuple{}) where {T} = OffsetArray(parent(A), ())
# OffsetArray(A::OffsetArray{T,N}, inds::Tuple{}) where {T,N} = error("this should never be called")

Base.IndexStyle(::Type{OA}) where {OA<:OffsetArray} = IndexStyle(parenttype(OA))
parenttype(::Type{OffsetArray{T,N,AA}}) where {T,N,AA} = AA
parenttype(A::OffsetArray) = parenttype(typeof(A))

Base.parent(A::OffsetArray) = A.parent

Base.eachindex(::IndexCartesian, A::OffsetArray) = CartesianIndices(axes(A))
Base.eachindex(::IndexLinear, A::OffsetVector)   = axes(A, 1)

@inline Base.size(A::OffsetArray) = size(parent(A))
@inline Base.size(A::OffsetArray, d) = size(parent(A), d)

@inline Base.axes(A::OffsetArray) = map(IdOffsetRange, axes(parent(A)), A.offsets)
@inline Base.axes(A::OffsetArray, d) = d <= ndims(A) ? IdOffsetRange(axes(parent(A), d), A.offsets[d]) : Base.OneTo(1)
@inline Base.axes1(A::OffsetArray{T,0}) where {T} = Base.OneTo(1)  # we only need to specialize this one

const OffsetAxis = Union{Integer, UnitRange, Base.OneTo, IdentityUnitRange, IdOffsetRange, Colon}
Base.similar(A::OffsetArray, ::Type{T}, dims::Dims) where T =
    similar(parent(A), T, dims)
function Base.similar(A::AbstractArray, ::Type{T}, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}}) where T
    B = similar(A, T, map(indexlength, inds))
    return OffsetArray(B, map(offset, axes(B), inds))
end

Base.reshape(A::AbstractArray, inds::OffsetAxis...) = reshape(A, inds)
function Base.reshape(A::AbstractArray, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}})
    AR = reshape(A, map(indexlength, inds))
    return OffsetArray(AR, map(offset, axes(AR), inds))
end

# Reshaping OffsetArrays can "pop" the original OffsetArray wrapper and return
# an OffsetArray(reshape(...)) instead of an OffsetArray(reshape(OffsetArray(...)))
Base.reshape(A::OffsetArray, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}}) =
    OffsetArray(reshape(parent(A), map(indexlength, inds)), map(indexoffset, inds))
# And for non-offset axes, we can just return a reshape of the parent directly
Base.reshape(A::OffsetArray, inds::Tuple{Union{Integer,Base.OneTo},Vararg{Union{Integer,Base.OneTo}}}) = reshape(parent(A), inds)
Base.reshape(A::OffsetArray, inds::Dims) = reshape(parent(A), inds)
Base.reshape(A::OffsetArray, ::Colon) = A
Base.reshape(A::OffsetArray, inds::Union{Int,Colon}...) = reshape(parent(A), inds)
Base.reshape(A::OffsetArray, inds::Tuple{Vararg{Union{Int,Colon}}}) = reshape(parent(A), inds)

function Base.similar(::Type{T}, shape::Tuple{OffsetAxis,Vararg{OffsetAxis}}) where {T<:AbstractArray}
    P = T(undef, map(indexlength, shape))
    OffsetArray(P, map(offset, axes(P), shape))
end

Base.fill(v, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(Array{typeof(v), N}(undef, map(indexlength, inds)), map(indexoffset, inds)), v)
Base.zeros(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(OffsetArray(Array{T, N}(undef, map(indexlength, inds)), map(indexoffset, inds)), zero(T))
Base.ones(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(OffsetArray(Array{T, N}(undef, map(indexlength, inds)), map(indexoffset, inds)), one(T))
Base.trues(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(BitArray{N}(undef, map(indexlength, inds)), map(indexoffset, inds)), true)
Base.falses(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(BitArray{N}(undef, map(indexlength, inds)), map(indexoffset, inds)), false)

## Indexing

# Note this gets the index of the parent *array*, not the index of the parent *range*
# Here's how one can think about this:
#   Δi = i - first(r)
#   i′ = first(r.parent) + Δi
# and one obtains the result below.
parentindex(r::IdOffsetRange, i) = i - r.offset

@propagate_inbounds function Base.getindex(A::OffsetArray{T,N}, I::Vararg{Int,N}) where {T,N}
    J = map(parentindex, axes(A), I)
    return parent(A)[J...]
end

@propagate_inbounds Base.getindex(A::OffsetVector, i::Int) = parent(A)[parentindex(Base.axes1(A), i)]
@propagate_inbounds Base.getindex(A::OffsetArray, i::Int)  = parent(A)[i]

@propagate_inbounds function Base.setindex!(A::OffsetArray{T,N}, val, I::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, I...)
    J = @inbounds map(parentindex, axes(A), I)
    @inbounds parent(A)[J...] = val
    val
end

@propagate_inbounds function Base.setindex!(A::OffsetVector, val, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[parentindex(Base.axes1(A), i)] = val
    val
end
@propagate_inbounds function Base.setindex!(A::OffsetArray, val, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[i] = val
    val
end

# For fast broadcasting: ref https://discourse.julialang.org/t/why-is-there-a-performance-hit-on-broadcasting-with-offsetarrays/32194
Base.dataids(A::OffsetArray) = Base.dataids(parent(A))
Broadcast.broadcast_unalias(dest::OffsetArray, src::OffsetArray) = parent(dest) === parent(src) ? src : Broadcast.unalias(dest, src)

### Special handling for AbstractRange

const OffsetRange{T} = OffsetArray{T,1,<:AbstractRange{T}}
const IIUR = IdentityUnitRange{S} where S<:AbstractUnitRange{T} where T<:Integer

Base.step(a::OffsetRange) = step(parent(a))

Base.getindex(a::OffsetRange, r::OffsetRange) = OffsetArray(a[parent(r)], r.offsets)
Base.getindex(a::OffsetRange, r::AbstractRange) = a.parent[r .- a.offsets[1]]
Base.getindex(a::AbstractRange, r::OffsetRange) = OffsetArray(a[parent(r)], r.offsets)

@propagate_inbounds Base.getindex(r::UnitRange, s::IIUR) =
    OffsetArray(r[s.indices], s)

@propagate_inbounds Base.getindex(r::StepRange, s::IIUR) =
    OffsetArray(r[s.indices], s)

@inline @propagate_inbounds Base.getindex(r::StepRangeLen{T,<:Base.TwicePrecision,<:Base.TwicePrecision}, s::IIUR) where T =
    OffsetArray(r[s.indices], s)
@inline @propagate_inbounds Base.getindex(r::StepRangeLen{T}, s::IIUR) where {T} =
    OffsetArray(r[s.indices], s)

@inline @propagate_inbounds Base.getindex(r::LinRange, s::IIUR) =
    OffsetArray(r[s.indices], s)

function Base.show(io::IO, r::OffsetRange)
    show(io, r.parent)
    o = r.offsets[1]
    print(io, " with indices ", o+1:o+length(r))
end
Base.show(io::IO, ::MIME"text/plain", r::OffsetRange) = show(io, r)

### Convenience functions ###

Base.fill(x, inds::Tuple{UnitRange,Vararg{UnitRange}}) =
    fill!(OffsetArray{typeof(x)}(undef, inds), x)
@inline Base.fill(x, ind1::UnitRange, inds::UnitRange...) = fill(x, (ind1, inds...))


### Some mutating functions defined only for OffsetVector ###

Base.resize!(A::OffsetVector, nl::Integer) = (resize!(A.parent, nl); A)
Base.push!(A::OffsetVector, x...) = (push!(A.parent, x...); A)
Base.pop!(A::OffsetVector) = pop!(A.parent)
Base.empty!(A::OffsetVector) = (empty!(A.parent); A)

### Low-level utilities ###

indexoffset(r::AbstractRange) = first(r) - 1
indexoffset(i::Integer) = 0
indexoffset(i::Colon) = 0
indexlength(r::AbstractRange) = length(r)
indexlength(i::Integer) = i
indexlength(i::Colon) = Colon()

function Base.showarg(io::IO, a::OffsetArray, toplevel)
    print(io, "OffsetArray(")
    Base.showarg(io, parent(a), false)
    if ndims(a) > 0
        print(io, ", ")
        printindices(io, axes(a)...)
    end
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(a))
end
printindices(io::IO, ind1, inds...) =
    (print(io, _unslice(ind1), ", "); printindices(io, inds...))
printindices(io::IO, ind1) = print(io, _unslice(ind1))
_unslice(x) = x
_unslice(x::IdentityUnitRange) = x.indices

function no_offset_view(A::AbstractArray)
    if Base.has_offset_axes(A)
        OffsetArray(A, map(r->1-first(r), axes(A)))
    else
        A
    end
end

no_offset_view(A::OffsetArray) = no_offset_view(parent(A))

end # module

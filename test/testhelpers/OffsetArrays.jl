# This file is a part of Julia. License is MIT: https://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

# OffsetArrays v1.1.3
# No compat patch and docstrings
# cherry-pick 1d294f8ea0f6ccd3a2b413535a6e79fac61af728
# cherry-pick reshape bug fix: https://github.com/JuliaArrays/OffsetArrays.jl/pull/151
module OffsetArrays

using Base: Indices, tail, @propagate_inbounds
using Base: IdentityUnitRange

export OffsetArray, OffsetMatrix, OffsetVector

struct IdOffsetRange{T<:Integer,I<:AbstractUnitRange{T}} <: AbstractUnitRange{T}
    parent::I
    offset::T

    IdOffsetRange{T,I}(r::I, offset::T) where {T<:Integer,I<:AbstractUnitRange{T}} = new{T,I}(r, offset)
end

# Construction/coercion from arbitrary AbstractUnitRanges
function IdOffsetRange{T,I}(r::AbstractUnitRange, offset::Integer = 0) where {T<:Integer,I<:AbstractUnitRange{T}}
    rc, o = offset_coerce(I, r)
    return IdOffsetRange{T,I}(rc, convert(T, o+offset))
end
function IdOffsetRange{T}(r::AbstractUnitRange, offset::Integer = 0) where T<:Integer
    rc = convert(AbstractUnitRange{T}, r)::AbstractUnitRange{T}
    return IdOffsetRange{T,typeof(rc)}(rc, convert(T, offset))
end
IdOffsetRange(r::AbstractUnitRange{T}, offset::Integer = 0) where T<:Integer =
    IdOffsetRange{T,typeof(r)}(r, convert(T, offset))

# Coercion from other IdOffsetRanges
IdOffsetRange{T,I}(r::IdOffsetRange{T,I}) where {T<:Integer,I<:AbstractUnitRange{T}} = r
function IdOffsetRange{T,I}(r::IdOffsetRange) where {T<:Integer,I<:AbstractUnitRange{T}}
    rc, offset = offset_coerce(I, r.parent)
    return IdOffsetRange{T,I}(rc, r.offset+offset)
end
function IdOffsetRange{T}(r::IdOffsetRange) where T<:Integer
    return IdOffsetRange(convert(AbstractUnitRange{T}, r.parent), r.offset)
end
IdOffsetRange(r::IdOffsetRange) = r

# TODO: uncomment these when Julia is ready
# # Conversion preserves both the values and the indexes, throwing an InexactError if this
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
offset_coerce(::Type{I}, r::AbstractUnitRange) where I<:AbstractUnitRange{T} where T =
    convert(I, r), 0

@inline Base.parent(r::IdOffsetRange) = r.parent
@inline Base.axes(r::IdOffsetRange) = (Base.axes1(r),)
@inline Base.axes1(r::IdOffsetRange) = IdOffsetRange(Base.axes1(r.parent), r.offset)
@inline Base.unsafe_indices(r::IdOffsetRange) = (r,)
@inline Base.length(r::IdOffsetRange) = length(r.parent)
Base.reduced_index(i::IdOffsetRange) = typeof(i)(first(i):first(i))
# Workaround for #92 on Julia < 1.4
Base.reduced_index(i::IdentityUnitRange{<:IdOffsetRange}) = typeof(i)(first(i):first(i))

@inline function Base.iterate(r::IdOffsetRange)
    ret = iterate(r.parent)
    ret === nothing && return nothing
    return (ret[1] + r.offset, ret[2])
end
@inline function Base.iterate(r::IdOffsetRange, i)
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
@propagate_inbounds function Base.getindex(r::IdOffsetRange, s::IdOffsetRange)
    return IdOffsetRange(r.parent[s .- r.offset], r.offset)
end

# offset-preserve broadcasting
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(-), r::IdOffsetRange{T}, x::Integer) where T =
    IdOffsetRange{T}(r.parent .- x, r.offset)
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(+), r::IdOffsetRange{T}, x::Integer) where T =
    IdOffsetRange{T}(r.parent .+ x, r.offset)
Broadcast.broadcasted(::Base.Broadcast.DefaultArrayStyle{1}, ::typeof(+), x::Integer, r::IdOffsetRange{T}) where T =
    IdOffsetRange{T}(x .+ r.parent, r.offset)

Base.show(io::IO, r::IdOffsetRange) = print(io, "OffsetArrays.IdOffsetRange(",first(r), ':', last(r),")")

# Optimizations
@inline Base.checkindex(::Type{Bool}, inds::IdOffsetRange, i::Real) = Base.checkindex(Bool, inds.parent, i - inds.offset)

## OffsetArray
struct OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
OffsetVector{T,AA<:AbstractArray} = OffsetArray{T,1,AA}
OffsetMatrix{T,AA<:AbstractArray} = OffsetArray{T,2,AA}

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

# OffsetMatrix constructors
OffsetMatrix(A::AbstractMatrix, offset1, offset2) = OffsetArray(A, offset1, offset2)
OffsetMatrix{T}(init::ArrayInitializer, inds1::AbstractUnitRange, inds2::AbstractUnitRange) where {T} = OffsetArray{T}(init, inds1, inds2)

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
function OffsetArray(A::OffsetArray, offsets::NTuple{N,Int}) where {N}
    OffsetArray(parent(A), offsets .+ A.offsets)
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
@inline Base.axes(A::OffsetArray, d) = d <= ndims(A) ? IdOffsetRange(axes(parent(A), d), A.offsets[d]) : IdOffsetRange(axes(parent(A), d))
@inline Base.axes1(A::OffsetArray{T,0}) where {T} = IdOffsetRange(axes(parent(A), 1))  # we only need to specialize this one

const OffsetAxisKnownLength = Union{Integer, UnitRange, Base.OneTo, IdentityUnitRange, IdOffsetRange}

Base.similar(A::OffsetArray, ::Type{T}, dims::Dims) where T =
    similar(parent(A), T, dims)
function Base.similar(A::AbstractArray, ::Type{T}, inds::Tuple{OffsetAxisKnownLength,Vararg{OffsetAxisKnownLength}}) where T
    B = similar(A, T, map(indexlength, inds))
    return OffsetArray(B, map(offset, axes(B), inds))
end

# reshape accepts a single colon
const OffsetAxis = Union{OffsetAxisKnownLength, Colon}
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
Base.reshape(A::OffsetArray, ::Colon) = reshape(parent(A), Colon())
Base.reshape(A::OffsetVector, ::Colon) = A
Base.reshape(A::OffsetArray, inds::Union{Int,Colon}...) = reshape(parent(A), inds)
Base.reshape(A::OffsetArray, inds::Tuple{Vararg{Union{Int,Colon}}}) = reshape(parent(A), inds)

function Base.similar(::Type{T}, shape::Tuple{OffsetAxis,Vararg{OffsetAxis}}) where {T<:AbstractArray}
    P = T(undef, map(indexlength, shape))
    OffsetArray(P, map(offset, axes(P), shape))
end

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

## Indexing

# Note this gets the index of the parent *array*, not the index of the parent *range*
# Here's how one can think about this:
#   Δi = i - first(r)
#   i′ = first(r.parent) + Δi
# and one obtains the result below.
parentindex(r::IdOffsetRange, i) = i - r.offset

@inline function Base.getindex(A::OffsetArray{T,N}, I::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, I...)
    J = map(parentindex, axes(A), I)
    @inbounds parent(A)[J...]
end

@inline function Base.getindex(A::OffsetVector, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[parentindex(Base.axes1(A), i)]
end
@propagate_inbounds Base.getindex(A::OffsetArray, i::Int)  = parent(A)[i]

@inline function Base.setindex!(A::OffsetArray{T,N}, val, I::Vararg{Int,N}) where {T,N}
    @boundscheck checkbounds(A, I...)
    J = @inbounds map(parentindex, axes(A), I)
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

function Base.inds2string(inds::Tuple{Vararg{Union{IdOffsetRange, IdentityUnitRange{<:IdOffsetRange}}}})
    Base.inds2string(map(UnitRange, inds))
end
Base.showindices(io::IO, ind1::IdOffsetRange, inds::IdOffsetRange...) = Base.showindices(io, map(UnitRange, (ind1, inds...))...)

function Base.showarg(io::IO, a::OffsetArray, toplevel)
    print(io, "OffsetArray(")
    Base.showarg(io, parent(a), false)
    Base.showindices(io, axes(a)...)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(a))
end

function Base.replace_in_print_matrix(A::OffsetArray{<:Any,2}, i::Integer, j::Integer, s::AbstractString)
    J = map(parentindex, axes(A), (i,j))
    Base.replace_in_print_matrix(parent(A), J..., s)
end
function Base.replace_in_print_matrix(A::OffsetArray{<:Any,1}, i::Integer, j::Integer, s::AbstractString)
    ip = parentindex(axes(A,1), i)
    Base.replace_in_print_matrix(parent(A), ip, j, s)
end

function no_offset_view(A::AbstractArray)
    if Base.has_offset_axes(A)
        OffsetArray(A, map(r->1-first(r), axes(A)))
    else
        A
    end
end

no_offset_view(A::OffsetArray) = no_offset_view(parent(A))

end # module

# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHelpers

using Serialization

include("dimensionful.jl")
export Furlong

function open_fake_pty()
    @static if Sys.iswindows()
        error("Unable to create a fake PTY in Windows")
    end

    O_RDWR = Base.Filesystem.JL_O_RDWR
    O_NOCTTY = Base.Filesystem.JL_O_NOCTTY

    fdm = ccall(:posix_openpt, Cint, (Cint,), O_RDWR|O_NOCTTY)
    fdm == -1 && error("Failed to open PTY master")
    rc = ccall(:grantpt, Cint, (Cint,), fdm)
    rc != 0 && error("grantpt failed")
    rc = ccall(:unlockpt, Cint, (Cint,), fdm)
    rc != 0 && error("unlockpt")

    fds = ccall(:open, Cint, (Ptr{UInt8}, Cint),
        ccall(:ptsname, Ptr{UInt8}, (Cint,), fdm), O_RDWR|O_NOCTTY)

    # slave
    slave = RawFD(fds)
    master = Base.TTY(RawFD(fdm); readable = true)
    slave, master
end

function with_fake_pty(f)
    slave, master = open_fake_pty()
    try
        f(slave, master)
    finally
        ccall(:close,Cint,(Cint,),slave) # XXX: this causes the kernel to throw away all unread data on the pty
        close(master)
    end
end

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OAs

using Base: Indices, IndexCartesian, IndexLinear, tail

export OffsetArray

struct OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
OffsetVector{T,AA<:AbstractArray} = OffsetArray{T,1,AA}

OffsetArray(A::AbstractArray{T,N}, offsets::NTuple{N,Int}) where {T,N} = OffsetArray{T,N,typeof(A)}(A, offsets)
OffsetArray(A::AbstractArray{T,N}, offsets::Vararg{Int,N}) where {T,N} = OffsetArray(A, offsets)

OffsetArray{T,N}(::UndefInitializer, inds::Indices{N}) where {T,N} =
    OffsetArray{T,N,Array{T,N}}(Array{T,N}(undef, map(length, inds)), map(indsoffset, inds))
OffsetArray{T}(::UndefInitializer, inds::Indices{N}) where {T,N} =
    OffsetArray{T,N}(undef, inds)

Base.IndexStyle(::Type{T}) where {T<:OffsetArray} = Base.IndexStyle(parenttype(T))
parenttype(::Type{OffsetArray{T,N,AA}}) where {T,N,AA} = AA
parenttype(A::OffsetArray) = parenttype(typeof(A))

Base.parent(A::OffsetArray) = A.parent

Base.size(A::OffsetArray) = size(A.parent)
Base.size(A::OffsetArray, d) = size(A.parent, d)
Base.eachindex(::IndexCartesian, A::OffsetArray) = CartesianIndices(axes(A))
Base.eachindex(::IndexLinear, A::OffsetVector) = axes(A, 1)

# Implementations of indices and axes1. Since bounds-checking is
# performance-critical and relies on indices, these are usually worth
# optimizing thoroughly.
@inline Base.axes(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? Base.Slice(axes(parent(A))[d] .+ A.offsets[d]) : Base.Slice(1:1)
@inline Base.axes(A::OffsetArray) = _indices(axes(parent(A)), A.offsets)  # would rather use ntuple, but see #15276
@inline _indices(inds, offsets) = (Base.Slice(inds[1] .+ offsets[1]), _indices(tail(inds), tail(offsets))...)
_indices(::Tuple{}, ::Tuple{}) = ()
Base.axes1(A::OffsetArray{T,0}) where {T} = Base.Slice(1:1)  # we only need to specialize this one

const OffsetAxis = Union{Integer, UnitRange, Base.Slice{<:UnitRange}}
function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}})
    B = similar(A, T, map(indslength, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.similar(::Type{T}, shape::Tuple{OffsetAxis,Vararg{OffsetAxis}}) where {T<:AbstractArray} =
    OffsetArray(T(undef, map(indslength, shape)), map(indsoffset, shape))

Base.reshape(A::AbstractArray, inds::Tuple{OffsetAxis,Vararg{OffsetAxis}}) = OffsetArray(reshape(A, map(indslength, inds)), map(indsoffset, inds))

Base.fill(v, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(Array{typeof(v), N}(undef, map(indslength, inds)), map(indsoffset, inds)), v)
Base.zeros(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(OffsetArray(Array{T, N}(undef, map(indslength, inds)), map(indsoffset, inds)), zero(T))
Base.ones(::Type{T}, inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {T, N} =
    fill!(OffsetArray(Array{T, N}(undef, map(indslength, inds)), map(indsoffset, inds)), one(T))
Base.trues(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(BitArray{N}(undef, map(indslength, inds)), map(indsoffset, inds)), true)
Base.falses(inds::NTuple{N, Union{Integer, AbstractUnitRange}}) where {N} =
    fill!(OffsetArray(BitArray{N}(undef, map(indslength, inds)), map(indsoffset, inds)), false)

@inline function Base.getindex(A::OffsetArray{T,N}, I::Vararg{Int,N}) where {T,N}
    checkbounds(A, I...)
    @inbounds ret = parent(A)[offset(A.offsets, I)...]
    ret
end
# Vectors don't support one-based linear indexing; they always use the offsets
@inline function Base.getindex(A::OffsetVector, i::Int)
    checkbounds(A, i)
    @inbounds ret = parent(A)[offset(A.offsets, (i,))[1]]
    ret
end
# But multidimensional arrays allow one-based linear indexing
@inline function Base.getindex(A::OffsetArray, i::Int)
    checkbounds(A, i)
    @inbounds ret = parent(A)[i]
    ret
end
@inline function Base.setindex!(A::OffsetArray{T,N}, val, I::Vararg{Int,N}) where {T,N}
    checkbounds(A, I...)
    @inbounds parent(A)[offset(A.offsets, I)...] = val
    val
end
@inline function Base.setindex!(A::OffsetVector, val, i::Int)
    checkbounds(A, i)
    @inbounds parent(A)[offset(A.offsets, (i,))[1]] = val
    val
end
@inline function Base.setindex!(A::OffsetArray, val, i::Int)
    checkbounds(A, i)
    @inbounds parent(A)[i] = val
    val
end

@inline function Base.deleteat!(A::OffsetArray, i::Int)
    checkbounds(A, i)
    @inbounds deleteat!(parent(A), offset(A.offsets, (i,))[1])
end

@inline function Base.deleteat!(A::OffsetArray{T,N}, I::Vararg{Int, N}) where {T,N}
    checkbounds(A, I...)
    @inbounds deleteat!(parent(A), offset(A.offsets, I)...)
end

@inline function Base.deleteat!(A::OffsetArray, i::UnitRange{Int})
    checkbounds(A, first(i))
    checkbounds(A, last(i))
    first_idx = offset(A.offsets, (first(i),))[1]
    last_idx = offset(A.offsets, (last(i),))[1]
    @inbounds deleteat!(parent(A), first_idx:last_idx)
end

# Computing a shifted index (subtracting the offset)
offset(offsets::NTuple{N,Int}, inds::NTuple{N,Int}) where {N} = _offset((), offsets, inds)
_offset(out, ::Tuple{}, ::Tuple{}) = out
@inline _offset(out, offsets, inds) = _offset((out..., inds[1]-offsets[1]), Base.tail(offsets), Base.tail(inds))

indsoffset(r::AbstractRange) = first(r) - 1
indsoffset(i::Integer) = 0
indslength(r::AbstractRange) = length(r)
indslength(i::Integer) = i


Base.resize!(A::OffsetVector, nl::Integer) = (resize!(A.parent, nl); A)

end

# Mimic a quantity with a physical unit that is not convertible to a real number
struct PhysQuantity{n,T}   # n is like the exponent of the unit
    val::T
end
PhysQuantity{n}(x::T) where {n,T} = PhysQuantity{n,T}(x)
Base.zero(::Type{PhysQuantity{n,T}}) where {n,T} = PhysQuantity{n,T}(zero(T))
Base.zero(x::PhysQuantity) = zero(typeof(x))
Base.:+(x::PhysQuantity{n}, y::PhysQuantity{n}) where n = PhysQuantity{n}(x.val + y.val)
Base.:-(x::PhysQuantity{n}, y::PhysQuantity{n}) where n = PhysQuantity{n}(x.val - y.val)
Base.:*(x::PhysQuantity{n,T}, y::Int) where {n,T} = PhysQuantity{n}(x.val*y)
Base.:/(x::PhysQuantity{n,T}, y::Int) where {n,T} = PhysQuantity{n}(x.val/y)
Base.:*(x::PhysQuantity{n1,S}, y::PhysQuantity{n2,T}) where {n1,n2,S,T} =
    PhysQuantity{n1+n2}(x.val*y.val)
Base.:/(x::PhysQuantity{n1,S}, y::PhysQuantity{n2,T}) where {n1,n2,S,T} =
    PhysQuantity{n1-n2}(x.val/y.val)
Base.convert(::Type{PhysQuantity{0,T}}, x::Int) where T = PhysQuantity{0}(convert(T, x))
Base.convert(::Type{P}, ::Int) where P<:PhysQuantity =
    error("Int is incommensurate with PhysQuantity")

end

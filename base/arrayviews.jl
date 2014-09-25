module ArrayViews

import Base: Ptr, convert, eltype, getindex, getindex_bool_1d, length, ndims, parent, pointer, setindex!, similar, size, stride, strides, to_index

export ArrayView, ContiguousArray, ContiguousVector, ContiguousMatrix, ContiguousView, StridedView
export contiguousrank, contiguous_view, ellipview, iscontiguous, reshape_view, strided_view, view

#################################################
#
#  View types
#
#################################################

### Type definitions

# type parameters:
#   T: element type
#   N: the number of dimensions
#   M: contiguous rank
#
# Note: M is the maximum dimension such that 
# slices up to this dimension are contiguous.
#
# For example, given a 3D contiguous array a,
# the contiguous rank of a is 3, that of 
# view(a, :, :, 1:3) is 2, and that of
# view(a, :, 1:2, 1:3) is 1, etc.
#
# This rank plays a key role in efficient 
# linear indexing and type-stable subview
# calculation.
#
abstract ArrayView{T,N,M} <: DenseArray{T,N}

# a type for indicating contiguous rank (statically)
type ContRank{M} end

# use ContiguousView when contiguousness can be determined statically
immutable ContiguousView{T,N,Arr<:Array{T}} <: ArrayView{T,N,N}
    arr::Arr
    offset::Int
    len::Int
    shp::NTuple{N,Int}
end

contiguous_view{T,N}(arr::Array{T}, offset::Int, shp::NTuple{N,Int}) = 
    ContiguousView{T,N,typeof(arr)}(arr, offset, prod(shp), shp)

contiguous_view(arr::Array, shp::Dims) = contiguous_view(arr, 0, shp)

contiguous_view(v::ContiguousView, shp::Dims) = contiguous_view(v.arr, v.offset, shp)
contiguous_view(v::ContiguousView, offset::Int, shp::Dims) = contiguous_view(v.arr, offset + v.offset, shp)

# use StridedView otherwise
# condition: M <= N
immutable StridedView{T,N,M,Arr<:Array{T}} <: ArrayView{T,N,M}
    arr::Arr
    offset::Int
    len::Int
    shp::NTuple{N,Int}
    strides::NTuple{N,Int}
end

function strided_view{T,N,M}(arr::Array{T}, offset::Int, shp::NTuple{N,Int}, 
                             ::Type{ContRank{M}}, strides::NTuple{N,Int})
    @assert M <= N
    StridedView{T,N,M,typeof(arr)}(arr, offset, prod(shp), shp, strides)
end

function strided_view{T,N,M}(arr::Array{T}, shp::NTuple{N,Int}, 
                             ::Type{ContRank{M}}, strides::NTuple{N,Int})
    @assert M <= N
    StridedView{T,N,M,typeof(arr)}(arr, 0, prod(shp), shp, strides)
end


### Basic methods

# generic methods

contiguousrank{T,N,M}(a::ArrayView{T,N,M}) = M
contrank{T,N}(a::Array{T,N}) = ContRank{N}
contrank{T,N,M}(a::ArrayView{T,N,M}) = ContRank{M}

getdim{N}(s::NTuple{N,Int}, d::Integer) = 
    (d > 0 || error("dimension out of range."); d <= N ? s[d] : 1)
size{T,N}(a::ArrayView{T,N}, d::Integer) = getdim(size(a), d)

pointer(a::ArrayView) = pointer(parent(a), offset(a)+1)
convert{T}(::Type{Ptr{T}}, a::ArrayView{T}) = pointer(a)
convert{T,N}(::Type{Array{T,N}}, A::ArrayView{T,N}) = copy(A)

similar{T}(a::ArrayView{T}) = Array(T, size(a))
similar{T}(a::ArrayView{T}, dims::Dims) = Array(T, dims)
similar{T}(a::ArrayView, ::Type{T}, dims::Dims) = Array(T, dims)

# methods specific to ContiguousView

offset(a::Array) = 0

parent(a::ContiguousView) = a.arr
offset(a::ContiguousView) = a.offset
length(a::ContiguousView) = a.len
size(a::ContiguousView) = a.shp

iscontiguous(a::AbstractArray) = false
iscontiguous(a::Array) = true
iscontiguous(a::ContiguousView) = true

strides{T}(a::ContiguousView{T,1}) = (1,)
strides{T}(a::ContiguousView{T,2}) = (1, a.shp[1])
strides{T}(a::ContiguousView{T,3}) = (1, a.shp[1], a.shp[1] * a.shp[2])
strides{T}(a::ContiguousView{T,4}) = 
    (1, a.shp[1], a.shp[1] * a.shp[2], a.shp[1] * a.shp[2] * a.shp[3])

function strides{T,N}(a::ContiguousView{T,N})
    s = Array(Int, N)
    s[1] = p = 1
    d = 1
    while d < N
        p *= a.shp[d]
        d += 1
        s[d] = p
    end 
    return tuple(s...)::NTuple{N,Int}
end

stride{T}(a::ContiguousView{T,1}, d::Integer) = 
    (d > 0 || error("dimension out of range."); 
     d == 1 ? 1 : length(a))::Int

stride{T}(a::ContiguousView{T,2}, d::Integer) = 
    (d > 0 || error("dimension out of range."); 
     d == 1 ? 1 : 
     d == 2 ? a.shp[1] : length(a))::Int

stride{T}(a::ContiguousView{T,3}, d::Integer) = 
    (d > 0 || error("dimension out of range."); 
     d == 1 ? 1 : 
     d == 2 ? a.shp[1] : 
     d == 3 ? a.shp[1] * a.shp[2] : length(a))::Int

stride{T,N}(a::ContiguousView{T,N}, d::Integer) = 
    (d > 0 || error("dimension out of range."); 
     d == 1 ? 1 : 
     d == 2 ? a.shp[1] :
     d <= N ? *(a.shp[1:d-1]...) : length(a))::Int

# methods specific to StridedView

parent(a::StridedView) = a.arr
offset(a::StridedView) = a.offset
length(a::StridedView) = a.len
size(a::StridedView) = a.shp

strides(a::StridedView) = a.strides
stride{T,N}(a::StridedView{T,N}, d::Integer) = 
    (1 <= d <= N || error("dimension out of range."); 
     a.strides[d])

iscontiguous{T,N}(a::StridedView{T,N,N}) = true;
iscontiguous{T,N}(a::StridedView{T,N,N}) = true;
iscontiguous{T,N}(a::StridedView{T,N}) = _iscontiguous(a.shp, a.strides)

_iscontiguous(shp::(), strides::()) = true
_iscontiguous(shp::(Int,), strides::(Int,)) = (strides[1] == 1)
_iscontiguous(shp::(Int,Int), strides::(Int,Int)) = 
    (strides[1] == 1 && strides[2] == shp[1])
_iscontiguous(shp::(Int,Int,Int), strides::(Int,Int,Int)) = 
    (strides[1] == 1 && 
     strides[2] == shp[1] &&
     strides[3] == shp[1] * shp[2])

function _iscontiguous{N}(shp::NTuple{N,Int}, strides::NTuple{N,Int})
    s = 1
    for i = 1:N
        if strides[i] != s
            return false
        end
        s *= shp[i]
    end
    return true
end


#################################################
#
#  Indexing
#
#################################################

# Note: each subtype of ArrayView should implement uindex methods,
# which getindex and setindex! rely on to locate the element position

# getindex

getindex(a::ArrayView) = getindex(parent(a), offset(a) + 1)

getindex(a::ArrayView, i::Int) = getindex(parent(a), uindex(a, i))
getindex(a::ArrayView, i1::Int, i2::Int) = getindex(parent(a), uindex(a, i1, i2))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int) = 
    getindex(parent(a), uindex(a, i1, i2, i3))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int) = 
    getindex(parent(a), uindex(a, i1, i2, i3, i4))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) = 
    getindex(parent(a), uindex(a, i1, i2, i3, i4, i5))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int, i6::Int, I::Int...) = 
    getindex(parent(a), uindex(a, i1, i2, i3, i4, i5, i6, I...))

getindex(a::ArrayView, i::Real) = getindex(a, to_index(i))
getindex(a::ArrayView, i1::Real, i2::Real) = getindex(a, to_index(i1), to_index(i2))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real, I::Int...) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), 
                to_index(i4), to_index(i5), to_index(i6), I...)

# setindex!

setindex!{T}(a::ArrayView{T}, v, i::Int) = setindex!(parent(a), convert(T, v), uindex(a, i))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int) = 
    setindex!(parent(a), convert(T, v), uindex(a, i1, i2))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int) = 
    setindex!(parent(a), convert(T, v), uindex(a, i1, i2, i3))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int) = 
    setindex!(parent(a), convert(T, v), uindex(a, i1, i2, i3, i4))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) = 
    setindex!(parent(a), convert(T, v), uindex(a, i1, i2, i3, i4, i5))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int, i6::Int, I::Int...) = 
    setindex!(parent(a), convert(T, v), uindex(a, i1, i2, i3, i4, i5, i6, I...))

setindex!(a::ArrayView, v, i::Real) = setindex!(a, v, to_index(i))
setindex!(a::ArrayView, v, i1::Real, i2::Real) = setindex!(a, v, to_index(i1), to_index(i2))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), 
                    to_index(i4), to_index(i5), to_index(i6))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real, I::Int...) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), 
                    to_index(i4), to_index(i5), to_index(i6), I...)

# index calculation for ContiguousView

uindex{T,N}(a::ArrayView{T,N,N}, i::Int) = offset(a) + i
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int) = 
    offset(a) + sub2ind(size(a), i1, i2)
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int, i3::Int) = 
    offset(a) + sub2ind(size(a), i1, i2, i3)
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int, i3::Int, i4::Int, I::Int...) = 
    offset(a) + sub2ind(size(a), i1, i2, i3, i4, I...)

# index calculation for StridedView

uindex{T}(a::StridedView{T,0}, i::Int) = 1

uindex{T}(a::StridedView{T,1,0}, i::Int) = a.offset + 1 + (i-1)*a.strides[1]
uindex{T}(a::StridedView{T,1,1}, i::Int) = a.offset + i
uindex{T}(a::StridedView{T,1}, i1::Int, i2::Int) = 
    (i2 == 1 || throw(BoundsError()); uindex(a, i1))
uindex{T}(a::StridedView{T,1}, i1::Int, i2::Int, i3::Int) = 
    ((i2 == i3 == 1) || throw(BoundsError()); uindex(a, i1))

uindex{T}(a::StridedView{T,2}, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T}(a::StridedView{T,2,0}, i1::Int, i2::Int) = 
    a.offset + 1 + (i1-1)*a.strides[1] + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2,1}, i1::Int, i2::Int) = 
    a.offset + i1 + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2,2}, i1::Int, i2::Int) = 
    a.offset + i1 + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2}, i1::Int, i2::Int, i3::Int) = 
    (i3 == 1 || throw(BoundsError()); uindex(a, i1, i2))

uindex{T}(a::StridedView{T,3}, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T}(a::StridedView{T,3}, i1::Int, i2::Int) = 
    uindex(a, i1, ind2sub((a.shp[2], a.shp[3]), i2)...)
uindex{T}(a::StridedView{T,3}, i1::Int, i2::Int, i3::Int) = 
    a.offset + i1 + (i2-1)*a.strides[2] + (i3-1)*a.strides[3]
uindex{T}(a::StridedView{T,3,0}, i1::Int, i2::Int, i3::Int) = 
    a.offset + 1 + (i1-1)*a.strides[1] + (i2-1)*a.strides[2] + (i3-1)*a.strides[3]

uindex(a::StridedView, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T,N}(a::StridedView{T,N}, i1::Int, i2::Int) = 
    uindex(a, i1, ind2sub(a.shp[2:N], i2)...)
uindex{T,N}(a::StridedView{T,N}, i1::Int, i2::Int, i3::Int) = 
    uindex(a, i1, i2, ind2sub(a.shp[3:N], i3)...)
uindex{T}(a::StridedView{T}, i1::Int, i2::Int, i3::Int, i4::Int, I::Int...) = 
    _uindex(a, tuple(i1, i2, i3, i4, I...))::Int

function _uindex{T,N,L}(a::StridedView{T,N}, subs::NTuple{L,Int})
    if L == N
        s = a.offset + 1
        for i = 1:N
            s += (subs[i] - 1) * a.strides[i]
        end
        return s

    elseif L < N
        return uindex(a, tuple(subs[1:L-1]..., ind2sub(a.shp[L+1:N], subs[L])...))

    else # L > N
        for i = N+1:L
            subs[i] == 1 || throw(BoundsError())
        end
        return uindex(a, subs[1:N]...)
    end
end


#################################################
#
#  Sub-views
#
#################################################

# Note: construction of subviews involve several steps:
#
#  - compute view offset (w.r.t. the parent)
#  - compute view shape
#  - compute strides (for non-contiguous views only)
#  - decide contiguous rank (statically)
#  - make the view
#

## auxiliary union types to simplify method definition
## (for internal use only)


typealias Subs Union(Real,Colon,Range)
typealias SubsNC Union(Real,Range)
typealias SubsRange Union(Colon,Range)

##### Compute offset #####

_offset(i::Colon) = 0
_offset(i::Int) = i - 1
_offset(i::Real) = to_index(i) - 1
_offset(i::Range) = to_index(first(i)) - 1

_step(i::Real) = 1
_step(i::Colon) = 1
_step(i::Range) = step(i)

# aoffset: offset w.r.t. the underlying array (i.e. parent)

aoffset(a::Array, i::Subs) = roffset(a, i)
aoffset(a::Array, i1::Subs, i2::Subs) = roffset(a, i1, i2)
aoffset(a::Array, i1::Subs, i2::Subs, i3::Subs) = roffset(a, i1, i2, i3)
aoffset(a::Array, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    roffset(a, i1, i2, i3, i4)
aoffset(a::Array, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    roffset(a, i1, i2, i3, i4, i5, I...)

aoffset(a::ArrayView, i::Subs) = offset(a) + roffset(a, i)
aoffset(a::ArrayView, i1::Subs, i2::Subs) = offset(a) + roffset(a, i1, i2)
aoffset(a::ArrayView, i1::Subs, i2::Subs, i3::Subs) = 
    offset(a) + roffset(a, i1, i2, i3)
aoffset(a::ArrayView, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    offset(a) + roffset(a, i1, i2, i3, i4)
aoffset(a::ArrayView, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    offset(a) + roffset(a, i1, i2, i3, i4, i5, I...)

# roffset: offset w.r.t. the first element of the view

# for contiguous arrays

typealias ContiguousArray{T,N} Union(Array{T,N}, ContiguousView{T,N})
typealias ContiguousVector{T} ContiguousArray{T,1}
typealias ContiguousMatrix{T} ContiguousArray{T,2}

# 1D

roffset(a::ContiguousArray, i::Colon) = 0
roffset(a::ContiguousArray, i::SubsNC) = _offset(i)

roffset(a::StridedVector, i::Colon) = 0
roffset(a::StridedVector, i::SubsNC) = _offset(i) * stride(a,1)

# 2D

roffset(a::ContiguousArray, i1::Colon, i2::Colon) = 0
roffset(a::ContiguousArray, i1::Colon, i2::SubsNC) = size(a,1) * _offset(i2)
roffset(a::ContiguousArray, i1::SubsNC, i2::Colon) = _offset(i1)
roffset(a::ContiguousArray, i1::SubsNC, i2::SubsNC) = 
    _offset(i1) + size(a,1) * _offset(i2)

roffset(a::StridedMatrix, i1::Colon, i2::Colon) = 0
roffset(a::StridedMatrix, i1::Colon, i2::SubsNC) = _offset(i2) * stride(a,2)
roffset(a::StridedMatrix, i1::SubsNC, i2::Colon) = _offset(i1) * stride(a,1)
roffset(a::StridedMatrix, i1::SubsNC, i2::SubsNC) = 
    _offset(i1) * stride(a,1) + _offset(i2) * stride(a,2)

# 3D

roffset(a::ContiguousArray, i1::Colon, i2::Colon, i3::Colon) = 0
roffset(a::ContiguousArray, i1::Colon, i2::Colon, i3::SubsNC) = 
    size(a,1) * size(a,2) * _offset(i3)
roffset(a::ContiguousArray, i1::Colon, i2::SubsNC, i3::Colon) = 
    size(a,1) * _offset(i2)
roffset(a::ContiguousArray, i1::Colon, i2::SubsNC, i3::SubsNC) = 
    size(a,1) * (_offset(i2) + size(a,2) * _offset(i3))
roffset(a::ContiguousArray, i1::SubsNC, i2::Colon, i3::Colon) = _offset(i1)
roffset(a::ContiguousArray, i1::SubsNC, i2::Colon, i3::SubsNC) = 
    _offset(i1) + (size(a,1) * size(a,2) * _offset(i3))
roffset(a::ContiguousArray, i1::SubsNC, i2::SubsNC, i3::Colon) = 
    _offset(i1) + size(a,1) * _offset(i2)
roffset(a::ContiguousArray, i1::SubsNC, i2::SubsNC, i3::SubsNC) = 
    _offset(i1) + size(a,1) * (_offset(i2) + size(a,2) * _offset(i3))

roffset{T}(a::StridedArray{T,3}, i1::Colon, i2::Colon, i3::Colon) = 0
roffset{T}(a::StridedArray{T,3}, i1::Colon, i2::Colon, i3::SubsNC) = 
    _offset(i3) * stride(a,3)
roffset{T}(a::StridedArray{T,3}, i1::Colon, i2::SubsNC, i3::Colon) = 
    _offset(i2) * stride(a,2)
roffset{T}(a::StridedArray{T,3}, i1::Colon, i2::SubsNC, i3::SubsNC) = 
    _offset(i2) * stride(a,2) + _offset(i3) * stride(a,3)
roffset{T}(a::StridedArray{T,3}, i1::SubsNC, i2::Colon, i3::Colon) = 
    _offset(i1) * stride(a,1)
roffset{T}(a::StridedArray{T,3}, i1::SubsNC, i2::Colon, i3::SubsNC) = 
    _offset(i1) * stride(a,1) + _offset(i3) * stride(a,3)
roffset{T}(a::StridedArray{T,3}, i1::SubsNC, i2::SubsNC, i3::Colon) = 
    _offset(i1) * stride(a,1) + _offset(i2) * stride(a,2)
roffset{T}(a::StridedArray{T,3}, i1::SubsNC, i2::SubsNC, i3::SubsNC) = 
    _offset(i1) * stride(a,1) + _offset(i2) * stride(a,2) + _offset(i3) * stride(a,3)

# General

roffset(a::ContiguousArray, i1::Colon, i2::Colon, i3::Colon, i4::Colon, I::Colon...) = 0

function roffset(a::ContiguousArray, i1::Subs, i2::Subs, i3::Subs, i4::Subs, I::Subs...)
    o = _offset(i1)
    s = size(a,1)
    o += s * _offset(i2)
    o += (s *= size(a,2)) * _offset(i3)
    o += (s *= size(a,3)) * _offset(i4)
    for i = 1:length(I)
        o += (s *= size(a,i+3)) * _offset(I[i])
    end
    return o::Int
end

roffset(a::StridedArray, i1::Subs, i2::Subs, i3::Subs, I::Subs...) = 
    _roffset(strides(a), tuple(i1, i2, i3, I...))::Int

function _roffset{N}(ss::NTuple{N,Int}, subs::NTuple{N})
    o = _offset(subs[1]) * ss[1]
    for i = 2:N
        o += _offset(subs[i]) * ss[i]
    end
    return o::Int
end


##### Compute view shape #####

_dim(a::AbstractArray, d::Int, r::Colon) = size(a, d)
_dim(a::AbstractArray, d::Int, r::Range) = length(r)
_dim(a::AbstractArray, d::Int, r::Real) = 1

_dim{N}(siz::NTuple{N,Int}, d::Int, r::Colon) = d <= N ? siz[d] : 1
_dim(siz::Tuple, d::Int, r::Range) = length(r)
_dim(siz::Tuple, d::Int, r::Real) = 1

# 1D
vshape(a::DenseArray, i::Real) = ()
vshape(a::DenseArray, i::Colon) = (length(a),)
vshape(a::DenseArray, i::Range) = (length(i),)

# 2D

_succlen2{T}(a::DenseArray{T,1}) = 1
_succlen2{T}(a::DenseArray{T,2}) = size(a, 2)
_succlen2{T}(a::DenseArray{T,3}) = size(a, 2) * size(a, 3)
_succlen2(a::DenseArray) = prod(size(a)[2:end])::Int

vshape(a::DenseArray, i1::Real, i2::Real) = ()
vshape(a::DenseArray, i1::SubsRange, i2::Real) = (_dim(a,1,i1),)
vshape(a::DenseArray, i1::Subs, i2::Colon) = (_dim(a,1,i1), _succlen2(a))
vshape(a::DenseArray, i1::Subs, i2::Range) = (_dim(a,1,i1), length(i2))

# 3D

_succlen3{T}(a::DenseArray{T,1}) = 1
_succlen3{T}(a::DenseArray{T,2}) = 1
_succlen3{T}(a::DenseArray{T,3}) = size(a, 3)
_succlen3{T}(a::DenseArray{T,4}) = size(a, 3) * size(a, 4)
_succlen3(a::DenseArray) = prod(size(a)[3:end])::Int

vshape(a::DenseArray, i1::Real, i2::Real, i3::Real) = ()
vshape(a::DenseArray, i1::SubsRange, i2::Real, i3::Real) = (_dim(a,1,i1),)
vshape(a::DenseArray, i1::Subs, i2::SubsRange, i3::Real) = 
    (_dim(a,1,i1), _dim(a,2,i2))

vshape(a::DenseArray, i1::Subs, i2::Subs, i3::Colon) = 
    (_dim(a,1,i1), _dim(a,2,i2), _succlen3(a))
vshape(a::DenseArray, i1::Subs, i2::Subs, i3::Range) = 
    (_dim(a,1,i1), _dim(a,2,i2), length(i3))

# 4D

_succlen4{T}(a::DenseArray{T,1}) = 1
_succlen4{T}(a::DenseArray{T,2}) = 1
_succlen4{T}(a::DenseArray{T,3}) = 1
_succlen4{T}(a::DenseArray{T,4}) = size(a, 4) 
_succlen4{T}(a::DenseArray{T,5}) = size(a, 4) * size(a, 5)
_succlen4(a::DenseArray) = prod(size(a)[4:end])::Int

vshape(a::DenseArray, i1::Real, i2::Real, i3::Real, i4::Real) = ()
vshape(a::DenseArray, i1::SubsRange, i2::Real, i3::Real, i4::Real) = (_dim(a,1,i1),)
vshape(a::DenseArray, i1::Subs, i2::SubsRange, i3::Real, i4::Real) = 
    (_dim(a,1,i1), _dim(a,2,i2))
vshape(a::DenseArray, i1::Subs, i2::Subs, i3::SubsRange, i4::Real) = 
    (_dim(a,1,i1), _dim(a,2,i2), _dim(a,3,i3))

vshape(a::DenseArray, i1::Subs, i2::Subs, i3::Subs, i4::Colon) = 
    (_dim(a,1,i1), _dim(a,2,i2), _dim(a,3,i3), _succlen4(a))
vshape(a::DenseArray, i1::Subs, i2::Subs, i3::Subs, i4::Range) = 
    (_dim(a,1,i1), _dim(a,2,i2), _dim(a,3,i3), length(i4))

# multi-dimensional

vshape{T,N}(a::DenseArray{T,N}, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    _vshape(size(a), i1, i2, i3, i4, i5, I...)

_vshape{N}(siz::NTuple{N,Int}, i1::Real) = ()
_vshape{N}(siz::NTuple{N,Int}, i1::Real, i2::Real...) = ()

_vshape{N}(siz::NTuple{N,Int}, i1::Colon) = (prod(siz),)
_vshape{N}(siz::NTuple{N,Int}, i1::Range) = (length(i1),)
_vshape{N}(siz::NTuple{N,Int}, i1::Subs, i2::Subs...) = tuple(_dim(siz,1,i1), _vshape(siz[2:N], i2...)...)


##### Compute strides #####

# 1D

vstrides(a::ContiguousArray, i::Subs) = (_step(i),)
vstrides(a::DenseArray, i::Subs) = (stride(a,1) * _step(i),)

# 2D

vstrides(a::ContiguousArray, i1::Subs, i2::Real) = (_step(i1),)
vstrides(a::ContiguousArray, i1::Subs, i2::Union(Colon,UnitRange)) = (_step(i1), stride(a,2))
vstrides(a::ContiguousArray, i1::Subs, i2::Range) = (_step(i1), stride(a,2) * _step(i2))

vstrides(a::DenseArray, i1::Subs, i2::Real) = (stride(a,1) * _step(i1),)
vstrides(a::DenseArray, i1::Subs, i2::Subs) = 
    (stride(a,1) * _step(i1), stride(a,2) * _step(i2))

# 3D

vstrides(a::ContiguousArray, i1::Subs, i2::Real, i3::Real) = 
    (_step(i1),)
vstrides(a::ContiguousArray, i1::Subs, i2::Subs, i3::Real) = 
    (_step(i1), stride(a,2) * _step(i2))
vstrides(a::ContiguousArray, i1::Subs, i2::Subs, i3::Subs) = 
    (_step(i1), stride(a,2) * _step(i2), stride(a,3) * _step(i3))

vstrides(a::DenseArray, i1::Subs, i2::Real, i3::Real) = 
    (stride(a,1) * _step(i1),)
vstrides(a::DenseArray, i1::Subs, i2::Subs, i3::Real) = 
    (stride(a,1) * _step(i1), stride(a,2) * _step(i2))
vstrides(a::DenseArray, i1::Subs, i2::Subs, i3::Subs) = 
    (stride(a,1) * _step(i1), stride(a,2) * _step(i2), stride(a,3) * _step(i3))

# multi-dimensional array

vstrides(a::DenseArray, i1::Subs, i2::Subs, i3::Subs, i4::Subs, I::Subs...) = 
    _vstrides(strides(a), 1, i1, i2, i3, i4, I...)

_vstrides{N}(ss::NTuple{N,Int}, k::Int, i1::Real, i2::Real) = ()
_vstrides{N}(ss::NTuple{N,Int}, k::Int, i1::Subs, i2::Real) = 
    (ss[k] * _step(i1),)
_vstrides{N}(ss::NTuple{N,Int}, k::Int, i1::Subs, i2::Subs) = 
    (ss[k] * _step(i1), ss[k+1] * _step(i2))

_vstrides{N}(ss::NTuple{N,Int}, k::Int, i1::Real, i2::Real, i3::Real, I::Real...) = ()
_vstrides{N}(ss::NTuple{N,Int}, k::Int, i1::Subs, i2::Subs, i3::Subs, I::Subs...) = 
    tuple(ss[k] * _step(i1), _vstrides(ss, k+1, i2, i3, I...)...)


##### View construction ######

make_view{N}(a::DenseArray, cr::Type{ContRank{N}}, shp::NTuple{N,Int}, i::Subs) = 
    contiguous_view(parent(a), aoffset(a, i), shp)

make_view{N}(a::DenseArray, cr::Type{ContRank{N}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs) = 
    contiguous_view(parent(a), aoffset(a, i1, i2), shp)

make_view{N}(a::DenseArray, cr::Type{ContRank{N}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs) = 
    contiguous_view(parent(a), aoffset(a, i1, i2, i3), shp)

make_view{N}(a::DenseArray, cr::Type{ContRank{N}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    contiguous_view(parent(a), aoffset(a, i1, i2, i3, i4), shp)

make_view{N}(a::DenseArray, cr::Type{ContRank{N}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    contiguous_view(parent(a), aoffset(a, i1, i2, i3, i4, i5, I...), shp)

make_view{M,N}(a::DenseArray, cr::Type{ContRank{M}}, shp::NTuple{N,Int}, i::Subs) = 
    strided_view(parent(a), aoffset(a, i), shp, cr, vstrides(a, i))

make_view{M,N}(a::DenseArray, cr::Type{ContRank{M}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs) = 
    strided_view(parent(a), aoffset(a, i1, i2), shp, cr, vstrides(a, i1, i2))

make_view{M,N}(a::DenseArray, cr::Type{ContRank{M}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs) = 
    strided_view(parent(a), aoffset(a, i1, i2, i3), shp, cr, vstrides(a, i1, i2, i3))

make_view{M,N}(a::DenseArray, cr::Type{ContRank{M}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    strided_view(parent(a), aoffset(a, i1, i2, i3, i4), shp, cr, vstrides(a, i1, i2, i3, i4))

make_view{M,N}(a::DenseArray, cr::Type{ContRank{M}}, shp::NTuple{N,Int}, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    strided_view(parent(a), aoffset(a, i1, i2, i3, i4, i5, I...), shp, cr, vstrides(a, i1, i2, i3, i4, i5, I...))

view(a::Array) = contiguous_view(a, size(a))
view(a::ArrayView) = a

view(a::DenseArray, i::Subs) = 
    (shp = vshape(a, i); make_view(a, restrict_crank(acontrank(a, i), shp), shp, i))

view(a::DenseArray, i1::Subs, i2::Subs) = 
    (shp = vshape(a, i1, i2); make_view(a, restrict_crank(acontrank(a, i1, i2), shp), shp, i1, i2))

view(a::DenseArray, i1::Subs, i2::Subs, i3::Subs) = 
    (shp = vshape(a, i1, i2, i3); make_view(a, restrict_crank(acontrank(a, i1, i2, i3), shp), shp, i1, i2, i3))

view(a::DenseArray, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    (shp = vshape(a, i1, i2, i3, i4); make_view(a, restrict_crank(acontrank(a, i1, i2, i3, i4), shp), shp, i1, i2, i3, i4))

view(a::DenseArray, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    (shp = vshape(a, i1, i2, i3, i4, i5, I...); 
     make_view(a, restrict_crank(acontrank(a, i1, i2, i3, i4, i5, I...), shp), shp, i1, i2, i3, i4, i5, I...))

## reshape_view

function reshape_view{N}(a::ContiguousArray, shp::NTuple{N,Int})
    prod(shp) == length(a) || throw(DimensionMismatch("Inconsistent array size."))
    contiguous_view(parent(a), offset(a), shp)
end


## ellipview

ellipview{T}(a::DenseArray{T,2}, i::Integer) = view(a, :, i)
ellipview{T}(a::DenseArray{T,3}, i::Integer) = view(a, :, :, i)
ellipview{T}(a::DenseArray{T,4}, i::Integer) = view(a, :, :, :, i)
ellipview{T}(a::DenseArray{T,5}, i::Integer) = view(a, :, :, :, :, i)
ellipview{T,N}(a::DenseArray{T,N}, i::Integer) = view(a, ntuple(N-1, i->Colon())..., i)


##### Arithmetics on contiguous ranks #####

for m=0:4, n=0:4
    global addrank
    @eval addrank(::Type{ContRank{$m}}, ::Type{ContRank{$n}}) = ContRank{$(m+n)}
end

addrank{M,N}(::Type{ContRank{M}}, ::Type{ContRank{N}}) = ContRank{M+N}
addrank{N}(::Type{ContRank{0}}, ::Type{ContRank{N}}) = ContRank{N}
addrank{N}(::Type{ContRank{N}}, ::Type{ContRank{0}}) = ContRank{N}

for m=0:4, n=0:4
    global minrank
    @eval minrank(::Type{ContRank{$m}}, ::Type{ContRank{$n}}) = ContRank{$(min(m,n))}
end

minrank{M,N}(::Type{ContRank{M}}, ::Type{ContRank{N}}) = ContRank{min(M,N)}
minrank{N}(::Type{ContRank{0}}, ::Type{ContRank{N}}) = ContRank{0}
minrank{N}(::Type{ContRank{N}}, ::Type{ContRank{0}}) = ContRank{0}

for m=0:4, n=0:4
    global restrict_crank
    @eval restrict_crank(::Type{ContRank{$m}}, ::NTuple{$n,Int}) = ContRank{$(min(m,n))}
end

restrict_crank{M,N}(::Type{ContRank{M}}, ::NTuple{N,Int}) = ContRank{min(M,N)}
restrict_crank{N}(::Type{ContRank{0}}, ::NTuple{N,Int}) = ContRank{0}
restrict_crank{N}(::Type{ContRank{N}}, ::()) = ContRank{0}

### contiguous rank computation based on indices

# 0D
contrank() = ContRank{0}

# 1D
contrank(i::Union(Colon,UnitRange)) = ContRank{1}
contrank(i::Subs) = ContRank{0}

# 2D
contrank(i1::Colon, i2::Union(Colon,UnitRange)) = ContRank{2}
contrank(i1::Colon, i2::Subs) = ContRank{1}
contrank(i1::UnitRange, i2::Subs) = ContRank{1}
contrank(i1::Subs, i2::Subs) = ContRank{0}

# 3D
contrank(i1::Colon, i2::Colon, i3::Union(Colon,UnitRange)) = ContRank{3}
contrank(i1::Colon, i2::Colon, i3::Subs) = ContRank{2}
contrank(i1::Colon, i2::UnitRange, i3::Subs) = ContRank{2}
contrank(i1::Colon, i2::Subs, i3::Subs) = ContRank{1}
contrank(i1::UnitRange, i2::Subs, i3::Subs) = ContRank{1}
contrank(i1::Subs, i2::Subs, i3::Subs) = ContRank{0}

# ND
contrank(i1::Colon, i2::Colon, i3::Colon, I::Subs...) = addrank(ContRank{3}, contrank(I...))
contrank(i1::Colon, i2::Colon, i3::UnitRange, I::Subs...) = ContRank{3}
contrank(i1::Colon, i2::Colon, i3::Subs, I::Subs...) = ContRank{2}
contrank(i1::Colon, i2::UnitRange, i3::Subs, I::Subs...) = ContRank{2}
contrank(i1::Colon, i2::Subs, i3::Subs, I::Subs...) = ContRank{1}
contrank(i1::UnitRange, i2::Subs, i3::Subs, I::Subs...) = ContRank{1}
contrank(i1::Subs, i2::Subs, i3::Subs, I::Subs...) = ContRank{0}

# contiguous rank with array & arrayviews

acontrank(a::Array, i1::Subs) = contrank(i1)
acontrank(a::Array, i1::Subs, i2::Subs) = contrank(i1, i2)
acontrank(a::Array, i1::Subs, i2::Subs, i3::Subs) = contrank(i1, i2, i3)
acontrank(a::Array, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = contrank(i1, i2, i3, i4)
acontrank(a::Array, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    contrank(i1, i2, i3, i4, i5, I...)

acontrank{T,N}(a::ArrayView{T,N,N}, i1::Subs) = contrank(i1)
acontrank{T,N}(a::ArrayView{T,N,N}, i1::Subs, i2::Subs) = contrank(i1, i2)
acontrank{T,N}(a::ArrayView{T,N,N}, i1::Subs, i2::Subs, i3::Subs) = contrank(i1, i2, i3)
acontrank{T,N}(a::ArrayView{T,N,N}, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    contrank(i1, i2, i3, i4)
acontrank{T,N}(a::ArrayView{T,N,N}, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    contrank(i1, i2, i3, i4, i5, I...)

acontrank{T,N,M}(a::ArrayView{T,N,M}, i1::Subs) = minrank(contrank(i1), ContRank{M})
acontrank{T,N,M}(a::ArrayView{T,N,M}, i1::Subs, i2::Subs) = 
    minrank(contrank(i1, i2), ContRank{M})
acontrank{T,N,M}(a::ArrayView{T,N,M}, i1::Subs, i2::Subs, i3::Subs) = 
    minrank(contrank(i1, i2, i3), ContRank{M})
acontrank{T,N,M}(a::ArrayView{T,N,M}, i1::Subs, i2::Subs, i3::Subs, i4::Subs) = 
    minrank(contrank(i1, i2, i3, i4), ContRank{M})
acontrank{T,N,M}(a::ArrayView{T,N,M}, i1::Subs, i2::Subs, i3::Subs, i4::Subs, i5::Subs, I::Subs...) = 
    minrank(contrank(i1, i2, i3, i4, i5, I...), ContRank{M})

# Added by Andreas Noack 24 September 2014 when including this in Base
getindex{T,N}(A::ArrayView{T,N}, I::AbstractArray{Bool,N}) = getindex_bool_1d(A, I)
#getindex(A::ArrayView, I::Subs...) = view(A, I...)

end  # module ArrayViews

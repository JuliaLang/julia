module ArrayViews

import Base: eltype, ndims, size, length, stride, strides
import Base: to_index, getindex, setindex!, parent, similar
import Base: convert, Ptr, pointer

export ArrayView, ContiguousView, StridedView, view

####  View types

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

type ContRank{M} end

# use ContiguousView when the contiguousness can be determined statically 
immutable ContiguousView{T,N,Arr<:Array{T}} <: ArrayView{T,N,N}
    arr::Arr
    offset::Int
    len::Int
    shp::NTuple{N,Int}
end

contiguous_view{T,N}(arr::Array{T}, offset::Int, shp::NTuple{N,Int}) = 
    ContiguousView{T,N,typeof(arr)}(arr, offset, *(shp...), shp)

contiguous_view{T,N}(arr::Array{T}, shp::NTuple{N,Int}) = contiguous_view(arr, 0, shp)

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
    StridedView{T,N,M,typeof(arr)}(arr, offset, *(shp...), shp, strides)
end

function strided_view{T,N,M}(arr::Array{T}, shp::NTuple{N,Int}, 
    ::Type{ContRank{M}}, strides::NTuple{N,Int})
    @assert M <= N
    StridedView{T,N,M,typeof(arr)}(arr, 0, *(shp...), shp, strides)
end


#### basic functions for array views

# generic functions

eltype{T}(a::ArrayView{T}) = T
ndims{T,N}(a::ArrayView{T,N}) = N
contiguousrank{T,N,M}(a::ArrayView{T,N,M}) = M

contrank{T,N}(a::Array{T,N}) = ContRank{N}
contrank{T,N,M}(a::ArrayView{T,N,M}) = ContRank{M}

pointer(a::ArrayView) = pointer(parent(a), offset(a)+1)
convert{T}(::Type{Ptr{T}}, a::ArrayView{T}) = pointer(a)

similar{T}(a::ArrayView{T}) = Array(T, size(a))
similar{T}(a::ArrayView{T}, dims::Dims) = Array(T, dims)
similar{T}(a::ArrayView, ::Type{T}, dims::Dims) = Array(T, dims)

getdim{N}(s::NTuple{N,Int}, d::Integer) = 
    (d > 0 || error("dimension out of range."); d <= N ? s[d] : 1)
size{T,N}(a::ArrayView{T,N}, d::Integer) = getdim(size(a), d)

# methods specific to ContiguousView

parent(a::ContiguousView) = a.arr
offset(a::ContiguousView) = a.offset
length(a::ContiguousView) = a.len
size(a::ContiguousView) = a.shp
iscontiguous(a::ContiguousView) = true

strides{T}(a::ContiguousView{T,1}) = (1,)
strides{T}(a::ContiguousView{T,2}) = (1, a.shp[1])
strides{T}(a::ContiguousView{T,3}) = (1, a.shp[1], a.shp[1] * a.shp[2])
strides{T}(a::ContiguousView{T,4}) = (s=a.shp[1]*a.shp[2]; (1, a.shp[1], s, s*a.shp[3]))

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

stride{T}(a::ContiguousView{T,1}, d::Integer) = (d > 0 || error("dimension out of range."); 
                                                 d == 1 ? 1 : length(a))

stride{T}(a::ContiguousView{T,2}, d::Integer) = (d > 0 || error("dimension out of range."); 
                                                 d == 1 ? 1 : 
                                                 d == 2 ? a.shp[1] : length(a))

stride{T,N}(a::ContiguousView{T,N}, d::Integer) = (d > 0 || error("dimension out of range."); 
                                                   d == 1 ? 1 : 
                                                   d == 2 ? a.shp[1] :
                                                   d <= N ? prod(a.shp[1:d-1]) : length(a))

# methods specific to StridedView

parent(a::StridedView) = a.arr
offset(a::StridedView) = a.offset
length(a::StridedView) = a.len
size(a::StridedView) = a.shp

iscontiguous{T,N}(a::StridedView{T,N,N}) = true;
iscontiguous{T,N}(a::StridedView{T,N}) = _iscontiguous(a.shp, a.strides)

_iscontiguous(shp::(), strides::()) = true
_iscontiguous(shp::(Int,), strides::(Int,)) = (strides[1] == 1)
_iscontiguous(shp::(Int,Int), strides::(Int,Int)) = (strides[1] == 1 && strides[2] == shp[1])
_iscontiguous(shp::(Int,Int,Int), strides::(Int,Int,Int)) = (strides[1] == 1 &&
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

strides(a::StridedView) = a.strides

stride{T,N}(a::StridedView{T,N}, d::Integer) = (1 <= d <= N || error("dimension out of range."); 
                                                a.strides[d])


## getindex

# Note: uindex should be implemented for each specific subtype of ArrayView
# which the getindex relies on to calculate the index w.r.t. the parent

getindex(a::ArrayView, i::Int) = arrayref(a.arr, uindex(a, i))
getindex(a::ArrayView, i1::Int, i2::Int) = arrayref(a.arr, uindex(a, i1, i2))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int) = arrayref(a.arr, uindex(a, i1, i2, i3))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int) = 
    arrayref(a.arr, uindex(a, i1, i2, i3, i4))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) = 
    arrayref(a.arr, uindex(a, i1, i2, i3, i4, i5))
getindex(a::ArrayView, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int, i6::Int, I::Int...) = 
    arrayref(a.arr, uindex(a, i1, i2, i3, i4, i5, i6, I...))

# methods that accept real indexes

getindex(a::ArrayView, i::Real) = getindex(a, to_index(i))
getindex(a::ArrayView, i1::Real, i2::Real) = getindex(a, to_index(i1), to_index(i2))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(i6))
getindex(a::ArrayView, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real, I::Int...) = 
    getindex(a, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(i6), I...)

## setindex!

setindex!{T}(a::ArrayView{T}, v, i::Int) = arrayset(a.arr, convert(T, v), uindex(a, i))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int) = 
    arrayset(a.arr, convert(T, v), uindex(a, i1, i2))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int) = 
    arrayset(a.arr, convert(T, v), uindex(a, i1, i2, i3))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int) = 
    arrayset(a.arr, convert(T, v), uindex(a, i1, i2, i3, i4))
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int) = 
    arrayset(a.arr, convert(T, v), uindex(a, i1, i2, i3, i4, i5))    
setindex!{T}(a::ArrayView{T}, v, i1::Int, i2::Int, i3::Int, i4::Int, i5::Int, i6::Int, I::Int...) = 
    arrayset(a.arr, convert(T, v), uindex(a, i1, i2, i3, i4, i5, i6, I...))

# methods that accept real indexes

setindex!(a::ArrayView, v, i::Real) = setindex!(a, v, to_index(i))
setindex!(a::ArrayView, v, i1::Real, i2::Real) = setindex!(a, v, to_index(i1), to_index(i2))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(i6))
setindex!(a::ArrayView, v, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, i6::Real, I::Int...) = 
    setindex!(a, v, to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(i6), I...)


#### index calculation 

## index calculation for continuous views

uindex{T,N}(a::ArrayView{T,N,N}, i::Int) = a.offset + i
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int) = a.offset + sub2ind(size(a), i1, i2)
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int, i3::Int) = 
    a.offset + sub2ind(size(a), i1, i2, i3)
uindex{T,N}(a::ArrayView{T,N,N}, i1::Int, i2::Int, i3::Int, i4::Int, I::Int...) = 
    a.offset + sub2ind(size(a), i1, i2, i3, i4, I...)

## index calculation for strided views

# 0D
uindex{T}(a::StridedView{T,0}, i::Int) = 1
# 1D
uindex{T}(a::StridedView{T,1,0}, i::Int) = a.offset + 1 + (i-1)*a.strides[1]
uindex{T}(a::StridedView{T,1,1}, i::Int) = a.offset + i
uindex{T}(a::StridedView{T,1}, i1::Int, i2::Int) = 
    (i2 == 1 || throw(BoundsError()); uindex(a, i1))
uindex{T}(a::StridedView{T,1}, i1::Int, i2::Int, i3::Int) = 
    ((i2 == i3 == 1) || throw(BoundsError()); uindex(a, i1))
# 2D
uindex{T}(a::StridedView{T,2}, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T}(a::StridedView{T,2,0}, i1::Int, i2::Int) = 
    a.offset + 1 + (i1-1)*a.strides[1] + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2,1}, i1::Int, i2::Int) = a.offset + i1 + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2,2}, i1::Int, i2::Int) = a.offset + i1 + (i2-1)*a.strides[2]
uindex{T}(a::StridedView{T,2}, i1::Int, i2::Int, i3::Int) = 
    (i3 == 1 || throw(BoundsError()); uindex(a, i1, i2))
# 3D
uindex{T}(a::StridedView{T,3}, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T}(a::StridedView{T,3}, i1::Int, i2::Int) = 
    uindex(a, i1, ind2sub((a.shp[2], a.shp[3]), i2)...)
uindex{T}(a::StridedView{T,3}, i1::Int, i2::Int, i3::Int) = 
    a.offset + i1 + (i2-1)*a.strides[2] + (i3-1)*a.strides[3]
uindex{T}(a::StridedView{T,3,0}, i1::Int, i2::Int, i3::Int) = 
    a.offset + 1 + (i1-1)*a.strides[1] + (i2-1)*a.strides[2] + (i3-1)*a.strides[3]

# multi-dimensional
uindex(a::StridedView, i::Int) = uindex(a, ind2sub(size(a), i)...)
uindex{T,N}(a::StridedView{T,N}, i1::Int, i2::Int) = uindex(a, i1, ind2sub(a.shp[2:N], i2)...)
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

end  # module ArrayViews

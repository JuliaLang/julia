using  Base.MultiplicativeInverses: SignedMultiplicativeInverse

immutable ReshapedArray{T,N,P<:AbstractArray,MI<:Tuple{Vararg{SignedMultiplicativeInverse{Int}}}} <: AbstractArray{T,N}
    parent::P
    dims::NTuple{N,Int}
    mi::MI
end
ReshapedArray{T,N}(parent::AbstractArray{T}, dims::NTuple{N,Int}, mi) = ReshapedArray{T,N,typeof(parent),typeof(mi)}(parent, dims, mi)

# Fast iteration on ReshapedArrays: use the parent iterator
immutable ReshapedRange{I,M}
    iter::I
    mi::NTuple{M,SignedMultiplicativeInverse{Int}}
end
ReshapedRange(A::ReshapedArray) = reshapedrange(A.parent, A.mi)
function reshapedrange{M}(P, mi::NTuple{M})
    iter = eachindex(P)
    ReshapedRange{typeof(iter),M}(iter, mi)
end

immutable ReshapedIndex{T}
    parentindex::T
end

eachindex(A::ReshapedArray) = ReshapedRange(A)
start(R::ReshapedRange) = start(R.iter)
 done(R::ReshapedRange, i) = done(R.iter, i)
function next(R::ReshapedRange, i)
    @_inline_meta
    item, inext = next(R.iter, i)
    ReshapedIndex(item), inext
end

function reshape(parent::AbstractArray, dims::Dims)
    prod(dims) == length(parent) || throw(DimensionMismatch("parent has $(length(parent)) elements, which is incompatible with size $dims"))
    _reshape((parent, linearindexing(parent)), dims)
end
reshape(R::ReshapedArray, dims::Dims) = reshape(R.parent, dims)
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

function _reshape(p::Tuple{AbstractArray,LinearSlow}, dims::Dims)
    parent = p[1]
    strds = strides(parent)
    mi = map(SignedMultiplicativeInverse, tail(strds))
    ReshapedArray(parent, dims, reverse(mi))
end

function _reshape(p::Tuple{AbstractArray,LinearFast}, dims::Dims)
    parent = p[1]
    ReshapedArray(parent, dims, ())
end

size(A::ReshapedArray) = A.dims
size(A::ReshapedArray, d) = d <= ndims(A) ? A.dims[d] : 1
similar(A::ReshapedArray, eltype::Type, dims...) = similar(A.parent, eltype, dims...)
linearindexing{T,N,P<:AbstractArray}(::Type{ReshapedArray{T,N,P,Tuple{}}}) = LinearFast()

ind2sub_rs(::Tuple{}, i::Int) = i
ind2sub_rs(strds, i) = (@_inline_meta; ind2sub_rs((), strds, i-1))
ind2sub_rs(out, ::Tuple{}, ind) = (@_inline_meta; (ind+1, out...))
function ind2sub_rs(out, strds, ind)
    @_inline_meta
    d, r = divrem(ind, strds[1])
    ind2sub_rs((d+1, out...), tail(strds), r)
end

getindex(A::ReshapedArray, index::Int) = A.parent[ind2sub_rs(A.mi, index)]
getindex(A::ReshapedArray, indexes::Int...) = (@_inline_meta; A.parent[ind2sub_rs(A.mi, sub2ind(size(A), indexes...))...])
getindex(A::ReshapedArray, index::ReshapedIndex) = A.parent[index.parentindex]

setindex!(A::ReshapedArray, val, index::Int) = (@_inline_meta; A.parent[ind2sub_rs(A.mi, index)...] = val)
setindex!(A::ReshapedArray, val, indexes::Int...) = (@_inline_meta; A.parent[ind2sub_rs(A.mi, sub2ind(size(A), indexes...))...] = val)
setindex!(A::ReshapedArray, val, index::ReshapedIndex) = A.parent[index.parentindex] = val

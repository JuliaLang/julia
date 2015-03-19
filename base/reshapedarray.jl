module Reshaped

import Base: getindex, ind2sub, linearindexing, reshape, similar, size
# just using, not overloading:
import Base: LinearFast, LinearSlow, FastDivInteger, FastDivInteger1, tail

# Remapping a block of In dimensions of the parent array to Out dimensions of the view
immutable IndexMD{In,Out,MI<:(FastDivInteger...)}
    invstride_parent::MI
    dims_view::NTuple{Out,Int}
end
IndexMD(mi, dims) = IndexMD{length(mi),length(dims),typeof(mi)}(mi, dims)

typealias ReshapeIndex Union(Colon, IndexMD)

immutable ReshapedArray{T,N,P<:AbstractArray,I<:(ReshapeIndex...)} <: AbstractArray{T,N}
    parent::P
    indexes::I
    dims::NTuple{N,Int}
end

function ReshapedArray(parent::AbstractArray, indexes::(ReshapeIndex...), dims)
    ReshapedArray{eltype(parent),length(dims),typeof(parent),typeof(indexes)}(parent, indexes, dims)
end

reshape(parent::AbstractArray, dims::Dims) = reshape((parent, linearindexing(parent)), dims)

function reshape(p::(AbstractArray,LinearSlow), dims::Dims)
    parent = p[1]
    # Split on dimensions where the strides line up
    stridep = dimstrides(size(parent))
    stridev = dimstrides(dims)
    stridep[end] == stridev[end] || throw(DimensionMismatch("Must have the same number of elements"))
    indexes = Any[]
    iplast = ivlast = 1
    ip = iv = 2
    while ip <= length(stridep) || iv <= length(stridev)
        if stridep[ip] == stridev[iv]
            if ip-iplast == iv-ivlast == 1
                push!(indexes, Colon())
            else
                mi = map(multiplicativeinverse, size(parent)[iplast:ip-1])
                imd = IndexMD(mi, dims[ivlast:iv-1])
                push!(indexes, imd)
            end
            iplast = ip
            ivlast = iv
            ip += 1
            iv += 1
        elseif stridep[ip] < stridev[iv]
            ip += 1
        else
            iv += 1
        end
    end
    ReshapedArray(parent, tuple(indexes...), dims)
end

function reshape(p::(AbstractArray,LinearFast), dims::Dims)
    parent = p[1]
    prod(dims) == length(parent) || throw(DimensionMismatch("Must have the same number of elements"))
    ReshapedArray(parent, (IndexMD((), dims),), dims)
end

reshape(parent::ReshapedArray, dims::Dims) = reshape(parent.parent, dims)

size(A::ReshapedArray) = A.dims
similar{T}(A::ReshapedArray, ::Type{T}, dims::Dims) = Array(T, dims)
linearindexing(A::ReshapedArray) = linearindexing(A.parent)

size(index::IndexMD) = index.dims_view

@inline function getindex(indx::IndexMD{1}, indexes::Int...)
    sub2ind(indx.dims_view, indexes...)
end
@inline function getindex(indx::IndexMD, indexes::Int...)
    ind2sub(indx.invstride_parent, sub2ind(indx.dims_view, indexes...))
end

consumes{In,Out,MI}(::Type{IndexMD{In,Out,MI}}) = Out
consumes(::Type{Colon}) = 1
produces{In,Out,MI}(::Type{IndexMD{In,Out,MI}}) = In
produces(::Type{Colon}) = 1

getindex(A::ReshapedArray) = A.parent[1]
getindex(A::ReshapedArray, indx::Real) = A.parent[indx]

stagedfunction getindex{T,N,P,I}(A::ReshapedArray{T,N,P,I}, indexes::Real...)
    length(indexes) == N || throw(DimensionMismatch("Must index with all $N indexes"))
    c = map(consumes, I)
    breaks = [0;cumsum([c...])]
    argbreaks = Any[]
    for i = 1:length(c)
        ab = Expr[]
        for j = breaks[i]+1:breaks[i+1]
            push!(ab, :(indexes[$j]))
        end
        push!(argbreaks, ab)
    end
    argsin = Expr[:(getindex(A.indexes[$i], $(argbreaks[i]...))) for i = 1:length(c)]
    np = map(produces, I)
    npc = [0;cumsum([np...])]
    argsout = Expr[]
    if length(argsin) > 1
        for i = 1:npc[end]
            j = findlast(npc .< i)
            di = i - npc[j]
            push!(argsout, :(tindex[$j][$di]))
        end
    end
    meta = Expr(:meta, :inline)
    ex = length(argsin) == 1 ?
        quote
            $meta
            getindex(A.parent, $(argsin...))
        end :
        quote
            $meta
            tindex = tuple($(argsin...))
            getindex(A.parent, $(argsout...))
        end
    ex
end

# Like strides, but operates on a Dims tuple, and returns one extra element (the total size)
dimstrides(::()) = ()
dimstrides(s::Dims) = dimstrides((1,), s)
dimstrides(t::Tuple, ::()) = t
@inline dimstrides(t::Tuple, sz::Dims) = dimstrides(tuple(t..., t[end]*sz[1]), tail(sz))

ind2sub(dims::(FastDivInteger,), ind::Integer) = ind
@inline ind2sub(dims::(FastDivInteger,FastDivInteger), ind::Integer) = begin
    dv, rm = divrem(ind-1,dims[1])
    rm+1, dv+1
end
@inline ind2sub(dims::(FastDivInteger,FastDivInteger,FastDivInteger...), ind::Integer) = begin
    dv, rm = divrem(ind-1,dims[1])
    tuple(rm+1, ind2sub(tail(dims),dv+1)...)
end

end

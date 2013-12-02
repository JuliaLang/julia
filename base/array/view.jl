importall Base

isdefined(:ArrayView) || immutable ArrayView{T,n,m} <: AbstractArray
    array::Array{T,m}
    size::NTuple{n,Int}
    strides::NTuple{n,Int}
    origin::Int
end

size(v::ArrayView) = v.size
size(v::ArrayView, i::Int) = v.size[i]
ndims{T,n}(v::ArrayView{T,n}) = n
eltype(v::ArrayView) = eltype(v.array)
show(io::IO, v::ArrayView) = invoke(show,(IO,Any),io,v)

function v2a{n}(strides::NTuple{n,Int}, o::Int, I::NTuple{n,Int})
    @inbounds for k = 1:n
        o += (I[k]-1)*strides[k]
    end
    return o
end

function v2a{n}(size::NTuple{n,Int}, strides::NTuple{n,Int}, o::Int, i::Int)
    i -= 1
    @inbounds for k = 1:n
        o += rem(i,size[k])*strides[k]
        i =  div(i,size[k])
    end
    return o
end

v2a{T,n}(v::ArrayView{T,n}, I::NTuple{n,Int}) = v2a(v.strides, v.origin, I)
v2a{T,n}(v::ArrayView{T,n}, i::Int) = v2a(v.size, v.strides, v.origin, i)

getindex(v::ArrayView, i::Int) = @inbounds return v.array[v2a(v,i)]
getindex(v::ArrayView, I::Int...) = @inbounds return v.array[v2a(v,I)]
setindex!(v::ArrayView, x, i::Int) = (@inbounds v.array[v2a(v,i)] = x)
setindex!(v::ArrayView, x, I::Int...) = (@inbounds v.array[v2a(v,I)] = x)

function ArrayView{T,n,m}(a::Array{T,m}, R::NTuple{n,Ranges})
    sz = ntuple(n,k->length(R[k]))
    origin = prod = 1
    strides = ntuple(n) do k
        origin += prod*(first(R[k])-1)
        stride = prod*step(R[k])
        prod *= size(a,k)
        return stride
    end
    ArrayView(a,sz,strides,origin)
end

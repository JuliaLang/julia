importall Base

isdefined(:ArrayView) || immutable ArrayView{T,n} <: AbstractArray
    data::Vector{T}
    offset::Int
    strides::NTuple{n,Int}
    size::NTuple{n,Int}
end

size(v::ArrayView) = v.size
size(v::ArrayView, i::Int) = v.size[i]
ndims{T,n}(v::ArrayView{T,n}) = n
show(io::IO, v::ArrayView) = invoke(show,(IO,Any),io,v)

function view2data{T,n}(v::ArrayView{T,n}, I::NTuple{n,Int})
    d = v.offset
    t = v.strides
    # z = v.size
    @inbounds for k = 1:n
        j = I[k]
        # 1 <= j <= z[k] || throw(BoundsError())
        d += t[k]*(j-1)
    end
    return d+1
end

function view2data{T,n}(v::ArrayView{T,n}, i::Int)
    i -= 1
    d = v.offset
    z = v.size
    t = v.strides
    @inbounds for k = 1:n
        d += t[k]*rem(i,z[k])
        i = div(i,z[k])
    end
    return d+1
end

getindex(v::ArrayView, i::Int) = @inbounds return v.data[view2data(v,i)]
getindex(v::ArrayView, I::Int...) = @inbounds return v.data[view2data(v,I)]
setindex!(v::ArrayView, x, i::Int) = (@inbounds v.data[view2data(v,i)] = x)
setindex!(v::ArrayView, x, I::Int...) = (@inbounds v.data[view2data(v,I)] = x)

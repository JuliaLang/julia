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

function view2data{n}(d::Int, strides::NTuple{n,Int}, I::NTuple{n,Int})
    @inbounds for k = 1:n
        d += (I[k]-1)*strides[k]
    end
    d + 1
end

function view2data{n}(d::Int, strides::NTuple{n,Int}, size::NTuple{n,Int}, i::Int)
    i -= 1
    @inbounds for k = 1:n
        d += rem(i,strides[k])*size[k]
        i =  div(i,strides[k])
    end
    d + 1
end

view2data{T,n}(v::ArrayView{T,n}, I::NTuple{n,Int}) = view2data(v.offset, v.strides, I)
view2data{T,n}(v::ArrayView{T,n}, i::Int) = view2data(v.offset, v.strides, v.size, i)

getindex(v::ArrayView, i::Int) = @inbounds return v.data[view2data(v,i)]
getindex(v::ArrayView, I::Int...) = @inbounds return v.data[view2data(v,I)]
setindex!(v::ArrayView, x, i::Int) = (@inbounds v.data[view2data(v,i)] = x)
setindex!(v::ArrayView, x, I::Int...) = (@inbounds v.data[view2data(v,I)] = x)

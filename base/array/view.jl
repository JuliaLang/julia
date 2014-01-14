importall Base

isdefined(:ArrayView) || immutable ArrayView{T,n,m} <: AbstractArray{T,n}
    array::Array{T,m}
    sizes::NTuple{n,Int}
    strides::NTuple{n,Int}
    origin::Int
    magic::NTuple{n,Int}
    coefs::NTuple{n,Int}
end

size(v::ArrayView) = v.sizes
size(v::ArrayView, i::Int) = v.sizes[i]
ndims{T,n}(v::ArrayView{T,n}) = n
eltype(v::ArrayView) = eltype(v.array)

function v2a{n}(strides::NTuple{n,Int}, o::Int, I::NTuple{n,Int})
    @inbounds for k = 1:n
        o += (I[k]-1)*strides[k]
    end
    return o
end

function v2a{n}(magic::NTuple{n,Int}, coefs::NTuple{n,Int}, o::Int, i::Int)
    i -= 1
    o += i*coefs[1]
    @inbounds for k = 2:n
        o += (magic[k]*i>>>32)*coefs[k]
    end
    return o
end

v2a{T,n}(v::ArrayView{T,n}, I::NTuple{n,Int}) = v2a(v.strides, v.origin, I)
v2a{T,n}(v::ArrayView{T,n}, i::Int) = v2a(v.magic, v.coefs, v.origin, i)

getindex(v::ArrayView, i::Int) = @inbounds return v.array[v2a(v,i)]
getindex(v::ArrayView, I::Int...) = @inbounds return v.array[v2a(v,I)]
setindex!(v::ArrayView, x, i::Int) = (@inbounds v.array[v2a(v,i)] = x)
setindex!(v::ArrayView, x, I::Int...) = (@inbounds v.array[v2a(v,I)] = x)

function ArrayView{n}(a::Array, R::NTuple{n,Ranges})
    prod = origin = 1
    strides = ntuple(n) do k
        origin += prod*(first(R[k])-1)
        stride = prod*step(R[k])
        prod *= size(a,k)
        return stride
    end
    sizes = map(length,R)
    prod = 1
    magic = ntuple(n) do k
        m = div(1<<32+prod-1,prod)
        prod *= sizes[k]
        return m
    end
    coefs = ntuple(n) do k
        k == 1 ? strides[k] : strides[k]-sizes[k-1]*strides[k-1]
    end
    ArrayView(a,sizes,strides,origin,magic,coefs)
end

function repeated_linear_indexing(a,cap=0.01)
    n = 0
    time = 0.0
    total = zero(eltype(a))
    while time < cap
        time += @elapsed for i = 1:length(a)
            total += a[i]
        end
        n += 1
    end
    total, time/n
end

function benchmark(z=100000,n=10,m=10)
    results = Array(Float64,n,m,3)
    for d = 1:n, x = 1:m
        S = [ iround(3z^(1/d)+1) for _=1:d ]
        A = rand(S...)
        R = map(s->2:3:s-1,size(A))
        a = A[R...]
        v = ArrayView(A,R)
        s = sub(A,R)
        @show S, size(a), length(a)
        results[d,x,1] = repeated_linear_indexing(a)[2]
        results[d,x,2] = repeated_linear_indexing(v)[2]
        results[d,x,3] = repeated_linear_indexing(s)[2]
    end
    return results
end

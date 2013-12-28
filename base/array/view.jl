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

function repeated_linear_indexing(a,n=1000)
    t = zero(eltype(a))
    for _ = 1:n, i = 1:length(a)
        t += a[i]
    end
    return t
end

using Distributions

function rand_slice(z)
    a = max(1,rand(Binomial(z,1/3)))
    b = max(1,rand(Binomial(z,2/3)))
    s = max(1,rand(NegativeBinomial(1,1/2)))
    r = a:copysign(s,sign(b-a)):b
    randbool() ? r : z-r+1
end

function benchmark(z=1000000,n=10,m=10)
    results = Array(Float64,n,m,3)
    for d = 1:n, _ = 1:m
        S = max(1,rand(Binomial((2z)^(1/d),0.5),d))
        A = rand(S...)
        R = map(rand_slice,size(A))
        # @show size(A), R
        a = A[R...]
        v = ArrayView(A,R)
        s = sub(A,R)
        @assert a == v
        @assert a == s
        @assert v == s
        ta = @elapsed repeated_linear_indexing(a)
        tv = @elapsed repeated_linear_indexing(v)
        ts = @elapsed repeated_linear_indexing(s)
        results[d,_,:] = [ta,tv,ts]
    end
    return results
end

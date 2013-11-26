type DArray{T,N,A} <: AbstractArray{T,N}
    dims::NTuple{N,Int}
    ad::ArrayDist{N}
end


typealias SubDArray{T,N,D<:DArray} SubArray{T,N,D}
typealias SubOrDArray{T,N}         Union(DArray{T,N}, SubDArray{T,N})

## core constructors ##

function DArray(alloc_arg::Union(Type, Function), dims, dprocs, dist; init=false)
    N = length(dims)
    
    ad = 
    if isa(alloc_arg, Type)
        alloc_chunk = (idxs) -> Array(alloc_arg, map(length, idxs))
        ArrayDist(alloc_chunk, dims, dprocs, dist)
    else
        ArrayDist(alloc_arg, dims, dprocs, dist)
    end
    
    p = max(1, localpartindex(ad))
    A = remotecall_fetch(procs(ad)[p], r->typeof(fetch(r)), chunk_ref(ad, p))
    T = eltype(A)
    d = DArray{T,N,A}(dims, ad)

    # if present, init function is called on each of the parts
    @sync begin 
        if isa(init, Function)
            for i in procs(d)
                @async remotecall_wait(i, init, d)
            end
        end
    end
    d
end

function DArray(alloc_arg, dims, procs; kwargs...)
    if isempty(procs)
        error("no processors")
    end
    DArray(alloc_arg, dims, procs, defaultdist(dims,procs); kwargs...)
end
DArray(alloc_arg, dims; kwargs...) = DArray(alloc_arg, dims, workers()[1:min(nworkers(),maximum(dims))]; kwargs...)

# new DArray similar to an existing one
DArray{T}(d::DArray{T}; kwargs...) = DArray(T, size(d), procs(d), [dimdist(d.ad)...]; kwargs...)

length(d::DArray) = prod(d.dims)
size(d::DArray) = d.dims
procs(d::DArray) = procs(d.ad)

chunktype{T,N,A}(d::DArray{T,N,A}) = A

localpartindex(d::DArray) = localpartindex(d.ad)

function localpart{T,N,A}(d::DArray{T,N,A})
    lpidx = localpartindex(d)
    if lpidx == 0
        convert(A, Array(T, ntuple(N,i->0)))::A
    else
        fetch(chunk_ref(d.ad, lpidx))::A
    end
end
myindexes(d::DArray) = myindexes(d.ad)
locate(d::DArray, I::Int...) = locate(d.ad, I...)

chunk{T,N,A}(d::DArray{T,N,A}, i...) = fetch(chunk_ref(d.ad, i...))::A

## convenience constructors ##

dzeros(args...) = DArray(I->zeros(map(length,I)), args...)
dzeros(d::Int...) = dzeros(d)
dones(args...) = DArray(I->ones(map(length,I)), args...)
dones(d::Int...) = dones(d)
dfill(v, args...) = DArray(I->fill(v, map(length,I)), args...)
dfill(v, d::Int...) = dfill(v, d)
drand(args...)  = DArray(I->rand(map(length,I)), args...)
drand(d::Int...) = drand(d)
drandn(args...) = DArray(I->randn(map(length,I)), args...)
drandn(d::Int...) = drandn(d)
## conversions ##


function distribute(a::AbstractArray)
    owner = myid()
    rr = RemoteRef()
    put(rr, a)
    DArray(size(a)) do I
        remotecall_fetch(owner, ()->fetch(rr)[I...])
    end
end

convert{T,N}(::Type{Array}, d::SubOrDArray{T,N}) = convert(Array{T,N}, d)

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    @sync begin
        for i = 1:length(d.ad)
            @async a[indexes(d.ad)[i]...] = chunk(d, i)
        end
    end
    a
end

function convert{S,T,N}(::Type{Array{S,N}}, s::SubDArray{T,N})
    I = s.indexes
    d = s.parent
    if isa(I,(Range1{Int}...)) && S<:T && T<:S
        l = locate(d, map(first, I)...)
        if isequal(indexes(d.ad)[l...], I)
            # SubDArray corresponds to a chunk
            return chunk(d, l...)
        end
    end
    a = Array(S, size(s))
    a[[1:size(a,i) for i=1:N]...] = s
    a
end

function reshape{T,S<:Array}(A::DArray{T,1,S}, d::Dims)
    if prod(d) != length(A)
        error("dimensions must be consistent with array size")
    end
    DArray(d) do I
        sz = map(length,I)
        d1offs = first(I[1])
        nd = length(I)

        B = Array(T,sz)
        nr = size(B,1)
        sztail = size(B)[2:end]

        for i=1:div(length(B),nr)
            i2 = ind2sub(sztail, i)
            globalidx = [ I[j][i2[j-1]] for j=2:nd ]
            
            a = sub2ind(d, d1offs, globalidx...)
            
            B[:,i] = A[a:(a+nr-1)]
        end
        B
    end
end

## indexing ##

function getchunk(r::RemoteRef, args...)
    if r.where==myid()
        getindex(fetch(r), args...)
    else
        remotecall_fetch(r.where, getchunk, r, args...)
    end
end

getindex(d::DArray, i::Int) = getindex(d, ind2sub(size(d), i))
getindex(d::DArray, i::Int...) = getindex(d, sub2ind(size(d), i...))

function getindex{T}(d::DArray{T}, I::(Int...))
    chunk_rr, localidx = localindex_rr(d.ad, I)
    getchunk(chunk_rr, localidx...)::T
end

getindex(d::DArray) = d[1]
getindex(d::DArray, I::Union(Int,Range1{Int})...) = sub(d,I)

copy(d::SubOrDArray) = d

# local copies are obtained by convert(Array, ) or assigning from
# a SubDArray to a local Array.

function setindex!(a::Array, d::DArray, I::Range1{Int}...)
    n = length(I)
    @sync begin
        for i = 1:length(d.ad)
            K = indexes(d.ad)[i]
            @async a[[I[j][K[j]] for j=1:n]...] = chunk(d, i)
        end
    end
    a
end

function setindex!(a::Array, s::SubDArray, I::Range1{Int}...)
    n = length(I)
    d = s.parent
    J = s.indexes
    if length(J) < n
        a[I...] = convert(Array,s)
        return a
    end
    offs = [isa(J[i],Int) ? J[i]-1 : first(J[i])-1 for i=1:n]
    @sync begin
        for i = 1:length(d.ad)
            K_c = {indexes(d.ad)[i]...}
            K = [ intersect(J[j],K_c[j]) for j=1:n ]
            if !any(isempty, K)
                idxs = [ I[j][K[j]-offs[j]] for j=1:n ]
                if isequal(K, K_c)
                    # whole chunk
                    @async a[idxs...] = chunk(d, i)
                else
                    # partial chunk
                    ch = chunk_ref(d.ad, i)
                    @async a[idxs...] = remotecall_fetch(ch.where, ()->sub(fetch(ch), [K[j]-first(K_c[j])+1 for j=1:n]...))
                end
            end
        end
    end
    a
end

# to disambiguate
setindex!(a::Array{Any}, d::SubOrDArray, i::Int) = arrayset(a, d, i)

setindex!(a::Array, d::SubOrDArray, I::Union(Int,Range1{Int})...) =
    setindex!(a, d, [isa(i,Int) ? (i:i) : i for i in I ]...)

## higher-order functions ##

map(f::Callable, d::DArray) = DArray(I->map(f, localpart(d)), d)

reduce(f::Function, d::DArray) =
    mapreduce(fetch, f,
              { @spawnat p reduce(f, localpart(d)) for p in procs(d) })

type DArray{T,N,A} <: AbstractArray{T,N}
    cdist::ChunkedDist{N}

    chunks::Array{RemoteRef,N}

    function DArray(cdist, chunks)
        # check invariants
        assert(size(chunks) == dist(cdist))
        assert(length(chunks) == length(procs(cdist)))
        new(cdist, chunks)
    end
end

typealias SubDArray{T,N,D<:DArray} SubArray{T,N,D}
typealias SubOrDArray{T,N}         Union(DArray{T,N}, SubDArray{T,N})

## core constructors ##

# dist == size(chunks)
function DArray(init, cdist::ChunkedDist)
    chunks = Array(RemoteRef, dist(cdist)...)
    pids = procs(cdist)
    for i = 1:nprocs(cdist)
        chunks[i] = remotecall(pids[i], init, cdist[i])
    end
    p = max(1, localpartindex(cdist))
    A = remotecall_fetch(pids[p], r->typeof(fetch(r)), chunks[p])
    DArray{eltype(A),length(size(cdist)),A}(cdist, chunks)
end

DArray(init, dims, pids) = DArray(init, ChunkedDist(dims; pids=pids))
DArray(init, dims) = DArray(init, dims, workers()[1:min(nworkers(),maximum(dims))])

# new DArray similar to an existing one
DArray(init, d::DArray) = DArray(init, distribution(d))

size(d::DArray) = size(d.cdist)
procs(d::DArray) = procs(d.cdist)
distribution(d::DArray) = d.cdist

chunktype{T,N,A}(d::DArray{T,N,A}) = A

function localpart{T,N,A}(d::DArray{T,N,A})
    lpidx = localpartindex(d.cdist)
    if lpidx == 0
        convert(A, Array(T, ntuple(N,i->0)))::A
    else
        fetch(d.chunks[lpidx])::A
    end
end

function localindexes{T,N}(d::DArray{T,N})
    lpidx = localpartindex(d)
    if lpidx == 0
        ntuple(N, i->1:0)
    else
        d.cdist[lpidx]
    end
end

# find which piece holds index (I...)
locate(d::DArray, I::Int...) = locate(d.cdist, I...)

chunk{T,N,A}(d::DArray{T,N,A}, i...) = fetch(d.chunks[i...])::A

## convenience constructors ##

zeros(cdist::ChunkedDist) = DArray(I->zeros(map(length,I)), cdist)
zeros{T}(::Type{T}, cdist::ChunkedDist) = DArray(I->zeros(T, map(length,I)), cdist)
ones(cdist::ChunkedDist) = DArray(I->ones(map(length,I)), cdist)
ones{T}(::Type{T}, cdist::ChunkedDist) = DArray(I->ones(T, map(length,I)), cdist)
fill(v, cdist::ChunkedDist) = DArray(I->fill(v, map(length,I)), cdist)
rand(cdist::ChunkedDist)  = DArray(I->rand(map(length,I)), cdist)
randn(cdist::ChunkedDist) = DArray(I->randn(map(length,I)), cdist)


## conversions ##

function distribute(a::AbstractArray)
    owner = myid()
    rr = RemoteRef()
    put!(rr, a)
    DArray(size(a)) do I
        remotecall_fetch(owner, ()->fetch(rr)[I...])
    end
end

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    @sync begin
        for i = 1:length(d.chunks)
            @async a[d.cdist[i]...] = chunk(d, i)
        end
    end
    a
end

function convert{S,T,N}(::Type{Array{S,N}}, s::SubDArray{T,N})
    I = s.indexes
    d = s.parent
    if isa(I,(Range1{Int}...)) && S<:T && T<:S
        l = locate(d, map(first, I)...)
        if isequal(d.cdist[l...], I)
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

function getindex(r::RemoteRef, args...)
    if r.where==myid()
        getindex(fetch(r), args...)
    else
        remotecall_fetch(r.where, getindex, r, args...)
    end
end

getindex(d::DArray, i::Int) = getindex(d, ind2sub(size(d), i))
getindex(d::DArray, i::Int...) = getindex(d, sub2ind(size(d), i...))

function getindex{T,N}(d::DArray{T,N}, I::(Int...))
    chidx = locate(d, I...)
    chunk = d.chunks[chidx...]
    idxs = d.cdist[chidx...]
    localidx = ntuple(N, i->(I[i]-first(idxs[i])+1))
    chunk[localidx...]::T
end

getindex(d::DArray) = d[1]
getindex(d::DArray, I::Union(Int,Range1{Int})...) = sub(d,I)

copy(d::SubOrDArray) = d

# local copies are obtained by convert(Array, ) or assigning from
# a SubDArray to a local Array.

function setindex!(a::Array, d::DArray, I::Range1{Int}...)
    n = length(I)
    @sync begin
        for i = 1:length(d.chunks)
            K = d.cdist[i]
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
        for i = 1:length(d.chunks)
            K_c = {d.cdist[i]...}
            K = [ intersect(J[j],K_c[j]) for j=1:n ]
            if !any(isempty, K)
                idxs = [ I[j][K[j]-offs[j]] for j=1:n ]
                if isequal(K, K_c)
                    # whole chunk
                    @async a[idxs...] = chunk(d, i)
                else
                    # partial chunk
                    ch = d.chunks[i]
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

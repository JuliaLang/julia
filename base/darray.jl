type DArray{T,N,A} <: AbstractArray{T,N}
    dims::NTuple{N,Int}

    chunks::Array{RemoteRef,N}

    # pmap[i]==p ⇒ processor p has piece i
    pmap::Vector{Int}

    # indexes held by piece i
    indexes::Array{NTuple{N,UnitRange{Int}},N}
    # cuts[d][i] = first index of chunk i in dimension d
    cuts::Vector{Vector{Int}}

    function DArray(dims, chunks, pmap, indexes, cuts)
        # check invariants
        assert(size(chunks) == size(indexes))
        assert(length(chunks) == length(pmap))
        assert(dims == map(last,last(indexes)))
        new(dims, chunks, pmap, indexes, cuts)
    end
end

typealias SubDArray{T,N,D<:DArray} SubArray{T,N,D}
typealias SubOrDArray{T,N}         Union(DArray{T,N}, SubDArray{T,N})

## core constructors ##

# dist == size(chunks)
function DArray(init, dims, procs, dist)
    np = prod(dist)
    procs = procs[1:np]
    idxs, cuts = chunk_idxs([dims...], dist)
    chunks = Array(RemoteRef, dist...)
    for i = 1:np
        chunks[i] = remotecall(procs[i], init, idxs[i])
    end
    p = max(1, localpartindex(procs))
    A = remotecall_fetch(procs[p], r->typeof(fetch(r)), chunks[p])
    DArray{eltype(A),length(dims),A}(dims, chunks, procs, idxs, cuts)
end

function DArray(init, dims, procs)
    if isempty(procs)
        error("no processors")
    end
    DArray(init, dims, procs, defaultdist(dims,procs))
end
DArray(init, dims) = DArray(init, dims, workers()[1:min(nworkers(),maximum(dims))])

# new DArray similar to an existing one
DArray(init, d::DArray) = DArray(init, size(d), procs(d), [size(d.chunks)...])

similar(d::DArray, T, dims::Dims)= DArray(I->Array(T, map(length,I)), dims, procs(d))
similar(d::DArray, T)= similar(d, T, size(d))
similar{T}(d::DArray{T}, dims::Dims)= similar(d, T, dims)
similar{T}(d::DArray{T})= similar(d, T, size(d))

size(d::DArray) = d.dims
procs(d::DArray) = d.pmap

chunktype{T,N,A}(d::DArray{T,N,A}) = A

## chunk index utilities ##

# decide how to divide each dimension
# returns size of chunks array
function defaultdist(dims, procs)
    dims = [dims...]
    chunks = ones(Int, length(dims))
    np = length(procs)
    f = sort!(collect(keys(factor(np))), rev=true)
    k = 1
    while np > 1
        # repeatedly allocate largest factor to largest dim
        if np%f[k] != 0
            k += 1
            if k > length(f)
                break
            end
        end
        fac = f[k]
        (d, dno) = findmax(dims)
        # resolve ties to highest dim
        dno = last(find(dims .== d))
        if dims[dno] >= fac
            dims[dno] = div(dims[dno], fac)
            chunks[dno] *= fac
        end
        np = div(np,fac)
    end
    chunks
end

# get array of start indexes for dividing sz into nc chunks
function defaultdist(sz::Int, nc::Int)
    if sz >= nc
        round(Int,linspace(1, sz+1, nc+1))
    else
        [[1:(sz+1)], zeros(Int, nc-sz)]
    end
end

# compute indexes array for dividing dims into chunks
function chunk_idxs(dims, chunks)
    cuts = map(defaultdist, dims, chunks)
    n = length(dims)
    idxs = Array(NTuple{n,UnitRange{Int}},chunks...)
    cartesianmap(tuple(chunks...)) do cidx...
        idxs[cidx...] = ntuple(n, i->(cuts[i][cidx[i]]:cuts[i][cidx[i]+1]-1))
    end
    idxs, cuts
end

function localpartindex(pmap::Vector{Int})
    mi = myid()
    for i = 1:length(pmap)
        if pmap[i] == mi
            return i
        end
    end
    return 0
end

localpartindex(d::DArray) = localpartindex(d.pmap)

function localpart{T,N,A}(d::DArray{T,N,A})
    lpidx = localpartindex(d)
    if lpidx == 0
        convert(A, Array(T, ntuple(N,i->0)))::A
    else
        fetch(d.chunks[lpidx])::A
    end
end
function localindexes(d::DArray)
    lpidx = localpartindex(d)
    if lpidx == 0
        ntuple(ndims(d), i->1:0)
    else
        d.indexes[lpidx]
    end
end

# find which piece holds index (I...)
function locate(d::DArray, I::Int...)
    ntuple(ndims(d), i->searchsortedlast(d.cuts[i], I[i]))
end

chunk{T,N,A}(d::DArray{T,N,A}, i...) = fetch(d.chunks[i...])::A

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
    put!(rr, a)
    DArray(size(a)) do I
        remotecall_fetch(owner, ()->fetch(rr)[I...])
    end
end

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    @sync begin
        for i = 1:length(d.chunks)
            @async a[d.indexes[i]...] = chunk(d, i)
        end
    end
    a
end

function convert{S,T,N}(::Type{Array{S,N}}, s::SubDArray{T,N})
    I = s.indexes
    d = s.parent
    if isa(I,(UnitRange{Int}...)) && S<:T && T<:S
        l = locate(d, map(first, I)...)
        if isequal(d.indexes[l...], I)
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

getindex(d::DArray, i::Int) = getindex_tuple(d, ind2sub(size(d), i))
getindex(d::DArray, i::Int...) = getindex_tuple(d, i)

function getindex_tuple{T}(d::DArray{T}, I::(Int...))
    chidx = locate(d, I...)
    chunk = d.chunks[chidx...]
    idxs = d.indexes[chidx...]
    localidx = ntuple(ndims(d), i->(I[i]-first(idxs[i])+1))
    chunk[localidx...]::T
end

getindex(d::DArray) = d[1]
getindex(d::DArray, I::Union(Int,UnitRange{Int})...) = sub(d,I...)

copy(d::SubOrDArray) = d

# local copies are obtained by convert(Array, ) or assigning from
# a SubDArray to a local Array.

function setindex!(a::Array, d::DArray, I::UnitRange{Int}...)
    n = length(I)
    @sync begin
        for i = 1:length(d.chunks)
            K = d.indexes[i]
            @async a[[I[j][K[j]] for j=1:n]...] = chunk(d, i)
        end
    end
    a
end

function setindex!(a::Array, s::SubDArray, I::UnitRange{Int}...)
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
            K_c = Any[d.indexes[i]...]
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

setindex!(a::Array, d::SubOrDArray, I::Union(Int,UnitRange{Int})...) =
    setindex!(a, d, [isa(i,Int) ? (i:i) : i for i in I ]...)

## higher-order functions ##

map(f::Callable, d::DArray) = DArray(I->map(f, localpart(d)), d)

reduce(f::Function, d::DArray) =
    mapreduce(fetch, f,
              Any[ @spawnat p reduce(f, localpart(d)) for p in procs(d) ])


function map!(f::Callable, d::DArray)
    @sync begin
        for p in procs(d)
            @spawnat p map!(f, localpart(d))
        end
    end
end


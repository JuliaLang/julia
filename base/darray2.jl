type DArray{T,N,A} <: AbstractArray{T,N}
    dims::NTuple{N,Int}

    chunks::Array{RemoteRef,N}

    # pmap[i]==p ⇒ processor p has piece i
    pmap::Vector{Int}

    # indexes held by piece i
    indexes::Array{NTuple{N,Range1{Int}},N}
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

## core constructors ##

# dist == size(chunks)
function DArray(init, dims, procs, dist)
    np = prod(dist)
    procs = procs[1:np]
    idxs, cuts = chunk_idxs([dims...], dist)
    chunks = Array(RemoteRef, dist...)
    for i = 1:np
        chunks[i] = remote_call(procs[i], init, idxs[i])
    end
    p = max(1, localpiece(procs))
    A = remote_call_fetch(procs[p], r->typeof(fetch(r)), chunks[p])
    DArray{eltype(A),length(dims),A}(dims, chunks, procs, idxs, cuts)
end

DArray(init, dims, procs) = DArray(init, dims, procs, defaultdist(dims,procs))
DArray(init, dims) = DArray(init, dims, [1:min(nprocs(),max(dims))])

size(d::DArray) = d.dims
procs(d::DArray) = d.pmap

## chunk index utilities ##

# decide how to divide each dimension
# returns size of chunks array
function defaultdist(dims, procs)
    dims = [dims...]
    chunks = ones(Int, length(dims))
    np = length(procs)
    f = sortr(keys(factor(np)))
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
        linspace(1, sz+1, nc+1)
    else
        [[1:(sz+1)], zeros(Int, nc-sz)]
    end
end

# compute indexes array for dividing dims into chunks
function chunk_idxs(dims, chunks)
    cuts = map(defaultdist, dims, chunks)
    n = length(dims)
    idxs = Array(NTuple{n,Range1{Int}},chunks...)
    cartesian_map(tuple(chunks...)) do cidx...
        idxs[cidx...] = ntuple(n, i->(cuts[i][cidx[i]]:cuts[i][cidx[i]+1]-1))
    end
    idxs, cuts
end

function localpiece(pmap::Vector{Int})
    mi = myid()
    for i = 1:length(pmap)
        if pmap[i] == mi
            return i
        end
    end
    return 0
end

localpiece(d::DArray) = localpiece(d.pmap)

localize{T,N,A}(d::DArray{T,N,A}) = fetch(d.chunks[localpiece(d)])::A
myindexes(d::DArray) = d.indexes[localpiece(d)]

# find which piece holds index (I...)
function locate(d::DArray, I::Int...)
    ntuple(ndims(d), i->search_sorted_last(d.cuts[i], I[i]))
end

chunk{T,N,A}(d::DArray{T,N,A}, i...) = fetch(d.chunks[i...])::A

## convenience constructors ##

drand(args...)  = DArray(I->rand(map(length,I)), args...)
drandn(args...) = DArray(I->randn(map(length,I)), args...)

## conversions ##

function distribute(a::Array)
    owner = myid()
    rr = RemoteRef()
    put(rr, a)
    DArray(size(a)) do I
        remote_call_fetch(owner, ()->fetch(rr)[I...])
    end
end

convert{T,N}(::Type{Array}, d::DArray{T,N}) = convert(Array{T,N}, d)

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    for i = 1:length(d.chunks)
        a[d.indexes[i]...] = chunk(d, i)
    end
    a
end

## indexing ##

function ref(r::RemoteRef, args...)
    if r.where==myid()
        ref(fetch(r), args...)
    else
        remote_call_fetch(r.where, ref, r, args...)
    end
end

ref(d::DArray, i::Int) = ref(d, ind2sub(size(d), i))
ref(d::DArray, i::Int...) = ref(d, sub2ind(size(d), i...))

function ref{T}(d::DArray{T}, I::(Int...))
    chidx = locate(d, I...)
    chunk = d.chunks[chidx...]
    idxs = d.indexes[chidx...]
    localidx = ntuple(ndims(d), i->(I[i]-first(idxs[i])+1))
    chunk[localidx...]::T
end

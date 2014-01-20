abstract ArrayDist

type SharedDist{N} <: ArrayDist
    dims::NTuple{N,Int}
    pids::Vector{Int}
end

type ChunkedDist {N} <: ArrayDist
    dims::NTuple{N,Int}
    pids::Vector{Int}
    
    # indexes held by piece i
    indexes::Array{NTuple{N,Range1{Int}},N}
    
    # cuts[d][i] = first index of chunk i in dimension d
    cuts::Vector{Vector{Int}}
    
    dist::NTuple{N,Int}
end

function SharedDist(dims; pids=workers())
    SharedDist{length(dims)}(dims, pids)
end
SharedDist(dims::Integer...; kwargs...) = SharedDist(dims; kwargs...)

function ChunkedDist(dims; pids=workers(), dist=distbylargestdim(dims, length(pids)))
    if isa(dist, Array)
        dist = ntuple(x->dist[x], length(dist))
    end

    npids = length(pids)
    nchunks = prod(dist)
    
    if nchunks > npids
        error("Total requested number of chunks is greater than the number of workers")
    elseif nchunks < npids
        pids = pids[1:nchunks]
    end
    
    idxs, cuts = chunk_idxs([dims...], dist)
    assert(dims == map(last,last(idxs)))

    ChunkedDist{length(dims)}(dims, pids, idxs, cuts, dist)
end

ChunkedDist(dims::Integer...; kwargs...) = ChunkedDist(dims; kwargs...)

## chunk index utilities ##

# decide how to divide each dimension
# returns size of chunks array
# allocates largest factor to largest dim
function distbylargestdim(dims, ngroups)
    dims = [dims...]
    chunks = ones(Int, length(dims))
    f = sort!(collect(keys(factor(ngroups))), rev=true)
    k = 1
    while ngroups > 1
        # repeatedly allocate largest factor to largest dim
        if ngroups%f[k] != 0
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
        ngroups = div(ngroups,fac)
    end
    chunks
end

# get array of start indexes for dividing sz into nc chunks
function distbylargestdim(sz::Int, nc::Int)
    if sz >= nc
        iround(linspace(1, sz+1, nc+1))
    else
        [[1:(sz+1)], zeros(Int, nc-sz)]
    end
end


# compute indexes array for dividing dims into chunks
function chunk_idxs(dims, chunks)
    cuts = map(distbylargestdim, dims, chunks)
    n = length(dims)
    idxs = Array(NTuple{n,Range1{Int}},chunks...)
    cartesianmap(tuple(chunks...)) do cidx...
        idxs[cidx...] = ntuple(n, i->(cuts[i][cidx[i]]:cuts[i][cidx[i]+1]-1))
    end
    idxs, cuts
end


size(ad::ArrayDist) = ad.dims
procs(ad::ArrayDist) = ad.pids
nprocs(ad::ArrayDist) = length(ad.pids)

getindex(cdist::ChunkedDist, i::Int) = cdist.indexes[i]
getindex(cdist::ChunkedDist, i::Int...) = cdist.indexes[i...]

dist(cdist::ChunkedDist) = cdist.dist

# find which piece holds index (I...)
function locate{N}(cdist::ChunkedDist{N}, I::Int...)
    ntuple(N, i->searchsortedlast(cdist.cuts[i], I[i]))
end

localpartindex(cdist::ChunkedDist) = findfirst(cdist.pids, myid())




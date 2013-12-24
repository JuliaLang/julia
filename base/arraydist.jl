# ArrayDist holds the chunks of an array distribution across workers
type ArrayDist{N}
    # ppmap[i]==p ⇒ processor p has piece i
    ppmap::Vector{Int}

    # indexes held by piece i
    indexes::Array{NTuple{N,Range1{Int}},N}
    
    # cuts[d][i] = first index of chunk i in dimension d
    cuts::Vector{Vector{Int}}
    
    chunks::Array{RemoteRef,N}
    
    function ArrayDist(dims, ppmap, indexes, cuts, chunks) 
        # check invariants
        assert(dims == map(last,last(indexes)))
        assert(size(chunks) == size(indexes))
        assert(length(chunks) == length(ppmap))
    
        new(ppmap, indexes, cuts, chunks)
    end
end

function ArrayDist(allocf, dims, procs, dist)
    np = prod(dist)
    if np > length(procs)
        error("Total requested number of chunks is greater than the number of workers")
    end
    ppmap = procs[1:np]
    idxs, cuts = chunk_idxs([dims...], dist)
    chunks = Array(RemoteRef, dist...)
    for i = 1:np
        chunks[i] = remotecall(procs[i], allocf, idxs[i])
    end

    ArrayDist{length(dims)}(dims, ppmap, idxs, cuts, chunks)
end

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
        iround(linspace(1, sz+1, nc+1))
    else
        [[1:(sz+1)], zeros(Int, nc-sz)]
    end
end


# compute indexes array for dividing dims into chunks
function chunk_idxs(dims, chunks)
    cuts = map(defaultdist, dims, chunks)
    n = length(dims)
    idxs = Array(NTuple{n,Range1{Int}},chunks...)
    cartesianmap(tuple(chunks...)) do cidx...
        idxs[cidx...] = ntuple(n, i->(cuts[i][cidx[i]]:cuts[i][cidx[i]+1]-1))
    end
    idxs, cuts
end

function localpartindex(dist::ArrayDist)
    ppmap = dist.ppmap
    mi = myid()
    for i = 1:length(ppmap)
        if ppmap[i] == mi
            return i
        end
    end
    return 0
end

indexes(ad::ArrayDist) = ad.indexes
procs(ad::ArrayDist) = ad.ppmap
length(ad::ArrayDist) = length(ad.chunks)
dimdist(ad::ArrayDist) = size(ad.chunks)   # number of parts in each dimension

chunk_ref(ad::ArrayDist, i::Int) = ad.chunks[i]
chunk_ref(ad::ArrayDist, i...) = ad.chunks[i...]

function myindexes{N}(ad::ArrayDist{N})
    lpidx = localpartindex(ad)
    if lpidx == 0
        ntuple(N, i->1:0)
    else
        ad.indexes[lpidx]
    end
end


# find which piece holds index (I...)
function locate{N}(ad::ArrayDist{N}, I::Int...)
    ntuple(N, i->searchsortedlast(ad.cuts[i], I[i]))
end

function localindex_rr{N}(ad::ArrayDist{N}, I::(Int...))
    chidx = locate(ad, I...)
    idxs = ad.indexes[chidx...]
    chunk_rr = ad.chunks[chidx...]
    chunk_rr, ntuple(N, i->(I[i]-first(idxs[i])+1))
end


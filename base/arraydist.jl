# ArrayDist specifies the distribution of an array 
# Specifically it should provide the following:
# dims(ad) : dimensions of the array being distributed 
# pdims(ad) : dimensions of how the workers are partitoned - Number of parts in each dimension
# ngroups(ad) : Equal to the number of workers
# length(ad) : Number of tiles/partitions
# getindex(ad, i) : array of indices at index i 
# locate(ad, I) : returns the partition index of the requested array index
# dmode(ad) : returns a const indicating the type of array using the distribution


abstract ArrayDist

# To be implemented
#type  TileDist <: ArrayDist
#end

# Different ways in which an ArrDist may be distributed.
global const DISTMODE_DISTRIBUTED = 1
global const DISTMODE_SHARED = 2

type DimDist{N} <: ArrayDist
    dims::NTuple{N,Int}
    
    # number of parts in each dimension
    pdims::Vector{Int}
    
    # indexes held by piece i
    indexes::Array{NTuple{N,Range1{Int}},N}
    
    # cuts[d][i] = first index of chunk i in dimension d
    cuts::Vector{Vector{Int}}
    
    dmode::Integer
    
    DimDist(dims, pdims, indexes, cuts, mode) = new(dims, pdims, indexes, cuts, mode)
end

function DimDist(dims, ngroups, pdims; mode=DISTMODE_DISTRIBUTED)
    if prod(pdims) > ngroups
        error("Total requested number of chunks is greater than the number of workers")
    end
    idxs, cuts = chunk_idxs([dims...], pdims)
    assert(dims == map(last,last(idxs)))
    
    DimDist{length(dims)}(dims, pdims, idxs, cuts, mode)
end

DimDist(dims, ngroups; kwargs...) = DimDist(dims, ngroups, defaultdist(dims, ngroups); kwargs...)
DimDist(dims::(Integer...); kwargs...) = DimDist(dims, nworkers(); kwargs...)
DimDist(dims::Integer...; kwargs...) = DimDist(dims; kwargs...)

## chunk index utilities ##

# decide how to divide each dimension
# returns size of chunks array
# allocates largest factor to largest dim
function defaultdist(dims, ngroups)
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


dims(dimdist::DimDist) = dimdist.dims
pdims(dimdist::DimDist) = dimdist.pdims
ngroups(dimdist::DimDist) = length(dimdist.indexes)
length(dimdist::DimDist) = length(dimdist.indexes)
dmode(dimdist::DimDist) = dimdist.dmode

getindex(dimdist::DimDist, i::Int) = dimdist.indexes[i]
getindex(dimdist::DimDist, i::Int...) = dimdist.indexes[i...]

# find which piece holds index (I...)
function locate{N}(dimdist::DimDist{N}, I::Int...)
    ntuple(N, i->searchsortedlast(dimdist.cuts[i], I[i]))
end


# Helper types and functions for distributed and shared arrays
type DistRefs{N}
    ppmap::Vector{Int}
    chunks::Array{RemoteRef,N}
    
    DistRefs(p, c) = new(p,c)
end

procs(dr::DistRefs) = dr.ppmap
length(dr::DistRefs) = length(dr.chunks)

getindex(ar::DistRefs, i::Int) = ar.chunks[i]
getindex(ar::DistRefs, i...) = ar.chunks[i...]


function setup_chunks(allocf, dprocs, arrdist)
    if ngroups(arrdist) > length(dprocs)
        error("Number of array partitions requested is more than the number of workers specified")
    end
    
    ppmap = dprocs[1:ngroups(arrdist)]
    chunks = Array(RemoteRef, pdims(arrdist)...)
    for (i, p) in enumerate(ppmap)
        chunks[i] = remotecall(p, allocf, arrdist[i])
    end

    dr = DistRefs{length(dims(arrdist))}(ppmap, chunks)
    
#    assert(size(chunks) == size(arrdist))
    assert(length(chunks) == length(ppmap))
    
    dr
end

localpartindex(dr::DistRefs) = findfirst(dr.ppmap, myid())


# additional distributed/shared array constructors
function fill(v, dimdist::DimDist; kwargs...) 
    if dmode(dimdist) ==  DISTMODE_DISTRIBUTED
        DArray(I->fill(v, map(length,I)), dimdist; kwargs...)
    else
        SharedArray(typeof(v), dimdist; init = S->fill!(localpart(S), v), kwargs...)
    end
end

# rand variant with range
function rand(TR::Union(DataType, Range1), dimdist::DimDist; kwargs...) 
    if dmode(dimdist) ==  DISTMODE_DISTRIBUTED
        DArray(I->rand(TR, map(length,I)), dimdist; kwargs...)
    else
        if isa(TR, Range1)
            SharedArray(Int, dimdist; init = S->map!((x)->rand(TR), localpart(S)), kwargs...)
        else
            SharedArray(TR, dimdist; init = S->map!((x)->rand(TR), localpart(S)), kwargs...)
        end
    end
end

rand(dimdist::DimDist; kwargs...) = rand(Float64, dimdist; kwargs...)

function randn(dimdist::DimDist; kwargs...) 
    if dmode(dimdist) ==  DISTMODE_DISTRIBUTED 
        DArray(I->randn(map(length,I)), dimdist; kwargs...)
    else
        SharedArray(Float64, dimdist; init = S-> map!((x)->randn(), localpart(S)), kwargs...)
    end
end

# ambiguity warning removal
similar{T}(a::Array{T, 1}, dimdist::DimDist) = make_distributed(a, T, dimdist)
similar{T}(a::Array{T, 2}, dimdist::DimDist) = make_distributed(a, T, dimdist)

function make_distributed(a, T, dimdist; kwargs...)
    if dmode(dimdist) ==  DISTMODE_DISTRIBUTED 
        owner = myid()
        rr = RemoteRef()
        put(rr, a)
        DArray(dimdist; kwargs...) do I
            remotecall_fetch(owner, ()->fetch(rr)[I...])
        end
    else
        sa = SharedArray(T, dimdist; kwargs...)
        if isdefined(sa, :local_shmmap)
            copy!(sa.local_shmmap, a)
        else
            remotecall_fetch(procs(sa)[1], SA -> copy!(SA.local_shmmap, a), SA)  
        end
        sa
    end
end

# generic version
similar{T}(a::AbstractArray{T}, dimdist::DimDist; kwargs...) = make_distributed(a, T, dimdist; kwargs...)



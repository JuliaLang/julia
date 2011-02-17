type DArray{T,N} <: Tensor{T,N}
    dims::NTuple{N,Size}
    locl::Union((),RemoteRef,Array{T,N})
    # the distributed array has N pieces
    # pmap[i]==p ⇒ processor p has piece i
    pmap::Vector{Int32}
    # piece i consists of indexes dist[i-1]+1 to dist[i]. dist[0]==0
    #  == prefix sum of the sizes of the pieces
    dist::Vector{Int32}
    # dimension of distribution
    distdim::Int32
    localpiece::Int32  # my piece #; pmap[localpiece]==myid()
    go::GlobalObject

    function DArray(go, T, distdim, dims, initializer)
        global PGRP
        np = PGRP.np
        mi = myid()
        # for now assume layout will be 1:n
        pmap = linspace(1,np)
        # determine size of local pieces
        sdd = dims[distdim]
        each = div(sdd,np)
        dist = fill(Array(Int32,np), each)
        dist[end] += rem(sdd,np)
        # todo: in general, if we're not part of this array
        mysz = (mi==0 ? 0 : dist[mi])
        locsz = ntuple(length(dims), i->(i==distdim?mysz:dims[i]))
        lp = 0
        for i=1:length(pmap)
            if pmap[i]==mi
                lp=i; break
            end
        end
        dist = cumsum(dist)
        da = new(dims,
                 (), #Array(T, locsz),
                 pmap, dist, distdim, lp, go)
        da.locl = remote_call(LocalProcess(), initializer, T, locsz, da)
        go.local_identity = da
        da
    end

    # don't use DArray() directly; use darray() below instead
    function DArray(T, distdim, dims, initializer)
        global PGRP
        np = PGRP.np
        mi = myid()
        go = GlobalObject()
        for i=1:np
            if i != mi
                remote_do(i, DArray, go, T, distdim, dims, initializer)
            end
        end
        DArray(go, T, distdim, dims, initializer)
    end
end

size(d::DArray) = d.dims

serialize(s, d::DArray) = serialize(s, d.go)

# when we actually need the data, wait for it
function localdata(d::DArray)
    if isa(d.locl, RemoteRef)
        d.locl = fetch(d.locl)
    end
    d.locl
end

# find which piece holds index i in the distributed dimension
function locate(d::DArray, i::Index)
    p = 1
    while i > d.dist[p]
        p += 1
    end
    p
end

## Constructors ##

function maxdim(dims)
    dsz = reduce(max,dims)
    for i=length(dims):-1:1
        if dims[i]==dsz
            return i
        end
    end
end

darray{T,N}(init, ::Type{T}, dims::NTuple{N,Size}, distdim) =
    DArray{T,N}(T,distdim,dims,init)
darray{T}(init, ::Type{T}, dims::Dims) = darray(init,T,dims,maxdim(dims))
darray(init, T::Type, dims::Size...) = darray(init, T, dims)
darray(init, dims::Dims) = darray(init, Float64, dims)
darray(init, dims::Size...) = darray(init, dims)

clone(d::DArray, T::Type, dims::Dims) =
    darray((T,d,da)->Array(T,d), T, dims,
           d.distdim>length(dims) ? maxdim(dims) : d.distdim)

dzeros(args...) = darray((T,d,da)->zeros(T,d), args...)
dones(args...)  = darray((T,d,da)->ones(T,d), args...)
drand(args...)  = darray((T,d,da)->rand(d), args...)
drandf(args...) = darray((T,d,da)->randf(d), args...)
drandn(args...) = darray((T,d,da)->randn(d), args...)

distribute(a::Array) = distribute(a, maxdim(size(a)))

function distribute{T}(a::Array{T}, distdim)
    sz = size(a)
    owner = myid()
    # create a remotely-visible reference to the array
    rr = remote_call(LocalProcess(), identity, a)
    wait(rr)
    darray((T,lsz,da)->get_my_piece(T,lsz,da,distdim,owner,rr),
           T, size(a), distdim)
end

# fetch one processor's piece of an array being distributed
function get_my_piece(T, lsz, da, distdim, owner, orig_array)
    if prod(lsz)==0
        return Array(T, lsz)
    end
    p = da.localpiece
    i1 = (p==1) ? 1 : da.dist[p-1]+1  # my first index in distdim
    iend = i1+lsz[distdim]-1
    # indexes of original array I will take
    idxs = ntuple(length(da.dims),
                  i->(i==distdim ? (i1:iend) : (1:lsz[i])))
    remote_call_fetch(owner, ref, orig_array, idxs...)
end

## Transpose and redist ##

transpose{T}(a::DArray{T,2}) = darray((T,d,da)->transpose(localdata(a)),
                                      T, (size(a,2),size(a,1)), (3-a.distdim))
ctranspose{T}(a::DArray{T,2}) = darray((T,d,da)->ctranspose(localdata(a)),
                                       T, (size(a,2),size(a,1)), (3-a.distdim))

## Indexing ##

function ref{T}(d::DArray{T,1}, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        offs = (p==1) ? 0 : d.dist[p-1]
        return localdata(d)[i-offs]
    end
    return remote_call_fetch(d.pmap[p], ref, d, i)
end

assign{T}(d::DArray{T,1}, v::Tensor{T,1}, i::Index) =
    invoke(assign, (DArray{T,1}, Any, Index), d, v, i)

function assign{T}(d::DArray{T,1}, v, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        offs = (p==1) ? 0 : d.dist[p-1]
        localdata(d)[i-offs] = v
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end

function ref(d::DArray, i::Index)
    sub = ind2sub(d.dims, i)
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = (p==1) ? 0 : d.dist[p-1]
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        return localdata(d)[sub...]
    end
    return remote_call_fetch(d.pmap[p], ref, d, i)
end

assign(d::DArray, v::Tensor, i::Index) =
    invoke(assign, (DArray, Any, Index), d, v, i)

function assign(d::DArray, v, i::Index)
    sub = ind2sub(d.dims, i)
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = (p==1) ? 0 : d.dist[p-1]
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        localdata(d)[sub...] = v
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end

full{T}(d::DArray{T}) = copy_to(Array(T, size(d)), d)

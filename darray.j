type DArray{T,N} <: Tensor{T,N}
    dims
    locl #::Array{T,N}
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

    global init_darray
    function init_darray(go, T, distdim, dims, initializer)
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

    function DArray(T, distdim, dims, initializer)
        global PGRP
        np = PGRP.np
        mi = myid()
        go = GlobalObject()
        for i=1:np
            if i != mi
                remote_do(i, init_darray, go, T, distdim, dims, initializer)
            end
        end
        init_darray(go, T, distdim, dims, initializer)
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

function locate(d::DArray, i::Index)
    p = 1
    while i > d.dist[p]
        p += 1
        if p > length(d.dist)
            error("invalid index")
        end
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

darray{T}(init, ::Type{T}, dims::Dims) = DArray(T,maxdim(dims),dims,init)
darray(init, T::Type, dims::Size...) = darray(init, T, dims)
darray(init, dims::Dims) = darray(init, Float64, dims)
darray(init, dims::Size...) = darray(init, dims)

dzeros(args...) = darray((T,d,da)->zeros(T,d), args...)
dones(args...)  = darray((T,d,da)->ones(T,d), args...)
drand(args...)  = darray((T,d,da)->rand(d), args...)
drandf(args...) = darray((T,d,da)->randf(d), args...)
drandn(args...) = darray((T,d,da)->randn(d), args...)

## Indexing ##

function ref{T}(d::DArray{T,1}, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        if p == 1
            return localdata(d)[i]
        else
            return localdata(d)[i-d.dist[p-1]]
        end
    end
    return fetch(remote_call(d.pmap[p], ref, d, i))
end

assign{T}(d::DArray{T,1}, v::Tensor{T,1}, i::Index) =
    invoke(assign, (DArray, Any, Index), d, v, i)

function assign{T}(d::DArray{T,1}, v, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        if p == 1
            localdata(d)[i] = v
        else
            localdata(d)[i-d.dist[p-1]] = v
        end
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end

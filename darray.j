type DArray{T,N} <: Tensor{T,N}
    dims
    locl::Array{T,N}
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
    function init_darray(go, T, distdim, dims)
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
        da = new(dims, Array(T, locsz), pmap, cumsum(dist), distdim, lp, go)
        go.local_identity = da
        da
    end

    function DArray(T, distdim, dims)
        global PGRP
        np = PGRP.np
        mi = myid()
        go = GlobalObject()
        for i=1:np
            if i != mi
                remote_do(i, init_darray, go, T, distdim, dims)
            end
        end
        init_darray(go, T, distdim, dims)
    end
end

size(d::DArray) = d.dims

serialize(s, d::DArray) = serialize(s, d.go)

localize(d::DArray) = d.locl

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

function ref{T}(d::DArray{T,1}, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        if p == 1
            return d.locl[i]
        else
            return d.locl[i-d.dist[p-1]]
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
            d.locl[i] = v
        else
            d.locl[i-d.dist[p-1]] = v
        end
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end

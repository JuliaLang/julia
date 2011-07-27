type DArray{T,N,distdim} <: AbstractArray{T,N}
    dims::NTuple{N,Size}
    locl::Array{T,N}
    # the distributed array has N pieces
    # pmap[i]==p ⇒ processor p has piece i
    pmap::Array{Int32,1}
    # piece i consists of indexes dist[i] through dist[i+1]-1
    dist::Array{Size,1}
    # dimension of distribution
    distdim::Int32
    localpiece::Int32  # my piece #; pmap[localpiece]==myid()
    go::GlobalObject

    function DArray(go, dims, initializer, pmap, dist)
        mi = myid()
        lp = 0
        for i=1:length(pmap)
            if pmap[i]==mi
                lp=i; break
            end
        end

        mysz = lp==0 ? 0 : (dist[lp+1]-dist[lp])
        locsz = ntuple(length(dims), i->(i==distdim?mysz:dims[i]))
        da = new()
        da.dims = dims
        da.pmap = pmap
        da.dist = dist
        da.distdim = distdim
        da.localpiece = lp
        da.go = go
        da.locl = initializer(T, locsz, da)
        #remote_call(LocalProcess(), initializer, T, locsz, da)
        da
    end

    # don't use DArray() directly; use darray() below instead
    function DArray(dims, initializer, procs, dist)
        go = GlobalObject(g->DArray{T,N,distdim}(g,dims,initializer,procs,dist))
        go.local_identity
    end

    function DArray(dims, initializer::Function)
        global PGRP
        procs = linspace(1,PGRP.np)
        dist = defaultdist(distdim, dims, length(procs))
        DArray{T,N,distdim}(dims, initializer, procs, dist)
    end
end

size(d::DArray) = d.dims

function serialize{T,N,dd}(s, d::DArray{T,N,dd})
    i = worker_id_from_socket(s)
    if is(member(d.go,i), false)
        sz = size(d)
        emptylocl = Array(T, ntuple(length(sz), i->(i==d.distdim?0:sz[i])))
        invoke(serialize, (Any, Any),
               s,
               ccall(:jl_new_structt, Any, (Any, Any),
                     DArray{T,N,dd},
                     (sz, emptylocl, d.pmap, d.dist, d.distdim, 0, d.go)))
    else
        serialize(s, d.go)
    end
end

# compute balanced dist vector
function defaultdist(distdim, dims, np)
    sdd = dims[distdim]
    each = div(sdd,np)
    sizes = fill(Array(Int32,np), each)
    sizes[end] += rem(sdd,np)
    [[1], cumsum(sizes)+1]
end

# when we actually need the data, wait for it
localize(r::RemoteRef) = localize(fetch(r))
localize{T,N}(d::DArray{T,N}) = d.locl

# find which piece holds index i in the distributed dimension
function locate(d::DArray, i::Index)
    p = 1
    while i >= d.dist[p+1]
        p += 1
    end
    p
end

#find which pieces hold which subranges in distributed dimension
#returns (pmap,dist) where pmap[i] contains dist[i]:dist[i+1]-1
function locate(d::DArray, I::Range1{Index})
    i = I[1]
    imax = I[length(I)]
    pmap = Array(Index,0)
    dist = [i]
    j = 1
    while i <= imax
        if i >= d.dist[j+1]
            j += 1
        else
            push(pmap,j)
            i = min(imax+1,d.dist[j+1])
            push(dist,i)
            j += 1
        end
    end
    return (pmap, dist)
end

#find which pieces hold which subranges in distributed dimension
#returns (pmap,dist,perm) where pmap[i] contains dist[i]:dist[i+1]-1
#and perm is the permutation which sorts I
function locate(d::DArray, I::Vector{Index})
    if isa(I, Range{Index}); I = I[:]; end
    (I, perm) = sortperm(I)

    i = I[1]
    imax = I[length(I)]
    pmap = Array(Index,0)
    dist = [i]
    j = 1
    while i <= imax
        if i >= d.dist[j+1]
            j += 1
        else
            push(pmap,j)
            i = min(imax+1,d.dist[j+1])
            push(dist,i)
            j += 1
        end
    end
    return (pmap, dist, perm)
end
## Constructors ##

function maxdim(dims)
    dsz = reduce(max,dims)
    for i=length(dims):-1:1
        if dims[i]==dsz
            return i
        end
    end
    error("unreachable")
end

# initializer is a function accepting (el_type, local_size, darray) where
# the last argument is the full DArray being constructed.
darray{T}(init, ::Type{T}, dims::Dims, distdim, procs, dist) =
    DArray{T,length(dims),long(distdim)}(dims, init, procs, dist)
darray{T}(init, ::Type{T}, dims::Dims, distdim, procs) =
    darray(init, T, dims, distdim, procs,
           defaultdist(distdim, dims, length(procs)))
darray{T}(init, ::Type{T}, dims::Dims, distdim) =
    darray(init, T, dims, distdim, linspace(1,PGRP.np))
darray{T}(init, ::Type{T}, dims::Dims) = darray(init,T,dims,maxdim(dims))
darray(init, T::Type, dims::Size...) = darray(init, T, dims)
darray(init, dims::Dims) = darray(init, Float64, dims)
darray(init, dims::Size...) = darray(init, dims)

similar(d::DArray, T::Type, dims::Dims) =
    darray((T,lsz,da)->Array(T,lsz), T, dims,
           d.distdim>length(dims) ? maxdim(dims) : d.distdim, d.pmap)

copy{T}(d::DArray{T}) =
    darray((T,lsz,da)->copy(localize(d)), T, size(d), d.distdim, d.pmap)

dzeros(args...)  = darray((T,d,da)->zeros(T,d), args...)
dones(args...)   = darray((T,d,da)->ones(T,d), args...)
dfill(v,args...) = darray((T,d,da)->fill(Array(T,d), v), typeof(v), args...)
drand(args...)   = darray((T,d,da)->rand(d), args...)
drandf(args...)  = darray((T,d,da)->randf(d), args...)
drandn(args...)  = darray((T,d,da)->randn(d), args...)

zero{T}(d::DArray{T}) = dzeros(T, size(d), d.distdim, d.pmap)

distribute(a::Array) = distribute(a, maxdim(size(a)))

function distribute{T}(a::Array{T}, distdim)
    owner = myid()
    # create a remotely-visible reference to the array
    rr = RemoteRef()
    put(rr, a)
    darray((T,lsz,da)->get_my_piece(T,lsz,da,distdim,owner,rr),
           T, size(a), distdim)
end

# fetch one processor's piece of an array being distributed
function get_my_piece(T, lsz, da, distdim, owner, orig_array)
    if prod(lsz)==0
        return Array(T, lsz)
    end
    p = da.localpiece
    i1 = da.dist[p]             # my first index in distdim
    iend = i1+lsz[distdim]-1    # my last  "
    # indexes of original array I will take
    idxs = { 1:lsz[i] | i=1:length(da.dims) }
    idxs[distdim] = (i1:iend)
    remote_call_fetch(owner, ref, orig_array, idxs...)
end

convert{T,N}(::Type{Array}, d::DArray{T,N}) = convert(Array{T,N}, d)

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    idxs = { 1:size(a,i) | i=1:N }
    for p = 1:length(d.dist)-1
        idxs[d.distdim] = d.dist[p]:(d.dist[p+1]-1)
        a[idxs...] = remote_call_fetch(d.pmap[p], localize, d)
    end
    a
end

## Transpose and redist ##

transpose{T}(a::DArray{T,2}) = darray((T,d,da)->transpose(localize(a)),
                                      T, (size(a,2),size(a,1)), (3-a.distdim),
                                      a.pmap)
ctranspose{T}(a::DArray{T,2}) = darray((T,d,da)->ctranspose(localize(a)),
                                       T, (size(a,2),size(a,1)), (3-a.distdim),
                                       a.pmap)

function node_ref(A::DArray, to_dist, range)
    Al = localize(A)
    locdims = size(Al)
    slice = { i == to_dist ? range : (1:locdims[i]) | i=1:ndims(A) }
    Al[slice...]
end

function node_changedist{T}(A::DArray{T}, da, local_size)
    locl = Array(T, local_size)
    if isempty(locl)
        return locl
    end
    to_dist = da.distdim
    newdist = da.dist
    from_dist = A.distdim
    dimsA = size(A)
    myidxs = newdist[da.localpiece]:newdist[da.localpiece+1]-1

    for p = 1:length(A.dist)-1
        R = remote_call_fetch(A.pmap[p], node_ref, A, to_dist, myidxs)
        sliceR = { i == from_dist ? (A.dist[p]:A.dist[p+1]-1) : (1:local_size[i]) | i=1:ndims(A) }
        locl[sliceR...] = R
    end
    locl
end

function changedist{T}(A::DArray{T}, to_dist)
    if A.distdim == to_dist; return A; end
    return darray((T,sz,da)->node_changedist(A, da, sz), T, size(A), to_dist,
                  A.pmap)
end

## Indexing ##

ref(r::RemoteRef) = invoke(ref, (RemoteRef, Any...), r)
function ref(r::RemoteRef, args...)
    global PGRP
    if r.where==myid()
        ref(fetch(r), args...)
    else
        remote_call_fetch(r.where, ref, r, args...)
    end
end

function assign(r::RemoteRef, args...)
    global PGRP
    if r.where==myid()
        assign(fetch(r), args...)
    else
        remote_do(r.where, assign, r, args...)
    end
end

function ref{T}(d::DArray{T,1}, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        offs = d.dist[p]-1
        return localize(d)[i-offs]
    end
    return remote_call_fetch(d.pmap[p], ref, d, i)::T
end

assign{T}(d::DArray{T,1}, v::AbstractArray, i::Index) =
    invoke(assign, (DArray{T,1}, Any, Index), d, v, i)

function assign{T}(d::DArray{T,1}, v, i::Index)
    p = locate(d, i)
    if p==d.localpiece
        offs = d.dist[p]-1
        localize(d)[i-offs] = v
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end

function ref{T}(d::DArray{T}, i::Index)
    sub = ind2sub(d.dims, i)
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = d.dist[p]-1
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        return localize(d)[sub...]::T
    end
    return remote_call_fetch(d.pmap[p], ref, d, i)::T
end

ref(d::DArray) = d
#ref(d::DArray, I::Range1{Index}) = ref(d, (I,))
#ref{distdim}(d::DArray{Any,1,distdim},I::Range1{Index}) = ref(d, (I,))
#ref{T,distdim}(d::DArray{T,1,distdim},I::Range1{Index}) = ref(d, (I,))
#ref{T}(d::DArray{T}, I::Range1{Index}...) = ref(d, I)
#ref{T}(d::DArray{T}, ::()) = ()

#function ref{T}(d::DArray{T}, I::(Range1{Index}...,))
function ref{T}(d::DArray{T}, I::Range1{Index}...)
    (pmap, dist) = locate(d, I[d.distdim])
    A = Array(T, map(range -> length(range), I))
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(length(size(d)), i -> (i == d.distdim ? I[i]-offs :
                                                           I[i]))
        A = localize(d)[J...]
        return A
    end
    for p = 1:length(pmap)
        offs = I[d.distdim][1] - 1
        J = ntuple(length(size(d)),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1))-offs :
                                                      (1:length(I[i]))))
        K = ntuple(length(size(d)),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1)) :
                                                      I[i]))
        A[J...] = remote_call_fetch(pmap[p], ref, d, K...)
    end
    return A
end

ref(d::DArray, I::Range1{Index}, j::Index) = d[I, j:j]
ref(d::DArray, i::Index, J::Range1{Index}) = d[i:i, J]

function ref{T}(d::DArray{T}, I::Vector{Index}...)
    (pmap, dist, perm) = locate(d, I[d.distdim])
    A = Array(T, map(range -> length(range), I))
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(length(size(d)), i -> (i == d.distdim ? I[i]-offs :
                                                           I[i]))
        A = localize(d)[J...]
        return A
    end
    n = length(perm)
    j = 1
    II = I[d.distdim][perm] #the sorted indexes in the distributed dimension
    for p = 1:length(pmap)
        if dist[p] > II[j]; continue; end
        lower = j
        while j <= n && II[j] < dist[p+1]
            j += 1
        end
        J = ntuple(length(size(d)),i->(i==d.distdim ? perm[lower:(j-1)] :
                                                      (1:length(I[i]))))
        K = ntuple(length(size(d)),i->(i==d.distdim ? II[lower:(j-1)] :
                                                      I[i]))
        A[J...] = remote_call_fetch(pmap[p], ref, d, K...)
    end
    return A
end

ref(d::DArray, I::Vector{Index}, j::Index) = d[I, [j]]
ref(d::DArray, i::Index, J::Vector{Index}) = d[[i], J]

assign(d::DArray, v::AbstractArray, i::Index) =
    invoke(assign, (DArray, Any, Index), d, v, i)

function assign(d::DArray, v, i::Index)
    sub = ind2sub(d.dims, i)
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = d.dist[p]-1
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        localize(d)[sub...] = v
    else
        remote_do(d.pmap[p], assign, d, v, i)
    end
    d
end


## matrix multiply ##

function node_multiply{T}(A::AbstractArray{T}, B, sz)
    locl = Array(T, sz)
    if !isempty(locl)
        cols = B.dist
        Adata = localize(A)
        for p=1:length(A.dist)-1
            r = remote_call_fetch(p, localize, B)
            locl[:, cols[p]:cols[p+1]-1] = Adata * r
        end
    end
    locl
end

function (*){T}(A::DArray{T,2,1}, B::DArray{T,2,2})
    if !isequal(A.pmap, B.pmap)
        error("unsupported case of distributed *")
    end
    darray((T,sz,da)->node_multiply(A,B,sz), T, (size(A,1),size(B,2)), 1,
           A.pmap)
end

(*){T}(A::DArray{T,2}, B::DArray{T,2}) = changedist(A, 1) * changedist(B, 2)

## elementwise operators ##

macro binary_darray_op(f)
    quote
        function ($f){T}(A::Number, B::DArray{T})
            S = promote_type(typeof(A),T)
            darray((T,lsz,da)->($f)(A, localize(B)),
                   S, size(B), B.distdim, B.pmap)
        end
        function ($f){T}(A::DArray{T}, B::Number)
            S = promote_type(T,typeof(B))
            darray((T,lsz,da)->($f)(localize(A), B),
                   S, size(A), A.distdim, A.pmap)
        end
    end # quote
end # macro

@binary_darray_op (+)
@binary_darray_op (-)
@binary_darray_op (.*)
@binary_darray_op (.^)

macro unary_darray_op(f)
    quote
        function ($f){T}(A::DArray{T})
            darray((T,lsz,da)->($f)(localize(A)),
                   T, size(A), A.distdim, A.pmap)
        end
    end # quote
end # macro

@unary_darray_op (-)
@unary_darray_op (~)
@unary_darray_op (conj)

macro unary_darray_c2r_op(f)
    quote
        function ($f){T}(A::DArray{T})
            S = typeof(($f)(zero(T)))
            darray((T,lsz,da)->($f)(localize(A)),
                   S, size(A), A.distdim, A.pmap)
        end
    end # quote
end # macro

@unary_darray_c2r_op (real)
@unary_darray_c2r_op (imag)

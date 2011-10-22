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

    function DArray(go, initializer, dims, pmap, dist)
        mi = myid()
        lp = 0
        for i=1:length(pmap)
            if pmap[i]==mi
                lp=i; break
            end
        end

        mysz = lp==0 ? 0 : (dist[lp+1]-dist[lp])
        locsz = ntuple(length(dims), i->(i==distdim ? mysz : dims[i]))
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
    function DArray(initializer, dims, procs, dist)
        go = GlobalObject(g->DArray{T,N,distdim}(g,initializer,dims,procs,dist))
        go.local_identity
    end

    function DArray(initializer::Function, dims)
        procs = linspace(1, min(nprocs(),dims[distdim]))
        dist = defaultdist(distdim, dims, length(procs))
        DArray{T,N,distdim}(initializer, dims, procs, dist)
    end
end

typealias SubDArray{T,N}   SubArray{T,N,DArray{T}}
typealias SubOrDArray{T,N} Union(DArray{T,N}, SubDArray{T,N})

size(d::DArray) = d.dims
distdim(d::DArray) = d.distdim
procmap(d::DArray) = d.pmap
distdim(d::SubDArray) = d.parent.distdim
procmap(d::SubDArray) = d.parent.pmap

function serialize{T,N,dd}(s, d::DArray{T,N,dd})
    i = worker_id_from_socket(s)
    if is(member(d.go,i), false)
        sz = size(d)
        emptylocl = Array(T, ntuple(length(sz), i->(i==d.distdim ? 0 : sz[i])))
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
    if sdd >= np
        linspace(1, sdd+1, np+1)
    else
        [[1:(sdd+1)], zeros(Size, np-sdd)]
    end
end

function pieceindexes(d::DArray, p)
    if p == 0
        return ntuple(ndims(d), i->(i==d.distdim ? (1:0) : 1:size(d,i)))
    end
    ntuple(ndims(d), i->(i==d.distdim ? (d.dist[p]:d.dist[p+1]-1) :
                                        1:size(d,i)))
end

myindexes(d::DArray) = pieceindexes(d, d.localpiece)

function myindexes(s::SubDArray, locl)
    d = s.parent
    lo = d.dist[d.localpiece]
    hi = d.dist[d.localpiece+1]-1
    sdi = s.indexes[d.distdim]
    l = localize(d)
    if isa(sdi,Int)
        if lo <= sdi <= hi
            r = sdi
            if locl
                r -= (lo-1)
            end
        else
            return ntuple(ndims(l), i->(i==d.distdim ? (1:0) : 1:size(s,i)))
        end
    else
        r = intersect(lo:hi, sdi)
        if locl
            r -= (lo-1)
        end
    end
    return ntuple(ndims(l), i->(i==d.distdim ? r : 1:size(s,i)))
end

myindexes(s::SubDArray) = myindexes(s, false)

# when we actually need the data, wait for it
localize(r::RemoteRef) = localize(fetch(r))
localize(d::DArray) = d.locl

localize(s::SubDArray) = sub(localize(s.parent), myindexes(s, true))

# fetch the part of "src" that overlaps with the local part of "dest"
# equivalent to src[myindexes(dest)...]
function localize(src::DArray, dest::DArray)
    if size(src)==size(dest) && distdim(src)==distdim(dest) &&
       src.dist[src.localpiece]   == dest.dist[dest.localpiece] &&
       src.dist[src.localpiece+1] == dest.dist[dest.localpiece+1]
        return localize(src)
    end
    src[myindexes(dest)...]
end

function localize(src::SubDArray, dest::DArray)
    di = myindexes(dest)
    if isequal(myindexes(src), di)
        return localize(src)
    end
    src[di...]
end

# find which piece holds index i in the distributed dimension
function locate(d::DArray, i::Index)
    p = 1
    while i >= d.dist[p+1]
        p += 1
    end
    p
end

# find which processor holds index i in the distributed dimension
owner(d::DArray, i::Index) = d.pmap[locate(d, i)]

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
function locate(d::DArray, I::AbstractVector{Index})
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
    DArray{T,length(dims),long(distdim)}(init, dims, procs, dist)

function darray{T}(init, ::Type{T}, dims::Dims, distdim, procs)
    sdd = dims[distdim]
    np = length(procs)
    if sdd < np
        procs = procs[1:sdd]
    end
    darray(init, T, dims, distdim, procs,
           defaultdist(distdim, dims, length(procs)))
end

function darray{T}(init, ::Type{T}, dims::Dims, distdim)
    procs = linspace(1, min(nprocs(),dims[distdim]))
    darray(init, T, dims, distdim, procs,
           defaultdist(distdim, dims, length(procs)))
end

darray{T}(init::Function, ::Type{T}, dims::Dims) =
    darray(init,T,dims,maxdim(dims))
darray(init::Function, T::Type, dims::Size...) = darray(init, T, dims)
darray(init::Function, dims::Dims) = darray(init, Float64, dims)
darray(init::Function, dims::Size...) = darray(init, dims)

darray(T::Type, args...)     = darray((T,lsz,da)->Array(T,lsz), T, args...)
darray(dims::Dims, args...)  = darray((T,lsz,da)->Array(T,lsz), dims, args...)
darray(dims::Size...)        = darray((T,lsz,da)->Array(T,lsz), dims)

similar(d::DArray, T::Type, dims::Dims) =
    darray((T,lsz,da)->Array(T,lsz), T, dims,
           d.distdim>length(dims) ? maxdim(dims) : d.distdim, d.pmap)

copy{T}(d::DArray{T}) =
    darray((T,lsz,da)->copy(localize(d)), T, size(d), d.distdim,d.pmap,d.dist)

function copy_to(d::DArray, src::DArray)
    @sync begin
        for p = d.pmap
            @spawnat p copy_to(localize(d), src[myindexes(d)...])
        end
    end
    return d
end

function copy_to(d::DArray, src::AbstractArray)
    @sync begin
        for i = 1:length(d.pmap)
            p = d.pmap[i]
            block = src[pieceindexes(d, i)...]
            @spawnat p copy_to(localize(d), block)
        end
    end
    return d
end

dzeros(args...)  = darray((T,d,da)->zeros(T,d), args...)
dones(args...)   = darray((T,d,da)->ones(T,d), args...)
dfill(v,args...) = darray((T,d,da)->fill(Array(T,d), v), typeof(v), args...)
dcell(args...)   = darray((T,d,da)->cell(d), Any, args...)
drand(args...)   = darray((T,d,da)->rand(d), Float64, args...)
drandn(args...)  = darray((T,d,da)->randn(d), Float64, args...)

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
                                      a.pmap, a.dist)
ctranspose{T}(a::DArray{T,2}) = darray((T,d,da)->ctranspose(localize(a)),
                                       T, (size(a,2),size(a,1)), (3-a.distdim),
                                       a.pmap, a.dist)

function changedist{T}(A::DArray{T}, to_dist)
    if A.distdim == to_dist; return A; end
    return darray((T,sz,da)->A[myindexes(da)...], T, size(A), to_dist, A.pmap)
end

## Indexing ##

ref(r::RemoteRef) = invoke(ref, (RemoteRef, Any...), r)
function ref(r::RemoteRef, args...)
    if r.where==myid()
        ref(fetch(r), args...)
    else
        remote_call_fetch(r.where, ref, r, args...)
    end
end

function assign(r::RemoteRef, args...)
    if r.where==myid()
        assign(fetch(r), args...)
    else
        remote_do(r.where, assign, r, args...)
    end
end

# 1d scalar ref
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

# 1d scalar assign
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

# Nd scalar ref
function ref_elt{T}(d::DArray{T}, sub::(Index...))
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = d.dist[p]-1
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        return localize(d)[sub...]::T
    end
    return remote_call_fetch(d.pmap[p], ref_elt, d, sub)::T
end

ref{T}(d::DArray{T}, i::Index)    = ref_elt(d, ind2sub(d.dims, i))
ref{T}(d::DArray{T}, I::Index...) = ref_elt(d, I)

ref(d::DArray) = d

# Nd ref with Range1 indexes
function ref{T}(d::DArray{T}, I::Range1{Index}...)
    (pmap, dist) = locate(d, I[d.distdim])
    A = Array(T, map(range -> length(range), I))
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                    I[i]))
        A = localize(d)[J...]
        return A
    end
    for p = 1:length(pmap)
        offs = I[d.distdim][1] - 1
        J = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1))-offs :
                                               (1:length(I[i]))))
        K = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1)) :
                                               I[i]))
        A[J...] = remote_call_fetch(d.pmap[pmap[p]], ref, d, K...)
    end
    return A
end

# combinations of Range1 and scalar indexes
ref(d::DArray, I::Range1{Index}, j::Index) = d[I, j:j]
ref(d::DArray, i::Index, J::Range1{Index}) = d[i:i, J]

ref(d::DArray, I::Union(Index,Range1{Index})...) =
    d[ntuple(length(I),i->(isa(I[i],Index) ? (I[i]:I[i]) : I[i] ))...]


# Nd ref with vector indexes
function ref{T}(d::DArray{T}, I::AbstractVector{Index}...)
    (pmap, dist, perm) = locate(d,[I[d.distdim]])
    A = Array(T, map(range -> length(range), I))
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
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
        J = ntuple(ndims(d),i->(i==d.distdim ? perm[lower:(j-1)] :
                                               (1:length(I[i]))))
        K = ntuple(ndims(d),i->(i==d.distdim ? II[lower:(j-1)] :
                                               I[i]))
        A[J...] = remote_call_fetch(d.pmap[pmap[p]], ref, d, K...)
    end
    return A
end

# combinations of vector and scalar indexes
ref(d::DArray, I::AbstractVector{Index}, j::Index) = d[I, [j]]
ref(d::DArray, i::Index, J::AbstractVector{Index}) = d[[i], J]

ref(d::DArray, I::Union(Index,AbstractVector{Index})...) =
    d[ntuple(length(I),i->(isa(I[i],Index) ? [I[i]] : I[i] ))...]

# Nd scalar assign
function assign_elt(d::DArray, v, sub::(Index...))
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = d.dist[p]-1
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        localize(d)[sub...] = v
    else
        remote_do(d.pmap[p], assign_elt, d, v, sub)
    end
    d
end

# disambiguating definitions
assign(d::DArray, v::AbstractArray) = assign_elt(d, v, ())

assign(d::DArray, v::AbstractArray, i::Index) =
    assign_elt(d, v, ind2sub(d.dims, i))

assign{T}(d::DArray{T,2}, v::AbstractArray, i0::Index, i1::Index) =
    assign_elt(d, v, (i0,i1))
assign(d::DArray, v::AbstractArray, i0::Index, i1::Index) =
    assign_elt(d, v, (i0,i1))

assign(d::DArray, v::AbstractArray, i0::Index, I::Index...) =
    assign_elt(d, v, tuple(i0,I...))

assign(d::DArray, v, i::Index) = assign_elt(d, v, ind2sub(d.dims, i))
assign(d::DArray, v, i0::Index, I::Index...) = assign_elt(d, v, tuple(i0,I...))

#TODO: Fix this
assign(d::DArray, v) = error("distributed arrays of dimension 0 not supported")

# Nd assign, scalar fill case, with Range1 indexes
function assign(d::DArray, v, I::Range1{Index}...)
    (pmap, dist) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                    I[i]))
        localize(d)[J...] = v
        return d
    end
    for p = 1:length(pmap)
        offs = I[d.distdim][1] - 1
        K = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1)) :
                                               I[i]))
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v, K...))
    end
    return d
end

# Nd assign, array copy case, with Range1 indexes
#TODO: check for same size
function assign(d::DArray, v::AbstractArray, I::Range1{Index}...)
    (pmap, dist) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                    I[i]))
        localize(d)[J...] = v
        return d
    end
    for p = 1:length(pmap)
        offs = I[d.distdim][1] - 1
        J = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1))-offs :
                                               (1:length(I[i]))))
        K = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1)) :
                                               I[i]))
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v[J...], K...))
    end
    return d
end

# Nd assign, scalar fill case, vector indexes
function assign(d::DArray, v, I::AbstractVector{Index}...)
    (pmap, dist, perm) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                    I[i]))
        localize(d)[J...] = v
        return d
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
        K = ntuple(ndims(d),i->(i==d.distdim ? II[lower:(j-1)] :
                                               I[i]))
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v, K...))
    end
    return d
end

# Nd assign, array copy case, vector indexes
#TODO: check for same size
function assign(d::DArray, v::AbstractArray, I::AbstractVector{Index}...)
    (pmap, dist, perm) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                    I[i]))
        localize(d)[J...] = v
        return d
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
        J = ntuple(ndims(d),i->(i==d.distdim ? perm[lower:(j-1)] :
                                               (1:length(I[i]))))
        K = ntuple(ndims(d),i->(i==d.distdim ? II[lower:(j-1)] :
                                               I[i]))
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v[J...], K...))
    end
    return d
end

# assign with combinations of Range1 and scalar indexes
assign(d::DArray, v, I::Union(Index,Range1{Index})...) =
    assign(d,v,ntuple(length(I),i->(isa(I[i],Index) ? (I[i]:I[i]) : I[i] ))...)

# assign with combinations of vector and scalar indexes
assign(d::DArray, v, I::Union(Index,AbstractVector{Index})...) =
    assign(d,v,ntuple(length(I),i->(isa(I[i],Index) ? [I[i]] : I[i] ))...)

## matrix multiply ##

function node_multiply2{T}(A::AbstractArray{T}, B, sz)
    locl = Array(T, sz)
    if !isempty(locl)
        Bdata = localize(B)
        np = length(B.pmap)
        nr = size(locl,1)
        if np >= nr
            rows = [1,nr+1]
        else
            rows = linspace(1,nr+1,np+1)
        end
        for p=1:length(rows)-1
            R = rows[p]:rows[p+1]-1
            locl[R, :] = A[R, :] * Bdata
        end
    end
    locl
end

function (*){T}(A::DArray{T,2,1}, B::DArray{T,2,2})
    darray((T,sz,da)->node_multiply2(A,B,sz), T, (size(A,1),size(B,2)), 2,
           B.pmap)
end

function (*){T}(A::DArray{T,2}, B::DArray{T,2,2})
    darray((T,sz,da)->node_multiply2(A,B,sz), T, (size(A,1),size(B,2)), 2,
           B.pmap)
end

function node_multiply1{T}(A::AbstractArray{T}, B, sz)
    locl = Array(T, sz)
    if !isempty(locl)
        Adata = localize(A)
        np = length(A.pmap)
        nc = size(locl,2)
        if np >= nc
            cols = [1,nc+1]
        else
            cols = linspace(1,nc+1,np+1)
        end
        for p=1:length(cols)-1
            C = cols[p]:cols[p+1]-1
            locl[:, C] = Adata * B[:, C]
        end
    end
    locl
end

function (*){T}(A::DArray{T,2,1}, B::DArray{T,2})
    darray((T,sz,da)->node_multiply1(A,B,sz), T, (size(A,1),size(B,2)), 1,
           A.pmap)
end

(*){T}(A::DArray{T,2}, B::DArray{T,2}) = A * changedist(B, 2)

## elementwise operators ##

function .^{T}(A::Int, B::SubOrDArray{T})
    S = promote_type(typeof(A),T)
    darray((T,lsz,da)->.^(A, localize(B, da)),
           S, size(B), distdim(B), procmap(B))
end
function .^{T}(A::SubOrDArray{T}, B::Int)
    S = promote_type(T,typeof(B))
    darray((T,lsz,da)->.^(localize(A, da), B),
           S, size(A), distdim(A), procmap(A))
end

function .^{T<:Int}(A::Int, B::SubOrDArray{T})
    darray((T,lsz,da)->.^(A, localize(B, da)),
           Float64, size(B), distdim(B), procmap(B))
end
function .^{T<:Int}(A::SubOrDArray{T}, B::Int)
    S = B < 0 ? Float64 : promote_type(T,typeof(B))
    darray((T,lsz,da)->.^(localize(A, da), B),
           S, size(A), distdim(A), procmap(A))
end

macro binary_darray_op(f)
    quote
        function ($f){T}(A::Number, B::SubOrDArray{T})
            S = typeof(($f)(one(A),one(T)))
            darray((T,lsz,da)->($f)(A, localize(B, da)),
                   S, size(B), distdim(B), procmap(B))
        end
        function ($f){T}(A::SubOrDArray{T}, B::Number)
            S = typeof(($f)(one(T),one(B)))
            darray((T,lsz,da)->($f)(localize(A, da), B),
                   S, size(A), distdim(A), procmap(A))
        end
        function ($f){T,S}(A::SubOrDArray{T}, B::SubOrDArray{S})
            if size(A) != size(B)
                error("argument dimensions must match")
            end
            R = typeof(($f)(one(T), one(S)))
            darray((T,lsz,da)->($f)(localize(A, da), localize(B, da)),
                   R, size(A), distdim(A), procmap(A))
        end
    end # quote
end # macro

@binary_darray_op (+)
@binary_darray_op (-)
@binary_darray_op (.*)
@binary_darray_op (./)
@binary_darray_op (.^)
@binary_darray_op (&)
@binary_darray_op (|)
@binary_darray_op ($)

macro unary_darray_op(f)
    quote
        function ($f){T}(A::SubOrDArray{T})
            darray((T,lsz,da)->($f)(localize(A, da)),
                   T, size(A), distdim(A), procmap(A))
        end
    end # quote
end # macro

@unary_darray_op (-)
@unary_darray_op (~)
@unary_darray_op (conj)

macro unary_darray_c2r_op(f)
    quote
        function ($f){T}(A::SubOrDArray{T})
            S = typeof(($f)(zero(T)))
            darray((T,lsz,da)->($f)(localize(A, da)),
                   S, size(A), distdim(A), procmap(A))
        end
    end # quote
end # macro

@unary_darray_c2r_op (real)
@unary_darray_c2r_op (imag)

function map(f, A::SubOrDArray)
    T = typeof(f(A[1]))
    darray((T,lsz,da)->map_to(Array(T,lsz), f, localize(A, da)),
           T, size(A), distdim(A), procmap(A))
end

# map a function that is already vectorized
function map_vectorized(f, A::SubOrDArray)
    T = typeof(f(A[1]))
    darray((T,lsz,da)->f(localize(A, da)),
           T, size(A), distdim(A), procmap(A))
end

for f = (:ceil, :floor, :trunc, :round, :iround, :itrunc,
         :sqrt, :cbrt, :sin, :cos, :tan, :sinh, :cosh, :tanh,
         :asin, :acos, :atan,
         :log, :log2, :exp, :expm1)
    @eval ($f)(A::SubOrDArray) = map_vectorized($f, A)
end

macro binary_darray_comparison_op(f)
    quote
        function ($f){T}(A::Number, B::SubOrDArray{T})
            darray((T,lsz,da)->($f)(A, localize(B, da)),
                   Bool, size(B), distdim(B), procmap(B))
        end
        function ($f){T}(A::SubOrDArray{T}, B::Number)
            darray((T,lsz,da)->($f)(localize(A, da), B),
                   Bool, size(A), distdim(A), procmap(A))
        end
        function ($f){T,S}(A::SubOrDArray{T}, B::SubOrDArray{S})
            if size(A) != size(B)
                error("argument dimensions must match")
            end
            darray((T,lsz,da)->($f)(localize(A, da), localize(B, da)),
                   Bool, size(A), distdim(A), procmap(A))
        end
    end # quote
end # macro

@binary_darray_comparison_op (==)
@binary_darray_comparison_op (!=)
@binary_darray_comparison_op (<)
@binary_darray_comparison_op (<=)

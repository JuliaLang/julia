type DArray{T,N,distdim} <: AbstractArray{T,N}
    go::GlobalObject
    dims::NTuple{N,Int}
    # the distributed array has N pieces
    # pmap[i]==p ⇒ processor p has piece i
    pmap::Array{Int,1}
    # piece i consists of indexes dist[i] through dist[i+1]-1
    dist::Array{Int,1}
    # dimension of distribution
    distdim::Int
    localpiece::Int  # my piece #; pmap[localpiece]==myid()
    locl::Array{T,N}

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
        da = new(go, dims, pmap, dist, distdim, lp)
        da.locl = lp==0 ? Array(T, locsz) : initializer(T, locsz, da)
        return da
    end

    # don't use DArray() directly; use darray() below instead
    function DArray(initializer, dims, procs, dist::Array{Int,1})
        go = GlobalObject(procs,
                          g->DArray{T,N,distdim}(g,initializer,dims,procs,dist))
        go.local_identity
    end

    function DArray(initializer::Function, dims)
        procs = [1:min(nprocs(),dims[distdim])]
        dist = defaultdist(distdim, dims, length(procs))
        DArray{T,N,distdim}(initializer, dims, procs, dist)
    end
end

typealias SubDArray{T,N,D<:DArray} SubArray{T,N,D}
typealias SubOrDArray{T,N}         Union(DArray{T,N}, SubDArray{T,N})

size(d::DArray) = d.dims
distdim(d::DArray) = d.distdim
procs(d::DArray) = d.pmap
dist(d::DArray) = d.dist
distdim(d::SubDArray) = d.parent.distdim

function serialize{T,N,dd}(s, d::DArray{T,N,dd})
    i = worker_id_from_socket(s)
    if is(is_go_member(d.go,i), false)
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
        [[1:(sdd+1)], zeros(Int, np-sdd)]
    end
end

function pieceindexes(d::DArray, p)
    dd = distdim(d)
    ntuple(ndims(d), i->(i==dd ? pieceindex(d, p) : 1:size(d,i)))
end

function pieceindexes(d::SubDArray, p, locl::Bool)
    dd = distdim(d)
    if locl
        ntuple(ndims(d), i->(i==dd ? pieceindex(d, p, locl) : d.indexes[i]))
    else
        ntuple(ndims(d), i->(i==dd ? pieceindex(d, p, locl) : 1:size(d,i)))
    end
end

pieceindexes(s::SubDArray, p) = pieceindexes(s, p, false)

function pieceindex(d::DArray, p)
    if p == 0
        return 1:0
    end
    d.dist[p]:(d.dist[p+1]-1)
end

pieceindex(s::SubDArray, p) = pieceindex(s, p, false)

function pieceindex(s::SubDArray, p, locl::Bool)
    d = s.parent
    dd = d.distdim
    nd = ndims(d)
    if p == 0
        return 1:0
    end
    lo = d.dist[p]
    hi = d.dist[p+1]-1
    sdi = s.indexes[dd]
    if isa(sdi,Integer)
        if lo <= sdi <= hi
            r = sdi
            if locl
                r -= (lo-1)
            end
        else
            return 1:0
        end
    else
        r = intersect(lo:hi, sdi)
        if locl
            r -= (lo-1)
        end
    end
    return r
end

myindexes(d::DArray) = pieceindexes(d, d.localpiece)

myindexes(s::SubDArray) = pieceindexes(s, s.parent.localpiece)

myindexes(s::SubDArray, locl) = pieceindexes(s, s.parent.localpiece, locl)

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

function localize_copy(src::DArray, dest::DArray)
    if size(src)==size(dest) && distdim(src)==distdim(dest) &&
       src.dist[src.localpiece]   == dest.dist[dest.localpiece] &&
       src.dist[src.localpiece+1] == dest.dist[dest.localpiece+1]
        return copy(localize(src))
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

function localize_copy(src::SubDArray, dest::DArray)
    di = myindexes(dest)
    if isequal(myindexes(src), di)
        return copy(localize(src))
    end
    src[di...]
end

# piece numbers covered by a subarray
function sub_da_pieces(s::SubDArray)
    dd = s.parent.distdim
    sdi = s.indexes[dd]
    if isa(sdi,Integer)
        p = locate(s.parent, sdi)
        return p:p
    end
    lo = locate(s.parent, sdi[1])
    hi = locate(s.parent, sdi[end])
    if hi < lo
        return hi:lo
    end
    return lo:hi
end

procs(s::SubDArray) = s.parent.pmap[sub_da_pieces(s)]

function dist(s::SubDArray)
    pcs = sub_da_pieces(s)
    sizes = [ length(pieceindex(s, p)) for p = pcs ]
    cumsum([1, sizes])
end

# find which piece holds index i in the distributed dimension
function locate(d::DArray, i::Int)
    p = 1
    while i >= d.dist[p+1]
        p += 1
    end
    p
end

# find which processor holds index i in the distributed dimension
owner(d::DArray, i::Int) = d.pmap[locate(d, i)]

#find which pieces hold which subranges in distributed dimension
#returns (pmap,dist) where pmap[i] contains dist[i]:dist[i+1]-1
function locate(d::DArray, I::Range1{Int})
    i = I[1]
    imax = I[length(I)]
    pmap = Array(Int,0)
    dist = [i]
    j = 1
    while i <= imax
        if i >= d.dist[j+1]
            j += 1
        else
            push!(pmap,j)
            i = min(imax+1,d.dist[j+1])
            push!(dist,i)
            j += 1
        end
    end
    return (pmap, dist)
end

#find which pieces hold which subranges in distributed dimension
#returns (pmap,dist,perm) where pmap[i] contains dist[i]:dist[i+1]-1
#and perm is the permutation which sorts I
function locate(d::DArray, I::AbstractVector{Int})
    if isa(I, Range{Int}); I = I[:]; end
    (I, perm) = sortperm(I)

    i = I[1]
    imax = I[length(I)]
    pmap = Array(Int,0)
    dist = [i]
    j = 1
    while i <= imax
        if i >= d.dist[j+1]
            j += 1
        else
            push!(pmap,j)
            i = min(imax+1,d.dist[j+1])
            push!(dist,i)
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
darray{T}(init, ::Type{T}, dims::Dims, distdim, procs, dist::Array{Int,1}) =
    DArray{T,length(dims),int(distdim)}(init, dims, procs, dist)

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
    procs = [1:min(nprocs(),dims[distdim])]
    darray(init, T, dims, distdim, procs,
           defaultdist(distdim, dims, length(procs)))
end

darray{T}(init::Function, ::Type{T}, dims::Dims) =
    darray(init,T,dims,maxdim(dims))
darray(init::Function, T::Type, dims::Int...) = darray(init, T, dims)
darray(init::Function, dims::Dims) = darray(init, Float64, dims)
darray(init::Function, dims::Int...) = darray(init, dims)

darray(T::Type, args...)     = darray((T,lsz,da)->Array(T,lsz), T, args...)
darray(dims::Dims, args...)  = darray((T,lsz,da)->Array(T,lsz), dims, args...)
darray(dims::Int...)         = darray((T,lsz,da)->Array(T,lsz), dims)

# construct a DArray as a function of each block of another
function darray(f::Function, A::SubOrDArray)
    darray((T,lsz,da)->f(localize(A, da)),
           eltype(A), size(A), distdim(A), procs(A))
end

similar(d::DArray, T::Type, dims::Dims) =
    darray((T,lsz,da)->Array(T,lsz), T, dims,
           d.distdim>length(dims) ? maxdim(dims) : d.distdim, d.pmap)

copy{T}(d::SubOrDArray{T}) =
    darray((T,lsz,da)->localize_copy(d, da), T, size(d), distdim(d), procs(d))

function copy!(d::DArray, src::SubOrDArray)
    @sync begin
        for p = d.pmap
            @spawnat p copy!(localize(d), localize(src, d))
        end
    end
    return d
end

function copy!(d::DArray, src::AbstractArray)
    @sync begin
        for i = 1:length(d.pmap)
            p = d.pmap[i]
            block = src[pieceindexes(d, i)...]
            @spawnat p copy!(localize(d), block)
        end
    end
    return d
end

dzeros(args...)  = darray((T,d,da)->zeros(T,d), args...)
dones(args...)   = darray((T,d,da)->ones(T,d), args...)
dfill(v,args...) = darray((T,d,da)->fill(v, d), typeof(v), args...)
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
    darray((T,lsz,da)->distribute_one(T,lsz,da,distdim,owner,rr),
           T, size(a), distdim)
end

# fetch one processor's piece of an array being distributed
function distribute_one(T, lsz, da, distdim, owner, orig_array)
    if prod(lsz)==0
        return Array(T, lsz)
    end
    p = da.localpiece
    i1 = da.dist[p]             # my first index in distdim
    iend = i1+lsz[distdim]-1    # my last  "
    # indexes of original array I will take
    idxs = { 1:lsz[i] for i=1:length(da.dims) }
    idxs[distdim] = (i1:iend)
    remote_call_fetch(owner, ref, orig_array, idxs...)
end

convert{T,N}(::Type{Array}, d::DArray{T,N}) = convert(Array{T,N}, d)

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    idxs = { 1:size(a,i) for i=1:N }
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

function da_reshape(T, sz, da, A)
    mi = myindexes(da)
    i0s = map(first, mi)
    i1s = map(last, mi)
    i0 = sub2ind(size(da), i0s...)
    i1 = sub2ind(size(da), i1s...)
    I = map(colon, ind2sub(size(A), i0), ind2sub(size(A), i1))
    reshape(A[I...], sz)
end

function reshape(A::DArray, dims::Dims)
    if prod(dims) != length(A)
        error("reshape: invalid dimensions")
    end
    darray((T,sz,da)->da_reshape(T,sz,da,A),
           eltype(A), dims, maxdim(dims), A.pmap)
end

transpose{T} (v::DArray{T,1}) = reshape(v, 1, size(v,1))
ctranspose{T<:Number}(v::DArray{T,1}) = conj(reshape(v, 1, size(v,1)))
ctranspose{T<:Real}(v::DArray{T,1}) = transpose(v)

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
        sync_add(remote_call(r.where, assign, r, args...))
    end
end

# 1d scalar ref
function ref{T}(d::DArray{T,1}, i::Int)
    p = locate(d, i)
    if p==d.localpiece
        offs = d.dist[p]-1
        return localize(d)[i-offs]
    end
    return remote_call_fetch(d.pmap[p], ref, d, i)::T
end

# 1d scalar assign
function assign{T}(d::DArray{T,1}, v, i::Int)
    p = locate(d, i)
    if p==d.localpiece
        offs = d.dist[p]-1
        localize(d)[i-offs] = v
    else
        sync_add(remote_call(d.pmap[p], assign, d, v, i))
    end
    d
end

# Nd scalar ref
function ref_elt{T}(d::DArray{T}, sub::(Int...))
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

ref(d::DArray, i::Int)    = ref_elt(d, ind2sub(d.dims, i))
ref(d::DArray, I::Int...) = ref_elt(d, I)

ref(d::DArray) = d

function da_sub(d::DArray, I::Range1{Int}...)
    offs = d.dist[d.localpiece]-1
    J = ntuple(ndims(d), i -> (i == d.distdim ? I[i]-offs :
                                                I[i]))
    return sub(localize(d), J)
end

# Nd ref with Range1 indexes
function ref{T}(d::DArray{T}, I::Range1{Int}...)
    (pmap, dist) = locate(d, I[d.distdim])
    np = length(pmap)
    if np == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
        return localize(d)[J...]
    end
    A = Array(T, map(length, I))
    deps = cell(np)
    for p = 1:np
        K = [ i==d.distdim ? (dist[p]:(dist[p+1]-1)) : I[i] for i=1:ndims(d) ]
        if np == 1
            # use remote_call_fetch if we only need to communicate with 1 proc
            deps[p] = remote_call_fetch(d.pmap[pmap[p]], da_sub, d, K...)
        else
            deps[p] = remote_call(d.pmap[pmap[p]], da_sub, d, K...)
        end
    end
    for p = 1:np
        offs = I[d.distdim][1] - 1
        J = [ i==d.distdim ? (dist[p]:(dist[p+1]-1))-offs : (1:length(I[i])) for i=1:ndims(d) ]
        A[J...] = fetch(deps[p])
    end
    return A
end

# combinations of Range1 and scalar indexes
ref(d::DArray, I::Range1{Int}, j::Int) = d[I, j:j]
ref(d::DArray, i::Int, J::Range1{Int}) = d[i:i, J]

ref(d::DArray, I::Union(Int,Range1{Int})...) =
    d[[isa(i,Int) ? (i:i) : i for i in I ]...]


# Nd ref with vector indexes
function ref{T}(d::DArray{T}, I::AbstractVector{Int}...)
    (pmap, dist, perm) = locate(d,[I[d.distdim]])
    np = length(pmap)
    if np == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
        return localize(d)[J...]
    end
    A = Array(T, map(length, I))
    n = length(perm)
    II = I[d.distdim][perm] #the sorted indexes in the distributed dimension
    deps = cell(np)
    j = 1
    for p = 1:np
        if dist[p] > II[j]; continue; end
        lower = j
        while j <= n && II[j] < dist[p+1]
            j += 1
        end
        K = [ i==d.distdim ? II[lower:(j-1)] : I[i] for i=1:ndims(d) ]
        if np == 1
            deps[p] = remote_call_fetch(d.pmap[pmap[p]], ref, d, K...)
        else
            deps[p] = remote_call(d.pmap[pmap[p]], ref, d, K...)
        end
    end
    j = 1
    for p = 1:np
        if dist[p] > II[j]; continue; end
        lower = j
        while j <= n && II[j] < dist[p+1]
            j += 1
        end
        J = [ i==d.distdim ? perm[lower:(j-1)] : (1:length(I[i])) for i=1:ndims(d) ]
        A[J...] = fetch(deps[p])
    end
    return A
end

# combinations of vector and scalar indexes
ref(d::DArray, I::AbstractVector{Int}, j::Int) = d[I, [j]]
ref(d::DArray, i::Int, J::AbstractVector{Int}) = d[[i], J]

ref(d::DArray, I::Union(Int,AbstractVector{Int})...) =
    d[[isa(i,Int) ? [i] : i for i in I]...]

# Nd scalar assign
function assign_elt(d::DArray, v, sub::(Int...))
    p = locate(d, sub[d.distdim])
    if p==d.localpiece
        offs = d.dist[p]-1
        if offs > 0
            sub = ntuple(length(sub),
                         ind->(ind==d.distdim ? sub[ind]-offs : sub[ind]))
        end
        localize(d)[sub...] = v
    else
        sync_add(remote_call(d.pmap[p], assign_elt, d, v, sub))
    end
    d
end

assign(d::DArray, v, i::Int) = assign_elt(d, v, ind2sub(d.dims, i))
assign(d::DArray, v, i0::Int, I::Int...) = assign_elt(d, v, tuple(i0,I...))

#TODO: Fix this
assign(d::DArray, v) = error("distributed arrays of dimension 0 not supported")

# Nd assign, scalar fill case, with Range1 indexes
function assign(d::DArray, v, I::Range1{Int}...)
    (pmap, dist) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
        localize(d)[J...] = v
        return d
    end
    for p = 1:length(pmap)
        K = [ i==d.distdim ? (dist[p]:(dist[p+1]-1)) : I[i] for i=1:ndims(d) ]
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v, K...))
    end
    return d
end

# Nd assign, array copy case, with Range1 indexes
function assign(d::DArray, v::AbstractArray, I::Range1{Int}...)
    assign_shape_check(v, I...)
    (pmap, dist) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
        localize(d)[J...] = v
        return d
    end
    offs = I[d.distdim][1] - 1
    if ndims(v) != length(I)
        v = reshape(v, map(length, I))
    end
    for p = 1:length(pmap)
        J = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1))-offs :
                                               (1:length(I[i]))))
        K = ntuple(ndims(d),i->(i==d.distdim ? (dist[p]:(dist[p+1]-1)) :
                                               I[i]))
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, sub(v,J), K...))
    end
    return d
end

# Nd assign, scalar fill case, vector indexes
function assign(d::DArray, v, I::AbstractVector{Int}...)
    (pmap, dist, perm) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
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
        K = [ i==d.distdim ? II[lower:(j-1)] : I[i] for i=1:ndims(d) ]
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, v, K...))
    end
    return d
end

# Nd assign, array copy case, vector indexes
function assign(d::DArray, v::AbstractArray, I::AbstractVector{Int}...)
    assign_shape_check(v, I...)
    (pmap, dist, perm) = locate(d, I[d.distdim])
    if length(pmap) == 1 && pmap[1] == d.localpiece
        offs = d.dist[pmap[1]]-1
        J = [ i == d.distdim ? I[i]-offs : I[i] for i=1:ndims(d) ]
        localize(d)[J...] = v
        return d
    end
    n = length(perm)
    j = 1
    II = I[d.distdim][perm] #the sorted indexes in the distributed dimension
    if ndims(v) != length(I)
        v = reshape(v, map(length, I))
    end
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
        sync_add(remote_call(d.pmap[pmap[p]], assign, d, sub(v,J), K...))
    end
    return d
end

# assign with combinations of Range1 and scalar indexes
assign(d::DArray, v, I::Union(Int,Range1{Int})...) =
    assign(d, v, [isa(i,Int) ? (i:i) : i for i in I]...)

# assign with combinations of vector and scalar indexes
assign(d::DArray, v, I::Union(Int,AbstractVector{Int})...) =
    assign(d, v, [isa(i,Int) ? [i] : i for i in I]...)

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

function .^{T}(A::Integer, B::SubOrDArray{T})
    S = promote_type(typeof(A),T)
    darray((T,lsz,da)->.^(A, localize(B, da)),
           S, size(B), distdim(B), procs(B))
end
function .^{T}(A::SubOrDArray{T}, B::Integer)
    S = promote_type(T,typeof(B))
    darray((T,lsz,da)->.^(localize(A, da), B),
           S, size(A), distdim(A), procs(A))
end

function .^{T<:Integer}(A::Integer, B::SubOrDArray{T})
    darray((T,lsz,da)->.^(A, localize(B, da)),
           Float64, size(B), distdim(B), procs(B))
end
function .^{T<:Integer}(A::SubOrDArray{T}, B::Integer)
    S = B < 0 ? Float64 : promote_type(T,typeof(B))
    darray((T,lsz,da)->.^(localize(A, da), B),
           S, size(A), distdim(A), procs(A))
end

for f in (:+, :-, :.*, :./, :.^, :&, :|, :$)
    @eval begin
        function ($f){T}(A::Number, B::SubOrDArray{T})
            S = eltype(($f)([one(A)],[one(T)]))
            darray((T,lsz,da)->($f)(A, localize(B, da)),
                   S, size(B), distdim(B), procs(B))
        end
        function ($f){T}(A::SubOrDArray{T}, B::Number)
            S = eltype(($f)([one(T)],[one(B)]))
            darray((T,lsz,da)->($f)(localize(A, da), B),
                   S, size(A), distdim(A), procs(A))
        end
        function ($f){T,S}(A::SubOrDArray{T}, B::SubOrDArray{S})
            if size(A) != size(B)
                error("argument dimensions must match")
            end
            R = eltype(($f)([one(T)],[one(S)]))
            darray((T,lsz,da)->($f)(localize(A, da), localize(B, da)),
                   R, size(A), distdim(A), procs(A))
        end
    end # eval
end # for

-(A::SubOrDArray) = darray(-, A)
~(A::SubOrDArray) = darray(~, A)
conj(A::SubOrDArray) = darray(conj, A)

for f in (:real, :imag)
    @eval begin
        function ($f){T}(A::SubOrDArray{T})
            S = typeof(($f)(zero(T)))
            darray((T,lsz,da)->($f)(localize(A, da)),
                   S, size(A), distdim(A), procs(A))
        end
    end # eval
end # for

function map(f, A::SubOrDArray)
    T = typeof(f(A[1]))
    darray((T,lsz,da)->map!(f, Array(T,lsz), localize(A, da)),
           T, size(A), distdim(A), procs(A))
end

# map a function that is already vectorized
function map_vectorized(f, A::SubOrDArray)
    T = typeof(f(A[1]))
    darray((T,lsz,da)->f(localize(A, da)),
           T, size(A), distdim(A), procs(A))
end

for f = (:ceil,   :floor,  :trunc,  :round,
         :iceil,  :ifloor, :itrunc, :iround,
         :abs,    :angle,  :log10,
         :sqrt,   :cbrt,   :log,    :log2,   :exp,   :expm1,
         :sin,    :cos,    :tan,    :cot,    :sec,   :csc,
         :sinh,   :cosh,   :tanh,   :coth,   :sech,  :csch,
         :asin,   :acos,   :atan,   :acot,   :asec,  :acsc,
         :acoth,  :asech,  :acsch,  :sinc,   :cosc)
    @eval ($f)(A::SubOrDArray) = map_vectorized($f, A)
end

for (f,t) in ((:(.==),:Number), (:.!=,:Number), (:.<,:Real), (:.<=,:Real))
    @eval begin
        function ($f)(A::($t), B::SubOrDArray)
            darray((T,lsz,da)->($f)(A, localize(B, da)),
                   Bool, size(B), distdim(B), procs(B))
        end
        function ($f)(A::SubOrDArray, B::($t))
            darray((T,lsz,da)->($f)(localize(A, da), B),
                   Bool, size(A), distdim(A), procs(A))
        end
        function ($f)(A::SubOrDArray, B::SubOrDArray)
            if size(A) != size(B)
                error("argument dimensions must match")
            end
            darray((T,lsz,da)->($f)(localize(A, da), localize(B, da)),
                   Bool, size(A), distdim(A), procs(A))
        end
    end # eval
end # for

function (==)(A::DArray, B::DArray)
    if size(A) != size(B)
        return false
    end
    mapreduce(fetch, all,
              { @spawnat p localize(A)==localize(B,A) for p in procs(A) })
end

function (!=)(A::DArray, B::DArray)
    if size(A) != size(B)
        return true
    end
    mapreduce(fetch, any,
              { @spawnat p localize(A)!=localize(B,A) for p in procs(A) })
end

function reduce(f, v::DArray)
    mapreduce(fetch, f,
              { @spawnat p reduce(f,localize(v)) for p = procs(v) })
end

function mapreduce(f, op, v::DArray)
    mapreduce(fetch, op,
              { @spawnat p mapreduce(f,op,localize(v)) for p = procs(v) })
end

sum(d::DArray) = reduce(+, d)
prod(d::DArray) = reduce(*, d)
min(d::DArray) = reduce(min, d)
max(d::DArray) = reduce(max, d)

reducedim(f::Function, d::DArray, r, v0, T::Type) = error("not yet implemented")
cumsum(d::DArray) = error("not yet implemented")
cumprod(d::DArray) = error("not yet implemented")

function sum{T}(A::DArray{T}, d::Int)
    if d < 1
        throw(ArgumentError("invalid dimension"))
    end
    if d > ndims(A)
        return A
    end
    S = typeof(zero(T)+zero(T))
    sz = ntuple(ndims(A), i->(i==d ? 1 : size(A,i)))
    if d == distdim(A)
        darray(S, sz, distdim(A)) do T,lsz,da
            mapreduce(fetch, +,
                      {@spawnat p sum(localize(A),d) for p in procs(A)})
        end
    else
        darray((T,lsz,da)->sum(localize(A),d), S, sz, distdim(A), procs(A))
    end
end

function each_vec{T}(f::Function, A::DArray{T,2}, d::Int)
    if !(d==1 || d==2)
        throw(ArgumentError("invalid dimension"))
    end
    if d == distdim(A)
        error("not yet implemented")
    end
    darray(eltype(A), size(A), distdim(A)) do T,lsz,da
        each_vec(f, localize(A), d)
    end
end

sort{T}(A::DArray{T,2}, d::Int) = each_vec(sort!, A, d)

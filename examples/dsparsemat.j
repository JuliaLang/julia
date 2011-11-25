type DSparseMat{T} <: AbstractArray{T,2}
    dims::(Size,Size)
    locl::SparseMatrixCSC{T}
    # the distributed array has N pieces
    # pmap[i]==p ⇒ processor p has piece i
    pmap::Array{Int32,1}
    # piece i consists of indexes dist[i] through dist[i+1]-1
    dist::Array{Size,1}
    localpiece::Int32  # my piece #; pmap[localpiece]==myid()
    go::GlobalObject

    function DSparseMat(go, initializer, dims, pmap, dist)
        mi = myid()
        lp = 0
        for i=1:length(pmap)
            if pmap[i]==mi
                lp=i; break
            end
        end

        mysz = lp==0 ? 0 : (dist[lp+1]-dist[lp])
        locsz = (dims[1], mysz)
        da = new()
        da.dims = dims
        da.pmap = pmap
        da.dist = dist
        da.localpiece = lp
        da.go = go
        da.locl = initializer(T, locsz, da)
        #remote_call(LocalProcess(), initializer, T, locsz, da)
        da
    end

    # don't use DSparseMat() directly; use dsparsemat() below instead
    function DSparseMat(initializer, dims, procs, dist)
        go = GlobalObject(g->DSparseMat{T}(g,initializer,dims,procs,dist))
        go.local_identity
    end

    function DSparseMat(initializer::Function, dims)
        procs = linspace(1, min(nprocs(),dims[2]))
        dist = defaultdist(dims, length(procs))
        DSparseMat{T}(initializer, dims, procs, dist)
    end
end

function show(d::DSparseMat)
    S = d.locl
    println(d.dims[1], "-by-", d.dims[2], " distributed sparse matrix with ", nnz(S), " local nonzeros:")
    for col = 1:S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            print("\t[")
            show(S.rowval[k])
            print(",\t", col, "]\t= ")
            show(S.nzval[k])
            println()
        end
    end
end

size(d::DSparseMat) = d.dims
procs(d::DSparseMat) = d.pmap

#serialize?
function serialize{T}(s, d::DSparseMat{T})
    i = worker_id_from_socket(s)
    if is(is_go_member(d.go,i), false)
        sz = size(d)
        emptylocl = SparseMatrixCSC{T}(sz[1],0,ones(Size,1),Array(Size,0),Array(T,0))
        invoke(serialize, (Any, Any),
               s,
               ccall(:jl_new_structt, Any, (Any, Any),
                     DSparseMat{T},
                     (sz, emptylocl, d.pmap, d.dist, 0, d.go)))
    else
        serialize(s, d.go)
    end
end

# compute balanced dist vector
function defaultdist(dims, np)
    sdd = dims[2]
    if sdd >= np
        linspace(1, sdd+1, np+1)
    else
        [[1:(sdd+1)], zeros(Size, np-sdd)]
    end
end

#??
function pieceindexes(d::DSparseMat, p)
    if p == 0
        return (1:size(d,1), 1:0)
    end
    (1:size(d,1), d.dist[p]:d.dist[p+1]-1)
end

#??
function myindexes(s::DSparseMat, locl)
    d = s.parent
    lo = d.dist[d.localpiece]
    hi = d.dist[d.localpiece+1]-1
    sdi = s.indexes[2]
    l = localize(d)
    if isa(sdi,Int)
        if lo <= sdi <= hi
            r = sdi
            if locl
                r -= (lo-1)
            end
        else
            return (1:size(s,1), 1:0)
        end
    else
        r = intersect(lo:hi, sdi)
        if locl
            r -= (lo-1)
        end
    end
    return (1:size(s,1), r)
end

# when we actually need the data, wait for it
#localize(r::RemoteRef) = localize(fetch(r))
localize(d::DSparseMat) = d.locl

# fetch the part of "src" that overlaps with the local part of "dest"
# equivalent to src[myindexes(dest)...]
function localize(src::DSparseMat, dest::DSparseMat)
    if size(src)==size(dest) && distdim(src)==distdim(dest) &&
       src.dist[src.localpiece]   == dest.dist[dest.localpiece] &&
       src.dist[src.localpiece+1] == dest.dist[dest.localpiece+1]
        return localize(src)
    end
    src[myindexes(dest)...]
end

# find which piece holds index i in the distributed dimension
function locate(d::DSparseMat, i::Index)
    p = 1
    while i >= d.dist[p+1]
        p += 1
    end
    p
end

# find which processor holds index i in the distributed dimension
owner(d::DSparseMat, i::Index) = d.pmap[locate(d, i)]

#find which pieces hold which subranges in distributed dimension
#returns (pmap,dist) where pmap[i] contains dist[i]:dist[i+1]-1
function locate(d::DSparseMat, I::Range1{Index})
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
function locate(d::DSparseMat, I::AbstractVector{Index})
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

# initializer is a function accepting (el_type, local_size, darray) where
# the last argument is the full DSparseMat being constructed.
dsparsemat{T}(init, ::Type{T}, dims::Dims, procs, dist) =
    DSparseMat{T}(init, dims, procs, dist)

function dsparsemat{T}(init, ::Type{T}, dims::Dims, procs)
    sdd = dims[2]
    np = length(procs)
    if sdd < np
        procs = procs[1:sdd]
    end
    dsparsemat(init, T, dims, procs,
           defaultdist(dims, length(procs)))
end

function dsparsemat{T}(init, ::Type{T}, dims::Dims)
    procs = linspace(1, min(nprocs(),dims[2]))
    dsparsemat(init, T, dims, procs,
           defaultdist(dims, length(procs)))
end

dsparsemat(init::Function, T::Type, dims::Size...) = dsparsemat(init, T, dims)
dsparsemat(init::Function, dims::Dims) = dsparsemat(init, Float64, dims)
dsparsemat(init::Function, dims::Size...) = dsparsemat(init, dims)

dsparsemat(T::Type, args...)     =
    dsparsemat((T,lsz,da)->SparseMatrixCSC{T}(lsz[1],lsz[2],ones(Size,lsz[2]+1),Array(Size,0),Array(T,0)), T, args...)
dsparsemat(dims::Dims, args...)  =
    dsparsemat((T,lsz,da)->SparseMatrixCSC{T}(lsz[1],lsz[2],ones(Size,lsz[2]+1),Array(Size,0),Array(T,0)), dims, args...)
dsparsemat(dims::Size...)        =
    dsparsemat((T,lsz,da)->SparseMatrixCSC{T}(lsz[1],lsz[2],ones(Size,lsz[2]+1),Array(Size,0),Array(T,0)), dims)

similar(d::DSparseMat, T::Type, dims::Dims) =
    dsparsemat((T,lsz,da)->SparseMatrixCSC{T}(lsz[1],lsz[2],ones(Size,lsz[2]+1),Array(Size,0),Array(T,0)), T, dims,
           2>length(dims) ? 2 : 2, d.pmap)

#copy
#copy_to?

dsparsezeros(args...)  = dsparsemat((T,lsz,da)->SparseMatrixCSC{T}(lsz[1],lsz[2],ones(Size,lsz[2]+1),Array(Size,0),Array(T,0)), args...)

#random matrix with bandwidth?
#zero?

function sparse_distribute{T}(a::Array{T})
    owner = myid()
    # create a remotely-visible reference to the array
    rr = RemoteRef()
    put(rr, a)
    dsparsemat((T,lsz,da)->_jl_sparse_distribute_one(T,lsz,da,owner,rr),
           T, size(a))
end

# fetch one processor's piece of an array being distributed
function _jl_sparse_distribute_one(T, lsz, da, owner, orig_array)
    if prod(lsz)==0
        return SparseMatrixCSC{T}(lsz[1],lsz[2],Array(Size,0),Array(Size,0),Array(Size,0))
    end
    p = da.localpiece
    i1 = da.dist[p]             # my first index in distdim
    iend = i1+lsz[2]-1    # my last  "
    # indexes of original array I will take
    idxs = {1:lsz[1], i1:iend}
    #idxs = { 1:lsz[i] | i=1:length(da.dims) }
    #idxs[2] = (i1:iend)
    return sparse(remote_call_fetch(owner, ref, orig_array, idxs...))
end

convert{T}(::Type{Array}, d::DSparseMat{T}) = convert(Array{T,2}, d)

function convert{S,T}(::Type{Array{S,2}}, d::DSparseMat{T})
    a = Array(S, size(d))
    idxs = { 1:size(a,i) | i=1:2 }
    for p = 1:length(d.dist)-1
        idxs[2] = d.dist[p]:(d.dist[p+1]-1)
        a[idxs...] = full(remote_call_fetch(d.pmap[p], localize, d))
    end
    a
end

#convert to SparseMatrixCSC{T}

#transpose
#ctranspose

## Indexing ##
ref{T}(d::DSparseMat{T}, i::Index) = ref(d, ind2sub(d.dims, i))
ref{T}(d::DSparseMat{T}, I::(Index,Index)) = ref(d, I[1], I[2])
function ref{T}(d::DSparseMat{T}, i0::Index, i1::Index)
    p = locate(d, i1)
    if p == d.localpiece
        offs = d.dist[p]-1
        sub2 = offs > 0 ? i1-offs : i1
        return localize(d)[i0, sub2]::T
    end
    return remote_call_fetch(d.pmap[p], ref, d, i0, i1)::T
end

assign{T,S,N}(d::DSparseMat{T}, v::AbstractArray{S,N}, i0::Index, i1::Index) =
    invoke(assign, (DSparseMat{T}, Any, Index, Index), d, v, i0, i1)  
assign{T,N}(d::DSparseMat{T}, v::AbstractArray{T,N}, i0::Index, i1::Index) =
    invoke(assign, (DSparseMat{T}, Any, Index, Index), d, v, i0, i1)  
assign{T,N}(d::DSparseMat{T}, v::AbstractArray{T,N}, i::Index) =
    invoke(assign, (DSparseMat{T}, Any, Index), d, v, i)
assign{T}(d::DSparseMat{T}, v, i::Index) = assign(d, v, ind2sub(d.dims, i))
assign{T}(d::DSparseMat{T}, v, I::(Index,Index)) = assign(d, v, I[1], I[2])
function assign{T}(d::DSparseMat{T}, v, i0::Index, i1::Index)
    p = locate(d, i1)
    if p==d.localpiece
        offs = d.dist[p]-1
            sub2 = offs > 0 ? i1-offs: i1
        localize(d)[i0, sub2] = v
    else
        remote_do(d.pmap[p], assign, d, v, i0, i1)
    end
    d
end
#operators
#*
#elementwise
#binary
#vectorized

#nnz
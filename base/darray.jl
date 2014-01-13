type DArray{T,N,A} <: AbstractArray{T,N}
    dimdist::DimDist{N}
    refs::DistRefs{N}    
    DArray(ad, r) = new(ad, r)
end


typealias SubDArray{T,N,D<:DArray} SubArray{T,N,D}
typealias SubOrDArray{T,N}         Union(DArray{T,N}, SubDArray{T,N})

## core constructors ##

function DArray(alloc_arg::Union(Type, Function), dimdist::DimDist; init=false, dprocs=workers())
    N = length(dims(dimdist))
    
    if isa(alloc_arg, Type)
        allocf = (idxs) -> Array(alloc_arg, map(length, idxs))
    else
        allocf = alloc_arg
    end
    
    refs = setup_chunks(allocf, dprocs, dimdist)

    p = max(1, localpartindex(refs))
    A = remotecall_fetch(procs(refs)[p], r->typeof(fetch(r)), refs[p])
    T = eltype(A)
    d = DArray{T,N,A}(dimdist, refs)

    # if present, init function is called on each of the parts
    @sync begin 
        if isa(init, Function)
            for i in procs(d)
                @async remotecall_wait(i, init, d)
            end
        end
    end
    d
end

function DArray(alloc_arg, dims; kwargs...)
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        dprocs = kwargs[idx][2]
        DArray(alloc_arg, DimDist(dims, length(dprocs)); kwargs...)
    else
        DArray(alloc_arg, DimDist(dims); kwargs...)
    end
end
function DArray(alloc_arg, dims, dist; kwargs...) 
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        dprocs = kwargs[idx][2]
        DArray(alloc_arg, DimDist(dims, length(dprocs), dist); kwargs...)
    else
        DArray(alloc_arg, DimDist(dims, length(workers()), dist); kwargs...)
    end
end

# new DArray similar to an existing one, ensure length of dprocs is same as original if present
function DArray{T}(d::DArray{T}; kwargs...) 
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        dprocs = kwargs[idx][2]
        if length(procs(d)) != length(dprocs)
            error("Requested number of workers must be same as existing DArray")
        end
        DArray(T, DimDist(size(d), length(dprocs), pdims(d.dimdist)); kwargs...)
    else
        append!(kwargs, [(:dprocs, procs(d))])
        DArray(T, DimDist(size(d), length(procs(d)), pdims(d.dimdist)); kwargs...)
    end
end

length(d::DArray) = prod(dims(d.dimdist))
size(d::DArray) = dims(d.dimdist)
procs(d::DArray) = procs(d.refs)

chunktype{T,N,A}(d::DArray{T,N,A}) = A

localpartindex(d::DArray) = localpartindex(d.refs)

function localpart{T,N,A}(d::DArray{T,N,A})
    lpidx = localpartindex(d.refs)
    if lpidx == 0
        convert(A, Array(T, ntuple(N,i->0)))::A
    else
        fetch(d.refs[lpidx])::A
    end
end


function myindexes(d::DArray) 
    lpidx = localpartindex(d.refs)
    if lpidx == 0
        ntuple(N, i->1:0)
    else
        (d.dimdist[lpidx])[1]
    end
end

locate(d::DArray, I::Int...) = locate(d.dimdist, I...)

chunk{T,N,A}(d::DArray{T,N,A}, i...) = fetch(d.refs[i...])::A



## conversions ##

convert{T,N}(::Type{Array}, d::SubOrDArray{T,N}) = convert(Array{T,N}, d)

function convert{S,T,N}(::Type{Array{S,N}}, d::DArray{T,N})
    a = Array(S, size(d))
    @sync begin
        for i = 1:length(d.dimdist)
            @async a[(d.dimdist[i])...] = chunk(d, i)
        end
    end
    a
end

function convert{S,T,N}(::Type{Array{S,N}}, s::SubDArray{T,N})
    I = s.indexes
    d = s.parent
    if isa(I,(Range1{Int}...)) && S<:T && T<:S
        l = locate(d, map(first, I)...)
        if isequal(d.dimdist[l...], I)
            # SubDArray corresponds to a chunk
            return chunk(d, l...)
        end
    end
    a = Array(S, size(s))
    a[[1:size(a,i) for i=1:N]...] = s
    a
end

function reshape{T,S<:Array}(A::DArray{T,1,S}, d::Dims)
    if prod(d) != length(A)
        error("dimensions must be consistent with array size")
    end
    DArray(d) do I
        sz = map(length,I)
        d1offs = first(I[1])
        nd = length(I)

        B = Array(T,sz)
        nr = size(B,1)
        sztail = size(B)[2:end]

        for i=1:div(length(B),nr)
            i2 = ind2sub(sztail, i)
            globalidx = [ I[j][i2[j-1]] for j=2:nd ]
            
            a = sub2ind(d, d1offs, globalidx...)
            
            B[:,i] = A[a:(a+nr-1)]
        end
        B
    end
end

## indexing ##

function getchunk(r::RemoteRef, args...)
    if r.where==myid()
        getindex(fetch(r), args...)
    else
        remotecall_fetch(r.where, getchunk, r, args...)
    end
end


getindex(d::DArray, i::Int) = getindex(d, ind2sub(size(d), i))
getindex(d::DArray, i::Int...) = getindex(d, sub2ind(size(d), i...))

function getindex{T, N}(d::DArray{T,N}, I::(Int...))
    chidx = locate(d.dimdist, I...)
    idxs = d.dimdist[chidx...]
    chunk_rr = d.refs[chidx...]
    localidx = ntuple(N, i->(I[i]-first(idxs[i])+1))
    
    getchunk(chunk_rr, localidx...)::T
end

getindex(d::DArray) = d[1]
getindex(d::DArray, I::Union(Int,Range1{Int})...) = sub(d,I)

copy(d::SubOrDArray) = d

# local copies are obtained by convert(Array, ) or assigning from
# a SubDArray to a local Array.

function setindex!(a::Array, d::DArray, I::Range1{Int}...)
    n = length(I)
    @sync begin
        for i = 1:length(d.dimdist)
            K = d.dimdist[i]
            @async a[[I[j][K[j]] for j=1:n]...] = chunk(d, i)
        end
    end
    a
end

function setindex!(a::Array, s::SubDArray, I::Range1{Int}...)
    n = length(I)
    d = s.parent
    J = s.indexes
    if length(J) < n
        a[I...] = convert(Array,s)
        return a
    end
    offs = [isa(J[i],Int) ? J[i]-1 : first(J[i])-1 for i=1:n]
    @sync begin
        for i = 1:length(d.dimdist)
            K_c = {d.dimdist[i]...}
            K = [ intersect(J[j],K_c[j]) for j=1:n ]
            if !any(isempty, K)
                idxs = [ I[j][K[j]-offs[j]] for j=1:n ]
                if isequal(K, K_c)
                    # whole chunk
                    @async a[idxs...] = chunk(d, i)
                else
                    # partial chunk
                    ch = d.refs[i]
                    @async a[idxs...] = remotecall_fetch(ch.where, ()->sub(fetch(ch), [K[j]-first(K_c[j])+1 for j=1:n]...))
                end
            end
        end
    end
    a
end

# to disambiguate
setindex!(a::Array{Any}, d::SubOrDArray, i::Int) = arrayset(a, d, i)

setindex!(a::Array, d::SubOrDArray, I::Union(Int,Range1{Int})...) =
    setindex!(a, d, [isa(i,Int) ? (i:i) : i for i in I ]...)

## higher-order functions ##

map(f::Callable, d::DArray) = DArray(I->map(f, localpart(d)), d)

reduce(f::Function, d::DArray) =
    mapreduce(fetch, f,
              { @spawnat p reduce(f, localpart(d)) for p in procs(d) })

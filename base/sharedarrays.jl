module SharedArrays

import Base: eltype, mmap_array, myindexes, ndims, serialize, size
using Base.FS

export AbstractSharedArray, SharedArray, cutdim, cutdim!, myarray, nchunks, nrefs, pcall, pcall_bw, proc, sharedsync

abstract AbstractSharedArray{T,N} <: AbstractArray{T,N}

# A SharedArray should live only on process id 1, everyone else
# sees a SSharedArray
# SSharedArray = Serializable shared array

type SSharedArray{T,N} <: AbstractSharedArray{T,N}
    dims::NTuple{N,Int}
    refs::Array{RemoteRef,1}
    cutdim::Int
end

type SharedArray{T,N} <: AbstractSharedArray{T,N}
    data::Array{T,N}   # the array on process id 1
    sarray::SSharedArray{T,N}
end

function SharedArray{T}(::Type{T}, dims::Dims, procs = procs(); filename::ASCIIString = "/julia.base.sharedarray." * string(getpid()), mode::ASCIIString = "", cutdim::Integer = defaultcutdim(dims, length(procs)))
    shm_unlink(filename) # Delete if found

    data = shm_mmap_array(T, dims, filename)
    refs = Array(RemoteRef, 0)
    n = prod(dims)
    for p in procs
        if p != 1
            push!(refs, remotecall_wait(p, shm_mmap_array, T, dims, filename))
        end
    end
    
    shm_unlink(filename)  # so the file gets cleaned up automatically
    S = SharedArray{T,length(dims)}(data, SSharedArray{T,length(dims)}(dims, refs, int(cutdim)))
end

function sharedsync(procs = 1:nprocs())
    n = length(setdiff(procs, 1))
    SharedArray(Bool, (n,), procs)
end

serialize(io, S::SharedArray) = serialize(io, S.sarray)


eltype{T}(S::AbstractSharedArray{T}) = T
ndims{T,N}(S::AbstractSharedArray{T,N}) = N
size(S::SharedArray) = size(S.data)
size(S::SharedArray, i::Integer) = size(S.data, i)
size(S::SSharedArray) = S.dims

nchunks(S::SharedArray) = nchunks(S.sarray)
nchunks(S::SSharedArray) = S.cutdim == 0 ? 1 : length(S.refs)+1

nrefs(S::SharedArray) = length(S.sarray.refs)

proc(S::SharedArray, ind::Integer) = S.sarray.refs[ind].where

cutdim(S::SharedArray) = cutdim(S.sarray)
cutdim(S::SSharedArray) = S.cutdim
cutdim!(S::SharedArray, dim::Integer) = S.sarray.cutdim = dim

myindex(S::SharedArray) = 0

function myindex(S::SSharedArray)
    id = myid()
    if id == 1
        return 0  # generally this shouldn't occur, since myindex(SharedArray) should cover this
    end
    for i = 1:length(S.refs)
        if id == S.refs[i].where
            return i
        end
    end
    -1
end

myarray(S::SharedArray) = S.data

function myarray{T,N}(S::SSharedArray{T,N})
    ind = myindex(S)
    if ind == 0
        return S.data
    elseif ind > 0
        return fetch(S.refs[ind])::Array{T,N}
    end
    error("No local version of SharedArray")
end

function myindexes(S::SharedArray)
    cutdim = S.sarray.cutdim
    if cutdim == 0
        return ntuple(ndims(S), i->1:size(S, i))
    end
    n = size(S, cutdim)
    k = nchunks(S)
    ntuple(ndims(S), i->(i == cutdim) ? (1:iround(n/k)) : (1:size(S, i)) )
end

function myindexes(S::SSharedArray)
    ind = myindex(S)
    if ind < 0
        return ntuple(ndims(S), i->1:0)
    end
    if S.cutdim == 0
        return ntuple(ndims(S), i->1:S.dims[i])
    end
    n = S.dims[S.cutdim]
    k = nchunks(S)
    ntuple(ndims(S), i->(i == S.cutdim) ? (iround(ind*n/k)+1:iround((ind+1)*n/k)) : (1:S.dims[i]) )
end

function pcall(f::Function, args...)
    for i = 1:length(args)
        if isa(args[i], SharedArray)
            return pcall_arg(f, i, args...)
        end
    end
    f(args...), Array(RemoteRef, 0)
end

function pcall_arg(f::Function, index::Integer, args...)
    S = args[index]
    r = Array(RemoteRef, nrefs(S))
    for i = 1:nrefs(S)
        r[i] = remotecall(proc(S, i), f, args...)
    end
    ret = f(args...)
    for i = 1:nrefs(S)
        wait(r[i])
    end
    ret, r
end

# A busy-wait version of pcall
# done should be created with sharedsync
function pcall_bw(f::Function, done::SharedArray, args...)
    n = nrefs(done)
    fill!(done, false)
    r = Array(RemoteRef, n)
    for i = 1:n
        r[i] = remotecall(proc(done, i), pcall_bw, f, done, args...)
    end
    ret = f(args...)
    while !all(done) end
    ret, r
end

function pcall_bw(f::Function, done::SSharedArray, args...)
    ret = f(args...)
    bw = myarray(done)
    bw[myindex(done)] = true
    ret
end

getindex(S::SharedArray, ind::Real) = getindex(S.data, ind)
getindex(S::SharedArray, I::AbstractArray) = getindex(S.data, I)
setindex!(S::SharedArray, val, ind::Real) = setindex!(S.data, val, ind)
getindex(S::SharedArray, ind...) = getindex(S.data, ind...)
setindex!(S::SharedArray, val, ind...) = setindex!(S.data, val, ind...)

# Allow myarray and myindexes to work on arbitrary AbstractArrays,
# so that algorithms may work for either SharedArrays or other array types
myarray(A::AbstractArray) = A
myindexes(A::AbstractArray) = ntuple(ndims(A), i->1:size(A,i))

# Utilities
function shm_mmap_array(T, dims, filename::String)
    mode = (1 == myid() ? JL_O_CREAT | JL_O_RDWR : JL_O_RDWR)
    
    fd_mem = ccall(:shm_open, Int, (Ptr{Uint8}, Int, Int), filename, mode, S_IRUSR | S_IWUSR)
    if !(fd_mem > 0) error("shm_open() failed") end

    s = fdio(fd_mem, true)
    mmap_array(T, dims, s, 0)
end

@unix_only shm_unlink(filename) = ccall(:shm_unlink, Cint, (Ptr{Uint8},), filename)
@unix_only shm_open(filename, oflags, permissions) = ccall(:shm_open, Int, (Ptr{Uint8}, Int, Int), filename, oflags, permissions)

function defaultcutdim(dims, n)
    d = length(dims)
    while d > 0 && n <= dims[d]
        d -= 1
    end
    if d == 0
        d = indmax(dims)
    end
    d
end


end  # module

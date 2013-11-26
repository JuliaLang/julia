type SharedArray{T,N,A} <: AbstractArray{T,N}
    dims::NTuple{N,Int}
    ad::ArrayDist{N}
    
    # Local shmem map. Not to be serialized.
    local_shmmap::Array{T,N}
    
    # local index into chunks when this SharedArray is serialized onto a different node, 0 if non-existent locally.
    local_idx::Int
end

function SharedArray(T::Type, dims, dprocs, dist; init=false)
    N = length(dims)

    @windows_only error(" SharedArray is not available on Windows yet.")
    
    # Ensure that all processes are on localhost
    if !(all(x->islocalwrkr(x), [dprocs, myid()]))
        error("SharedArray requires all requested processes to be on the same machine.")
    end

    local shm_seg_name = ""
    local local_shmmap 
    local sa = nothing 
    try
        # On OSX, the shm_seg_name length must be < 32 characters
        shm_seg_name = string("/jl", getpid(), int64(time() * 10^9)) 

        local_shmmap = shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR)
    
        func_alloc = (idxs) -> begin 
            basemap = shm_mmap_array(T, dims, shm_seg_name, JL_O_RDWR)
            sub(basemap, idxs)
        end
        
        ad = ArrayDist(func_alloc, dims, dprocs, dist)
        
        # Wait till all the workers have mapped the segment
        for i in 1:length(ad)
            wait(chunk_ref(ad, i))
        end
        
        # All good, immediately unlink the segment.
        shm_unlink(shm_seg_name) 
        shm_seg_name = "" ;
        
        
        # get the typeof the chunks 
        A = typeof(sub(Array(T, ntuple(N,i->0)), ntuple(N, i->1:0)))
        sa = SharedArray{T,N,A}(dims, ad, local_shmmap, localpartindex(ad))

        # if present init function is called on each of the parts
        @sync begin 
            if isa(init, Function)
                for i in procs(sa)
                    @async remotecall_wait(i, init, sa)
                end
            end
        end
        
    finally
        if shm_seg_name != "" 
            shm_unlink(shm_seg_name) 
        end
    end
    sa
end


function SharedArray(T, dims, procs; kwargs...)
    if isempty(procs)
        error("no processors")
    end
    SharedArray(T, dims, procs, defaultdist(dims,procs); kwargs...)
end
SharedArray(T, dims; kwargs...) = SharedArray(T, dims, workers()[1:min(nworkers(),maximum(dims))]; kwargs...)

# new DArray similar to an existing one
SharedArray{T}(sa::SharedArray{T}; kwargs...) = SharedArray(T, size(sa), procs(sa), [dimdist(sa.ad)...]; kwargs...)

length(sa::SharedArray) = prod(sa.dims)
size(sa::SharedArray) = sa.dims
procs(sa::SharedArray) = procs(sa.ad)

chunktype{T,N,A}(sa::SharedArray{T,N,A}) = A

localpartindex(sa::SharedArray) = localpartindex(sa.ad)

function localpart{T,N,A}(sa::SharedArray{T,N,A})
    if sa.local_idx == 0
        sub(Array(T, ntuple(N,i->0)), ntuple(N, i->1:0))
    else
        fetch(chunk_ref(sa.ad, sa.local_idx))::A
    end
end
myindexes(sa::SharedArray) = myindexes(sa.ad)
locate(sa::SharedArray, I::Int...) = locate(sa.ad, I...)

## convenience constructors ##

sharedzeros(args...) = SharedArray(Float64, args...; init = S->fill!(localpart(S), 0.0))
sharedzeros(d::Int...) = sharedzeros(d)
sharedones(args...) = SharedArray(Float64, args...; init = S->fill!(localpart(S), 1.0))
sharedones(d::Int...) = sharedones(d)
sharedfill(v, args...) = SharedArray(typeof(v), args...; init = S->fill!(localpart(S), v))
sharedfill(v, d::Int...) = sharedfill(v, d)
sharedrand(args...)  = SharedArray(Float64, args...; init = S->fill!(localpart(S), rand()))
sharedrand(d::Int...) = sharedrand(d)
sharedrandn(args...) = SharedArray(Float64, args...; init = S->fill!(localpart(S), randn()))
sharedrandn(d::Int...) = sharedrandn(d)
## conversions ##


function share{T}(a::AbstractArray{T})
    sa = SharedArray(T, size(a))
    copy!(sa.local_shmmap, a)
    sa
end


# Don't serialize local_shmmap (it is the complete array) and 
# local_idx, which is relevant to the current process only
function serialize(s, sa::SharedArray)
    serialize_type(s, typeof(sa))
    serialize(s, length(SharedArray.names)) 
    for n in SharedArray.names
        if n == :local_shmmap
            writetag(s, UndefRefTag)
        else
            serialize(s, getfield(sa, n)) 
        end
    end
end

function deserialize{T,N,A}(s, t::Type{SharedArray{T,N,A}})
    sa = invoke(deserialize, (Any, DataType), s, t)
    
    sa.local_idx = localpartindex(sa)
    if (sa.local_idx > 0) 
        sa.local_shmmap = parent(fetch(chunk_ref(sa.ad, sa.local_idx)))
    else
        error("SharedArray cannot be used on a non-participating process")
    end
    sa
end

localpartindex(sa::SharedArray) = localpartindex(sa.ad)

convert(::Type{Array}, sa::SharedArray) = sa.local_shmmap

# avoiding ambiguity warnings
getindex(sa::SharedArray, x::Real) = getindex(sa.local_shmmap, x)
getindex(sa::SharedArray, x::AbstractArray) = getindex(sa.local_shmmap, x)

# pass through getindex and setindex! - they always work on the complete array unlike DArrays
getindex(sa::SharedArray, args...) = getindex(sa.local_shmmap, args...)
setindex!(sa::SharedArray, args...) = (setindex!(sa.local_shmmap, args...); sa)


# utilities
function shm_mmap_array(T, dims, shm_seg_name, mode)
    local s = nothing
    local A = nothing 
    try
        fd_mem = shm_open(shm_seg_name, mode, S_IRUSR | S_IWUSR)
        if !(fd_mem > 0) 
            error("shm_open() failed") 
        end

        s = fdio(fd_mem, true)
        
        # On OSX, ftruncate must to used to set size of segment, just lseek does not work.
        # and only at creation time
        if (mode & JL_O_CREAT) == JL_O_CREAT
            rc = ccall(:ftruncate, Int, (Int, Int), fd_mem, prod(dims)*sizeof(T))
            if rc != 0
                ec = errno()
                error("ftruncate() failed, errno : ", ec) 
            end
        end
        
        A = mmap_array(T, dims, s, 0, grow=false)
    catch e
        @linux_only pfx = "kernel"
        @osx_only pfx = "kern.sysv"
        
        shmmax_MB = div(int(split(readall(readsfrom(`sysctl $(pfx).shmmax`)[1]))[end]), 1024*1024)
        page_size = int(split(readall(readsfrom(`getconf PAGE_SIZE`)[1]))[end])
        shmall_MB = div(int(split(readall(readsfrom(`sysctl $(pfx).shmall`)[1]))[end]) * page_size, 1024*1024)
        
        println("System max size of single shmem segment(MB) : ", shmmax_MB, 
            "\nSystem max size of all shmem segments(MB) : ", shmall_MB,
            "\nRequested size(MB) : ", div(prod(dims)*sizeof(T), 1024*1024),
            "\nPlease ensure requested size is within system limits.",
            "\nIf not, increase system limits and try again."
        )
        
        rethrow()
        
    finally
        if s != nothing 
            close(s)
        end
    end
    A
end

@unix_only shm_unlink(shm_seg_name) = ccall(:shm_unlink, Cint, (Ptr{Uint8},), shm_seg_name)
@unix_only shm_open(shm_seg_name, oflags, permissions) = ccall(:shm_open, Int, (Ptr{Uint8}, Int, Int), shm_seg_name, oflags, permissions)


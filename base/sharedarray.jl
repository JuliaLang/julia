type SharedArray{T,N} <: AbstractArray{T,N}
    arrdist::ArrayDist
    refs::DistRefs{N}    
    
    # Local shmem map. Not to be serialized.
    local_shmmap::Array{T,N}
    
    # local index into chunks when this SharedArray is serialized onto a different node, 0 if non-existent locally.
    local_idx::Int
end

function SharedArray(T::Type, arrdist::ArrayDist; init=false, dprocs=workers())
    N = length(dims(arrdist))

    !isbits(T) ? error("Type of Shared Array elements must be bits types") : nothing
    @windows_only error(" SharedArray is not supported on Windows yet.")
    
    onlocalhost = assert_same_host(dprocs)

    local shm_seg_name = ""
    local local_shmmap 
    local sa = nothing 
    local shmmem_create_pid
    try
        # On OSX, the shm_seg_name length must be < 32 characters
        shm_seg_name = string("/jl", getpid(), int64(time() * 10^9)) 
        if onlocalhost
            shmmem_create_pid = myid()
            local_shmmap = shm_mmap_array(T, dims(arrdist), shm_seg_name, JL_O_CREAT | JL_O_RDWR)
        else
            # The shared array is being created on a remote machine....
            shmmem_create_pid = dprocs[1]
            remotecall(dprocs[1], () -> begin shm_mmap_array(T, dims(arrdist), shm_seg_name, JL_O_CREAT | JL_O_RDWR); nothing end) 
        end

        func_alloc = (idxs) -> begin 
            basemap = shm_mmap_array(T, dims(arrdist), shm_seg_name, JL_O_RDWR)
            sub(basemap, idxs)
        end
        
        refs = setup_chunks(func_alloc, dprocs, arrdist)
        
        # Wait till all the workers have mapped the segment
        for i in 1:length(refs)
            wait(refs[i])
        end
        
        # All good, immediately unlink the segment.
        remotecall(shmmem_create_pid, () -> begin shm_unlink(shm_seg_name); nothing end)  
        shm_seg_name = "" 
        
        if onlocalhost
            sa = SharedArray{T,N}(arrdist, refs, local_shmmap, localpartindex(refs))
        else
            sa = SharedArray{T,N}(arrdist, refs)
            sa.local_idx = 0 
        end
        
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
            remotecall(shmmem_create_pid, () -> begin shm_unlink(shm_seg_name); nothing end)  
        end
    end
    sa
end


function SharedArray(T, dims; kwargs...) 
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        SharedArray(T, DimDist(dims, length(kwargs[idx][2]); mode=DISTMODE_SHARED); kwargs...)
    else
        SharedArray(T, DimDist(dims, length(workers()); mode=DISTMODE_SHARED); kwargs...)
    end
end

function SharedArray(T, dims, dist; kwargs...) 
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        SharedArray(T, DimDist(dims, length(kwargs[idx][2]), dist; mode=DISTMODE_SHARED); kwargs...)
    else
        SharedArray(T, DimDist(dims, length(workers()), dist; mode=DISTMODE_SHARED); kwargs...)
    end
end

# new SharedArray similar to an existing one
function SharedArray{T}(sa::SharedArray{T}; kwargs...) 
    idx = findfirst(Arg -> begin (S,_) = Arg; S == :dprocs end, kwargs)
    if idx > 0
        dprocs = kwargs[idx][2]
        if length(procs(d)) != length(dprocs)
            error("Requested number of workers must be same as existing SharedArray")
        end
        SharedArray(T, size(sa), length(dprocs), pdims(sa.arrdist); kwargs...) 
    else
        append!(kwargs, [(:dprocs, procs(d))])
        SharedArray(T, size(sa), length(dprocs), length(procs(d)), pdims(sa.arrdist); kwargs...) 
    end
end


length(sa::SharedArray) = prod(dims(sa.arrdist))
size(sa::SharedArray) = dims(sa.arrdist)
procs(sa::SharedArray) = procs(sa.refs)

localpartindex(sa::SharedArray) = localpartindex(sa.refs)

function localpart{T,N}(sa::SharedArray{T,N})
    if sa.local_idx == 0
        sub(Array(T, ntuple(N,i->0)), ntuple(N, i->1:0))
    else
        fetch(sa.refs[sa.local_idx])
    end
end
function myindexes(sa::SharedArray) 
    lpidx = localpartindex(sa.refs)
    if lpidx == 0
        ntuple(N, i->1:0)
    else
        d.dimdist[lpidx]
    end

end

locate(sa::SharedArray, I::Int...) = locate(sa.arrdist, I...)



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

function deserialize{T,N}(s, t::Type{SharedArray{T,N}})
    sa = invoke(deserialize, (Any, DataType), s, t)
    
    sa.local_idx = localpartindex(sa)
    if (sa.local_idx > 0) 
        sa.local_shmmap = parent(fetch(sa.refs[sa.local_idx]))
    else
        error("SharedArray cannot be used on a non-participating process")
    end
    sa
end

localpartindex(sa::SharedArray) = localpartindex(sa.refs)

convert(::Type{Array}, sa::SharedArray) = sa.local_shmmap

# avoiding ambiguity warnings
getindex(sa::SharedArray, x::Real) = getindex(sa.local_shmmap, x)
getindex(sa::SharedArray, x::AbstractArray) = getindex(sa.local_shmmap, x)

# pass through getindex and setindex! - they always work on the complete array unlike DArrays
getindex(sa::SharedArray, args...) = getindex(sa.local_shmmap, args...)
setindex!(sa::SharedArray, args...) = (setindex!(sa.local_shmmap, args...); sa)

function print_shmem_limits(slen)
    try
        @linux_only pfx = "kernel"
        @osx_only pfx = "kern.sysv"

        shmmax_MB = div(int(split(readall(readsfrom(`sysctl $(pfx).shmmax`)[1]))[end]), 1024*1024)
        page_size = int(split(readall(readsfrom(`getconf PAGE_SIZE`)[1]))[end])
        shmall_MB = div(int(split(readall(readsfrom(`sysctl $(pfx).shmall`)[1]))[end]) * page_size, 1024*1024)
        
        println("System max size of single shmem segment(MB) : ", shmmax_MB, 
            "\nSystem max size of all shmem segments(MB) : ", shmall_MB,
            "\nRequested size(MB) : ", div(slen, 1024*1024),
            "\nPlease ensure requested size is within system limits.",
            "\nIf not, increase system limits and try again."
        )
    catch e
        nothing # Ignore any errors in this... 
    end
end

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
        print_shmem_limits(prod(dims)*sizeof(T))
        rethrow(e)
        
    finally
        if s != nothing 
            close(s)
        end
    end
    A
end

@unix_only shm_unlink(shm_seg_name) = ccall(:shm_unlink, Cint, (Ptr{Uint8},), shm_seg_name)
@unix_only shm_open(shm_seg_name, oflags, permissions) = ccall(:shm_open, Int, (Ptr{Uint8}, Int, Int), shm_seg_name, oflags, permissions)


function assert_same_host(procs)
    myip = 
    resp = Array(Any, length(procs))
    
    @sync begin
        for (i, p) in enumerate(procs)
            @async resp[i] = remotecall_fetch(p, () -> getipaddr())
        end
    end
    
    if !all(x->x==resp[1], resp) 
        error("SharedArray requires all requested processes to be on the same machine.")
    end
    
    return (resp[1] != getipaddr()) ? false : true
end

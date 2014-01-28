type SharedArray{T,N} <: AbstractArray{T,N}
    dims::NTuple{N,Int}
    pids::Vector{Int}
    refs::Array{RemoteRef}
    
    # Fields below are not to be serialized
    # Local shmem map. 
    loc_shmarr::Array{T,N}
    
    # idx of current workers pid into the pids vector, 0 if this shared array is not mapped locally.
    loc_pididx::Int
    
    # the local partition into the array when viewed as a single dimensional array.
    loc_subarr_1d
    
    SharedArray(d,p,r) = new(d,p,r)
end

function SharedArray(T::Type, dims::NTuple; init=false, pids=workers())
    N = length(dims)

    !isbits(T) ? error("Type of Shared Array elements must be bits types") : nothing
    @windows_only error(" SharedArray is not supported on Windows yet.")
    
    len_sa = prod(dims)
    if length(pids) > len_sa
        pids = pids[1:len_sa]
    end
    
    onlocalhost = assert_same_host(pids)

    local shm_seg_name = ""
    local loc_shmarr 
    local sa = nothing 
    local shmmem_create_pid
    try
        # On OSX, the shm_seg_name length must be < 32 characters
        shm_seg_name = string("/jl", getpid(), int64(time() * 10^9)) 
        if onlocalhost
            shmmem_create_pid = myid()
            loc_shmarr = shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR)
        else
            # The shared array is being created on a remote machine....
            shmmem_create_pid = pids[1]
            remotecall(pids[1], () -> begin shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR); nothing end) 
        end

        func_mapshmem = () -> shm_mmap_array(T, dims, shm_seg_name, JL_O_RDWR)
        
        refs = Array(RemoteRef, length(pids))
        for (i, p) in enumerate(pids)
            refs[i] = remotecall(p, func_mapshmem)
        end

        # Wait till all the workers have mapped the segment
        for i in 1:length(refs)
            wait(refs[i])
        end
        
        # All good, immediately unlink the segment.
        if onlocalhost
            shm_unlink(shm_seg_name)
        else
            remotecall(shmmem_create_pid, shm_unlink, shm_seg_name)  
        end
        shm_seg_name = "" 
        
        sa = SharedArray{T,N}(dims, pids, refs)
        if onlocalhost
            init_loc_flds(sa)
            
            # In the event that myid() is not part of pids, loc_shmarr will not be set 
            # in the init function above, hence setting it here if available.
            sa.loc_shmarr = loc_shmarr
        else
            sa.loc_pididx = 0 
        end
        
        # if present init function is called on each of the parts
        @sync begin 
            if isa(init, Function)
                for p in pids
                    @async remotecall_wait(p, init, sa)
                end
            end
        end
        
    finally
        if shm_seg_name != "" 
            remotecall_fetch(shmmem_create_pid, shm_unlink, shm_seg_name)  
        end
    end
    sa
end

SharedArray(T, I::Int...; kwargs...) = SharedArray(T, I; kwargs...)


length(sa::SharedArray) = prod(sa.dims)
size(sa::SharedArray) = sa.dims
procs(sa::SharedArray) = sa.pids



function range_1dim(sa::SharedArray, n) 
    l = length(sa)
    nw = length(sa.pids)
    partlen = div(l, nw)

    if n == nw
        return (((n-1) * partlen) + 1):l
    else
        return (((n-1) * partlen) + 1):(n*partlen) 
    end
end

sub_1dim(sa::SharedArray, n) = sub(sa.loc_shmarr, range_1dim(sa, n))

function init_loc_flds(sa)
    if myid() in sa.pids
        sa.loc_pididx = findfirst(sa.pids, myid())
        sa.loc_shmarr = fetch(sa.refs[sa.loc_pididx])
        sa.loc_subarr_1d = sub_1dim(sa, sa.loc_pididx)
    else
        sa.loc_pididx = 0
    end
end


# Don't serialize loc_shmarr (it is the complete array) and 
# pididx, which is relevant to the current process only
function serialize(s, sa::SharedArray)
    serialize_type(s, typeof(sa))
    serialize(s, length(SharedArray.names)) 
    for n in SharedArray.names
        if n in [:loc_shmarr, :loc_pididx, :loc_subarr_1d]
            writetag(s, UndefRefTag)
        else
            serialize(s, getfield(sa, n)) 
        end
    end
end

function deserialize{T,N}(s, t::Type{SharedArray{T,N}})
    sa = invoke(deserialize, (Any, DataType), s, t)
    init_loc_flds(sa)
    if (sa.loc_pididx == 0) 
        error("SharedArray cannot be used on a non-participating process")
    end
    sa
end

convert(::Type{Array}, S::SharedArray) = S.loc_shmarr

# # pass through getindex and setindex! - they always work on the complete array unlike DArrays
getindex(S::SharedArray) = getindex(S.loc_shmarr)
getindex(S::SharedArray, I::Real) = getindex(S.loc_shmarr, I)
getindex(S::SharedArray, I::AbstractArray) = getindex(S.loc_shmarr, I)
getindex(S::SharedArray, I) = getindex(S.loc_shmarr, I)
getindex(S::SharedArray, I, J) = getindex(S.loc_shmarr, I, J)
getindex(S::SharedArray, I...) = getindex(S.loc_shmarr, I...)

setindex!(S::SharedArray, x) = (setindex!(S.loc_shmarr, x); S)
setindex!(S::SharedArray, x, I::Real) = (setindex!(S.loc_shmarr, x, I); S)
setindex!(S::SharedArray, x, I::AbstractArray) = (setindex!(S.loc_shmarr, x, I); S)
setindex!(S::SharedArray, x, I) = (setindex!(S.loc_shmarr, x, I); S)
setindex!(S::SharedArray, x, I, J) = (setindex!(S.loc_shmarr, x, I, J); S)
setindex!(S::SharedArray, x, I...) = (setindex!(S.loc_shmarr, x, I...); S)

# convenience constructors
function shmem_fill(v, dims; kwargs...) 
    SharedArray(typeof(v), dims; init = S->fill!(S.loc_subarr_1d, v), kwargs...)
end
shmem_fill(v, I::Int...; kwargs...) = shmem_fill(v, I; kwargs...)

# rand variant with range
function shmem_rand(TR::Union(DataType, Range1), dims; kwargs...) 
    if isa(TR, Range1)
        SharedArray(Int, dims; init = S -> map!((x)->rand(TR), S.loc_subarr_1d), kwargs...)
    else
        SharedArray(TR, dims; init = S -> map!((x)->rand(TR), S.loc_subarr_1d), kwargs...)
    end
end
shmem_rand(TR::Union(DataType, Range1), i::Int; kwargs...) = shmem_rand(TR, (i,); kwargs...)  
shmem_rand(TR::Union(DataType, Range1), I::Int...; kwargs...) = shmem_rand(TR, I; kwargs...)  

shmem_rand(dims; kwargs...) = shmem_rand(Float64, dims; kwargs...)
shmem_rand(I::Int...; kwargs...) = shmem_rand(I; kwargs...)

function shmem_randn(dims; kwargs...) 
    SharedArray(Float64, dims; init = S-> map!((x)->randn(), S.loc_subarr_1d), kwargs...)
end
shmem_randn(I::Int...; kwargs...) = shmem_randn(I; kwargs...)



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

@unix_only begin
function shm_unlink(shm_seg_name) 
    rc = ccall(:shm_unlink, Cint, (Ptr{Uint8},), shm_seg_name)
    systemerror("Error unlinking shmem segment " * shm_seg_name, rc != 0)
end
end

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

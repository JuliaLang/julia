type SharedArray{T,N} <: DenseArray{T,N}
    dims::NTuple{N,Int}
    pids::Vector{Int}
    refs::Array{RemoteRef}

    # The segname is currently used only in the test scripts to ensure that
    # the shmem segment has been unlinked.
    segname::String

    # Fields below are not to be serialized
    # Local shmem map.
    s::Array{T,N}

    # idx of current workers pid into the pids vector, 0 if this shared array is not mapped locally.
    pidx::Int

    # the local partition into the array when viewed as a single dimensional array.
    # this can be removed when @parallel or its equivalent supports looping on
    # a subset of workers.
    loc_subarr_1d

    SharedArray(d,p,r,sn) = new(d,p,r,sn)
end

function SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])
    N = length(dims)

    isbits(T) || error("Type of Shared Array elements must be bits types")
    @windows_only error(" SharedArray is not supported on Windows yet.")

    if isempty(pids)
        # only use workers on the current host
        pids = procs(myid())
        onlocalhost = true
    else
        if !check_same_host(pids) 
            error("SharedArray requires all requested processes to be on the same machine.")
        end
    
        onlocalhost = myid() in procs(pids[1])
    end

    local shm_seg_name = ""
    local s
    local S = nothing
    local shmmem_create_pid
    try
        # On OSX, the shm_seg_name length must be < 32 characters
        shm_seg_name = string("/jl", getpid(), int64(time() * 10^9))
        if onlocalhost
            shmmem_create_pid = myid()
            s = shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR)
        else
            # The shared array is created on a remote machine....
            shmmem_create_pid = pids[1]
            remotecall_fetch(pids[1], () -> begin shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR); nothing end)
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
        if prod(dims) > 0
            if onlocalhost
                shm_unlink(shm_seg_name)
            else
                remotecall_fetch(shmmem_create_pid, shm_unlink, shm_seg_name)
            end
        end
        S = SharedArray{T,N}(dims, pids, refs, shm_seg_name)
        shm_seg_name = ""

        if onlocalhost
            init_loc_flds(S)

            # In the event that myid() is not part of pids, s will not be set
            # in the init function above, hence setting it here if available.
            S.s = s
        else
            S.pidx = 0
        end

        # if present init function is called on each of the parts
        if isa(init, Function)
            @sync begin
                for p in pids
                    @async remotecall_wait(p, init, S)
                end
            end
        end

    finally
        if shm_seg_name != ""
            remotecall_fetch(shmmem_create_pid, shm_unlink, shm_seg_name)
        end
    end
    S
end

SharedArray(T, I::Int...; kwargs...) = SharedArray(T, I; kwargs...)

typealias SharedVector{T} SharedArray{T,1}
typealias SharedMatrix{T} SharedArray{T,2}

length(S::SharedArray) = prod(S.dims)
size(S::SharedArray) = S.dims

function reshape{T,N}(a::SharedArray{T}, dims::NTuple{N,Int})
    (length(a) != prod(dims)) && error("dimensions must be consistent with array size")
    refs = Array(RemoteRef, length(a.pids))
    for (i, p) in enumerate(a.pids)
        refs[i] = remotecall(p, (r,d)->reshape(fetch(r),d), a.refs[i], dims)
    end

    A = SharedArray{T,N}(dims, a.pids, refs, a.segname)
    init_loc_flds(A)
    (a.pidx == 0) && isdefined(a, :s) && (A.s = reshape(a.s, dims))
    A
end

procs(S::SharedArray) = S.pids
indexpids(S::SharedArray) = S.pidx

sdata(S::SharedArray) = S.s
sdata(A::AbstractArray) = A

localindexes(S::SharedArray) = S.pidx > 0 ? range_1dim(S, S.pidx) : error("SharedArray is not mapped to this process")

convert{T}(::Type{Ptr{T}}, S::SharedArray) = convert(Ptr{T}, sdata(S))

convert(::Type{SharedArray}, A::Array) = (S = SharedArray(eltype(A), size(A)); copy!(S, A))
convert{T}(::Type{SharedArray{T}}, A::Array) = (S = SharedArray(T, size(A)); copy!(S, A))
convert{TS,TA,N}(::Type{SharedArray{TS,N}}, A::Array{TA,N}) = (S = SharedArray(TS, size(A)); copy!(S, A))

function deepcopy_internal(S::SharedArray, stackdict::ObjectIdDict)
    haskey(stackdict, S) && return stackdict[S]
    # Note: copy can be used here because SharedArrays are restricted to isbits types
    R = copy(S)
    stackdict[S] = R
    return R
end

function range_1dim(S::SharedArray, pidx)
    l = length(S)
    nw = length(S.pids)
    partlen = div(l, nw)

    if l < nw
        if pidx <= l
            return pidx:pidx
        else
            return 1:0
        end
    elseif pidx == nw
        return (((pidx-1) * partlen) + 1):l
    else
        return (((pidx-1) * partlen) + 1):(pidx*partlen)
    end
end

sub_1dim(S::SharedArray, pidx) = sub(S.s, range_1dim(S, pidx))

function init_loc_flds(S)
    if myid() in S.pids
        S.pidx = findfirst(S.pids, myid())
        S.s = fetch(S.refs[S.pidx])
        S.loc_subarr_1d = sub_1dim(S, S.pidx)
    else
        S.pidx = 0
    end
end


# Don't serialize s (it is the complete array) and
# pidx, which is relevant to the current process only
function serialize(s, S::SharedArray)
    serialize_type(s, typeof(S))
    serialize(s, length(SharedArray.names))
    for n in SharedArray.names
        if n in [:s, :pidx, :loc_subarr_1d]
            writetag(s, UndefRefTag)
        else
            serialize(s, getfield(S, n))
        end
    end
end

function deserialize{T,N}(s, t::Type{SharedArray{T,N}})
    S = invoke(deserialize, (Any, DataType), s, t)
    init_loc_flds(S)
    if (S.pidx == 0)
        error("SharedArray cannot be used on a non-participating process")
    end
    S
end

convert(::Type{Array}, S::SharedArray) = S.s

# # pass through getindex and setindex! - they always work on the complete array unlike DArrays
getindex(S::SharedArray) = getindex(S.s)
getindex(S::SharedArray, I::Real) = getindex(S.s, I)
getindex(S::SharedArray, I::AbstractArray) = getindex(S.s, I)
@nsplat N 1:5 getindex(S::SharedArray, I::NTuple{N,Any}...) = getindex(S.s, I...)

setindex!(S::SharedArray, x) = (setindex!(S.s, x); S)
setindex!(S::SharedArray, x, I::Real) = (setindex!(S.s, x, I); S)
setindex!(S::SharedArray, x, I::AbstractArray) = (setindex!(S.s, x, I); S)
@nsplat N 1:5 setindex!(S::SharedArray, x, I::NTuple{N,Any}...) = (setindex!(S.s, x, I...); S)

function fill!(S::SharedArray, v)
    f = S->fill!(S.loc_subarr_1d, v)
    @sync for p in procs(S)
        @async remotecall_wait(p, f, S)
    end
    return S
end

function rand!{T}(S::SharedArray{T})
    f = S->map!(x->rand(T), S.loc_subarr_1d)
    @sync for p in procs(S)
        @async remotecall_wait(p, f, S)
    end
    return S
end

function randn!(S::SharedArray)
    f = S->map!(x->randn, S.loc_subarr_1d)
    @sync for p in procs(S)
        @async remotecall_wait(p, f, S)
    end
    return S
end

# convenience constructors
function shmem_fill(v, dims; kwargs...)
    SharedArray(typeof(v), dims; init = S->fill!(S.loc_subarr_1d, v), kwargs...)
end
shmem_fill(v, I::Int...; kwargs...) = shmem_fill(v, I; kwargs...)

# rand variant with range
function shmem_rand(TR::Union(DataType, UnitRange), dims; kwargs...)
    if isa(TR, UnitRange)
        SharedArray(Int, dims; init = S -> map!((x)->rand(TR), S.loc_subarr_1d), kwargs...)
    else
        SharedArray(TR, dims; init = S -> map!((x)->rand(TR), S.loc_subarr_1d), kwargs...)
    end
end
shmem_rand(TR::Union(DataType, UnitRange), i::Int; kwargs...) = shmem_rand(TR, (i,); kwargs...)
shmem_rand(TR::Union(DataType, UnitRange), I::Int...; kwargs...) = shmem_rand(TR, I; kwargs...)

shmem_rand(dims; kwargs...) = shmem_rand(Float64, dims; kwargs...)
shmem_rand(I::Int...; kwargs...) = shmem_rand(I; kwargs...)

function shmem_randn(dims; kwargs...)
    SharedArray(Float64, dims; init = S-> map!((x)->randn(), S.loc_subarr_1d), kwargs...)
end
shmem_randn(I::Int...; kwargs...) = shmem_randn(I; kwargs...)

similar(S::SharedArray, T, dims::Dims) = SharedArray(T, dims; pids=procs(S))
similar(S::SharedArray, T) = similar(S, T, size(S))
similar(S::SharedArray, dims::Dims) = similar(S, eltype(S), dims)
similar(S::SharedArray) = similar(S, eltype(S), size(S))

map(f::Callable, S::SharedArray) = (S2 = similar(S); S2[:] = S[:]; map!(f, S2); S2)

reduce(f::Function, S::SharedArray) =
    mapreduce(fetch, f,
              { @spawnat p reduce(f, S.loc_subarr_1d) for p in procs(S) })


function map!(f::Callable, S::SharedArray)
    @sync for p in procs(S)
        @spawnat p begin
            for idx in localindexes(S)
                S.s[idx] = f(S.s[idx])
            end
        end
    end
    return S
end

copy!(S::SharedArray, A::Array) = (copy!(S.s, A); S)

function copy!(S::SharedArray, R::SharedArray)
    length(S) == length(R) || throw(BoundsError())
    ps = intersect(procs(S), procs(R))
    isempty(ps) && error("source and destination arrays don't share any process")
    l = length(S)
    length(ps) > l && (ps = ps[1:l])
    nw = length(ps)
    partlen = div(l, nw)

    @sync for i = 1:nw
        p = ps[i]
        idx = i < nw ?  ((i-1)*partlen+1:i*partlen) : ((i-1)*partlen+1:l)
        @spawnat p begin
            S.s[idx] = R.s[idx]
        end
    end

    return S
end

complex(S1::SharedArray,S2::SharedArray) = convert(SharedArray, complex(S1.s, S2.s))

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

    if prod(dims) == 0
        return Array(T, dims)
    end
    
    try
        fd_mem = shm_open(shm_seg_name, mode, S_IRUSR | S_IWUSR)
        systemerror("shm_open() failed for " * shm_seg_name, fd_mem <= 0)

        s = fdio(fd_mem, true)

        # On OSX, ftruncate must to used to set size of segment, just lseek does not work.
        # and only at creation time
        if (mode & JL_O_CREAT) == JL_O_CREAT
            rc = ccall(:ftruncate, Int, (Int, Int), fd_mem, prod(dims)*sizeof(T))
            systemerror("ftruncate() failed for shm segment " * shm_seg_name, rc != 0)
        end

        A = mmap_array(T, dims, s, zero(FileOffset), grow=false)
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
    rc
end
end

@unix_only shm_open(shm_seg_name, oflags, permissions) = ccall(:shm_open, Int, (Ptr{Uint8}, Int, Int), shm_seg_name, oflags, permissions)



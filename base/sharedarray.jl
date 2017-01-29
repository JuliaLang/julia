# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Serializer: serialize_cycle_header, serialize_type, writetag, UNDEFREF_TAG

type SharedArray{T,N} <: DenseArray{T,N}
    dims::NTuple{N,Int}
    pids::Vector{Int}
    refs::Vector

    # The segname is currently used only in the test scripts to ensure that
    # the shmem segment has been unlinked.
    segname::String

    # Fields below are not to be serialized
    # Local shmem map.
    s::Array{T,N}

    # idx of current worker's pid in the pids vector, 0 if this shared array is not mapped locally.
    pidx::Int

    # the local partition into the array when viewed as a single dimensional array.
    # this can be removed when @parallel or its equivalent supports looping on
    # a subset of workers.
    loc_subarr_1d::SubArray{T,1,Array{T,1},Tuple{UnitRange{Int}},true}

    function SharedArray{T,N}(d,p,r,sn,s) where {T,N}
        new(d,p,r,sn,s,0,view(Array{T}(ntuple(d->0,N)), 1:0))
    end
end

"""
    SharedArray{T}(dims::NTuple; init=false, pids=Int[])
    SharedArray{T,N}(...)

Construct a `SharedArray` of a bitstype `T` and size `dims` across the
processes specified by `pids` - all of which have to be on the same
host.  If `N` is specified by calling `SharedArray{T,N}(dims)`, then
`N` must match the length of `dims`.

If `pids` is left unspecified, the shared array will be mapped across all processes on the
current host, including the master. But, `localindexes` and `indexpids` will only refer to
worker processes. This facilitates work distribution code to use workers for actual
computation with the master process acting as a driver.

If an `init` function of the type `initfn(S::SharedArray)` is specified, it is called on all
the participating workers.

    SharedArray{T}(filename::AbstractString, dims::NTuple, [offset=0]; mode=nothing, init=false, pids=Int[])
    SharedArray{T,N}(...)

Construct a `SharedArray` backed by the file `filename`, with element
type `T` (must be a `bitstype`) and size `dims`, across the processes
specified by `pids` - all of which have to be on the same host. This
file is mmapped into the host memory, with the following consequences:

- The array data must be represented in binary format (e.g., an ASCII
  format like CSV cannot be supported)

- Any changes you make to the array values (e.g., `A[3] = 0`) will
  also change the values on disk

If `pids` is left unspecified, the shared array will be mapped across
all processes on the current host, including the master. But,
`localindexes` and `indexpids` will only refer to worker
processes. This facilitates work distribution code to use workers for
actual computation with the master process acting as a driver.

`mode` must be one of `"r"`, `"r+"`, `"w+"`, or `"a+"`, and defaults
to `"r+"` if the file specified by `filename` already exists, or
`"w+"` if not. If an `init` function of the type
`initfn(S::SharedArray)` is specified, it is called on all the
participating workers. You cannot specify an `init` function if the
file is not writable.

`offset` allows you to skip the specified number of bytes at the
beginning of the file.
"""
SharedArray

function (::Type{SharedArray{T,N}}){T,N}(dims::Dims{N}; init=false, pids=Int[])
    isbits(T) || throw(ArgumentError("type of SharedArray elements must be bits types, got $(T)"))

    pids, onlocalhost = shared_pids(pids)

    local shm_seg_name = ""
    local s = Array{T}(ntuple(d->0,N))
    local S
    local shmmem_create_pid
    try
        # On OSX, the shm_seg_name length must be <= 31 characters (including the terminating NULL character)
        shm_seg_name = @sprintf("/jl%06u%s", getpid() % 10^6, randstring(20))
        if onlocalhost
            shmmem_create_pid = myid()
            s = shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR)
        else
            # The shared array is created on a remote machine
            shmmem_create_pid = pids[1]
            remotecall_fetch(pids[1]) do
                shm_mmap_array(T, dims, shm_seg_name, JL_O_CREAT | JL_O_RDWR)
                nothing
            end
        end

        func_mapshmem = () -> shm_mmap_array(T, dims, shm_seg_name, JL_O_RDWR)

        refs = Array{Future}(length(pids))
        for (i, p) in enumerate(pids)
            refs[i] = remotecall(func_mapshmem, p)
        end

        # Wait till all the workers have mapped the segment
        for ref in refs
            wait(ref)
        end

        # All good, immediately unlink the segment.
        if (prod(dims) > 0) && (sizeof(T) > 0)
            if onlocalhost
                rc = shm_unlink(shm_seg_name)
            else
                rc = remotecall_fetch(shm_unlink, shmmem_create_pid, shm_seg_name)
            end
            systemerror("Error unlinking shmem segment " * shm_seg_name, rc != 0)
        end
        S = SharedArray{T,N}(dims, pids, refs, shm_seg_name, s)
        initialize_shared_array(S, onlocalhost, init, pids)
        shm_seg_name = ""

    finally
        if shm_seg_name != ""
            remotecall_fetch(shm_unlink, shmmem_create_pid, shm_seg_name)
        end
    end
    S
end

(::Type{SharedArray{T,N}}){T,N}(I::Integer...; kwargs...) =
    SharedArray{T,N}(I; kwargs...)
(::Type{SharedArray{T}}){T}(d::NTuple; kwargs...) =
    SharedArray{T,length(d)}(d; kwargs...)
(::Type{SharedArray{T}}){T}(I::Integer...; kwargs...) =
    SharedArray{T,length(I)}(I; kwargs...)
(::Type{SharedArray{T}}){T}(m::Integer; kwargs...) =
    SharedArray{T,1}(m; kwargs...)
(::Type{SharedArray{T}}){T}(m::Integer, n::Integer; kwargs...) =
    SharedArray{T,2}(m, n; kwargs...)
(::Type{SharedArray{T}}){T}(m::Integer, n::Integer, o::Integer; kwargs...) =
    SharedArray{T,3}(m, n, o; kwargs...)

function (::Type{SharedArray{T,N}}){T,N}(filename::AbstractString, dims::NTuple{N,Int},
        offset::Integer=0; mode=nothing, init=false, pids::Vector{Int}=Int[])
    if !isabspath(filename)
        throw(ArgumentError("$filename is not an absolute path; try abspath(filename)?"))
    end
    if !isbits(T)
        throw(ArgumentError("type of SharedArray elements must be bits types, got $(T)"))
    end

    pids, onlocalhost = shared_pids(pids)

    # If not supplied, determine the appropriate mode
    have_file = onlocalhost ? isfile(filename) : remotecall_fetch(isfile, pids[1], filename)
    if mode === nothing
        mode = have_file ? "r+" : "w+"
    end
    workermode = mode == "w+" ? "r+" : mode  # workers don't truncate!

    # Ensure the file will be readable
    if !(mode in ("r", "r+", "w+", "a+"))
        throw(ArgumentError("mode must be readable, but $mode is not"))
    end
    if init !== false
        typeassert(init, Function)
        if !(mode in ("r+", "w+", "a+"))
            throw(ArgumentError("cannot initialize unwritable array (mode = $mode)"))
        end
    end
    if mode == "r" && !isfile(filename)
        throw(ArgumentError("file $filename does not exist, but mode $mode cannot create it"))
    end

    # Create the file if it doesn't exist, map it if it does
    refs = Array{Future}(length(pids))
    func_mmap = mode -> open(filename, mode) do io
        Mmap.mmap(io, Array{T,N}, dims, offset; shared=true)
    end
    s = Array{T}(ntuple(d->0,N))
    if onlocalhost
        s = func_mmap(mode)
        refs[1] = remotecall(pids[1]) do
            func_mmap(workermode)
        end
    else
        refs[1] = remotecall_wait(pids[1]) do
            func_mmap(mode)
        end
    end

    # Populate the rest of the workers
    for i = 2:length(pids)
        refs[i] = remotecall(pids[i]) do
            func_mmap(workermode)
        end
    end

    # Wait till all the workers have mapped the segment
    for ref in refs
        wait(ref)
    end

    S = SharedArray{T,N}(dims, pids, refs, filename, s)
    initialize_shared_array(S, onlocalhost, init, pids)
    S
end

(::Type{SharedArray{T}}){T,N}(filename::AbstractString, dims::NTuple{N,Int}, offset::Integer=0;
                              mode=nothing, init=false, pids::Vector{Int}=Int[]) =
    SharedArray{T,N}(filename, dims, offset; mode=mode, init=init, pids=pids)

function initialize_shared_array(S, onlocalhost, init, pids)
    if onlocalhost
        init_loc_flds(S)
    else
        S.pidx = 0
    end

    # if present, init function is called on each of the parts
    if isa(init, Function)
        @sync begin
            for p in pids
                @async remotecall_wait(init, p, S)
            end
        end
    end

    finalizer(S, finalize_refs)
    S
end

function finalize_refs{T,N}(S::SharedArray{T,N})
    if length(S.pids) > 0
        for r in S.refs
            finalize(r)
        end
        empty!(S.pids)
        empty!(S.refs)
        init_loc_flds(S)
        S.s = Array{T}(ntuple(d->0,N))
    end
    S
end

typealias SharedVector{T} SharedArray{T,1}
typealias SharedMatrix{T} SharedArray{T,2}

length(S::SharedArray) = prod(S.dims)
size(S::SharedArray) = S.dims
ndims(S::SharedArray) = length(S.dims)
linearindexing{S<:SharedArray}(::Type{S}) = LinearFast()

function reshape{T,N}(a::SharedArray{T}, dims::NTuple{N,Int})
    if length(a) != prod(dims)
        throw(DimensionMismatch("dimensions must be consistent with array size"))
    end
    refs = Array{Future}(length(a.pids))
    for (i, p) in enumerate(a.pids)
        refs[i] = remotecall(p, a.refs[i], dims) do r,d
            reshape(fetch(r),d)
        end
    end

    A = SharedArray{T,N}(dims, a.pids, refs, a.segname, reshape(a.s, dims))
    init_loc_flds(A)
    A
end

"""
    procs(S::SharedArray)

Get the vector of processes mapping the shared array.
"""
procs(S::SharedArray) = S.pids

"""
    indexpids(S::SharedArray)

Returns the current worker's index in the list of workers
mapping the `SharedArray` (i.e. in the same list returned by `procs(S)`), or
0 if the `SharedArray` is not mapped locally.
"""
indexpids(S::SharedArray) = S.pidx

"""
    sdata(S::SharedArray)

Returns the actual `Array` object backing `S`.
"""
sdata(S::SharedArray) = S.s
sdata(A::AbstractArray) = A

"""
    localindexes(S::SharedArray)

Returns a range describing the "default" indexes to be handled by the
current process.  This range should be interpreted in the sense of
linear indexing, i.e., as a sub-range of `1:length(S)`.  In
multi-process contexts, returns an empty range in the parent process
(or any process for which [`indexpids`](@ref) returns 0).

It's worth emphasizing that `localindexes` exists purely as a
convenience, and you can partition work on the array among workers any
way you wish. For a `SharedArray`, all indexes should be equally fast
for each worker process.
"""
localindexes(S::SharedArray) = S.pidx > 0 ? range_1dim(S, S.pidx) : 1:0

unsafe_convert{T}(::Type{Ptr{T}}, S::SharedArray{T}) = unsafe_convert(Ptr{T}, sdata(S))
unsafe_convert{T}(::Type{Ptr{T}}, S::SharedArray   ) = unsafe_convert(Ptr{T}, sdata(S))

function convert(::Type{SharedArray}, A::Array)
    S = SharedArray{eltype(A),ndims(A)}(size(A))
    copy!(S, A)
end
function convert{T}(::Type{SharedArray{T}}, A::Array)
    S = SharedArray{T,ndims(A)}(size(A))
    copy!(S, A)
end
function convert{TS,TA,N}(::Type{SharedArray{TS,N}}, A::Array{TA,N})
    S = SharedArray{TS,ndims(A)}(size(A))
    copy!(S, A)
end

function deepcopy_internal(S::SharedArray, stackdict::ObjectIdDict)
    haskey(stackdict, S) && return stackdict[S]
    R = SharedArray{eltype(S),ndims(S)}(size(S); pids = S.pids)
    copy!(sdata(R), sdata(S))
    stackdict[S] = R
    return R
end

function shared_pids(pids)
    if isempty(pids)
        # only use workers on the current host
        pids = procs(myid())
        if length(pids) > 1
            pids = filter(x -> x != 1, pids)
        end

        onlocalhost = true
    else
        if !check_same_host(pids)
            throw(ArgumentError("SharedArray requires all requested processes to be on the same machine."))
        end

        onlocalhost = myid() in procs(pids[1])
    end
    pids, onlocalhost
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

sub_1dim(S::SharedArray, pidx) = view(S.s, range_1dim(S, pidx))

function init_loc_flds{T,N}(S::SharedArray{T,N}, empty_local=false)
    if myid() in S.pids
        S.pidx = findfirst(S.pids, myid())
        if isa(S.refs[1], Future)
            refid = remoteref_id(S.refs[S.pidx])
        else
            refid = S.refs[S.pidx]
        end
        c = channel_from_id(refid)
        S.s = fetch(c)
        S.loc_subarr_1d = sub_1dim(S, S.pidx)
    else
        S.pidx = 0
        if empty_local
            S.s = Array{T}(ntuple(d->0,N))
        end
        S.loc_subarr_1d = view(Array{T}(ntuple(d->0,N)), 1:0)
    end
end


# Don't serialize s (it is the complete array) and
# pidx, which is relevant to the current process only
function serialize(s::AbstractSerializer, S::SharedArray)
    serialize_cycle_header(s, S) && return
    for n in fieldnames(SharedArray)
        if n in [:s, :pidx, :loc_subarr_1d]
            writetag(s.io, UNDEFREF_TAG)
        elseif n == :refs
            v = getfield(S, n)
            if isa(v[1], Future)
                # convert to ids to avoid distributed GC overhead
                ids = [remoteref_id(x) for x in v]
                serialize(s, ids)
            else
                serialize(s, v)
            end
        else
            serialize(s, getfield(S, n))
        end
    end
end

function deserialize{T,N}(s::AbstractSerializer, t::Type{SharedArray{T,N}})
    S = invoke(deserialize, Tuple{AbstractSerializer,DataType}, s, t)
    init_loc_flds(S, true)
    S
end

function show(io::IO, S::SharedArray)
    if length(S.s) > 0
        invoke(show, Tuple{IO,DenseArray}, io, S)
    else
        show(io, remotecall_fetch(sharr->sharr.s, S.pids[1], S))
    end
end

function show(io::IO, mime::MIME"text/plain", S::SharedArray)
    if length(S.s) > 0
        invoke(show, Tuple{IO,MIME"text/plain",DenseArray}, io, MIME"text/plain"(), S)
    else
        # retrieve from the first worker mapping the array.
        println(io, summary(S), ":")
        showarray(io, remotecall_fetch(sharr->sharr.s, S.pids[1], S), false; header=false)
    end
end

convert(::Type{Array}, S::SharedArray) = S.s

# pass through getindex and setindex! - unlike DArrays, these always work on the complete array
getindex(S::SharedArray, i::Real) = getindex(S.s, i)

setindex!(S::SharedArray, x, i::Real) = setindex!(S.s, x, i)

function fill!(S::SharedArray, v)
    vT = convert(eltype(S), v)
    f = S->fill!(S.loc_subarr_1d, vT)
    @sync for p in procs(S)
        @async remotecall_wait(f, p, S)
    end
    return S
end

function rand!{T}(S::SharedArray{T})
    f = S->map!(x -> rand(T), S.loc_subarr_1d, S.loc_subarr_1d)
    @sync for p in procs(S)
        @async remotecall_wait(f, p, S)
    end
    return S
end

function randn!(S::SharedArray)
    f = S->map!(x -> randn(), S.loc_subarr_1d, S.loc_subarr_1d)
    @sync for p in procs(S)
        @async remotecall_wait(f, p, S)
    end
    return S
end

# convenience constructors
function shmem_fill(v, dims; kwargs...)
    SharedArray{typeof(v),length(dims)}(dims; init = S->fill!(S.loc_subarr_1d, v), kwargs...)
end
shmem_fill(v, I::Int...; kwargs...) = shmem_fill(v, I; kwargs...)

# rand variant with range
function shmem_rand(TR::Union{DataType, UnitRange}, dims; kwargs...)
    if isa(TR, UnitRange)
        SharedArray{Int,length(dims)}(dims; init = S -> map!(x -> rand(TR), S.loc_subarr_1d, S.loc_subarr_1d), kwargs...)
    else
        SharedArray{TR,length(dims)}(dims; init = S -> map!(x -> rand(TR), S.loc_subarr_1d, S.loc_subarr_1d), kwargs...)
    end
end
shmem_rand(TR::Union{DataType, UnitRange}, i::Int; kwargs...) = shmem_rand(TR, (i,); kwargs...)
shmem_rand(TR::Union{DataType, UnitRange}, I::Int...; kwargs...) = shmem_rand(TR, I; kwargs...)

shmem_rand(dims; kwargs...) = shmem_rand(Float64, dims; kwargs...)
shmem_rand(I::Int...; kwargs...) = shmem_rand(I; kwargs...)

function shmem_randn(dims; kwargs...)
    SharedArray{Float64,length(dims)}(dims; init = S-> map!(x -> randn(), S.loc_subarr_1d, S.loc_subarr_1d), kwargs...)
end
shmem_randn(I::Int...; kwargs...) = shmem_randn(I; kwargs...)

similar(S::SharedArray, T::Type, dims::Dims) = similar(S.s, T, dims)
similar(S::SharedArray, T::Type) = similar(S.s, T, size(S))
similar(S::SharedArray, dims::Dims) = similar(S.s, eltype(S), dims)
similar(S::SharedArray) = similar(S.s, eltype(S), size(S))

reduce(f, S::SharedArray) =
    mapreduce(fetch, f,
              Any[ @spawnat p reduce(f, S.loc_subarr_1d) for p in procs(S) ])


function map!(f, S::SharedArray, Q::SharedArray)
    if !(S === Q) && (procs(S) != procs(Q) || localindexes(S) != localindexes(Q))
        throw(ArgumentError("incompatible source and destination arguments"))
    end
    @sync for p in procs(S)
        @spawnat p begin
            for idx in localindexes(S)
                S.s[idx] = f(Q.s[idx])
            end
        end
    end
    return S
end

copy!(S::SharedArray, A::Array) = (copy!(S.s, A); S)

function copy!(S::SharedArray, R::SharedArray)
    length(S) == length(R) || throw(BoundsError())
    ps = intersect(procs(S), procs(R))
    isempty(ps) && throw(ArgumentError("source and destination arrays don't share any process"))
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

function print_shmem_limits(slen)
    try
        if is_linux()
            pfx = "kernel"
        elseif is_apple()
            pfx = "kern.sysv"
        else
            return
        end

        shmmax_MB = div(parse(Int, split(readstring(`sysctl $(pfx).shmmax`))[end]), 1024*1024)
        page_size = parse(Int, split(readstring(`getconf PAGE_SIZE`))[end])
        shmall_MB = div(parse(Int, split(readstring(`sysctl $(pfx).shmall`))[end]) * page_size, 1024*1024)

        println("System max size of single shmem segment(MB) : ", shmmax_MB,
            "\nSystem max size of all shmem segments(MB) : ", shmall_MB,
            "\nRequested size(MB) : ", div(slen, 1024*1024),
            "\nPlease ensure requested size is within system limits.",
            "\nIf not, increase system limits and try again."
        )
    catch e
        nothing # Ignore any errors in this
    end
end

# utilities
function shm_mmap_array(T, dims, shm_seg_name, mode)
    local s = nothing
    local A = nothing

    if (prod(dims) == 0) || (sizeof(T) == 0)
        return Array{T}(dims)
    end

    try
        A = _shm_mmap_array(T, dims, shm_seg_name, mode)
    catch e
        print_shmem_limits(prod(dims)*sizeof(T))
        rethrow(e)

    finally
        if s !== nothing
            close(s)
        end
    end
    A
end


# platform-specific code

if is_windows()
function _shm_mmap_array(T, dims, shm_seg_name, mode)
    readonly = !((mode & JL_O_RDWR) == JL_O_RDWR)
    create = (mode & JL_O_CREAT) == JL_O_CREAT
    s = Mmap.Anonymous(shm_seg_name, readonly, create)
    Mmap.mmap(s, Array{T,length(dims)}, dims, zero(Int64))
end

# no-op in windows
shm_unlink(shm_seg_name) = 0

else # !windows
function _shm_mmap_array(T, dims, shm_seg_name, mode)
    fd_mem = shm_open(shm_seg_name, mode, S_IRUSR | S_IWUSR)
    systemerror("shm_open() failed for " * shm_seg_name, fd_mem < 0)

    s = fdio(fd_mem, true)

    # On OSX, ftruncate must to used to set size of segment, just lseek does not work.
    # and only at creation time
    if (mode & JL_O_CREAT) == JL_O_CREAT
        rc = ccall(:jl_ftruncate, Cint, (Cint, Int64), fd_mem, prod(dims)*sizeof(T))
        systemerror("ftruncate() failed for shm segment " * shm_seg_name, rc != 0)
    end

    Mmap.mmap(s, Array{T,length(dims)}, dims, zero(Int64); grow=false)
end

shm_unlink(shm_seg_name) = ccall(:shm_unlink, Cint, (Cstring,), shm_seg_name)
shm_open(shm_seg_name, oflags, permissions) = ccall(:shm_open, Cint,
    (Cstring, Cint, Cmode_t), shm_seg_name, oflags, permissions)

end # os-test

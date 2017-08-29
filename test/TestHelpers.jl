# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHelpers

include("dimensionful.jl")
export Furlong

mutable struct FakeTerminal <: Base.Terminals.UnixTerminal
    in_stream::Base.IO
    out_stream::Base.IO
    err_stream::Base.IO
    hascolor::Bool
    raw::Bool
    FakeTerminal(stdin,stdout,stderr,hascolor=true) =
        new(stdin,stdout,stderr,hascolor,false)
end

Base.Terminals.hascolor(t::FakeTerminal) = t.hascolor
Base.Terminals.raw!(t::FakeTerminal, raw::Bool) = t.raw = raw
Base.Terminals.size(t::FakeTerminal) = (24, 80)

function open_fake_pty()
    @static if Sys.iswindows()
        error("Unable to create a fake PTY in Windows")
    end

    O_RDWR = Base.Filesystem.JL_O_RDWR
    O_NOCTTY = Base.Filesystem.JL_O_NOCTTY

    fdm = ccall(:posix_openpt, Cint, (Cint,), O_RDWR|O_NOCTTY)
    fdm == -1 && error("Failed to open PTY master")
    rc = ccall(:grantpt, Cint, (Cint,), fdm)
    rc != 0 && error("grantpt failed")
    rc = ccall(:unlockpt, Cint, (Cint,), fdm)
    rc != 0 && error("unlockpt")

    fds = ccall(:open, Cint, (Ptr{UInt8}, Cint),
        ccall(:ptsname, Ptr{UInt8}, (Cint,), fdm), O_RDWR|O_NOCTTY)

    # slave
    slave = RawFD(fds)
    master = Base.TTY(RawFD(fdm); readable = true)
    slave, master
end

function with_fake_pty(f)
    slave, master = open_fake_pty()
    try
        f(slave, master)
    finally
        ccall(:close,Cint,(Cint,),slave) # XXX: this causes the kernel to throw away all unread data on the pty
        close(master)
    end
end

function challenge_prompt(code::AbstractString, challenges; timeout::Integer=10, debug::Bool=true)
    output_file = tempname()
    wrapped_code = """
    result = let
        $code
    end
    open("$output_file", "w") do fp
        serialize(fp, result)
    end
    """
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $wrapped_code`
    try
        challenge_prompt(cmd, challenges, timeout=timeout, debug=debug)
        return open(output_file, "r") do fp
            deserialize(fp)
        end
    finally
        isfile(output_file) && rm(output_file)
    end
    return nothing
end

function challenge_prompt(cmd::Cmd, challenges; timeout::Integer=10, debug::Bool=true)
    function format_output(output)
        debug ? "Process output found:\n\"\"\"\n$(read(seekstart(out), String))\n\"\"\"" : ""
    end
    out = IOBuffer()
    with_fake_pty() do slave, master
        p = spawn(detach(cmd), slave, slave, slave)
        # Kill the process if it takes too long. Typically occurs when process is waiting for input
        @async begin
            sleep(timeout)
            kill(p)
            close(master)
        end
        try
            for (challenge, response) in challenges
                process_exited(p) && error("Too few prompts. $(format_output(out))")

                write(out, readuntil(master, challenge))
                if !isopen(master)
                    error("Could not locate challenge: \"$challenge\". $(format_output(out))")
                end
                write(master, response)
            end
            wait(p)
        finally
            kill(p)
        end
        # Determine if the process was explicitly killed
        killed = process_exited(p) && (p.exitcode != 0 || p.termsignal != 0)
        killed && error("Too many prompts. $(format_output(out))")
    end
    nothing
end

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OAs

using Base: Indices, IndexCartesian, IndexLinear, tail

export OffsetArray

struct OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
OffsetVector{T,AA<:AbstractArray} = OffsetArray{T,1,AA}

OffsetArray(A::AbstractArray{T,N}, offsets::NTuple{N,Int}) where {T,N} = OffsetArray{T,N,typeof(A)}(A, offsets)
OffsetArray(A::AbstractArray{T,N}, offsets::Vararg{Int,N}) where {T,N} = OffsetArray(A, offsets)

OffsetArray{T,N}(inds::Indices{N}) where {T,N} = OffsetArray{T,N,Array{T,N}}(Array{T,N}(map(length, inds)), map(indsoffset, inds))
OffsetArray{T}(inds::Indices{N}) where {T,N} = OffsetArray{T,N}(inds)

Base.IndexStyle(::Type{T}) where {T<:OffsetArray} = Base.IndexStyle(parenttype(T))
parenttype(::Type{OffsetArray{T,N,AA}}) where {T,N,AA} = AA
parenttype(A::OffsetArray) = parenttype(typeof(A))

Base.parent(A::OffsetArray) = A.parent

errmsg(A) = error("size not supported for arrays with indices $(indices(A)); see https://docs.julialang.org/en/latest/devdocs/offset-arrays/")
Base.size(A::OffsetArray) = errmsg(A)
Base.size(A::OffsetArray, d) = errmsg(A)
Base.eachindex(::IndexCartesian, A::OffsetArray) = CartesianRange(indices(A))
Base.eachindex(::IndexLinear, A::OffsetVector) = indices(A, 1)

# Implementations of indices and indices1. Since bounds-checking is
# performance-critical and relies on indices, these are usually worth
# optimizing thoroughly.
@inline Base.indices(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? indices(parent(A))[d] + A.offsets[d] : (1:1)
@inline Base.indices(A::OffsetArray) = _indices(indices(parent(A)), A.offsets)  # would rather use ntuple, but see #15276
@inline _indices(inds, offsets) = (inds[1]+offsets[1], _indices(tail(inds), tail(offsets))...)
_indices(::Tuple{}, ::Tuple{}) = ()
Base.indices1(A::OffsetArray{T,0}) where {T} = 1:1  # we only need to specialize this one

function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{UnitRange,Vararg{UnitRange}})
    B = similar(A, T, map(length, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.similar(f::Union{Function,Type}, shape::Tuple{UnitRange,Vararg{UnitRange}}) = OffsetArray(f(map(length, shape)), map(indsoffset, shape))

Base.reshape(A::AbstractArray, inds::Tuple{UnitRange,Vararg{UnitRange}}) = OffsetArray(reshape(A, map(length, inds)), map(indsoffset, inds))

@inline function Base.getindex(A::OffsetArray{T,N}, I::Vararg{Int,N}) where {T,N}
    checkbounds(A, I...)
    @inbounds ret = parent(A)[offset(A.offsets, I)...]
    ret
end
# Vectors don't support one-based linear indexing; they always use the offsets
@inline function Base.getindex(A::OffsetVector, i::Int)
    checkbounds(A, i)
    @inbounds ret = parent(A)[offset(A.offsets, (i,))[1]]
    ret
end
# But multidimensional arrays allow one-based linear indexing
@inline function Base.getindex(A::OffsetArray, i::Int)
    checkbounds(A, i)
    @inbounds ret = parent(A)[i]
    ret
end
@inline function Base.setindex!(A::OffsetArray{T,N}, val, I::Vararg{Int,N}) where {T,N}
    checkbounds(A, I...)
    @inbounds parent(A)[offset(A.offsets, I)...] = val
    val
end
@inline function Base.setindex!(A::OffsetVector, val, i::Int)
    checkbounds(A, i)
    @inbounds parent(A)[offset(A.offsets, (i,))[1]] = val
    val
end
@inline function Base.setindex!(A::OffsetArray, val, i::Int)
    checkbounds(A, i)
    @inbounds parent(A)[i] = val
    val
end

@inline function Base.deleteat!(A::OffsetArray, i::Int)
    checkbounds(A, i)
    @inbounds deleteat!(parent(A), offset(A.offsets, (i,))[1])
end

@inline function Base.deleteat!(A::OffsetArray{T,N}, I::Vararg{Int, N}) where {T,N}
    checkbounds(A, I...)
    @inbounds deleteat!(parent(A), offset(A.offsets, I)...)
end

@inline function Base.deleteat!(A::OffsetArray, i::UnitRange{Int})
    checkbounds(A, first(i))
    checkbounds(A, last(i))
    first_idx = offset(A.offsets, (first(i),))[1]
    last_idx = offset(A.offsets, (last(i),))[1]
    @inbounds deleteat!(parent(A), first_idx:last_idx)
end

# Computing a shifted index (subtracting the offset)
offset(offsets::NTuple{N,Int}, inds::NTuple{N,Int}) where {N} = _offset((), offsets, inds)
_offset(out, ::Tuple{}, ::Tuple{}) = out
@inline _offset(out, offsets, inds) = _offset((out..., inds[1]-offsets[1]), Base.tail(offsets), Base.tail(inds))

indsoffset(r::Range) = first(r) - 1
indsoffset(i::Integer) = 0

Base.resize!(A::OffsetVector, nl::Integer) = (resize!(A.parent, nl); A)

end

end

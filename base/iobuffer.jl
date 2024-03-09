# This file is a part of Julia. License is MIT: https://julialang.org/license

## work with AbstractVector{UInt8} via I/O primitives ##

# Stateful string
mutable struct GenericIOBuffer{T<:AbstractVector{UInt8}} <: IO
    data::T # T should support: getindex, setindex!, length, copyto!, similar, and (optionally) resize!
    reinit::Bool # if true, data needs to be re-allocated (after take!)
    readable::Bool
    writable::Bool
    seekable::Bool # if not seekable, implementation is free to destroy (compact) past read data
    append::Bool # add data at end instead of at pointer
    size::Int # end pointer (and write pointer if append == true) + offset
    maxsize::Int # fixed array size (typically pre-allocated)
    ptr::Int # read (and maybe write) pointer + offset
    offset::Int # offset of ptr and size from actual start of data and actual size
    mark::Int # reset mark location for ptr (or <0 for no mark)

    function GenericIOBuffer{T}(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                                maxsize::Integer) where T<:AbstractVector{UInt8}
        require_one_based_indexing(data)
        return new(data, false, readable, writable, seekable, append, length(data), maxsize, 1, 0, -1)
    end
end

const IOBuffer = GenericIOBuffer{Memory{UInt8}}

function GenericIOBuffer(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer) where T<:AbstractVector{UInt8}
    GenericIOBuffer{T}(data, readable, writable, seekable, append, maxsize)
end
function GenericIOBuffer(data::Vector{UInt8}, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer)
    ref = data.ref
    buf = GenericIOBuffer(ref.mem, readable, writable, seekable, append, maxsize)
    offset = memoryrefoffset(ref) - 1
    buf.ptr += offset
    buf.size = length(data) + offset
    buf.offset = offset
    return buf
end

# allocate Vector{UInt8}s for IOBuffer storage that can efficiently become Strings
StringMemory(n::Integer) = unsafe_wrap(Memory{UInt8}, _string_n(n))
StringVector(n::Integer) = wrap(Array, StringMemory(n))

# IOBuffers behave like Files. They are typically readable and writable. They are seekable. (They can be appendable).

"""
    IOBuffer([data::AbstractVector{UInt8}]; keywords...) -> IOBuffer

Create an in-memory I/O stream, which may optionally operate on a pre-existing array.

It may take optional keyword arguments:
- `read`, `write`, `append`: restricts operations to the buffer; see `open` for details.
- `truncate`: truncates the buffer size to zero length.
- `maxsize`: specifies a size beyond which the buffer may not be grown.
- `sizehint`: suggests a capacity of the buffer (`data` must implement `sizehint!(data, size)`).

When `data` is not given, the buffer will be both readable and writable by default.

# Examples
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.", " It has many members.")
56

julia> String(take!(io))
"JuliaLang is a GitHub organization. It has many members."

julia> io = IOBuffer(b"JuliaLang is a GitHub organization.")
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=35, maxsize=Inf, ptr=1, mark=-1)

julia> read(io, String)
"JuliaLang is a GitHub organization."

julia> write(io, "This isn't writable.")
ERROR: ArgumentError: ensureroom failed, IOBuffer is not writeable

julia> io = IOBuffer(UInt8[], read=true, write=true, maxsize=34)
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=0, maxsize=34, ptr=1, mark=-1)

julia> write(io, "JuliaLang is a GitHub organization.")
34

julia> String(take!(io))
"JuliaLang is a GitHub organization"

julia> length(read(IOBuffer(b"data", read=true, truncate=false)))
4

julia> length(read(IOBuffer(b"data", read=true, truncate=true)))
0
```
"""
function IOBuffer(
        data::AbstractVector{UInt8};
        read::Union{Bool,Nothing}=nothing,
        write::Union{Bool,Nothing}=nothing,
        append::Union{Bool,Nothing}=nothing,
        truncate::Union{Bool,Nothing}=nothing,
        maxsize::Integer=typemax(Int),
        sizehint::Union{Integer,Nothing}=nothing)
    if maxsize < 0
        throw(ArgumentError("negative maxsize"))
    end
    if sizehint !== nothing
        sizehint!(data, sizehint)
    end
    flags = open_flags(read=read, write=write, append=append, truncate=truncate)
    buf = GenericIOBuffer(data, flags.read, flags.write, true, flags.append, Int(maxsize))
    if flags.truncate
        buf.size = buf.offset
    end
    return buf
end

function IOBuffer(;
        read::Union{Bool,Nothing}=true,
        write::Union{Bool,Nothing}=true,
        append::Union{Bool,Nothing}=nothing,
        truncate::Union{Bool,Nothing}=true,
        maxsize::Integer=typemax(Int),
        sizehint::Union{Integer,Nothing}=nothing)
    size = sizehint !== nothing ? Int(sizehint) : maxsize != typemax(Int) ? Int(maxsize) : 32
    flags = open_flags(read=read, write=write, append=append, truncate=truncate)
    buf = IOBuffer(
        StringMemory(size),
        read=flags.read,
        write=flags.write,
        append=flags.append,
        truncate=flags.truncate,
        maxsize=maxsize)
    fill!(buf.data, 0)
    return buf
end

# PipeBuffers behave somewhat more like Unix Pipes (than Files). They are typically readable and writable, they act appendable, and are not seekable.
# However, they do not support stream notification, so for that there is the BufferStream wrapper around this.

"""
    PipeBuffer(data::AbstractVector{UInt8}=UInt8[]; maxsize::Integer = typemax(Int))

An [`IOBuffer`](@ref) that allows reading and performs writes by appending.
Seeking and truncating are not supported.
See [`IOBuffer`](@ref) for the available constructors.
If `data` is given, creates a `PipeBuffer` to operate on a data vector,
optionally specifying a size beyond which the underlying `Array` may not be grown.
"""
PipeBuffer(data::AbstractVector{UInt8}=Memory{UInt8}(); maxsize::Int = typemax(Int)) =
    GenericIOBuffer(data, true, true, false, true, maxsize)
PipeBuffer(maxsize::Integer) = (x = PipeBuffer(StringMemory(maxsize), maxsize = maxsize); x.size = 0; x)

_similar_data(b::GenericIOBuffer, len::Int) = similar(b.data, len)
_similar_data(b::IOBuffer, len::Int) = StringMemory(len)

function copy(b::GenericIOBuffer)
    ret = typeof(b)(b.reinit ? _similar_data(b, 0) : b.writable ?
                    copyto!(_similar_data(b, length(b.data)), b.data) : b.data,
                    b.readable, b.writable, b.seekable, b.append, b.maxsize)
    ret.size = b.size
    ret.ptr  = b.ptr
    ret.mark = b.mark
    ret.offset = b.offset
    return ret
end

show(io::IO, b::GenericIOBuffer) = print(io, "IOBuffer(data=UInt8[...], ",
                                      "readable=", b.readable, ", ",
                                      "writable=", b.writable, ", ",
                                      "seekable=", b.seekable, ", ",
                                      "append=",   b.append, ", ",
                                      "size=",     b.size - b.offset, ", ",
                                      "maxsize=",  b.maxsize == typemax(Int) ? "Inf" : b.maxsize, ", ",
                                      "ptr=",      b.ptr - b.offset, ", ",
                                      "mark=",     b.mark, ")")

@noinline function _throw_not_readable()
    # See https://github.com/JuliaLang/julia/issues/29688.
    throw(ArgumentError("read failed, IOBuffer is not readable"))
end

function unsafe_read(from::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    from.readable || _throw_not_readable()
    avail = bytesavailable(from)
    adv = min(avail, nb)
    GC.@preserve from unsafe_copyto!(p, pointer(from.data, from.ptr), adv)
    from.ptr += adv
    if nb > avail
        throw(EOFError())
    end
    nothing
end

function peek(from::GenericIOBuffer, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    from.readable || _throw_not_readable()
    avail = bytesavailable(from)
    nb = sizeof(T)
    if nb > avail
        throw(EOFError())
    end
    GC.@preserve from begin
        ptr::Ptr{T} = pointer(from.data, from.ptr)
        x = unsafe_load(ptr)
    end
    return x
end

function read(from::GenericIOBuffer, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    x = peek(from, T)
    from.ptr += sizeof(T)
    return x
end

function read_sub(from::GenericIOBuffer, a::AbstractArray{T}, offs, nel) where T
    require_one_based_indexing(a)
    from.readable || _throw_not_readable()
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbitstype(T) && isa(a,MutableByteArray)
        nb = UInt(nel * sizeof(T))
        GC.@preserve a unsafe_read(from, pointer(a, offs), nb)
    else
        for i = offs:offs+nel-1
            a[i] = read(from, T)
        end
    end
    return a
end

@inline function read(from::GenericIOBuffer, ::Type{UInt8})
    from.readable || _throw_not_readable()
    ptr = from.ptr
    size = from.size
    if ptr > size
        throw(EOFError())
    end
    @inbounds byte = from.data[ptr]::UInt8
    from.ptr = ptr + 1
    return byte
end

function peek(from::GenericIOBuffer, ::Type{UInt8})
    from.readable || _throw_not_readable()
    if from.ptr > from.size
        throw(EOFError())
    end
    return from.data[from.ptr]::UInt8
end

read(from::GenericIOBuffer, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(from, UInt))

isreadable(io::GenericIOBuffer) = io.readable
iswritable(io::GenericIOBuffer) = io.writable

filesize(io::GenericIOBuffer) = (io.seekable ? io.size - io.offset : bytesavailable(io))
bytesavailable(io::GenericIOBuffer) = io.size - io.ptr + 1
position(io::GenericIOBuffer) = io.ptr - io.offset - 1

function skip(io::GenericIOBuffer, n::Integer)
    seekto = io.ptr + n
    n < 0 && return seek(io, seekto-1) # Does error checking
    io.ptr = min(seekto, io.size+1)
    return io
end

function seek(io::GenericIOBuffer, n::Integer)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end
    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an GenericIOBuffer), so that would need to be fixed in order to throw an error here
    #(n < 0 || n > io.size) && throw(ArgumentError("Attempted to seek outside IOBuffer boundaries."))
    #io.ptr = n+1
    io.ptr = min(max(0, n)+io.offset, io.size)+1
    return io
end

function seekend(io::GenericIOBuffer)
    io.ptr = io.size+1
    return io
end

# choose a resize strategy based on whether `resize!` is defined:
# for a Vector, we use `resize!`, but for most other types,
# this calls `similar`+copy
function _resize!(io::GenericIOBuffer, sz::Int)
    a = io.data
    offset = io.offset
    if applicable(resize!, a, sz)
        if offset != 0
            size = io.size
            size > offset && copyto!(a, 1, a, offset + 1, min(sz, size - offset))
            io.ptr -= offset
            io.size -= offset
            io.offset = 0
        end
        resize!(a, sz)
    else
        size = io.size
        if size >= sz && sz != 0
            b = a
        else
            b = _similar_data(io, sz == 0 ? 0 : max(overallocation(size - io.offset), sz))
        end
        size > offset && copyto!(b, 1, a, offset + 1, min(sz, size - offset))
        io.data = b
        io.ptr -= offset
        io.size -= offset
        io.offset = 0
    end
    return io
end

function truncate(io::GenericIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.maxsize && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer maxsize $(io.maxsize)"))
    n = Int(n)
    if io.reinit
        io.data = _similar_data(io, n)
        io.reinit = false
    elseif n > length(io.data) + io.offset
        _resize!(io, n)
    end
    ismarked(io) && io.mark > n && unmark(io)
    n += io.offset
    io.data[io.size+1:n] .= 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
    return io
end

function compact(io::GenericIOBuffer)
    io.writable || throw(ArgumentError("compact failed, IOBuffer is not writeable"))
    io.seekable && throw(ArgumentError("compact failed, IOBuffer is seekable"))
    io.reinit && return
    local ptr::Int, bytes_to_move::Int
    if ismarked(io) && io.mark < position(io)
        io.mark == 0 && return
        ptr = io.mark + io.offset
        bytes_to_move = bytesavailable(io) + (io.ptr - ptr)
    else
        ptr = io.ptr
        bytes_to_move = bytesavailable(io)
    end
    copyto!(io.data, 1, io.data, ptr, bytes_to_move)
    io.size -= ptr - 1
    io.ptr -= ptr - 1
    io.offset = 0
    return
end

@noinline function ensureroom_slowpath(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    if io.reinit
        io.data = _similar_data(io, nshort % Int)
        io.reinit = false
    end
    if !io.seekable
        if !ismarked(io) && io.ptr > io.offset+1 && io.size <= io.ptr - 1
            io.ptr = 1
            io.size = 0
            io.offset = 0
        else
            datastart = (ismarked(io) ? io.mark : io.ptr - io.offset)
            if (io.size-io.offset+nshort > io.maxsize) ||
                (datastart > 4096 && datastart > io.size - io.ptr) ||
                (datastart > 262144)
                # apply somewhat arbitrary heuristics to decide when to destroy
                # old, read data to make more room for new data
                compact(io)
            end
        end
    end
    return
end

@inline ensureroom(io::GenericIOBuffer, nshort::Int) = ensureroom(io, UInt(nshort))
@inline function ensureroom(io::GenericIOBuffer, nshort::UInt)
    if !io.writable || (!io.seekable && io.ptr > io.offset+1) || io.reinit
        ensureroom_slowpath(io, nshort)
    end
    n = min((nshort % Int) + (io.append ? io.size : io.ptr-1) - io.offset, io.maxsize)
    l = length(io.data) + io.offset
    if n > l
        _resize!(io, Int(n))
    end
    return io
end

eof(io::GenericIOBuffer) = (io.ptr - 1 >= io.size)

function closewrite(io::GenericIOBuffer)
    io.writable = false
    nothing
end

@noinline function close(io::GenericIOBuffer{T}) where T
    io.readable = false
    io.writable = false
    io.seekable = false
    io.size = 0
    io.offset = 0
    io.maxsize = 0
    io.ptr = 1
    io.mark = -1
    if io.writable && !io.reinit
        io.data = _resize!(io, 0)
    end
    nothing
end

isopen(io::GenericIOBuffer) = io.readable || io.writable || io.seekable || bytesavailable(io) > 0

"""
    take!(b::IOBuffer)

Obtain the contents of an `IOBuffer` as an array. Afterwards, the `IOBuffer` is reset to its initial state.

# Examples
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.", " It has many members.")
56

julia> String(take!(io))
"JuliaLang is a GitHub organization. It has many members."
```
"""
function take!(io::GenericIOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        nbytes = io.size - io.offset
        data = copyto!(StringVector(nbytes), 1, io.data, io.offset + 1, nbytes)
    else
        nbytes = bytesavailable(io)
        data = read!(io, StringVector(nbytes))
    end
    if io.writable
        io.ptr = 1
        io.size = 0
        io.offset = 0
    end
    return data
end
function take!(io::IOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        nbytes = filesize(io)
        if nbytes == 0 || io.reinit
            data = StringVector(0)
        elseif io.writable
            data = wrap(Array, MemoryRef(io.data, io.offset + 1), nbytes)
        else
            data = copyto!(StringVector(io.size), 1, io.data, io.offset + 1, nbytes)
        end
    else
        nbytes = bytesavailable(io)
        if nbytes == 0
            data = StringVector(0)
        elseif io.writable
            data = wrap(Array, MemoryRef(io.data, io.ptr), nbytes)
        else
            data = read!(io, data)
        end
    end
    if io.writable
        io.reinit = true
        io.ptr = 1
        io.size = 0
        io.offset = 0
    end
    return data
end

"""
    _unsafe_take!(io::IOBuffer)

This simply returns the raw resized `io.data`, with no checks to be
sure that `io` is readable etcetera, and leaves `io` in an inconsistent
state.  This should only be used internally for performance-critical
`String` routines that immediately discard `io` afterwards, and it
*assumes* that `io` is writable and seekable.

It might save an allocation compared to `take!` (if the compiler elides the
Array allocation), as well as omits some checks.
"""
_unsafe_take!(io::IOBuffer) =
    wrap(Array, io.size == io.offset ?
        MemoryRef(Memory{UInt8}()) :
        MemoryRef(io.data, io.offset + 1),
        io.size - io.offset)

function write(to::IO, from::GenericIOBuffer)
    written::Int = bytesavailable(from)
    if to === from
        from.ptr = from.size + 1
    else
        written = GC.@preserve from unsafe_write(to, pointer(from.data, from.ptr), UInt(written))
        from.ptr += written
    end
    return written
end

function unsafe_write(to::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    ensureroom(to, nb)
    ptr = (to.append ? to.size+1 : to.ptr)
    written = Int(min(nb, Int(length(to.data))::Int - ptr + 1))
    towrite = written
    d = to.data
    while towrite > 0
        @inbounds d[ptr] = unsafe_load(p)
        ptr += 1
        p += 1
        towrite -= 1
    end
    to.size = max(to.size, ptr - 1)
    if !to.append
        to.ptr += written
    end
    return written
end

@inline function write(to::GenericIOBuffer, a::UInt8)
    ensureroom(to, UInt(1))
    ptr = (to.append ? to.size+1 : to.ptr)
    if ptr > to.maxsize
        return 0
    else
        to.data[ptr] = a
    end
    to.size = max(to.size, ptr)
    if !to.append
        to.ptr += 1
    end
    return sizeof(UInt8)
end

const MutableByteArray = Union{
    Array{UInt8},
    Memory{UInt8},
    FastContiguousSubArray{UInt8,<:Any,<:Array{UInt8}},
    FastContiguousSubArray{UInt8,<:Any,<:Memory{UInt8}},
}

readbytes!(io::GenericIOBuffer, b::MutableByteArray, nb=length(b)) = readbytes!(io, b, Int(nb))
function readbytes!(io::GenericIOBuffer, b::MutableByteArray, nb::Int)
    nr = min(nb, bytesavailable(io))
    if length(b) < nr
        resize!(b, nr)
    end
    read_sub(io, b, 1, nr)
    return nr
end
read(io::GenericIOBuffer) = read!(io, StringVector(bytesavailable(io)))
readavailable(io::GenericIOBuffer) = read(io)
read(io::GenericIOBuffer, nb::Integer) = read!(io, StringVector(min(nb, bytesavailable(io))))

function occursin(delim::UInt8, buf::IOBuffer)
    p = pointer(buf.data, buf.ptr)
    q = GC.@preserve buf ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, delim, bytesavailable(buf))
    return q != C_NULL
end

function occursin(delim::UInt8, buf::GenericIOBuffer)
    data = buf.data
    for i = buf.ptr:buf.size
        @inbounds b = data[i]
        b == delim && return true
    end
    return false
end

function copyuntil(out::IO, io::GenericIOBuffer, delim::UInt8; keep::Bool=false)
    data = view(io.data, io.ptr:io.size)
    # note: findfirst + copyto! is much faster than a single loop
    #       except for nout ≲ 20.  A single loop is 2x faster for nout=5.
    nout = nread = something(findfirst(==(delim), data), length(data))
    if !keep && nout > 0 && data[nout] == delim
        nout -= 1
    end
    write(out, view(io.data, io.ptr:io.ptr+nout-1))
    io.ptr += nread
    return out
end

function copyline(out::GenericIOBuffer, s::IO; keep::Bool=false)
    copyuntil(out, s, 0x0a, keep=true)
    line = out.data
    i = out.size # XXX: this is only correct for appended data. if the data was inserted, only ptr should change
    if keep || i == out.offset || line[i] != 0x0a
        return out
    elseif i < 2 || line[i-1] != 0x0d
        i -= 1
    else
        i -= 2
    end
    out.size = i
    if !out.append
        out.ptr = i+1
    end
    return out
end

function _copyline(out::IO, io::GenericIOBuffer; keep::Bool=false)
    data = view(io.data, io.ptr:io.size)
    # note: findfirst + copyto! is much faster than a single loop
    #       except for nout ≲ 20.  A single loop is 2x faster for nout=5.
    nout = nread = something(findfirst(==(0x0a), data), length(data))
    if !keep && nout > 0 && data[nout] == 0x0a
        nout -= 1
        nout > 0 && data[nout] == 0x0d && (nout -= 1)
    end
    write(out, view(io.data, io.ptr:io.ptr+nout-1))
    io.ptr += nread
    return out
end
copyline(out::IO, io::GenericIOBuffer; keep::Bool=false) = _copyline(out, io; keep)
copyline(out::GenericIOBuffer, io::GenericIOBuffer; keep::Bool=false) = _copyline(out, io; keep)


# copy-free crc32c of IOBuffer:
function _crc32c(io::IOBuffer, nb::Integer, crc::UInt32=0x00000000)
    nb < 0 && throw(ArgumentError("number of bytes to checksum must be ≥ 0, got $nb"))
    io.readable || _throw_not_readable()
    n = min(nb, bytesavailable(io))
    n == 0 && return crc
    crc = GC.@preserve io unsafe_crc32c(pointer(io.data, io.ptr), n, crc)
    io.ptr += n
    return crc
end
_crc32c(io::IOBuffer, crc::UInt32=0x00000000) = _crc32c(io, bytesavailable(io), crc)

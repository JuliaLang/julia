# This file is a part of Julia. License is MIT: https://julialang.org/license

## work with AbstractVector{UInt8} via I/O primitives ##

# Here, u represents used bytes (already read), X represents bytes still to read,
# - represents bytes uninitialized data but which can be written to later,
# and . represents content of `data` which the IOBuffer will not interact with.

#   ...uuuuuuuuuuuuuXXXXXXXXXXXXX------.......
#   | |        |    |           |     |     |
#   | offset   |    ptr        size   |     |
#   1         mark                    |     lastindex(data)
#                                     maxsize

#            AFTER COMPACTION
# Mark, ptr and size decreases by (mark - offset - 1)

#   ...uuuuuXXXXXXXXXXXXX--------------.......
#   | ||    |           |             |      |
#   | offset|           size          |      lastindex(data)
#   1  |    ptr                       maxsize
#      mark ( == offset + 1)

# * The underlying array is always 1-indexed
# * Data in 1:offset is never touched. This allows an IOBuffer to use a subset of a larger array.
# * Data in maxsize+1:lastindex(data) is also never touched.
# * Data in offset+1:mark-1, if mark > 0 can be deleted, shifting the whole thing to the left
#   to make room for more data, without replacing or resizing data

mutable struct GenericIOBuffer{T<:AbstractVector{UInt8}} <: IO
    # T should support: getindex, setindex!, length, copyto!, similar, and (optionally) resize!
    data::T

    # The user can take control of `data` out of this struct. When that happens, instead of eagerly allocating
    # a new array, we set `.reinit` to true, and then allocate a new one when needed.
    reinit::Bool
    readable::Bool
    writable::Bool

    # If not seekable, implementation is free to destroy (compact) data in offset+1:mark-1
    seekable::Bool

    # If true, write new data to the index size+1 instead of the index ptr.
    append::Bool

    # Last index of `data` that has been written to. Data in size+1:end has not yet been used,
    # and may contain arbitrary values.
    # This value is always in offset : min(maxsize, lastindex(data))
    size::Int

    # Size can never be larger than this value. This is useful for two use cases:
    # 1. To prevent `data` from becoming too large, using too much memory
    # 2. To guarantee that data in maxsize+1:lastindex(data) is never touched
    # Note that this can be larger than lastindex(data), since data may be replaced.
    # This value is always in 0:typemax(Int)
    maxsize::Int

    # Data is read/written from/to ptr, except in situations where append is true, in which case
    # data is still read from ptr, but written to size+1.
    # This value is alwaus in offset+1 : size+1
    ptr::Int

    # When constructed from a Vector `v`, `data` is its underlying memory, but the memory may contain
    # data before v[1]. In that case, the offset is the number of leading bytes in the memory that
    # the IO may never touch.
    # This value is always in 0:lastindex(data)
    offset::Int

    # Data before the marked location for non-seekable buffers can be compacted.
    # If this is -1, the mark is not set.
    # The purpose of the mark is to reset the stream to a given position using reset.
    # This value is always ==  -1, or in offset+1:size
    mark::Int

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

# For this method, we use the underlying Memory of the vector. Therefore, we need to set the offset,
# ptr and size accordingly, so the buffer only uses the part of the memory that the vector does.
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

!!! warning "Passing `data` as scratch space to `IOBuffer` with `write=true` may give unexpected behavior"
    Once `write` is called on an `IOBuffer`, it is best to consider any
    previous references to `data` invalidated; in effect `IOBuffer` "owns"
    this data until a call to `take!`. Any indirect mutations to `data`
    could lead to undefined behavior by breaking the abstractions expected
    by `IOBuffer`. If `write=true` the IOBuffer may store data at any
    offset leaving behind arbitrary values at other offsets. If `maxsize > length(data)`,
    the IOBuffer might re-allocate the data entirely, which
    may or may not be visible in any outstanding bindings to `array`.
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
        # A common usecase of IOBuffer is to incrementally construct strings. By using StringMemory
        # as the default storage, we can turn the result into a string without copying.
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
    unsafe_read!(p, from.data, from.ptr, adv)
    from.ptr += adv
    if nb > avail
        throw(EOFError())
    end
    nothing
end

function unsafe_read!(dest::Ptr{UInt8}, src::AbstractVector{UInt8}, so::Integer, nbytes::UInt)
    for i in 1:nbytes
        unsafe_store!(dest, @inbounds(src[so+i-1]), i)
    end
end

# Note: Currently, CodeUnits <: DenseVector, which makes this union redundant w.r.t
# DenseArrayType{UInt8}, but this is a bug, and may be removed in future versions
# of Julia. See #54002
const DenseBytes = Union{
    <:DenseArrayType{UInt8},
    CodeUnits{UInt8, <:Union{String, SubString{String}}},
}

function unsafe_read!(dest::Ptr{UInt8}, src::DenseBytes, so::Integer, nbytes::UInt)
    GC.@preserve src unsafe_copyto!(dest, pointer(src, so), nbytes)
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
    # Use memcpy where applicable for performance
    if isa(a, MutableDenseArrayType{UInt8})
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

# Number of bytes that can be read from the buffer, if you seek to the start first.
filesize(io::GenericIOBuffer) = (io.seekable ? io.size - io.offset : bytesavailable(io))

# Number of bytes that can be read from the buffer.
bytesavailable(io::GenericIOBuffer) = io.size - io.ptr + 1

# Position is zero-indexed, but ptr is one-indexed, hence the -1
position(io::GenericIOBuffer) = io.ptr - io.offset - 1

function skip(io::GenericIOBuffer, n::Integer)
    skip(io, clamp(n, Int))
end

function skip(io::GenericIOBuffer, n::Int)
    # In both cases, the result will never go to before the first position,
    # nor beyond the last position, and will not throw an error unless the stream
    # is not seekable and try to skip a negative number of bytes.
    if signbit(n)
        # Skipping a negative number of bytes is equivalent to seeking backwards.
        seekto = clamp(widen(position(io)) + widen(n), Int)
        seek(io, seekto) # Does error checking
    else
        # Don't use seek in order to allow a non-seekable IO to still skip bytes.
        n_max = io.size + 1 - io.ptr
        io.ptr += min(n, n_max)
        io
    end
end

function seek(io::GenericIOBuffer, n::Integer)
    seek(io, clamp(n, Int))
end

function seek(io::GenericIOBuffer, n::Int)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end
    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an GenericIOBuffer), so that would need to be fixed in order to throw an error here
    #(n < 0 || n > io.size - io.offset) && throw(ArgumentError("Attempted to seek outside IOBuffer boundaries."))
    #io.ptr = n + io.offset + 1
    io.ptr = clamp(n, 0, io.size - io.offset) + io.offset + 1
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
        # TODO: This is buggy: The buffer should never touch data before the offset.
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
        # Make a new data buffer, only if there is not room in existing buffer
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

# Internal method. Delete used data in the buffer, shifting the rest to the left.
# Does not delete data at or after the mark, nor data before the offset, nor data
# after ptr.
function compact(io::GenericIOBuffer)
    # For a seekable buffer, the user could always seek back to the used data.
    # Therefore, it is invalid to compact a seekable buffer
    io.writable || throw(ArgumentError("compact failed, IOBuffer is not writeable"))
    io.seekable && throw(ArgumentError("compact failed, IOBuffer is seekable"))
    #  If the data is invalid and needs to be replaced, no point in compacting
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
    # TODO: Invalid: Buffer must not touch data before offset
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
        # If the buffer is seekable, then the previously consumed bytes from ptr+1:size
        # must still be output, as they are not truly gone.
        # Hence, we output all bytes from offset+1:io.size
        nbytes = io.size - io.offset
        data = copyto!(StringVector(nbytes), 1, io.data, io.offset + 1, nbytes)
    else
        # Else, if not seekable, bytes from offset+1:ptr-1 are truly gone and should not
        # be output. Hence, we output `bytesavailable`, which is ptr:size
        nbytes = bytesavailable(io)
        data = read!(io, StringVector(nbytes))
    end
    # TODO: Why do we not reinit here? The user has taken control of the buffer
    # so it's not safe to hold onto it.
    if io.writable
        io.ptr = 1
        io.size = 0
        io.offset = 0
    end
    return data
end

# This method is specialized because we know the underlying data is a Memory, so we can
# e.g. wrap directly in an array without copying. Otherwise the logic is the same as
# the generic method
function take!(io::IOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        nbytes = filesize(io)
        if nbytes == 0 || io.reinit
            data = StringVector(0)
        elseif io.writable
            data = wrap(Array, memoryref(io.data, io.offset + 1), nbytes)
        else
            data = copyto!(StringVector(nbytes), 1, io.data, io.offset + 1, nbytes)
        end
    else
        nbytes = bytesavailable(io)
        if nbytes == 0
            data = StringVector(0)
        elseif io.writable
            data = wrap(Array, memoryref(io.data, io.ptr), nbytes)
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
        memoryref(Memory{UInt8}()) :
        memoryref(io.data, io.offset + 1),
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
    # TODO: This inbounds is unsafe, since the underlying data may be of any type
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

# TODO: We should have a method that uses memcpy (`copyto!`) for the IOBuffer case.
# Preliminary testing suggests this would be ~10x faster than the current implementation

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

readbytes!(io::GenericIOBuffer, b::MutableDenseArrayType{UInt8}, nb=length(b)) = readbytes!(io, b, Int(nb))
function readbytes!(io::GenericIOBuffer, b::MutableDenseArrayType{UInt8}, nb::Int)
    nr = min(nb, bytesavailable(io))
    if length(b) < nr
        resize!(b, nr)
    end
    read_sub(io, b, 1, nr)
    return nr
end
read(io::GenericIOBuffer) = read!(io, StringVector(bytesavailable(io)))

# For IO buffers, all the data is immediately available.
readavailable(io::GenericIOBuffer) = read(io)

read(io::GenericIOBuffer, nb::Integer) = read!(io, StringVector(min(nb, bytesavailable(io))))

function occursin(delim::UInt8, buf::IOBuffer)
    # TODO: This pointer call is invalid. Also, perhaps this should use the default `in` method,
    # which already should be implemented using memchr.
    p = pointer(buf.data, buf.ptr)
    q = GC.@preserve buf ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, delim, bytesavailable(buf))
    return q != C_NULL
end

# TODO: Invalid use of @inbounds, and also we can use the generic `in` method.
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
    # TODO: This computation seems to be invalid. The buffer size may be smaller than out.size
    # due to the presence of out.offset.
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
    nout = nread = something(findfirst(==(0x0a), data), length(data))::Int
    # Remove the 0x0a (newline) if not keep, and also remove the 0x0d (\r) if it is there
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

# This file is a part of Julia. License is MIT: https://julialang.org/license

# IOBuffer is a Memory{UInt8} backed IO type for in-memory IO.

# Here, u represents used bytes (already read), X represents bytes still to read,
# - represents bytes uninitialized data but which can be written to later.
# . represents bytes before offset, which the buffer will not touch, until
# a write operation happens.

#   .....uuuuuuuuuuuuuXXXXXXXXXXXXX------------
#   |   |            |           |            |    |
#   |   offset       ptr         size         |    maxsize (≥ lastindex)
#   1                                         lastindex(data)

# N.B: `mark` does not correspond to any index in the buffer, but is instead equal
# to position (== io.offset + io.ptr - 1) at the time it is set.

#            AFTER COMPACTION

#   uuuuuXXXXXXXXXXXXX---------------------
#  ||    |           |                    |    |
#  |1    ptr         size                 |    maxsize (≥ lastindex)
#  |                                      lastindex(data)
#  offset (set to zero)

# * The underlying array is always 1-indexed
# * The IOBuffer has full control (ownership) of the underlying array, only when
#   buffer.write == true.
# * Data before the mark can be deleted, shifting the whole thing to the left
#   to make room for more data, without replacing or resizing data.
#   This can be done only if the buffer is not seekable

mutable struct GenericIOBuffer{T<:AbstractVector{UInt8}} <: IO
    # T should support: getindex, setindex!, length, copyto!, similar, and (optionally) resize!
    data::T

    # The user can take control of `data` out of this struct. When that happens, instead of eagerly allocating
    # a new array, we set `.reinit` to true, and then allocate a new one when needed.
    # If reinit is true, the buffer is writable, and offset and size is zero. See `take!`
    reinit::Bool
    readable::Bool
    writable::Bool

    # If not seekable, implementation is free to destroy (compact) data in 1:mark-1.
    # If it IS seekable, the user may always recover any data in 1:size by seeking,
    # so no data can be destroyed.
    # Non-seekable IOBuffers can only be constructed with `PipeBuffer`, which are writable,
    # readable and append.
    seekable::Bool

    # If true, write new data to the index size+1 instead of the index ptr.
    append::Bool

    # Last index of `data` that has been written to. Data in size+1:end has not yet been used,
    # and may contain arbitrary values.
    # This value is always in 0 : lastindex(data)
    size::Int

    # When the buffer is resized, or a new buffer allocated, this is the maximum size of the buffer.
    # A new GenericIOBuffer may be constructed with an existing data larger than `maxsize`.
    # When that happens, the buffer will not write to data in maxsize + 1 : lastindex(data).
    # This value is always in 0:typemax(Int).
    maxsize::Int

    # Data is read/written from/to ptr, except in situations where append is true, in which case
    # data is still read from ptr, but written to size+1.
    # This value is always in offset + 1 : size+1
    ptr::Int

    # This is used when seeking. seek(io, 0) results in ptr == offset.
    # The offset is needed because, if a buffer is instantiated from a Vector with a non-zero
    # memory offset, the start of the vector, and thus the start of data, does not correspond
    # to the start of its underlying memory.
    # Once the offset is set to zero, it will never be set to nonzero.
    # This is always in 0:lastindex(data)
    offset::Int

    # mark is the position (as given by `position`, i.e. io.ptr - io.offset - 1)
    # which can be seeked back using `reset`, even for non-seekable buffers.
    # For non-seekable buffers that can be compacted, data before the mark can be
    # destroyed.
    # This value is always in -1 : size - offset
    mark::Int

    # Unsafe constructor which does not do any checking
    global function _new_generic_iobuffer(
            ::Type{T},
            data::T,
            readable::Bool,
            writable::Bool,
            seekable::Bool,
            append::Bool,
            maxsize::Int,
        ) where T<:AbstractVector{UInt8}
        len = Int(length(data))::Int
        return new{T}(data, false, readable, writable, seekable, append, len, maxsize, 1, 0, -1)
    end
end

function GenericIOBuffer{T}(
        data::T,
        readable::Bool,
        writable::Bool,
        seekable::Bool,
        append::Bool,
        maxsize::Integer,
    ) where T<:AbstractVector{UInt8}
    require_one_based_indexing(data)
    mz = Int(maxsize)::Int
    len = Int(length(data))::Int
    if mz < len
        throw(ArgumentError("maxsize must not be smaller than data length"))
    end
    return _new_generic_iobuffer(T, data, readable, writable, seekable, append, mz)
end

const IOBuffer = GenericIOBuffer{Memory{UInt8}}

function GenericIOBuffer(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer) where T<:AbstractVector{UInt8}
    GenericIOBuffer{T}(data, readable, writable, seekable, append, maxsize)
end

# For this method, we use the underlying Memory of the vector. Therefore, we need to set the,
# ptr and size accordingly, so the buffer only uses the part of the memory that the vector does.
function GenericIOBuffer(data::Vector{UInt8}, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer)
    ref = data.ref
    mem = ref.mem
    offset = memoryrefoffset(ref) - 1
    # The user may pass a vector of length <= maxsize, but where the underlying memory
    # is larger than maxsize. Don't throw an error in that case.
    mz = Int(maxsize)::Int
    if mz < length(data)
        throw(ArgumentError("maxsize must not be smaller than data length"))
    end
    buf = _new_generic_iobuffer(Memory{UInt8}, mem, readable, writable, seekable, append, mz)
    buf.size = length(data) + offset
    buf.ptr = offset + 1
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
    if sizehint !== nothing
        sizehint!(data, sizehint)
    end
    flags = open_flags(read=read, write=write, append=append, truncate=truncate)
    buf = GenericIOBuffer(data, flags.read, flags.write, true, flags.append, maxsize)
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
        sizehint::Union{Integer,Nothing}=nothing,
    )
    mz = Int(maxsize)::Int
    if mz < 0
        throw(ArgumentError("negative maxsize"))
    end
    size = if sizehint !== nothing
        # Allow negative sizehint, just like `sizehint!` does
        min(mz, max(0, Int(sizehint)::Int))
    else
        min(mz, 32)
    end
    flags = open_flags(read=read, write=write, append=append, truncate=truncate)
    # A common usecase of IOBuffer is to incrementally construct strings. By using StringMemory
    # as the default storage, we can turn the result into a string without copying.
    buf = _new_generic_iobuffer(Memory{UInt8}, StringMemory(size), flags.read, flags.write, true, flags.append, mz)
    buf.size = 0
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

# Note: Copying may change the value of the position (and mark) for un-seekable streams.
# However, these values are not stable anyway due to compaction.

function copy(b::GenericIOBuffer{T}) where T
    if b.reinit
        # If buffer is used up, allocate a new size-zero buffer
        # Reinit implies wriable, and that ptr, size, offset and mark are already the default values
        return typeof(b)(_similar_data(b, 0), b.readable, b.writable, b.seekable, b.append, b.maxsize)
    elseif b.writable
        # Else, we just copy the reachable bytes. If buffer is seekable, all bytes
        # after offset are reachable, since they can be seeked to
        used_span = if b.seekable
            b.offset + 1 : b.size
        else
            # Even non-seekable streams can be seeked using `reset`. Therefore, we need to
            # copy all data from mark if it's set and below ptr.
            (b.mark > -1 ? min(b.ptr, b.mark) : b.ptr) : b.size
        end
        len = length(used_span)
        data = copyto!(_similar_data(b, len), view(b.data, used_span))
        ret = typeof(b)(data, b.readable, b.writable, b.seekable, b.append, b.maxsize)
        ret.size = len
        ret.offset = 0
        ret.ptr = b.ptr - first(used_span) + 1
        ret.mark = b.mark < 0 ? -1 : (b.mark - first(used_span) + 1)
        return ret
    else
        # When the buffer is just readable, they can share the same data, so we just make
        # a shallow copy of the IOBuffer struct.
        # Use internal constructor because we want to allow b.maxsize to be larger than data,
        # in case that is the case for `b`.
        ret = _new_generic_iobuffer(T, b.data, b.readable, b.writable, b.seekable, b.append, b.maxsize)
        ret.offset = b.offset
        ret.ptr = b.ptr
        ret.mark = b.mark
        return ret
    end
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
    avail = bytesavailable(from) % UInt
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

const MultiByteBitNumberType = Union{
    Type{UInt16},
    Type{Int16},
    Type{UInt32},
    Type{Int32},
    Type{UInt64},
    Type{Int64},
    Type{UInt128},
    Type{Int128},
    Type{Float16},
    Type{Float32},
    Type{Float64},
}

function load_from_array(T::MultiByteBitNumberType, data::AbstractArray{UInt8}, from::Int)
    x = if T <: AbstractFloat
        uinttype(T)(0)
    else
        unsigned(T)(0)
    end
    for i in 0:sizeof(x)-1
        x |= typeof(x)(data[from + i]) << (8 * i)
    end
    reinterpret(T, ltoh(x))
end

function peek(from::GenericIOBuffer, T::MultiByteBitNumberType)
    from.readable || _throw_not_readable()
    avail = bytesavailable(from)
    nb = sizeof(T)
    if nb > avail
        throw(EOFError())
    end
    return load_from_array(T, from.data, from.ptr)
end

# This method can use a pointer, since the underlying buffer is dense
# and memory backed
function peek(from::GenericIOBuffer{<:MutableDenseArrayType}, T::MultiByteBitNumberType)
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

function read(from::GenericIOBuffer, T::MultiByteBitNumberType)
    x = peek(from, T)
    from.ptr += sizeof(T)
    return x
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
# TODO: Document that position for an unseekable stream is invalid, or
# make it error
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
        # Handle overflow
        io.ptr = min(io.size + 1, clamp(widen(io.ptr) + widen(n), Int))
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

# TODO: Should check for seekable and error if not
function seekend(io::GenericIOBuffer)
    io.ptr = io.size+1
    return io
end

# Resize data to exactly size `sz`. Either resize the underlying data,
# or allocate a new one and copy.
# This should only be called after the offset is zero - any operation which calls
# _resize! should reset offset before so.
function _resize!(io::GenericIOBuffer, new_size::Int)
    old_data = io.data
    if applicable(resize!, old_data, new_size)
        resize!(old_data, new_size)
    else
        size = io.size
        # Make a new data buffer, only if there is not room in existing buffer
        if size >= new_size && !iszero(new_size)
            new_data = old_data
        else
            new_data = _similar_data(io, new_size)
            io.data = new_data
        end
        size > 0 && copyto!(new_data, 1, old_data, 1, min(new_size, size))
    end
    return io
end

# TODO: These errors cannot be converted to LazyString, but it's wasteful to interpolate them here.
function truncate(io::GenericIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    # Non-seekable buffers can only be constructed with `PipeBuffer`, which is explicitly
    # documented to not be truncatable.
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.maxsize && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer maxsize $(io.maxsize)"))
    n = Int(n)::Int
    if io.reinit
        io.data = _similar_data(io, n)
        io.reinit = false
    elseif n > min(io.maxsize, length(io.data)) - io.offset
        # We zero the offset here because that allows us to minimize the resizing,
        # saving memory.
        zero_offset!(io)
        n > min(io.maxsize, length(io.data)) && _resize!(io, n)
    end
    # Since mark is zero-indexed, we must also clear it if they're equal
    ismarked(io) && io.mark >= n && (io.mark = -1)
    io.data[io.size+1:n+io.offset] .= 0
    io.size = n + io.offset
    io.ptr = min(io.ptr, n+io.offset+1)
    return io
end

# Ensure that the buffer has room for at least `nshort` more bytes, except when
# doing that would exceed maxsize.
@inline ensureroom(io::GenericIOBuffer, nshort::Int) = ensureroom(io, UInt(nshort))

@inline function ensureroom(io::GenericIOBuffer, nshort::UInt)
    # If the IO is not writable, we call the slow path only to error.
    # If reinit, the data has been handed out to the user, and the IOBuffer
    # no longer controls it, so we need to allocate a new one.
    if !io.writable || io.reinit
        return ensureroom_reallocate(io, nshort)
    end
    # The fast path here usually checks there is already room, then does nothing.
    # When append is true, new data is added after io.size, not io.ptr
    existing_space = min(lastindex(io.data), io.maxsize) - (io.append ? io.size : io.ptr - 1)
    if existing_space < nshort % Int
        # Outline this function to make it more likely that ensureroom inlines itself
        return ensureroom_slowpath(io, nshort)
    end
    return io
end

# Throw error (placed in this function to outline it) or reinit the buffer
@noinline function ensureroom_reallocate(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    io.data = _similar_data(io, min(io.maxsize, nshort % Int))
    io.reinit = false
    io.offset = 0
    return io
end

@noinline function ensureroom_slowpath(io::GenericIOBuffer, nshort::UInt)
    # Begin by zeroing out offset and check if that gives us room enough
    if !iszero(io.offset)
        nshort_after_zero_offset = (nshort % Int) - zero_offset!(io)
        nshort_after_zero_offset < 1 && return io
        nshort = nshort_after_zero_offset % UInt
    end

    data_len = min(lastindex(io.data), io.maxsize)

    # Else, try to compact the data. To do this, the buffer must not be seekable.
    # If it's seekable, the user can recover used data by seeking before ptr,
    # and so we can't delete it.
    if (!io.seekable && io.ptr > 1)
        ptr = io.ptr
        mark = io.mark
        size = io.size
        data = io.data
        to_delete = (mark > -1 ? min(mark, ptr - 1) : ptr - 1)
        # Only shift data if:
        if (
                # It will prevent us from having to resize buffer, or
                to_delete >= nshort % Int ||
                # We will recover at least 256 bytes, and at least 1/8th
                # of the data buffer's total length
                (to_delete > data_len >>> 3 && to_delete > 255)
            )
            copyto!(data, 1, data, to_delete + 1, size - to_delete)
            io.ptr = ptr - to_delete
            io.mark = max(-1, mark - to_delete)
            io.size = size - to_delete
        end
        nshort -= min(nshort, to_delete % UInt)
        iszero(nshort) && return io
    end
    # Don't exceed maxsize. Otherwise, we overshoot the number of bytes needed,
    # such that we don't need to resize too often.
    new_size = min(io.maxsize, overallocation(data_len + nshort % Int))
    _resize!(io, new_size)
    return io
end

function zero_offset!(io::GenericIOBuffer)::Int
    offset = io.offset
    iszero(offset) && return 0
    size = io.size
    if size != offset
        data = io.data
        unsafe_copyto!(data, 1, data, offset + 1, size - offset)
    end
    io.offset = 0
    io.ptr -= offset
    io.size -= offset
    return offset
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
    io.maxsize = 0
    io.ptr = 1
    io.mark = -1
    io.offset = 0
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
    io.mark = -1
    if io.seekable
        # If the buffer is seekable, then the previously consumed bytes from ptr+1:size
        # must still be output, as they are not truly gone.
        # Hence, we output all bytes from 1:io.size
        nbytes = io.size - io.offset
        data = copyto!(StringVector(nbytes), 1, io.data, io.offset + 1, nbytes)
    else
        # Else, if not seekable, bytes from 1:ptr-1 are truly gone and should not
        # be output. Hence, we output `bytesavailable`, which is ptr:size
        nbytes = bytesavailable(io)
        data = read!(io, StringVector(nbytes))
    end
    if io.writable
        io.reinit = true
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
    # This would cause an infinite loop, as it should read until the end, but more
    # data is being written into it continuously.
    if to === from
        throw(ArgumentError("Writing all content fron an IOBuffer into itself in invalid"))
    else
        available = bytesavailable(from)
        written = GC.@preserve from unsafe_write(to, pointer(from.data, from.ptr), UInt(available))
        from.ptr = from.size + 1
    end
    return written
end

function unsafe_write(to::GenericIOBuffer, p::Ptr{UInt8}, nb::UInt)
    ensureroom(to, nb)
    size = to.size
    append = to.append
    ptr = append ? size+1 : to.ptr
    data = to.data
    to_write = min(nb, (min(Int(length(data))::Int, to.maxsize) - ptr + 1) % UInt) % Int
    # Dispatch based on the type of data, to possibly allow using memcpy
    _unsafe_write(data, p, ptr, to_write % UInt)
    # Update to.size only if the ptr has advanced to higher than
    # the previous size. Otherwise, we just overwrote existing data
    to.size = max(size, ptr + to_write - 1)
    # If to.append, we only update size, not ptr.
    if !append
        to.ptr = ptr + to_write
    end
    return to_write
end

@inline function _unsafe_write(data::AbstractVector{UInt8}, p::Ptr{UInt8}, from::Int, nb::UInt)
    for i in 0:nb-1
        data[from + i] = unsafe_load(p)
        p += 1
    end
end

@inline function _unsafe_write(data::MutableDenseArrayType{UInt8}, p::Ptr{UInt8}, from::Int, nb::UInt)
    # Calling `unsafe_copyto!` is very efficient for large arrays, but has some overhead
    # for small (< 5 bytes) arrays.
    # Since a common use case of IOBuffer is to construct strings incrementally, often
    # one char at a time, it's crucial to be fast in the case of small arrays.
    # This optimization only gives a minor 10% speed boost in the best case.
    if nb < 5
        @inbounds for i in UInt(1):nb
            data[from + (i % Int) - 1] = unsafe_load(p, i)
        end
    else
        GC.@preserve data begin
            ptr = Ptr{UInt8}(pointer(data, from))::Ptr{UInt8}
            @inline unsafe_copyto!(ptr, p, nb)
        end
    end
end

@inline function write(to::GenericIOBuffer, a::UInt8)
    ensureroom(to, UInt(1))
    ptr = (to.append ? to.size+1 : to.ptr)
    # We have just ensured there is room for 1 byte, EXCEPT if we were to exceed
    # maxsize. So, we just need to check that here.
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
    io.readable || _throw_not_readable()
    to_read = min(nb, bytesavailable(io))
    if length(b) < to_read
        resize!(b, to_read)
    end
    checkbounds(b, 1:to_read)
    GC.@preserve b unsafe_read(io, pointer(b), to_read)
    to_read
end
read(io::GenericIOBuffer) = read!(io, StringVector(bytesavailable(io)))

# For IO buffers, all the data is immediately available.
readavailable(io::GenericIOBuffer) = read(io)

read(io::GenericIOBuffer, nb::Integer) = read!(io, StringVector(min(nb, bytesavailable(io))))

function occursin(delim::UInt8, buf::GenericIOBuffer)
    return in(delim, view(buf.data, buf.ptr:buf.size))
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
    # If the data is copied into the middle of the buffer of `out` instead of appended to the end,
    # and !keep, and the line copied ends with \r\n, then the copyuntil (even if keep=false)
    # will overwrite one too many bytes with the new \r byte.
    # Work around this by making a new temporary buffer.
    # Could perhaps be done better
    if !out.append && out.ptr < out.size + 1
        newbuf = IOBuffer()
        copyuntil(newbuf, s, 0x0a, keep=true)
        v = take!(newbuf)
        # Remove \r\n or \n if present
        if !keep
            if length(v) > 1 && last(v) == UInt8('\n')
                pop!(v)
            end
            if length(v) > 1 && last(v) == UInt8('\r')
                pop!(v)
            end
        end
        write(out, v)
        return out
    else
        # Else, we can just copy the data directly into the buffer, and then
        # subtract the last one or two bytes depending on `keep`.
        copyuntil(out, s, 0x0a, keep=true)
        line = out.data
        i = out.size
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

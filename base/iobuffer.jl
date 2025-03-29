# This file is a part of Julia. License is MIT: https://julialang.org/license

# IOBuffer is a Memory{UInt8} backed IO type for in-memory IO.

# Here, u represents used bytes (already read), X represents bytes still to read,
# - represents bytes uninitialized data but which can be written to later.
# . represents bytes before offset, which the buffer will not touch, until
# a write operation happens.

#   .....uuuuuuuuuuuuuXXXXXXXXXXXXX------------
#   |   |            |            |           |    |
#   |   offset       ptr         size         |    maxsize
#   1                                         lastindex(data)

# N.B: `mark` does not correspond to any index in the buffer. Instead, it stores
# the mark at virtual offset in the buffer.

#            AFTER COMPACTION

#   XXXXXXXXXXXXX--------------------------
#  ||    |           |                    |    |
#  |1    ptr         size                 |    maxsize
#  |                                      lastindex(data)
#  offset (set to zero)

# * The underlying array is always 1-indexed
# * The IOBuffer has full control (ownership) of the underlying array, only when
#   buffer.write == true.
# * Unreachable data can be deleted in the buffer's data, shifting the whole thing to the left
#   to make room for more data, without replacing or resizing data.
#   This can be done only if the buffer is not seekable

mutable struct GenericIOBuffer{T<:AbstractVector{UInt8}} <: IO
    # T should support: getindex, setindex!, length, copyto!, similar, size and (optionally) resize!
    data::T

    # The user can take control of `data` out of this struct. When that happens, instead of eagerly allocating
    # a new array, we set `.reinit` to true, and then allocate a new one when needed.
    # If reinit is true, the buffer is writable, and offset_or_compacted and size is zero. See `take!`
    reinit::Bool
    readable::Bool
    writable::Bool

    # If not seekable, implementation is free to destroy (compact) data before ptr, unless
    # it can be recovered using the mark by using `reset`.
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
    # When that happensm we must make sure to not have more than `maxsize` bytes in the buffer,
    # else reallocating will lose data. So, never write to indices > `maxsize + get_offset(io)`
    # This value is always in 0:typemax(Int).
    maxsize::Int

    # Data is read/written from/to ptr, except in situations where append is true, in which case
    # data is still read from ptr, but written to size+1.
    # This value is always in offset + 1 : size+1
    ptr::Int

    # This field has two distinct meanings:
    # If the value is positive, it encodes an offset of the start of the data in `data`.
    # This is used if the buffer is instantiated from a Vector with non-zero memory offset.
    # Then, the IOBuffer stores the underlying memory, and so the first data in the buffer
    # is not at index 1.
    # If the value is negative, then `-io.offset_or_compacted` gets the number of compacted
    # bytes. That's the number of unused bytes deleted from a non-seekable stream to make space.
    # We need to keep track of it in order to make `mark` and `position` etc work, that is,
    # we need to know the virtual position of the mark even when an arbitrary number
    # of unused bytes has been deleted due to compaction.
    # Since compaction will move data in the buffer and thereby zero the offset, either the
    # offset or the number of compacted bytes will be zero at any point, so both can be
    # stored in one field.
    # If offset: Value is always in 0:lastindex(data)
    # If compacted: Value is in typemin(Int):0
    offset_or_compacted::Int

    # The mark is -1 if not set, else the zero-indexed virtual position of ptr in the buffer.
    # Due to compaction and offset, this value is not an index into the buffer, but may be translated
    # to an index.
    # This value is in -1:typemax(Int)
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
        truncate::Bool,
    ) where T<:AbstractVector{UInt8}
    require_one_based_indexing(data)
    mz = Int(maxsize)::Int
    len = Int(length(data))::Int
    if !truncate && mz < len
        throw(ArgumentError("maxsize must not be smaller than data length"))
    end
    buf = _new_generic_iobuffer(T, data, readable, writable, seekable, append, mz)
    if truncate
        buf.size = buf.offset_or_compacted
    end
    buf
end

const IOBuffer = GenericIOBuffer{Memory{UInt8}}

function GenericIOBuffer(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer, truncate::Bool) where T<:AbstractVector{UInt8}
    GenericIOBuffer{T}(data, readable, writable, seekable, append, maxsize, truncate)
end

# For this method, we use the underlying Memory of the vector. Therefore, we need to set the,
# ptr and size accordingly, so the buffer only uses the part of the memory that the vector does.
function GenericIOBuffer(data::Vector{UInt8}, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                         maxsize::Integer, truncate::Bool)
    ref = data.ref
    mem = ref.mem
    offset = memoryrefoffset(ref) - 1
    # The user may pass a vector of length <= maxsize, but where the underlying memory
    # is larger than maxsize. Don't throw an error in that case.
    mz = Int(maxsize)::Int
    if !truncate && mz < length(data)
        throw(ArgumentError("maxsize must not be smaller than data length"))
    end
    buf = _new_generic_iobuffer(Memory{UInt8}, mem, readable, writable, seekable, append, mz)
    buf.offset_or_compacted = offset
    buf.ptr = offset + 1
    if truncate
        buf.size = offset
    else
        buf.size = length(data) + offset
    end
    return buf
end

get_offset(io::GenericIOBuffer) = max(0, io.offset_or_compacted)
get_compacted(io::GenericIOBuffer) = max(0, -io.offset_or_compacted)

# allocate Vector{UInt8}s for IOBuffer storage that can efficiently become Strings
StringMemory(n::Integer) = unsafe_wrap(Memory{UInt8}, _string_n(n))
StringVector(n::Integer) = wrap(Array, StringMemory(n))

# IOBuffers behave like Files. They are typically readable and writable. They are seekable. (They can be appendable).

"""
    IOBuffer([data::AbstractVector{UInt8}]; keywords...)::IOBuffer

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
    buf = GenericIOBuffer(data, flags.read, flags.write, true, flags.append, maxsize, flags.truncate)
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
    GenericIOBuffer(data, true, true, false, true, maxsize, false)
PipeBuffer(maxsize::Integer) = (x = PipeBuffer(StringMemory(maxsize), maxsize = maxsize); x.size = 0; x)

# Internal method where truncation IS supported
function _truncated_pipebuffer(data::AbstractVector{UInt8}=Memory{UInt8}(); maxsize::Int = typemax(Int))
    buf = PipeBuffer(data)
    buf.size = get_offset(buf)
    buf.maxsize = maxsize
    buf
end

_similar_data(b::GenericIOBuffer, len::Int) = similar(b.data, len)
_similar_data(b::IOBuffer, len::Int) = StringMemory(len)

# Note: Copying may change the value of the position (and mark) for un-seekable streams.
# However, these values are not stable anyway due to compaction.

function copy(b::GenericIOBuffer{T}) where T
    if b.reinit
        # If buffer is used up, allocate a new size-zero buffer
        # Reinit implies writable, and that ptr, size, offset and mark are already the default values
        return typeof(b)(_similar_data(b, 0), b.readable, b.writable, b.seekable, b.append, b.maxsize, false)
    elseif b.writable
        # Else, we just copy the reachable bytes. If buffer is seekable, all bytes
        # after offset are reachable, since they can be seeked to
        used_span = get_used_span(b)
        compacted = first(used_span) - get_offset(b) - 1
        len = length(used_span)
        data = copyto!(_similar_data(b, len), view(b.data, used_span))
        ret = typeof(b)(data, b.readable, b.writable, b.seekable, b.append, b.maxsize, false)
        ret.size = len
        # Copying data over implicitly compacts, and may add compaction
        ret.offset_or_compacted = -get_compacted(b) - compacted
        ret.ptr = b.ptr - first(used_span) + 1
        ret.mark = b.mark
        return ret
    else
        # When the buffer is just readable, they can share the same data, so we just make
        # a shallow copy of the IOBuffer struct.
        # Use internal constructor because we want to allow b.maxsize to be larger than data,
        # in case that is the case for `b`.
        ret = _new_generic_iobuffer(T, b.data, b.readable, b.writable, b.seekable, b.append, b.maxsize)
        ret.offset_or_compacted = b.offset_or_compacted
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
                                      "size=",     b.size - get_offset(b), ", ",
                                      "maxsize=",  b.maxsize == typemax(Int) ? "Inf" : b.maxsize, ", ",
                                      "ptr=",      b.ptr - get_offset(b), ", ",
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
filesize(io::GenericIOBuffer) = (io.seekable ? io.size - get_offset(io) : bytesavailable(io))

# Number of bytes that can be read from the buffer.
bytesavailable(io::GenericIOBuffer) = io.size - io.ptr + 1

# TODO: Document that position for an unmarked and unseekable stream is invalid (and make it error?)
function position(io::GenericIOBuffer)
    # Position is zero-indexed, but ptr is one-indexed, hence the -1
    io.ptr - io.offset_or_compacted - 1
end

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
        # Handle overflow.
        maxptr = io.size + 1
        io.ptr = n > maxptr || io.ptr - n > maxptr ? maxptr : io.ptr + n
        io
    end
end

function seek(io::GenericIOBuffer, n::Integer)
    seek(io, clamp(n, Int))
end

function translate_seek_position(io::GenericIOBuffer, n::Int)
    # If there is an offset (the field F is positive), then there are F unused bytes at the beginning
    # of the data, and we need to seek to n + F + 1. (Also compensate for `seek` being zero-
    # indexed)

    # If bytes has been compacted (field F is negative), then F bytes has been deleted from
    # the buffer, and a virtual position n means a position n + F in the data.
    # Remember that F is negative, so n + F is subtracting from n. So we also end up with
    # n + F + 1.
    clamp(widen(n) + widen(io.offset_or_compacted) + widen(1), Int)
end

function seek(io::GenericIOBuffer, n::Int)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end

    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an GenericIOBuffer), so that would need to be fixed in order to throw an error here
    max_ptr = io.size + 1
    min_ptr = get_offset(io) + 1
    io.ptr = clamp(translate_seek_position(io, n), min_ptr, max_ptr)
    return io
end

function seekend(io::GenericIOBuffer)
    io.ptr = io.size+1
    return io
end

# Resize the io's data to `new_size`, which must not be > io.maxsize.
# Use `resize!` if the data supports it, else reallocate a new one and
# copy the old data over.
# If not `exact` and resizing is not supported, overallocate in order to
# prevent excessive resizing.
function _resize!(io::GenericIOBuffer, new_size::Int, exact::Bool)
    old_data = io.data
    if applicable(resize!, old_data, new_size)
        resize!(old_data, new_size)
    else
        new_size = exact ? new_size : min(io.maxsize, overallocation(new_size))
        used_span = get_used_span(io)
        deleted = first(used_span) - 1
        compacted = deleted - get_offset(io)
        new_data = _similar_data(io, new_size)
        io.data = new_data
        iszero(new_size) && return io
        len_used = length(used_span)
        iszero(len_used) || copyto!(new_data, 1, old_data, first(used_span), len_used)
        # Copying will implicitly compact, and so compaction must be updated
        io.offset_or_compacted = -get_compacted(io) - compacted
        io.ptr -= deleted
        io.size = len_used
    end
    return io
end

function truncate(io::GenericIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    # Non-seekable buffers can only be constructed with `PipeBuffer`, which is explicitly
    # documented to not be truncatable.
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.maxsize && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer maxsize $(io.maxsize)"))
    n = Int(n)::Int
    offset = get_offset(io)
    current_size = io.size - offset
    if io.reinit
        # If reinit, we don't need to truncate anything but just reinitializes
        # the buffer with zeros. Mark, ptr and offset has already been reset.
        io.data = fill!(_similar_data(io, n), 0x00)
        io.reinit = false
        io.size = n
    elseif n < current_size
        # Else, if we need to shrink the iobuffer, we simply change the pointers without
        # actually shrinking the underlying storage, or copying data.

        # Clear the mark if it points to data that has now been deleted.
        if translate_seek_position(io, io.mark) > n+offset
            io.mark = -1
        end
        io.size = n + offset
        io.ptr = min(io.ptr, n + offset + 1)
    elseif n > current_size
        if n + offset > io.maxsize
            compact!(io)
        end
        _resize!(io, n + get_offset(io), false)
        fill!(view(io.data, io.size + 1:min(length(io.data), n + get_offset(io))), 0x00)
        io.size = min(length(io.data), n + get_offset(io))
    end
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
    existing_space = min(lastindex(io.data), io.maxsize + get_offset(io)) - (io.append ? io.size : io.ptr - 1)
    if existing_space < nshort % Int
        # Outline this function to make it more likely that ensureroom inlines itself
        return ensureroom_slowpath(io, nshort, existing_space)
    end
    return io
end

# Throw error (placed in this function to outline it) or reinit the buffer
@noinline function ensureroom_reallocate(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    io.data = _similar_data(io, min(io.maxsize, nshort % Int))
    io.reinit = false
    io.offset_or_compacted = -get_compacted(io)
    return io
end

# Here, we already know there is not enough room at the end of the io's data.
@noinline function ensureroom_slowpath(io::GenericIOBuffer, nshort::UInt, available_bytes::Int)
    reclaimable_bytes = first(get_used_span(io)) - 1
    # Avoid resizing and instead compact the buffer, only if we gain enough bytes from
    # doing so (at least 32 bytes and 1/8th of the data length). Also, if we would have
    # to resize anyway, there would be no point in compacting, so also check that.
    if (
            reclaimable_bytes ≥ 32 &&
            reclaimable_bytes ≥ length(io.data) >>> 3 &&
            (reclaimable_bytes + available_bytes) % UInt ≥ nshort
        )
        compact!(io)
        return io
    end

    desired_size = length(io.data) + Int(nshort) - available_bytes
    if desired_size > io.maxsize
        # If we can't fit all the requested data in the new buffer, we need to
        # fit as much as possible, so we must compact
        if !iszero(reclaimable_bytes)
            desired_size -= compact!(io)
        end
        # Max out the buffer size if we want more than the buffer size
        if length(io.data) < io.maxsize
            _resize!(io, io.maxsize, true)
        end
    else
        # Else, we request only the requested size, but set `exact` to `false`,
        # in order to overallocate to avoid growing the buffer by too little
        _resize!(io, desired_size, false)
    end

    return io
end

# Get the indices in data which cannot be deleted
function get_used_span(io::IOBuffer)
    # A seekable buffer can recover data before ptr
    return if io.seekable
        get_offset(io) + 1 : io.size
    # If non-seekable, the mark can be used to recover data before ptr,
    # so data at the mark and after must also be saved
    elseif io.mark > -1
        min(io.ptr, translate_seek_position(io, io.mark)) : io.size
    else
        io.ptr : io.size
    end
end

# Delete any offset, and also compact data if buffer is not seekable.
# Return the number of bytes deleted
function compact!(io::GenericIOBuffer)::Int
    offset = get_offset(io)
    used_span = get_used_span(io)
    deleted = first(used_span) - 1
    compacted = deleted - offset
    iszero(deleted) && return 0
    data = io.data
    copyto!(data, 1, data, deleted + 1, length(used_span))
    io.offset_or_compacted = -get_compacted(io) - compacted
    io.ptr -= deleted
    io.size -= deleted
    return deleted
end

eof(io::GenericIOBuffer) = (io.ptr - 1 >= io.size)

function closewrite(io::GenericIOBuffer)
    io.writable = false
    nothing
end

@noinline function close(io::GenericIOBuffer{T}) where T
    if io.writable && !io.reinit
        _resize!(io, 0, true)
    end
    io.readable = false
    io.writable = false
    io.seekable = false
    io.size = 0
    io.maxsize = 0
    io.ptr = 1
    io.mark = -1
    io.offset_or_compacted = -get_compacted(io)
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
        offset = get_offset(io)
        nbytes = io.size - offset
        data = copyto!(StringVector(nbytes), 1, io.data, offset + 1, nbytes)
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
        io.offset_or_compacted = 0
    end
    return data
end

# This method is specialized because we know the underlying data is a Memory, so we can
# e.g. wrap directly in an array without copying. Otherwise the logic is the same as
# the generic method
function take!(io::IOBuffer)
    io.mark = -1
    if io.seekable
        nbytes = filesize(io)
        if nbytes == 0 || io.reinit
            data = StringVector(0)
        elseif io.writable
            data = wrap(Array, memoryref(io.data, get_offset(io) + 1), nbytes)
        else
            data = copyto!(StringVector(nbytes), 1, io.data, get_offset(io) + 1, nbytes)
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
        io.offset_or_compacted = 0
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
function _unsafe_take!(io::IOBuffer)
    offset = get_offset(io)
    mem = if io.size == offset
        memoryref(Memory{UInt8}())
    else
        memoryref(io.data, offset + 1)
    end
    wrap(Array, mem, io.size - offset)
end

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
    to_write = min(nb, (min(Int(length(data))::Int, to.maxsize + get_offset(to)) - ptr + 1) % UInt) % Int
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
    if ptr > to.maxsize + get_offset(to)
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
        if keep || i == out.offset_or_compacted || line[i] != 0x0a
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

# This file is a part of Julia. License is MIT: https://julialang.org/license

## work with AbstractVector{UInt8} via I/O primitives ##

# Here, u represents used bytes (already read), X represents bytes still to read,
# - represents bytes uninitialized data but which can be written to later.
# . are bytes which can neither be read nor written - but the IOBuffer has control
# of the full array.

#   uuuuuuuuuuuuuXXXXXXXXXXXXX------------
#   |       |    |           |           |    |
#   |       |    ptr         size        |    maxsize (≥ lastindex)
#   1       mark (zero-indexed)          lastindex(data)

#            AFTER COMPACTION
# Mark, ptr and size decreases by `mark`

#   uuuuuXXXXXXXXXXXXX---------------------
#  ||    |           |                    |    |
#  |1    ptr         size                 |    maxsize (≥ lastindex)
#  mark (zero-indexed)                    lastindex(data)

# * The underlying array is always 1-indexed
# * The IOBuffer has full control of the underlying array.
# * Data in 1:mark, if mark > -1 can be deleted, shifting the whole thing to the left
#   to make room for more data, without replacing or resizing data

mutable struct GenericIOBuffer{T<:AbstractVector{UInt8}} <: IO
    # T should support: getindex, setindex!, length, copyto!, similar, and (optionally) resize!
    data::T

    # The user can take control of `data` out of this struct. When that happens, instead of eagerly allocating
    # a new array, we set `.reinit` to true, and then allocate a new one when needed.
    reinit::Bool
    readable::Bool
    writable::Bool

    # If not seekable, implementation is free to destroy (compact) data in 1:mark-1
    seekable::Bool

    # If true, write new data to the index size+1 instead of the index ptr.
    append::Bool

    # Last index of `data` that has been written to. Data in size+1:end has not yet been used,
    # and may contain arbitrary values.
    # This value is always in 0 : lastindex(data)
    size::Int

    # This is the maximum length that the buffer size can grow to.
    # This value is always in 0:typemax(Int).
    # We always have length(data) <= maxsize
    maxsize::Int

    # Data is read/written from/to ptr, except in situations where append is true, in which case
    # data is still read from ptr, but written to size+1.
    # This value is alwaus in 1 : size+1
    ptr::Int

    # Data at the marked location or before for non-seekable buffers can be deleted.
    # The mark is zero-indexed. If it is -1, the mark is not set.
    # The purpose of the mark is to reset the stream to a given position using reset.
    # This value is always == -1, or in 0:size-1
    mark::Int

    # TODO: The invariants of the values should be enforced in all constructors,
    # except explicitly unsafe ones.
    function GenericIOBuffer{T}(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool,
                                maxsize::Integer) where T<:AbstractVector{UInt8}
        require_one_based_indexing(data)
        return new(data, false, readable, writable, seekable, append, length(data), maxsize, 1, -1)
    end
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
    len = length(data)
    offset = memoryrefoffset(ref) - 1
    if !iszero(offset)
        unsafe_copyto!(mem, 1, mem, offset+1, len)
    end
    buf = GenericIOBuffer(mem, readable, writable, seekable, append, maxsize)
    buf.size = len
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
    # TODO: Add a check that length(data) <= maxsize and error if not.
    if maxsize < 0
        throw(ArgumentError("negative maxsize"))
    end
    if sizehint !== nothing
        sizehint!(data, sizehint)
    end
    flags = open_flags(read=read, write=write, append=append, truncate=truncate)
    buf = GenericIOBuffer(data, flags.read, flags.write, true, flags.append, Int(maxsize)::Int)
    if flags.truncate
        buf.size = 0
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
    data = fill!(StringMemory(size), 0)
    buf = GenericIOBuffer(data, flags.read, flags.write, true, flags.append, mz)
    if flags.truncate
        buf.size = 0
    end
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
    return ret
end

show(io::IO, b::GenericIOBuffer) = print(io, "IOBuffer(data=UInt8[...], ",
                                      "readable=", b.readable, ", ",
                                      "writable=", b.writable, ", ",
                                      "seekable=", b.seekable, ", ",
                                      "append=",   b.append, ", ",
                                      "size=",     b.size, ", ",
                                      "maxsize=",  b.maxsize == typemax(Int) ? "Inf" : b.maxsize, ", ",
                                      "ptr=",      b.ptr, ", ",
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

function read_sub(from::GenericIOBuffer, a::MutableDenseArrayType{T}, offs, nel) where T
    require_one_based_indexing(a)
    from.readable || _throw_not_readable()
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    nb = UInt(nel * sizeof(T))
    GC.@preserve a unsafe_read(from, pointer(a, offs), nb)
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
filesize(io::GenericIOBuffer) = (io.seekable ? io.size : bytesavailable(io))

# Number of bytes that can be read from the buffer.
bytesavailable(io::GenericIOBuffer) = io.size - io.ptr + 1

# Position is zero-indexed, but ptr is one-indexed, hence the -1
# TODO: Document that position for an unseekable stream is invalid, or
# make it error
position(io::GenericIOBuffer) = io.ptr - 1

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
    io.ptr = clamp(n, 0, io.size) + 1
    return io
end

# TODO: Should check for seekable and error if not
function seekend(io::GenericIOBuffer)
    io.ptr = io.size+1
    return io
end

# Resize data to exactly size `sz`. Either resize the underlying data,
# or allocate a new one and copy.
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

function truncate(io::GenericIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.maxsize && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer maxsize $(io.maxsize)"))
    n = Int(n)
    if io.reinit
        io.data = _similar_data(io, n)
        io.reinit = false
    elseif n > length(io.data)
        _resize!(io, n)
    end
    ismarked(io) && io.mark > n && unmark(io)
    io.data[io.size+1:n] .= 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
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
        return ensureroom_slowpath(io, nshort)
    end
    # The fast path here usually checks there is already room, then does nothing.
    # When append is true, new data is added after io.size, not io.ptr
    existing_space = lastindex(io.data) - (io.append ? io.size : io.ptr - 1)
    if existing_space < nshort % Int
        # If the buffer is seekable, the user can seek to before ptr, and so we
        # cannot compact the data.
        if (!io.seekable && io.ptr > 1)
            nshort = ensureroom_compact(io, nshort)
            iszero(nshort) && return io
        end
        # Don't exceed maxsize. Otherwise, we overshoot the number of bytes needed,
        # such that we don't need to resize too often.
        new_size = min(io.maxsize, overallocation(length(io.data) + nshort % Int))
        _resize!(io, new_size)
    end
    return io
end

# Throw error (placed in this function to outline it) or reinit the buffer
@noinline function ensureroom_slowpath(io::GenericIOBuffer, nshort::UInt)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    io.data = _similar_data(io, min(io.maxsize, nshort % Int))
    io.reinit = false
    return io
end

# Compact data. Only called if the buffer is not seekable.
# Returns the number of bytes still not available after compacting
@noinline function ensureroom_compact(io::GenericIOBuffer, nshort::UInt)::UInt
    ptr = io.ptr
    mark = io.mark
    size = io.size
    data = io.data
    data_len = lastindex(data)
    to_delete = (mark > -1 ? min(mark, ptr - 1) : ptr - 1)
    # Only shift data if any of these:
    if (
            to_delete > 1 >> 12 || # We can recover > 4 KiB from doing so
            to_delete >= nshort % Int || # It will prevent us from having to resize buffer
            to_delete > data_len >> 3 # We can recover more than 1/8th of the data's length
        )
        copyto!(data, 1, data, to_delete + 1, size - to_delete)
        io.ptr = ptr - to_delete
        io.mark = max(-1, mark - to_delete)
        io.size = size - to_delete
    end
    return nshort - min(nshort, to_delete % UInt)
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
        # Hence, we output all bytes from 1:io.size
        nbytes = io.size
        data = copyto!(StringVector(nbytes), 1, io.data, 1, nbytes)
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
            data = wrap(Array, memoryref(io.data, 1), nbytes)
        else
            data = copyto!(StringVector(nbytes), 1, io.data, 1, nbytes)
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
    wrap(Array, io.size == 0 ?
        memoryref(Memory{UInt8}()) :
        memoryref(io.data, 1),
        io.size)

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
    ptr = to.append ? to.size+1 : to.ptr
    data = to.data
    to_write = min(nb % Int, Int(length(data))::Int - ptr + 1)
    # Dispatch based on the type of data, to possibly allow using memcpy
    _unsafe_write(data, p, ptr, to_write % UInt)
    # Update to.size only if the ptr has advanced to higher than
    # the previous size. Otherwise, we just overwrote existing data
    to.size = max(to.size, ptr + to_write - 1)
    # If to.append, we only update size, not ptr.
    if !to.append
        to.ptr += to_write
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
    GC.@preserve data begin
        ptr = Ptr{UInt8}(pointer(data, from))::Ptr{UInt8}
        unsafe_copyto!(ptr, p, nb)
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

function occursin(delim::UInt8, buf::GenericIOBuffer)
    ptr = buf.ptr
    return in(delim, view(buf.data, ptr:ptr + bytesavailable(buf)-1))
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
        if keep || iszero(i) || line[i] != 0x0a
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

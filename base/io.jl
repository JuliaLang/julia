# This file is a part of Julia. License is MIT: http://julialang.org/license

# Generic IO stubs -- all subtypes should implement these (if meaningful)

lock(::IO) = nothing
unlock(::IO) = nothing
reseteof(x::IO) = nothing

const SZ_UNBUFFERED_IO = 65536
buffer_writes(x::IO, bufsize=SZ_UNBUFFERED_IO) = nothing

"""
    isopen(object) -> Bool

Determine whether an object - such as a stream, timer, or mmap -- is not yet closed. Once an
object is closed, it will never produce a new event. However, a closed stream may still have
data to read in its buffer, use [`eof`](:func:`eof`) to check for the ability to read data.
Use [`poll_fd`](:func:`poll_fd`) to be notified when a stream might be writable or readable.
"""
function isopen end

"""
    close(stream)

Close an I/O stream. Performs a [`flush`](:func:`flush`) first.
"""
function close end
function flush end
function wait_connected end
function wait_readnb end
function wait_readbyte end
function wait_close end
function nb_available end
function readavailable end

"""
    isreadable(io) -> Bool

Returns `true` if the specified IO object is readable (if that can be determined).
"""
function isreadable end

"""
    iswritable(io) -> Bool

Returns `true` if the specified IO object is writable (if that can be determined).
"""
function iswritable end
function copy end
function eof end

"""
    write(stream::IO, x)
    write(filename::AbstractString, x)

Write the canonical binary representation of a value to the given I/O stream or file.
Returns the number of bytes written into the stream.

You can write multiple values with the same `write` call. i.e. the following are equivalent:

    write(stream, x, y...)
    write(stream, x) + write(stream, y...)
"""
function write end

read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

"""
    unsafe_write(io::IO, ref, nbytes::UInt)

Copy nbytes from ref (converted to a pointer) into the IO stream object.

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_write(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_write(s::IO, p::Ptr{UInt8}, n::UInt)
    local written::Int = 0
    for i = 1:n
        written += write(s, unsafe_load(p, i))
    end
    return written
end

"""
    unsafe_read(io::IO, ref, nbytes::UInt)

Copy nbytes from the `IO` stream object into `ref` (converted to a pointer).

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_read(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_read(s::IO, p::Ptr{UInt8}, n::UInt)
    for i = 1:n
        unsafe_store!(p, read(s, UInt8)::UInt8, i)
    end
    nothing
end


# Generic wrappers around other IO objects
abstract AbstractPipe <: IO
function pipe_reader end
function pipe_writer end

write(io::AbstractPipe, byte::UInt8) = write(pipe_writer(io), byte)
unsafe_write(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_write(pipe_writer(io), p, nb)
buffer_writes(io::AbstractPipe, args...) = buffer_writes(pipe_writer(io), args...)
flush(io::AbstractPipe) = flush(pipe_writer(io))

read(io::AbstractPipe, byte::Type{UInt8}) = read(pipe_reader(io), byte)
unsafe_read(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_read(pipe_reader(io), p, nb)
read(io::AbstractPipe) = read(pipe_reader(io))
readuntil{T<:AbstractPipe}(io::T, args...) = readuntil(pipe_reader(io), args...)
readavailable(io::AbstractPipe) = readavailable(pipe_reader(io))

isreadable(io::AbstractPipe) = isreadable(pipe_reader(io))
iswritable(io::AbstractPipe) = iswritable(pipe_writer(io))
isopen(io::AbstractPipe) = isopen(pipe_writer(io)) || isopen(pipe_reader(io))
close(io::AbstractPipe) = (close(pipe_writer(io)); close(pipe_reader(io)))
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io), nb)
wait_readbyte(io::AbstractPipe, byte::UInt8) = wait_readbyte(pipe_reader(io), byte)
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)); wait_close(pipe_reader(io)))

"""
    nb_available(stream)

Returns the number of bytes available for reading before a read from this stream or buffer will block.
"""
nb_available(io::AbstractPipe) = nb_available(pipe_reader(io))

"""
    eof(stream) -> Bool

Tests whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this
function will block to wait for more data if necessary, and then return `false`. Therefore
it is always safe to read one byte after seeing `eof` return `false`. `eof` will return
`false` as long as buffered data is still available, even if the remote end of a connection
is closed.
"""
eof(io::AbstractPipe) = eof(pipe_reader(io))
reseteof(io::AbstractPipe) = reseteof(pipe_reader(io))


# Exception-safe wrappers (io = open(); try f(io) finally close(io))

write(filename::AbstractString, args...) = open(io->write(io, args...), filename, "w")

"""
    read(filename::AbstractString, args...)

Open a file and read its contents. `args` is passed to `read`: this is equivalent to
`open(io->read(io, args...), filename)`.
"""
read(filename::AbstractString, args...) = open(io->read(io, args...), filename)
read!(filename::AbstractString, a) = open(io->read!(io, a), filename)
readstring(filename::AbstractString) = open(readstring, filename)

"""
    readuntil(stream::IO, delim)
    readuntil(filename::AbstractString, delim)

Read a string from an I/O stream or a file, up to and including the given delimiter byte.
The text is assumed to be encoded in UTF-8.
"""
readuntil(filename::AbstractString, args...) = open(io->readuntil(io, args...), filename)

"""
    readline(stream::IO=STDIN)
    readline(filename::AbstractString)

Read a single line of text, including a trailing newline character (if one is reached before
the end of the input), from the given I/O stream or file (defaults to `STDIN`).
When reading from a file, the text is assumed to be encoded in UTF-8.
"""
readline(filename::AbstractString) = open(readline, filename)

"""
    readlines(stream::IO)
    readlines(filename::AbstractString)

Read all lines of an I/O stream or a file as a vector of strings.
The text is assumed to be encoded in UTF-8.
"""
readlines(filename::AbstractString) = open(readlines, filename)


## byte-order mark, ntoh & hton ##

"""
    ENDIAN_BOM

The 32-bit byte-order-mark indicates the native byte order of the host machine.
Little-endian machines will contain the value `0x04030201`. Big-endian machines will contain
the value `0x01020304`.
"""
const ENDIAN_BOM = reinterpret(UInt32,UInt8[1:4;])[1]

if ENDIAN_BOM == 0x01020304
    ntoh(x) = x
    hton(x) = x
    ltoh(x) = bswap(x)
    htol(x) = bswap(x)
elseif ENDIAN_BOM == 0x04030201
    ntoh(x) = bswap(x)
    hton(x) = bswap(x)
    ltoh(x) = x
    htol(x) = x
else
    error("seriously? what is this machine?")
end


"""
    ntoh(x)

Converts the endianness of a value from Network byte order (big-endian) to that used by the Host.
"""
ntoh(x)

"""
    hton(x)

Converts the endianness of a value from that used by the Host to Network byte order (big-endian).
"""
hton(x)

"""
    ltoh(x)

Converts the endianness of a value from Little-endian to that used by the Host.
"""
ltoh(x)

"""
    htol(x)

Converts the endianness of a value from that used by the Host to Little-endian.
"""
htol(x)


"""
    isreadonly(stream) -> Bool

Determine whether a stream is read-only.
"""
isreadonly(s) = isreadable(s) && !iswritable(s)

## binary I/O ##

write(io::IO, x) = throw(MethodError(write, (io, x)))
function write(io::IO, xs...)
    local written::Int = 0
    for x in xs
        written += write(io, x)
    end
    return written
end

@noinline unsafe_write{T}(s::IO, p::Ref{T}, n::Integer) = unsafe_write(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_write(s::IO, p::Ptr, n::Integer) = unsafe_write(s, convert(Ptr{UInt8}, p), convert(UInt, n))
write{T}(s::IO, x::Ref{T}) = unsafe_write(s, x, Core.sizeof(T))
write(s::IO, x::Int8) = write(s, reinterpret(UInt8, x))
function write(s::IO, x::Union{Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,Float16,Float32,Float64})
    return write(s, Ref(x))
end

write(s::IO, x::Bool) = write(s, UInt8(x))
write(to::IO, p::Ptr) = write(to, convert(UInt, p))

function write(s::IO, A::AbstractArray)
    nb = 0
    for a in A
        nb += write(s, a)
    end
    return nb
end

@noinline function write(s::IO, a::Array{UInt8}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    return unsafe_write(s, pointer(a), sizeof(a))
end

@noinline function write{T}(s::IO, a::Array{T}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    if isbits(T)
        return unsafe_write(s, pointer(a), sizeof(a))
    else
        nb = 0
        for b in a
            nb += write(s, b)
        end
        return nb
    end
end


function write(s::IO, ch::Char)
    c = reinterpret(UInt32, ch)
    if c < 0x80
        return write(s, c%UInt8)
    elseif c < 0x800
        return (write(s, (( c >> 6          ) | 0xC0)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x10000
        return (write(s, (( c >> 12         ) | 0xE0)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x110000
        return (write(s, (( c >> 18         ) | 0xF0)%UInt8)) +
               (write(s, (((c >> 12) & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    else
        return write(s, '\ufffd')
    end
end

function write(io::IO, s::Symbol)
    pname = unsafe_convert(Ptr{UInt8}, s)
    return unsafe_write(io, pname, Int(ccall(:strlen, Csize_t, (Cstring,), pname)))
end

function write(to::IO, from::IO)
    while !eof(from)
        write(to, readavailable(from))
    end
end

@noinline unsafe_read{T}(s::IO, p::Ref{T}, n::Integer) = unsafe_read(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_read(s::IO, p::Ptr, n::Integer) = unsafe_read(s, convert(Ptr{UInt8}, p), convert(UInt, n))
read{T}(s::IO, x::Ref{T}) = (unsafe_read(s, x, Core.sizeof(T)); x)

read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s, UInt8))
function read(s::IO, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    return read(s, Ref{T}(0))[]::T
end

read(s::IO, ::Type{Bool})    = (read(s,UInt8)!=0)
read{T}(s::IO, ::Type{Ptr{T}}) = convert(Ptr{T}, read(s, UInt))

read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) = read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, convert(Tuple{Vararg{Int}},tuple(d1,dims...)))

"""
    read(stream::IO, T, dims)

Read a series of values of type `T` from `stream`, in canonical binary representation.
`dims` is either a tuple or a series of integer arguments specifying the size of the `Array{T}`
to return.
"""
read{T}(s::IO, ::Type{T}, dims::Dims) = read!(s, Array{T}(dims))

@noinline function read!(s::IO, a::Array{UInt8}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    unsafe_read(s, pointer(a), sizeof(a))
    return a
end

@noinline function read!{T}(s::IO, a::Array{T}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    if isbits(T)
        unsafe_read(s, pointer(a), sizeof(a))
    else
        for i in eachindex(a)
            a[i] = read(s, T)
        end
    end
    return a
end

function read(s::IO, ::Type{Char})
    ch = read(s, UInt8)
    if ch < 0x80
        return Char(ch)
    end

    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::UInt32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = read(s, UInt8)
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    return Char(c)
end

function readuntil(s::IO, delim::Char)
    if delim < Char(0x80)
        return String(readuntil(s, delim % UInt8))
    end
    out = IOBuffer()
    while !eof(s)
        c = read(s, Char)
        write(out, c)
        if c == delim
            break
        end
    end
    return takebuf_string(out)
end

function readuntil{T}(s::IO, delim::T)
    out = T[]
    while !eof(s)
        c = read(s, T)
        push!(out, c)
        if c == delim
            break
        end
    end
    return out
end

# based on code by Glen Hertz
function readuntil(s::IO, t::AbstractString)
    l = length(t)
    if l == 0
        return ""
    end
    if l > 40
        warn("readuntil(IO,AbstractString) will perform poorly with a long string")
    end
    out = IOBuffer()
    m = Array{Char}(l)  # last part of stream to match
    t = collect(t)
    i = 0
    while !eof(s)
        i += 1
        c = read(s, Char)
        write(out, c)
        if i <= l
            m[i] = c
        else
            # shift to last part of s
            for j = 2:l
                m[j-1] = m[j]
            end
            m[l] = c
        end
        if i >= l && m == t
            break
        end
    end
    return takebuf_string(out)
end

readline() = readline(STDIN)
readline(s::IO) = readuntil(s, '\n')

"""
    readchomp(x)

Read the entirety of `x` as a string and remove a single trailing newline.
Equivalent to `chomp!(readstring(x))`.
"""
readchomp(x) = chomp!(readstring(x))

# read up to nb bytes into nb, returning # bytes read

"""
    readbytes!(stream::IO, b::AbstractVector{UInt8}, nb=length(b))

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.
"""
function readbytes!(s::IO, b::AbstractArray{UInt8}, nb=length(b))
    olb = lb = length(b)
    nr = 0
    while nr < nb && !eof(s)
        a = read(s, UInt8)
        nr += 1
        if nr > lb
            lb = nr * 2
            resize!(b, lb)
        end
        b[nr] = a
    end
    if lb > olb
        resize!(b, nr) # shrink to just contain input data if was resized
    end
    return nr
end

"""
    read(s::IO, nb=typemax(Int))

Read at most `nb` bytes from `s`, returning a `Vector{UInt8}` of the bytes read.
"""
function read(s::IO, nb=typemax(Int))
    # Let readbytes! grow the array progressively by default
    # instead of taking of risk of over-allocating
    b = Array{UInt8}(nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    return resize!(b, nr)
end

readstring(s::IO) = String(read(s))

## high-level iterator interfaces ##

type EachLine
    stream::IO
    ondone::Function
    EachLine(stream) = EachLine(stream, ()->nothing)
    EachLine(stream, ondone) = new(stream, ondone)
end
eachline(stream::IO) = EachLine(stream)
function eachline(filename::AbstractString)
    s = open(filename)
    EachLine(s, ()->close(s))
end

start(itr::EachLine) = nothing
function done(itr::EachLine, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
next(itr::EachLine, nada) = (readline(itr.stream), nothing)
eltype(::Type{EachLine}) = String

readlines(s=STDIN) = collect(eachline(s))

iteratorsize(::Type{EachLine}) = SizeUnknown()

# IOStream Marking
# Note that these functions expect that io.mark exists for
# the concrete IO type. This may not be true for IO types
# not in base.

"""
    mark(s)

Add a mark at the current position of stream `s`. Returns the marked position.

See also [`unmark`](:func:`unmark`), [`reset`](:func:`reset`), [`ismarked`](:func:`ismarked`).
"""
function mark(io::IO)
    io.mark = position(io)
end

"""
    unmark(s)

Remove a mark from stream `s`. Returns `true` if the stream was marked, `false` otherwise.

See also [`mark`](:func:`mark`), [`reset`](:func:`reset`), [`ismarked`](:func:`ismarked`).
"""
function unmark(io::IO)
    !ismarked(io) && return false
    io.mark = -1
    return true
end

"""
    reset(s)

Reset a stream `s` to a previously marked position, and remove the mark. Returns the
previously marked position. Throws an error if the stream is not marked.

See also [`mark`](:func:`mark`), [`unmark`](:func:`unmark`), [`ismarked`](:func:`ismarked`).
"""
function reset{T<:IO}(io::T)
    ismarked(io) || throw(ArgumentError("$(T) not marked"))
    m = io.mark
    seek(io, m)
    io.mark = -1 # must be after seek, or seek may fail
    return m
end

"""
    ismarked(s)

Returns `true` if stream `s` is marked.

See also [`mark`](:func:`mark`), [`unmark`](:func:`unmark`), [`reset`](:func:`reset`).
"""
ismarked(io::IO) = io.mark >= 0

# Make sure all IO streams support flush, even if only as a no-op,
# to make it easier to write generic I/O code.

"""
    flush(stream)

Commit all currently buffered writes to the given stream.
"""
flush(io::IO) = nothing

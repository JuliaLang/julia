# This type is used to check that public Base IO function continue
# to work with IO objects defined before the new IO interface
# from 2025.
# Since new IO objects should use the new, well defined interface,
# this type is not public.
struct GenericOldIO <: IO
    inner::IOBuffer
end

# Read functionality
Base.read(io::GenericOldIO, ::Type{UInt8}) = read(io.inner, UInt8)

# Write functionality
function Base.unsafe_write(io::GenericOldIO, ptr::Ptr{UInt8}, nbytes::UInt)
    unsafe_write(io.inner, ptr, nbytes)
end

function Base.write(io::GenericOldIO, n::UInt8)
    write(io.inner, n)
end

"""
    GenericBufferedIO(buf::IOBuffer, [size::Integer, write::Bool=false])

Create a buffered IO, which implements the minimal interface for a
buffered reader. Useful for testing generic IO code.
The underlying `IOBuffer` it wraps is stored in the property `inner`.
Mutating the inner buffer directly invalidates the wrapper.

# Examples
```
julia> my_read_line(io::IO) = String(readuntil(io, 0x0a));

julia> buf = Test.GenericBufferedIO(IOBuffer("abcde\r\nfg"));

julia> my_read_line(buf)
"abcde\r\n"
```
"""
mutable struct GenericBufferedIO <: IO
    # The presence of this property is documented
    const inner::IOBuffer
    buffer::Memory{UInt8}
    # Used data is start:stop
    start::Int
    stop::Int
end

# Helper functions
function GenericBufferedIO(buf::IOBuffer, size::Integer=16)
    mem = Memory{UInt8}(undef, Int(size))
    GenericBufferedIO(buf, mem, 1, 0)
end

# Basic buffered reading interface
Base.getbuffer(io::GenericBufferedIO) = view(io.buffer, io.start:io.stop)

function Base.fillbuffer(io::GenericBufferedIO)
    if io.stop == length(io.buffer)
        len = io.stop - io.start + 1
        if io.start > length(io.buffer) ÷ 2
            copyto!(io.buffer, 1, io.buffer, io.start, len)
        else
            newbuf = Memory{UInt8}(undef, 2 * length(io.buffer))
            copyto!(newbuf, 1, io.buffer, io.start, len)
            io.buffer = newbuf
        end
        io.start = 1
        io.stop = len
    end
    n = readinto!(io.inner, io.buffer)
    io.stop += n
    n
end

function Base.consume(io::GenericBufferedIO, nbytes::Int)
    (nbytes < 0 || nbytes > (io.stop - io.start + 1)) && throw(ConsumeBufferError())
    io.start += nbytes
    nothing
end

"""
    GenericUnbufferedIO(buf::IOBuffer, [size::Integer, write::Bool=false])

Create an unbuffered IO, which implements the minimal interface for an
unbuffered reader. Useful for testing generic IO code.
The underlying `IOBuffer` it wraps is stored in the property `inner`, and
Mutating the inner buffer directly invalidates the wrapper.

# Examples
```
julia> buf = GenericUnbufferedIO(IOBuffer("abcde\r\nfg"));

julia> read(buf, String)
"abcde\r\nfg"
```
"""
struct GenericUnbufferedIO <: IO
    inner::IOBuffer
end

Base.readbuffering(::Type{GenericUnbufferedIO}) = Base.NotBuffered()

function Base.readinto!(io::GenericUnbufferedIO, v::AbstractVector{UInt8})
    readinto!(io.inner, v)
end

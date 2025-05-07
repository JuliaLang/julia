# This file is a part of Julia. License is MIT: https://julialang.org/license

# Generic IO stubs -- all subtypes should implement these (if meaningful)

"""
    EOFError()

No more data was available to read from a file or stream.
"""
struct EOFError <: Exception end

"""
    SystemError(prefix::AbstractString, [errno::Int32])

A system call failed with an error code (in the `errno` global variable).
"""
struct SystemError <: Exception
    prefix::String
    errnum::Int32
    extrainfo
    SystemError(p::AbstractString, e::Integer, extrainfo) = new(p, e, extrainfo)
    SystemError(p::AbstractString, e::Integer) = new(p, e, nothing)
    SystemError(p::AbstractString) = new(p, Libc.errno(), nothing)
end

lock(::IO) = nothing
unlock(::IO) = nothing

"""
    reseteof(io)

Clear the EOF flag from IO so that further reads (and possibly writes) are
again allowed. Note that it may immediately get re-set, if the underlying
stream object is at EOF and cannot be resumed.
"""
reseteof(x::IO) = nothing

const SZ_UNBUFFERED_IO = 65536
buffer_writes(x::IO, bufsize=SZ_UNBUFFERED_IO) = x

"""
    isopen(object)::Bool

Determine whether an object - such as a stream or timer
-- is not yet closed. Once an object is closed, it will never produce a new event.
However, since a closed stream may still have data to read in its buffer,
use [`eof`](@ref) to check for the ability to read data.
Use the `FileWatching` package to be notified when a stream might be writable or readable.

# Examples
```jldoctest
julia> io = open("my_file.txt", "w+");

julia> isopen(io)
true

julia> close(io)

julia> isopen(io)
false
```
"""
function isopen end

"""
    close(stream)

Close an I/O stream. Performs a [`flush`](@ref) first.
"""
function close end

"""
    closewrite(stream)

Shutdown the write half of a full-duplex I/O stream. Performs a [`flush`](@ref)
first. Notify the other end that no more data will be written to the underlying
file. This is not supported by all IO types.

If implemented, `closewrite` causes subsequent `read` or `eof` calls that would
block to instead throw EOF or return true, respectively. If the stream is
already closed, this is idempotent.

# Examples
```jldoctest
julia> io = Base.BufferStream(); # this never blocks, so we can read and write on the same Task

julia> write(io, "request");

julia> # calling `read(io)` here would block forever

julia> closewrite(io);

julia> read(io, String)
"request"
```
"""
function closewrite end

"""
    flush(stream)

Commit all currently buffered writes to the given stream.
"""
function flush end

"""
    bytesavailable(io)

Return the number of bytes available for reading before a read from this stream or buffer will block.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> bytesavailable(io)
34
```
"""
function bytesavailable end

"""
    readavailable(stream)

Read available buffered data from a stream. Actual I/O is performed only if no
data has already been buffered. The result is a `Vector{UInt8}`.

!!! warning
    The amount of data returned is implementation-dependent; for example it can
    depend on the internal choice of buffer size. Other functions such as [`read`](@ref)
    should generally be used instead.
"""
function readavailable end

function isexecutable end

"""
    isreadable(io)::Bool

Return `false` if the specified IO object is not readable.

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           print(io, "Hello world!");
           isreadable(io)
       end
false

julia> open("myfile.txt", "r") do io
           isreadable(io)
       end
true

julia> rm("myfile.txt")
```
"""
isreadable(io::IO) = isopen(io)

"""
    iswritable(io)::Bool

Return `false` if the specified IO object is not writable.

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           print(io, "Hello world!");
           iswritable(io)
       end
true

julia> open("myfile.txt", "r") do io
           iswritable(io)
       end
false

julia> rm("myfile.txt")
```
"""
iswritable(io::IO) = isopen(io)

"""
    eof(stream)::Bool

Test whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this
function will block to wait for more data if necessary, and then return `false`. Therefore
it is always safe to read one byte after seeing `eof` return `false`. `eof` will return
`false` as long as buffered data is still available, even if the remote end of a connection
is closed.

# Examples
```jldoctest
julia> b = IOBuffer("my buffer");

julia> eof(b)
false

julia> seekend(b);

julia> eof(b)
true
```
"""
function eof end

function copy end
function wait_readnb end
function wait_close end

"""
    read(io::IO, T)

Read a single value of type `T` from `io`, in canonical binary representation.

Note that Julia does not convert the endianness for you. Use [`ntoh`](@ref) or
[`ltoh`](@ref) for this purpose.

    read(io::IO, String)

Read the entirety of `io`, as a `String` (see also [`readchomp`](@ref)).

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> read(io, Char)
'J': ASCII/Unicode U+004A (category Lu: Letter, uppercase)

julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> read(io, String)
"JuliaLang is a GitHub organization"
```
"""
read(stream, t)
read(stream, ::Type{Union{}}, slurp...; kwargs...) = error("cannot read a value of type Union{}")


"""
    write(io::IO, x)

Write the canonical binary representation of a value to the given I/O stream or file.
Return the number of bytes written into the stream. See also [`print`](@ref) to
write a text representation (with an encoding that may depend upon `io`).

The endianness of the written value depends on the endianness of the host system.
Convert to/from a fixed endianness when writing/reading (e.g. using  [`htol`](@ref) and
[`ltoh`](@ref)) to get results that are consistent across platforms.

You can write multiple values with the same `write` call, i.e. the following are equivalent:

    write(io, x, y...)
    write(io, x) + write(io, y...)

# Examples
Consistent serialization:
```jldoctest
julia> fname = tempname(); # random temporary filename

julia> open(fname,"w") do f
           # Make sure we write 64bit integer in little-endian byte order
           write(f,htol(Int64(42)))
       end
8

julia> open(fname,"r") do f
           # Convert back to host byte order and host integer type
           Int(ltoh(read(f,Int64)))
       end
42
```

Merging write calls:
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.", " It has many members.")
56

julia> String(take!(io))
"JuliaLang is a GitHub organization. It has many members."

julia> write(io, "Sometimes those members") + write(io, " write documentation.")
44

julia> String(take!(io))
"Sometimes those members write documentation."
```
User-defined plain-data types without `write` methods can be written when wrapped in a `Ref`:
```jldoctest
julia> struct MyStruct; x::Float64; end

julia> io = IOBuffer()
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=0, maxsize=Inf, ptr=1, mark=-1)

julia> write(io, Ref(MyStruct(42.0)))
8

julia> seekstart(io); read!(io, Ref(MyStruct(NaN)))
Base.RefValue{MyStruct}(MyStruct(42.0))
```
"""
function write end

read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

"""
    unsafe_write(io::IO, ref, nbytes::UInt)

Copy `nbytes` from `ref` (converted to a pointer) into the `IO` object.

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_write(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_write(s::IO, p::Ptr{UInt8}, n::UInt)
    written::Int = 0
    for i = 1:n
        written += write(s, unsafe_load(p, i))
    end
    return written
end

"""
    unsafe_read(io::IO, ref, nbytes::UInt)

Copy `nbytes` from the `IO` stream object into `ref` (converted to a pointer).

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

function peek(s::IO, ::Type{T}) where T
    mark(s)
    try read(s, T)::T
    finally
        reset(s)
    end
end

peek(s) = peek(s, UInt8)::UInt8

# Generic `open` methods

"""
    open_flags(; keywords...)::NamedTuple

Compute the `read`, `write`, `create`, `truncate`, `append` flag value for
a given set of keyword arguments to [`open`](@ref) a [`NamedTuple`](@ref).
"""
function open_flags(;
    read     :: Union{Bool,Nothing} = nothing,
    write    :: Union{Bool,Nothing} = nothing,
    create   :: Union{Bool,Nothing} = nothing,
    truncate :: Union{Bool,Nothing} = nothing,
    append   :: Union{Bool,Nothing} = nothing,
)
    if write === true && read !== true && append !== true
        create   === nothing && (create   = true)
        truncate === nothing && (truncate = true)
    end

    if truncate === true || append === true
        write  === nothing && (write  = true)
        create === nothing && (create = true)
    end

    write    === nothing && (write    = false)
    read     === nothing && (read     = !write)
    create   === nothing && (create   = false)
    truncate === nothing && (truncate = false)
    append   === nothing && (append   = false)

    return (
        read = read,
        write = write,
        create = create,
        truncate = truncate,
        append = append,
    )
end

"""
    open(f::Function, args...; kwargs...)

Apply the function `f` to the result of `open(args...; kwargs...)` and close the resulting file
descriptor upon completion.

# Examples
```jldoctest
julia> write("myfile.txt", "Hello world!");

julia> open(io->read(io, String), "myfile.txt")
"Hello world!"

julia> rm("myfile.txt")
```
"""
function open(f::Function, args...; kwargs...)
    io = open(args...; kwargs...)
    try
        f(io)
    finally
        close(io)
    end
end

"""
    AbstractPipe

`AbstractPipe` is an abstract supertype that exists for the convenience of creating
pass-through wrappers for other IO objects, so that you only need to implement the
additional methods relevant to your type. A subtype only needs to implement one or both of
these methods:

    struct P <: AbstractPipe; ...; end
    pipe_reader(io::P) = io.out
    pipe_writer(io::P) = io.in

If `pipe isa AbstractPipe`, it must obey the following interface:

- `pipe.in` or `pipe.in_stream`, if present, must be of type `IO` and be used to provide input to the pipe
- `pipe.out` or `pipe.out_stream`, if present, must be of type `IO` and be used for output from the pipe
- `pipe.err` or `pipe.err_stream`, if present, must be of type `IO` and be used for writing errors from the pipe
"""
abstract type AbstractPipe <: IO end

function getproperty(pipe::AbstractPipe, name::Symbol)
    if name === :in || name === :in_stream || name === :out || name === :out_stream ||
       name === :err || name === :err_stream
        return getfield(pipe, name)::IO
    end
    return getfield(pipe, name)
end

function pipe_reader end
function pipe_writer end

for f in (:flush, :closewrite, :iswritable)
    @eval $(f)(io::AbstractPipe) = $(f)(pipe_writer(io)::IO)
end
write(io::AbstractPipe, byte::UInt8) = write(pipe_writer(io)::IO, byte)
write(to::IO, from::AbstractPipe) = write(to, pipe_reader(from))
unsafe_write(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_write(pipe_writer(io)::IO, p, nb)::Union{Int,UInt}
buffer_writes(io::AbstractPipe, args...) = buffer_writes(pipe_writer(io)::IO, args...)

for f in (
        # peek/mark interface
        :mark, :unmark, :reset, :ismarked,
        # Simple reader functions
        :read, :readavailable, :bytesavailable, :reseteof, :isreadable)
    @eval $(f)(io::AbstractPipe) = $(f)(pipe_reader(io)::IO)
end
read(io::AbstractPipe, byte::Type{UInt8}) = read(pipe_reader(io)::IO, byte)::UInt8
unsafe_read(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_read(pipe_reader(io)::IO, p, nb)
copyuntil(out::IO, io::AbstractPipe, arg::UInt8; kw...) = copyuntil(out, pipe_reader(io)::IO, arg; kw...)
copyuntil(out::IO, io::AbstractPipe, arg::AbstractChar; kw...) = copyuntil(out, pipe_reader(io)::IO, arg; kw...)
copyuntil(out::IO, io::AbstractPipe, arg::AbstractString; kw...) = copyuntil(out, pipe_reader(io)::IO, arg; kw...)
copyuntil(out::IO, io::AbstractPipe, arg::AbstractVector; kw...) = copyuntil(out, pipe_reader(io)::IO, arg; kw...)
readuntil_vector!(io::AbstractPipe, target::AbstractVector, keep::Bool, out) = readuntil_vector!(pipe_reader(io)::IO, target, keep, out)
readbytes!(io::AbstractPipe, target::AbstractVector{UInt8}, n=length(target)) = readbytes!(pipe_reader(io)::IO, target, n)
peek(io::AbstractPipe, ::Type{T}) where {T} = peek(pipe_reader(io)::IO, T)::T
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io)::IO, nb)
eof(io::AbstractPipe) = eof(pipe_reader(io)::IO)::Bool

isopen(io::AbstractPipe) = isopen(pipe_writer(io)::IO) || isopen(pipe_reader(io)::IO)
close(io::AbstractPipe) = (close(pipe_writer(io)::IO); close(pipe_reader(io)::IO))
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)::IO); wait_close(pipe_reader(io)::IO))


# Exception-safe wrappers (io = open(); try f(io) finally close(io))


"""
    write(filename::AbstractString, content)

Write the canonical binary representation of `content` to a file, which will be created if it does not exist yet or overwritten if it does exist.

Return the number of bytes written into the file.
"""
write(filename::AbstractString, a1, args...) = open(io->write(io, a1, args...), convert(String, filename)::String, "w")

"""
    read(filename::AbstractString)

Read the entire contents of a file as a `Vector{UInt8}`.

    read(filename::AbstractString, String)

Read the entire contents of a file as a string.

    read(filename::AbstractString, args...)

Open a file and read its contents. `args` is passed to `read`: this is equivalent to
`open(io->read(io, args...), filename)`.
"""
read(filename::AbstractString, args...) = open(io->read(io, args...), convert(String, filename)::String)

read(filename::AbstractString, ::Type{T}) where {T} = open(io->read(io, T), convert(String, filename)::String)

"""
    read!(stream::IO, array::AbstractArray)
    read!(filename::AbstractString, array::AbstractArray)

Read binary data from an I/O stream or file, filling in `array`.
"""
function read! end

read!(filename::AbstractString, a) = open(io->read!(io, a), convert(String, filename)::String)

"""
    readuntil(stream::IO, delim; keep::Bool = false)
    readuntil(filename::AbstractString, delim; keep::Bool = false)

Read a string from an I/O `stream` or a file, up to the given delimiter.
The delimiter can be a `UInt8`, `AbstractChar`, string, or vector.
Keyword argument `keep` controls whether the delimiter is included in the result.
The text is assumed to be encoded in UTF-8.

Return a `String` if `delim` is an `AbstractChar` or a string
or otherwise return a `Vector{typeof(delim)}`.   See also [`copyuntil`](@ref)
to instead write in-place to another stream (which can be a preallocated [`IOBuffer`](@ref)).

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> readuntil("my_file.txt", 'L')
"Julia"

julia> readuntil("my_file.txt", '.', keep = true)
"JuliaLang is a GitHub organization."

julia> rm("my_file.txt")
```
"""
readuntil(filename::AbstractString, delim; kw...) = open(io->readuntil(io, delim; kw...), convert(String, filename)::String)
readuntil(stream::IO, delim::UInt8; kw...) = _unsafe_take!(copyuntil(IOBuffer(sizehint=16), stream, delim; kw...))
readuntil(stream::IO, delim::Union{AbstractChar, AbstractString}; kw...) = String(_unsafe_take!(copyuntil(IOBuffer(sizehint=16), stream, delim; kw...)))
readuntil(stream::IO, delim::T; keep::Bool=false) where T = _copyuntil(Vector{T}(), stream, delim, keep)


"""
    copyuntil(out::IO, stream::IO, delim; keep::Bool = false)
    copyuntil(out::IO, filename::AbstractString, delim; keep::Bool = false)

Copy a string from an I/O `stream` or a file, up to the given delimiter, to
the `out` stream, returning `out`.
The delimiter can be a `UInt8`, `AbstractChar`, string, or vector.
Keyword argument `keep` controls whether the delimiter is included in the result.
The text is assumed to be encoded in UTF-8.

Similar to [`readuntil`](@ref), which returns a `String`; in contrast,
`copyuntil` writes directly to `out`, without allocating a string.
(This can be used, for example, to read data into a pre-allocated [`IOBuffer`](@ref).)

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> String(take!(copyuntil(IOBuffer(), "my_file.txt", 'L')))
"Julia"

julia> String(take!(copyuntil(IOBuffer(), "my_file.txt", '.', keep = true)))
"JuliaLang is a GitHub organization."

julia> rm("my_file.txt")
```
"""
copyuntil(out::IO, filename::AbstractString, delim; kw...) = open(io->copyuntil(out, io, delim; kw...), convert(String, filename)::String)

"""
    readline(io::IO=stdin; keep::Bool=false)
    readline(filename::AbstractString; keep::Bool=false)

Read a single line of text from the given I/O stream or file (defaults to `stdin`).
When reading from a file, the text is assumed to be encoded in UTF-8. Lines in the
input end with `'\\n'` or `"\\r\\n"` or the end of an input stream. When `keep` is
false (as it is by default), these trailing newline characters are removed from the
line before it is returned. When `keep` is true, they are returned as part of the
line.

Return a `String`.   See also [`copyline`](@ref) to instead write in-place
to another stream (which can be a preallocated [`IOBuffer`](@ref)).

See also [`readuntil`](@ref) for reading until more general delimiters.

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> readline("my_file.txt")
"JuliaLang is a GitHub organization."

julia> readline("my_file.txt", keep=true)
"JuliaLang is a GitHub organization.\\n"

julia> rm("my_file.txt")
```
```julia-repl
julia> print("Enter your name: ")
Enter your name:

julia> your_name = readline()
Logan
"Logan"
```
"""
readline(filename::AbstractString; keep::Bool=false) =
    open(io -> readline(io; keep), filename)
readline(s::IO=stdin; keep::Bool=false) =
    String(_unsafe_take!(copyline(IOBuffer(sizehint=16), s; keep)))

"""
    copyline(out::IO, io::IO=stdin; keep::Bool=false)
    copyline(out::IO, filename::AbstractString; keep::Bool=false)

Copy a single line of text from an I/O `stream` or a file to the `out` stream,
returning `out`.

When reading from a file, the text is assumed to be encoded in UTF-8. Lines in the
input end with `'\\n'` or `"\\r\\n"` or the end of an input stream. When `keep` is
false (as it is by default), these trailing newline characters are removed from the
line before it is returned. When `keep` is true, they are returned as part of the
line.

Similar to [`readline`](@ref), which returns a `String`; in contrast,
`copyline` writes directly to `out`, without allocating a string.
(This can be used, for example, to read data into a pre-allocated [`IOBuffer`](@ref).)

See also [`copyuntil`](@ref) for reading until more general delimiters.

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> String(take!(copyline(IOBuffer(), "my_file.txt")))
"JuliaLang is a GitHub organization."

julia> String(take!(copyline(IOBuffer(), "my_file.txt", keep=true)))
"JuliaLang is a GitHub organization.\\n"

julia> rm("my_file.txt")
```
"""
copyline(out::IO, filename::AbstractString; keep::Bool=false) =
    open(io -> copyline(out, io; keep), filename)

# fallback to optimized methods for IOBuffer in iobuffer.jl
function copyline(out::IO, s::IO; keep::Bool=false)
    if keep
        return copyuntil(out, s, 0x0a, keep=true)
    else
        # more complicated to deal with CRLF logic
        while !eof(s)
            b = read(s, UInt8)
            b == 0x0a && break
            if b == 0x0d && !eof(s)
                b = read(s, UInt8)
                b == 0x0a && break
                write(out, 0x0d)
            end
            write(out, b)
        end
        return out
    end
end

"""
    readlines(io::IO=stdin; keep::Bool=false)
    readlines(filename::AbstractString; keep::Bool=false)

Read all lines of an I/O stream or a file as a vector of strings. Behavior is
equivalent to saving the result of reading [`readline`](@ref) repeatedly with the same
arguments and saving the resulting lines as a vector of strings.  See also
[`eachline`](@ref) to iterate over the lines without reading them all at once.

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> readlines("my_file.txt")
2-element Vector{String}:
 "JuliaLang is a GitHub organization."
 "It has many members."

julia> readlines("my_file.txt", keep=true)
2-element Vector{String}:
 "JuliaLang is a GitHub organization.\\n"
 "It has many members.\\n"

julia> rm("my_file.txt")
```
"""
function readlines(filename::AbstractString; kw...)
    open(filename) do f
        readlines(f; kw...)
    end
end
readlines(s=stdin; kw...) = collect(eachline(s; kw...))

## byte-order mark, ntoh & hton ##

let a = UInt32[0x01020304]
    endian_bom = GC.@preserve a unsafe_load(convert(Ptr{UInt8}, pointer(a)))
    global ntoh, hton, ltoh, htol
    if endian_bom == 0x01
        ntoh(x) = x
        hton(x) = x
        ltoh(x) = bswap(x)
        htol(x) = bswap(x)
        const global ENDIAN_BOM = 0x01020304
    elseif endian_bom == 0x04
        ntoh(x) = bswap(x)
        hton(x) = bswap(x)
        ltoh(x) = x
        htol(x) = x
        const global ENDIAN_BOM = 0x04030201
    else
        error("seriously? what is this machine?")
    end
end

"""
    ENDIAN_BOM

The 32-bit byte-order-mark indicates the native byte order of the host machine.
Little-endian machines will contain the value `0x04030201`. Big-endian machines will contain
the value `0x01020304`.
"""
ENDIAN_BOM

"""
    ntoh(x)

Convert the endianness of a value from Network byte order (big-endian) to that used by the Host.
"""
ntoh(x)

"""
    hton(x)

Convert the endianness of a value from that used by the Host to Network byte order (big-endian).
"""
hton(x)

"""
    ltoh(x)

Convert the endianness of a value from Little-endian to that used by the Host.
"""
ltoh(x)

"""
    htol(x)

Convert the endianness of a value from that used by the Host to Little-endian.
"""
htol(x)


"""
    isreadonly(io)::Bool

Determine whether a stream is read-only.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> isreadonly(io)
true

julia> io = IOBuffer();

julia> isreadonly(io)
false
```
"""
isreadonly(s) = isreadable(s) && !iswritable(s)

## binary I/O ##

write(io::IO, x) = throw(MethodError(write, (io, x)))
function write(io::IO, x1, xs...)
    written::Int = write(io, x1)
    for x in xs
        written += write(io, x)
    end
    return written
end

@noinline unsafe_write(s::IO, p::Ref{T}, n::Integer) where {T} =
    unsafe_write(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_write(s::IO, p::Ptr, n::Integer) = unsafe_write(s, convert(Ptr{UInt8}, p), convert(UInt, n))
function write(s::IO, x::Ref{T}) where {T}
    x isa Ptr && error("write cannot copy from a Ptr")
    if isbitstype(T)
        Int(unsafe_write(s, x, Core.sizeof(T)))
    else
        write(s, x[])
    end
end
write(s::IO, x::Int8) = write(s, reinterpret(UInt8, x))
function write(s::IO, x::Union{Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,Float16,Float32,Float64})
    return unsafe_write(s, Ref(x), Core.sizeof(x))
end

write(s::IO, x::Bool) = write(s, UInt8(x))
write(to::IO, p::Ptr) = write(to, convert(UInt, p))

function write(s::IO, A::AbstractArray)
    if !isbitstype(eltype(A))
        error("`write` is not supported on non-isbits arrays")
    end
    nb = 0
    r = Ref{eltype(A)}()
    for a in A
        r[] = a
        nb += @noinline unsafe_write(s, r, Core.sizeof(r)) # r must be heap-allocated
    end
    return nb
end

function write(s::IO, A::StridedArray)
    if !isbitstype(eltype(A))
        error("`write` is not supported on non-isbits arrays")
    end
    _checkcontiguous(Bool, A) &&
        return GC.@preserve A unsafe_write(s, pointer(A), elsize(A) * length(A))
    sz::Dims = size(A)
    st::Dims = strides(A)
    msz, mst, n = merge_adjacent_dim(sz, st)
    mst == 1 || return invoke(write, Tuple{IO, AbstractArray}, s, A)
    n == ndims(A) &&
        return GC.@preserve A unsafe_write(s, pointer(A), elsize(A) * length(A))
    sz′, st′ = tail(sz), tail(st)
    while n > 1
        sz′ = (tail(sz′)..., 1)
        st′ = (tail(st′)..., 0)
        n -= 1
    end
    GC.@preserve A begin
        nb = 0
        iter = CartesianIndices(sz′)
        for I in iter
            p = pointer(A)
            for i in 1:length(sz′)
                p += elsize(A) * st′[i] * (I[i] - 1)
            end
            nb += unsafe_write(s, p, elsize(A) * msz)
        end
        return nb
    end
end

function write(io::IO, c::Char)
    u = bswap(reinterpret(UInt32, c))
    n = 0
    while true
        n += write(io, u % UInt8)
        (u >>= 8) == 0 && return n
    end
end
# write(io, ::AbstractChar) is not defined: implementations
# must provide their own encoding-specific method.

function write(io::IO, s::Symbol)
    pname = unsafe_convert(Ptr{UInt8}, s)
    return unsafe_write(io, pname, ccall(:strlen, Csize_t, (Cstring,), pname))
end

function write(to::IO, from::IO)
    n = 0
    while !eof(from)
        n += write(to, readavailable(from))
    end
    return n
end

@noinline unsafe_read(s::IO, p::Ref{T}, n::Integer) where {T} = unsafe_read(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_read(s::IO, p::Ptr, n::Integer) = unsafe_read(s, convert(Ptr{UInt8}, p), convert(UInt, n))
function read!(s::IO, x::Ref{T}) where {T}
    x isa Ptr && error("read! cannot copy into a Ptr")
    if isbitstype(T)
        unsafe_read(s, x, Core.sizeof(T))
    else
        x[] = read(s, T)
    end
    return x
end

read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s, UInt8))
function read(s::IO, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    r = Ref{T}(0)
    unsafe_read(s, r, Core.sizeof(T))
    return r[]
end

read(s::IO, ::Type{Bool}) = (read(s, UInt8) != 0)
read(s::IO, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(s, UInt))

function read!(s::IO, A::AbstractArray{T}) where {T}
    if isbitstype(T) && _checkcontiguous(Bool, A)
        GC.@preserve A unsafe_read(s, pointer(A), elsize(A) * length(A))
    else
        if isbitstype(T)
            r = Ref{T}()
            for i in eachindex(A)
                @noinline unsafe_read(s, r, Core.sizeof(r)) # r must be heap-allocated
                A[i] = r[]
            end
        else
            for i in eachindex(A)
                A[i] = read(s, T)
            end
        end
    end
    return A
end

function read!(s::IO, A::StridedArray{T}) where {T}
    if !isbitstype(T) || _checkcontiguous(Bool, A)
        return invoke(read!, Tuple{IO, AbstractArray}, s, A)
    end
    sz::Dims = size(A)
    st::Dims = strides(A)
    msz, mst, n = merge_adjacent_dim(sz, st)
    mst == 1 || return invoke(read!, Tuple{IO, AbstractArray}, s, A)
    if n == ndims(A)
        GC.@preserve A unsafe_read(s, pointer(A), elsize(A) * length(A))
    else
        sz′, st′ = tail(sz), tail(st)
        while n > 1
            sz′ = (tail(sz′)..., 1)
            st′ = (tail(st′)..., 0)
            n -= 1
        end
        GC.@preserve A begin
            iter = CartesianIndices(sz′)
            for I in iter
                p = pointer(A)
                for i in 1:length(sz′)
                    p += elsize(A) * st′[i] * (I[i] - 1)
                end
                unsafe_read(s, p, elsize(A) * msz)
            end
        end
    end
    return A
end

function read(io::IO, ::Type{Char})
    b0 = read(io, UInt8)::UInt8
    l = 0x08 * (0x04 - UInt8(leading_ones(b0)))
    c = UInt32(b0) << 24
    if l ≤ 0x10
        s = 16
        while s ≥ l && !eof(io)::Bool
            peek(io) & 0xc0 == 0x80 || break
            b = read(io, UInt8)::UInt8
            c |= UInt32(b) << s
            s -= 8
        end
    end
    return reinterpret(Char, c)
end
# read(io, T) is not defined for other AbstractChar: implementations
# must provide their own encoding-specific method.

function copyuntil(out::IO, s::IO, delim::AbstractChar; keep::Bool=false)
    if delim ≤ '\x7f'
        return copyuntil(out, s, delim % UInt8; keep)
    end
    for c in readeach(s, Char)
        if c == delim
            keep && write(out, c)
            break
        end
        write(out, c)
    end
    return out
end

# note: optimized methods of copyuntil for IOStreams and delim::UInt8 in iostream.jl
#       and for IOBuffer with delim::UInt8 in iobuffer.jl
copyuntil(out::IO, s::IO, delim; keep::Bool=false) = _copyuntil(out, s, delim, keep)

# supports out::Union{IO, AbstractVector} for use with both copyuntil & readuntil
function _copyuntil(out, s::IO, delim::T, keep::Bool) where T
    output! = isa(out, IO) ? write : push!
    for c in readeach(s, T)
        if c == delim
            keep && output!(out, c)
            break
        end
        output!(out, c)
    end
    return out
end

# requires that indices for target are the integer unit range from firstindex to lastindex
# returns whether the delimiter was matched
# uses the Knuth–Morris–Pratt_algorithm, with the first and second cache entries unrolled
# For longer targets, the cache improves the big-O efficiency of scanning of sequences
# with repeated patterns
# Each cache entry tells us which index we should start the search at.
# We assume this is unlikely, so we only lazy-initialize as much of the cache as we need to use
# When we allocate the cache, we initialize it to 0 (and offset by the first index afterwards).
# Suppose target is:
#    Index:  1245689
#    Value: "aδcaδcx"
# We would set the cache to
#    0 0 0 1 2 3 4 0
# So after if we mismatch after the second aδc sequence,
# we can immediately jump back to index 5 (4 + 1).
function readuntil_vector!(io::IO, target::AbstractVector{T}, keep::Bool, out) where {T}
    first = firstindex(target)
    last = lastindex(target)
    len = last - first + 1
    if len < 1
        return true
    end
    pos = 0 # array-offset
    max_pos = 1 # array-offset in cache
    local cache # will be lazy initialized when needed
    output! = (isa(out, IO) ? write : push!)
    for c in readeach(io, T)
        # Backtrack until the next target character matches what was found
        while true
            c1 = target[pos + first]
            if c == c1
                pos += 1
                break
            elseif pos == 0
                break
            elseif pos == 1
                if !keep
                    output!(out, target[first])
                end
                pos = 0
            else
                # grow cache to contain up to `pos`
                if !@isdefined(cache)
                    cache = zeros(Int, len)
                end
                while max_pos < pos
                    ci = target[max_pos + first]
                    b = max_pos
                    max_pos += 1
                    while b != 0
                        b = cache[b]
                        cb = target[b + first]
                        if ci == cb
                            cache[max_pos] = b + 1
                            break
                        end
                    end
                end
                # read new position from cache
                pos1 = cache[pos]
                if !keep
                    # and add the removed prefix from the target to the output
                    # if not always keeping the match
                    for b in 1:(pos - pos1)
                        output!(out, target[b - 1 + first])
                    end
                end
                pos = pos1
            end
        end
        if keep || pos == 0
            output!(out, c)
        end
        pos == len && return true
    end
    if !keep
        # failed early without finishing the match,
        # add the partial match to the output
        # if not always keeping the match
        for b in 1:pos
            output!(out, target[b - 1 + first])
        end
    end
    return false
end

function copyuntil(out::IO, io::IO, target::AbstractString; keep::Bool=false)
    # small-string target optimizations
    x = Iterators.peel(target)
    isnothing(x) && return out
    c, rest = x
    if isempty(rest) && c <= '\x7f'
        return copyuntil(out, io, c % UInt8; keep)
    end
    # convert String to a utf8-byte-iterator
    if !(target isa String) && !(target isa SubString{String})
        target = String(target)
    end
    target = codeunits(target)::AbstractVector
    return copyuntil(out, io, target, keep=keep)
end

function readuntil(io::IO, target::AbstractVector{T}; keep::Bool=false) where T
    out = (T === UInt8 ? resize!(StringVector(16), 0) : Vector{T}())
    readuntil_vector!(io, target, keep, out)
    return out
end
copyuntil(out::IO, io::IO, target::AbstractVector; keep::Bool=false) =
    (readuntil_vector!(io, target, keep, out); out)

"""
    readchomp(x)

Read the entirety of `x` as a string and remove a single trailing newline
if there is one. Equivalent to `chomp(read(x, String))`.

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\nIt has many members.\\n");

julia> readchomp("my_file.txt")
"JuliaLang is a GitHub organization.\\nIt has many members."

julia> rm("my_file.txt");
```
"""
readchomp(x) = chomp(read(x, String))

# read up to nb bytes into nb, returning # bytes read

"""
    readbytes!(stream::IO, b::AbstractVector{UInt8}, nb=length(b))

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.
"""
function readbytes!(s::IO, b::AbstractArray{UInt8}, nb=length(b))
    require_one_based_indexing(b)
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
function read(s::IO, nb::Integer = typemax(Int))
    # Let readbytes! grow the array progressively by default
    # instead of taking of risk of over-allocating
    b = Vector{UInt8}(undef, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    return resize!(b, nr)
end

read(s::IO, ::Type{String}) = String(read(s)::Vector{UInt8})
read(s::IO, T::Type) = error("The IO stream does not support reading objects of type $T.")

## high-level iterator interfaces ##

struct EachLine{IOT <: IO}
    stream::IOT
    ondone::Function
    keep::Bool
    EachLine(stream::IO=stdin; ondone::Function=()->nothing, keep::Bool=false) =
        new{typeof(stream)}(stream, ondone, keep)
end

"""
    eachline(io::IO=stdin; keep::Bool=false)
    eachline(filename::AbstractString; keep::Bool=false)

Create an iterable `EachLine` object that will yield each line from an I/O stream
or a file. Iteration calls [`readline`](@ref) on the stream argument repeatedly with
`keep` passed through, determining whether trailing end-of-line characters are
retained. When called with a file name, the file is opened once at the beginning of
iteration and closed at the end. If iteration is interrupted, the file will be
closed when the `EachLine` object is garbage collected.

To iterate over each line of a `String`, `eachline(IOBuffer(str))` can be used.

[`Iterators.reverse`](@ref) can be used on an `EachLine` object to read the lines
in reverse order (for files, buffers, and other I/O streams supporting [`seek`](@ref)),
and [`first`](@ref) or [`last`](@ref) can be used to extract the initial or final
lines, respectively.

# Examples
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\n It has many members.\\n");

julia> for line in eachline("my_file.txt")
           print(line)
       end
JuliaLang is a GitHub organization. It has many members.

julia> rm("my_file.txt");
```

!!! compat "Julia 1.8"
       Julia 1.8 is required to use `Iterators.reverse` or `last` with `eachline` iterators.
"""
function eachline(stream::IO=stdin; keep::Bool=false)
    EachLine(stream, keep=keep)::EachLine
end

function eachline(filename::AbstractString; keep::Bool=false)
    s = open(filename)
    EachLine(s, ondone=()->close(s), keep=keep)::EachLine
end

function iterate(itr::EachLine, state=nothing)
    eof(itr.stream) && return (itr.ondone(); nothing)
    (readline(itr.stream, keep=itr.keep), nothing)
end

eltype(::Type{<:EachLine}) = String

IteratorSize(::Type{<:EachLine}) = SizeUnknown()

isdone(itr::EachLine, state...) = eof(itr.stream)

# Reverse-order iteration for the EachLine iterator for seekable streams,
# which works by reading the stream from the end in 4kiB chunks.
function iterate(r::Iterators.Reverse{<:EachLine})
    p0 = position(r.itr.stream)
    seekend(r.itr.stream) # may throw if io is non-seekable
    p = position(r.itr.stream)
    # chunks = circular buffer of 4kiB blocks read from end of stream
    chunks = empty!(Vector{Vector{UInt8}}(undef, 2)) # allocate space for 2 buffers (common case)
    inewline = jnewline = 0
    while p > p0 && inewline == 0 # read chunks until we find a newline or we read whole file
        chunk = Vector{UInt8}(undef, min(4096, p-p0))
        p -= length(chunk)
        readbytes!(seek(r.itr.stream, p), chunk)
        pushfirst!(chunks, chunk)
        inewline = something(findlast(==(UInt8('\n')), chunk), 0)
        if length(chunks) == 1 && inewline == length(chunks[1])
            # found newline at end of file … keep looking
            jnewline = inewline
            inewline = something(findprev(==(UInt8('\n')), chunk, inewline-1), 0)
        end
    end
    return iterate(r, (; p0, p, chunks, ichunk=1, inewline, jchunk=length(chunks), jnewline = jnewline == 0 && !isempty(chunks) ? length(chunks[end]) : jnewline))
end
function iterate(r::Iterators.Reverse{<:EachLine}, state)
    function _stripnewline(keep, pos, data)
        # strip \n or \r\n from data[pos] by decrementing pos
        if !keep && pos > 0 && data[pos] == UInt8('\n')
            pos -= 1
            pos -= pos > 0 && data[pos] == UInt8('\r')
        end
        return pos
    end
    # state tuple: p0 = initial file position, p = current position,
    #              chunks = circular array of chunk buffers,
    #              current line is from chunks[ichunk][inewline+1] to chunks[jchunk][jnewline]
    p0, p, chunks, ichunk, inewline, jchunk, jnewline = state
    if inewline == 0 # no newline found, remaining line = rest of chunks (if any)
        isempty(chunks) && return (r.itr.ondone(); nothing)
        buf = IOBuffer(sizehint = ichunk==jchunk ? jnewline : 4096)
        while ichunk != jchunk
            write(buf, chunks[ichunk])
            ichunk = ichunk == length(chunks) ? 1 : ichunk + 1
        end
        chunk = chunks[jchunk]
        write(buf, view(chunk, 1:jnewline))
        buf.size = _stripnewline(r.itr.keep, buf.size, buf.data)
        empty!(chunks) # will cause next iteration to terminate
        seekend(r.itr.stream) # reposition to end of stream for isdone
        s = String(_unsafe_take!(buf))
    else
        # extract the string from chunks[ichunk][inewline+1] to chunks[jchunk][jnewline]
        if ichunk == jchunk # common case: current and previous newline in same chunk
            chunk = chunks[ichunk]
            s = String(view(chunk, inewline+1:_stripnewline(r.itr.keep, jnewline, chunk)))
        else
            buf = IOBuffer(sizehint=max(128, length(chunks[ichunk])-inewline+jnewline))
            write(buf, view(chunks[ichunk], inewline+1:length(chunks[ichunk])))
            i = ichunk
            while true
                i = i == length(chunks) ? 1 : i + 1
                i == jchunk && break
                write(buf, chunks[i])
            end
            write(buf, view(chunks[jchunk], 1:jnewline))
            buf.size = _stripnewline(r.itr.keep, buf.size, buf.data)
            s = String(_unsafe_take!(buf))

            # overwrite obsolete chunks (ichunk+1:jchunk)
            i = jchunk
            while i != ichunk
                chunk = chunks[i]
                p -= length(resize!(chunk, min(4096, p-p0)))
                readbytes!(seek(r.itr.stream, p), chunk)
                i = i == 1 ? length(chunks) : i - 1
            end
        end

        # find the newline previous to inewline
        jchunk = ichunk
        jnewline = inewline
        while true
            inewline = something(findprev(==(UInt8('\n')), chunks[ichunk], inewline-1), 0)
            inewline > 0 && break
            ichunk = ichunk == 1 ? length(chunks) : ichunk - 1
            ichunk == jchunk && break # found nothing — may need to read more chunks
            inewline = length(chunks[ichunk])+1 # start for next findprev
        end

        # read more chunks to look for a newline (should rarely happen)
        if inewline == 0 && p > p0
            ichunk = jchunk + 1
            while true
                chunk = Vector{UInt8}(undef, min(4096, p-p0))
                p -= length(chunk)
                readbytes!(seek(r.itr.stream, p), chunk)
                insert!(chunks, ichunk, chunk)
                inewline = something(findlast(==(UInt8('\n')), chunk), 0)
                (p == p0 || inewline > 0) && break
            end
        end
    end
    return (s, (; p0, p, chunks, ichunk, inewline, jchunk, jnewline))
end
isdone(r::Iterators.Reverse{<:EachLine}, state) = isempty(state.chunks)
isdone(r::Iterators.Reverse{<:EachLine}) = isdone(r.itr)

# use reverse iteration to get end of EachLines (if possible)
last(itr::EachLine) = first(Iterators.reverse(itr))

struct ReadEachIterator{T, IOT <: IO}
    stream::IOT
end

"""
    readeach(io::IO, T)

Return an iterable object yielding [`read(io, T)`](@ref).

See also [`skipchars`](@ref), [`eachline`](@ref), [`readuntil`](@ref).

!!! compat "Julia 1.6"
    `readeach` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.\\n It has many members.\\n");

julia> for c in readeach(io, Char)
           c == '\\n' && break
           print(c)
       end
JuliaLang is a GitHub organization.
```
"""
readeach(stream::IOT, T::Type) where IOT<:IO = ReadEachIterator{T,IOT}(stream)

iterate(itr::ReadEachIterator{T}, state=nothing) where T =
    eof(itr.stream) ? nothing : (read(itr.stream, T), nothing)

eltype(::Type{ReadEachIterator{T}}) where T = T

IteratorSize(::Type{<:ReadEachIterator}) = SizeUnknown()

isdone(itr::ReadEachIterator, state...) = eof(itr.stream)

# IOStream Marking
# Note that these functions expect that io.mark exists for
# the concrete IO type. This may not be true for IO types
# not in base.

"""
    mark(s::IO)

Add a mark at the current position of stream `s`. Return the marked position.

See also [`unmark`](@ref), [`reset`](@ref), [`ismarked`](@ref).
"""
function mark(io::IO)
    io.mark = position(io)
end

"""
    unmark(s::IO)

Remove a mark from stream `s`. Return `true` if the stream was marked, `false` otherwise.

See also [`mark`](@ref), [`reset`](@ref), [`ismarked`](@ref).
"""
function unmark(io::IO)
    !ismarked(io) && return false
    io.mark = -1
    return true
end

"""
    reset(s::IO)

Reset a stream `s` to a previously marked position, and remove the mark. Return the
previously marked position. Throw an error if the stream is not marked.

See also [`mark`](@ref), [`unmark`](@ref), [`ismarked`](@ref).
"""
function reset(io::T) where T<:IO
    ismarked(io) || throw(ArgumentError(LazyString(T, " not marked")))
    m = io.mark
    seek(io, m)
    io.mark = -1 # must be after seek, or seek may fail
    return m
end

"""
    ismarked(s::IO)

Return `true` if stream `s` is marked.

See also [`mark`](@ref), [`unmark`](@ref), [`reset`](@ref).
"""
ismarked(io::IO) = io.mark >= 0

# Make sure all IO streams support flush, even if only as a no-op,
# to make it easier to write generic I/O code.

flush(io::IO) = nothing

"""
    skipchars(predicate, io::IO; linecomment=nothing)

Advance the stream `io` such that the next-read character will be the first remaining for
which `predicate` returns `false`. If the keyword argument `linecomment` is specified, all
characters from that character until the start of the next line are ignored.

# Examples
```jldoctest
julia> buf = IOBuffer("    text")
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=1, mark=-1)

julia> skipchars(isspace, buf)
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=5, mark=-1)

julia> String(readavailable(buf))
"text"
```
"""
function skipchars(predicate, io::IO; linecomment=nothing)
    for c in readeach(io, Char)
        if c === linecomment
            readline(io)
        elseif !predicate(c)
            skip(io, -ncodeunits(c))
            break
        end
    end
    return io
end

"""
    countlines(io::IO; eol::AbstractChar = '\\n')
    countlines(filename::AbstractString; eol::AbstractChar = '\\n')

Read `io` until the end of the stream/file and count the number of lines. To specify a file
pass the filename as the first argument. EOL markers other than `'\\n'` are supported by
passing them as the second argument.  The last non-empty line of `io` is counted even if it does not
end with the EOL, matching the length returned by [`eachline`](@ref) and [`readlines`](@ref).

To count lines of a `String`, `countlines(IOBuffer(str))` can be used.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.\\n");

julia> countlines(io)
1

julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> countlines(io)
1

julia> eof(io) # counting lines moves the file pointer
true

julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> countlines(io, eol = '.')
1
```
```jldoctest
julia> write("my_file.txt", "JuliaLang is a GitHub organization.\\n")
36

julia> countlines("my_file.txt")
1

julia> countlines("my_file.txt", eol = 'n')
4

julia> rm("my_file.txt")

```
"""
function countlines(io::IO; eol::AbstractChar='\n')
    isascii(eol) || throw(ArgumentError("only ASCII line terminators are supported"))
    aeol = UInt8(eol)
    a = Vector{UInt8}(undef, 8192)
    nl = nb = 0
    while !eof(io)
        nb = readbytes!(io, a)
        @simd for i=1:nb
            @inbounds nl += a[i] == aeol
        end
    end
    if nb > 0 && a[nb] != aeol
        nl += 1 # final line is not terminated with eol
    end
    nl
end

countlines(f::AbstractString; eol::AbstractChar = '\n') = open(io->countlines(io, eol = eol), f)::Int

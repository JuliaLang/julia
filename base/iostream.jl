# This file is a part of Julia. License is MIT: https://julialang.org/license

## IOStream

const sizeof_ios_t = Int(ccall(:jl_sizeof_ios_t, Cint, ()))

mutable struct IOStream <: IO
    handle::Ptr{Cvoid}
    ios::Array{UInt8,1}
    name::AbstractString
    mark::Int64

    IOStream(name::AbstractString, buf::Array{UInt8,1}) = new(pointer(buf), buf, name, -1)
end
# TODO: delay adding finalizer, e.g. for memio with a small buffer, or
# in the case where we take! it.
function IOStream(name::AbstractString, finalize::Bool)
    buf = zeros(UInt8,sizeof_ios_t)
    x = IOStream(name, buf)
    if finalize
        finalizer(close, x)
    end
    return x
end
IOStream(name::AbstractString) = IOStream(name, true)

unsafe_convert(T::Type{Ptr{Cvoid}}, s::IOStream) = convert(T, pointer(s.ios))
show(io::IO, s::IOStream) = print(io, "IOStream(", s.name, ")")

"""
    fd(stream)

Return the file descriptor backing the stream or file. Note that this function only applies
to synchronous `File`'s and `IOStream`'s not to any of the asynchronous streams.
"""
fd(s::IOStream) = Int(ccall(:jl_ios_fd, Clong, (Ptr{Cvoid},), s.ios))

stat(s::IOStream) = stat(fd(s))
close(s::IOStream) = ccall(:ios_close, Cvoid, (Ptr{Cvoid},), s.ios)
isopen(s::IOStream) = ccall(:ios_isopen, Cint, (Ptr{Cvoid},), s.ios)!=0
function flush(s::IOStream)
    sigatomic_begin()
    bad = ccall(:ios_flush, Cint, (Ptr{Cvoid},), s.ios) != 0
    sigatomic_end()
    systemerror("flush", bad)
end
iswritable(s::IOStream) = ccall(:ios_get_writable, Cint, (Ptr{Cvoid},), s.ios)!=0
isreadable(s::IOStream) = ccall(:ios_get_readable, Cint, (Ptr{Cvoid},), s.ios)!=0

"""
    truncate(file, n)

Resize the file or buffer given by the first argument to exactly `n` bytes, filling
previously unallocated space with '\\0' if the file or buffer is grown.

# Examples
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.")
35

julia> truncate(io, 15)
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=15, maxsize=Inf, ptr=16, mark=-1)

julia> String(take!(io))
"JuliaLang is a "

julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.");

julia> truncate(io, 40);

julia> String(take!(io))
"JuliaLang is a GitHub organization.\\0\\0\\0\\0\\0"
```
"""
function truncate(s::IOStream, n::Integer)
    systemerror("truncate", ccall(:ios_trunc, Cint, (Ptr{Cvoid}, Csize_t), s.ios, n) != 0)
    return s
end

"""
    seek(s, pos)

Seek a stream to the given position.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> seek(io, 5);

julia> read(io, Char)
'L': ASCII/Unicode U+004c (category Lu: Letter, uppercase)
```
"""
function seek(s::IOStream, n::Integer)
    ret = ccall(:ios_seek, Int64, (Ptr{Cvoid}, Int64), s.ios, n)
    systemerror("seek", ret == -1)
    ret < -1 && error("seek failed")
    return s
end

"""
    seekstart(s)

Seek a stream to its beginning.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> seek(io, 5);

julia> read(io, Char)
'L': ASCII/Unicode U+004c (category Lu: Letter, uppercase)

julia> seekstart(io);

julia> read(io, Char)
'J': ASCII/Unicode U+004a (category Lu: Letter, uppercase)
```
"""
seekstart(s::IO) = seek(s,0)

"""
    seekend(s)

Seek a stream to its end.
"""
function seekend(s::IOStream)
    systemerror("seekend", ccall(:ios_seek_end, Int64, (Ptr{Cvoid},), s.ios) != 0)
    return s
end

"""
    skip(s, offset)

Seek a stream relative to the current position.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> seek(io, 5);

julia> skip(io, 10);

julia> read(io, Char)
'G': ASCII/Unicode U+0047 (category Lu: Letter, uppercase)
```
"""
function skip(s::IOStream, delta::Integer)
    ret = ccall(:ios_skip, Int64, (Ptr{Cvoid}, Int64), s.ios, delta)
    systemerror("skip", ret == -1)
    ret < -1 && error("skip failed")
    return s
end

"""
    position(s)

Get the current position of a stream.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> seek(io, 5);

julia> position(io)
5

julia> skip(io, 10);

julia> position(io)
15

julia> seekend(io);

julia> position(io)
35
```
"""
function position(s::IOStream)
    pos = ccall(:ios_pos, Int64, (Ptr{Cvoid},), s.ios)
    systemerror("position", pos == -1)
    return pos
end

eof(s::IOStream) = ccall(:ios_eof_blocking, Cint, (Ptr{Cvoid},), s.ios)!=0

## constructing and opening streams ##

# "own" means the descriptor will be closed with the IOStream

"""
    fdio([name::AbstractString, ]fd::Integer[, own::Bool=false]) -> IOStream

Create an `IOStream` object from an integer file descriptor. If `own` is `true`, closing
this object will close the underlying descriptor. By default, an `IOStream` is closed when
it is garbage collected. `name` allows you to associate the descriptor with a named file.
"""
function fdio(name::AbstractString, fd::Integer, own::Bool=false)
    s = IOStream(name)
    ccall(:ios_fd, Ptr{Cvoid}, (Ptr{Cvoid}, Clong, Cint, Cint),
          s.ios, fd, 0, own)
    return s
end
fdio(fd::Integer, own::Bool=false) = fdio(string("<fd ",fd,">"), fd, own)

"""
    open_flags(; keywords...) -> NamedTuple

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
    open(filename::AbstractString; keywords...) -> IOStream

Open a file in a mode specified by five boolean keyword arguments:

| Keyword    | Desciption             | Default                                 |
|:-----------|:-----------------------|:----------------------------------------|
| `read`     | open for reading       | `!write`                                |
| `write`    | open for writing       | `truncate \\| append`                   |
| `create`   | create if non-existent | `!read & write \\| truncate \\| append` |
| `truncate` | truncate to zero size  | `!read & write`                         |
| `append`   | seek to end            | `false`                                 |

The default when no keywords are passed is to open files for reading only.
Returns a stream for accessing the opened file.
"""
function open(fname::AbstractString;
    read     :: Union{Bool,Nothing} = nothing,
    write    :: Union{Bool,Nothing} = nothing,
    create   :: Union{Bool,Nothing} = nothing,
    truncate :: Union{Bool,Nothing} = nothing,
    append   :: Union{Bool,Nothing} = nothing,
)
    flags = open_flags(
        read = read,
        write = write,
        create = create,
        truncate = truncate,
        append = append,
    )
    s = IOStream(string("<file ",fname,">"))
    systemerror("opening file $fname",
                ccall(:ios_file, Ptr{Cvoid},
                      (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
                      s.ios, fname, flags.read, flags.write, flags.create, flags.truncate) == C_NULL)
    if flags.append
        systemerror("seeking to end of file $fname", ccall(:ios_seek_end, Int64, (Ptr{Cvoid},), s.ios) != 0)
    end
    return s
end

"""
    open(filename::AbstractString, [mode::AbstractString]) -> IOStream

Alternate syntax for open, where a string-based mode specifier is used instead of the five
booleans. The values of `mode` correspond to those from `fopen(3)` or Perl `open`, and are
equivalent to setting the following boolean groups:

| Mode | Description                   | Keywords                            |
|:-----|:------------------------------|:------------------------------------|
| `r`  | read                          | none                                |
| `w`  | write, create, truncate       | `write = true`                      |
| `a`  | write, create, append         | `append = true`                     |
| `r+` | read, write                   | `read = true, write = true`         |
| `w+` | read, write, create, truncate | `truncate = true, read = true`      |
| `a+` | read, write, create, append   | `append = true, read = true`        |

# Examples
```jldoctest
julia> io = open("myfile.txt", "w");

julia> write(io, "Hello world!");

julia> close(io);

julia> io = open("myfile.txt", "r");

julia> read(io, String)
"Hello world!"

julia> write(io, "This file is read only")
ERROR: ArgumentError: write failed, IOStream is not writeable
[...]

julia> close(io)

julia> io = open("myfile.txt", "a");

julia> write(io, "This stream is not read only")
28

julia> close(io)

julia> rm("myfile.txt")
```
"""
function open(fname::AbstractString, mode::AbstractString)
    mode == "r"  ? open(fname, read = true)                  :
    mode == "r+" ? open(fname, read = true, write = true)    :
    mode == "w"  ? open(fname, truncate = true)              :
    mode == "w+" ? open(fname, truncate = true, read = true) :
    mode == "a"  ? open(fname, append = true)                :
    mode == "a+" ? open(fname, append = true, read = true)   :
    throw(ArgumentError("invalid open mode: $mode"))
end

"""
    open(f::Function, args...; kwargs....)

Apply the function `f` to the result of `open(args...; kwargs...)` and close the resulting file
descriptor upon completion.

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           write(io, "Hello world!")
       end;

julia> open(f->read(f, String), "myfile.txt")
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

## low-level calls ##

function write(s::IOStream, b::UInt8)
    iswritable(s) || throw(ArgumentError("write failed, IOStream is not writeable"))
    Int(ccall(:ios_putc, Cint, (Cint, Ptr{Cvoid}), b, s.ios))
end

function unsafe_write(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    iswritable(s) || throw(ArgumentError("write failed, IOStream is not writeable"))
    return Int(ccall(:ios_write, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s.ios, p, nb))
end

# num bytes available without blocking
bytesavailable(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Cvoid},), s.ios)

readavailable(s::IOStream) = read!(s, Vector{UInt8}(undef, bytesavailable(s)))

function read(s::IOStream, ::Type{UInt8})
    b = ccall(:ios_getc, Cint, (Ptr{Cvoid},), s.ios)
    if b == -1
        throw(EOFError())
    end
    return b % UInt8
end

if ENDIAN_BOM == 0x04030201
function read(s::IOStream, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64}})
    return ccall(:jl_ios_get_nbyte_int, UInt64, (Ptr{Cvoid}, Csize_t), s.ios, sizeof(T)) % T
end
end

function unsafe_read(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    if ccall(:ios_readall, Csize_t,
             (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s, p, nb) != nb
        throw(EOFError())
    end
    nothing
end

## text I/O ##

take!(s::IOStream) =
    ccall(:jl_take_buffer, Vector{UInt8}, (Ptr{Cvoid},), s.ios)

function readuntil(s::IOStream, delim::UInt8; keep::Bool=false)
    ccall(:jl_readuntil, Array{UInt8,1}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, delim, 0, !keep)
end

# like readuntil, above, but returns a String without requiring a copy
function readuntil_string(s::IOStream, delim::UInt8, keep::Bool)
    ccall(:jl_readuntil, Ref{String}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, delim, 1, !keep)
end

function readline(s::IOStream; chomp=nothing, keep::Bool=false)
    if chomp !== nothing
        keep = !chomp
        depwarn("The `chomp=$chomp` argument to `readline` is deprecated in favor of `keep=$keep`.", :readline)
    end
    ccall(:jl_readuntil, Ref{String}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, '\n', 1, keep ? 0 : 2)
end

function readbytes_all!(s::IOStream, b::Array{UInt8}, nb)
    olb = lb = length(b)
    nr = 0
    GC.@preserve b while nr < nb
        if lb < nr+1
            lb = max(65536, (nr+1) * 2)
            resize!(b, lb)
        end
        nr += Int(ccall(:ios_readall, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
                        s.ios, pointer(b, nr+1), min(lb-nr, nb-nr)))
        eof(s) && break
    end
    if lb > olb && lb > nr
        resize!(b, nr) # shrink to just contain input data if was resized
    end
    return nr
end

function readbytes_some!(s::IOStream, b::Array{UInt8}, nb)
    olb = lb = length(b)
    if nb > lb
        resize!(b, nb)
    end
    nr = GC.@preserve b Int(ccall(:ios_read, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
                                  s.ios, pointer(b), nb))
    if lb > olb && lb > nr
        resize!(b, nr)
    end
    return nr
end

"""
    readbytes!(stream::IOStream, b::AbstractVector{UInt8}, nb=length(b); all::Bool=true)

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.

See [`read`](@ref) for a description of the `all` option.
"""
function readbytes!(s::IOStream, b::Array{UInt8}, nb=length(b); all::Bool=true)
    return all ? readbytes_all!(s, b, nb) : readbytes_some!(s, b, nb)
end

function read(s::IOStream)
    sz = 0
    try # filesize is just a hint, so ignore if it fails
        sz = filesize(s)
        pos = ccall(:ios_pos, Int64, (Ptr{Cvoid},), s.ios)
        if pos > 0
            sz -= pos
        end
    end
    b = StringVector(sz<=0 ? 1024 : sz)
    nr = readbytes_all!(s, b, typemax(Int))
    resize!(b, nr)
end

"""
    read(s::IOStream, nb::Integer; all=true)

Read at most `nb` bytes from `s`, returning a `Vector{UInt8}` of the bytes read.

If `all` is `true` (the default), this function will block repeatedly trying to read all
requested bytes, until an error or end-of-file occurs. If `all` is `false`, at most one
`read` call is performed, and the amount of data returned is device-dependent. Note that not
all stream types support the `all` option.
"""
function read(s::IOStream, nb::Integer; all::Bool=true)
    b = Vector{UInt8}(undef, nb)
    nr = readbytes!(s, b, nb, all=all)
    resize!(b, nr)
end

## Character streams ##

function peekchar(s::IOStream)
    chref = Ref{UInt32}()
    if ccall(:ios_peekutf8, Cint, (Ptr{Cvoid}, Ptr{UInt32}), s, chref) < 0
        return typemax(Char)
    end
    return Char(chref[])
end

function peek(s::IOStream)
    ccall(:ios_peekc, Cint, (Ptr{Cvoid},), s)
end

function peek(s::IO)
    mark(s)
    try read(s, UInt8)
    finally
        reset(s)
    end
end

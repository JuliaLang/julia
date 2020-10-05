# This file is a part of Julia. License is MIT: https://julialang.org/license

## IOStream

const sizeof_ios_t = Int(ccall(:jl_sizeof_ios_t, Cint, ()))

"""
    IOStream

A buffered IO stream wrapping an OS file descriptor.
Mostly used to represent files returned by [`open`](@ref).
"""
mutable struct IOStream <: IO
    handle::Ptr{Cvoid}
    ios::Array{UInt8,1}
    name::AbstractString
    mark::Int64
    lock::ReentrantLock
    _dolock::Bool

    IOStream(name::AbstractString, buf::Array{UInt8,1}) = new(pointer(buf), buf, name, -1, ReentrantLock(), true)
end

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

macro _lock_ios(s, expr)
    s = esc(s)
    quote
        l = ($s)._dolock
        temp = ($s).lock
        l && lock(temp)
        val = $(esc(expr))
        l && unlock(temp)
        val
    end
end

"""
    fd(stream)

Return the file descriptor backing the stream or file. Note that this function only applies
to synchronous `File`'s and `IOStream`'s not to any of the asynchronous streams.
"""
fd(s::IOStream) = Int(ccall(:jl_ios_fd, Clong, (Ptr{Cvoid},), s.ios))

stat(s::IOStream) = stat(fd(s))

isopen(s::IOStream) = ccall(:ios_isopen, Cint, (Ptr{Cvoid},), s.ios) != 0

function close(s::IOStream)
    bad = @_lock_ios s ccall(:ios_close, Cint, (Ptr{Cvoid},), s.ios) != 0
    systemerror("close", bad)
end

function flush(s::IOStream)
    sigatomic_begin()
    bad = @_lock_ios s ccall(:ios_flush, Cint, (Ptr{Cvoid},), s.ios) != 0
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
    err = @_lock_ios s ccall(:ios_trunc, Cint, (Ptr{Cvoid}, Csize_t), s.ios, n) != 0
    systemerror("truncate", err)
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
'L': ASCII/Unicode U+004C (category Lu: Letter, uppercase)
```
"""
function seek(s::IOStream, n::Integer)
    ret = @_lock_ios s ccall(:ios_seek, Int64, (Ptr{Cvoid}, Int64), s.ios, n)
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
'L': ASCII/Unicode U+004C (category Lu: Letter, uppercase)

julia> seekstart(io);

julia> read(io, Char)
'J': ASCII/Unicode U+004A (category Lu: Letter, uppercase)
```
"""
seekstart(s::IO) = seek(s,0)

"""
    seekend(s)

Seek a stream to its end.
"""
function seekend(s::IOStream)
    err = @_lock_ios s ccall(:ios_seek_end, Int64, (Ptr{Cvoid},), s.ios) != 0
    systemerror("seekend", err)
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
    ret = @_lock_ios s ccall(:ios_skip, Int64, (Ptr{Cvoid}, Int64), s.ios, delta)
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
    pos = @_lock_ios s ccall(:ios_pos, Int64, (Ptr{Cvoid},), s.ios)
    systemerror("position", pos == -1)
    return pos
end

function filesize(s::IOStream)
    sz = @_lock_ios s ccall(:ios_filesize, Int64, (Ptr{Cvoid},), s.ios)
    if sz == -1
        err = Libc.errno()
        throw(IOError(string("filesize: ", Libc.strerror(err), " for ", s.name), err))
    end
    return sz
end

_eof_nolock(s::IOStream) = ccall(:ios_eof_blocking, Cint, (Ptr{Cvoid},), s.ios) != 0
eof(s::IOStream) = @_lock_ios s _eof_nolock(s)

## constructing and opening streams ##

# "own" means the descriptor will be closed with the IOStream

"""
    fdio([name::AbstractString, ]fd::Integer[, own::Bool=false]) -> IOStream

Create an [`IOStream`](@ref) object from an integer file descriptor. If `own` is `true`, closing
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
    open(filename::AbstractString; lock = true, keywords...) -> IOStream

Open a file in a mode specified by five boolean keyword arguments:

| Keyword    | Description             | Default                                 |
|:-----------|:-----------------------|:----------------------------------------|
| `read`     | open for reading       | `!write`                                |
| `write`    | open for writing       | `truncate \\| append`                   |
| `create`   | create if non-existent | `!read & write \\| truncate \\| append` |
| `truncate` | truncate to zero size  | `!read & write`                         |
| `append`   | seek to end            | `false`                                 |

The default when no keywords are passed is to open files for reading only.
Returns a stream for accessing the opened file.

The `lock` keyword argument controls whether operations will be locked for
safe multi-threaded access.

!!! compat "Julia 1.5"
    The `lock` argument is available as of Julia 1.5.
"""
function open(fname::AbstractString; lock = true,
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
    if !lock
        s._dolock = false
    end
    systemerror("opening file $(repr(fname))",
                ccall(:ios_file, Ptr{Cvoid},
                      (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
                      s.ios, fname, flags.read, flags.write, flags.create, flags.truncate) == C_NULL)
    if flags.append
        systemerror("seeking to end of file $fname", ccall(:ios_seek_end, Int64, (Ptr{Cvoid},), s.ios) != 0)
    end
    return s
end

"""
    open(filename::AbstractString, [mode::AbstractString]; lock = true) -> IOStream

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

The `lock` keyword argument controls whether operations will be locked for
safe multi-threaded access.

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

!!! compat "Julia 1.5"
    The `lock` argument is available as of Julia 1.5.
"""
function open(fname::AbstractString, mode::AbstractString; lock = true)
    mode == "r"  ? open(fname, lock = lock, read = true)                  :
    mode == "r+" ? open(fname, lock = lock, read = true, write = true)    :
    mode == "w"  ? open(fname, lock = lock, truncate = true)              :
    mode == "w+" ? open(fname, lock = lock, truncate = true, read = true) :
    mode == "a"  ? open(fname, lock = lock, append = true)                :
    mode == "a+" ? open(fname, lock = lock, append = true, read = true)   :
    throw(ArgumentError("invalid open mode: $mode"))
end

## low-level calls ##

function write(s::IOStream, b::UInt8)
    iswritable(s) || throw(ArgumentError("write failed, IOStream is not writeable"))
    Int(@_lock_ios s ccall(:ios_putc, Cint, (Cint, Ptr{Cvoid}), b, s.ios))
end

function unsafe_write(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    iswritable(s) || throw(ArgumentError("write failed, IOStream is not writeable"))
    return Int(@_lock_ios s ccall(:ios_write, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s.ios, p, nb))
end

# num bytes available without blocking
bytesavailable(s::IOStream) = @_lock_ios s ccall(:jl_nb_available, Int32, (Ptr{Cvoid},), s.ios)

function readavailable(s::IOStream)
    lock(s.lock)
    nb = ccall(:jl_nb_available, Int32, (Ptr{Cvoid},), s.ios)
    if nb == 0
        ccall(:ios_fillbuf, Cssize_t, (Ptr{Cvoid},), s.ios)
        nb = ccall(:jl_nb_available, Int32, (Ptr{Cvoid},), s.ios)
    end
    a = Vector{UInt8}(undef, nb)
    nr = ccall(:ios_readall, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s, a, nb)
    if nr != nb
        unlock(s.lock)
        throw(EOFError())
    end
    unlock(s.lock)
    return a
end

function read(s::IOStream, ::Type{UInt8})
    b = @_lock_ios s ccall(:ios_getc, Cint, (Ptr{Cvoid},), s.ios)
    if b == -1
        throw(EOFError())
    end
    return b % UInt8
end

if ENDIAN_BOM == 0x04030201
function read(s::IOStream, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64}})
    n = sizeof(T)
    lock(s.lock)
    if ccall(:jl_ios_buffer_n, Cint, (Ptr{Cvoid}, Csize_t), s.ios, n) != 0
        unlock(s.lock)
        throw(EOFError())
    end
    x = ccall(:jl_ios_get_nbyte_int, UInt64, (Ptr{Cvoid}, Csize_t), s.ios, n) % T
    unlock(s.lock)
    return x
end

read(s::IOStream, ::Type{Float16}) = reinterpret(Float16, read(s, Int16))
read(s::IOStream, ::Type{Float32}) = reinterpret(Float32, read(s, Int32))
read(s::IOStream, ::Type{Float64}) = reinterpret(Float64, read(s, Int64))
end

function unsafe_read(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    nr = @_lock_ios s ccall(:ios_readall, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), s, p, nb)
    if nr != nb
        throw(EOFError())
    end
    nothing
end

## text I/O ##

take!(s::IOStream) =
    @_lock_ios s ccall(:jl_take_buffer, Vector{UInt8}, (Ptr{Cvoid},), s.ios)

function readuntil(s::IOStream, delim::UInt8; keep::Bool=false)
    @_lock_ios s ccall(:jl_readuntil, Array{UInt8,1}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, delim, 0, !keep)
end

# like readuntil, above, but returns a String without requiring a copy
function readuntil_string(s::IOStream, delim::UInt8, keep::Bool)
    @_lock_ios s ccall(:jl_readuntil, Ref{String}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, delim, 1, !keep)
end

function readline(s::IOStream; keep::Bool=false)
    @_lock_ios s ccall(:jl_readuntil, Ref{String}, (Ptr{Cvoid}, UInt8, UInt8, UInt8), s.ios, '\n', 1, keep ? 0 : 2)
end

function readbytes_all!(s::IOStream,
                        b::Union{Array{UInt8}, FastContiguousSubArray{UInt8,<:Any,<:Array{UInt8}}},
                        nb::Integer)
    olb = lb = length(b)
    nr = 0
    @_lock_ios s begin
    GC.@preserve b while nr < nb
        if lb < nr+1
            lb = max(65536, (nr+1) * 2)
            resize!(b, lb)
        end
        thisr = Int(ccall(:ios_readall, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
                          s.ios, pointer(b, nr+1), min(lb-nr, nb-nr)))
        nr += thisr
        (nr == nb || thisr == 0 || _eof_nolock(s)) && break
    end
    end
    if lb > olb && lb > nr
        resize!(b, max(olb, nr)) # shrink to just contain input data if was resized
    end
    return nr
end

function readbytes_some!(s::IOStream,
                         b::Union{Array{UInt8}, FastContiguousSubArray{UInt8,<:Any,<:Array{UInt8}}},
                         nb::Integer)
    olb = length(b)
    if nb > olb
        resize!(b, nb)
    end
    local nr
    @_lock_ios s begin
    nr = GC.@preserve b Int(ccall(:ios_read, Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
                                  s.ios, pointer(b), nb))
    end
    lb = length(b)
    if lb > olb && lb > nr
        resize!(b, max(olb, nr)) # shrink to just contain input data if was resized
    end
    return nr
end

"""
    readbytes!(stream::IOStream, b::AbstractVector{UInt8}, nb=length(b); all::Bool=true)

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.

If `all` is `true` (the default), this function will block repeatedly trying to read all
requested bytes, until an error or end-of-file occurs. If `all` is `false`, at most one
`read` call is performed, and the amount of data returned is device-dependent. Note that not
all stream types support the `all` option.
"""
function readbytes!(s::IOStream,
                    b::Union{Array{UInt8}, FastContiguousSubArray{UInt8,<:Any,<:Array{UInt8}}},
                    nb=length(b);
                    all::Bool=true)
    return all ? readbytes_all!(s, b, nb) : readbytes_some!(s, b, nb)
end

function read(s::IOStream)
    # First we try to fill the buffer. If that gives us the whole file,
    # copy it out and return. Otherwise look at the file size and use it
    # to prealloate space. Determining the size requires extra syscalls,
    # which we want to avoid for small files.
    @_lock_ios s begin
        nb = ccall(:ios_fillbuf, Cssize_t, (Ptr{Cvoid},), s.ios)
        if nb != -1
            b = StringVector(nb)
            readbytes_all!(s, b, nb)
        else
            sz = try # filesize is just a hint, so ignore if it fails
                filesize(s)
            catch ex
                ex isa IOError || rethrow()
                Int64(-1)
            end
            if sz > 0
                pos = position(s)
                if pos > 0
                    sz -= pos
                end
            end
            b = StringVector(sz < 0 ? 1024 : sz)
            nr = readbytes_all!(s, b, sz < 0 ? typemax(Int) : sz)
            resize!(b, nr)
        end
    end
    return b
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
    # When all=false we have to allocate a buffer of the requested size upfront
    # since a single call will be made
    b = Vector{UInt8}(undef, all && nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb, all=all)
    resize!(b, nr)
    return b
end

## peek ##

function peek(s::IOStream, ::Type{UInt8})
    b = @_lock_ios s ccall(:ios_peekc, Cint, (Ptr{Cvoid},), s.ios)
    if b == -1
        throw(EOFError())
    end
    return b % UInt8
end

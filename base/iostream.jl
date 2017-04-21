# This file is a part of Julia. License is MIT: https://julialang.org/license

## IOStream

const sizeof_ios_t = Int(ccall(:jl_sizeof_ios_t, Cint, ()))

mutable struct IOStream <: IO
    handle::Ptr{Void}
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
        finalizer(x, close)
    end
    return x
end
IOStream(name::AbstractString) = IOStream(name, true)

unsafe_convert(T::Type{Ptr{Void}}, s::IOStream) = convert(T, pointer(s.ios))
show(io::IO, s::IOStream) = print(io, "IOStream(", s.name, ")")
fd(s::IOStream) = Int(ccall(:jl_ios_fd, Clong, (Ptr{Void},), s.ios))
stat(s::IOStream) = stat(fd(s))
close(s::IOStream) = ccall(:ios_close, Void, (Ptr{Void},), s.ios)
isopen(s::IOStream) = ccall(:ios_isopen, Cint, (Ptr{Void},), s.ios)!=0
function flush(s::IOStream)
    sigatomic_begin()
    bad = ccall(:ios_flush, Cint, (Ptr{Void},), s.ios) != 0
    sigatomic_end()
    systemerror("flush", bad)
end
iswritable(s::IOStream) = ccall(:ios_get_writable, Cint, (Ptr{Void},), s.ios)!=0
isreadable(s::IOStream) = ccall(:ios_get_readable, Cint, (Ptr{Void},), s.ios)!=0

function truncate(s::IOStream, n::Integer)
    systemerror("truncate", ccall(:ios_trunc, Cint, (Ptr{Void}, Csize_t), s.ios, n) != 0)
    return s
end

function seek(s::IOStream, n::Integer)
    ret = ccall(:ios_seek, Int64, (Ptr{Void}, Int64), s.ios, n)
    systemerror("seek", ret == -1)
    ret < -1 && error("seek failed")
    return s
end

seekstart(s::IO) = seek(s,0)

function seekend(s::IOStream)
    systemerror("seekend", ccall(:ios_seek_end, Int64, (Ptr{Void},), s.ios) != 0)
    return s
end

function skip(s::IOStream, delta::Integer)
    ret = ccall(:ios_skip, Int64, (Ptr{Void}, Int64), s.ios, delta)
    systemerror("skip", ret == -1)
    ret < -1 && error("skip failed")
    return s
end

function position(s::IOStream)
    pos = ccall(:ios_pos, Int64, (Ptr{Void},), s.ios)
    systemerror("position", pos == -1)
    return pos
end

eof(s::IOStream) = ccall(:ios_eof_blocking, Cint, (Ptr{Void},), s.ios)!=0

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
    ccall(:ios_fd, Ptr{Void}, (Ptr{Void}, Clong, Cint, Cint),
          s.ios, fd, 0, own)
    return s
end
fdio(fd::Integer, own::Bool=false) = fdio(string("<fd ",fd,">"), fd, own)


"""
    open(filename::AbstractString, [read::Bool, write::Bool, create::Bool, truncate::Bool, append::Bool]) -> IOStream

Open a file in a mode specified by five boolean arguments. The default is to open files for
reading only. Returns a stream for accessing the file.
"""
function open(fname::AbstractString, rd::Bool, wr::Bool, cr::Bool, tr::Bool, ff::Bool)
    s = IOStream(string("<file ",fname,">"))
    systemerror("opening file $fname",
                ccall(:ios_file, Ptr{Void},
                      (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
                      s.ios, fname, rd, wr, cr, tr) == C_NULL)
    if ff
        systemerror("seeking to end of file $fname", ccall(:ios_seek_end, Int64, (Ptr{Void},), s.ios) != 0)
    end
    return s
end
open(fname::AbstractString) = open(fname, true, false, false, false, false)

"""
    open(filename::AbstractString, [mode::AbstractString]) -> IOStream

Alternate syntax for open, where a string-based mode specifier is used instead of the five
booleans. The values of `mode` correspond to those from `fopen(3)` or Perl `open`, and are
equivalent to setting the following boolean groups:

| Mode | Description                   |
|:-----|:------------------------------|
| r    | read                          |
| r+   | read, write                   |
| w    | write, create, truncate       |
| w+   | read, write, create, truncate |
| a    | write, create, append         |
| a+   | read, write, create, append   |
"""
function open(fname::AbstractString, mode::AbstractString)
    mode == "r"  ? open(fname, true , false, false, false, false) :
    mode == "r+" ? open(fname, true , true , false, false, false) :
    mode == "w"  ? open(fname, false, true , true , true , false) :
    mode == "w+" ? open(fname, true , true , true , true , false) :
    mode == "a"  ? open(fname, false, true , true , false, true ) :
    mode == "a+" ? open(fname, true , true , true , false, true ) :
    throw(ArgumentError("invalid open mode: $mode"))
end

"""
    open(f::Function, args...)

Apply the function `f` to the result of `open(args...)` and close the resulting file
descriptor upon completion.

**Example**: `open(readstring, "file.txt")`
"""
function open(f::Function, args...)
    io = open(args...)
    try
        f(io)
    finally
        close(io)
    end
end

## low-level calls ##

write(s::IOStream, b::UInt8) = Int(ccall(:ios_putc, Cint, (Cint, Ptr{Void}), b, s.ios))

function unsafe_write(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    if !iswritable(s)
        throw(ArgumentError("write failed, IOStream is not writeable"))
    end
    return Int(ccall(:ios_write, Csize_t, (Ptr{Void}, Ptr{Void}, Csize_t), s.ios, p, nb))
end

function write{T,N}(s::IOStream, a::SubArray{T,N,<:Array})
    if !isbits(T) || stride(a,1)!=1
        return invoke(write, Tuple{Any, AbstractArray}, s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N<=1
        return unsafe_write(s, pointer(a, 1), colsz)
    else
        for idxs in CartesianRange((1, size(a)[2:end]...))
            unsafe_write(s, pointer(a, idxs.I), colsz)
        end
        return colsz*trailingsize(a,2)
    end
end

# num bytes available without blocking
nb_available(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Void},), s.ios)

readavailable(s::IOStream) = read!(s, Vector{UInt8}(nb_available(s)))

function read(s::IOStream, ::Type{UInt8})
    b = ccall(:ios_getc, Cint, (Ptr{Void},), s.ios)
    if b == -1
        throw(EOFError())
    end
    return b % UInt8
end

if ENDIAN_BOM == 0x04030201
function read(s::IOStream, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64}})
    return ccall(:jl_ios_get_nbyte_int, UInt64, (Ptr{Void}, Csize_t), s.ios, sizeof(T)) % T
end
end

function unsafe_read(s::IOStream, p::Ptr{UInt8}, nb::UInt)
    if ccall(:ios_readall, Csize_t,
             (Ptr{Void}, Ptr{Void}, Csize_t), s, p, nb) != nb
        throw(EOFError())
    end
    nothing
end

## text I/O ##

function write(s::IOStream, c::Char)
    if !iswritable(s)
        throw(ArgumentError("write failed, IOStream is not writeable"))
    end
    Int(ccall(:ios_pututf8, Cint, (Ptr{Void}, UInt32), s.ios, c))
end
read(s::IOStream, ::Type{Char}) = Char(ccall(:jl_getutf8, UInt32, (Ptr{Void},), s.ios))

take!(s::IOStream) =
    ccall(:jl_take_buffer, Vector{UInt8}, (Ptr{Void},), s.ios)

function readuntil(s::IOStream, delim::UInt8)
    ccall(:jl_readuntil, Array{UInt8,1}, (Ptr{Void}, UInt8, UInt8, UInt8), s.ios, delim, 0, 0)
end

# like readuntil, above, but returns a String without requiring a copy
function readuntil_string(s::IOStream, delim::UInt8)
    ccall(:jl_readuntil, Ref{String}, (Ptr{Void}, UInt8, UInt8, UInt8), s.ios, delim, 1, false)
end

function readline(s::IOStream; chomp::Bool=true)
    ccall(:jl_readuntil, Ref{String}, (Ptr{Void}, UInt8, UInt8, UInt8), s.ios, '\n', 1, chomp)
end

function readbytes_all!(s::IOStream, b::Array{UInt8}, nb)
    olb = lb = length(b)
    nr = 0
    while nr < nb
        if lb < nr+1
            lb = max(65536, (nr+1) * 2)
            resize!(b, lb)
        end
        nr += Int(ccall(:ios_readall, Csize_t, (Ptr{Void}, Ptr{Void}, Csize_t),
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
    nr = Int(ccall(:ios_read, Csize_t, (Ptr{Void}, Ptr{Void}, Csize_t),
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
        pos = ccall(:ios_pos, Int64, (Ptr{Void},), s.ios)
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
    b = Array{UInt8,1}(nb)
    nr = readbytes!(s, b, nb, all=all)
    resize!(b, nr)
end

## Character streams ##
const _chtmp = Ref{Char}()
function peekchar(s::IOStream)
    if ccall(:ios_peekutf8, Cint, (Ptr{Void}, Ptr{Char}), s, _chtmp) < 0
        return typemax(Char)
    end
    return _chtmp[]
end

function peek(s::IOStream)
    ccall(:ios_peekc, Cint, (Ptr{Void},), s)
end

function skipchars(io::IOStream, pred; linecomment=nothing)
    while !eof(io)
        c = read(io, Char)
        if c === linecomment
            readline(io)
        elseif !pred(c)
            skip(io, -codelen(c))
            break
        end
    end
    return io
end

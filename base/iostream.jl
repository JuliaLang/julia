# This file is a part of Julia. License is MIT: http://julialang.org/license

## IOStream

const sizeof_ios_t = Int(ccall(:jl_sizeof_ios_t, Int32, ()))

type IOStream <: IO
    handle::Ptr{Void}
    ios::Array{UInt8,1}
    name::AbstractString
    mark::Int64

    IOStream(name::AbstractString, buf::Array{UInt8,1}) = new(pointer(buf), buf, name, -1)
end
# TODO: delay adding finalizer, e.g. for memio with a small buffer, or
# in the case where we takebuf it.
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
    s
end
iswritable(s::IOStream) = ccall(:ios_get_writable, Cint, (Ptr{Void},), s.ios)!=0
isreadable(s::IOStream) = ccall(:ios_get_readable, Cint, (Ptr{Void},), s.ios)!=0

function truncate(s::IOStream, n::Integer)
    systemerror("truncate", ccall(:ios_trunc, Int32, (Ptr{Void}, UInt), s.ios, n) != 0)
    return s
end

function seek(s::IOStream, n::Integer)
    ret = ccall(:ios_seek, FileOffset, (Ptr{Void}, FileOffset), s.ios, n)
    systemerror("seek", ret == -1)
    ret < -1 && error("seek failed")
    return s
end

seekstart(s::IO) = seek(s,0)

function seekend(s::IOStream)
    systemerror("seekend", ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios) != 0)
    return s
end

function skip(s::IOStream, delta::Integer)
    ret = ccall(:ios_skip, FileOffset, (Ptr{Void}, FileOffset), s.ios, delta)
    systemerror("skip", ret == -1)
    ret < -1 && error("skip failed")
    return s
end

function position(s::IOStream)
    pos = ccall(:ios_pos, FileOffset, (Ptr{Void},), s.ios)
    systemerror("position", pos == -1)
    return pos
end

eof(s::IOStream) = ccall(:ios_eof_blocking, Int32, (Ptr{Void},), s.ios)!=0

## constructing and opening streams ##

# "own" means the descriptor will be closed with the IOStream
function fdio(name::AbstractString, fd::Integer, own::Bool=false)
    s = IOStream(name)
    ccall(:ios_fd, Ptr{Void}, (Ptr{Void}, Clong, Int32, Int32),
          s.ios, fd, 0, own);
    return s
end
fdio(fd::Integer, own::Bool=false) = fdio(string("<fd ",fd,">"), fd, own)

function open(fname::AbstractString, rd::Bool, wr::Bool, cr::Bool, tr::Bool, ff::Bool)
    s = IOStream(string("<file ",fname,">"))
    systemerror("opening file $fname",
                ccall(:ios_file, Ptr{Void},
                      (Ptr{UInt8}, Cstring, Int32, Int32, Int32, Int32),
                      s.ios, fname, rd, wr, cr, tr) == C_NULL)
    if ff
        systemerror("seeking to end of file $fname", ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios) != 0)
    end
    return s
end
open(fname::AbstractString) = open(fname, true, false, false, false, false)

function open(fname::AbstractString, mode::AbstractString)
    mode == "r"  ? open(fname, true , false, false, false, false) :
    mode == "r+" ? open(fname, true , true , false, false, false) :
    mode == "w"  ? open(fname, false, true , true , true , false) :
    mode == "w+" ? open(fname, true , true , true , true , false) :
    mode == "a"  ? open(fname, false, true , true , false, true ) :
    mode == "a+" ? open(fname, true , true , true , false, true ) :
    throw(ArgumentError("invalid open mode: $mode"))
end

function open(f::Function, args...)
    io = open(args...)
    try
        f(io)
    finally
        close(io)
    end
end

## low-level calls ##

write(s::IOStream, b::UInt8) = Int(ccall(:ios_putc, Int32, (UInt8, Ptr{Void}), b, s.ios))

function write{T}(s::IOStream, a::Array{T})
    if isbits(T)
        if !iswritable(s)
            throw(ArgumentError("write failed, IOStream is not writeable"))
        end
        Int(ccall(:ios_write, UInt, (Ptr{Void}, Ptr{Void}, UInt),
                  s.ios, a, length(a)*sizeof(T)))
    else
        write_each(s, a)
    end
end

function write(s::IOStream, p::Ptr, nb::Integer)
    if !iswritable(s)
        throw(ArgumentError("write failed, IOStream is not writeable"))
    end
    Int(ccall(:ios_write, UInt, (Ptr{Void}, Ptr{Void}, UInt), s.ios, p, nb))
end

function write{T,N,A<:Array}(s::IOStream, a::SubArray{T,N,A})
    if !isbits(T) || stride(a,1)!=1
        return write_each(s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N<=1
        return write(s, pointer(a, 1), colsz)
    else
        for idxs in CartesianRange((1, size(a)[2:end]...))
            write(s, pointer(a, idxs.I), colsz)
        end
        return colsz*trailingsize(a,2)
    end
end

# num bytes available without blocking
nb_available(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Void},), s.ios)

function read(s::IOStream, ::Type{UInt8})
    b = ccall(:ios_getc, Int32, (Ptr{Void},), s.ios)
    if b == -1
        throw(EOFError())
    end
    b % UInt8
end

function read{T<:Union{UInt16, Int16, UInt32, Int32, UInt64, Int64}}(s::IOStream, ::Type{T})
    ccall(:jl_ios_get_nbyte_int, UInt64, (Ptr{Void}, Csize_t), s.ios, sizeof(T)) % T
end

function read!{T}(s::IOStream, a::Array{T})
    if isbits(T)
        nb = length(a)*sizeof(T)
        if ccall(:ios_readall, UInt,
                 (Ptr{Void}, Ptr{Void}, UInt), s.ios, a, nb) < nb
            throw(EOFError())
        end
    else
        for i in eachindex(a)
            a[i] = read(s, T)
        end
    end
    a
end

## text I/O ##

function write(s::IOStream, c::Char)
    if !iswritable(s)
        throw(ArgumentError("write failed, IOStream is not writeable"))
    end
    Int(ccall(:ios_pututf8, Int32, (Ptr{Void}, Char), s.ios, c))
end
read(s::IOStream, ::Type{Char}) = ccall(:jl_getutf8, Char, (Ptr{Void},), s.ios)

takebuf_string(s::IOStream) =
    ccall(:jl_takebuf_string, Any, (Ptr{Void},), s.ios)::ByteString

takebuf_array(s::IOStream) =
    ccall(:jl_takebuf_array, Vector{UInt8}, (Ptr{Void},), s.ios)

function takebuf_raw(s::IOStream)
    sz = position(s)
    buf = ccall(:jl_takebuf_raw, Ptr{UInt8}, (Ptr{Void},), s.ios)
    return buf, sz
end

function sprint(size::Integer, f::Function, args...)
    s = IOBuffer(Array(UInt8,size), true, true)
    truncate(s,0)
    f(s, args...)
    takebuf_string(s)
end

sprint(f::Function, args...) = sprint(0, f, args...)

write(x) = write(STDOUT::IO, x)

function readuntil(s::IOStream, delim::UInt8)
    ccall(:jl_readuntil, Array{UInt8,1}, (Ptr{Void}, UInt8), s.ios, delim)
end

function readbytes_all!(s::IOStream, b::Array{UInt8}, nb)
    olb = lb = length(b)
    nr = 0
    while nr < nb
        if lb < nr+1
            lb = max(65536, (nr+1) * 2)
            resize!(b, lb)
        end
        nr += Int(ccall(:ios_readall, UInt, (Ptr{Void}, Ptr{Void}, UInt),
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
    nr = Int(ccall(:ios_read, UInt, (Ptr{Void}, Ptr{Void}, UInt),
                   s.ios, pointer(b), nb))
    if lb > olb && lb > nr
        resize!(b, nr)
    end
    return nr
end

function readbytes!(s::IOStream, b::Array{UInt8}, nb=length(b); all::Bool=true)
    return all ? readbytes_all!(s, b, nb) : readbytes_some!(s, b, nb)
end

function readbytes(s::IOStream)
    sz = 0
    try # filesize is just a hint, so ignore if it fails
        sz = filesize(s)
        pos = ccall(:ios_pos, FileOffset, (Ptr{Void},), s.ios)
        if pos > 0
            sz -= pos
        end
    end
    b = Array(UInt8, sz<=0 ? 1024 : sz)
    nr = readbytes_all!(s, b, typemax(Int))
    resize!(b, nr)
end

function readbytes(s::IOStream, nb::Integer; all::Bool=true)
    b = Array(UInt8, nb)
    nr = readbytes!(s, b, nb, all=all)
    resize!(b, nr)
end

## Character streams ##
const _chtmp = Array(Char, 1)
function peekchar(s::IOStream)
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _chtmp) < 0
        return Char(-1)
    end
    return _chtmp[1]
end

function peek(s::IOStream)
    ccall(:ios_peekc, Int32, (Ptr{Void},), s)
end

function skipchars(s::IOStream, pred; linecomment::Char=Char(0xffffffff))
    ch = peekchar(s); status = Int(ch)
    while status >= 0 && (pred(ch) || ch == linecomment)
        if ch == linecomment
            readline(s)
        else
            read(s, Char)  # advance one character
        end
        ch = peekchar(s); status = Int(ch)
    end
    return s
end

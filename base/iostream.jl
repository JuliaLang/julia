## IOStream

const sizeof_ios_t = int(ccall(:jl_sizeof_ios_t, Int32, ()))

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

convert(T::Type{Ptr{Void}}, s::IOStream) = convert(T, s.ios)
show(io::IO, s::IOStream) = print(io, "IOStream(", s.name, ")")
fd(s::IOStream) = int(ccall(:jl_ios_fd, Clong, (Ptr{Void},), s.ios))
stat(s::IOStream) = stat(fd(s))
close(s::IOStream) = ccall(:ios_close, Void, (Ptr{Void},), s.ios)
isopen(s::IOStream) = bool(ccall(:ios_isopen, Cint, (Ptr{Void},), s.ios))
function flush(s::IOStream)
    sigatomic_begin()
    systemerror("flush", ccall(:ios_flush, Cint, (Ptr{Void},), s.ios) != 0)
    sigatomic_end()
    s
end
iswritable(s::IOStream) = bool(ccall(:ios_get_writable, Cint, (Ptr{Void},), s.ios))
isreadable(s::IOStream) = bool(ccall(:ios_get_readable, Cint, (Ptr{Void},), s.ios))
modestr(s::IO) = modestr(isreadable(s), iswritable(s))
modestr(r::Bool, w::Bool) = r ? (w ? "r+" : "r") : (w ? "w" : error("Neither readable nor writable"))

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

# For interfacing with C FILE* functions


immutable CFILE
    ptr::Ptr{Void}
end

function CFILE(s::IO)
    @unix_only FILEp = ccall(:fdopen, Ptr{Void}, (Cint, Ptr{UInt8}), convert(Cint, fd(s)), modestr(s))
    @windows_only FILEp = ccall(:_fdopen, Ptr{Void}, (Cint, Ptr{UInt8}), convert(Cint, fd(s)), modestr(s))
    systemerror("fdopen", FILEp == C_NULL)
    seek(CFILE(FILEp), position(s))
end

convert(::Type{CFILE}, s::IO) = CFILE(s)

function seek(h::CFILE, offset::Integer)
    systemerror("fseek", ccall(:fseek, Cint, (Ptr{Void}, Clong, Cint),
                               h.ptr, convert(Clong, offset), int32(0)) != 0)
    h
end

position(h::CFILE) = ccall(:ftell, Clong, (Ptr{Void},), h.ptr)

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
                      (Ptr{UInt8}, Ptr{UInt8}, Int32, Int32, Int32, Int32),
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
    error("invalid open mode: ", mode)
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

write(s::IOStream, b::UInt8) = int(ccall(:ios_putc, Int32, (UInt8, Ptr{Void}), b, s.ios))

function write{T}(s::IOStream, a::Array{T})
    if isbits(T)
        if !iswritable(s)
            error("attempt to write to a read-only IOStream")
        end
        int(ccall(:ios_write, UInt, (Ptr{Void}, Ptr{Void}, UInt),
                  s.ios, a, length(a)*sizeof(T)))
    else
        invoke(write, (IO, Array), s, a)
    end
end

function write(s::IOStream, p::Ptr, nb::Integer)
    if !iswritable(s)
        error("attempt to write to a read-only IOStream")
    end
    int(ccall(:ios_write, UInt, (Ptr{Void}, Ptr{Void}, UInt), s.ios, p, nb))
end

function write{T,N,A<:Array}(s::IOStream, a::SubArray{T,N,A})
    if !isbits(T) || stride(a,1)!=1
        return invoke(write, (Any, AbstractArray), s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N<=1
        return write(s, pointer(a, 1), colsz)
    else
        cartesianmap((idxs...)->write(s, pointer(a, idxs), colsz),
                     tuple(1, size(a)[2:end]...))
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
    uint8(b)
end

function read!{T}(s::IOStream, a::Array{T})
    if isbits(T)
        nb = length(a)*sizeof(T)
        if ccall(:ios_readall, UInt,
                 (Ptr{Void}, Ptr{Void}, UInt), s.ios, a, nb) < nb
            throw(EOFError())
        end
    else
        invoke(read!, (IO, Array), s, a)
    end
    a
end

## text I/O ##

function write(s::IOStream, c::Char)
    if !iswritable(s)
        error("attempt to write to a read-only IOStream")
    end
    int(ccall(:ios_pututf8, Int32, (Ptr{Void}, Char), s.ios, c))
end
read(s::IOStream, ::Type{Char}) = ccall(:jl_getutf8, Char, (Ptr{Void},), s.ios)

takebuf_string(s::IOStream) =
    ccall(:jl_takebuf_string, ByteString, (Ptr{Void},), s.ios)

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

function readbytes!(s::IOStream, b::Array{UInt8}, nb=length(b))
    olb = lb = length(b)
    nr = 0
    while !eof(s) && nr < nb
        if lb < nr+1
            lb = max(65536, (nr+1) * 2)
            resize!(b, lb)
        end
        nr += int(ccall(:ios_readall, UInt,
                        (Ptr{Void}, Ptr{Void}, UInt),
                        s.ios, pointer(b, nr+1), min(lb-nr, nb-nr)))
    end
    if lb > olb
        resize!(b, nr) # shrink to just contain input data if was resized
    end
    return nr
end

## Character streams ##
const _chtmp = Array(Char, 1)
function peekchar(s::IOStream)
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Char}), s, _chtmp) < 0
        return char(-1)
    end
    return _chtmp[1]
end

function peek(s::IOStream)
    ccall(:ios_peekc, Int32, (Ptr{Void},), s)
end

function skipchars(s::IOStream, pred; linecomment::Char=char(0xffffffff))
    ch = peekchar(s); status = int(ch)
    while status >= 0 && (pred(ch) || ch == linecomment)
        if ch == linecomment
            readline(s)
        else
            read(s, Char)  # advance one character
        end
        ch = peekchar(s); status = int(ch)
    end
    return s
end

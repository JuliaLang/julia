## core stream types ##

# the first argument to any IO MUST be a POINTER (to a JL_STREAM) or using show on it will cause memory corruption

# Generic IO functions

## byte-order mark, ntoh & hton ##

abstract ByteOrder

type HostByteOrder <: ByteOrder 
end

type ReverseByteOrder <: ByteOrder 
end

const ENDIAN_BOM = reinterpret(Uint32,uint8([1:4]))[1]

if ENDIAN_BOM == 0x01020304
    typealias NetworkByteOrder HostByteOrder 
    typealias LittleByteOrder ReverseByteOrder 
    ntoh(x) = identity(x)
    hton(x) = identity(x)
    ltoh(x) = bswap(x)
    htol(x) = bswap(x)
elseif ENDIAN_BOM == 0x04030201
    typealias NetworkByteOrder ReverseByteOrder
    typealias LittleByteOrder HostByteOrder
    ntoh(x) = bswap(x)
    hton(x) = bswap(x)
    ltoh(x) = identity(x)
    htol(x) = identity(x)
else
    error("seriously? what is this machine?")
end

## binary I/O ##

# all subtypes should implement this
write(s::IO, x::Uint8) = error(typeof(s)," does not support byte I/O")

write(io::IO, x) = throw(MethodError(write, (io, x)))
write(io::IO, xs...) = for x in xs write(io, x) end

function write(s::IO, x::Integer,::Type{NetworkByteOrder})
    sz = sizeof(x)
    for n = sz:-1:1
        write(s, uint8((x>>>((n-1)<<3))))
    end
    sz
end
function write(s::IO, x::Integer,::Type{LittleByteOrder})
    sz = sizeof(x)
    for n = 1:sz
        write(s, uint8((x>>>((n-1)<<3))))
    end
    sz
end

write(s::IO, x::Bool) = write(s, uint8(x))
#write{O<:ByteOrder}(s::IO, x::Float16, ::Type{O}) = write(s, reinterpret(Int16,x), O)
write{O<:ByteOrder}(s::IO, x::Float32, ::Type{O}) = write(s, reinterpret(Int32,x), O)
write{O<:ByteOrder}(s::IO, x::Float64, ::Type{O}) = write(s, reinterpret(Int64,x), O)
function write{O<:ByteOrder}(s::IO, z::Complex, ::Type{O})
    write(s,real(z),O)
    write(s,imag(z),O)
end

write(s::IO, x::Number) = write(s, x, HostByteOrder)

function write(s::IO, a::AbstractArray)
    nb = 0
    for i = 1:length(a)
        nb += write(s, a[i])
    end
    nb
end
function write{O<:ByteOrder}(s::IO, a::AbstractArray, ::Type{O})
    nb = 0
    for i = 1:length(a)
        nb += write(s, a[i],O)
    end
    nb
end


function write(s::IO, c::Char)
    if c < 0x80
        write(s, uint8(c))
        return 1
    elseif c < 0x800
        write(s, uint8(( c >> 6          ) | 0xC0))
        write(s, uint8(( c        & 0x3F ) | 0x80))
        return 2
    elseif c < 0x10000
        write(s, uint8(( c >> 12         ) | 0xE0))
        write(s, uint8(((c >> 6)  & 0x3F ) | 0x80))
        write(s, uint8(( c        & 0x3F ) | 0x80))
        return 3
    elseif c < 0x110000
        write(s, uint8(( c >> 18         ) | 0xF0))
        write(s, uint8(((c >> 12) & 0x3F ) | 0x80))
        write(s, uint8(((c >> 6)  & 0x3F ) | 0x80))
        write(s, uint8(( c        & 0x3F ) | 0x80))
        return 4
    else
        return write(s, '\ufffd')
    end
end

# all subtypes should implement this
read(s::IO, x::Type{Uint8}) = error(typeof(s)," does not support byte I/O")

function read{T <: Integer}(s::IO, ::Type{T},::Type{LittleByteOrder})
    x = zero(T)
    for n = 1:sizeof(x)
        x |= (convert(T,read(s,Uint8))<<((n-1)<<3))
    end
    return x
end

function read{T <: Integer}(s::IO, ::Type{T},::Type{NetworkByteOrder})
    x = zero(T)
    for n = sizeof(x):-1:1
        x |= (convert(T,read(s,Uint8))<<((n-1)<<3))
    end
    return x
end

read(s::IO, ::Type{Bool})    = (read(s,Uint8)!=0)
read{O<:ByteOrder}(s::IO, ::Type{Float32}, ::Type{O}) = reinterpret(Float32,read(s,Int32,O))
read{O<:ByteOrder}(s::IO, ::Type{Float64}, ::Type{O}) = reinterpret(Float64,read(s,Int64,O))
function read{T<:Real,O<:ByteOrder}(s::IO, ::Type{Complex{T}}, ::Type{O})
    r = read(s,T,O)
    i = read(s,T,O)
    Complex{T}(r,i)
end

read{N<:Number}(s::IO, ::Type{N}) = read(s,N,HostByteOrder)


read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) =
    read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, map(int,tuple(d1,dims...)))
read{T}(s::IO, ::Type{T}, dims::Dims) = read(s, Array(T, dims))
function read{T}(s::IO, a::Array{T})
    for i = 1:length(a)
        a[i] = read(s, T)
    end
    return a
end

read{T,O<:ByteOrder}(s::IO, t::Type{T}, ::Type{O}, d1::Int, dims::Int...) =
    read(s, t, O, tuple(d1,dims...))
read{T,O<:ByteOrder}(s::IO, t::Type{T}, ::Type{O}, d1::Integer, dims::Integer...) =
    read(s, t, O, map(int,tuple(d1,dims...)))
read{T,O<:ByteOrder}(s::IO, ::Type{T}, ::Type{O}, dims::Dims) = read(s, Array(T, dims), O)

function read{T,O<:ByteOrder}(s::IO, a::Array{T}, ::Type{O})
    for i = 1:length(a)
        a[i] = read(s, T, O)
    end
    return a
end



function read(s::IO, ::Type{Char})
    ch = read(s, Uint8)
    if ch < 0x80
        return char(ch)
    end

    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::Uint32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = read(s, Uint8)
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    char(c)
end

function readuntil(s::IO, delim::Char)
    if delim < 0x80
        data = readuntil(s, uint8(delim))
        enc = byte_string_classify(data)
        return (enc==1) ? ASCIIString(data) : UTF8String(data)
    end
    out = IOBuffer()
    while !eof(s)
        c = read(s, Char)
        write(out, c)
        if c == delim
            break
        end
    end
    takebuf_string(out)
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
    out
end

readline(s::IO) = readuntil(s, '\n')

function readall(s::IO)
    out = IOBuffer()
    while !eof(s)
        a = read(s, Uint8)
        write(out, a)
    end
    takebuf_string(out)
end

readchomp(x) = chomp!(readall(x))

function readall(s::IO)
    b = readbytes(s)
    return is_valid_ascii(b) ? ASCIIString(b) : UTF8String(b)
end
readall(filename::String) = open(readall, filename)

## high-level iterator interfaces ##

type EachLine
    stream::IO
    ondone::Function
    EachLine(stream) = EachLine(stream, ()->nothing)
    EachLine(stream, ondone) = new(stream, ondone)
end
eachline(stream::IO) = EachLine(stream)

start(itr::EachLine) = nothing
function done(itr::EachLine, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
next(itr::EachLine, nada) = (readline(itr.stream), nothing)

function readlines(s, fx::Function...)
    a = {}
    for l in eachline(s)
        for f in fx
          l = f(l)
        end
        push!(a, l)
    end
    return a
end


## IOStream

const sizeof_ios_t = int(ccall(:jl_sizeof_ios_t, Int32, ()))

type IOStream <: IO
    handle::Ptr{Void}
    ios::Array{Uint8,1}
    name::String

    IOStream(name::String, buf::Array{Uint8,1}) = new(pointer(buf), buf, name)
end
# TODO: delay adding finalizer, e.g. for memio with a small buffer, or
# in the case where we takebuf it.
function IOStream(name::String, finalize::Bool)
    buf = zeros(Uint8,sizeof_ios_t)
    x = IOStream(name, buf)
    if finalize
        finalizer(x, close)
    end
    return x
end
IOStream(name::String) = IOStream(name, true)

convert(T::Type{Ptr{Void}}, s::IOStream) = convert(T, s.ios)
show(io::IO, s::IOStream) = print(io, "IOStream(", s.name, ")")
fd(s::IOStream) = int(ccall(:jl_ios_fd, Clong, (Ptr{Void},), s.ios))
close(s::IOStream) = ccall(:ios_close, Void, (Ptr{Void},), s.ios)
flush(s::IOStream) = ccall(:ios_flush, Void, (Ptr{Void},), s.ios)
isreadonly(s::IOStream) = bool(ccall(:ios_get_readonly, Cint, (Ptr{Void},), s.ios))

truncate(s::IOStream, n::Integer) =
    (ccall(:ios_trunc, Int32, (Ptr{Void}, Uint), s.ios, n)==0 ||
     error("truncate failed"))

seek(s::IOStream, n::Integer) =
    (ccall(:ios_seek, FileOffset, (Ptr{Void}, FileOffset), s.ios, n)==0 ||
     error("seek failed"))

seekstart(s::IO) = seek(s, 0)

seekend(s::IOStream) =
    (ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios)==0 ||
     error("seekend failed"))

skip(s::IOStream, delta::Integer) =
    (ccall(:ios_skip, FileOffset, (Ptr{Void}, FileOffset), s.ios, delta)==0 ||
     error("skip failed"))

position(s::IOStream) = ccall(:ios_pos, FileOffset, (Ptr{Void},), s.ios)

eof(s::IOStream) = bool(ccall(:jl_ios_eof, Int32, (Ptr{Void},), s.ios))

## constructing and opening streams ##

# "own" means the descriptor will be closed with the IOStream
function fdio(name::String, fd::Integer, own::Bool)
    s = IOStream(name)
    ccall(:ios_fd, Ptr{Void}, (Ptr{Void}, Clong, Int32, Int32),
          s.ios, fd, 0, own);
    return s
end
fdio(name::String, fd::Integer) = fdio(name, fd, false)
fdio(fd::Integer, own::Bool) = fdio(string("<fd ",fd,">"), fd, own)
fdio(fd::Integer) = fdio(fd, false)

function open(fname::String, rd::Bool, wr::Bool, cr::Bool, tr::Bool, ff::Bool)
    s = IOStream(string("<file ",fname,">"))
    if ccall(:ios_file, Ptr{Void},
             (Ptr{Uint8}, Ptr{Uint8}, Int32, Int32, Int32, Int32),
             s.ios, fname, rd, wr, cr, tr) == C_NULL
        error("could not open file ", fname)
    end
    if ff && ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios) != 0
        error("error seeking to end of file ", fname)
    end
    return s
end
open(fname::String) = open(fname, true, false, false, false, false)

function open(fname::String, mode::String)
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

write(s::IOStream, b::Uint8) = int(ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b, s.ios))

function write{T}(s::IOStream, a::Array{T})
    if isbits(T)
        if isreadonly(s)
            error("Cannot write to a read-only IOStream")
        end
        int(ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
                  s.ios, a, length(a)*sizeof(T)))
    else
        invoke(write, (IO, Array), s, a)
    end
end

function write(s::IOStream, p::Ptr, nb::Integer)
    if isreadonly(s)
        error("Cannot write to a read-only IOStream")
    end
    int(ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint), s.ios, p, nb))
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
                     tuple(1, size(a)[2:]...))
        return colsz*trailingsize(a,2)
    end
end

# num bytes available without blocking
nb_available(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Void},), s.ios)

function read(s::IOStream, ::Type{Uint8})
    b = ccall(:ios_getc, Int32, (Ptr{Void},), s.ios)
    if b == -1
        throw(EOFError())
    end
    uint8(b)
end

function read{T}(s::IOStream, a::Array{T})
    if isbits(T)
        nb = length(a)*sizeof(T)
        if ccall(:ios_readall, Uint,
                 (Ptr{Void}, Ptr{Void}, Uint), s.ios, a, nb) < nb
            throw(EOFError())
        end
    else
        invoke(read, (IO, Array), s, a)
    end
    a
end

## text I/O ##

function write(s::IOStream, c::Char)
    if isreadonly(s)
        error("Cannot write to a read-only IOStream")
    end
    int(ccall(:ios_pututf8, Int32, (Ptr{Void}, Char), s.ios, c))
end
read(s::IOStream, ::Type{Char}) = ccall(:jl_getutf8, Char, (Ptr{Void},), s.ios)

takebuf_string(s::IOStream) =
    ccall(:jl_takebuf_string, ByteString, (Ptr{Void},), s.ios)

takebuf_array(s::IOStream) =
    ccall(:jl_takebuf_array, Vector{Uint8}, (Ptr{Void},), s.ios)

function takebuf_raw(s::IOStream)
    sz = position(s)
    buf = ccall(:jl_takebuf_raw, Ptr{Uint8}, (Ptr{Void},), s.ios)
    return buf, sz
end

function sprint(size::Integer, f::Function, args...)
    s = IOBuffer(Array(Uint8,size), true, true)
    truncate(s,0)
    f(s, args...)
    takebuf_string(s)
end

sprint(f::Function, args...) = sprint(0, f, args...)

write(x) = write(STDOUT::IO, x)

function readuntil(s::IOStream, delim::Uint8)
    ccall(:jl_readuntil, Array{Uint8,1}, (Ptr{Void}, Uint8), s.ios, delim)
end

function readbytes(s::IOStream)
    n = 65536
    b = Array(Uint8, n)
    p = 1
    while true
        nr = int(ccall(:ios_readall, Uint,
                       (Ptr{Void}, Ptr{Void}, Uint), s.ios, pointer(b,p), n))
        if eof(s)
            resize!(b, p+nr-1)
            break
        end
        p += nr
        resize!(b, p+n-1)
    end
    b
end

# based on code by Glen Hertz
function readuntil(s::IO, t::String)
    l = length(t)
    if l == 0
        return ""
    end
    if l > 40
        warn("readuntil(IO,String) will perform poorly with a long string")
    end
    out = IOBuffer()
    m = Array(Char, l)  # last part of stream to match
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

## Character streams ##
const _chtmp = Array(Char, 1)
function peekchar(s::IOStream)
    if ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Uint32}), s, _chtmp) < 0
        return char(-1)
    end
    return _chtmp[1]
end

function peek(s::IOStream)
    ccall(:ios_peekc, Int32, (Ptr{Void},), s)
end

function eatwspace(s::IOStream)
    ch = peekchar(s); status = int(ch)
    while status >= 0 && isspace(ch)
        read(s, Char)  # advance one character
        ch = peekchar(s); status = int(ch)
    end
end

function eatwspace_comment(s::IOStream, cmt::Char)
    ch = peekchar(s); status = int(ch)
    while status >= 0 && (isspace(ch) || ch == cmt)
        if ch == cmt
            readline(s)
        else
            read(s, Char)  # advance one character
        end
        ch = peekchar(s); status = int(ch)
    end
end

# BitArray I/O

write(s::IO, B::BitArray) = write(s, B.chunks)
write{O<:ByteOrder}(s::IO, B::BitArray, ::Type{O}) = write(s, B.chunks, O)
read(s::IO, B::BitArray) = read(s, B.chunks)
read{O<:ByteOrder}(s::IO, B::BitArray, ::Type{O}) = read(s, B.chunks, O)

## core stream types ##

abstract IO

# Generic IO functions

## byte-order mark, ntoh & hton ##

const ENDIAN_BOM = reinterpret(Uint32,uint8([1:4]))[1]

if ENDIAN_BOM == 0x01020304
    ntoh(x) = identity(x)
    hton(x) = identity(x)
    ltoh(x) = bswap(x)
    htol(x) = bswap(x)
elseif ENDIAN_BOM == 0x04030201
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

if ENDIAN_BOM == 0x01020304
    function write(s, x::Integer)
        for n = sizeof(x):-1:1
            write(s, uint8((x>>>((n-1)<<3))))
        end
    end
else
    function write(s, x::Integer)
        for n = 1:sizeof(x)
            write(s, uint8((x>>>((n-1)<<3))))
        end
    end
end

write(s::IO, x::Bool)    = write(s, uint8(x))
write(s::IO, x::Float32) = write(s, box(Int32,unbox(Float32,x)))
write(s::IO, x::Float64) = write(s, box(Int64,unbox(Float64,x)))

function write(s::IO, a::AbstractArray)
    for i = 1:numel(a)
        write(s, a[i])
    end
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
    end
    error("invalid Unicode code point: U+", hex(c))
end

# all subtypes should implement this
read(s::IO, x::Type{Uint8}) = error(typeof(s)," does not support byte I/O")

function read{T <: Integer}(s::IO, ::Type{T})
    x = zero(T)
    for n = 1:sizeof(x)
        x |= (convert(T,read(s,Uint8))<<((n-1)<<3))
    end
    return x
end

read(s::IO, ::Type{Bool})    = (read(s,Uint8)!=0)
read(s::IO, ::Type{Float32}) = box(Float32,unbox(Int32,read(s,Int32)))
read(s::IO, ::Type{Float64}) = box(Float64,unbox(Int64,read(s,Int64)))

read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) =
    read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, map(int,tuple(d1,dims...)))

read{T}(s::IO, ::Type{T}, dims::Dims) = read(s, Array(T, dims))

function read{T}(s::IO, a::Array{T})
    for i = 1:numel(a)
        a[i] = read(s, T)
    end
    return a
end

function read(s::IO, ::Type{Char})
    ch = read(s, Uint8)
    if ch < 0x80
        return char(ch)
    end

    # mimic utf8.next function
    trailing = Base._jl_utf8_trailing[ch+1]
    c::Uint32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = read(s, Uint8)
    end
    c += ch
    c -= Base._jl_utf8_offset[trailing+1]
    char(c)
end

function readuntil(s::IO, delim)
    out = memio()
    while (!eof(s))
        c = read(s, Char)
        write(out, c)
        if c == delim
            break
        end
    end
    takebuf_string(out)
end

readline(s::IO) = readuntil(s, '\n')

function readall(s::IO)
    out = memio()
    while (!eof(s))
        a = read(s, Uint8)
        write(out, a)
    end
    takebuf_string(out)
end

readchomp(x) = chomp(readall(x))

## high-level iterator interfaces ##

type EachLine
    stream::IO
end
each_line(stream::IO) = EachLine(stream)

start(itr::EachLine) = readline(itr.stream)
function done(itr::EachLine, line)
    if !isempty(line)
        return false
    end
    close(itr.stream)
    true
end
next(itr::EachLine, this_line) = (this_line, readline(itr.stream))

function readlines(s, fx::Function...)
    a = {}
    for l = each_line(s)
        for f in fx
          l = f(l)
        end
        push(a, l)
    end
    return a
end


## IOStream

const sizeof_off_t = int(ccall(:jl_sizeof_off_t, Int32, ()))
const sizeof_ios_t = int(ccall(:jl_sizeof_ios_t, Int32, ()))

if sizeof_off_t == 4
    typealias FileOffset Int32
else
    typealias FileOffset Int64
end

type IOStream <: IO
    # NOTE: for some reason the order of these field is significant!?
    ios::Array{Uint8,1}
    name::String

    IOStream(name::String, buf::Array{Uint8,1}) = new(buf, name)

    # TODO: delay adding finalizer, e.g. for memio with a small buffer, or
    # in the case where we takebuf it.
    function IOStream(name::String, finalize::Bool)
        x = new(zeros(Uint8,sizeof_ios_t), name)
        if finalize
            finalizer(x, close)
        end
        return x
    end
    IOStream(name::String) = IOStream(name, true)
end

convert(T::Type{Ptr{Void}}, s::IOStream) = convert(T, s.ios)

show(io, s::IOStream) = print(io, "IOStream(", s.name, ")")

fd(s::IOStream) = ccall(:jl_ios_fd, Int, (Ptr{Void},), s.ios)

close(s::IOStream) = ccall(:ios_close, Void, (Ptr{Void},), s.ios)

flush(s::IOStream) = ccall(:ios_flush, Void, (Ptr{Void},), s.ios)

truncate(s::IOStream, n::Integer) =
    (ccall(:ios_trunc, Int32, (Ptr{Void}, Uint), s.ios, n)==0 ||
     error("truncate failed"))

seek(s::IOStream, n::Integer) =
    (ccall(:ios_seek, FileOffset, (Ptr{Void}, FileOffset), s.ios, n)==0 ||
     error("seek failed"))

seek_end(s::IOStream) =
    (ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios)==0 ||
     error("seek_end failed"))

skip(s::IOStream, delta::Integer) =
    (ccall(:ios_skip, FileOffset, (Ptr{Void}, FileOffset), s.ios, delta)==0 ||
     error("skip failed"))

position(s::IOStream) = ccall(:ios_pos, FileOffset, (Ptr{Void},), s.ios)

eof(s::IOStream) = bool(ccall(:jl_ios_eof, Int32, (Ptr{Void},), s.ios))

## constructing and opening streams ##

# "own" means the descriptor will be closed with the IOStream
function fdio(name::String, fd::Integer, own::Bool)
    s = IOStream(name)
    ccall(:ios_fd, Void, (Ptr{Uint8}, Int, Int32, Int32),
          s.ios, fd, 0, own);
    return s
end
fdio(name::String, fd::Integer) = fdio(name, fd, false)
fdio(fd::Integer, own::Bool) = fdio(string("<fd ",fd,">"), fd, own)
fdio(fd::Integer) = fdio(fd, false)

make_stdin_stream() = fdio("<stdin>", ccall(:jl_stdin, Int32, ()))
make_stderr_stream() = fdio("<stderr>", ccall(:jl_stderr, Int32, ()))
make_stdout_stream() = IOStream("<stdout>", ccall(:jl_stdout_stream, Any, ()))

function open(fname::String, rd::Bool, wr::Bool, cr::Bool, tr::Bool, ff::Bool)
    s = IOStream(strcat("<file ",fname,">"))
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
    x = try f(io) catch err
        close(io)
        throw(err)
    end
    close(io)
    return x
end

function memio(x::Integer, finalize::Bool)
    s = IOStream("<memio>", finalize)
    ccall(:ios_mem, Ptr{Void}, (Ptr{Uint8}, Uint), s.ios, x)
    return s
end
memio(x::Integer) = memio(x, true)
memio() = memio(0, true)

## low-level calls ##

write(s::IOStream, b::Uint8) = ccall(:ios_putc, Int32, (Int32, Ptr{Void}), b, s.ios)

function write{T}(s::IOStream, a::Array{T})
    if isa(T,BitsKind)
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              s.ios, a, numel(a)*sizeof(T))
    else
        invoke(write, (Any, Array), s, a)
    end
end

function write(s::IOStream, p::Ptr, nb::Integer)
    ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint), s.ios, p, nb)
end

function write{T,N,A<:Array}(s::IOStream, a::SubArray{T,N,A})
    if !isa(T,BitsKind) || stride(a,1)!=1
        return invoke(write, (Any, AbstractArray), s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N<=1
        write(s, pointer(a, 1), colsz)
    else
        cartesian_map((idxs...)->write(s, pointer(a, idxs), colsz),
                      tuple(1, size(a)[2:]...))
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

function read{T<:Union(Int8,Uint8,Int16,Uint16,Int32,Uint32,Int64,Uint64,Int128,Uint128,Float32,Float64,Complex64,Complex128)}(s::IOStream, a::Array{T})
    nb = numel(a)*sizeof(T)
    if ccall(:ios_readall, Uint,
             (Ptr{Void}, Ptr{Void}, Uint), s.ios, a, nb) < nb
        throw(EOFError())
    end
    a
end

## text I/O ##

write(s::IOStream, c::Char) = ccall(:ios_pututf8, Int32, (Ptr{Void}, Char), s.ios, c)

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
    s = memio(size, false)
    f(s, args...)
    takebuf_string(s)
end

sprint(f::Function, args...) = sprint(0, f, args...)

function repr(x)
    s = memio(0, false)
    show(s, x)
    takebuf_string(s)
end

# using this is not recommended
function with_output_to_string(thunk)
    global OUTPUT_STREAM
    oldio = OUTPUT_STREAM
    m = memio()
    OUTPUT_STREAM = m
    try
        thunk()
        OUTPUT_STREAM = oldio
    catch e
        OUTPUT_STREAM = oldio
        throw(e)
    end
    takebuf_string(m)
end

write(x) = write(OUTPUT_STREAM::IOStream, x)

function readuntil(s::IOStream, delim)
    # TODO: faster versions that avoid the encoding check
    ccall(:jl_readuntil, ByteString, (Ptr{Void}, Uint8), s.ios, delim)
end

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end
readall(filename::String) = open(readall, filename)

## select interface ##

const sizeof_fd_set = int(ccall(:jl_sizeof_fd_set, Int32, ()))

type FDSet
    data::Array{Uint8,1}
    nfds::Int32

    function FDSet()
        ar = Array(Uint8, sizeof_fd_set)
        ccall(:jl_fd_zero, Void, (Ptr{Void},), ar)
        new(ar, 0)
    end
end

isempty(s::FDSet) = (s.nfds==0)

function add(s::FDSet, i::Integer)
    if !(0 <= i < sizeof_fd_set*8)
        error("invalid descriptor ", i)
    end
    ccall(:jl_fd_set, Void, (Ptr{Void}, Int32), s.data, i)
    if i >= s.nfds
        s.nfds = i+1
    end
    return s
end

function has(s::FDSet, i::Integer)
    if 0 <= i < sizeof_fd_set*8
        return ccall(:jl_fd_isset, Int32, (Ptr{Void}, Int32), s.data, i)!=0
    end
    return false
end

function del(s::FDSet, i::Integer)
    if 0 <= i < sizeof_fd_set*8
        ccall(:jl_fd_clr, Void, (Ptr{Void}, Int32), s.data, i)
        if i == s.nfds-1
            s.nfds -= 1
            while s.nfds>0 && !has(s, s.nfds-1)
                s.nfds -= 1
            end
        end
    end
    return s
end

function del_all(s::FDSet)
    ccall(:jl_fd_zero, Void, (Ptr{Void},), s.data)
    s.nfds = 0
    return s
end

begin
    local tv = Array(Uint8, int(ccall(:jl_sizeof_timeval, Int32, ())))
    global select_read
    function select_read(readfds::FDSet, timeout::Real)
        if timeout == Inf
            tout = C_NULL
        else
            ccall(:jl_set_timeval, Void, (Ptr{Void}, Float64), tv, timeout)
            tout = convert(Ptr{Void}, tv)
        end
        ccall(:select, Int32,
              (Int32, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
              readfds.nfds, readfds.data, C_NULL, C_NULL, tout)
    end
end

## Character streams ##
const _wstmp = Array(Char, 1)
function eatwspace(s::IOStream)
    status = ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Uint32}), s.ios, _wstmp)
    while status > 0 && iswspace(_wstmp[1])
        read(s, Char)  # advance one character
        status = ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Uint32}), s.ios, _wstmp)
    end
end

function eatwspace_comment(s::IOStream, cmt::Char)
    status = ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Uint32}), s.ios, _wstmp)
    while status > 0 && (iswspace(_wstmp[1]) || _wstmp[1] == cmt)
        if _wstmp[1] == cmt
            readline(s)
        else
            read(s, Char)  # advance one character
        end
        status = ccall(:ios_peekutf8, Int32, (Ptr{Void}, Ptr{Uint32}), s.ios, _wstmp)
    end
end

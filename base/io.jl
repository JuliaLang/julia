## core stream types ##

# the first argument to any IO MUST be a POINTER (to a JL_STREAM) or using show on it will cause memory corruption

# Generic IO functions

## byte-order mark, ntoh & hton ##

const ENDIAN_BOM = reinterpret(UInt32,uint8([1:4]))[1]

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

isreadonly(s) = isreadable(s) && !iswritable(s)

## binary I/O ##

# all subtypes should implement this
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

write(io::IO, x) = throw(MethodError(write, (io, x)))
write(io::IO, xs...) = for x in xs write(io, x) end

if ENDIAN_BOM == 0x01020304
    function write(s::IO, x::Integer)
        sz = sizeof(x)
        for n = sz:-1:1
            write(s, uint8((x>>>((n-1)<<3))))
        end
        sz
    end
else
    function write(s::IO, x::Integer)
        sz = sizeof(x)
        for n = 1:sz
            write(s, uint8((x>>>((n-1)<<3))))
        end
        sz
    end
end

write(s::IO, x::Bool)    = write(s, uint8(x))
write(s::IO, x::Float16) = write(s, reinterpret(Int16,x))
write(s::IO, x::Float32) = write(s, reinterpret(Int32,x))
write(s::IO, x::Float64) = write(s, reinterpret(Int64,x))

function write(s::IO, a::AbstractArray)
    nb = 0
    for i = 1:length(a)
        nb += write(s, a[i])
    end
    nb
end

function write(s::IO, ch::Char)
    c = reinterpret(UInt32, ch)
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

function write(s::IO, p::Ptr, n::Integer)
    for i=1:n
        write(s, unsafe_load(p, i))
    end
    n
end

function write(io::IO, s::Symbol)
    pname = convert(Ptr{UInt8}, s)
    write(io, pname, int(ccall(:strlen, Csize_t, (Ptr{UInt8},), pname)))
end

# all subtypes should implement this
read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")

read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s,UInt8))

function read{T <: Integer}(s::IO, ::Type{T})
    x = zero(T)
    for n = 1:sizeof(x)
        x |= (convert(T,read(s,UInt8))<<((n-1)<<3))
    end
    return x
end

read(s::IO, ::Type{Bool})    = (read(s,UInt8)!=0)
read(s::IO, ::Type{Float16}) = box(Float16,unbox(Int16,read(s,Int16)))
read(s::IO, ::Type{Float32}) = box(Float32,unbox(Int32,read(s,Int32)))
read(s::IO, ::Type{Float64}) = box(Float64,unbox(Int64,read(s,Int64)))

read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) =
    read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, map(int,tuple(d1,dims...)))

read{T}(s::IO, ::Type{T}, dims::Dims) = read!(s, Array(T, dims))

function read!{T}(s::IO, a::Array{T})
    for i = 1:length(a)
        a[i] = read(s, T)
    end
    return a
end

function read(s::IO, ::Type{Char})
    ch = read(s, UInt8)
    if ch < 0x80
        return char(ch)
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
    char(c)
end

function readuntil(s::IO, delim::Char)
    if delim < char(0x80)
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


readline(s::IO) = readuntil(s, '\n')
readchomp(x) = chomp!(readall(x))

# read up to nb bytes into nb, returning # bytes read
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

# read up to nb bytes from s, returning a Vector{UInt8} of bytes read.
function readbytes(s::IO, nb=typemax(Int))
    b = Array(UInt8, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    resize!(b, nr)
end

function readall(s::IO)
    b = readbytes(s)
    return is_valid_ascii(b) ? ASCIIString(b) : UTF8String(b)
end
readall(filename::AbstractString) = open(readall, filename)

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
eltype(itr::EachLine) = ByteString

readlines(s=STDIN) = collect(eachline(s))

# IOStream Marking

# Note that these functions expect that io.mark exists for
# the concrete IO type.  This may not be true for IO types
# not in base.

function mark(io::IO)
    io.mark = position(io)
end

function unmark(io::IO)
    !ismarked(io) && return false
    io.mark = -1
    return true
end

function reset(io::IO)
    !ismarked(io) && error(io, " not marked")
    m = io.mark
    seek(io, m)
    io.mark = -1 # must be after seek, or seek may fail
    return m
end

ismarked(io::IO) = io.mark >= 0

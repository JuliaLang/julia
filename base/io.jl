# This file is a part of Julia. License is MIT: http://julialang.org/license

# Generic IO stubs -- all subtypes should implement these (if meaningful)

lock(::IO) = nothing
unlock(::IO) = nothing
reseteof(x::IO) = nothing

const SZ_UNBUFFERED_IO = 65536
buffer_writes(x::IO, bufsize=SZ_UNBUFFERED_IO) = nothing

function isopen end
function close end
function flush end
function wait_connected end
function wait_readnb end
function wait_readbyte end
function wait_close end
function nb_available end
function readavailable end
function isreadable end
function iswritable end
function copy end
function eof end

read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

"""
    unsafe_write(io, ref, nbytes)

Copy nbytes from ref (converted to a pointer) into the IO stream object.

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_write(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_write(s::IO, p::Ptr{UInt8}, n::UInt)
    local written::Int = 0
    for i = 1:n
        written += write(s, unsafe_load(p, i))
    end
    return written
end

"""
    unsafe_read(io, ref, nbytes)

Copy nbytes from the IO stream object into ref (converted to a pointer).

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


# Generic wrappers around other IO objects
abstract AbstractPipe <: IO
function pipe_reader end
function pipe_writer end

write(io::AbstractPipe, byte::UInt8) = write(pipe_writer(io), byte)
unsafe_write(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_write(pipe_writer(io), p, nb)
buffer_writes(io::AbstractPipe, args...) = buffer_writes(pipe_writer(io), args...)
flush(io::AbstractPipe) = flush(pipe_writer(io))

read(io::AbstractPipe, byte::Type{UInt8}) = read(pipe_reader(io), byte)
unsafe_read(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_read(pipe_reader(io), p, nb)
read(io::AbstractPipe) = read(pipe_reader(io))
readuntil{T<:AbstractPipe}(io::T, args...) = readuntil(pipe_reader(io), args...)
readavailable(io::AbstractPipe) = readavailable(pipe_reader(io))

isreadable(io::AbstractPipe) = isreadable(pipe_reader(io))
iswritable(io::AbstractPipe) = iswritable(pipe_writer(io))
isopen(io::AbstractPipe) = isopen(pipe_writer(io)) || isopen(pipe_reader(io))
close(io::AbstractPipe) = (close(pipe_writer(io)); close(pipe_reader(io)))
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io), nb)
wait_readbyte(io::AbstractPipe, byte::UInt8) = wait_readbyte(pipe_reader(io), byte)
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)); wait_close(pipe_reader(io)))
nb_available(io::AbstractPipe) = nb_available(pipe_reader(io))
eof(io::AbstractPipe) = eof(pipe_reader(io))
reseteof(io::AbstractPipe) = reseteof(pipe_reader(io))


# Exception-safe wrappers (io = open(); try f(io) finally close(io))

write(filename::AbstractString, args...) = open(io->write(io, args...), filename, "w")

read(filename::AbstractString, args...) = open(io->read(io, args...), filename)
read!(filename::AbstractString, a) = open(io->read!(io, a), filename)
readstring(filename::AbstractString) = open(readstring, filename)
readuntil(filename::AbstractString, args...) = open(io->readuntil(io, args...), filename)
readline(filename::AbstractString) = open(readline, filename)
readlines(filename::AbstractString) = open(readlines, filename)


## byte-order mark, ntoh & hton ##

const ENDIAN_BOM = reinterpret(UInt32,UInt8[1:4;])[1]

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

write(io::IO, x) = throw(MethodError(write, (io, x)))
function write(io::IO, xs...)
    local written::Int = 0
    for x in xs
        written += write(io, x)
    end
    return written
end

@noinline unsafe_write{T}(s::IO, p::Ref{T}, n::Integer) = unsafe_write(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_write(s::IO, p::Ptr, n::Integer) = unsafe_write(s, convert(Ptr{UInt8}, p), convert(UInt, n))
write{T}(s::IO, x::Ref{T}) = unsafe_write(s, x, Core.sizeof(T))
write(s::IO, x::Int8) = write(s, reinterpret(UInt8, x))
function write(s::IO, x::Union{Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,Float16,Float32,Float64})
    return write(s, Ref(x))
end

write(s::IO, x::Bool)    = write(s, UInt8(x))
write(to::IO, p::Ptr) = write(to, convert(UInt, p))

function write(s::IO, A::AbstractArray)
    nb = 0
    for a in A
        nb += write(s, a)
    end
    return nb
end

@noinline function write(s::IO, a::Array{UInt8}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    return unsafe_write(s, pointer(a), sizeof(a))
end

@noinline function write{T}(s::IO, a::Array{T}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    if isbits(T)
        return unsafe_write(s, pointer(a), sizeof(a))
    else
        nb = 0
        for b in a
            nb += write(s, b)
        end
        return nb
    end
end


function write(s::IO, ch::Char)
    c = reinterpret(UInt32, ch)
    if c < 0x80
        return write(s, c%UInt8)
    elseif c < 0x800
        return (write(s, (( c >> 6          ) | 0xC0)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x10000
        return (write(s, (( c >> 12         ) | 0xE0)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x110000
        return (write(s, (( c >> 18         ) | 0xF0)%UInt8)) +
               (write(s, (((c >> 12) & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    else
        return write(s, '\ufffd')
    end
end

function write(io::IO, s::Symbol)
    pname = unsafe_convert(Ptr{UInt8}, s)
    return unsafe_write(io, pname, Int(ccall(:strlen, Csize_t, (Cstring,), pname)))
end

function write(to::IO, from::IO)
    while !eof(from)
        write(to, readavailable(from))
    end
end

@noinline unsafe_read{T}(s::IO, p::Ref{T}, n::Integer) = unsafe_read(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_read(s::IO, p::Ptr, n::Integer) = unsafe_read(s, convert(Ptr{UInt8}, p), convert(UInt, n))
read{T}(s::IO, x::Ref{T}) = (unsafe_read(s, x, Core.sizeof(T)); x)

read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s, UInt8))
function read(s::IO, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    return read(s, Ref{T}(0))[]::T
end

read(s::IO, ::Type{Bool})    = (read(s,UInt8)!=0)
read{T}(s::IO, ::Type{Ptr{T}}) = convert(Ptr{T}, read(s, UInt))

read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) = read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, convert(Tuple{Vararg{Int}},tuple(d1,dims...)))

read{T}(s::IO, ::Type{T}, dims::Dims) = read!(s, Array(T, dims))

@noinline function read!(s::IO, a::Array{UInt8}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    unsafe_read(s, pointer(a), sizeof(a))
    return a
end

@noinline function read!{T}(s::IO, a::Array{T}) # mark noinline to ensure the array is gc-rooted somewhere (by the caller)
    if isbits(T)
        unsafe_read(s, pointer(a), sizeof(a))
    else
        for i in eachindex(a)
            a[i] = read(s, T)
        end
    end
    return a
end

function read(s::IO, ::Type{Char})
    ch = read(s, UInt8)
    if ch < 0x80
        return Char(ch)
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
    return Char(c)
end

function readuntil(s::IO, delim::Char)
    if delim < Char(0x80)
        data = readuntil(s, delim%UInt8)
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
    return takebuf_string(out)
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
    return out
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

readline() = readline(STDIN)
readline(s::IO) = readuntil(s, '\n')
readchomp(x) = chomp!(readstring(x))

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
function read(s::IO, nb=typemax(Int))
    # Let readbytes! grow the array progressively by default
    # instead of taking of risk of over-allocating
    b = Array(UInt8, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    return resize!(b, nr)
end

function readstring(s::IO)
    b = read(s)
    return isvalid(ASCIIString, b) ? ASCIIString(b) : UTF8String(b)
end

## high-level iterator interfaces ##

type EachLine
    stream::IO
    ondone::Function
    EachLine(stream) = EachLine(stream, ()->nothing)
    EachLine(stream, ondone) = new(stream, ondone)
end
eachline(stream::IO) = EachLine(stream)
function eachline(filename::AbstractString)
    s = open(filename)
    EachLine(s, ()->close(s))
end

start(itr::EachLine) = nothing
function done(itr::EachLine, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
next(itr::EachLine, nada) = (readline(itr.stream), nothing)
eltype(::Type{EachLine}) = ByteString

readlines(s=STDIN) = collect(eachline(s))

iteratorsize(::Type{EachLine}) = SizeUnknown()

# IOStream Marking
# Note that these functions expect that io.mark exists for
# the concrete IO type. This may not be true for IO types
# not in base.

function mark(io::IO)
    io.mark = position(io)
end

function unmark(io::IO)
    !ismarked(io) && return false
    io.mark = -1
    return true
end

function reset{T<:IO}(io::T)
    ismarked(io) || throw(ArgumentError("$(T) not marked"))
    m = io.mark
    seek(io, m)
    io.mark = -1 # must be after seek, or seek may fail
    return m
end

ismarked(io::IO) = io.mark >= 0

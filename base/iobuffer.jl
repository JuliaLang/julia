## work with Vector{UInt8} via I/O primitives ##

# Stateful string
type IOBuffer <: IO
    data::Vector{UInt8}
    readable::Bool
    writable::Bool
    seekable::Bool # if not seekable, free to destroy (compact) past read data
    append::Bool # add data at end instead of at pointer
    size::Int
    maxsize::Int # pre-allocated, fixed array size
    ptr::Int # read (and maybe write) pointer
    mark::Int

    IOBuffer(data::Vector{UInt8},readable::Bool,writable::Bool,seekable::Bool,append::Bool,maxsize::Int) =
        new(data,readable,writable,seekable,append,length(data),maxsize,1,-1)
end

function copy(b::IOBuffer)
    ret = IOBuffer(b.writable?copy(b.data):b.data,
                   b.readable,b.writable,b.seekable,b.append,b.maxsize)
    ret.size = b.size
    ret.ptr  = b.ptr
    ret
end

show(io::IO, b::IOBuffer) = print(io, "IOBuffer(data=UInt8[...], ",
                                      "readable=", b.readable, ", ",
                                      "writable=", b.writable, ", ",
                                      "seekable=", b.seekable, ", ",
                                      "append=",   b.append, ", ",
                                      "size=",     b.size, ", ",
                                      "maxsize=",  b.maxsize == typemax(Int) ? "Inf" : b.maxsize, ", ",
                                      "ptr=",      b.ptr, ", ",
                                      "mark=",     b.mark, ")")

# PipeBuffers behave like Unix Pipes. They are readable and writable, the act appendable, and not seekable.
PipeBuffer(data::Vector{UInt8},maxsize::Int) = IOBuffer(data,true,true,false,true,maxsize)
PipeBuffer(data::Vector{UInt8}) = PipeBuffer(data,typemax(Int))
PipeBuffer() = PipeBuffer(UInt8[])
PipeBuffer(maxsize::Int) = (x = PipeBuffer(Array(UInt8,maxsize),maxsize); x.size=0; x)

# IOBuffers behave like Files. They are readable and writable. They are seekable. (They can be appendable).
IOBuffer(data::Vector{UInt8},readable::Bool,writable::Bool,maxsize::Int) =
        IOBuffer(data,readable,writable,true,false,maxsize)
IOBuffer(data::Vector{UInt8},readable::Bool,writable::Bool) = IOBuffer(data,readable,writable,typemax(Int))
IOBuffer(data::Vector{UInt8}) = IOBuffer(data, true, false)
IOBuffer(str::ByteString) = IOBuffer(str.data, true, false)
IOBuffer(readable::Bool,writable::Bool) = IOBuffer(UInt8[],readable,writable)
IOBuffer() = IOBuffer(UInt8[], true, true)
IOBuffer(maxsize::Int) = (x=IOBuffer(Array(UInt8,maxsize),true,true,maxsize); x.size=0; x)

is_maxsize_unlimited(io::IOBuffer) = (io.maxsize == typemax(Int))

read!(from::IOBuffer, a::Array) = read_sub(from, a, 1, length(a))

function read_sub{T}(from::IOBuffer, a::Array{T}, offs, nel)
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if !isbits(T)
        error("read from IOBuffer only supports bits types or arrays of bits types; got "*string(T))
    end
    read!(from, pointer(a, offs), nel*sizeof(T))
    return a
end

read!(from::IOBuffer, p::Ptr, nb::Integer) = read!(from, p, int(nb))
function read!(from::IOBuffer, p::Ptr, nb::Int)
    if !from.readable error("read failed") end
    avail = nb_available(from)
    adv = min(avail,nb)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt), p, pointer(from.data,from.ptr), adv)
    from.ptr += adv
    if nb > avail
        throw(EOFError())
    end
    p
end

function read(from::IOBuffer, ::Type{UInt8})
    if !from.readable error("read failed") end
    if from.ptr > from.size
        throw(EOFError())
    end
    byte = from.data[from.ptr]
    from.ptr += 1
    return byte
end

function peek(from::IOBuffer)
    if !from.readable error("read failed") end
    if from.ptr > from.size
        throw(EOFError())
    end
    return from.data[from.ptr]
end

read{T}(from::IOBuffer, ::Type{Ptr{T}}) = convert(Ptr{T}, read(from, UInt))

isreadable(io::IOBuffer) = io.readable
iswritable(io::IOBuffer) = io.writable

# TODO: IOBuffer is not iterable, so doesn't really have a length.
# This should maybe be sizeof() instead.
#length(io::IOBuffer) = (io.seekable ? io.size : nb_available(io))
nb_available(io::IOBuffer) = io.size - io.ptr + 1
position(io::IOBuffer) = io.ptr-1

function skip(io::IOBuffer, n::Integer)
    io.ptr = min(io.ptr + n, io.size+1)
    return io
end

function seek(io::IOBuffer, n::Integer)
    !io.seekable && (!ismarked(io) || n!=io.mark) && error("seek failed")
    io.ptr = min(n+1, io.size+1)
    return io
end

function seekend(io::IOBuffer)
    io.ptr = io.size+1
    return io
end

function truncate(io::IOBuffer, n::Integer)
    if !io.writable error("truncate failed") end
    if !io.seekable error("truncate failed") end #because absolute offsets are meaningless
    if n > io.maxsize || n < 0 error("truncate failed") end
    if n > length(io.data)
        resize!(io.data, n)
    end
    io.data[io.size+1:n] = 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
    ismarked(io) && io.mark > n && unmark(io)
    return io
end

function compact(io::IOBuffer)
    if !io.writable error("compact failed") end
    if io.seekable error("compact failed") end
    local ptr::Int, bytes_to_move::Int
    if ismarked(io) && io.mark < io.ptr
        if io.mark == 0 return end
        ptr = io.mark
        bytes_to_move = nb_available(io) + (io.ptr-io.mark)
    else
        ptr = io.ptr
        bytes_to_move = nb_available(io)
    end
    ccall(:memmove, Ptr{Void}, (Ptr{Void},Ptr{Void},UInt),
          io.data, pointer(io.data,ptr), bytes_to_move)
    io.size -= ptr - 1
    io.ptr -= ptr - 1
    io.mark -= ptr - 1
    return io
end

function ensureroom(io::IOBuffer, nshort::Int)
    if !io.writable error("ensureroom failed") end
    if !io.seekable
        if nshort < 0 error("ensureroom failed") end
        if !ismarked(io) && io.ptr > 1 && io.size <= io.ptr - 1
            io.ptr = 1
            io.size = 0
        else
            datastart = ismarked(io) ? io.mark : io.ptr
            if (io.size+nshort > io.maxsize) ||
                (datastart > 4096 && datastart > io.size - io.ptr) ||
                (datastart > 262144)
                # apply somewhat arbitrary heuristics to decide when to destroy
                # old, read data to make more room for new data
                compact(io)
            end
        end
    end
    n = min(nshort + (io.append ? io.size : io.ptr-1), io.maxsize)
    if n > length(io.data)
        resize!(io.data, n)
    end
    return io
end
eof(io::IOBuffer) = (io.ptr-1 == io.size)
function close(io::IOBuffer)
    if io.writable
        resize!(io.data, 0)
    else
        io.data = UInt8[]
    end
    io.readable = false
    io.writable = false
    io.seekable = false
    io.size = 0
    io.maxsize = 0
    io.ptr = 1
    io.mark = -1
    nothing
end
isopen(io::IOBuffer) = io.readable || io.writable || io.seekable || nb_available(io) > 0
function bytestring(io::IOBuffer)
    if !io.readable error("bytestring read failed") end
    if !io.seekable error("bytestring read failed") end
    bytestring(pointer(io.data), io.size)
end
function takebuf_array(io::IOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        data = io.data
        if io.writable
            maxsize = (io.maxsize == typemax(Int) ? 0 : io.maxsize)
            io.data = Array(UInt8,maxsize)
        else
            data = copy(data)
        end
        resize!(data,io.size)
    else
        nbytes = nb_available(io)
        a = Array(UInt8, nbytes)
        data = read!(io, a)
    end
    if io.writable
        io.ptr = 1
        io.size = 0
    end
    data
end
takebuf_string(io::IOBuffer) = bytestring(takebuf_array(io))

function write(to::IOBuffer, from::IOBuffer)
    write(to, pointer(from.data,from.ptr), nb_available(from))
    from.ptr += nb_available(from)
end

write(to::IOBuffer, p::Ptr, nb::Integer) = write(to, p, int(nb))
function write(to::IOBuffer, p::Ptr, nb::Int)
    !to.writable && error("write failed")
    ensureroom(to, nb)
    ptr = (to.append ? to.size+1 : to.ptr)
    nb = min(nb, length(to.data) - ptr + 1)
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt), pointer(to.data,ptr), p, nb)
    to.size = max(to.size, ptr - 1 + nb)
    if !to.append to.ptr += nb end
    nb
end

function write_sub{T}(to::IOBuffer, a::Array{T}, offs, nel)
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbits(T)
        write(to, pointer(a,offs), nel*sizeof(T))
    else
        nb = 0
        for i = offs:offs+nel-1
            nb += write(to, a[i])
        end
        nb
    end
end

write(to::IOBuffer, a::Array) = write_sub(to, a, 1, length(a))

function write(to::IOBuffer, a::UInt8)
    if !to.writable error("write failed") end
    ensureroom(to, 1)
    ptr = (to.append ? to.size+1 : to.ptr)
    if ptr > to.maxsize
        return 0
    else
        to.data[ptr] = a
    end
    to.size = max(to.size, ptr)
    if !to.append to.ptr += 1 end
    sizeof(UInt8)
end

write(to::IOBuffer, p::Ptr) = write(to, convert(UInt, p))

function readbytes!(io::IOBuffer, b::Array{UInt8}, nb=length(b))
    nr = min(nb, nb_available(io))
    if length(b) < nr
        resize!(b, nr)
    end
    read_sub(io, b, 1, nr)
    return nr
end
readbytes(io::IOBuffer) = read!(io, Array(UInt8, nb_available(io)))
readbytes(io::IOBuffer, nb) = read!(io, Array(UInt8, min(nb, nb_available(io))))

function search(buf::IOBuffer, delim)
    p = pointer(buf.data, buf.ptr)
    q = ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),p,delim,nb_available(buf))
    nb = (q == C_NULL ? 0 : q-p+1)
end

function readuntil(io::IOBuffer, delim::UInt8)
    lb = 70
    A = Array(UInt8, lb)
    n = 0
    data = io.data
    for i = io.ptr : io.size
        n += 1
        if n > lb
            lb = n*2
            resize!(A, lb)
        end
        @inbounds b = data[i]
        @inbounds A[n] = b
        if b == delim
            break
        end
    end
    io.ptr += n
    if lb != n
        resize!(A, n)
    end
    A
end

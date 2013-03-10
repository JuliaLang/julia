## work with Vector{Uint8} via I/O primitives ##

# Stateful string
type IOBuffer <: IO
    data::Vector{Uint8}
    readable::Bool
    writable::Bool
    seekable::Bool # if not seekable, free to destroy (compact) past read data
    append::Bool # add data at end instead of at pointer
    size::Int
    maxsize::Int # pre-allocated, fixed array size
    ptr::Int # read (and maybe write) pointer
    IOBuffer(data::Vector{Uint8},readable::Bool,writable::Bool,seekable::Bool,append::Bool,maxsize::Int) = 
        new(data,readable,writable,seekable,append,length(data),maxsize,1)
end
typealias IOString IOBuffer
#typealias PipeBuffer IOBuffer
# PipeBuffers behave like Unix Pipes. They are readable and writable, the act appendable, and not seekable.
PipeBuffer(data::Vector{Uint8},maxsize::Int) = IOBuffer(data,true,true,false,true,maxsize)
PipeBuffer(data::Vector{Uint8}) = PipeBuffer(data,typemax(Int))
PipeBuffer() = PipeBuffer(Uint8[])
PipeBuffer(maxsize::Int) = (x = PipeBuffer(Array(Uint8,maxsize),data,maxsize); x.size=0; x)

const PipeString = PipeBuffer

# IOBuffers behave like Files. They are readable and writable. They are seekable. (They can be appendable).
IOBuffer(data::Vector{Uint8},readable::Bool,writable::Bool,maxsize::Int) =
        IOBuffer(data,readable,writable,true,false,maxsize)
IOBuffer(data::Vector{Uint8},readable::Bool,writable::Bool) = IOBuffer(data,readable,writable,typemax(Int))
IOBuffer(data::Vector{Uint8}) = IOBuffer(data, true, false)
IOBuffer(str::String) = IOBuffer(str.data, true, false)
IOBuffer(readable::Bool,writable::Bool) = IOBuffer(Uint8[],readable,writable)
IOBuffer() = IOBuffer(Uint8[], true, true)
IOBuffer(maxsize::Int) = (x=IOBuffer(Array(Uint8,maxsize),true,true,maxsize); x.size=0; x)

function read{T}(from::IOBuffer, a::Array{T})
    if !from.readable error("read failed") end
    if isbits(T)
        nb = length(a)*sizeof(T)
        if nb > nb_available(from)
            throw(EOFError())
        end
        ccall(:memcpy, Void, (Ptr{Void}, Ptr{Void}, Int), a, pointer(from.data,from.ptr), nb)
        from.ptr += nb
        return a
    else
        error("Read from IOBuffer only supports bits types or arrays of bits types; got "*string(T)*".")
    end
end

function read(from::IOBuffer, ::Type{Uint8})
    if !from.readable error("read failed") end
    if from.ptr > from.size
        throw(EOFError())
    end
    byte = from.data[from.ptr]
    from.ptr += 1
    return byte
end

read{T}(from::IOBuffer, ::Type{Ptr{T}}) = convert(Ptr{T}, read(from, Uint))

# TODO: IOBuffer is not iterable, so doesn't really have a length.
# This should maybe be sizeof() instead.
#length(io::IOBuffer) = (io.seekable ? io.size : nb_available(io))
nb_available(io::IOBuffer) = io.size - io.ptr + 1
skip(io::IOBuffer, n::Integer) = (io.ptr = min(io.ptr + n, io.size+1))
function seek(io::IOBuffer, n::Integer)
    if !io.seekable error("seek failed") end
    io.ptr = min(n+1, io.size+1)
    return true
end
function seek_end(io::IOBuffer)
    io.ptr = io.size+1
    return true 
end
position(io::IOBuffer) = io.ptr-1
function truncate(io::IOBuffer, n::Integer)
    if !io.writable error("truncate failed") end 
    if !io.seekable error("truncate failed") end #because absolute offsets are meaningless
    if n > io.maxsize || n < 0 error("truncate failed") end
    if n > length(io.data)
        resize!(io.data, n)
    end
    io.data[io.size+1:end] = 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
    return true
end
function compact(io::IOBuffer)
    if !io.writable error("compact failed") end 
    if io.seekable error("compact failed") end
    ccall(:memmove, Void, (Ptr{Void},Ptr{Void},Int), io.data, pointer(io.data,io.ptr), nb_available(io))
    io.size -= io.ptr - 1
    io.ptr = 1
    return true
end
function ensureroom(io::IOBuffer, nshort::Int)
    if !io.writable error("ensureroom failed") end 
    if !io.seekable
        if nshort < 0 error("ensureroom failed") end
        if io.ptr > 1 && io.size <= io.ptr - 1
            io.ptr = 1
            io.size = 0
        elseif (io.size+nshort > io.maxsize) ||
                (io.ptr > io.size - io.ptr > 4096) ||
                (io.ptr > 262144)
            # apply somewhat arbitrary heuristics to decide when to destroy 
            # old, read data to make more room for new data
            compact(io)
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
        io.data = Uint8[]
    end
    io.readable = false
    io.writable = false
    io.seekable = false
    io.size = 0
    io.maxsize = 0
    io.ptr = 1
    nothing
end
function bytestring(io::IOBuffer)
    if !io.readable error("bytestring read failed") end 
    if !io.seekable error("bytestring read failed") end 
    bytestring(io.data[1:io.size])
end
function takebuf_array(io::IOBuffer)
    if io.seekable
        data = io.data
        if io.writable
            maxsize = (io.maxsize == typemax(Int) ? 0 : io.maxsize)
            io.data = Array(Uint8,maxsize)
        else
            data = copy(data)
        end
        resize!(data,io.size)
    else
        nbytes = nb_available(io)
        a = Array(Uint8, nbytes)
        data = read(io, a)
    end
    if io.writable
        io.ptr = 1
        io.size = 0
    end
    data
end
takebuf_string(io::IOBuffer) = bytestring(takebuf_array(io))

function write_sub{T}(to::IOBuffer, a::Array{T}, offs, nel)
    if !to.writable; error("write failed") end
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbits(T)
        nb = nel*sizeof(T)
        ensureroom(to, nb)
        ptr = (to.append ? to.size+1 : to.ptr)
        nb = min(nb, length(to.data) - ptr + 1)
        ccall(:memcpy, Void, (Ptr{Void}, Ptr{Void}, Int), pointer(to.data,ptr), pointer(a,offs), nb)
        to.size = max(to.size, ptr - 1 + nb)
        if !to.append; to.ptr += nb; end
    else
        error("Write to IOBuffer only supports bits types or arrays of bits types; got "*string(T)*".")
    end
    nb
end

write(to::IOBuffer, a::Array) = write_sub(to, a, 1, length(a))

function write(to::IOBuffer, a::Uint8)
    if !to.writable error("write failed") end
    ensureroom(to, 1)
    ptr = (to.append ? to.size+1 : to.ptr)
    if ptr > to.maxsize
        return 0
    else
        to.data[ptr] = a
    end
    to.size = max(to.size, ptr)
    if !to.append; to.ptr += 1; end
    sizeof(Uint8)
end

write(to::IOBuffer, p::Ptr) = write(to, convert(Uint, p))

readbytes(io::IOBuffer,nb::Integer) = bytestring(read(io, Array(Uint8, nb)))
readall(io::IOBuffer) = readbytes(io,nb_available(io))
function search(buf::IOBuffer, delim)
    p = pointer(buf.data, buf.ptr)
    q = ccall(:memchr,Ptr{Uint8},(Ptr{Uint8},Int32,Int32),p,delim,nb_available(buf))
    nb = (q == C_NULL ? 0 : q-p+1)
end
function readuntil(io::IOBuffer, delim::Uint8)
    nb = search(io, delim)
    if nb == 0
        nb = nb_available(io)
    end
    read(io, Array(Uint8, nb))
end

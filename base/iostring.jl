## work with Vector{Uint8} via I/O primitives ##

abstract Buffer <: IO

# Stateful string
type IOString <: Buffer
    data::Vector{Uint8}
    readable::Bool
    writable::Bool
    seekable::Bool # if not seekable, free to destroy (compact) past read data
    append::Bool # add data at end instead of at pointer
    size::Int
    maxsize::Int # pre-allocated, fixed array size
    ptr::Int # read (and maybe write) pointer
    IOString(data::Vector{Uint8},readable::Bool,writable::Bool,seekable::Bool,append::Bool,maxsize::Int) = 
        new(data,readable,writable,seekable,append,length(data),maxsize,1)
end
#typealias PipeString IOString
# PipeStrings behave like Unix Pipes. They are readable and writable, the act appendable, and not seekable.
PipeString(data::Vector{Uint8},maxsize::Int) = IOString(data,true,true,false,true,maxsize)
PipeString(data::Vector{Uint8}) = PipeString(data,typemax(Int))
PipeString() = PipeString(Uint8[])
PipeString(maxsize::Int) = (x = PipeString(Array(Uint8,maxsize),data,maxsize); x.size=0; x)

# IOStrings behave like Files. They are readable and writable. They are seekable. (They can be appendable).
IOString(data::Vector{Uint8},readable::Bool,writable::Bool,maxsize::Int) =
        IOString(data,readable,writable,true,false,maxsize)
IOString(data::Vector{Uint8},readable::Bool,writable::Bool) = IOString(data,readable,writable,typemax(Int))
IOString(data::Vector{Uint8}) = IOString(data, true, false)
IOString(str::String) = IOString(str.data, true, false)
IOString(readable::Bool,writable::Bool) = IOString(Uint8[],readable,writable)
IOString() = IOString(Uint8[], true, true)
IOString(maxsize::Int) = (x=IOString(Array(Uint8,maxsize),true,true,maxsize); x.size=0; x)

function read{T}(from::IOString, a::Array{T})
    if !from.readable error("read failed") end
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        if nb > nb_available(from)
            throw(EOFError())
        end
        ccall(:memcpy, Void, (Ptr{Void}, Ptr{Void}, Int), a, pointer(from.data,from.ptr), nb)
        from.ptr += nb
        return a
    else
        error("Read from IOString only supports bits types or arrays of bits types; got "*string(T)*".")
    end
end

function read(from::IOString, ::Type{Uint8})
    if !from.readable error("read failed") end
    if from.ptr > from.size
        throw(EOFError())
    end
    byte = from.data[from.ptr]
    from.ptr += 1
    return byte
end

read{T}(from::IOString, ::Type{Ptr{T}}) = convert(Ptr{T}, read(from, Uint))

length(io::IOString) = (io.seekable ? io.size : nb_available(io))
nb_available(io::IOString) = io.size - io.ptr + 1
skip(io::IOString, n::Integer) = (io.ptr = min(io.ptr + n, io.size+1))
function seek(io::IOString, n::Integer) 
    if !io.seekable error("seek failed") end
    io.ptr = min(n+1, io.size+1)
    return true
end
function seek_end(io::IOString)
    io.ptr = io.size+1
    return true 
end
position(io::IOString) = io.ptr-1
function truncate(io::IOString, n::Integer)
    if !io.writable error("truncate failed") end 
    if !io.seekable error("truncate failed") end #because absolute offsets are meaningless
    if n > io.maxsize || n < 0 error("truncate failed") end
    nadd = n - length(io.data)
    if nadd > 0
        grow(io.data, nadd)
    end
    io.data[io.size+1:end] = 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
    return true
end
function compact(io::IOString)
    if !io.writable error("compact failed") end 
    if io.seekable error("compact failed") end
    ccall(:memmove, Void, (Ptr{Void},Ptr{Void},Int), io.data, pointer(io.data,io.ptr), nb_available(io))
    io.size -= io.ptr - 1
    io.ptr = 1
    return true
end
function ensureroom(io::IOString, nshort::Int)
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
    ngrow = n - length(io.data)
    if ngrow > 0
        grow(io.data, ngrow)
    end
    return io
end
eof(io::IOString) = (io.ptr-1 == io.size)
function close(io::IOString)
    if io.writable
        grow(io.data, -length(io.data))
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
function bytestring(io::IOString)
    if !io.readable error("bytestring read failed") end 
    if !io.seekable error("bytestring read failed") end 
    bytestring(io.data[1:io.size])
end
function takebuf_array(io::IOString)
    if io.seekable
        data = io.data
        if io.writable
            maxsize = (io.maxsize == typemax(Int) ? 0 : io.maxsize)
            io.data = Array(Uint8,maxsize)
        else
            data = copy(data)
        end
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
takebuf_string(io::IOString) = bytestring(takebuf_array(io))

function write{T}(to::IOString, a::Array{T})
    if !to.writable error("write failed") end
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        ensureroom(to, nb)
        ptr = (to.append ? to.size+1 : to.ptr)
        nb = min(nb, length(to.data) - ptr + 1)
        ccall(:memcpy, Void, (Ptr{Void}, Ptr{Void}, Int), pointer(to.data,ptr), a, nb)
        to.size = max(to.size, ptr - 1 + nb)
        if !to.append; to.ptr += nb; end
    else
        error("Write to IOString only supports bits types or arrays of bits types; got "*string(T)*".")
    end
    nb
end

function write(to::IOString, a::Uint8)
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

write(to::IOString, p::Ptr) = write(to, convert(Uint, p))

readbytes(io::IOString,nb::Integer) = bytestring(read(io, Array(Uint8, nb)))
readall(io::IOString) = readbytes(io,nb_available(io))
function memchr(buf::IOString, delim)
    p = pointer(buf.data, buf.ptr)
    q = ccall(:memchr,Ptr{Uint8},(Ptr{Uint8},Int32,Int32),p,delim,nb_available(buf))
    nb = (q == C_NULL ? 0 : q-p+1)
end
function readuntil(io::IOString, delim)
    nb = memchr(io, delim)
    if nb == 0
        nb = nb_available(io)
    end
    readbytes(io,nb)
end


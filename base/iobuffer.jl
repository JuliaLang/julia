# This file is a part of Julia. License is MIT: http://julialang.org/license

## work with AbstractVector{UInt8} via I/O primitives ##

# Stateful string
type AbstractIOBuffer{T<:AbstractVector{UInt8}} <: IO
    data::T # T should support: getindex, setindex!, length, copy!, resize!, and T()
    readable::Bool
    writable::Bool
    seekable::Bool # if not seekable, implementation is free to destroy (compact) past read data
    append::Bool # add data at end instead of at pointer
    size::Int # end pointer (and write pointer if append == true)
    maxsize::Int # fixed array size (typically pre-allocated)
    ptr::Int # read (and maybe write) pointer
    mark::Int # reset mark location for ptr (or <0 for no mark)

    AbstractIOBuffer(data::T,readable::Bool,writable::Bool,seekable::Bool,append::Bool,maxsize::Int) =
        new(data,readable,writable,seekable,append,length(data),maxsize,1,-1)
end
typealias IOBuffer AbstractIOBuffer{Vector{UInt8}}

AbstractIOBuffer{T<:AbstractVector{UInt8}}(data::T, readable::Bool, writable::Bool, seekable::Bool, append::Bool, maxsize::Int) =
    AbstractIOBuffer{T}(data, readable, writable, seekable, append, maxsize)

# IOBuffers behave like Files. They are typically readable and writable. They are seekable. (They can be appendable).
IOBuffer(data::AbstractVector{UInt8}, readable::Bool=true, writable::Bool=false, maxsize::Int=typemax(Int)) =
    AbstractIOBuffer(data, readable, writable, true, false, maxsize)
IOBuffer(readable::Bool, writable::Bool) = IOBuffer(UInt8[], readable, writable)
IOBuffer() = IOBuffer(true, true)
IOBuffer(maxsize::Int) = (x=IOBuffer(Array(UInt8,maxsize), true, true, maxsize); x.size=0; x)

# PipeBuffers behave like Unix Pipes. They are typically readable and writable, they act appendable, and are not seekable.
PipeBuffer(data::Vector{UInt8}=UInt8[], maxsize::Int=typemax(Int)) =
    AbstractIOBuffer(data,true,true,false,true,maxsize)
PipeBuffer(maxsize::Int) = (x = PipeBuffer(Array(UInt8,maxsize),maxsize); x.size=0; x)

function copy(b::AbstractIOBuffer)
    ret = typeof(b)(b.writable ? copy(b.data) : b.data,
                    b.readable, b.writable, b.seekable, b.append, b.maxsize)
    ret.size = b.size
    ret.ptr  = b.ptr
    ret
end

show(io::IO, b::AbstractIOBuffer) = print(io, "IOBuffer(data=UInt8[...], ",
                                      "readable=", b.readable, ", ",
                                      "writable=", b.writable, ", ",
                                      "seekable=", b.seekable, ", ",
                                      "append=",   b.append, ", ",
                                      "size=",     b.size, ", ",
                                      "maxsize=",  b.maxsize == typemax(Int) ? "Inf" : b.maxsize, ", ",
                                      "ptr=",      b.ptr, ", ",
                                      "mark=",     b.mark, ")")

read!(from::AbstractIOBuffer, a::Vector{UInt8}) = read_sub(from, a, 1, length(a))
read!(from::AbstractIOBuffer, a::Array) = read_sub(from, a, 1, length(a))

function read_sub{T}(from::AbstractIOBuffer, a::AbstractArray{T}, offs, nel)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    if isbits(T) && isa(a,Array)
        nb = nel * sizeof(T)
        avail = nb_available(from)
        adv = min(avail, nb)
        copy!(pointer_to_array(convert(Ptr{UInt8},pointer(a)), sizeof(a)), # reinterpret(UInt8,a) but without setting the shared data property on a
              1 + (1 - offs) * sizeof(T),
              from.data,
              from.ptr,
              adv)
        from.ptr += adv
        if nb > avail
            throw(EOFError())
        end
    else
        for i = offs:offs+nel-1
            a[i] = read(to, T)
        end
    end
    return a
end

@inline function read(from::AbstractIOBuffer, ::Type{UInt8})
    ptr = from.ptr
    size = from.size
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if ptr > size
        throw(EOFError())
    end
    @inbounds byte = from.data[ptr]
    from.ptr = ptr + 1
    return byte
end

function peek(from::AbstractIOBuffer)
    from.readable || throw(ArgumentError("read failed, IOBuffer is not readable"))
    if from.ptr > from.size
        throw(EOFError())
    end
    return from.data[from.ptr]
end

read{T}(from::AbstractIOBuffer, ::Type{Ptr{T}}) = convert(Ptr{T}, read(from, UInt))

isreadable(io::AbstractIOBuffer) = io.readable
iswritable(io::AbstractIOBuffer) = io.writable

# TODO: AbstractIOBuffer is not iterable, so doesn't really have a length.
# This should maybe be sizeof() instead.
#length(io::AbstractIOBuffer) = (io.seekable ? io.size : nb_available(io))
nb_available(io::AbstractIOBuffer) = io.size - io.ptr + 1
position(io::AbstractIOBuffer) = io.ptr-1

function skip(io::AbstractIOBuffer, n::Integer)
    seekto = io.ptr + n
    n < 0 && return seek(io, seekto-1) # Does error checking
    io.ptr = min(seekto, io.size+1)
    return io
end

function seek(io::AbstractIOBuffer, n::Integer)
    if !io.seekable
        ismarked(io) || throw(ArgumentError("seek failed, IOBuffer is not seekable and is not marked"))
        n == io.mark || throw(ArgumentError("seek failed, IOBuffer is not seekable and n != mark"))
    end
    # TODO: REPL.jl relies on the fact that this does not throw (by seeking past the beginning or end
    #       of an AbstractIOBuffer), so that would need to be fixed in order to throw an error here
    #(n < 0 || n > io.size) && throw(ArgumentError("Attempted to seek outside IOBuffer boundaries."))
    #io.ptr = n+1
    io.ptr = max(min(n+1, io.size+1), 1)
    return io
end

function seekend(io::AbstractIOBuffer)
    io.ptr = io.size+1
    return io
end

function truncate(io::AbstractIOBuffer, n::Integer)
    io.writable || throw(ArgumentError("truncate failed, IOBuffer is not writeable"))
    io.seekable || throw(ArgumentError("truncate failed, IOBuffer is not seekable"))
    n < 0 && throw(ArgumentError("truncate failed, n bytes must be ≥ 0, got $n"))
    n > io.maxsize && throw(ArgumentError("truncate failed, $(n) bytes is exceeds IOBuffer maxsize $(io.maxsize)"))
    if n > length(io.data)
        resize!(io.data, n)
    end
    io.data[io.size+1:n] = 0
    io.size = n
    io.ptr = min(io.ptr, n+1)
    ismarked(io) && io.mark > n && unmark(io)
    return io
end

function compact(io::AbstractIOBuffer)
    io.writable || throw(ArgumentError("compact failed, IOBuffer is not writeable"))
    io.seekable && throw(ArgumentError("compact failed, IOBuffer is seekable"))
    local ptr::Int, bytes_to_move::Int
    if ismarked(io) && io.mark < io.ptr
        if io.mark == 0 return end
        ptr = io.mark
        bytes_to_move = nb_available(io) + (io.ptr-io.mark)
    else
        ptr = io.ptr
        bytes_to_move = nb_available(io)
    end
    copy!(io.data, 1, io.data, ptr, bytes_to_move)
    io.size -= ptr - 1
    io.ptr -= ptr - 1
    io.mark -= ptr - 1
    return io
end

function ensureroom(io::AbstractIOBuffer, nshort::Int)
    io.writable || throw(ArgumentError("ensureroom failed, IOBuffer is not writeable"))
    if !io.seekable
        nshort >= 0 || throw(ArgumentError("ensureroom failed, requested number of bytes must be ≥ 0, got $nshort"))
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

eof(io::AbstractIOBuffer) = (io.ptr-1 == io.size)

function close{T}(io::AbstractIOBuffer{T})
    io.readable = false
    io.writable = false
    io.seekable = false
    io.size = 0
    io.maxsize = 0
    io.ptr = 1
    io.mark = -1
    if io.writable
        resize!(io.data, 0)
    else
        io.data = T()
    end
    nothing
end

isopen(io::AbstractIOBuffer) = io.readable || io.writable || io.seekable || nb_available(io) > 0

function bytestring(io::AbstractIOBuffer)
    io.readable || throw(ArgumentError("bytestring read failed, IOBuffer is not readable"))
    io.seekable || throw(ArgumentError("bytestring read failed, IOBuffer is not seekable"))
    b = copy!(Array(UInt8, io.size), 1, io.data, 1, io.size)
    return isvalid(ASCIIString, b) ? ASCIIString(b) : UTF8String(b)
end

function takebuf_array(io::AbstractIOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        nbytes = io.size
        data = copy!(Array(UInt8, nbytes), 1, io.data, 1, nbytes)
    else
        nbytes = nb_available(io)
        data = read!(io, Array(UInt8, nbytes))
    end
    if io.writable
        io.ptr = 1
        io.size = 0
    end
    data
end
function takebuf_array(io::IOBuffer)
    ismarked(io) && unmark(io)
    if io.seekable
        data = io.data
        if io.writable
            maxsize = (io.maxsize == typemax(Int) ? 0 : min(length(io.data),io.maxsize))
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
function takebuf_string(io::AbstractIOBuffer)
    b = takebuf_array(io)
    return isvalid(ASCIIString, b) ? ASCIIString(b) : UTF8String(b)
end

function write(to::AbstractIOBuffer, from::AbstractIOBuffer)
    if to === from
        from.ptr = from.size + 1
        return 0
    end
    written::Int = write_sub(to, from.data, from.ptr, nb_available(from))
    from.ptr += written
    written
end

write(to::AbstractIOBuffer, p::Ptr, nb::Integer) = write(to, p, Int(nb))
function write(to::AbstractIOBuffer, p::Ptr, nb::Int)
    ensureroom(to, nb)
    ptr = (to.append ? to.size+1 : to.ptr)
    written = min(nb, length(to.data) - ptr + 1)
    p_u8 = convert(Ptr{UInt8}, p)
    for i = 0:written - 1
        @inbounds to.data[ptr + i] = unsafe_load(p_u8 + i)
    end
    to.size = max(to.size, ptr - 1 + written)
    if !to.append to.ptr += written end
    written
end

function write_sub{T}(to::AbstractIOBuffer, a::AbstractArray{T}, offs, nel)
    if offs+nel-1 > length(a) || offs < 1 || nel < 0
        throw(BoundsError())
    end
    local written::Int
    if isbits(T) && isa(a,Array)
        nb = nel * sizeof(T)
        ensureroom(to, Int(nb))
        ptr = (to.append ? to.size+1 : to.ptr)
        written = min(nb, length(to.data) - ptr + 1)
        unsafe_copy!(pointer(to.data, ptr),
                     convert(Ptr{UInt8}, pointer(a, offs)), written)
        to.size = max(to.size, ptr - 1 + written)
        if !to.append to.ptr += written end
    else
        written = 0
        ensureroom(to, sizeof(a))
        for i = offs:offs+nel-1
            written += write(to, a[i])
        end
    end
    written
end

write(to::AbstractIOBuffer, a::Array) = write_sub(to, a, 1, length(a))

function write(to::AbstractIOBuffer, a::UInt8)
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

write(to::AbstractIOBuffer, p::Ptr) = write(to, convert(UInt, p))

function readbytes!(io::AbstractIOBuffer, b::Array{UInt8}, nb=length(b))
    nr = min(nb, nb_available(io))
    if length(b) < nr
        resize!(b, nr)
    end
    read_sub(io, b, 1, nr)
    return nr
end
readbytes(io::AbstractIOBuffer) = read!(io, Array(UInt8, nb_available(io)))
readbytes(io::AbstractIOBuffer, nb) = read!(io, Array(UInt8, min(nb, nb_available(io))))

function search(buf::IOBuffer, delim::UInt8)
    p = pointer(buf.data, buf.ptr)
    q = ccall(:memchr,Ptr{UInt8},(Ptr{UInt8},Int32,Csize_t),p,delim,nb_available(buf))
    nb::Int = (q == C_NULL ? 0 : q-p+1)
    return nb
end

function search(buf::AbstractIOBuffer, delim::UInt8)
    data = buf.data
    for i = buf.ptr : buf.size
        @inbounds b = data[i]
        if b == delim
            return i - buf.ptr + 1
        end
    end
    return 0
end

function readuntil(io::AbstractIOBuffer, delim::UInt8)
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

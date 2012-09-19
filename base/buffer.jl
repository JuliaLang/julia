## work with Vector{Uint8} via I/O primitives ##

abstract Buffer <: IO

type DynamicBuffer <: Buffer
    data::Vector{Uint8}
    ptr::Int
    
    DynamicBuffer(data::Vector{Uint8}) = new(data, 1)
    DynamicBuffer(str::String) = DynamicBuffer(str.data)
    DynamicBuffer() = DynamicBuffer(Uint8[])
end

type FixedBuffer <: Buffer
    data::Vector{Uint8}
    ptr::Int

    FixedBuffer(data::Vector{Uint8}) = new(data, 1)
    FixedBuffer(str::String) = FixedBuffer(str.data)
    FixedBuffer() = FixedBuffer(Uint8[])
end

type LineBuffer <: Buffer
    data::Vector{Uint8}
    ptr::Int
    nlpos::Int

    LineBuffer(data::Vector{Uint8}) = new(data, 1)
    LineBuffer(str::String) = LineBuffer(str.data)
    LineBuffer() = LineBuffer(Uint8[])
end

function read{T}(from::Buffer, a::Array{T})
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        if length(from.data) - from.ptr < nb
            throw(EOFError())
        end
        from.ptr += nb
        return reshape(reinterpret(T, from.data[from.ptr-nb:from.ptr-1]), size(a))
    else
        #error("Read from Buffer only supports bits types or arrays of bits types; got $T.")
        error("Read from Buffer only supports bits types or arrays of bits types")
    end
end

function read(from::Buffer, ::Type{Uint8})
    if from.ptr > length(from.data)
        throw(EOFError())
    end
    from.ptr += 1
    return from.data[from.ptr-1]
end

skip(io::Buffer, n::Integer) = io.ptr += n
seek(io::Buffer, n::Integer) = io.ptr = n+1


grow(to::Buffer, nshort::Integer) = grow(to.data, nshort)
function takebuf_string(from::Buffer)
    array=from.data
    grow(array,from.ptr-length(array)-1)
    from.data=Array(Uint8,1)
    from.ptr=1
    convert(ASCIIString,array)
end

function write{T}(to::Buffer, a::Array{T})
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        nshort = to.ptr + nb - length(to.data) - 1
        if nshort > 0
            grow(to, nshort)
        end
        to.data[to.ptr:to.ptr+nb-1] = reinterpret(Uint8, a, (numel(a),))
        to.ptr += nb
    else
        error("Write to Buffer only supports bits types or arrays of bits types.")
    end
    nb
end

function write(to::Buffer, a::Uint8)
    if to.ptr + 1 > length(to.data)
        push(to.data, a)
    else
        to.data[to.ptr] = a
    end
    to.ptr += 1
    sizeof(Uint8)
end

function take_line(buffer::LineBuffer)
    ret = Array(Uint8,buffer.nlpos)
    ret[1:buffer.nlpos] = buffer.data[1:buffer.nlpos]
    ret
end

function takebuf_array(b::Buffer)
    d = b.data
    b.data = Uint8[]
    grow(d,b.ptr-length(d)-1)
    b.ptr = 1
    d
end

position(b::Buffer) = b.ptr

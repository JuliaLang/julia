## work with Vector{Uint8} via I/O primitives ##

# Stateful string
type Buffer <: IO
    data::Vector{Uint8}
    ptr::Int
    
    Buffer(data::Vector{Uint8}) = new(data, 1)
    # TODO: should be copy on write if given a string
    Buffer(str::String) = Buffer(str.data)
    Buffer() = Buffer(Uint8[])
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

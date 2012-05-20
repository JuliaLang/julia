## work with Vector{Uint8} via I/O primitives ##

# Stateful string
type IOString <: IO
    data::Vector{Uint8}
    ptr::Int
    
    IOString(data::Vector{Uint8}) = new(data, 1)
    # TODO: should be copy on write if given a string
    IOString(str::String) = IOString(str.data)
    IOString() = IOString(Uint8[])
end

function read{T}(from::IOString, a::Array{T})
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        if length(from.data) - from.ptr < nb
            throw(EOFError())
        end
        from.ptr += nb
        return reshape(reinterpret(T, from.data[from.ptr-nb:from.ptr-1]), size(a))
    else
        error("Read from IOString only supports bits types or arrays of bits types; got $T.")
    end
end

function read(from::IOString, ::Type{Uint8})
    if from.ptr > length(from.data)
        throw(EOFError())
    end
    from.ptr += 1
    return from.data[from.ptr-1]
end

skip(io::IOString, n::Integer) = io.ptr += n
seek(io::IOString, n::Integer) = io.ptr = n+1

function write{T}(to::IOString, a::Array{T})
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        if to.ptr + nb > length(to.data)
            # this is absurdly inefficient
            to.data = [to.data; Array(Uint8, to.ptr + nb - length(to.data) - 1)]
        end
        to.data[to.ptr:to.ptr+nb-1] = reinterpret(Uint8, a)
        to.ptr += nb
    else
        error("Write to IOString only supports bits types or arrays of bits types; got $T.")
    end
    nb
end

function write(to::IOString, a::Uint8)
    if to.ptr + 1 > length(to.data)
        # even more absurdly inefficient
        to.data = [to.data; uint8(0)]
    end
    to.data[to.ptr] = a
    to.ptr += 1
end

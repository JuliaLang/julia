## work with Vector{Uint8} via I/O primitives ##

import Base.read, Base.skip, Base.seek, Base.seek_end, Base.position
import Base.truncate, Base.eof, Base.close, Base.write

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
        if length(from.data) - from.ptr + 1 < nb
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

read{T}(from::IOString, ::Type{Ptr{T}}) = convert(Ptr{T}, read(from, Uint))

skip(io::IOString, n::Integer) = io.ptr += n
seek(io::IOString, n::Integer) = io.ptr = n+1
seek_end(io::IOString) = io.ptr = length(io.data)+1
position(io::IOString) = io.ptr-1
truncate(io::IOString, n::Integer) = (grow(io.data, n-length(io.data)); io.ptr = min(io.ptr, n+1); uint(0))
eof(io::IOString) = io.ptr-1 == length(io.data)
close(io::IOString) = (grow(io.data, 0); io.ptr = 1; nothing)

bytestring(io::IOString) = bytestring(io.data)

function write{T}(to::IOString, a::Array{T})
    if isa(T, BitsKind)
        nb = numel(a)*sizeof(T)
        nshort = to.ptr + nb - length(to.data) - 1
        if nshort > 0
            grow(to.data, nshort)
        end
        to.data[to.ptr:to.ptr+nb-1] = reinterpret(Uint8, a, (sizeof(T)*numel(a),))
        to.ptr += nb
    else
        error("Write to IOString only supports bits types or arrays of bits types; got $T.")
    end
    nb
end

function write(to::IOString, a::Uint8)
    if to.ptr + 1 > length(to.data)
        push(to.data, a)
    else
        to.data[to.ptr] = a
    end
    to.ptr += 1
    sizeof(Uint8)
end

write(to::IOString, p::Ptr) = write(to, convert(Uint, p))

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Data buffer for pipes.
mutable struct Buffer
    data::Vector{UInt8}
    ptr::Ptr{UInt8}
    size::Int

    function Buffer(bufsize)
        data = Vector{UInt8}(undef, bufsize)
        return new(data, pointer(data), 0)
    end
end

Base.empty!(buffer::Buffer) = buffer.size = 0
Base.getindex(buffer::Buffer, i::Integer) = unsafe_load(buffer.ptr, i)
Base.setindex!(buffer::Buffer, v::UInt8, i::Integer) = unsafe_store!(buffer.ptr, v, i)
Base.firstindex(buffer::Buffer) = 1
Base.lastindex(buffer::Buffer) = buffer.size
Base.pointer(buffer::Buffer) = buffer.ptr
capacity(buffer::Buffer) = Int(pointer(buffer.data, lastindex(buffer.data) + 1) - buffer.ptr)

function consumed!(buffer::Buffer, n::Integer)
    @assert n ≤ buffer.size
    buffer.ptr += n
    buffer.size -= n
end

function read_to_buffer(io::IO, buffer::Buffer)
    offset = buffer.ptr - pointer(buffer.data)
    copyto!(buffer.data, 1, buffer.data, offset, buffer.size)
    buffer.ptr = pointer(buffer.data) + buffer.size
    if !eof(io)
        n = min(bytesavailable(io), capacity(buffer) - buffer.size)
        unsafe_read(io, buffer.ptr + buffer.size, n)
        buffer.size += n
    end
    return
end

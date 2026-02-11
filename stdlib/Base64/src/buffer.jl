# This file is a part of Julia. License is MIT: https://julialang.org/license

# Data buffer for pipes.
mutable struct Buffer
    const data::Memory{UInt8}
    offset::Int
    size::Int

    function Buffer(bufsize)
        data = Memory{UInt8}(undef, bufsize)
        return new(data, 0, 0)
    end
end

Base.empty!(buffer::Buffer) = buffer.size = 0
Base.getindex(buffer::Buffer, i::Integer) = buffer.data[buffer.offset + i]
Base.setindex!(buffer::Buffer, v::UInt8, i::Integer) = buffer.data[buffer.offset + i] = v
Base.firstindex(buffer::Buffer) = 1
Base.lastindex(buffer::Buffer) = buffer.size
Base.pointer(buffer::Buffer) = pointer(buffer.data) + buffer.offset
capacity(buffer::Buffer) = length(buffer.data) - buffer.offset

function consumed!(buffer::Buffer, n::Integer)
    @assert n ≤ buffer.size
    buffer.offset += n
    buffer.size -= n
end

function read_to_buffer(io::IO, buffer::Buffer)
    offset = buffer.offset
    copyto!(buffer.data, 1, buffer.data, offset + 1, buffer.size)
    buffer.offset = 0
    if !eof(io)
        n = min(bytesavailable(io), capacity(buffer) - buffer.size)
        unsafe_read(io, pointer(buffer) + buffer.size, n)
        buffer.size += n
    end
    return
end

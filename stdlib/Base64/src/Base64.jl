module Base64

export
    Base64EncodePipe,
    base64encode,
    Base64DecodePipe,
    base64decode

# Data buffer for pipes.
mutable struct Buffer
    data::Vector{UInt8}
    ptr::Ptr{UInt8}
    size::Int

    function Buffer(bufsize)
        data = Vector{UInt8}(bufsize)
        return new(data, pointer(data), 0)
    end
end

Base.empty!(buffer::Buffer) = buffer.size = 0
Base.getindex(buffer::Buffer, i::Integer) = unsafe_load(buffer.ptr, i)
Base.setindex!(buffer::Buffer, v::UInt8, i::Integer) = unsafe_store!(buffer.ptr, v, i)
Base.endof(buffer::Buffer) = buffer.size
Base.pointer(buffer::Buffer) = buffer.ptr
capacity(buffer::Buffer) = Int(pointer(buffer.data, endof(buffer.data) + 1) - buffer.ptr)

function consumed!(buffer::Buffer, n::Integer)
    @assert n ≤ buffer.size
    buffer.ptr += n
    buffer.size -= n
end

function read_to_buffer(io::IO, buffer::Buffer)
    offset = buffer.ptr - pointer(buffer.data)
    copy!(buffer.data, 1, buffer.data, offset, buffer.size)
    buffer.ptr = pointer(buffer.data) + buffer.size
    if !eof(io)
        n = min(nb_available(io), capacity(buffer) - buffer.size)
        unsafe_read(io, buffer.ptr + buffer.size, n)
        buffer.size += n
    end
    return
end

const BASE64_ENCODE = [UInt8(x) for x in ['A':'Z'; 'a':'z'; '0':'9'; '+'; '/']]
encode(x::UInt8) = BASE64_ENCODE[(x & 0x3f) + 1]
encodepadding()  = UInt8('=')

const BASE64_CODE_END = 0x40
const BASE64_CODE_PAD = 0x41
const BASE64_CODE_IGN = 0x42
const BASE64_DECODE = fill(BASE64_CODE_IGN, 256)
for (i, c) in enumerate(BASE64_ENCODE)
    BASE64_DECODE[Int(c)+1] = UInt8(i - 1)
end
BASE64_DECODE[Int(encodepadding())+1] = BASE64_CODE_PAD
decode(x::UInt8) = BASE64_DECODE[x + 1]


# Encoder
# -------

struct Base64EncodePipe <: IO
    io::IO
    buffer::Buffer

    function Base64EncodePipe(io::IO)
        # The buffer size must be at least 3.
        buffer = Buffer(512)
        pipe = new(io, buffer)
        finalizer(buffer, _ -> close(pipe))
        return pipe
    end
end

function Base.unsafe_write(pipe::Base64EncodePipe, ptr::Ptr{UInt8}, n::UInt)::Int
    buffer = pipe.buffer
    m = buffer.size
    b1, b2, b3, k = loadtriplet!(buffer, ptr, n)
    @assert k ≥ m
    p = ptr + k - m
    if k < 3
        if k == 1
            buffer[1] = b1
            buffer.size = 1
        elseif k == 2
            buffer[1] = b1
            buffer[2] = b2
            buffer.size = 2
        end
        return p - ptr
    end
    @assert buffer.size == 0

    capacity = length(buffer.data)
    i = 0
    p_end = ptr + n
    while true
        buffer[i+1] = encode(b1 >> 2          )
        buffer[i+2] = encode(b1 << 4 | b2 >> 4)
        buffer[i+3] = encode(b2 << 2 | b3 >> 6)
        buffer[i+4] = encode(          b3     )
        i += 4
        if p + 2 < p_end
            b1 = unsafe_load(p, 1)
            b2 = unsafe_load(p, 2)
            b3 = unsafe_load(p, 3)
            p += 3
        else
            break
        end
        if i + 4 > capacity
            unsafe_write(pipe.io, pointer(buffer), i)
            i = 0
        end
    end
    if i > 0
        unsafe_write(pipe.io, pointer(buffer), i)
    end

    while p < p_end
        buffer[buffer.size+=1] = unsafe_load(p, 1)
        p += 1
    end
    return p - ptr
end

function Base.write(pipe::Base64EncodePipe, x::UInt8)
    buffer = pipe.buffer
    buffer[buffer.size+=1] = x
    if buffer.size == 3
        unsafe_write(pipe, C_NULL, 0)
    end
    return 1
end

function Base.close(pipe::Base64EncodePipe)
    b1, b2, b3, k = loadtriplet!(pipe.buffer, convert(Ptr{UInt8}, C_NULL), 0)
    if k == 0
        # no leftover and padding
    elseif k == 1
        write(pipe.io,
              encode(b1 >> 2),
              encode(b1 << 4),
              encodepadding(),
              encodepadding())
    elseif k == 2
        write(pipe.io,
              encode(          b1 >> 2),
              encode(b1 << 4 | b2 >> 4),
              encode(b2 << 2          ),
              encodepadding())
    else
        @assert k == 3
        write(pipe.io,
              encode(b1 >> 2          ),
              encode(b1 << 4 | b2 >> 4),
              encode(b2 << 2 | b3 >> 6),
              encode(          b3     ))
    end
    return nothing
end

# Load three bytes from buffer and ptr.
function loadtriplet!(buffer::Buffer, ptr::Ptr{UInt8}, n::Integer)
    b1 = b2 = b3 = 0x00
    if buffer.size == 0
        if n == 0
            k = 0
        elseif n == 1
            b1 = unsafe_load(ptr, 1)
            k = 1
        elseif n == 2
            b1 = unsafe_load(ptr, 1)
            b2 = unsafe_load(ptr, 2)
            k = 2
        else
            b1 = unsafe_load(ptr, 1)
            b2 = unsafe_load(ptr, 2)
            b3 = unsafe_load(ptr, 3)
            k = 3
        end
    elseif buffer.size == 1
        b1 = buffer[1]
        if n == 0
            k = 1
        elseif n == 1
            b2 = unsafe_load(ptr, 1)
            k = 2
        else
            b2 = unsafe_load(ptr, 1)
            b3 = unsafe_load(ptr, 2)
            k = 3
        end
    elseif buffer.size == 2
        b1 = buffer[1]
        b2 = buffer[2]
        if n == 0
            k = 2
        else
            b3 = unsafe_load(ptr, 1)
            k = 3
        end
    else
        @assert buffer.size == 3
        b1 = buffer[1]
        b2 = buffer[2]
        b3 = buffer[3]
        k = 3
    end
    empty!(buffer)
    return b1, b2, b3, k
end

function base64encode(f::Function, args...)
    s = IOBuffer()
    b = Base64EncodePipe(s)
    f(b, args...)
    close(b)
    return String(take!(s))
end
base64encode(args...) = base64encode(write, args...)


# Decoder
# -------

struct Base64DecodePipe <: IO
    io::IO
    buffer::Buffer
    rest::Vector{UInt}

    function Base64DecodePipe(io::IO)
        buffer = Buffer(512)
        pipe = new(io, buffer, UInt8[])
        finalizer(buffer, _ -> close(pipe))
        return pipe
    end
end

function Base.unsafe_read(pipe::Base64DecodePipe, ptr::Ptr{UInt8}, n::UInt)
    p = read_avail(pipe, ptr, n)
    if p < ptr + n
        throw(EOFError())
    end
    return nothing
end

function read_avail(pipe::Base64DecodePipe, ptr::Ptr{UInt8}, n::UInt)
    p = ptr
    p_end = ptr + n
    while !isempty(pipe.rest) && p < p_end
        unsafe_store!(p, shift!(pipe.rest))
        p += 1
    end

    buffer = pipe.buffer
    i = 0
    b1 = b2 = b3 = b4 = BASE64_CODE_IGN
    while true
        if b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 < 0x40 && p + 2 < p_end
            # fast path to decode
            unsafe_store!(p    , b1 << 2 | b2 >> 4)
            unsafe_store!(p + 1, b2 << 4 | b3 >> 2)
            unsafe_store!(p + 2, b3 << 6 | b4     )
            p += 3
        else
            i, p, finished = decode_slow(b1, b2, b3, b4, buffer, i, pipe.io, p, p_end - p, pipe.rest)
            if finished
                break
            end
        end
        if p < p_end
            if i + 4 ≤ endof(buffer)
                b1 = decode(buffer[i+1])
                b2 = decode(buffer[i+2])
                b3 = decode(buffer[i+3])
                b4 = decode(buffer[i+4])
                i += 4
            else
                consumed!(buffer, i)
                read_to_buffer(pipe.io, buffer)
                i = 0
                b1 = b2 = b3 = b4 = BASE64_CODE_IGN
            end
        else
            break
        end
    end
    consumed!(buffer, i)

    return p
end

function Base.read(pipe::Base64DecodePipe, ::Type{UInt8})
    if isempty(pipe.rest)
        unsafe_read(pipe, convert(Ptr{UInt8}, C_NULL), 0)
        if isempty(pipe.rest)
            throw(EOFError())
        end
    end
    return shift!(pipe.rest)
end

function Base.readbytes!(pipe::Base64DecodePipe, data::AbstractVector{UInt8}, nb::Integer=length(data))
    filled::Int = 0
    while filled < nb && !eof(pipe)
        if length(data) == filled
            resize!(data, min(length(data) * 2, nb))
        end
        p = pointer(data, filled + 1)
        p_end = read_avail(pipe, p, UInt(min(length(data), nb) - filled))
        filled += p_end - p
    end
    resize!(data, filled)
    return filled
end

Base.eof(pipe::Base64DecodePipe) = isempty(pipe.rest) && eof(pipe.io)
Base.close(pipe::Base64DecodePipe) = nothing

# Decode data from (b1, b2, b3, b5, buffer, input) into (ptr, rest).
function decode_slow(b1, b2, b3, b4, buffer, i, input, ptr, n, rest)
    # Skip ignore code.
    while true
        if b1 == BASE64_CODE_IGN
            b1, b2, b3 = b2, b3, b4
        elseif b2 == BASE64_CODE_IGN
            b2, b3 = b3, b4
        elseif b3 == BASE64_CODE_IGN
            b3 = b4
        elseif b4 == BASE64_CODE_IGN
            # pass
        else
            break
        end
        if i + 1 ≤ endof(buffer)
            b4 = decode(buffer[i+=1])
        elseif !eof(input)
            b4 = decode(read(input, UInt8))
        else
            b4 = BASE64_CODE_END
            break
        end
    end

    # Check the decoded quadruplet.
    k = 0
    finished = false
    if b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 < 0x40
        # pass
        k = 3
    elseif b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 == BASE64_CODE_PAD
        b4 = 0x00
        k = 2
    elseif b1 < 0x40 && b2 < 0x40 && b3 == b4 == BASE64_CODE_PAD
        b3 = b4 = 0x00
        k = 1
    elseif b1 == b2 == b3 == BASE64_CODE_IGN && b4 == BASE64_CODE_END
        b1 = b2 = b3 = b4 = 0x00
        finished = true
    else
        throw(ArgumentError("malformed base64 sequence"))
    end

    # Write output.
    p = ptr
    p_end = ptr + n
    function output(b)
        if p < p_end
            unsafe_store!(p, b)
            p += 1
        else
            push!(rest, b)
        end
    end
    k ≥ 1 && output(b1 << 2 | b2 >> 4)
    k ≥ 2 && output(b2 << 4 | b3 >> 2)
    k ≥ 3 && output(b3 << 6 | b4     )

    return i, p, finished
end

function base64decode(s)
    b = IOBuffer(s)
    try
        return read(Base64DecodePipe(b))
    finally
        close(b)
    end
end

end

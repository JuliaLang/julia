# This file is a part of Julia. License is MIT: https://julialang.org/license

# Generate decode table.
const BASE64_CODE_END = 0x40
const BASE64_CODE_PAD = 0x41
const BASE64_CODE_IGN = 0x42
const BASE64_DECODE = fill(BASE64_CODE_IGN, 256)
for (i, c) in enumerate(BASE64_ENCODE)
    BASE64_DECODE[Int(c)+1] = UInt8(i - 1)
end
BASE64_DECODE[Int(encodepadding())+1] = BASE64_CODE_PAD
decode(x::UInt8) = @inbounds return BASE64_DECODE[x + 1]

"""
    Base64DecodePipe(istream)

Return a new read-only I/O stream, which decodes base64-encoded data read from
`istream`.

# Examples
```jldoctest
julia> io = IOBuffer();

julia> iob64_decode = Base64DecodePipe(io);

julia> write(io, "SGVsbG8h")
8

julia> seekstart(io);

julia> String(read(iob64_decode))
"Hello!"
```
"""
struct Base64DecodePipe <: IO
    io::IO
    buffer::Buffer
    rest::Vector{UInt8}

    function Base64DecodePipe(io::IO)
        buffer = Buffer(512)
        return new(io, buffer, UInt8[])
    end
end

function Base.unsafe_read(pipe::Base64DecodePipe, ptr::Ptr{UInt8}, n::UInt)
    p = read_until_end(pipe, ptr, n)
    if p < ptr + n
        throw(EOFError())
    end
    return nothing
end

# Read and decode as much data as possible.
function read_until_end(pipe::Base64DecodePipe, ptr::Ptr{UInt8}, n::UInt)
    p = ptr
    p_end = ptr + n
    while !isempty(pipe.rest) && p < p_end
        unsafe_store!(p, popfirst!(pipe.rest))
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
            i, p, ended = decode_slow(b1, b2, b3, b4, buffer, i, pipe.io, p, p_end - p, pipe.rest)
            if ended
                break
            end
        end
        if p < p_end
            if i + 4 ≤ lastindex(buffer)
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
    return popfirst!(pipe.rest)
end

function Base.readbytes!(pipe::Base64DecodePipe, data::AbstractVector{UInt8}, nb::Integer=length(data))
    require_one_based_indexing(data)
    filled::Int = 0
    while filled < nb && !eof(pipe)
        if length(data) == filled
            resize!(data, min(length(data) * 2, nb))
        end
        p = pointer(data, filled + 1)
        p_end = read_until_end(pipe, p, UInt(min(length(data), nb) - filled))
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
        if i + 1 ≤ lastindex(buffer)
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
    if b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 < 0x40
        k = 3
    elseif b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 == BASE64_CODE_PAD
        b4 = 0x00
        k = 2
    elseif b1 < 0x40 && b2 < 0x40 && b3 == b4 == BASE64_CODE_PAD
        b3 = b4 = 0x00
        k = 1
    elseif b1 == b2 == b3 == BASE64_CODE_IGN && b4 == BASE64_CODE_END
        b1 = b2 = b3 = b4 = 0x00
    else
        throw(ArgumentError("malformed base64 sequence"))
    end

    # Write output.
    p::Ptr{UInt8} = ptr
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

    return i, p, k == 0
end

"""
    base64decode(string)

Decode the base64-encoded `string` and returns a `Vector{UInt8}` of the decoded
bytes.

See also [`base64encode`](@ref).

# Examples
```jldoctest
julia> b = base64decode("SGVsbG8h")
6-element Array{UInt8,1}:
 0x48
 0x65
 0x6c
 0x6c
 0x6f
 0x21

julia> String(b)
"Hello!"
```
"""
function base64decode(s)
    b = IOBuffer(s)
    try
        return read(Base64DecodePipe(b))
    finally
        close(b)
    end
end

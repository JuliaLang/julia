# This file is a part of Julia. License is MIT: https://julialang.org/license

# Generate decode table.
const BASE64_CODE_END = 0x40
const BASE64_CODE_PAD = 0x41
const BASE64_CODE_IGN = 0x42
const BASE64_DECODE = fill(BASE64_CODE_IGN, 256)
const BASE64URL_DECODE = fill(BASE64_CODE_IGN, 256)
for (i, c) in enumerate(BASE64_ENCODE)
    BASE64_DECODE[Int(c)+1] = UInt8(i - 1)
end
for (i, c) in enumerate(BASE64URL_ENCODE)
    BASE64URL_DECODE[Int(c)+1] = UInt8(i - 1)
end

const PADIND_DECODE = Int(ENCODEPADDING)+1
BASE64_DECODE[PADIND_DECODE] = BASE64_CODE_PAD
BASE64URL_DECODE[PADIND_DECODE] = BASE64_CODE_PAD
const TABLE_DECODE = hcat(BASE64_DECODE, BASE64URL_DECODE)

decodeonechar(x::UInt8, decode::Base64Format) = @inbounds return TABLE_DECODE[x + 1, UInt8(decode)]

"""
    Base64DecodePipe(istream; decode::Base64Format=BASE64)

Return a new read-only I/O stream, which decodes base64-encoded data read from
`istream`.

`Base64Format` object given as `decode` determines whether this pipe decodes data
encoded in Base64 or Base64URL.

See also [`Base64Format`](@ref).

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
    decode::Base64Format
    rest::Vector{UInt8}

    function Base64DecodePipe(io::IO; decode::Base64Format=BASE64)
        buffer = Buffer(512)
        return new(io, buffer, decode, UInt8[])
    end
end

Base.isreadable(pipe::Base64DecodePipe) = !isempty(pipe.rest) || isreadable(pipe.io)
Base.iswritable(::Base64DecodePipe) = false

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
            bs, i = skip_ignore(pipe.decode, b1, b2, b3, b4, buffer, i, pipe.io)
            i, p, ended = decode_slow(bs..., i, p, p_end - p, pipe.rest)
            if ended
                break
            end
        end
        if p < p_end
            if i + 4 ≤ lastindex(buffer)
                b1 = decodeonechar(buffer[i+1], pipe.decode)
                b2 = decodeonechar(buffer[i+2], pipe.decode)
                b3 = decodeonechar(buffer[i+3], pipe.decode)
                b4 = decodeonechar(buffer[i+4], pipe.decode)
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

Base.eof(pipe::Base64DecodePipe) = isempty(pipe.rest) && eof(pipe.io)::Bool
Base.close(pipe::Base64DecodePipe) = nothing

# Skip ignore code in b1, b2, b3, b4.
function skip_ignore(decode, b1, b2, b3, b4, buffer, i, input)
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
            b4 = decodeonechar(buffer[i+=1], decode)
        elseif !eof(input)
            b4 = decodeonechar(read(input, UInt8), decode)
        else
            b4 = BASE64_CODE_END
        end
    end

    return (b1, b2, b3, b4), i
end

# Decode data from (b1, b2, b3, b5, buffer, input) into (ptr, rest).
function decode_slow(b1, b2, b3, b4, i, ptr, n, rest)
    # Check the decoded quadruplet.
    k = 0
    if b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && b4 < 0x40
        k = 3
    elseif b1 < 0x40 && b2 < 0x40 && b3 < 0x40 && (b4 == BASE64_CODE_PAD || b4 == BASE64_CODE_END)
        b4 = 0x00
        k = 2
    elseif b1 < 0x40 && b2 < 0x40 && (b3 == BASE64_CODE_PAD || b3 == BASE64_CODE_END) && (b4 == BASE64_CODE_PAD || b4 == BASE64_CODE_END)
        b3 = b4 = 0x00
        k = 1
    elseif b1 == b2 == b3 == b4 == BASE64_CODE_END
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
    base64decode(string; decode=BASE64)

Decode the base64-encoded `string` and returns a `Vector{UInt8}` of the decoded
bytes.

`decode::Base64Format` specifies an encoding format in which `string` is encoded.

See also [`Base64Format`](@ref) and [`base64encode`](@ref).

# Examples
```jldoctest
julia> b = base64decode("SGVsbG8h")
6-element Vector{UInt8}:
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
function base64decode(s; decode=BASE64)
    b = IOBuffer(s)
    try
        return read(Base64DecodePipe(b; decode=decode))
    finally
        close(b)
    end
end

immutable UTF16String <: String
    data::Array{Uint16,1} # includes 16-bit NULL termination after string chars
    function UTF16String(data::Vector{Uint16})
        if length(data) < 1 || data[end] != 0
            throw(ArgumentError("UTF16String data must be NULL-terminated"))
        end
        new(data)
    end
end

utf16_is_lead(c::Uint16) = (c & 0xfc00) == 0xd800
utf16_is_trail(c::Uint16) = (c & 0xfc00) == 0xdc00
utf16_is_surrogate(c::Uint16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::Uint16, trail::Uint16) = char(uint32(lead-0xd7f7)<<10 + trail)

function endof(s::UTF16String)
    d = s.data
    i = length(d) - 1
    i == 0 && return i
    utf16_is_surrogate(d[i]) ? i-1 : i
end
function next(s::UTF16String, i::Int)
    if !utf16_is_surrogate(s.data[i])
        return char(s.data[i]), i+1
    elseif length(s.data)-1 > i && utf16_is_lead(s.data[i]) && utf16_is_trail(s.data[i+1])
        return utf16_get_supplementary(s.data[i], s.data[i+1]), i+2
    end
    error("invalid UTF-16 character index")
end

# TODO: optmize this
function encode16(s::String)
    buf = Uint16[]
    for c in s
        if c < 0x10000
            push!(buf, uint16(c))
        else
            push!(buf, uint16(0xd7c0 + (c>>10) & 0x3ff))
            push!(buf, uint16(0xdc00 + c & 0x3ff))
        end
    end
    push!(buf, 0) # NULL termination
    UTF16String(buf)
end

utf16(x) = convert(UTF16String, x)
convert(::Type{UTF16String}, s::UTF16String) = s
convert(::Type{UTF16String}, s::String) = encode16(s)
convert(::Type{Array{Uint16,1}}, s::UTF16String) = s.data
convert(::Type{Array{Uint16}}, s::UTF16String) = s.data

# TODO: optimize this
convert(::Type{UTF8String}, s::UTF16String) =
    sprint(length(s.data)-1, io->for c in s; write(io,c::Char); end)

sizeof(s::UTF16String) = sizeof(s.data) - sizeof(Uint16)
convert{T<:Union(Int16,Uint16)}(::Type{Ptr{T}}, s::UTF16String) =
    convert(Ptr{T}, pointer(s))

function is_valid_utf16(data::AbstractArray{Uint16})
    i = 1
    n = length(data) # this may include NULL termination; that's okay
    while i < n # check for unpaired surrogates
        if utf16_is_lead(data[i]) && utf16_is_trail(data[i+1])
            i += 2
        elseif utf16_is_surrogate(data[i])
            return false
        else
            i += 1
        end
    end
    return i > n || !utf16_is_surrogate(data[i])
end

is_valid_utf16(s::UTF16String) = is_valid_utf16(s.data)

function convert(::Type{UTF16String}, data::AbstractVector{Uint16})
    !is_valid_utf16(data) && throw(ArgumentError("invalid UTF16 data"))
    len = length(data)
    d = Array(Uint16, len + 1)
    d[end] = 0 # NULL terminate
    UTF16String(copy!(d,1, data,1, len))
end

convert(T::Type{UTF16String}, data::AbstractArray{Uint16}) =
    convert(T, reshape(data, length(data)))

convert(T::Type{UTF16String}, data::AbstractArray{Int16}) =
    convert(T, reinterpret(Uint16, data))

function convert(T::Type{UTF16String}, bytes::AbstractArray{Uint8})
    isempty(bytes) && return UTF16String(Uint16[0])
    isodd(length(bytes)) && throw(ArgumentError("odd number of bytes"))
    data = reinterpret(Uint16, bytes)
    # check for byte-order mark (BOM):
    if data[1] == 0xfeff        # native byte order
        d = Array(Uint16, length(data))
        copy!(d,1, data,2, length(data)-1)
    elseif data[1] == 0xfffe    # byte-swapped
        d = Array(Uint16, length(data))
        for i = 2:length(data)
            d[i-1] = bswap(data[i])
        end
    else
        d = Array(Uint16, length(data) + 1)
        copy!(d,1, data,1, length(data)) # assume native byte order
    end
    d[end] = 0 # NULL terminate
    !is_valid_utf16(d) && throw(ArgumentError("invalid UTF16 data"))
    UTF16String(d)
end

utf16(p::Ptr{Uint16}, len::Integer) = utf16(pointer_to_array(p, len))
utf16(p::Ptr{Int16}, len::Integer) = utf16(convert(Ptr{Uint16}, p), len)
function utf16(p::Union(Ptr{Uint16}, Ptr{Int16}))
    len = 0
    while unsafe_load(p, len+1) != 0; len += 1; end
    utf16(p, len)
end

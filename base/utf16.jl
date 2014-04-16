immutable UTF16String <: String
    data::Array{Uint16,1}
end

utf16_is_lead(c::Uint16) = (c & 0xfc00) == 0xd800
utf16_is_trail(c::Uint16) = (c & 0xfc00) == 0xdc00
utf16_is_surrogate(c::Uint16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::Uint16, trail::Uint16) = char((lead-0xd7f7)<<10 + trail)

function endof(s::UTF16String)
    d = s.data
    i = length(d)
    i == 0 && return i
    utf16_is_surrogate(d[i]) ? i-1 : i
end

function next(s::UTF16String, i::Int)
    if !utf16_is_surrogate(s.data[i])
        return char(s.data[i]), i+1
    elseif length(s.data) > i && utf16_is_lead(s.data[i]) && utf16_is_trail(s.data[i+1])
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
    UTF16String(buf)
end

utf16(x) = convert(UTF16String, x)
convert(::Type{UTF16String}, s::UTF16String) = s
convert(::Type{UTF16String}, s::String) = encode16(s)
convert(::Type{UTF8String}, s::UTF16String) =
    sprint(length(s.data), io->for c in s; write(io,c::Char); end)
convert(::Type{Array{Uint16,1}}, s::UTF16String) = s.data
convert(::Type{Array{Uint16}}, s::UTF16String) = s.data

sizeof(s::UTF16String) = sizeof(s.data)
convert{T<:Union(Int16,Uint16)}(::Type{Ptr{T}}, s::UTF16String) =
    convert(Ptr{T}, s.data)

function is_valid_utf16(data::Array{Uint16})
    i = 1
    n = length(data)
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

function convert(::Type{UTF16String}, data::Array{Uint16})
    !is_valid_utf16(data) && throw(ArgumentError("invalid UTF16 data"))
    UTF16String(data)
end

function convert(T::Type{UTF16String}, bytes::Array{Uint8})
    isempty(bytes) && return UTF16String(Uint16[])
    isodd(length(bytes)) && throw(ArgumentError("odd number of bytes"))
    data = reinterpret(Uint16, bytes)    
    # check for byte-order mark (BOM):
    if data[1] == 0xfeff        # native byte order
        convert(T, data[2:end])
    elseif data[1] == 0xfffe    # byte-swapped
        convert(T, Uint16[bswap(data[i]) for i=2:length(data)])
    else
        convert(T, copy(data)) # assume native byte order
    end
end

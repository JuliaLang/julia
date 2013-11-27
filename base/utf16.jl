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

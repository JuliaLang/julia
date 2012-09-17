module UTF16
import Base.*

export UTF16String,
       convert,
       length,
       next,
       utf16

type UTF16String <: String
    data::Array{Uint16,1}
end

length(s::UTF16String) = length(s.data)

utf16_is_lead(c::Uint16) = (c & 0xfc00) == 0xd800
utf16_is_trail(c::Uint16) = (c & 0xfc00) == 0xdc00
utf16_is_surrogate(c::Uint16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::Uint16, trail::Uint16) = char((lead-0xd7f7)<<10 + trail)

function next(s::UTF16String, i::Int)
    if !utf16_is_surrogate(s.data[i])
        return char(s.data[i]), i+1
    elseif length(s.data) > i && utf16_is_lead(s.data[i]) && utf16_is_trail(s.data[i+1])
        return utf16_get_supplementary(s.data[i], s.data[i+1]), i+2
    end
    error("invalid UTF-16 character index")
end

function encode16(s::String)
    buf = Array(Uint16, length(s))
    n = 0
    for c in s
        if c < 0x10000
            n += 1
            buf[n] = uint16(c)
        else
            n += 1
            buf[n] = uint16(0xd7c0 + (c>>10) & 0x3ff)
            n += 1
            buf[n] = uint16(0xdc00 + c & 0x3ff)
        end
    end
    return UTF16String(buf[1:n])
end

utf16(x) = convert(UTF16String, x)
convert(::Type{UTF16String}, s::UTF16String) = s
convert(::Type{UTF16String}, s::String) = encode16(s)
convert(::Type{UTF8String}, s::UTF16String) =
    sprint(length(s), io->for c in s; write(io,c::Char); end)

function test_utf16()
    u8 = "𝕥𝟶f𠂊"
    u16 = utf16(u8)
    @assert length(u16) == 7
    @assert strlen(u16) == 4
    @assert utf8(u16) == u8
end

end # module

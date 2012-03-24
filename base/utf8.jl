## from src/boot.jl:
#
# type UTF8String <: String
#     data::Array{Uint8,1}
# end
#

## basic UTF-8 decoding & iteration ##

const _jl_utf8_offset = [
    0x00000000, 0x00003080,
    0x000e2080, 0x03c82080,
    0xfa082080, 0x82082080,
]

const _jl_utf8_trailing = [
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,
]

is_utf8_start(byte::Uint8) = ((byte&0xc0)!=0x80)

## required core functionality ##

length(s::UTF8String) = length(s.data)

function next(s::UTF8String, i::Int)
    if !is_utf8_start(s.data[i])
        error("invalid UTF-8 character index")
    end
    trailing = _jl_utf8_trailing[s.data[i]+1]
    if length(s.data) < i + trailing
        error("premature end of UTF-8 data")
    end
    c = uint32(0)
    for j = 1:trailing
        c += s.data[i]
        c <<= 6
        i += 1
    end
    c += s.data[i]
    i += 1
    c -= _jl_utf8_offset[trailing+1]
    char(c), i
end

## overload methods for efficiency ##

isvalid(s::UTF8String, i::Integer) =
    (1 <= i <= length(s.data)) && is_utf8_start(s.data[i])

function ref(s::UTF8String, r::Range1{Int})
    i = isvalid(s,first(r)) ? first(r) : nextind(s,first(r))
    j = nextind(s,last(r)) - 1
    UTF8String(s.data[i:j])
end

strchr(s::UTF8String, c::Char) =
    c < 0x80 ? memchr(s.data, c) : invoke(strchr, (String,Char), s, c)

strcat(a::ByteString, b::ByteString, c::ByteString...) = UTF8String(memcat(a,b,c...))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called

transform_to_utf8(s::String, f::Function) =
    print_to_string(length(s), @thunk for c=s; print(f(c)); end)

uppercase(s::UTF8String) = transform_to_utf8(s, uppercase)
lowercase(s::UTF8String) = transform_to_utf8(s, lowercase)

ucfirst(s::UTF8String) = print_to_string(length(s), print, uppercase(s[1]), s[2:])
lcfirst(s::UTF8String) = print_to_string(length(s), print, lowercase(s[1]), s[2:])

## outputing UTF-8 strings ##

print(s::UTF8String) = print(s.data)
write(io, s::UTF8String) = write(io, s.data)

## transcoding to UTF-8 ##

utf8(x) = convert(UTF8String, x)
convert(::Type{UTF8String}, s::UTF8String) = s
convert(::Type{UTF8String}, s::ASCIIString) = UTF8String(s.data)
convert(::Type{UTF8String}, a::Array{Uint8,1}) = check_utf8(UTF8String(a))
convert(::Type{UTF8String}, s::String) = utf8(cstring(s))

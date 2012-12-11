## from base/boot.jl:
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
strlen(s::UTF8String) = ccall(:u8_strlen, Int, (Ptr{Uint8},), s.data)

function ref(s::UTF8String, i::Int)
    d = s.data
    b = d[i]
    if !is_utf8_start(b)
        error("invalid UTF-8 character index")
    end
    trailing = _jl_utf8_trailing[b+1]
    if length(d) < i + trailing
        error("premature end of UTF-8 data")
    end
    c::Uint32 = 0
    for j = 1:trailing+1
        c <<= 6
        c += d[i]
        i += 1
    end
    c -= _jl_utf8_offset[trailing+1]
    char(c)
end

# this is a trick to allow inlining and tuple elision
next(s::UTF8String, i::Int) = (s[i], i+1+_jl_utf8_trailing[s.data[i]+1])

function first_utf8_byte(c::Char)
    c < 0x80    ? uint8(c)            :
    c < 0x800   ? uint8((c>>6 )|0xc0) :
    c < 0x10000 ? uint8((c>>12)|0xe0) :
                  uint8((c>>18)|0xf0)
end

## overload methods for efficiency ##

isvalid(s::UTF8String, i::Integer) =
    (1 <= i <= length(s.data)) && is_utf8_start(s.data[i])

function ref(s::UTF8String, r::Range1{Int})
    i = isvalid(s,first(r)) ? first(r) : nextind(s,first(r))
    j = nextind(s,last(r))-1
    UTF8String(s.data[i:j])
end

function strchr(s::UTF8String, c::Char, i::Integer)
    if c < 0x80 return memchr(s.data, c, i) end
    while true
        i = memchr(s.data, first_utf8_byte(c), i)
        if i==0 || s[i]==c return i end
        i = next(s,i)[2]
    end
end

strcat(a::ByteString, b::ByteString, c::ByteString...) =
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called
    UTF8String([a.data,b.data,map(s->s.data,c)...])

transform_to_utf8(s::String, f::Function) =
    sprint(length(s), io->for c in s; write(io,f(c)::Char); end)

uppercase(s::UTF8String) = transform_to_utf8(s, uppercase)
lowercase(s::UTF8String) = transform_to_utf8(s, lowercase)

ucfirst(s::UTF8String) = string(uppercase(s[1]), s[2:])
lcfirst(s::UTF8String) = string(lowercase(s[1]), s[2:])

## outputing UTF-8 strings ##

print(io::IO, s::UTF8String) = (write(io, s.data);nothing)
write(io::IO, s::UTF8String) = write(io, s.data)

## transcoding to UTF-8 ##

utf8(x) = convert(UTF8String, x)
convert(::Type{UTF8String}, s::UTF8String) = s
convert(::Type{UTF8String}, s::ASCIIString) = UTF8String(s.data)
convert(::Type{UTF8String}, a::Array{Uint8,1}) = check_utf8(UTF8String(a))
convert(::Type{UTF8String}, s::String) = utf8(bytestring(s))

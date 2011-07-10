## from src/boot.j:
# type UTF8String <: String; data::Array{Uint8,1}; end

## basic UTF-8 decoding & iteration ##

utf8_offset = [
    int64(0),
    int64(12416),
    int64(925824),
    int64(63447168),
    int64(4194836608),
    int64(2181570688),
]

utf8_trailing = [
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,
]

is_utf8_start(byte::Uint8) = ((byte&192)!=128)

function next(s::UTF8String, i::Index)
    if !is_utf8_start(s.data[i])
        error("invalid UTF-8 character index")
    end
    trailing = utf8_trailing[s.data[i]+1]
    if length(s.data) < i + trailing
        error("premature end of UTF-8 data")
    end
    c = 0
    for j = 1:trailing
        c += s.data[i]
        c <<= 6
        i += 1
    end
    c += s.data[i]
    i += 1
    c -= utf8_offset[trailing+1]
    char(c), i
end

## overload methods for efficiency ##

length(s::UTF8String) = length(s.data)
cmp(a::UTF8String, b::UTF8String) = lexcmp(a.data, b.data)
strchr(s::UTF8String, c::Char) =
    c < 0x80 ? memchr(s.data, c) : invoke(strchr, (String,Char), s, c)
strcat(a::ByteString, b::ByteString, c::ByteString...) = UTF8String(memcat(a,b,c...))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called
ref(s::UTF8String, r::Range1{Index}) =
    UTF8String(s.data[nextind(s,r.start):nextind(s,r.stop+1)-1])

function nextind(s::UTF8String, ind::Int)
    for i = ind:length(s)
        if is_utf8_start(s.data[i])
            return i
        end
    end
    length(s) + 1
end

function prevind(s::UTF8String, ind::Int)
    for i = ind-1:-1:1
        if is_utf8_start(s.data[i])
            return i
        end
    end
    0
end

## outputing UTF-8 strings ##

print(s::UTF8String) = print(s.data)
write(io, s::UTF8String) = write(io, s.data)

## transcoding to UTF-8 ##

utf8(s::UTF8String) = s
utf8(s::String) =
    UTF8String(print_to_string(()->for c=s; print(c); end).data)

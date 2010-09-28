## from boot.j:
# struct UTF8String <: String
#     data::Array{Uint8,1}
# end

## basic UTF-8 decoding & iteration ##

utf8_offset = [
    int64(0),
    int64(12416),
    int64(925824),
    int64(63447168),
    int64(4194836608),
    int64(2181570688),
]

utf8_encoding_bytes = [
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6,
]

is_utf8_start(byte::Uint8) = ((byte&192)!=128)

function next(s::UTF8String, i::Index)
    if !is_utf8_start(s.data[i])
        error(strcat("not a valid UTF-8 char at byte ", string(i)))
    end
    bytes = utf8_encoding_bytes[s.data[i]+1]
    if length(s.data) < i+bytes-1
        error("premature end of UTF-8 data")
    end
    c = 0
    for j = 1:bytes-1
        c += s.data[i]
        c <<= 6
        i += 1
    end
    c += s.data[i]
    i += 1
    c -= utf8_offset[bytes]
    char(c), i
end

## overload methods for efficiency ##

length(s::UTF8String) = length(s.data)

## outputing UTF-8 strings ##

print(s::UTF8String) = print(s.data)
write(io, s::UTF8String) = write(io, s.data)

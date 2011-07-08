## from src/boot.j
# type ASCIIString <: String; data::Array{Uint8,1}; end

next(s::ASCIIString, i::Index) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

length(s::ASCIIString) = length(s.data)
cmp(a::ASCIIString, b::ASCIIString) = lexcmp(a.data, b.data)
ind2chr(s::ASCIIString, i::Int) = i
chr2ind(s::ASCIIString, i::Int) = i
strchr(s::ASCIIString, c::Char) = c < 0x80 ? memchr(s.data, c) : error("char not found")
nextind(s::ASCIIString, i::Int) = i
prevind(s::ASCIIString, i::Int) = i-1
strcat(a::ASCIIString, b::ASCIIString, c::ASCIIString...)= ASCIIString(memcat(a,b,c...))

## outputing ASCII strings ##

print(s::ASCIIString) = print(s.data)
write(io, s::ASCIIString) = write(io, s.data)

## transcoding to ASCII ##

ascii(s::ASCIIString) = s
function ascii(s::String)
    f = c -> (c < 0x80) ? uint8(c) : error("invalid ASCII code point: U+$(hex(c))")
    ASCIIString(map(f, chars(s)))
end

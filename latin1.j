## from boot.j:
# struct Latin1String <: String
#     data::Array{Uint8,1}
# end

next(s::Latin1String, i::Index) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

length(s::Latin1String) = length(s.data)
strind(s::Latin1String, i::Int) = s[i]
cmp(a::Latin1String, b::Latin1String) = lexcmp(a.data, b.data)
ind2chr(s::Latin1String, i::Int) = i
chr2ind(s::Latin1String, i::Int) = i
strchr(s::Latin1String, c::Char) =
    c <= 0xff ? memchr(s.data, c) : error("char not found")

## outputing Latin-1 strings ##

print(s::Latin1String) = print(s.data)
write(io, s::Latin1String) = write(io, s.data)

## transcoding to Latin-1 ##

latin1(s::Latin1String) = s
function latin1(s::String)
    f = c -> c <= 0xff ? uint8(c) :
        error("invalid Latin-1 character: $c (0x$(uint2str(c,16)))")
    Latin1String(map(f, chars(s)))
end

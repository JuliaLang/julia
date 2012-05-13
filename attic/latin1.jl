type Latin1String <: DirectIndexString
    data::Array{Uint8,1}
end

## required core functionality ##

length(s::Latin1String) = length(s.data)
next(s::Latin1String, i::Int) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

cmp(a::Latin1String, b::Latin1String) = lexcmp(a.data, b.data)
ref(s::Latin1String, r::Range1{Int}) = Latin1String(ref(s.data,r))
strchr(s::Latin1String, c::Char) = c < 0x80 ? memchr(s.data, c) : 0
strcat(a::Latin1String, b::Latin1String, c::Latin1String...) = Latin1String(memcat(a,b,c...))

## outputing Latin-1 strings ##

print(s::Latin1String) = print(s.data)
write(io, s::Latin1String) = write(io, s.data)

## transcoding to Latin-1 ##

latin1(s::Latin1String) = s
function latin1(s::String)
    f(c) = c < 0x80 ? uint8(c) : error("invalid Latin-1 code point: U+$(hex(c))")
    Latin1String(map(f, chars(s)))
end

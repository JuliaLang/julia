## from boot.j:
# struct Latin1String <: String
#     data::Array{Uint8,1}
# end

next(s::Latin1String, i::Index) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

length(s::Latin1String) = length(s.data)
strind(s::Latin1String, i::Index) = s[i]
cmp(a::Latin1String, b::Latin1String) = lexcmp(a.data, b.data)

## outputing Latin-1 strings ##

print(s::Latin1String) = print(s.data)
write(io, s::Latin1String) = write(io, s.data)

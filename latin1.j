## from boot.j:
# struct Latin1String <: String
#     data::Array{Uint8,1}
# end

next(s::Latin1String, i::Index) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

length(s::Latin1String) = length(s.data)

libc = dlopen("libc")

function cmp(a::Latin1String, b::Latin1String)
    d = ccall(dlsym(libc,"memcmp"), Int32,
              (Ptr{Uint8}, Ptr{Uint8}, Size),
              a, b, min(length(a),length(b)))
    d < 0 ? -1 : d > 0 ? +1 : cmp(length(a),length(b))
end

## outputing Latin-1 strings ##

print(s::Latin1String) = print(s.data)
write(io, s::Latin1String) = write(io, s.data)

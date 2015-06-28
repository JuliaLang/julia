# This file is a part of Julia. License is MIT: http://julialang.org/license

## core text I/O ##

print(io::IO, x) = show(io, x)
print(io::IO, xs...) = for x in xs print(io, x) end

println(io::IO, xs...) = print(io, xs..., '\n')

print(xs...)   = print(STDOUT, xs...)
println(xs...) = println(STDOUT, xs...)

## conversion of general objects to strings ##

function print_to_string(xs...)
    # specialized for performance reasons
    s = IOBuffer(Array(UInt8,isa(xs[1],AbstractString) ? endof(xs[1]) : 0), true, true)
    for x in xs
        print(s, x)
    end
    d = s.data
    resize!(d,s.size)
    bytestring(d)
end

string(xs...) = print_to_string(xs...)
bytestring(s::AbstractString...) = print_to_string(s...)

print(io::IO, s::AbstractString) = (write(io, s); nothing)
write(io::IO, s::AbstractString) = (len = 0; for c in s; len += write(io, c); end; len)
show(io::IO, s::AbstractString) = print_quoted(io, s)

## printing literal quoted string data ##

# this is the inverse of print_unescaped_chars(io, s, "\\\")

function print_quoted_literal(io, s::AbstractString)
    print(io, '"')
    for c = s; c == '"' ? print(io, "\\\"") : print(io, c); end
    print(io, '"')
end

function repr(x)
    s = IOBuffer()
    showall(s, x)
    takebuf_string(s)
end

# IOBuffer views of a (byte)string:
IOBuffer(str::ByteString) = IOBuffer(str.data)
IOBuffer{T<:ByteString}(s::SubString{T}) = IOBuffer(sub(s.string.data, s.offset + 1 : s.offset + sizeof(s)))

# This file is a part of Julia. License is MIT: http://julialang.org/license

## uppercase and lowercase transformations ##
uppercase(s::AbstractString) = map(uppercase, s)
lowercase(s::AbstractString) = map(lowercase, s)

function ucfirst(s::AbstractString)
    isempty(s) || isupper(s[1]) ? s : string(uppercase(s[1]),s[nextind(s,1):end])
end
function lcfirst(s::AbstractString)
    isempty(s) || islower(s[1]) ? s : string(lowercase(s[1]),s[nextind(s,1):end])
end

## string map, filter, has ##

map_result(s::AbstractString, a::Vector{UInt8}) = UTF8String(a)
map_result(s::Union{ASCIIString,SubString{ASCIIString}}, a::Vector{UInt8}) = bytestring(a)

function map(f, s::AbstractString)
    out = IOBuffer(Array(UInt8,endof(s)),true,true)
    truncate(out,0)
    for c in s
        c2 = f(c)
        if !isa(c2,Char)
            throw(ArgumentError("map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"))
        end
        write(out, c2::Char)
    end
    map_result(s, takebuf_array(out))
end

function filter(f, s::AbstractString)
    out = IOBuffer(Array(UInt8,endof(s)),true,true)
    truncate(out,0)
    for c in s
        if f(c)
            write(out, c)
        end
    end
    takebuf_string(out)
end

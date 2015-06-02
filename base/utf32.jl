# This file is a part of Julia. License is MIT: http://julialang.org/license

# UTF-32 basic functions
next(s::UTF32String, i::Int) = (s.data[i], i+1)
endof(s::UTF32String) = length(s.data) - 1
length(s::UTF32String) = length(s.data) - 1

reverse(s::UTF32String) = UTF32String(reverse!(copy(s.data), 1, length(s)))

sizeof(s::UTF32String) = sizeof(s.data) - sizeof(Char)

function isvalid(::Type{UTF32String}, str::Union(Vector{Char}, Vector{UInt32}))
    for i=1:length(str)
        @inbounds if !isvalid(Char, UInt32(str[i])) ; return false ; end
    end
    return true
end
isvalid(str::Vector{Char}) = isvalid(UTF32String, str)

function map(f, s::UTF32String)
    d = s.data
    out = similar(d)
    out[end] = 0

    @inbounds for i = 1:(length(d)-1)
        c2 = f(d[i])
        if !isa(c2, Char)
            throw(ArgumentError("map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"))
        end
        out[i] = (c2::Char)
    end
    UTF32String(out)
end

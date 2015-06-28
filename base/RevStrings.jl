# This file is a part of Julia. License is MIT: http://julialang.org/license

## reversed strings without data movement ##

immutable RevString{T<:AbstractString} <: AbstractString
    string::T
end

endof(s::RevString) = endof(s.string)
length(s::RevString) = length(s.string)
sizeof(s::RevString) = sizeof(s.string)

function next(s::RevString, i::Int)
    n = endof(s); j = n-i+1
    (s.string[j], n-prevind(s.string,j)+1)
end

reverse(s::AbstractString) = RevString(s)
reverse(s::RevString) = s.string

isascii(s::RevString{ASCIIString}) = true

## reverse an index i so that reverse(s)[i] == s[reverseind(s,i)]

reverseind(s::Union{DirectIndexString,SubString{DirectIndexString}}, i::Integer) = length(s) + 1 - i
reverseind(s::RevString, i::Integer) = endof(s) - i + 1
lastidx(s::AbstractString) = nextind(s, endof(s)) - 1
lastidx(s::DirectIndexString) = length(s)
reverseind(s::SubString, i::Integer) =
    reverseind(s.string, lastidx(s.string)-s.offset-s.endof+i) - s.offset

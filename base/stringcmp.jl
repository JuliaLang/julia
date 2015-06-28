# This file is a part of Julia. License is MIT: http://julialang.org/license

## String comparision functions ##

function cmp(a::AbstractString, b::AbstractString)
    if a === b
        return 0
    end
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d
            return c < d ? -1 : +1
        end
    end
    done(a,i) && !done(b,j) ? -1 :
    !done(a,i) && done(b,j) ? +1 : 0
end

==(a::AbstractString, b::AbstractString) = cmp(a,b) == 0
isless(a::AbstractString, b::AbstractString) = cmp(a,b) < 0

# starts with and ends with predicates

function startswith(a::AbstractString, b::AbstractString)
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d return false end
    end
    done(b,i)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && str[start(str)] in chars

function endswith(a::AbstractString, b::AbstractString)
    i = endof(a)
    j = endof(b)
    a1 = start(a)
    b1 = start(b)
    while a1 <= i && b1 <= j
        c = a[i]
        d = b[j]
        if c != d return false end
        i = prevind(a,i)
        j = prevind(b,j)
    end
    j < b1
end
endswith(str::AbstractString, chars::Chars) = !isempty(str) && str[end] in chars

# faster comparisons for byte strings and symbols

cmp(a::ByteString, b::ByteString) = lexcmp(a.data, b.data)
cmp(a::Symbol, b::Symbol) = Int(sign(ccall(:strcmp, Int32, (Cstring, Cstring), a, b)))

==(a::ByteString, b::ByteString) = endof(a) == endof(b) && cmp(a,b) == 0
isless(a::Symbol, b::Symbol) = cmp(a,b) < 0

startswith(a::ByteString, b::ByteString) = startswith(a.data, b.data)
startswith(a::Vector{UInt8}, b::Vector{UInt8}) =
    (length(a) >= length(b) && ccall(:strncmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, length(b)) == 0)

# TODO: fast endswith

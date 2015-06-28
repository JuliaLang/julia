# This file is a part of Julia. License is MIT: http://julialang.org/license

## efficient representation of repeated strings ##

immutable RepString <: AbstractString
    string::AbstractString
    repeat::Integer
end

function endof(s::RepString)
    e = endof(s.string)
    (next(s.string,e)[2]-1) * (s.repeat-1) + e
end
length(s::RepString) = length(s.string)*s.repeat
sizeof(s::RepString) = sizeof(s.string)*s.repeat

function next(s::RepString, i::Int)
    if i < 1
        throw(BoundsError(s, i))
    end
    e = endof(s.string)
    sz = next(s.string,e)[2]-1

    r, j = divrem(i-1, sz)
    j += 1

    if r >= s.repeat || j > e
        throw(BoundsError(s, i))
    end

    c, k = next(s.string, j)
    c, k-j+i
end

function repeat(s::AbstractString, r::Integer)
    r <  0 ? throw(ArgumentError("can't repeat a string $r times")) :
    r == 0 ? "" :
    r == 1 ? s  :
    RepString(s,r)
end

convert(::Type{RepString}, s::AbstractString) = RepString(s,1)

function repeat(s::ByteString, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    d = s.data; n = length(d)
    out = Array(UInt8, n*r)
    for i=1:r
        copy!(out, 1+(i-1)*n, d, 1, n)
    end
    convert(typeof(s), out)
end

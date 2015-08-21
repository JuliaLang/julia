# This file is a part of Julia. License is MIT: http://julialang.org/license

# SubString, RevString, RepString, and RopeString types

## substrings reference original strings ##

immutable SubString{T<:AbstractString} <: AbstractString
    string::T
    offset::Int
    endof::Int

    function SubString(s::T, i::Int, j::Int)
        if i > endof(s) || j<i
            return new(s, i-1, 0)
        else
            if !isvalid(s,i)
                throw(ArgumentError("invalid SubString index"))
            end

            while !isvalid(s,j) && j > i
                j -= 1
            end

            o = i-1
            new(s, o, max(0, j-o))
        end
    end
end
SubString{T<:AbstractString}(s::T, i::Int, j::Int) = SubString{T}(s, i, j)
SubString(s::SubString, i::Int, j::Int) = SubString(s.string, s.offset+i, s.offset+j)
SubString(s::AbstractString, i::Integer, j::Integer) = SubString(s, Int(i), Int(j))
SubString(s::AbstractString, i::Integer) = SubString(s, i, endof(s))

sizeof(s::SubString{UTF8String}) = s.endof == 0 ? 0 : nextind(s, s.endof) - 1

# TODO: length(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces
length{T<:DirectIndexString}(s::SubString{T}) = endof(s)

function length(s::SubString{UTF8String})
    return s.endof==0 ? 0 : Int(ccall(:u8_charnum, Csize_t, (Ptr{UInt8}, Csize_t),
                                      pointer(s), nextind(s, s.endof) - 1))
end

function next(s::SubString, i::Int)
    if i < 1 || i > s.endof
        throw(BoundsError(s, i))
    end
    c, i = next(s.string, i+s.offset)
    c, i-s.offset
end

function getindex(s::SubString, i::Int)
    if i < 1 || i > s.endof
        throw(BoundsError(s, i))
    end
    getindex(s.string, i+s.offset)
end

endof(s::SubString) = s.endof

function isvalid(s::SubString, i::Integer)
    return (start(s) <= i <= endof(s)) && isvalid(s.string, s.offset+i)
end

isvalid{T<:DirectIndexString}(s::SubString{T}, i::Integer) = (start(s) <= i <= endof(s))

ind2chr{T<:DirectIndexString}(s::SubString{T}, i::Integer) = begin checkbounds(s,i); i end
chr2ind{T<:DirectIndexString}(s::SubString{T}, i::Integer) = begin checkbounds(s,i); i end

nextind(s::SubString, i::Integer) = nextind(s.string, i+s.offset)-s.offset
prevind(s::SubString, i::Integer) = prevind(s.string, i+s.offset)-s.offset

convert{T<:AbstractString}(::Type{SubString{T}}, s::T) = SubString(s, 1, endof(s))

bytestring{T <: ByteString}(p::SubString{T}) = bytestring(p.string.data[1+p.offset:p.offset+nextind(p, p.endof)-1])

function getindex(s::AbstractString, r::UnitRange{Int})
    if first(r) < 1 || endof(s) < last(r)
        throw(BoundsError(s, r))
    end
    SubString(s, first(r), last(r))
end

function cmp{T<:ByteString,S<:ByteString}(a::SubString{T}, b::SubString{S})
    na = sizeof(a)
    nb = sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              pointer(a), pointer(b), min(na,nb))
    c < 0 ? -1 : c > 0 ? +1 : cmp(na,nb)
end

# don't make unnecessary copies when passing substrings to C functions
cconvert{T<:ByteString}(::Type{Ptr{UInt8}}, s::SubString{T}) = s
cconvert{T<:ByteString}(::Type{Ptr{Int8}}, s::SubString{T}) = s
function unsafe_convert{T<:ByteString, R<:Union{Int8, UInt8}}(::Type{Ptr{R}}, s::SubString{T})
    unsafe_convert(Ptr{R}, s.string.data) + s.offset
end

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

## reverse an index i so that reverse(s)[i] == s[reverseind(s,i)]

reverseind(s::Union{DirectIndexString,SubString{DirectIndexString}}, i::Integer) = length(s) + 1 - i
reverseind(s::RevString, i::Integer) = endof(s) - i + 1
lastidx(s::AbstractString) = nextind(s, endof(s)) - 1
lastidx(s::DirectIndexString) = length(s)
reverseind(s::SubString, i::Integer) =
    reverseind(s.string, lastidx(s.string)-s.offset-s.endof+i) - s.offset

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

(^)(s::AbstractString, r::Integer) = repeat(s,r)

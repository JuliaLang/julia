# This file is a part of Julia. License is MIT: http://julialang.org/license

# SubString and RevString types

## substrings reference original strings ##

immutable SubString{T<:AbstractString} <: AbstractString
    string::T
    offset::Int
    endof::Int

    function SubString{T}(s::T, i::Int, j::Int) where T<:AbstractString
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
SubString(s::T, i::Int, j::Int) where T<:AbstractString = SubString{T}(s, i, j)
SubString(s::SubString, i::Int, j::Int) = SubString(s.string, s.offset+i, s.offset+j)
SubString(s::AbstractString, i::Integer, j::Integer) = SubString(s, Int(i), Int(j))
SubString(s::AbstractString, i::Integer) = SubString(s, i, endof(s))

sizeof(s::SubString{String}) = s.endof == 0 ? 0 : nextind(s, s.endof) - 1

# TODO: length(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces
length{T<:DirectIndexString}(s::SubString{T}) = endof(s)

function length(s::SubString{String})
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

String(p::SubString{String}) =
    unsafe_string(pointer(p.string, p.offset+1), nextind(p, p.endof)-1)

function getindex(s::AbstractString, r::UnitRange{Int})
    checkbounds(s, r) || throw(BoundsError(s, r))
    SubString(s, first(r), last(r))
end

function cmp(a::SubString{String}, b::SubString{String})
    na = sizeof(a)
    nb = sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              pointer(a), pointer(b), min(na,nb))
    c < 0 ? -1 : c > 0 ? +1 : cmp(na,nb)
end

# don't make unnecessary copies when passing substrings to C functions
cconvert(::Type{Ptr{UInt8}}, s::SubString{String}) = s
cconvert(::Type{Ptr{Int8}}, s::SubString{String}) = s
function unsafe_convert{R<:Union{Int8, UInt8}}(::Type{Ptr{R}}, s::SubString{String})
    convert(Ptr{R}, pointer(s.string)) + s.offset
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

"""
    reverse(s::AbstractString) -> AbstractString

Reverses a string.
```jldoctest
julia> reverse("JuliaLang")
"gnaLailuJ"
```
"""
reverse(s::AbstractString) = RevString(s)
reverse(s::RevString) = s.string

## reverse an index i so that reverse(s)[i] == s[reverseind(s,i)]

reverseind(s::AbstractString, i) = chr2ind(s, length(s) + 1 - ind2chr(reverse(s), i))
reverseind(s::Union{DirectIndexString,SubString{DirectIndexString}}, i::Integer) = length(s) + 1 - i
reverseind(s::RevString, i::Integer) = endof(s) - i + 1
reverseind(s::SubString{String}, i::Integer) =
    reverseind(s.string, nextind(s.string, endof(s.string))-s.offset-s.endof+i-1) - s.offset

function repeat(s::AbstractString, r::Integer)
    r <  0 ? throw(ArgumentError("can't repeat a string $r times")) :
    r == 0 ? "" :
    r == 1 ? s  :
    repeat(convert(String, s), r)
end

"""
    ^(s::AbstractString, n::Integer)

Repeat `n` times the string `s`.
The [`repeat`](@ref) function is an alias to this operator.

```jldoctest
julia> "Test "^3
"Test Test Test "
```
"""
(^)(s::AbstractString, r::Integer) = repeat(s,r)

pointer(x::SubString{String}) = pointer(x.string) + x.offset
pointer(x::SubString{String}, i::Integer) = pointer(x.string) + x.offset + (i-1)

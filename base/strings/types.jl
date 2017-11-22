# This file is a part of Julia. License is MIT: https://julialang.org/license

# SubString and RevString types

## substrings reference original strings ##

"""
    SubString(s::AbstractString, i::Integer, j::Integer=endof(s))
    SubString(s::AbstractString, r::UnitRange{<:Integer})

Like [`getindex`](@ref), but returns a view into the parent string `s`
within range `i:j` or `r` respectively instead of making a copy.

# Examples
```jldoctest
julia> SubString("abc", 1, 2)
"ab"

julia> SubString("abc", 1:2)
"ab"

julia> SubString("abc", 2)
"bc"
```
"""
struct SubString{T<:AbstractString} <: AbstractString
    string::T
    offset::Int
    endof::Int

    function SubString{T}(s::T, i::Int, j::Int) where T<:AbstractString
        i > j && return new(s, i - 1, 0) # always allow i > j as it is consistent with getindex
        isvalid(s, i) || throw(BoundsError(s, i))
        isvalid(s, j) || throw(BoundsError(s, j))
        new(s, i-1, j-i+1)
    end
end

SubString(s::T, i::Int, j::Int) where {T<:AbstractString} = SubString{T}(s, i, j)
SubString(s::AbstractString, i::Integer, j::Integer=endof(s)) = SubString(s, Int(i), Int(j))
SubString(s::AbstractString, r::UnitRange{<:Integer}) = SubString(s, first(r), last(r))

function SubString(s::SubString, i::Int, j::Int)
    # always allow i > j as it is consistent with getindex
    i > j && return SubString(s.string, s.offset + i, s.offset + j)
    i >= 1 || throw(BoundsError(s, i))
    j <= endof(s) || throw(BoundsError(s, j))
    SubString(s.string, s.offset + i, s.offset + j)
end

SubString(s::AbstractString) = SubString(s, 1, endof(s))
SubString{T}(s::T) where {T<:AbstractString} = SubString{T}(s, 1, endof(s))

String(p::SubString{String}) =
    unsafe_string(pointer(p.string, p.offset+1), nextind(p, p.endof)-1)

sizeof(s::SubString{String}) = s.endof == 0 ? 0 : nextind(s, s.endof) - 1

# TODO: length(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces
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

function thisind(s::SubString{String}, i::Integer)
    j = Int(i)
    j < start(s) && return 0
    n = ncodeunits(s)
    j > n && return n + 1
    offset = s.offset
    str = s.string
    j += offset
    @inbounds while j > offset && is_valid_continuation(codeunit(str, j))
        j -= 1
    end
    j - offset
end

nextind(s::SubString, i::Integer) = nextind(s.string, i+s.offset)-s.offset
prevind(s::SubString, i::Integer) = prevind(s.string, i+s.offset)-s.offset

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
function unsafe_convert(::Type{Ptr{R}}, s::SubString{String}) where R<:Union{Int8, UInt8}
    convert(Ptr{R}, pointer(s.string)) + s.offset
end

## reversed strings without data movement ##

struct RevString{T<:AbstractString} <: AbstractString
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

Technically, this function reverses the codepoints in a string, and its
main utility is for reversed-order string processing, especially for reversed
regular-expression searches.  See also [`reverseind`](@ref) to convert indices
in `s` to indices in `reverse(s)` and vice-versa, and [`graphemes`](@ref)
to operate on user-visible "characters" (graphemes) rather than codepoints.
See also [`Iterators.reverse`](@ref) for reverse-order iteration without making a copy.

# Examples
```jldoctest
julia> reverse("JuliaLang")
"gnaLailuJ"

julia> reverse("ax̂e") # combining characters can lead to surprising results
"êxa"

julia> join(reverse(collect(graphemes("ax̂e")))) # reverses graphemes
"ex̂a"
```
"""
reverse(s::AbstractString) = RevString(s)
reverse(s::RevString) = s.string

## reverse an index i so that reverse(s)[i] == s[reverseind(s,i)]

"""
    reverseind(v, i)

Given an index `i` in [`reverse(v)`](@ref), return the corresponding index in `v` so that
`v[reverseind(v,i)] == reverse(v)[i]`. (This can be nontrivial in cases where `v` contains
non-ASCII characters.)

# Examples
```jldoctest
julia> r = reverse("Julia")
"ailuJ"

julia> for i in 1:length(r)
           print(r[reverseind("Julia", i)])
       end
Julia
```
"""
reverseind(s::AbstractString, i) = chr2ind(s, length(s) + 1 - ind2chr(reverse(s), i))
reverseind(s::RevString, i::Integer) = endof(s) - i + 1
reverseind(s::SubString{String}, i::Integer) =
    reverseind(s.string, nextind(s.string, endof(s.string))-s.offset-s.endof+i-1) - s.offset

"""
    repeat(s::AbstractString, r::Integer)

Repeat a string `r` times. This can equivalently be accomplished by calling [`s^r`](@ref ^).

# Examples
```jldoctest
julia> repeat("ha", 3)
"hahaha"
```
"""
repeat(s::AbstractString, r::Integer) = repeat(convert(String, s), r)

"""
    ^(s::Union{AbstractString,Char}, n::Integer)

Repeat a string or character `n` times.
The [`repeat`](@ref) function is an alias to this operator.

# Examples
```jldoctest
julia> "Test "^3
"Test Test Test "
```
"""
(^)(s::Union{AbstractString,Char}, r::Integer) = repeat(s,r)

pointer(x::SubString{String}) = pointer(x.string) + x.offset
pointer(x::SubString{String}, i::Integer) = pointer(x.string) + x.offset + (i-1)

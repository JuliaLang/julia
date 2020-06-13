# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    SubString(s::AbstractString, i::Integer, j::Integer=lastindex(s))
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
    ncodeunits::Int

    function SubString{T}(s::T, i::Int, j::Int) where T<:AbstractString
        i ≤ j || return new(s, 0, 0)
        @boundscheck begin
            checkbounds(s, i:j)
            @inbounds isvalid(s, i) || string_index_err(s, i)
            @inbounds isvalid(s, j) || string_index_err(s, j)
        end
        return new(s, i-1, nextind(s,j)-i)
    end
end

@propagate_inbounds SubString(s::T, i::Int, j::Int) where {T<:AbstractString} = SubString{T}(s, i, j)
@propagate_inbounds SubString(s::AbstractString, i::Integer, j::Integer=lastindex(s)) = SubString(s, Int(i), Int(j))
@propagate_inbounds SubString(s::AbstractString, r::AbstractUnitRange{<:Integer}) = SubString(s, first(r), last(r))

@propagate_inbounds function SubString(s::SubString, i::Int, j::Int)
    @boundscheck i ≤ j && checkbounds(s, i:j)
    SubString(s.string, s.offset+i, s.offset+j)
end

SubString(s::AbstractString) = SubString(s, 1, lastindex(s))
SubString{T}(s::T) where {T<:AbstractString} = SubString{T}(s, 1, lastindex(s))

@propagate_inbounds view(s::AbstractString, r::AbstractUnitRange{<:Integer}) = SubString(s, r)
@propagate_inbounds maybeview(s::AbstractString, r::AbstractUnitRange{<:Integer}) = view(s, r)
@propagate_inbounds maybeview(s::AbstractString, args...) = getindex(s, args...)

convert(::Type{SubString{S}}, s::AbstractString) where {S<:AbstractString} =
    SubString(convert(S, s))
convert(::Type{T}, s::T) where {T<:SubString} = s

function String(s::SubString{String})
    parent = s.string
    copy = GC.@preserve parent unsafe_string(pointer(parent, s.offset+1), s.ncodeunits)
    return copy
end

ncodeunits(s::SubString) = s.ncodeunits
codeunit(s::SubString) = codeunit(s.string)
length(s::SubString) = length(s.string, s.offset+1, s.offset+s.ncodeunits)

function codeunit(s::SubString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds return codeunit(s.string, s.offset + i)
end

function iterate(s::SubString, i::Integer=firstindex(s))
    i == ncodeunits(s)+1 && return nothing
    @boundscheck checkbounds(s, i)
    y = iterate(s.string, s.offset + i)
    y === nothing && return nothing
    c, i = y
    return c, i - s.offset
end

function getindex(s::SubString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds return getindex(s.string, s.offset + i)
end

function isvalid(s::SubString, i::Integer)
    ib = true
    @boundscheck ib = checkbounds(Bool, s, i)
    @inbounds return ib && isvalid(s.string, s.offset + i)
end

byte_string_classify(s::SubString{String}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), s, sizeof(s))

isvalid(::Type{String}, s::SubString{String}) = byte_string_classify(s) ≠ 0
isvalid(s::SubString{String}) = isvalid(String, s)

thisind(s::SubString{String}, i::Int) = _thisind_str(s, i)
nextind(s::SubString{String}, i::Int) = _nextind_str(s, i)

function ==(a::Union{String, SubString{String}}, b::Union{String, SubString{String}})
    s = sizeof(a)
    s == sizeof(b) && 0 == _memcmp(a, b, s)
end

function cmp(a::SubString{String}, b::SubString{String})
    na = sizeof(a)
    nb = sizeof(b)
    c = _memcmp(a, b, min(na, nb))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(na, nb)
end

# don't make unnecessary copies when passing substrings to C functions
cconvert(::Type{Ptr{UInt8}}, s::SubString{String}) = s
cconvert(::Type{Ptr{Int8}}, s::SubString{String}) = s

function unsafe_convert(::Type{Ptr{R}}, s::SubString{String}) where R<:Union{Int8, UInt8}
    convert(Ptr{R}, pointer(s.string)) + s.offset
end

pointer(x::SubString{String}) = pointer(x.string) + x.offset
pointer(x::SubString{String}, i::Integer) = pointer(x.string) + x.offset + (i-1)

"""
    reverse(s::AbstractString) -> AbstractString

Reverses a string. Technically, this function reverses the codepoints in a string and its
main utility is for reversed-order string processing, especially for reversed
regular-expression searches. See also [`reverseind`](@ref) to convert indices in `s` to
indices in `reverse(s)` and vice-versa, and `graphemes` from module `Unicode` to
operate on user-visible "characters" (graphemes) rather than codepoints.
See also [`Iterators.reverse`](@ref) for
reverse-order iteration without making a copy. Custom string types must implement the
`reverse` function themselves and should typically return a string with the same type
and encoding. If they return a string with a different encoding, they must also override
`reverseind` for that string type to satisfy `s[reverseind(s,i)] == reverse(s)[i]`.

# Examples
```jldoctest
julia> reverse("JuliaLang")
"gnaLailuJ"
```

!!! note
    The examples below may be rendered differently on different systems.
    The comments indicate how they're supposed to be rendered

Combining characters can lead to surprising results:

```jldoctest
julia> reverse("ax̂e") # hat is above x in the input, above e in the output
"êxa"

julia> using Unicode

julia> join(reverse(collect(graphemes("ax̂e")))) # reverses graphemes; hat is above x in both in- and output
"ex̂a"
```
"""
function reverse(s::Union{String,SubString{String}})::String
    # Read characters forwards from `s` and write backwards to `out`
    out = _string_n(sizeof(s))
    offs = sizeof(s) + 1
    for c in s
        offs -= ncodeunits(c)
        __unsafe_string!(out, c, offs)
    end
    return out
end

string(a::String)            = String(a)
string(a::SubString{String}) = String(a)

@inline function __unsafe_string!(out, c::Char, offs::Integer) # out is a (new) String (or StringVector)
    x = bswap(reinterpret(UInt32, c))
    n = ncodeunits(c)
    GC.@preserve out begin
        unsafe_store!(pointer(out, offs), x % UInt8)
        n == 1 && return n
        x >>= 8
        unsafe_store!(pointer(out, offs+1), x % UInt8)
        n == 2 && return n
        x >>= 8
        unsafe_store!(pointer(out, offs+2), x % UInt8)
        n == 3 && return n
        x >>= 8
        unsafe_store!(pointer(out, offs+3), x % UInt8)
    end
    return n
end

@inline function __unsafe_string!(out, s::Union{String, SubString{String}}, offs::Integer)
    n = sizeof(s)
    GC.@preserve s out unsafe_copyto!(pointer(out, offs), pointer(s), n)
    return n
end

function string(a::Union{Char, String, SubString{String}}...)
    n = 0
    for v in a
        if v isa Char
            n += ncodeunits(v)
        else
            n += sizeof(v)
        end
    end
    out = _string_n(n)
    offs = 1
    for v in a
        offs += __unsafe_string!(out, v, offs)
    end
    return out
end

function repeat(s::Union{String, SubString{String}}, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    r == 0 && return ""
    r == 1 && return String(s)
    n = sizeof(s)
    out = _string_n(n*r)
    if n == 1 # common case: repeating a single-byte string
        @inbounds b = codeunit(s, 1)
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), out, b, r)
    else
        for i = 0:r-1
            GC.@preserve s out unsafe_copyto!(pointer(out, i*n+1), pointer(s), n)
        end
    end
    return out
end

function filter(f, s::Union{String, SubString{String}})
    out = StringVector(sizeof(s))
    offset = 1
    for c in s
        if f(c)
            offset += __unsafe_string!(out, c, offset)
        end
    end
    resize!(out, offset-1)
    sizehint!(out, offset-1)
    return String(out)
end

getindex(s::AbstractString, r::UnitRange{<:Integer}) = SubString(s, r)

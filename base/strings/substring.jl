# This file is a part of Julia. License is MIT: https://julialang.org/license

# SubString type

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

reverseind(s::SubString{String}, i::Integer) =
    reverseind(s.string, nextind(s.string, endof(s.string))-s.offset-s.endof+i-1) - s.offset

pointer(x::SubString{String}) = pointer(x.string) + x.offset
pointer(x::SubString{String}, i::Integer) = pointer(x.string) + x.offset + (i-1)

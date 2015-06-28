# This file is a part of Julia. License is MIT: http://julialang.org/license

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

write{T<:ByteString}(to::AbstractIOBuffer, s::SubString{T}) =
    s.endof==0 ? 0 : write_sub(to, s.string.data, s.offset + 1, nextind(s, s.endof) - 1)

sizeof(s::SubString{ASCIIString}) = s.endof
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

isascii(s::SubString{ASCIIString}) = true

function cmp{T<:ByteString,S<:ByteString}(a::SubString{T}, b::SubString{S})
    na = sizeof(a)
    nb = sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              pointer(a), pointer(b), min(na,nb))
    c < 0 ? -1 : c > 0 ? +1 : cmp(na,nb)
end

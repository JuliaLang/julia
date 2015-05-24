# This file is a part of Julia. License is MIT: http://julialang.org/license

## from base/boot.jl:
#
# immutable ASCIIString <: DirectIndexString
#     data::Array{UInt8,1}
# end
#

## required core functionality ##

endof(s::ASCIIString) = length(s.data)
getindex(s::ASCIIString, i::Int) = (x=s.data[i]; x < 0x80 ? Char(x) : '\ufffd')

## overload methods for efficiency ##

sizeof(s::ASCIIString) = sizeof(s.data)

getindex(s::ASCIIString, r::Vector) = ASCIIString(getindex(s.data,r))
getindex(s::ASCIIString, r::UnitRange{Int}) = ASCIIString(getindex(s.data,r))
getindex(s::ASCIIString, indx::AbstractVector{Int}) = ASCIIString(s.data[indx])
search(s::ASCIIString, c::Char, i::Integer) = c < Char(0x80) ? search(s.data,c%UInt8,i) : 0
rsearch(s::ASCIIString, c::Char, i::Integer) = c < Char(0x80) ? rsearch(s.data,c%UInt8,i) : 0

function string(c::ASCIIString...)
    if length(c) == 1
        return c[1]
    end
    n = 0
    for s in c
        n += length(s.data)
    end
    v = Array(UInt8,n)
    o = 1
    for s in c
        ls = length(s.data)
        unsafe_copy!(v, o, s.data, 1, ls)
        o += ls
    end
    ASCIIString(v)
end

function ucfirst(s::ASCIIString)
    if length(s) > 0 && 'a' <= s[1] <= 'z'
        t = ASCIIString(copy(s.data))
        t.data[1] -= 32
        return t
    end
    return s
end
function lcfirst(s::ASCIIString)
    if length(s) > 0 && 'A' <= s[1] <= 'Z'
        t = ASCIIString(copy(s.data))
        t.data[1] += 32
        return t
    end
    return s
end

function uppercase(s::ASCIIString)
    d = s.data
    for i = 1:length(d)
        if 'a' <= Char(d[i]) <= 'z'
            td = copy(d)
            for j = i:length(td)
                if 'a' <= Char(td[j]) <= 'z'
                    td[j] -= 32
                end
            end
            return ASCIIString(td)
        end
    end
    return s
end
function lowercase(s::ASCIIString)
    d = s.data
    for i = 1:length(d)
        if 'A' <= Char(d[i]) <= 'Z'
            td = copy(d)
            for j = i:length(td)
                if 'A' <= Char(td[j]) <= 'Z'
                    td[j] += 32
                end
            end
            return ASCIIString(td)
        end
    end
    return s
end

reverse(s::ASCIIString) = ASCIIString(reverse(s.data))

## outputing ASCII strings ##

write(io::IO, s::ASCIIString) = write(io, s.data)

## transcoding to ASCII ##

ascii(x) = convert(ASCIIString, x)
convert(::Type{ASCIIString}, s::ASCIIString) = s
convert(::Type{ASCIIString}, s::UTF8String) = ascii(s.data)
convert(::Type{ASCIIString}, a::Vector{UInt8}) = begin
    isvalid(ASCIIString,a) || throw(ArgumentError("invalid ASCII sequence"))
    return ASCIIString(a)
end

ascii(p::Ptr{UInt8}) = ASCIIString(bytestring(p))
ascii(p::Ptr{UInt8}, len::Integer) = ascii(pointer_to_array(p, len))

function convert(::Type{ASCIIString}, a::Array{UInt8,1}, invalids_as::ASCIIString)
    l = length(a)
    idx = 1
    iscopy = false
    while idx <= l
        (a[idx] < 0x80) && (idx +=1; continue)
        !iscopy && (a = copy(a); iscopy = true)
        endn = idx
        while endn <= l
            (a[endn] < 0x80) && break
            endn += 1
        end
        (endn > idx) && (endn -= 1)
        splice!(a, idx:endn, invalids_as.data)
        l = length(a)
    end
    convert(ASCIIString, a)
end
convert(::Type{ASCIIString}, s::AbstractString) = ascii(bytestring(s))

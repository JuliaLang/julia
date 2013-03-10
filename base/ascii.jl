## from base/boot.jl:
#
# immutable ASCIIString <: DirectIndexString
#     data::Array{Uint8,1}
# end
#

## required core functionality ##

endof(s::ASCIIString) = length(s.data)
getindex(s::ASCIIString, i::Int) = (x=s.data[i]; x < 0x80 ? char(x) : '\ufffd')

## overload methods for efficiency ##

getindex(s::ASCIIString, r::Vector) = ASCIIString(getindex(s.data,r))
getindex(s::ASCIIString, r::Range1{Int}) = ASCIIString(getindex(s.data,r))
getindex(s::ASCIIString, indx::AbstractVector{Int}) = ASCIIString(s.data[indx])
search(s::ASCIIString, c::Char, i::Integer) = c < 0x80 ? search(s.data,c,i) : 0
string(a::ASCIIString, b::ASCIIString, c::ASCIIString...) =
    ASCIIString([a.data,b.data,map(s->s.data,c)...])

function ucfirst(s::ASCIIString)
    if 'a' <= s[1] <= 'z'
        t = ASCIIString(copy(s.data))
        t.data[1] -= 32
        return t
    end
    return s
end
function lcfirst(s::ASCIIString)
    if 'A' <= s[1] <= 'Z'
        t = ASCIIString(copy(s.data))
        t.data[1] += 32
        return t
    end
    return s
end

function uppercase(s::ASCIIString)
    d = s.data
    for i = 1:length(d)
        if 'a' <= d[i] <= 'z'
            td = copy(d)
            for j = i:length(td)
                if 'a' <= td[j] <= 'z'
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
        if 'A' <= d[i] <= 'Z'
            td = copy(d)
            for j = i:length(td)
                if 'A' <= td[j] <= 'Z'
                    td[j] += 32
                end
            end
            return ASCIIString(td)
        end
    end
    return s
end

## outputing ASCII strings ##

print(io::IO, s::ASCIIString) = (write(io, s);nothing)
write(io::IO, s::ASCIIString) = write(io, s.data)

## transcoding to ASCII ##

ascii(x) = convert(ASCIIString, x)
convert(::Type{ASCIIString}, s::ASCIIString) = s
convert(::Type{ASCIIString}, s::UTF8String) = ascii(s.data)
convert(::Type{ASCIIString}, a::Array{Uint8,1}) = check_ascii(ASCIIString(a))
convert(::Type{ASCIIString}, s::String) = ascii(bytestring(s))

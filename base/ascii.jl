## from base/boot.jl:
#
# type ASCIIString <: DirectIndexString
#     data::Array{Uint8,1}
# end
#

## required core functionality ##

length(s::ASCIIString) = length(s.data)
ref(s::ASCIIString, i::Int) = char(s.data[i])

## overload methods for efficiency ##

ref(s::ASCIIString, r::Vector) = ASCIIString(ref(s.data,r))
ref(s::ASCIIString, r::Range1{Int}) = ASCIIString(ref(s.data,r))
ref(s::ASCIIString, indx::AbstractVector{Int}) = ASCIIString(s.data[indx])
strchr(s::ASCIIString, c::Char, i::Integer) = c < 0x80 ? memchr(s.data,c,i) : 0
strcat(a::ASCIIString, b::ASCIIString, c::ASCIIString...) =
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
    for i = 1:length(s)
        if 'a' <= s[i] <= 'z'
            t = ASCIIString(copy(s.data))
            while i <= length(t)
                if 'a' <= t[i] <= 'z'
                    t.data[i] -= 32
                end
                i += 1
            end
            return t
        end
    end
    return s
end
function lowercase(s::ASCIIString)
    for i = 1:length(s)
        if 'A' <= s[i] <= 'Z'
            t = ASCIIString(copy(s.data))
            while i <= length(t)
                if 'A' <= t[i] <= 'Z'
                    t.data[i] += 32
                end
                i += 1
            end
            return t
        end
    end
    return s
end

## outputing ASCII strings ##

print(io::IO, s::ASCIIString) = (write(io, s.data);nothing)
write(io::IO, s::ASCIIString) = write(io, s.data)

## transcoding to ASCII ##

ascii(x) = convert(ASCIIString, x)
convert(::Type{ASCIIString}, s::ASCIIString) = s
convert(::Type{ASCIIString}, s::UTF8String) = ascii(s.data)
convert(::Type{ASCIIString}, a::Array{Uint8,1}) = check_ascii(ASCIIString(a))
convert(::Type{ASCIIString}, s::String) = ascii(bytestring(s))

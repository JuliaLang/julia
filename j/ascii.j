## from src/boot.j
# type ASCIIString <: String; data::Array{Uint8,1}; end

next(s::ASCIIString, i::Index) = (char(s.data[i]), i+1)

## overload methods for efficiency ##

length(s::ASCIIString) = length(s.data)
cmp(a::ASCIIString, b::ASCIIString) = lexcmp(a.data, b.data)
ind2chr(s::ASCIIString, i::Int) = i
chr2ind(s::ASCIIString, i::Int) = i
strchr(s::ASCIIString, c::Char) = c < 0x80 ? memchr(s.data, c) : error("char not found")
nextind(s::ASCIIString, i::Int) = i
prevind(s::ASCIIString, i::Int) = i-1
strcat(a::ASCIIString, b::ASCIIString, c::ASCIIString...) = ASCIIString(memcat(a,b,c...))
ref(s::ASCIIString, r::Range1{Index}) = ASCIIString(ref(s.data,r))
# TODO: fast has(str,c) implementation

function ucfirst(s::ASCIIString)
    if 'a' <= s[1] <= 'z'
        t = strcpy(s)
        t.data[1] -= 32
        return t
    end
    return s
end
function lcfirst(s::ASCIIString)
    if 'A' <= s[1] <= 'Z'
        t = strcpy(s)
        t.data[1] += 32
        return t
    end
    return s
end

function uc(s::ASCIIString)
    for i = 1:length(s)
        if 'a' <= s[i] <= 'z'
            t = strcpy(s)
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
function lc(s::ASCIIString)
    for i = 1:length(s)
        if 'A' <= s[i] <= 'Z'
            t = strcpy(s)
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

print(s::ASCIIString) = print(s.data)
write(io, s::ASCIIString) = write(io, s.data)

## transcoding to ASCII ##

ascii(s::ASCIIString) = s
ascii(a::Array{Uint8,1}) = check_ascii(ASCIIString(a))
ascii(s::UTF8String) = ascii(s.data)
ascii(s::String) = ascii(cstring(s))

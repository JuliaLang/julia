## core text I/O ##

print(io::IO, x) = show(io, x)
print(io::IO, xs...) = for x in xs print(io, x) end
println(io::IO, xs...) = print(io, xs..., '\n')

print(xs...)   = print(STDOUT, xs...)
println(xs...) = println(STDOUT, xs...)

## core string functions ##

endof(s::String) = error("you must implement endof(", typeof(s), ")")
next(s::String, i::Int) = error("you must implement next(", typeof(s), ",Int)")
next(s::DirectIndexString, i::Int) = (s[i],i+1)
next(s::String, i::Integer) = next(s,int(i))

## conversion of general objects to strings ##

function print_to_string(xs...)
    # specialized for performance reasons
    s = IOBuffer(Array(Uint8,isa(xs[1],String) ? endof(xs[1]) : 0), true, true)
    truncate(s,0)
    for x in xs
        print(s, x)
    end
    takebuf_string(s)
end

string() = ""
string(s::String) = s
string(xs...) = print_to_string(xs...)

bytestring() = ""
bytestring(s::Array{Uint8,1}) = bytestring(pointer(s),length(s))
bytestring(s::String...) = print_to_string(s...)

function bytestring(p::Union(Ptr{Uint8},Ptr{Int8}))
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_cstr_to_string, ByteString, (Ptr{Uint8},), p)
end

function bytestring(p::Union(Ptr{Uint8},Ptr{Int8}),len::Integer)
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_pchar_to_string, ByteString, (Ptr{Uint8},Int), p, len)
end

convert(::Type{Array{Uint8,1}}, s::String) = bytestring(s).data
convert(::Type{Array{Uint8}}, s::String) = bytestring(s).data
convert(::Type{ByteString}, s::String) = bytestring(s)
convert(::Type{Array{Char,1}}, s::String) = collect(s)
convert(::Type{Symbol}, s::String) = symbol(s)

## generic supplied functions ##

start(s::String) = 1
done(s::String,i) = (i > endof(s))
getindex(s::String, i::Int) = next(s,i)[1]
getindex(s::String, i::Integer) = s[int(i)]
getindex(s::String, x::Real) = s[to_index(x)]
getindex{T<:Integer}(s::String, r::Range1{T}) = s[int(first(r)):int(last(r))]
# TODO: handle other ranges with stride ±1 specially?
getindex(s::String, v::AbstractVector) =
    sprint(length(v), io->(for i in v write(io,s[i]) end))

symbol(s::String) = symbol(bytestring(s))

print(io::IO, s::String) = write(io, s)
write(io::IO, s::String) = for c in s write(io, c) end
show(io::IO, s::String) = print_quoted(io, s)

sizeof(s::String) = error("type $(typeof(s)) has no canonical binary representation")

eltype(::String) = Char
eltype{T<:String}(::Type{T}) = Char

(*)(s::String...) = string(s...)
(^)(s::String, r::Integer) = repeat(s,r)

length(s::DirectIndexString) = endof(s)
function length(s::String)
    i = start(s)
    if done(s,i)
        return 0
    end
    n = 1
    while true
        c, j = next(s,i)
        if done(s,j)
            return n
        end
        n += 1
        i = j
    end
end

isvalid(s::DirectIndexString, i::Integer) = (start(s) <= i <= endof(s))
function isvalid(s::String, i::Integer)
    try
        next(s,i)
        true
    catch
        false
    end
end

prevind(s::DirectIndexString, i::Integer) = i-1
prevind(s                   , i::Integer) = i-1
nextind(s::DirectIndexString, i::Integer) = i+1
nextind(s                   , i::Integer) = i+1

function prevind(s::String, i::Integer)
    e = endof(s)
    if i > e
        return e
    end
    j = i-1
    while j >= 1
        if isvalid(s,j)
            return j
        end
        j -= 1
    end
    return 0 # out of range
end

function nextind(s::String, i::Integer)
    e = endof(s)
    if i < 1
        return 1
    end
    if i > e
        return i+1
    end
    for j = i+1:e
        if isvalid(s,j)
            return j
        end
    end
    next(s,e)[2] # out of range
end

ind2chr(s::DirectIndexString, i::Integer) = i
chr2ind(s::DirectIndexString, i::Integer) = i

function ind2chr(s::String, i::Integer)
    s[i] # throws error if invalid
    j = 1
    k = start(s)
    while true
        c, l = next(s,k)
        if i <= k
            return j
        end
        j += 1
        k = l
    end
end

function chr2ind(s::String, i::Integer)
    if i < 1
        return i
    end
    j = 1
    k = start(s)
    while true
        c, l = next(s,k)
        if i == j
            return k
        end
        j += 1
        k = l
    end
end

typealias Chars Union(Char,AbstractVector{Char},Set{Char})

function search(s::String, c::Chars, i::Integer)
    if isempty(c)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               error(BoundsError)
    end

    if i < 1 error(BoundsError) end
    i = nextind(s,i-1)
    while !done(s,i)
        d, j = next(s,i)
        if d in c
            return i
        end
        i = j
    end
    return 0
end
search(s::String, c::Chars) = search(s,c,start(s))

in(c::Char, s::String) = (search(s,c)!=0)

function _searchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               error(BoundsError)
    end
    t1, j2 = next(t,start(t))
    while true
        i = search(s,t1,i)
        if i == 0 return 0 end
        c, ii = next(s,i)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(s,k)
                matched = false
                break
            end
            c, k = next(s,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return i
        end
        i = ii
    end
end

function _search_bloom_mask(c)
    uint64(1) << (c & 63)
end

function _searchindex(s::Array, t::Array, i)
    n = length(t)
    m = length(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return search(s, t[1], i)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = uint64(0)
    skip = n - 1
    tlast = t[end]
    for j in 1:n
        bloom_mask |= _search_bloom_mask(t[j])
        if t[j] == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if s[i+n] == tlast
            # check candidate
            j = 0
            while j < n - 1
                if s[i+j+1] != t[j+1]
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(s[i+n+1]) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(s[i+n+1]) == 0
                i += n
            end
        end
        i += 1
    end

    0
end

searchindex(s::Union(Array{Uint8,1},Array{Int8,1}),t::Union(Array{Uint8,1},Array{Int8,1}),i) = _searchindex(s,t,i)
searchindex(s::String, t::String, i::Integer) = _searchindex(s,t,i)
searchindex(s::String, t::String) = searchindex(s,t,start(s))

function searchindex(s::ByteString, t::ByteString, i::Integer=1)
    if length(t) == 1
        search(s, t[1], i)
    else
        searchindex(s.data, t.data, i)
    end
end

function search(s::Union(Array{Uint8,1},Array{Int8,1}),t::Union(Array{Uint8,1},Array{Int8,1}),i)
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function search(s::String, t::String, i::Integer=start(s))
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function rsearch(s::String, c::Chars, i::Integer=endof(s))
    e = endof(s)
    j = search(RevString(s), c, e-i+1)
    j == 0 && return 0
    e-j+1
end

function _rsearchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               error(BoundsError)
    end
    t = RevString(t)
    rs = RevString(s)
    l = endof(s)
    t1, j2 = next(t,start(t))
    while true
        i = rsearch(s,t1,i)
        if i == 0 return 0 end
        c, ii = next(rs,l-i+1)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(rs,k)
                matched = false
                break
            end
            c, k = next(rs,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return nextind(s,l-k+1)
        end
        i = l-ii+1
    end
end

function _rsearchindex(s::Array, t::Array, k)
    n = length(t)
    m = length(s)

    if n == 0
        return 0 <= k <= m ? max(k, 1) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return rsearch(s, t[1], k)
    end

    w = m - n
    if w < 0 || k <= 0
        return 0
    end

    bloom_mask = uint64(0)
    skip = n - 1
    tfirst = t[1]
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(t[j])
        if t[j] == tfirst && j > 1
            skip = j - 2
        end
    end

    i = min(k - n + 1, w + 1)
    while i > 0
        if s[i] == tfirst
            # check candidate
            j = 1
            while j < n
                if s[i+j] != t[j+1]
                    break
                end
                j += 1
            end

            # match found
            if j == n
                return i
            end

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(s[i-1]) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(s[i-1]) == 0
                i -= n
            end
        end
        i -= 1
    end

    0
end

rsearchindex(s::Union(Array{Uint8,1},Array{Int8,1}),t::Union(Array{Uint8,1},Array{Int8,1}),i) = _rsearchindex(s,t,i)
rsearchindex(s::String, t::String, i::Integer) = _rsearchindex(s,t,i)
rsearchindex(s::String, t::String) = (isempty(s) && isempty(t)) ? 1 : rsearchindex(s,t,endof(s))

function rsearchindex(s::ByteString, t::ByteString)
    if length(t) == 1
        rsearch(s, t[1])
    else
        rsearchindex(s.data, t.data, length(s.data))
    end
end

function rsearchindex(s::ByteString, t::ByteString, i::Integer)
    if length(t) == 1
        rsearch(s, t[1], i)
    else
        rsearchindex(s.data, t.data, i)
    end
end

function rsearch(s::Union(Array{Uint8,1},Array{Int8,1}),t::Union(Array{Uint8,1},Array{Int8,1}),i)
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function rsearch(s::String, t::String, i::Integer=endof(s))
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

contains(a::String, b::String) = searchindex(a,b)!=0

in(::String, ::String) = error("use contains(x,y) for string containment")

function cmp(a::String, b::String)
    if a === b
        return 0
    end
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d
            return c < d ? -1 : +1
        end
    end
    done(a,i) && !done(b,j) ? -1 :
    !done(a,i) && done(b,j) ? +1 : 0
end

isequal(a::String, b::String) = cmp(a,b) == 0
isless(a::String, b::String)  = cmp(a,b) <  0

# begins with and ends with predicates

function beginswith(a::String, b::String)
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d return false end
    end
    done(b,i)
end
beginswith(a::String, c::Char) = !isempty(a) && a[start(a)] == c

function endswith(a::String, b::String)
    i = endof(a)
    j = endof(b)
    a1 = start(a)
    b1 = start(b)
    while a1 <= i && b1 <= j
        c = a[i]
        d = b[j]
        if c != d return false end
        i = prevind(a,i)
        j = prevind(b,j)
    end
    j < b1
end
endswith(a::String, c::Char) = !isempty(a) && a[end] == c

# faster comparisons for byte strings

cmp(a::ByteString, b::ByteString)     = lexcmp(a.data, b.data)
isequal(a::ByteString, b::ByteString) = endof(a)==endof(b) && cmp(a,b)==0
beginswith(a::ByteString, b::ByteString) = beginswith(a.data, b.data)

beginswith(a::Array{Uint8,1}, b::Array{Uint8,1}) =
    (length(a) >= length(b) && ccall(:strncmp, Int32, (Ptr{Uint8}, Ptr{Uint8}, Uint), a, b, length(b)) == 0)

cmp(a::Symbol, b::Symbol) =
    int(sign(ccall(:strcmp, Int32, (Ptr{Uint8}, Ptr{Uint8}), a, b)))
isless(a::Symbol, b::Symbol) = cmp(a,b)<0

# TODO: fast endswith

## character column width function ##

charwidth(c::Char) = max(0,int(ccall(:wcwidth, Int32, (Uint32,), c)))
strwidth(s::String) = (w=0; for c in s; w += charwidth(c); end; w)
strwidth(s::ByteString) = int(ccall(:u8_strwidth, Csize_t, (Ptr{Uint8},), s.data))
# TODO: implement and use u8_strnwidth that takes a length argument

## libc character class predicates ##

isascii(c::Char) = c < 0x80
isascii(s::String) = all(isascii, s)
isascii(s::ASCIIString) = true

for name = ("alnum", "alpha", "cntrl", "digit", "graph",
            "lower", "print", "punct", "space", "upper")
    f = symbol(string("is",name))
    @eval ($f)(c::Char) = bool(ccall($(string("isw",name)), Int32, (Cwchar_t,), c))
    @eval $f(s::String) = all($f, s)
end

isblank(c::Char) = c==' ' || c=='\t'
isblank(s::String) = all(isblank, s)

## generic string uses only endof and next ##

immutable GenericString <: String
    string::String
end

endof(s::GenericString) = endof(s.string)
next(s::GenericString, i::Int) = next(s.string, i)

## plain old character arrays ##

immutable UTF32String <: DirectIndexString
    data::Array{Char,1}

    UTF32String(a::Array{Char,1}) = new(a)
    UTF32String(c::Char...) = new([ c[i] for i=1:length(c) ])
end
UTF32String(x...) = UTF32String(map(char,x)...)

next(s::UTF32String, i::Int) = (s.data[i], i+1)
endof(s::UTF32String) = length(s.data)
length(s::UTF32String) = length(s.data)

utf32(x) = convert(UTF32String, x)
convert(::Type{UTF32String}, s::UTF32String) = s
convert(::Type{UTF32String}, s::String) = UTF32String(Char[c for c in s])
convert{T<:String}(::Type{T}, v::Vector{Char}) = convert(T, UTF32String(v))
convert(::Type{Array{Char,1}}, s::UTF32String) = s.data
convert(::Type{Array{Char}}, s::UTF32String) = s.data

reverse(s::UTF32String) = UTF32String(reverse(s.data))

sizeof(s::UTF32String) = sizeof(s.data)
convert{T<:Union(Int32,Uint32,Char)}(::Type{Ptr{T}}, s::UTF32String) =
    convert(Ptr{T}, s.data)

## substrings reference original strings ##

immutable SubString{T<:String} <: String
    string::T
    offset::Int
    endof::Int

    function SubString(s::T, i::Int, j::Int)
        if i > endof(s) || j<i
            return new(s, i-1, 0)
        else
            if !isvalid(s,i)
                error("invalid SubString indexes")
            end

            while !isvalid(s,j) && j > i
                j -= 1
            end

            o = i-1
            new(s, o, max(0, j-o))
        end
    end
end
SubString{T<:String}(s::T, i::Int, j::Int) = SubString{T}(s, i, j)
SubString(s::SubString, i::Int, j::Int) = SubString(s.string, s.offset+i, s.offset+j)
SubString(s::String, i::Integer, j::Integer) = SubString(s, int(i), int(j))
SubString(s::String, i::Integer) = SubString(s, i, endof(s))

write{T<:ByteString}(to::IOBuffer, s::SubString{T}) =
    s.endof==0 ? 0 : write_sub(to, s.string.data, s.offset+1, next(s,s.endof)[2]-1)
print(io::IOBuffer, s::SubString) = write(io, s)

sizeof{T<:ByteString}(s::SubString{T}) = s.endof==0 ? 0 : next(s,s.endof)[2]-1

function next(s::SubString, i::Int)
    if i < 1 || i > s.endof
        error(BoundsError)
    end
    c, i = next(s.string, i+s.offset)
    c, i-s.offset
end

function getindex(s::SubString, i::Int)
    if i < 1 || i > s.endof
        error(BoundsError)
    end
    getindex(s.string, i+s.offset)
end

endof(s::SubString) = s.endof
# TODO: length(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces

nextind(s::SubString, i::Integer) = nextind(s.string, i+s.offset)-s.offset
prevind(s::SubString, i::Integer) = prevind(s.string, i+s.offset)-s.offset

convert{T<:String}(::Type{SubString{T}}, s::T) = SubString(s, 1, endof(s))

bytestring{T <: ByteString}(p::SubString{T}) = bytestring(pointer(p.string.data)+p.offset, nextind(p, p.endof)-1)

function serialize{T}(s, ss::SubString{T})
    # avoid saving a copy of the parent string, keeping the type of ss
    invoke(serialize, (Any,Any), s, convert(SubString{T}, convert(T,ss)))
end

function getindex(s::String, r::Range1{Int})
    if first(r) < 1 || endof(s) < last(r)
        error(BoundsError)
    end
    SubString(s, first(r), last(r))
end

function convert{P<:Union(Int8,Uint8),T<:ByteString}(::Type{Ptr{P}}, s::SubString{T})
    if s.offset+s.endof < endof(s.string)
        error("a SubString must coincide with the end of the original string to be convertible to pointer")
    end
    convert(Ptr{P}, s.string.data) + s.offset
end

isascii(s::SubString{ASCIIString}) = true

## efficient representation of repeated strings ##

immutable RepString <: String
    string::String
    repeat::Integer
end

endof(s::RepString)  = endof(s.string)*s.repeat
length(s::RepString) = length(s.string)*s.repeat
sizeof(s::RepString) = sizeof(s.string)*s.repeat

function next(s::RepString, i::Int)
    if i < 1 || i > endof(s)
        error(BoundsError)
    end
    j = mod1(i,length(s.string))
    c, k = next(s.string, j)
    c, k-j+i
end

function repeat(s::String, r::Integer)
    r <  0 ? error("can't repeat a string ",r," times") :
    r == 0 ? "" :
    r == 1 ? s  :
    RepString(s,r)
end

convert(::Type{RepString}, s::String) = RepString(s,1)

function repeat(s::ByteString, r::Integer)
    if r < 0
        error("can't repeat a string ",r," times")
    end
    d = s.data; n = length(d)
    out = Array(Uint8, n*r)
    for i=1:r
        copy!(out, 1+(i-1)*n, d, 1, n)
    end
    convert(typeof(s), out)
end

## reversed strings without data movement ##

immutable RevString{T<:String} <: String
    string::T
end

endof(s::RevString) = endof(s.string)
length(s::RevString) = length(s.string)
sizeof(s::RevString) = sizeof(s.string)

function next(s::RevString, i::Int)
    n = endof(s); j = n-i+1
    (s.string[j], n-prevind(s.string,j)+1)
end

reverse(s::String) = RevString(s)
reverse(s::RevString) = s.string

isascii(s::RevString{ASCIIString}) = true

## ropes for efficient concatenation, etc. ##

immutable RopeString <: String
    head::String
    tail::String
    depth::Int32
    endof::Int

    RopeString(h::RopeString, t::RopeString) =
        strdepth(h.tail) + strdepth(t) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, max(h.depth,t.depth)+1, endof(h)+endof(t))

    RopeString(h::RopeString, t::String) =
        strdepth(h.tail) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, h.depth+1, endof(h)+endof(t))

    RopeString(h::String, t::RopeString) =
        strdepth(t.head) < strdepth(t.tail) ?
            RopeString(RopeString(h, t.head), t.tail) :
            new(h, t, t.depth+1, endof(h)+endof(t))

    RopeString(h::String, t::String) =
        new(h, t, 1, endof(h)+endof(t))
end
RopeString(s::String) = RopeString(s,"")

strdepth(s::String) = 0
strdepth(s::RopeString) = s.depth

function next(s::RopeString, i::Int)
    eh = endof(s.head)
    if i <= eh
        return next(s.head, i)
    else
        c, j = next(s.tail, i-eh)
        return c, j+eh
    end
end

endof(s::RopeString) = s.endof
length(s::RopeString) = length(s.head) + length(s.tail)
print(io::IO, s::RopeString) = print(io, s.head, s.tail)
write(io::IO, s::RopeString) = (write(io, s.head); write(io, s.tail))
sizeof(s::RopeString) = sizeof(s.head) + sizeof(s.tail)

## uppercase and lowercase transformations ##

uppercase(c::Char) = convert(Char, ccall(:towupper, Cwchar_t, (Cwchar_t,), c))
lowercase(c::Char) = convert(Char, ccall(:towlower, Cwchar_t, (Cwchar_t,), c))

uppercase(s::String) = map(uppercase, s)
lowercase(s::String) = map(lowercase, s)

function ucfirst(s::String)
    isempty(s) || isupper(s[1]) ? s : string(uppercase(s[1]),s[nextind(s,1):end])
end
function lcfirst(s::String)
    isempty(s) || islower(s[1]) ? s : string(lowercase(s[1]),s[nextind(s,1):end])
end

## string map, filter, has ##

function map(f::Function, s::String)
    out = IOBuffer(Array(Uint8,endof(s)),true,true)
    truncate(out,0)
    for c in s
        c2 = f(c)
        if !isa(c2,Char)
            error("map(f,s::String) requires f to return Char; try map(f,collect(s)) or a comprehension instead")
        end
        write(out, c2::Char)
    end
    takebuf_string(out)
end

function filter(f::Function, s::String)
    out = IOBuffer(Array(Uint8,endof(s)),true,true)
    truncate(out,0)
    for c in s
        if f(c)
            write(out, c)
        end
    end
    takebuf_string(out)
end

## string promotion rules ##

promote_rule{S<:String,T<:String}(::Type{S}, ::Type{T}) = UTF8String

## printing literal quoted string data ##

# this is the inverse of print_unescaped_chars(io, s, "\\\")

function print_quoted_literal(io, s::String)
    print(io, '"')
    for c = s; c == '"' ? print(io, "\\\"") : print(io, c); end
    print(io, '"')
end

## string escaping & unescaping ##

escape_nul(s::String, i::Int) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? "\\x00" : "\\0"

isxdigit(c::Char) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'
isxdigit(s::String) = all(isxdigit, s)
need_full_hex(s::String, i::Int) = !done(s,i) && isxdigit(next(s,i)[1])

function print_escaped(io, s::String, esc::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'       ? print(io, escape_nul(s,j)) :
        c == '\e'       ? print(io, "\\e") :
        c == '\\'       ? print(io, "\\\\") :
        c in esc        ? print(io, '\\', c) :
        7 <= c <= 13    ? print(io, '\\', "abtnvfr"[int(c-6)]) :
        isprint(c)      ? print(io, c) :
        c <= '\x7f'     ? print(io, "\\x", hex(c, 2)) :
        c <= '\uffff'   ? print(io, "\\u", hex(c, need_full_hex(s,j) ? 4 : 2)) :
                          print(io, "\\U", hex(c, need_full_hex(s,j) ? 8 : 4))
        i = j
    end
end

escape_string(s::String) = sprint(endof(s), print_escaped, s, "\"")
function print_quoted(io, s::String)
    print(io, '"')
    print_escaped(io, s, "\"\$") #"# work around syntax highlighting problem
    print(io, '"')
end

# bare minimum unescaping function unescapes only given characters

function print_unescaped_chars(io, s::String, esc::String)
    if !('\\' in esc)
        esc = string("\\", esc)
    end
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if c == '\\' && !done(s,i) && s[i] in esc
            c, i = next(s,i)
        end
        print(io, c)
    end
end

unescape_chars(s::String, esc::String) =
    sprint(endof(s), print_unescaped_chars, s, esc)

# general unescaping of traditional C and Unicode escape sequences

function print_unescaped(io, s::String)
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if !done(s,i) && c == '\\'
            c, i = next(s,i)
            if c == 'x' || c == 'u' || c == 'U'
                n = k = 0
                m = c == 'x' ? 2 :
                    c == 'u' ? 4 : 8
                while (k+=1) <= m && !done(s,i)
                    c, j = next(s,i)
                    n = '0' <= c <= '9' ? n<<4 + c-'0' :
                        'a' <= c <= 'f' ? n<<4 + c-'a'+10 :
                        'A' <= c <= 'F' ? n<<4 + c-'A'+10 : break
                    i = j
                end
                if k == 1
                    error("\\x used with no following hex digits")
                end
                if m == 2 # \x escape sequence
                    write(io, uint8(n))
                else
                    print(io, char(n))
                end
            elseif '0' <= c <= '7'
                k = 1
                n = c-'0'
                while (k+=1) <= 3 && !done(s,i)
                    c, j = next(s,i)
                    n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                    i = j
                end
                if n > 255
                    error("octal escape sequence out of range")
                end
                write(io, uint8(n))
            else
                print(io, c == 'a' ? '\a' :
                          c == 'b' ? '\b' :
                          c == 't' ? '\t' :
                          c == 'n' ? '\n' :
                          c == 'v' ? '\v' :
                          c == 'f' ? '\f' :
                          c == 'r' ? '\r' :
                          c == 'e' ? '\e' : c)
            end
        else
            print(io, c)
        end
    end
end

unescape_string(s::String) = sprint(endof(s), print_unescaped, s)

## checking UTF-8 & ACSII validity ##

byte_string_classify(data::Array{Uint8,1}) =
    ccall(:u8_isvalid, Int32, (Ptr{Uint8}, Int), data, length(data))
byte_string_classify(s::ByteString) = byte_string_classify(s.data)
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

is_valid_ascii(s::Union(Array{Uint8,1},ByteString)) = byte_string_classify(s) == 1
is_valid_utf8 (s::Union(Array{Uint8,1},ByteString)) = byte_string_classify(s) != 0

## multiline strings ##

function blank_width(c::Char)
    c == ' '   ? 1 :
    c == '\t'  ? 8 :
    error("not a blank character")
end

# width of leading blank space, also check if string is blank
function indentation(s::String)
    count = 0
    for c in s
        if isblank(c)
            count += blank_width(c)
        else
            return count, false
        end
    end
    count, true
end

function unindent(s::String, indent::Int)
    buf = IOBuffer(Array(Uint8,endof(s)), true, true)
    truncate(buf,0)
    a = i = start(s)
    cutting = false
    cut = 0
    while !done(s,i)
        c,i_ = next(s,i)
        if cutting && isblank(c)
            a = i_
            cut += blank_width(c)
            if cut > indent
                cutting = false
                for _ = (indent+1):cut write(buf, ' ') end
            end
        elseif c == '\n'
            print(buf, s[a:i])
            a = i_
            cutting = true
            cut = 0
        else
            cutting = false
        end
        i = i_
    end
    print(buf, s[a:end])
    takebuf_string(buf)
end

function triplequoted(args...)
    sx = { isa(arg,ByteString) ? arg : esc(arg) for arg in args }

    indent = 0
    rlines = split(RevString(sx[end]), '\n', 2)
    last_line = rlines[1]
    if length(rlines) > 1 && lstrip(last_line) == ""
        indent,_ = indentation(last_line)
    else
        indent = typemax(Int)
        for s in sx
            if isa(s,ByteString)
                lines = split(s,'\n')
                for line in lines[2:end]
                    n,blank = indentation(line)
                    if !blank
                        indent = min(indent, n)
                    end
                end
            end
        end
    end

    for i in 1:length(sx)
        if isa(sx[i],ByteString)
            sx[i] = unindent(sx[i], indent)
        end
    end

    # strip leading blank line
    s = sx[1]
    j = search(s,'\n')
    if j != 0 && lstrip(s[1:j]) == ""
        sx[1] = s[j+1:end]
    end

    length(sx) == 1 ? sx[1] : Expr(:call, :string, sx...)
end

## core string macros ##

macro b_str(s); :($(unescape_string(s)).data); end

macro mstr(s...); triplequoted(s...); end

## shell-like command parsing ##

function shell_parse(raw::String, interp::Bool)
    s = strip(raw)
    last_parse = 0:-1
    isempty(s) && return interp ? (Expr(:tuple,:()),last_parse) : ({},last_parse)

    in_single_quotes = false
    in_double_quotes = false

    args = {}
    arg = {}
    i = start(s)
    j = i

    function update_arg(x)
        if !isa(x,String) || !isempty(x)
            push!(arg, x)
        end
    end
    function append_arg()
        if isempty(arg); arg = {"",}; end
        push!(args, arg)
        arg = {}
    end

    while !done(s,j)
        c, k = next(s,j)
        if !in_single_quotes && !in_double_quotes && isspace(c)
            update_arg(s[i:j-1])
            append_arg()
            j = k
            while !done(s,j)
                c, k = next(s,j)
                if !isspace(c)
                    i = j
                    break
                end
                j = k
            end
        elseif interp && !in_single_quotes && c == '$'
            update_arg(s[i:j-1]); i = k; j = k
            if done(s,k)
                error("\$ right before end of command")
            end
            if isspace(s[k])
                error("space not allowed right after \$")
            end
            stpos = j
            ex, j = parse(s,j,greedy=false)
            last_parse = stpos:j
            update_arg(esc(ex)); i = j
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                update_arg(s[i:j-1]); i = k
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                update_arg(s[i:j-1]); i = k
            elseif c == '\\'
                if in_double_quotes
                    if done(s,k)
                        error("unterminated double quote")
                    end
                    if s[k] == '"' || s[k] == '$'
                        update_arg(s[i:j-1]); i = k
                        c, k = next(s,k)
                    end
                elseif !in_single_quotes
                    if done(s,k)
                        error("dangling backslash")
                    end
                    update_arg(s[i:j-1]); i = k
                    c, k = next(s,k)
                end
            end
            j = k
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    update_arg(s[i:end])
    append_arg()

    if !interp
        return (args,last_parse)
    end

    # construct an expression
    ex = Expr(:tuple)
    for arg in args
        push!(ex.args, Expr(:tuple, arg...))
    end
    (ex,last_parse)
end
shell_parse(s::String) = shell_parse(s,true)

function shell_split(s::String)
    parsed = shell_parse(s,false)[1]
    args = String[]
    for arg in parsed
       push!(args, string(arg...))
    end
    args
end

function print_shell_word(io::IO, word::String)
    if isempty(word)
        print(io, "''")
    end
    has_single = false
    has_special = false
    for c in word
        if isspace(c) || c=='\\' || c=='\'' || c=='"' || c=='$'
            has_special = true
            if c == '\''
                has_single = true
            end
        end
    end
    if !has_special
        print(io, word)
    elseif !has_single
        print(io, '\'', word, '\'')
    else
        print(io, '"')
        for c in word
            if c == '"' || c == '$'
                print(io, '\\')
            end
            print(io, c)
        end
        print(io, '"')
    end
end

function print_shell_escaped(io::IO, cmd::String, args::String...)
    print_shell_word(io, cmd)
    for arg in args
        print(io, ' ')
        print_shell_word(io, arg)
    end
end
print_shell_escaped(io::IO) = nothing

shell_escape(args::String...) = sprint(print_shell_escaped, args...)

## interface to parser ##

function parse(str::String, pos::Int; greedy::Bool=true, raise::Bool=true)
    # returns (expr, end_pos). expr is () in case of parse error.
    ex, pos = ccall(:jl_parse_string, Any,
                    (Ptr{Uint8}, Int32, Int32),
                    str, pos-1, greedy ? 1:0)
    if raise && isa(ex,Expr) && is(ex.head,:error)
        throw(ParseError(ex.args[1]))
    end
    if ex == ()
        if raise
            throw(ParseError("end of input"))
        else
            ex = Expr(:error, "end of input")
        end
    end
    ex, pos+1 # C is zero-based, Julia is 1-based
end

function parse(str::String; raise::Bool=true)
    ex, pos = parse(str, start(str), greedy=true, raise=raise)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if !done(str, pos)
        raise && error("extra token after end of expression")
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

## miscellaneous string functions ##

function lpad(s::String, n::Integer, p::String=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return bytestring(p^m * s)
    end
    q = div(m,l)
    r = m - q*l
    bytestring(p^q*p[1:chr2ind(p,r)]*s)
end

function rpad(s::String, n::Integer, p::String=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return bytestring(s * p^m)
    end
    q = div(m,l)
    r = m - q*l
    bytestring(s*p^q*p[1:chr2ind(p,r)])
end

lpad(s, n::Integer, p=" ") = lpad(string(s),n,string(p))
rpad(s, n::Integer, p=" ") = rpad(string(s),n,string(p))
cpad(s, n::Integer, p=" ") = rpad(lpad(s,div(n+strwidth(s),2),p),n,p)


# splitter can be a Char, Vector{Char}, String, Regex, ...
# any splitter that provides search(s::String, splitter)
split{T<:SubString}(str::T, splitter, limit::Integer, keep_empty::Bool) = _split(str, splitter, limit, keep_empty, T[])
split{T<:String}(str::T, splitter, limit::Integer, keep_empty::Bool) = _split(str, splitter, limit, keep_empty, SubString{T}[])
function _split{T<:String,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
    i = start(str)
    n = endof(str)
    r = search(str,splitter,i)
    j, k = first(r), nextind(str,last(r))
    while 0 < j <= n && length(strs) != limit-1
        if i < k
            if keep_empty || i < j
                push!(strs, SubString(str,i,prevind(str,j)))
            end
            i = k
        end
        if k <= j; k = nextind(str,j) end
        r = search(str,splitter,k)
        j, k = first(r), nextind(str,last(r))
    end
    if keep_empty || !done(str,i)
        push!(strs, SubString(str,i))
    end
    return strs
end
split(s::String, spl, n::Integer) = split(s, spl, n, true)
split(s::String, spl, keep::Bool) = split(s, spl, 0, keep)
split(s::String, spl)             = split(s, spl, 0, true)

# a bit oddball, but standard behavior in Perl, Ruby & Python:
const _default_delims = [' ','\t','\n','\v','\f','\r']
split(str::String)                = split(str, _default_delims, 0, false)


rsplit{T<:SubString}(str::T, splitter, limit::Integer, keep_empty::Bool) = _rsplit(str, splitter, limit, keep_empty, T[])
rsplit{T<:String}(str::T, splitter, limit::Integer, keep_empty::Bool) = _rsplit(str, splitter, limit, keep_empty, SubString{T}[])
function _rsplit{T<:String,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
    i = start(str)
    n = endof(str)
    r = rsearch(str,splitter)
    j = first(r)-1
    k = last(r)
    while((0 <= j < n) && (length(strs) != limit-1))
        if i <= k
            (keep_empty || (k < n)) && unshift!(strs, SubString(str,k+1,n))
            n = j
        end
        (k <= j) && (j = prevind(str,j))
        r = rsearch(str,splitter,j)
        j = first(r)-1
        k = last(r)
    end
    (keep_empty || (n > 0)) && unshift!(strs, SubString(str,1,n))
    return strs
end
rsplit(s::String, spl, n::Integer) = rsplit(s, spl, n, true)
rsplit(s::String, spl, keep::Bool) = rsplit(s, spl, 0, keep)
rsplit(s::String, spl)             = rsplit(s, spl, 0, true)
#rsplit(str::String) = rsplit(str, _default_delims, 0, false)

function replace(str::ByteString, pattern, repl::Function, limit::Integer)
    n = 1
    e = endof(str)
    i = a = start(str)
    r = search(str,pattern,i)
    j, k = first(r), last(r)
    out = IOBuffer()
    while j != 0
        if i == a || i <= k
            write(out, SubString(str,i,prevind(str,j)))
            write(out, string(repl(SubString(str,j,k))))
        end
        if k<j
            i = j
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        if j > e
            break
        end
        r = search(str,pattern,k)
        j, k = first(r), last(r)
        n == limit && break
        n += 1
    end
    write(out, SubString(str,i))
    takebuf_string(out)
end
replace(s::String, pat, f::Function, n::Integer) = replace(bytestring(s), pat, f, n)
replace(s::String, pat, r, n::Integer) = replace(s, pat, x->r, n)
replace(s::String, pat, r) = replace(s, pat, r, 0)

function print_joined(io, strings, delim, last)
    i = start(strings)
    if done(strings,i)
        return
    end
    str, i = next(strings,i)
    print(io, str)
    while !done(strings,i)
        str, i = next(strings,i)
        print(io, done(strings,i) ? last : delim)
        print(io, str)
    end
end

function print_joined(io, strings, delim)
    i = start(strings)
    while !done(strings,i)
        str, i = next(strings,i)
        print(io, str)
        if !done(strings,i)
            print(io, delim)
        end
    end
end
print_joined(io, strings) = print_joined(io, strings, "")

join(args...) = sprint(print_joined, args...)

chop(s::String) = s[1:end-1]

function chomp(s::String)
    i = endof(s)
    if (i < 1 || s[i] != '\n') return s end
    j = prevind(s,i)
    if (j < 1 || s[j] != '\r') return s[1:i-1] end
    return s[1:j-1]
end
chomp(s::ByteString) =
    (endof(s) < 1 || s.data[end]   != 0x0a) ? s :
    (endof(s) < 2 || s.data[end-1] != 0x0d) ? s[1:end-1] : s[1:end-2]

# NOTE: use with caution -- breaks the immutable string convention!
function chomp!(s::ByteString)
    if !isempty(s) && s.data[end] == 0x0a
        n = (endof(s) < 2 || s.data[end-1] != 0x0d) ? 1 : 2
        ccall(:jl_array_del_end, Void, (Any, Uint), s.data, n)
    end
    return s
end
chomp!(s::String) = chomp(s) # copying fallback for other string types

function lstrip(s::String, chars::Chars=_default_delims)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !(c in chars)
            return s[i:end]
        end
        i = j
    end
    ""
end

function rstrip(s::String, chars::Chars=_default_delims)
    r = RevString(s)
    i = start(r)
    while !done(r,i)
        c, j = next(r,i)
        if !(c in chars)
            return s[1:end-i+1]
        end
        i = j
    end
    ""
end

strip(s::String) = lstrip(rstrip(s))
strip(s::String, chars::Chars) = lstrip(rstrip(s, chars), chars)

## string to integer functions ##

function parseint(c::Char, base::Integer=36, a::Int=(base <= 36 ? 10 : 36))
    2 <= base <= 62 || error("invalid base: $base")
    d = '0' <= c <= '9' ? c-'0'    :
        'A' <= c <= 'Z' ? c-'A'+10 :
        'a' <= c <= 'z' ? c-'a'+a  : error("invalid digit: $(repr(c))")
    d < base || error("invalid base $base digit $(repr(c))")
    d
end
parseint{T<:Integer}(::Type{T}, c::Char, base::Integer) = convert(T,parseint(c,base))
parseint{T<:Integer}(::Type{T}, c::Char) = convert(T,parseint(c))

function parseint_next(s::String, i::Int=start(s))
    done(s,i) && error("premature end of integer: $(repr(s))")
    j = i
    c, i = next(s,i)
    c, i, j
end

function parseint_preamble(signed::Bool, s::String, base::Int)
    c, i, j = parseint_next(s)
    while isspace(c)
        c, i, j = parseint_next(s,i)
    end
    sgn = 1
    if signed
        if c == '-' || c == '+'
            (c == '-') && (sgn = -1)
            c, i, j = parseint_next(s,i)
        end
    end
    while isspace(c)
        c, i, j = parseint_next(s,i)
    end
    if base == 0
        if c == '0' && !done(s,i)
            c, i = next(s,i)
            base = c=='b' ? 2 : c=='o' ? 8 : c=='x' ? 16 : 10
            if base != 10
                c, i, j = parseint_next(s,i)
            end
        else
            base = 10
        end
    end
    return sgn, base, j
end

function parseint_nocheck{T<:Integer}(::Type{T}, s::String, base::Int, a::Int)
    sgn, base, i = parseint_preamble(T<:Signed,s,base)
    c, i = parseint_next(s,i)
    base = convert(T,base)
    ## FIXME: remove 128-bit specific code once 128-bit div doesn't rely on BigInt
    m::T = T===Uint128 || T===Int128 ? typemax(T) : div(typemax(T)-base+1,base)
    n::T = 0
    while n <= m
        d::T = '0' <= c <= '9' ? c-'0'    :
               'A' <= c <= 'Z' ? c-'A'+10 :
               'a' <= c <= 'z' ? c-'a'+a  : base
        d < base || error("invalid base $base digit $(repr(c)) in $(repr(s))")
        n *= base
        n += d
        if done(s,i)
            n *= sgn
            return n
        end
        c, i = next(s,i)
        isspace(c) && break
    end
    (T <: Signed) && (n *= sgn)
    while !isspace(c)
        d::T = '0' <= c <= '9' ? c-'0'    :
               'A' <= c <= 'Z' ? c-'A'+10 :
               'a' <= c <= 'z' ? c-'a'+a  : base
        d < base || error("invalid base $base digit $(repr(c)) in $(repr(s))")
        (T <: Signed) && (d *= sgn)
        n = checked_mul(n,base)
        n = checked_add(n,d)
        done(s,i) && return n
        c, i = next(s,i)
    end
    while !done(s,i)
        c, i = next(s,i)
        isspace(c) || error("extra characters after whitespace in $(repr(s))")
    end
    return n
end
parseint_nocheck{T<:Integer}(::Type{T}, s::String, base::Int) =
    parseint_nocheck(T, s, base, base <= 36 ? 10 : 36)

parseint{T<:Integer}(::Type{T}, s::String, base::Integer) =
    2 <= base <= 62 ? parseint_nocheck(T,s,int(base)) : error("invalid base: $base")
parseint{T<:Integer}(::Type{T}, s::String) = parseint_nocheck(T,s,0)
parseint(s::String, base::Integer) = parseint(Int,s,base)
parseint(s::String) = parseint_nocheck(Int,s,0)

integer (s::String) = int(s)
unsigned(s::String) = uint(s)
int     (s::String) = parseint(Int,s)
uint    (s::String) = parseint(Uint,s)
int8    (s::String) = parseint(Int8,s)
uint8   (s::String) = parseint(Uint8,s)
int16   (s::String) = parseint(Int16,s)
uint16  (s::String) = parseint(Uint16,s)
int32   (s::String) = parseint(Int32,s)
uint32  (s::String) = parseint(Uint32,s)
int64   (s::String) = parseint(Int64,s)
uint64  (s::String) = parseint(Uint64,s)
int128  (s::String) = parseint(Int128,s)
uint128 (s::String) = parseint(Uint128,s)

## stringifying integers more efficiently ##

string(x::Union(Int8,Int16,Int32,Int64,Int128)) = dec(x)

## string to float functions ##

float64_isvalid(s::String, out::Array{Float64,1}) =
    ccall(:jl_strtod, Int32, (Ptr{Uint8},Ptr{Float64}), s, out) == 0
float32_isvalid(s::String, out::Array{Float32,1}) =
    ccall(:jl_strtof, Int32, (Ptr{Uint8},Ptr{Float32}), s, out) == 0

float64_isvalid(s::SubString, out::Array{Float64,1}) =
    ccall(:jl_substrtod, Int32, (Ptr{Uint8},Csize_t,Int,Ptr{Float64}), s.string, convert(Csize_t,s.offset), s.endof, out) == 0
float32_isvalid(s::SubString, out::Array{Float32,1}) =
    ccall(:jl_substrtof, Int32, (Ptr{Uint8},Csize_t,Int,Ptr{Float32}), s.string, convert(Csize_t,s.offset), s.endof, out) == 0

begin
    local tmp::Array{Float64,1} = Array(Float64,1)
    local tmpf::Array{Float32,1} = Array(Float32,1)
    global float64, float32
    function float64(s::String)
        if !float64_isvalid(s, tmp)
            throw(ArgumentError("float64(String): invalid number format"))
        end
        return tmp[1]
    end

    function float32(s::String)
        if !float32_isvalid(s, tmpf)
            throw(ArgumentError("float32(String): invalid number format"))
        end
        return tmpf[1]
    end
end

float(x::String) = float64(x)
parsefloat(x::String) = float64(x)
parsefloat(::Type{Float64}, x::String) = float64(x)
parsefloat(::Type{Float32}, x::String) = float32(x)

for conv in (:float, :float32, :float64,
             :int, :int8, :int16, :int32, :int64,
             :uint, :uint8, :uint16, :uint32, :uint64)
    @eval ($conv){S<:String}(a::AbstractArray{S}) = map($conv, a)
end

# find the index of the first occurrence of a value in a byte array

typealias ByteArray Union(Array{Uint8,1},Array{Int8,1})

function search(a::ByteArray, b::Union(Int8,Uint8), i::Integer)
    if i < 1 error(BoundsError) end
    n = length(a)
    if i > n return i == n+1 ? 0 : error(BoundsError) end
    p = pointer(a)
    q = ccall(:memchr, Ptr{Uint8}, (Ptr{Uint8}, Int32, Csize_t), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : int(q-p+1)
end
function search(a::ByteArray, b::Char, i::Integer)
    if isascii(b)
        search(a,uint8(b),i)
    else
        search(a,string(b).data,i).start
    end
end
search(a::ByteArray, b::Union(Int8,Uint8,Char)) = search(a,b,1)

function rsearch(a::Union(Array{Uint8,1},Array{Int8,1}), b::Union(Int8,Uint8), i::Integer)
    if i < 1 return i == 0 ? 0 : error(BoundsError) end
    n = length(a)
    if i > n return i == n+1 ? 0 : error(BoundsError) end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{Uint8}, (Ptr{Uint8}, Int32, Csize_t), p, b, i)
    q == C_NULL ? 0 : int(q-p+1)
end
function rsearch(a::ByteArray, b::Char, i::Integer)
    if isascii(b)
        rsearch(a,uint8(b),i)
    else
        rsearch(a,string(b).data,i).start
    end
end
rsearch(a::ByteArray, b::Union(Int8,Uint8,Char)) = rsearch(a,b,length(a))

# return a random string (often useful for temporary filenames/dirnames)
let
    global randstring
    const b = uint8(['0':'9','A':'Z','a':'z'])
    randstring(n::Int) = ASCIIString(b[rand(1:length(b),n)])
    randstring() = randstring(8)
end

function hex2bytes(s::ASCIIString)
    len = length(s)
    iseven(len) || error("string length must be even: $(repr(s))")
    arr = zeros(Uint8, div(len,2))
    i = j = 0
    while i < len
        n = 0
        c = s[i+=1]
        n = '0' <= c <= '9' ? c - '0' :
            'a' <= c <= 'f' ? c - 'a' + 10 :
            'A' <= c <= 'F' ? c - 'A' + 10 :
                error("not a hexadecimal string: $(repr(s))")
        c = s[i+=1]
        n = '0' <= c <= '9' ? n << 4 + c - '0' :
            'a' <= c <= 'f' ? n << 4 + c - 'a' + 10 :
            'A' <= c <= 'F' ? n << 4 + c - 'A' + 10 :
                error("not a hexadecimal string: $(repr(s))")
        arr[j+=1] = n
    end
    return arr
end

bytes2hex{T<:Uint8}(arr::Array{T,1}) = join([hex(i,2) for i in arr])

function repr(x)
    s = IOBuffer()
    showall(s, x)
    takebuf_string(s)
end

# pointer conversions of ASCII/UTF8/UTF16/UTF32 strings:
pointer(x::Union(ByteString,UTF16String,UTF32String)) = pointer(x.data)
pointer{T<:ByteString}(x::SubString{T}) = pointer(x.string.data) + x.offset
pointer(x::ByteString, i::Integer) = pointer(x.data)+(i-1)
pointer{T<:ByteString}(x::SubString{T}, i::Integer) = pointer(x.string.data) + x.offset + (i-1)
pointer(x::Union(UTF16String,UTF32String), i::Integer) = pointer(x)+(i-1)*sizeof(eltype(x.data))
pointer{T<:Union(UTF16String,UTF32String)}(x::SubString{T}) = pointer(x.string.data) + x.offset*sizeof(eltype(x.data))
pointer{T<:Union(UTF16String,UTF32String)}(x::SubString{T}, i::Integer) = pointer(x.string.data) + (x.offset + (i-1))*sizeof(eltype(x.data))

# string hashing:
if WORD_SIZE == 64
    hash{T<:ByteString}(s::Union(T,SubString{T})) =
        ccall(:memhash, Uint64, (Ptr{Void}, Int), pointer(s), sizeof(s))
    hash{T<:ByteString}(s::Union(T,SubString{T}), seed::Union(Int,Uint)) =
        ccall(:memhash_seed, Uint64, (Ptr{Void}, Int, Uint32),
              pointer(s), sizeof(s), uint32(seed))
else
    hash{T<:ByteString}(s::Union(T,SubString{T})) =
        ccall(:memhash32, Uint32, (Ptr{Void}, Int), pointer(s), sizeof(s))
    hash{T<:ByteString}(s::Union(T,SubString{T}), seed::Union(Int,Uint)) =
        ccall(:memhash32_seed, Uint32, (Ptr{Void}, Int, Uint32),
              pointer(s), sizeof(s), uint32(seed))
end
hash(s::String) = hash(bytestring(s))

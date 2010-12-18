## generic string functions ##

start(s::String) = 1
done(s::String,i) = (i > length(s))
ref(s::String, i::Index) = next(s,i)[1]
length(s::String) = at_string_end(s)[1]
strlen(s::String) = at_string_end(s)[2]
string(s::String) = s

print(c::Char) = (write(current_output_stream(), c); ())
print(s::String) = for c = s; print(c); end
println(args...) = print(args..., '\n')

function show(c::Char)
    print('\'')
    if c == '\''
        print("\\'")
    else
        print_escaped(string(c), false, '\xff')
    end
    print('\'')
end
show(s::String) = print_quoted(s)

(*)(s::String...) = strcat(s...)
(^)(s::String, r::Int) = repeat(s,r)

size(s::String) = (length(s),)
function size(s::String, d::Index)
    if d != 1
        error("in size: tupleref: index ",d," out of range")
    end
    length(s)
end

function at_string_end(s::String)
    n = 0
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        n += 1
    end
    return i, n
end

function strind(s::String, i::Int)
    j = start(s)
    for k = 1:i-1; c, j = next(s,j); end
    return j
end

function strchr(s::String, c::Char, i::Int)
    while !done(s,i)
        d, j = next(s,i)
        if c == d
            return i
        end
        i = j
    end
    error("char not found")
end

strchr(s::String, c::Char) = strchr(s, c, start(s))

(<) (a::String, b::String) = cmp(a,b) < 0
(>) (a::String, b::String) = cmp(a,b) > 0
(==)(a::String, b::String) = cmp(a,b) == 0
(<=)(a::String, b::String) = cmp(a,b) <= 0
(>=)(a::String, b::String) = cmp(a,b) >= 0

function cmp(a::String, b::String)
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

## plain old character arrays ##

struct CharString <: String
    chars::Array{Char,1}

    CharString(a::Array{Char,1}) = new(a)
    CharString(c::Char...) = new([ c[i] | i=1:length(c) ])
    CharString(x...) = CharString(map(char,x)...)
end

next(s::CharString, i::Index) = (s.chars[i], i+1)
length(s::CharString) = length(s.chars)
strlen(s::CharString) = length(s)

string(c::Char) = CharString(c)
string(c::Char, x::Char...) = CharString(c, x...)

## substrings reference original strings ##

struct SubString <: String
    string::String
    offset::Index
    length::Index

    SubString(s::String, i::Index, j::Index) = new(s, i-1, j-i+1)
    SubString(s::SubString, i::Index, j::Index) =
        new(s.string, i-1+s.offset, j-i+1)
end

function next(s::SubString, i::Index)
    if i < 1 || i > s.length
        error("string index out of bounds")
    end
    c, i = next(s.string, i+s.offset)
    c, i-s.offset
end

length(s::SubString) = s.length
# TODO: strlen(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces

ref(s::String, r::Range1{Index})    = SubString(s, r.start, r.stop)

## efficient representation of repeated strings ##

struct RepString <: String
    string::String
    repeat::Int
end

function next(s::RepString, i::Index)
    if i < 1 || i > length(s)
        error("string index out of bounds")
    end
    j = mod1(i,length(s.string))
    c, k = next(s.string, j)
    c, k-j+i
end

length(s::RepString) = length(s.string)*s.repeat
strlen(s::RepString) = strlen(s.string)*s.repeat

repeat(s::String, r::Int) = r <= 0 ? "" :
                            r == 1 ? s  : RepString(s,r)

## ropes for efficient concatenation, etc. ##

# Idea: instead of this standard binary tree structure,
# how about we keep an array of substrings, with an
# offset array. We can do binary search on the offset
# array so we get O(log(n)) indexing time still, but we
# can compute the offsets lazily and avoid all the
# futzing around while the string is being constructed.

struct RopeString <: String
    head::String
    tail::String
    depth::Int32
    length::Index

    # TODO: be more clever about cases like empty strings.

    RopeString(h::RopeString, t::RopeString) =
        depth(h.tail) + depth(t) < depth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, max(h.depth, t.depth)+1, length(h)+length(t))

    RopeString(h::RopeString, t::String) =
        depth(h.tail) < depth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, h.depth+1, length(h)+length(t))

    RopeString(h::String, t::RopeString) =
        depth(t.head) < depth(t.tail) ?
            RopeString(RopeString(h, t.head), t.tail) :
            new(h, t, t.depth+1, length(h)+length(t))

    RopeString(h::String, t::String) =
        new(h, t, 1, length(h)+length(t))
end

depth(s::String) = 0
depth(s::RopeString) = s.depth

function next(s::RopeString, i::Index)
    if i <= length(s.head)
        return next(s.head, i)
    else
        c, j = next(s.tail, i-length(s.head))
        return c, j+length(s.head)
    end
end

length(s::RopeString) = s.length
strlen(s::RopeString) = strlen(s.head) + strlen(s.tail)

strcat() = ""
strcat(s::String) = s
strcat(s::String, t::String...) = RopeString(s, strcat(t...))
strcat(x...) = strcat(map(string,x)...)

print(s::RopeString) = print(s.head,s.tail)

## conversion of general objects to strings ##

function string(p::Ptr{Uint8})
    if p == C_NULL
        error("cannot convert NULL to string")
    end
    ccall(dlsym(JuliaDLHandle,"jl_cstr_to_string"),
          Any, (Ptr{Uint8},), p)::String
end

string(x) = string(ccall(dlsym(JuliaDLHandle,"jl_show_to_string"),
                         Ptr{Uint8}, (Any,), x))

bstring(str::ByteString) = str
bstring(args...) = print_to_string(print, args...)

cstring(str::ByteString) = str.data
cstring(args...) = print_to_array(print, args...)

## generic string utilities ##

for f = {:iswalnum, :iswalpha, :iswascii, :iswblank, :iswcntrl, :iswdigit,
         :iswgraph, :iswhexnumber, :iswideogram, :iswlower, :iswnumber,
         :iswphonogram, :iswprint, :iswpunct, :iswrune, :iswspace,
         :iswspecial, :iswupper, :iswxdigit}
    eval(quote
        ($f)(c::Char) = bool(ccall(dlsym(libc,$string(f)), Int32, (Char,), c))
    end)
end

escape_nul(s::String, i::Index) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? "\\x00" : "\\0"

function print_escaped(s::String, q::Bool, xmax::Char)
    if q; print('"'); end
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'     ? print(escape_nul(s,j)) :
        c == '\\'     ? print("\\\\") :
        c == '\e'     ? print("\\e") :
   q && c == '\"'     ? print("\\\"") :
        iswprint(c)   ? print(c) :
        7 <= c <= 13  ? print('\\', "abtnvfr"[c-6]) :
        c <= xmax     ? print("\\x", uint2str(c,16,2)) :
        c <= '\uffff' ? print("\\u", uint2str(c,16,4)) :
                        print("\\U", uint2str(c,16,8))
        i = j
    end
    if q; print('"'); end
end

print_escaped(s::Latin1String, q) = print_escaped(s, q, '\xff')
print_escaped(s::String, q)       = print_escaped(s, q, '\x7f')
print_escaped(s::String)          = print_escaped(s, false)
print_quoted (s::String)          = print_escaped(s, true)

escape_string(s::String) = print_to_string(length(s),   print_escaped, s)
quote_string (s::String) = print_to_string(length(s)+2, print_quoted,  s)

# TODO: unescaping needs to work on bytes to match the parser

function print_unescaped(s::String)
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if !done(s,i) && c == '\\'
            c, i = next(s,i)
            x = c == 'a' ?  7 :
                c == 'b' ?  8 :
                c == 't' ?  9 :
                c == 'n' ? 10 :
                c == 'v' ? 11 :
                c == 'f' ? 12 :
                c == 'r' ? 13 :
                c == 'e' ? 27 :
                c == 'x' ||
                c == 'u' ||
                c == 'U' ? begin
                    m = c == 'x' ? 2 :
                        c == 'u' ? 4 : 8
                    n = 0
                    k = 0
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
                    n
                end :
                '0' <= c <= '7' ? begin
                    n = c-'0'
                    k = 1
                    while (k+=1) <= 3 && !done(s,i)
                        c, j = next(s,i)
                        n = '0' <= c <= '7' ? n<<3 + c-'0' : break
                        i = j
                    end
                    if n > 255
                        error("octal escape sequence out of range")
                    end
                    n
                end : int32(c)
            print(char(x))
        else
            print(c)
        end
    end
end

unescape_string(s::String) = print_to_string(print_unescaped, s)

function lpad(s::String, n::Int, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    q = div(m,l)
    r = m - q*l
    # TODO: this is correct but inefficient for long p
    p^q * (r > 0 ? p[1:strind(p,r)] : "") * s
end

function rpad(s::String, n::Int, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    q = div(m,l)
    r = m - q*l
    # TODO: this is correct but inefficient for long p
    s * p^q * (r > 0 ? p[1:strind(p,r)] : "")
end

lpad(s, n::Int, p) = lpad(string(s), n, string(p))
rpad(s, n::Int, p) = rpad(string(s), n, string(p))

## string to integer functions ##

function parse_int(T::Type{Int}, s::String, base::Int)
    n = zero(T)
    base = convert(T,base)
    for c = s
        d = '0' <= c <= '9' ? c-'0' :
            'A' <= c <= 'Z' ? c-'A'+10 :
            'a' <= c <= 'z' ? c-'a'+10 :
            error("non alphanumeric digit")
        d = convert(T,d)
        if base <= d
            error("digit not valid in base")
        end
        n = n*base + d
    end
    return n
end

bin(s::String) = parse_int(Int64, s,  2)
oct(s::String) = parse_int(Int64, s,  8)
dec(s::String) = parse_int(Int64, s, 10)
hex(s::String) = parse_int(Int64, s, 16)

## integer to string functions ##

function uint2str(n::Int, b::Int)
    if n < zero(n); error("uint2str: negative argument ", n); end
    if b < 2; error("uint2str: invalid base ", b); end
    ndig = n==convert(typeof(n),0) ? 1 : int32(floor(log(n)/log(b)+1))
    sz = ndig+1
    data = Array(Uint8, sz)
    ccall(dlsym(JuliaDLHandle,"uint2str"), Ptr{Uint8},
          (Ptr{Uint8}, Size, Uint64, Uint32),
          data, sz, uint64(n), uint32(b))
    Latin1String(data[1:(sz-1)]) # cut out terminating NUL
end

uint2str(n::Int, b::Int, len::Int) = lpad(uint2str(n,b),len,'0')

## lexicographically compare byte arrays (used by Latin-1 and UTF-8) ##

function lexcmp(a::Array{Uint8,1}, b::Array{Uint8,1})
    d = ccall(dlsym(libc,"memcmp"), Int32,
              (Ptr{Uint8}, Ptr{Uint8}, Size),
              a, b, min(length(a),length(b)))
    d < 0 ? -1 : d > 0 ? +1 : cmp(length(a),length(b))
end

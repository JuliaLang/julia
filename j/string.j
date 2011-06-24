## generic string functions ##

start(s::String) = 1
done(s::String,i) = (i > length(s))
isempty(s::String) = done(s,start(s))
ref(s::String, i::Index) = next(s,i)[1]
ref(s::String, x::Real) = s[int32(round(x))]
length(s::String) = at_string_end(s)[1]
strlen(s::String) = at_string_end(s)[2]
symbol(s::String) = symbol(cstring(s))
string(s::String) = s

print(c::Char) = (write(current_output_stream(), c); nothing)
print(s::String) = for c = s; print(c); end
print(x...) = (for i=x; print(i); end)
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
size(s::String, d::Index) = d == 1 ? length(s) :
    error("in size: tupleref: index ",d," out of range")

function at_string_end(s::String)
    n = 0
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        n += 1
    end
    return i, n
end

function nextind(s::String, ind::Int)
    for i = ind:length(s)
        try
            c = s[i]
            return i
        catch
        end
    end
    length(s) + 1
end

function prevind(s::String, ind::Int)
    for i = ind-1:-1:1
        try
            c = s[i]
            return i
        catch
        end
    end
    0
end

function ind2chr(s::String, ind::Int)
    if ind < 1
        error("in next: arrayref: index out of range")
    end
    i = 1
    j = start(s)
    while true
        c, k = next(s,j)
        if ind <= j
            return i
        end
        i += 1
        j = k
    end
end

function chr2ind(s::String, chr::Int)
    if chr < 1
        error("in next: arrayref: index out of range")
    end
    i = 1
    j = start(s)
    while true
        c, k = next(s,j)
        if chr == i
            return j
        end
        i += 1
        j = k
    end
end

function strchr(s::String, c::Char, i::Int)
    i = nextind(s,i)
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

function chars(s::String)
    cx = Array(Char,strlen(s))
    i = 0
    for c = s
        cx[i += 1] = c
    end
    cx
end

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

type CharString <: String
    chars::Array{Char,1}

    CharString(a::Array{Char,1}) = new(a)
    CharString(c::Char...) = new([ c[i] | i=1:length(c) ])
end
CharString(x...) = CharString(map(char,x)...)

next(s::CharString, i::Index) = (s.chars[i], i+1)
length(s::CharString) = length(s.chars)
strlen(s::CharString) = length(s)

string(c::Char) = CharString(c)
string(c::Char, x::Char...) = CharString(c, x...)

## substrings reference original strings ##

type SubString <: String
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

function ref(s::String, r::Range1{Index})
    if r.start < 1 || length(s) < r.stop
        error("in substring slice: index out of range")
    end
    SubString(s, r.start, r.stop)
end

## efficient representation of repeated strings ##

type RepString <: String
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

type RopeString <: String
    head::String
    tail::String
    depth::Int32
    length::Index

    RopeString(h::RopeString, t::RopeString) =
        depth(h.tail) + depth(t) < depth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h,
                t,
                max(h.depth,
                    t.depth)+1,
                length(h)+length(t))

    RopeString(h::RopeString, t::String) =
        depth(h.tail) < depth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h,
                t,
                h.depth+1,
                length(h)+length(t))

    RopeString(h::String, t::RopeString) =
        depth(t.head) < depth(t.tail) ?
            RopeString(RopeString(h, t.head), t.tail) :
            new(h,
                t,
                t.depth+1,
                length(h)+length(t))

    RopeString(h::String, t::String) =
        new(h,
            t,
            1,
            length(h)+length(t))
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
strcat(x...) = strcat(map(string,x)...)
strcat(s::String, t::String...) =
    (t = strcat(t...); isempty(s) ? t : isempty(t) ? s : RopeString(s, t))

print(s::RopeString) = print(s.head, s.tail)

## transformed strings ##

type TransformedString <: String
    transform::Function
    string::String
end

length(s::TransformedString) = length(s.string)
length(s::TransformedString) = strlen(s.string)

function next(s::TransformedString, i::Index)
    c, j = next(s.string,i)
    c = s.transform(c, i)
    return c, j
end

## uppercase and lowercase transformations ##

uc(c::Char) = ccall(dlsym(libc, :towupper), Char, (Char,), c)
lc(c::Char) = ccall(dlsym(libc, :towlower), Char, (Char,), c)

uc(str::String) = TransformedString((c,i)->uc(c), str)
lc(str::String) = TransformedString((c,i)->lc(c), str)

ucfirst(str::String) = TransformedString((c,i)->i==1 ? uc(c) : c, str)
lcfirst(str::String) = TransformedString((c,i)->i==1 ? lc(c) : c, str)

## conversion of general objects to strings ##

function string(p::Ptr{Uint8})
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_cstr_to_string, Any, (Ptr{Uint8},), p)::String
end

string(x) = print_to_string(show, x)

cstring(args...) = print_to_string(print, args...)

## printing literal quoted string data ##

function print_quoted_literal(s::String)
    print('"')
    for c = s; c == '"' ? print("\\\"") : print(c); end
    print('"')
end

## string escaping & unescaping ##

escape_nul(s::String, i::Index) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? L"\x00" : L"\0"

function print_escaped(s::String, q::Bool, xmax::Char)
    if q; print('"'); end
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'     ? print(escape_nul(s,j)) :
        c == '\\'     ? print("\\\\") :
        c == '\e'     ? print(L"\e") :
   q && c == '"'      ? print("\\\"") :
        c == '$'      ? print(L"\$") :
        iswprint(c)   ? print(c) :
        7 <= c <= 13  ? print('\\', "abtnvfr"[c-6]) :
        c <= xmax     ? print(L"\x", hex(c,2)) :
        c <= '\uffff' ? print(L"\u", hex(c,4)) :
                        print(L"\U", hex(c,8))
        i = j
    end
    if q; print('"'); end
end

# TODO: make sure ASCII, Latin-1 and UTF-8 strings all get
# printed so that when input back they are equivalent.

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

## string interpolation parsing ##

function interp_parse(str::String, unescape::Function)
    strs = ()
    i = j = start(str)
    while !done(str,j)
        c, k = next(str,j)
        if c == '$'
            if !isempty(str[i:j-1])
                strs = append(strs,(unescape(str[i:j-1]),))
            end
            ex, j = parseatom(str,k)
            strs = append(strs,(ex,)); i = j
        elseif c == '\\' && !done(str,k)
            c, j = next(str,k)
        else
            j = k
        end
    end
    if !isempty(str[i:])
        strs = append(strs,(unescape(str[i:j-1]),))
    end
    length(strs) == 1 ? strs[1] : expr(:call,:strcat,strs...)
end

interp_parse(str::String) = interp_parse(str, unescape_string)

## core string macros ##

macro   str(raw); interp_parse(raw); end
macro S_str(raw); interp_parse(raw); end
macro I_str(raw); interp_parse(raw, x->x); end
macro Q_str(raw); unescape_string(raw); end

# TODO: S"foo\xe2\x88\x80" == "foo\xe2\x88\x80"

## shell-like command parsing ##

function shell_parse(str::String, interp::Bool)

    in_single_quotes = false
    in_double_quotes = false

    args = ()
    arg = ()
    i = start(str)
    j = i

    function update_arg(s)
        if !isa(s,String) || !isempty(s)
            arg = append(arg,(s,))
        end
    end
    function append_arg()
        if arg == (); arg = ("",); end
        args = append(args,(arg,))
        arg = ()
    end

    while !done(str,j)
        c, k = next(str,j)
        if !in_single_quotes && !in_double_quotes && iswspace(c)
            update_arg(str[i:j-1])
            append_arg()
            j = k
            while !done(str,j)
                c, k = next(str,j)
                if !iswspace(c)
                    i = j
                    break
                end
                j = k
            end
        elseif interp && !in_single_quotes && c == '$'
            update_arg(str[i:j-1]); i = k
            j = k
            ex, j = parseatom(str,j)
            update_arg(ex); i = j
        else
            if !in_double_quotes && c == '\''
                in_single_quotes = !in_single_quotes
                update_arg(str[i:j-1]); i = k
            elseif !in_single_quotes && c == '"'
                in_double_quotes = !in_double_quotes
                update_arg(str[i:j-1]); i = k
            elseif c == '\\'
                if in_double_quotes
                    # TODO: handle \$ in double quotes
                    if done(str,k)
                        error("unterminated double quote")
                    end
                    if str[k] == '"'
                        update_arg(str[i:j-1]); i = k
                        c, k = next(str,k)
                    end
                elseif !in_single_quotes
                    if done(str,k)
                        error("dangling backslash")
                    end
                    update_arg(str[i:j-1]); i = k
                    c, k = next(str,k)
                end
            end
            j = k
        end
    end

    if in_single_quotes; error("unterminated single quote"); end
    if in_double_quotes; error("unterminated double quote"); end

    update_arg(str[i:])
    append_arg()

    if !interp
        return args
    end

    # construct an expression
    exprs = ()
    for arg = args
        exprs = append(exprs,(expr(:tuple,arg...),))
    end
    expr(:tuple,exprs...)
end

shell_parse(str::String) = shell_parse(str,true)

function shell_split(str::String)
    parsed = shell_parse(str, false)
    args = ()
    for arg = parsed
        args = append(args,(strcat(arg...),))
    end
    args
end

function print_shell_word(word::String)
    if isempty(word)
        print("''")
    end
    has_spaces = false
    has_backsl = false
    has_single = false
    has_double = false
    has_dollar = false
    for c = word
        if iswspace(c)
            has_spaces = true
        elseif c == '\\'
            has_backsl = true
        elseif c == '\''
            has_single = true
        elseif c == '"'
            has_double = true
        elseif c == '$'
            has_dollar = true
        end
    end
    if !(has_spaces || has_backsl || has_single || has_double || has_dollar)
        print(word)
    elseif !has_single
        print('\'', word, '\'')
    else
        print('"')
        for c = word
            if c == '"' || c == '$'
                print('\\')
            end
            print(c)
        end
        print('"')
    end
end

function print_shell_escaped(cmd::String, args::String...)
    print_shell_word(cmd)
    for arg = args
        print(' ')
        print_shell_word(arg)
    end
end

shell_escape(cmd::String, args::String...) =
    print_to_string(print_shell_escaped, cmd, args...)

## interface to parser ##

function parse(s::String, pos, greedy)
	# returns (expr, end_pos). expr is () in case of parse error.
    ex, pos = ccall(:jl_parse_string, Any,
                    (Ptr{Uint8},Int32,Int32),
                    cstring(s), int32(pos)-1, greedy ? 1:0)
    if isa(ex,Expr) && is(ex.head,:error)
        throw(ParseError(ex.args[1]))
    end
    if ex == (); throw(ParseError("end of input")); end
    ex, pos+1 # C is zero-based, Julia is 1-based
end

parse(s::String)          = parse(s, 1, true)
parse(s::String, pos)     = parse(s, pos, true)
parseatom(s::String)      = parse(s, 1, false)
parseatom(s::String, pos) = parse(s, pos, false)

## miscellaneous string functions ##

function lpad(s::String, n::Int, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    q = div(m,l)
    r = m - q*l
    p^q * (r > 0 ? p[1:chr2ind(p,r)] : "") * s
end

function rpad(s::String, n::Int, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    q = div(m,l)
    r = m - q*l
    s * p^q * (r > 0 ? p[1:chr2ind(p,r)] : "")
end

lpad(s, n::Int, p) = lpad(string(s), n, string(p))
rpad(s, n::Int, p) = rpad(string(s), n, string(p))

lpad(s, n::Int) = lpad(string(s), n, " ")
rpad(s, n::Int) = rpad(string(s), n, " ")

function split(s::String, delims, include_empty)
    i = 1
    strs = {}
    len = length(s)
    while true
        tokstart = tokend = i
        while !done(s,i)
            (c,i) = next(s,i)
            if has(delims, c)
                break
            end
            tokend = i
        end
        tok = s[tokstart:(tokend-1)]
        if !isempty(tok) || include_empty
            push(strs, tok)
        end
        if !((i <= len) || (i==len+1 && tokend!=i))
            break
        end
    end
    strs
end

split(s::String, delims) = split(s, delims, true)
split(s::String, c::Char) = split(s, Set(c))
split(s::String, c::Char, incl) = split(s, Set(c), incl)

function print_joined(delim, strings)
    for i = 1:length(strings)
        print(strings[i])
        if (i < length(strings))
            print(delim)
        end
    end
end

join(delim, strings) = print_to_string(print_joined, delim, strings)

## string to integer functions ##

function parse_int{T<:Int}(::Type{T}, s::String, base::Int)
    n = zero(T)
    base = convert(T,base)
    for c = s
        d = '0' <= c <= '9' ? c-'0' :
            'A' <= c <= 'Z' ? c-'A'+10 :
            'a' <= c <= 'z' ? c-'a'+10 :
            error(c, " is not an alphanumeric digit")
        d = convert(T,d)
        if base <= d
            error(c, " is not a valid digit in base ", base)
        end
        n = n*base + d
    end
    return n
end

parse_bin(s::String) = parse_int(Int64, s,  2)
parse_oct(s::String) = parse_int(Int64, s,  8)
parse_dec(s::String) = parse_int(Int64, s, 10)
parse_hex(s::String) = parse_int(Int64, s, 16)

int   (s::String) = parse_dec(s)
int8  (s::String) = int8(int(s))
uint8 (s::String) = uint8(int(s))
int16 (s::String) = int16(int(s))
uint16(s::String) = uint16(int(s))
int32 (s::String) = int32(int(s))
uint32(s::String) = uint32(int(s))
int64 (s::String) = int64(int(s))
uint64(s::String) = parse_int(Uint64, s, 10)

## integer to string functions ##

function uint2str(n::Int, b::Int)
    if n < zero(n); error("uint2str: negative argument ", n); end
    if b < 2; error("uint2str: invalid base ", b); end
    ndig = n==convert(typeof(n),0) ? 1 : int32(floor(log(n)/log(b)+1))
    sz = ndig+1
    data = Array(Uint8, sz)
    ccall(:uint2str, Ptr{Uint8},
          (Ptr{Uint8}, Ulong, Uint64, Uint32),
          data, ulong(sz), uint64(n), uint32(b))
    ASCIIString(data[1:(sz-1)]) # cut out terminating NUL
end

uint2str(n::Int, b::Int, len::Int) = lpad(uint2str(n,b),len,'0')

# TODO: support signed Ints too

bin(n::Int) = uint2str(n,  2)
oct(n::Int) = uint2str(n,  8)
dec(n::Int) = uint2str(n, 10)
hex(n::Int) = uint2str(n, 16)

bin(n::Int, l::Int) = lpad(bin(n), l, '0')
oct(n::Int, l::Int) = lpad(oct(n), l, '0')
dec(n::Int, l::Int) = lpad(dec(n), l, '0')
hex(n::Int, l::Int) = lpad(hex(n), l, '0')

## string to float functions ##

let tmp = Array(Ptr{Uint8},1)
    global float64, float32
    function float64(s::String)
        s = cstring(s)
        p = pointer(s.data)
        f = ccall(:strtod, Float64, (Ptr{Uint8},Ptr{Ptr{Uint8}}), p, tmp)
        if p==tmp[1] || errno()!=0
            throw(ArgumentError("float64(String): invalid number format"))
        end
        f
    end

    function float32(s::String)
        s = cstring(s)
        p = pointer(s.data)
        f = ccall(:strtof, Float32, (Ptr{Uint8},Ptr{Ptr{Uint8}}), p, tmp)
        if p==tmp[1] || errno()!=0
            throw(ArgumentError("float32(String): invalid number format"))
        end
        f
    end
end

float(x::String) = float64(x)

## fast C-based memory functions for string implementations ##

# lexicographically compare byte arrays (used by Latin-1 and UTF-8)

function lexcmp(a::Array{Uint8,1}, b::Array{Uint8,1})
    c = ccall(dlsym(libc, :memcmp), Int32,
              (Ptr{Uint8}, Ptr{Uint8}, Ulong),
              a, b, ulong(min(length(a),length(b))))
    c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

# find the index of a byte in a byte array

function memchr(a::Array{Uint8,1}, b::Int)
    p = pointer(a)
    q = ccall(dlsym(libc, :memchr), Ptr{Uint8},
              (Ptr{Uint8}, Int32, Ulong),
              p, int32(b), ulong(length(a)))
    if q == C_NULL
        error("char not found")
    end
    q - p + 1
end

# concatenate byte arrays into a single array

function memcat(arrays::Array{Uint8,1}...)
    n = 0
    for a = arrays
        n += length(a)
    end
    arr = Array(Uint8, n)
    ptr = pointer(arr)
    offset = 0
    for a = arrays
        ccall(dlsym(libc, :memcpy), Ptr{Uint8},
              (Ptr{Uint8}, Ptr{Uint8}, Ulong),
              ptr + offset, pointer(a), ulong(length(a)))
        offset += length(a)
    end
    return arr
end

# concatenate the data fields of byte strings

function strdatacat(strs::ByteString...)
    n = 0
    for s = strs
        n += length(s)
    end
    data = Array(Uint8, n)
    ptr = pointer(data)
    offset = 0
    for s = strs
        ccall(dlsym(libc, :memcpy), Ptr{Uint8},
              (Ptr{Uint8}, Ptr{Uint8}, Ulong),
              ptr + offset, pointer(s.data), ulong(length(s)))
        offset += length(s)
    end
    data
end

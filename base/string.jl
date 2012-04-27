## core string functions ##

length(s::String) = error("you must implement length(",typeof(s),")")
next(s::String, i::Int) = error("you must implement next(",typeof(s),",Int)")
next(s::DirectIndexString, i::Int) = (s[i],i+1)
next(s::String, i::Integer) = next(s,int(i))

## generic supplied functions ##

start(s::String) = 1
done(s::String,i) = (i > length(s))
isempty(s::String) = done(s,start(s))
ref(s::String, i::Int) = next(s,i)[1]
ref(s::String, i::Integer) = s[int(i)]
ref(s::String, x::Real) = s[iround(x)]
ref{T<:Integer}(s::String, r::Range1{T}) = s[int(first(r)):int(last(r))]
# TODO: handle other ranges with stride ±1 specially?
ref(s::String, v::AbstractVector) =
    print_to_string(length(v), @thunk for i in v print(s[i]) end)

symbol(s::String) = symbol(cstring(s))
string(s::String) = s

print(s::String) = for c in s print(c) end
print(x...) = for i in x print(i) end
println(args...) = print(args..., '\n')

show(s::String) = print_quoted(s)
showln(x) = (show(x); println())

(*)(s::String...) = strcat(s...)
(^)(s::String, r::Integer) = repeat(s,r)


size(s::String) = (length(s),)
size(s::String, d::Integer) = d==1 ? length(s) :
    error("in size: dimension ",d," out of range")

strlen(s::DirectIndexString) = length(s)
function strlen(s::String)
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

isvalid(s::DirectIndexString, i::Integer) = (start(s) <= i <= length(s))
function isvalid(s::String, i::Integer)
    try
        next(s,i)
        true
    catch
        false
    end
end

prevind(s::DirectIndexString, i::Integer) = i-1
thisind(s::DirectIndexString, i::Integer) = i
nextind(s::DirectIndexString, i::Integer) = i+1

prevind(s::String, i::Integer) = thisind(s,thisind(s,i)-1)

function thisind(s::String, i::Integer)
    for j = i:-1:1
        if isvalid(s,j)
            return j
        end
    end
    return 0 # out of range
end

function nextind(s::String, i::Integer)
    for j = i+1:length(s)
        if isvalid(s,j)
            return j
        end
    end
    length(s)+1 # out of range
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

typealias Chars Union(Char,AbstractVector{Char})

function strchr(s::String, c::Chars, i::Integer)
    if i < 1 error("index out of range") end
    i = nextind(s,i-1)
    while !done(s,i)
        d, j = next(s,i)
        if contains(c,d)
            return i
        end
        i = j
    end
    return 0
end
strchr(s::String, c::Chars) = strchr(s,c,start(s))

contains(s::String, c::Char) = (strchr(s,c)!=0)

search(s::String, c::Chars, i::Integer) = (i=strchr(s,c,i); (i,nextind(s,i)))
search(s::String, c::Chars) = search(s,c,start(s))

function search(s::String, t::String, i::Integer)
    if isempty(t)
        return 1 <= i <= length(s)+1 ? (i,i) :
               i == length(s)+2      ? (0,0) :
               error("index out of range")
    end
    t1, j2 = next(t,start(t))
    while true
        i = strchr(s,t1,i)
        if i == 0 return (0,0) end
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
            return (i,k)
        end
        i = ii
    end
end
search(s::String, t::String) = search(s,t,start(s))

type EachSearch
    string::String
    pattern
end
each_search(string::String, pattern) = EachSearch(string, pattern)

start(itr::EachSearch) = search(itr.string, itr.pattern)
done(itr::EachSearch, st) = (st[1]==0)
next(itr::EachSearch, st) =
    (st, search(itr.string, itr.pattern, max(nextind(itr.string,st[1]),st[2])))

function chars(s::String)
    cx = Array(Char,strlen(s))
    i = 0
    for c in s
        cx[i+=1] = c
    end
    return cx
end

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

isequal(a::String, b::String) = cmp(a,b) == 0
isless(a::String, b::String)  = cmp(a,b) <  0

hash(s::String) = hash(cstring(s))

# faster comparisons for byte strings

cmp(a::ByteString, b::ByteString)     = lexcmp(a.data, b.data)
isequal(a::ByteString, b::ByteString) = length(a)==length(b) && cmp(a,b)==0

## character column width function ##

charwidth(c::Char) = max(0,int(ccall(:wcwidth, Int32, (Char,), c)))
strwidth(s::String) = (w=0; for c in s; w += charwidth(c); end; w)
strwidth(s::ByteString) = ccall(:u8_strwidth, Int, (Ptr{Uint8},), s.data)
# TODO: implement and use u8_strnwidth that takes a length argument

## generic string uses only length and next ##

type GenericString <: String
    string::String
end

length(s::GenericString) = length(s.string)
next(s::GenericString, i::Int) = next(s.string, i)

## plain old character arrays ##

type CharString <: String
    chars::Array{Char,1}

    CharString(a::Array{Char,1}) = new(a)
    CharString(c::Char...) = new([ c[i] | i=1:length(c) ])
end
CharString(x...) = CharString(map(char,x)...)

next(s::CharString, i::Int) = (s.chars[i], i+1)
length(s::CharString) = length(s.chars)
strlen(s::CharString) = length(s)

function string(c::Char, x::Char...)
    s = memio(1+length(x), false)
    write(s, c)
    for ch in x
        write(s, ch)
    end
    takebuf_string(s)
end

## substrings reference original strings ##

type SubString{T<:String} <: String
    string::T
    offset::Int
    length::Int

    SubString(s::T, i::Int, j::Int) =
        (o=nextind(s,i-1)-1; new(s,o,thisind(s,j)-o))
end
SubString{T<:String}(s::T, i::Int, j::Int) = SubString{T}(s, i, j)
SubString(s::SubString, i::Int, j::Int) = SubString(s.string, s.offset+i, s.offset+j)
SubString(s::String, i::Integer, j::Integer) = SubString(s, int(i), int(j))
SubString(s::String, i::Integer) = SubString(s, i, length(s))

function next(s::SubString, i::Int)
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

function ref(s::String, r::Range1{Int})
    if first(r) < 1 || length(s) < last(r)
        error("index out of range")
    end
    SubString(s, first(r), last(r))
end

## efficient representation of repeated strings ##

type RepString <: String
    string::String
    repeat::Integer
end

length(s::RepString) = length(s.string)*s.repeat
strlen(s::RepString) = strlen(s.string)*s.repeat

function next(s::RepString, i::Int)
    if i < 1 || i > length(s)
        error("string index out of bounds")
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

## reversed strings without data movement ##

type RevString <: String
    string::String
end

length(s::RevString) = length(s.string)
strlen(s::RevString) = strlen(s.string)

start(s::RevString) = (n=length(s); n-thisind(s.string,n)+1)
function next(s::RevString, i::Int)
    n = length(s); j = n-i+1
    (s.string[j], n-thisind(s.string,j-1)+1)
end

reverse(s::String) = RevString(s)
reverse(s::RevString) = s.string

## ropes for efficient concatenation, etc. ##

type RopeString <: String
    head::String
    tail::String
    depth::Int32
    length::Int

    RopeString(h::RopeString, t::RopeString) =
        depth(h.tail) + depth(t) < depth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, max(h.depth,t.depth)+1, length(h)+length(t))

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
RopeString(s::String) = RopeString(s,"")

depth(s::String) = 0
depth(s::RopeString) = s.depth

function next(s::RopeString, i::Int)
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
strlen(s::TransformedString) = strlen(s.string)

function next(s::TransformedString, i::Int)
    c, j = next(s.string,i)
    c = s.transform(c, i)
    return c, j
end

## uppercase and lowercase transformations ##

uppercase(c::Char) = ccall(:towupper, Char, (Char,), c)
lowercase(c::Char) = ccall(:towlower, Char, (Char,), c)

uppercase(s::String) = TransformedString((c,i)->uppercase(c), s)
lowercase(s::String) = TransformedString((c,i)->lowercase(c), s)

ucfirst(s::String) = TransformedString((c,i)->i==1 ? uppercase(c) : c, s)
lcfirst(s::String) = TransformedString((c,i)->i==1 ? lowercase(c) : c, s)

const uc = uppercase
const lc = lowercase

## string map ##

function map(f::Function, s::String)
    out = memio(length(s))
    for c in s
        write(out, f(c)::Char)
    end
    takebuf_string(out)
end

## conversion of general objects to strings ##

string(x) = print_to_string(show, x)
cstring(x...) = print_to_string(print, x...)

function cstring(p::Ptr{Uint8})
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_cstr_to_string, ByteString, (Ptr{Uint8},), p)
end

convert(::Type{Ptr{Uint8}}, s::String) = convert(Ptr{Uint8}, cstring(s))

## string promotion rules ##

promote_rule(::Type{UTF8String} , ::Type{ASCIIString}) = UTF8String
promote_rule(::Type{UTF8String} , ::Type{CharString} ) = UTF8String
promote_rule(::Type{ASCIIString}, ::Type{CharString} ) = UTF8String

## printing literal quoted string data ##

# TODO: this is really the inverse of print_unbackslashed

function print_quoted_literal(s::String)
    print('"')
    for c = s; c == '"' ? print("\\\"") : print(c); end
    print('"')
end

## string escaping & unescaping ##

escape_nul(s::String, i::Int) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? L"\x00" : L"\0"

is_hex_digit(c::Char) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'
need_full_hex(s::String, i::Int) = !done(s,i) && is_hex_digit(next(s,i)[1])

function print_escaped(s::String, esc::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'       ? print(escape_nul(s,j)) :
        c == '\e'       ? print(L"\e") :
        c == '\\'       ? print("\\\\") :
        contains(esc,c) ? print('\\', c) :
        iswprint(c)     ? print(c) :
        7 <= c <= 13    ? print('\\', "abtnvfr"[c-6]) :
        c <= '\x7f'     ? print(L"\x", hex(c, 2)) :
        c <= '\uffff'   ? print(L"\u", hex(c, need_full_hex(s,j) ? 4 : 2)) :
                          print(L"\U", hex(c, need_full_hex(s,j) ? 8 : 4))
        i = j
    end
end

escape_string(s::String) = print_to_string(length(s), print_escaped, s, "\"")
print_quoted(s::String) = (print('"'); print_escaped(s, "\"\$"); print('"'))
#"  # work around syntax highlighting problem
quote_string(s::String) = print_to_string(length(s)+2, print_quoted, s)

# bare minimum unescaping function unescapes only given characters

function print_unescaped_chars(s::String, esc::String)
    if !contains(esc,'\\')
        esc = strcat("\\", esc)
    end
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if c == '\\' && !done(s,i) && contains(esc,s[i])
            c, i = next(s,i)
        end
        print(c)
    end
end

unescape_chars(s::String, esc::String) =
    print_to_string(length(s), print_unescaped_chars, s, esc)

# general unescaping of traditional C and Unicode escape sequences

function print_unescaped(s::String)
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
                    write(uint8(n))
                else
                    print(char(n))
                end
            elseif '0' <= c <= '7'
                k = 1
                n = c-'0'
                while (k+=1) <= 3 && !done(s,i)
                    c, j = next(s,i)
                    n = '0' <= c <= '7' ? n<<3 + c-'0' : break
                    i = j
                end
                if n > 255
                    error("octal escape sequence out of range")
                end
                write(uint8(n))
            else
                print(c == 'a' ? '\a' :
                      c == 'b' ? '\b' :
                      c == 't' ? '\t' :
                      c == 'n' ? '\n' :
                      c == 'v' ? '\v' :
                      c == 'f' ? '\f' :
                      c == 'r' ? '\r' :
                      c == 'e' ? '\e' : c)
            end
        else
            print(c)
        end
    end
end

unescape_string(s::String) = print_to_string(length(s), print_unescaped, s)

## checking UTF-8 & ACSII validity ##

byte_string_classify(s::ByteString) =
    ccall(:u8_isvalid, Int32, (Ptr{Uint8}, Int), s.data, length(s))
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

is_valid_ascii(s::ByteString) = byte_string_classify(s) == 1
is_valid_utf8 (s::ByteString) = byte_string_classify(s) != 0

check_ascii(s::ByteString) = is_valid_ascii(s) ? s : error("invalid ASCII sequence")
check_utf8 (s::ByteString) = is_valid_utf8(s)  ? s : error("invalid UTF-8 sequence")

## string interpolation parsing ##

function _jl_interp_parse(s::String, unescape::Function, printer::Function)
    sx = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '$'
            if !isempty(s[i:j-1])
                push(sx, unescape(s[i:j-1]))
            end
            ex, j = parseatom(s,k)
            if isa(ex,Expr) && is(ex.head,:continue)
                throw(ParseError("incomplete expression"))
            end
            push(sx, ex)
            i = j
        elseif c == '\\' && !done(s,k)
            if s[k] == '$'
                if !isempty(s[i:j-1])
                    push(sx, unescape(s[i:j-1]))
                end
                i = k
            end
            c, j = next(s,k)
        else
            j = k
        end
    end
    if !isempty(s[i:])
        push(sx, unescape(s[i:j-1]))
    end
    length(sx) == 1 && isa(sx[1],ByteString) ? sx[1] :
        expr(:call, :print_to_string, printer, sx...)
end

_jl_interp_parse(s::String, u::Function) = _jl_interp_parse(s, u, print)
_jl_interp_parse(s::String) = _jl_interp_parse(s, x->check_utf8(unescape_string(x)))

function _jl_interp_parse_bytes(s::String)
    writer(x...) = for w=x; write(w); end
    _jl_interp_parse(s, unescape_string, writer)
end

## core string macros ##

macro   str(s); _jl_interp_parse(s); end
macro S_str(s); _jl_interp_parse(s); end
macro I_str(s); _jl_interp_parse(s, x->unescape_chars(x,"\"")); end
macro E_str(s); check_utf8(unescape_string(s)); end
macro B_str(s); _jl_interp_parse_bytes(s); end
macro b_str(s); ex = _jl_interp_parse_bytes(s); :(($ex).data); end

## shell-like command parsing ##

function _jl_shell_parse(s::String, interp::Bool)

    in_single_quotes = false
    in_double_quotes = false

    args = {}
    arg = {}
    i = start(s)
    j = i

    function update_arg(x)
        if !isa(x,String) || !isempty(x)
            push(arg, x)
        end
    end
    function append_arg()
        if isempty(arg); arg = {"",}; end
        push(args, arg)
        arg = {}
    end

    while !done(s,j)
        c, k = next(s,j)
        if !in_single_quotes && !in_double_quotes && iswspace(c)
            update_arg(s[i:j-1])
            append_arg()
            j = k
            while !done(s,j)
                c, k = next(s,j)
                if !iswspace(c)
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
            if iswspace(s[k])
                error("space not allowed right after \$")
            end
            ex, j = parseatom(s,j)
            update_arg(ex); i = j
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

    update_arg(s[i:])
    append_arg()

    if !interp
        return args
    end

    # construct an expression
    exprs = {}
    for arg in args
        push(exprs, expr(:tuple, arg))
    end
    expr(:tuple,exprs)
end
_jl_shell_parse(s::String) = _jl_shell_parse(s,true)

function shell_split(s::String)
    parsed = _jl_shell_parse(s,false)
    args = String[]
    for arg in parsed
       push(args, strcat(arg...))
    end
    args
end

function print_shell_word(word::String)
    if isempty(word)
        print("''")
    end
    has_single = false
    has_special = false
    for c in word
        if iswspace(c) || c=='\\' || c=='\'' || c=='"' || c=='$'
            has_special = true
            if c == '\''
                has_single = true
            end
        end
    end
    if !has_special
        print(word)
    elseif !has_single
        print('\'', word, '\'')
    else
        print('"')
        for c in word
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
    for arg in args
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
                    (Ptr{Uint8}, Int32, Int32),
                    s, pos-1, greedy ? 1:0)
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

function lpad(s::String, n::Integer, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    if l==1
        return p^m * s
    end
    q = div(m,l)
    r = m - q*l
    cstring(p^q*p[1:chr2ind(p,r)]*s)
end

function rpad(s::String, n::Integer, p::String)
    m = n - strlen(s)
    if m <= 0; return s; end
    l = strlen(p)
    if l==1
        return s * p^m
    end
    q = div(m,l)
    r = m - q*l
    cstring(s*p^q*p[1:chr2ind(p,r)])
end

lpad(s, n::Integer, p) = lpad(string(s), n, string(p))
rpad(s, n::Integer, p) = rpad(string(s), n, string(p))

lpad(s, n::Integer) = lpad(string(s), n, " ")
rpad(s, n::Integer) = rpad(string(s), n, " ")

# splitter can be a Char, Vector{Char}, String, Regex, ...
# any splitter that provides search(s::String, splitter)

function split(str::String, splitter, limit::Integer, keep_empty::Bool)
    strs = String[]
    i = start(str)
    n = length(str)
    j, k = search(str,splitter,i)
    while 0 < j <= n && length(strs) != limit-1
        if i < k
            if keep_empty || i < j
                push(strs, str[i:j-1])
            end
            i = k
        end
        if k <= j; k = nextind(str,j) end
        j, k = search(str,splitter,k)
    end
    if keep_empty || !done(str,i)
        push(strs, str[i:])
    end
    return strs
end
split(s::String, spl, n::Integer) = split(s, spl, n, true)
split(s::String, spl, keep::Bool) = split(s, spl, 0, keep)
split(s::String, spl)             = split(s, spl, 0, true)

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::String) = split(str, [' ','\t','\n','\v','\f','\r'], 0, false)

function replace(str::ByteString, splitter, repl::Function, limit::Integer)
    n = 1
    rstr = ""
    i = a = start(str)
    j, k = search(str,splitter,i)
    while j != 0
        if i == a || i < k
            rstr = RopeString(rstr,SubString(str,i,j-1))
            rstr = RopeString(rstr,string(repl(SubString(str,j,k-1))))
            i = k
        end
        if k <= j; k = nextind(str,j) end
        j, k = search(str,splitter,k)
        if n == limit break end
        n += 1
    end
    rstr = RopeString(rstr,SubString(str,i))
    print_to_string(length(rstr),print,rstr)
end
replace(s::String, spl, f::Function, n::Integer) = replace(cstring(s), spl, f, n)
replace(s::String, spl, r, n::Integer) = replace(s, spl, x->r, n)
replace(s::String, spl, r) = replace(s, spl, r, 0)

function print_joined(strings, delim, last)
    i = start(strings)
    if done(strings,i)
        return
    end
    str, i = next(strings,i)
    print(str)
    while !done(strings,i)
        str, i = next(strings,i)
        print(done(strings,i) ? last : delim)
        print(str)
    end
end

function print_joined(strings, delim)
    i = start(strings)
    while !done(strings,i)
        str, i = next(strings,i)
        print(str)
        if !done(strings,i)
            print(delim)
        end
    end
end
print_joined(strings) = print_joined(strings, "")

join(args...) = print_to_string(print_joined, args...)

chop(s::String) = s[1:thisind(s,length(s))-1]
chomp(s::String) = (i=thisind(s,length(s)); i>0 && s[i]=='\n' ? s[1:i-1] : s)
chomp(s::ByteString) = (isempty(s) || s.data[end]!=0x0a) ? s : s[1:end-1]

function lstrip(s::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !iswspace(c)
            return s[i:end]
        end
        i = j
    end
    ""
end

function rstrip(s::String)
    r = reverse(s)
    i = start(r)
    while !done(r,i)
        c, j = next(r,i)
        if !iswspace(c)
            return s[1:end-i+1]
        end
        i = j
    end
    ""
end

strip(s::String) = lstrip(rstrip(s))

## string to integer functions ##

function parse_int{T<:Integer}(::Type{T}, s::String, base::Integer)
    if !(2 <= base <= 36); error("invalid base: ",base); end
    i = start(s)
    while true
        if done(s,i)
            throw(ArgumentError(strcat(
                "premature end of integer (in ",show_to_string(s),")"
            )))
        end
        c,i = next(s,i)
        if !iswspace(c)
            break
        end
    end
    sgn = one(T)
    if T <: Signed && c == '-'
        sgn = -sgn
        if done(s,i)
            throw(ArgumentError(strcat(
                "premature end of integer (in ", show_to_string(s), ")"
            )))
        end
        c,i = next(s,i)
    elseif c == '+'
        if done(s,i)
            throw(ArgumentError(strcat(
                "premature end of integer (in ", show_to_string(s), ")"
            )))
        end
        c,i = next(s,i)
    end
    base = convert(T,base)
    n::T = 0
    while true
        d = '0' <= c <= '9' ? c-'0' :
            'A' <= c <= 'Z' ? c-'A'+10 :
            'a' <= c <= 'z' ? c-'a'+10 : typemax(Int)
        if d >= base
            if !iswspace(c)
                throw(ArgumentError(strcat(
                    show_to_string(c), " is not a valid digit (in ", show_to_string(s), ")"
                )))
            end
            while !done(s,i)
                c,i = next(s,i)
                if !iswspace(c)
                    throw(ArgumentError(strcat(
                        "extra characters after whitespace (in ", show_to_string(s), ")"
                    )))
                end
            end
        else
            # TODO: overflow detection?
            n = n*base + d
        end
        if done(s,i)
            break
        end
        c,i = next(s,i)
    end
    if T <: Signed
        return flipsign(n,sgn)
    else
        return n
    end
end

parse_int(s::String, base::Integer) = parse_int(Int,s,base)
parse_int(T::Type, s::String)       = parse_int(T,s,10)
parse_int(s::String)                = parse_int(Int,s,10)

parse_bin(T::Type, s::String) = parse_int(T,s,2)
parse_oct(T::Type, s::String) = parse_int(T,s,8)
parse_hex(T::Type, s::String) = parse_int(T,s,16)

parse_bin(s::String) = parse_int(Int,s,2)
parse_oct(s::String) = parse_int(Int,s,8)
parse_hex(s::String) = parse_int(Int,s,16)

integer (s::String) = int(s)
unsigned(s::String) = uint(s)
int     (s::String) = parse_int(Int,s)
uint    (s::String) = parse_int(Uint,s)
int8    (s::String) = parse_int(Int8,s)
uint8   (s::String) = parse_int(Uint8,s)
int16   (s::String) = parse_int(Int16,s)
uint16  (s::String) = parse_int(Uint16,s)
int32   (s::String) = parse_int(Int32,s)
uint32  (s::String) = parse_int(Uint32,s)
int64   (s::String) = parse_int(Int64,s)
uint64  (s::String) = parse_int(Uint64,s)

## integer to string functions ##

const _jl_dig_syms = "0123456789abcdefghijklmnopqrstuvwxyz".data

function int2str(n::Union(Int64,Uint64), b::Integer, l::Int)
    if b < 2 || b > 36; error("int2str: invalid base ", b); end
    neg = n < 0
    n = unsigned(abs(n))
    b = convert(typeof(n), b)
    ndig = ndigits(n, b)
    sz = max(convert(Int, ndig), l) + neg
    data = Array(Uint8, sz)
    i = sz
    if ispow2(b)
        digmask = b-1
        shift = trailing_zeros(b)
        while i > neg
            ch = n & digmask
            data[i] = _jl_dig_syms[int(ch)+1]
            n >>= shift
            i -= 1
        end
    else
        while i > neg
            ch = n % b
            data[i] = _jl_dig_syms[int(ch)+1]
            n = div(n,b)
            i -= 1
        end
    end
    if neg
        data[1] = '-'
    end
    ASCIIString(data)
end
int2str(n::Integer, b::Integer)         = int2str(n, b, 0)
int2str(n::Integer, b::Integer, l::Int) = int2str(int64(n), b, l)

string(x::Signed) = dec(int64(x))
cstring(x::Signed) = dec(int64(x))

## string to float functions ##

float64_isvalid(s::String, out::Array{Float64,1}) =
    ccall(:jl_strtod, Int32, (Ptr{Uint8},Ptr{Float64}), s, out) == 0
float32_isvalid(s::String, out::Array{Float32,1}) =
    ccall(:jl_strtof, Int32, (Ptr{Uint8},Ptr{Float32}), s, out) == 0

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
parse_float(x::String) = float64(x)
parse_float(::Type{Float64}, x::String) = float64(x)
parse_float(::Type{Float32}, x::String) = float32(x)

for conv in (:float, :float32, :float64,
             :int, :int8, :int16, :int32, :int64,
             :uint, :uint8, :uint16, :uint32, :uint64)
    @eval ($conv){S<:String}(a::AbstractArray{S}) = map($conv, a)
end

# lexicographically compare byte arrays (used by Latin-1 and UTF-8)

function lexcmp(a::Array{Uint8,1}, b::Array{Uint8,1})
    c = ccall(:memcmp, Int32, (Ptr{Uint8}, Ptr{Uint8}, Uint),
              a, b, min(length(a),length(b)))
    c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

# find the index of the first occurrence of a value in a byte array

function memchr(a::Array{Uint8,1}, b::Integer, i::Integer)
    if i < 1 error("index out of range") end
    n = length(a)
    if i > n return i == n+1 ? 0 : error("index out of range") end
    p = pointer(a)
    q = ccall(:memchr, Ptr{Uint8}, (Ptr{Uint8}, Int32, Uint), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : int(q-p+1)
end
memchr(a::Array{Uint8,1}, b::Integer) = memchr(a,b,1)

# concatenate byte arrays into a single array

memcat() = Array(Uint8,0)
memcat(a::Array{Uint8,1}) = copy(a)

function memcat(arrays::Array{Uint8,1}...)
    n = 0
    for a in arrays
        n += length(a)
    end
    arr = Array(Uint8, n)
    ptr = pointer(arr)
    offset = 0
    for a in arrays
        ccall(:memcpy, Ptr{Uint8}, (Ptr{Uint8}, Ptr{Uint8}, Uint),
              ptr+offset, a, length(a))
        offset += length(a)
    end
    return arr
end

# concatenate the data fields of byte strings

memcat(s::ByteString) = memcat(s.data)
memcat(sx::ByteString...) = memcat(map(s->s.data, sx)...)

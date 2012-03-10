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

symbol(s::String) = symbol(cstring(s))
string(s::String) = s

print(s::String) = for c=s; print(c); end
print(x...) = for i=x; print(i); end
println(args...) = print(args..., '\n')

show(s::String) = print_quoted(s)

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

function strchr(s::String, c::Char, i::Integer)
    i = nextind(s,i)
    while !done(s,i)
        d, j = next(s,i)
        if c == d
            return i
        end
        i = j
    end
    return 0
end
strchr(s::String, c::Char) = strchr(s, c, start(s))
contains(s::String, c::Char) = (strchr(s,c)!=0)

function chars(s::String)
    cx = Array(Char,strlen(s))
    i = 0
    for c in s
        cx[i += 1] = c
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

string(c::Char) = CharString(c)
string(c::Char, x::Char...) = CharString(c, x...)

## substrings reference original strings ##

type SubString <: String
    string::String
    offset::Int
    length::Int

    SubString(s::String, i::Int, j::Int) = new(s, i-1, j-i+1)
    SubString(s::SubString, i::Int, j::Int) =
        new(s.string, i-1+s.offset, j-i+1)
end
SubString(s::String, i::Integer, j::Integer) = SubString(s, int(i), int(j))

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
        error("in substring slice: index out of range")
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
    ccall(:jl_cstr_to_string, Any, (Ptr{Uint8},), p)::ByteString
end

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
                    cstring(s), pos-1, greedy ? 1:0)
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

function split(s::String, delims, include_empty::Bool)
    i = 1
    strs = String[]
    len = length(s)
    while true
        tokstart = tokend = i
        while !done(s,i)
            (c,i) = next(s,i)
            if contains(delims, c)
                break
            end
            tokend = i
        end
        tok = s[tokstart:(tokend-1)]
        if include_empty || !isempty(tok)
            push(strs, tok)
        end
        if !((i <= len) || (i==len+1 && tokend!=i))
            break
        end
    end
    strs
end

split(s::String) = split(s, (' ','\t','\n','\v','\f','\r'), false)
split(s::String, x) = split(s, x, true)
split(s::String, x::Char, incl::Bool) = split(s, (x,), incl)

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
chomp(s::String) = (i=thisind(s,length(s)); s[i]=='\n' ? s[1:i-1] : s)
chomp(s::ByteString) = s.data[end]==0x0a ? s[1:end-1] : s

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
    if done(s,i)
        error("premature end of integer (in ",show_to_string(s),")")
    end
    c,i = next(s,i)
    sgn = one(T)
    if T <: Signed && c == '-'
        sgn = -sgn
        if done(s,i)
            error("premature end of integer (in ",show_to_string(s),")")
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
            error(show_to_string(c)," is not a valid digit (in ",show_to_string(s),")")
        end
        # TODO: overflow detection?
        n = n*base + d
        if done(s,i)
            break
        end
        c,i = next(s,i)
    end
    return flipsign(n,sgn)
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

function float64_isvalid(s::String, out::Array{Float64,1})
    s = cstring(s)
    return (ccall(:jl_strtod, Int32, (Ptr{Uint8},Ptr{Float64}), s, out)==0)
end

function float32_isvalid(s::String, out::Array{Float32,1})
    s = cstring(s)
    return (ccall(:jl_strtof, Int32, (Ptr{Uint8},Ptr{Float32}), s, out)==0)
end

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

# copying a byte string (generally not needed due to "immutability")

strcpy{T<:ByteString}(s::T) = T(copy(s.data))

# lexicographically compare byte arrays (used by Latin-1 and UTF-8)

function lexcmp(a::Array{Uint8,1}, b::Array{Uint8,1})
    c = ccall(:memcmp, Int32, (Ptr{Uint8}, Ptr{Uint8}, Uint),
              a, b, min(length(a),length(b)))
    c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

# find the index of the first occurrence of a byte value in a byte array

function memchr(a::Array{Uint8,1}, b::Integer)
    p = pointer(a)
    q = ccall(:memchr, Ptr{Uint8}, (Ptr{Uint8}, Int32, Uint), p, b, length(a))
    q == C_NULL ? 0 : q - p + 1
end

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

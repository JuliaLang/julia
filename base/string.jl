## core text I/O ##

print(io::IO, x) = show(io, x)
print(io::IO, xs...) = for x in xs print(io, x) end
println(io::IO, xs...) = print(io, xs..., '\n')

print(xs...)   = print(OUTPUT_STREAM, xs...)
println(xs...) = println(OUTPUT_STREAM, xs...)

## core string functions ##

endof(s::String) = error("you must implement endof(", typeof(s), ")")
next(s::String, i::Int) = error("you must implement next(", typeof(s), ",Int)")
next(s::DirectIndexString, i::Int) = (s[i],i+1)
next(s::String, i::Integer) = next(s,int(i))

## conversion of general objects to strings ##

function print_to_string(xs...)
    s = memio(isa(xs[1],String) ? endof(xs[1]) : 0, false)
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

function bytestring(p::Ptr{Uint8})
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_cstr_to_string, ByteString, (Ptr{Uint8},), p)
end

function bytestring(p::Ptr{Uint8},len::Int)
    p == C_NULL ? error("cannot convert NULL to string") :
    ccall(:jl_pchar_to_string, ByteString, (Ptr{Uint8},Int), p, len)
end

convert(::Type{Array{Uint8,1}}, s::String) = bytestring(s).data
convert(::Type{Array{Uint8}}, s::String) = bytestring(s).data
convert(::Type{ByteString}, s::String) = bytestring(s)

## generic supplied functions ##

start(s::String) = 1
done(s::String,i) = (i > endof(s))
isempty(s::String) = done(s,start(s))
ref(s::String, i::Int) = next(s,i)[1]
ref(s::String, i::Integer) = s[int(i)]
ref(s::String, x::Real) = s[to_index(x)]
ref{T<:Integer}(s::String, r::Range1{T}) = s[int(first(r)):int(last(r))]
# TODO: handle other ranges with stride ±1 specially?
ref(s::String, v::AbstractVector) =
    sprint(length(v), io->(for i in v write(io,s[i]) end))

symbol(s::String) = symbol(bytestring(s))

print(io::IO, s::String) = for c in s write(io, c) end
write(io::IO, s::String) = print(io, s)
show(io::IO, s::String) = print_quoted(io, s)

(*)(s::Union(String,Char)...) = string(s...)
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
    for j = i+1:endof(s)
        if isvalid(s,j)
            return j
        end
    end
    endof(s)+1 # out of range
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
        return 1 <= i <= endof(s)+1 ? i :
               i == endof(s)+2      ? 0 :
               error(BoundsError)
    end

    if i < 1 error(BoundsError) end
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
search(s::String, c::Chars) = search(s,c,start(s))

contains(s::String, c::Char) = (search(s,c)!=0)

function search(s::String, t::String, i::Integer)
    if isempty(t)
        return 1 <= i <= endof(s)+1 ? (i:i-1) :
               i == endof(s)+2      ? (0:-1) :
               error(BoundsError)
    end
    t1, j2 = next(t,start(t))
    while true
        i = search(s,t1,i)
        if i == 0 return (0:-1) end
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
            return i:k-1
        end
        i = ii
    end
end
search(s::String, t::String) = search(s,t,start(s))

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

hash(s::String) = hash(bytestring(s))

# begins with and ends with predicates

function begins_with(a::String, b::String)
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d return false end
    end
    done(b,i)
end
begins_with(a::String, c::Char) = !isempty(a) && a[start(a)] == c

function ends_with(a::String, b::String)
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
ends_with(a::String, c::Char) = !isempty(a) && a[end] == c

# faster comparisons for byte strings

cmp(a::ByteString, b::ByteString)     = cmp(a.data, b.data)
isequal(a::ByteString, b::ByteString) = endof(a)==endof(b) && cmp(a,b)==0

# TODO: fast begins_with and ends_with

## character column width function ##

charwidth(c::Char) = max(0,int(ccall(:wcwidth, Int32, (Char,), c)))
strwidth(s::String) = (w=0; for c in s; w += charwidth(c); end; w)
strwidth(s::ByteString) = ccall(:u8_strwidth, Int, (Ptr{Uint8},), s.data)
# TODO: implement and use u8_strnwidth that takes a length argument

## libc character class predicates ##

isascii(c::Char) = c < 0x80

for name = ("alnum", "alpha", "blank", "cntrl", "digit", "graph",
            "lower", "print", "punct", "space", "upper")
    f = symbol(string("is",name))
    @eval ($f)(c::Char) = bool(ccall($(string("isw",name)), Int32, (Char,), c))
end

## generic string uses only endof and next ##

type GenericString <: String
    string::String
end

endof(s::GenericString) = endof(s.string)
next(s::GenericString, i::Int) = next(s.string, i)

## plain old character arrays ##

type CharString <: DirectIndexString
    chars::Array{Char,1}

    CharString(a::Array{Char,1}) = new(a)
    CharString(c::Char...) = new([ c[i] for i=1:length(c) ])
end
CharString(x...) = CharString(map(char,x)...)

next(s::CharString, i::Int) = (s.chars[i], i+1)
endof(s::CharString) = length(s.chars)
length(s::CharString) = length(s.chars)

## substrings reference original strings ##

type SubString{T<:String} <: String
    string::T
    offset::Int
    endof::Int

    SubString(s::T, i::Int, j::Int) =
        (o=nextind(s,i-1)-1; new(s,o,nextind(s,j)-o-1))
end
SubString{T<:String}(s::T, i::Int, j::Int) = SubString{T}(s, i, j)
SubString(s::SubString, i::Int, j::Int) = SubString(s.string, s.offset+i, s.offset+j)
SubString(s::String, i::Integer, j::Integer) = SubString(s, int(i), int(j))
SubString(s::String, i::Integer) = SubString(s, i, endof(s))

write{T<:ByteString}(to::IOBuffer, s::SubString{T}) = write_sub(to, s.string.data, s.offset+1, s.endof)

function next(s::SubString, i::Int)
    if i < 1 || i > s.endof
        error(BoundsError)
    end
    c, i = next(s.string, i+s.offset)
    c, i-s.offset
end

endof(s::SubString) = s.endof
# TODO: length(s::SubString) = ??
# default implementation will work but it's slow
# can this be delegated efficiently somehow?
# that may require additional string interfaces

function ref(s::String, r::Range1{Int})
    if first(r) < 1 || endof(s) < last(r)
        error(BoundsError)
    end
    SubString(s, first(r), last(r))
end

## efficient representation of repeated strings ##

type RepString <: String
    string::String
    repeat::Integer
end

endof(s::RepString)  = endof(s.string)*s.repeat
length(s::RepString) = length(s.string)*s.repeat

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

## reversed strings without data movement ##

type RevString <: String
    string::String
end

endof(s::RevString) = endof(s.string)
length(s::RevString) = length(s.string)

function next(s::RevString, i::Int)
    n = endof(s); j = n-i+1
    (s.string[j], n-thisind(s.string,j-1)+1)
end

reverse(s::String) = RevString(s)
reverse(s::RevString) = s.string

## ropes for efficient concatenation, etc. ##

type RopeString <: String
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
print(io::IO, s::RopeString) = print(io, s.head, s.tail)
write(io::IO, s::RopeString) = (write(io, s.head); write(io, s.tail))

## uppercase and lowercase transformations ##

uppercase(c::Char) = ccall(:towupper, Char, (Char,), c)
lowercase(c::Char) = ccall(:towlower, Char, (Char,), c)

uppercase(c::Uint8) = ccall(:toupper, Uint8, (Uint8,), c)
lowercase(c::Uint8) = ccall(:tolower, Uint8, (Uint8,), c)

uppercase(s::String) = map(uppercase, s)
lowercase(s::String) = map(lowercase, s)

ucfirst(s::String) = isupper(s[1]) ? s : string(uppercase(s[1]),s[nextind(s,1):end])
lcfirst(s::String) = islower(s[1]) ? s : string(lowercase(s[1]),s[nextind(s,1):end])

## string map, filter, has ##

function map(f::Function, s::String)
    out = memio(endof(s))
    for c in s
        write(out, f(c)::Char)
    end
    takebuf_string(out)
end

function filter(f::Function, s::String)
    out = memio(endof(s))
    for c in s
        if f(c)
            write(out, c)
        end
    end
    takebuf_string(out)
end

## string promotion rules ##

promote_rule(::Type{UTF8String} , ::Type{ASCIIString}) = UTF8String
promote_rule(::Type{UTF8String} , ::Type{CharString} ) = UTF8String
promote_rule(::Type{ASCIIString}, ::Type{CharString} ) = UTF8String
promote_rule{T<:String}(::Type{RepString}, ::Type{T}) = RepString

## printing literal quoted string data ##

# TODO: this is really the inverse of print_unbackslashed

function print_quoted_literal(io, s::String)
    print(io, '"')
    for c = s; c == '"' ? print(io, "\\\"") : print(io, c); end
    print(io, '"')
end

## string escaping & unescaping ##

escape_nul(s::String, i::Int) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? L"\x00" : L"\0"

isxdigit(c::Char) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'
need_full_hex(s::String, i::Int) = !done(s,i) && isxdigit(next(s,i)[1])

function print_escaped(io, s::String, esc::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'       ? print(io, escape_nul(s,j)) :
        c == '\e'       ? print(io, L"\e") :
        c == '\\'       ? print(io, "\\\\") :
        contains(esc,c) ? print(io, '\\', c) :
        7 <= c <= 13    ? print(io, '\\', "abtnvfr"[int(c-6)]) :
        isprint(c)      ? print(io, c) :
        c <= '\x7f'     ? print(io, L"\x", hex(c, 2)) :
        c <= '\uffff'   ? print(io, L"\u", hex(c, need_full_hex(s,j) ? 4 : 2)) :
                          print(io, L"\U", hex(c, need_full_hex(s,j) ? 8 : 4))
        i = j
    end
end

escape_string(s::String) = sprint(endof(s), print_escaped, s, "\"")
function print_quoted(io, s::String)
    print(io, '"')
    print_escaped(io, s, "\"\$") #"# work around syntax highlighting problem
    print(io, '"')
end
quote_string(s::String) = sprint(endof(s)+2, io->print_quoted(io,s))

# bare minimum unescaping function unescapes only given characters

function print_unescaped_chars(io, s::String, esc::String)
    if !contains(esc,'\\')
        esc = string("\\", esc)
    end
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if c == '\\' && !done(s,i) && contains(esc,s[i])
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

is_valid_ascii(s::ByteString) = byte_string_classify(s) == 1
is_valid_utf8 (s::ByteString) = byte_string_classify(s) != 0

check_ascii(s::ByteString) = is_valid_ascii(s) ? s : error("invalid ASCII sequence")
check_utf8 (s::ByteString) = is_valid_utf8(s)  ? s : error("invalid UTF-8 sequence")

## string interpolation parsing ##

function interp_parse(s::String, unescape::Function, printer::Function)
    sx = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '$'
            if !isempty(s[i:j-1])
                push!(sx, unescape(s[i:j-1]))
            end
            ex, j = parse(s,k,false)
            if isa(ex,Expr) && is(ex.head,:continue)
                throw(ParseError("incomplete expression"))
            end
            push!(sx, esc(ex))
            i = j
        elseif c == '\\' && !done(s,k)
            if s[k] == '$'
                if !isempty(s[i:j-1])
                    push!(sx, unescape(s[i:j-1]))
                end
                i = k
            end
            c, j = next(s,k)
        else
            j = k
        end
    end
    if !isempty(s[i:])
        push!(sx, unescape(s[i:j-1]))
    end
    length(sx) == 1 && isa(sx[1],ByteString) ? sx[1] :
        expr(:call, :sprint, printer, sx...)
end

interp_parse(s::String, u::Function) = interp_parse(s, u, print)
interp_parse(s::String) = interp_parse(s, x->check_utf8(unescape_string(x)))

function interp_parse_bytes(s::String)
    writer(io,x...) = for w=x; write(io,w); end
    interp_parse(s, unescape_string, writer)
end

## core string macros ##

macro   str(s); interp_parse(s); end
macro I_str(s); interp_parse(s, x->unescape_chars(x,"\"")); end
macro E_str(s); check_utf8(unescape_string(s)); end
macro B_str(s); interp_parse_bytes(s); end
macro b_str(s); ex = interp_parse_bytes(s); :(($ex).data); end

## shell-like command parsing ##

function shell_parse(raw::String, interp::Bool)

    s = strip(raw)
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
            ex, j = parse(s,j,false)
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

    update_arg(s[i:])
    append_arg()

    if !interp
        return args
    end

    # construct an expression
    exprs = {}
    for arg in args
        push!(exprs, expr(:tuple, arg))
    end
    expr(:tuple,exprs)
end
shell_parse(s::String) = shell_parse(s,true)

function shell_split(s::String)
    parsed = shell_parse(s,false)
    args = String[]
    for arg in parsed
       push!(args, string(arg...))
    end
    args
end

function print_shell_word(io, word::String)
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

function print_shell_escaped(io, cmd::String, args::String...)
    print_shell_word(io, cmd)
    for arg in args
        print(io, ' ')
        print_shell_word(io, arg)
    end
end

shell_escape(cmd::String, args::String...) =
    sprint(print_shell_escaped, cmd, args...)

## interface to parser ##

function parse(str::String, pos::Int, greedy::Bool)
    # returns (expr, end_pos). expr is () in case of parse error.
    ex, pos = ccall(:jl_parse_string, Any,
                    (Ptr{Uint8}, Int32, Int32),
                    str, pos-1, greedy ? 1:0)
    if isa(ex,Expr) && is(ex.head,:error)
        throw(ParseError(ex.args[1]))
    end
    if ex == (); throw(ParseError("end of input")); end
    ex, pos+1 # C is zero-based, Julia is 1-based
end
parse(str::String, pos::Int) = parse(str, pos, true)

function parse(str::String)
    ex, pos = parse(str, start(str))
    done(str, pos) || error("syntax: extra token after end of expression")
    return ex
end

## miscellaneous string functions ##

function lpad(s::String, n::Integer, p::String)
    m = n - length(s)
    if m <= 0; return s; end
    l = length(p)
    if l==1
        return bytestring(p^m * s)
    end
    q = div(m,l)
    r = m - q*l
    bytestring(p^q*p[1:chr2ind(p,r)]*s)
end

function rpad(s::String, n::Integer, p::String)
    m = n - length(s)
    if m <= 0; return s; end
    l = length(p)
    if l==1
        return bytestring(s * p^m)
    end
    q = div(m,l)
    r = m - q*l
    bytestring(s*p^q*p[1:chr2ind(p,r)])
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
    n = endof(str)
    r = search(str,splitter,i)
    j, k = first(r), last(r)+1
    while 0 < j <= n && length(strs) != limit-1
        if i < k
            if keep_empty || i < j
                push!(strs, str[i:j-1])
            end
            i = k
        end
        if k <= j; k = nextind(str,j) end
        r = search(str,splitter,k)
        j, k = first(r), last(r)+1
    end
    if keep_empty || !done(str,i)
        push!(strs, str[i:])
    end
    return strs
end
split(s::String, spl, n::Integer) = split(s, spl, n, true)
split(s::String, spl, keep::Bool) = split(s, spl, 0, keep)
split(s::String, spl)             = split(s, spl, 0, true)

# a bit oddball, but standard behavior in Perl, Ruby & Python:
const _default_delims = [' ','\t','\n','\v','\f','\r']
split(str::String) = split(str, _default_delims, 0, false)

function replace(str::ByteString, pattern, repl::Function, limit::Integer)
    n = 1
    rstr = ""
    i = a = start(str)
    r = search(str,pattern,i)
    j, k = first(r), last(r)+1
    out = IOBuffer()
    while j != 0
        if i == a || i < k
            write(out, SubString(str,i,j-1))
            write(out, string(repl(SubString(str,j,k-1))))
            i = k
        end
        if k <= j; k = nextind(str,j) end
        r = search(str,pattern,k)
        j, k = first(r), last(r)+1
        if n == limit break end
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

function lstrip(s::String)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !isspace(c)
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
        if !isspace(c)
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
            throw(ArgumentError(string(
                "premature end of integer (in ", repr(s) ,")"
            )))
        end
        c,i = next(s,i)
        if !isspace(c)
            break
        end
    end
    sgn = one(T)
    if T <: Signed && c == '-'
        sgn = -sgn
        if done(s,i)
            throw(ArgumentError(string(
                "premature end of integer (in ", repr(s), ")"
            )))
        end
        c,i = next(s,i)
    elseif c == '+'
        if done(s,i)
            throw(ArgumentError(string(
                "premature end of integer (in ", repr(s), ")"
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
            if !isspace(c)
                throw(ArgumentError(string(
                    repr(c)," is not a valid digit (in ", repr(s), ")"
                )))
            end
            while !done(s,i)
                c,i = next(s,i)
                if !isspace(c)
                    throw(ArgumentError(string(
                        "extra characters after whitespace (in ", repr(s), ")"
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
int128  (s::String) = parse_int(Int128,s)
uint128 (s::String) = parse_int(Uint128,s)

## stringifying integers more efficiently ##

string(x::Union(Int8,Int16,Int32,Int64,Int128)) = dec(x)

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

# find the index of the first occurrence of a value in a byte array

function search(a::Array{Uint8,1}, b, i::Integer)
    if i < 1 error(BoundsError) end
    n = length(a)
    if i > n return i == n+1 ? 0 : error(BoundsError) end
    p = pointer(a)
    q = ccall(:memchr, Ptr{Uint8}, (Ptr{Uint8}, Int32, Uint), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : int(q-p+1)
end
search(a::Array{Uint8,1}, b) = search(a,b,1)

# return a random string (often useful for temporary filenames/dirnames)
let
    global randstring
    const b = uint8(['0':'9','A':'Z','a':'z'])
    randstring(n::Int) = ASCIIString(b[rand(1:length(b),n)])
    randstring() = randstring(8)
end

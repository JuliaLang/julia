# This file is a part of Julia. License is MIT: http://julialang.org/license

# starts with and ends with predicates

"""
    startswith(s::AbstractString, prefix::AbstractString)

Returns `true` if `s` starts with `prefix`. If `prefix` is a vector or set
of characters, tests whether the first character of `s` belongs to that set.

See also [`endswith`](@ref).

```jldoctest
julia> startswith("JuliaLang", "Julia")
true
```
"""
function startswith(a::AbstractString, b::AbstractString)
    i = start(a)
    j = start(b)
    while !done(a,i) && !done(b,i)
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d return false end
    end
    done(b,i)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && first(str) in chars

"""
    endswith(s::AbstractString, suffix::AbstractString)

Returns `true` if `s` ends with `suffix`. If `suffix` is a vector or set of
characters, tests whether the last character of `s` belongs to that set.

See also [`startswith`](@ref).

```jldoctest
julia> endswith("Sunday", "day")
true
```
"""
function endswith(a::AbstractString, b::AbstractString)
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
endswith(str::AbstractString, chars::Chars) = !isempty(str) && last(str) in chars

startswith(a::String, b::String) =
    (a.len >= b.len && ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, b.len) == 0)
startswith(a::Vector{UInt8}, b::Vector{UInt8}) =
    (length(a) >= length(b) && ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, length(b)) == 0)

# TODO: fast endswith

"""
    chop(s::AbstractString)

Remove the last character from `s`.

```jldoctest
julia> a = "March"
"March"

julia> chop(a)
"Marc"
```
"""
chop(s::AbstractString) = SubString(s, 1, endof(s)-1)

"""
    chomp(s::AbstractString)

Remove a single trailing newline from a string.

```jldoctest
julia> chomp("Hello\n")
"Hello"
```
"""
function chomp(s::AbstractString)
    i = endof(s)
    if (i < 1 || s[i] != '\n') return SubString(s, 1, i) end
    j = prevind(s,i)
    if (j < 1 || s[j] != '\r') return SubString(s, 1, i-1) end
    return SubString(s, 1, j-1)
end
function chomp(s::String)
    i = endof(s)
    if i < 1 || codeunit(s,i) != 0x0a
        SubString(s, 1, i)
    elseif i < 2 || codeunit(s,i-1) != 0x0d
        SubString(s, 1, i-1)
    else
        SubString(s, 1, i-2)
    end
end

# NOTE: use with caution -- breaks the immutable string convention!
# TODO: this is hard to provide with the new representation
#function chomp!(s::String)
#    if !isempty(s) && codeunit(s,s.len) == 0x0a
#        n = (endof(s) < 2 || s.data[end-1] != 0x0d) ? 1 : 2
#        ccall(:jl_array_del_end, Void, (Any, UInt), s.data, n)
#    end
#    return s
#end
chomp!(s::AbstractString) = chomp(s) # copying fallback for other string types

const _default_delims = [' ','\t','\n','\v','\f','\r']

"""
    lstrip(s::AbstractString[, chars::Chars])

Return `s` with any leading whitespace and delimiters removed.
The default delimiters to remove are `' '`, `\\t`, `\\n`, `\\v`,
`\\f`, and `\\r`.
If `chars` (a character, or vector or set of characters) is provided,
instead remove characters contained in it.

```jldoctest
julia> a = lpad("March", 20)
"               March"

julia> lstrip(a)
"March"
```
"""
function lstrip(s::AbstractString, chars::Chars=_default_delims)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !(c in chars)
            return s[i:end]
        end
        i = j
    end
    s[end+1:end]
end

"""
    rstrip(s::AbstractString[, chars::Chars])

Return `s` with any trailing whitespace and delimiters removed.
The default delimiters to remove are `' '`, `\\t`, `\\n`, `\\v`,
`\\f`, and `\\r`.
If `chars` (a character, or vector or set of characters) is provided,
instead remove characters contained in it.

```jldoctest
julia> a = rpad("March", 20)
"March               "

julia> rstrip(a)
"March"
```
"""
function rstrip(s::AbstractString, chars::Chars=_default_delims)
    r = RevString(s)
    i = start(r)
    while !done(r,i)
        c, j = next(r,i)
        if !(c in chars)
            return s[1:end-i+1]
        end
        i = j
    end
    s[1:0]
end

"""
    strip(s::AbstractString, [chars::Chars])

Return `s` with any leading and trailing whitespace removed.
If `chars` (a character, or vector or set of characters) is provided,
instead remove characters contained in it.

```jldoctest
julia> strip("{3, 5}\n", ['{', '}', '\n'])
"3, 5"
```
"""
strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)

## string padding functions ##

function lpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return string(p^m, s)
    end
    q = div(m,l)
    r = m - q*l
    i = r != 0 ? chr2ind(p, r) : -1
    string(p^q, p[1:i], s)
end

function rpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - strwidth(s)
    if m <= 0; return s; end
    l = strwidth(p)
    if l==1
        return string(s, p^m)
    end
    q = div(m,l)
    r = m - q*l
    i = r != 0 ? chr2ind(p, r) : -1
    string(s, p^q, p[1:i])
end

"""
    lpad(s, n::Integer, p::AbstractString=" ")

Make a string at least `n` columns wide when printed by padding `s` on the left
with copies of `p`.

```jldoctest
julia> lpad("March",10)
"     March"
```
"""
lpad(s, n::Integer, p=" ") = lpad(string(s),n,string(p))

"""
    rpad(s, n::Integer, p::AbstractString=" ")

Make a string at least `n` columns wide when printed by padding `s` on the right
with copies of `p`.

```jldoctest
julia> rpad("March",20)
"March               "
```
"""
rpad(s, n::Integer, p=" ") = rpad(string(s),n,string(p))
cpad(s, n::Integer, p=" ") = rpad(lpad(s,div(n+strwidth(s),2),p),n,p)

# splitter can be a Char, Vector{Char}, AbstractString, Regex, ...
# any splitter that provides search(s::AbstractString, splitter)
split{T<:SubString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _split(str, splitter, limit, keep, T[])

"""
    split(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

Return an array of substrings by splitting the given string on occurrences of the given
character delimiters, which may be specified in any of the formats allowed by `search`'s
second argument (i.e. a single character, collection of characters, string, or regular
expression). If `chars` is omitted, it defaults to the set of all space characters, and
`keep` is taken to be `false`. The two keyword arguments are optional: they are a
maximum size for the result and a flag determining whether empty fields should be kept in
the result.

```jldoctest
julia> a = "Ma.rch"
"Ma.rch"

julia> split(a,".")
2-element Array{SubString{String},1}:
 "Ma"
 "rch"
```
"""
split{T<:AbstractString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _split(str, splitter, limit, keep, SubString{T}[])
function _split{T<:AbstractString,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
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

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::AbstractString) = split(str, _default_delims; limit=0, keep=false)

rsplit{T<:SubString}(str::T, splitter; limit::Integer=0, keep::Bool=true) = _rsplit(str, splitter, limit, keep, T[])

"""
    rsplit(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

Similar to [`split`](@ref), but starting from the end of the string.

```jldoctest
julia> a = "M.a.r.c.h"
"M.a.r.c.h"

julia> rsplit(a,".")
5-element Array{SubString{String},1}:
 "M"
 "a"
 "r"
 "c"
 "h"

julia> rsplit(a,".";limit=1)
1-element Array{SubString{String},1}:
 "M.a.r.c.h"

julia> rsplit(a,".";limit=2)
2-element Array{SubString{String},1}:
 "M.a.r.c"
 "h"
```
"""
rsplit{T<:AbstractString}(str::T, splitter   ; limit::Integer=0, keep::Bool=true) = _rsplit(str, splitter, limit, keep, SubString{T}[])
function _rsplit{T<:AbstractString,U<:Array}(str::T, splitter, limit::Integer, keep_empty::Bool, strs::U)
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
#rsplit(str::AbstractString) = rsplit(str, _default_delims, 0, false)

_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))

function replace(str::String, pattern, repl, limit::Integer)
    n = 1
    e = endof(str)
    i = a = start(str)
    r = search(str,pattern,i)
    j, k = first(r), last(r)
    out = IOBuffer(StringVector(floor(Int, 1.2sizeof(str))), true, true)
    out.size = 0
    out.ptr = 1
    while j != 0
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace(out, repl, str, r, pattern)
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
    String(take!(out))
end

"""
    replace(string::AbstractString, pat, r[, n::Integer=0])

Search for the given pattern `pat`, and replace each occurrence with `r`. If `n` is
provided, replace at most `n` occurrences. As with search, the second argument may be a
single character, a vector or a set of characters, a string, or a regular expression. If `r`
is a function, each occurrence is replaced with `r(s)` where `s` is the matched substring.
If `pat` is a regular expression and `r` is a `SubstitutionString`, then capture group
references in `r` are replaced with the corresponding matched text.
"""
replace(s::AbstractString, pat, f, n::Integer) = replace(String(s), pat, f, n)
replace(s::AbstractString, pat, r) = replace(s, pat, r, 0)

# hex <-> bytes conversion

"""
    hex2bytes(s::AbstractString)

Convert an arbitrarily long hexadecimal string to its binary representation. Returns an
`Array{UInt8,1}`, i.e. an array of bytes.

```jldoctest
julia> a = hex(12345)
"3039"

julia> hex2bytes(a)
2-element Array{UInt8,1}:
 0x30
 0x39
```
"""
function hex2bytes(s::AbstractString)
    a = zeros(UInt8, div(endof(s), 2))
    i, j = start(s), 0
    while !done(s, i)
        c, i = next(s, i)
        n = '0' <= c <= '9' ? c - '0' :
            'a' <= c <= 'f' ? c - 'a' + 10 :
            'A' <= c <= 'F' ? c - 'A' + 10 :
            throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        done(s, i) &&
            throw(ArgumentError("string length must be even: length($(repr(s))) == $(length(s))"))
        c, i = next(s, i)
        n = '0' <= c <= '9' ? n << 4 + c - '0' :
            'a' <= c <= 'f' ? n << 4 + c - 'a' + 10 :
            'A' <= c <= 'F' ? n << 4 + c - 'A' + 10 :
            throw(ArgumentError("not a hexadecimal string: $(repr(s))"))
        a[j += 1] = n
    end
    resize!(a, j)
    return a
end

"""
    bytes2hex(bin_arr::Array{UInt8, 1}) -> String

Convert an array of bytes to its hexadecimal representation.
All characters are in lower-case.

```jldoctest
julia> a = hex(12345)
"3039"

julia> b = hex2bytes(a)
2-element Array{UInt8,1}:
 0x30
 0x39

julia> bytes2hex(b)
"3039"
```
"""
function bytes2hex(a::AbstractArray{UInt8})
    b = Vector{UInt8}(2*length(a))
    i = 0
    for x in a
        b[i += 1] = hex_chars[1 + x >> 4]
        b[i += 1] = hex_chars[1 + x & 0xf]
    end
    return String(b)
end

# check for pure ASCII-ness

function ascii(s::String)
    for (i, b) in enumerate(Vector{UInt8}(s))
        b < 0x80 || throw(ArgumentError("invalid ASCII at index $i in $(repr(s))"))
    end
    return s
end

"""
    ascii(s::AbstractString)

Convert a string to `String` type and check that it contains only ASCII data, otherwise
throwing an `ArgumentError` indicating the position of the first non-ASCII byte.

```jldoctest
julia> ascii("abcdeγfgh")
ERROR: ArgumentError: invalid ASCII at index 6 in "abcdeγfgh"
Stacktrace:
 [1] ascii(::String) at ./strings/util.jl:473

julia> ascii("abcdefgh")
"abcdefgh"
```
"""
ascii(x::AbstractString) = ascii(convert(String, x))

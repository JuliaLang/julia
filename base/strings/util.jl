# This file is a part of Julia. License is MIT: https://julialang.org/license

# starts with and ends with predicates

"""
    startswith(s::AbstractString, prefix::AbstractString)

Returns `true` if `s` starts with `prefix`. If `prefix` is a vector or set
of characters, tests whether the first character of `s` belongs to that set.

See also [`endswith`](@ref).

# Examples
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
        (c != d) && (return false)
    end
    done(b,i)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && first(str) in chars

"""
    endswith(s::AbstractString, suffix::AbstractString)

Returns `true` if `s` ends with `suffix`. If `suffix` is a vector or set of
characters, tests whether the last character of `s` belongs to that set.

See also [`startswith`](@ref).

# Examples
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
        (c != d) && (return false)
        i = prevind(a,i)
        j = prevind(b,j)
    end
    j < b1
end
endswith(str::AbstractString, chars::Chars) = !isempty(str) && last(str) in chars

startswith(a::String, b::String) =
    (sizeof(a) >= sizeof(b) && ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, sizeof(b)) == 0)
startswith(a::Vector{UInt8}, b::Vector{UInt8}) =
    (length(a) >= length(b) && ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, length(b)) == 0)

# TODO: fast endswith

"""
    chop(s::AbstractString, head::Integer=0, tail::Integer=1)

Remove the first `head` and the last `tail` characters from `s`.
The call `chop(s)` removes the last character from `s`.
If it is requested to remove more characters than `length(s)`
then an empty string is returned.

# Examples
```jldoctest
julia> a = "March"
"March"

julia> chop(a)
"Marc"

julia> chop(a, 1, 2)
"ar"

julia> chop(a, 5, 5)
""
```
"""
function chop(s::AbstractString, head::Integer, tail::Integer)
    # negative values of head/tail will throw error in nextind/prevind
    headidx = head == 0 ? start(s) : nextind(s, start(s), head)
    tailidx = tail == 0 ? endof(s) : prevind(s, endof(s), tail)
    SubString(s, headidx, tailidx)
end

# no head/tail version left for performance reasons
chop(s::AbstractString) = SubString(s, start(s), prevind(s, endof(s)))

"""
    chomp(s::AbstractString)

Remove a single trailing newline from a string.

# Examples
```jldoctest
julia> chomp("Hello\\n")
"Hello"
```
"""
function chomp(s::AbstractString)
    i = endof(s)
    (i < 1 || s[i] != '\n') && (return SubString(s, 1, i))
    j = prevind(s,i)
    (j < 1 || s[j] != '\r') && (return SubString(s, 1, j))
    return SubString(s, 1, prevind(s,j))
end
function chomp(s::String)
    i = endof(s)
    if i < 1 || codeunit(s,i) != 0x0a
        SubString(s, 1, i)
    elseif i < 2 || codeunit(s,i-1) != 0x0d
        SubString(s, 1, prevind(s, i))
    else
        SubString(s, 1, prevind(s, i-1))
    end
end

# NOTE: use with caution -- breaks the immutable string convention!
# TODO: this is hard to provide with the new representation
#function chomp!(s::String)
#    if !isempty(s) && codeunit(s,sizeof(s)) == 0x0a
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

# Examples
```jldoctest
julia> a = lpad("March", 20)
"               March"

julia> lstrip(a)
"March"
```
"""
function lstrip(s::AbstractString, chars::Chars=_default_delims)
    e = endof(s)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        if !(c in chars)
            return SubString(s, i, e)
        end
        i = j
    end
    SubString(s, e+1, e)
end

"""
    rstrip(s::AbstractString[, chars::Chars])

Return `s` with any trailing whitespace and delimiters removed.
The default delimiters to remove are `' '`, `\\t`, `\\n`, `\\v`,
`\\f`, and `\\r`.
If `chars` (a character, or vector or set of characters) is provided,
instead remove characters contained in it.

# Examples
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
            return SubString(s, 1, endof(s)-i+1)
        end
        i = j
    end
    SubString(s, 1, 0)
end

"""
    strip(s::AbstractString, [chars::Chars])

Return `s` with any leading and trailing whitespace removed.
If `chars` (a character, or vector or set of characters) is provided,
instead remove characters contained in it.

# Examples
```jldoctest
julia> strip("{3, 5}\\n", ['{', '}', '\\n'])
"3, 5"
```
"""
strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)

## string padding functions ##

function lpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - textwidth(s)
    (m <= 0) && (return s)
    l = textwidth(p)
    if l==1
        return string(p^m, s)
    end
    q = div(m,l)
    r = m - q*l
    i = r != 0 ? chr2ind(p, r) : -1
    string(p^q, p[1:i], s)
end

function rpad(s::AbstractString, n::Integer, p::AbstractString=" ")
    m = n - textwidth(s)
    (m <= 0) && (return s)
    l = textwidth(p)
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

# Examples
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

# Examples
```jldoctest
julia> rpad("March",20)
"March               "
```
"""
rpad(s, n::Integer, p=" ") = rpad(string(s),n,string(p))

# splitter can be a Char, Vector{Char}, AbstractString, Regex, ...
# any splitter that provides search(s::AbstractString, splitter)
split(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:SubString} =
    _split(str, splitter, limit, keep, T[])

"""
    split(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

Return an array of substrings by splitting the given string on occurrences of the given
character delimiters, which may be specified in any of the formats allowed by `search`'s
second argument (i.e. a single character, collection of characters, string, or regular
expression). If `chars` is omitted, it defaults to the set of all space characters, and
`keep` is taken to be `false`. The two keyword arguments are optional: they are a
maximum size for the result and a flag determining whether empty fields should be kept in
the result.

# Examples
```jldoctest
julia> a = "Ma.rch"
"Ma.rch"

julia> split(a,".")
2-element Array{SubString{String},1}:
 "Ma"
 "rch"
```
"""
split(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _split(str, splitter, limit, keep, SubString{T}[])
function _split(str::AbstractString, splitter, limit::Integer, keep_empty::Bool, strs::Array)
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
        (k <= j) && (k = nextind(str,j))
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

rsplit(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:SubString} =
    _rsplit(str, splitter, limit, keep, T[])

"""
    rsplit(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

Similar to [`split`](@ref), but starting from the end of the string.

# Examples
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
rsplit(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _rsplit(str, splitter, limit, keep, SubString{T}[])
function _rsplit(str::AbstractString, splitter, limit::Integer, keep_empty::Bool, strs::Array)
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

# TODO: rename to `replace` when `replace` is removed from deprecated.jl
function replace_new(str::String, pattern, repl, count::Integer)
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
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
        n == count && break
        n += 1
    end
    write(out, SubString(str,i))
    String(take!(out))
end

"""
    replace(s::AbstractString, pat, r, [count::Integer])

Search for the given pattern `pat` in `s`, and replace each occurrence with `r`.
If `count` is provided, replace at most `count` occurrences.
As with [`search`](@ref), the second argument may be a
single character, a vector or a set of characters, a string, or a regular expression. If `r`
is a function, each occurrence is replaced with `r(s)` where `s` is the matched substring.
If `pat` is a regular expression and `r` is a `SubstitutionString`, then capture group
references in `r` are replaced with the corresponding matched text.
To remove instances of `pat` from `string`, set `r` to the empty `String` (`""`).

# Examples
```jldoctest
julia> replace("Python is a programming language.", "Python", "Julia")
"Julia is a programming language."

julia> replace("The quick foxes run quickly.", "quick", "slow", 1)
"The slow foxes run quickly."

julia> replace("The quick foxes run quickly.", "quick", "", 1)
"The  foxes run quickly."
```
"""
replace(s::AbstractString, pat, f) = replace_new(String(s), pat, f, typemax(Int))
# TODO: change this to the following when `replace` is removed from deprecated.jl:
# replace(s::AbstractString, pat, f, count::Integer=typemax(Int)) =
#     replace(String(s), pat, f, count)


# hex <-> bytes conversion

"""
    hex2bytes(s::Union{AbstractString,AbstractVector{UInt8}})

Given a string or array `s` of ASCII codes for a sequence of hexadecimal digits, returns a
`Vector{UInt8}` of bytes  corresponding to the binary representation: each successive pair
of hexadecimal digits in `s` gives the value of one byte in the return vector.

The length of `s` must be even, and the returned array has half of the length of `s`.
See also [`hex2bytes!`](@ref) for an in-place version, and [`bytes2hex`](@ref) for the inverse.

# Examples
```jldoctest
julia> s = hex(12345)
"3039"

julia> hex2bytes(s)
2-element Array{UInt8,1}:
 0x30
 0x39

julia> a = b"01abEF"
6-element Array{UInt8,1}:
 0x30
 0x31
 0x61
 0x62
 0x45
 0x46

julia> hex2bytes(a)
3-element Array{UInt8,1}:
 0x01
 0xab
 0xef
```
"""
function hex2bytes end

hex2bytes(s::AbstractString) = hex2bytes(Vector{UInt8}(String(s)))
hex2bytes(s::AbstractVector{UInt8}) = hex2bytes!(Vector{UInt8}(length(s) >> 1), s)

"""
    hex2bytes!(d::AbstractVector{UInt8}, s::AbstractVector{UInt8})

Convert an array `s` of bytes representing a hexadecimal string to its binary
representation, similar to [`hex2bytes`](@ref) except that the output is written in-place
in `d`.   The length of `s` must be exactly twice the length of `d`.
"""
function hex2bytes!(d::AbstractVector{UInt8}, s::AbstractVector{UInt8})
    if 2length(d) != length(s)
        isodd(length(s)) && throw(ArgumentError("input hex array must have even length"))
        throw(ArgumentError("output array must be half length of input array"))
    end
    j = first(eachindex(d)) - 1
    for i = first(eachindex(s)):2:endof(s)
        @inbounds d[j += 1] = number_from_hex(s[i]) << 4 + number_from_hex(s[i+1])
    end
    return d
end

@inline number_from_hex(c) =
    (UInt8('0') <= c <= UInt8('9')) ? c - UInt8('0') :
    (UInt8('A') <= c <= UInt8('F')) ? c - (UInt8('A') - 0x0a) :
    (UInt8('a') <= c <= UInt8('f')) ? c - (UInt8('a') - 0x0a) :
    throw(ArgumentError("byte is not an ASCII hexadecimal digit"))

"""
    bytes2hex(bin_arr::Array{UInt8, 1}) -> String

Convert an array of bytes to its hexadecimal representation.
All characters are in lower-case.
# Examples
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

# Examples
```jldoctest
julia> ascii("abcdeγfgh")
ERROR: ArgumentError: invalid ASCII at index 6 in "abcdeγfgh"
Stacktrace:
[...]

julia> ascii("abcdefgh")
"abcdefgh"
```
"""
ascii(x::AbstractString) = ascii(String(x))

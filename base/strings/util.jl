# This file is a part of Julia. License is MIT: https://julialang.org/license

const Chars = Union{Char,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}

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

# FIXME: check that end of `b` doesn't match a partial character in `a`
startswith(a::String, b::String) = sizeof(a) ≥ sizeof(b) &&
    ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, sizeof(b)) == 0

startswith(a::Vector{UInt8}, b::Vector{UInt8}) = length(a) ≥ length(b) &&
    ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, length(b)) == 0

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
chop(s::AbstractString) = SubString(s, start(s), prevind(s, endof(s)))
chop(s::AbstractString, head::Integer, tail::Integer) =
    SubString(s, nextind(s, start(s), head), prevind(s, endof(s), tail))

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
    a = start(s)
    i = endof(s)
    while a ≤ i
        c = s[i]
        j = prevind(s, i)
        c in chars || return SubString(s, 1:i)
        i = j
    end
    SubString(s, a, a-1)
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

"""
    lpad(s, n::Integer, p::Union{Char,AbstractString}=' ') -> String

Stringify `s` and pad the resulting string on the left with `p` to make it `n`
characters (code points) long. If `s` is already `n` characters long, an equal
string is returned. Pad with spaces by default.

# Examples
```jldoctest
julia> lpad("March", 10)
"     March"
```
"""
lpad(s, n::Integer, p::Union{Char,AbstractString}=' ') = lpad(string(s), n, string(p))

function lpad(
    s::Union{Char,AbstractString},
    n::Integer,
    p::Union{Char,AbstractString}=' ',
) :: String
    m = n - length(s)
    m ≤ 0 && return string(s)
    l = length(p)
    q, r = divrem(m, l)
    r == 0 ? string(p^q, s) : string(p^q, first(p, r), s)
end

"""
    rpad(s, n::Integer, p::Union{Char,AbstractString}=' ') -> String

Stringify `s` and pad the resulting string on the right with `p` to make it `n`
characters (code points) long. If `s` is already `n` characters long, an equal
string is returned. Pad with spaces by default.

# Examples
```jldoctest
julia> rpad("March", 20)
"March               "
```
"""
rpad(s, n::Integer, p::Union{Char,AbstractString}=' ') = rpad(string(s), n, string(p))

function rpad(
    s::Union{Char,AbstractString},
    n::Integer,
    p::Union{Char,AbstractString}=' ',
) :: String
    m = n - length(s)
    m ≤ 0 && return string(s)
    l = length(p)
    q, r = divrem(m, l)
    r == 0 ? string(s, p^q) : string(s, p^q, first(p, r))
end

"""
    split(s::AbstractString, [chars]; limit::Integer=0, keep::Bool=true)

Return an array of substrings by splitting the given string on occurrences of the given
character delimiters, which may be specified in any of the formats allowed by
[`findnext`](@ref)'s first argument (i.e. as a string, regular expression or a function),
or as a single character or collection of characters.

If `chars` is omitted, it defaults to the set of all space characters, and
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
function split end

split(str::T, splitter;
      limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _split(str, splitter, limit, keep, T <: SubString ? T[] : SubString{T}[])
split(str::T, splitter::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}};
      limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _split(str, occursin(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])
split(str::T, splitter::Char;
      limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _split(str, equalto(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])

function _split(str::AbstractString, splitter, limit::Integer, keep_empty::Bool, strs::Array)
    i = start(str)
    n = endof(str)
    r = findfirst(splitter,str)
    if r != 0:-1
        j, k = first(r), nextind(str,last(r))
        while 0 < j <= n && length(strs) != limit-1
            if i < k
                if keep_empty || i < j
                    push!(strs, SubString(str,i,prevind(str,j)))
                end
                i = k
            end
            (k <= j) && (k = nextind(str,j))
            r = findnext(splitter,str,k)
            r == 0:-1 && break
            j, k = first(r), nextind(str,last(r))
        end
    end
    if keep_empty || !done(str,i)
        push!(strs, SubString(str,i))
    end
    return strs
end

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::AbstractString) = split(str, _default_delims; limit=0, keep=false)

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
function rsplit end

rsplit(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
    _rsplit(str, splitter, limit, keep, T <: SubString ? T[] : SubString{T}[])
rsplit(str::T, splitter::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}};
       limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
  _rsplit(str, occursin(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])
rsplit(str::T, splitter::Char;
       limit::Integer=0, keep::Bool=true) where {T<:AbstractString} =
  _rsplit(str, equalto(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])

function _rsplit(str::AbstractString, splitter, limit::Integer, keep_empty::Bool, strs::Array)
    i = start(str)
    n = endof(str)
    r = findlast(splitter, str)
    j = first(r)-1
    k = last(r)
    while((0 <= j < n) && (length(strs) != limit-1))
        if i <= k
            (keep_empty || (k < n)) && pushfirst!(strs, SubString(str,k+1,n))
            n = j
        end
        (k <= j) && (j = prevind(str,j))
        r = findprev(splitter,str,j)
        j = first(r)-1
        k = last(r)
    end
    (keep_empty || (n > 0)) && pushfirst!(strs, SubString(str,1,n))
    return strs
end
#rsplit(str::AbstractString) = rsplit(str, _default_delims, 0, false)

_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))

# replace Char with function that compares with this Char
predicatepair(p::Pair{Char}) = equalto(p.first) => p.second

# replace collection of Char with function that checks occurrence in this collection
function predicatepair(p::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    occursin(p.first) => p.second
end
predicatepair(p::Pair) = p

# specialized search function for Char
function findfunc(p::Pair{Char})
    (s::AbstractString, i::Int) -> begin
        r = findnext(equalto(p.first), s, i)
        r, r, ind
    end
end

# specialized search function for collections of Char
function findfunc(p::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    (s::AbstractString, i::Int) -> begin
        r = findnext(occursin(p.first), s, i)
        r, r, p
    end
end

# search function for all other cases (p.first can be passed to findnext)
function findfunc(p::Pair)
    (s::AbstractString, i::Int) -> begin
        r = findnext(p.first, s, i)
        first(r), last(r), p
    end
end

# accumulate in st/d - discard duplicate patterns
function pushrx!(c::String, repl::Any, st::Vector{String}, d::Dict{String,Any})
    cs = string(c)
    if !haskey(d, cs)
        d[cs] = repl
        push!(st, cs)
    end
end

# push pattern-repl pair depending on type of pattern
function pushpat!(pair::Pair{<:Union{Char,AbstractString}}, pairs, st, d)
    pushrx!(string(pair.first), pair.second, st, d)
end
function pushpat!(pair::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}}, pairs, st, d)
    for c in pat
        pushrx!(string(c), pair.second, st, d)
    end
end
function pushpat!(pair::Pair{<:Base.EqualTo}, pairs, st, d)
    pushrx!(string(pair.first.x), pair.second, st, d)
end
function pushpat!(pair::Pair{<:Base.OccursIn}, pairs, st, d)
    for c in pair.first.x
        pushrx!(string(c), pair.second, st, d)
    end
end

pushpat!(pair::Pair, pairs, st, d) = push!(pairs, pair)

function _postprocessor(d::Dict{String,Any})
    (s::AbstractString) -> _postprocess(s, d[s])
end

_postprocess(s::AbstractString, ds::Union{AbstractString,Char}) = ds
_postprocess(s::AbstractString, ds::Function) = ds(s)
_postprocess(s::AbstractString, ds) = string(ds)

# extract all String, Char, and collection of Char patterns
# and combine to one regex + replacement function
# all others remain
function build_regex(pat_repls::NTuple{N,Pair}) where N
    pairs = Vector{Pair}()
    st = Vector{String}()
    d = Dict{String,Any}()

    for pair in pat_repls
        pushpat!(pair, pairs, st, d)
    end

    if length(st) == 1
        str = st[1]
        pushfirst!(pairs, str=>d[str])
    elseif length(st) > 1
        buf = IOBuffer()
        sort!(st, lt=(a,b)->sizeof(a)>sizeof(b))
        frst = true
        for pat in st
            frst || print(buf, '|')
            escape_string(buf, pat, "[\\^\$.|?*+()")
            frst = false
        end
        rs = String(take!(buf))
        pushfirst!(pairs, Regex(rs)=>_postprocessor(d))
    end
    pairs
end

# find the first of a list of patterns.
# if several match at the same start position prefer the longer one
# if also the length is equal, prefer the first in list
# return index-range of the successful search and the pair
# if no match is found, return 0, 0 and some pair
# The search moves forward through s exactly once for each pair
# If no match is found for a pair, it is removed from the list
function findnextall(fun::Vector{Function}, indo::Vector{Pair}, rv1::Vector{Int}, rv2::Vector{Int}, s::AbstractString)

    n = length(fun)
    if n == 0
        0, 0, (""=>"")
    else
        minstart = rv1[1]
        minend = rv2[1]
        minp = indo[1]
        for i = 2:n
            r1 = rv1[i]
            r2 = rv2[i]
            if r1 < minstart || ( r1 == minstart && r2 > minend)
                minstart = r1
                minend = r2
                minp = indo[i]
            end
        end
        # next match at minstart:minend for pat_repl with original index minp

        nexttome = nextind(s, minend)
        for i = 1:n
            if rv1[i] <= minend #overlap
                # new search starting one after minend
                rv1[i], rv2[i], indo[i] = fun[i](s, nexttome)
            end
        end
        # remove all pairs without matching patterns
        for i = n:-1:1
            if rv1[i] == 0
                deleteat!(fun, i)
                deleteat!(indo, i)
                deleteat!(rv1, i)
                deleteat!(rv2, i)
            end
        end
        minstart, minend, minp
    end
end

# cover the multiple pairs case
function replace(str::String, pat_repls::Pair...; count::Integer=typemax(Int))
    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    fun = Vector{Function}()
    indo = Vector{Pair}()
    rv1 = Vector{Int}()
    rv2 = Vector{Int}()

    pairs = build_regex(pat_repls)
    n = length(pairs)
    n == 0 && return str
    n == 1 && return replace(str, pairs[1], count=count) # special for one pair

    for i = 1:n
        pair = pairs[i]
        f = findfunc(pair)
        r1, r2, rp = f(str, 1)
        if r1 > 0
            push!(fun, f)
            push!(indo, pair)
            push!(rv1, r1)
            push!(rv2, r2)
        end
    end
    n = 1
    e = endof(str)
    i = a = start(str)
    j, k, ind = findnextall(fun, indo, rv1, rv2, str)
    out = IOBuffer(StringVector(sizeof(str)*12÷10), true, true)
    out.size = 0
    out.ptr = 1
    while j != 0
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            pattern, repl = ind
            _replace(out, repl, str, j:k, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        j, k, ind = findnextall(fun, indo, rv1, rv2, str)
        j == 0 || n == count && break
        n += 1
    end
    write(out, SubString(str, i))
    String(take!(out))
end

# the case of a single pair has twice better performance
function replace(str::String, pat_repl::Pair; count::Integer=typemax(Int))
    pattern, repl = predicatepair(pat_repl)
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    n = 1
    e = endof(str)
    i = a = start(str)
    r = findnext(pattern,str,i)
    j, k = first(r), last(r)
    out = IOBuffer(StringVector(floor(Int, 1.2sizeof(str))), true, true)
    out.size = 0
    out.ptr = 1
    while j != 0
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace(out, repl, str, r, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        r = findnext(pattern,str,k)
        r == 0:-1 || n == count && break
        j, k = first(r), last(r)
        n += 1
    end
    write(out, SubString(str,i))
    String(take!(out))
end

"""
    replace(s::AbstractString, pat=>r...; [count::Integer])

Search for any of the given patterns `pat` in `s`, and replace each occurrence with
corresponding `r`.
If `count` is provided, replace at most `count` occurrences.
Each `pat` may be a single character, a vector or a set of characters, a string,
or a regular expression. If `r`
is a function, each occurrence is replaced with `r(s)` where `s` is the matched substring.
If `pat` is a regular expression and `r` is a `SubstitutionString`, then capture group
references in `r` are replaced with the corresponding matched text.
To remove instances of `pat` from `string`, set `r` to the empty `String` (`""`).
In the case of multiple patterns, the precedence is by low start position of matched pattern,
then longer pattern, then order of the input list.

# Examples
```jldoctest
julia> replace("Python is a programming language.", "Python" => "Julia")
"Julia is a programming language."

julia> replace("The quick foxes run quickly.", "quick" => "slow", count=1)
"The slow foxes run quickly."

julia> replace("The quick foxes run quickly.", "quick" => "", count=1)
"The  foxes run quickly."

julia> replace("The quicky quick fox", "quick"=>"slow", "quicky"=>"elegant")
"The elegant slow fox"
```
"""
function replace(s::AbstractString, pat_f::Pair...; count=typemax(Int))
    if count == 0 || length(pat_f) == 0
        return s
    end
    replace(String(s), pat_f..., count=count)
end

# TODO: allow transform as the first argument to replace?

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

hex2bytes(s::AbstractString) = hex2bytes(String(s))
hex2bytes(s::Union{String,AbstractVector{UInt8}}) = hex2bytes!(Vector{UInt8}(uninitialized, length(s) >> 1), s)

_firstbyteidx(s::String) = 1
_firstbyteidx(s::AbstractVector{UInt8}) = first(eachindex(s))
_lastbyteidx(s::String) = sizeof(s)
_lastbyteidx(s::AbstractVector{UInt8}) = endof(s)

"""
    hex2bytes!(d::AbstractVector{UInt8}, s::Union{String,AbstractVector{UInt8}})

Convert an array `s` of bytes representing a hexadecimal string to its binary
representation, similar to [`hex2bytes`](@ref) except that the output is written in-place
in `d`.   The length of `s` must be exactly twice the length of `d`.
"""
function hex2bytes!(d::AbstractVector{UInt8}, s::Union{String,AbstractVector{UInt8}})
    if 2length(d) != sizeof(s)
        isodd(sizeof(s)) && throw(ArgumentError("input hex array must have even length"))
        throw(ArgumentError("output array must be half length of input array"))
    end
    j = first(eachindex(d)) - 1
    for i = _firstbyteidx(s):2:_lastbyteidx(s)
        @inbounds d[j += 1] = number_from_hex(_nthbyte(s,i)) << 4 + number_from_hex(_nthbyte(s,i+1))
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
    b = Vector{UInt8}(uninitialized, 2*length(a))
    i = 0
    for x in a
        b[i += 1] = hex_chars[1 + x >> 4]
        b[i += 1] = hex_chars[1 + x & 0xf]
    end
    return String(b)
end

# check for pure ASCII-ness

function ascii(s::String)
    for i = 1:sizeof(s)
        b = codeunit(s,i)
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

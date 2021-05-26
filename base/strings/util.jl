# This file is a part of Julia. License is MIT: https://julialang.org/license

const Chars = Union{AbstractChar,Tuple{Vararg{AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}}

# starts with and ends with predicates

"""
    startswith(s::AbstractString, prefix::AbstractString)

Return `true` if `s` starts with `prefix`. If `prefix` is a vector or set
of characters, test whether the first character of `s` belongs to that set.

See also [`endswith`](@ref), [`contains`](@ref).

# Examples
```jldoctest
julia> startswith("JuliaLang", "Julia")
true
```
"""
function startswith(a::AbstractString, b::AbstractString)
    a, b = Iterators.Stateful(a), Iterators.Stateful(b)
    all(splat(==), zip(a, b)) && isempty(b)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && first(str)::AbstractChar in chars

"""
    endswith(s::AbstractString, suffix::AbstractString)

Return `true` if `s` ends with `suffix`. If `suffix` is a vector or set of
characters, test whether the last character of `s` belongs to that set.

See also [`startswith`](@ref), [`contains`](@ref).

# Examples
```jldoctest
julia> endswith("Sunday", "day")
true
```
"""
function endswith(a::AbstractString, b::AbstractString)
    a = Iterators.Stateful(Iterators.reverse(a))
    b = Iterators.Stateful(Iterators.reverse(b))
    all(splat(==), zip(a, b)) && isempty(b)
end
endswith(str::AbstractString, chars::Chars) = !isempty(str) && last(str) in chars

function startswith(a::Union{String, SubString{String}},
                    b::Union{String, SubString{String}})
    cub = ncodeunits(b)
    if ncodeunits(a) < cub
        false
    elseif _memcmp(a, b, sizeof(b)) == 0
        nextind(a, cub) == cub + 1
    else
        false
    end
end

function endswith(a::Union{String, SubString{String}},
                  b::Union{String, SubString{String}})
    cub = ncodeunits(b)
    astart = ncodeunits(a) - ncodeunits(b) + 1
    if astart < 1
        false
    elseif GC.@preserve(a, _memcmp(pointer(a, astart), b, sizeof(b))) == 0
        thisind(a, astart) == astart
    else
        false
    end
end

"""
    contains(haystack::AbstractString, needle)

Return `true` if `haystack` contains `needle`.
This is the same as `occursin(needle, haystack)`, but is provided for consistency with
`startswith(haystack, needle)` and `endswith(haystack, needle)`.

See also [`occursin`](@ref), [`in`](@ref), [`issubset`](@ref).

# Examples
```jldoctest
julia> contains("JuliaLang is pretty cool!", "Julia")
true

julia> contains("JuliaLang is pretty cool!", 'a')
true

julia> contains("aba", r"a.a")
true

julia> contains("abba", r"a.a")
false
```

!!! compat "Julia 1.5"
    The `contains` function requires at least Julia 1.5.
"""
contains(haystack::AbstractString, needle) = occursin(needle, haystack)

"""
    endswith(suffix)

Create a function that checks whether its argument ends with `suffix`, i.e.
a function equivalent to `y -> endswith(y, suffix)`.

The returned function is of type `Base.Fix2{typeof(endswith)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.5"
    The single argument `endswith(suffix)` requires at least Julia 1.5.

# Examples
```jldoctest
julia> endswith_julia = endswith("Julia");

julia> endswith_julia("Julia")
true

julia> endswith_julia("JuliaLang")
false
```
"""
endswith(s) = Base.Fix2(endswith, s)

"""
    startswith(prefix)

Create a function that checks whether its argument starts with `prefix`, i.e.
a function equivalent to `y -> startswith(y, prefix)`.

The returned function is of type `Base.Fix2{typeof(startswith)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.5"
    The single argument `startswith(prefix)` requires at least Julia 1.5.

# Examples
```jldoctest
julia> startswith_julia = startswith("Julia");

julia> startswith_julia("Julia")
true

julia> startswith_julia("NotJulia")
false
```
"""
startswith(s) = Base.Fix2(startswith, s)

"""
    contains(needle)

Create a function that checks whether its argument contains `needle`, i.e.
a function equivalent to `haystack -> contains(haystack, needle)`.

The returned function is of type `Base.Fix2{typeof(contains)}`, which can be
used to implement specialized methods.
"""
contains(needle) = Base.Fix2(contains, needle)

"""
    chop(s::AbstractString; head::Integer = 0, tail::Integer = 1)

Remove the first `head` and the last `tail` characters from `s`.
The call `chop(s)` removes the last character from `s`.
If it is requested to remove more characters than `length(s)`
then an empty string is returned.

See also [`chomp`](@ref), [`startswith`](@ref), [`first`](@ref).

# Examples
```jldoctest
julia> a = "March"
"March"

julia> chop(a)
"Marc"

julia> chop(a, head = 1, tail = 2)
"ar"

julia> chop(a, head = 5, tail = 5)
""
```
"""
function chop(s::AbstractString; head::Integer = 0, tail::Integer = 1)
    if isempty(s)
        return SubString(s)
    end
    SubString(s, nextind(s, firstindex(s), head), prevind(s, lastindex(s), tail))
end

# TODO: optimization for the default case based on
# chop(s::AbstractString) = SubString(s, firstindex(s), prevind(s, lastindex(s)))

"""
    chomp(s::AbstractString) -> SubString

Remove a single trailing newline from a string.

See also [`chop`](@ref).

# Examples
```jldoctest
julia> chomp("Hello\\n")
"Hello"
```
"""
function chomp(s::AbstractString)
    i = lastindex(s)
    (i < 1 || s[i] != '\n') && (return SubString(s, 1, i))
    j = prevind(s,i)
    (j < 1 || s[j] != '\r') && (return SubString(s, 1, j))
    return SubString(s, 1, prevind(s,j))
end
function chomp(s::String)
    i = lastindex(s)
    if i < 1 || codeunit(s,i) != 0x0a
        return @inbounds SubString(s, 1, i)
    elseif i < 2 || codeunit(s,i-1) != 0x0d
        return @inbounds SubString(s, 1, prevind(s, i))
    else
        return @inbounds SubString(s, 1, prevind(s, i-1))
    end
end

"""
    lstrip([pred=isspace,] str::AbstractString) -> SubString
    lstrip(str::AbstractString, chars) -> SubString

Remove leading characters from `str`, either those specified by `chars` or those for
which the function `pred` returns `true`.

The default behaviour is to remove leading whitespace and delimiters: see
[`isspace`](@ref) for precise details.

The optional `chars` argument specifies which characters to remove: it can be a single
character, or a vector or set of characters.

See also [`strip`](@ref) and [`rstrip`](@ref).

# Examples
```jldoctest
julia> a = lpad("March", 20)
"               March"

julia> lstrip(a)
"March"
```
"""
function lstrip(f, s::AbstractString)
    e = lastindex(s)
    for (i::Int, c::AbstractChar) in pairs(s)
        !f(c) && return @inbounds SubString(s, i, e)
    end
    SubString(s, e+1, e)
end
lstrip(s::AbstractString) = lstrip(isspace, s)
lstrip(s::AbstractString, chars::Chars) = lstrip(in(chars), s)

"""
    rstrip([pred=isspace,] str::AbstractString) -> SubString
    rstrip(str::AbstractString, chars) -> SubString

Remove trailing characters from `str`, either those specified by `chars` or those for
which the function `pred` returns `true`.

The default behaviour is to remove trailing whitespace and delimiters: see
[`isspace`](@ref) for precise details.

The optional `chars` argument specifies which characters to remove: it can be a single
character, or a vector or set of characters.

See also [`strip`](@ref) and [`lstrip`](@ref).

# Examples
```jldoctest
julia> a = rpad("March", 20)
"March               "

julia> rstrip(a)
"March"
```
"""
function rstrip(f, s::AbstractString)
    for (i, c) in Iterators.reverse(pairs(s))
        f(c::AbstractChar) || return @inbounds SubString(s, 1, i::Int)
    end
    SubString(s, 1, 0)
end
rstrip(s::AbstractString) = rstrip(isspace, s)
rstrip(s::AbstractString, chars::Chars) = rstrip(in(chars), s)

"""
    strip([pred=isspace,] str::AbstractString) -> SubString
    strip(str::AbstractString, chars) -> SubString

Remove leading and trailing characters from `str`, either those specified by `chars` or
those for which the function `pred` returns `true`.

The default behaviour is to remove leading and trailing whitespace and delimiters: see
[`isspace`](@ref) for precise details.

The optional `chars` argument specifies which characters to remove: it can be a single
character, vector or set of characters.

See also [`lstrip`](@ref) and [`rstrip`](@ref).

!!! compat "Julia 1.2"
    The method which accepts a predicate function requires Julia 1.2 or later.

# Examples
```jldoctest
julia> strip("{3, 5}\\n", ['{', '}', '\\n'])
"3, 5"
```
"""
strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)
strip(f, s::AbstractString) = lstrip(f, rstrip(f, s))

## string padding functions ##

"""
    lpad(s, n::Integer, p::Union{AbstractChar,AbstractString}=' ') -> String

Stringify `s` and pad the resulting string on the left with `p` to make it `n`
characters (code points) long. If `s` is already `n` characters long, an equal
string is returned. Pad with spaces by default.

# Examples
```jldoctest
julia> lpad("March", 10)
"     March"
```
"""
lpad(s, n::Integer, p::Union{AbstractChar,AbstractString}=' ') = lpad(string(s)::AbstractString, n, string(p))

function lpad(
    s::Union{AbstractChar,AbstractString},
    n::Integer,
    p::Union{AbstractChar,AbstractString}=' ',
) :: String
    n = Int(n)::Int
    m = signed(n) - Int(length(s))::Int
    m ≤ 0 && return string(s)
    l = length(p)
    q, r = divrem(m, l)
    r == 0 ? string(p^q, s) : string(p^q, first(p, r), s)
end

"""
    rpad(s, n::Integer, p::Union{AbstractChar,AbstractString}=' ') -> String

Stringify `s` and pad the resulting string on the right with `p` to make it `n`
characters (code points) long. If `s` is already `n` characters long, an equal
string is returned. Pad with spaces by default.

# Examples
```jldoctest
julia> rpad("March", 20)
"March               "
```
"""
rpad(s, n::Integer, p::Union{AbstractChar,AbstractString}=' ') = rpad(string(s)::AbstractString, n, string(p))

function rpad(
    s::Union{AbstractChar,AbstractString},
    n::Integer,
    p::Union{AbstractChar,AbstractString}=' ',
) :: String
    n = Int(n)::Int
    m = signed(n) - Int(length(s))::Int
    m ≤ 0 && return string(s)
    l = length(p)
    q, r = divrem(m, l)
    r == 0 ? string(s, p^q) : string(s, p^q, first(p, r))
end

"""
    split(str::AbstractString, dlm; limit::Integer=0, keepempty::Bool=true)
    split(str::AbstractString; limit::Integer=0, keepempty::Bool=false)

Split `str` into an array of substrings on occurrences of the delimiter(s) `dlm`.  `dlm`
can be any of the formats allowed by [`findnext`](@ref)'s first argument (i.e. as a
string, regular expression or a function), or as a single character or collection of
characters.

If `dlm` is omitted, it defaults to [`isspace`](@ref).

The optional keyword arguments are:
 - `limit`: the maximum size of the result. `limit=0` implies no maximum (default)
 - `keepempty`: whether empty fields should be kept in the result. Default is `false` without
   a `dlm` argument, `true` with a `dlm` argument.

See also [`rsplit`](@ref).

# Examples
```jldoctest
julia> a = "Ma.rch"
"Ma.rch"

julia> split(a, ".")
2-element Vector{SubString{String}}:
 "Ma"
 "rch"
```
"""
function split end

function split(str::T, splitter;
               limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _split(str, splitter, limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end
function split(str::T, splitter::Union{Tuple{Vararg{AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}};
               limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _split(str, in(splitter), limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end
function split(str::T, splitter::AbstractChar;
               limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _split(str, isequal(splitter), limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end

function _split(str::AbstractString, splitter::F, limit::Integer, keepempty::Bool, strs::Vector) where F
    # Forcing specialization on `splitter` improves performance (roughly 30% decrease in runtime)
    # and prevents a major invalidation risk (1550 MethodInstances)
    i = 1 # firstindex(str)
    n = lastindex(str)::Int
    r = findfirst(splitter,str)::Union{Nothing,Int,UnitRange{Int}}
    if r !== nothing
        j, k = first(r), nextind(str,last(r))::Int
        while 0 < j <= n && length(strs) != limit-1
            if i < k
                if keepempty || i < j
                    push!(strs, @inbounds SubString(str,i,prevind(str,j)::Int))
                end
                i = k
            end
            (k <= j) && (k = nextind(str,j)::Int)
            r = findnext(splitter,str,k)::Union{Nothing,Int,UnitRange{Int}}
            r === nothing && break
            j, k = first(r), nextind(str,last(r))::Int
        end
    end
    if keepempty || i <= ncodeunits(str)::Int
        push!(strs, @inbounds SubString(str,i))
    end
    return strs
end

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::AbstractString;
      limit::Integer=0, keepempty::Bool=false) =
    split(str, isspace; limit=limit, keepempty=keepempty)

"""
    rsplit(s::AbstractString; limit::Integer=0, keepempty::Bool=false)
    rsplit(s::AbstractString, chars; limit::Integer=0, keepempty::Bool=true)

Similar to [`split`](@ref), but starting from the end of the string.

# Examples
```jldoctest
julia> a = "M.a.r.c.h"
"M.a.r.c.h"

julia> rsplit(a, ".")
5-element Vector{SubString{String}}:
 "M"
 "a"
 "r"
 "c"
 "h"

julia> rsplit(a, "."; limit=1)
1-element Vector{SubString{String}}:
 "M.a.r.c.h"

julia> rsplit(a, "."; limit=2)
2-element Vector{SubString{String}}:
 "M.a.r.c"
 "h"
```
"""
function rsplit end

function rsplit(str::T, splitter;
                limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _rsplit(str, splitter, limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end
function rsplit(str::T, splitter::Union{Tuple{Vararg{AbstractChar}},AbstractVector{<:AbstractChar},Set{<:AbstractChar}};
                limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _rsplit(str, in(splitter), limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end
function rsplit(str::T, splitter::AbstractChar;
                limit::Integer=0, keepempty::Bool=true) where {T<:AbstractString}
    _rsplit(str, isequal(splitter), limit, keepempty, T <: SubString ? T[] : SubString{T}[])
end

function _rsplit(str::AbstractString, splitter, limit::Integer, keepempty::Bool, strs::Array)
    n = lastindex(str)::Int
    r = something(findlast(splitter, str)::Union{Nothing,Int,UnitRange{Int}}, 0)
    j, k = first(r), last(r)
    while j > 0 && k > 0 && length(strs) != limit-1
        (keepempty || k < n) && pushfirst!(strs, @inbounds SubString(str,nextind(str,k)::Int,n))
        n = prevind(str, j)::Int
        r = something(findprev(splitter,str,n)::Union{Nothing,Int,UnitRange{Int}}, 0)
        j, k = first(r), last(r)
    end
    (keepempty || n > 0) && pushfirst!(strs, SubString(str,1,n))
    return strs
end
rsplit(str::AbstractString;
      limit::Integer=0, keepempty::Bool=false) =
    rsplit(str, isspace; limit=limit, keepempty=keepempty)

_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))
_replace(io, repl::Function, str, r, pattern::Function) =
    print(io, repl(str[first(r)]))

replace(str::String, pat_repl::Pair{<:AbstractChar}; count::Integer=typemax(Int)) =
    replace(str, isequal(first(pat_repl)) => last(pat_repl); count=count)

replace(str::String, pat_repl::Pair{<:Union{Tuple{Vararg{AbstractChar}},
                                            AbstractVector{<:AbstractChar},Set{<:AbstractChar}}};
        count::Integer=typemax(Int)) =
    replace(str, in(first(pat_repl)) => last(pat_repl), count=count)

_pat_replacer(x) = x
_free_pat_replacer(x) = nothing

function replace(str::String, pat_repl::Pair; count::Integer=typemax(Int))
    pattern, repl = pat_repl
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    n = 1
    e = lastindex(str)
    i = a = firstindex(str)
    pattern = _pat_replacer(pattern)
    r = something(findnext(pattern,str,i), 0)
    j, k = first(r), last(r)
    if j == 0
        _free_pat_replacer(pattern)
        return str
    end
    out = IOBuffer(sizehint=floor(Int, 1.2sizeof(str)))
    while j != 0
        if i == a || i <= k
            GC.@preserve str unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace(out, repl, str, r, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        r = something(findnext(pattern,str,k), 0)
        r === 0:-1 || n == count && break
        j, k = first(r), last(r)
        n += 1
    end
    _free_pat_replacer(pattern)
    write(out, SubString(str,i))
    String(take!(out))
end

"""
    replace(s::AbstractString, pat=>r; [count::Integer])

Search for the given pattern `pat` in `s`, and replace each occurrence with `r`.
If `count` is provided, replace at most `count` occurrences.
`pat` may be a single character, a vector or a set of characters, a string,
or a regular expression.
If `r` is a function, each occurrence is replaced with `r(s)`
where `s` is the matched substring (when `pat` is a `AbstractPattern` or `AbstractString`) or
character (when `pat` is an `AbstractChar` or a collection of `AbstractChar`).
If `pat` is a regular expression and `r` is a [`SubstitutionString`](@ref), then capture group
references in `r` are replaced with the corresponding matched text.
To remove instances of `pat` from `string`, set `r` to the empty `String` (`""`).

# Examples
```jldoctest
julia> replace("Python is a programming language.", "Python" => "Julia")
"Julia is a programming language."

julia> replace("The quick foxes run quickly.", "quick" => "slow", count=1)
"The slow foxes run quickly."

julia> replace("The quick foxes run quickly.", "quick" => "", count=1)
"The  foxes run quickly."

julia> replace("The quick foxes run quickly.", r"fox(es)?" => s"bus\\1")
"The quick buses run quickly."
```
"""
replace(s::AbstractString, pat_f::Pair; count=typemax(Int)) =
    replace(String(s), pat_f, count=count)

# TODO: allow transform as the first argument to replace?

# hex <-> bytes conversion

"""
    hex2bytes(itr)

Given an iterable `itr` of ASCII codes for a sequence of hexadecimal digits, returns a
`Vector{UInt8}` of bytes  corresponding to the binary representation: each successive pair
of hexadecimal digits in `itr` gives the value of one byte in the return vector.

The length of `itr` must be even, and the returned array has half of the length of `itr`.
See also [`hex2bytes!`](@ref) for an in-place version, and [`bytes2hex`](@ref) for the inverse.

!!! compat "Julia 1.7"
    Calling hex2bytes with iterables producing UInt8 requires
    version 1.7. In earlier versions, you can collect the iterable
    before calling instead.

# Examples
```jldoctest
julia> s = string(12345, base = 16)
"3039"

julia> hex2bytes(s)
2-element Vector{UInt8}:
 0x30
 0x39

julia> a = b"01abEF"
6-element Base.CodeUnits{UInt8, String}:
 0x30
 0x31
 0x61
 0x62
 0x45
 0x46

julia> hex2bytes(a)
3-element Vector{UInt8}:
 0x01
 0xab
 0xef
```
"""
function hex2bytes end

hex2bytes(s) = hex2bytes!(Vector{UInt8}(undef, length(s) >> 1), s)

# special case - valid bytes are checked in the generic implementation
function hex2bytes!(dest::AbstractArray{UInt8}, s::String)
    sizeof(s) != length(s) && throw(ArgumentError("input string must consist of hexadecimal characters only"))

    hex2bytes!(dest, transcode(UInt8, s))
end

"""
    hex2bytes!(dest::AbstractVector{UInt8}, itr)

Convert an iterable `itr` of bytes representing a hexadecimal string to its binary
representation, similar to [`hex2bytes`](@ref) except that the output is written in-place
to `dest`. The length of `dest` must be half the length of `itr`.

!!! compat "Julia 1.7"
    Calling hex2bytes! with iterators producing UInt8 requires
    version 1.7. In earlier versions, you can collect the iterable
    before calling instead.
"""
function hex2bytes!(dest::AbstractArray{UInt8}, itr)
    isodd(length(itr)) && throw(ArgumentError("length of iterable must be even"))
    @boundscheck 2*length(dest) != length(itr) && throw(ArgumentError("length of output array must be half of the length of input iterable"))
    iszero(length(itr)) && return dest

    next = iterate(itr)
    @inbounds for i in eachindex(dest)
        x,state = next::NTuple{2,Any}
        y,state = iterate(itr, state)::NTuple{2,Any}
        next = iterate(itr, state)
        dest[i] = number_from_hex(x) << 4 + number_from_hex(y)
    end

    return dest
end

@inline number_from_hex(c::AbstractChar) = number_from_hex(Char(c))
@inline number_from_hex(c::Char) = number_from_hex(UInt8(c))
@inline function number_from_hex(c::UInt8)
    UInt8('0') <= c <= UInt8('9') && return c - UInt8('0')
    c |= 0b0100000
    UInt8('a') <= c <= UInt8('f') && return c - UInt8('a') + 0x0a
    throw(ArgumentError("byte is not an ASCII hexadecimal digit"))
end

"""
    bytes2hex(itr) -> String
    bytes2hex(io::IO, itr)

Convert an iterator `itr` of bytes to its hexadecimal string representation, either
returning a `String` via `bytes2hex(itr)` or writing the string to an `io` stream
via `bytes2hex(io, itr)`.  The hexadecimal characters are all lowercase.

!!! compat "Julia 1.7"
    Calling bytes2hex with iterators producing UInt8 requires
    version 1.7. In earlier versions, you can collect the iterable
    before calling instead.

# Examples
```jldoctest
julia> a = string(12345, base = 16)
"3039"

julia> b = hex2bytes(a)
2-element Vector{UInt8}:
 0x30
 0x39

julia> bytes2hex(b)
"3039"
```
"""
function bytes2hex end

function bytes2hex(itr)
    eltype(itr) === UInt8 || throw(ArgumentError("eltype of iterator not UInt8"))
    b = Base.StringVector(2*length(itr))
    @inbounds for (i, x) in enumerate(itr)
        b[2i - 1] = hex_chars[1 + x >> 4]
        b[2i    ] = hex_chars[1 + x & 0xf]
    end
    return String(b)
end

function bytes2hex(io::IO, itr)
    eltype(itr) === UInt8 || throw(ArgumentError("eltype of iterator not UInt8"))
    for x in itr
        print(io, Char(hex_chars[1 + x >> 4]), Char(hex_chars[1 + x & 0xf]))
    end
end

# check for pure ASCII-ness
function ascii(s::String)
    for i in 1:sizeof(s)
        @inbounds codeunit(s, i) < 0x80 || __throw_invalid_ascii(s, i)
    end
    return s
end
@noinline __throw_invalid_ascii(s::String, i::Int) = throw(ArgumentError("invalid ASCII at index $i in $(repr(s))"))

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

Base.rest(s::Union{String,SubString{String}}, i=1) = SubString(s, i)
function Base.rest(s::AbstractString, st...)
    io = IOBuffer()
    for c in Iterators.rest(s, st...)
        print(io, c)
    end
    return String(take!(io))
end

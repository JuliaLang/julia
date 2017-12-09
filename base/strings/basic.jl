# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
The `AbstractString` type is the supertype of all string implementations in
Julia. Strings are encodings of sequences of [Unicode](https://unicode.org/)
code points as represented by the `Char` type. Julia makes a few assumptions
about strings:

* Strings are encoded in terms of fixed-size "code units"
  * Code units can be extracted with `codeunit(s, i)`
  * The first code unit has index `1`
  * The last code unit has index `ncodeunits(s)`
  * Any index `i` such that `1 ≤ i ≤ ncodeunits(s)` is in bounds
* String indexing is done in terms of these code units:
  * Characters are extracted by `s[i]` with a valid string index `i`
  * Each `Char` in a string is encoded by one or more code units
  * Only the index of the first code unit of a `Char` is a valid index
  * The encoding of a `Char` is independent of what precedes or follows it
  * String encodings are "self-synchronizing" – i.e. `isvalid(s,i)` is O(1)

Some string functions error if you use an out-of-bounds or invalid string index,
including code unit extraction `codeunit(s,i)`, string indexing `s[i]`, and
string iteration `next(s,i)`. Other string functions take a more relaxed
approach to indexing and give you the closest valid string index when in-bounds,
or when out-of-bounds, behave as if there were an infinite number of characters
padding each side of the string. Usually these imaginary padding characters have
code unit length `1`, but string types may choose different sizes. Relaxed
indexing functions include those intended for index arithmetic: `thisind`,
`nextind` and `prevind`. This model allows index arithmetic to work with out-of-
bounds indices as intermediate values so long as one never uses them to retrieve
a character, which often helps avoid needing to code around edge cases.

See also: `codeunit`, `ncodeunits`, `thisind`, `nextind`, `prevind`
"""
AbstractString

## required string functions ##

"""
    ncodeunits(s::AbstractString) -> Int

Return the number of code units in a string. Indices that are in bounds to
access this string must satisfy `1 ≤ i ≤ ncodeunits(s)`. Not all such indices
are valid – they may not be the start of a character, but they will return a
code unit value when calling `codeunit(s,i)`.

See also: `codeunit`, `checkbounds`, `sizeof`, `length`, `endof`
"""
ncodeunits(s::AbstractString)

"""
    codeunit(s::AbstractString) -> Type{<:Union{UInt8, UInt16, UInt32}}

Return the code unit type of the given string object. For ASCII, Latin-1, or
UTF-8 encoded strings, this would be `UInt8`; for UCS-2 and UTF-16 it would be
`UInt16`; for UTF-32 it would be `UInt32`. The unit code type need not be
limited to these three types, but it's hard to think of widely used string
encodings that don't use one of these units. `codeunit(s)` is the same as
`typeof(codeunit(s,1))` when `s` is a non-empty string.

See also: `ncodeunits`
"""
codeunit(s::AbstractString)

"""
    codeunit(s::AbstractString, i::Integer) -> Union{UInt8, UInt16, UInt32}

Return the code unit value in the string `s` at index `i`. Note that

    codeunit(s, i) :: codeunit(s)

I.e. the value returned by `codeunit(s, i)` is of the type returned by
`codeunit(s)`.

See also: `ncodeunits`, `checkbounds`
"""
codeunit(s::AbstractString, i::Integer) = typeof(i) === Int ?
    throw(MethodError(codeunit, Tuple{typeof(s),Int})) :
        codeunit(s, Int(i))

"""
    isvalid(s::AbstractString, i::Integer) -> Bool

Predicate indicating whether the given index is the start of the encoding of
a character in `s` or not. If `isvalid(s, i)` is true then `s[i]` will return
the character whose encoding starts at that index, if it's false, then `s[i]`
will raise an invalid index error. Behavior of `next(s, i)` is similar except
that the character is returned along with the index of the following character.
In order for `isvalid(s, i)` to be an O(1) function, the encoding of `s` must
be [self-synchronizing](https://en.wikipedia.org/wiki/Self-synchronizing_code);
this is a basic assumption of Julia's generic string support.

See also: `getindex`, `next`, `thisind`, `nextind`, `prevind`, `length`

# Examples

```jldoctest
julia> str = "αβγdef";

julia> isvalid(str, 1)
true

julia> str[1]
'α': Unicode U+03b1 (category Ll: Letter, lowercase)

julia> isvalid(str, 2)
false

julia> str[2]
ERROR: UnicodeError: invalid character index
Stacktrace:
[...]
```
"""
isvalid(s::AbstractString, i::Integer) = typeof(i) === Int ?
    throw(MethodError(isvalid, Tuple{typeof(s),Int})) :
        isvalid(s, Int(i))

"""
    next(s::AbstractString, i::Integer) -> Tuple{Char, Int}

Return a tuple of the character in `s` at index `i` with the index of the start
of the following character in `s`. This is the key method that allows strings to
be iterated, yielding a sequences of characters. If `i` is out of bounds in `s`
then a bounds error is raised; if `i` is not a valid character index in `s` then
a Unicode index error is raised.

See also: `getindex`, `start`, `done`, `checkbounds`
"""
next(s::AbstractString, i::Integer) = typeof(i) === Int ?
    throw(MethodError(next, Tuple{typeof(s),Int})) :
        next(s, Int(i))

## basic generic definitions ##

start(s::AbstractString) = 1
done(s::AbstractString, i::Integer) = i > ncodeunits(s)
eltype(::Type{<:AbstractString}) = Char
sizeof(s::AbstractString) = ncodeunits(s) * sizeof(codeunit(s))
endof(s::AbstractString) = thisind(s, ncodeunits(s))

getindex(s::AbstractString, i::Integer) = next(s, i)[1]
getindex(s::AbstractString, i::Colon) = s
# TODO: handle other ranges with stride ±1 specially?
getindex(s::AbstractString, r::UnitRange{<:Integer}) = SubString(s, r)
getindex(s::AbstractString, v::AbstractVector{<:Integer}) =
    sprint(length(v), io->(for i in v; write(io, s[i]) end))
getindex(s::AbstractString, v::AbstractVector{Bool}) =
    throw(ArgumentError("logical indexing not supported for strings"))

get(s::AbstractString, i::Integer, default) = checkbounds(Bool, s, i) ? s[i] : default

## bounds checking ##

checkbounds(::Type{Bool}, s::AbstractString, i::Integer) =
    1 ≤ i ≤ ncodeunits(s)
checkbounds(::Type{Bool}, s::AbstractString, r::AbstractRange{<:Integer}) =
    isempty(r) || (1 ≤ minimum(r) && maximum(r) ≤ ncodeunits(s))
checkbounds(::Type{Bool}, s::AbstractString, I::AbstractArray{<:Real}) =
    all(i -> checkbounds(s, i), I)
checkbounds(::Type{Bool}, s::AbstractString, I::AbstractArray{<:Integer}) =
    all(i -> checkbounds(s, i), I)
checkbounds(s::AbstractString, I::Union{Integer,AbstractArray}) =
    checkbounds(Bool, s, I) || throw(BoundsError(s, I))

## construction, conversion, promotion ##

string() = ""
string(s::AbstractString) = s

(::Type{Vector{UInt8}})(s::AbstractString) = Vector{UInt8}(String(s))
(::Type{Array{UInt8}})(s::AbstractString) = Vector{UInt8}(s)
(::Type{Vector{Char}})(s::AbstractString) = collect(s)

Symbol(s::AbstractString) = Symbol(String(s))

convert(::Type{T}, s::T) where {T<:AbstractString} = s
convert(::Type{T}, s::AbstractString) where {T<:AbstractString} = T(s)

promote_rule(::Type{<:AbstractString}, ::Type{<:AbstractString}) = String

## string & character concatenation ##

"""
    *(s::Union{AbstractString, Char}, t::Union{AbstractString, Char}...) -> String

Concatenate strings and/or characters, producing a [`String`](@ref). This is equivalent
to calling the [`string`](@ref) function on the arguments.

# Examples
```jldoctest
julia> "Hello " * "world"
"Hello world"

julia> 'j' * "ulia"
"julia"
```
"""
(*)(s1::Union{Char, AbstractString}, ss::Union{Char, AbstractString}...) = string(s1, ss...)

one(::Union{T,Type{T}}) where {T<:AbstractString} = convert(T, "")

## generic string comparison ##

"""
    cmp(a::AbstractString, b::AbstractString) -> Int

Compare two strings for equality. Return `0` if both strings have the same
length and the character at each index is the same in both strings. Return `-1`
if `a` is a substring of `b`, or if `a` comes before `b` in alphabetical order.
Return `1` if `b` is a substring of `a`, or if `b` comes before `a` in
alphabetical order (technically, lexicographical order by Unicode code points).

# Examples
```jldoctest
julia> cmp("abc", "abc")
0

julia> cmp("ab", "abc")
-1

julia> cmp("abc", "ab")
1

julia> cmp("ab", "ac")
-1

julia> cmp("ac", "ab")
1

julia> cmp("α", "a")
1

julia> cmp("b", "β")
-1
```
"""
function cmp(a::AbstractString, b::AbstractString)
    a === b && return 0
    i = start(a)
    j = start(b)
    while !done(a, i)
        done(b, j) && return 1
        c, i = next(a, i)
        d, j = next(b, j)
        c ≠ d && return ifelse(c < d, -1, 1)
    end
    return ifelse(done(b, j), 0, -1)
end

"""
    ==(a::AbstractString, b::AbstractString) -> Bool

Test whether two strings are equal character by character (technically, Unicode
code point by code point).

# Examples
```jldoctest
julia> "abc" == "abc"
true

julia> "abc" == "αβγ"
false
```
"""
==(a::AbstractString, b::AbstractString) = cmp(a, b) == 0

"""
    isless(a::AbstractString, b::AbstractString) -> Bool

Test whether string `a` comes before string `b` in alphabetical order
(technically, in lexicographical order by Unicode code points).

# Examples
```jldoctest
julia> isless("a", "b")
true

julia> isless("β", "α")
false

julia> isless("a", "a")
false
```
"""
isless(a::AbstractString, b::AbstractString) = cmp(a, b) < 0

# faster comparisons for symbols

cmp(a::Symbol, b::Symbol) = Int(sign(ccall(:strcmp, Int32, (Cstring, Cstring), a, b)))

isless(a::Symbol, b::Symbol) = cmp(a, b) < 0

## character index arithmetic ##

"""
    length(s::AbstractString, lo::Integer=1, hi::Integer=ncodeunits(s)) -> Integer

The number of characters in string `s` from indices `lo` through `hi`. This is
computed as the number of code unit indices from `lo` to `hi` which are valid
character indices. Without only a single string argument, this computes the
number of characters in the entire string. If `lo` or `hi` are out of ranges
each out of range code unit is considered to be one character. This matches the
"loose" indexing model of `thisind`, `nextind` and `prevind`.

See also: `isvalid`, `ncodeunits`, `endof`, `thisind`, `nextind`, `prevind`

# Examples
```jldoctest
julia> length("jμΛIα")
5
```
"""
function length(s::AbstractString, lo::Integer=1, hi::Integer=ncodeunits(s))
    z = ncodeunits(s)
    a = Int(max(1, min(z, lo)))
    b = Int(min(z, max(1, hi)))
    n = a - b
    for i = a:b
        n += isvalid(s, i)
    end
    return n + hi - lo
end

"""
    thisind(s::AbstractString, i::Integer) -> Int

If `i` is in bounds in `s` return the index of the start of the character whose
encoding code unit `i` is part of. In other words, if `i` is the start of a
character, return `i`; if `i` is not the start of a character, rewind until the
start of a character and return that index. If `i` is out of bounds in `s`
return `i`.

# Examples
```jldoctest
julia> thisind("αβγdef", -5)
-5

julia> thisind("αβγdef", 1)
1

julia> thisind("αβγdef", 3)
3

julia> thisind("αβγdef", 4)
3

julia> thisind("αβγdef", 9)
9

julia> thisind("αβγdef", 10)
10

julia> thisind("αβγdef", 20)
20
"""
function thisind(s::AbstractString, i::Integer)
    i ≤ ncodeunits(s) || return i
    @inbounds while 1 < i && !isvalid(s, i)
        i -= 1
    end
    return i
end

"""
    prevind(str::AbstractString, i::Integer, n::Integer=1) -> Int

If `i` is in bounds in `s` return the index of the start of the character whose
encoding starts before index `i`. In other words, if `i` is the start of a
character, return the start of the previous character; if `i` is not the start
of a character, rewind until the start of a character and return that index.
If `i` is out of bounds in `s` return `i - 1`. If `n == 0` return `i`.

# Examples
```jldoctest
julia> prevind("αβγdef", 3)
1

julia> prevind("αβγdef", 1)
0

julia> prevind("αβγdef", 0)
-1

julia> prevind("αβγdef", 3, 2)
0
```
"""
function prevind(s::AbstractString, i::Integer, n::Integer=1)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    z = ncodeunits(s) + 1
    if i > z
        n -= i - z
        i = z
    end
    while n > 0 && 1 < i
        @inbounds n -= isvalid(s, i -= 1)
    end
    return i - n
end

"""
    nextind(str::AbstractString, i::Integer, n::Integer=1) -> Int

If `i` is in bounds in `s` return the index of the start of the character whose
encoding starts after index `i`. If `i` is out of bounds in `s` return `i + 1`.
If `n == 0` return `i`.

# Examples
```jldoctest
julia> str = "αβγdef";

julia> nextind(str, 1)
3

julia> nextind(str, 1, 2)
5

julia> endof(str)
9

julia> nextind(str, 9)
10
```
"""
function nextind(s::AbstractString, i::Integer, n::Integer=1)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    if i < 1
        n += i - 1
        i = 1
    end
    z = ncodeunits(s)
    while n > 0 && i < z
        @inbounds n -= isvalid(s, i += 1)
    end
    return i + n
end

"""
    ind2chr(s::AbstractString, i::Integer)

Convert a byte index `i` to a character index with
respect to string `s`.

See also [`chr2ind`](@ref).

# Examples
```jldoctest
julia> str = "αβγdef";

julia> ind2chr(str, 3)
2

julia> chr2ind(str, 2)
3
```
"""
ind2chr(s::AbstractString, i::Integer) = length(s, 1, i)

"""
    chr2ind(s::AbstractString, i::Integer)

Convert a character index `i` to a byte index.

See also [`ind2chr`](@ref).

# Examples
```jldoctest
julia> str = "αβγdef";

julia> chr2ind(str, 2)
3

julia> ind2chr(str, 3)
2
```
"""
chr2ind(s::AbstractString, n::Integer) =
    n < 0 ? prevind(s, 0, -n) : nextind(s, 0, n)

## string index iteration type ##

struct EachStringIndex{T<:AbstractString}
    s::T
end
keys(s::AbstractString) = EachStringIndex(s)

length(e::EachStringIndex) = length(e.s)
start(e::EachStringIndex) = start(e.s)
next(e::EachStringIndex, state) = (state, nextind(e.s, state))
done(e::EachStringIndex, state) = done(e.s, state)
eltype(::Type{EachStringIndex}) = Int

"""
    isascii(c::Union{Char,AbstractString}) -> Bool

Test whether a character belongs to the ASCII character set, or whether this is true for
all elements of a string.

# Examples
```jldoctest
julia> isascii('a')
true

julia> isascii('α')
false

julia> isascii("abc")
true

julia> isascii("αβγ")
false
```
"""
isascii(c::Char) = reinterpret(Int32, c) ≥ 0
isascii(s::AbstractString) = all(isascii, s)

"""
    isxdigit(c::Char) -> Bool

Test whether a character is a valid hexadecimal digit. Note that this does not
include `x` (as in the standard `0x` prefix).

# Examples
```jldoctest
julia> isxdigit('a')
true

julia> isxdigit('x')
false
```
"""
isxdigit(c::Char) = '0' ≤ c ≤ '9' || 'a' ≤ c ≤ 'f' || 'A' ≤ c ≤ 'F'

## uppercase, lowercase, and titlecase transformations ##

"""
    uppercase(s::AbstractString) -> String

Return `s` with all characters converted to uppercase.

# Examples
```jldoctest
julia> uppercase("Julia")
"JULIA"
```
"""
uppercase(s::AbstractString) = map(uppercase, s)

"""
    lowercase(s::AbstractString) -> String

Return `s` with all characters converted to lowercase.

# Examples
```jldoctest
julia> lowercase("STRINGS AND THINGS")
"strings and things"
```
"""
lowercase(s::AbstractString) = map(lowercase, s)

"""
    titlecase(s::AbstractString) -> String

Capitalize the first character of each word in `s`.
See also [`ucfirst`](@ref) to capitalize only the first
character in `s`.

# Examples
```jldoctest
julia> titlecase("the Julia programming language")
"The Julia Programming Language"
```
"""
function titlecase(s::AbstractString)
    startword = true
    b = IOBuffer()
    for c in s
        if isspace(c)
            print(b, c)
            startword = true
        else
            print(b, startword ? titlecase(c) : c)
            startword = false
        end
    end
    return String(take!(b))
end

"""
    ucfirst(s::AbstractString) -> String

Return `s` with the first character converted to uppercase (technically "title
case" for Unicode). See also [`titlecase`](@ref) to capitalize the first
character of every word in `s`.

See also: `lcfirst`, `uppercase`, `lowercase`, `titlecase`

# Examples
```jldoctest
julia> ucfirst("python")
"Python"
```
"""
function ucfirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = titlecase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

"""
    lcfirst(s::AbstractString)

Return `s` with the first character converted to lowercase.

See also: `ucfirst`, `uppercase`, `lowercase`, `titlecase`

# Examples
```jldoctest
julia> lcfirst("Julia")
"julia"
```
"""
function lcfirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = lowercase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

## string map, filter, has ##

function map(f, s::AbstractString)
    out = IOBuffer(StringVector(endof(s)), true, true)
    truncate(out, 0)
    for c in s
        c′ = f(c)
        isa(c′, Char) || throw(ArgumentError(
            "map(f, s::AbstractString) requires f to return Char; " *
            "try map(f, collect(s)) or a comprehension instead"))
        write(out, c′::Char)
    end
    String(take!(out))
end

function filter(f, s::AbstractString)
    out = IOBuffer(StringVector(endof(s)), true, true)
    truncate(out, 0)
    for c in s
        f(c) && write(out, c)
    end
    String(take!(out))
end

## string first and last ##

"""
    first(s::AbstractString, n::Integer)

Get a string consisting of the first `n` characters of `s`.

```jldoctest
julia> first("∀ϵ≠0: ϵ²>0", 0)
""

julia> first("∀ϵ≠0: ϵ²>0", 1)
"∀"

julia> first("∀ϵ≠0: ϵ²>0", 3)
"∀ϵ≠"
```
"""
first(s::AbstractString, n::Integer) = s[1:min(end, nextind(s, 0, n))]

"""
    last(s::AbstractString, n::Integer)

Get a string consisting of the last `n` characters of `s`.

```jldoctest
julia> last("∀ϵ≠0: ϵ²>0", 0)
""

julia> last("∀ϵ≠0: ϵ²>0", 1)
"0"

julia> last("∀ϵ≠0: ϵ²>0", 3)
"²>0"
```
"""
last(s::AbstractString, n::Integer) = s[max(1, prevind(s, ncodeunits(s)+1, n)):end]

"""
    reverseind(v, i)

Given an index `i` in [`reverse(v)`](@ref), return the corresponding index in `v` so that
`v[reverseind(v,i)] == reverse(v)[i]`. (This can be nontrivial in cases where `v` contains
non-ASCII characters.)

# Examples
```jldoctest
julia> r = reverse("Julia")
"ailuJ"

julia> for i in 1:length(r)
           print(r[reverseind("Julia", i)])
       end
Julia
```
"""
reverseind(s::AbstractString, i::Integer) = thisind(s, ncodeunits(s)-i+1)

"""
    repeat(s::AbstractString, r::Integer)

Repeat a string `r` times. This can equivalently be accomplished by calling [`s^r`](@ref ^).

# Examples
```jldoctest
julia> repeat("ha", 3)
"hahaha"
```
"""
repeat(s::AbstractString, r::Integer) = repeat(convert(String, s), r)

"""
    ^(s::Union{AbstractString,Char}, n::Integer)

Repeat a string or character `n` times.
The [`repeat`](@ref) function is an alias to this operator.

# Examples
```jldoctest
julia> "Test "^3
"Test Test Test "
```
"""
(^)(s::Union{AbstractString,Char}, r::Integer) = repeat(s, r)

# reverse-order iteration for strings and indices thereof
start(r::Iterators.Reverse{<:AbstractString}) = endof(r.itr)
done(r::Iterators.Reverse{<:AbstractString}, i) = i < start(r.itr)
next(r::Iterators.Reverse{<:AbstractString}, i) = (r.itr[i], prevind(r.itr, i))
start(r::Iterators.Reverse{<:EachStringIndex}) = endof(r.itr.s)
done(r::Iterators.Reverse{<:EachStringIndex}, i) = i < start(r.itr.s)
next(r::Iterators.Reverse{<:EachStringIndex}, i) = (i, prevind(r.itr.s, i))

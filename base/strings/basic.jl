# This file is a part of Julia. License is MIT: https://julialang.org/license

import Core: Symbol

"""
The `AbstractString` type is the supertype of all string implementations in
Julia. Strings are encodings of sequences of [Unicode](https://unicode.org/)
code points as represented by the `AbstractChar` type. Julia makes a few assumptions
about strings:

* Strings are encoded in terms of fixed-size "code units"
  * Code units can be extracted with `codeunit(s, i)`
  * The first code unit has index `1`
  * The last code unit has index `ncodeunits(s)`
  * Any index `i` such that `1 ≤ i ≤ ncodeunits(s)` is in bounds
* String indexing is done in terms of these code units:
  * Characters are extracted by `s[i]` with a valid string index `i`
  * Each `AbstractChar` in a string is encoded by one or more code units
  * Only the index of the first code unit of an `AbstractChar` is a valid index
  * The encoding of an `AbstractChar` is independent of what precedes or follows it
  * String encodings are [self-synchronizing](https://en.wikipedia.org/wiki/Self-synchronizing_code) – i.e. `isvalid(s, i)` is O(1)

Some string functions that extract code units, characters or substrings from
strings error if you pass them out-of-bounds or invalid string indices. This
includes `codeunit(s, i)` and `s[i]`. Functions that do string
index arithmetic take a more relaxed approach to indexing and give you the
closest valid string index when in-bounds, or when out-of-bounds, behave as if
there were an infinite number of characters padding each side of the string.
Usually these imaginary padding characters have code unit length `1` but string
types may choose different "imaginary" character sizes as makes sense for their
implementations (e.g. substrings may pass index arithmetic through to the
underlying string they provide a view into). Relaxed indexing functions include
those intended for index arithmetic: `thisind`, `nextind` and `prevind`. This
model allows index arithmetic to work with out-of-bounds indices as
intermediate values so long as one never uses them to retrieve a character,
which often helps avoid needing to code around edge cases.

See also [`codeunit`](@ref), [`ncodeunits`](@ref), [`thisind`](@ref),
[`nextind`](@ref), [`prevind`](@ref).
"""
AbstractString

## required string functions ##

"""
    ncodeunits(s::AbstractString)::Int

Return the number of code units in a string. Indices that are in bounds to
access this string must satisfy `1 ≤ i ≤ ncodeunits(s)`. Not all such indices
are valid – they may not be the start of a character, but they will return a
code unit value when calling `codeunit(s,i)`.

# Examples
```jldoctest
julia> ncodeunits("The Julia Language")
18

julia> ncodeunits("∫eˣ")
6

julia> ncodeunits('∫'), ncodeunits('e'), ncodeunits('ˣ')
(3, 1, 2)
```

See also [`codeunit`](@ref), [`checkbounds`](@ref), [`sizeof`](@ref),
[`length`](@ref), [`lastindex`](@ref).
"""
ncodeunits(s::AbstractString)

"""
    codeunit(s::AbstractString)::Type{<:Union{UInt8, UInt16, UInt32}}

Return the code unit type of the given string object. For ASCII, Latin-1, or
UTF-8 encoded strings, this would be `UInt8`; for UCS-2 and UTF-16 it would be
`UInt16`; for UTF-32 it would be `UInt32`. The code unit type need not be
limited to these three types, but it's hard to think of widely used string
encodings that don't use one of these units. `codeunit(s)` is the same as
`typeof(codeunit(s,1))` when `s` is a non-empty string.

See also [`ncodeunits`](@ref).
"""
codeunit(s::AbstractString)

const CodeunitType = Union{Type{UInt8},Type{UInt16},Type{UInt32}}

"""
    codeunit(s::AbstractString, i::Integer)::Union{UInt8, UInt16, UInt32}

Return the code unit value in the string `s` at index `i`. Note that

    codeunit(s, i) :: codeunit(s)

I.e. the value returned by `codeunit(s, i)` is of the type returned by
`codeunit(s)`.

# Examples
```jldoctest
julia> a = codeunit("Hello", 2)
0x65

julia> typeof(a)
UInt8
```

See also [`ncodeunits`](@ref), [`checkbounds`](@ref).
"""
@propagate_inbounds codeunit(s::AbstractString, i::Integer) = i isa Int ?
    throw(MethodError(codeunit, (s, i))) : codeunit(s, Int(i))

"""
    isvalid(s::AbstractString, i::Integer)::Bool

Predicate indicating whether the given index is the start of the encoding of a
character in `s` or not. If `isvalid(s, i)` is true then `s[i]` will return the
character whose encoding starts at that index, if it's false, then `s[i]` will
raise an invalid index error or a bounds error depending on if `i` is in bounds.
In order for `isvalid(s, i)` to be an O(1) function, the encoding of `s` must be
[self-synchronizing](https://en.wikipedia.org/wiki/Self-synchronizing_code). This
is a basic assumption of Julia's generic string support.

See also [`getindex`](@ref), [`iterate`](@ref), [`thisind`](@ref),
[`nextind`](@ref), [`prevind`](@ref), [`length`](@ref).

# Examples
```jldoctest
julia> str = "αβγdef";

julia> isvalid(str, 1)
true

julia> str[1]
'α': Unicode U+03B1 (category Ll: Letter, lowercase)

julia> isvalid(str, 2)
false

julia> str[2]
ERROR: StringIndexError: invalid index [2], valid nearby indices [1]=>'α', [3]=>'β'
Stacktrace:
[...]
```
"""
@propagate_inbounds isvalid(s::AbstractString, i::Integer) = i isa Int ?
    throw(MethodError(isvalid, (s, i))) : isvalid(s, Int(i))

"""
    iterate(s::AbstractString, i::Integer)::Union{Tuple{<:AbstractChar, Int}, Nothing}

Return a tuple of the character in `s` at index `i` with the index of the start
of the following character in `s`. This is the key method that allows strings to
be iterated, yielding a sequences of characters. The `iterate` function, as part
of the iteration protocol may assume that `i` is the start of a character in `s`.

See also [`getindex`](@ref), [`checkbounds`](@ref).
"""
@propagate_inbounds iterate(s::AbstractString, i::Integer) = i isa Int ?
    throw(MethodError(iterate, (s, i))) : iterate(s, Int(i))

## basic generic definitions ##

eltype(::Type{<:AbstractString}) = Char # some string types may use another AbstractChar

"""
    sizeof(str::AbstractString)

Size, in bytes, of the string `str`. Equal to the number of code units in `str` multiplied by
the size, in bytes, of one code unit in `str`.

# Examples
```jldoctest
julia> sizeof("")
0

julia> sizeof("∀")
3
```
"""
sizeof(s::AbstractString) = ncodeunits(s)::Int * sizeof(codeunit(s)::CodeunitType)
firstindex(s::AbstractString) = 1
lastindex(s::AbstractString) = thisind(s, ncodeunits(s)::Int)
isempty(s::AbstractString) = iszero(ncodeunits(s)::Int)

@propagate_inbounds first(s::AbstractString) = s[firstindex(s)]

function getindex(s::AbstractString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds return isvalid(s, i) ? (iterate(s, i)::NTuple{2,Any})[1] : string_index_err(s, i)
end

getindex(s::AbstractString, i::Colon) = s
# TODO: handle other ranges with stride ±1 specially?
# TODO: add more @propagate_inbounds annotations?
getindex(s::AbstractString, v::AbstractVector{<:Integer}) =
    sprint(io->(for i in v; write(io, s[i]) end), sizehint=length(v))
getindex(s::AbstractString, v::AbstractVector{Bool}) =
    throw(ArgumentError("logical indexing not supported for strings"))

function get(s::AbstractString, i::Integer, default)
    checkbounds(Bool, s, i) ? (@inbounds s[i]) : default
end

## bounds checking ##

checkbounds(::Type{Bool}, s::AbstractString, i::Integer) =
    1 ≤ i ≤ ncodeunits(s)::Int
checkbounds(::Type{Bool}, s::AbstractString, r::AbstractRange{<:Integer}) =
    isempty(r) || (1 ≤ minimum(r) && maximum(r) ≤ ncodeunits(s)::Int)
checkbounds(::Type{Bool}, s::AbstractString, I::AbstractArray{<:Real}) =
    all(i -> checkbounds(Bool, s, i), I)
checkbounds(::Type{Bool}, s::AbstractString, I::AbstractArray{<:Integer}) =
    all(i -> checkbounds(Bool, s, i), I)
checkbounds(s::AbstractString, I::Union{Integer,AbstractArray}) =
    checkbounds(Bool, s, I) ? nothing : throw(BoundsError(s, I))

## construction, conversion, promotion ##

string() = ""
string(s::AbstractString) = s

Vector{UInt8}(s::AbstractString) = unsafe_wrap(Vector{UInt8}, String(s))
Array{UInt8}(s::AbstractString) = unsafe_wrap(Vector{UInt8}, String(s))
Vector{T}(s::AbstractString) where {T<:AbstractChar} = collect(T, s)

Symbol(s::AbstractString) = Symbol(String(s))
Symbol(x...) = Symbol(string(x...))

convert(::Type{T}, s::T) where {T<:AbstractString} = s
convert(::Type{T}, s::AbstractString) where {T<:AbstractString} = T(s)::T

## summary ##

function summary(io::IO, s::AbstractString)
    prefix = isempty(s) ? "empty" : string(ncodeunits(s), "-codeunit")
    print(io, prefix, " ", typeof(s))
end

## string & character concatenation ##

"""
    *(s::Union{AbstractString, AbstractChar}, t::Union{AbstractString, AbstractChar}...)::AbstractString

Concatenate strings and/or characters, producing a [`String`](@ref) or
[`AnnotatedString`](@ref) (as appropriate). This is equivalent to calling the
[`string`](@ref) or [`annotatedstring`](@ref) function on the arguments. Concatenation of built-in string
types always produces a value of type `String` but other string types may choose
to return a string of a different type as appropriate.

# Examples
```jldoctest
julia> "Hello " * "world"
"Hello world"

julia> 'j' * "ulia"
"julia"
```
"""
function (*)(s1::Union{AbstractChar, AbstractString}, ss::Union{AbstractChar, AbstractString}...)
    if _isannotated(s1) || any(_isannotated, ss)
        annotatedstring(s1, ss...)
    else
        string(s1, ss...)
    end
end

one(::Union{T,Type{T}}) where {T<:AbstractString} = convert(T, "")

# This could be written as a single statement with three ||-clauses, however then effect
# analysis thinks it may throw and runtime checks are added.
# Also see `substring.jl` for the `::SubString{T}` method.
_isannotated(S::Type) = S != Union{} && (S <: AnnotatedString || S <: AnnotatedChar)
_isannotated(s) = _isannotated(typeof(s))

## generic string comparison ##

"""
    cmp(a::AbstractString, b::AbstractString)::Int

Compare two strings. Return `0` if both strings have the same length and the character
at each index is the same in both strings. Return `-1` if `a` is a prefix of `b`, or if
`a` comes before `b` in alphabetical order. Return `1` if `b` is a prefix of `a`, or if
`b` comes before `a` in alphabetical order (technically, lexicographical order by Unicode
code points).

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
    (iv1, iv2) = (iterate(a), iterate(b))
    while iv1 !== nothing && iv2 !== nothing
        (c, d) = (first(iv1)::AbstractChar, first(iv2)::AbstractChar)
        c ≠ d && return ifelse(c < d, -1, 1)
        (iv1, iv2) = (iterate(a, last(iv1)), iterate(b, last(iv2)))
    end
    return iv1 === nothing ? (iv2 === nothing ? 0 : -1) : 1
end

"""
    ==(a::AbstractString, b::AbstractString)::Bool

Test whether two strings are equal character by character (technically, Unicode
code point by code point). Should either string be a [`AnnotatedString`](@ref) the
string properties must match too.

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
    isless(a::AbstractString, b::AbstractString)::Bool

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

@assume_effects :total function cmp(a::Symbol, b::Symbol)
    Int(sign(ccall(:strcmp, Int32, (Cstring, Cstring), a, b)))
end

isless(a::Symbol, b::Symbol) = cmp(a, b) < 0

# hashing

hash(s::AbstractString, h::UInt) = hash(String(s), h)

## character index arithmetic ##

"""
    length(s::AbstractString)::Int
    length(s::AbstractString, i::Integer, j::Integer)::Int

Return the number of characters in string `s` from indices `i` through `j`.

This is computed as the number of code unit indices from `i` to `j` which are
valid character indices. With only a single string argument, this computes
the number of characters in the entire string. With `i` and `j` arguments it
computes the number of indices between `i` and `j` inclusive that are valid
indices in the string `s`. In addition to in-bounds values, `i` may take the
out-of-bounds value `ncodeunits(s) + 1` and `j` may take the out-of-bounds
value `0`.

!!! note
    The time complexity of this operation is linear in general. That is, it
    will take the time proportional to the number of bytes or characters in
    the string because it counts the value on the fly. This is in contrast to
    the method for arrays, which is a constant-time operation.

See also [`isvalid`](@ref), [`ncodeunits`](@ref), [`lastindex`](@ref),
[`thisind`](@ref), [`nextind`](@ref), [`prevind`](@ref).

# Examples
```jldoctest
julia> length("jμΛIα")
5
```
"""
length(s::AbstractString) = @inbounds return length(s, 1, ncodeunits(s)::Int)

function length(s::AbstractString, i::Int, j::Int)
    @boundscheck begin
        0 < i ≤ ncodeunits(s)::Int+1 || throw(BoundsError(s, i))
        0 ≤ j < ncodeunits(s)::Int+1 || throw(BoundsError(s, j))
    end
    n = 0
    for k = i:j
        @inbounds n += isvalid(s, k)
    end
    return n
end

@propagate_inbounds length(s::AbstractString, i::Integer, j::Integer) =
    length(s, Int(i), Int(j))

"""
    thisind(s::AbstractString, i::Integer)::Int

If `i` is in bounds in `s` return the index of the start of the character whose
encoding code unit `i` is part of. In other words, if `i` is the start of a
character, return `i`; if `i` is not the start of a character, rewind until the
start of a character and return that index. If `i` is equal to 0 or `ncodeunits(s)+1`
return `i`. In all other cases throw `BoundsError`.

# Examples
```jldoctest
julia> thisind("α", 0)
0

julia> thisind("α", 1)
1

julia> thisind("α", 2)
1

julia> thisind("α", 3)
3

julia> thisind("α", 4)
ERROR: BoundsError: attempt to access 2-codeunit String at index [4]
[...]

julia> thisind("α", -1)
ERROR: BoundsError: attempt to access 2-codeunit String at index [-1]
[...]
```
"""
thisind(s::AbstractString, i::Integer) = thisind(s, Int(i))

function thisind(s::AbstractString, i::Int)
    z = ncodeunits(s)::Int + 1
    i == z && return i
    @boundscheck 0 ≤ i ≤ z || throw(BoundsError(s, i))
    @inbounds while 1 < i && !(isvalid(s, i)::Bool)
        i -= 1
    end
    return i
end

"""
    prevind(str::AbstractString, i::Integer, n::Integer=1)::Int

* Case `n == 1`

  If `i` is in bounds in `s` return the index of the start of the character whose
  encoding starts before index `i`. In other words, if `i` is the start of a
  character, return the start of the previous character; if `i` is not the start
  of a character, rewind until the start of a character and return that index.
  If `i` is equal to `1` return `0`.
  If `i` is equal to `ncodeunits(str)+1` return `lastindex(str)`.
  Otherwise throw `BoundsError`.

* Case `n > 1`

  Behaves like applying `n` times `prevind` for `n==1`. The only difference
  is that if `n` is so large that applying `prevind` would reach `0` then each remaining
  iteration decreases the returned value by `1`.
  This means that in this case `prevind` can return a negative value.

* Case `n == 0`

  Return `i` only if `i` is a valid index in `str` or is equal to `ncodeunits(str)+1`.
  Otherwise `StringIndexError` or `BoundsError` is thrown.

# Examples
```jldoctest
julia> prevind("α", 3)
1

julia> prevind("α", 1)
0

julia> prevind("α", 0)
ERROR: BoundsError: attempt to access 2-codeunit String at index [0]
[...]

julia> prevind("α", 2, 2)
0

julia> prevind("α", 2, 3)
-1
```
"""
prevind(s::AbstractString, i::Integer, n::Integer) = prevind(s, Int(i), Int(n))
prevind(s::AbstractString, i::Integer)             = prevind(s, Int(i))
prevind(s::AbstractString, i::Int)                 = prevind(s, i, 1)

function prevind(s::AbstractString, i::Int, n::Int)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    z = ncodeunits(s)::Int + 1
    @boundscheck 0 < i ≤ z || throw(BoundsError(s, i))
    n == 0 && return thisind(s, i)::Int == i ? i : string_index_err(s, i)
    while n > 0 && 1 < i
        @inbounds n -= isvalid(s, i -= 1)::Bool
    end
    return i - n
end

"""
    nextind(str::AbstractString, i::Integer, n::Integer=1)::Int

* Case `n == 1`

  If `i` is in bounds in `s` return the index of the start of the character whose
  encoding starts after index `i`. In other words, if `i` is the start of a
  character, return the start of the next character; if `i` is not the start
  of a character, move forward until the start of a character and return that index.
  If `i` is equal to `0` return `1`.
  If `i` is in bounds but greater or equal to `lastindex(str)` return `ncodeunits(str)+1`.
  Otherwise throw `BoundsError`.

* Case `n > 1`

  Behaves like applying `n` times `nextind` for `n==1`. The only difference
  is that if `n` is so large that applying `nextind` would reach `ncodeunits(str)+1` then
  each remaining iteration increases the returned value by `1`. This means that in this
  case `nextind` can return a value greater than `ncodeunits(str)+1`.

* Case `n == 0`

  Return `i` only if `i` is a valid index in `s` or is equal to `0`.
  Otherwise `StringIndexError` or `BoundsError` is thrown.

# Examples
```jldoctest
julia> nextind("α", 0)
1

julia> nextind("α", 1)
3

julia> nextind("α", 3)
ERROR: BoundsError: attempt to access 2-codeunit String at index [3]
[...]

julia> nextind("α", 0, 2)
3

julia> nextind("α", 1, 2)
4
```
"""
nextind(s::AbstractString, i::Integer, n::Integer) = nextind(s, Int(i), Int(n))
nextind(s::AbstractString, i::Integer)             = nextind(s, Int(i))
nextind(s::AbstractString, i::Int)                 = nextind(s, i, 1)

function nextind(s::AbstractString, i::Int, n::Int)
    n < 0 && throw(ArgumentError("n cannot be negative: $n"))
    z = ncodeunits(s)::Int
    @boundscheck 0 ≤ i ≤ z || throw(BoundsError(s, i))
    n == 0 && return thisind(s, i)::Int == i ? i : string_index_err(s, i)
    while n > 0 && i < z
        @inbounds n -= isvalid(s, i += 1)::Bool
    end
    return i + n
end

## string index iteration type ##

struct EachStringIndex{T<:AbstractString}
    s::T
end
keys(s::AbstractString) = EachStringIndex(s)

length(e::EachStringIndex) = length(e.s)
first(::EachStringIndex) = 1
last(e::EachStringIndex) = lastindex(e.s)
iterate(e::EachStringIndex, state=firstindex(e.s)) = state > ncodeunits(e.s) ? nothing : (state, nextind(e.s, state))
eltype(::Type{<:EachStringIndex}) = Int

"""
    isascii(c::Union{AbstractChar,AbstractString})::Bool

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
For example, `isascii` can be used as a predicate function for [`filter`](@ref) or [`replace`](@ref)
to remove or replace non-ASCII characters, respectively:
```jldoctest
julia> filter(isascii, "abcdeγfgh") # discard non-ASCII chars
"abcdefgh"

julia> replace("abcdeγfgh", !isascii=>' ') # replace non-ASCII chars with spaces
"abcde fgh"
```
"""
isascii(c::Char) = bswap(reinterpret(UInt32, c)) < 0x80
isascii(s::AbstractString) = all(isascii, s)
isascii(c::AbstractChar) = UInt32(c) < 0x80

@inline function _isascii(code_units::AbstractVector{CU}, first, last) where {CU}
    r = zero(CU)
    for n = first:last
        @inbounds r |= code_units[n]
    end
    return 0 ≤ r < 0x80
end

#The chunking algorithm makes the last two chunks overlap inorder to keep the size fixed
@inline function  _isascii_chunks(chunk_size,cu::AbstractVector{CU}, first,last) where {CU}
    n=first
    while n <= last - chunk_size
        _isascii(cu,n,n+chunk_size-1) || return false
        n += chunk_size
    end
    return  _isascii(cu,last-chunk_size+1,last)
end
"""
    isascii(cu::AbstractVector{CU}) where {CU <: Integer}::Bool

Test whether all values in the vector belong to the ASCII character set (0x00 to 0x7f).
This function is intended to be used by other string implementations that need a fast ASCII check.
"""
function isascii(cu::AbstractVector{CU}) where {CU <: Integer}
    chunk_size = 1024
    chunk_threshold =  chunk_size + (chunk_size ÷ 2)
    first = firstindex(cu);   last = lastindex(cu)
    l = last - first + 1
    l < chunk_threshold && return _isascii(cu,first,last)
    return _isascii_chunks(chunk_size,cu,first,last)
end

## string map, filter ##

function map(f, s::AbstractString)
    out = StringVector(max(4, sizeof(s)::Int÷sizeof(codeunit(s)::CodeunitType)))
    index = UInt(1)
    for c::AbstractChar in s
        c′ = f(c)
        isa(c′, AbstractChar) || throw(ArgumentError(
            "map(f, s::AbstractString) requires f to return AbstractChar; " *
            "try map(f, collect(s)) or a comprehension instead"))
        index + 3 > length(out) && resize!(out, unsigned(2 * length(out)))
        index += __unsafe_string!(out, convert(Char, c′), index)
    end
    resize!(out, index-1)
    sizehint!(out, index-1)
    return String(out)
end

function filter(f, s::AbstractString)
    out = IOBuffer(sizehint=sizeof(s))
    for c in s
        f(c) && write(out, c)
    end
    String(_unsafe_take!(out))
end

## string first and last ##

"""
    first(s::AbstractString, n::Integer)

Get a string consisting of the first `n` characters of `s`.

# Examples
```jldoctest
julia> first("∀ϵ≠0: ϵ²>0", 0)
""

julia> first("∀ϵ≠0: ϵ²>0", 1)
"∀"

julia> first("∀ϵ≠0: ϵ²>0", 3)
"∀ϵ≠"
```
"""
first(s::AbstractString, n::Integer) = @inbounds s[1:min(end, nextind(s, 0, n))]

"""
    last(s::AbstractString, n::Integer)

Get a string consisting of the last `n` characters of `s`.

# Examples
```jldoctest
julia> last("∀ϵ≠0: ϵ²>0", 0)
""

julia> last("∀ϵ≠0: ϵ²>0", 1)
"0"

julia> last("∀ϵ≠0: ϵ²>0", 3)
"²>0"
```
"""
last(s::AbstractString, n::Integer) = @inbounds s[max(1, prevind(s, ncodeunits(s)+1, n)):end]

"""
    reverseind(v, i)

Given an index `i` in [`reverse(v)`](@ref), return the corresponding index in
`v` so that `v[reverseind(v,i)] == reverse(v)[i]`. (This can be nontrivial in
cases where `v` contains non-ASCII characters.)

# Examples
```jldoctest
julia> s = "Julia🚀"
"Julia🚀"

julia> r = reverse(s)
"🚀ailuJ"

julia> for i in eachindex(s)
           print(r[reverseind(r, i)])
       end
Julia🚀
```
"""
reverseind(s::AbstractString, i::Integer) = thisind(s, ncodeunits(s)-i+1)

"""
    repeat(s::AbstractString, r::Integer)

Repeat a string `r` times. This can be written as `s^r`.

See also [`^`](@ref :^(::Union{AbstractString, AbstractChar}, ::Integer)).

# Examples
```jldoctest
julia> repeat("ha", 3)
"hahaha"
```
"""
repeat(s::AbstractString, r::Integer) = repeat(String(s), r)

"""
    ^(s::Union{AbstractString,AbstractChar}, n::Integer)::AbstractString

Repeat a string or character `n` times. This can also be written as `repeat(s, n)`.

See also [`repeat`](@ref).

# Examples
```jldoctest
julia> "Test "^3
"Test Test Test "
```
"""
(^)(s::Union{AbstractString,AbstractChar}, r::Integer) = repeat(s, r)

# reverse-order iteration for strings and indices thereof
iterate(r::Iterators.Reverse{<:AbstractString}, i=lastindex(r.itr)) = i < firstindex(r.itr) ? nothing : (r.itr[i], prevind(r.itr, i))
iterate(r::Iterators.Reverse{<:EachStringIndex}, i=lastindex(r.itr.s)) = i < firstindex(r.itr.s) ? nothing : (i, prevind(r.itr.s, i))

## code unit access ##

"""
    CodeUnits(s::AbstractString)

Wrap a string (without copying) in an immutable vector-like object that accesses the code units
of the string's representation.
"""
struct CodeUnits{T,S<:AbstractString} <: DenseVector{T}
    s::S
    CodeUnits(s::S) where {S<:AbstractString} = new{codeunit(s),S}(s)
end

length(s::CodeUnits) = ncodeunits(s.s)
sizeof(s::CodeUnits{T}) where {T} = ncodeunits(s.s) * sizeof(T)
size(s::CodeUnits) = (length(s),)
elsize(s::Type{<:CodeUnits{T}}) where {T} = sizeof(T)
@propagate_inbounds getindex(s::CodeUnits, i::Int) = codeunit(s.s, i)
IndexStyle(::Type{<:CodeUnits}) = IndexLinear()
@inline iterate(s::CodeUnits, i=1) = (i % UInt) - 1 < length(s) ? (@inbounds s[i], i + 1) : nothing


write(io::IO, s::CodeUnits) = write(io, s.s)

cconvert(::Type{Ptr{T}},    s::CodeUnits{T}) where {T} = cconvert(Ptr{T}, s.s)
cconvert(::Type{Ptr{Int8}}, s::CodeUnits{UInt8}) = cconvert(Ptr{Int8}, s.s)

"""
    codeunits(s::AbstractString)

Obtain a vector-like object containing the code units of a string.
Returns a `CodeUnits` wrapper by default, but `codeunits` may optionally be defined
for new string types if necessary.

# Examples
```jldoctest
julia> codeunits("Juλia")
6-element Base.CodeUnits{UInt8, String}:
 0x4a
 0x75
 0xce
 0xbb
 0x69
 0x61
```
"""
codeunits(s::AbstractString) = CodeUnits(s)

function _split_rest(s::AbstractString, n::Int)
    lastind = lastindex(s)
    i = try
        prevind(s, lastind, n)
    catch e
        e isa BoundsError || rethrow()
        _check_length_split_rest(length(s), n)
    end
    last_n = SubString(s, nextind(s, i), lastind)
    front = s[begin:i]
    return front, last_n
end

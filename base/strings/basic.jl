# This file is a part of Julia. License is MIT: https://julialang.org/license

## core string functions ##

endof(s::AbstractString) = error("you must implement endof(", typeof(s), ")")
next(s::AbstractString, i::Int) = error("you must implement next(", typeof(s), ",Int)")
next(s::AbstractString, i::Integer) = next(s,Int(i))

string() = ""
string(s::AbstractString) = s

(::Type{Vector{UInt8}})(s::AbstractString) = Vector{UInt8}(String(s))
(::Type{Array{UInt8}})(s::AbstractString) = Vector{UInt8}(s)
(::Type{Vector{Char}})(s::AbstractString) = collect(s)

Symbol(s::AbstractString) = Symbol(String(s))

# string types are convertible
convert(::Type{T}, s::T) where {T<:AbstractString} = s
convert(::Type{T}, s::AbstractString) where {T<:AbstractString} = T(s)

## generic supplied functions ##

start(s::AbstractString) = 1
done(s::AbstractString,i) = (i > endof(s))
getindex(s::AbstractString, i::Int) = next(s,i)[1]
getindex(s::AbstractString, i::Integer) = s[Int(i)]
getindex(s::AbstractString, i::Colon) = s
getindex(s::AbstractString, r::UnitRange{<:Integer}) = s[Int(first(r)):Int(last(r))]
# TODO: handle other ranges with stride ±1 specially?
getindex(s::AbstractString, v::AbstractVector{<:Integer}) =
    sprint(length(v), io->(for i in v; write(io,s[i]) end))
getindex(s::AbstractString, v::AbstractVector{Bool}) =
    throw(ArgumentError("logical indexing not supported for strings"))

get(s::AbstractString, i::Integer, default) = isvalid(s,i) ? s[i] : default

"""
    sizeof(s::AbstractString)

The number of bytes in string `s`.

# Examples
```jldoctest
julia> sizeof("❤")
3
```
"""
sizeof(s::AbstractString) = error("type $(typeof(s)) has no canonical binary representation")

eltype(::Type{<:AbstractString}) = Char

"""
    *(s::Union{AbstractString, Char}, t::Union{AbstractString, Char}...)

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

# generic number of code units; implementations generally know how long a string
# is though and should override this with a more efficient method
ncodeunits(s::AbstractString) = nextind(s, endof(s)) - 1

"""
    length(s::AbstractString)

The number of characters in string `s`.

# Examples
```jldoctest
julia> length("jμΛIα")
5
```
"""
function length(s::AbstractString)
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

## string comparison functions ##
"""
    cmp(a::AbstractString, b::AbstractString)

Compare two strings for equality.

Return `0` if both strings have the same length and the character
at each index is the same in both strings.
Return `-1` if `a` is a substring of `b`, or if `a` comes before
`b` in alphabetical order.
Return `1` if `b` is a substring of `a`, or if `b` comes before
`a` in alphabetical order.

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
    if a === b
        return 0
    end
    i = start(a)
    j = start(b)
    while !done(a,i)
        if done(b,j)
            return +1
        end
        c, i = next(a,i)
        d, j = next(b,j)
        if c != d
            return c < d ? -1 : +1
        end
    end
    done(b,j) ? 0 : -1
end

"""
    ==(a::AbstractString, b::AbstractString)

Test whether two strings are equal character by character.

# Examples
```jldoctest
julia> "abc" == "abc"
true

julia> "abc" == "αβγ"
false
```
"""
==(a::AbstractString, b::AbstractString) = cmp(a,b) == 0

"""
    isless(a::AbstractString, b::AbstractString)

Test whether string `a` comes before string `b` in alphabetical order.

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
isless(a::AbstractString, b::AbstractString) = cmp(a,b) < 0

# faster comparisons for symbols

cmp(a::Symbol, b::Symbol) = Int(sign(ccall(:strcmp, Int32, (Cstring, Cstring), a, b)))

isless(a::Symbol, b::Symbol) = cmp(a,b) < 0

## Generic validation functions ##

"""
    isvalid(str::AbstractString, i::Integer)

Tell whether index `i` is valid for the given string.

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
function isvalid(s::AbstractString, i::Integer)
    i < 1 && return false
    done(s,i) && return false
    try
        next(s,i)
        true
    catch
        false
    end
end

## Generic indexing functions ##

"""
    thisind(s::AbstractString, i::Integer)

If `i` is the index into a character in `s` then `thisind` returns the index of the
start of that character. If `i < start(s)` then it returns `start(s) - 1`.
If `i > ncodeunits(s)` then it returns `ncodeunits(s) + 1`.

# Examples
```jldoctest
julia> thisind("αβγdef", -5)
0

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
10
"""
function thisind(s::AbstractString, i::Integer)
    j = Int(i)
    isvalid(s, j) && return j
    j < start(s) && return 0
    n = ncodeunits(s)
    j > n && return n + 1
    prevind(s, j)
end

"""
    prevind(str::AbstractString, i::Integer, nchar::Integer=1)

Get the previous valid string index before `i`.
Returns a value less than `1` at the beginning of the string.
If the `nchar` argument is given the function goes back `nchar` characters.

# Examples
```jldoctest
julia> prevind("αβγdef", 3)
1

julia> prevind("αβγdef", 1)
0

julia> prevind("αβγdef", 3, 2)
0
```
"""
function prevind(s::AbstractString, i::Integer)
    e = endof(s)
    if i > e
        return e
    end
    j = Int(i)-1
    while j >= 1
        if isvalid(s,j)
            return j
        end
        j -= 1
    end
    return 0 # out of range
end

function prevind(s::AbstractString, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    e = endof(s)
    j = Int(i)
    j < 1 && return 0
    while nchar > 0
        if j > e
            j = e
        else
            j -= 1
            while j >= 1 && !isvalid(s,j)
                j -= 1
            end
        end
        j < 1 && return 0
        nchar -= 1
    end
    j
end

"""
    nextind(str::AbstractString, i::Integer, nchar::Integer=1)

Get the next valid string index after `i`.
Returns a value greater than `endof(str)` at or after the end of the string.
If the `nchar` argument is given the function goes forward `nchar` characters.

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
function nextind(s::AbstractString, i::Integer)
    e = endof(s)
    if i < 1
        return 1
    end
    if i > e
        return Int(i)+1
    end
    for j = Int(i)+1:e
        if isvalid(s,j)
            return j
        end
    end
    next(s,e)[2] # out of range
end

function nextind(s::AbstractString, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    e = endof(s)
    j = Int(i)
    while nchar > 0
        if j < 1
            j = 1
        else
            j > e && return j + nchar
            j == e && return next(s,e)[2] + nchar - 1
            for outer j = j+1:e
                isvalid(s,j) && break
            end
        end
        nchar -= 1
    end
    j
end

checkbounds(s::AbstractString, i::Integer) = start(s) <= i <= endof(s) || throw(BoundsError(s, i))
checkbounds(s::AbstractString, r::AbstractRange{<:Integer}) = isempty(r) || (minimum(r) >= start(s) && maximum(r) <= endof(s)) || throw(BoundsError(s, r))
# The following will end up using a deprecated checkbounds, when the covariant parameter is not Integer
checkbounds(s::AbstractString, I::AbstractArray{<:Real}) = all(i -> checkbounds(s, i), I)
checkbounds(s::AbstractString, I::AbstractArray{<:Integer}) = all(i -> checkbounds(s, i), I)


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
function ind2chr(s::AbstractString, i::Integer)
    s[i] # throws error if invalid
    unsafe_ind2chr(s, i)
end

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
function chr2ind(s::AbstractString, i::Integer)
    i < start(s) && throw(BoundsError(s, i))
    k = unsafe_chr2ind(s, i)
    s[k] # throws error if invalid
    k
end

function map_chr_ind(s::AbstractString, i::Integer, stop, ret)
    j = 1
    k = start(s)
    while true
        i == stop((j, k)) && return ret((j, k)) # k could point after the last character
        _, k = next(s, k)
        j += 1
    end
end

unsafe_ind2chr(s::AbstractString, i::Integer) = map_chr_ind(s, i, last, first)
unsafe_chr2ind(s::AbstractString, i::Integer) = map_chr_ind(s, i, first, last)


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
isascii(c::Char) = c < Char(0x80)
isascii(s::AbstractString) = all(isascii, s)

## string promotion rules ##

promote_rule(::Type{<:AbstractString}, ::Type{<:AbstractString}) = String

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
isxdigit(c::Char) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'

## uppercase, lowercase, and titlecase transformations ##

"""
    uppercase(s::AbstractString)

Return `s` with all characters converted to uppercase.

# Examples
```jldoctest
julia> uppercase("Julia")
"JULIA"
```
"""
uppercase(s::AbstractString) = map(uppercase, s)

"""
    lowercase(s::AbstractString)

Return `s` with all characters converted to lowercase.

# Examples
```jldoctest
julia> lowercase("STRINGS AND THINGS")
"strings and things"
```
"""
lowercase(s::AbstractString) = map(lowercase, s)

"""
    titlecase(s::AbstractString)

Capitalize the first character of each word in `s`.
See also [`ucfirst`](@ref) to capitalize only the first
character in `s`.

# Examples
```jldoctest
julia> titlecase("the julia programming language")
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
    ucfirst(s::AbstractString)

Return `string` with the first character converted to uppercase
(technically "title case" for Unicode).
See also [`titlecase`](@ref) to capitalize the first character of
every word in `s`.

# Examples
```jldoctest
julia> ucfirst("python")
"Python"
```
"""
function ucfirst(s::AbstractString)
    isempty(s) && return s
    c = s[1]
    tc = titlecase(c)
    return c==tc ? s : string(tc,s[nextind(s,1):end])
end

"""
    lcfirst(s::AbstractString)

Return `string` with the first character converted to lowercase.

# Examples
```jldoctest
julia> lcfirst("Julia")
"julia"
```
"""
function lcfirst(s::AbstractString)
    isempty(s) || islower(s[1]) ? s : string(lowercase(s[1]),s[nextind(s,1):end])
end

## string map, filter, has ##

function map(f, s::AbstractString)
    out = IOBuffer(StringVector(endof(s)),true,true)
    truncate(out,0)
    for c in s
        c2 = f(c)
        if !isa(c2,Char)
            throw(ArgumentError("map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"))
        end
        write(out, c2::Char)
    end
    String(take!(out))
end

function filter(f, s::AbstractString)
    out = IOBuffer(StringVector(endof(s)),true,true)
    truncate(out,0)
    for c in s
        if f(c)
            write(out, c)
        end
    end
    String(take!(out))
end

## string first and last ##

"""
    first(str::AbstractString, nchar::Integer)

Get a string consisting of the first `nchar` characters of `str`.

```jldoctest
julia> first("∀ϵ≠0: ϵ²>0", 0)
""

julia> first("∀ϵ≠0: ϵ²>0", 1)
"∀"

julia> first("∀ϵ≠0: ϵ²>0", 3)
"∀ϵ≠"
```
"""
function first(str::AbstractString, nchar::Integer)
    if 0 <= nchar <= 1
        return str[1:nchar]
    end
    str[1:nextind(str, 1, nchar-1)]
end

"""
    last(str::AbstractString, nchar::Integer)

Get a string consisting of the last `nchar` characters of `str`.

```jldoctest
julia> last("∀ϵ≠0: ϵ²>0", 0)
""

julia> last("∀ϵ≠0: ϵ²>0", 1)
"0"

julia> last("∀ϵ≠0: ϵ²>0", 3)
"²>0"
```
"""
function last(str::AbstractString, nchar::Integer)
    e = endof(str)
    if 0 <= nchar <= 1
        return str[(e-nchar+1):e]
    end
    str[prevind(str, e, nchar-1):e]
end

# reverse-order iteration for strings and indices thereof
start(r::Iterators.Reverse{<:AbstractString}) = endof(r.itr)
done(r::Iterators.Reverse{<:AbstractString}, i) = i < start(r.itr)
next(r::Iterators.Reverse{<:AbstractString}, i) = (r.itr[i], prevind(r.itr, i))
start(r::Iterators.Reverse{<:EachStringIndex}) = endof(r.itr.s)
done(r::Iterators.Reverse{<:EachStringIndex}, i) = i < start(r.itr.s)
next(r::Iterators.Reverse{<:EachStringIndex}, i) = (i, prevind(r.itr.s, i))

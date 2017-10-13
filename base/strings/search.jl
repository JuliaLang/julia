# This file is a part of Julia. License is MIT: https://julialang.org/license

const Chars = Union{Char,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}

"""
    search(string, chars, [start::Integer])

Search for the first occurrence of the given characters within the given string. The second
argument may be a single character, a vector or a set of characters, a string, or a regular
expression (though regular expressions are only allowed on contiguous strings, such as ASCII
or UTF-8 strings).
Alternatively the first and the second arguments may be arrays of `Int8` or `UInt8`.
The third argument optionally specifies a starting index.
If it is not a valid index into the first argument then error is thrown.
The return value when the second argument is a string or a vector of bytes is the samllest
range of indexes where the matching sequence is found, such that `s[search(s,x)] == x`,
or `0:-1` if no such range of indexes exits:

`search(string, "substring", i)` = `start:end` such that `string[start:end] == "substring"`
and `start >= i`.

In particular a search for a zero length second argument returns `i:(i-1)`.

If the first argument is empty and third argument is not given `0:-1` is returned.

The return value when the second argument is a signle character, a vector or a set of
characters is the smallest index for which the match is found, i.e. `s[search(s,x)] in x`,
or `0` if no such index exists.

`search(string, ['a', 'b'], i)` = `index` such that `string[index] in ['a', 'b']`.

In particular a search for `Char[]` or `Set{Char}()` as a second argument returns `0`.

If the first argument is empty and third argument is not given `0` is returned.

Additionally the second argument can be `Int8` or `UInt8` in this case if the first argument
is `String` then it is searched as if it were an array of bytes and a returned match might
point to an invalid index for a given string.

# Examples
```jldoctest
julia> search("Hello to the world", "z")
0:-1

julia> search("JuliaLang","Julia")
1:5

julia> search(Int8[1,2,3], Int8[2,3])
2:3

julia> search("Hello to the world", 'z')
0

julia> search("Hello to the world", ['e', 'l'])
2

julia> search("", ['e', 'l'])
0
```
"""
function search(s::AbstractString, c::Chars, i::Integer)
    isvalid(s, i) || throw(ArgumentError("index $i is invalid"))
    isempty(c) && return 0
    while !done(s, i)
        d, j = next(s, i)
        if d in c
            return i
        end
        i = j
    end
    return 0
end

search(s::AbstractString, c::Chars) = s == "" ? 0 : search(s, c, start(s))

in(c::Char, s::AbstractString) = search(s,c) != 0

function _searchindex(s, t, i)
    # assumes that caller has checked if i is a valid index
    isempty(t) && return i

    t1, j2 = next(t,start(t))
    while true
        i = search(s, t1, i)
        i == 0 && return 0
        c, ii = next(s, i)
        j = j2; k = ii
        matched = true
        while !done(t, j)
            if done(s, k)
                matched = false
                break
            end
            c, k = next(s, k)
            d, j = next(t, j)
            if c != d
                matched = false
                break
            end
        end
        matched && return i
        i = ii
    end
end

function _search_bloom_mask(c)
    UInt64(1) << (c & 63)
end

_nthbyte(s::String, i) = @inbounds codeunit(s, i)
_nthbyte(a::ByteArray, i) = @inbounds a[i]

function _searchindex(s::Union{String,ByteArray}, t::Union{String,ByteArray}, i)
    # assumes that caller has checked if i is a valid index
    isempty(t) && return i

    m = sizeof(s)
    n = sizeof(t)

    n == 0 && return i
    n == 1 && return search(s, _nthbyte(t, 1), i)
    w = m - n
    w < i - 1 && return 0

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = _nthbyte(t, n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(_nthbyte(t, j))
        if _nthbyte(t, j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if _nthbyte(s, i + n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s, i + j + 1) != _nthbyte(t, j + 1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i + 1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s, i + n + 1)) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(_nthbyte(s, i + n + 1)) == 0
                i += n
            end
        end
        i += 1
    end
    0
end

"""
    searchindex(s, t, [start::Integer])

Similar to [`search`](@ref), but return only the start index at which the second argument
is found, or `0` if it is not.

# Examples
```jldoctest
julia> searchindex("Hello to the world", "z")
0

julia> searchindex("JuliaLang", "Julia")
1

julia> searchindex("JuliaLang", "Lang")
6

julia> searchindex(Int8[1, 2, 3], Int8[3], 2)
3

julia> searchindex("abc", 'b', 3)
0

julia> searchindex("", 'b', 3)
0
```
"""
searchindex(s::AbstractString, c::Chars, i::Integer) = search(s, c, i)
searchindex(s::AbstractString, c::Chars) = s == "" ? 0 : search(s, c, start(s))
function searchindex(s::ByteArray, t::ByteArray, i::Integer)
    if !(1 <= i <= length(s))
        throw(BoundsError(s, i))
    end
    _searchindex(s, t, i)
end
searchindex(s::ByteArray, t::ByteArray) = length(s) == 0 ? 0 : searchindex(s, t, 1)

function searchindex(s::AbstractString, t::AbstractString, i::Integer)
    # Check for fast case of a single character
    # (for multi-character sequences, use searchindex on byte arrays instead)
    if endof(t) == 1
        search(s, t[1], i)
    else
        isvalid(s, i) || throw(ArgumentError("index $i is invalid"))
        _searchindex(s, t, i)
    end
end

searchindex(s::AbstractString, t::AbstractString) = s == "" ? 0 : searchindex(s, t, start(s))

function _search(s, t, i::Integer)
    idx = searchindex(s, t, i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

search(s::AbstractString, t::AbstractString, i::Integer) = _search(s, t, i)
search(s::AbstractString, t::AbstractString) = s == "" ? (0:-1) : _search(s, t, start(s))
search(s::ByteArray, t::ByteArray, i::Integer) = _search(s, t, i)
search(s::ByteArray, t::ByteArray) = length(s)==0 ? (0:-1) : _search(s, t, 1)

function rsearch(s::AbstractString, c::Chars)
    j = search(RevString(s), c)
    j == 0 && return 0
    endof(s)-j+1
end

"""
    rsearch(s::AbstractString, chars::Chars, [start::Integer])

Similar to [`search`](@ref), but returning the last occurrence of the given characters within the
given string, searching in reverse from `start`.

# Examples
```jldoctest
julia> rsearch("aaabbb","b")
6:6
```
"""
function rsearch(s::AbstractString, c::Chars, i::Integer)
    e = endof(s)
    j = search(RevString(s), c, e-i+1)
    j == 0 && return 0
    e-j+1
end

function _rsearchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t = RevString(t)
    rs = RevString(s)
    l = endof(s)
    t1, j2 = next(t,start(t))
    while true
        i = rsearch(s,t1,i)
        if i == 0 return 0 end
        c, ii = next(rs,l-i+1)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(rs,k)
                matched = false
                break
            end
            c, k = next(rs,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return nextind(s,l-k+1)
        end
        i = l-ii+1
    end
end

function _rsearchindex(s::Union{String,ByteArray}, t::Union{String,ByteArray}, k)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 0 <= k <= m ? max(k, 1) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return rsearch(s, _nthbyte(t,1), k)
    end

    w = m - n
    if w < 0 || k <= 0
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tfirst = _nthbyte(t,1)
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tfirst && j > 1
            skip = j - 2
        end
    end

    i = min(k - n + 1, w + 1)
    while i > 0
        if _nthbyte(s,i) == tfirst
            # check candidate
            j = 1
            while j < n
                if _nthbyte(s,i+j) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n
                return i
            end

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            end
        end
        i -= 1
    end

    0
end

rsearchindex(s::ByteArray, t::ByteArray, i::Integer) = _rsearchindex(s,t,i)

"""
    rsearchindex(s::AbstractString, substring, [start::Integer])

Similar to [`rsearch`](@ref), but return only the start index at which the substring is found, or `0` if it is not.

# Examples
```jldoctest
julia> rsearchindex("aaabbb","b")
6

julia> rsearchindex("aaabbb","a")
3
```
"""
rsearchindex(s::AbstractString, t::AbstractString, i::Integer) = _rsearchindex(s,t,i)
rsearchindex(s::AbstractString, t::AbstractString) = (isempty(s) && isempty(t)) ? 1 : rsearchindex(s,t,endof(s))

function rsearchindex(s::String, t::String)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1])
    else
        _rsearchindex(s, t, sizeof(s))
    end
end

function rsearchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1], i)
    elseif endof(t) != 0
        _rsearchindex(s, t, nextind(s, i)-1)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end

function _rsearch(s, t, i::Integer)
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

rsearch(s::AbstractString, t::AbstractString, i::Integer=endof(s)) = _rsearch(s, t, i)
rsearch(s::ByteArray, t::ByteArray, i::Integer=endof(s)) = _rsearch(s, t, i)

"""
    contains(haystack::AbstractString, needle::Union{AbstractString,Char})

Determine whether the second argument is a substring of the first.

# Examples
```jldoctest
julia> contains("JuliaLang is pretty cool!", "Julia")
true
```
"""
contains(haystack::AbstractString, needle::Union{AbstractString,Char}) = searchindex(haystack,needle)!=0

in(::AbstractString, ::AbstractString) = error("use contains(x,y) for string containment")

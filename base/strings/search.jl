# This file is a part of Julia. License is MIT: http://julialang.org/license

typealias Chars Union{Char,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}

"""
    search(string::AbstractString, chars::Chars, [start::Integer])

Search for the first occurrence of the given characters within the given string. The second
argument may be a single character, a vector or a set of characters, a string, or a regular
expression (though regular expressions are only allowed on contiguous strings, such as ASCII
or UTF-8 strings). The third argument optionally specifies a starting index. The return
value is a range of indexes where the matching sequence is found, such that `s[search(s,x)] == x`:

`search(string, "substring")` = `start:end` such that `string[start:end] == "substring"`, or
`0:-1` if unmatched.

`search(string, 'c')` = `index` such that `string[index] == 'c'`, or `0` if unmatched.

```jldoctest
julia> search("Hello to the world", "z")
0:-1

julia> search("JuliaLang","Julia")
1:5
```
"""
function search(s::AbstractString, c::Chars, i::Integer)
    if isempty(c)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    if i < 1 || i > nextind(s,endof(s))
        throw(BoundsError(s, i))
    end
    while !done(s,i)
        d, j = next(s,i)
        if d in c
            return i
        end
        i = j
    end
    return 0
end
search(s::AbstractString, c::Chars) = search(s,c,start(s))

in(c::Char, s::AbstractString) = (search(s,c)!=0)

function _searchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t1, j2 = next(t,start(t))
    while true
        i = search(s,t1,i)
        if i == 0 return 0 end
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
            return i
        end
        i = ii
    end
end

function _search_bloom_mask(c)
    UInt64(1) << (c & 63)
end

_nthbyte(s::String, i) = codeunit(s, i)
_nthbyte(a::ByteArray, i) = a[i]

function _searchindex(s::Union{String,ByteArray}, t::Union{String,ByteArray}, i)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return search(s, _nthbyte(t,1), i)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = _nthbyte(t,n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            end
        end
        i += 1
    end

    0
end

searchindex(s::ByteArray, t::ByteArray, i) = _searchindex(s,t,i)

"""
    searchindex(s::AbstractString, substring, [start::Integer])

Similar to [`search`](@ref), but return only the start index at which
the substring is found, or `0` if it is not.

```jldoctest
julia> searchindex("Hello to the world", "z")
0

julia> searchindex("JuliaLang","Julia")
1

julia> searchindex("JuliaLang","Lang")
6
```
"""
searchindex(s::AbstractString, t::AbstractString, i::Integer) = _searchindex(s,t,i)
searchindex(s::AbstractString, t::AbstractString) = searchindex(s,t,start(s))
searchindex(s::AbstractString, c::Char, i::Integer) = _searchindex(s,c,i)
searchindex(s::AbstractString, c::Char) = searchindex(s,c,start(s))

function searchindex(s::String, t::String, i::Integer=1)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use searchindex on byte arrays instead)
    if endof(t) == 1
        search(s, t[1], i)
    else
        _searchindex(s, t, i)
    end
end

function _search(s, t, i::Integer)
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

search(s::AbstractString, t::AbstractString, i::Integer=start(s)) = _search(s, t, i)
search(s::ByteArray, t::ByteArray, i::Integer=start(s)) = _search(s, t, i)

function rsearch(s::AbstractString, c::Chars)
    j = search(RevString(s), c)
    j == 0 && return 0
    endof(s)-j+1
end

"""
    rsearch(s::AbstractString, chars::Chars, [start::Integer])

Similar to [`search`](@ref), but returning the last occurrence of the given characters within the
given string, searching in reverse from `start`.

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
    contains(haystack::AbstractString, needle::AbstractString)

Determine whether the second argument is a substring of the first.

```jldoctest
julia> contains("JuliaLang is pretty cool!", "Julia")
true
```
"""
contains(haystack::AbstractString, needle::AbstractString) = searchindex(haystack,needle)!=0

in(::AbstractString, ::AbstractString) = error("use contains(x,y) for string containment")

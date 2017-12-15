# This file is a part of Julia. License is MIT: https://julialang.org/license

function search(s::String, c::Char, i::Integer = 1)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return 0
        throw(BoundsError(s, i))
    end
    @inbounds isvalid(s, i) || string_index_err(s, i)
    c ≤ '\x7f' && return search(s, c % UInt8, i)
    while true
        i = search(s, first_utf8_byte(c), i)
        (i == 0 || s[i] == c) && return i
        i = next(s, i)[2]
    end
end

function search(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = 1)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : Int(q-p+1)
end

function search(a::ByteArray, b::Char, i::Integer = 1)
    if Unicode.isascii(b)
        search(a,UInt8(b),i)
    else
        search(a,Vector{UInt8}(string(b)),i).start
    end
end

function rsearch(s::String, c::Char, i::Integer = sizeof(s))
    c ≤ '\x7f' && return rsearch(s, c % UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(s, b, i)
        (i == 0 || s[i] == c) && return i
        i = prevind(s, i)
    end
end

function rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = sizeof(s))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
    q == C_NULL ? 0 : Int(q-p+1)
end

function rsearch(a::ByteArray, b::Char, i::Integer = length(a))
    if Unicode.isascii(b)
        rsearch(a,UInt8(b),i)
    else
        rsearch(a,Vector{UInt8}(string(b)),i).start
    end
end

const Chars = Union{Char,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}

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

# Examples
```jldoctest
julia> search("Hello to the world", "z")
0:-1

julia> search("JuliaLang","Julia")
1:5
```
"""
function search(s::AbstractString, c::Chars, i::Integer)
    z = ncodeunits(s) + 1
    isempty(c) && return 1 ≤ i ≤ z ? i : throw(BoundsError(s, i))
    1 ≤ i ≤ z || throw(BoundsError(s, i))
    @inbounds i == z || isvalid(s, i) || string_index_err(s, i)
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

_searchindex(s, t::Char, i) = search(s, t, i)

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

# Examples
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
function rsearch(s::AbstractString, c::Chars, i::Integer=start(s))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(s, i))
    end
    n = ncodeunits(s)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(s, i))
    end
    # r[reverseind(r,i)] == reverse(r)[i] == s[i]
    # s[reverseind(s,j)] == reverse(s)[j] == r[j]
    r = reverse(s)
    j = search(r, c, reverseind(r, i))
    j == 0 ? 0 : reverseind(s, j)
end

function _rsearchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s, endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t = reverse(t)
    rs = reverse(s)
    l = endof(s)
    t1, j2 = next(t, start(t))
    while true
        i = rsearch(s, t1, i)
        i == 0 && return 0
        c, ii = next(rs, reverseind(rs, i))
        j = j2; k = ii
        matched = true
        while !done(t, j)
            if done(rs, k)
                matched = false
                break
            end
            c, k = next(rs, k)
            d, j = next(t, j)
            if c != d
                matched = false
                break
            end
        end
        matched && return nextind(s, reverseind(s, k))
        i = reverseind(s, ii)
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
        j = i ≤ ncodeunits(s) ? nextind(s, i)-1 : i
        _rsearchindex(s, t, j)
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

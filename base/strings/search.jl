# This file is a part of Julia. License is MIT: http://julialang.org/license

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

function _searchindex(s::Array, t::Array, i)
    n = length(t)
    m = length(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return search(s, t[1], i)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = t[end]
    for j in 1:n
        bloom_mask |= _search_bloom_mask(t[j])
        if t[j] == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if s[i+n] == tlast
            # check candidate
            j = 0
            while j < n - 1
                if s[i+j+1] != t[j+1]
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(s[i+n+1]) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(s[i+n+1]) == 0
                i += n
            end
        end
        i += 1
    end

    0
end

searchindex(s::ByteArray, t::ByteArray, i) = _searchindex(s,t,i)
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
        searchindex(s.data, t.data, i)
    end
end

function search(s::ByteArray, t::ByteArray, i)
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function search(s::AbstractString, t::AbstractString, i::Integer=start(s))
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function rsearch(s::AbstractString, c::Chars)
    j = search(RevString(s), c)
    j == 0 && return 0
    endof(s)-j+1
end

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

function _rsearchindex(s::Array, t::Array, k)
    n = length(t)
    m = length(s)

    if n == 0
        return 0 <= k <= m ? max(k, 1) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return rsearch(s, t[1], k)
    end

    w = m - n
    if w < 0 || k <= 0
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tfirst = t[1]
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(t[j])
        if t[j] == tfirst && j > 1
            skip = j - 2
        end
    end

    i = min(k - n + 1, w + 1)
    while i > 0
        if s[i] == tfirst
            # check candidate
            j = 1
            while j < n
                if s[i+j] != t[j+1]
                    break
                end
                j += 1
            end

            # match found
            if j == n
                return i
            end

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(s[i-1]) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(s[i-1]) == 0
                i -= n
            end
        end
        i -= 1
    end

    0
end

rsearchindex(s::ByteArray,t::ByteArray,i) = _rsearchindex(s,t,i)
rsearchindex(s::AbstractString, t::AbstractString, i::Integer) = _rsearchindex(s,t,i)
rsearchindex(s::AbstractString, t::AbstractString) = (isempty(s) && isempty(t)) ? 1 : rsearchindex(s,t,endof(s))

function rsearchindex(s::String, t::String)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1])
    else
        _rsearchindex(s.data, t.data, length(s.data))
    end
end

function rsearchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1], i)
    elseif endof(t) != 0
        _rsearchindex(s.data, t.data, nextind(s, i)-1)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end

function rsearch(s::ByteArray, t::ByteArray, i::Integer)
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

function rsearch(s::AbstractString, t::AbstractString, i::Integer=endof(s))
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

contains(haystack::AbstractString, needle::AbstractString) = searchindex(haystack,needle)!=0

in(::AbstractString, ::AbstractString) = error("use contains(x,y) for string containment")

# ByteArray optimizations

# find the index of the first occurrence of a value in a byte array

function search(a::ByteArray, b::Union{Int8,UInt8}, i::Integer)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = length(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
    q == C_NULL ? 0 : Int(q-p+1)
end
function search(a::ByteArray, b::Char, i::Integer)
    if isascii(b)
        search(a,UInt8(b),i)
    else
        search(a,string(b).data,i).start
    end
end
search(a::ByteArray, b::Union{Int8,UInt8,Char}) = search(a,b,1)

function rsearch(a::ByteArray, b::Union{Int8,UInt8}, i::Integer)
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = length(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
    q == C_NULL ? 0 : Int(q-p+1)
end
function rsearch(a::ByteArray, b::Char, i::Integer)
    if isascii(b)
        rsearch(a,UInt8(b),i)
    else
        rsearch(a,string(b).data,i).start
    end
end
rsearch(a::ByteArray, b::Union{Int8,UInt8,Char}) = rsearch(a,b,length(a))



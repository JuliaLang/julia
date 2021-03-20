# This file is a part of Julia. License is MIT: https://julialang.org/license

const Fix2Eq{T} = Fix2{<:Union{typeof(isequal),typeof(==)},T}

#=
nothing_sentinel(i) = i == 0 ? nothing : i

function findnext(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:AbstractChar},
                  s::String, i::Integer)
    if i < 1 || i > sizeof(s)
        i == sizeof(s) + 1 && return nothing
        throw(BoundsError(s, i))
    end
    @inbounds isvalid(s, i) || string_index_err(s, i)
    c = pred.x
    c ≤ '\x7f' && return nothing_sentinel(_search(s, c % UInt8, i))
    while true
        i = _search(s, first_utf8_byte(c), i)
        i == 0 && return nothing
        pred(s[i]) && return i
        i = nextind(s, i)
    end
end

findfirst(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:Union{Int8,UInt8}}, a::ByteArray) =
    nothing_sentinel(_search(a, pred.x))

findnext(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:Union{Int8,UInt8}}, a::ByteArray, i::Integer) =
    nothing_sentinel(_search(a, pred.x, i))

function _search(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = 1)
    if i < 1
        throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = GC.@preserve a ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p+i-1, b, n-i+1)
    return q == C_NULL ? 0 : Int(q-p+1)
end

function _search(a::ByteArray, b::AbstractChar, i::Integer = 1)
    if isascii(b)
        _search(a,UInt8(b),i)
    else
        _search(a,unsafe_wrap(Vector{UInt8},string(b)),i).start
    end
end

function findprev(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:AbstractChar},
                  s::String, i::Integer)
    c = pred.x
    c ≤ '\x7f' && return nothing_sentinel(_rsearch(s, c % UInt8, i))
    b = first_utf8_byte(c)
    while true
        i = _rsearch(s, b, i)
        i == 0 && return nothing
        pred(s[i]) && return i
        i = prevind(s, i)
    end
end
findlast(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:Union{Int8,UInt8}}, a::ByteArray) =
    nothing_sentinel(_rsearch(a, pred.x))

findprev(pred::Fix2{<:Union{typeof(isequal),typeof(==)},<:Union{Int8,UInt8}}, a::ByteArray, i::Integer) =
    nothing_sentinel(_rsearch(a, pred.x, i))

function _rsearch(a::Union{String,ByteArray}, b::Union{Int8,UInt8}, i::Integer = sizeof(a))
    if i < 1
        return i == 0 ? 0 : throw(BoundsError(a, i))
    end
    n = sizeof(a)
    if i > n
        return i == n+1 ? 0 : throw(BoundsError(a, i))
    end
    p = pointer(a)
    q = GC.@preserve a ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, i)
    return q == C_NULL ? 0 : Int(q-p+1)
end

function _rsearch(a::ByteArray, b::AbstractChar, i::Integer = length(a))
    if isascii(b)
        _rsearch(a,UInt8(b),i)
    else
        _rsearch(a,unsafe_wrap(Vector{UInt8},string(b)),i).start
    end
end
=#

"""
    findfirst(pattern::AbstractString, string::AbstractString)
    findfirst(pattern::AbstractPattern, string::String)

Find the first occurrence of `pattern` in `string`. Equivalent to
[`findnext(pattern, string, firstindex(s))`](@ref).

# Examples
```jldoctest
julia> findfirst("z", "Hello to the world") # returns nothing, but not printed in the REPL

julia> findfirst("Julia", "JuliaLang")
1:5
```
"""
findfirst(a::AbstractString, b::AbstractString) = findnext(a, b, firstindex(b))

"""
    findfirst(ch::AbstractChar, string::AbstractString)

Find the first occurrence of character `ch` in `string`.

!!! compat "Julia 1.3"
    This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> findfirst('a', "happy")
2

julia> findfirst('z', "happy") === nothing
true
```
"""
findfirst(a::AbstractChar, b::AbstractString) = findfirst(isequal(a), b)

"""
    findfirst(pattern::AbstractVector{<:Union{Int8,UInt8}},
              A::AbstractVector{<:Union{Int8,UInt8}})

Find the first occurrence of sequence `pattern` in vector `A`.

!!! compat "Julia 1.6"
    This method requires at least Julia 1.6.

# Examples
```jldoctest
julia> findfirst([0x52, 0x62], [0x40, 0x52, 0x62, 0x63])
2:3
```
"""
findfirst(a::AbstractVector{<:Union{Int8,UInt8}},
          b::AbstractVector{<:Union{Int8,UInt8}}) = findnext(a, b, firstindex(b))

# AbstractString implementation of the generic findnext interface
function findnext(p::Function, b::AbstractString, start::Integer)
    i = Int(start)
    i < firstindex(b) && throw(BoundsError(b, i))
    last = lastindex(b)
    i > last && return nothing
    if i ≠ thisind(b, i)
        i = nextind(b, i)
    end
    @inbounds while i ≤ last
        p(b[i]) && return i
        i = nextind(b, i)
    end
    return nothing
end

in(a::AbstractChar, b::AbstractString) = findfirst(isequal(a), b) !== nothing

#=
function _searchindex(s::Union{AbstractString,ByteArray},
                      t::Union{AbstractString,AbstractChar,Int8,UInt8},
                      i::Integer)
    if isempty(t)
        return 1 <= i <= nextind(s,lastindex(s))::Int ? i :
               throw(BoundsError(s, i))
    end
    t1, trest = Iterators.peel(t)
    while true
        i = findnext(isequal(t1),s,i)
        if i === nothing return 0 end
        ii = nextind(s, i)::Int
        a = Iterators.Stateful(trest)
        matched = all(splat(==), zip(SubString(s, ii), a))
        (isempty(a) && matched) && return i
        i = ii
    end
end

_searchindex(s::AbstractString, t::AbstractChar, i::Integer) = something(findnext(isequal(t), s, i), 0)

function _search_bloom_mask(c)
    UInt64(1) << (c & 63)
end

_nthbyte(s::String, i) = codeunit(s, i)
_nthbyte(t::AbstractVector, index) = t[index + (firstindex(t)-1)]

function _searchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    _searchindex(unsafe_wrap(Vector{UInt8},s), unsafe_wrap(Vector{UInt8},t), i)
end

function _searchindex(s::AbstractVector{<:Union{Int8,UInt8}},
                      t::AbstractVector{<:Union{Int8,UInt8}},
                      _i::Integer)
    sentinel = firstindex(s) - 1
    n = length(t)
    m = length(s)
    i = Int(_i) - sentinel
    (i < 1 || i > m+1) && throw(BoundsError(s, _i))

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : sentinel
    elseif m == 0
        return sentinel
    elseif n == 1
        return something(findnext(isequal(_nthbyte(t,1)), s, i), sentinel)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return sentinel
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
                # restore in case `s` is an OffSetArray
                return i+firstindex(s)
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

    sentinel
end

function _search(s::Union{AbstractString,AbstractVector{<:Union{Int8,UInt8}}},
                 t::Union{AbstractString,AbstractChar,AbstractVector{<:Union{Int8,UInt8}}},
                 i::Integer)
    idx = _searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    elseif idx >= firstindex(s)
        idx:(idx + lastindex(t) - 1)
    else
        nothing
    end
end
=#

"""
    findnext(pattern::AbstractString, string::AbstractString, start::Integer)
    findnext(pattern::AbstractPattern, string::String, start::Integer)

Find the next occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`.

The return value is a range of indices where the matching sequence is found, such that
`s[findnext(x, s, i)] == x`:

`findnext("substring", string, i)` == `start:stop` such that
`string[start:stop] == "substring"` and `i <= start`, or `nothing` if unmatched.

# Examples
```jldoctest
julia> findnext("z", "Hello to the world", 1) === nothing
true

julia> findnext("o", "Hello to the world", 6)
8:8

julia> findnext("Lang", "JuliaLang", 2)
6:9
```
"""
function findnext(a::AbstractString, b::AbstractString, start::Integer)
    i = Int(start)
    i < firstindex(b) && throw(BoundsError(b, i))
    i > ncodeunits(b) + 1 && return nothing
    if i ≠ thisind(b, i)
        i = nextind(b, i)
    end
    isempty(a) && return i:i-1
    a1 = first(a)
    last = lastindex(b)
    while i ≤ last
        i = findnext(a1, b, i)
        i === nothing && break
        # TODO: If a and b are the same type, this is fine. But if not?
        startswith(SubString(b, i, last), a) && return i:i+lastindex(a)-1
        i = nextind(b, i)
    end
    return nothing
end

function findnext(a::Union{String,SubString{String}}, b::Union{String,SubString{String}}, start::Integer)
    i = Int(start)
    i < firstindex(b) && throw(BoundsError(b, i))
    i > ncodeunits(b) + 1 && return nothing
    if i ≠ thisind(b, i)
        i = nextind(b, i)
    end
    offset = search_forward(codeunits(a), codeunits(b), i - 1)
    return offset ≥ 0 ? (offset+1:offset+lastindex(a)) : nothing
end

"""
    findnext(ch::AbstractChar, string::AbstractString, start::Integer)

Find the next occurrence of character `ch` in `string` starting at position `start`.

!!! compat "Julia 1.3"
    This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> findnext('z', "Hello to the world", 1) === nothing
true

julia> findnext('o', "Hello to the world", 6)
8
```
"""
findnext(a::AbstractChar, b::AbstractString, start::Integer) = findnext(isequal(a), b, start)

function findnext(p::Fix2Eq{<:AbstractChar}, b::Union{String,SubString{String}}, start::Integer)
    i = Int(start)
    first = firstindex(b)
    i < first && throw(BoundsError(b, i))
    i > ncodeunits(b) && return nothing
    if i ≠ thisind(b, i)
        i = nextind(b, i)
    end
    probe = first_utf8_byte(p.x)
    while true
        offset = search_forward(probe, codeunits(b), i - 1)
        if offset < 0
            return nothing
        elseif p(b[offset+first])
            return offset + first
        end
        i = nextind(b, i)
    end
end

"""
    findnext(pattern::AbstractVector{<:Union{Int8,UInt8}},
             A::AbstractVector{<:Union{Int8,UInt8}},
             start::Integer)

Find the next occurrence of the sequence `pattern` in vector `A` starting at position `start`.

!!! compat "Julia 1.6"
    This method requires at least Julia 1.6.

# Examples
```jldoctest
julia> findnext([0x52, 0x62], [0x52, 0x62, 0x72], 3) === nothing
true

julia> findnext([0x52, 0x62], [0x40, 0x52, 0x62, 0x52, 0x62], 3)
4:5
```
"""
function findnext(a::AbstractVector{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, start::Integer)
    i = Int(start)
    first = firstindex(b)
    i < first && throw(BoundsError(b, i))
    i > lastindex(b) + 1 && return nothing
    offset = search_forward(a, b, i - 1)
    return offset ≥ 0 ? (offset+first:offset+first+lastindex(a)-1) : nothing
end

function findnext(p::Fix2Eq{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, start::Integer)
    i = Int(start)
    first = firstindex(b)
    i < first && throw(BoundsError(b, i))
    i > lastindex(b) && return nothing
    offset = search_forward(p.x, b, i - 1)
    return offset ≥ 0 ? offset + first : nothing
end

findfirst(p::Fix2Eq{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}) = findnext(p, b, firstindex(b))

"""
    findlast(pattern::AbstractString, string::AbstractString)

Find the last occurrence of `pattern` in `string`. Equivalent to
[`findprev(pattern, string, lastindex(string))`](@ref).

# Examples
```jldoctest
julia> findlast("o", "Hello to the world")
15:15

julia> findfirst("Julia", "JuliaLang")
1:5
```
"""
findlast(a::AbstractString, b::AbstractString) = findprev(a, b, ncodeunits(b))

"""
    findlast(pattern::AbstractVector{<:Union{Int8,UInt8}},
             A::AbstractVector{<:Union{Int8,UInt8}})

Find the last occurrence of `pattern` in array `A`. Equivalent to
[`findprev(pattern, A, lastindex(A))`](@ref).

# Examples
```jldoctest
julia> findlast([0x52, 0x62], [0x52, 0x62, 0x52, 0x62])
3:4
```
"""
findlast(a::AbstractVector{<:Union{Int8,UInt8}},
         b::AbstractVector{<:Union{Int8,UInt8}}) = findprev(a, b, lastindex(b))

"""
    findlast(ch::AbstractChar, string::AbstractString)

Find the last occurrence of character `ch` in `string`.

!!! compat "Julia 1.3"
    This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> findlast('p', "happy")
4

julia> findlast('z', "happy") === nothing
true
```
"""
findlast(a::AbstractChar, b::AbstractString) = findlast(isequal(a), b)

# AbstractString implementation of the generic findprev interface
function findprev(p::Function, b::AbstractString, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first && return nothing
    i > lastindex(b) && throw(BoundsError(b, i))
    i = thisind(b, i)
    @inbounds while i ≥ first
        p(b[i]) && return i
        i = prevind(s, i)
    end
    return nothing
end

#=
function _rsearchindex(s::AbstractString,
                       t::Union{AbstractString,AbstractChar,Int8,UInt8},
                       i::Integer)
    if isempty(t)
        return 1 <= i <= nextind(s, lastindex(s))::Int ? i :
               throw(BoundsError(s, i))
    end
    t1, trest = Iterators.peel(Iterators.reverse(t))
    while true
        i = findprev(isequal(t1), s, i)
        i === nothing && return 0
        ii = prevind(s, i)::Int
        a = Iterators.Stateful(trest)
        b = Iterators.Stateful(Iterators.reverse(
            pairs(SubString(s, 1, ii))))
        matched = all(splat(==), zip(a, (x[2] for x in b)))
        if matched && isempty(a)
            isempty(b) && return firstindex(s)
            return nextind(s, popfirst!(b)[1])::Int
        end
        i = ii
    end
end

function _rsearchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    if lastindex(t) == 1
        return something(findprev(isequal(t[1]), s, i), 0)
    elseif lastindex(t) != 0
        j = i ≤ ncodeunits(s) ? nextind(s, i)-1 : i
        return _rsearchindex(unsafe_wrap(Vector{UInt8}, s), unsafe_wrap(Vector{UInt8}, t), j)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end

function _rsearchindex(s::AbstractVector{<:Union{Int8,UInt8}}, t::AbstractVector{<:Union{Int8,UInt8}}, _k::Integer)
    sentinel = firstindex(s) - 1
    n = length(t)
    m = length(s)
    k = Int(_k) - sentinel
    k < 1 && throw(BoundsError(s, _k))

    if n == 0
        return 0 <= k <= m ? max(k, 1) : sentinel
    elseif m == 0
        return sentinel
    elseif n == 1
        return something(findprev(isequal(_nthbyte(t,1)), s, k), sentinel)
    end

    w = m - n
    if w < 0 || k <= 0
        return sentinel
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

            # match found, restore in case `s` is an OffsetArray
            if j == n
                return i + sentinel
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

    sentinel
end

function _rsearch(s::Union{AbstractString,AbstractVector{<:Union{Int8,UInt8}}},
                  t::Union{AbstractString,AbstractChar,AbstractVector{<:Union{Int8,UInt8}}},
                  i::Integer)
    idx = _rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    elseif idx > firstindex(s) - 1
        idx:(idx + lastindex(t) - 1)
    else
        nothing
    end
end
=#

"""
    findprev(pattern::AbstractString, string::AbstractString, start::Integer)

Find the previous occurrence of `pattern` in `string` starting at position `start`.

The return value is a range of indices where the matching sequence is found, such that
`s[findprev(x, s, i)] == x`:

`findprev("substring", string, i)` == `start:stop` such that
`string[start:stop] == "substring"` and `stop <= i`, or `nothing` if unmatched.

# Examples
```jldoctest
julia> findprev("z", "Hello to the world", 18) === nothing
true

julia> findprev("o", "Hello to the world", 18)
15:15

julia> findprev("Julia", "JuliaLang", 6)
1:5
```
"""
function findprev(a::AbstractString, b::AbstractString, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first - 1 && return nothing
    n = ncodeunits(b)
    i > n && throw(BoundsError(b, i))
    isempty(a) && return i-1:i
    while i ≥ first
        i = findprev(isequal(a), b, i)
        i === nothing && break
        # TODO: If a and b are the same type, this is fine. But if not?
        endswith(SubString(b, first, i), a) && return i-lastindex(a)+1:i
        i = prevind(b, i)
    end
    return nothing
end

function findprev(a::Union{String,SubString{String}}, b::Union{String,SubString{String}}, stop::Integer)
    i = Int(stop)
    i < firstindex(b) - 1 && return nothing
    n = ncodeunits(b)
    i > n && throw(BoundsError(b, i))
    offset = search_backward(codeunits(a), codeunits(b), n + 1 - nextind(b, i))
    return offset ≥ 0 ? (offset+1:offset+lastindex(a)) : nothing
end

"""
    findprev(ch::AbstractChar, string::AbstractString, start::Integer)

Find the previous occurrence of character `ch` in `string` starting at position `start`.

!!! compat "Julia 1.3"
    This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> findprev('z', "Hello to the world", 18) === nothing
true

julia> findprev('o', "Hello to the world", 18)
15
```
"""
findprev(a::AbstractChar, b::AbstractString, start::Integer) = findprev(isequal(a), b, start)

function findprev(p::Fix2Eq{<:AbstractChar}, b::Union{String,SubString{String}}, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first && return nothing
    n = ncodeunits(b)
    i > n && throw(BoundsError(b, i))
    i = thisind(b, i)
    probe = first_utf8_byte(p.x)
    while true
        offset = search_backward(probe, codeunits(b), n - i)
        if offset < 0
            return nothing
        elseif p(b[offset+first])
            return offset + first
        end
        i = prevind(b, i)
    end
end

"""
    findprev(pattern::AbstractVector{<:Union{Int8,UInt8}},
             A::AbstractVector{<:Union{Int8,UInt8}},
             start::Integer)

Find the previous occurrence of the sequence `pattern` in vector `A` starting at position `start`.

!!! compat "Julia 1.6"
    This method requires at least Julia 1.6.

# Examples
```jldoctest
julia> findprev([0x52, 0x62], [0x40, 0x52, 0x62, 0x52, 0x62], 3)
2:3
```
"""
function findprev(a::AbstractVector{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first - 1 && return nothing
    i > lastindex(b) && throw(BoundsError(b, i))
    offset = search_backward(a, b, lastindex(b) - i)
    return offset ≥ 0 ? (offset+first:offset+first+lastindex(a)-1) : nothing
end

function findprev(p::Fix2Eq{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first && return nothing
    i > lastindex(b) && throw(BoundsError(b, i))
    offset = search_backward(p.x, b, length(b) - i)
    return offset ≥ 0 ? offset + first : nothing
end

findlast(p::Fix2Eq{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}) = findprev(p, b, lastindex(b))

"""
    occursin(needle::Union{AbstractString,AbstractPattern,AbstractChar}, haystack::AbstractString)

Determine whether the first argument is a substring of the second. If `needle`
is a regular expression, checks whether `haystack` contains a match.

# Examples
```jldoctest
julia> occursin("Julia", "JuliaLang is pretty cool!")
true

julia> occursin('a', "JuliaLang is pretty cool!")
true

julia> occursin(r"a.a", "aba")
true

julia> occursin(r"a.a", "abba")
false
```

See also: [`contains`](@ref).
"""
occursin(a::Union{AbstractString,AbstractChar}, b::AbstractString) = findfirst(a, b) !== nothing

"""
    occursin(haystack)

Create a function that checks whether its argument occurs in `haystack`, i.e.
a function equivalent to `needle -> occursin(needle, haystack)`.

The returned function is of type `Base.Fix2{typeof(occursin)}`.

!!! compat "Julia 1.6"
    This method requires Julia 1.6 or later.
"""
occursin(haystack) = Base.Fix2(occursin, haystack)

in(::AbstractString, ::AbstractString) = error("use occursin(x, y) for string containment")

function search_forward(a::Union{Int8,UInt8}, b::AbstractVector{<:Union{Int8,UInt8}}, k::Int)
    typemin(eltype(b)) ≤ a ≤ typemax(eltype(b)) || return -1
    @inbounds for i in firstindex(b)+k:lastindex(b)
        a == b[i] && return i - firstindex(b)
    end
    return -1
end

function search_forward(a::Union{Int8,UInt8}, b::Union{Vector{<:Union{Int8,UInt8}},CodeUnits{<:Union{Int8,UInt8},<:Union{String,SubString{String}}}}, k::Int)
    typemin(eltype(b)) ≤ a ≤ typemax(eltype(b)) || return -1
    n = length(b)
    n ≤ k && return -1
    p = GC.@preserve b ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Cint, Csize_t), pointer(b) + k, a, n - k)
    return p ≠ C_NULL ? Int(p - pointer(b)) : -1
end

function search_backward(a::Union{Int8,UInt8}, b::AbstractVector{<:Union{Int8,UInt8}}, k::Int)
    typemin(eltype(b)) ≤ a ≤ typemax(eltype(b)) || return -1
    @inbounds for i in lastindex(b)-k:-1:firstindex(b)
        a == b[i] && return i - firstindex(b)
    end
    return -1
end

function search_backward(a::Union{Int8,UInt8}, b::Union{Vector{<:Union{Int8,UInt8}},CodeUnits{<:Union{Int8,UInt8},<:Union{String,SubString{String}}}}, k::Int)
    typemin(eltype(b)) ≤ a ≤ typemax(eltype(b)) || return -1
    n = length(b)
    n ≤ k && return -1
    p = GC.@preserve b ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Cint, Csize_t), pointer(b), a, n - k)
    return p ≠ C_NULL ? Int(p - pointer(b)) : -1
end

function search_forward(a::AbstractVector{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, k::Int)
    m = length(a)
    n = length(b) - k
    if m > n
        return -1
    elseif m == 0
        return k
    end

    # preprocess
    a_end = a[end]
    filter = bloom_filter_bit(a_end)
    displacement = m
    @inbounds for i in firstindex(a):lastindex(a)-1
        filter |= bloom_filter_bit(a[i])
        if a[i] == a_end
            displacement = lastindex(a) - i
        end
    end

    # main loop
    last = lastindex(b)
    p = firstindex(b) + k
    @inbounds while p + m - 1 ≤ last
        if a_end == b[p+m-1]
            # the last byte is matching
            i = firstindex(a)
            while i < lastindex(a)
                a[i] == b[p+i-1] || break
                i += 1
            end
            if i == lastindex(a)
                return p - firstindex(b)
            elseif p + m ≤ last && !mayhave(filter, b[p+m])
                p += m + 1
            else
                p += displacement
            end
        else
            if p + m ≤ last && !mayhave(filter, b[p+m])
                p += m +  1
            else
                p += 1
            end
        end
    end
    return -1
end

function search_backward(a::AbstractVector{<:Union{Int8,UInt8}}, b::AbstractVector{<:Union{Int8,UInt8}}, k::Int)
    m = length(a)
    n = length(b) - k
    if m > n
        return -1
    elseif m == 0
        return n
    end

    # preprocess
    a_begin = a[begin]
    filter = bloom_filter_bit(a_begin)
    displacement = m
    @inbounds for i in lastindex(a):-1:firstindex(a)+1
        filter |= bloom_filter_bit(a[i])
        if a[i] == a_begin
            displacement = firstindex(a) - i
        end
    end

    # main loop
    first = firstindex(b)
    p = lastindex(b) + 1 - (m + k)
    @inbounds while p ≥ first
        if a_begin == b[p]
            # the first byte is matching
            i = lastindex(a)
            while i > firstindex(a)
                a[i] == b[p+i-1] || break
                i -= 1
            end
            if i == firstindex(a)
                return p - first
            elseif p - 1 ≥ first && !mayhave(filter, b[p-1])
                p -= m + 1
            else
                p -= displacement
            end
        else
            if p - 1 ≥ first && !mayhave(filter, b[p-1])
                p -= m + 1
            else
                p -= 1
            end
        end
    end
    return -1
end

# Bloom filter using a 64-bit integer
bloom_filter_bit(x::Union{Int8,UInt8}) = UInt64(1) << (x & 63)
mayhave(filter::UInt64, x::Union{Int8,UInt8}) = filter & bloom_filter_bit(x) ≠ 0
# This file is a part of Julia. License is MIT: https://julialang.org/license

const Fix2Eq{T} = Fix2{<:Union{typeof(isequal),typeof(==)},T}
const ByteVector = Union{Vector{Int8},Vector{UInt8},CodeUnits{UInt8}}

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

function findnext(p::Fix2Eq{<:Union{Int8,UInt8}}, b::ByteVector, start::Integer)
    i = Int(start)
    first = firstindex(b)
    i < first && throw(BoundsError(b, i))
    i > lastindex(b) && return nothing
    offset = search_forward(p.x, b, i - 1)
    return offset ≥ 0 ? offset + first : nothing
end

findfirst(p::Fix2Eq{<:Union{Int8,UInt8}}, b::ByteVector) = findnext(p, b, firstindex(b))

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

function findprev(p::Function, b::AbstractString, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first && return nothing
    i > lastindex(b) && throw(BoundsError(b, i))
    i = thisind(b, i)
    @inbounds while i ≥ first
        p(b[i]) && return i
        i = prevind(b, i)
    end
    return nothing
end

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

function findprev(p::Fix2Eq{<:Union{Int8,UInt8}}, b::ByteVector, stop::Integer)
    i = Int(stop)
    first = firstindex(b)
    i < first && return nothing
    i > lastindex(b) && throw(BoundsError(b, i))
    offset = search_backward(p.x, b, length(b) - i)
    return offset ≥ 0 ? offset + first : nothing
end

findlast(p::Fix2Eq{<:Union{Int8,UInt8}}, b::ByteVector) = findprev(p, b, lastindex(b))

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

in(a::AbstractChar, b::AbstractString) = findfirst(isequal(a), b) !== nothing
in(::AbstractString, ::AbstractString) = error("use occursin(x, y) for string containment")

# Both search_forward and search_backward take three arguments:
#   1. needle   (`a`)
#   2. haystack (`b`)
#   3. trimming (`k`)
# The search_forward funtion ignores `k` bytes from the head of `b` and the
# search_backward function ignores `k` bytes from the tail of `b`. `k` must be
# nonnegative, but `k` may be larger than the length of `b` (`a` matches
# nowhere in this case). These two functions return an offset if any match is
# found in `b`. That means the actual starting position of the match is `offset
# + firstindex(b)`. If there is no match, a negative integer is returned.

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
    @assert displacement > 0

    # main loop
    last = lastindex(b)
    p = firstindex(b) + k
    @inbounds while p + m - 1 ≤ last
        if a_end == b[p+m-1]
            # the last byte is matching
            i = firstindex(a)
            while i < lastindex(a) && a[i] == b[p+i-1]
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
                p += m + 1
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
            displacement = i - firstindex(a)
        end
    end
    @assert displacement > 0

    # main loop
    first = firstindex(b)
    p = lastindex(b) + 1 - (m + k)
    @inbounds while p ≥ first
        if a_begin == b[p]
            # the first byte is matching
            i = lastindex(a)
            while i > firstindex(a) && a[i] == b[p+i-1]
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

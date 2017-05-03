# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct Set{T} <: AbstractSet{T}
    dict::Dict{T,Void}

    Set{T}() where {T} = new(Dict{T,Void}())
    Set{T}(itr) where {T} = union!(new(Dict{T,Void}()), itr)
end
Set() = Set{Any}()
Set(itr) = Set{eltype(itr)}(itr)
function Set(g::Generator)
    T = _default_eltype(typeof(g))
    (isleaftype(T) || T === Union{}) || return grow_to!(Set{T}(), g)
    return Set{T}(g)
end

eltype(::Type{Set{T}}) where {T} = T
similar(s::Set{T}) where {T} = Set{T}()
similar(s::Set, T::Type) = Set{T}()

function show(io::IO, s::Set)
    print(io,"Set")
    if isempty(s)
        print(io,"{",eltype(s),"}()")
        return
    end
    print(io,"(")
    show_vector(io,s,"[","]")
    print(io,")")
end

isempty(s::Set) = isempty(s.dict)
length(s::Set)  = length(s.dict)
in(x, s::Set) = haskey(s.dict, x)
push!(s::Set, x) = (s.dict[x] = nothing; s)
pop!(s::Set, x) = (pop!(s.dict, x); x)
pop!(s::Set, x, deflt) = x in s ? pop!(s, x) : deflt
pop!(s::Set) = (idx = start(s.dict); val = s.dict.keys[idx]; _delete!(s.dict, idx); val)
delete!(s::Set, x) = (delete!(s.dict, x); s)

copy(s::Set) = union!(similar(s), s)

sizehint!(s::Set, newsz) = (sizehint!(s.dict, newsz); s)
empty!(s::Set) = (empty!(s.dict); s)
rehash!(s::Set) = (rehash!(s.dict); s)

start(s::Set)       = start(s.dict)
done(s::Set, state) = done(s.dict, state)
# NOTE: manually optimized to take advantage of Dict representation
next(s::Set, i)     = (s.dict.keys[i], skip_deleted(s.dict,i+1))

union() = Set()
union(s::Set) = copy(s)
function union(s::Set, sets::Set...)
    u = Set{join_eltype(s, sets...)}()
    union!(u,s)
    for t in sets
        union!(u,t)
    end
    return u
end
const ∪ = union
union!(s::Set, xs) = (for x=xs; push!(s,x); end; s)
union!(s::Set, xs::AbstractArray) = (sizehint!(s,length(xs));for x=xs; push!(s,x); end; s)
join_eltype() = Bottom
join_eltype(v1, vs...) = typejoin(eltype(v1), join_eltype(vs...))

intersect(s::Set) = copy(s)
function intersect(s::Set, sets::Set...)
    i = similar(s)
    for x in s
        inall = true
        for t in sets
            if !in(x,t)
                inall = false
                break
            end
        end
        inall && push!(i, x)
    end
    return i
end
const ∩ = intersect

function setdiff(a::Set, b::Set)
    d = similar(a)
    for x in a
        if !(x in b)
            push!(d, x)
        end
    end
    d
end
setdiff!(s::Set, xs) = (for x=xs; delete!(s,x); end; s)

==(l::Set, r::Set) = (length(l) == length(r)) && (l <= r)
<( l::Set, r::Set) = (length(l) < length(r)) && (l <= r)
<=(l::Set, r::Set) = issubset(l, r)

function issubset(l, r)
    for elt in l
        if !in(elt, r)
            return false
        end
    end
    return true
end
const ⊆ = issubset
⊊(l::Set, r::Set) = <(l, r)
⊈(l::Set, r::Set) = !⊆(l, r)
⊇(l, r) = issubset(r, l)
⊉(l::Set, r::Set) = !⊇(l, r)
⊋(l::Set, r::Set) = <(r, l)

"""
    unique(itr)

Returns an array containing one value from `itr` for each unique value,
as determined by [`isequal`](@ref).

```jldoctest
julia> unique([1; 2; 2; 6])
3-element Array{Int64,1}:
 1
 2
 6
```
"""
function unique(itr)
    T = _default_eltype(typeof(itr))
    out = Vector{T}()
    seen = Set{T}()
    i = start(itr)
    if done(itr, i)
        return out
    end
    x, i = next(itr, i)
    if !isleaftype(T)
        S = typeof(x)
        return _unique_from(itr, S[x], Set{S}((x,)), i)
    end
    push!(seen, x)
    push!(out, x)
    return unique_from(itr, out, seen, i)
end

_unique_from(itr, out, seen, i) = unique_from(itr, out, seen, i)
@inline function unique_from{T}(itr, out::Vector{T}, seen, i)
    while !done(itr, i)
        x, i = next(itr, i)
        S = typeof(x)
        if !(S === T || S <: T)
            R = typejoin(S, T)
            seenR = convert(Set{R}, seen)
            outR = convert(Vector{R}, out)
            if !in(x, seenR)
                push!(seenR, x)
                push!(outR, x)
            end
            return _unique_from(itr, outR, seenR, i)
        end
        if !in(x, seen)
            push!(seen, x)
            push!(out, x)
        end
    end
    return out
end

"""
    unique(f, itr)

Returns an array containing one value from `itr` for each unique value produced by `f`
applied to elements of `itr`.

```jldoctest
julia> unique(isodd, [1; 2; 2; 6])
2-element Array{Int64,1}:
 1
 2
```
"""
function unique(f::Callable, C)
    out = Vector{eltype(C)}()
    seen = Set()
    for x in C
        y = f(x)
        if !in(y, seen)
            push!(seen, y)
            push!(out, x)
        end
    end
    out
end

"""
    allunique(itr) -> Bool

Return `true` if all values from `itr` are distinct when compared with [`isequal`](@ref).

```jldoctest
julia> a = [1; 2; 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> allunique([a, a])
false
```
"""
function allunique(C)
    seen = Set{eltype(C)}()
    for x in C
        if in(x, seen)
            return false
        else
            push!(seen, x)
        end
    end
    true
end

allunique(::Set) = true

allunique(r::Range{T}) where {T} = (step(r) != zero(T)) || (length(r) <= 1)

function filter(f, s::Set)
    u = similar(s)
    for x in s
        if f(x)
            push!(u, x)
        end
    end
    return u
end
function filter!(f, s::Set)
    for x in s
        if !f(x)
            delete!(s, x)
        end
    end
    return s
end

const hashs_seed = UInt === UInt64 ? 0x852ada37cfe8e0ce : 0xcfe8e0ce
function hash(s::Set, h::UInt)
    h = hash(hashs_seed, h)
    for x in s
        h ⊻= hash(x)
    end
    return h
end

convert(::Type{Set{T}}, s::Set{T}) where {T} = s
convert(::Type{Set{T}}, x::Set) where {T} = Set{T}(x)

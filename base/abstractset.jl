# This file is a part of Julia. License is MIT: https://julialang.org/license

eltype(::Type{<:AbstractSet{T}}) where {T} = @isdefined(T) ? T : Any
sizehint!(s::AbstractSet, n) = nothing

"""
    union(s, itrs...)
    ∪(s, itrs...)

Construct the union of sets. Maintain order with arrays.

# Examples
```jldoctest
julia> union([1, 2], [3, 4])
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> union([1, 2], [2, 4])
3-element Array{Int64,1}:
 1
 2
 4

julia> union([4, 2], 1:2)
3-element Array{Int64,1}:
 4
 2
 1

julia> union(Set([1, 2]), 2:3)
Set([2, 3, 1])
```
"""
function union end

_in(itr) = x -> x in itr

union(s, sets...) = union!(emptymutable(s, promote_eltype(s, sets...)), s, sets...)
union(s::AbstractSet) = copy(s)

const ∪ = union

"""
    union!(s::Union{AbstractSet,AbstractVector}, itrs...)

Construct the union of passed in sets and overwrite `s` with the result.
Maintain order with arrays.

# Examples
```jldoctest
julia> a = Set([1, 3, 4, 5]);

julia> union!(a, 1:2:8);

julia> a
Set([7, 4, 3, 5, 1])
```
"""
function union!(s::AbstractSet, sets...)
    for x in sets
        union!(s, x)
    end
    return s
end

max_values(::Type) = typemax(Int)
max_values(T::Type{<:Union{Nothing,BitIntegerSmall}}) = 1 << (8*sizeof(T))
max_values(T::Union) = max(max_values(T.a), max_values(T.b))
max_values(::Type{Bool}) = 2

function union!(s::AbstractSet{T}, itr) where T
    haslength(itr) && sizehint!(s, length(s) + length(itr))
    for x=itr
        push!(s, x)
        length(s) == max_values(T) && break
    end
    s
end

"""
    intersect(s, itrs...)
    ∩(s, itrs...)

Construct the intersection of sets.
Maintain order with arrays.

# Examples
```jldoctest
julia> intersect([1, 2, 3], [3, 4, 5])
1-element Array{Int64,1}:
 3

julia> intersect([1, 4, 4, 5, 6], [4, 6, 6, 7, 8])
2-element Array{Int64,1}:
 4
 6

julia> intersect(Set([1, 2]), BitSet([2, 3]))
Set([2])
```
"""
intersect(s::AbstractSet, itr, itrs...) = intersect!(intersect(s, itr), itrs...)
intersect(s) = union(s)
intersect(s::AbstractSet, itr) = mapfilter(_in(s), push!, itr, emptymutable(s))

const ∩ = intersect

"""
    intersect!(s::Union{AbstractSet,AbstractVector}, itrs...)

Intersect all passed in sets and overwrite `s` with the result.
Maintain order with arrays.
"""
function intersect!(s::AbstractSet, itrs...)
    for x in itrs
        intersect!(s, x)
    end
    return s
end
intersect!(s::AbstractSet, s2::AbstractSet) = filter!(_in(s2), s)
intersect!(s::AbstractSet, itr) =
    intersect!(s, union!(emptymutable(s, eltype(itr)), itr))

"""
    setdiff(s, itrs...)

Construct the set of elements in `s` but not in any of the iterables in `itrs`.
Maintain order with arrays.

# Examples
```jldoctest
julia> setdiff([1,2,3], [3,4,5])
2-element Array{Int64,1}:
 1
 2
```
"""
setdiff(s::AbstractSet, itrs...) = setdiff!(copymutable(s), itrs...)
setdiff(s) = union(s)

"""
    setdiff!(s, itrs...)

Remove from set `s` (in-place) each element of each iterable from `itrs`.
Maintain order with arrays.

# Examples
```jldoctest
julia> a = Set([1, 3, 4, 5]);

julia> setdiff!(a, 1:2:6);

julia> a
Set([4])
```
"""
function setdiff!(s::AbstractSet, itrs...)
    for x in itrs
        setdiff!(s, x)
    end
    return s
end
function setdiff!(s::AbstractSet, itr)
    for x in itr
        delete!(s, x)
    end
    return s
end


"""
    symdiff(s, itrs...)

Construct the symmetric difference of elements in the passed in sets.
When `s` is not an `AbstractSet`, the order is maintained.
Note that in this case the multiplicity of elements matters.

# Examples
```jldoctest
julia> symdiff([1,2,3], [3,4,5], [4,5,6])
3-element Array{Int64,1}:
 1
 2
 6

julia> symdiff([1,2,1], [2, 1, 2])
2-element Array{Int64,1}:
 1
 2

julia> symdiff(unique([1,2,1]), unique([2, 1, 2]))
0-element Array{Int64,1}
```
"""
symdiff(s, sets...) = symdiff!(emptymutable(s, promote_eltype(s, sets...)), s, sets...)
symdiff(s) = symdiff!(copy(s))

"""
    symdiff!(s::Union{AbstractSet,AbstractVector}, itrs...)

Construct the symmetric difference of the passed in sets, and overwrite `s` with the result.
When `s` is an array, the order is maintained.
Note that in this case the multiplicity of elements matters.
"""
function symdiff!(s::AbstractSet, itrs...)
    for x in itrs
        symdiff!(s, x)
    end
    return s
end

function symdiff!(s::AbstractSet, itr)
    for x in itr
        x in s ? delete!(s, x) : push!(s, x)
    end
    s
end

==(l::AbstractSet, r::AbstractSet) = length(l) == length(r) && l ⊆ r
# convenience functions for AbstractSet
# (if needed, only their synonyms ⊊ and ⊆ must be specialized)
<( l::AbstractSet, r::AbstractSet) = l ⊊ r
<=(l::AbstractSet, r::AbstractSet) = l ⊆ r

function issubset(l, r)
    if haslength(r)
        rlen = length(r)
        #This threshold was empirically determined by repeatedly
        #sampling using these two methods (see #26198)
        lenthresh = 70

        if rlen > lenthresh && !isa(r, AbstractSet)
            return issubset(l, Set(r))
        end
    end

    for elt in l
        if !in(elt, r)
            return false
        end
    end
    return true
end
# use the implementation below when it becomes as efficient
# issubset(l, r) = all(_in(r), l)
const ⊆ = issubset
⊇(l, r) = r ⊆ l
"""
    issubset(a, b)
    ⊆(a,b)  -> Bool
    ⊇(b, a) -> Bool

Determine whether every element of `a` is also in `b`, using [`in`](@ref).

# Examples
```jldoctest
julia> issubset([1, 2], [1, 2, 3])
true

julia> [1, 2, 3] ⊆ [1, 2]
false

julia> [1, 2, 3] ⊇ [1, 2]
true
```
"""
issubset, ⊆, ⊇

"""
    issetequal(a, b)

Determine whether `a` and `b` have the same elements. Equivalent
to `a ⊆ b && b ⊆ a`.

# Examples
```jldoctest
julia> issetequal([1, 2], [1, 2, 3])
false

julia> issetequal([1, 2], [2, 1])
true
```
"""
issetequal(l, r) = length(l) == length(r) && l ⊆ r
issetequal(l::AbstractSet, r::AbstractSet) = l == r

⊊(l, r) = length(l) < length(r) && l ⊆ r
⊋(l, r) = r ⊊ l
"""
    ⊊(a, b)
    ⊋(b, a)

Determines if `a` is a subset of, but not equal to, `b`.

# Examples
```jldoctest
julia> (1, 2) ⊊ (1, 2, 3)
true

julia> (1, 2) ⊊ (1, 2)
false
```
"""
⊊, ⊋

⊈(l, r) = !⊆(l, r)
⊉(l, r) = r ⊈ l
"""
    ⊈(a, b)
    ⊉(b, a)

Negation of `⊆` and `⊇`, i.e. checks that `a` is not a subset of `b`.

# Examples
```jldoctest
julia> (1, 2) ⊈ (2, 3)
true

julia> (1, 2) ⊈ (1, 2, 3)
false
```
"""
⊈, ⊉

filter(pred, s::AbstractSet) = mapfilter(pred, push!, s, emptymutable(s))

# it must be safe to delete the current element while iterating over s:
unsafe_filter!(pred, s::AbstractSet) = mapfilter(!pred, delete!, s, s)

# TODO: delete mapfilter in favor of comprehensions/foldl/filter when competitive
function mapfilter(pred, f, itr, res)
    for x in itr
        pred(x) && f(res, x)
    end
    res
end

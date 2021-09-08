# This file is a part of Julia. License is MIT: https://julialang.org/license

eltype(::Type{<:AbstractSet{T}}) where {T} = @isdefined(T) ? T : Any
sizehint!(s::AbstractSet, n) = nothing

copy!(dst::AbstractSet, src::AbstractSet) = union!(empty!(dst), src)

## set operations (union, intersection, symmetric difference)

"""
    union(s, itrs...)
    ∪(s, itrs...)

Construct the union of sets. Maintain order with arrays.

See also: [`intersect`](@ref), [`isdisjoint`](@ref), [`vcat`](@ref), [`Iterators.flatten`](@ref).

# Examples
```jldoctest
julia> union([1, 2], [3, 4])
4-element Vector{Int64}:
 1
 2
 3
 4

julia> union([1, 2], [2, 4])
3-element Vector{Int64}:
 1
 2
 4

julia> union([4, 2], 1:2)
3-element Vector{Int64}:
 4
 2
 1

julia> union(Set([1, 2]), 2:3)
Set{Int64} with 3 elements:
  2
  3
  1
```
"""
function union end

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
Set{Int64} with 5 elements:
  5
  4
  7
  3
  1
```
"""
function union!(s::AbstractSet, sets...)
    for x in sets
        union!(s, x)
    end
    return s
end

max_values(::Type) = typemax(Int)
max_values(T::Union{map(X -> Type{X}, BitIntegerSmall_types)...}) = 1 << (8*sizeof(T))
# saturated addition to prevent overflow with typemax(Int)
function max_values(T::Union)
    a = max_values(T.a)::Int
    b = max_values(T.b)::Int
    return max(a, b, a + b)
end
max_values(::Type{Bool}) = 2
max_values(::Type{Nothing}) = 1

function union!(s::AbstractSet{T}, itr) where T
    haslength(itr) && sizehint!(s, length(s) + Int(length(itr))::Int)
    for x in itr
        push!(s, x)
        length(s) == max_values(T) && break
    end
    return s
end

"""
    intersect(s, itrs...)
    ∩(s, itrs...)

Construct the intersection of sets.
Maintain order with arrays.

See also: [`setdiff`](@ref), [`isdisjoint`](@ref), [`issubset`](@ref Base.issubset), [`issetequal`](@ref).

!!! compat "Julia 1.8"
    As of Julia 1.8 intersect returns a result with the eltype of the
    type-promoted eltypes of the two inputs

# Examples
```jldoctest
julia> intersect([1, 2, 3], [3, 4, 5])
1-element Vector{Int64}:
 3

julia> intersect([1, 4, 4, 5, 6], [4, 6, 6, 7, 8])
2-element Vector{Int64}:
 4
 6

julia> intersect(Set([1, 2]), BitSet([2, 3]))
Set{Int64} with 1 element:
  2
```
"""
function intersect(s::AbstractSet, itr, itrs...)
    T = promote_eltype(s, itr, itrs...)
    return intersect!(Set{T}(s), itr, itrs...)
end
intersect(s) = union(s)
intersect(s::AbstractSet, itr) = mapfilter(in(s), push!, itr, emptymutable(s, promote_eltype(s, itr)))

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
intersect!(s::AbstractSet, s2::AbstractSet) = filter!(in(s2), s)
intersect!(s::AbstractSet, itr) =
    intersect!(s, union!(emptymutable(s, eltype(itr)), itr))

"""
    setdiff(s, itrs...)

Construct the set of elements in `s` but not in any of the iterables in `itrs`.
Maintain order with arrays.

See also [`setdiff!`](@ref), [`union`](@ref) and [`intersect`](@ref).

# Examples
```jldoctest
julia> setdiff([1,2,3], [3,4,5])
2-element Vector{Int64}:
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
Set{Int64} with 1 element:
  4
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

See also [`symdiff!`](@ref), [`setdiff`](@ref), [`union`](@ref) and [`intersect`](@ref).

# Examples
```jldoctest
julia> symdiff([1,2,3], [3,4,5], [4,5,6])
3-element Vector{Int64}:
 1
 2
 6

julia> symdiff([1,2,1], [2, 1, 2])
2-element Vector{Int64}:
 1
 2

julia> symdiff(unique([1,2,1]), unique([2, 1, 2]))
Int64[]
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
    return s
end

## non-strict subset comparison

const ⊆ = issubset
function ⊇ end
"""
    issubset(a, b) -> Bool
    ⊆(a, b) -> Bool
    ⊇(b, a) -> Bool

Determine whether every element of `a` is also in `b`, using [`in`](@ref).

See also [`⊊`](@ref), [`⊈`](@ref), [`∩`](@ref intersect), [`∪`](@ref union), [`contains`](@ref).

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

const FASTIN_SET_THRESHOLD = 70

function issubset(a, b)
    if haslength(b) && (isa(a, AbstractSet) || !hasfastin(b))
        blen = length(b) # conditions above make this length computed only when needed
        # check a for too many unique elements
        if isa(a, AbstractSet) && length(a) > blen
            return false
        end
        # when `in` would be too slow and b is big enough, convert it to a Set
        # this threshold was empirically determined (cf. #26198)
        if !hasfastin(b) && blen > FASTIN_SET_THRESHOLD
            return issubset(a, Set(b))
        end
    end
    for elt in a
        elt in b || return false
    end
    return true
end

"""
    hasfastin(T)

Determine whether the computation `x ∈ collection` where `collection::T` can be considered
as a "fast" operation (typically constant or logarithmic complexity).
The definition `hasfastin(x) = hasfastin(typeof(x))` is provided for convenience so that instances
can be passed instead of types.
However the form that accepts a type argument should be defined for new types.
"""
hasfastin(::Type) = false
hasfastin(::Union{Type{<:AbstractSet},Type{<:AbstractDict},Type{<:AbstractRange}}) = true
hasfastin(x) = hasfastin(typeof(x))

⊇(a, b) = b ⊆ a

## strict subset comparison

function ⊊ end
function ⊋ end
"""
    ⊊(a, b) -> Bool
    ⊋(b, a) -> Bool

Determines if `a` is a subset of, but not equal to, `b`.

See also [`issubset`](@ref) (`⊆`), [`⊈`](@ref).

# Examples
```jldoctest
julia> (1, 2) ⊊ (1, 2, 3)
true

julia> (1, 2) ⊊ (1, 2)
false
```
"""
⊊, ⊋

⊊(a::AbstractSet, b::AbstractSet) = length(a) < length(b) && a ⊆ b
⊊(a, b) = Set(a) ⊊ Set(b)
⊋(a, b) = b ⊊ a

function ⊈ end
function ⊉ end
"""
    ⊈(a, b) -> Bool
    ⊉(b, a) -> Bool

Negation of `⊆` and `⊇`, i.e. checks that `a` is not a subset of `b`.

See also [`issubset`](@ref) (`⊆`), [`⊊`](@ref).

# Examples
```jldoctest
julia> (1, 2) ⊈ (2, 3)
true

julia> (1, 2) ⊈ (1, 2, 3)
false
```
"""
⊈, ⊉

⊈(a, b) = !⊆(a, b)
⊉(a, b) = b ⊈ a

## set equality comparison

"""
    issetequal(a, b) -> Bool

Determine whether `a` and `b` have the same elements. Equivalent
to `a ⊆ b && b ⊆ a` but more efficient when possible.

See also: [`isdisjoint`](@ref), [`union`](@ref).

# Examples
```jldoctest
julia> issetequal([1, 2], [1, 2, 3])
false

julia> issetequal([1, 2], [2, 1])
true
```
"""
issetequal(a::AbstractSet, b::AbstractSet) = a == b
issetequal(a::AbstractSet, b) = issetequal(a, Set(b))

function issetequal(a, b::AbstractSet)
    if haslength(a)
        # check b for too many unique elements
        length(a) < length(b) && return false
    end
    return issetequal(Set(a), b)
end

function issetequal(a, b)
    haslength(a) && return issetequal(a, Set(b))
    haslength(b) && return issetequal(b, Set(a))
    return issetequal(Set(a), Set(b))
end

## set disjoint comparison
"""
    isdisjoint(a, b) -> Bool

Determine whether the collections `a` and `b` are disjoint.
Equivalent to `isempty(a ∩ b)` but more efficient when possible.

See also: [`intersect`](@ref), [`isempty`](@ref), [`issetequal`](@ref).

!!! compat "Julia 1.5"
    This function requires at least Julia 1.5.

# Examples
```jldoctest
julia> isdisjoint([1, 2], [2, 3, 4])
false

julia> isdisjoint([3, 1], [2, 4])
true
```
"""
function isdisjoint(a, b)
    function _isdisjoint(a, b)
        hasfastin(b) && return !any(in(b), a)
        hasfastin(a) && return !any(in(a), b)
        haslength(b) && length(b) < FASTIN_SET_THRESHOLD &&
            return !any(in(b), a)
        return !any(in(Set(b)), a)
    end
    if haslength(a) && haslength(b) && length(b) < length(a)
        return _isdisjoint(b, a)
    end
    _isdisjoint(a, b)
end

## partial ordering of sets by containment

==(a::AbstractSet, b::AbstractSet) = length(a) == length(b) && a ⊆ b
# convenience functions for AbstractSet
# (if needed, only their synonyms ⊊ and ⊆ must be specialized)
<( a::AbstractSet, b::AbstractSet) = a ⊊ b
<=(a::AbstractSet, b::AbstractSet) = a ⊆ b

## filtering sets

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

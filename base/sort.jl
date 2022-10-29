# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sort

using Base.Order

using Base: copymutable, midpoint, require_one_based_indexing,
    sub_with_overflow, add_with_overflow, OneTo, BitSigned, BitIntegerType

import Base:
    sort,
    sort!,
    issorted,
    sortperm,
    to_indices

export # also exported by Base
    # order-only:
    issorted,
    searchsorted,
    searchsortedfirst,
    searchsortedlast,
    insorted,
    # order & algorithm:
    sort,
    sort!,
    sortperm,
    sortperm!,
    partialsort,
    partialsort!,
    partialsortperm,
    partialsortperm!,
    # algorithms:
    InsertionSort,
    QuickSort,
    MergeSort,
    PartialQuickSort

export # not exported by Base
    Algorithm,
    DEFAULT_UNSTABLE,
    DEFAULT_STABLE,
    SMALL_ALGORITHM,
    SMALL_THRESHOLD


## functions requiring only ordering ##

function issorted(itr, order::Ordering)
    y = iterate(itr)
    y === nothing && return true
    prev, state = y
    y = iterate(itr, state)
    while y !== nothing
        this, state = y
        lt(order, this, prev) && return false
        prev = this
        y = iterate(itr, state)
    end
    return true
end

"""
    issorted(v, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Test whether a vector is in sorted order. The `lt`, `by` and `rev` keywords modify what
order is considered to be sorted just as they do for [`sort`](@ref).

# Examples
```jldoctest
julia> issorted([1, 2, 3])
true

julia> issorted([(1, "b"), (2, "a")], by = x -> x[1])
true

julia> issorted([(1, "b"), (2, "a")], by = x -> x[2])
false

julia> issorted([(1, "b"), (2, "a")], by = x -> x[2], rev=true)
true
```
"""
issorted(itr;
    lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
    issorted(itr, ord(lt,by,rev,order))

function partialsort!(v::AbstractVector, k::Union{Integer,OrdinalRange}, o::Ordering)
    _sort!(v, _PartialQuickSort(k), o)
    maybeview(v, k)
end

maybeview(v, k) = view(v, k)
maybeview(v, k::Integer) = v[k]

"""
    partialsort!(v, k; by=<transform>, lt=<comparison>, rev=false)

Partially sort the vector `v` in place, according to the order specified by `by`, `lt` and
`rev` so that the value at index `k` (or range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted. If `k` is a single
index, that value is returned; if `k` is a range, an array of values at those indices is
returned. Note that `partialsort!` may not fully sort the input array.

# Examples
```jldoctest
julia> a = [1, 2, 4, 3, 4]
5-element Vector{Int64}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4)
4

julia> a
5-element Vector{Int64}:
 1
 2
 3
 4
 4

julia> a = [1, 2, 4, 3, 4]
5-element Vector{Int64}:
 1
 2
 4
 3
 4

julia> partialsort!(a, 4, rev=true)
2

julia> a
5-element Vector{Int64}:
 4
 4
 3
 2
 1
```
"""
partialsort!(v::AbstractVector, k::Union{Integer,OrdinalRange};
             lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
    partialsort!(v, k, ord(lt,by,rev,order))

"""
    partialsort(v, k, by=<transform>, lt=<comparison>, rev=false)

Variant of [`partialsort!`](@ref) which copies `v` before partially sorting it, thereby returning the
same thing as `partialsort!` but leaving `v` unmodified.
"""
partialsort(v::AbstractVector, k::Union{Integer,OrdinalRange}; kws...) =
    partialsort!(copymutable(v), k; kws...)

# reference on sorted binary search:
#   http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equal to x;
# returns lastindex(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
    hi = hi + T(1)
    len = hi - lo
    @inbounds while len != 0
        half_len = len >>> 0x01
        m = lo + half_len
        if lt(o, v[m], x)
            lo = m + 1
            len -= half_len + 1
        else
            hi = m
            len = half_len
        end
    end
    return lo
end

# index of the last value of vector a that is less than or equal to x;
# returns firstindex(v)-1 if x is less than all values of v.
function searchsortedlast(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
    u = T(1)
    lo = lo - u
    hi = hi + u
    @inbounds while lo < hi - u
        m = midpoint(lo, hi)
        if lt(o, x, v[m])
            hi = m
        else
            lo = m
        end
    end
    return lo
end

# returns the range of indices of v equal to x
# if v does not contain x, returns a 0-length range
# indicating the insertion point of x
function searchsorted(v::AbstractVector, x, ilo::T, ihi::T, o::Ordering)::UnitRange{keytype(v)} where T<:Integer
    u = T(1)
    lo = ilo - u
    hi = ihi + u
    @inbounds while lo < hi - u
        m = midpoint(lo, hi)
        if lt(o, v[m], x)
            lo = m
        elseif lt(o, x, v[m])
            hi = m
        else
            a = searchsortedfirst(v, x, max(lo,ilo), m, o)
            b = searchsortedlast(v, x, m, min(hi,ihi), o)
            return a : b
        end
    end
    return (lo + 1) : (hi - 1)
end

function searchsortedlast(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if lt(o, x, f)
        0
    elseif h == 0 || !lt(o, x, l)
        length(a)
    else
        n = round(Integer, (x - f) / h + 1)
        lt(o, x, a[n]) ? n - 1 : n
    end
end

function searchsortedfirst(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if !lt(o, f, x)
        1
    elseif h == 0 || lt(o, l, x)
        length(a) + 1
    else
        n = round(Integer, (x - f) / h + 1)
        lt(o, a[n], x) ? n + 1 : n
    end
end

function searchsortedlast(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if lt(o, x, f)
        0
    elseif h == 0 || !lt(o, x, l)
        length(a)
    else
        if o isa ForwardOrdering
            fld(floor(Integer, x) - f, h) + 1
        else
            fld(ceil(Integer, x) - f, h) + 1
        end
    end
end

function searchsortedfirst(a::AbstractRange{<:Integer}, x::Real, o::DirectOrdering)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if !lt(o, f, x)
        1
    elseif h == 0 || lt(o, l, x)
        length(a) + 1
    else
        if o isa ForwardOrdering
            cld(ceil(Integer, x) - f, h) + 1
        else
            cld(floor(Integer, x) - f, h) + 1
        end
    end
end

searchsorted(a::AbstractRange{<:Real}, x::Real, o::DirectOrdering) =
    searchsortedfirst(a, x, o) : searchsortedlast(a, x, o)

for s in [:searchsortedfirst, :searchsortedlast, :searchsorted]
    @eval begin
        $s(v::AbstractVector, x, o::Ordering) = $s(v,x,firstindex(v),lastindex(v),o)
        $s(v::AbstractVector, x;
           lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) =
            $s(v,x,ord(lt,by,rev,order))
    end
end

"""
    searchsorted(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the range of indices of `a` which compare as equal to `x` (using binary search)
according to the order specified by the `by`, `lt` and `rev` keywords, assuming that `a`
is already sorted in that order. Return an empty range located at the insertion point
if `a` does not contain values equal to `x`.

See also: [`insorted`](@ref), [`searchsortedfirst`](@ref), [`sort`](@ref), [`findall`](@ref).

# Examples
```jldoctest
julia> searchsorted([1, 2, 4, 5, 5, 7], 4) # single match
3:3

julia> searchsorted([1, 2, 4, 5, 5, 7], 5) # multiple matches
4:5

julia> searchsorted([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
3:2

julia> searchsorted([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
7:6

julia> searchsorted([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
1:0
```
""" searchsorted

"""
    searchsortedfirst(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the index of the first value in `a` greater than or equal to `x`, according to the
specified order. Return `lastindex(a) + 1` if `x` is greater than all values in `a`.
`a` is assumed to be sorted.

`insert!`ing `x` at this index will maintain sorted order.

See also: [`searchsortedlast`](@ref), [`searchsorted`](@ref), [`findfirst`](@ref).

# Examples
```jldoctest
julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 4) # single match
3

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 5) # multiple matches
4

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
3

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
7

julia> searchsortedfirst([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
1
```
""" searchsortedfirst

"""
    searchsortedlast(a, x; by=<transform>, lt=<comparison>, rev=false)

Return the index of the last value in `a` less than or equal to `x`, according to the
specified order. Return `firstindex(a) - 1` if `x` is less than all values in `a`. `a` is
assumed to be sorted.

# Examples
```jldoctest
julia> searchsortedlast([1, 2, 4, 5, 5, 7], 4) # single match
3

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 5) # multiple matches
5

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
2

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
6

julia> searchsortedlast([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
0
```
""" searchsortedlast

"""
    insorted(x, a; by=<transform>, lt=<comparison>, rev=false) -> Bool

Determine whether an item `x` is in the sorted collection `a`, in the sense that
it is [`==`](@ref) to one of the values of the collection according to the order
specified by the `by`, `lt` and `rev` keywords, assuming that `a` is already
sorted in that order, see [`sort`](@ref) for the keywords.

See also [`in`](@ref).

# Examples
```jldoctest
julia> insorted(4, [1, 2, 4, 5, 5, 7]) # single match
true

julia> insorted(5, [1, 2, 4, 5, 5, 7]) # multiple matches
true

julia> insorted(3, [1, 2, 4, 5, 5, 7]) # no match
false

julia> insorted(9, [1, 2, 4, 5, 5, 7]) # no match
false

julia> insorted(0, [1, 2, 4, 5, 5, 7]) # no match
false
```

!!! compat "Julia 1.6"
     `insorted` was added in Julia 1.6.
"""
function insorted end
insorted(x, v::AbstractVector; kw...) = !isempty(searchsorted(v, x; kw...))
insorted(x, r::AbstractRange) = in(x, r)

## sorting algorithm components ##

abstract type Algorithm end



#
# Missing values always go at the end
#
struct MissingOptimization{T <: Algorithm} <: Algorithm
    next::T
end

struct WithoutMissingVector{T, U <: AbstractVector{Union{T, Missing}}} <: AbstractVector{T}
    data::U
    function WithoutMissingVector(data; unsafe=false)
        if !unsafe && any(ismissing, data)
            throw(ArgumentError("data must not contain missing values"))
        end
        new{nonmissingtype(eltype(data)), typeof(data)}(data)
    end
end
Base.@propagate_inbounds function Base.getindex(v::WithoutMissingVector, i)
    out = v.data[i]
    @assert !out isa Missing
    out::eltype(v)
end
Base.@propagate_inbounds function Base.setindex!(v::WithoutMissingVector{T}, x::T, i) where T
    v.data[i] = x
    v
end
Base.size(v::WithoutMissingVector) = size(v.data)

# TODO simplify this further, remove redundancy, try a reverse view.
"""
    send_to_end!(f::Function, v::AbstractVector)

Send every element of `v` for which `f` returns `true` to the end of that range and return
the number of elements index of the last element which for which `f` returns `false`.

Preserves the order of the elements that are not sent to the end.
"""
function send_to_end!(f::Function, v::AbstractVector, ::ReverseOrdering; lo, hi)
    i = hi
    @inbounds while lo <= i && !f(v[i])
        i -= 1
    end
    j = i - 1
    @inbounds while lo <= j
        if !f(v[j])
            v[i], v[j] = v[j], v[i]
            i -= 1
        end
        j -= 1
    end
    return (i+1, hi), (lo, i)
end
function send_to_end!(f::Function, v::AbstractVector, ::ForwardOrdering; lo, hi)
    i = lo
    @inbounds while i <= hi && !f(v[i])
        i += 1
    end
    j = i + 1
    @inbounds while j <= hi
        if !f(v[j])
            v[i], v[j] = v[j], v[i]
            i += 1
        end
        j += 1
    end
    return (lo, i-1), (i, hi)
end

function _sort!(v::AbstractVector, a::MissingOptimization, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), kw...)
    if nonmissingtype(eltype(v)) != eltype(v) && o isa DirectOrdering
        (lo, hi), _ = send_to_end!(ismissing, v, o; lo, hi)
        _sort!(WithoutMissingVector(v, unsafe=true), a.next, o; lo, hi, kw...)
        v
    elseif eltype(v) <: Integer && o isa Perm{DirectOrdering} && nonmissingtype(eltype(o.data)) != eltype(o.data)
        (lo, hi), _ = send_to_end!(i -> ismissing(@inbounds o.data[i]), v, o)
        _sort!(v, a.next, Perm(o.order, WithoutMissingVector(o.data, unsafe=true)); lo, hi, kw...)
    else
        _sort!(v, a.next, o; lo, hi, kw...)
    end
end



#
# fast clever sorting for floats
#
struct IEEEFloatOptimization{T <: Algorithm} <: Algorithm
    next::T
end

UIntType(::Type{Float16}) = UInt16
UIntType(::Type{Float32}) = UInt32
UIntType(::Type{Float64}) = UInt64
after_zero(::ForwardOrdering, x) = 0 <= x
after_zero(::ReverseOrdering, x) = x < 0
is_concrete_IEEEFloat(T::Type) = T <: Base.IEEEFloat && isconcretetype(T)
function _sort!(v::AbstractVector, a::IEEEFloatOptimization, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), kw...)
    if is_concrete_IEEEFloat(eltype(v)) && o isa DirectOrdering
        _, (lo, hi) = send_to_end!(!isnan, v, ReverseOrdering(o); lo, hi)
        iv = reinterpret(UIntType(eltype(v)), v)
        (_, j), _ = send_to_end!(x -> after_zero(o, x), v, Forward; lo, hi)
        _sort!(iv, a.next, Reverse; lo, hi=j, kw...)
        _sort!(iv, a.next, Forward; lo=j+1, hi, kw...)
    elseif eltype(v) <: Integer && o isa Perm && o.order isa DirectOrdering && is_concrete_IEEEFloat(eltype(o.data))
        _, (lo, hi) = send_to_end!(i -> !isnan(@inbounds o.data[i]), v, ReverseOrdering(o.order); lo, hi)
        ip = reinterpret(UIntType(eltype(o.data)), o.data)
        (_, j), _ = send_to_end!(i -> after_zero(o.order, @inbounds o.data[i]), v, Forward; lo, hi)
        _sort!(v, a.next, Perm(Reverse, ip); lo, hi=j, kw...)
        _sort!(v, a.next, Perm(Forward, ip); lo=j+1, hi, kw...)
    else
        _sort!(v, a.next, o; lo, hi, kw...)
    end
    v
end



# For AbstractVector{Bool}, counting sort is always best.
# This is an implementation of counting sort specialized for Bools.
# Accepts unused scratch to avoid method ambiguity.
struct BoolOptimization{T <: Algorithm} <: Algorithm
    next::T
end
_sort!(v::AbstractVector, a::BoolOptimization, o::Ordering; kw...) = _sort!(v, a.next, o; kw...)
function _sort!(v::AbstractVector{Bool}, ::BoolOptimization, o::Ordering; lo::Integer, hi::Integer, kw...)
    first = lt(o, false, true) ? false : lt(o, true, false) ? true : return v
    count = 0
    @inbounds for i in lo:hi
        if v[i] == first
            count += 1
        end
    end
    @inbounds v[lo:lo+count-1] .= first
    @inbounds v[lo+count:hi] .= !first
    v
end



#
#
#
struct IsUIntMappable{T <: Algorithm, U <: Algorithm} <: Algorithm
    yes::T
    no::U
end
function _sort!(v::AbstractVector, a::IsUIntMappable, o::Ordering;
                U = UIntMappable(eltype(v), o), kw...)
    if U !== nothing
        _sort!(v, a.yes, o; U, kw...)
    else
        _sort!(v, a.no, o; kw...)
    end
end



#
#
#
struct Small{N, T <: Algorithm, U <: Algorithm} <: Algorithm
    small::T
    big::U
end
Small{N}(big) where N = Small{N, typeof(SMALL_ALGORITHM), typeof(big)}(SMALL_ALGORITHM, big)
function _sort!(v::AbstractVector, a::Small{N}, o::Ordering;
                lo::Integer=firstindex(v), hi::Integer=lastindex(v), lenm1 = hi-lo, kw...) where N
    if lenm1 < N
        _sort!(v, a.small, o; lo, hi, lenm1, kw...)
    else
        _sort!(v, a.big, o; lo, hi, lenm1, kw...)
    end
end



#
#
#
struct InsertionSortAlg <: Algorithm end

"""
    InsertionSort

Indicate that a sorting function should use the insertion sort algorithm.

Insertion sort traverses the collection one element at a time, inserting
each element into its correct, sorted position in the output vector.

Characteristics:
  * *stable*: preserves the ordering of elements which
    compare equal (e.g. "a" and "A" in a sort of letters
    which ignores case).
  * *in-place* in memory.
  * *quadratic performance* in the number of elements to be sorted:
    it is well-suited to small collections but should not be used for large ones.
"""
const InsertionSort = InsertionSortAlg()
const SMALL_ALGORITHM = InsertionSort
function _sort!(v::AbstractVector, ::InsertionSortAlg, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), kw...)
    lo_plus_1 = (lo + 1)::Integer
    @inbounds for i = lo_plus_1:hi
        j = i
        x = v[i]
        while j > lo
            y = v[j-1]
            if !(lt(o, x, y)::Bool)
                break
            end
            v[j] = y
            j -= 1
        end
        v[j] = x
    end
    return v
end



#
#
#
struct CheckSorted{T <: Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::CheckSorted, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), lenm1 = hi-lo, kw...)
    # For most arrays, a presorted check is cheap (overhead < 5%) and for most large
    # arrays it is essentially free (<1%). Insertion sort runs in a fast O(n) on presorted
    # input and this guarantees presorted input will always be efficiently handled
    _issorted(v, lo, hi, o) && return v

    # For large arrays, a reverse-sorted check is essentially free (overhead < 1%)
    if lenm1 >= 500 && _issorted(v, lo, hi, ReverseOrdering(o))
        # If reversing is valid, do so. This does not violate stability
        # because being UIntMappable implies a linear order.
        reverse!(v, lo, hi)
        return v
    end

    _sort!(v, a.next, o; lo, hi, lenm1, kw...)
end



#
# Prerequisite: region to be sorted [lo, hi] is nonempty
#
struct ComputeExtrema{T <: Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::ComputeExtrema, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), kw...)
    mn = mx = v[lo]
    @inbounds for i in (lo+1):hi
        vi = v[i]
        lt(o, vi, mn) && (mn = vi)
        lt(o, mx, vi) && (mx = vi)
    end
    mn, mx

    lt(o, mn, mx) || return v # all same

    _sort!(v, a.next, o; lo, hi, mn, mx, kw...)
end



#
# Consider counting sort
#
struct ConsiderCountingSort{T <: Algorithm, U <: Algorithm} <: Algorithm
    counting::T
    next::U
end
ConsiderCountingSort(next) = ConsiderCountingSort(CountingSort(), next)
function _sort!(v::AbstractVector{<:Integer}, a::ConsiderCountingSort, o::DirectOrdering;
                lo=firstindex(v), hi=lastindex(v), lenm1=hi-lo,
                U = UIntMapping(eltype(v), o),
                mn, mx, range=maybe_unsigned(o === Reverse ? mn-mx : mx-mn), kw...)

    if range < (sizeof(U) > 8 ? 5lenm1-100 : div(lenm1, 2))
        _sort!(v, a.counting, o; lo, hi, lenm1, mn, mx, range, kw...)
    else
        _sort!(v, a.next, o; lo, hi, lenm1, mn, mx, range, kw...)
    end
end
_sort!(v::AbstractVector, a::ConsiderCountingSort, o::Ordering; kw...) = _sort!(v, a.next, o; kw...)



#
# Counting sort
#
struct CountingSort <: Algorithm end
maybe_reverse(o::ForwardOrdering, x) = x
maybe_reverse(o::ReverseOrdering, x) = reverse(x)
function _sort!(v::AbstractVector{<:Integer}, ::CountingSort, o::DirectOrdering;
                lo=firstindex(v), hi=lastindex(v), lenm1=hi-lo,
                mn, mx, range=maybe_unsigned(o === Reverse ? mn-mx : mx-mn), kw...)
    offs = 1 - (o === Reverse ? mx : mn)

    counts = fill(0, range+1)
    @inbounds for i = lo:hi
        counts[v[i] + offs] += 1
    end

    idx = lo
    @inbounds for i = maybe_reverse(o, 1:range+1)
        lastidx = idx + counts[i] - 1
        val = i-offs
        for j = idx:lastidx
            v[j] = val
        end
        idx = lastidx + 1
    end

    v
end



#
# Consider radix sort
#
struct ConsiderRadixSort{T <: Algorithm, U <: Algorithm} <: Algorithm
    radix::T
    next::U
end
ConsiderRadixSort(next) = ConsiderRadixSort(RadixSort(), next)
function _sort!(v::AbstractVector, a::ConsiderRadixSort, o::DirectOrdering;
                lo=firstindex(v), hi=lastindex(v), lenm1=hi-lo,
                U = UIntMappable(eltype(v), o),
                mn, mx, umn=uint_map(mn, o), umx=uint_map(mx, o), urange=umx-umn,
                bits = unsigned(8sizeof(urange) - leading_zeros(urange)), kw...)
    if sizeof(U) <= 8 && bits+70 < 22log(lenm1)
        _sort!(v, a.radix, o; lo, hi, lenm1, mn, mx, umn, umx, urange, bits, kw...)
    else
        _sort!(v, a.next, o; lo, hi, lenm1, mn, mx, umn, umx, urange, bits, kw...)
    end
end



#
# Radix sort
#
struct RadixSort <: Algorithm end
function _sort!(v::AbstractVector, a::RadixSort, o::DirectOrdering;
                lo=firstindex(v), hi=lastindex(v), lenm1=hi-lo,
                mn, mx, umn=uint_mapping(mn, o), umx=uint_mapping(mx, o), urange=umx-umn,
                bits = unsigned(8sizeof(urange) - leading_zeros(urange)),
                U = UIntMappable(eltype(v), o), scratch=nothing, kw...)

    # At this point, we are committed to radix sort.
    u = uint_map!(v, lo, hi, o)

    # we subtract umn to avoid radixing over unnecessary bits. For example,
    # Int32[3, -1, 2] uint_maps to UInt32[0x80000003, 0x7fffffff, 0x80000002]
    # which uses all 32 bits, but once we subtract umn = 0x7fffffff, we are left with
    # UInt32[0x00000004, 0x00000000, 0x00000003] which uses only 3 bits, and
    # Float32[2.012, 400.0, 12.345] uint_maps to UInt32[0x3fff3b63, 0x3c37ffff, 0x414570a4]
    # which is reduced to UInt32[0x03c73b64, 0x00000000, 0x050d70a5] using only 26 bits.
    # the overhead for this subtraction is small enough that it is worthwhile in many cases.

    # this is faster than u[lo:hi] .-= umn as of v1.9.0-DEV.100
    @inbounds for i in lo:hi
        u[i] -= umn
    end

    len = lenm1 + 1
    if scratch !== nothing && checkbounds(Bool, scratch, lo:hi) # Fully preallocated and aligned scratch
        u2 = radix_sort!(u, lo, hi, bits, reinterpret(U, scratch))
        uint_unmap!(v, u2, lo, hi, o, umn)
    elseif scratch !== nothing && (applicable(resize!, scratch, len) || length(scratch) >= len) # Viable scratch
        length(scratch) >= len || resize!(scratch, len)
        t1 = axes(scratch, 1) isa OneTo ? scratch : view(scratch, firstindex(scratch):lastindex(scratch))
        u2 = radix_sort!(view(u, lo:hi), 1, len, bits, reinterpret(U, t1))
        uint_unmap!(view(v, lo:hi), u2, 1, len, o, umn)
    else # No viable scratch
        u2 = radix_sort!(u, lo, hi, bits, similar(u))
        uint_unmap!(v, u2, lo, hi, o, umn)
    end
end



#
# Quicksort
#
"""
    PartialQuickSort(lo::Union{Integer, Missing}, hi::Union{Integer, Missing})

Indicate that a sorting function should use the partial quick sort algorithm.

Partial quick sort finds and sorts the elements that would end up in positions
`lo:hi` using [`QuickSort`](@ref).

Characteristics:
  * *stable*: preserves the ordering of elements which compare equal
    (e.g. "a" and "A" in a sort of letters which ignores case).
  * *not in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).
"""
struct PartialQuickSort{L<:Union{Integer,Missing}, H<:Union{Integer,Missing}, T<:Algorithm} <: Algorithm
    lo::L
    hi::H
    next::T
end
PartialQuickSort(k::Integer) = InitialOptimizations(PartialQuickSort(missing, k, SMALL_ALGORITHM))
PartialQuickSort(k::OrdinalRange) = InitialOptimizations(PartialQuickSort(first(k), last(k), SMALL_ALGORITHM))
_PartialQuickSort(k::Integer) = InitialOptimizations(PartialQuickSort(k:k))
_PartialQuickSort(k::OrdinalRange) = InitialOptimizations(PartialQuickSort(k))

"""
    QuickSort

Indicate that a sorting function should use the quick sort algorithm.

Quick sort picks a pivot element, partitions the array based on the pivot,
and then sorts the elements before and after the pivot recursively.

Characteristics:
  * *stable*: preserves the ordering of elements which compare equal
    (e.g. "a" and "A" in a sort of letters which ignores case).
  * *not in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).
  * *good performance* for almost all large collections.
  * *quadratic worst case runtime* in pathological cases
    (vanishingly rare for non-malicious input)
"""
const QuickSort = PartialQuickSort(missing, missing, SMALL_ALGORITHM)

# select a pivot for QuickSort
#
# This method is redefined to rand(lo:hi) in Random.jl
# We can't use rand here because it is not available in Core.Compiler and
# because rand is defined in the stdlib Random.jl after sorting is used in Base.
select_pivot(lo::Integer, hi::Integer) = typeof(hi-lo)(hash(lo) % (hi-lo+1)) + lo

# select a pivot, partition v[lo:hi] according
# to the pivot, and store the result in t[lo:hi].
#
# returns (pivot, pivot_index) where pivot_index is the location the pivot
# should end up, but does not set t[pivot_index] = pivot
function partition!(t::AbstractVector, lo::Integer, hi::Integer, o::Ordering, v::AbstractVector, rev::Bool)
    pivot_index = select_pivot(lo, hi)
    trues = 0
    @inbounds begin
        pivot = v[pivot_index]
        while lo < pivot_index
            x = v[lo]
            fx = rev ? !lt(o, x, pivot) : lt(o, pivot, x)
            t[(fx ? hi : lo) - trues] = x
            trues += fx
            lo += 1
        end
        while lo < hi
            x = v[lo+1]
            fx = rev ? lt(o, pivot, x) : !lt(o, x, pivot)
            t[(fx ? hi : lo) - trues] = x
            trues += fx
            lo += 1
        end
    end

    # pivot_index = lo-trues
    # t[pivot_index] is whatever it was before
    # t[<pivot_index] <* pivot, stable
    # t[>pivot_index] >* pivot, reverse stable

    pivot, lo-trues
end

function _sort!(v::AbstractVector, a::PartialQuickSort, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), scratch=similar(v),
                t=reinterpret(eltype(v), checkbounds(Bool, scratch, lo:hi) ? scratch : resize!(scratch, length(v))),
                swap=false, rev=false, kw...)

    while lo < hi && hi - lo > SMALL_THRESHOLD
        pivot, j = swap ? partition!(v, lo, hi, o, t, rev) : partition!(t, lo, hi, o, v, rev)
        @inbounds v[j] = pivot
        swap = !swap

        # For QuickSort, a.lo === a.hi === missing, so the first two branches get skipped
        if !ismissing(a.lo) && j <= a.lo # Skip sorting the lower part
            swap && copyto!(v, lo, t, lo, j-lo)
            rev && reverse!(v, lo, j-1)
            lo = j+1
            rev = !rev
        elseif !ismissing(a.hi) && a.hi <= j # Skip sorting the upper part
            swap && copyto!(v, j+1, t, j+1, hi-j)
            rev || reverse!(v, j+1, hi)
            hi = j-1
        elseif j-lo < hi-j
            # Sort the lower part recursively because it is smaller. Recursing on the
            # smaller part guarantees O(log(n)) stack space even on pathological inputs.
            _sort!(v, a, o; lo, hi=j-1, scratch, t, swap, rev, kw...)
            lo = j+1
            rev = !rev
        else # Sort the higher part recursively
            _sort!(v, a, o; lo=j+1, hi, scratch, t, swap, rev=!rev, kw...)
            hi = j-1
        end
    end
    hi < lo && return v
    swap && copyto!(v, lo, t, lo, hi-lo+1)
    rev && reverse!(v, lo, hi)
    _sort!(v, a.next, o; lo, hi, scratch, t, kw...)
end



#
# StableCheckSorted
#
struct StableCheckSorted{T<:Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::StableCheckSorted, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), kw...)
    if _issorted(v, lo, hi, o)
        return v
    elseif _issorted(v, lo, hi, Lt((x, y) -> !lt(o, x, y)))
        # Reverse only if necessary. Using issorted(..., Reverse(o)) would violate stability.
        return reverse!(v, lo, hi)
    end

    _sort!(v, a.next, o; lo, hi, kw...)
end


# This is a stable least significant bit first radix sort.
#
# That is, it first sorts the entire vector by the last chunk_size bits, then by the second
# to last chunk_size bits, and so on. Stability means that it will not reorder two elements
# that compare equal. This is essential so that the order introduced by earlier,
# less significant passes is preserved by later passes.
#
# Each pass divides the input into 2^chunk_size == mask+1 buckets. To do this, it
#  * counts the number of entries that fall into each bucket
#  * uses those counts to compute the indices to move elements of those buckets into
#  * moves elements into the computed indices in the swap array
#  * switches the swap and working array
#
# In the case of an odd number of passes, the returned vector will === the input vector t,
# not v. This is one of the many reasons radix_sort! is not exported.
function radix_sort!(v::AbstractVector{U}, lo::Integer, hi::Integer, bits::Unsigned,
                     t::AbstractVector{U}, chunk_size=radix_chunk_size_heuristic(lo, hi, bits)) where U <: Unsigned
    # bits is unsigned for performance reasons.
    mask = UInt(1) << chunk_size - 1
    counts = Vector{Int}(undef, mask+2)

    @inbounds for shift in 0:chunk_size:bits-1

        # counts[2:mask+2] will store the number of elements that fall into each bucket.
        # if chunk_size = 8, counts[2] is bucket 0x00 and counts[257] is bucket 0xff.
        counts .= 0
        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 2 # compute its bucket's index for this pass
            counts[i] += 1            # increment that bucket's count
        end

        counts[1] = lo                # set target index for the first bucket
        cumsum!(counts, counts)       # set target indices for subsequent buckets
        # counts[1:mask+1] now stores indices where the first member of each bucket
        # belongs, not the number of elements in each bucket. We will put the first element
        # of bucket 0x00 in t[counts[1]], the next element of bucket 0x00 in t[counts[1]+1],
        # and the last element of bucket 0x00 in t[counts[2]-1].

        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 1 # compute its bucket's index for this pass
            j = counts[i]             # lookup the target index
            t[j] = x                  # put the element where it belongs
            counts[i] = j + 1         # increment the target index for the next
        end                           #  ↳ element in this bucket

        v, t = t, v # swap the now sorted destination vector t back into primary vector v

    end

    v
end
function radix_chunk_size_heuristic(lo::Integer, hi::Integer, bits::Unsigned)
    # chunk_size is the number of bits to radix over at once.
    # We need to allocate an array of size 2^chunk size, and on the other hand the higher
    # the chunk size the fewer passes we need. Theoretically, chunk size should be based on
    # the Lambert W function applied to length. Empirically, we use this heuristic:
    guess = min(10, log(maybe_unsigned(hi-lo))*3/4+3)
    # TODO the maximum chunk size should be based on architecture cache size.

    # We need iterations * chunk size ≥ bits, and these cld's
    # make an effort to get iterations * chunk size ≈ bits
    UInt8(cld(bits, cld(bits, guess)))
end

maybe_unsigned(x::Integer) = x # this is necessary to avoid calling unsigned on BigInt
maybe_unsigned(x::BitSigned) = unsigned(x)
function _extrema(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    mn = mx = v[lo]
    @inbounds for i in (lo+1):hi
        vi = v[i]
        lt(o, vi, mn) && (mn = vi)
        lt(o, mx, vi) && (mx = vi)
    end
    mn, mx
end
function _issorted(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    @boundscheck checkbounds(v, lo:hi)
    @inbounds for i in (lo+1):hi
        lt(o, v[i], v[i-1]) && return false
    end
    true
end

## default sorting policy ##

InitialOptimizations(x) = MissingOptimization(BoolOptimization(Small{10}(IEEEFloatOptimization(x))))
const DEFAULT_STABLE = InitialOptimizations(IsUIntMappable(
    Small{40}(CheckSorted(ComputeExtrema(ConsiderCountingSort(ConsiderRadixSort(Small{80}(QuickSort)))))),
    StableCheckSorted(QuickSort)))
const DEFAULT_UNSTABLE = DEFAULT_STABLE
const SMALL_THRESHOLD  = 20



defalg(v::AbstractArray) = DEFAULT_STABLE

"""
    sort!(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the vector `v` in place. A stable algorithm is used by default. You can select a
specific algorithm to use via the `alg` keyword (see [Sorting Algorithms](@ref) for
available algorithms). The `by` keyword lets you provide a function that will be applied to
each element before comparison; the `lt` keyword allows providing a custom "less than"
function (note that for every `x` and `y`, only one of `lt(x,y)` and `lt(y,x)` can return
`true`); use `rev=true` to reverse the sorting order. These options are independent and can
be used together in all possible combinations: if both `by` and `lt` are specified, the `lt`
function is applied to the result of the `by` function; `rev=true` reverses whatever
ordering specified via the `by` and `lt` keywords.

# Examples
```jldoctest
julia> v = [3, 1, 2]; sort!(v); v
3-element Vector{Int64}:
 1
 2
 3

julia> v = [3, 1, 2]; sort!(v, rev = true); v
3-element Vector{Int64}:
 3
 2
 1

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[1]); v
3-element Vector{Tuple{Int64, String}}:
 (1, "c")
 (2, "b")
 (3, "a")

julia> v = [(1, "c"), (3, "a"), (2, "b")]; sort!(v, by = x -> x[2]); v
3-element Vector{Tuple{Int64, String}}:
 (3, "a")
 (2, "b")
 (1, "c")
```
"""
function sort!(v::AbstractVector{T};
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward,
               scratch::Union{AbstractVector{T}, Nothing}=nothing) where T
    if scratch === nothing # TODO: reduce redundancy
        _sort!(v, alg, ord(lt,by,rev,order))
    else
        _sort!(v, alg, ord(lt,by,rev,order); scratch)
    end
end

"""
    sort(v; alg::Algorithm=defalg(v), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Variant of [`sort!`](@ref) that returns a sorted copy of `v` leaving `v` itself unmodified.

# Examples
```jldoctest
julia> v = [3, 1, 2];

julia> sort(v)
3-element Vector{Int64}:
 1
 2
 3

julia> v
3-element Vector{Int64}:
 3
 1
 2
```
"""
sort(v::AbstractVector; kws...) = sort!(copymutable(v); kws...)

## partialsortperm: the permutation to sort the first k elements of an array ##

"""
    partialsortperm(v, k; by=<transform>, lt=<comparison>, rev=false)

Return a partial permutation `I` of the vector `v`, so that `v[I]` returns values of a fully
sorted version of `v` at index `k`. If `k` is a range, a vector of indices is returned; if
`k` is an integer, a single index is returned. The order is specified using the same
keywords as `sort!`. The permutation is stable, meaning that indices of equal elements
appear in ascending order.

Note that this function is equivalent to, but more efficient than, calling `sortperm(...)[k]`.

# Examples
```jldoctest
julia> v = [3, 1, 2, 1];

julia> v[partialsortperm(v, 1)]
1

julia> p = partialsortperm(v, 1:3)
3-element view(::Vector{Int64}, 1:3) with eltype Int64:
 2
 4
 3

julia> v[p]
3-element Vector{Int64}:
 1
 1
 2
```
"""
partialsortperm(v::AbstractVector, k::Union{Integer,OrdinalRange}; kwargs...) =
    partialsortperm!(similar(Vector{eltype(k)}, axes(v,1)), v, k; kwargs..., initialized=false)

"""
    partialsortperm!(ix, v, k; by=<transform>, lt=<comparison>, rev=false, initialized=false)

Like [`partialsortperm`](@ref), but accepts a preallocated index vector `ix` the same size as
`v`, which is used to store (a permutation of) the indices of `v`.

If the index vector `ix` is initialized with the indices of `v` (or a permutation thereof), `initialized` should be set to
`true`.

If `initialized` is `false` (the default), then `ix` is initialized to contain the indices of `v`.

If `initialized` is `true`, but `ix` does not contain (a permutation of) the indices of `v`, the behavior of
`partialsortperm!` is undefined.

(Typically, the indices of `v` will be `1:length(v)`, although if `v` has an alternative array type
with non-one-based indices, such as an `OffsetArray`, `ix` must also be an `OffsetArray` with the same
indices, and must contain as values (a permutation of) these same indices.)

Upon return, `ix` is guaranteed to have the indices `k` in their sorted positions, such that

```julia
partialsortperm!(ix, v, k);
v[ix[k]] == partialsort(v, k)
```

The return value is the `k`th element of `ix` if `k` is an integer, or view into `ix` if `k` is
a range.

# Examples
```jldoctest
julia> v = [3, 1, 2, 1];

julia> ix = Vector{Int}(undef, 4);

julia> partialsortperm!(ix, v, 1)
2

julia> ix = [1:4;];

julia> partialsortperm!(ix, v, 2:3, initialized=true)
2-element view(::Vector{Int64}, 2:3) with eltype Int64:
 4
 3
```
 """
function partialsortperm!(ix::AbstractVector{<:Integer}, v::AbstractVector,
                          k::Union{Integer, OrdinalRange};
                          lt::Function=isless,
                          by::Function=identity,
                          rev::Union{Bool,Nothing}=nothing,
                          order::Ordering=Forward,
                          initialized::Bool=false)
    if axes(ix,1) != axes(v,1)
        throw(ArgumentError("The index vector is used as scratch space and must have the " *
                            "same length/indices as the source vector, $(axes(ix,1)) != $(axes(v,1))"))
    end
    if !initialized
        @inbounds for i in eachindex(ix)
            ix[i] = i
        end
    end

    # do partial quicksort
    _sort!(ix, _PartialQuickSort(k), Perm(ord(lt, by, rev, order), v))

    maybeview(ix, k)
end

## sortperm: the permutation to sort an array ##

"""
    sortperm(A; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, [dims::Integer])

Return a permutation vector or array `I` that puts `A[I]` in sorted order along the given dimension.
If `A` has more than one dimension, then the `dims` keyword argument must be specified. The order is specified
using the same keywords as [`sort!`](@ref). The permutation is guaranteed to be stable even
if the sorting algorithm is unstable, meaning that indices of equal elements appear in
ascending order.

See also [`sortperm!`](@ref), [`partialsortperm`](@ref), [`invperm`](@ref), [`indexin`](@ref).
To sort slices of an array, refer to [`sortslices`](@ref).

# Examples
```jldoctest
julia> v = [3, 1, 2];

julia> p = sortperm(v)
3-element Vector{Int64}:
 2
 3
 1

julia> v[p]
3-element Vector{Int64}:
 1
 2
 3

julia> A = [8 7; 5 6]
2×2 Matrix{Int64}:
 8  7
 5  6

julia> sortperm(A, dims = 1)
2×2 Matrix{Int64}:
 2  4
 1  3

julia> sortperm(A, dims = 2)
2×2 Matrix{Int64}:
 3  1
 2  4
```
"""
function sortperm(A::AbstractArray;
                  alg::Algorithm=DEFAULT_UNSTABLE,
                  lt=isless,
                  by=identity,
                  rev::Union{Bool,Nothing}=nothing,
                  order::Ordering=Forward,
                  scratch::Union{AbstractVector{<:Integer}, Nothing}=nothing,
                  dims...) #to optionally specify dims argument
    ordr = ord(lt,by,rev,order)
    if ordr === Forward && isa(A,Vector) && eltype(A)<:Integer
        n = length(A)
        if n > 1
            min, max = extrema(A)
            (diff, o1) = sub_with_overflow(max, min)
            (rangelen, o2) = add_with_overflow(diff, oneunit(diff))
            if !(o1 || o2)::Bool && rangelen < div(n,2)
                return sortperm_int_range(A, rangelen, min)
            end
        end
    end
    ix = copymutable(LinearIndices(A))
    sort!(ix; alg, order = Perm(ordr, vec(A)), scratch, dims...)
end


"""
    sortperm!(ix, A; alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward, initialized::Bool=false, [dims::Integer])

Like [`sortperm`](@ref), but accepts a preallocated index vector or array `ix` with the same `axes` as `A`.  If `initialized` is `false`
(the default), `ix` is initialized to contain the values `LinearIndices(A)`.

# Examples
```jldoctest
julia> v = [3, 1, 2]; p = zeros(Int, 3);

julia> sortperm!(p, v); p
3-element Vector{Int64}:
 2
 3
 1

julia> v[p]
3-element Vector{Int64}:
 1
 2
 3

julia> A = [8 7; 5 6]; p = zeros(Int,2, 2);

julia> sortperm!(p, A; dims=1); p
2×2 Matrix{Int64}:
 2  4
 1  3

julia> sortperm!(p, A; dims=2); p
2×2 Matrix{Int64}:
 3  1
 2  4
```
"""
function sortperm!(ix::AbstractArray{T}, A::AbstractArray;
                   alg::Algorithm=DEFAULT_UNSTABLE,
                   lt=isless,
                   by=identity,
                   rev::Union{Bool,Nothing}=nothing,
                   order::Ordering=Forward,
                   initialized::Bool=false,
                   scratch::Union{AbstractVector{T}, Nothing}=nothing,
                   dims...) where T <: Integer #to optionally specify dims argument
    (typeof(A) <: AbstractVector) == (:dims in keys(dims)) && throw(ArgumentError("Dims argument incorrect for type $(typeof(A))"))
    axes(ix) == axes(A) || throw(ArgumentError("index array must have the same size/axes as the source array, $(axes(ix)) != $(axes(A))"))

    if !initialized
        ix .= LinearIndices(A)
    end
    sort!(ix; alg, order = Perm(ord(lt, by, rev, order), vec(A)), scratch, dims...)
end

# sortperm for vectors of few unique integers
function sortperm_int_range(x::Vector{<:Integer}, rangelen, minval)
    offs = 1 - minval
    n = length(x)

    counts = fill(0, rangelen+1)
    counts[1] = 1
    @inbounds for i = 1:n
        counts[x[i] + offs + 1] += 1
    end

    #cumsum!(counts, counts)
    @inbounds for i = 2:length(counts)
        counts[i] += counts[i-1]
    end

    P = Vector{Int}(undef, n)
    @inbounds for i = 1:n
        label = x[i] + offs
        P[counts[label]] = i
        counts[label] += 1
    end

    return P
end

## sorting multi-dimensional arrays ##

"""
    sort(A; dims::Integer, alg::Algorithm=defalg(A), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort a multidimensional array `A` along the given dimension.
See [`sort!`](@ref) for a description of possible
keyword arguments.

To sort slices of an array, refer to [`sortslices`](@ref).

# Examples
```jldoctest
julia> A = [4 3; 1 2]
2×2 Matrix{Int64}:
 4  3
 1  2

julia> sort(A, dims = 1)
2×2 Matrix{Int64}:
 1  2
 4  3

julia> sort(A, dims = 2)
2×2 Matrix{Int64}:
 3  4
 1  2
```
"""
function sort(A::AbstractArray{T};
              dims::Integer,
              alg::Algorithm=defalg(A),
              lt=isless,
              by=identity,
              rev::Union{Bool,Nothing}=nothing,
              order::Ordering=Forward,
              scratch::Union{AbstractVector{T}, Nothing}=similar(A, size(A, dims))) where T
    dim = dims
    order = ord(lt,by,rev,order)
    n = length(axes(A, dim))
    if dim != 1
        pdims = (dim, setdiff(1:ndims(A), dim)...)  # put the selected dimension first
        Ap = permutedims(A, pdims)
        Av = vec(Ap)
        sort_chunks!(Av, n, alg, order, scratch)
        permutedims(Ap, invperm(pdims))
    else
        Av = A[:]
        sort_chunks!(Av, n, alg, order, scratch)
        reshape(Av, axes(A))
    end
end

@noinline function sort_chunks!(Av, n, alg, order, scratch)
    inds = LinearIndices(Av)
    for lo = first(inds):n:last(inds)
        _sort!(Av, alg, order; lo, hi=lo+n-1, scratch)
    end
    Av
end
# TODO: reduce redundancy
@noinline function sort_chunks!(Av, n, alg, order, scratch::Nothing)
    inds = LinearIndices(Av)
    for lo = first(inds):n:last(inds)
        _sort!(Av, alg, order; lo, hi=lo+n-1)
    end
    Av
end


"""
    sort!(A; dims::Integer, alg::Algorithm=defalg(A), lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort the multidimensional array `A` along dimension `dims`.
See [`sort!`](@ref) for a description of possible keyword arguments.

To sort slices of an array, refer to [`sortslices`](@ref).

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> A = [4 3; 1 2]
2×2 Matrix{Int64}:
 4  3
 1  2

julia> sort!(A, dims = 1); A
2×2 Matrix{Int64}:
 1  2
 4  3

julia> sort!(A, dims = 2); A
2×2 Matrix{Int64}:
 1  2
 3  4
```
"""
function sort!(A::AbstractArray{T};
               dims::Integer,
               alg::Algorithm=defalg(A),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward,
               scratch::Union{AbstractVector{T}, Nothing}=similar(A, size(A, dims))) where T
    _sort!(A, Val(dims), alg, ord(lt, by, rev, order), scratch)
end
function _sort!(A::AbstractArray{T}, ::Val{K},
                alg::Algorithm,
                order::Ordering,
                scratch::Union{AbstractVector{T}, Nothing}) where {K,T}
    nd = ndims(A)

    1 <= K <= nd || throw(ArgumentError("dimension out of range"))

    remdims = ntuple(i -> i == K ? 1 : axes(A, i), nd)
    for idx in CartesianIndices(remdims)
        Av = view(A, ntuple(i -> i == K ? Colon() : idx[i], nd)...)
        sort!(Av; alg, order, scratch)
    end
    A
end


## uint mapping to allow radix sorting primitives other than UInts ##

"""
    UIntMappable(T::Type, order::Ordering)

Return `typeof(uint_map(x::T, order))` if [`uint_map`](@ref) and
[`uint_unmap`](@ref) are implemented.

If either is not implemented, return `nothing`.
"""
UIntMappable(T::Type, order::Ordering) = nothing

"""
    uint_map(x, order::Ordering)::Unsigned

Map `x` to an un unsigned integer, maintaining sort order.

The map should be reversible with [`uint_unmap`](@ref), so `isless(order, a, b)` must be
a linear ordering for `a, b <: typeof(x)`. Satisfies
`isless(order, a, b) === (uint_map(a, order) < uint_map(b, order))`
and `x === uint_unmap(typeof(x), uint_map(x, order), order)`

See also: [`UIntMappable`](@ref) [`uint_unmap`](@ref)
"""
function uint_map end

"""
    uint_unmap(T::Type, u::Unsigned, order::Ordering)

Reconstruct the unique value `x::T` that uint_maps to `u`. Satisfies
`x === uint_unmap(T, uint_map(x::T, order), order)` for all `x <: T`.

See also: [`uint_map`](@ref) [`UIntMappable`](@ref)
"""
function uint_unmap end


### Primitive Types

# Integers
uint_map(x::Unsigned, ::ForwardOrdering) = x
uint_unmap(::Type{T}, u::T, ::ForwardOrdering) where T <: Unsigned = u

uint_map(x::Signed, ::ForwardOrdering) =
    unsigned(xor(x, typemin(x)))
uint_unmap(::Type{T}, u::Unsigned, ::ForwardOrdering) where T <: Signed =
    xor(signed(u), typemin(T))

UIntMappable(T::BitIntegerType, ::ForwardOrdering) = unsigned(T)

# Floats are not UIntMappable under regular orderings because they fail on NaN edge cases.
# uint mappings for floats are defined in Float, where the Left and Right orderings
# guarantee that there are no NaN values

# Chars
uint_map(x::Char, ::ForwardOrdering) = reinterpret(UInt32, x)
uint_unmap(::Type{Char}, u::UInt32, ::ForwardOrdering) = reinterpret(Char, u)
UIntMappable(::Type{Char}, ::ForwardOrdering) = UInt32

### Reverse orderings
uint_map(x, rev::ReverseOrdering) = ~uint_map(x, rev.fwd)
uint_unmap(T::Type, u::Unsigned, rev::ReverseOrdering) = uint_unmap(T, ~u, rev.fwd)
UIntMappable(T::Type, order::ReverseOrdering) = UIntMappable(T, order.fwd)


### Vectors

# Convert v to unsigned integers in place, maintaining sort order.
function uint_map!(v::AbstractVector, lo::Integer, hi::Integer, order::Ordering)
    u = reinterpret(UIntMappable(eltype(v), order), v)
    @inbounds for i in lo:hi
        u[i] = uint_map(v[i], order)
    end
    u
end

function uint_unmap!(v::AbstractVector, u::AbstractVector{U}, lo::Integer, hi::Integer,
                     order::Ordering, offset::U=zero(U)) where U <: Unsigned
    @inbounds for i in lo:hi
        v[i] = uint_unmap(eltype(v), u[i]+offset, order)
    end
    v
end



### Unused ###

struct MergeSortAlg{T <: Algorithm} <: Algorithm
    next::T
end
"""
    MergeSort

Indicate that a sorting function should use the merge sort algorithm.

Merge sort divides the collection into subcollections and
repeatedly merges them, sorting each subcollection at each step,
until the entire collection has been recombined in sorted form.

Characteristics:
  * *stable*: preserves the ordering of elements which compare
    equal (e.g. "a" and "A" in a sort of letters which ignores
    case).
  * *not in-place* in memory.
  * *divide-and-conquer* sort strategy.
"""
const MergeSort = MergeSortAlg(SMALL_ALGORITHM)


function _sort!(v::AbstractVector, a::MergeSortAlg, o::Ordering;
                lo=firstindex(v), hi=lastindex(v), scratch=nothing)
    @inbounds if lo < hi
        hi-lo <= SMALL_THRESHOLD && return _sort!(v, a.next, o; lo, hi)

        m = midpoint(lo, hi)

        t = scratch === nothing ? similar(v, m-lo+1) : scratch
        length(t) < m-lo+1 && resize!(t, m-lo+1)
        Base.require_one_based_indexing(t)

        _sort!(v, a, o; lo, hi=m, scratch=t)
        _sort!(v, a, o; lo=m+1, hi, scratch=t)

        i, j = 1, lo
        while j <= m
            t[i] = v[j]
            i += 1
            j += 1
        end

        i, k = 1, lo
        while k < j <= hi
            if lt(o, v[j], t[i])
                v[k] = v[j]
                j += 1
            else
                v[k] = t[i]
                i += 1
            end
            k += 1
        end
        while k < j
            v[k] = t[i]
            k += 1
            i += 1
        end
    end

    return v
end

# Support 3- and 5-argument version of sort! for backwards compatability
sort!(v::AbstractVector, a::Algorithm, o::Ordering) = _sort!(v, a, o)
sort!(v::AbstractVector, lo::Integer, hi::Integer, a::Algorithm, o::Ordering) = _sort!(v, a, o; lo, hi)

end # module Sort

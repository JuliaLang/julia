# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sort

using Base.Order

using Base: copymutable, midpoint, require_one_based_indexing, uinttype, tail,
    sub_with_overflow, add_with_overflow, OneTo, BitSigned, BitIntegerType, top_set_bit

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

abstract type Algorithm end

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
    issorted(v, lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward)

Test whether a collection is in sorted order. The keywords modify what
order is considered sorted, as described in the [`sort!`](@ref) documentation.

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

julia> issorted([1, 2, -2, 3], by=abs)
true
```
"""
function issorted(itr;
        lt=isless, by=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward)
    # Explicit branching because the compiler can't optimize away the
    # type instability of the `ord` call with Bool `rev` parameter.
    if rev === true
        issorted(itr, ord(lt, by, true, order))
    else
        issorted(itr, ord(lt, by, nothing, order))
    end
end

function partialsort!(v::AbstractVector, k::Union{Integer,OrdinalRange}, o::Ordering)
    # TODO move k from `alg` to `kw`
    # Don't perform InitialOptimizations before Bracketing. The optimizations take O(n)
    # time and so does the whole sort. But do perform them before recursive calls because
    # that can cause significant speedups when the target range is large so the runtime is
    # dominated by k log k and the optimizations runs in O(k) time.
    _sort!(v, BoolOptimization(
        Small{12}( # Very small inputs should go straight to insertion sort
            BracketedSort(k))),
        o, (;))
    maybeview(v, k)
end

maybeview(v, k) = view(v, k)
maybeview(v, k::Integer) = v[k]

"""
    partialsort!(v, k; by=identity, lt=isless, rev=false)

Partially sort the vector `v` in place so that the value at index `k` (or
range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted. If `k` is a single
index, that value is returned; if `k` is a range, an array of values at those indices is
returned. Note that `partialsort!` may not fully sort the input array.

For the keyword arguments, see the documentation of [`sort!`](@ref).


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
    partialsort(v, k, by=identity, lt=isless, rev=false)

Variant of [`partialsort!`](@ref) that copies `v` before partially sorting it, thereby returning the
same thing as `partialsort!` but leaving `v` unmodified.
"""
partialsort(v::AbstractVector, k::Union{Integer,OrdinalRange}; kws...) =
    partialsort!(copymutable(v), k; kws...)

# reference on sorted binary search:
#   https://www.tbray.org/ongoing/When/200x/2003/03/22/Binary

# index of the first value of vector a that is greater than or equivalent to x;
# returns lastindex(v)+1 if x is greater than all values in v.
function searchsortedfirst(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
    hi = hi + T(1)
    len = hi - lo
    while len != 0
        half_len = len >>> 0x01
        m = lo + half_len
        if lt(o, @inbounds(v[m]), x)
            lo = m + 1
            len -= half_len + 1
        else
            hi = m
            len = half_len
        end
    end
    return lo
end

# index of the last value of vector a that is less than or equivalent to x;
# returns firstindex(v)-1 if x is less than all values of v.
function searchsortedlast(v::AbstractVector, x, lo::T, hi::T, o::Ordering)::keytype(v) where T<:Integer
    u = T(1)
    lo = lo - u
    hi = hi + u
    while lo != hi - u
        m = midpoint(lo, hi)
        if lt(o, x, @inbounds(v[m]))
            hi = m
        else
            lo = m
        end
    end
    return lo
end

# returns the range of indices of v equivalent to x
# if v does not contain x, returns a 0-length range
# indicating the insertion point of x
function searchsorted(v::AbstractVector, x, ilo::T, ihi::T, o::Ordering)::UnitRange{keytype(v)} where T<:Integer
    u = T(1)
    lo = ilo - u
    hi = ihi + u
    while lo != hi - u
        m = midpoint(lo, hi)
        if lt(o, @inbounds(v[m]), x)
            lo = m
        elseif lt(o, x, @inbounds(v[m]))
            hi = m
        else
            a = searchsortedfirst(v, x, lo+u, m, o)
            b = searchsortedlast(v, x, m, hi-u, o)
            return a : b
        end
    end
    return (lo + 1) : (hi - 1)
end


const FastRangeOrderings = Union{DirectOrdering,Lt{typeof(<)},ReverseOrdering{Lt{typeof(<)}}}

function searchsortedlast(a::AbstractRange{<:Real}, x::Real, o::FastRangeOrderings)::keytype(a)
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

function searchsortedfirst(a::AbstractRange{<:Real}, x::Real, o::FastRangeOrderings)::keytype(a)
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

function searchsortedlast(a::AbstractRange{<:Integer}, x::Real, o::FastRangeOrderings)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if lt(o, x, f)
        0
    elseif h == 0 || !lt(o, x, l)
        length(a)
    else
        if !(o isa ReverseOrdering)
            fld(floor(Integer, x) - f, h) + 1
        else
            fld(ceil(Integer, x) - f, h) + 1
        end
    end
end

function searchsortedfirst(a::AbstractRange{<:Integer}, x::Real, o::FastRangeOrderings)::keytype(a)
    require_one_based_indexing(a)
    f, h, l = first(a), step(a), last(a)
    if !lt(o, f, x)
        1
    elseif h == 0 || lt(o, l, x)
        length(a) + 1
    else
        if !(o isa ReverseOrdering)
            cld(ceil(Integer, x) - f, h) + 1
        else
            cld(floor(Integer, x) - f, h) + 1
        end
    end
end

searchsorted(a::AbstractRange{<:Real}, x::Real, o::FastRangeOrderings) =
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
    searchsorted(v, x; by=identity, lt=isless, rev=false)

Return the range of indices in `v` where values are equivalent to `x`, or an
empty range located at the insertion point if `v` does not contain values
equivalent to `x`. The vector `v` must be sorted according to the order defined
by the keywords. Refer to [`sort!`](@ref) for the meaning of the keywords and
the definition of equivalence. Note that the `by` function is applied to the
searched value `x` as well as the values in `v`.

The range is generally found using binary search, but there are optimized
implementations for some inputs.

See also: [`searchsortedfirst`](@ref), [`sort!`](@ref), [`insorted`](@ref), [`findall`](@ref).

# Examples
```jldoctest
julia> searchsorted([1, 2, 4, 5, 5, 7], 4) # single match
3:3

julia> searchsorted([1, 2, 4, 5, 5, 7], 5) # multiple matches
4:5

julia> searchsorted([1, 2, 4, 5, 5, 7], 3) # no match, insert in the middle
3:2 (empty range)

julia> searchsorted([1, 2, 4, 5, 5, 7], 9) # no match, insert at end
7:6 (empty range)

julia> searchsorted([1, 2, 4, 5, 5, 7], 0) # no match, insert at start
1:0 (empty range)

julia> searchsorted([1=>"one", 2=>"two", 2=>"two", 4=>"four"], 2=>"two", by=first) # compare the keys of the pairs
2:3
```
""" searchsorted

"""
    searchsortedfirst(v, x; by=identity, lt=isless, rev=false)

Return the index of the first value in `v` that is not ordered before `x`.
If all values in `v` are ordered before `x`, return `lastindex(v) + 1`.

The vector `v` must be sorted according to the order defined by the keywords.
`insert!`ing `x` at the returned index will maintain the sorted order.
Refer to [`sort!`](@ref) for the meaning and use of the keywords.
Note that the `by` function is applied to the searched value `x` as well as the
values in `v`.

The index is generally found using binary search, but there are optimized
implementations for some inputs.

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

julia> searchsortedfirst([1=>"one", 2=>"two", 4=>"four"], 3=>"three", by=first) # compare the keys of the pairs
3
```
""" searchsortedfirst

"""
    searchsortedlast(v, x; by=identity, lt=isless, rev=false)

Return the index of the last value in `v` that is not ordered after `x`.
If all values in `v` are ordered after `x`, return `firstindex(v) - 1`.

The vector `v` must be sorted according to the order defined by the keywords.
`insert!`ing `x` immediately after the returned index will maintain the sorted order.
Refer to [`sort!`](@ref) for the meaning and use of the keywords.
Note that the `by` function is applied to the searched value `x` as well as the
values in `v`.

The index is generally found using binary search, but there are optimized
implementations for some inputs

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

julia> searchsortedlast([1=>"one", 2=>"two", 4=>"four"], 3=>"three", by=first) # compare the keys of the pairs
2
```
""" searchsortedlast

"""
    insorted(x, v; by=identity, lt=isless, rev=false)::Bool

Determine whether a vector `v` contains any value equivalent to `x`.
The vector `v` must be sorted according to the order defined by the keywords.
Refer to [`sort!`](@ref) for the meaning of the keywords and the definition of
equivalence. Note that the `by` function is applied to the searched value `x`
as well as the values in `v`.

The check is generally done using binary search, but there are optimized
implementations for some inputs.

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

julia> insorted(2=>"TWO", [1=>"one", 2=>"two", 4=>"four"], by=first) # compare the keys of the pairs
true
```

!!! compat "Julia 1.6"
     `insorted` was added in Julia 1.6.
"""
function insorted end
insorted(x, v::AbstractVector; kw...) = !isempty(searchsorted(v, x; kw...))
insorted(x, r::AbstractRange) = in(x, r)

## Alternative keyword management

macro getkw(syms...)
    getters = (getproperty(Sort, Symbol(:_, sym)) for sym in syms)
    Expr(:block, (:($(esc(:((kw, $sym) = $getter(v, o, kw))))) for (sym, getter) in zip(syms, getters))...)
end

for (sym, exp, type) in [
        (:lo, :(firstindex(v)), Integer),
        (:hi, :(lastindex(v)),  Integer),
        (:mn, :(throw(ArgumentError("mn is needed but has not been computed"))), :(eltype(v))),
        (:mx, :(throw(ArgumentError("mx is needed but has not been computed"))), :(eltype(v))),
        (:scratch, nothing, :(Union{Nothing, Vector})), # could have different eltype
        (:legacy_dispatch_entry, nothing, Union{Nothing, Algorithm})]
    usym = Symbol(:_, sym)
    @eval function $usym(v, o, kw)
        # using missing instead of nothing because scratch could === nothing.
        res = get(kw, $(Expr(:quote, sym)), missing)
        res !== missing && return kw, res::$type
        $sym = $exp
        (;kw..., $sym), $sym::$type
    end
end

## Scratch space management

"""
    make_scratch(scratch::Union{Nothing, Vector}, T::Type, len::Integer)

Returns `(s, t)` where `t` is an `AbstractVector` of type `T` with length at least `len`
that is backed by the `Vector` `s`. If `scratch !== nothing`, then `s === scratch`.

This function will allocate a new vector if `scratch === nothing`, `resize!` `scratch` if it
is too short, and `reinterpret` `scratch` if its eltype is not `T`.
"""
function make_scratch(scratch::Nothing, T::Type, len::Integer)
    s = Vector{T}(undef, len)
    s, s
end
function make_scratch(scratch::Vector{T}, ::Type{T}, len::Integer) where T
    len > length(scratch) && resize!(scratch, len)
    scratch, scratch
end
function make_scratch(scratch::Vector, T::Type, len::Integer)
    len_bytes = len * sizeof(T)
    len_scratch = div(len_bytes, sizeof(eltype(scratch)))
    len_scratch > length(scratch) && resize!(scratch, len_scratch)
    scratch, reinterpret(T, scratch)
end


## sorting algorithm components ##

"""
    _sort!(v::AbstractVector, a::Base.Sort.Algorithm, o::Base.Order.Ordering, kw; t, offset)

An internal function that sorts `v` using the algorithm `a` under the ordering `o`,
subject to specifications provided in `kw` (such as `lo` and `hi` in which case it only
sorts `view(v, lo:hi)`)

Returns a scratch space if provided or constructed during the sort, or `nothing` if
no scratch space is present.

!!! note
    `_sort!` modifies but does not return `v`.

A returned scratch space will be a `Vector{T}` where `T` is usually the eltype of `v`. There
are some exceptions, for example if `eltype(v) == Union{Missing, T}` then the scratch space
may be a `Vector{T}` due to `MissingOptimization` changing the eltype of `v` to `T`.

`t` is an appropriate scratch space for the algorithm at hand, to be accessed as
`t[i + offset]`. `t` is used for an algorithm to pass a scratch space back to itself in
internal or recursive calls.
"""
function _sort! end

# TODO: delete this optimization when views have no overhead.
const UnwrappableSubArray = SubArray{T, 1, <:AbstractArray{T}, <:Tuple{AbstractUnitRange, Vararg{Number}}, true} where T
"""
    SubArrayOptimization(next) isa Base.Sort.Algorithm

Unwrap certain known SubArrays because views have a performance overhead 😢

Specifically, unwraps some instances of the type

    $UnwrappableSubArray
"""
struct SubArrayOptimization{T <: Algorithm} <: Algorithm
    next::T
end

_sort!(v::AbstractVector, a::SubArrayOptimization, o::Ordering, kw) = _sort!(v, a.next, o, kw)
function _sort!(v::UnwrappableSubArray, a::SubArrayOptimization, o::Ordering, kw)
    @getkw lo hi
    # @assert v.stride1 == 1
    parent = v.parent
    if parent isa Array && !(parent isa Vector) && hi - lo < 100
        # vec(::Array{T, ≠1}) allocates and is therefore somewhat expensive.
        # We don't want that for small inputs.
        _sort!(v, a.next, o, kw)
    else
        _sort!(vec(parent), a.next, o, (;kw..., lo = lo + v.offset1, hi = hi + v.offset1))
    end
end

"""
    MissingOptimization(next) isa Base.Sort.Algorithm

Filter out missing values.

Missing values are placed after other values according to `DirectOrdering`s. This pass puts
them there and passes on a view into the original vector that excludes the missing values.
This pass is triggered for both `sort([1, missing, 3])` and `sortperm([1, missing, 3])`.
"""
struct MissingOptimization{T <: Algorithm} <: Algorithm
    next::T
end

struct WithoutMissingVector{T, U} <: AbstractVector{T}
    data::U
    function WithoutMissingVector(data; unsafe=false)
        if !unsafe && any(ismissing, data)
            throw(ArgumentError("data must not contain missing values"))
        end
        new{nonmissingtype(eltype(data)), typeof(data)}(data)
    end
end
Base.@propagate_inbounds function Base.getindex(v::WithoutMissingVector, i::Integer)
    out = v.data[i]
    @assert !(out isa Missing)
    out::eltype(v)
end
Base.@propagate_inbounds function Base.setindex!(v::WithoutMissingVector, x, i::Integer)
    v.data[i] = x
    v
end
Base.size(v::WithoutMissingVector) = size(v.data)
Base.axes(v::WithoutMissingVector) = axes(v.data)

"""
    send_to_end!(f::Function, v::AbstractVector; [lo, hi])

Send every element of `v` for which `f` returns `true` to the end of the vector and return
the index of the last element for which `f` returns `false`.

`send_to_end!(f, v, lo, hi)` is equivalent to `send_to_end!(f, view(v, lo:hi))+lo-1`

Preserves the order of the elements that are not sent to the end.
"""
function send_to_end!(f::F, v::AbstractVector; lo=firstindex(v), hi=lastindex(v)) where F <: Function
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
    i - 1
end
"""
    send_to_end!(f::Function, v::AbstractVector, o::Base.Order.DirectOrdering[, end_stable]; lo, hi)

Return `(a, b)` where `v[a:b]` are the elements that are not sent to the end.

If `o isa ReverseOrdering` then the "end" of `v` is `v[lo]`.

If `end_stable` is set, the elements that are sent to the end are stable instead of the
elements that are not
"""
@inline send_to_end!(f::F, v::AbstractVector, ::ForwardOrdering, end_stable=false; lo, hi) where F <: Function =
    end_stable ? (lo, hi-send_to_end!(!f, view(v, hi:-1:lo))) : (lo, send_to_end!(f, v; lo, hi))
@inline send_to_end!(f::F, v::AbstractVector, ::ReverseOrdering, end_stable=false; lo, hi) where F <: Function =
    end_stable ? (send_to_end!(!f, v; lo, hi)+1, hi) : (hi-send_to_end!(f, view(v, hi:-1:lo))+1, hi)


function _sort!(v::AbstractVector, a::MissingOptimization, o::Ordering, kw)
    @getkw lo hi
    if o isa DirectOrdering && eltype(v) >: Missing && nonmissingtype(eltype(v)) != eltype(v)
        lo, hi = send_to_end!(ismissing, v, o; lo, hi)
        _sort!(WithoutMissingVector(v, unsafe=true), a.next, o, (;kw..., lo, hi))
    elseif o isa Perm && o.order isa DirectOrdering && eltype(v) <: Integer &&
                eltype(o.data) >: Missing && nonmissingtype(eltype(o.data)) != eltype(o.data) &&
                all(i === j for (i,j) in zip(v, eachindex(o.data)))
        # TODO make this branch known at compile time
        # This uses a custom function because we need to ensure stability of both sides and
        # we can assume v is equal to eachindex(o.data) which allows a copying partition
        # without allocations.
        lo_i, hi_i = lo, hi
        cv = eachindex(o.data) # equal to copy(v)
        for i in lo:hi
            x = o.data[cv[i]]
            if ismissing(x) == (o.order == Reverse) # should x go at the beginning/end?
                v[lo_i] = i
                lo_i += 1
            else
                v[hi_i] = i
                hi_i -= 1
            end
        end
        reverse!(v, lo_i, hi)
        if o.order == Reverse
            lo = lo_i
        else
            hi = hi_i
        end

        _sort!(v, a.next, Perm(o.order, WithoutMissingVector(o.data, unsafe=true)), (;kw..., lo, hi))
    else
        _sort!(v, a.next, o, kw)
    end
end


"""
    IEEEFloatOptimization(next) isa Base.Sort.Algorithm

Move NaN values to the end, partition by sign, and reinterpret the rest as unsigned integers.

IEEE floating point numbers (`Float64`, `Float32`, and `Float16`) compare the same as
unsigned integers with the bits with a few exceptions. This pass

This pass is triggered for both `sort([1.0, NaN, 3.0])` and `sortperm([1.0, NaN, 3.0])`.
"""
struct IEEEFloatOptimization{T <: Algorithm} <: Algorithm
    next::T
end

after_zero(::ForwardOrdering, x) = !signbit(x)
after_zero(::ReverseOrdering, x) = signbit(x)
is_concrete_IEEEFloat(T::Type) = T <: Base.IEEEFloat && isconcretetype(T)
function _sort!(v::AbstractVector, a::IEEEFloatOptimization, o::Ordering, kw)
    @getkw lo hi
    if is_concrete_IEEEFloat(eltype(v)) && o isa DirectOrdering
        lo, hi = send_to_end!(isnan, v, o, true; lo, hi)
        iv = reinterpret(uinttype(eltype(v)), v)
        j = send_to_end!(x -> after_zero(o, x), v; lo, hi)
        scratch = _sort!(iv, a.next, Reverse, (;kw..., lo, hi=j))
        if scratch === nothing # Union split
            _sort!(iv, a.next, Forward, (;kw..., lo=j+1, hi, scratch))
        else
            _sort!(iv, a.next, Forward, (;kw..., lo=j+1, hi, scratch))
        end
    elseif eltype(v) <: Integer && o isa Perm && o.order isa DirectOrdering && is_concrete_IEEEFloat(eltype(o.data))
        lo, hi = send_to_end!(i -> isnan(@inbounds o.data[i]), v, o.order, true; lo, hi)
        ip = reinterpret(uinttype(eltype(o.data)), o.data)
        j = send_to_end!(i -> after_zero(o.order, @inbounds o.data[i]), v; lo, hi)
        scratch = _sort!(v, a.next, Perm(Reverse, ip), (;kw..., lo, hi=j))
        if scratch === nothing # Union split
            _sort!(v, a.next, Perm(Forward, ip), (;kw..., lo=j+1, hi, scratch))
        else
            _sort!(v, a.next, Perm(Forward, ip), (;kw..., lo=j+1, hi, scratch))
        end
    else
        _sort!(v, a.next, o, kw)
    end
end


"""
    BoolOptimization(next) isa Base.Sort.Algorithm

Sort `AbstractVector{Bool}`s using a specialized version of counting sort.

Accesses each element at most twice (one read and one write), and performs at most two
comparisons.
"""
struct BoolOptimization{T <: Algorithm} <: Algorithm
    next::T
end
_sort!(v::AbstractVector, a::BoolOptimization, o::Ordering, kw) = _sort!(v, a.next, o, kw)
function _sort!(v::AbstractVector{Bool}, ::BoolOptimization, o::Ordering, kw)
    first = lt(o, false, true) ? false : lt(o, true, false) ? true : return v
    @getkw lo hi scratch
    count = 0
    @inbounds for i in lo:hi
        if v[i] == first
            count += 1
        end
    end
    @inbounds v[lo:lo+count-1] .= first
    @inbounds v[lo+count:hi] .= !first
    scratch
end


"""
    IsUIntMappable(yes, no) isa Base.Sort.Algorithm

Determines if the elements of a vector can be mapped to unsigned integers while preserving
their order under the specified ordering.

If they can be, dispatch to the `yes` algorithm and record the unsigned integer type that
the elements may be mapped to. Otherwise dispatch to the `no` algorithm.
"""
struct IsUIntMappable{T <: Algorithm, U <: Algorithm} <: Algorithm
    yes::T
    no::U
end
function _sort!(v::AbstractVector, a::IsUIntMappable, o::Ordering, kw)
    if UIntMappable(eltype(v), o) !== nothing
        _sort!(v, a.yes, o, kw)
    else
        _sort!(v, a.no, o, kw)
    end
end


"""
    Small{N}(small=SMALL_ALGORITHM, big) isa Base.Sort.Algorithm

Sort inputs with `length(lo:hi) <= N` using the `small` algorithm. Otherwise use the `big`
algorithm.
"""
struct Small{N, T <: Algorithm, U <: Algorithm} <: Algorithm
    small::T
    big::U
end
Small{N}(small, big) where N = Small{N, typeof(small), typeof(big)}(small, big)
Small{N}(big) where N = Small{N}(SMALL_ALGORITHM, big)
function _sort!(v::AbstractVector, a::Small{N}, o::Ordering, kw) where N
    @getkw lo hi
    if (hi-lo) < N
        _sort!(v, a.small, o, kw)
    else
        _sort!(v, a.big, o, kw)
    end
end


struct InsertionSortAlg <: Algorithm end

"""
    InsertionSort

Use the insertion sort algorithm.

Insertion sort traverses the collection one element at a time, inserting
each element into its correct, sorted position in the output vector.

Characteristics:
* *stable*: preserves the ordering of elements that compare equal
(e.g. "a" and "A" in a sort of letters that ignores case).
* *in-place* in memory.
* *quadratic performance* in the number of elements to be sorted:
it is well-suited to small collections but should not be used for large ones.
"""
const InsertionSort = InsertionSortAlg()

"""
    SMALL_ALGORITHM

Default sorting algorithm for small arrays.

This is an alias for a simple low-overhead algorithm that does not scale well
to large arrays, unlike high-overhead recursive algorithms used for larger arrays.
`SMALL_ALGORITHM` is a good choice for the base case of a recursive algorithm.
"""
const SMALL_ALGORITHM = InsertionSortAlg()

function _sort!(v::AbstractVector, ::InsertionSortAlg, o::Ordering, kw)
    @getkw lo hi scratch
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
    scratch
end


"""
    CheckSorted(next) isa Base.Sort.Algorithm

Check if the input is already sorted and for large inputs, also check if it is
reverse-sorted. The reverse-sorted check is unstable.
"""
struct CheckSorted{T <: Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::CheckSorted, o::Ordering, kw)
    @getkw lo hi scratch

    # For most arrays, a presorted check is cheap (overhead < 5%) and for most large
    # arrays it is essentially free (<1%).
    _issorted(v, lo, hi, o) && return scratch

    # For most large arrays, a reverse-sorted check is essentially free (overhead < 1%)
    if hi-lo >= 500 && _issorted(v, lo, hi, ReverseOrdering(o))
        # If reversing is valid, do so. This violates stability.
        reverse!(v, lo, hi)
        return scratch
    end

    _sort!(v, a.next, o, kw)
end


"""
    ComputeExtrema(next) isa Base.Sort.Algorithm

Compute the extrema of the input under the provided order.

If the minimum is no less than the maximum, then the input is already sorted. Otherwise,
dispatch to the `next` algorithm.
"""
struct ComputeExtrema{T <: Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::ComputeExtrema, o::Ordering, kw)
    @getkw lo hi scratch
    mn = mx = v[lo]
    @inbounds for i in (lo+1):hi
        vi = v[i]
        lt(o, vi, mn) && (mn = vi)
        lt(o, mx, vi) && (mx = vi)
    end

    lt(o, mn, mx) || return scratch # all same

    _sort!(v, a.next, o, (;kw..., mn, mx))
end


"""
    ConsiderCountingSort(counting=CountingSort(), next) isa Base.Sort.Algorithm

If the input's range is small enough, use the `counting` algorithm. Otherwise, dispatch to
the `next` algorithm.

For most types, the threshold is if the range is shorter than half the length, but for types
larger than Int64, bitshifts are expensive and RadixSort is not viable, so the threshold is
much more generous.
"""
struct ConsiderCountingSort{T <: Algorithm, U <: Algorithm} <: Algorithm
    counting::T
    next::U
end
ConsiderCountingSort(next) = ConsiderCountingSort(CountingSort(), next)
function _sort!(v::AbstractVector{<:Integer}, a::ConsiderCountingSort, o::DirectOrdering, kw)
    @getkw lo hi mn mx
    range = maybe_unsigned(o === Reverse ? mn-mx : mx-mn)

    if range < (sizeof(eltype(v)) > 8 ? 5(hi-lo)-100 : div(hi-lo, 2))
        _sort!(v, a.counting, o, kw)
    else
        _sort!(v, a.next, o, kw)
    end
end
_sort!(v::AbstractVector, a::ConsiderCountingSort, o::Ordering, kw) = _sort!(v, a.next, o, kw)


"""
    CountingSort() isa Base.Sort.Algorithm

Use the counting sort algorithm.

`CountingSort` is an algorithm for sorting integers that runs in Θ(length + range) time and
space. It counts the number of occurrences of each value in the input and then iterates
through those counts repopulating the input with the values in sorted order.
"""
struct CountingSort <: Algorithm end
maybe_reverse(o::ForwardOrdering, x) = x
maybe_reverse(o::ReverseOrdering, x) = reverse(x)
function _sort!(v::AbstractVector{<:Integer}, ::CountingSort, o::DirectOrdering, kw)
    @getkw lo hi mn mx scratch
    range = maybe_unsigned(o === Reverse ? mn-mx : mx-mn)
    offs = 1 - (o === Reverse ? mx : mn)

    counts = fill(0, range+1) # TODO use scratch (but be aware of type stability)
    @inbounds for i = lo:hi
        counts[v[i] + offs] += 1
    end

    idx = lo
    @inbounds for i = maybe_reverse(o, 1:range+1)
        lastidx = idx + counts[i] - 1
        val = i-offs
        for j = idx:lastidx
            v[j] = val isa Unsigned && eltype(v) <: Signed ? signed(val) : val
        end
        idx = lastidx + 1
    end

    scratch
end


"""
    ConsiderRadixSort(radix=RadixSort(), next) isa Base.Sort.Algorithm

If the number of bits in the input's range is small enough and the input supports efficient
bitshifts, use the `radix` algorithm. Otherwise, dispatch to the `next` algorithm.
"""
struct ConsiderRadixSort{T <: Algorithm, U <: Algorithm} <: Algorithm
    radix::T
    next::U
end
ConsiderRadixSort(next) = ConsiderRadixSort(RadixSort(), next)
function _sort!(v::AbstractVector, a::ConsiderRadixSort, o::DirectOrdering, kw)
    @getkw lo hi mn mx
    urange = uint_map(mx, o)-uint_map(mn, o)
    bits = unsigned(top_set_bit(urange))
    if sizeof(eltype(v)) <= 8 && bits+70 < 22log(hi-lo)
        _sort!(v, a.radix, o, kw)
    else
        _sort!(v, a.next, o, kw)
    end
end


"""
    RadixSort() isa Base.Sort.Algorithm

Use the radix sort algorithm.

`RadixSort` is a stable least significant bit first radix sort algorithm that runs in
`O(length * log(range))` time and linear space.

It first sorts the entire vector by the last `chunk_size` bits, then by the second
to last `chunk_size` bits, and so on. Stability means that it will not reorder two elements
that compare equal. This is essential so that the order introduced by earlier,
less significant passes is preserved by later passes.

Each pass divides the input into `2^chunk_size == mask+1` buckets. To do this, it
 * counts the number of entries that fall into each bucket
 * uses those counts to compute the indices to move elements of those buckets into
 * moves elements into the computed indices in the swap array
 * switches the swap and working array

`chunk_size` is larger for larger inputs and determined by an empirical heuristic.
"""
struct RadixSort <: Algorithm end
function _sort!(v::AbstractVector, a::RadixSort, o::DirectOrdering, kw)
    @getkw lo hi mn mx scratch
    umn = uint_map(mn, o)
    urange = uint_map(mx, o)-umn
    bits = unsigned(top_set_bit(urange))

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

    scratch, t = make_scratch(scratch, eltype(v), hi-lo+1)
    tu = reinterpret(eltype(u), t)
    if radix_sort!(u, lo, hi, bits, tu, 1-lo)
        uint_unmap!(v, u, lo, hi, o, umn)
    else
        uint_unmap!(v, tu, lo, hi, o, umn, 1-lo)
    end
    scratch
end


"""
    ScratchQuickSort(next::Base.Sort.Algorithm=Base.Sort.SMALL_ALGORITHM) isa Base.Sort.Algorithm
    ScratchQuickSort(lo::Union{Integer, Missing}, hi::Union{Integer, Missing}=lo, next::Base.Sort.Algorithm=Base.Sort.SMALL_ALGORITHM) isa Base.Sort.Algorithm

Use the `ScratchQuickSort` algorithm with the `next` algorithm as a base case.

`ScratchQuickSort` is like `QuickSort`, but utilizes scratch space to operate faster and allow
for the possibility of maintaining stability.

If `lo` and `hi` are provided, finds and sorts the elements in the range `lo:hi`, reordering
but not necessarily sorting other elements in the process. If `lo` or `hi` is `missing`, it
is treated as the first or last index of the input, respectively.

`lo` and `hi` may be specified together as an `AbstractUnitRange`.

Characteristics:
  * *stable*: preserves the ordering of elements that compare equal
    (e.g. "a" and "A" in a sort of letters that ignores case).
  * *not in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`QuickSort`](@ref).
  * *linear runtime* if `length(lo:hi)` is constant
  * *quadratic worst case runtime* in pathological cases
  (vanishingly rare for non-malicious input)
"""
struct ScratchQuickSort{L<:Union{Integer,Missing}, H<:Union{Integer,Missing}, T<:Algorithm} <: Algorithm
    lo::L
    hi::H
    next::T
end
ScratchQuickSort(next::Algorithm=SMALL_ALGORITHM) = ScratchQuickSort(missing, missing, next)
ScratchQuickSort(lo::Union{Integer, Missing}, hi::Union{Integer, Missing}) = ScratchQuickSort(lo, hi, SMALL_ALGORITHM)
ScratchQuickSort(lo::Union{Integer, Missing}, next::Algorithm=SMALL_ALGORITHM) = ScratchQuickSort(lo, lo, next)
ScratchQuickSort(r::OrdinalRange, next::Algorithm=SMALL_ALGORITHM) = ScratchQuickSort(first(r), last(r), next)

# select a pivot, partition v[lo:hi] according
# to the pivot, and store the result in t[lo:hi].
#
# sets `pivot_dest[pivot_index+pivot_index_offset] = pivot` and returns that index.
function partition!(t::AbstractVector, lo::Integer, hi::Integer, offset::Integer, o::Ordering,
        v::AbstractVector, rev::Bool, pivot_dest::AbstractVector, pivot_index_offset::Integer)
    # Ideally we would use `pivot_index = rand(lo:hi)`, but that requires Random.jl
    # and would mutate the global RNG in sorting.
    pivot_index = mod(hash(lo), lo:hi)
    @inbounds begin
        pivot = v[pivot_index]
        while lo < pivot_index
            x = v[lo]
            fx = rev ? !lt(o, x, pivot) : lt(o, pivot, x)
            t[(fx ? hi : lo) - offset] = x
            offset += fx
            lo += 1
        end
        while lo < hi
            x = v[lo+1]
            fx = rev ? lt(o, pivot, x) : !lt(o, x, pivot)
            t[(fx ? hi : lo) - offset] = x
            offset += fx
            lo += 1
        end
        pivot_index = lo-offset + pivot_index_offset
        pivot_dest[pivot_index] = pivot
    end

    # t_pivot_index = lo-offset (i.e. without pivot_index_offset)
    # t[t_pivot_index] is whatever it was before unless t is the pivot_dest
    # t[<t_pivot_index] <* pivot, stable
    # t[>t_pivot_index] >* pivot, reverse stable

    pivot_index
end

function _sort!(v::AbstractVector, a::ScratchQuickSort, o::Ordering, kw;
                t=nothing, offset=nothing, swap=false, rev=false)
    @getkw lo hi scratch

    if t === nothing
        scratch, t = make_scratch(scratch, eltype(v), hi-lo+1)
        offset = 1-lo
        kw = (;kw..., scratch)
    end

    while lo < hi && hi - lo > SMALL_THRESHOLD
        j = if swap
            partition!(v, lo+offset, hi+offset, offset, o, t, rev, v, 0)
        else
            partition!(t, lo, hi, -offset, o, v, rev, v, -offset)
        end
        swap = !swap

        # For ScratchQuickSort(), a.lo === a.hi === missing, so the first two branches get skipped
        if !ismissing(a.lo) && j <= a.lo # Skip sorting the lower part
            swap && copyto!(v, lo, t, lo+offset, j-lo)
            rev && reverse!(v, lo, j-1)
            lo = j+1
            rev = !rev
        elseif !ismissing(a.hi) && a.hi <= j # Skip sorting the upper part
            swap && copyto!(v, j+1, t, j+1+offset, hi-j)
            rev || reverse!(v, j+1, hi)
            hi = j-1
        elseif j-lo < hi-j
            # Sort the lower part recursively because it is smaller. Recursing on the
            # smaller part guarantees O(log(n)) stack space even on pathological inputs.
            _sort!(v, a, o, (;kw..., lo, hi=j-1); t, offset, swap, rev)
            lo = j+1
            rev = !rev
        else # Sort the higher part recursively
            _sort!(v, a, o, (;kw..., lo=j+1, hi); t, offset, swap, rev=!rev)
            hi = j-1
        end
    end
    hi < lo && return scratch
    swap && copyto!(v, lo, t, lo+offset, hi-lo+1)
    rev && reverse!(v, lo, hi)
    _sort!(v, a.next, o, (;kw..., lo, hi))
end


"""
    BracketedSort(target[, next::Algorithm]) isa Base.Sort.Algorithm

Perform a partialsort for the elements that fall into the indices specified by the `target`
using BracketedSort with the `next` algorithm for subproblems.

BracketedSort takes a random* sample of the input, estimates the quantiles of the input
using the quantiles of the sample to find signposts that almost certainly bracket the target
values, filters the value in the input that fall between the signpost values to the front of
the input, and then, if that "almost certainly" turned out to be true, finds the target
within the small chunk that are, by value, between the signposts and now by position, at the
front of the vector. On small inputs or when target is close to the size of the input,
BracketedSort falls back to the `next` algorithm directly. Otherwise, BracketedSort uses the
`next` algorithm only to compute quantiles of the sample and to find the target within the
small chunk.

## Performance

If the `next` algorithm has `O(n * log(n))` runtime and the input is not pathological then
the runtime of this algorithm is `O(n + k * log(k))` where `n` is the length of the input
and `k` is `length(target)`. On pathological inputs the asymptotic runtime is the same as
the runtime of the `next` algorithm.

BracketedSort itself does not allocate. If `next` is in-place then BracketedSort is also
in-place. If `next` is not in place, and it's space usage increases monotonically with input
length then BracketedSort's maximum space usage will never be more than the space usage
of `next` on the input BracketedSort receives. For large nonpathological inputs and targets
substantially smaller than the size of the input, BracketedSort's maximum memory usage will
be much less than `next`'s. If the maximum additional space usage of `next` scales linearly
then for small k the average* maximum additional space usage of BracketedSort will be
`O(n^(2.3/3))`.

By default, BracketedSort uses the `O(n)` space and `O(n + k log k)` runtime
`ScratchQuickSort` algorithm recursively.

*Sorting is unable to depend on Random.jl because Random.jl depends on sorting.
 Consequently, we use `hash` as a source of randomness. The average runtime guarantees
 assume that `hash(x::Int)` produces a random result. However, as this randomization is
 deterministic, if you try hard enough you can find inputs that consistently reach the
 worst case bounds. Actually constructing such inputs is an exercise left to the reader.
 Have fun :).

Characteristics:
  * *unstable*: does not preserve the ordering of elements that compare equal
    (e.g. "a" and "A" in a sort of letters that ignores case).
  * *in-place* in memory if the `next` algorithm is in-place.
  * *estimate-and-filter*: strategy
  * *linear runtime* if `length(target)` is constant and `next` is reasonable
  * *n + k log k* worst case runtime if `next` has that runtime.
  * *pathological inputs* can significantly increase constant factors.
"""
struct BracketedSort{T, F} <: Algorithm
    target::T
    get_next::F
end

# TODO: this composition between BracketedSort and ScratchQuickSort does not bring me joy
BracketedSort(k) = BracketedSort(k, k -> InitialOptimizations(ScratchQuickSort(k)))

function bracket_kernel!(v::AbstractVector, lo, hi, lo_signpost, hi_signpost, o)
    i = 0
    count_below = 0
    checkbounds(v, lo:hi)
    for j in lo:hi
        x = @inbounds v[j]
        a = lo_signpost !== nothing && lt(o, x, lo_signpost)
        b = hi_signpost === nothing || !lt(o, hi_signpost, x)
        count_below += a
        # if a != b # This branch is almost never taken, so making it branchless is bad.
        #     @inbounds v[i], v[j] = v[j], v[i]
        #     i += 1
        # end
        c = a != b # JK, this is faster.
        k = i * c + j
        # Invariant: @assert firstindex(v) ≤ lo ≤ i + j ≤ k ≤ j ≤ hi ≤ lastindex(v)
        @inbounds v[j], v[k] = v[k], v[j]
        i += c - 1
    end
    count_below, i+hi
end

function move!(v, target, source)
    # This function never dominates runtime—only add `@inbounds` if you can demonstrate a
    # performance improvement. And if you do, also double check behavior when `target`
    # is out of bounds.
    @assert length(target) == length(source)
    if length(target) == 1 || isdisjoint(target, source)
        for (i, j) in zip(target, source)
            v[i], v[j] = v[j], v[i]
        end
    else
        @assert minimum(source) <= minimum(target)
        reverse!(v, minimum(source), maximum(target))
        reverse!(v, minimum(target), maximum(target))
    end
end

function _sort!(v::AbstractVector, a::BracketedSort, o::Ordering, kw)
    @getkw lo hi scratch
    # TODO for further optimization: reuse scratch between trials better, from signpost
    # selection to recursive calls, and from the fallback (but be aware of type stability,
    # especially when sorting IEEE floats.

    # We don't need to bounds check target because that is done higher up in the stack
    # However, we cannot assume the target is inbounds.
    lo < hi || return scratch
    ln = hi - lo + 1

    # This is simply a precomputed short-circuit to avoid doing scalar math for small inputs.
    # It does not change dispatch at all.
    ln < 260 && return _sort!(v, a.get_next(a.target), o, kw)

    target = a.target
    k = cbrt(ln)
    k2 = round(Int, k^2)
    k2ln = k2/ln
    offset = .15k*top_set_bit(k2) # TODO for further optimization: tune this
    lo_signpost_i, hi_signpost_i =
        (floor(Int, (tar - lo) * k2ln + lo + off) for (tar, off) in
            ((minimum(target), -offset), (maximum(target), offset)))
    lastindex_sample = lo+k2-1
    expected_middle_ln = (min(lastindex_sample, hi_signpost_i) - max(lo, lo_signpost_i) + 1) / k2ln
    # This heuristic is complicated because it fairly accurately reflects the runtime of
    # this algorithm which is necessary to get good dispatch when both the target is large
    # and the input are large.
    # expected_middle_ln is a float and k2 is significantly below typemax(Int), so this will
    # not overflow:
    # TODO move target from alg to kw to avoid this ickyness:
    ln <= 130 + 2k2 + 2expected_middle_ln && return _sort!(v, a.get_next(a.target), o, kw)

    # We store the random sample in
    #     sample = view(v, lo:lo+k2)
    # but views are not quite as fast as using the input array directly,
    # so we don't actually construct this view at runtime.

    # TODO for further optimization: handle lots of duplicates better.
    # Right now lots of duplicates rounds up when it could use some super fast optimizations
    # in some cases.
    # e.g.
    #
    # Target:                      |----|
    # Sorted input: 000000000000000000011111112222223333333333
    #
    # Will filter all zeros and ones to the front when it could just take the first few
    # it encounters. This optimization would be especially potent when `allequal(ans)` and
    # equal elements are egal.

    # 3 random trials should typically give us 0.99999 reliability; we can assume
    # the input is pathological and abort to fallback if we fail three trials.
    seed = hash(ln, Int === Int64 ? 0x85eb830e0216012d : 0xae6c4e15)
    for attempt in 1:3
        seed = hash(attempt, seed)
        for i in lo:lo+k2-1
            j = mod(hash(i, seed), i:hi) # TODO for further optimization: be sneaky and remove this division
            v[i], v[j] = v[j], v[i]
        end
        count_below, lastindex_middle = if lo_signpost_i <= lo && lastindex_sample <= hi_signpost_i
            # The heuristics higher up in this function that dispatch to the `next`
            # algorithm should prevent this from happening.
            # Specifically, this means that expected_middle_ln == ln, so
            # ln <= ... + 2.0expected_middle_ln && return ...
            # will trigger.
            @assert false
            # But if it does happen, the kernel reduces to
            0, hi
        elseif lo_signpost_i <= lo
            _sort!(v, a.get_next(hi_signpost_i), o, (;kw..., hi=lastindex_sample))
            bracket_kernel!(v, lo, hi, nothing, v[hi_signpost_i], o)
        elseif lastindex_sample <= hi_signpost_i
            _sort!(v, a.get_next(lo_signpost_i), o, (;kw..., hi=lastindex_sample))
            bracket_kernel!(v, lo, hi, v[lo_signpost_i], nothing, o)
        else
            # TODO for further optimization: don't sort the middle elements
            _sort!(v, a.get_next(lo_signpost_i:hi_signpost_i), o, (;kw..., hi=lastindex_sample))
            bracket_kernel!(v, lo, hi, v[lo_signpost_i], v[hi_signpost_i], o)
        end
        target_in_middle = target .- count_below
        if lo <= minimum(target_in_middle) && maximum(target_in_middle) <= lastindex_middle
            scratch = _sort!(v, a.get_next(target_in_middle), o, (;kw..., hi=lastindex_middle))
            move!(v, target, target_in_middle)
            return scratch
        end
        # This line almost never runs.
    end
    # This line only runs on pathological inputs. Make sure it's covered by tests :)
    _sort!(v, a.get_next(target), o, kw)
end


"""
    StableCheckSorted(next) isa Base.Sort.Algorithm

Check if an input is sorted and/or reverse-sorted.

The definition of reverse-sorted is that for every pair of adjacent elements, the latter is
less than the former. This is stricter than `issorted(v, Reverse(o))` to avoid swapping pairs
of elements that compare equal.
"""
struct StableCheckSorted{T<:Algorithm} <: Algorithm
    next::T
end
function _sort!(v::AbstractVector, a::StableCheckSorted, o::Ordering, kw)
    @getkw lo hi scratch
    if _issorted(v, lo, hi, o)
        return scratch
    elseif _issorted(v, lo, hi, Lt((x, y) -> !lt(o, x, y)))
        # Reverse only if necessary. Using issorted(..., Reverse(o)) would violate stability.
        reverse!(v, lo, hi)
        return scratch
    end

    _sort!(v, a.next, o, kw)
end


# The return value indicates whether v is sorted (true) or t is sorted (false)
# This is one of the many reasons radix_sort! is not exported.
function radix_sort!(v::AbstractVector{U}, lo::Integer, hi::Integer, bits::Unsigned,
                     t::AbstractVector{U}, offset::Integer,
                     chunk_size=radix_chunk_size_heuristic(lo, hi, bits)) where U <: Unsigned
    # bits is unsigned for performance reasons.
    counts = Vector{Int}(undef, 1 << chunk_size + 1) # TODO use scratch for this

    shift = 0
    while true
        @noinline radix_sort_pass!(t, lo, hi, offset, counts, v, shift, chunk_size)
        # the latest data resides in t
        shift += chunk_size
        shift < bits || return false
        @noinline radix_sort_pass!(v, lo+offset, hi+offset, -offset, counts, t, shift, chunk_size)
        # the latest data resides in v
        shift += chunk_size
        shift < bits || return true
    end
end
function radix_sort_pass!(t, lo, hi, offset, counts, v, shift, chunk_size)
    mask = UInt(1) << chunk_size - 1  # mask is defined in pass so that the compiler
    @inbounds begin                   #  ↳ knows it's shape
        # counts[2:mask+2] will store the number of elements that fall into each bucket.
        # if chunk_size = 8, counts[2] is bucket 0x00 and counts[257] is bucket 0xff.
        counts .= 0
        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 2 # compute its bucket's index for this pass
            counts[i] += 1            # increment that bucket's count
        end

        counts[1] = lo + offset       # set target index for the first bucket
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
    end
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
function _issorted(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    @boundscheck checkbounds(v, lo:hi)
    @inbounds for i in (lo+1):hi
        lt(o, v[i], v[i-1]) && return false
    end
    true
end


## default sorting policy ##

"""
    InitialOptimizations(next) isa Base.Sort.Algorithm

Attempt to apply a suite of low-cost optimizations to the input vector before sorting. These
optimizations may be automatically applied by the `sort!` family of functions when
`alg=InsertionSort`, `alg=MergeSort`, or `alg=QuickSort` is passed as an argument.

`InitialOptimizations` is an implementation detail and subject to change or removal in
future versions of Julia.

If `next` is stable, then `InitialOptimizations(next)` is also stable.

The specific optimizations attempted by `InitialOptimizations` are
[`SubArrayOptimization`](@ref), [`MissingOptimization`](@ref), [`BoolOptimization`](@ref),
dispatch to [`InsertionSort`](@ref) for inputs with `length <= 10`, and
[`IEEEFloatOptimization`](@ref).
"""
InitialOptimizations(next) = SubArrayOptimization(
    MissingOptimization(
        BoolOptimization(
            Small{10}(
                IEEEFloatOptimization(
                    next)))))

"""
    struct DefaultStable <: Algorithm end

`DefaultStable` is an algorithm which indicates that a fast, general purpose sorting
algorithm should be used, but does not specify exactly which algorithm.

Currently, when sorting short NTuples, this is an unrolled mergesort, and otherwise it is
composed of two parts: the [`InitialOptimizations`](@ref) and a hybrid of Radix, Insertion,
Counting, Quick sorts.

We begin with MissingOptimization because it has no runtime cost when it is not
triggered and can enable other optimizations to be applied later. For example,
BoolOptimization cannot apply to an `AbstractVector{Union{Missing, Bool}}`, but after
[`MissingOptimization`](@ref) is applied, that input will be converted into am
`AbstractVector{Bool}`.

We next apply [`BoolOptimization`](@ref) because it also has no runtime cost when it is not
triggered and when it is triggered, it is an incredibly efficient algorithm (sorting `Bool`s
is quite easy).

Next, we dispatch to [`InsertionSort`](@ref) for inputs with `length <= 10`. This dispatch
occurs before the [`IEEEFloatOptimization`](@ref) pass because the
[`IEEEFloatOptimization`](@ref)s are not beneficial for very small inputs.

To conclude the [`InitialOptimizations`](@ref), we apply [`IEEEFloatOptimization`](@ref).

After these optimizations, we branch on whether radix sort and related algorithms can be
applied to the input vector and ordering. We conduct this branch by testing if
`UIntMappable(v, order) !== nothing`. That is, we see if we know of a reversible mapping
from `eltype(v)` to `UInt` that preserves the ordering `order`. We perform this check after
the initial optimizations because they can change the input vector's type and ordering to
make them `UIntMappable`.

If the input is not [`UIntMappable`](@ref), then we perform a presorted check and dispatch
to [`ScratchQuickSort`](@ref).

Otherwise, we dispatch to [`InsertionSort`](@ref) for inputs with `length <= 40` and then
perform a presorted check ([`CheckSorted`](@ref)).

We check for short inputs before performing the presorted check to avoid the overhead of the
check for small inputs. Because the alternate dispatch is to [`InsertionSort`](@ref) which
has efficient `O(n)` runtime on presorted inputs, the check is not necessary for small
inputs.

We check if the input is reverse-sorted for long vectors (more than 500 elements) because
the check is essentially free unless the input is almost entirely reverse sorted.

Note that once the input is determined to be [`UIntMappable`](@ref), we know the order forms
a [total order](wikipedia.org/wiki/Total_order) over the inputs and so it is impossible to
perform an unstable sort because no two elements can compare equal unless they _are_ equal,
in which case switching them is undetectable. We utilize this fact to perform a more
aggressive reverse sorted check that will reverse the vector `[3, 2, 2, 1]`.

After these potential fast-paths are tried and failed, we [`ComputeExtrema`](@ref) of the
input. This computation has a fairly fast `O(n)` runtime, but we still try to delay it until
it is necessary.

Next, we [`ConsiderCountingSort`](@ref). If the range the input is small compared to its
length, we apply [`CountingSort`](@ref).

Next, we [`ConsiderRadixSort`](@ref). This is similar to the dispatch to counting sort,
but we consider the number of _bits_ in the range, rather than the range itself.
Consequently, we apply [`RadixSort`](@ref) for any reasonably long inputs that reach this
stage.

Finally, if the input has length less than 80, we dispatch to [`InsertionSort`](@ref) and
otherwise we dispatch to [`ScratchQuickSort`](@ref).
"""
struct DefaultStable <: Algorithm end

"""
    DEFAULT_STABLE

The default sorting algorithm.

This algorithm is guaranteed to be stable (i.e. it will not reorder elements that compare
equal). It makes an effort to be fast for most inputs.

The algorithms used by `DEFAULT_STABLE` are an implementation detail. See the docstring
of `Base.Sort.DefaultStable` for the current dispatch system.
"""
const DEFAULT_STABLE = DefaultStable()

"""
    DefaultUnstable <: Algorithm

Like [`DefaultStable`](@ref), but does not guarantee stability.
"""
struct DefaultUnstable <: Algorithm end

"""
    DEFAULT_UNSTABLE

An efficient sorting algorithm which may or may not be stable.

The algorithms used by `DEFAULT_UNSTABLE` are an implementation detail. They are currently
the same as those used by [`DEFAULT_STABLE`](@ref), but this is subject to change in future.
"""
const DEFAULT_UNSTABLE = DefaultUnstable()

const _DEFAULT_ALGORITHMS_FOR_VECTORS = InitialOptimizations(
    IsUIntMappable(
        Small{40}(
            CheckSorted(
                ComputeExtrema(
                    ConsiderCountingSort(
                        ConsiderRadixSort(
                            Small{80}(
                                ScratchQuickSort())))))),
        StableCheckSorted(
            ScratchQuickSort())))

_sort!(v::AbstractVector, ::Union{DefaultStable, DefaultUnstable}, o::Ordering, kw) =
    _sort!(v, _DEFAULT_ALGORITHMS_FOR_VECTORS, o, kw)

const SMALL_THRESHOLD  = 20

function Base.show(io::IO, alg::Algorithm)
    print_tree(io, alg, 0)
end
function print_tree(io::IO, alg::Algorithm, cols::Int)
    print(io, "    "^cols)
    show_type(io, alg)
    print(io, '(')
    for (i, name) in enumerate(fieldnames(typeof(alg)))
        arg = getproperty(alg, name)
        i > 1 && print(io, ',')
        if arg isa Algorithm
            println(io)
            print_tree(io, arg, cols+1)
        else
            i > 1 && print(io, ' ')
            print(io, arg)
        end
    end
    print(io, ')')
end
show_type(io::IO, alg::Algorithm) = Base.show_type_name(io, typeof(alg).name)
show_type(io::IO, alg::Small{N}) where N = print(io, "Base.Sort.Small{$N}")

defalg(v::AbstractArray) = DEFAULT_STABLE
defalg(v::AbstractArray{<:Union{Number, Missing}}) = DEFAULT_UNSTABLE
defalg(v::AbstractArray{Missing}) = DEFAULT_UNSTABLE # for method disambiguation
defalg(v::AbstractArray{Union{}}) = DEFAULT_UNSTABLE # for method disambiguation
defalg(v) = DEFAULT_STABLE

"""
    sort!(v; alg::Base.Sort.Algorithm=Base.Sort.defalg(v), lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward)

Sort the vector `v` in place. A stable algorithm is used by default: the
ordering of elements that compare equal is preserved. A specific algorithm can
be selected via the `alg` keyword (see [Sorting Algorithms](@ref) for available
algorithms).

Elements are first transformed with the function `by` and then compared
according to either the function `lt` or the ordering `order`. Finally, the
resulting order is reversed if `rev=true` (this preserves forward stability:
elements that compare equal are not reversed). The current implementation applies
the `by` transformation before each comparison rather than once per element.

Passing an `lt` other than `isless` along with an `order` other than
[`Base.Order.Forward`](@ref) or [`Base.Order.Reverse`](@ref) is not permitted,
otherwise all options are independent and can be used together in all possible
combinations. Note that `order` can also include a "by" transformation, in
which case it is applied after that defined with the `by` keyword. For more
information on `order` values see the documentation on [Alternate
Orderings](@ref).

Relations between two elements are defined as follows (with "less" and
"greater" exchanged when `rev=true`):

* `x` is less than `y` if `lt(by(x), by(y))` (or `Base.Order.lt(order, by(x), by(y))`) yields true.
* `x` is greater than `y` if `y` is less than `x`.
* `x` and `y` are equivalent if neither is less than the other ("incomparable"
  is sometimes used as a synonym for "equivalent").

The result of `sort!` is sorted in the sense that every element is greater than
or equivalent to the previous one.

The `lt` function must define a strict weak order, that is, it must be

* irreflexive: `lt(x, x)` always yields `false`,
* asymmetric: if `lt(x, y)` yields `true` then `lt(y, x)` yields `false`,
* transitive: `lt(x, y) && lt(y, z)` implies `lt(x, z)`,
* transitive in equivalence: `!lt(x, y) && !lt(y, x)` and `!lt(y, z) && !lt(z,
  y)` together imply `!lt(x, z) && !lt(z, x)`. In words: if `x` and `y` are
  equivalent and `y` and `z` are equivalent then `x` and `z` must be
  equivalent.

For example `<` is a valid `lt` function for `Int` values but `≤` is not: it
violates irreflexivity. For `Float64` values even `<` is invalid as it violates
the fourth condition: `1.0` and `NaN` are equivalent and so are `NaN` and `2.0`
but `1.0` and `2.0` are not equivalent.

See also [`sort`](@ref), [`sortperm`](@ref), [`sortslices`](@ref),
[`partialsort!`](@ref), [`partialsortperm`](@ref), [`issorted`](@ref),
[`searchsorted`](@ref), [`insorted`](@ref), [`Base.Order.ord`](@ref).

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

julia> sort(0:3, by=x->x-2, order=Base.Order.By(abs))
4-element Vector{Int64}:
 2
 1
 3
 0

julia> sort(0:3, by=x->x-2, order=Base.Order.By(abs)) == sort(0:3, by=x->abs(x-2))
true

julia> sort([2, NaN, 1, NaN, 3]) # correct sort with default lt=isless
5-element Vector{Float64}:
   1.0
   2.0
   3.0
 NaN
 NaN

julia> sort([2, NaN, 1, NaN, 3], lt=<) # wrong sort due to invalid lt. This behavior is undefined.
5-element Vector{Float64}:
   2.0
 NaN
   1.0
 NaN
   3.0
```
"""
function sort!(v::AbstractVector{T};
               alg::Algorithm=defalg(v),
               lt=isless,
               by=identity,
               rev::Union{Bool,Nothing}=nothing,
               order::Ordering=Forward,
               scratch::Union{Vector{T}, Nothing}=nothing) where T
    _sort!(v, maybe_apply_initial_optimizations(alg), ord(lt,by,rev,order), (;scratch))
    v
end

"""
    sort(v; alg::Base.Sort.Algorithm=Base.Sort.defalg(v), lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward)
    sort(v::NTuple; kws...)::NTuple

Variant of [`sort!`](@ref) that returns a sorted copy of `v` leaving `v` itself unmodified.

When calling `sort` on the [`keys`](@ref) or [`values](@ref) of a dictionary, `v` is
collected and then sorted in place.

!!! compat "Julia 1.12"
    Sorting `NTuple`s requires Julia 1.12 or later.

!!! compat "Julia 1.13"
    Sorting keys sets and values iterators requires Julia 1.13 or later.

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

julia> sort(values(Dict('a'=>2, 'b'=>1)))
2-element Vector{Int64}:
 1
 2
```
"""
sort(v::AbstractVector; kws...) = sort!(copymutable(v); kws...)

const COLLECT_ON_SORT_TYPES = Union{Base.KeySet, Base.ValueIterator}
sort(v::COLLECT_ON_SORT_TYPES; kws...) = sort!(collect(v); kws...)

function sort(x::NTuple;
              alg::Algorithm=defalg(x),
              lt=isless,
              by=identity,
              rev::Union{Bool,Nothing}=nothing,
              order::Ordering=Forward,
              scratch::Union{Vector, Nothing}=nothing)
    # Can't do this check with type parameters because of https://github.com/JuliaLang/julia/issues/56698
    scratch === nothing || eltype(x) == eltype(scratch) || throw(ArgumentError("scratch has the wrong eltype"))
    _sort(x, alg, ord(lt,by,rev,order), (;scratch))::typeof(x)
end
# Folks who want to hack internals can define a new _sort(x::NTuple, ::TheirAlg, o::Ordering)
# or _sort(x::NTuple{N, TheirType}, ::DefaultStable, o::Ordering) where N
function _sort(x::NTuple, a::Union{DefaultStable, DefaultUnstable}, o::Ordering, kw)
    # The unrolled tuple sort is prohibitively slow to compile for length > 9.
    # See https://github.com/JuliaLang/julia/pull/46104#issuecomment-1435688502 for benchmarks
    if length(x) > 9
        v = copymutable(x)
        _sort!(v, a, o, kw)
        typeof(x)(v)
    else
        _mergesort(x, o)
    end
end
_mergesort(x::Union{NTuple{0}, NTuple{1}}, o::Ordering) = x
function _mergesort(x::NTuple, o::Ordering)
    a, b = Base.IteratorsMD.split(x, Val(length(x)>>1))
    merge(_mergesort(a, o), _mergesort(b, o), o)
end
merge(x::NTuple, y::NTuple{0}, o::Ordering) = x
merge(x::NTuple{0}, y::NTuple, o::Ordering) = y
merge(x::NTuple{0}, y::NTuple{0}, o::Ordering) = x # Method ambiguity
merge(x::NTuple, y::NTuple, o::Ordering) =
    (lt(o, y[1], x[1]) ? (y[1], merge(x, tail(y), o)...) : (x[1], merge(tail(x), y, o)...))

## partialsortperm: the permutation to sort the first k elements of an array ##

"""
    partialsortperm(v, k; by=identity, lt=isless, rev=false)

Return a partial permutation `I` of the vector `v`, so that `v[I]` returns values of a fully
sorted version of `v` at index `k`. If `k` is a range, a vector of indices is returned; if
`k` is an integer, a single index is returned. The order is specified using the same
keywords as `sort!`. The permutation is stable: the indices of equal elements
will appear in ascending order.

This function is equivalent to, but more efficient than, calling `sortperm(...)[k]`.

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
    partialsortperm!(similar(Vector{eltype(k)}, axes(v,1)), v, k; kwargs...)

"""
    partialsortperm!(ix, v, k; by=identity, lt=isless, rev=false)

Like [`partialsortperm`](@ref), but accepts a preallocated index vector `ix` the same size as
`v`, which is used to store (a permutation of) the indices of `v`.

`ix` is initialized to contain the indices of `v`.

(Typically, the indices of `v` will be `1:length(v)`, although if `v` has an alternative array type
with non-one-based indices, such as an `OffsetArray`, `ix` must share those same indices)

Upon return, `ix` is guaranteed to have the indices `k` in their sorted positions, such that

```julia
partialsortperm!(ix, v, k);
v[ix[k]] == partialsort(v, k)
```

The return value is the `k`th element of `ix` if `k` is an integer, or view into `ix` if `k` is
a range.

$(Base._DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> v = [3, 1, 2, 1];

julia> ix = Vector{Int}(undef, 4);

julia> partialsortperm!(ix, v, 1)
2

julia> ix = [1:4;];

julia> partialsortperm!(ix, v, 2:3)
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
    @inbounds for i in eachindex(ix)
        ix[i] = i
    end

    # do partial quicksort
    _sort!(ix, InitialOptimizations(ScratchQuickSort(k)), Perm(ord(lt, by, rev, order), v), (;))

    maybeview(ix, k)
end

## sortperm: the permutation to sort an array ##

"""
    sortperm(A; alg::Base.Sort.Algorithm=Base.Sort.DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward, [dims::Integer])

Return a permutation vector or array `I` that puts `A[I]` in sorted order along the given dimension.
If `A` has more than one dimension, then the `dims` keyword argument must be specified. The order is specified
using the same keywords as [`sort!`](@ref). The permutation is guaranteed to be stable even
if the sorting algorithm is unstable: the indices of equal elements will appear in
ascending order.

See also [`sortperm!`](@ref), [`partialsortperm`](@ref), [`invperm`](@ref), [`indexin`](@ref).
To sort slices of an array, refer to [`sortslices`](@ref).

!!! compat "Julia 1.9"
    The method accepting `dims` requires at least Julia 1.9.

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
                  scratch::Union{Vector{<:Integer}, Nothing}=nothing,
                  dims...) #to optionally specify dims argument
    if rev === true
        _sortperm(A; alg, order=ord(lt, by, true, order), scratch, dims...)
    else
        _sortperm(A; alg, order=ord(lt, by, nothing, order), scratch, dims...)
    end
end
function _sortperm(A::AbstractArray; alg, order, scratch, dims...)
    if order === Forward && isa(A,Vector) && eltype(A)<:Integer
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
    sort!(ix; alg, order = Perm(order, vec(A)), scratch, dims...)
end


"""
    sortperm!(ix, A; alg::Base.Sort.Algorithm=Base.Sort.DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward, [dims::Integer])

Like [`sortperm`](@ref), but accepts a preallocated index vector or array `ix` with the same `axes` as `A`.
`ix` is initialized to contain the values `LinearIndices(A)`.

$(Base._DOCS_ALIASING_WARNING)

!!! compat "Julia 1.9"
    The method accepting `dims` requires at least Julia 1.9.

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
@inline function sortperm!(ix::AbstractArray{T}, A::AbstractArray;
                   alg::Algorithm=DEFAULT_UNSTABLE,
                   lt=isless,
                   by=identity,
                   rev::Union{Bool,Nothing}=nothing,
                   order::Ordering=Forward,
                   initialized::Bool=false,
                   scratch::Union{Vector{T}, Nothing}=nothing,
                   dims...) where T <: Integer #to optionally specify dims argument
    (typeof(A) <: AbstractVector) == (:dims in keys(dims)) && throw(ArgumentError("Dims argument incorrect for type $(typeof(A))"))
    axes(ix) == axes(A) || throw(ArgumentError("index array must have the same size/axes as the source array, $(axes(ix)) != $(axes(A))"))

    ix .= LinearIndices(A)
    if rev === true
        sort!(ix; alg, order=Perm(ord(lt, by, true, order), vec(A)), scratch, dims...)
    else
        sort!(ix; alg, order=Perm(ord(lt, by, nothing, order), vec(A)), scratch, dims...)
    end
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
    sort(A; dims::Integer, alg::Base.Sort.Algorithm=Base.Sort.defalg(A), lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward)

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
              scratch::Union{Vector{T}, Nothing}=nothing) where T
    dim = dims
    order = ord(lt,by,rev,order)
    n = length(axes(A, dim))
    if dim != 1
        pdims = (dim, setdiff(1:ndims(A), dim)...)  # put the selected dimension first
        Ap = permutedims(A, pdims)
        Av = vec(Ap)
        sort_chunks!(Av, n, maybe_apply_initial_optimizations(alg), order, scratch)
        permutedims(Ap, invperm(pdims))
    else
        Av = A[:]
        sort_chunks!(Av, n, maybe_apply_initial_optimizations(alg), order, scratch)
        reshape(Av, axes(A))
    end
end

@noinline function sort_chunks!(Av, n, alg, order, scratch)
    inds = LinearIndices(Av)
    sort_chunks!(Av, n, alg, order, scratch, first(inds), last(inds))
end

@noinline function sort_chunks!(Av, n, alg, order, scratch::Nothing, fst, lst)
    for lo = fst:n:lst
        s = _sort!(Av, alg, order, (; lo, hi=lo+n-1, scratch))
        s !== nothing && return sort_chunks!(Av, n, alg, order, s, lo+n, lst)
    end
    Av
end

@noinline function sort_chunks!(Av, n, alg, order, scratch::AbstractVector, fst, lst)
    for lo = fst:n:lst
        _sort!(Av, alg, order, (; lo, hi=lo+n-1, scratch))
    end
    Av
end


"""
    sort!(A; dims::Integer, alg::Base.Sort.Algorithm=Base.Sort.defalg(A), lt=isless, by=identity, rev::Bool=false, order::Base.Order.Ordering=Base.Order.Forward)

Sort the multidimensional array `A` along dimension `dims`.
See the one-dimensional version of [`sort!`](@ref) for a description of
possible keyword arguments.

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
               order::Ordering=Forward, # TODO stop eagerly over-allocating.
               scratch::Union{Vector{T}, Nothing}=size(A, dims) < 10 ? nothing : Vector{T}(undef, size(A, dims))) where T
    nd = ndims(A)
    1 <= dims <= nd || throw(ArgumentError("dimension out of range"))
    alg2 = maybe_apply_initial_optimizations(alg)
    order2 = ord(lt, by, rev, order)
    foreach(ntuple(Val, nd)) do d
        get_value(d) == dims || return
        # We assume that an Integer between 1 and nd must be equal to one of the
        # values 1:nd. If this assumption is false, then what's an integer? and
        # also sort! will silently do nothing.

        idxs = CartesianIndices(ntuple(i -> i == get_value(d) ? 1 : axes(A, i), ndims(A)))
        get_view(idx) = view(A, ntuple(i -> i == get_value(d) ? Colon() : idx[i], ndims(A))...)
        if d == Val(1) || size(A, get_value(d)) < 30
            for idx in idxs
                sort!(get_view(idx); alg=alg2, order=order2, scratch)
            end
        else
            v = similar(get_view(first(idxs)))
            for idx in idxs
                vw = get_view(idx)
                v .= vw
                sort!(v; alg=alg2, order=order2, scratch)
                vw .= v
            end
        end
        A
    end
    A
end
get_value(::Val{x}) where x = x


## uint mapping to allow radix sorting primitives other than UInts ##

"""
    UIntMappable(T::Type, order::Base.Order.Ordering)

Return `typeof(uint_map(x::T, order))` if [`uint_map`](@ref) and
[`uint_unmap`](@ref) are implemented.

If either is not implemented, return `nothing`.
"""
UIntMappable(T::Type, order::Ordering) = nothing

"""
    uint_map(x, order::Base.Order.Ordering)::Unsigned

Map `x` to an un unsigned integer, maintaining sort order.

The map should be reversible with [`uint_unmap`](@ref), so `isless(order, a, b)` must be
a linear ordering for `a, b <: typeof(x)`. Satisfies
`isless(order, a, b) === (uint_map(a, order) < uint_map(b, order))`
and `x === uint_unmap(typeof(x), uint_map(x, order), order)`

See also: [`UIntMappable`](@ref) [`uint_unmap`](@ref)
"""
function uint_map end

"""
    uint_unmap(T::Type, u::Unsigned, order::Base.Order.Ordering)

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
                     order::Ordering, offset::U=zero(U),
                     index_offset::Integer=0) where U <: Unsigned
    @inbounds for i in lo:hi
        v[i] = uint_unmap(eltype(v), u[i+index_offset]+offset, order)
    end
    v
end



### Unused constructs for backward compatibility ###

## Old algorithms ##

struct QuickSortAlg     <: Algorithm end
struct MergeSortAlg     <: Algorithm end

"""
    PartialQuickSort{T <: Union{Integer,OrdinalRange}}

Indicate that a sorting function should use the partial quick sort algorithm.
`PartialQuickSort(k)` is like `QuickSort`, but is only required to find and
sort the elements that would end up in `v[k]` were `v` fully sorted.

Characteristics:
  * *not stable*: does not preserve the ordering of elements that
    compare equal (e.g. "a" and "A" in a sort of letters that
    ignores case).
  * *in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).

Note that `PartialQuickSort(k)` does not necessarily sort the whole array. For example,

```jldoctest
julia> x = rand(100);

julia> k = 50:100;

julia> s1 = sort(x; alg=QuickSort);

julia> s2 = sort(x; alg=PartialQuickSort(k));

julia> map(issorted, (s1, s2))
(true, false)

julia> map(x->issorted(x[k]), (s1, s2))
(true, true)

julia> s1[k] == s2[k]
true
```
"""
struct PartialQuickSort{T <: Union{Integer,OrdinalRange}} <: Algorithm
    k::T
end

"""
    QuickSort

Indicate that a sorting function should use the quick sort
algorithm, which is *not* stable.

Characteristics:
  * *not stable*: does not preserve the ordering of elements that
    compare equal (e.g. "a" and "A" in a sort of letters that
    ignores case).
  * *in-place* in memory.
  * *divide-and-conquer*: sort strategy similar to [`MergeSort`](@ref).
  * *good performance* for large collections.
"""
const QuickSort     = QuickSortAlg()

"""
    MergeSort

Indicate that a sorting function should use the merge sort
algorithm. Merge sort divides the collection into
subcollections and repeatedly merges them, sorting each
subcollection at each step, until the entire
collection has been recombined in sorted form.

Characteristics:
  * *stable*: preserves the ordering of elements that compare
    equal (e.g. "a" and "A" in a sort of letters that ignores
    case).
  * *not in-place* in memory.
  * *divide-and-conquer* sort strategy.
  * *good performance* for large collections but typically not quite as
    fast as [`QuickSort`](@ref).
"""
const MergeSort     = MergeSortAlg()

maybe_apply_initial_optimizations(alg::Algorithm) = alg
maybe_apply_initial_optimizations(alg::QuickSortAlg) = InitialOptimizations(alg)
maybe_apply_initial_optimizations(alg::MergeSortAlg) = InitialOptimizations(alg)
maybe_apply_initial_optimizations(alg::InsertionSortAlg) = InitialOptimizations(alg)

# selectpivot!
#
# Given 3 locations in an array (lo, mi, and hi), sort v[lo], v[mi], v[hi] and
# choose the middle value as a pivot
#
# Upon return, the pivot is in v[lo], and v[hi] is guaranteed to be
# greater than the pivot

@inline function selectpivot!(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    @inbounds begin
        mi = midpoint(lo, hi)

        # sort v[mi] <= v[lo] <= v[hi] such that the pivot is immediately in place
        if lt(o, v[lo], v[mi])
            v[mi], v[lo] = v[lo], v[mi]
        end

        if lt(o, v[hi], v[lo])
            if lt(o, v[hi], v[mi])
                v[hi], v[lo], v[mi] = v[lo], v[mi], v[hi]
            else
                v[hi], v[lo] = v[lo], v[hi]
            end
        end

        # return the pivot
        return v[lo]
    end
end

# partition!
#
# select a pivot, and partition v according to the pivot

function partition!(v::AbstractVector, lo::Integer, hi::Integer, o::Ordering)
    pivot = selectpivot!(v, lo, hi, o)
    # pivot == v[lo], v[hi] > pivot
    i, j = lo, hi
    @inbounds while true
        i += 1; j -= 1
        while lt(o, v[i], pivot); i += 1; end;
        while lt(o, pivot, v[j]); j -= 1; end;
        i >= j && break
        v[i], v[j] = v[j], v[i]
    end
    v[j], v[lo] = pivot, v[j]

    # v[j] == pivot
    # v[k] >= pivot for k > j
    # v[i] <= pivot for i < j
    return j
end

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::QuickSortAlg, o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)
        if j-lo < hi-j
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
            lo < (j-1) && sort!(v, lo, j-1, a, o)
            lo = j+1
        else
            j+1 < hi && sort!(v, j+1, hi, a, o)
            hi = j-1
        end
    end
    return v
end

sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::MergeSortAlg, o::Ordering, t0::Vector{T}) where T =
    invoke(sort!, Tuple{typeof.((v, lo, hi, a, o))..., AbstractVector{T}}, v, lo, hi, a, o, t0) # For disambiguation
function sort!(v::AbstractVector{T}, lo::Integer, hi::Integer, a::MergeSortAlg, o::Ordering,
        t0::Union{AbstractVector{T}, Nothing}=nothing) where T
    @inbounds if lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)

        m = midpoint(lo, hi)

        t = t0 === nothing ? similar(v, m-lo+1) : t0
        length(t) < m-lo+1 && resize!(t, m-lo+1)
        Base.require_one_based_indexing(t)

        sort!(v, lo,  m,  a, o, t)
        sort!(v, m+1, hi, a, o, t)

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

function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::PartialQuickSort,
               o::Ordering)
    @inbounds while lo < hi
        hi-lo <= SMALL_THRESHOLD && return sort!(v, lo, hi, SMALL_ALGORITHM, o)
        j = partition!(v, lo, hi, o)

        if j <= first(a.k)
            lo = j+1
        elseif j >= last(a.k)
            hi = j-1
        else
            # recurse on the smaller chunk
            # this is necessary to preserve O(log(n))
            # stack space in the worst case (rather than O(n))
            if j-lo < hi-j
                lo < (j-1) && sort!(v, lo, j-1, a, o)
                lo = j+1
            else
                hi > (j+1) && sort!(v, j+1, hi, a, o)
                hi = j-1
            end
        end
    end
    return v
end

## Old extensibility mechanisms ##

# Support 3-, 5-, and 6-argument versions of sort! for calling into the internals in the old way
sort!(v::AbstractVector, a::Algorithm, o::Ordering) = sort!(v, firstindex(v), lastindex(v), a, o)
function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::Algorithm, o::Ordering)
    _sort!(v, a, o, (; lo, hi, legacy_dispatch_entry=a))
    v
end
sort!(v::AbstractVector, lo::Integer, hi::Integer, a::Algorithm, o::Ordering, _) = sort!(v, lo, hi, a, o)
function sort!(v::AbstractVector, lo::Integer, hi::Integer, a::Algorithm, o::Ordering, scratch::Vector)
    _sort!(v, a, o, (; lo, hi, scratch, legacy_dispatch_entry=a))
    v
end

# Support dispatch on custom algorithms in the old way
# sort!(::AbstractVector, ::Integer, ::Integer, ::MyCustomAlgorithm, ::Ordering) = ...
function _sort!(v::AbstractVector, a::Algorithm, o::Ordering, kw)
    @getkw lo hi scratch legacy_dispatch_entry
    if legacy_dispatch_entry === a
        # This error prevents infinite recursion for unknown algorithms
        throw(ArgumentError(LazyString("Base.Sort._sort!(::", typeof(v), ", ::", typeof(a), ", ::", typeof(o), ", ::Any) is not defined")))
    else
        sort!(v, lo, hi, a, o)
        scratch
    end
end

end # module Sort

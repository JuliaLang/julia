# This file is a part of Julia. License is MIT: https://julialang.org/license

## Functions to compute the reduced shape

# for reductions that expand 0 dims to 1
reduced_index(i::OneTo{T}) where {T} = OneTo(one(T))
reduced_index(i::Union{Slice, IdentityUnitRange}) = oftype(i, first(i):first(i))
reduced_index(i::AbstractUnitRange) =
    throw(ArgumentError(
"""
No method is implemented for reducing index range of type $(typeof(i)). Please implement
reduced_index for this index type or report this as an issue.
"""
    ))
reduced_indices(a::AbstractArrayOrBroadcasted, region) = reduced_indices(axes(a), region)

# for reductions that keep 0 dims as 0
reduced_indices0(a::AbstractArray, region) = reduced_indices0(axes(a), region)

function reduced_indices(axs::Indices{N}, region) where N
    _check_valid_region(region)
    ntuple(d -> d in region ? reduced_index(axs[d]) : axs[d], Val(N))
end

function reduced_indices0(axs::Indices{N}, region) where N
    _check_valid_region(region)
    ntuple(d -> d in region && !isempty(axs[d]) ? reduced_index(axs[d]) : axs[d], Val(N))
end

# The inverse of reduced_indices
inner_indices(a::AbstractArrayOrBroadcasted, region) = inner_indices(axes(a), region)
function inner_indices(axs::Indices{N}, region) where N
    ntuple(d -> d in region ? axs[d] : reduced_index(axs[d]), Val(N))
end

# Given an outer and an inner cartesian index, merge them depending on the dims
merge_outer_inner(outer::CartesianIndex{N}, inner::CartesianIndex{N}, use_inner::NTuple{N, Bool}) where {N} =
    CartesianIndex(_merge_outer_inner(outer.I, inner.I, use_inner))
_merge_outer_inner(outer::Tuple, inner, use_inner) = (ifelse(use_inner[1], inner[1], outer[1]), _merge_outer_inner(tail(outer), tail(inner), tail(use_inner))...)
_merge_outer_inner(outer::Tuple{}, inner, use_inner) = ()
merge_outer_inner_prep(tup::NTuple{N}, region) where {N} = ntuple(d->d in region, Val(N))

function _check_valid_region(region)
    for d in region
        isa(d, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        Int(d) < 1 && throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
    end
end

###### Generic reduction functions #####

## initialization
# initarray! is only called by sum!, prod!, etc.
for (Op, initfun) in ((:(typeof(add_sum)), :zero), (:(typeof(mul_prod)), :one))
    @eval initarray!(a::AbstractArray{T}, ::Any, ::$(Op), init::Bool, src::AbstractArray) where {T} = (init && fill!(a, $(initfun)(T)); a)
end

initarray!(a::AbstractArray{T}, f, ::Union{typeof(min),typeof(max),typeof(_extrema_rf)},
    init::Bool, src::AbstractArray) where {T} = (init && mapfirst!(f, a, src); a)

for (Op, initval) in ((:(typeof(&)), true), (:(typeof(|)), false))
    @eval initarray!(a::AbstractArray, ::Any, ::$(Op), init::Bool, src::AbstractArray) = (init && fill!(a, $initval); a)
end

# reducedim_initarray is called by
reducedim_initarray(A::AbstractArrayOrBroadcasted, region, init, ::Type{R}) where {R} = fill!(similar(A,R,reduced_indices(A,region)), init)
reducedim_initarray(A::AbstractArrayOrBroadcasted, region, init::T) where {T} = reducedim_initarray(A, region, init, T)

promote_union(T::Union) = promote_type(promote_union(T.a), promote_union(T.b))
promote_union(T) = T

_realtype(::Type{<:Complex}) = Real
_realtype(::Type{Complex{T}}) where T<:Real = T
_realtype(T::Type) = T
_realtype(::Union{typeof(abs),typeof(abs2)}, T) = _realtype(T)
_realtype(::Any, T) = T

## generic (map)reduction

has_fast_linear_indexing(a::AbstractArrayOrBroadcasted) = IndexStyle(a) === IndexLinear()
has_fast_linear_indexing(a::AbstractVector) = true

check_reducedims(R, A) = check_reducedims_axes(axes(R), axes(A))
function check_reducedims_axes(Rax, Aax)
    # Check whether R has compatible dimensions w.r.t. A for reduction
    #
    # It returns an integer value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, dims=1) or sum(A, dims=(1,2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, dims=2) or sum(A, dims=(1,3)), it returns 0.
    #
    length(Rax) <= length(Aax) || throw(DimensionMismatch("cannot reduce $(length(Aax))-dimensional array to $(length(Rax)) dimensions"))
    lsiz = 1
    had_nonreduc = false
    for i = 1:length(Aax)
        Ri, Ai = get(Rax, i, OneTo(1)), Aax[i]
        sRi, sAi = length(Ri), length(Ai)
        if sRi == 1
            if sAi > 1
                if had_nonreduc
                    lsiz = 0  # to reduce along i, but some previous dimensions were non-reducing
                else
                    lsiz *= sAi  # if lsiz was set to zero, it will stay to be zero
                end
            end
        else
            Ri == Ai || throw(DimensionMismatch("reduction on array with indices $(Aax) with output with indices $(Rax)"))
            had_nonreduc = true
        end
    end
    return lsiz
end

"""
Extract first entry of slices of array A into existing array R.
"""
copyfirst!(R::AbstractArray, A::AbstractArray) = mapfirst!(identity, R, A)

function mapfirst!(f::F, R::AbstractArray, A::AbstractArray{<:Any,N}) where {N, F}
    lsiz = check_reducedims(R, A)
    t = _firstreducedslice(axes(R), axes(A))
    map!(f, R, view(A, t...))
end
# We know that the axes of R and A are compatible, but R might have a different number of
# dimensions than A, which is trickier than it seems due to offset arrays and type stability
_firstreducedslice(::Tuple{}, a::Tuple{}) = ()
_firstreducedslice(::Tuple, ::Tuple{}) = ()
@inline _firstreducedslice(::Tuple{}, a::Tuple) = (_firstslice(a[1]), _firstreducedslice((), tail(a))...)
@inline _firstreducedslice(r::Tuple, a::Tuple) = (length(r[1])==1 ? _firstslice(a[1]) : r[1], _firstreducedslice(tail(r), tail(a))...)
_firstslice(i::OneTo) = OneTo(1)
_firstslice(i::Slice) = Slice(_firstslice(i.indices))
_firstslice(i) = i[firstindex(i):firstindex(i)]

function _mapreducedim!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted)
    lsiz = check_reducedims(R,A)
    isempty(A) && return R

    if has_fast_linear_indexing(A) && lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        ibase = first(LinearIndices(A))-1
        for i in eachindex(R) # TODO: add tests for this change
            @inbounds R[i] = op(R[i], mapreduce_impl(f, op, A, ibase+1, ibase+lsiz))
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = safe_tail(axes(A)), safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    if reducedim1(R, A)
        # keep the accumulator as a local variable when reducing along the first dimension
        i1 = first(axes1(R))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            @simd for i in axes(A, 1)
                r = op(r, f(A[i, IA])) # this could also be done pairwise
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in axes(A, 1)
                R[i,IR] = op(R[i,IR], f(A[i,IA]))
            end
        end
    end
    return R
end

#### Reduction initialization ####
#
# Similar to the scalar mapreduce, there are two*three cases we need to consider
# when constructing the initial results array:
# * Have init: Fill results array with init, proceed (regardless of reduction size)
#   * Don't worry about widening and do a straight iteration with in-place updates over all values
# * Otherwise:
#   * No values: Fill with `mapreduce_empty` & return
#   * One value: Fill with `mapreduce_first` & return
#   * Two+ values: Fill with `op(f(a1), f(a2))`, proceed <--- this is the hard case:
#       * Proceeding involves skipping the first two inner iterations
function _mapreduce_dim(f, op, ::_InitialValue, A::AbstractArrayOrBroadcasted, dims)
    rinds = reduced_indices(A, dims)
    lsiz = check_reducedims_axes(rinds, axes(A))
    outer_inds = CartesianIndices(rinds)
    inner_inds = CartesianIndices(inner_indices(A, dims))
    n = length(inner_inds)
    (n == 0 || isempty(A)) && return _mapreduce_empty_array(f, op, A, rinds)
    n == 1 && return _mapreduce_one_array(f, op, A, rinds)

    inner1, inner2 = inner_inds
    merger = merge_outer_inner_prep(rinds, dims)
    # Create the result vector with the first step of the reduction
    # note that this is using a (potentially) bad cache ordering, so we don't want to continue like this
    i1 = first(outer_inds)
    v = op(f(A[merge_outer_inner(i1, inner1, merger)]), f(A[merge_outer_inner(i1, inner2, merger)]))
    R = _mapreduce_similar(f, op, A, typeof(v), rinds)
    R[begin] = v
    for i in Iterators.drop(outer_inds, 1)
        v = op(f(A[merge_outer_inner(i, inner1, merger)]), f(A[merge_outer_inner(i, inner2, merger)]))
        if !(typeof(v) <: eltype(R))
            Rnew = _mapreduce_similar(f, op, A, promote_type(eltype(R), typeof(v)), rinds)
            copyto!(Rnew, R) # TODO: could only copy from begin:i
            R = Rnew
        end
        R[i] = v
    end

    if has_fast_linear_indexing(A) && lsiz > 0
        ibase = first(LinearIndices(A))-1
        if lsiz > 16
            # use mapreduce_impl, which is better tuned to achieve higher performance and works pairwise
            for i in eachindex(R)
                @inbounds R[i] = op(R[i], mapreduce_impl(f, op, A, ibase+3, ibase+lsiz))
                ibase += lsiz
            end
        else
            for i in eachindex(R)
                @inbounds r = R[i]
                @simd for Ai in ibase+3:ibase+lsiz
                    r = op(r, f(@inbounds(A[Ai])))
                end
                @inbounds R[i] = r
                ibase += lsiz
            end
        end
    else
        # Dynamically switch loops to prioritize the largest first dimension
        if something(findfirst((>)(1), size(inner_inds))) < something(findfirst((>)(1), size(outer_inds)), typemax(Int))
            for IR in outer_inds
                @inbounds r = R[IR]
                for IA in Iterators.drop(inner_inds, 2)
                    iA = merge_outer_inner(IR, IA, merger)
                    r = op(r, f(@inbounds(A[iA])))
                end
                @inbounds R[IR] = r
            end
        else
            for IA in Iterators.drop(inner_inds, 2)
                @simd for IR in outer_inds
                    iA = merge_outer_inner(IR, IA, merger)
                    v = op(@inbounds(R[IR]), f(@inbounds(A[iA])))
                    @inbounds R[IR] = v
                end
            end
        end
    end
    return R
end

_mapreduce_similar(f, op, A, ::Type{T}, axes) where {T} = similar(A,T,axes)
# Extrema, max and min have special support for preserving the original matrix eltype with identity maps
function _mapreduce_similar(f::ExtremaMap{typeof(identity)}, op::typeof(_extrema_rf), A, ::Type{Tuple{T,T}}, axes) where {T}
    similar(A,Tuple{promote_type(eltype(A),T),promote_type(eltype(A),T)},axes)
end
function _mapreduce_similar(f::typeof(identity), op::Union{typeof(min), typeof(max)}, A, ::Type{T}, axes) where {T}
    similar(A,promote_type(eltype(A),T),axes)
end

function _mapreduce_empty_array(f, op, A, axes)
    init = mapreduce_empty(f, op, eltype(A))
    return fill!(_mapreduce_similar(f, op, A, typeof(init), axes), init)
end

function _mapreduce_one_array(f, op, A, axes)
    idxs = CartesianIndices(axes)
    i1 = first(idxs)
    v = mapreduce_first(f, op, A[i1])
    R = _mapreduce_similar(f, op, A, typeof(v), axes)
    R[i1] = v
    for i in Iterators.drop(idxs, 1)
        v = mapreduce_first(f, op, A[i])
        if !(typeof(v) <: eltype(R))
            Rnew = _mapreduce_similar(f, op, A, promote_type(eltype(R), typeof(v)), axes)
            copyto!(Rnew, R)
            R = Rnew
        end
        R[i] = v
    end
    return R
end

mapreducedim!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted) =
    (_mapreducedim!(f, op, R, A); R)

reducedim!(op, R::AbstractArray{RT}, A::AbstractArrayOrBroadcasted) where {RT} =
    mapreducedim!(identity, op, R, A)

"""
    mapreduce(f, op, A::AbstractArray...; dims=:, [init])

Evaluates to the same as `reduce(op, map(f, A...); dims=dims, init=init)`, but is generally
faster because the intermediate array is avoided.

!!! compat "Julia 1.2"
    `mapreduce` with multiple iterators requires Julia 1.2 or later.

# Examples
```jldoctest
julia> a = reshape(Vector(1:16), (4,4))
4×4 Matrix{Int64}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> mapreduce(isodd, *, a, dims=1)
1×4 Matrix{Bool}:
 0  0  0  0

julia> mapreduce(isodd, |, a, dims=1)
1×4 Matrix{Bool}:
 1  1  1  1
```
"""
mapreduce(f, op, A::AbstractArrayOrBroadcasted; dims=:, init=_InitialValue()) =
    _mapreduce_dim(f, op, init, A, dims)
mapreduce(f, op, A::AbstractArrayOrBroadcasted, B::AbstractArrayOrBroadcasted...; kw...) =
    reduce(op, map(f, A, B...); kw...)

_mapreduce_dim(f, op, nt, A::AbstractArrayOrBroadcasted, ::Colon) =
    mapfoldl_impl(f, op, nt, A)

_mapreduce_dim(f, op, ::_InitialValue, A::AbstractArrayOrBroadcasted, ::Colon) =
    _mapreduce(f, op, IndexStyle(A), A)

_mapreduce_dim(f, op, nt, A::AbstractArrayOrBroadcasted, dims) =
    mapreducedim!(f, op, reducedim_initarray(A, dims, nt), A)

"""
    reduce(f, A::AbstractArray; dims=:, [init])

Reduce 2-argument function `f` along dimensions of `A`. `dims` is a vector specifying the
dimensions to reduce, and the keyword argument `init` is the initial value to use in the
reductions. For `+`, `*`, `max` and `min` the `init` argument is optional.

The associativity of the reduction is implementation-dependent; if you need a particular
associativity, e.g. left-to-right, you should write your own loop or consider using
[`foldl`](@ref) or [`foldr`](@ref). See documentation for [`reduce`](@ref).

# Examples
```jldoctest
julia> a = reshape(Vector(1:16), (4,4))
4×4 Matrix{Int64}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> reduce(max, a, dims=2)
4×1 Matrix{Int64}:
 13
 14
 15
 16

julia> reduce(max, a, dims=1)
1×4 Matrix{Int64}:
 4  8  12  16
```
"""
reduce(op, A::AbstractArray; kw...) = mapreduce(identity, op, A; kw...)

##### Specific reduction functions #####

"""
    count([f=identity,] A::AbstractArray; dims=:)

Count the number of elements in `A` for which `f` returns `true` over the given
dimensions.

!!! compat "Julia 1.5"
    `dims` keyword was added in Julia 1.5.

!!! compat "Julia 1.6"
    `init` keyword was added in Julia 1.6.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> count(<=(2), A, dims=1)
1×2 Matrix{Int64}:
 1  1

julia> count(<=(2), A, dims=2)
2×1 Matrix{Int64}:
 2
 0
```
"""
count(A::AbstractArrayOrBroadcasted; dims=:, init=0) = count(identity, A; dims, init)
count(f, A::AbstractArrayOrBroadcasted; dims=:, init=0) = _count(f, A, dims, init)

_count(f, A::AbstractArrayOrBroadcasted, dims::Colon, init) = _simple_count(f, A, init)
_count(f, A::AbstractArrayOrBroadcasted, dims, init) = mapreduce(_bool(f), add_sum, A; dims, init)

"""
    count!([f=identity,] r, A)

Count the number of elements in `A` for which `f` returns `true` over the
singleton dimensions of `r`, writing the result into `r` in-place.

$(_DOCS_ALIASING_WARNING)

!!! compat "Julia 1.5"
    inplace `count!` was added in Julia 1.5.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> count!(<=(2), [1 1], A)
1×2 Matrix{Int64}:
 1  1

julia> count!(<=(2), [1; 1], A)
2-element Vector{Int64}:
 2
 0
```
"""
count!(r::AbstractArray, A::AbstractArrayOrBroadcasted; init::Bool=true) = count!(identity, r, A; init=init)
count!(f, r::AbstractArray, A::AbstractArrayOrBroadcasted; init::Bool=true) =
    mapreducedim!(_bool(f), add_sum, initarray!(r, f, add_sum, init, A), A)

"""
    sum(A::AbstractArray; dims)

Sum elements of an array over the given dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> sum(A, dims=1)
1×2 Matrix{Int64}:
 4  6

julia> sum(A, dims=2)
2×1 Matrix{Int64}:
 3
 7
```
"""
sum(A::AbstractArray; dims)

"""
    sum(f, A::AbstractArray; dims)

Sum the results of calling function `f` on each element of an array over the given
dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> sum(abs2, A, dims=1)
1×2 Matrix{Int64}:
 10  20

julia> sum(abs2, A, dims=2)
2×1 Matrix{Int64}:
  5
 25
```
"""
sum(f, A::AbstractArray; dims)

"""
    sum!(r, A)

Sum elements of `A` over the singleton dimensions of `r`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> sum!([1; 1], A)
2-element Vector{Int64}:
 3
 7

julia> sum!([1 1], A)
1×2 Matrix{Int64}:
 4  6
```
"""
sum!(r, A)

"""
    prod(A::AbstractArray; dims)

Multiply elements of an array over the given dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> prod(A, dims=1)
1×2 Matrix{Int64}:
 3  8

julia> prod(A, dims=2)
2×1 Matrix{Int64}:
  2
 12
```
"""
prod(A::AbstractArray; dims)

"""
    prod(f, A::AbstractArray; dims)

Multiply the results of calling the function `f` on each element of an array over the given
dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> prod(abs2, A, dims=1)
1×2 Matrix{Int64}:
 9  64

julia> prod(abs2, A, dims=2)
2×1 Matrix{Int64}:
   4
 144
```
"""
prod(f, A::AbstractArray; dims)

"""
    prod!(r, A)

Multiply elements of `A` over the singleton dimensions of `r`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> prod!([1; 1], A)
2-element Vector{Int64}:
  2
 12

julia> prod!([1 1], A)
1×2 Matrix{Int64}:
 3  8
```
"""
prod!(r, A)

"""
    maximum(A::AbstractArray; dims)

Compute the maximum value of an array over the given dimensions. See also the
[`max(a,b)`](@ref) function to take the maximum of two or more arguments,
which can be applied elementwise to arrays via `max.(a,b)`.

See also: [`maximum!`](@ref), [`extrema`](@ref), [`findmax`](@ref), [`argmax`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> maximum(A, dims=1)
1×2 Matrix{Int64}:
 3  4

julia> maximum(A, dims=2)
2×1 Matrix{Int64}:
 2
 4
```
"""
maximum(A::AbstractArray; dims)

"""
    maximum(f, A::AbstractArray; dims)

Compute the maximum value by calling the function `f` on each element of an array over the given
dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> maximum(abs2, A, dims=1)
1×2 Matrix{Int64}:
 9  16

julia> maximum(abs2, A, dims=2)
2×1 Matrix{Int64}:
  4
 16
```
"""
maximum(f, A::AbstractArray; dims)

"""
    maximum!(r, A)

Compute the maximum value of `A` over the singleton dimensions of `r`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> maximum!([1; 1], A)
2-element Vector{Int64}:
 2
 4

julia> maximum!([1 1], A)
1×2 Matrix{Int64}:
 3  4
```
"""
maximum!(r, A)

"""
    minimum(A::AbstractArray; dims)

Compute the minimum value of an array over the given dimensions. See also the
[`min(a,b)`](@ref) function to take the minimum of two or more arguments,
which can be applied elementwise to arrays via `min.(a,b)`.

See also: [`minimum!`](@ref), [`extrema`](@ref), [`findmin`](@ref), [`argmin`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> minimum(A, dims=1)
1×2 Matrix{Int64}:
 1  2

julia> minimum(A, dims=2)
2×1 Matrix{Int64}:
 1
 3
```
"""
minimum(A::AbstractArray; dims)

"""
    minimum(f, A::AbstractArray; dims)

Compute the minimum value by calling the function `f` on each element of an array over the given
dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> minimum(abs2, A, dims=1)
1×2 Matrix{Int64}:
 1  4

julia> minimum(abs2, A, dims=2)
2×1 Matrix{Int64}:
 1
 9
```
"""
minimum(f, A::AbstractArray; dims)

"""
    minimum!(r, A)

Compute the minimum value of `A` over the singleton dimensions of `r`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> minimum!([1; 1], A)
2-element Vector{Int64}:
 1
 3

julia> minimum!([1 1], A)
1×2 Matrix{Int64}:
 1  2
```
"""
minimum!(r, A)

"""
    extrema(A::AbstractArray; dims) -> Array{Tuple}

Compute the minimum and maximum elements of an array over the given dimensions.

See also: [`minimum`](@ref), [`maximum`](@ref), [`extrema!`](@ref).

# Examples
```jldoctest
julia> A = reshape(Vector(1:2:16), (2,2,2))
2×2×2 Array{Int64, 3}:
[:, :, 1] =
 1  5
 3  7

[:, :, 2] =
  9  13
 11  15

julia> extrema(A, dims = (1,2))
1×1×2 Array{Tuple{Int64, Int64}, 3}:
[:, :, 1] =
 (1, 7)

[:, :, 2] =
 (9, 15)
```
"""
extrema(A::AbstractArray; dims)

"""
    extrema(f, A::AbstractArray; dims) -> Array{Tuple}

Compute the minimum and maximum of `f` applied to each element in the given dimensions
of `A`.

!!! compat "Julia 1.2"
    This method requires Julia 1.2 or later.
"""
extrema(f, A::AbstractArray; dims)

"""
    extrema!(r, A)

Compute the minimum and maximum value of `A` over the singleton dimensions of `r`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

!!! compat "Julia 1.8"
    This method requires Julia 1.8 or later.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> extrema!([(1, 1); (1, 1)], A)
2-element Vector{Tuple{Int64, Int64}}:
 (1, 2)
 (3, 4)

julia> extrema!([(1, 1);; (1, 1)], A)
1×2 Matrix{Tuple{Int64, Int64}}:
 (1, 3)  (2, 4)
```
"""
extrema!(r, A)

"""
    all(A; dims)

Test whether all values along the given dimensions of an array are `true`.

# Examples
```jldoctest
julia> A = [true false; true true]
2×2 Matrix{Bool}:
 1  0
 1  1

julia> all(A, dims=1)
1×2 Matrix{Bool}:
 1  0

julia> all(A, dims=2)
2×1 Matrix{Bool}:
 0
 1
```
"""
all(A::AbstractArray; dims)

"""
    all(p, A; dims)

Determine whether predicate `p` returns `true` for all elements along the given dimensions of an array.

# Examples
```jldoctest
julia> A = [1 -1; 2 2]
2×2 Matrix{Int64}:
 1  -1
 2   2

julia> all(i -> i > 0, A, dims=1)
1×2 Matrix{Bool}:
 1  0

julia> all(i -> i > 0, A, dims=2)
2×1 Matrix{Bool}:
 0
 1
```
"""
all(::Function, ::AbstractArray; dims)

"""
    all!(r, A)

Test whether all values in `A` along the singleton dimensions of `r` are `true`, and write results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Matrix{Bool}:
 1  0
 1  0

julia> all!([1; 1], A)
2-element Vector{Int64}:
 0
 0

julia> all!([1 1], A)
1×2 Matrix{Int64}:
 1  0
```
"""
all!(r, A)

"""
    any(A; dims)

Test whether any values along the given dimensions of an array are `true`.

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Matrix{Bool}:
 1  0
 1  0

julia> any(A, dims=1)
1×2 Matrix{Bool}:
 1  0

julia> any(A, dims=2)
2×1 Matrix{Bool}:
 1
 1
```
"""
any(::AbstractArray; dims)

"""
    any(p, A; dims)

Determine whether predicate `p` returns `true` for any elements along the given dimensions of an array.

# Examples
```jldoctest
julia> A = [1 -1; 2 -2]
2×2 Matrix{Int64}:
 1  -1
 2  -2

julia> any(i -> i > 0, A, dims=1)
1×2 Matrix{Bool}:
 1  0

julia> any(i -> i > 0, A, dims=2)
2×1 Matrix{Bool}:
 1
 1
```
"""
any(::Function, ::AbstractArray; dims)

"""
    any!(r, A)

Test whether any values in `A` along the singleton dimensions of `r` are `true`, and write
results to `r`.

$(_DOCS_ALIASING_WARNING)

# Examples
```jldoctest
julia> A = [true false; true false]
2×2 Matrix{Bool}:
 1  0
 1  0

julia> any!([1; 1], A)
2-element Vector{Int64}:
 1
 1

julia> any!([1 1], A)
1×2 Matrix{Int64}:
 1  0
```
"""
any!(r, A)

for (fname, _fname, op) in [(:sum,     :_sum,     :add_sum), (:prod,    :_prod,    :mul_prod),
                            (:maximum, :_maximum, :max),     (:minimum, :_minimum, :min),
                            (:extrema, :_extrema, :_extrema_rf)]
    mapf = fname === :extrema ? :(ExtremaMap(f)) : :f
    @eval begin
        # User-facing methods with keyword arguments
        @inline ($fname)(a::AbstractArray; dims=:, kw...) = ($_fname)(a, dims; kw...)
        @inline ($fname)(f, a::AbstractArray; dims=:, kw...) = ($_fname)(f, a, dims; kw...)

        # Underlying implementations using dispatch
        ($_fname)(a, ::Colon; kw...) = ($_fname)(identity, a, :; kw...)
        ($_fname)(f, a, ::Colon; kw...) = mapreduce($mapf, $op, a; kw...)
    end
end

any(a::AbstractArray; dims=:)              = _any(a, dims)
any(f::Function, a::AbstractArray; dims=:) = _any(f, a, dims)
_any(a, ::Colon)                           = _any(identity, a, :)
all(a::AbstractArray; dims=:)              = _all(a, dims)
all(f::Function, a::AbstractArray; dims=:) = _all(f, a, dims)
_all(a, ::Colon)                           = _all(identity, a, :)

for (fname, op) in [(:sum, :add_sum), (:prod, :mul_prod),
                    (:maximum, :max), (:minimum, :min),
                    (:all, :&),       (:any, :|),
                    (:extrema, :_extrema_rf)]
    fname! = Symbol(fname, '!')
    _fname = Symbol('_', fname)
    mapf = fname === :extrema ? :(ExtremaMap(f)) : :f
    @eval begin
        $(fname!)(f::Function, r::AbstractArray, A::AbstractArray; init::Bool=true) =
            mapreducedim!($mapf, $(op), initarray!(r, $mapf, $(op), init, A), A)
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) = $(fname!)(identity, r, A; init=init)

        $(_fname)(A, dims; kw...)    = $(_fname)(identity, A, dims; kw...)
        $(_fname)(f, A, dims; kw...) = mapreduce($mapf, $(op), A; dims=dims, kw...)
    end
end

##### findmin & findmax #####
# The initial values of Rval are not used if the corresponding indices in Rind are 0.
#
function findminmax!(f, op, Rval, Rind, A::AbstractArray{T,N}) where {T,N}
    (isempty(Rval) || isempty(A)) && return Rval, Rind
    lsiz = check_reducedims(Rval, A)
    for i = 1:N
        axes(Rval, i) == axes(Rind, i) || throw(DimensionMismatch("Find-reduction: outputs must have the same indices"))
    end
    # If we're reducing along dimension 1, for efficiency we can make use of a temporary.
    # Otherwise, keep the result in Rval/Rind so that we traverse A in storage order.
    indsAt, indsRt = safe_tail(axes(A)), safe_tail(axes(Rval))
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    ks = keys(A)
    y = iterate(ks)
    zi = zero(eltype(ks))
    if reducedim1(Rval, A)
        i1 = first(axes1(Rval))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            tmpRv = Rval[i1,IR]
            tmpRi = Rind[i1,IR]
            for i in axes(A,1)
                k, kss = y::Tuple
                tmpAv = f(A[i,IA])
                if tmpRi == zi || op(tmpRv, tmpAv)
                    tmpRv = tmpAv
                    tmpRi = k
                end
                y = iterate(ks, kss)
            end
            Rval[i1,IR] = tmpRv
            Rind[i1,IR] = tmpRi
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            for i in axes(A, 1)
                k, kss = y::Tuple
                tmpAv = f(A[i,IA])
                tmpRv = Rval[i,IR]
                tmpRi = Rind[i,IR]
                if tmpRi == zi || op(tmpRv, tmpAv)
                    Rval[i,IR] = tmpAv
                    Rind[i,IR] = k
                end
                y = iterate(ks, kss)
            end
        end
    end
    Rval, Rind
end

"""
    findmin!(rval, rind, A) -> (minval, index)

Find the minimum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as less than all other values except `missing`.

$(_DOCS_ALIASING_WARNING)
"""
function findmin!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    findminmax!(identity, isgreater, init && !isempty(A) ? fill!(rval, first(A)) : rval, fill!(rind,zero(eltype(keys(A)))), A)
end

"""
    findmin(A; dims) -> (minval, index)

For an array input, returns the value and index of the minimum over the given dimensions.
`NaN` is treated as less than all other values except `missing`.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> findmin(A, dims=1)
([1.0 2.0], CartesianIndex{2}[CartesianIndex(1, 1) CartesianIndex(1, 2)])

julia> findmin(A, dims=2)
([1.0; 3.0;;], CartesianIndex{2}[CartesianIndex(1, 1); CartesianIndex(2, 1);;])
```
"""
findmin(A::AbstractArray; dims=:) = _findmin(A, dims)
_findmin(A, dims) = _findmin(identity, A, dims)

"""
    findmin(f, A; dims) -> (f(x), index)

For an array input, returns the value in the codomain and index of the corresponding value
which minimize `f` over the given dimensions.

# Examples
```jldoctest
julia> A = [-1.0 1; -0.5 2]
2×2 Matrix{Float64}:
 -1.0  1.0
 -0.5  2.0

julia> findmin(abs2, A, dims=1)
([0.25 1.0], CartesianIndex{2}[CartesianIndex(2, 1) CartesianIndex(1, 2)])

julia> findmin(abs2, A, dims=2)
([1.0; 0.25;;], CartesianIndex{2}[CartesianIndex(1, 1); CartesianIndex(2, 1);;])
```
"""
findmin(f, A::AbstractArray; dims=:) = _findmin(f, A, dims)

function _findmin(f, A, region)
    ri = reduced_indices0(A, region)
    if isempty(A)
        if prod(map(length, reduced_indices(A, region))) != 0
            throw(ArgumentError("collection slices must be non-empty"))
        end
        similar(A, promote_op(f, eltype(A)), ri), zeros(eltype(keys(A)), ri)
    else
        fA = f(first(A))
        findminmax!(f, isgreater, fill!(similar(A, _findminmax_inittype(f, A), ri), fA),
                    zeros(eltype(keys(A)), ri), A)
    end
end

"""
    findmax!(rval, rind, A) -> (maxval, index)

Find the maximum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as greater than all other values except `missing`.

$(_DOCS_ALIASING_WARNING)
"""
function findmax!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    findminmax!(identity, isless, init && !isempty(A) ? fill!(rval, first(A)) : rval, fill!(rind,zero(eltype(keys(A)))), A)
end

"""
    findmax(A; dims) -> (maxval, index)

For an array input, returns the value and index of the maximum over the given dimensions.
`NaN` is treated as greater than all other values except `missing`.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> findmax(A, dims=1)
([3.0 4.0], CartesianIndex{2}[CartesianIndex(2, 1) CartesianIndex(2, 2)])

julia> findmax(A, dims=2)
([2.0; 4.0;;], CartesianIndex{2}[CartesianIndex(1, 2); CartesianIndex(2, 2);;])
```
"""
findmax(A::AbstractArray; dims=:) = _findmax(A, dims)
_findmax(A, dims) = _findmax(identity, A, dims)

"""
    findmax(f, A; dims) -> (f(x), index)

For an array input, returns the value in the codomain and index of the corresponding value
which maximize `f` over the given dimensions.

# Examples
```jldoctest
julia> A = [-1.0 1; -0.5 2]
2×2 Matrix{Float64}:
 -1.0  1.0
 -0.5  2.0

julia> findmax(abs2, A, dims=1)
([1.0 4.0], CartesianIndex{2}[CartesianIndex(1, 1) CartesianIndex(2, 2)])

julia> findmax(abs2, A, dims=2)
([1.0; 4.0;;], CartesianIndex{2}[CartesianIndex(1, 1); CartesianIndex(2, 2);;])
```
"""
findmax(f, A::AbstractArray; dims=:) = _findmax(f, A, dims)

function _findmax(f, A, region)
    ri = reduced_indices0(A, region)
    if isempty(A)
        if prod(map(length, reduced_indices(A, region))) != 0
            throw(ArgumentError("collection slices must be non-empty"))
        end
        similar(A, promote_op(f, eltype(A)), ri), zeros(eltype(keys(A)), ri)
    else
        fA = f(first(A))
        findminmax!(f, isless, fill!(similar(A, _findminmax_inittype(f, A), ri), fA),
                    zeros(eltype(keys(A)), ri), A)
    end
end

function _findminmax_inittype(f, A::AbstractArray)
    T = _realtype(f, promote_union(eltype(A)))
    v0 = f(first(A))
    # First conditional: T is >: typeof(v0), so return it
    # Second conditional: handle missing specifically, as most often, f(missing) = missing;
    # certainly, some predicate functions return Bool, but not all.
    # Else, return the type of the transformation.
    Tr = v0 isa T ? T : Missing <: eltype(A) ? Union{Missing, typeof(v0)} : typeof(v0)
end

reducedim1(R, A) = length(axes1(R)) == 1

"""
    argmin(A; dims) -> indices

For an array input, return the indices of the minimum elements over the given dimensions.
`NaN` is treated as less than all other values except `missing`.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> argmin(A, dims=1)
1×2 Matrix{CartesianIndex{2}}:
 CartesianIndex(1, 1)  CartesianIndex(1, 2)

julia> argmin(A, dims=2)
2×1 Matrix{CartesianIndex{2}}:
 CartesianIndex(1, 1)
 CartesianIndex(2, 1)
```
"""
argmin(A::AbstractArray; dims=:) = findmin(A; dims=dims)[2]

"""
    argmax(A; dims) -> indices

For an array input, return the indices of the maximum elements over the given dimensions.
`NaN` is treated as greater than all other values except `missing`.

# Examples
```jldoctest
julia> A = [1.0 2; 3 4]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> argmax(A, dims=1)
1×2 Matrix{CartesianIndex{2}}:
 CartesianIndex(2, 1)  CartesianIndex(2, 2)

julia> argmax(A, dims=2)
2×1 Matrix{CartesianIndex{2}}:
 CartesianIndex(1, 2)
 CartesianIndex(2, 2)
```
"""
argmax(A::AbstractArray; dims=:) = findmax(A; dims=dims)[2]

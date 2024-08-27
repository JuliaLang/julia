# This file is a part of Julia. License is MIT: https://julialang.org/license

## Functions to compute the reduced shape

# for reductions that don't reduce over a zero-length dimension
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
reduced_indices_array(a::AbstractArrayOrBroadcasted, r::AbstractArray) = reduced_indices_axes(axes(a), axes(r), a, r)

# for reductions that keep 0-length reduced dims as 0-length
reduced_indices0(a::AbstractArray, region) = reduced_indices0(axes(a), region)
reduced_indices0(a::AbstractArray, r::AbstractArray) = reduced_indices(a, r)

function reduced_indices_axes(axs::Tuple, raxs::Tuple, a, r)
    length(raxs[1]) in (1,length(axs[1])) || throw(DimensionMismatch("cannot reduce $(size(a)) array into an array of $(size(r))"))
    (raxs[1], reduced_indices_axes(tail(axs), tail(raxs), a, r)...)
end
reduced_indices_axes(axs::Tuple{}, raxs::Tuple{}, a, r) = ()
reduced_indices_axes(axs::Tuple{}, raxs::Tuple, a, r) = throw(DimensionMismatch("cannot reduce $(size(a)) array into an array of $(size(r))"))
reduced_indices_axes(axs::Tuple, raxs::Tuple{}, a, r) = (reduced_index(axs[1]), reduced_indices_axes(tail(axs), (), a, r)...)

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
sliceall(x) = @inbounds x[begin:end]
sliceat(x, y) = @inbounds x[y:y]

mergeindices(b, x::CartesianIndices, y::CartesianIndices) = CartesianIndices(map((b,x,y)->ifelse(b, x, y), b, x.indices, y.indices))
mergeindices(b, x::CartesianIndex, y::CartesianIndices) = mergeindices(b, CartesianIndices(map(sliceat, y.indices, x.I)), CartesianIndices(map(sliceall, y.indices)))
mergeindices(b, x::CartesianIndex, y::CartesianIndex) = CartesianIndex(map((b,x,y)->ifelse(b, x, y), b, x.I, y.I))

function _check_valid_region(region)
    for d in region
        isa(d, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        Int(d) < 1 && throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
    end
end

reducedim1(R, A) = length(axes1(R)) == 1

## generic (map)reduction

has_fast_linear_indexing(a::AbstractArrayOrBroadcasted) = IndexStyle(a) === IndexLinear()
has_fast_linear_indexing(a::AbstractVector) = true

_linear_reduction_length(A, Rax) = _linear_reduction_length(IndexStyle(A), A, Rax)
_linear_reduction_length(_, _, _) = 0
function _linear_reduction_length(::IndexLinear, A, Rax)
    # It returns an integer value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, dims=1) or sum(A, dims=(1,2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, dims=2) or sum(A, dims=(1,3)), it returns 0.
    #
    Aax = axes(A)
    lsiz = 1
    for i = 1:length(Aax)
        Ri, Ai = get(Rax, i, OneTo(1)), Aax[i]
        sRi, sAi = length(Ri), length(Ai)
        if sRi == 1
            lsiz *= sAi
        else
            return 0
        end
    end
    return lsiz
end

# !!! this internal function assumes that R is valid for use as an operand to `op`, and only
#     visits values in A within the shape passed in `Aax`.
function _mapreducedim!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted, Aax=axes(A))
    isempty(A) && return R
    lsiz = _linear_reduction_length(A, axes(R))

    if has_fast_linear_indexing(A) && lsiz > 16 && axes(A) === Aax
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        ibase = first(LinearIndices(A))-1
        for i in eachindex(R) # TODO: add tests for this change
            @inbounds R[i] = op(R[i], mapreduce_impl(f, op, A, ibase+1, ibase+lsiz))
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = safe_tail(Aax), safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    if reducedim1(R, A)
        # keep the accumulator as a local variable when reducing along the first dimension
        i1 = first(axes1(R))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            @simd for i in Aax[1]
                r = op(r, f(A[i, IA])) # this could also be done pairwise
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in Aax[1]
                R[i,IR] = op(R[i,IR], f(A[i,IA]))
            end
        end
    end
    return R
end

if isdefined(Core, :Compiler)
    _mapreduce_might_widen(f::F, op::G, ::Type{T}, out) where {F,G,T} = false
    function _mapreduce_might_widen(f::F, op::G, ::Type{T}, ::Nothing) where {F,G,T}
        return !isconcretetype(Core.Compiler.return_type(x->op(f(first(x)), f(first(x))), Tuple{T}))
    end
else
    _mapreduce_might_widen(_, _, _, ::Nothing) = true
    _mapreduce_might_widen(_, _, _, _) = false
end

mapreduce_similar(A, ::Type{T}, dims) where {T} = similar(A, T, dims)
function _mapreduce_similar_curried(A)
    return function(::Type{T}, dims) where {T}
        mapreduce_similar(A, T, dims)
    end
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
_mapreducedim_impl(f::Type{F}, op, init, A::AbstractArrayOrBroadcasted, raxs, out) where {F} =
    _mapreducedim_impl(x->F(x), op, init, A::AbstractArrayOrBroadcasted, raxs, out)
function _mapreducedim_impl(f, op, init, A::AbstractArrayOrBroadcasted, raxs, out)
    # @show f, op, init, A, raxs, out
    outer_inds = CartesianIndices(raxs)
    a_inds = CartesianIndices(axes(A))
    Aaxs = axes(A)
    @assert length(Aaxs) == length(raxs)
    # The "outer dimension" of the reduction are the preserved dimensions
    is_outer_dim = map((>(1))∘length, raxs)
    n = prod(map((b, ax)->ifelse(b, 1, length(ax)), is_outer_dim, Aaxs))

    # We have special support for returning empty arrays
    # isempty(outer_inds) && return collect_allocator(_mapreduce_similar_curried(A),
    #     (_mapreduce_naive_inner_loop(f, op, init, A, is_outer_dim, outer_ind, a_inds) for outer_ind in outer_inds))
    (n == 0 || isempty(A)) && return _mapreduce_empty_array(f, op, init, A, raxs, out)
    n == 1 && return _mapreduce_one_array(f, op, init, A, raxs, out)

    _mapreduce_might_widen(f, op, typeof(A), out) && return collect_allocator(_mapreduce_similar_curried(A),
        (_mapreduce_naive_inner_loop(f, op, init, A, is_outer_dim, outer_ind, a_inds) for outer_ind in outer_inds))

    # Create the result vector with the first step of the reduction, using the passed output if given
    # note that this is using a (potentially) bad cache ordering, so we don't want to continue like this
    i1 = first(outer_inds)
    inner1 = mergeindices(is_outer_dim, i1, a_inds)
    i1, i2 = inner1
    @inbounds a1, a2 = A[i1], A[i2]
    v = _mapreduce_start(f, op, init, a1, a2)
    R = isnothing(out) ? mapreduce_similar(A, typeof(v), raxs) : out
    @inbounds R[i1] = v
    for i in Iterators.drop(outer_inds, 1)
        innerj = mergeindices(is_outer_dim, i, a_inds)
        j1, j2 = innerj
        @inbounds c1, c2 = A[j1], A[j2]
        @inbounds R[i] = _mapreduce_start(f, op, init, c1, c2)
    end
    # If we only had two elements we can quickly return
    # n == 2 && return R

    lsiz = _linear_reduction_length(A, raxs)
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
        # Take care of the smallest cartesian "rectangle" that includes our two peeled elements
        small_inner, large_inner = _split2(inner1)
        # and we may as well try to do this small part in a cache-advantageous manner
        if something(findfirst((>)(1), size(inner1))) < something(findfirst((>)(1), size(outer_inds)), typemax(Int))
            for iR in outer_inds
                @inbounds r = R[iR]
                inner_inds = mergeindices(is_outer_dim, CartesianIndices(map(sliceat, Aaxs, iR.I)), small_inner)
                for iA in Iterators.drop(inner_inds, 2)
                    r = op(r, f(@inbounds(A[iA])))
                end
                @inbounds R[iR] = r
            end
        else
            for iA in Iterators.drop(small_inner, 2)
                for iR in outer_inds
                    iA = mergeindices(is_outer_dim, iR, iA)
                    v = op(@inbounds(R[iR]), f(@inbounds(A[iA])))
                    @inbounds R[iR] = v
                end
            end
        end
        # And then if there's anything left, defer to _mapreducedim!
        if !isempty(large_inner)
            large_inds = mergeindices(is_outer_dim, outer_inds, large_inner)
            _mapreducedim!(f, op, R, A, large_inds.indices)
        end
    end
    return R
end

function _mapreduce_naive_inner_loop(f, op, init, A, is_outer_dim, outer_ind, a_inds)
    inner_inds = mergeindices(is_outer_dim, outer_ind, a_inds)
    i1, i2 = inner_inds
    @inbounds a1, a2 = A[i1], A[i2]
    r = _mapreduce_start(f, op, init, a1, a2)
    for i in Iterators.drop(inner_inds, 2)
        r = op(r, f(@inbounds A[i]))
    end
    return r
end

# Split a CartesianIndices into two parts such that we can Iterators.drop 2 elements from the smallest
# rectangular subsection and then iterate more quickly over the larger remainder.
#
# We have a choice in how we do this split: we can take the away the first column, the first *two* rows, or the first page or...
# We want to pick the _smallest_ split. For example, given four non-singleton dimensions, we have a choice of:
# first: (:, :, :, 1:1)   or (:, :, 1:1,   :) or (:, 1:1, :, :)   or (1:2,   :, :, :)
# rest:  (:, :, :, 2:end) or (:, :, 2:end, :) or (:, 2:end, :, :) or (3:end, :, :, :)
# With dimension lengths of (a, b, c, d), that's minimizing: (a*b*c) vs (a*b*d) vs (a*c*d) vs (2*b*c*d)
# The smallest subsection will always be the one that slices on the _longest_ dimension.
function _split2(ci::CartesianIndices{N}) where {N}
    inds = ci.indices
    first_non_singleton = something(findfirst((>)(1), size(ci)))
    # The first non-singleton dimension "costs half" because we take two elements
    lengths = map(length, inds)
    weights = setindex(lengths, lengths[first_non_singleton]÷2, first_non_singleton)
    sliced_dim = argmax(weights)

    nskip = Int(sliced_dim == first_non_singleton)
    return (CartesianIndices(ntuple(d->d==sliced_dim ? inds[d][begin:begin+nskip] : inds[d][begin:end], Val(N))),
            CartesianIndices(ntuple(d->d==sliced_dim ? inds[d][begin+nskip+1:end] : inds[d][begin:end], Val(N))))
end

_mapreduce_empty_array(_, _, init, A, axes, ::Nothing) = fill!(mapreduce_similar(A, typeof(init), axes), init)
_mapreduce_empty_array(_, _, init, A, axes, out) = fill!(out, init)
function _mapreduce_empty_array(f, op, ::_InitialValue, A, axes, ::Nothing)
    init = mapreduce_empty(f, op, eltype(A))
    return fill!(mapreduce_similar(A, typeof(init), axes), init)
end
function _mapreduce_empty_array(f, op, ::_InitialValue, A, axes, out)
    init = mapreduce_empty(f, op, eltype(A))
    return fill!(out, init)
end

_mapreduce_one_array(f, op, init, A, axes, ::Nothing) = collect_allocator(_mapreduce_similar_curried(A), (op(init, f(@inbounds(A[i]))) for i in CartesianIndices(axes)))
_mapreduce_one_array(f, op, init, A, axes, out) = (for i in CartesianIndices(axes); out[i] = op(init, f(@inbounds(A[i]))); end; out)
_mapreduce_one_array(f, op, ::_InitialValue, A, axes, ::Nothing) = collect_allocator(_mapreduce_similar_curried(A), (mapreduce_first(f, op, @inbounds(A[i])) for i in CartesianIndices(axes)))
_mapreduce_one_array(f, op, ::_InitialValue, A, axes, out) = (for i in CartesianIndices(axes); out[i] = mapreduce_first(f, op, @inbounds(A[i])); end; out)

# Given two initial values in a reduction, compute the beginning of a op chain
_mapreduce_start(f, op, init, a1, a2) = op(op(init, f(a1)), f(a2))
_mapreduce_start(f, op, ::_InitialValue, a1, a2) = op(f(a1), f(a2))

"""
    mapreduce!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init])

Compute `mapreduce(f, op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    The previous values in `R` are _not_ used as initial values; they are completely ignored
"""
function mapreduce!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue())
    raxs = reduced_indices_array(A,R)
    _mapreducedim_impl(f, op, init, A, raxs, R)
end
"""
    reduce!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init])

Compute `reduce(op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    The previous values in `R` are _not_ used as initial values; they are completely ignored
"""
reduce!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue()) = mapreduce!(identity, op, R, A; init)

# !!! this assumes that R holds the initial values
mapreducedim!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted) =
    (reduced_indices_array(A, R); _mapreducedim!(f, op, R, A); R)

reducedim!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted) =
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

function _mapreduce_dim(f, op, init, A::AbstractArrayOrBroadcasted, dims)
    raxs = reduced_indices(A, dims)
    _mapreducedim_impl(f, op, init, A, raxs, nothing)
end

_mapreduce_dim(f, op, ::_InitialValue, A::AbstractArrayOrBroadcasted, ::Colon) =
    _mapreduce(f, op, IndexStyle(A), A)

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
count(f, A::AbstractArray; dims=:)

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
count!(f, r::AbstractArray, A::AbstractArray)

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
                            (:count,   :_count,   :add_sum), (:extrema, :_extrema, :_extrema_rf)]
    mapf = fname === :extrema ? :(ExtremaMap(f)) :
           fname === :count ? :(_bool(f)) : :f
    @eval begin
        # User-facing methods with keyword arguments
        @inline ($fname)(a::AbstractArray; dims=:, kw...) = ($_fname)(a, dims; kw...)
        @inline ($fname)(f, a::AbstractArray; dims=:, kw...) = ($_fname)(f, a, dims; kw...)

        # Underlying implementations using dispatch
        ($_fname)(a, ::Colon; kw...) = ($_fname)(identity, a, :; kw...)
        ($_fname)(f, a, ::Colon; kw...) = mapreduce($mapf, $op, a; kw...)
    end
end
# TODO: why are any/all different?
any(a::AbstractArray; dims=:)              = _any(a, dims)
any(f::Function, a::AbstractArray; dims=:) = _any(f, a, dims)
_any(a, ::Colon)                           = _any(identity, a, :)
all(a::AbstractArray; dims=:)              = _all(a, dims)
all(f::Function, a::AbstractArray; dims=:) = _all(f, a, dims)
_all(a, ::Colon)                           = _all(identity, a, :)

for (fname, op) in [(:sum, :add_sum), (:prod, :mul_prod),
                    (:maximum, :max), (:minimum, :min),
                    (:all, :&),       (:any, :|),
                    (:count, :add_sum), (:extrema, :_extrema_rf)]
    fname! = Symbol(fname, '!')
    _fname = Symbol('_', fname)
    mapf = fname === :extrema ? :(ExtremaMap(f)) :
           fname === :count ? :(_bool(f)) : :f
    @eval begin
        $(fname!)(f::Function, r::AbstractArray, A::AbstractArray; init::Bool=true) = init ?
            mapreduce!($mapf, $op, r, A) : mapreducedim!($mapf, $op, r, A)
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) = $(fname!)(identity, r, A; init=init)

        $(_fname)(A, dims; kw...)    = $(_fname)(identity, A, dims; kw...)
        $(_fname)(f, A, dims; kw...) = mapreduce($mapf, $(op), A; dims=dims, kw...)
    end
end

##### findmin & findmax #####

struct PairsArray{T,N,A} <: AbstractArray{T,N}
    array::A
end
PairsArray(array::AbstractArray{T,N}) where {T, N} = PairsArray{Tuple{keytype(array),T}, N, typeof(array)}(array)
const PairsVector{T,A} = PairsArray{T, 1, A}
IndexStyle(::PairsVector) = IndexLinear()
IndexStyle(::PairsArray) = IndexCartesian()
size(P::PairsArray) = size(P.array)
axes(P::PairsArray) = axes(P.array)
@inline function getindex(P::PairsVector, i::Int)
    @boundscheck checkbounds(P, i)
    @inbounds (i, P.array[i])
end
@inline function getindex(P::PairsArray{<:Any,N}, I::CartesianIndex{N}) where {N}
    @boundscheck checkbounds(P, I)
    @inbounds (I, P.array[I])
end
@propagate_inbounds getindex(P::PairsVector, i::CartesianIndex{1}) = P[i.I[1]]
@propagate_inbounds getindex(P::PairsArray{<:Any,N}, I::Vararg{Int, N}) where {N} = P[CartesianIndex(I)]
# Defer a few functions to the parent array
similar(P::PairsArray, ::Type{T}, dims::Tuple{Union{Int, AbstractUnitRange}, Vararg{Union{Int, AbstractUnitRange}}}) where {T} = similar(P.array, T, dims)
similar(P::PairsArray, ::Type{T}, dims::Tuple{Union{Int, Base.OneTo}, Vararg{Union{Int, Base.OneTo}}}) where {T} = similar(P.array, T, dims)
similar(P::PairsArray, ::Type{T}, dims::Tuple{Int, Vararg{Int}}) where {T} = similar(P.array, T, dims)
similar(P::PairsArray, ::Type{T}, dims::Tuple{}) where {T} = similar(P.array, T, dims)
mapreduce_similar(P::PairsArray, ::Type{T}, dims) where {T} = mapreduce_similar(P.array, T, dims)

# Use an ad-hoc specialized StructArray to allow in-place AoS->SoA transform
struct ZippedArray{T,N,Style,A,B} <: AbstractArray{T,N}
    first::A
    second::B
end
function ZippedArray(A::AbstractArray{T,N},B::AbstractArray{S,N}) where {T,S,N}
    axes(A) == axes(B) || throw(DimensionMismatch("both arrays must have the same shape"))
    # TODO: It'd be better if we could transform a Tuple{Int, Union{Int, Missing}} to Union{Tuple{Int,Int}, Tuple{Int, Missing}}
    ZippedArray{Tuple{T,S},N,IndexStyle(A,B),typeof(A),typeof(B)}(A,B)
end
size(Z::ZippedArray) = size(Z.first)
axes(Z::ZippedArray) = axes(Z.first)
IndexStyle(::ZippedArray{<:Any,<:Any,Style}) where {Style} = Style
@inline function getindex(Z::ZippedArray, I::Int...)
    @boundscheck checkbounds(Z, I...)
    @inbounds (Z.first[I...], Z.second[I...])
end
@propagate_inbounds setindex!(Z::ZippedArray{T}, v, I::Int...) where {T} = setindex!(Z, convert(T, v), I...)
@inline function setindex!(Z::ZippedArray{T}, v::T, I::Int...) where {T}
    @boundscheck checkbounds(Z, I...)
    @inbounds Z.first[I...] = v[1]
    @inbounds Z.second[I...] = v[2]
    return Z
end
_unzip(Z::ZippedArray) = (Z.first, Z.second)
_unzip(A::AbstractArray) = ([a[1] for a in A], [a[2] for a in A])

_transform_pair(f) = x-> (x[1], f(x[2]))
_transform_pair(f::typeof(identity)) = f

"""
    findmin!(rval, rind, A) -> (minval, index)

Find the minimum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as less than all other values except `missing`.

$(_DOCS_ALIASING_WARNING)
"""
findmin!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray; init::Bool=true) = findmin!(identity, rval, rind, A; init)
function findmin!(f, rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    if init
        mapreduce!(_transform_pair(f), (x,y)->ifelse(isgreater(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A))
    else
        mapreducedim!(_transform_pair(f), (x,y)->ifelse(isgreater(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A))
    end
    return (rval, rind)
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
    if f === identity
        # Fast path with pre-allocated arrays
        axs = reduced_indices(A, region)
        return findmin!(identity, mapreduce_similar(A, eltype(A), axs), mapreduce_similar(A, keytype(A), axs), A)
    else
        P = _mapreduce_dim(_transform_pair(f), (x,y)->ifelse(isgreater(x[2], y[2]), y, x), _InitialValue(), PairsArray(A), region)
        (inds, vals) = _unzip(P)
        return (vals, inds)
    end
end
"""
    findmax!(rval, rind, A) -> (maxval, index)

Find the maximum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as greater than all other values except `missing`.

$(_DOCS_ALIASING_WARNING)
"""
findmax!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray; init::Bool=true) = findmax!(identity, rval, rind, A; init)
function findmax!(f, rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true)
    if init
        mapreduce!(_transform_pair(f), (x,y)->ifelse(isless(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A))
    else
        mapreducedim!(_transform_pair(f), (x,y)->ifelse(isless(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A))
    end
    return (rval, rind)
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
    if f === identity
        axs = reduced_indices(A, region)
        return findmax!(identity, mapreduce_similar(A, eltype(A), axs), mapreduce_similar(A, keytype(A), axs), A)
    else
        P = _mapreduce_dim(_transform_pair(f), (x,y)->ifelse(isless(x[2], y[2]), y, x), _InitialValue(), PairsArray(A), region)
        (inds, vals) = _unzip(P)
        return (vals, inds)
    end
end

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

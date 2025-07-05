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

function _check_valid_region(region)
    for d in region
        isa(d, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        Int(d) < 1 && throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
    end
end

###### Generic reduction functions #####

# Given two indices or ranges, merge them by dimension akin to a broadcasted `ifelse` over the dims
_sliceall(x) = x[begin]:x[end] # avoid instabilities with OneTos and offset axes
_ifelseslice(b,x,y) = ifelse(b, _sliceall(x), _sliceall(y))
mergeindices(b::NTuple{N,Bool}, x::CartesianIndices{N}, y::CartesianIndices{N}) where {N} =
    CartesianIndices(map(_ifelseslice, b, x.indices, y.indices))
mergeindices(b::NTuple{N,Bool}, x::CartesianIndex{N}, y::CartesianIndices{N}) where {N} =
    CartesianIndices(map(_ifelseslice, b, x.I, y.indices))
mergeindices(b::NTuple{N,Bool}, x::CartesianIndices{N}, y::CartesianIndex{N}) where {N} =
    CartesianIndices(map(_ifelseslice, b, x.indices, y.I))
mergeindices(b::NTuple{N,Bool}, x::CartesianIndex{N}, y::CartesianIndex{N}) where {N} =
    CartesianIndex(map((b,x,y)->ifelse(b, x, y), b, x.I, y.I))

keep_first_trues(::Tuple{}) = ()
keep_first_trues(t) = t[1] ? (true, keep_first_trues(tail(t))...) : ntuple(Returns(false), length(t))

# These functions aren't used in the implementation here, but are used widely in the ecosystem
promote_union(T::Union) = promote_type(promote_union(T.a), promote_union(T.b))
promote_union(T) = T
_realtype(::Type{<:Complex}) = Real
_realtype(::Type{Complex{T}}) where T<:Real = T
_realtype(T::Type) = T
_realtype(::Union{typeof(abs),typeof(abs2)}, T) = _realtype(T)
_realtype(::Any, T) = T

mapreduce_similar(A, ::Type{T}, dims) where {T} = similar(A, T, dims)

# These special internal types allow exposing both in- and out-of-place array initialization
# as a comprehension-like function call with a generator
struct _MapReduceAllocator{T}
    itr::T
end
_similar_for(c::_MapReduceAllocator, ::Type{T}, itr, ::SizeUnknown, ::Nothing) where {T} =
    mapreduce_similar(c.itr, T, (0,))
_similar_for(c::_MapReduceAllocator, ::Type{T}, itr, ::HasLength, len::Integer) where {T} =
    mapreduce_similar(c.itr, T, (len,))
_similar_for(c::_MapReduceAllocator, ::Type{T}, itr, ::HasShape, axs) where {T} =
    mapreduce_similar(c.itr, T, axs)
(mra::_MapReduceAllocator)(itr) = collect_similar(mra, itr)

struct _MapReduceInPlace{T,F}
    A::T
    op::F
    update::Bool
end
function (mri::_MapReduceInPlace)(g::Generator{<:AbstractArray})
    A = mri.A
    if mri.update
        for i in g.iter
            A[i] = mri.op(A[i], g.f(i))
        end
    else
        for i in g.iter
            A[i] = g.f(i)
        end
    end
    return A
end
function (mri::_MapReduceInPlace)(g::Generator{<:Iterators.Enumerate{<:AbstractArray}})
    A = mri.A
    if mri.update
        for ki in g.iter
            A[ki[2]] = mri.op(A[ki[2]], g.f(ki))
        end
    else
        for ki in g.iter
            A[ki[2]] = g.f(ki)
        end
    end
    return A
end

if isdefined(Core, :Compiler)
    _mapreduce_might_widen(_, _, _, _, ::_MapReduceInPlace) = false
    function _mapreduce_might_widen(f::F, op::G, A::T, init, _) where {F,G,T}
        return !isconcretetype(Core.Compiler.return_type(x->op(_mapreduce_start(f, op, first(x), init), f(first(x))), Tuple{T}))
    end
else
    _mapreduce_might_widen(_, _, _, _, ::_MapReduceInPlace) = false
    _mapreduce_might_widen(_, _, _, _, _) = true
end

# When performing dimensional reductions over arrays with singleton dimensions, we have
# a choice as to whether that singleton dimenion should be a part of the reduction or not;
# it does not affect the output. It's advantageous to additionally consider _leading_ singleton
# dimensions as part of the reduction as that allows more cases to be considered contiguous;
# but once we've broken contiguity it's more helpful to _ignore_ those cases.
compute_inner_dims(flagged_dims, source_size) =
    flagged_dims[1] || source_size[1] == 1 ?
        (true, compute_inner_dims(tail(flagged_dims), tail(source_size))...) :
        (false, map((flag,sz)->flag && sz != 1, tail(flagged_dims), tail(source_size))...)
compute_inner_dims(::Tuple{}, ::Tuple{}) = ()

function mapreducedim(f::F, op::OP, A, init, dims, alloc=_MapReduceAllocator(A)) where {F, OP}
    if alloc isa _MapReduceInPlace
        # We can ignore dims and just trust the output array's axes. Note that the
        # other branch here optimizes for the case where the input array _also_ has
        # a singleton dimension, but we cannot do that here because OffsetArrays
        # supports reductions into differently-offset singleton dimensions. This means
        # we cannot index directly into A with an `outer` index. The output may also have
        # fewer dimensions than A, so we may need to add trailing dims here:
        outer = CartesianIndices(ntuple(d->axes(alloc.A, d), ndims(A)))
        is_inner_dim = map(==(1), size(outer))
    else
        is_inner_dim = compute_inner_dims(ntuple(d->d in dims, ndims(A)), size(A))
        outer = CartesianIndices(reduced_indices(A, dims))
    end
    inner = CartesianIndices(map((b,ax)->b ? ax : reduced_index(ax), is_inner_dim, axes(A)))
    n = length(inner)
    # Handle the empty and trivial 1-element cases:
    if (n == 0 || isempty(A))
        # This is broken out of the comprehension to ensure it's called, avoiding an empty Vector{Union{}}
        v = _mapreduce_start(f, op, A, init)
        return alloc(v for _ in outer)
    end
    n == 1 && return alloc(_mapreduce_start(f, op, A, init, A[i]) for i in outer)
    # Now there are multiple loop ordering strategies depending upon the `dims`:
    if is_inner_dim == keep_first_trues(is_inner_dim) || _mapreduce_might_widen(f, op, A, init, alloc)
        # Column major contiguous reduction! This is the easy case
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    elseif is_inner_dim[1] # `dims` includes the first dimension
        return mapreducedim_colmajor(f, op, A, init, is_inner_dim, inner, outer, alloc)
    else
        return mapreducedim_rowmajor(f, op, A, init, is_inner_dim, inner, outer, alloc)
    end
end

linear_size(A, is_inner_dim) = linear_size(IndexStyle(A), A, is_inner_dim)
linear_size(::IndexLinear, A, is_inner_dim) = if is_inner_dim == keep_first_trues(is_inner_dim)
    prod(map((b,sz)->ifelse(b, sz, 1), is_inner_dim, size(A)))
else
    0
end
linear_size(::IndexStyle, _, _) = 0

function mapreducedim_naive(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc) where {F, OP}
    lsiz = linear_size(A, is_inner_dim)
    if lsiz > 0
        i0 = first(LinearIndices(A))
        alloc(mapreduce_pairwise(f, op, A, init, (i0:i0+lsiz-1) .+ (lsiz*(i-1))) for (i,_) in enumerate(outer))
    else
        alloc(mapreduce_pairwise(f, op, A, init, mergeindices(is_inner_dim, inner, i)) for i in outer)
    end
end
function mapreducedim_colmajor(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc, enforce_pairwise=true) where {F, OP}
    is_contiguous_inner = keep_first_trues(is_inner_dim)
    contiguous_inner = mergeindices(is_contiguous_inner, inner, first(inner))
    discontiguous_inner = mergeindices(is_contiguous_inner, first(inner), inner)
    if enforce_pairwise && length(discontiguous_inner) > pairwise_blocksize(f, op)
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    end
    # Initialize our reduction with the result of doing the first contiguous reduction
    R = alloc(mapreduce_pairwise(f, op, A, init, mergeindices(is_inner_dim, contiguous_inner, o)) for o in outer)
    for dci in Iterators.drop(discontiguous_inner, 1)
        for o in outer
            i = mergeindices(is_inner_dim, dci, o)
            R[o] = op(R[o], mapreduce_pairwise(f, op, A, init, mergeindices(is_contiguous_inner, contiguous_inner, i)))
        end
    end
    return R
end

function mapreducedim_rowmajor(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc, enforce_pairwise=true) where {F, OP}
    # This will only be cache-optimal in the fully "backwards" case — that is, dims are `twoplus:ndims(A)`
    # And even then, it's simply not possible to do this cache-optimally with a pairwise reassociation;
    # this is effectively a foldl.
    if enforce_pairwise && length(inner) > pairwise_blocksize(f, op)
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    end
    # Take one element from each outer iteration to initialize R
    R = alloc(_mapreduce_start(f, op, A, init, A[mergeindices(is_inner_dim, first(inner), o)]) for o in outer)
    for i in Iterators.drop(inner, 1)
        @simd for o in outer
            # SIMD still helps here! It doesn't reassociate _within_ each reduction, but it can allow
            # evaluations _across_ the multiple reductions to coalesce into a single instruction at times
            iA = mergeindices(is_inner_dim, i, o)
            v = op(@inbounds(R[o]), f(@inbounds(A[iA])))
            @inbounds R[o] = v
        end
    end
    return R
end

"""
    mapreduce!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init])

Compute `mapreduce(f, op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    The previous values in `R` are _not_ used as initial values; they are completely ignored
"""
function mapreduce!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue(), update=false)
    if ndims(R) > ndims(A) || !all(d->size(R, d) == 1 || axes(R, d) == axes(A, d), 1:max(ndims(R), ndims(A)))
        throw(DimensionMismatch())
    end
    return mapreducedim(f, op, A, init, nothing, _MapReduceInPlace(R, op, update))
end

"""
    reduce!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init])

Compute `reduce(op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    The previous values in `R` are _not_ used as initial values; they are completely ignored
"""
reduce!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue(), update=false) = mapreduce!(identity, op, R, A; init, update)

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
    mapreduce!(_bool(f), add_sum, r, A; update=!init)

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

julia> all!(Bool[1; 1], A)
2-element Vector{Bool}:
 0
 0

julia> all!(Bool[1 1], A)
1×2 Matrix{Bool}:
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

julia> any!(Bool[1; 1], A)
2-element Vector{Bool}:
 1
 1

julia> any!(Bool[1 1], A)
1×2 Matrix{Bool}:
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
                    (:all, :and_all), (:any, :or_any),
                    (:extrema, :_extrema_rf)]
    fname! = Symbol(fname, '!')
    _fname = Symbol('_', fname)
    mapf = fname === :extrema ? :(ExtremaMap(f)) : :f
    @eval begin
        $(fname!)(f::Function, r::AbstractArray, A::AbstractArray; init::Bool=true) =
            mapreduce!($mapf, $op, r, A; update=!init)
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
        for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @inbounds tmpRv = Rval[i1,IR]
            @inbounds tmpRi = Rind[i1,IR]
            for i in axes(A,1)
                k, kss = y::Tuple
                tmpAv = f(@inbounds(A[i,IA]))
                if tmpRi == zi || op(tmpRv, tmpAv)
                    tmpRv = tmpAv
                    tmpRi = k
                end
                y = iterate(ks, kss)
            end
            @inbounds Rval[i1,IR] = tmpRv
            @inbounds Rind[i1,IR] = tmpRi
        end
    else
        for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            for i in axes(A, 1)
                k, kss = y::Tuple
                tmpAv = f(@inbounds(A[i,IA]))
                @inbounds tmpRv = Rval[i,IR]
                @inbounds tmpRi = Rind[i,IR]
                if tmpRi == zi || op(tmpRv, tmpAv)
                    @inbounds Rval[i,IR] = tmpAv
                    @inbounds Rind[i,IR] = k
                end
                y = iterate(ks, kss)
            end
        end
    end
    Rval, Rind
end

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
_transform_pair(::Type{F}) where {F} = x-> (x[1], F(x[2]))
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
    mapreduce!(_transform_pair(f), (x,y)->ifelse(isgreater(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A); update=!init)
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
        P = mapreduce(_transform_pair(f), (x,y)->ifelse(isgreater(x[2], y[2]), y, x), PairsArray(A); dims=region)
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
    mapreduce!(_transform_pair(f), (x,y)->ifelse(isless(x[2], y[2]), y, x), ZippedArray(rind, rval), PairsArray(A); update=!init)
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
        P = mapreduce(_transform_pair(f), (x,y)->ifelse(isless(x[2], y[2]), y, x), PairsArray(A); dims=region)
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

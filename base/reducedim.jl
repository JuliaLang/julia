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

function check_reducedims(R, A)
    # Check whether R has compatible dimensions w.r.t. A for reduction
    #
    # It returns an integer value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, dims=1) or sum(A, dims=(1,2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, dims=2) or sum(A, dims=(1,3)), it returns 0.
    #
    ndims(R) <= ndims(A) || throw(DimensionMismatch("cannot reduce $(ndims(A))-dimensional array to $(ndims(R)) dimensions"))
    lsiz = 1
    had_nonreduc = false
    for i = 1:ndims(A)
        Ri, Ai = axes(R, i), axes(A, i)
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
            Ri == Ai || throw(DimensionMismatch("reduction on array with indices $(axes(A)) with output with indices $(axes(R))"))
            had_nonreduc = true
        end
    end
    return lsiz
end

reducedim1(R, A) = length(axes1(R)) == 1

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

# The in-place callable is only defined for the generator shapes the mapreducedim
# strategies produce: the generator's iterator must be the output array's own index
# space (or an enumeration of it), since it doubles as the store index.
struct _MapReduceInPlace{T,F}
    A::T
    op::F
    update::Bool
end
(mri::_MapReduceInPlace)(itr) =
    error("_MapReduceInPlace must be called with a generator over the output's indices (or an enumeration thereof)")
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

# N.B.: the compiler must be referenced lazily from inside the function body (as
# `promote_op` does); a load-time `isdefined(Core, :Compiler)` check here is always
# false during bootstrap, which would permanently bake the pessimistic fallback into
# the sysimage and route every out-of-place dimensional reduction to the naive
# column-major strategy.
_mapreduce_might_widen(_, _, _, _, ::_MapReduceInPlace) = false
function _mapreduce_might_widen(f::F, op::G, A::T, init, _) where {F,G,T}
    # The probes model the start of a *nonempty* chain: seed with the container's first
    # element (the five-argument `_mapreduce_start`), then fold one more mapped element.
    # Passing an element where `_mapreduce_start`'s container belongs would instead probe
    # the empty-reduction path, whose `mapreduce_empty` methods don't exist for most
    # mapping functions, spuriously flagging them as widening.
    #
    # The typed (non-widening) strategies allocate their output from the seeds and then
    # accumulate in place, so they require the accumulator's type not to change as the
    # chain grows — the seed type must already be the chain type. `prod` over an array
    # of block transposes is a live counterexample: its seeds are lazy `Transpose`s
    # while any longer chain multiplies out to a plain `Matrix`.
    seedT = promote_op(x->_mapreduce_start(f, op, x, init, first(x)), T)
    chainT = promote_op(x->op(_mapreduce_start(f, op, x, init, first(x)), f(first(x))), T)
    return !(isconcretetype(chainT) && seedT === chainT)
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
    contiguous = is_inner_dim == keep_first_trues(is_inner_dim)
    if contiguous && IndexStyle(A) isa IndexLinear
        # Column major contiguous reduction! This is the easy case: each output is
        # one pairwise reduction over a linear index range with tuned kernels
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    elseif _mapreduce_might_widen(f, op, A, init, alloc)
        # The naive strategy carries its accumulators through `op` without ever
        # storing them in a typed output, making it the widening-safe path
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    elseif A isa StridedArray && n <= pairwise_blocksize(f, op) &&
           (is_inner_dim[1] ||
            (!(IndexStyle(A) isa IndexLinear) &&
             all(map((b, sz) -> b | (sz == 1), tail(is_inner_dim), tail(size(A)))) &&
             count(map((b, sz) -> b & (sz > 1), is_inner_dim, size(A))) > 1))
        # Each output's fold chain fits within the pairwise blocksize, so a single
        # storage-order left-fold pass over the whole array stays within the
        # documented reassociation latitude — and with cheap strided `getindex` it
        # runs at memory speed where the pairwise-per-output strategies would pay
        # per-column or per-slab overheads. That covers every reduction along the
        # first dimension (except contiguous IndexLinear ones, already handled
        # above), plus Cartesian reductions with several reduced extents that keep
        # only the first dimension (every trailing dimension reduced or singleton,
        # so the seeds consume exactly the first column of the trailing space).
        # IndexLinear reductions that keep the first dimension stay with
        # the naive/rowmajor strategies: their tuned strided kernels and the
        # vectorized linear sweep measure faster than this scalar sweep there.
        return mapreducedim_fold(f, op, A, init, is_inner_dim, inner, outer, alloc)
    elseif contiguous
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
# A single storage-order pass over `A`, accumulating directly into the output:
# seed every output with the first element of its chain, then left-fold each further
# element into its output as it streams by. Each chain is folded in the same
# column-major order the other strategies use — only the interleaving *across*
# chains differs, which is unobservable — so this requires no commutativity. It
# does fold each chain serially, so callers must ensure the chains are no longer
# than `pairwise_blocksize` to stay within the pairwise reassociation bounds.
# The per-column accumulator fold: ops we know to be commutative get `@simd`,
# licensing LLVM to reassociate the chain into vector lanes (worth 5-9x for
# floating-point sums; a no-op for ops the vectorizer cannot handle anyway).
# Everything else gets a strict left-to-right fold with deterministic,
# compiler-independent results — matching `mapreduce_kernel`'s policy of
# reserving its reassociating lane kernels for `is_commutative_op` ops. The
# branch is a compile-time constant per `op`, so only one loop is ever emitted.
@inline function _mapreducedim_fold_column(f::F, op::OP, A, r, is, IA) where {F, OP}
    if is_commutative_op(op)
        @inbounds @simd for i in is
            r = op(r, f(A[i, IA]))
        end
    else
        @inbounds for i in is
            r = op(r, f(A[i, IA]))
        end
    end
    return r
end

function mapreducedim_fold(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc) where {F, OP}
    R = alloc(_mapreduce_start(f, op, A, init, A[mergeindices(is_inner_dim, first(inner), o)]) for o in outer)
    ax1 = axes(A, 1)
    keep, Idefault = Broadcast.shapeindexer(safe_tail(axes(R)))
    tailax = tail(axes(A))
    if is_inner_dim[1]
        # The first axis is reduced: keep each column's accumulator in a local.
        # Iterate as reduced-tail slices (ascending, so every chain still folds in
        # its column-major order) crossed with the kept-tail positions. The seeds
        # above consumed the first element of each output's first column, so the
        # first reduced-tail slice is peeled off with a shortened column range —
        # keeping both loop nests free of per-column branches, which LLVM
        # otherwise pessimizes badly.
        t_mask = tail(is_inner_dim)
        tA_first = CartesianIndex(map(first, tailax))
        di_space = mergeindices(t_mask, CartesianIndices(tailax), tA_first)
        ok_space = mergeindices(t_mask, tA_first, CartesianIndices(tailax))
        i1 = first(axes(R, 1))
        di1 = first(di_space)
        @inbounds for OK in ok_space
            IA = di1 + OK - tA_first
            IR = Broadcast.newindex(OK, keep, Idefault)
            R[i1, IR] = _mapreducedim_fold_column(f, op, A, R[i1, IR], first(ax1)+1:last(ax1), IA)
        end
        @inbounds for DI in Iterators.drop(di_space, 1)
            for OK in ok_space
                IA = DI + OK - tA_first
                IR = Broadcast.newindex(OK, keep, Idefault)
                R[i1, IR] = _mapreducedim_fold_column(f, op, A, R[i1, IR], ax1, IA)
            end
        end
    else
        # The first axis is kept and every trailing dimension is reduced or
        # singleton (the only pattern routed here): the seeds above consumed
        # exactly the first column of the tail space, so simply drop it and sweep
        # the rest in storage order. Each iteration of the inner loop touches a
        # different output, so there is no accumulator chain to reassociate and
        # no `@simd` annotation: this is a deterministic left fold per output
        # (measured codegen is identical with or without the annotation).
        @inbounds for IA in Iterators.drop(CartesianIndices(tailax), 1)
            IR = Broadcast.newindex(IA, keep, Idefault)
            for i in ax1
                v = op(R[i, IR], f(A[i, IA]))
                R[i, IR] = v
            end
        end
    end
    return R
end

function mapreducedim_colmajor(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc, enforce_pairwise=true) where {F, OP}
    is_contiguous_inner = keep_first_trues(is_inner_dim)
    contiguous_inner = mergeindices(is_contiguous_inner, inner, first(inner))
    discontiguous_inner = mergeindices(is_contiguous_inner, first(inner), inner)
    # Beyond the pairwise-required blocksize reroute, columns shorter than the
    # blocksize cannot amortize a full pairwise call (plus an `R` read-modify-write)
    # per column: the naive strategy amortizes that overhead over each output's whole
    # slab and measures at worst on par with the column-at-a-time sweep here even for
    # adversarially tiny columns with large outer extents.
    if enforce_pairwise && (length(discontiguous_inner) > pairwise_blocksize(f, op) ||
                            length(contiguous_inner) < pairwise_blocksize(f, op))
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

# True when every dimension at or after the first reduced one is either also reduced
# or has singleton extent: the kept dimensions then form a contiguous leading block
# of `length(outer)` elements in linear-index order, and each step along the reduced
# dimensions advances the linear index by exactly that block length.
function _rowmajor_is_linear_sweep(is_inner_dim, sz)
    seen_inner = false
    for d in eachindex(is_inner_dim)
        if is_inner_dim[d]
            seen_inner = true
        elseif seen_inner && sz[d] != 1
            return false
        end
    end
    return true
end

function mapreducedim_rowmajor(f::F, op::OP, A, init, is_inner_dim, inner, outer, alloc, enforce_pairwise=true) where {F, OP}
    # This will only be cache-optimal in the fully "backwards" case — that is, dims are `twoplus:ndims(A)`
    # And even then, it's simply not possible to do this cache-optimally with a pairwise reassociation;
    # this is effectively a foldl.
    if enforce_pairwise && length(inner) > pairwise_blocksize(f, op)
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    end
    if enforce_pairwise && length(outer) < 16
        # With only a handful of outputs, one tuned strided pairwise pass per output
        # is cheaper than sweeping across the outputs — even the vectorized linear
        # sweep below cannot fill its vectors; the measured crossover between the
        # two approaches is at roughly 16 outputs.
        return mapreducedim_naive(f, op, A, init, is_inner_dim, inner, outer, alloc)
    end
    is_linear_sweep = A isa AbstractArray && IndexStyle(A) isa IndexLinear &&
                      _rowmajor_is_linear_sweep(is_inner_dim, size(A))
    # Take one element from each outer iteration to initialize R
    R = alloc(_mapreduce_start(f, op, A, init, A[mergeindices(is_inner_dim, first(inner), o)]) for o in outer)
    if is_linear_sweep && IndexStyle(R) isa IndexLinear
        # Fold each contiguous `length(outer)`-long block into the outputs by linear
        # index — the same fold order as the Cartesian loop below, elementwise
        # bit-identical, but amenable to vectorization across the outputs. Each
        # `@simd` iteration touches a different output, so its reassociation
        # license goes unused; the annotation only aids memory-dependence analysis.
        rr = eachindex(R)
        offset = first(LinearIndices(A)) - first(rr)
        M = length(outer)
        for k in 1:length(inner)-1
            koffset = offset + k*M
            @simd for r in rr
                ai = @inbounds A[r + koffset]
                v = op(@inbounds(R[r]), f(ai))
                @inbounds R[r] = v
            end
        end
        return R
    end
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

_mapreduce_dim(f, op, nt, R::ReshapedArray, ::Colon) =
    _mapreduce_dim(f, op, nt, parent(R), :)

_mapreduce_dim(f, op, i::_InitialValue, R::ReshapedArray, ::Colon) =
    _mapreduce_dim(f, op, i, parent(R), :)

"""
    mapreduce!(f, op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init], [update])

Compute `mapreduce(f, op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    By default the previous values in `R` are _not_ used as initial values; they are
    completely ignored. Pass `update=true` to instead combine each reduction result
    with the corresponding existing value in `R` using `op`; this is the mechanism
    behind, e.g., `sum!(R, A; init=false)`.
"""
function mapreduce!(f::F, op::OP, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue(), update=false) where {F, OP}
    check_reducedims(R, A)
    return mapreducedim(f, op, A, init, nothing, _MapReduceInPlace(R, op, update))
end

"""
    reduce!(op, R::AbstractArray, A::AbstractArrayOrBroadcasted; [init], [update])

Compute `reduce(op, A; init, dims)` where `dims` are the singleton dimensions of `R`, storing the result into `R`.

!!! note
    By default the previous values in `R` are _not_ used as initial values; they are
    completely ignored. Pass `update=true` to instead combine each reduction result
    with the corresponding existing value in `R` using `op`.
"""
reduce!(op::OP, R::AbstractArray, A::AbstractArrayOrBroadcasted; init=_InitialValue(), update=false) where {OP} = mapreduce!(identity, op, R, A; init, update)

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
count(A::AbstractArrayOrBroadcasted; dims::D=:, init=0) where {D} = count(identity, A; dims, init)
count(f, A::AbstractArrayOrBroadcasted; dims::D=:, init=0) where {D} = _count(f, A, dims, init)

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
all(f, ::AbstractArray; dims)

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
any(f, ::AbstractArray; dims)

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
        @inline ($fname)(a::AbstractArray; dims::D=:, kw...) where {D} = ($_fname)(a, dims; kw...)
        @inline ($fname)(f, a::AbstractArray; dims::D=:, kw...) where {D} = ($_fname)(f, a, dims; kw...)

        # Underlying implementations using dispatch
        ($_fname)(a, ::Colon; kw...) = ($_fname)(identity, a, :; kw...)
        ($_fname)(f, a, ::Colon; kw...) = mapreduce($mapf, $op, a; kw...)

        ($_fname)(R::ReshapedArray, ::Colon) = ($fname)(parent(R))
    end
end

any(a::AbstractArray; kw...) = any(identity, a; kw...)
any(f, a::AbstractArray; dims::D=:) where D = _any(f, a, dims)
all(a::AbstractArray; kw...) = all(identity, a; kw...)
all(f, a::AbstractArray; dims::D=:) where D = _all(f, a, dims)

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

        $(_fname)(A, dims::D; kw...) where {D} = $(_fname)(identity, A, dims; kw...)
        $(_fname)(f, A, dims::D; kw...) where {D} = mapreduce($mapf, $(op), A; dims=dims, kw...)
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

# The reducing ops for dimensional findmin/findmax over the `(index, value)` pairs of a
# `PairsArray` (n.b. `_rf_findmin`/`_rf_findmax` in reduce.jl use `(value, index)` order).
# The `ifelse(..., y, x)` orientation keeps the incumbent on ties so the first index wins.
_rf_pairfindmin(x, y) = ifelse(isgreater(x[2], y[2]), y, x)
_rf_pairfindmax(x, y) = ifelse(isless(x[2], y[2]), y, x)

"""
    findmin!(rval, rind, A) -> (minval, index)

Find the minimum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
`NaN` is treated as less than all other values except `missing`.

$(_DOCS_ALIASING_WARNING)
"""
findmin!(rval::AbstractArray, rind::AbstractArray, A::AbstractArray; init::Bool=true) = findmin!(identity, rval, rind, A; init)
function findmin!(f::F, rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true) where {F}
    mapreduce!(_transform_pair(f), _rf_pairfindmin, ZippedArray(rind, rval), PairsArray(A); update=!init)
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
findmin(A::AbstractArray; dims::D=:) where {D} = _findmin(A, dims)
_findmin(A, dims::D) where {D} = _findmin(identity, A, dims)

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
findmin(f, A::AbstractArray; dims::D=:) where {D} = _findmin(f, A, dims)

function _findmin(f, A, region::D) where {D}
    if f === identity
        # Fast path with pre-allocated arrays
        axs = reduced_indices(A, region)
        return findmin!(identity, mapreduce_similar(A, eltype(A), axs), mapreduce_similar(A, keytype(A), axs), A)
    else
        P = mapreduce(_transform_pair(f), _rf_pairfindmin, PairsArray(A); dims=region)
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
function findmax!(f::F, rval::AbstractArray, rind::AbstractArray, A::AbstractArray;
                  init::Bool=true) where {F}
    mapreduce!(_transform_pair(f), _rf_pairfindmax, ZippedArray(rind, rval), PairsArray(A); update=!init)
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
findmax(A::AbstractArray; dims::D=:) where {D} = _findmax(A, dims)
_findmax(A, dims::D) where {D} = _findmax(identity, A, dims)

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
findmax(f, A::AbstractArray; dims::D=:) where {D} = _findmax(f, A, dims)
function _findmax(f, A, region::D) where {D}
    if f === identity
        axs = reduced_indices(A, region)
        return findmax!(identity, mapreduce_similar(A, eltype(A), axs), mapreduce_similar(A, keytype(A), axs), A)
    else
        P = mapreduce(_transform_pair(f), _rf_pairfindmax, PairsArray(A); dims=region)
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
argmin(A::AbstractArray; dims::D=:) where {D} = findmin(A; dims=dims)[2]

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
argmax(A::AbstractArray; dims::D=:) where {D} = findmax(A; dims=dims)[2]

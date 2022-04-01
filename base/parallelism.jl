# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    _is_effect_free(f, t::Type{<:Tuple}) -> ans::Bool

Return `true` if `f(args...)` with `args::t` is consdiered to be `:effect_free`
as defined in `@assume_effects`.
"""
function _is_effect_free(f::F, t) where {F}
    return Core.Compiler.is_effect_free(Core.Compiler.infer_effects(f, t))
end

# Choosing a rather very conservative bound for parallelism to avoid slowing
# down the case parallelism is not helpful.  This number is chosen s.t. the
# sequential `map(identity, ::Vector{Float64})` of this size takes about > 100
# μs.  So, the cost of task spawn/sync is likely neglegible even for very
# trivial `f`.  This is a global mutable so that it is easy to check the
# parallel path in end-to-end tests.
const _MAP_MIN_BASESIZE = Ref(2^18)

function _maybe_parallelize_collect(iter::Generator)
    # TODO: use transducer to cleanly implement this
    # TODO: support filter, flatten, product, etc.
    arrays = if iter.iter isa AbstractArray
        (iter.iter,)
    elseif iter.iter isa Iterators.Zip{<:Tuple{Vararg{AbstractArray}}}
        iter.iter.is
    else
        return nothing
    end
    arrays === () && return nothing

    # TODO: Maybe avoid parallel implementation if `f` and `getindex` are also
    # `:consistent`? It can happen for something like FillArrays.

    # TODO: guess a good number from the cost of `f` and `getindex`?
    min_basesize = max(2, _MAP_MIN_BASESIZE[])
    length(arrays[1]) < min_basesize && return nothing

    # Only handle compatible shape (and thus uniform length) for now.
    all(==(axes(arrays[1])), map(axes, tail(arrays))) || return nothing
    shape = size(arrays[1]) # relies on the check above

    Threads.nthreads() == 1 && return nothing

    indices = eachindex(arrays...)
    indextype = eltype(indices)
    _is_effect_free(ith_all, Tuple{indextype,typeof(arrays)}) || return nothing

    # FIXME: `getvalue` captures `iter` just in case `f` is a `Type` (which
    # would be captured as a `DataType`)
    getvalue = if iter.iter isa AbstractArray
        _is_effect_free(iter.f, Tuple{eltype(arrays[1])}) || return nothing
        @inline getvalue1(i) = iter.f(@inbounds arrays[1][i])
    else
        _is_effect_free(iter.f, Tuple{Tuple{map(eltype, arrays)...}}) || return nothing
        @inline getvalue2(i) = iter.f((@inbounds ith_all(i, arrays)))
    end

    # Cap the `basesize` assuming that the workload of `f` is uniform. This may
    # not be a good choice especially once we support filter and flatten.
    # However, since this code path is enabled automatically, it may be better
    # to play on the very safe side.
    basesize = min(min_basesize, cld(length(indices), Threads.nthreads()))

    # Note: `@default_eltype` is incorrect if `T(....)` (with `T::Type`) does
    # not return a `T`. However, `collect(::Generator)` already uses `@default_eltype`.
    et = @default_eltype(iter)
    if isconcretetype(et)
        # We do not leak compiler internal here even though `et` is visible from
        # the user because we have checked that input is not empty.  It is not
        # perfect since the sequential implementation of `collect(::Generator)`
        # itself does leak the compiler internal by returning `Array{et}`.
        # However, if/when `collect(::Generator)` solved this issue, the
        # parallel implementation does not get in the way of typocalyps-free
        # Base.
        dest = Array{et}(undef, size(arrays[1]))
        return Some(_parallel_map!(getvalue, dest, indices))
    else
        # TODO: use `_parallel_map!` if `allocatedinline(et)` and then refine
        # type (and fuse the mapping and the type bound computation)
        ys = _parallel_map(getvalue, indices; basesize)::Array{<:et}
        if length(shape) == 1
            return Some(ys)
        else
            return Some(reshape(ys, shape))
        end
    end
end

"""
    _parallel_map!(f, dest, xs) -> dest

A parallel version of `map!` (i.e., `dest .= f.(xs)`).

Before turning this to a proper API (say) `Threads.map!`:
* (ideally: define infrastructure for making it extensible)
* use basesize to control parallelism in a compositional manner
* reject obviously wrong inputs like `dest::BitArray` (or just support it)
"""
function _parallel_map!(f, dest, xs)
    # TODO: use divide-and-conquer strategy for fast spawn and sync
    # TODO: don't use `@threads`
    # TODO: since the caller allocates `dest` and `f` is effect-free, we can use
    # `@simd ivdep`
    Threads.@threads for i in eachindex(dest, xs)
        @inbounds dest[i] = f(xs[i])
    end
    return dest
end

function _halve(xs::AbstractVector)
    f = firstindex(xs)
    l = lastindex(xs)
    h = length(xs) ÷ 2
    return view(xs, f:f+h), view(xs, f+h+1:l)
end

_halve(xs::AbstractArray) = _halve_ndarray(xs)

function _halve_ndarray(xs::AbstractArray{N}, ::Val{D} = Val(N)) where {N,D}
    if D > 1
        size(xs, D) < 2 && return _halve_ndarray(xs, Val(D - 1))
    end
    f = firstindex(xs, D)
    l = lastindex(xs, D)
    h = size(xs, D) ÷ 2
    cs1 = ntuple(_ -> :, Val(D - 1))
    cs2 = ntuple(_ -> :, Val(N - D))
    return view(xs, cs1..., f:f+h, cs2...), view(xs, cs1..., f+h+1:l, cs2...)
end

"""
    _parallel_map(f, xs; basesize) -> ys::Vector

Note: The output is always a `Vector` even if the input can have arbitrary
`ndims`.  The caller is responsible for `reshape`ing the output properly.
"""
function _parallel_map(f, xs; basesize)
    length(xs) <= max(2, basesize) && return vec(_serial_collect(Iterators.map(f, xs)))
    xs1, xs2 = _halve(xs)
    task = Threads.@spawn _parallel_map(f, xs2; basesize)
    ys1 = _parallel_map(f, xs1; basesize)::Vector
    ys2 = fetch(task)::Vector
    if eltype(ys2) <: eltype(ys1)
        return append!(ys1, ys2)
    elseif eltype(ys1) <: eltype(ys2)
        insert!(ys1, firstindex(ys1), ys2)
        return ys2
    else
        # Note: we cannot use `vcat` here since `collect` uses
        # `promote_typejoin` instead of `promote`
        T = promote_typejoin(eltype(ys1), eltype(ys2))
        ys3 = empty!(Vector{T}(undef, length(ys1) + length(ys2)))
        return append!(ys3, ys1, ys2)
    end
end

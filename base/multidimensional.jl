# This file is a part of Julia. License is MIT: https://julialang.org/license

### Multidimensional iterators
module IteratorsMD
    import Base: eltype, length, size, start, done, next, first, last, in, getindex,
                 setindex!, IndexStyle, min, max, zero, one, isless, eachindex,
                 ndims, IteratorSize, convert, show

    import Base: +, -, *
    import Base: simd_outer_range, simd_inner_length, simd_index
    using Base: IndexLinear, IndexCartesian, AbstractCartesianIndex, fill_to_length, tail
    using Base.Iterators: Reverse

    export CartesianIndex, CartesianIndices, LinearIndices

    """
        CartesianIndex(i, j, k...)   -> I
        CartesianIndex((i, j, k...)) -> I

    Create a multidimensional index `I`, which can be used for
    indexing a multidimensional array `A`.  In particular, `A[I]` is
    equivalent to `A[i,j,k...]`.  One can freely mix integer and
    `CartesianIndex` indices; for example, `A[Ipre, i, Ipost]` (where
    `Ipre` and `Ipost` are `CartesianIndex` indices and `i` is an
    `Int`) can be a useful expression when writing algorithms that
    work along a single dimension of an array of arbitrary
    dimensionality.

    A `CartesianIndex` is sometimes produced by [`eachindex`](@ref), and
    always when iterating with an explicit [`CartesianIndices`](@ref).

    # Examples
    ```jldoctest
    julia> A = reshape(Vector(1:16), (2, 2, 2, 2))
    2×2×2×2 Array{Int64,4}:
    [:, :, 1, 1] =
     1  3
     2  4

    [:, :, 2, 1] =
     5  7
     6  8

    [:, :, 1, 2] =
      9  11
     10  12

    [:, :, 2, 2] =
     13  15
     14  16

    julia> A[CartesianIndex((1, 1, 1, 1))]
    1

    julia> A[CartesianIndex((1, 1, 1, 2))]
    9

    julia> A[CartesianIndex((1, 1, 2, 1))]
    5
    ```
    """
    struct CartesianIndex{N} <: AbstractCartesianIndex{N}
        I::NTuple{N,Int}
        CartesianIndex{N}(index::NTuple{N,Integer}) where {N} = new(index)
    end

    CartesianIndex(index::NTuple{N,Integer}) where {N} = CartesianIndex{N}(index)
    CartesianIndex(index::Integer...) = CartesianIndex(index)
    CartesianIndex{N}(index::Vararg{Integer,N}) where {N} = CartesianIndex{N}(index)
    # Allow passing tuples smaller than N
    CartesianIndex{N}(index::Tuple) where {N} = CartesianIndex{N}(fill_to_length(index, 1, Val(N)))
    CartesianIndex{N}(index::Integer...) where {N} = CartesianIndex{N}(index)
    CartesianIndex{N}() where {N} = CartesianIndex{N}(())
    # Un-nest passed CartesianIndexes
    CartesianIndex(index::Union{Integer, CartesianIndex}...) = CartesianIndex(flatten(index))
    flatten(I::Tuple{}) = I
    flatten(I::Tuple{Any}) = I
    flatten(I::Tuple{<:CartesianIndex}) = I[1].I
    @inline flatten(I) = _flatten(I...)
    @inline _flatten() = ()
    @inline _flatten(i, I...)                 = (i, _flatten(I...)...)
    @inline _flatten(i::CartesianIndex, I...) = (i.I..., _flatten(I...)...)
    CartesianIndex(index::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = CartesianIndex(index...)
    show(io::IO, i::CartesianIndex) = (print(io, "CartesianIndex"); show(io, i.I))

    # length
    length(::CartesianIndex{N}) where {N} = N
    length(::Type{CartesianIndex{N}}) where {N} = N

    # indexing
    getindex(index::CartesianIndex, i::Integer) = index.I[i]
    eltype(index::CartesianIndex) = eltype(index.I)

    # access to index tuple
    Tuple(index::CartesianIndex) = index.I

    # zeros and ones
    zero(::CartesianIndex{N}) where {N} = zero(CartesianIndex{N})
    zero(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(x -> 0, Val(N)))
    one(::CartesianIndex{N}) where {N} = one(CartesianIndex{N})
    one(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(x -> 1, Val(N)))

    # arithmetic, min/max
    @inline (-)(index::CartesianIndex{N}) where {N} =
        CartesianIndex{N}(map(-, index.I))
    @inline (+)(index1::CartesianIndex{N}, index2::CartesianIndex{N}) where {N} =
        CartesianIndex{N}(map(+, index1.I, index2.I))
    @inline (-)(index1::CartesianIndex{N}, index2::CartesianIndex{N}) where {N} =
        CartesianIndex{N}(map(-, index1.I, index2.I))
    @inline min(index1::CartesianIndex{N}, index2::CartesianIndex{N}) where {N} =
        CartesianIndex{N}(map(min, index1.I, index2.I))
    @inline max(index1::CartesianIndex{N}, index2::CartesianIndex{N}) where {N} =
        CartesianIndex{N}(map(max, index1.I, index2.I))

    @inline (+)(i::Integer, index::CartesianIndex) = index+i
    @inline (+)(index::CartesianIndex{N}, i::Integer) where {N} = CartesianIndex{N}(map(x->x+i, index.I))
    @inline (-)(index::CartesianIndex{N}, i::Integer) where {N} = CartesianIndex{N}(map(x->x-i, index.I))
    @inline (-)(i::Integer, index::CartesianIndex{N}) where {N} = CartesianIndex{N}(map(x->i-x, index.I))
    @inline (*)(a::Integer, index::CartesianIndex{N}) where {N} = CartesianIndex{N}(map(x->a*x, index.I))
    @inline (*)(index::CartesianIndex, a::Integer) = *(a,index)

    # comparison
    @inline isless(I1::CartesianIndex{N}, I2::CartesianIndex{N}) where {N} = _isless(0, I1.I, I2.I)
    @inline function _isless(ret, I1::NTuple{N,Int}, I2::NTuple{N,Int}) where N
        newret = ifelse(ret==0, icmp(I1[N], I2[N]), ret)
        _isless(newret, Base.front(I1), Base.front(I2))
    end
    _isless(ret, ::Tuple{}, ::Tuple{}) = ifelse(ret==1, true, false)
    icmp(a, b) = ifelse(isless(a,b), 1, ifelse(a==b, 0, -1))

    # conversions
    convert(::Type{T}, index::CartesianIndex{1}) where {T<:Number} = convert(T, index[1])
    convert(::Type{T}, index::CartesianIndex) where {T<:Tuple} = convert(T, index.I)

    # hashing
    const cartindexhash_seed = UInt == UInt64 ? 0xd60ca92f8284b8b0 : 0xf2ea7c2e
    function Base.hash(ci::CartesianIndex, h::UInt)
        h += cartindexhash_seed
        for i in ci.I
            h = hash(i, h)
        end
        return h
    end

    # nextind and prevind with CartesianIndex
    function Base.nextind(a::AbstractArray{<:Any,N}, i::CartesianIndex{N}) where {N}
        _, ni = next(CartesianIndices(axes(a)), i)
        return ni
    end
    function Base.prevind(a::AbstractArray{<:Any,N}, i::CartesianIndex{N}) where {N}
        _, ni = next(Iterators.reverse(CartesianIndices(axes(a))), i)
        return ni
    end

    # Iteration over the elements of CartesianIndex cannot be supported until its length can be inferred,
    # see #23719
    Base.start(::CartesianIndex) =
        error("iteration is deliberately unsupported for CartesianIndex. Use `I` rather than `I...`, or use `Tuple(I)...`")

    # Iteration
    """
        CartesianIndices(sz::Dims) -> R
        CartesianIndices(istart:istop, jstart:jstop, ...) -> R

    Define a region `R` spanning a multidimensional rectangular range
    of integer indices. These are most commonly encountered in the
    context of iteration, where `for I in R ... end` will return
    [`CartesianIndex`](@ref) indices `I` equivalent to the nested loops

        for j = jstart:jstop
            for i = istart:istop
                ...
            end
        end

    Consequently these can be useful for writing algorithms that
    work in arbitrary dimensions.

        CartesianIndices(A::AbstractArray) -> R

    As a convenience, constructing a `CartesianIndices` from an array makes a
    range of its indices.

    # Examples
    ```jldoctest
    julia> foreach(println, CartesianIndices((2, 2, 2)))
    CartesianIndex(1, 1, 1)
    CartesianIndex(2, 1, 1)
    CartesianIndex(1, 2, 1)
    CartesianIndex(2, 2, 1)
    CartesianIndex(1, 1, 2)
    CartesianIndex(2, 1, 2)
    CartesianIndex(1, 2, 2)
    CartesianIndex(2, 2, 2)

    julia> CartesianIndices(fill(1, (2,3)))
    2×3 CartesianIndices{2,Tuple{Base.OneTo{Int64},Base.OneTo{Int64}}}:
      CartesianIndex(1, 1)  CartesianIndex(1, 2)  CartesianIndex(1, 3)
      CartesianIndex(2, 1)  CartesianIndex(2, 2)  CartesianIndex(2, 3)
    ```

    ## Conversion between linear and cartesian indices

    Linear index to cartesian index conversion exploits the fact that a
    `CartesianIndices` is an `AbstractArray` and can be indexed linearly:

    ```jldoctest subarray
    julia> cartesian = CartesianIndices(1:3,1:2)
    3×2 CartesianIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
     CartesianIndex(1, 1)  CartesianIndex(1, 2)
     CartesianIndex(2, 1)  CartesianIndex(2, 2)
     CartesianIndex(3, 1)  CartesianIndex(3, 2)

    julia> cartesian[4]
    CartesianIndex(1, 2)
    ```

    For cartesian to linear index conversion, see [`LinearIndices`](@ref).
    """
    struct CartesianIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{CartesianIndex{N},N}
        indices::R
    end

    CartesianIndices(::Tuple{}) = CartesianIndices{0,typeof(())}(())
    CartesianIndices(inds::NTuple{N,AbstractUnitRange{Int}}) where {N} =
        CartesianIndices{N,typeof(inds)}(inds)
    CartesianIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} =
        CartesianIndices(inds)
    CartesianIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} =
        CartesianIndices(map(r->convert(AbstractUnitRange{Int}, r), inds))
    CartesianIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} =
        CartesianIndices(inds)

    CartesianIndices(index::CartesianIndex) = CartesianIndices(index.I)
    CartesianIndices(sz::NTuple{N,<:Integer}) where {N} = CartesianIndices(map(Base.OneTo, sz))
    CartesianIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} =
        CartesianIndices(map(i->first(i):last(i), inds))

    CartesianIndices(A::AbstractArray) = CartesianIndices(axes(A))

    convert(::Type{Tuple{}}, R::CartesianIndices{0}) = ()
    convert(::Type{NTuple{N,AbstractUnitRange{Int}}}, R::CartesianIndices{N}) where {N} =
        R.indices
    convert(::Type{NTuple{N,AbstractUnitRange}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,AbstractUnitRange{Int}}, R)
    convert(::Type{NTuple{N,UnitRange{Int}}}, R::CartesianIndices{N}) where {N} =
        UnitRange{Int}.(convert(NTuple{N,AbstractUnitRange}, R))
    convert(::Type{NTuple{N,UnitRange}}, R::CartesianIndices{N}) where {N} =
        UnitRange.(convert(NTuple{N,AbstractUnitRange}, R))
    convert(::Type{Tuple{Vararg{AbstractUnitRange{Int}}}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,AbstractUnitRange{Int}}, R)
    convert(::Type{Tuple{Vararg{AbstractUnitRange}}}, R::CartesianIndices) =
        convert(Tuple{Vararg{AbstractUnitRange{Int}}}, R)
    convert(::Type{Tuple{Vararg{UnitRange{Int}}}}, R::CartesianIndices{N}) where {N} =
        convert(NTuple{N,UnitRange{Int}}, R)
    convert(::Type{Tuple{Vararg{UnitRange}}}, R::CartesianIndices) =
        convert(Tuple{Vararg{UnitRange{Int}}}, R)

    # AbstractArray implementation
    Base.IndexStyle(::Type{CartesianIndices{N,R}}) where {N,R} = IndexCartesian()
    @inline Base.getindex(iter::CartesianIndices{N,R}, I::Vararg{Int, N}) where {N,R} = CartesianIndex(first.(iter.indices) .- 1 .+ I)

    ndims(R::CartesianIndices) = ndims(typeof(R))
    ndims(::Type{CartesianIndices{N}}) where {N} = N
    ndims(::Type{CartesianIndices{N,TT}}) where {N,TT} = N

    eachindex(::IndexCartesian, A::AbstractArray) = CartesianIndices(axes(A))

    @inline function eachindex(::IndexCartesian, A::AbstractArray, B::AbstractArray...)
        axsA = axes(A)
        Base._all_match_first(axes, axsA, B...) || Base.throw_eachindex_mismatch(IndexCartesian(), A, B...)
        CartesianIndices(axsA)
    end

    eltype(R::CartesianIndices) = eltype(typeof(R))
    eltype(::Type{CartesianIndices{N}}) where {N} = CartesianIndex{N}
    eltype(::Type{CartesianIndices{N,TT}}) where {N,TT} = CartesianIndex{N}
    IteratorSize(::Type{<:CartesianIndices}) = Base.HasShape()

    @inline function start(iter::CartesianIndices)
        iterfirst, iterlast = first(iter), last(iter)
        if any(map(>, iterfirst.I, iterlast.I))
            return iterlast+1
        end
        iterfirst
    end
    @inline function next(iter::CartesianIndices, state)
        state, CartesianIndex(inc(state.I, first(iter).I, last(iter).I))
    end
    # increment & carry
    @inline inc(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
    @inline inc(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int}) = (state[1]+1,)
    @inline function inc(state, start, stop)
        if state[1] < stop[1]
            return (state[1]+1,tail(state)...)
        end
        newtail = inc(tail(state), tail(start), tail(stop))
        (start[1], newtail...)
    end
    @inline done(iter::CartesianIndices, state) = state.I[end] > last(iter.indices[end])

    # 0-d cartesian ranges are special-cased to iterate once and only once
    start(iter::CartesianIndices{0}) = false
    next(iter::CartesianIndices{0}, state) = CartesianIndex(), true
    done(iter::CartesianIndices{0}, state) = state

    size(iter::CartesianIndices) = map(dimlength, first(iter).I, last(iter).I)
    dimlength(start, stop) = stop-start+1

    length(iter::CartesianIndices) = prod(size(iter))

    first(iter::CartesianIndices) = CartesianIndex(map(first, iter.indices))
    last(iter::CartesianIndices)  = CartesianIndex(map(last, iter.indices))

    @inline function in(i::CartesianIndex{N}, r::CartesianIndices{N}) where {N}
        _in(true, i.I, first(r).I, last(r).I)
    end
    _in(b, ::Tuple{}, ::Tuple{}, ::Tuple{}) = b
    @inline _in(b, i, start, stop) = _in(b & (start[1] <= i[1] <= stop[1]), tail(i), tail(start), tail(stop))

    simd_outer_range(iter::CartesianIndices{0}) = iter
    function simd_outer_range(iter::CartesianIndices)
        CartesianIndices(tail(iter.indices))
    end

    simd_inner_length(iter::CartesianIndices{0}, ::CartesianIndex) = 1
    simd_inner_length(iter::CartesianIndices, I::CartesianIndex) = length(iter.indices[1])

    simd_index(iter::CartesianIndices{0}, ::CartesianIndex, I1::Int) = first(iter)
    @inline function simd_index(iter::CartesianIndices, Ilast::CartesianIndex, I1::Int)
        CartesianIndex((I1+first(iter.indices[1]), Ilast.I...))
    end

    # Split out the first N elements of a tuple
    @inline function split(t, V::Val)
        ref = ntuple(d->true, V)  # create a reference tuple of length N
        _split1(t, ref), _splitrest(t, ref)
    end
    @inline _split1(t, ref) = (t[1], _split1(tail(t), tail(ref))...)
    @inline _splitrest(t, ref) = _splitrest(tail(t), tail(ref))
    # exit either when we've exhausted the input or reference tuple
    _split1(::Tuple{}, ::Tuple{}) = ()
    _split1(::Tuple{}, ref) = ()
    _split1(t, ::Tuple{}) = ()
    _splitrest(::Tuple{}, ::Tuple{}) = ()
    _splitrest(t, ::Tuple{}) = t
    _splitrest(::Tuple{}, ref) = ()

    @inline function split(I::CartesianIndex, V::Val)
        i, j = split(I.I, V)
        CartesianIndex(i), CartesianIndex(j)
    end
    function split(R::CartesianIndices, V::Val)
        i, j = split(R.indices, V)
        CartesianIndices(i), CartesianIndices(j)
    end

    # reversed CartesianIndices iteration
    @inline function start(r::Reverse{<:CartesianIndices})
        iterfirst, iterlast = last(r.itr), first(r.itr)
        if any(map(<, iterfirst.I, iterlast.I))
            return iterlast-1
        end
        iterfirst
    end
    @inline function next(r::Reverse{<:CartesianIndices}, state)
        state, CartesianIndex(dec(state.I, last(r.itr).I, first(r.itr).I))
    end
    # decrement & carry
    @inline dec(::Tuple{}, ::Tuple{}, ::Tuple{}) = ()
    @inline dec(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int}) = (state[1]-1,)
    @inline function dec(state, start, stop)
        if state[1] > stop[1]
            return (state[1]-1,tail(state)...)
        end
        newtail = dec(tail(state), tail(start), tail(stop))
        (start[1], newtail...)
    end
    @inline done(r::Reverse{<:CartesianIndices}, state) = state.I[end] < first(r.itr.indices[end])
    # 0-d cartesian ranges are special-cased to iterate once and only once
    start(iter::Reverse{<:CartesianIndices{0}}) = false
    next(iter::Reverse{<:CartesianIndices{0}}, state) = CartesianIndex(), true
    done(iter::Reverse{<:CartesianIndices{0}}, state) = state

    """
        LinearIndices(inds::CartesianIndices) -> R
        LinearIndices(sz::Dims) -> R
        LinearIndices(istart:istop, jstart:jstop, ...) -> R

    Define a mapping between cartesian indices and the corresponding linear index into a `CartesianIndices`.

    # Example

    The main purpose of this type is intuitive conversion from cartesian to linear indexing:

    ```jldoctest subarray
    julia> linear = LinearIndices(1:3,1:2)
    LinearIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}} with indices 1:3×1:2:
      1  4
      2  5
      3  6

    julia> linear[1,2]
    4
    ```
    """
    struct LinearIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{Int,N}
        indices::R
    end

    LinearIndices(inds::CartesianIndices{N,R}) where {N,R} = LinearIndices{N,R}(inds.indices)
    LinearIndices(::Tuple{}) = LinearIndices(CartesianIndices(()))
    LinearIndices(inds::NTuple{N,AbstractUnitRange{Int}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::Vararg{AbstractUnitRange{Int},N}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(inds::Vararg{AbstractUnitRange{<:Integer},N}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(index::CartesianIndex) = LinearIndices(CartesianIndices(index))
    LinearIndices(sz::NTuple{N,<:Integer}) where {N} = LinearIndices(CartesianIndices(sz))
    LinearIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} = LinearIndices(CartesianIndices(inds))
    LinearIndices(A::AbstractArray) = LinearIndices(CartesianIndices(A))

    # AbstractArray implementation
    Base.IndexStyle(::Type{LinearIndices{N,R}}) where {N,R} = IndexCartesian()
    Base.axes(iter::LinearIndices{N,R}) where {N,R} = iter.indices
    Base.size(iter::LinearIndices{N,R}) where {N,R} = length.(iter.indices)
    @inline function Base.getindex(iter::LinearIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        dims = length.(iter.indices)
        #without the inbounds, this is slower than Base._sub2ind(iter.indices, I...)
        @inbounds result = reshape(1:prod(dims), dims)[(I .- first.(iter.indices) .+ 1)...]
        return result
    end
end  # IteratorsMD


using .IteratorsMD

## Bounds-checking with CartesianIndex
# Disallow linear indexing with CartesianIndex
function checkbounds(::Type{Bool}, A::AbstractArray, i::Union{CartesianIndex, AbstractArray{<:CartesianIndex}})
    @_inline_meta
    checkbounds_indices(Bool, axes(A), (i,))
end

@inline checkbounds_indices(::Type{Bool}, ::Tuple{}, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, (), (I[1].I..., tail(I)...))
@inline checkbounds_indices(::Type{Bool}, IA::Tuple{Any}, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, IA, (I[1].I..., tail(I)...))
@inline checkbounds_indices(::Type{Bool}, IA::Tuple, I::Tuple{CartesianIndex,Vararg{Any}}) =
    checkbounds_indices(Bool, IA, (I[1].I..., tail(I)...))

# Indexing into Array with mixtures of Integers and CartesianIndices is
# extremely performance-sensitive. While the abstract fallbacks support this,
# codegen has extra support for SIMDification that sub2ind doesn't (yet) support
@propagate_inbounds getindex(A::Array, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    A[to_indices(A, (i1, I...))...]
@propagate_inbounds setindex!(A::Array, v, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    (A[to_indices(A, (i1, I...))...] = v; A)

# Support indexing with an array of CartesianIndex{N}s
# Here we try to consume N of the indices (if there are that many available)
# The first two simply handle ambiguities
@inline function checkbounds_indices(::Type{Bool}, ::Tuple{},
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}}) where N
    checkindex(Bool, (), I[1]) & checkbounds_indices(Bool, (), tail(I))
end
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple{Any},
        I::Tuple{AbstractArray{CartesianIndex{0}},Vararg{Any}})
    checkbounds_indices(Bool, IA, tail(I))
end
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple{Any},
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}}) where N
    checkindex(Bool, IA, I[1]) & checkbounds_indices(Bool, (), tail(I))
end
@inline function checkbounds_indices(::Type{Bool}, IA::Tuple,
        I::Tuple{AbstractArray{CartesianIndex{N}},Vararg{Any}}) where N
    IA1, IArest = IteratorsMD.split(IA, Val(N))
    checkindex(Bool, IA1, I[1]) & checkbounds_indices(Bool, IArest, tail(I))
end

function checkindex(::Type{Bool}, inds::Tuple, I::AbstractArray{<:CartesianIndex})
    b = true
    for i in I
        b &= checkbounds_indices(Bool, inds, (i,))
    end
    b
end

# combined count of all indices, including CartesianIndex and
# AbstractArray{CartesianIndex}
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferrable
@inline index_ndims(i1, I...) = (true, index_ndims(I...)...)
@inline function index_ndims(i1::CartesianIndex, I...)
    (map(x->true, i1.I)..., index_ndims(I...)...)
end
@inline function index_ndims(i1::AbstractArray{CartesianIndex{N}}, I...) where N
    (ntuple(x->true, Val(N))..., index_ndims(I...)...)
end
index_ndims() = ()

# combined dimensionality of all indices
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferrable
@inline index_dimsum(i1, I...) = (index_dimsum(I...)...,)
@inline index_dimsum(::Colon, I...) = (true, index_dimsum(I...)...)
@inline index_dimsum(::AbstractArray{Bool}, I...) = (true, index_dimsum(I...)...)
@inline function index_dimsum(::AbstractArray{<:Any,N}, I...) where N
    (ntuple(x->true, Val(N))..., index_dimsum(I...)...)
end
index_dimsum() = ()

# Recursively compute the lengths of a list of indices, without dropping scalars
index_lengths() = ()
@inline index_lengths(::Real, rest...) = (1, index_lengths(rest...)...)
@inline index_lengths(A::AbstractArray, rest...) = (length(A), index_lengths(rest...)...)
@inline index_lengths(A::Slice, rest...) = (length(indices1(A)), index_lengths(rest...)...)

# shape of array to create for getindex() with indices I, dropping scalars
# returns a Tuple{Vararg{AbstractUnitRange}} of indices
index_shape() = ()
@inline index_shape(::Real, rest...) = index_shape(rest...)
@inline index_shape(A::AbstractArray, rest...) = (axes(A)..., index_shape(rest...)...)

"""
    LogicalIndex(mask)

The `LogicalIndex` type is a special vector that simply contains all indices I
where `mask[I]` is true. This specialized type does not support indexing
directly as doing so would require O(n) lookup time. `AbstractArray{Bool}` are
wrapped with `LogicalIndex` upon calling [`to_indices`](@ref).
"""
struct LogicalIndex{T, A<:AbstractArray{Bool}} <: AbstractVector{T}
    mask::A
    sum::Int
    LogicalIndex{T,A}(mask::A) where {T,A<:AbstractArray{Bool}} = new(mask, count(mask))
end
LogicalIndex(mask::AbstractVector{Bool}) = LogicalIndex{Int, typeof(mask)}(mask)
LogicalIndex(mask::AbstractArray{Bool, N}) where {N} = LogicalIndex{CartesianIndex{N}, typeof(mask)}(mask)
(::Type{LogicalIndex{Int}})(mask::AbstractArray) = LogicalIndex{Int, typeof(mask)}(mask)
size(L::LogicalIndex) = (L.sum,)
length(L::LogicalIndex) = L.sum
collect(L::LogicalIndex) = [i for i in L]
show(io::IO, r::LogicalIndex) = print(io, "Base.LogicalIndex(", r.mask, ")")
# Iteration over LogicalIndex is very performance-critical, but it also must
# support arbitrary AbstractArray{Bool}s with both Int and CartesianIndex.
# Thus the iteration state contains an index iterator and its state. We also
# keep track of the count of elements since we already know how many there
# should be -- this way we don't need to look at future indices to check done.
@inline function start(L::LogicalIndex{Int})
    r = linearindices(L.mask)
    return (r, start(r), 1)
end
@inline function start(L::LogicalIndex{<:CartesianIndex})
    r = CartesianIndices(axes(L.mask))
    return (r, start(r), 1)
end
@propagate_inbounds function next(L::LogicalIndex, s)
    # We're looking for the n-th true element, using iterator r at state i
    r, i, n = s
    while true
        done(r, i) # Call done(r, i) for the iteration protocol, but trust done(L, s) was called
        idx, i = next(r, i)
        L.mask[idx] && return (idx, (r, i, n+1))
    end
end
done(L::LogicalIndex, s) = s[3] > length(L)
# When wrapping a BitArray, lean heavily upon its internals -- this is a common
# case. Just use the Int index and count as its state.
@inline start(L::LogicalIndex{Int,<:BitArray}) = (0, 1)
@inline function next(L::LogicalIndex{Int,<:BitArray}, s)
    i, n = s
    Bc = L.mask.chunks
    while true
        if Bc[_div64(i)+1] & (UInt64(1)<<_mod64(i)) != 0
            i += 1
            return (i, (i, n+1))
        end
        i += 1
    end
end
@inline done(L::LogicalIndex{Int,<:BitArray}, s) = s[2] > length(L)

@inline checkbounds(::Type{Bool}, A::AbstractArray, I::LogicalIndex{<:Any,<:AbstractArray{Bool,1}}) =
    linearindices(A) == linearindices(I.mask)
@inline checkbounds(::Type{Bool}, A::AbstractArray, I::LogicalIndex) = axes(A) == axes(I.mask)
@inline checkindex(::Type{Bool}, indx::AbstractUnitRange, I::LogicalIndex) = (indx,) == axes(I.mask)
checkindex(::Type{Bool}, inds::Tuple, I::LogicalIndex) = false

ensure_indexable(I::Tuple{}) = ()
@inline ensure_indexable(I::Tuple{Any, Vararg{Any}}) = (I[1], ensure_indexable(tail(I))...)
@inline ensure_indexable(I::Tuple{LogicalIndex, Vararg{Any}}) = (collect(I[1]), ensure_indexable(tail(I))...)

# In simple cases, we know that we don't need to use axes(A). Optimize those
# until Julia gets smart enough to elide the call on its own:
to_indices(A, I::Tuple{}) = ()
@inline to_indices(A, I::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = to_indices(A, (), I)
# But some index types require more context spanning multiple indices
# CartesianIndexes are simple; they just splat out
@inline to_indices(A, inds, I::Tuple{CartesianIndex, Vararg{Any}}) =
    to_indices(A, inds, (I[1].I..., tail(I)...))
# But for arrays of CartesianIndex, we just skip the appropriate number of inds
@inline function to_indices(A, inds, I::Tuple{AbstractArray{CartesianIndex{N}}, Vararg{Any}}) where N
    _, indstail = IteratorsMD.split(inds, Val(N))
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# And boolean arrays behave similarly; they also skip their number of dimensions
@inline function to_indices(A, inds, I::Tuple{AbstractArray{Bool, N}, Vararg{Any}}) where N
    _, indstail = IteratorsMD.split(inds, Val(N))
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# As an optimization, we allow trailing Array{Bool} and BitArray to be linear over trailing dimensions
@inline to_indices(A, inds, I::Tuple{Union{Array{Bool,N}, BitArray{N}}}) where {N} =
    (_maybe_linear_logical_index(IndexStyle(A), A, I[1]),)
_maybe_linear_logical_index(::IndexStyle, A, i) = to_index(A, i)
_maybe_linear_logical_index(::IndexLinear, A, i) = LogicalIndex{Int}(i)

# Colons get converted to slices by `uncolon`
@inline to_indices(A, inds, I::Tuple{Colon, Vararg{Any}}) =
    (uncolon(inds, I), to_indices(A, _maybetail(inds), tail(I))...)

const CI0 = Union{CartesianIndex{0}, AbstractArray{CartesianIndex{0}}}
uncolon(inds::Tuple{},    I::Tuple{Colon, Vararg{Any}}) = Slice(OneTo(1))
uncolon(inds::Tuple,      I::Tuple{Colon, Vararg{Any}}) = Slice(inds[1])

### From abstractarray.jl: Internal multidimensional indexing definitions ###
getindex(x::Number, i::CartesianIndex{0}) = x
getindex(t::Tuple,  i::CartesianIndex{1}) = getindex(t, i.I[1])

# These are not defined on directly on getindex to avoid
# ambiguities for AbstractArray subtypes. See the note in abstractarray.jl

@inline function _getindex(l::IndexStyle, A::AbstractArray, I::Union{Real, AbstractArray}...)
    @boundscheck checkbounds(A, I...)
    return _unsafe_getindex(l, _maybe_reshape(l, A, I...), I...)
end
# But we can speed up IndexCartesian arrays by reshaping them to the appropriate dimensionality:
_maybe_reshape(::IndexLinear, A::AbstractArray, I...) = A
_maybe_reshape(::IndexCartesian, A::AbstractVector, I...) = A
@inline _maybe_reshape(::IndexCartesian, A::AbstractArray, I...) = __maybe_reshape(A, index_ndims(I...))
@inline __maybe_reshape(A::AbstractArray{T,N}, ::NTuple{N,Any}) where {T,N} = A
@inline __maybe_reshape(A::AbstractArray, ::NTuple{N,Any}) where {N} = reshape(A, Val(N))

function _unsafe_getindex(::IndexStyle, A::AbstractArray, I::Vararg{Union{Real, AbstractArray}, N}) where N
    # This is specifically not inlined to prevent excessive allocations in type unstable code
    shape = index_shape(I...)
    dest = similar(A, shape)
    map(unsafe_length, axes(dest)) == map(unsafe_length, shape) || throw_checksize_error(dest, shape)
    _unsafe_getindex!(dest, A, I...) # usually a generated function, don't allow it to impact inference result
    return dest
end

# Always index with the exactly indices provided.
@generated function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Vararg{Union{Real, AbstractArray}, N}) where N
    quote
        @_inline_meta
        D = eachindex(dest)
        Ds = start(D)
        @inbounds @nloops $N j d->I[d] begin
            d, Ds = next(D, Ds)
            dest[d] = @ncall $N getindex src j
        end
        return dest
    end
end

@noinline throw_checksize_error(A, sz) = throw(DimensionMismatch("output array is the wrong size; expected $sz, got $(size(A))"))

## setindex! ##
function _setindex!(l::IndexStyle, A::AbstractArray, x, I::Union{Real, AbstractArray}...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    _unsafe_setindex!(l, _maybe_reshape(l, A, I...), x, I...)
    A
end

_iterable(v::AbstractArray) = v
_iterable(v) = Iterators.repeated(v)
@generated function _unsafe_setindex!(::IndexStyle, A::AbstractArray, x, I::Union{Real,AbstractArray}...)
    N = length(I)
    quote
        X = _iterable(x)
        @nexprs $N d->(I_d = I[d])
        idxlens = @ncall $N index_lengths I
        @ncall $N setindex_shape_check X (d->idxlens[d])
        Xs = start(X)
        @inbounds @nloops $N i d->I_d begin
            v, Xs = next(X, Xs)
            @ncall $N setindex! A v i
        end
        A
    end
end

##

# see discussion in #18364 ... we try not to widen type of the resulting array
# from cumsum or cumprod, but in some cases (+, Bool) we may not have a choice.
rcum_promote_type(op, ::Type{T}, ::Type{S}) where {T,S<:Number} = promote_op(op, T, S)
rcum_promote_type(op, ::Type{T}) where {T<:Number} = rcum_promote_type(op, T,T)
rcum_promote_type(op, ::Type{T}) where {T} = T

# handle sums of Vector{Bool} and similar.   it would be nice to handle
# any AbstractArray here, but it's not clear how that would be possible
rcum_promote_type(op, ::Type{Array{T,N}}) where {T,N} = Array{rcum_promote_type(op,T), N}

# accumulate_pairwise slightly slower then accumulate, but more numerically
# stable in certain situations (e.g. sums).
# it does double the number of operations compared to accumulate,
# though for cheap operations like + this does not have much impact (20%)
function _accumulate_pairwise!(op::Op, c::AbstractVector{T}, v::AbstractVector, s, i1, n)::T where {T,Op}
    @inbounds if n < 128
        s_ = v[i1]
        c[i1] = op(s, s_)
        for i = i1+1:i1+n-1
            s_ = op(s_, v[i])
            c[i] = op(s, s_)
        end
    else
        n2 = n >> 1
        s_ = _accumulate_pairwise!(op, c, v, s, i1, n2)
        s_ = op(s_, _accumulate_pairwise!(op, c, v, op(s, s_), i1+n2, n-n2))
    end
    return s_
end

function accumulate_pairwise!(op::Op, result::AbstractVector, v::AbstractVector) where Op
    li = linearindices(v)
    li != linearindices(result) && throw(DimensionMismatch("input and output array sizes and indices must match"))
    n = length(li)
    n == 0 && return result
    i1 = first(li)
    @inbounds result[i1] = v1 = v[i1]
    n == 1 && return result
    _accumulate_pairwise!(op, result, v, v1, i1+1, n-1)
    return result
end

function accumulate_pairwise(op, v::AbstractVector{T}) where T
    out = similar(v, rcum_promote_type(op, T))
    return accumulate_pairwise!(op, out, v)
end

function cumsum!(out, v::AbstractVector, dim::Integer)
    # we dispatch on the possibility of numerical stability issues
    _cumsum!(out, v, dim, ArithmeticStyle(eltype(out)))
end

function _cumsum!(out, v, dim, ::ArithmeticRounds)
    dim == 1 ? accumulate_pairwise!(+, out, v) : copyto!(out, v)
end
function _cumsum!(out, v, dim, ::ArithmeticUnknown)
    _cumsum!(out, v, dim, ArithmeticRounds())
end
function _cumsum!(out, v, dim, ::ArithmeticStyle)
    dim == 1 ? accumulate!(+, out, v) : copyto!(out, v)
end

"""
    cumsum(A, dim::Integer)

Cumulative sum along the dimension `dim`. See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumsum(a,1)
2×3 Array{Int64,2}:
 1  2  3
 5  7  9

julia> cumsum(a,2)
2×3 Array{Int64,2}:
 1  3   6
 4  9  15
```
"""
function cumsum(A::AbstractArray{T}, dim::Integer) where T
    out = similar(A, rcum_promote_type(+, T))
    cumsum!(out, A, dim)
end

"""
    cumsum(x::AbstractVector)

Cumulative sum a vector. See also [`cumsum!`](@ref)
to use a preallocated output array, both for performance and to control the precision of the
output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> cumsum([1, 1, 1])
3-element Array{Int64,1}:
 1
 2
 3

julia> cumsum([fill(1, 2) for i in 1:3])
3-element Array{Array{Int64,1},1}:
 [1, 1]
 [2, 2]
 [3, 3]
```
"""
cumsum(x::AbstractVector) = cumsum(x, 1)

"""
    cumsum!(B, A, dim::Integer)

Cumulative sum of `A` along the dimension `dim`, storing the result in `B`. See also [`cumsum`](@ref).
"""
cumsum!(B, A, dim::Integer) = accumulate!(+, B, A, dim)

"""
    cumsum!(y::AbstractVector, x::AbstractVector)

Cumulative sum of a vector `x`, storing the result in `y`. See also [`cumsum`](@ref).
"""
cumsum!(y::AbstractVector, x::AbstractVector) = cumsum!(y, x, 1)

"""
    cumprod(A, dim::Integer)

Cumulative product along the dimension `dim`. See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> cumprod(a,1)
2×3 Array{Int64,2}:
 1   2   3
 4  10  18

julia> cumprod(a,2)
2×3 Array{Int64,2}:
 1   2    6
 4  20  120
```
"""
cumprod(A::AbstractArray, dim::Integer) = accumulate(*, A, dim)

"""
    cumprod(x::AbstractVector)

Cumulative product of a vector. See also
[`cumprod!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow).

# Examples
```jldoctest
julia> cumprod(fill(1//2, 3))
3-element Array{Rational{Int64},1}:
 1//2
 1//4
 1//8

julia> cumprod([fill(1//3, 2, 2) for i in 1:3])
3-element Array{Array{Rational{Int64},2},1}:
 Rational{Int64}[1//3 1//3; 1//3 1//3]
 Rational{Int64}[2//9 2//9; 2//9 2//9]
 Rational{Int64}[4//27 4//27; 4//27 4//27]
```
"""
cumprod(x::AbstractVector) = cumprod(x, 1)

"""
    cumprod!(B, A, dim::Integer)

Cumulative product of `A` along the dimension `dim`, storing the result in `B`.
See also [`cumprod`](@ref).
"""
cumprod!(B, A, dim::Integer) = accumulate!(*, B, A, dim)

"""
    cumprod!(y::AbstractVector, x::AbstractVector)

Cumulative product of a vector `x`, storing the result in `y`.
See also [`cumprod`](@ref).
"""
cumprod!(y::AbstractVector, x::AbstractVector) = cumprod!(y, x, 1)

"""
    accumulate(op, A, dim::Integer)

Cumulative operation `op` along the dimension `dim`. See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

# Examples
```jldoctest
julia> accumulate(+, fill(1, 3, 3), 1)
3×3 Array{Int64,2}:
 1  1  1
 2  2  2
 3  3  3

julia> accumulate(+, fill(1, 3, 3), 2)
3×3 Array{Int64,2}:
 1  2  3
 1  2  3
 1  2  3
```
"""
function accumulate(op, A, dim::Integer)
    out = similar(A, rcum_promote_type(op, eltype(A)))
    accumulate!(op, out, A, dim)
end

"""
    accumulate(op, x::AbstractVector)

Cumulative operation `op` on a vector. See also
[`accumulate!`](@ref) to use a preallocated output array, both for performance and
to control the precision of the output (e.g. to avoid overflow). For common operations
there are specialized variants of `accumulate`, see:
[`cumsum`](@ref), [`cumprod`](@ref)

# Examples
```jldoctest
julia> accumulate(+, [1,2,3])
3-element Array{Int64,1}:
 1
 3
 6

julia> accumulate(*, [1,2,3])
3-element Array{Int64,1}:
 1
 2
 6
```
"""
accumulate(op, x::AbstractVector) = accumulate(op, x, 1)

"""
    accumulate!(op, B, A, dim::Integer)

Cumulative operation `op` on `A` along the dimension `dim`, storing the result in `B`.
See also [`accumulate`](@ref).

# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> B = [0 0; 0 0];

julia> accumulate!(-, B, A, 1);

julia> B
2×2 Array{Int64,2}:
  1   2
 -2  -2

julia> accumulate!(-, B, A, 2);

julia> B
2×2 Array{Int64,2}:
 1  -1
 3  -1
```
"""
function accumulate!(op, B, A, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    inds_t = axes(A)
    axes(B) == inds_t || throw(DimensionMismatch("shape of B must match A"))
    dim > ndims(A) && return copyto!(B, A)
    isempty(inds_t[dim]) && return B
    if dim == 1
        # We can accumulate to a temporary variable, which allows
        # register usage and will be slightly faster
        ind1 = inds_t[1]
        @inbounds for I in CartesianIndices(tail(inds_t))
            tmp = convert(eltype(B), A[first(ind1), I])
            B[first(ind1), I] = tmp
            for i_1 = first(ind1)+1:last(ind1)
                tmp = op(tmp, A[i_1, I])
                B[i_1, I] = tmp
            end
        end
    else
        R1 = CartesianIndices(axes(A)[1:dim-1])   # not type-stable
        R2 = CartesianIndices(axes(A)[dim+1:end])
        _accumulate!(op, B, A, R1, inds_t[dim], R2) # use function barrier
    end
    return B
end

"""
    accumulate!(op, y, x::AbstractVector)

Cumulative operation `op` on a vector `x`, storing the result in `y`.
See also [`accumulate`](@ref).

# Examples
``jldoctest
julia> x = SparseVector(7, [1, 3, 5], [2., 4., 5.]);

julia> y = zeros(7);

julia> accumulate!(+, y, x);

julia> y
7-element Array{Float64,1}:
  2.0
  2.0
  6.0
  6.0
 11.0
 11.0
 11.0
```
"""
function accumulate!(op::Op, y, x::AbstractVector) where Op
    isempty(x) && return y
    v1 = first(x)
    _accumulate1!(op, y, v1, x, 1)
end

@noinline function _accumulate!(op, B, A, R1, ind, R2)
    # Copy the initial element in each 1d vector along dimension `dim`
    ii = first(ind)
    @inbounds for J in R2, I in R1
        B[I, ii, J] = A[I, ii, J]
    end
    # Accumulate
    @inbounds for J in R2, i in first(ind)+1:last(ind), I in R1
        B[I, i, J] = op(B[I, i-1, J], A[I, i, J])
    end
    B
end

"""
    accumulate(op, v0, x::AbstractVector)

Like `accumulate`, but using a starting element `v0`. The first entry of the result will be
`op(v0, first(A))`.

# Examples
```jldoctest
julia> accumulate(+, 100, [1,2,3])
3-element Array{Int64,1}:
 101
 103
 106

julia> accumulate(min, 0, [1,2,-1])
3-element Array{Int64,1}:
  0
  0
 -1
```
"""
function accumulate(op, v0, x::AbstractVector)
    T = rcum_promote_type(op, typeof(v0), eltype(x))
    out = similar(x, T)
    accumulate!(op, out, v0, x)
end

function accumulate!(op, y, v0, x::AbstractVector)
    isempty(x) && return y
    v1 = op(v0, first(x))
    _accumulate1!(op, y, v1, x, 1)
end

function _accumulate1!(op, B, v1, A::AbstractVector, dim::Integer)
    dim > 0 || throw(ArgumentError("dim must be a positive integer"))
    inds = linearindices(A)
    inds == linearindices(B) || throw(DimensionMismatch("linearindices of A and B don't match"))
    dim > 1 && return copyto!(B, A)
    i1 = inds[1]
    cur_val = v1
    B[i1] = cur_val
    @inbounds for i in inds[2:end]
        cur_val = op(cur_val, A[i])
        B[i] = cur_val
    end
    return B
end

### from abstractarray.jl

"""
    fill!(A, x)

Fill array `A` with the value `x`. If `x` is an object reference, all elements will refer to
the same object. `fill!(A, Foo())` will return `A` filled with the result of evaluating
`Foo()` once.

# Examples
```jldoctest
julia> A = zeros(2,3)
2×3 Array{Float64,2}:
 0.0  0.0  0.0
 0.0  0.0  0.0

julia> fill!(A, 2.)
2×3 Array{Float64,2}:
 2.0  2.0  2.0
 2.0  2.0  2.0

julia> a = [1, 1, 1]; A = fill!(Vector{Vector{Int}}(uninitialized, 3), a); a[1] = 2; A
3-element Array{Array{Int64,1},1}:
 [2, 1, 1]
 [2, 1, 1]
 [2, 1, 1]

julia> x = 0; f() = (global x += 1; x); fill!(Vector{Int}(uninitialized, 3), f())
3-element Array{Int64,1}:
 1
 1
 1
```
"""
function fill!(A::AbstractArray{T}, x) where T
    xT = convert(T, x)
    for I in eachindex(A)
        @inbounds A[I] = xT
    end
    A
end

"""
    copyto!(dest::AbstractArray, src) -> dest


Copy all elements from collection `src` to array `dest`, whose length must be greater than
or equal to the length `n` of `src`. The first `n` elements of `dest` are overwritten,
the other elements are left untouched.

# Examples
```jldoctest
julia> x = [1., 0., 3., 0., 5.];

julia> y = zeros(7);

julia> copyto!(y, x);

julia> y
7-element Array{Float64,1}:
 1.0
 0.0
 3.0
 0.0
 5.0
 0.0
 0.0
```
"""
copyto!(dest, src)

function copyto!(dest::AbstractArray{T,N}, src::AbstractArray{T,N}) where {T,N}
    @boundscheck checkbounds(dest, axes(src)...)
    for I in eachindex(IndexStyle(src,dest), src)
        @inbounds dest[I] = src[I]
    end
    dest
end

function copyto!(dest::AbstractArray{T1,N}, Rdest::CartesianIndices{N},
               src::AbstractArray{T2,N}, Rsrc::CartesianIndices{N}) where {T1,T2,N}
    isempty(Rdest) && return dest
    if size(Rdest) != size(Rsrc)
        throw(ArgumentError("source and destination must have same size (got $(size(Rsrc)) and $(size(Rdest)))"))
    end
    @boundscheck checkbounds(dest, first(Rdest))
    @boundscheck checkbounds(dest, last(Rdest))
    @boundscheck checkbounds(src, first(Rsrc))
    @boundscheck checkbounds(src, last(Rsrc))
    ΔI = first(Rdest) - first(Rsrc)
    if @generated
        quote
            @nloops $N i (n->Rsrc.indices[n]) begin
                @inbounds @nref($N,dest,n->i_n+ΔI[n]) = @nref($N,src,i)
            end
        end
    else
        for I in Rsrc
            @inbounds dest[I + ΔI] = src[I]
        end
    end
    dest
end

"""
    copyto!(dest, Rdest::CartesianIndices, src, Rsrc::CartesianIndices) -> dest

Copy the block of `src` in the range of `Rsrc` to the block of `dest`
in the range of `Rdest`. The sizes of the two regions must match.
"""
copyto!(::AbstractArray, ::CartesianIndices, ::AbstractArray, ::CartesianIndices)

# circshift!
circshift!(dest::AbstractArray, src, ::Tuple{}) = copyto!(dest, src)
"""
    circshift!(dest, src, shifts)

Circularly shift, i.e. rotate, the data in `src`, storing the result in
`dest`. `shifts` specifies the amount to shift in each dimension.

The `dest` array must be distinct from the `src` array (they cannot
alias each other).

See also [`circshift`](@ref).
"""
@noinline function circshift!(dest::AbstractArray{T,N}, src, shiftamt::DimsInteger) where {T,N}
    dest === src && throw(ArgumentError("dest and src must be separate arrays"))
    inds = axes(src)
    axes(dest) == inds || throw(ArgumentError("indices of src and dest must match (got $inds and $(axes(dest)))"))
    _circshift!(dest, (), src, (), inds, fill_to_length(shiftamt, 0, Val(N)))
end
circshift!(dest::AbstractArray, src, shiftamt) = circshift!(dest, src, (shiftamt...,))

# For each dimension, we copy the first half of src to the second half
# of dest, and the second half of src to the first half of dest. This
# uses a recursive bifurcation strategy so that these splits can be
# encoded by ranges, which means that we need only one call to `mod`
# per dimension rather than one call per index.
# `rdest` and `rsrc` are tuples-of-ranges that grow one dimension at a
# time; when all the dimensions have been filled in, you call `copyto!`
# for that block. In other words, in two dimensions schematically we
# have the following call sequence (--> means a call):
#   circshift!(dest, src, shiftamt) -->
#     _circshift!(dest, src, ("first half of dim1",)) -->
#       _circshift!(dest, src, ("first half of dim1", "first half of dim2")) --> copyto!
#       _circshift!(dest, src, ("first half of dim1", "second half of dim2")) --> copyto!
#     _circshift!(dest, src, ("second half of dim1",)) -->
#       _circshift!(dest, src, ("second half of dim1", "first half of dim2")) --> copyto!
#       _circshift!(dest, src, ("second half of dim1", "second half of dim2")) --> copyto!
@inline function _circshift!(dest, rdest, src, rsrc,
                             inds::Tuple{AbstractUnitRange,Vararg{Any}},
                             shiftamt::Tuple{Integer,Vararg{Any}})
    ind1, d = inds[1], shiftamt[1]
    s = mod(d, length(ind1))
    sf, sl = first(ind1)+s, last(ind1)-s
    r1, r2 = first(ind1):sf-1, sf:last(ind1)
    r3, r4 = first(ind1):sl, sl+1:last(ind1)
    tinds, tshiftamt = tail(inds), tail(shiftamt)
    _circshift!(dest, (rdest..., r1), src, (rsrc..., r4), tinds, tshiftamt)
    _circshift!(dest, (rdest..., r2), src, (rsrc..., r3), tinds, tshiftamt)
end
# At least one of inds, shiftamt is empty
function _circshift!(dest, rdest, src, rsrc, inds, shiftamt)
    copyto!(dest, CartesianIndices(rdest), src, CartesianIndices(rsrc))
end

# circcopy!
"""
    circcopy!(dest, src)

Copy `src` to `dest`, indexing each dimension modulo its length.
`src` and `dest` must have the same size, but can be offset in
their indices; any offset results in a (circular) wraparound. If the
arrays have overlapping indices, then on the domain of the overlap
`dest` agrees with `src`.

# Examples
```julia-repl
julia> src = reshape(Vector(1:16), (4,4))
4×4 Array{Int64,2}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> dest = OffsetArray{Int}(uninitialized, (0:3,2:5))

julia> circcopy!(dest, src)
OffsetArrays.OffsetArray{Int64,2,Array{Int64,2}} with indices 0:3×2:5:
 8  12  16  4
 5   9  13  1
 6  10  14  2
 7  11  15  3

julia> dest[1:3,2:4] == src[1:3,2:4]
true
```
"""
function circcopy!(dest, src)
    dest === src && throw(ArgumentError("dest and src must be separate arrays"))
    indssrc, indsdest = axes(src), axes(dest)
    if (szsrc = map(length, indssrc)) != (szdest = map(length, indsdest))
        throw(DimensionMismatch("src and dest must have the same sizes (got $szsrc and $szdest)"))
    end
    shift = map((isrc, idest)->first(isrc)-first(idest), indssrc, indsdest)
    all(x->x==0, shift) && return copyto!(dest, src)
    _circcopy!(dest, (), indsdest, src, (), indssrc)
end

# This uses the same strategy described above for _circshift!
@inline function _circcopy!(dest, rdest, indsdest::Tuple{AbstractUnitRange,Vararg{Any}},
                            src,  rsrc,  indssrc::Tuple{AbstractUnitRange,Vararg{Any}})
    indd1, inds1 = indsdest[1], indssrc[1]
    l = length(indd1)
    s = mod(first(inds1)-first(indd1), l)
    sdf = first(indd1)+s
    rd1, rd2 = first(indd1):sdf-1, sdf:last(indd1)
    ssf = last(inds1)-s
    rs1, rs2 = first(inds1):ssf, ssf+1:last(inds1)
    tindsd, tindss = tail(indsdest), tail(indssrc)
    _circcopy!(dest, (rdest..., rd1), tindsd, src, (rsrc..., rs2), tindss)
    _circcopy!(dest, (rdest..., rd2), tindsd, src, (rsrc..., rs1), tindss)
end

# At least one of indsdest, indssrc are empty (and both should be, since we've checked)
function _circcopy!(dest, rdest, indsdest, src, rsrc, indssrc)
    copyto!(dest, CartesianIndices(rdest), src, CartesianIndices(rsrc))
end

### BitArrays

## getindex

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!
@inline function _unsafe_getindex!(X::BitArray, B::BitArray, I0::Union{UnitRange{Int},Slice})
    copy_chunks!(X.chunks, 1, B.chunks, indexoffset(I0)+1, length(I0))
    return X
end

# Optimization where the inner dimension is contiguous improves perf dramatically
@generated function _unsafe_getindex!(X::BitArray, B::BitArray,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        @nexprs $N d->(I_d = I[d])

        idxlens = @ncall $N index_lengths I0 I

        f0 = indexoffset(I0)+1
        l0 = idxlens[1]

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I_d)
            gap_lst_{d+1} *= stride
        end

        storeind = 1
        Xc, Bc = X.chunks, B.chunks
        @nloops($N, i, d->(1:idxlens[d+1]),
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_chunks!(Xc, storeind, Bc, ind, l0)
                    storeind += l0
                end)
        return X
    end
end

# in the general multidimensional non-scalar case, can we do about 10% better
# in most cases by manually hoisting the bitarray chunks access out of the loop
# (This should really be handled by the compiler or with an immutable BitArray)
@generated function _unsafe_getindex!(X::BitArray, B::BitArray, I::Union{Int,AbstractArray{Int}}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(B, d))
        $(Symbol(:offset_, N)) = 1
        ind = 0
        Xc, Bc = X.chunks, B.chunks
        @nloops $N i d->I[d] d->(@inbounds offset_{d-1} = offset_d + (i_d-1)*stride_d) begin
            ind += 1
            unsafe_bitsetindex!(Xc, unsafe_bitgetindex(Bc, offset_0), ind)
        end
        return X
    end
end

## setindex!

function copy_to_bitarray_chunks!(Bc::Vector{UInt64}, pos_d::Int, C::StridedArray, pos_s::Int, numbits::Int)
    bind = pos_d
    cind = pos_s
    lastind = pos_d + numbits - 1
    @inbounds while bind ≤ lastind
        unsafe_bitsetindex!(Bc, Bool(C[cind]), bind)
        bind += 1
        cind += 1
    end
end

# Note: the next two functions rely on the following definition of the conversion to Bool:
#   convert(::Type{Bool}, x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(...))
# they're used to pre-emptively check in bulk when possible, which is much faster.
# Also, the functions can be overloaded for custom types T<:Real :
#  a) in the unlikely eventuality that they use a different logic for Bool conversion
#  b) to skip the check if not necessary
@inline try_bool_conversion(x::Real) =
    x == 0 || x == 1 || throw(InexactError(:try_bool_conversion, Bool, x))
@inline unchecked_bool_convert(x::Real) = x == 1

function copy_to_bitarray_chunks!(Bc::Vector{UInt64}, pos_d::Int, C::StridedArray{<:Real}, pos_s::Int, numbits::Int)
    @inbounds for i = (1:numbits) .+ (pos_s - 1)
        try_bool_conversion(C[i])
    end

    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)

    delta_kd = kd1 - kd0

    u = _msk64
    if delta_kd == 0
        msk_d0 = msk_d1 = ~(u << ld0) | (u << (ld1+1))
        lt0 = ld1
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << (ld1+1))
        lt0 = 63
    end

    bind = kd0
    ind = pos_s
    @inbounds if ld0 > 0
        c = UInt64(0)
        for j = ld0:lt0
            c |= (UInt64(unchecked_bool_convert(C[ind])) << j)
            ind += 1
        end
        Bc[kd0] = (Bc[kd0] & msk_d0) | (c & ~msk_d0)
        bind += 1
    end

    nc = _div64(numbits - ind + pos_s)
    @inbounds for i = 1:nc
        c = UInt64(0)
        for j = 0:63
            c |= (UInt64(unchecked_bool_convert(C[ind])) << j)
            ind += 1
        end
        Bc[bind] = c
        bind += 1
    end

    @inbounds if bind ≤ kd1
        @assert bind == kd1
        c = UInt64(0)
        for j = 0:ld1
            c |= (UInt64(unchecked_bool_convert(C[ind])) << j)
            ind += 1
        end
        Bc[kd1] = (Bc[kd1] & msk_d1) | (c & ~msk_d1)
    end
end

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

@inline function setindex!(B::BitArray, X::StridedArray, J0::Union{Colon,UnitRange{Int}})
    I0 = to_indices(B, (J0,))[1]
    @boundscheck checkbounds(B, I0)
    l0 = length(I0)
    setindex_shape_check(X, l0)
    l0 == 0 && return B
    f0 = indexoffset(I0)+1
    copy_to_bitarray_chunks!(B.chunks, f0, X, 1, l0)
    return B
end

@inline function setindex!(B::BitArray, X::StridedArray,
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, X, J...)
end
@generated function _unsafe_setindex!(B::BitArray, X::StridedArray,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        idxlens = @ncall $N index_lengths I0 d->I[d]
        @ncall $N setindex_shape_check X idxlens[1] d->idxlens[d+1]
        isempty(X) && return B
        f0 = indexoffset(I0)+1
        l0 = idxlens[1]

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I[d])
            gap_lst_{d+1} *= stride
        end

        refind = 1
        Bc = B.chunks
        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                begin # BODY
                    copy_to_bitarray_chunks!(Bc, ind, X, refind, l0)
                    refind += l0
                end)

        return B
    end
end

@inline function setindex!(B::BitArray, x,
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, x, J...)
end
@generated function _unsafe_setindex!(B::BitArray, x,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Slice}...)
    N = length(I)
    quote
        y = Bool(x)
        idxlens = @ncall $N index_lengths I0 d->I[d]

        f0 = indexoffset(I0)+1
        l0 = idxlens[1]
        l0 == 0 && return B
        @nexprs $N d->(isempty(I[d]) && return B)

        gap_lst_1 = 0
        @nexprs $N d->(gap_lst_{d+1} = idxlens[d+1])
        stride = 1
        ind = f0
        @nexprs $N d->begin
            stride *= size(B, d)
            stride_lst_d = stride
            ind += stride * indexoffset(I[d])
            gap_lst_{d+1} *= stride
        end

        @nloops($N, i, d->I[d],
                d->nothing, # PRE
                d->(ind += stride_lst_d - gap_lst_d), # POST
                fill_chunks!(B.chunks, y, ind, l0) # BODY
                )

        return B
    end
end

## isassigned

@generated function isassigned(B::BitArray, I_0::Int, I::Int...)
    N = length(I)
    quote
        @nexprs $N d->(I_d = I[d])
        stride = 1
        index = I_0
        @nexprs $N d->begin
            l = size(B,d)
            stride *= l
            @boundscheck 1 <= I_{d-1} <= l || return false
            index += (I_d - 1) * stride
        end
        return isassigned(B, index)
    end
end

## permutedims

## Permute array dims ##

function permutedims(B::StridedArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    (ndimsB == length(perm) && isperm(perm)) || throw(ArgumentError("no valid permutation of dimensions"))
    dimsP = ntuple(i->dimsB[perm[i]], ndimsB)::typeof(dimsB)
    P = similar(B, dimsP)
    permutedims!(P, B, perm)
end

function checkdims_perm(P::AbstractArray{TP,N}, B::AbstractArray{TB,N}, perm) where {TP,TB,N}
    indsB = axes(B)
    length(perm) == N || throw(ArgumentError("expected permutation of size $N, but length(perm)=$(length(perm))"))
    isperm(perm) || throw(ArgumentError("input is not a permutation"))
    indsP = axes(P)
    for i = 1:length(perm)
        indsP[i] == indsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
    end
    nothing
end

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @generated function permutedims!(P::$PT{$(V...)}, B::$BT{$(V...)}, perm) where $(V...)
        quote
            checkdims_perm(P, B, perm)

            #calculates all the strides
            strides_1 = 0
            @nexprs $N d->(strides_{d+1} = stride(B, perm[d]))

            #Creates offset, because indexing starts at 1
            offset = 1 - sum(@ntuple $N d->strides_{d+1})

            if isa(B, SubArray)
                offset += first_index(B::SubArray) - 1
                B = B.parent
            end

            ind = 1
            @nexprs 1 d->(counts_{$N+1} = strides_{$N+1}) # a trick to set counts_($N+1)
            @nloops($N, i, P,
                    d->(counts_d = strides_d), # PRE
                    d->(counts_{d+1} += strides_{d+1}), # POST
                    begin # BODY
                        sumc = sum(@ntuple $N d->counts_{d+1})
                        @inbounds P[ind] = B[sumc+offset]
                        ind += 1
                    end)

            return P
        end
    end
end

## unique across dim

# TODO: this doesn't fit into the new hashing scheme in any obvious way

struct Prehashed
    hash::UInt
end
hash(x::Prehashed) = x.hash

"""
    unique(A::AbstractArray, dim::Int)

Return unique regions of `A` along dimension `dim`.

# Examples
```jldoctest
julia> A = map(isodd, reshape(Vector(1:8), (2,2,2)))
2×2×2 Array{Bool,3}:
[:, :, 1] =
  true   true
 false  false

[:, :, 2] =
  true   true
 false  false

julia> unique(A)
2-element Array{Bool,1}:
  true
 false

julia> unique(A, 2)
2×1×2 Array{Bool,3}:
[:, :, 1] =
  true
 false

[:, :, 2] =
  true
 false

julia> unique(A, 3)
2×2×1 Array{Bool,3}:
[:, :, 1] =
  true   true
 false  false
```
"""
@generated function unique(A::AbstractArray{T,N}, dim::Int) where {T,N}
    inds = inds -> zeros(UInt, inds)
    quote
        1 <= dim <= $N || return copy(A)
        hashes = similar($inds, axes(A, dim))

        # Compute hash for each row
        k = 0
        @nloops $N i A d->(if d == dim; k = i_d; end) begin
            @inbounds hashes[k] = hash(hashes[k], hash((@nref $N A i)))
        end

        # Collect index of first row for each hash
        uniquerow = similar(Array{Int}, axes(A, dim))
        firstrow = Dict{Prehashed,Int}()
        for k = axes(A, dim)
            uniquerow[k] = get!(firstrow, Prehashed(hashes[k]), k)
        end
        uniquerows = collect(values(firstrow))

        # Check for collisions
        collided = similar(falses, axes(A, dim))
        @inbounds begin
            @nloops $N i A d->(if d == dim
                k = i_d
                j_d = uniquerow[k]
            else
                j_d = i_d
            end) begin
                if (@nref $N A j) != (@nref $N A i)
                    collided[k] = true
                end
            end
        end

        if any(collided)
            nowcollided = similar(BitArray, axes(A, dim))
            while any(collided)
                # Collect index of first row for each collided hash
                empty!(firstrow)
                for j = axes(A, dim)
                    collided[j] || continue
                    uniquerow[j] = get!(firstrow, Prehashed(hashes[j]), j)
                end
                for v in values(firstrow)
                    push!(uniquerows, v)
                end

                # Check for collisions
                fill!(nowcollided, false)
                @nloops $N i A d->begin
                    if d == dim
                        k = i_d
                        j_d = uniquerow[k]
                        (!collided[k] || j_d == k) && continue
                    else
                        j_d = i_d
                    end
                end begin
                    if (@nref $N A j) != (@nref $N A i)
                        nowcollided[k] = true
                    end
                end
                (collided, nowcollided) = (nowcollided, collided)
            end
        end

        @nref $N A d->d == dim ? sort!(uniquerows) : (axes(A, d))
    end
end

"""
    extrema(A, dims) -> Array{Tuple}

Compute the minimum and maximum elements of an array over the given dimensions.

# Examples
```jldoctest
julia> A = reshape(Vector(1:2:16), (2,2,2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  5
 3  7

[:, :, 2] =
  9  13
 11  15

julia> extrema(A, (1,2))
1×1×2 Array{Tuple{Int64,Int64},3}:
[:, :, 1] =
 (1, 7)

[:, :, 2] =
 (9, 15)
```
"""
function extrema(A::AbstractArray, dims)
    sz = [size(A)...]
    sz[[dims...]] = 1
    B = Array{Tuple{eltype(A),eltype(A)}}(uninitialized, sz...)
    return extrema!(B, A)
end

@noinline function extrema!(B, A)
    sA = size(A)
    sB = size(B)
    for I in CartesianIndices(sB)
        AI = A[I]
        B[I] = (AI, AI)
    end
    Bmax = CartesianIndex(sB)
    @inbounds @simd for I in CartesianIndices(sA)
        J = min(Bmax,I)
        BJ = B[J]
        AI = A[I]
        if AI < BJ[1]
            B[J] = (AI, BJ[2])
        elseif AI > BJ[2]
            B[J] = (BJ[1], AI)
        end
    end
    return B
end

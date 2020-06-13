# This file is a part of Julia. License is MIT: https://julialang.org/license

### Multidimensional iterators
module IteratorsMD
    import .Base: eltype, length, size, first, last, in, getindex,
                 setindex!, IndexStyle, min, max, zero, oneunit, isless, eachindex,
                 ndims, IteratorSize, convert, show, iterate, promote_rule, to_indices

    import .Base: +, -, *, (:)
    import .Base: simd_outer_range, simd_inner_length, simd_index, setindex
    using .Base: IndexLinear, IndexCartesian, AbstractCartesianIndex, fill_to_length, tail,
        ReshapedArray, ReshapedArrayLF, OneTo
    using .Base.Iterators: Reverse, PartitionIterator

    export CartesianIndex, CartesianIndices

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
    Base.get(A::AbstractArray, I::CartesianIndex, default) = get(A, I.I, default)
    eltype(::Type{T}) where {T<:CartesianIndex} = eltype(fieldtype(T, :I))

    # access to index tuple
    Tuple(index::CartesianIndex) = index.I

    Base.setindex(x::CartesianIndex,i,j) = CartesianIndex(Base.setindex(Tuple(x),i,j))

    # equality
    Base.:(==)(a::CartesianIndex{N}, b::CartesianIndex{N}) where N = a.I == b.I

    # zeros and ones
    zero(::CartesianIndex{N}) where {N} = zero(CartesianIndex{N})
    zero(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(x -> 0, Val(N)))
    oneunit(::CartesianIndex{N}) where {N} = oneunit(CartesianIndex{N})
    oneunit(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(x -> 1, Val(N)))

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
        iter = CartesianIndices(axes(a))
        # might overflow
        I = inc(i.I, first(iter).I, last(iter).I)
        return I
    end
    function Base.prevind(a::AbstractArray{<:Any,N}, i::CartesianIndex{N}) where {N}
        iter = CartesianIndices(axes(a))
        # might underflow
        I = dec(i.I, last(iter).I, first(iter).I)
        return I
    end

    Base._ind2sub(t::Tuple, ind::CartesianIndex) = Tuple(ind)

    # Iteration over the elements of CartesianIndex cannot be supported until its length can be inferred,
    # see #23719
    Base.iterate(::CartesianIndex) =
        error("iteration is deliberately unsupported for CartesianIndex. Use `I` rather than `I...`, or use `Tuple(I)...`")

    # Iteration
    """
        CartesianIndices(sz::Dims) -> R
        CartesianIndices((istart:istop, jstart:jstop, ...)) -> R

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

    ```jldoctest
    julia> cartesian = CartesianIndices((1:3, 1:2))
    3×2 CartesianIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
     CartesianIndex(1, 1)  CartesianIndex(1, 2)
     CartesianIndex(2, 1)  CartesianIndex(2, 2)
     CartesianIndex(3, 1)  CartesianIndex(3, 2)

    julia> cartesian[4]
    CartesianIndex(1, 2)
    ```

    ## Broadcasting

    `CartesianIndices` support broadcasting arithmetic (+ and -) with a `CartesianIndex`.

    !!! compat "Julia 1.1"
        Broadcasting of CartesianIndices requires at least Julia 1.1.

    ```jldoctest
    julia> CIs = CartesianIndices((2:3, 5:6))
    2×2 CartesianIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
     CartesianIndex(2, 5)  CartesianIndex(2, 6)
     CartesianIndex(3, 5)  CartesianIndex(3, 6)

    julia> CI = CartesianIndex(3, 4)
    CartesianIndex(3, 4)

    julia> CIs .+ CI
    2×2 CartesianIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
     CartesianIndex(5, 9)  CartesianIndex(5, 10)
     CartesianIndex(6, 9)  CartesianIndex(6, 10)
    ```

    For cartesian to linear index conversion, see [`LinearIndices`](@ref).
    """
    struct CartesianIndices{N,R<:NTuple{N,AbstractUnitRange{Int}}} <: AbstractArray{CartesianIndex{N},N}
        indices::R
    end

    CartesianIndices(::Tuple{}) = CartesianIndices{0,typeof(())}(())
    CartesianIndices(inds::NTuple{N,AbstractUnitRange{<:Integer}}) where {N} =
        CartesianIndices(map(r->convert(AbstractUnitRange{Int}, r), inds))

    CartesianIndices(index::CartesianIndex) = CartesianIndices(index.I)
    CartesianIndices(sz::NTuple{N,<:Integer}) where {N} = CartesianIndices(map(Base.OneTo, sz))
    CartesianIndices(inds::NTuple{N,Union{<:Integer,AbstractUnitRange{<:Integer}}}) where {N} =
        CartesianIndices(map(i->first(i):last(i), inds))

    CartesianIndices(A::AbstractArray) = CartesianIndices(axes(A))

    """
        (:)(I::CartesianIndex, J::CartesianIndex)

    Construct [`CartesianIndices`](@ref) from two `CartesianIndex`.

    !!! compat "Julia 1.1"
        This method requires at least Julia 1.1.

    # Examples
    ```jldoctest
    julia> I = CartesianIndex(2,1);

    julia> J = CartesianIndex(3,3);

    julia> I:J
    2×3 CartesianIndices{2,Tuple{UnitRange{Int64},UnitRange{Int64}}}:
     CartesianIndex(2, 1)  CartesianIndex(2, 2)  CartesianIndex(2, 3)
     CartesianIndex(3, 1)  CartesianIndex(3, 2)  CartesianIndex(3, 3)
    ```
    """
    (:)(I::CartesianIndex{N}, J::CartesianIndex{N}) where N =
        CartesianIndices(map((i,j) -> i:j, Tuple(I), Tuple(J)))

    promote_rule(::Type{CartesianIndices{N,R1}}, ::Type{CartesianIndices{N,R2}}) where {N,R1,R2} =
        CartesianIndices{N,Base.indices_promote_type(R1,R2)}

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

    convert(::Type{CartesianIndices{N,R}}, inds::CartesianIndices{N}) where {N,R} =
        CartesianIndices(convert(R, inds.indices))

    # equality
    Base.:(==)(a::CartesianIndices{N}, b::CartesianIndices{N}) where N =
        all(map(==, a.indices, b.indices))
    Base.:(==)(a::CartesianIndices, b::CartesianIndices) = false

    # AbstractArray implementation
    Base.axes(iter::CartesianIndices{N,R}) where {N,R} = map(Base.axes1, iter.indices)
    Base.IndexStyle(::Type{CartesianIndices{N,R}}) where {N,R} = IndexCartesian()
    @inline function Base.getindex(iter::CartesianIndices{N,<:NTuple{N,Base.OneTo}}, I::Vararg{Int, N}) where {N}
        @boundscheck checkbounds(iter, I...)
        CartesianIndex(I)
    end
    @inline function Base.getindex(iter::CartesianIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        @boundscheck checkbounds(iter, I...)
        CartesianIndex(I .- first.(Base.axes1.(iter.indices)) .+ first.(iter.indices))
    end

    ndims(R::CartesianIndices) = ndims(typeof(R))
    ndims(::Type{CartesianIndices{N}}) where {N} = N
    ndims(::Type{CartesianIndices{N,TT}}) where {N,TT} = N

    eachindex(::IndexCartesian, A::AbstractArray) = CartesianIndices(axes(A))

    @inline function eachindex(::IndexCartesian, A::AbstractArray, B::AbstractArray...)
        axsA = axes(A)
        Base._all_match_first(axes, axsA, B...) || Base.throw_eachindex_mismatch(IndexCartesian(), A, B...)
        CartesianIndices(axsA)
    end

    eltype(::Type{CartesianIndices{N}}) where {N} = CartesianIndex{N}
    eltype(::Type{CartesianIndices{N,TT}}) where {N,TT} = CartesianIndex{N}
    IteratorSize(::Type{<:CartesianIndices{N}}) where {N} = Base.HasShape{N}()

    @inline function iterate(iter::CartesianIndices)
        iterfirst, iterlast = first(iter), last(iter)
        if any(map(>, iterfirst.I, iterlast.I))
            return nothing
        end
        iterfirst, iterfirst
    end
    @inline function iterate(iter::CartesianIndices, state)
        valid, I = __inc(state.I, first(iter).I, last(iter).I)
        valid || return nothing
        return CartesianIndex(I...), CartesianIndex(I...)
    end

    # increment & carry
    @inline function inc(state, start, stop)
        _, I = __inc(state, start, stop)
        return CartesianIndex(I...)
    end

    # increment post check to avoid integer overflow
    @inline __inc(::Tuple{}, ::Tuple{}, ::Tuple{}) = false, ()
    @inline function __inc(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int})
        valid = state[1] < stop[1]
        return valid, (state[1]+1,)
    end

    @inline function __inc(state, start, stop)
        if state[1] < stop[1]
            return true, (state[1]+1, tail(state)...)
        end
        valid, I = __inc(tail(state), tail(start), tail(stop))
        return valid, (start[1], I...)
    end

    # 0-d cartesian ranges are special-cased to iterate once and only once
    iterate(iter::CartesianIndices{0}, done=false) = done ? nothing : (CartesianIndex(), true)

    size(iter::CartesianIndices) = map(dimlength, first(iter).I, last(iter).I)
    dimlength(start, stop) = stop-start+1

    length(iter::CartesianIndices) = prod(size(iter))

    first(iter::CartesianIndices) = CartesianIndex(map(first, iter.indices))
    last(iter::CartesianIndices)  = CartesianIndex(map(last, iter.indices))

    # When used as indices themselves, CartesianIndices can simply become its tuple of ranges
    @inline to_indices(A, inds, I::Tuple{CartesianIndices, Vararg{Any}}) =
        to_indices(A, inds, (I[1].indices..., tail(I)...))
    # but preserve CartesianIndices{0} as they consume a dimension.
    @inline to_indices(A, inds, I::Tuple{CartesianIndices{0},Vararg{Any}}) =
        (first(I), to_indices(A, inds, tail(I))...)

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
    simd_inner_length(iter::CartesianIndices, I::CartesianIndex) = Base.length(iter.indices[1])

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

    @inline function iterate(r::Reverse{<:CartesianIndices})
        iterfirst, iterlast = last(r.itr), first(r.itr)
        if any(map(<, iterfirst.I, iterlast.I))
            return nothing
        end
        iterfirst, iterfirst
    end
    @inline function iterate(r::Reverse{<:CartesianIndices}, state)
        valid, I = __dec(state.I, last(r.itr).I, first(r.itr).I)
        valid || return nothing
        return CartesianIndex(I...), CartesianIndex(I...)
    end

    # decrement & carry
    @inline function dec(state, start, stop)
        _, I = __dec(state, start, stop)
        return CartesianIndex(I...)
    end

    # decrement post check to avoid integer overflow
    @inline __dec(::Tuple{}, ::Tuple{}, ::Tuple{}) = false, ()
    @inline function __dec(state::Tuple{Int}, start::Tuple{Int}, stop::Tuple{Int})
        valid = state[1] > stop[1]
        return valid, (state[1]-1,)
    end

    @inline function __dec(state, start, stop)
        if state[1] > stop[1]
            return true, (state[1]-1, tail(state)...)
        end
        valid, I = __dec(tail(state), tail(start), tail(stop))
        return valid, (start[1], I...)
    end

    # 0-d cartesian ranges are special-cased to iterate once and only once
    iterate(iter::Reverse{<:CartesianIndices{0}}, state=false) = state ? nothing : (CartesianIndex(), true)

    Base.LinearIndices(inds::CartesianIndices{N,R}) where {N,R} = LinearIndices{N,R}(inds.indices)

    # Views of reshaped CartesianIndices are used for partitions — ensure these are fast
    const CartesianPartition{T<:CartesianIndex, P<:CartesianIndices, R<:ReshapedArray{T,1,P}} = SubArray{T,1,R,Tuple{UnitRange{Int}},false}
    eltype(::Type{PartitionIterator{T}}) where {T<:ReshapedArrayLF} = SubArray{eltype(T), 1, T, Tuple{UnitRange{Int}}, true}
    eltype(::Type{PartitionIterator{T}}) where {T<:ReshapedArray} = SubArray{eltype(T), 1, T, Tuple{UnitRange{Int}}, false}
    Iterators.IteratorEltype(::Type{<:PartitionIterator{T}}) where {T<:ReshapedArray} = Iterators.IteratorEltype(T)

    eltype(::Type{PartitionIterator{T}}) where {T<:OneTo} = UnitRange{eltype(T)}
    eltype(::Type{PartitionIterator{T}}) where {T<:Union{UnitRange, StepRange, StepRangeLen, LinRange}} = T
    Iterators.IteratorEltype(::Type{<:PartitionIterator{T}}) where {T<:Union{OneTo, UnitRange, StepRange, StepRangeLen, LinRange}} = Iterators.IteratorEltype(T)


    @inline function iterate(iter::CartesianPartition)
        isempty(iter) && return nothing
        f = first(iter)
        return (f, (f, 1))
    end
    @inline function iterate(iter::CartesianPartition, (state, n))
        n >= length(iter) && return nothing
        I = IteratorsMD.inc(state.I, first(iter.parent.parent).I, last(iter.parent.parent).I)
        return I, (I, n+1)
    end

    @inline function simd_outer_range(iter::CartesianPartition)
        # In general, the Cartesian Partition might start and stop in the middle of the outer
        # dimensions — thus the outer range of a CartesianPartition is itself a
        # CartesianPartition.
        t = tail(iter.parent.parent.indices)
        ci = CartesianIndices(t)
        li = LinearIndices(t)
        return @inbounds view(ci, li[tail(iter[1].I)...]:li[tail(iter[end].I)...])
    end
    function simd_outer_range(iter::CartesianPartition{CartesianIndex{2}})
        # But for two-dimensional Partitions the above is just a simple one-dimensional range
        # over the second dimension; we don't need to worry about non-rectangular staggers in
        # higher dimensions.
        return @inbounds CartesianIndices((iter[1][2]:iter[end][2],))
    end
    @inline function simd_inner_length(iter::CartesianPartition, I::CartesianIndex)
        inner = iter.parent.parent.indices[1]
        @inbounds fi = iter[1].I
        @inbounds li = iter[end].I
        inner_start = I.I == tail(fi) ? fi[1] : first(inner)
        inner_end   = I.I == tail(li) ? li[1] : last(inner)
        return inner_end - inner_start + 1
    end
    @inline function simd_index(iter::CartesianPartition, Ilast::CartesianIndex, I1::Int)
        # I1 is the 0-based distance from the first dimension's offest
        offset = first(iter.parent.parent.indices[1]) # (this is 1 for 1-based arrays)
        # In the first column we need to also add in the iter's starting point (branchlessly)
        f = @inbounds iter[1]
        startoffset = (Ilast.I == tail(f.I))*(f[1] - 1)
        CartesianIndex((I1 + offset + startoffset, Ilast.I...))
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
checkindex(::Type{Bool}, inds::Tuple, I::CartesianIndices) = all(checkindex.(Bool, inds, I.indices))

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
LogicalIndex{Int}(mask::AbstractArray) = LogicalIndex{Int, typeof(mask)}(mask)
size(L::LogicalIndex) = (L.sum,)
length(L::LogicalIndex) = L.sum
collect(L::LogicalIndex) = [i for i in L]
show(io::IO, r::LogicalIndex) = print(io,collect(r))
print_array(io::IO, X::LogicalIndex) = print_array(io, collect(X))
# Iteration over LogicalIndex is very performance-critical, but it also must
# support arbitrary AbstractArray{Bool}s with both Int and CartesianIndex.
# Thus the iteration state contains an index iterator and its state. We also
# keep track of the count of elements since we already know how many there
# should be -- this way we don't need to look at future indices to check done.
@inline function iterate(L::LogicalIndex{Int})
    r = LinearIndices(L.mask)
    iterate(L, (1, r))
end
@inline function iterate(L::LogicalIndex{<:CartesianIndex})
    r = CartesianIndices(axes(L.mask))
    iterate(L, (1, r))
end
@propagate_inbounds function iterate(L::LogicalIndex, s)
    # We're looking for the n-th true element, using iterator r at state i
    n = s[1]
    n > length(L) && return nothing
    #unroll once to help inference, cf issue #29418
    idx, i = iterate(tail(s)...)
    s = (n+1, s[2], i)
    L.mask[idx] && return (idx, s)
    while true
        idx, i = iterate(tail(s)...)
        s = (n+1, s[2], i)
        L.mask[idx] && return (idx, s)
    end
end
# When wrapping a BitArray, lean heavily upon its internals.
@inline function iterate(L::Base.LogicalIndex{Int,<:BitArray})
    L.sum == 0 && return nothing
    Bc = L.mask.chunks
    return iterate(L, (1, @inbounds Bc[1]))
end
@inline function iterate(L::Base.LogicalIndex{Int,<:BitArray}, s)
    Bc = L.mask.chunks
    i1, c = s
    while c==0
        i1 % UInt >= length(Bc) % UInt && return nothing
        i1 += 1
        @inbounds c = Bc[i1]
    end
    tz = trailing_zeros(c) + 1
    c = _blsr(c)
    return ((i1-1)<<6 + tz, (i1, c))
end

@inline checkbounds(::Type{Bool}, A::AbstractArray, I::LogicalIndex{<:Any,<:AbstractArray{Bool,1}}) =
    eachindex(IndexLinear(), A) == eachindex(IndexLinear(), I.mask)
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

function _generate_unsafe_getindex!_body(N::Int)
    quote
        @_inline_meta
        D = eachindex(dest)
        Dy = iterate(D)
        @inbounds @nloops $N j d->I[d] begin
            # This condition is never hit, but at the moment
            # the optimizer is not clever enough to split the union without it
            Dy === nothing && return dest
            (idx, state) = Dy
            dest[idx] = @ncall $N getindex src j
            Dy = iterate(D, state)
        end
        return dest
    end
end

# Always index with the exactly indices provided.
@generated function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Vararg{Union{Real, AbstractArray}, N}) where N
    _generate_unsafe_getindex!_body(N)
end

# manually written-out specializations for 1 and 2 arguments to save compile time
@eval function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Vararg{Union{Real, AbstractArray},1})
    $(_generate_unsafe_getindex!_body(1))
end

@eval function _unsafe_getindex!(dest::AbstractArray, src::AbstractArray, I::Vararg{Union{Real, AbstractArray},2})
    $(_generate_unsafe_getindex!_body(2))
end

@noinline throw_checksize_error(A, sz) = throw(DimensionMismatch("output array is the wrong size; expected $sz, got $(size(A))"))

## setindex! ##
function _setindex!(l::IndexStyle, A::AbstractArray, x, I::Union{Real, AbstractArray}...)
    @_inline_meta
    @boundscheck checkbounds(A, I...)
    _unsafe_setindex!(l, _maybe_reshape(l, A, I...), x, I...)
    A
end

function _generate_unsafe_setindex!_body(N::Int)
    quote
        x′ = unalias(A, x)
        @nexprs $N d->(I_d = unalias(A, I[d]))
        idxlens = @ncall $N index_lengths I
        @ncall $N setindex_shape_check x′ (d->idxlens[d])
        Xy = iterate(x′)
        @inbounds @nloops $N i d->I_d begin
            # This is never reached, but serves as an assumption for
            # the optimizer that it does not need to emit error paths
            Xy === nothing && break
            (val, state) = Xy
            @ncall $N setindex! A val i
            Xy = iterate(x′, state)
        end
        A
    end
end

@generated function _unsafe_setindex!(::IndexStyle, A::AbstractArray, x, I::Vararg{Union{Real,AbstractArray}, N}) where N
    _generate_unsafe_setindex!_body(N)
end

@eval function _unsafe_setindex!(::IndexStyle, A::AbstractArray, x, I::Vararg{Union{Real,AbstractArray},1})
    $(_generate_unsafe_setindex!_body(1))
end

@eval function _unsafe_setindex!(::IndexStyle, A::AbstractArray, x, I::Vararg{Union{Real,AbstractArray},2})
    $(_generate_unsafe_setindex!_body(2))
end

diff(a::AbstractVector) = diff(a, dims=1)

"""
    diff(A::AbstractVector)
    diff(A::AbstractArray; dims::Integer)

Finite difference operator on a vector or a multidimensional array `A`. In the
latter case the dimension to operate on needs to be specified with the `dims`
keyword argument.

!!! compat "Julia 1.1"
    `diff` for arrays with dimension higher than 2 requires at least Julia 1.1.

# Examples
```jldoctest
julia> a = [2 4; 6 16]
2×2 Array{Int64,2}:
 2   4
 6  16

julia> diff(a, dims=2)
2×1 Array{Int64,2}:
  2
 10

julia> diff(vec(a))
3-element Array{Int64,1}:
  4
 -2
 12
```
"""
function diff(a::AbstractArray{T,N}; dims::Integer) where {T,N}
    require_one_based_indexing(a)
    1 <= dims <= N || throw(ArgumentError("dimension $dims out of range (1:$N)"))

    r = axes(a)
    r0 = ntuple(i -> i == dims ? UnitRange(1, last(r[i]) - 1) : UnitRange(r[i]), N)
    r1 = ntuple(i -> i == dims ? UnitRange(2, last(r[i])) : UnitRange(r[i]), N)

    return view(a, r1...) .- view(a, r0...)
end
function diff(r::AbstractRange{T}; dims::Integer=1) where {T}
    dims == 1 || throw(ArgumentError("dimension $dims out of range (1:1)"))
    return T[@inbounds r[i+1] - r[i] for i in firstindex(r):lastindex(r)-1]
end

### from abstractarray.jl

# In the common case where we have two views into the same parent, aliasing checks
# are _much_ easier and more important to get right
function mightalias(A::SubArray{T,<:Any,P}, B::SubArray{T,<:Any,P}) where {T,P}
    if !_parentsmatch(A.parent, B.parent)
        # We cannot do any better than the usual dataids check
        return !_isdisjoint(dataids(A), dataids(B))
    end
    # Now we know that A.parent === B.parent. This means that the indices of A
    # and B are the same length and indexing into the same dimensions. We can
    # just walk through them and check for overlaps: O(ndims(A)). We must finally
    # ensure that the indices don't alias with either parent
    return _indicesmightoverlap(A.indices, B.indices) ||
        !_isdisjoint(dataids(A.parent), _splatmap(dataids, B.indices)) ||
        !_isdisjoint(dataids(B.parent), _splatmap(dataids, A.indices))
end
_parentsmatch(A::AbstractArray, B::AbstractArray) = A === B
# Two reshape(::Array)s of the same size aren't `===` because they have different headers
_parentsmatch(A::Array, B::Array) = pointer(A) == pointer(B) && size(A) == size(B)

_indicesmightoverlap(A::Tuple{}, B::Tuple{}) = true
_indicesmightoverlap(A::Tuple{}, B::Tuple) = error("malformed subarray")
_indicesmightoverlap(A::Tuple, B::Tuple{}) = error("malformed subarray")
# For ranges, it's relatively cheap to construct the intersection
@inline function _indicesmightoverlap(A::Tuple{AbstractRange, Vararg{Any}}, B::Tuple{AbstractRange, Vararg{Any}})
    !isempty(intersect(A[1], B[1])) ? _indicesmightoverlap(tail(A), tail(B)) : false
end
# But in the common AbstractUnitRange case, there's an even faster shortcut
@inline function _indicesmightoverlap(A::Tuple{AbstractUnitRange, Vararg{Any}}, B::Tuple{AbstractUnitRange, Vararg{Any}})
    max(first(A[1]),first(B[1])) <= min(last(A[1]),last(B[1])) ? _indicesmightoverlap(tail(A), tail(B)) : false
end
# And we can check scalars against each other and scalars against arrays quite easily
@inline _indicesmightoverlap(A::Tuple{Real, Vararg{Any}}, B::Tuple{Real, Vararg{Any}}) =
    A[1] == B[1] ? _indicesmightoverlap(tail(A), tail(B)) : false
@inline _indicesmightoverlap(A::Tuple{Real, Vararg{Any}}, B::Tuple{AbstractArray, Vararg{Any}}) =
    A[1] in B[1] ? _indicesmightoverlap(tail(A), tail(B)) : false
@inline _indicesmightoverlap(A::Tuple{AbstractArray, Vararg{Any}}, B::Tuple{Real, Vararg{Any}}) =
    B[1] in A[1] ? _indicesmightoverlap(tail(A), tail(B)) : false
# And small arrays are quick, too
@inline function _indicesmightoverlap(A::Tuple{AbstractArray, Vararg{Any}}, B::Tuple{AbstractArray, Vararg{Any}})
    if length(A[1]) == 1
        return A[1][1] in B[1] ? _indicesmightoverlap(tail(A), tail(B)) : false
    elseif length(B[1]) == 1
        return B[1][1] in A[1] ? _indicesmightoverlap(tail(A), tail(B)) : false
    else
        # But checking larger arrays requires O(m*n) and is too much work
        return true
    end
end
# And in general, checking the intersection is too much work
_indicesmightoverlap(A::Tuple{Any, Vararg{Any}}, B::Tuple{Any, Vararg{Any}}) = true

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

julia> a = [1, 1, 1]; A = fill!(Vector{Vector{Int}}(undef, 3), a); a[1] = 2; A
3-element Array{Array{Int64,1},1}:
 [2, 1, 1]
 [2, 1, 1]
 [2, 1, 1]

julia> x = 0; f() = (global x += 1; x); fill!(Vector{Int}(undef, 3), f())
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

function copyto!(dest::AbstractArray{T1,N}, Rdest::CartesianIndices{N},
                  src::AbstractArray{T2,N}, Rsrc::CartesianIndices{N}) where {T1,T2,N}
    isempty(Rdest) && return dest
    if size(Rdest) != size(Rsrc)
        throw(ArgumentError("source and destination must have same size (got $(size(Rsrc)) and $(size(Rdest)))"))
    end
    checkbounds(dest, first(Rdest))
    checkbounds(dest, last(Rdest))
    checkbounds(src, first(Rsrc))
    checkbounds(src, last(Rsrc))
    src′ = unalias(dest, src)
    ΔI = first(Rdest) - first(Rsrc)
    if @generated
        quote
            @nloops $N i (n->Rsrc.indices[n]) begin
                @inbounds @nref($N,dest,n->i_n+ΔI[n]) = @nref($N,src′,i)
            end
        end
    else
        for I in Rsrc
            @inbounds dest[I + ΔI] = src′[I]
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
                             shiftamt::Tuple{Integer,Vararg{Any}})::typeof(dest)
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

julia> dest = OffsetArray{Int}(undef, (0:3,2:5))

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
                            src,  rsrc,  indssrc::Tuple{AbstractUnitRange,Vararg{Any}})::typeof(dest)
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

@inline function setindex!(B::BitArray, X::Union{StridedArray,BitArray}, J0::Union{Colon,UnitRange{Int}})
    I0 = to_indices(B, (J0,))[1]
    @boundscheck checkbounds(B, I0)
    l0 = length(I0)
    setindex_shape_check(X, l0)
    l0 == 0 && return B
    f0 = indexoffset(I0)+1
    copy_to_bitarray_chunks!(B.chunks, f0, X, 1, l0)
    return B
end

@inline function setindex!(B::BitArray, X::Union{StridedArray,BitArray},
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, X, J...)
end
@generated function _unsafe_setindex!(B::BitArray, X::Union{StridedArray,BitArray},
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

@propagate_inbounds function setindex!(B::BitArray, X::AbstractArray,
        I0::Union{Colon,UnitRange{Int}}, I::Union{Int,UnitRange{Int},Colon}...)
    _setindex!(IndexStyle(B), B, X, to_indices(B, (I0, I...))...)
end

## fill! contiguous views of BitArrays with a single value
function fill!(V::SubArray{Bool, <:Any, <:BitArray, Tuple{AbstractUnitRange{Int}}}, x)
    B = V.parent
    I0 = V.indices[1]
    l0 = length(I0)
    l0 == 0 && return V
    fill_chunks!(B.chunks, Bool(x), first(I0), l0)
    return V
end

fill!(V::SubArray{Bool, <:Any, <:BitArray, Tuple{AbstractUnitRange{Int}, Vararg{Union{Int,AbstractUnitRange{Int}}}}}, x) =
    _unsafe_fill_indices!(V.parent, x, V.indices...)

@generated function _unsafe_fill_indices!(B::BitArray, x,
        I0::AbstractUnitRange{Int}, I::Union{Int,AbstractUnitRange{Int}}...)
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
            native_strides = size_to_strides(1, size(B)...)
            strides_1 = 0
            @nexprs $N d->(strides_{d+1} = native_strides[perm[d]])

            #Creates offset, because indexing starts at 1
            offset = 1 - sum(@ntuple $N d->strides_{d+1})

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
    unique(A::AbstractArray; dims::Int)

Return unique regions of `A` along dimension `dims`.

# Examples
```jldoctest
julia> A = map(isodd, reshape(Vector(1:8), (2,2,2)))
2×2×2 Array{Bool,3}:
[:, :, 1] =
 1  1
 0  0

[:, :, 2] =
 1  1
 0  0

julia> unique(A)
2-element Array{Bool,1}:
 1
 0

julia> unique(A, dims=2)
2×1×2 Array{Bool,3}:
[:, :, 1] =
 1
 0

[:, :, 2] =
 1
 0

julia> unique(A, dims=3)
2×2×1 Array{Bool,3}:
[:, :, 1] =
 1  1
 0  0
```
"""
unique(A::AbstractArray; dims::Union{Colon,Integer} = :) = _unique_dims(A, dims)

_unique_dims(A::AbstractArray, dims::Colon) = invoke(unique, Tuple{Any}, A)

@generated function _unique_dims(A::AbstractArray{T,N}, dim::Integer) where {T,N}
    quote
        1 <= dim <= $N || return copy(A)
        hashes = zeros(UInt, axes(A, dim))

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
        collided = falses(axes(A, dim))
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
    extrema([f,] A::AbstractArray; dims, [init]) -> Array{Tuple}

Compute the minimum and maximum of `f` applied to each element (if given) in the given
dimensions of `A`.

!!! compat "Julia 1.2"
    2-argument `extrema` method requires Julia 1.2 or later.

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

julia> extrema(A, dims = (1,2))
1×1×2 Array{Tuple{Int64,Int64},3}:
[:, :, 1] =
 (1, 7)

[:, :, 2] =
 (9, 15)
```
"""
extrema(f::F, A::AbstractArray; dims=:, init=_InitialValue()) where {F} =
    _extrema_dims(f, A, dims, init)

_extrema_dims(f::F, A::AbstractArray, ::Colon, init) where {F} =
    mapreduce(_DupY(f), _extrema_rf, A; init = init)
_extrema_dims(f::F, A::AbstractArray, ::Colon, ::_InitialValue) where {F} =
    mapreduce(_DupY(f), _extrema_rf, A)
# Note: not passing `init = _InitialValue()` since user-defined
# `reduce`/`foldl` cannot be aware of `Base._InitialValue`.

_extrema_dims(f::F, A::AbstractArray, dims, init) where {F} =
    mapreduce(_DupY(f), _extrema_rf, A; dims = dims, init = init)
function _extrema_dims(f::F, A::AbstractArray, dims, ::_InitialValue) where {F}
    sz = [size(A)...]
    for d in dims
        sz[d] = 1
    end
    T = promote_op(f, eltype(A))
    B = Array{Tuple{T,T}}(undef, sz...)
    return extrema!(f, B, A)
end

@noinline function extrema!(f, B, A)
    require_one_based_indexing(B, A)
    sA = size(A)
    sB = size(B)
    for I in CartesianIndices(sB)
        fAI = f(A[I])
        B[I] = (fAI, fAI)
    end
    Bmax = CartesianIndex(sB)
    @inbounds @simd for I in CartesianIndices(sA)
        J = min(Bmax,I)
        BJ = B[J]
        fAI = f(A[I])
        if fAI < BJ[1]
            B[J] = (fAI, BJ[2])
        elseif fAI > BJ[2]
            B[J] = (BJ[1], fAI)
        end
    end
    return B
end
extrema!(B, A) = extrema!(identity, B, A)

# Show for pairs() with Cartesian indices. Needs to be here rather than show.jl for bootstrap order
function Base.showarg(io::IO, r::Iterators.Pairs{<:Integer, <:Any, <:Any, T}, toplevel) where T <: Union{AbstractVector, Tuple}
    print(io, "pairs(::$T)")
end
function Base.showarg(io::IO, r::Iterators.Pairs{<:CartesianIndex, <:Any, <:Any, T}, toplevel) where T <: AbstractArray
    print(io, "pairs(::$T)")
end

function Base.showarg(io::IO, r::Iterators.Pairs{<:CartesianIndex, <:Any, <:Any, T}, toplevel) where T<:AbstractVector
    print(io, "pairs(IndexCartesian(), ::$T)")
end

## sortslices

"""
    sortslices(A; dims, alg::Algorithm=DEFAULT_UNSTABLE, lt=isless, by=identity, rev::Bool=false, order::Ordering=Forward)

Sort slices of an array `A`. The required keyword argument `dims` must
be either an integer or a tuple of integers. It specifies the
dimension(s) over which the slices are sorted.

E.g., if `A` is a matrix, `dims=1` will sort rows, `dims=2` will sort columns.
Note that the default comparison function on one dimensional slices sorts
lexicographically.

For the remaining keyword arguments, see the documentation of [`sort!`](@ref).

# Examples
```jldoctest
julia> sortslices([7 3 5; -1 6 4; 9 -2 8], dims=1) # Sort rows
3×3 Array{Int64,2}:
 -1   6  4
  7   3  5
  9  -2  8

julia> sortslices([7 3 5; -1 6 4; 9 -2 8], dims=1, lt=(x,y)->isless(x[2],y[2]))
3×3 Array{Int64,2}:
  9  -2  8
  7   3  5
 -1   6  4

julia> sortslices([7 3 5; -1 6 4; 9 -2 8], dims=1, rev=true)
3×3 Array{Int64,2}:
  9  -2  8
  7   3  5
 -1   6  4

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2) # Sort columns
3×3 Array{Int64,2}:
  3   5  7
 -1  -4  6
 -2   8  9

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2, alg=InsertionSort, lt=(x,y)->isless(x[2],y[2]))
3×3 Array{Int64,2}:
  5   3  7
 -4  -1  6
  8  -2  9

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2, rev=true)
3×3 Array{Int64,2}:
 7   5   3
 6  -4  -1
 9   8  -2
```

# Higher dimensions

`sortslices` extends naturally to higher dimensions. E.g., if `A` is a
a 2x2x2 array, `sortslices(A, dims=3)` will sort slices within the 3rd dimension,
passing the 2x2 slices `A[:, :, 1]` and `A[:, :, 2]` to the comparison function.
Note that while there is no default order on higher-dimensional slices, you may
use the `by` or `lt` keyword argument to specify such an order.

If `dims` is a tuple, the order of the dimensions in `dims` is
relevant and specifies the linear order of the slices. E.g., if `A` is three
dimensional and `dims` is `(1, 2)`, the orderings of the first two dimensions
are re-arranged such such that the slices (of the remaining third dimension) are sorted.
If `dims` is `(2, 1)` instead, the same slices will be taken,
but the result order will be row-major instead.

# Higher dimensional examples
```
julia> A = permutedims(reshape([4 3; 2 1; 'A' 'B'; 'C' 'D'], (2, 2, 2)), (1, 3, 2))
2×2×2 Array{Any,3}:
[:, :, 1] =
 4  3
 2  1

[:, :, 2] =
 'A'  'B'
 'C'  'D'

julia> sortslices(A, dims=(1,2))
2×2×2 Array{Any,3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 'D'  'B'
 'C'  'A'

julia> sortslices(A, dims=(2,1))
2×2×2 Array{Any,3}:
[:, :, 1] =
 1  2
 3  4

[:, :, 2] =
 'D'  'C'
 'B'  'A'

julia> sortslices(reshape([5; 4; 3; 2; 1], (1,1,5)), dims=3, by=x->x[1,1])
1×1×5 Array{Int64,3}:
[:, :, 1] =
 1

[:, :, 2] =
 2

[:, :, 3] =
 3

[:, :, 4] =
 4

[:, :, 5] =
 5
```
"""
function sortslices(A::AbstractArray; dims::Union{Integer, Tuple{Vararg{Integer}}}, kws...)
    _sortslices(A, Val{dims}(); kws...)
end

# Works around inference's lack of ability to recognize partial constness
struct DimSelector{dims, T}
    A::T
end
DimSelector{dims}(x::T) where {dims, T} = DimSelector{dims, T}(x)
(ds::DimSelector{dims, T})(i) where {dims, T} = i in dims ? axes(ds.A, i) : (:,)

_negdims(n, dims) = filter(i->!(i in dims), 1:n)

function compute_itspace(A, ::Val{dims}) where {dims}
    negdims = _negdims(ndims(A), dims)
    axs = Iterators.product(ntuple(DimSelector{dims}(A), ndims(A))...)
    vec(permutedims(collect(axs), (dims..., negdims...)))
end

function _sortslices(A::AbstractArray, d::Val{dims}; kws...) where dims
    itspace = compute_itspace(A, d)
    vecs = map(its->view(A, its...), itspace)
    p = sortperm(vecs; kws...)
    if ndims(A) == 2 && isa(dims, Integer) && isa(A, Array)
        # At the moment, the performance of the generic version is subpar
        # (about 5x slower). Hardcode a fast-path until we're able to
        # optimize this.
        return dims == 1 ? A[p, :] : A[:, p]
    else
        B = similar(A)
        for (x, its) in zip(p, itspace)
            B[its...] = vecs[x]
        end
        B
    end
end

getindex(b::Ref, ::CartesianIndex{0}) = getindex(b)
setindex!(b::Ref, x, ::CartesianIndex{0}) = setindex!(b, x)

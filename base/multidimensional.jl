# This file is a part of Julia. License is MIT: https://julialang.org/license

### Multidimensional iterators
module IteratorsMD
    import .Base: eltype, length, size, first, last, in, getindex, setindex!,
                  min, max, zero, oneunit, isless, eachindex,
                  convert, show, iterate, promote_rule, to_indices, copy,
                  isassigned

    import .Base: +, -, *, (:)
    import .Base: simd_outer_range, simd_inner_length, simd_index, setindex
    import Core: Tuple
    using .Base: to_index, fill_to_length, tail, safe_tail
    using .Base: IndexLinear, IndexCartesian, AbstractCartesianIndex,
        ReshapedArray, ReshapedArrayLF, OneTo, Fix1
    using .Base.Iterators: Reverse, PartitionIterator
    using .Base: @propagate_inbounds

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

    An `I::CartesianIndex` is treated as a "scalar" (not a container)
    for `broadcast`.   In order to iterate over the components of a
    `CartesianIndex`, convert it to a tuple with `Tuple(I)`.

    # Examples
    ```jldoctest
    julia> A = reshape(Vector(1:16), (2, 2, 2, 2))
    2×2×2×2 Array{Int64, 4}:
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

    !!! compat "Julia 1.10"
        Using a `CartesianIndex` as a "scalar" for `broadcast` requires
        Julia 1.10; in previous releases, use `Ref(I)`.
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
    CartesianIndex{N}(index::CartesianIndex{N}) where {N} = index
    CartesianIndex(index::Union{Integer, CartesianIndex}...) = CartesianIndex(flatten(index))
    flatten(::Tuple{}) = ()
    flatten(I::Tuple{Any}) = Tuple(I[1])
    @inline flatten(I::Tuple) = (Tuple(I[1])..., flatten(tail(I))...)
    CartesianIndex(index::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = CartesianIndex(index...)
    function show(io::IO, i::CartesianIndex)
        print(io, "CartesianIndex(")
        join(io, i.I, ", ")
        print(io, ")")
    end

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
    zero(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(Returns(0), Val(N)))
    oneunit(::CartesianIndex{N}) where {N} = oneunit(CartesianIndex{N})
    oneunit(::Type{CartesianIndex{N}}) where {N} = CartesianIndex(ntuple(Returns(1), Val(N)))

    # arithmetic, min/max
    @inline (+)(index::CartesianIndex) = index
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
    isless(I1::CartesianIndex{N}, I2::CartesianIndex{N}) where {N} = isless(reverse(I1.I), reverse(I2.I))

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
        I = inc(i.I, iter.indices)
        return I
    end
    function Base.prevind(a::AbstractArray{<:Any,N}, i::CartesianIndex{N}) where {N}
        iter = CartesianIndices(axes(a))
        # might underflow
        I = dec(i.I, iter.indices)
        return I
    end

    Base._ind2sub(t::Tuple, ind::CartesianIndex) = Tuple(ind)

    # Iteration over the elements of CartesianIndex cannot be supported until its length can be inferred,
    # see #23719
    Base.iterate(::CartesianIndex) =
        error("iteration is deliberately unsupported for CartesianIndex. Use `I` rather than `I...`, or use `Tuple(I)...`")

    # ranges are deliberately disabled to prevent ambiguities with the colon constructor
    Base.range_start_step_length(start::CartesianIndex, step::CartesianIndex, len::Integer) =
        error("range with a specified length is deliberately unsupported for CartesianIndex arguments."*
            " Use StepRangeLen($start, $step, $len) to construct this range")

    # show is special-cased to avoid the start:stop:step display,
    # which constructs a CartesianIndices
    # See #50784
    function show(io::IO, r::StepRangeLen{<:CartesianIndex})
        print(io, "StepRangeLen(", first(r), ", ",
                    step(r), ", ", length(r), ")")
    end

    # Iteration
    const OrdinalRangeInt = OrdinalRange{Int, Int}
    """
        CartesianIndices(sz::Dims) -> R
        CartesianIndices((istart:[istep:]istop, jstart:[jstep:]jstop, ...)) -> R

    Define a region `R` spanning a multidimensional rectangular range
    of integer indices. These are most commonly encountered in the
    context of iteration, where `for I in R ... end` will return
    [`CartesianIndex`](@ref) indices `I` equivalent to the nested loops

        for j = jstart:jstep:jstop
            for i = istart:istep:istop
                ...
            end
        end

    Consequently these can be useful for writing algorithms that
    work in arbitrary dimensions.

        CartesianIndices(A::AbstractArray) -> R

    As a convenience, constructing a `CartesianIndices` from an array makes a
    range of its indices.

    !!! compat "Julia 1.6"
        The step range method `CartesianIndices((istart:istep:istop, jstart:[jstep:]jstop, ...))`
        requires at least Julia 1.6.

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
    CartesianIndices((2, 3))
    ```

    ## Conversion between linear and cartesian indices

    Linear index to cartesian index conversion exploits the fact that a
    `CartesianIndices` is an `AbstractArray` and can be indexed linearly:

    ```jldoctest
    julia> cartesian = CartesianIndices((1:3, 1:2))
    CartesianIndices((1:3, 1:2))

    julia> cartesian[4]
    CartesianIndex(1, 2)

    julia> cartesian = CartesianIndices((1:2:5, 1:2))
    CartesianIndices((1:2:5, 1:2))

    julia> cartesian[2, 2]
    CartesianIndex(3, 2)
    ```

    ## Broadcasting

    `CartesianIndices` support broadcasting arithmetic (+ and -) with a `CartesianIndex`.

    !!! compat "Julia 1.1"
        Broadcasting of CartesianIndices requires at least Julia 1.1.

    ```jldoctest
    julia> CIs = CartesianIndices((2:3, 5:6))
    CartesianIndices((2:3, 5:6))

    julia> CI = CartesianIndex(3, 4)
    CartesianIndex(3, 4)

    julia> CIs .+ CI
    CartesianIndices((5:6, 9:10))
    ```

    For cartesian to linear index conversion, see [`LinearIndices`](@ref).
    """
    struct CartesianIndices{N,R<:NTuple{N,OrdinalRangeInt}} <: AbstractArray{CartesianIndex{N},N}
        indices::R
    end

    CartesianIndices(::Tuple{}) = CartesianIndices{0,typeof(())}(())
    function CartesianIndices(inds::NTuple{N,OrdinalRange{<:Integer, <:Integer}}) where {N}
        indices = map(r->convert(OrdinalRangeInt, r), inds)
        CartesianIndices{N, typeof(indices)}(indices)
    end

    CartesianIndices(index::CartesianIndex) = CartesianIndices(index.I)
    CartesianIndices(inds::NTuple{N,Union{<:Integer,OrdinalRange{<:Integer}}}) where {N} =
        CartesianIndices(map(_convert2ind, inds))

    CartesianIndices(A::AbstractArray) = CartesianIndices(axes(A))

    _convert2ind(sz::Bool) = Base.OneTo(Int8(sz))
    _convert2ind(sz::Integer) = Base.oneto(sz)
    _convert2ind(sz::AbstractUnitRange) = first(sz):last(sz)
    _convert2ind(sz::OrdinalRange) = first(sz):step(sz):last(sz)

    function show(io::IO, iter::CartesianIndices)
        print(io, "CartesianIndices(")
        show(io, map(_xform_index, iter.indices))
        print(io, ")")
    end
    _xform_index(i) = i
    _xform_index(i::OneTo) = i.stop
    show(io::IO, ::MIME"text/plain", iter::CartesianIndices) = show(io, iter)

    """
        (:)(start::CartesianIndex, [step::CartesianIndex], stop::CartesianIndex)

    Construct [`CartesianIndices`](@ref) from two `CartesianIndex` and an optional step.

    !!! compat "Julia 1.1"
        This method requires at least Julia 1.1.

    !!! compat "Julia 1.6"
        The step range method start:step:stop requires at least Julia 1.6.

    # Examples
    ```jldoctest
    julia> I = CartesianIndex(2,1);

    julia> J = CartesianIndex(3,3);

    julia> I:J
    CartesianIndices((2:3, 1:3))

    julia> I:CartesianIndex(1, 2):J
    CartesianIndices((2:1:3, 1:2:3))
    ```
    """
    (:)(I::CartesianIndex{N}, J::CartesianIndex{N}) where N =
        CartesianIndices(map((i,j) -> i:j, Tuple(I), Tuple(J)))
    (:)(I::CartesianIndex{N}, S::CartesianIndex{N}, J::CartesianIndex{N}) where N =
        CartesianIndices(map((i,s,j) -> i:s:j, Tuple(I), Tuple(S), Tuple(J)))

    promote_rule(::Type{CartesianIndices{N,R1}}, ::Type{CartesianIndices{N,R2}}) where {N,R1,R2} =
        CartesianIndices{N,Base.indices_promote_type(R1,R2)}

    convert(::Type{Tuple{}}, R::CartesianIndices{0}) = ()
    for RT in (OrdinalRange{Int, Int}, StepRange{Int, Int}, AbstractUnitRange{Int})
        @eval convert(::Type{NTuple{N,$RT}}, R::CartesianIndices{N}) where {N} =
            map(x->convert($RT, x), R.indices)
    end
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
        CartesianIndices(convert(R, inds.indices))::CartesianIndices{N,R}

    # equality
    Base.:(==)(a::CartesianIndices{N}, b::CartesianIndices{N}) where N =
        all(map(==, a.indices, b.indices))
    Base.:(==)(a::CartesianIndices, b::CartesianIndices) = false

    # AbstractArray implementation
    Base.axes(iter::CartesianIndices{N,R}) where {N,R} = map(Base.axes1, iter.indices)
    Base.has_offset_axes(iter::CartesianIndices) = Base.has_offset_axes(iter.indices...)
    @propagate_inbounds function isassigned(iter::CartesianIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        for i in 1:N
            isassigned(iter.indices[i], I[i]) || return false
        end
        return true
    end

    # getindex for a 0D CartesianIndices is necessary for disambiguation
    @inline function Base.getindex(iter::CartesianIndices{0,R}) where {R}
        CartesianIndex()
    end
    @inline function Base.getindex(iter::CartesianIndices{N,R}, I::Vararg{Int, N}) where {N,R}
        # Eagerly do boundscheck before calculating each item of the CartesianIndex so that
        # we can pass `@inbounds` hint to inside the map and generates more efficient SIMD codes (#42115)
        @boundscheck checkbounds(iter, I...)
        index = map(iter.indices, I) do r, i
            @inbounds getindex(r, i)
        end
        CartesianIndex(index)
    end

    # CartesianIndices act as a multidimensional range, so cartesian indexing of CartesianIndices
    # with compatible dimensions may be seen as indexing into the component ranges.
    # This may use the special indexing behavior implemented for ranges to return another CartesianIndices
    @inline function Base.getindex(iter::CartesianIndices{N,R},
        I::Vararg{Union{OrdinalRange{<:Integer, <:Integer}, Colon}, N}) where {N,R}
        @boundscheck checkbounds(iter, I...)
        indices = map(iter.indices, I) do r, i
            @inbounds getindex(r, i)
        end
        CartesianIndices(indices)
    end
    @propagate_inbounds function Base.getindex(iter::CartesianIndices{N},
        C::CartesianIndices{N}) where {N}
        getindex(iter, C.indices...)
    end
    @inline Base.getindex(iter::CartesianIndices{0}, ::CartesianIndices{0}) = iter

    # If dimensions permit, we may index into a CartesianIndices directly instead of constructing a SubArray wrapper
    @propagate_inbounds function Base.view(c::CartesianIndices{N}, r::Vararg{Union{OrdinalRange{<:Integer, <:Integer}, Colon},N}) where {N}
        getindex(c, r...)
    end
    @propagate_inbounds function Base.view(c::CartesianIndices{N}, C::CartesianIndices{N}) where {N}
        getindex(c, C)
    end

    eachindex(::IndexCartesian, A::AbstractArray) = CartesianIndices(axes(A))

    @inline function eachindex(::IndexCartesian, A::AbstractArray, B::AbstractArray...)
        axsA = axes(A)
        Base._all_match_first(axes, axsA, B...) || Base.throw_eachindex_mismatch_indices(IndexCartesian(), axes(A), axes.(B)...)
        CartesianIndices(axsA)
    end

    @inline function iterate(iter::CartesianIndices)
        iterfirst = first(iter)
        if !all(map(in, iterfirst.I, iter.indices))
            return nothing
        end
        iterfirst, iterfirst
    end
    @inline function iterate(iter::CartesianIndices, state)
        valid, I = __inc(state.I, iter.indices)
        valid || return nothing
        return CartesianIndex(I...), CartesianIndex(I...)
    end

    # increment & carry
    @inline function inc(state, indices)
        _, I = __inc(state, indices)
        return CartesianIndex(I...)
    end

    # Unlike ordinary ranges, CartesianIndices continues the iteration in the next column when the
    # current column is consumed. The implementation is written recursively to achieve this.
    # `iterate` returns `Union{Nothing, Tuple}`, we explicitly pass a `valid` flag to eliminate
    # the type instability inside the core `__inc` logic, and this gives better runtime performance.
    __inc(::Tuple{}, ::Tuple{}) = false, ()
    @inline function __inc(state::Tuple{Int}, indices::Tuple{OrdinalRangeInt})
        rng = indices[1]
        I = state[1] + step(rng)
        valid = state[1] != last(rng)
        return valid, (I,)
    end
    @inline function __inc(state::Tuple{Int,Int,Vararg{Int}}, indices::Tuple{OrdinalRangeInt,OrdinalRangeInt,Vararg{OrdinalRangeInt}})
        rng = indices[1]
        I = state[1] + step(rng)
        if state[1] != last(rng)
            return true, (I, tail(state)...)
        end
        valid, I = __inc(tail(state), tail(indices))
        return valid, (first(rng), I...)
    end

    # 0-d cartesian ranges are special-cased to iterate once and only once
    iterate(iter::CartesianIndices{0}, done=false) = done ? nothing : (CartesianIndex(), true)

    size(iter::CartesianIndices) = map(length, iter.indices)

    length(iter::CartesianIndices) = prod(size(iter))

    # make CartesianIndices a multidimensional range
    Base.step(iter::CartesianIndices) = CartesianIndex(map(step, iter.indices))

    first(iter::CartesianIndices) = CartesianIndex(map(first, iter.indices))
    last(iter::CartesianIndices)  = CartesianIndex(map(last, iter.indices))

    # When used as indices themselves, CartesianIndices can simply become its tuple of ranges
    @inline function to_indices(A, inds, I::Tuple{CartesianIndices{N}, Vararg}) where N
        _, indstail = split(inds, Val(N))
        (map(Fix1(to_index, A), I[1].indices)..., to_indices(A, indstail, tail(I))...)
    end
    # but preserve CartesianIndices{0} as they consume a dimension.
    @inline to_indices(A, inds, I::Tuple{CartesianIndices{0}, Vararg}) =
        (first(I), to_indices(A, inds, tail(I))...)

    @inline in(i::CartesianIndex, r::CartesianIndices) = false
    @inline in(i::CartesianIndex{N}, r::CartesianIndices{N}) where {N} = all(map(in, i.I, r.indices))

    copy(iter::CartesianIndices) = iter

    simd_outer_range(iter::CartesianIndices{0}) = iter
    function simd_outer_range(iter::CartesianIndices)
        CartesianIndices(tail(iter.indices))
    end

    simd_inner_length(iter::CartesianIndices{0}, ::CartesianIndex) = 1
    simd_inner_length(iter::CartesianIndices, I::CartesianIndex) = Base.length(iter.indices[1])

    simd_index(iter::CartesianIndices{0}, ::CartesianIndex, I1::Int) = first(iter)
    @propagate_inbounds simd_index(iter::CartesianIndices, Ilast::CartesianIndex, I1::Int) =
        CartesianIndex(iter.indices[1][I1+firstindex(iter.indices[1])], Ilast)

    # Split out the first N elements of a tuple
    @inline function split(t, V::Val)
        ref = ntuple(Returns(true), V)  # create a reference tuple of length N
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
    @inline function Base._reverse(iter::CartesianIndices, ::Colon)
        CartesianIndices(reverse.(iter.indices))
    end

    Base.@constprop :aggressive function Base._reverse(iter::CartesianIndices, dim::Integer)
        1 <= dim <= ndims(iter) || throw(ArgumentError(Base.LazyString("invalid dimension ", dim, " in reverse")))
        ndims(iter) == 1 && return Base._reverse(iter, :)
        indices = iter.indices
        return CartesianIndices(Base.setindex(indices, reverse(indices[dim]), dim))
    end

    Base.@constprop :aggressive function Base._reverse(iter::CartesianIndices, dims::Tuple{Vararg{Integer}})
        indices = iter.indices
        # use `sum` to force const fold
        dimrev = ntuple(i -> sum(==(i), dims; init = 0) == 1, Val(length(indices)))
        length(dims) == sum(dimrev) || throw(ArgumentError(Base.LazyString("invalid dimensions ", dims, " in reverse")))
        length(dims) == length(indices) && return Base._reverse(iter, :)
        indices′ = map((i, f) -> f ? (@noinline reverse(i)) : i, indices, dimrev)
        return CartesianIndices(indices′)
    end

    # fix ambiguity with array.jl:
    Base._reverse(iter::CartesianIndices{1}, dims::Tuple{Integer}) =
        Base._reverse(iter, first(dims))

    @inline function iterate(r::Reverse{<:CartesianIndices})
        iterfirst = last(r.itr)
        if !all(map(in, iterfirst.I, r.itr.indices))
            return nothing
        end
        iterfirst, iterfirst
    end
    @inline function iterate(r::Reverse{<:CartesianIndices}, state)
        valid, I = __dec(state.I, r.itr.indices)
        valid || return nothing
        return CartesianIndex(I...), CartesianIndex(I...)
    end

    # decrement & carry
    @inline function dec(state, indices)
        _, I = __dec(state, indices)
        return CartesianIndex(I...)
    end

    # decrement post check to avoid integer overflow
    @inline __dec(::Tuple{}, ::Tuple{}) = false, ()
    @inline function __dec(state::Tuple{Int}, indices::Tuple{OrdinalRangeInt})
        rng = indices[1]
        I = state[1] - step(rng)
        valid = state[1] != first(rng)
        return valid, (I,)
    end
    @inline function __dec(state::Tuple{Int,Int,Vararg{Int}}, indices::Tuple{OrdinalRangeInt,OrdinalRangeInt,Vararg{OrdinalRangeInt}})
        rng = indices[1]
        I = state[1] - step(rng)
        if state[1] != first(rng)
            return true, (I, tail(state)...)
        end
        valid, I = __dec(tail(state), tail(indices))
        return valid, (last(rng), I...)
    end

    # 0-d cartesian ranges are special-cased to iterate once and only once
    iterate(iter::Reverse{<:CartesianIndices{0}}, state=false) = state ? nothing : (CartesianIndex(), true)

    function Base.LinearIndices(inds::CartesianIndices{N,R}) where {N,R<:NTuple{N, AbstractUnitRange}}
        LinearIndices{N,R}(inds.indices)
    end
    function Base.LinearIndices(inds::CartesianIndices)
        indices = inds.indices
        if all(x->step(x)==1, indices)
            indices = map(rng->first(rng):last(rng), indices)
            LinearIndices{length(indices), typeof(indices)}(indices)
        else
            # Given the fact that StepRange 1:2:4 === 1:2:3, we lost the original size information
            # and thus cannot calculate the correct linear indices when the steps are not 1.
            throw(ArgumentError(LazyString("LinearIndices for ", typeof(inds), " with non-1 step size is not yet supported.")))
        end
    end

    # This is currently needed because converting to LinearIndices is only available when steps are
    # all 1
    # NOTE: this is only a temporary patch and could be possibly removed when StepRange support to
    # LinearIndices is done
    function Base.collect(inds::CartesianIndices{N, R}) where {N,R<:NTuple{N, AbstractUnitRange}}
        Base._collect_indices(axes(inds), inds)
    end
    function Base.collect(inds::CartesianIndices)
        dest = Array{eltype(inds), ndims(inds)}(undef, size(inds))
        i = 0
        @inbounds for a in inds
            dest[i+=1] = a
        end
        dest
    end

    # array operations
    Base.intersect(a::CartesianIndices{N}, b::CartesianIndices{N}) where N =
        CartesianIndices(intersect.(a.indices, b.indices))
    Base.issubset(a::CartesianIndices{N}, b::CartesianIndices{N}) where N =
        isempty(a) || all(map(issubset, a.indices, b.indices))

    # Views of reshaped CartesianIndices are used for partitions — ensure these are fast
    const CartesianPartition{T<:CartesianIndex, P<:CartesianIndices, R<:ReshapedArray{T,1,P}} = SubArray{T,1,R,<:Tuple{AbstractUnitRange{Int}},false}
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
        I = IteratorsMD.inc(state.I, iter.parent.parent.indices)
        return I, (I, n+1)
    end

    @inline function simd_outer_range(iter::CartesianPartition)
        # In general, the Cartesian Partition might start and stop in the middle of the outer
        # dimensions — thus the outer range of a CartesianPartition is itself a
        # CartesianPartition.
        mi = iter.parent.mi
        ci = iter.parent.parent
        ax, ax1 = axes(ci), Base.axes1(ci)
        subs = Base.ind2sub_rs(ax, mi, first(iter.indices[1]))
        vl, fl = Base._sub2ind(tail(ax), tail(subs)...), subs[1]
        vr, fr = divrem(last(iter.indices[1]) - 1, mi[end]) .+ (1, first(ax1))
        oci = CartesianIndices(tail(ci.indices))
        # A fake CartesianPartition to reuse the outer iterate fallback
        outer = @inbounds view(ReshapedArray(oci, (length(oci),), mi), vl:vr)
        init = @inbounds dec(oci[tail(subs)...].I, oci.indices) # real init state
        # Use Generator to make inner loop branchless
        @inline function skip_len_I(i::Int, I::CartesianIndex)
            l = i == 1 ? fl : first(ax1)
            r = i == length(outer) ? fr : last(ax1)
            l - first(ax1), r - l + 1, I
        end
        (skip_len_I(i, I) for (i, I) in Iterators.enumerate(Iterators.rest(outer, (init, 0))))
    end
    @inline function simd_outer_range(iter::CartesianPartition{CartesianIndex{2}})
        # But for two-dimensional Partitions the above is just a simple one-dimensional range
        # over the second dimension; we don't need to worry about non-rectangular staggers in
        # higher dimensions.
        mi = iter.parent.mi
        ci = iter.parent.parent
        ax, ax1 = axes(ci), Base.axes1(ci)
        fl, vl = Base.ind2sub_rs(ax, mi, first(iter.indices[1]))
        fr, vr = Base.ind2sub_rs(ax, mi, last(iter.indices[1]))
        outer = @inbounds CartesianIndices((ci.indices[2][vl:vr],))
        # Use Generator to make inner loop branchless
        @inline function skip_len_I(I::CartesianIndex{1})
            l = I == first(outer) ? fl : first(ax1)
            r = I == last(outer) ? fr : last(ax1)
            l - first(ax1), r - l + 1, I
        end
        (skip_len_I(I) for I in outer)
    end
    @inline simd_inner_length(iter::CartesianPartition, (_, len, _)::Tuple{Int,Int,CartesianIndex}) = len
    @propagate_inbounds simd_index(iter::CartesianPartition, (skip, _, I)::Tuple{Int,Int,CartesianIndex}, n::Int) =
        simd_index(iter.parent.parent, I, n + skip)
end  # IteratorsMD


using .IteratorsMD

# from genericmemory.jl:
## generate vararg methods for atomic indexing
for ex in (
    :(getindex_atomic(mem::GenericMemory, order::Symbol, i::Int)),
    :(setindex_atomic!(mem::GenericMemory, order::Symbol, val, i::Int)),
    :(setindexonce_atomic!(mem::GenericMemory, success_order::Symbol, fail_order::Symbol, val, i::Int)),
    :(modifyindex_atomic!(mem::GenericMemory, order::Symbol, op, val, i::Int)),
    :(swapindex_atomic!(mem::GenericMemory, order::Symbol, val, i::Int)),
    :(replaceindex_atomic!(mem::GenericMemory, success_order::Symbol, fail_order::Symbol, expected, desired, i::Int,)),
)
    fn = ex.args[1]
    args = ex.args[2:end-1]

    @eval begin
        function $fn($(args...), i::Union{Integer,CartesianIndex}...)
            return $fn($(args...), CartesianIndex(to_indices($(args[1]), i)))
        end

        function $fn($(args...), i::CartesianIndex)
            return $fn($(args...), Tuple(i)...)
        end

        function $fn($(args...), i::Integer...)
            idcs = to_indices($(args[1]), i)
            S = IndexStyle($(args[1]))
            if isa(S, IndexLinear)
                return $fn($(args...), _to_linear_index($(args[1]), idcs...))
            else
                return $fn($(args...), _to_subscript_indices($(args[1]), idcs...))
            end
        end
    end
end

## Bounds-checking with CartesianIndex
# Disallow linear indexing with CartesianIndex
@inline checkbounds(::Type{Bool}, A::AbstractArray, i::CartesianIndex) =
    checkbounds_indices(Bool, axes(A), (i,))
# Here we try to consume N of the indices (if there are that many available)
@inline function checkbounds_indices(::Type{Bool}, inds::Tuple, I::Tuple{CartesianIndex,Vararg})
    inds1, rest = IteratorsMD.split(inds, Val(length(I[1])))
    checkindex(Bool, inds1, I[1]) & checkbounds_indices(Bool, rest, tail(I))
end
@inline checkindex(::Type{Bool}, inds::Tuple, I::CartesianIndex) =
    checkbounds_indices(Bool, inds, I.I)
@inline checkindex(::Type{Bool}, inds::Tuple, i::AbstractRange{<:CartesianIndex}) =
    isempty(i) | (checkindex(Bool, inds, first(i)) & checkindex(Bool, inds, last(i)))

# Indexing into Array with mixtures of Integers and CartesianIndices is
# extremely performance-sensitive. While the abstract fallbacks support this,
# codegen has extra support for SIMDification that sub2ind doesn't (yet) support
@propagate_inbounds getindex(A::Array, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    A[to_indices(A, (i1, I...))...]
@propagate_inbounds setindex!(A::Array, v, i1::Union{Integer, CartesianIndex}, I::Union{Integer, CartesianIndex}...) =
    (A[to_indices(A, (i1, I...))...] = v; A)

## Bounds-checking with arrays of CartesianIndex{N}
# Disallow linear indexing with an array of CartesianIndex{N}
@inline checkbounds(::Type{Bool}, A::AbstractArray, i::AbstractArray{CartesianIndex{N}}) where {N} =
    checkbounds_indices(Bool, axes(A), (i,))
# Here we try to consume N of the indices (if there are that many available)
@inline function checkbounds_indices(::Type{Bool}, inds::Tuple, I::Tuple{AbstractArray{CartesianIndex{N}},Vararg}) where N
    inds1, rest = IteratorsMD.split(inds, Val(N))
    checkindex(Bool, inds1, I[1]) & checkbounds_indices(Bool, rest, tail(I))
end
@inline checkindex(::Type{Bool}, inds::Tuple, I::CartesianIndices) =
    checkbounds_indices(Bool, inds, I.indices)

# combined count of all indices, including CartesianIndex and
# AbstractArray{CartesianIndex}
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferable
@inline index_ndims(i1, I...) = (true, index_ndims(I...)...)
@inline function index_ndims(i1::CartesianIndex, I...)
    (map(Returns(true), i1.I)..., index_ndims(I...)...)
end
@inline function index_ndims(i1::AbstractArray{CartesianIndex{N}}, I...) where N
    (ntuple(Returns(true), Val(N))..., index_ndims(I...)...)
end
index_ndims() = ()

# combined dimensionality of all indices
# rather than returning N, it returns an NTuple{N,Bool} so the result is inferable
@inline index_dimsum(i1, I...) = (index_dimsum(I...)...,)
@inline index_dimsum(::Colon, I...) = (true, index_dimsum(I...)...)
@inline index_dimsum(::AbstractArray{Bool}, I...) = (true, index_dimsum(I...)...)
@inline function index_dimsum(::AbstractArray{<:Any,N}, I...) where N
    (ntuple(Returns(true), Val(N))..., index_dimsum(I...)...)
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
    idx, i = iterate(tail(s)...)::Tuple{Any,Any}
    s = (n+1, s[2], i)
    L.mask[idx] && return (idx, s)
    while true
        idx, i = iterate(tail(s)...)::Tuple{Any,Any}
        s = (n+1, s[2], i)
        L.mask[idx] && return (idx, s)
    end
end
# When wrapping a BitArray, lean heavily upon its internals.
@inline function iterate(L::LogicalIndex{Int,<:BitArray})
    L.sum == 0 && return nothing
    Bc = L.mask.chunks
    return iterate(L, (1, 1, (), @inbounds Bc[1]))
end
@inline function iterate(L::LogicalIndex{<:CartesianIndex,<:BitArray})
    L.sum == 0 && return nothing
    Bc = L.mask.chunks
    irest = ntuple(one, ndims(L.mask)-1)
    return iterate(L, (1, 1, irest, @inbounds Bc[1]))
end
@inline function iterate(L::LogicalIndex{<:Any,<:BitArray}, (i1, Bi, irest, c))
    Bc = L.mask.chunks
    while c == 0
        Bi >= length(Bc) && return nothing
        i1 += 64
        @inbounds c = Bc[Bi+=1]
    end
    tz = trailing_zeros(c)
    c = _blsr(c)
    i1, irest = _overflowind(i1 + tz, irest, size(L.mask))
    return eltype(L)(i1, irest...), (i1 - tz, Bi, irest, c)
end

## Boundscheck for Logicalindex
# LogicalIndex: map all calls to mask
checkbounds(::Type{Bool}, A::AbstractArray, i::LogicalIndex) = checkbounds(Bool, A, i.mask)
# `checkbounds_indices` has been handled via `I::AbstractArray` fallback
checkindex(::Type{Bool}, inds::AbstractUnitRange, i::LogicalIndex) = checkindex(Bool, inds, i.mask)
checkindex(::Type{Bool}, inds::Tuple, i::LogicalIndex) = checkindex(Bool, inds, i.mask)

## Boundscheck for AbstractArray{Bool}
# Disallow linear indexing with AbstractArray{Bool}
checkbounds(::Type{Bool}, A::AbstractArray, i::AbstractArray{Bool}) =
    checkbounds_indices(Bool, axes(A), (i,))
# But allow linear indexing with AbstractVector{Bool}
checkbounds(::Type{Bool}, A::AbstractArray, i::AbstractVector{Bool}) =
    checkindex(Bool, eachindex(IndexLinear(), A), i)
@inline function checkbounds_indices(::Type{Bool}, inds::Tuple, I::Tuple{AbstractArray{Bool},Vararg})
    inds1, rest = IteratorsMD.split(inds, Val(ndims(I[1])))
    checkindex(Bool, inds1, I[1]) & checkbounds_indices(Bool, rest, tail(I))
end
checkindex(::Type{Bool}, inds::AbstractUnitRange, I::AbstractVector{Bool}) = axes1(I) == inds
checkindex(::Type{Bool}, inds::AbstractUnitRange, I::AbstractRange{Bool}) = axes1(I) == inds
checkindex(::Type{Bool}, inds::Tuple, I::AbstractArray{Bool}) = _check_boolean_axes(inds, axes(I))
_check_boolean_axes(inds::Tuple, axes::Tuple) = (inds[1] == axes[1]) & _check_boolean_axes(tail(inds), tail(axes))
_check_boolean_axes(::Tuple{}, axes::Tuple) = all(==(OneTo(1)), axes)

ensure_indexable(I::Tuple{}) = ()
@inline ensure_indexable(I::Tuple{Any, Vararg{Any}}) = (I[1], ensure_indexable(tail(I))...)
@inline ensure_indexable(I::Tuple{LogicalIndex, Vararg{Any}}) = (collect(I[1]), ensure_indexable(tail(I))...)

# In simple cases, we know that we don't need to use axes(A). Optimize those
# until Julia gets smart enough to elide the call on its own:
@inline to_indices(A, I::Tuple{Vararg{Union{Integer, CartesianIndex}}}) = to_indices(A, (), I)
# But some index types require more context spanning multiple indices
# CartesianIndex is unfolded outside the inner to_indices for better inference
@inline function to_indices(A, inds, I::Tuple{CartesianIndex{N}, Vararg}) where N
    _, indstail = IteratorsMD.split(inds, Val(N))
    (map(Fix1(to_index, A), I[1].I)..., to_indices(A, indstail, tail(I))...)
end
# For arrays of CartesianIndex, we just skip the appropriate number of inds
@inline function to_indices(A, inds, I::Tuple{AbstractArray{CartesianIndex{N}}, Vararg}) where N
    _, indstail = IteratorsMD.split(inds, Val(N))
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# And boolean arrays behave similarly; they also skip their number of dimensions
@inline function to_indices(A, inds, I::Tuple{AbstractArray{Bool, N}, Vararg}) where N
    _, indstail = IteratorsMD.split(inds, Val(N))
    (to_index(A, I[1]), to_indices(A, indstail, tail(I))...)
end
# As an optimization, we allow the only `AbstractArray{Bool}` to be linear-iterated
@inline to_indices(A, I::Tuple{AbstractArray{Bool}}) = (_maybe_linear_logical_index(IndexStyle(A), A, I[1]),)
_maybe_linear_logical_index(::IndexStyle, A, i) = to_index(A, i)
_maybe_linear_logical_index(::IndexLinear, A, i) = LogicalIndex{Int}(i)

# Colons get converted to slices by `uncolon`
@inline to_indices(A, inds, I::Tuple{Colon, Vararg}) =
    (uncolon(inds), to_indices(A, Base.safe_tail(inds), tail(I))...)

uncolon(::Tuple{}) = Slice(OneTo(1))
uncolon(inds::Tuple) = Slice(inds[1])

"""
    _prechecked_iterate(iter[, state])

Internal function used to eliminate the dead branch in `iterate`.
Fallback to `iterate` by default, but optimized for indices type in `Base`.
"""
@propagate_inbounds _prechecked_iterate(iter) = iterate(iter)
@propagate_inbounds _prechecked_iterate(iter, state) = iterate(iter, state)

_prechecked_iterate(iter::AbstractUnitRange, i = first(iter)) = i, convert(eltype(iter), i + step(iter))
_prechecked_iterate(iter::LinearIndices, i = first(iter)) = i, i + 1
_prechecked_iterate(iter::CartesianIndices) = first(iter), first(iter)
function _prechecked_iterate(iter::CartesianIndices, i)
    i′ = IteratorsMD.inc(i.I, iter.indices)
    return i′, i′
end
_prechecked_iterate(iter::SCartesianIndices2) = first(iter), first(iter)
function _prechecked_iterate(iter::SCartesianIndices2{K}, (;i, j)) where {K}
    I = i < K ? SCartesianIndex2{K}(i + 1, j) : SCartesianIndex2{K}(1, j + 1)
    return I, I
end

### From abstractarray.jl: Internal multidimensional indexing definitions ###
getindex(x::Union{Number,AbstractChar}, ::CartesianIndex{0}) = x
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
    map(length, axes(dest)) == map(length, shape) || throw_checksize_error(dest, shape)
    _unsafe_getindex!(dest, A, I...) # usually a generated function, don't allow it to impact inference result
    return dest
end

function _generate_unsafe_getindex!_body(N::Int)
    quote
        @inline
        D = eachindex(dest)
        Dy = _prechecked_iterate(D)
        @inbounds @nloops $N j d->I[d] begin
            (idx, state) = Dy::NTuple{2,Any}
            dest[idx] = @ncall $N getindex src j
            Dy = _prechecked_iterate(D, state)
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
    @inline
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
        X = eachindex(x′)
        Xy = _prechecked_iterate(X)
        @inbounds @nloops $N i d->I_d begin
            (idx, state) = Xy::NTuple{2,Any}
            @ncall $N setindex! A x′[idx] i
            Xy = _prechecked_iterate(X, state)
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
2×2 Matrix{Int64}:
 2   4
 6  16

julia> diff(a, dims=2)
2×1 Matrix{Int64}:
  2
 10

julia> diff(vec(a))
3-element Vector{Int64}:
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
    return [@inbounds r[i+1] - r[i] for i in firstindex(r):lastindex(r)-1]
end

### from abstractarray.jl

function mightalias(A::SubArray, B::SubArray)
    # There are three ways that SubArrays might _problematically_ alias one another:
    #   1. The parents are the same we can conservatively check if the indices might overlap OR
    #   2. The parents alias eachother in a more complicated manner (and we can't trace indices) OR
    #   3. One's parent is used in the other's indices
    # Note that it's ok for just the indices to alias each other as those should not be mutated,
    # so we can always do better than the default !_isdisjoint(dataids(A), dataids(B))
    if isbits(A.parent) || isbits(B.parent)
        return false # Quick out for immutables
    elseif _parentsmatch(A.parent, B.parent)
        # Each SubArray unaliases its own parent from its own indices upon construction, so if
        # the two parents are the same, then by construction one cannot alias the other's indices
        # and therefore this is the only test we need to perform:
        return _indicesmightoverlap(A.indices, B.indices)
    else
        A_parent_ids = dataids(A.parent)
        B_parent_ids = dataids(B.parent)
        return !_isdisjoint(A_parent_ids, B_parent_ids) ||
            !_isdisjoint(A_parent_ids, _splatmap(dataids, B.indices)) ||
            !_isdisjoint(B_parent_ids, _splatmap(dataids, A.indices))
    end
end
# Test if two arrays are backed by exactly the same memory in exactly the same order
_parentsmatch(A::AbstractArray, B::AbstractArray) = A === B
_parentsmatch(A::DenseArray, B::DenseArray) = elsize(A) == elsize(B) && pointer(A) == pointer(B) && size(A) == size(B)
_parentsmatch(A::StridedArray, B::StridedArray) = elsize(A) == elsize(B) && pointer(A) == pointer(B) && strides(A) == strides(B)

# Given two SubArrays with the same parent, check if the indices might overlap (returning true if unsure)
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
2×3 Matrix{Float64}:
 0.0  0.0  0.0
 0.0  0.0  0.0

julia> fill!(A, 2.)
2×3 Matrix{Float64}:
 2.0  2.0  2.0
 2.0  2.0  2.0

julia> a = [1, 1, 1]; A = fill!(Vector{Vector{Int}}(undef, 3), a); a[1] = 2; A
3-element Vector{Vector{Int64}}:
 [2, 1, 1]
 [2, 1, 1]
 [2, 1, 1]

julia> x = 0; f() = (global x += 1; x); fill!(Vector{Int}(undef, 3), f())
3-element Vector{Int64}:
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
    CRdest = CartesianIndices(Rdest)
    CRsrc = CartesianIndices(Rsrc)
    ΔI = first(CRdest) - first(CRsrc)
    if @generated
        quote
            @nloops $N i (n->CRsrc.indices[n]) begin
                @inbounds @nref($N,dest,n->Rdest.indices[n][i_n+ΔI[n]]) = @nref($N,src′,n->Rsrc.indices[n][i_n])
            end
        end
    else
        for I in CRsrc
            @inbounds dest[Rdest[I + ΔI]] = src′[Rsrc[I]]
        end
    end
    dest
end

"""
    copyto!(dest, Rdest::CartesianIndices, src, Rsrc::CartesianIndices) -> dest

Copy the block of `src` in the range of `Rsrc` to the block of `dest`
in the range of `Rdest`. The sizes of the two regions must match.

# Examples
```jldoctest
julia> A = zeros(5, 5);

julia> B = [1 2; 3 4];

julia> Ainds = CartesianIndices((2:3, 2:3));

julia> Binds = CartesianIndices(B);

julia> copyto!(A, Ainds, B, Binds)
5×5 Matrix{Float64}:
 0.0  0.0  0.0  0.0  0.0
 0.0  1.0  2.0  0.0  0.0
 0.0  3.0  4.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0  0.0
```
"""
copyto!(::AbstractArray, ::CartesianIndices, ::AbstractArray, ::CartesianIndices)

# circshift!
circshift!(dest::AbstractArray, src, ::Tuple{}) = copyto!(dest, src)
"""
    circshift!(dest, src, shifts)

Circularly shift, i.e. rotate, the data in `src`, storing the result in
`dest`. `shifts` specifies the amount to shift in each dimension.

$(_DOCS_ALIASING_WARNING)

See also [`circshift`](@ref).
"""
@noinline function circshift!(dest::AbstractArray{T,N}, src, shiftamt::DimsInteger) where {T,N}
    dest === src && throw(ArgumentError("dest and src must be separate arrays"))
    inds = axes(src)
    axes(dest) == inds || throw(ArgumentError("indices of src and dest must match (got $inds and $(axes(dest)))"))
    isempty(src) && return dest
    _circshift!(dest, (), src, (), inds, fill_to_length(shiftamt, 0, Val(N)))
end

circshift!(dest::AbstractArray, src, shiftamt) =
    circshift!(dest, src, map(Integer, (shiftamt...,)))

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

$(_DOCS_ALIASING_WARNING)

See also: [`circshift`](@ref).

# Examples
```julia-repl
julia> src = reshape(Vector(1:16), (4,4))
4×4 Matrix{Int64}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> dest = OffsetArray{Int}(undef, (0:3,2:5));

julia> circcopy!(dest, src)
4×4 OffsetArray(::Matrix{Int64}, 0:3, 2:5) with eltype Int64 with indices 0:3×2:5:
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
@inline function _unsafe_getindex!(X::BitArray, B::BitArray, I0::Union{AbstractUnitRange{Int},Slice})
    copy_chunks!(X.chunks, 1, B.chunks, indexoffset(I0)+1, length(I0))
    return X
end

# Optimization where the inner dimension is contiguous improves perf dramatically
@generated function _unsafe_getindex!(X::BitArray, B::BitArray,
        I0::Union{Slice,UnitRange{Int}}, I::Union{Int,AbstractUnitRange{Int},Slice}...)
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
# they're used to preemptively check in bulk when possible, which is much faster.
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

@inline function setindex!(B::BitArray, X::Union{StridedArray,BitArray}, J0::Union{Colon,AbstractUnitRange{Int}})
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
        I0::Union{Colon,AbstractUnitRange{Int}}, I::Union{Int,AbstractUnitRange{Int},Colon}...)
    J = to_indices(B, (I0, I...))
    @boundscheck checkbounds(B, J...)
    _unsafe_setindex!(B, X, J...)
end
@generated function _unsafe_setindex!(B::BitArray, X::Union{StridedArray,BitArray},
        I0::Union{Slice,AbstractUnitRange{Int}}, I::Union{Int,AbstractUnitRange{Int},Slice}...)
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
        I0::Union{Colon,AbstractUnitRange{Int}}, I::Union{Int,AbstractUnitRange{Int},Colon}...)
    _setindex!(IndexStyle(B), B, X, to_indices(B, (I0, I...))...)
end

## fill! contiguous views of BitArrays with a single value
function fill!(V::SubArray{Bool, <:Any, <:BitArray, <:Tuple{AbstractUnitRange{Int}}}, x)
    B = V.parent
    I0 = V.indices[1]
    l0 = length(I0)
    l0 == 0 && return V
    fill_chunks!(B.chunks, Bool(x), first(I0), l0)
    return V
end

fill!(V::SubArray{Bool, <:Any, <:BitArray, <:Tuple{AbstractUnitRange{Int}, Vararg{Union{Int,AbstractUnitRange{Int}}}}}, x) =
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

@propagate_inbounds isassigned(A::AbstractArray, i::CartesianIndex) = isassigned(A, Tuple(i)...)
@propagate_inbounds function isassigned(A::AbstractArray, i::Union{Integer, CartesianIndex}...)
    return isassigned(A, CartesianIndex(to_indices(A, i)))
end
@inline function isassigned(A::AbstractArray, i::Integer...)
    # convert to valid indices, checking for Bool
    inds = to_indices(A, i)
    @boundscheck checkbounds(Bool, A, inds...) || return false
    S = IndexStyle(A)
    ninds = length(inds)
    if (isa(S, IndexLinear) && ninds != 1)
        return @inbounds isassigned(A, _to_linear_index(A, inds...))
    elseif (!isa(S, IndexLinear) && ninds != ndims(A))
        return @inbounds isassigned(A, _to_subscript_indices(A, inds...)...)
    else
       try
            A[inds...]
            true
        catch e
            if isa(e, BoundsError) || isa(e, UndefRefError)
                return false
            else
                rethrow()
            end
        end
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

checkdims_perm(P::AbstractArray{TP,N}, B::AbstractArray{TB,N}, perm) where {TP,TB,N} = checkdims_perm(axes(P), axes(B), perm)
function checkdims_perm(indsP::NTuple{N, AbstractUnitRange}, indsB::NTuple{N, AbstractUnitRange}, perm) where {N}
    length(perm) == N || throw(ArgumentError(LazyString("expected permutation of size ", N, ", but length(perm)=", length(perm))))
    isperm(perm) || throw(ArgumentError("input is not a permutation"))
    for i in eachindex(perm)
        indsP[i] == indsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
    end
    nothing
end

for (V, PT, BT) in Any[((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval @generated function permutedims!(P::$PT{$(V...)}, B::$BT{$(V...)}, perm) where $(V...)
        quote
            checkdims_perm(axes(P), axes(B), perm)

            #calculates all the strides
            native_strides = size_to_strides(1, size(B)...)
            strides = @ntuple $N d->native_strides[perm[d]]
            strides::NTuple{$N,Integer}

            #Creates offset, because indexing starts at 1
            offset = 1 - reduce(+, strides, init = 0)

            sumc = 0
            ind = 1
            @nloops($N, i, P,
                    d->(sumc += i_d*strides[d]), # PRE
                    d->(sumc -= i_d*strides[d]), # POST
                    begin # BODY
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
2×2×2 Array{Bool, 3}:
[:, :, 1] =
 1  1
 0  0

[:, :, 2] =
 1  1
 0  0

julia> unique(A)
2-element Vector{Bool}:
 1
 0

julia> unique(A, dims=2)
2×1×2 Array{Bool, 3}:
[:, :, 1] =
 1
 0

[:, :, 2] =
 1
 0

julia> unique(A, dims=3)
2×2×1 Array{Bool, 3}:
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
                if !isequal((@nref $N A j), (@nref $N A i))
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
                    if !isequal((@nref $N A j), (@nref $N A i))
                        nowcollided[k] = true
                    end
                end
                (collided, nowcollided) = (nowcollided, collided)
            end
        end

        @nref $N A d->d == dim ? sort!(uniquerows) : (axes(A, d))
    end
end

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
3×3 Matrix{Int64}:
 -1   6  4
  7   3  5
  9  -2  8

julia> sortslices([7 3 5; -1 6 4; 9 -2 8], dims=1, lt=(x,y)->isless(x[2],y[2]))
3×3 Matrix{Int64}:
  9  -2  8
  7   3  5
 -1   6  4

julia> sortslices([7 3 5; -1 6 4; 9 -2 8], dims=1, rev=true)
3×3 Matrix{Int64}:
  9  -2  8
  7   3  5
 -1   6  4

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2) # Sort columns
3×3 Matrix{Int64}:
  3   5  7
 -1  -4  6
 -2   8  9

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2, alg=InsertionSort, lt=(x,y)->isless(x[2],y[2]))
3×3 Matrix{Int64}:
  5   3  7
 -4  -1  6
  8  -2  9

julia> sortslices([7 3 5; 6 -1 -4; 9 -2 8], dims=2, rev=true)
3×3 Matrix{Int64}:
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
are re-arranged such that the slices (of the remaining third dimension) are sorted.
If `dims` is `(2, 1)` instead, the same slices will be taken,
but the result order will be row-major instead.

# Higher dimensional examples
```
julia> A = [4 3; 2 1 ;;; 'A' 'B'; 'C' 'D']
2×2×2 Array{Any, 3}:
[:, :, 1] =
 4  3
 2  1

[:, :, 2] =
 'A'  'B'
 'C'  'D'

julia> sortslices(A, dims=(1,2))
2×2×2 Array{Any, 3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 'D'  'B'
 'C'  'A'

julia> sortslices(A, dims=(2,1))
2×2×2 Array{Any, 3}:
[:, :, 1] =
 1  2
 3  4

[:, :, 2] =
 'D'  'C'
 'B'  'A'

julia> sortslices(reshape([5; 4; 3; 2; 1], (1,1,5)), dims=3, by=x->x[1,1])
1×1×5 Array{Int64, 3}:
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
    if A isa Matrix && dims isa Integer && dims == 1
        # TODO: remove once the generic version becomes as fast or faster
        perm = sortperm(eachslice(A; dims); kws...)
        return A[perm, :]
    end

    B = similar(A)
    _sortslices!(B, A, Val{dims}(); kws...)
    B
end

function _sortslices!(B, A, ::Val{dims}; kws...) where dims
    ves = vec(eachslice(A; dims))
    perm = sortperm(ves; kws...)
    bes = eachslice(B; dims)

    # TODO for further optimization: traverse in memory order
    for (slice, i) in zip(eachslice(B; dims), perm)
        slice .= ves[i]
    end
end

getindex(b::Ref, ::CartesianIndex{0}) = getindex(b)
setindex!(b::Ref, x, ::CartesianIndex{0}) = setindex!(b, x)

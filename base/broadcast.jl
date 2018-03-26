# This file is a part of Julia. License is MIT: https://julialang.org/license

module Broadcast

using .Base.Cartesian
using .Base: Indices, OneTo, linearindices, tail, to_shape,
            _msk_end, unsafe_bitgetindex, bitcache_chunks, bitcache_size, dumpbitcache,
            isoperator, promote_typejoin, unalias
import .Base: broadcast, broadcast!
export BroadcastStyle, broadcast_indices, broadcast_similar,
       broadcast_getindex, broadcast_setindex!, dotview, @__dot__

### Objects with customized broadcasting behavior should declare a BroadcastStyle

"""
`BroadcastStyle` is an abstract type and trait-function used to determine behavior of
objects under broadcasting. `BroadcastStyle(typeof(x))` returns the style associated
with `x`. To customize the broadcasting behavior of a type, one can declare a style
by defining a type/method pair

    struct MyContainerStyle <: BroadcastStyle end
    Base.BroadcastStyle(::Type{<:MyContainer}) = MyContainerStyle()

One then writes method(s) (at least [`broadcast_similar`](@ref)) operating on
`MyContainerStyle`. There are also several pre-defined subtypes of `BroadcastStyle`
that you may be able to leverage; see the
[Interfaces chapter](@ref man-interfaces-broadcasting) for more information.
"""
abstract type BroadcastStyle end

"""
`Broadcast.Style{C}()` defines a [`BroadcastStyle`](@ref) signaling through the type
parameter `C`. You can use this as an alternative to creating custom subtypes of `BroadcastStyle`,
for example

    Base.BroadcastStyle(::Type{<:MyContainer}) = Broadcast.Style{MyContainer}()

There is a pre-defined [`broadcast_similar`](@ref) method

    broadcast_similar(f, ::Style{C}, ::Type{ElType}, inds, args...) =
        similar(C, ElType, inds)

Naturally you can specialize this for your particular `C` (e.g., `MyContainer`).
"""
struct Style{T} <: BroadcastStyle end

BroadcastStyle(::Type{<:Tuple}) = Style{Tuple}()

struct Unknown <: BroadcastStyle end
BroadcastStyle(::Type{Union{}}) = Unknown()  # ambiguity resolution
BroadcastStyle(::Type) = Unknown()

"""
`Broadcast.AbstractArrayStyle{N} <: BroadcastStyle` is the abstract supertype for any style
associated with an `AbstractArray` type.
The `N` parameter is the dimensionality, which can be handy for AbstractArray types
that only support specific dimensionalities:

    struct SparseMatrixStyle <: Broadcast.AbstractArrayStyle{2} end
    Base.BroadcastStyle(::Type{<:SparseMatrixCSC}) = SparseMatrixStyle()

For AbstractArray types that support arbitrary dimensionality, `N` can be set to `Any`:

    struct MyArrayStyle <: Broadcast.AbstractArrayStyle{Any} end
    Base.BroadcastStyle(::Type{<:MyArray}) = MyArrayStyle()

In cases where you want to be able to mix multiple `AbstractArrayStyle`s and keep track
of dimensionality, your style needs to support a `Val` constructor:

    struct MyArrayStyleDim{N} <: Broadcast.AbstractArrayStyle{N} end
    (::Type{<:MyArrayStyleDim})(::Val{N}) where N = MyArrayStyleDim{N}()

Note that if two or more `AbstractArrayStyle` subtypes conflict, broadcasting machinery
will fall back to producing `Array`s. If this is undesirable, you may need to
define binary [`BroadcastStyle`](@ref) rules to control the output type.

See also [`Broadcast.DefaultArrayStyle`](@ref).
"""
abstract type AbstractArrayStyle{N} <: BroadcastStyle end

"""
`Broadcast.ArrayStyle{MyArrayType}()` is a [`BroadcastStyle`](@ref) indicating that an object
behaves as an array for broadcasting. It presents a simple way to construct
[`Broadcast.AbstractArrayStyle`](@ref)s for specific `AbstractArray` container types.
Broadcast styles created this way lose track of dimensionality; if keeping track is important
for your type, you should create your own custom [`Broadcast.AbstractArrayStyle`](@ref).
"""
struct ArrayStyle{A<:AbstractArray} <: AbstractArrayStyle{Any} end
(::Type{<:ArrayStyle{A}})(::Val) where A = A()

"""
`Broadcast.DefaultArrayStyle{N}()` is a [`BroadcastStyle`](@ref) indicating that an object
behaves as an `N`-dimensional array for broadcasting. Specifically, `DefaultArrayStyle` is
used for any
AbstractArray type that hasn't defined a specialized style, and in the absence of
overrides from other `broadcast` arguments the resulting output type is `Array`.
When there are multiple inputs to `broadcast`, `DefaultArrayStyle` "loses" to any other [`Broadcast.ArrayStyle`](@ref).
"""
struct DefaultArrayStyle{N} <: AbstractArrayStyle{N} end
(::Type{<:DefaultArrayStyle})(::Val{N}) where N = DefaultArrayStyle{N}()
const DefaultVectorStyle = DefaultArrayStyle{1}
const DefaultMatrixStyle = DefaultArrayStyle{2}
BroadcastStyle(::Type{<:AbstractArray{T,N}}) where {T,N} = DefaultArrayStyle{N}()
BroadcastStyle(::Type{<:Union{Ref,Number}}) = DefaultArrayStyle{0}()

# `ArrayConflict` is an internal type signaling that two or more different `AbstractArrayStyle`
# objects were supplied as arguments, and that no rule was defined for resolving the
# conflict. The resulting output is `Array`. While this is the same output type
# produced by `DefaultArrayStyle`, `ArrayConflict` "poisons" the BroadcastStyle so that
# 3 or more arguments still return an `ArrayConflict`.
struct ArrayConflict <: AbstractArrayStyle{Any} end

### Binary BroadcastStyle rules
"""
    BroadcastStyle(::Style1, ::Style2) = Style3()

Indicate how to resolve different `BroadcastStyle`s. For example,

    Broadcast.rule(::Primary, ::Secondary) = Primary()

would indicate that style `Primary` has precedence over `Secondary`.
You do not have to (and generally should not) define both argument orders.
The result does not have to be one of the input arguments, it could be a third type.

Please see the [Interfaces chapter](@ref man-interfaces-broadcasting) of the manual for
more information.
"""
BroadcastStyle(::S, ::S) where S<:BroadcastStyle = S() # homogeneous types preserved
# Fall back to Unknown. This is necessary to implement argument-swapping
BroadcastStyle(::BroadcastStyle, ::BroadcastStyle) = Unknown()
# Unknown loses to everything
BroadcastStyle(::Unknown, ::Unknown) = Unknown()
BroadcastStyle(::S, ::Unknown) where S<:BroadcastStyle = S()
# Precedence rules
BroadcastStyle(a::AbstractArrayStyle{0}, b::Style{Tuple}) = b
BroadcastStyle(a::AbstractArrayStyle, ::Style{Tuple})    = a
BroadcastStyle(::A, ::A) where A<:ArrayStyle             = A()
BroadcastStyle(::ArrayStyle, ::ArrayStyle)               = Unknown()
BroadcastStyle(::A, ::A) where A<:AbstractArrayStyle     = A()
Base.@pure function BroadcastStyle(a::A, b::B) where {A<:AbstractArrayStyle{M},B<:AbstractArrayStyle{N}} where {M,N}
    if Base.typename(A).wrapper == Base.typename(B).wrapper
        return A(_max(Val(M),Val(N)))
    end
    Unknown()
end
# Any specific array type beats DefaultArrayStyle
BroadcastStyle(a::AbstractArrayStyle{Any}, ::DefaultArrayStyle) = a
BroadcastStyle(a::AbstractArrayStyle{N}, ::DefaultArrayStyle{N}) where N = a
BroadcastStyle(a::AbstractArrayStyle{M}, ::DefaultArrayStyle{N}) where {M,N} =
    typeof(a)(_max(Val(M),Val(N)))

## Allocating the output container
"""
    broadcast_similar(f, ::BroadcastStyle, ::Type{ElType}, inds, As...)

Allocate an output object for [`broadcast`](@ref), appropriate for the indicated
[`Broadcast.BroadcastStyle`](@ref). `ElType` and `inds` specify the desired element type and indices of the
container.
`f` is the broadcast operation, and `As...` are the arguments supplied to `broadcast`.
"""
broadcast_similar(f, ::DefaultArrayStyle{N}, ::Type{ElType}, inds::Indices{N}, As...) where {N,ElType} =
    similar(Array{ElType}, inds)
broadcast_similar(f, ::DefaultArrayStyle{N}, ::Type{Bool}, inds::Indices{N}, As...) where N =
    similar(BitArray, inds)
# In cases of conflict we fall back on Array
broadcast_similar(f, ::ArrayConflict, ::Type{ElType}, inds::Indices, As...) where ElType =
    similar(Array{ElType}, inds)
broadcast_similar(f, ::ArrayConflict, ::Type{Bool}, inds::Indices, As...) =
    similar(BitArray, inds)

## Computing the result's indices. Most types probably won't need to specialize this.
broadcast_indices() = ()
broadcast_indices(::Type{T}) where T = ()
broadcast_indices(A) = broadcast_indices(combine_styles(A), A)
broadcast_indices(::Style{Tuple}, A) = (OneTo(length(A)),)
broadcast_indices(::DefaultArrayStyle{0}, A::Ref) = ()
broadcast_indices(::BroadcastStyle, A) = Base.axes(A)
"""
    Base.broadcast_indices(::SrcStyle, A)

Compute the indices for objects `A` with [`BroadcastStyle`](@ref) `SrcStyle`.
If needed, you can specialize this method for your styles.
You should only need to provide a custom implementation for non-AbstractArrayStyles.
"""
broadcast_indices

### End of methods that users will typically have to specialize ###

## Broadcasting utilities ##
# special cases defined for performance
broadcast(f, x::Number...) = f(x...)
@inline broadcast(f, t::NTuple{N,Any}, ts::Vararg{NTuple{N,Any}}) where {N} = map(f, t, ts...)

## logic for deciding the BroadcastStyle
# Dimensionality: computing max(M,N) in the type domain so we preserve inferrability
_max(V1::Val{Any}, V2::Val{Any}) = Val(Any)
_max(V1::Val{Any}, V2::Val{N}) where N = Val(Any)
_max(V1::Val{N}, V2::Val{Any}) where N = Val(Any)
_max(V1::Val, V2::Val) = __max(longest(ntuple(identity, V1), ntuple(identity, V2)))
__max(::NTuple{N,Bool}) where N = Val(N)
longest(t1::Tuple, t2::Tuple) = (true, longest(Base.tail(t1), Base.tail(t2))...)
longest(::Tuple{}, t2::Tuple) = (true, longest((), Base.tail(t2))...)
longest(t1::Tuple, ::Tuple{}) = (true, longest(Base.tail(t1), ())...)
longest(::Tuple{}, ::Tuple{}) = ()

# combine_styles operates on values (arbitrarily many)
combine_styles(c) = result_style(BroadcastStyle(typeof(c)))
combine_styles(c1, c2) = result_style(combine_styles(c1), combine_styles(c2))
@inline combine_styles(c1, c2, cs...) = result_style(combine_styles(c1), combine_styles(c2, cs...))

# result_style works on types (singletons and pairs), and leverages `BroadcastStyle`
result_style(s::BroadcastStyle) = s
result_style(s1::S, s2::S) where S<:BroadcastStyle = S()
# Test both orders so users typically only have to declare one order
result_style(s1, s2) = result_join(s1, s2, BroadcastStyle(s1, s2), BroadcastStyle(s2, s1))

# result_join is the final arbiter. Because `BroadcastStyle` for undeclared pairs results in Unknown,
# we defer to any case where the result of `BroadcastStyle` is known.
result_join(::Any, ::Any, ::Unknown, ::Unknown)   = Unknown()
result_join(::Any, ::Any, ::Unknown, s::BroadcastStyle) = s
result_join(::Any, ::Any, s::BroadcastStyle, ::Unknown) = s
# For AbstractArray types with specialized broadcasting and undefined precedence rules,
# we have to signal conflict. Because ArrayConflict is a subtype of AbstractArray,
# this will "poison" any future operations (if we instead returned `DefaultArrayStyle`, then for
# 3-array broadcasting the returned type would depend on argument order).
result_join(::AbstractArrayStyle, ::AbstractArrayStyle, ::Unknown, ::Unknown) =
    ArrayConflict()
# Fallbacks in case users define `rule` for both argument-orders (not recommended)
result_join(::Any, ::Any, ::S, ::S) where S<:BroadcastStyle = S()
@noinline function result_join(::S, ::T, ::U, ::V) where {S,T,U,V}
    error("""
conflicting broadcast rules defined
  Broadcast.BroadcastStyle(::$S, ::$T) = $U()
  Broadcast.BroadcastStyle(::$T, ::$S) = $V()
One of these should be undefined (and thus return Broadcast.Unknown).""")
end

# Indices utilities
combine_indices(A, B...) = broadcast_shape(broadcast_indices(A), combine_indices(B...))
combine_indices(A) = broadcast_indices(A)

# shape (i.e., tuple-of-indices) inputs
broadcast_shape(shape::Tuple) = shape
broadcast_shape(shape::Tuple, shape1::Tuple, shapes::Tuple...) = broadcast_shape(_bcs(shape, shape1), shapes...)
# _bcs consolidates two shapes into a single output shape
_bcs(::Tuple{}, ::Tuple{}) = ()
_bcs(::Tuple{}, newshape::Tuple) = (newshape[1], _bcs((), tail(newshape))...)
_bcs(shape::Tuple, ::Tuple{}) = (shape[1], _bcs(tail(shape), ())...)
function _bcs(shape::Tuple, newshape::Tuple)
    return (_bcs1(shape[1], newshape[1]), _bcs(tail(shape), tail(newshape))...)
end
# _bcs1 handles the logic for a single dimension
_bcs1(a::Integer, b::Integer) = a == 1 ? b : (b == 1 ? a : (a == b ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size"))))
_bcs1(a::Integer, b) = a == 1 ? b : (first(b) == 1 && last(b) == a ? b : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
_bcs1(a, b::Integer) = _bcs1(b, a)
_bcs1(a, b) = _bcsm(b, a) ? b : (_bcsm(a, b) ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
# _bcsm tests whether the second index is consistent with the first
_bcsm(a, b) = a == b || length(b) == 1
_bcsm(a, b::Number) = b == 1
_bcsm(a::Number, b::Number) = a == b || b == 1

## Check that all arguments are broadcast compatible with shape
# comparing one input against a shape
check_broadcast_shape(shp) = nothing
check_broadcast_shape(shp, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, Ashp::Tuple) = throw(DimensionMismatch("cannot broadcast array to have fewer dimensions"))
function check_broadcast_shape(shp, Ashp::Tuple)
    _bcsm(shp[1], Ashp[1]) || throw(DimensionMismatch("array could not be broadcast to match destination"))
    check_broadcast_shape(tail(shp), tail(Ashp))
end
check_broadcast_indices(shp, A) = check_broadcast_shape(shp, broadcast_indices(A))
# comparing many inputs
@inline function check_broadcast_indices(shp, A, As...)
    check_broadcast_indices(shp, A)
    check_broadcast_indices(shp, As...)
end

## Indexing manipulations

# newindex(I, keep, Idefault) replaces a CartesianIndex `I` with something that
# is appropriate for a particular broadcast array/scalar. `keep` is a
# NTuple{N,Bool}, where keep[d] == true means that one should preserve
# I[d]; if false, replace it with Idefault[d].
# If dot-broadcasting were already defined, this would be `ifelse.(keep, I, Idefault)`.
@inline newindex(I::CartesianIndex, keep, Idefault) = CartesianIndex(_newindex(I.I, keep, Idefault))
@inline _newindex(I, keep, Idefault) =
    (ifelse(keep[1], I[1], Idefault[1]), _newindex(tail(I), tail(keep), tail(Idefault))...)
@inline _newindex(I, keep::Tuple{}, Idefault) = ()  # truncate if keep is shorter than I

# newindexer(shape, A) generates `keep` and `Idefault` (for use by
# `newindex` above) for a particular array `A`, given the
# broadcast indices `shape`
# `keep` is equivalent to map(==, axes(A), shape) (but see #17126)
@inline newindexer(shape, A) = shapeindexer(shape, broadcast_indices(A))
@inline shapeindexer(shape, indsA::Tuple{}) = (), ()
@inline function shapeindexer(shape, indsA::Tuple)
    ind1 = indsA[1]
    keep, Idefault = shapeindexer(tail(shape), tail(indsA))
    (shape[1] == ind1, keep...), (first(ind1), Idefault...)
end

# Equivalent to map(x->newindexer(shape, x), As) (but see #17126)
map_newindexer(shape, ::Tuple{}) = (), ()
@inline function map_newindexer(shape, As)
    A1 = As[1]
    keeps, Idefaults = map_newindexer(shape, tail(As))
    keep, Idefault = newindexer(shape, A1)
    (keep, keeps...), (Idefault, Idefaults...)
end
@inline function map_newindexer(shape, A, Bs)
    keeps, Idefaults = map_newindexer(shape, Bs)
    keep, Idefault = newindexer(shape, A)
    (keep, keeps...), (Idefault, Idefaults...)
end

Base.@propagate_inbounds _broadcast_getindex(::Type{T}, I) where T = T
Base.@propagate_inbounds _broadcast_getindex(A, I) = _broadcast_getindex(combine_styles(A), A, I)
Base.@propagate_inbounds _broadcast_getindex(::DefaultArrayStyle{0}, A, I) = A[]
Base.@propagate_inbounds _broadcast_getindex(::Any, A, I) = A[I]
Base.@propagate_inbounds _broadcast_getindex(::Style{Tuple}, A::Tuple{Any}, I) = A[1]

## Broadcasting core
# nargs encodes the number of As arguments (which matches the number
# of keeps). The first two type parameters are to ensure specialization.
@generated function _broadcast!(f, B::AbstractArray, keeps::K, Idefaults::ID, A::AT, Bs::BT, ::Val{N}, iter) where {K,ID,AT,BT,N}
    nargs = N + 1
    quote
        $(Expr(:meta, :inline))
        # destructure the keeps and As tuples
        A_1 = A
        @nexprs $N i->(A_{i+1} = Bs[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        @simd for I in iter
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function and store the result
            result = @ncall $nargs f val
            @inbounds B[I] = result
        end
        return B
    end
end

# For BitArray outputs, we cache the result in a "small" Vector{Bool},
# and then copy in chunks into the output
@generated function _broadcast!(f, B::BitArray, keeps::K, Idefaults::ID, A::AT, Bs::BT, ::Val{N}, iter) where {K,ID,AT,BT,N}
    nargs = N + 1
    quote
        $(Expr(:meta, :inline))
        # destructure the keeps and As tuples
        A_1 = A
        @nexprs $N i->(A_{i+1} = Bs[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        C = Vector{Bool}(undef, bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @simd for I in iter
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function and store the result
            @inbounds C[ind] = @ncall $nargs f val
            ind += 1
            if ind > bitcache_size
                dumpbitcache(Bc, cind, C)
                cind += bitcache_chunks
                ind = 1
            end
        end
        if ind > 1
            @inbounds C[ind:bitcache_size] = false
            dumpbitcache(Bc, cind, C)
        end
        return B
    end
end

"""
    broadcastable(x)

Return either `x` or an object like `x` such that it supports `axes` and indexing.

If `x` supports iteration, the returned value should have the same `axes` and indexing behaviors as [`collect(x)`](@ref).

# Examples
```jldoctest
julia> broadcastable([1,2,3]) # like `identity` since arrays already support axes and indexing
3-element Array{Int64,1}:
 1
 2
 3

julia> broadcastable(Int) # Types don't support axes, indexing, or iteration but are commonly used as scalars
Base.RefValue{Type{Int64}}(Int64)

julia> broadcastable("hello") # Strings break convention of matching iteration and act like a scalar instead
Base.RefValue{String}("hello")
```
"""
broadcastable(x::Union{Symbol,AbstractString,Function,UndefInitializer,Nothing,RoundingMode,Missing}) = Ref(x)
broadcastable(x::Ptr) = Ref{Ptr}(x) # Cannot use Ref(::Ptr) until ambiguous deprecation goes through
broadcastable(::Type{T}) where {T} = Ref{Type{T}}(T)
broadcastable(x::AbstractArray) = x
# In the future, default to collecting arguments. TODO: uncomment once deprecations are removed
# broadcastable(x) = BroadcastStyle(typeof(x)) isa Unknown ? collect(x) : x
# broadcastable(::Union{AbstractDict, NamedTuple}) = error("intentionally unimplemented to allow development in 1.x")

"""
    broadcast!(f, dest, As...)

Like [`broadcast`](@ref), but store the result of
`broadcast(f, As...)` in the `dest` array.
Note that `dest` is only used to store the result, and does not supply
arguments to `f` unless it is also listed in the `As`,
as in `broadcast!(f, A, A, B)` to perform `A[:] = broadcast(f, A, B)`.
"""
@inline function broadcast!(f::Tf, dest, As::Vararg{Any,N}) where {Tf,N}
    As′ = map(broadcastable, As)
    broadcast!(f, dest, combine_styles(As′...), As′...)
end
@inline broadcast!(f::Tf, dest, ::BroadcastStyle, As::Vararg{Any,N}) where {Tf,N} = broadcast!(f, dest, nothing, As...)

# Default behavior (separated out so that it can be called by users who want to extend broadcast!).
@inline function broadcast!(f, dest, ::Nothing, As::Vararg{Any, N}) where N
    if f isa typeof(identity) && N == 1
        A = As[1]
        if A isa AbstractArray && Base.axes(dest) == Base.axes(A)
            return copyto!(dest, A)
        end
    end
    _broadcast!(f, dest, As...)
    return dest
end

# Optimization for the case where all arguments are 0-dimensional
@inline function broadcast!(f, dest, ::AbstractArrayStyle{0}, As::Vararg{Any, N}) where N
    if dest isa AbstractArray
        if f isa typeof(identity) && N == 1
            return fill!(dest, As[1][])
        else
            @inbounds for I in eachindex(dest)
                dest[I] = f(map(getindex, As)...)
            end
            return dest
        end
    end
    _broadcast!(f, dest, As...)
    return dest
end

# For broadcasted assignments like `broadcast!(f, A, ..., A, ...)`, where `A`
# appears on both the LHS and the RHS of the `.=`, then we know we're only
# going to make one pass through the array, and even though `A` is aliasing
# against itself, the mutations won't affect the result as the indices on the
# LHS and RHS will always match. This is not true in general, but with the `.op=`
# syntax it's fairly common for an argument to be `===` a source.
broadcast_unalias(dest, src) = dest === src ? src : unalias(dest, src)

# This indirection allows size-dependent implementations.
@inline function _broadcast!(f, C, A, Bs::Vararg{Any,N}) where N
    shape = broadcast_indices(C)
    @boundscheck check_broadcast_indices(shape, A, Bs...)
    A′ = broadcast_unalias(C, A)
    Bs′ = map(B->broadcast_unalias(C, B), Bs)
    keeps, Idefaults = map_newindexer(shape, A′, Bs′)
    iter = CartesianIndices(shape)
    _broadcast!(f, C, keeps, Idefaults, A′, Bs′, Val(N), iter)
    return C
end

# broadcast with element type adjusted on-the-fly. This widens the element type of
# B as needed (allocating a new container and copying previously-computed values) to
# accommodate any incompatible new elements.
@generated function _broadcast!(f, B::AbstractArray, keeps::K, Idefaults::ID, As::AT, ::Val{nargs}, iter, st, count) where {K,ID,AT,nargs}
    quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        while !done(iter, st)
            I, st = next(iter, st)
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function
            V = @ncall $nargs f val
            # store the result
            if V isa eltype(B)
                @inbounds B[I] = V
            else
                # This element type doesn't fit in B. Allocate a new B with wider eltype,
                # copy over old values, and continue
                newB = Base.similar(B, promote_typejoin(eltype(B), typeof(V)))
                for II in Iterators.take(iter, count)
                    newB[II] = B[II]
                end
                newB[I] = V
                return _broadcast!(f, newB, keeps, Idefaults, As, Val(nargs), iter, st, count+1)
            end
            count += 1
        end
        return B
    end
end

maptoTuple(f) = Tuple{}
maptoTuple(f, a, b...) = Tuple{f(a), maptoTuple(f, b...).types...}

# An element type satisfying for all A:
# broadcast_getindex(
#     combine_styles(A),
#     A, broadcast_indices(A)
# )::_broadcast_getindex_eltype(A)
_broadcast_getindex_eltype(A) = _broadcast_getindex_eltype(combine_styles(A), A)
_broadcast_getindex_eltype(::BroadcastStyle, A) = eltype(A)  # Tuple, Array, etc.
_broadcast_getindex_eltype(::DefaultArrayStyle{0}, ::Ref{T}) where {T} = T

# Inferred eltype of result of broadcast(f, xs...)
combine_eltypes(f, A, As...) =
    Base._return_type(f, maptoTuple(_broadcast_getindex_eltype, A, As...))

"""
    broadcast(f, As...)

Broadcast the function `f` over the arrays, tuples, collections, `Ref`s and/or scalars `As`.

Broadcasting applies the function `f` over the elements of the container arguments and the
scalars themselves in `As`. Singleton and missing dimensions are expanded to match the
extents of the other arguments by virtually repeating the value. By default, only a limited
number of types are considered scalars, including `Number`s, `String`s, `Symbol`s, `Type`s,
`Function`s and some common singletons like `missing` and `nothing`. All other arguments are
iterated over or indexed into elementwise.

The resulting container type is established by the following rules:

 - If all the arguments are scalars or zero-dimensional arrays, it returns an unwrapped scalar.
 - If at least one argument is a tuple and all others are scalars or zero-dimensional arrays,
   it returns a tuple.
 - All other combinations of arguments default to returning an `Array`, but
   custom container types can define their own implementation and promotion-like
   rules to customize the result when they appear as arguments.

A special syntax exists for broadcasting: `f.(args...)` is equivalent to
`broadcast(f, args...)`, and nested `f.(g.(args...))` calls are fused into a
single broadcast loop.

# Examples
```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Array{Int64,2}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> broadcast(+, A, B)
5×2 Array{Int64,2}:
  2   3
  5   6
  8   9
 11  12
 14  15

julia> parse.(Int, ["1", "2"])
2-element Array{Int64,1}:
 1
 2

julia> abs.((1, -2))
(1, 2)

julia> broadcast(+, 1.0, (0, -2.0))
(1.0, -1.0)

julia> broadcast(+, 1.0, (0, -2.0), Ref(1))
2-element Array{Float64,1}:
 2.0
 0.0

julia> (+).([[0,2], [1,3]], Ref{Vector{Int}}([1,-1]))
2-element Array{Array{Int64,1},1}:
 [1, 1]
 [2, 2]

julia> string.(("one","two","three","four"), ": ", 1:4)
4-element Array{String,1}:
 "one: 1"
 "two: 2"
 "three: 3"
 "four: 4"

```
"""
@inline function broadcast(f, A, Bs...)
    A′ = broadcastable(A)
    Bs′ = map(broadcastable, Bs)
    broadcast(f, combine_styles(A′, Bs′...), nothing, nothing, A′, Bs′...)
end

# In the scalar case we unwrap the arguments and just call `f`
@inline broadcast(f, ::AbstractArrayStyle{0}, ::Nothing, ::Nothing, A, Bs...) = f(A[], map(getindex, Bs)...)

@inline broadcast(f, s::BroadcastStyle, ::Nothing, ::Nothing, A, Bs...) =
    broadcast(f, s, combine_eltypes(f, A, Bs...), combine_indices(A, Bs...), A, Bs...)

const NonleafHandlingTypes = Union{DefaultArrayStyle,ArrayConflict}

@inline function broadcast(f, s::NonleafHandlingTypes, ::Type{ElType}, inds::Indices, As...) where ElType
    if !Base.isconcretetype(ElType)
        return broadcast_nonleaf(f, s, ElType, inds, As...)
    end
    dest = broadcast_similar(f, s, ElType, inds, As...)
    broadcast!(f, dest, As...)
end

@inline function broadcast(f, s::BroadcastStyle, ::Type{ElType}, inds::Indices, As...) where ElType
    dest = broadcast_similar(f, s, ElType, inds, As...)
    broadcast!(f, dest, As...)
end

# When ElType is not concrete, use narrowing. Use the first element of each input to determine
# the starting output eltype; the _broadcast! method will widen `dest` as needed to
# accommodate later values.
function broadcast_nonleaf(f, s::NonleafHandlingTypes, ::Type{ElType}, shape::Indices, As...) where ElType
    nargs = length(As)
    iter = CartesianIndices(shape)
    if isempty(iter)
        return Base.similar(Array{ElType}, shape)
    end
    keeps, Idefaults = map_newindexer(shape, As)
    st = start(iter)
    I, st = next(iter, st)
    val = f([ _broadcast_getindex(As[i], newindex(I, keeps[i], Idefaults[i])) for i=1:nargs ]...)
    if val isa Bool
        dest = Base.similar(BitArray, shape)
    else
        dest = Base.similar(Array{typeof(val)}, shape)
    end
    dest[I] = val
    _broadcast!(f, dest, keeps, Idefaults, As, Val(nargs), iter, st, 1)
end

@inline broadcast(f, ::Style{Tuple}, ::Nothing, ::Nothing, A, Bs...) =
    tuplebroadcast(f, longest_tuple(A, Bs...), A, Bs...)
@inline tuplebroadcast(f, ::NTuple{N,Any}, As...) where {N} =
    ntuple(k -> f(tuplebroadcast_getargs(As, k)...), Val(N))
@inline tuplebroadcast(f, ::NTuple{N,Any}, ::Ref{Type{T}}, As...) where {N,T} =
    ntuple(k -> f(T, tuplebroadcast_getargs(As, k)...), Val(N))
longest_tuple(A::Tuple, B::Tuple, Bs...) = longest_tuple(_longest_tuple(A, B), Bs...)
longest_tuple(A, B::Tuple, Bs...) = longest_tuple(B, Bs...)
longest_tuple(A::Tuple, B, Bs...) = longest_tuple(A, Bs...)
longest_tuple(A, B, Bs...) = longest_tuple(Bs...)
longest_tuple(A::Tuple) = A
# Support only 1-tuples and N-tuples where there are no conflicts in N
_longest_tuple(A::Tuple{Any}, B::Tuple{Any}) = A
_longest_tuple(A::NTuple{N,Any}, B::NTuple{N,Any}) where N = A
_longest_tuple(A::NTuple{N,Any}, B::Tuple{Any}) where N = A
_longest_tuple(A::Tuple{Any}, B::NTuple{N,Any}) where N = B
@noinline _longest_tuple(A, B) =
    throw(DimensionMismatch("tuples $A and $B could not be broadcast to a common size"))

tuplebroadcast_getargs(::Tuple{}, k) = ()
@inline tuplebroadcast_getargs(As, k) =
    (_broadcast_getindex(first(As), k), tuplebroadcast_getargs(tail(As), k)...)


"""
    broadcast_getindex(A, inds...)

Equivalent to [`broadcast`](@ref)ing the `inds` arrays to a common size
and returning an array `[A[ks...] for ks in zip(indsb...)]` (where `indsb`
would be the broadcast `inds`). The shape of the output is equal to the shape of each
element of `indsb`.

# Examples
```jldoctest bc_getindex
julia> A = [11 12; 21 22]
2×2 Array{Int64,2}:
 11  12
 21  22

julia> A[1:2, 1:2]
2×2 Array{Int64,2}:
 11  12
 21  22

julia> broadcast_getindex(A, 1:2, 1:2)
2-element Array{Int64,1}:
 11
 22

julia> A[1:2, 2:-1:1]
2×2 Array{Int64,2}:
 12  11
 22  21

julia> broadcast_getindex(A, 1:2, 2:-1:1)
2-element Array{Int64,1}:
 12
 21
```
Because the indices are all vectors, these calls are like `[A[i[k], j[k]] for k = 1:2]`
where `i` and `j` are the two index vectors.

```jldoctest bc_getindex
julia> broadcast_getindex(A, 1:2, (1:2)')
2×2 Array{Int64,2}:
 11  12
 21  22

julia> broadcast_getindex(A, (1:2)', 1:2)
2×2 Array{Int64,2}:
 11  21
 12  22

julia> broadcast_getindex(A, [1 2 1; 1 2 2], [1, 2])
2×3 Array{Int64,2}:
 11  21  11
 12  22  22
```
"""
broadcast_getindex(src::AbstractArray, I::AbstractArray...) =
    broadcast_getindex!(Base.similar(Array{eltype(src)}, combine_indices(I...)),
                        src,
                        I...)

@generated function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        check_broadcast_indices(Base.axes(dest), $(Isplat...))  # unnecessary if this function is never called directly
        checkbounds(src, $(Isplat...))
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = Base.axes(I_k, d) == OneTo(1)))
        @nloops $N i dest d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
            @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
            @inbounds (@nref $N dest i) = (@nref $N src J)
        end
        dest
    end
end

"""
    broadcast_setindex!(A, X, inds...)

Efficient element-by-element setting of the values of `A` in a pattern established by `inds`.
Equivalent to broadcasting the `X` and `inds` arrays to a common size, and then executing

    for (is, js) in zip(zip(indsb), eachindex(Xb))
        A[is...] = Xb[js...]
    end

where `Xb` and `indsb` are the broadcast `X` and `inds`.

See [`broadcast_getindex`](@ref) for examples of the treatment of `inds`.
"""
@generated function broadcast_setindex!(A::AbstractArray, x, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        checkbounds(A, $(Isplat...))
        shape = combine_indices($(Isplat...))
        @nextract $N shape d->(length(shape) < d ? OneTo(1) : shape[d])
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = Base.axes(I_k, d) == 1:1))
        if !isa(x, AbstractArray)
            xA = convert(eltype(A), x)
            @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = xA
            end
        else
            X = x
            @nexprs $N d->(shapelen_d = length(shape_d))
            @ncall $N Base.setindex_shape_check X shapelen
            Xstate = start(X)
            @inbounds @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(J_k = @nref $N I_k d->j_d_k)
                x_el, Xstate = next(X, Xstate)
                (@nref $N A J) = x_el
            end
        end
        A
    end
end

############################################################

# x[...] .= f.(y...) ---> broadcast!(f, dotview(x, ...), y...).
# The dotview function defaults to getindex, but we override it in
# a few cases to get the expected in-place behavior without affecting
# explicit calls to view.   (All of this can go away if slices
# are changed to generate views by default.)

Base.@propagate_inbounds dotview(args...) = Base.maybeview(args...)

############################################################
# The parser turns @. into a call to the __dot__ macro,
# which converts all function calls and assignments into
# broadcasting "dot" calls/assignments:

dottable(x) = false # avoid dotting spliced objects (e.g. view calls inserted by @view)
# don't add dots to dot operators
dottable(x::Symbol) = (!isoperator(x) || first(string(x)) != '.' || x === :..) && x !== :(:)
dottable(x::Expr) = x.head != :$
undot(x) = x
function undot(x::Expr)
    if x.head == :.=
        Expr(:(=), x.args...)
    elseif x.head == :block # occurs in for x=..., y=...
        Expr(:block, map(undot, x.args)...)
    else
        x
    end
end
__dot__(x) = x
function __dot__(x::Expr)
    dotargs = map(__dot__, x.args)
    if x.head == :call && dottable(x.args[1])
        Expr(:., dotargs[1], Expr(:tuple, dotargs[2:end]...))
    elseif x.head == :$
        x.args[1]
    elseif x.head == :let # don't add dots to `let x=...` assignments
        Expr(:let, undot(dotargs[1]), dotargs[2])
    elseif x.head == :for # don't add dots to for x=... assignments
        Expr(:for, undot(dotargs[1]), dotargs[2])
    elseif (x.head == :(=) || x.head == :function || x.head == :macro) &&
           Meta.isexpr(x.args[1], :call) # function or macro definition
        Expr(x.head, x.args[1], dotargs[2])
    else
        head = string(x.head)
        if last(head) == '=' && first(head) != '.'
            Expr(Symbol('.',head), dotargs...)
        else
            Expr(x.head, dotargs...)
        end
    end
end
"""
    @. expr

Convert every function call or operator in `expr` into a "dot call"
(e.g. convert `f(x)` to `f.(x)`), and convert every assignment in `expr`
to a "dot assignment" (e.g. convert `+=` to `.+=`).

If you want to *avoid* adding dots for selected function calls in
`expr`, splice those function calls in with `\$`.  For example,
`@. sqrt(abs(\$sort(x)))` is equivalent to `sqrt.(abs.(sort(x)))`
(no dot for `sort`).

(`@.` is equivalent to a call to `@__dot__`.)

# Examples
```jldoctest
julia> x = 1.0:3.0; y = similar(x);

julia> @. y = x + 3 * sin(x)
3-element Array{Float64,1}:
 3.5244129544236893
 4.727892280477045
 3.4233600241796016
```
"""
macro __dot__(x)
    esc(__dot__(x))
end

end # module

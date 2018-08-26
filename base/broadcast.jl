# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.Broadcast

Module containing the broadcasting implementation.
"""
module Broadcast

using .Base.Cartesian
using .Base: Indices, OneTo, tail, to_shape, isoperator, promote_typejoin,
             _msk_end, unsafe_bitgetindex, bitcache_chunks, bitcache_size, dumpbitcache, unalias
import .Base: copy, copyto!
export broadcast, broadcast!, BroadcastStyle, broadcast_axes, broadcastable, dotview, @__dot__

### Objects with customized broadcasting behavior should declare a BroadcastStyle

"""
`BroadcastStyle` is an abstract type and trait-function used to determine behavior of
objects under broadcasting. `BroadcastStyle(typeof(x))` returns the style associated
with `x`. To customize the broadcasting behavior of a type, one can declare a style
by defining a type/method pair

    struct MyContainerStyle <: BroadcastStyle end
    Base.BroadcastStyle(::Type{<:MyContainer}) = MyContainerStyle()

One then writes method(s) (at least [`similar`](@ref)) operating on
`Broadcasted{MyContainerStyle}`. There are also several pre-defined subtypes of `BroadcastStyle`
that you may be able to leverage; see the
[Interfaces chapter](@ref man-interfaces-broadcasting) for more information.
"""
abstract type BroadcastStyle end

"""
`Broadcast.Style{C}()` defines a [`BroadcastStyle`](@ref) signaling through the type
parameter `C`. You can use this as an alternative to creating custom subtypes of `BroadcastStyle`,
for example

    Base.BroadcastStyle(::Type{<:MyContainer}) = Broadcast.Style{MyContainer}()
"""
struct Style{T} <: BroadcastStyle end

BroadcastStyle(::Type{<:Tuple}) = Style{Tuple}()

struct Unknown <: BroadcastStyle end
BroadcastStyle(::Type{Union{}}) = Unknown()  # ambiguity resolution

"""
`Broadcast.AbstractArrayStyle{N} <: BroadcastStyle` is the abstract supertype for any style
associated with an `AbstractArray` type.
The `N` parameter is the dimensionality, which can be handy for AbstractArray types
that only support specific dimensionalities:

    struct SparseMatrixStyle <: Broadcast.AbstractArrayStyle{2} end
    Base.BroadcastStyle(::Type{<:SparseMatrixCSC}) = SparseMatrixStyle()

For `AbstractArray` types that support arbitrary dimensionality, `N` can be set to `Any`:

    struct MyArrayStyle <: Broadcast.AbstractArrayStyle{Any} end
    Base.BroadcastStyle(::Type{<:MyArray}) = MyArrayStyle()

In cases where you want to be able to mix multiple `AbstractArrayStyle`s and keep track
of dimensionality, your style needs to support a [`Val`](@ref) constructor:

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
ArrayStyle{A}(::Val) where A = ArrayStyle{A}()

"""
`Broadcast.DefaultArrayStyle{N}()` is a [`BroadcastStyle`](@ref) indicating that an object
behaves as an `N`-dimensional array for broadcasting. Specifically, `DefaultArrayStyle` is
used for any
`AbstractArray` type that hasn't defined a specialized style, and in the absence of
overrides from other `broadcast` arguments the resulting output type is `Array`.
When there are multiple inputs to `broadcast`, `DefaultArrayStyle` "loses" to any other [`Broadcast.ArrayStyle`](@ref).
"""
struct DefaultArrayStyle{N} <: AbstractArrayStyle{N} end
DefaultArrayStyle(::Val{N}) where N = DefaultArrayStyle{N}()
DefaultArrayStyle{M}(::Val{N}) where {N,M} = DefaultArrayStyle{N}()
const DefaultVectorStyle = DefaultArrayStyle{1}
const DefaultMatrixStyle = DefaultArrayStyle{2}
BroadcastStyle(::Type{<:AbstractArray{T,N}}) where {T,N} = DefaultArrayStyle{N}()
BroadcastStyle(::Type{T}) where {T} = DefaultArrayStyle{ndims(T)}()

# `ArrayConflict` is an internal type signaling that two or more different `AbstractArrayStyle`
# objects were supplied as arguments, and that no rule was defined for resolving the
# conflict. The resulting output is `Array`. While this is the same output type
# produced by `DefaultArrayStyle`, `ArrayConflict` "poisons" the BroadcastStyle so that
# 3 or more arguments still return an `ArrayConflict`.
struct ArrayConflict <: AbstractArrayStyle{Any} end
ArrayConflict(::Val) = ArrayConflict()

### Binary BroadcastStyle rules
"""
    BroadcastStyle(::Style1, ::Style2) = Style3()

Indicate how to resolve different `BroadcastStyle`s. For example,

    BroadcastStyle(::Primary, ::Secondary) = Primary()

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

### Lazy-wrapper for broadcasting

# `Broadcasted` wrap the arguments to `broadcast(f, args...)`. A statement like
#    y = x .* (x .+ 1)
# will result in code that is essentially
#    y = copy(Broadcasted(*, x, Broadcasted(+, x, 1)))
# `broadcast!` results in `copyto!(dest, Broadcasted(...))`.

# The use of `Nothing` in place of a `BroadcastStyle` has a different
# application, in the fallback method
#    copyto!(dest, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))
# This allows methods
#    copyto!(dest::DestType,  bc::Broadcasted{Nothing})
# that specialize on `DestType` to be easily disambiguated from
# methods that instead specialize on `BroadcastStyle`,
#    copyto!(dest::AbstractArray, bc::Broadcasted{MyStyle})

struct Broadcasted{Style<:Union{Nothing,BroadcastStyle}, Axes, F, Args<:Tuple}
    f::F
    args::Args
    axes::Axes          # the axes of the resulting object (may be bigger than implied by `args` if this is nested inside a larger `Broadcasted`)
end

Broadcasted(f::F, args::Args, axes=nothing) where {F, Args<:Tuple} =
    Broadcasted{typeof(combine_styles(args...))}(f, args, axes)
function Broadcasted{Style}(f::F, args::Args, axes=nothing) where {Style, F, Args<:Tuple}
    # using Core.Typeof rather than F preserves inferrability when f is a type
    Broadcasted{Style, typeof(axes), Core.Typeof(f), Args}(f, args, axes)
end

Base.convert(::Type{Broadcasted{NewStyle}}, bc::Broadcasted{Style,Axes,F,Args}) where {NewStyle,Style,Axes,F,Args} =
    Broadcasted{NewStyle,Axes,F,Args}(bc.f, bc.args, bc.axes)

function Base.show(io::IO, bc::Broadcasted{Style}) where {Style}
    print(io, Broadcasted)
    # Only show the style parameter if we have a set of axes — representing an instantiated
    # "outermost" Broadcasted. The styles of nested Broadcasteds represent an intermediate
    # computation that is not relevant for dispatch, confusing, and just extra line noise.
    bc.axes isa Tuple && print(io, '{', Style, '}')
    print(io, '(', bc.f, ", ", bc.args, ')')
    nothing
end

## Allocating the output container
Base.similar(bc::Broadcasted{DefaultArrayStyle{N}}, ::Type{ElType}) where {N,ElType} =
    similar(Array{ElType}, axes(bc))
Base.similar(bc::Broadcasted{DefaultArrayStyle{N}}, ::Type{Bool}) where N =
    similar(BitArray, axes(bc))
# In cases of conflict we fall back on Array
Base.similar(bc::Broadcasted{ArrayConflict}, ::Type{ElType}) where ElType =
    similar(Array{ElType}, axes(bc))
Base.similar(bc::Broadcasted{ArrayConflict}, ::Type{Bool}) =
    similar(BitArray, axes(bc))

## Computing the result's axes. Most types probably won't need to specialize this.
broadcast_axes() = ()
broadcast_axes(A::Tuple) = (OneTo(length(A)),)
@inline broadcast_axes(A) = axes(A)
"""
    Base.broadcast_axes(A)

Compute the axes for `A`.

This should only be specialized for objects that do not define [`axes`](@ref) but want to participate in broadcasting.
"""
broadcast_axes

@inline Base.axes(bc::Broadcasted) = _axes(bc, bc.axes)
_axes(::Broadcasted, axes::Tuple) = axes
@inline _axes(bc::Broadcasted, ::Nothing)  = combine_axes(bc.args...)
_axes(bc::Broadcasted{Style{Tuple}}, ::Nothing) = (Base.OneTo(length(longest_tuple(nothing, bc.args))),)
_axes(bc::Broadcasted{<:AbstractArrayStyle{0}}, ::Nothing) = ()

BroadcastStyle(::Type{<:Broadcasted{Style}}) where {Style} = Style()
BroadcastStyle(::Type{<:Broadcasted{S}}) where {S<:Union{Nothing,Unknown}} =
    throw(ArgumentError("Broadcasted{Unknown} wrappers do not have a style assigned"))

argtype(::Type{Broadcasted{Style,Axes,F,Args}}) where {Style,Axes,F,Args} = Args
argtype(bc::Broadcasted) = argtype(typeof(bc))

@inline Base.eachindex(bc::Broadcasted) = _eachindex(axes(bc))
_eachindex(t::Tuple{Any}) = t[1]
_eachindex(t::Tuple) = CartesianIndices(t)

Base.ndims(::Broadcasted{<:Any,<:NTuple{N,Any}}) where {N} = N
Base.ndims(::Type{<:Broadcasted{<:Any,<:NTuple{N,Any}}}) where {N} = N

Base.length(bc::Broadcasted) = prod(map(length, axes(bc)))

function Base.iterate(bc::Broadcasted)
    iter = eachindex(bc)
    iterate(bc, (iter,))
end
Base.@propagate_inbounds function Base.iterate(bc::Broadcasted, s)
    y = iterate(s...)
    y === nothing && return nothing
    i, newstate = y
    return (bc[i], (s[1], newstate))
end

Base.IteratorSize(::Type{<:Broadcasted{<:Any,<:NTuple{N,Base.OneTo}}}) where {N} = Base.HasShape{N}()
Base.IteratorEltype(::Type{<:Broadcasted}) = Base.EltypeUnknown()

## Instantiation fills in the "missing" fields in Broadcasted.
instantiate(x) = x

"""
    Broadcast.instantiate(bc::Broadcasted)

Construct and check the axes for the lazy Broadcasted object `bc`.

Custom [`BroadcastStyle`](@ref)s may override this default in cases where it is fast and easy
to compute and verify the resulting `axes` on-demand, leaving the `axis` field
of the `Broadcasted` object empty (populated with [`nothing`](@ref)).
"""
@inline function instantiate(bc::Broadcasted{Style}) where {Style}
    if bc.axes isa Nothing # Not done via dispatch to make it easier to extend instantiate(::Broadcasted{Style})
        axes = combine_axes(bc.args...)
    else
        axes = bc.axes
        check_broadcast_axes(axes, bc.args...)
    end
    return Broadcasted{Style}(bc.f, bc.args, axes)
end
instantiate(bc::Broadcasted{<:Union{AbstractArrayStyle{0}, Style{Tuple}}}) = bc

## Flattening

"""
    bcf = flatten(bc)

Create a "flat" representation of a lazy-broadcast operation.
From
   f.(a, g.(b, c), d)
we produce the equivalent of
   h.(a, b, c, d)
where
   h(w, x, y, z) = f(w, g(x, y), z)
In terms of its internal representation,
   Broadcasted(f, a, Broadcasted(g, b, c), d)
becomes
   Broadcasted(h, a, b, c, d)

This is an optional operation that may make custom implementation of broadcasting easier in
some cases.
"""
function flatten(bc::Broadcasted{Style}) where {Style}
    isflat(bc) && return bc
    # concatenate the nested arguments into {a, b, c, d}
    args = cat_nested(bc)
    # build a function `makeargs` that takes a "flat" argument list and
    # and creates the appropriate input arguments for `f`, e.g.,
    #          makeargs = (w, x, y, z) -> (w, g(x, y), z)
    #
    # `makeargs` is built recursively and looks a bit like this:
    #     makeargs(w, x, y, z) = (w, makeargs1(x, y, z)...)
    #                          = (w, g(x, y), makeargs2(z)...)
    #                          = (w, g(x, y), z)
    let makeargs = make_makeargs(bc)
        newf = @inline function(args::Vararg{Any,N}) where N
            bc.f(makeargs(args...)...)
        end
        return Broadcasted{Style}(newf, args, bc.axes)
    end
end

const NestedTuple = Tuple{<:Broadcasted,Vararg{Any}}
isflat(bc::Broadcasted) = _isflat(bc.args)
_isflat(args::NestedTuple) = false
_isflat(args::Tuple) = _isflat(tail(args))
_isflat(args::Tuple{}) = true

cat_nested(t::Broadcasted, rest...) = (cat_nested(t.args...)..., cat_nested(rest...)...)
cat_nested(t::Any, rest...) = (t, cat_nested(rest...)...)
cat_nested() = ()

make_makeargs(bc::Broadcasted) = make_makeargs(()->(), bc.args)
@inline function make_makeargs(makeargs, t::Tuple)
    let makeargs = make_makeargs(makeargs, tail(t))
        return @inline function(head, tail::Vararg{Any,N}) where N
            (head, makeargs(tail...)...)
        end
    end
end
@inline function make_makeargs(makeargs, t::Tuple{<:Broadcasted,Vararg{Any}})
    bc = t[1]
    let makeargs = make_makeargs(makeargs, tail(t))
        let makeargs = make_makeargs(makeargs, bc.args)
            headargs, tailargs = make_headargs(bc.args), make_tailargs(bc.args)
            return @inline function(args::Vararg{Any,N}) where N
                args1 = makeargs(args...)
                a, b = headargs(args1...), tailargs(args1...)
                (bc.f(a...), b...)
            end
        end
    end
end
make_makeargs(makeargs, ::Tuple{}) = makeargs

@inline function make_headargs(t::Tuple)
    let headargs = make_headargs(tail(t))
        return @inline function(head, tail::Vararg{Any,N}) where N
            (head, headargs(tail...)...)
        end
    end
end
@inline function make_headargs(::Tuple{})
    return @inline function(tail::Vararg{Any,N}) where N
        ()
    end
end

@inline function make_tailargs(t::Tuple)
    let tailargs = make_tailargs(tail(t))
        return @inline function(head, tail::Vararg{Any,N}) where N
            tailargs(tail...)
        end
    end
end
@inline function make_tailargs(::Tuple{})
    return @inline function(tail::Vararg{Any,N}) where N
        tail
    end
end

## Broadcasting utilities ##

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
combine_styles() = DefaultArrayStyle{0}()
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
@inline combine_axes(A, B...) = broadcast_shape(broadcast_axes(A), combine_axes(B...))
combine_axes(A) = broadcast_axes(A)

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
check_broadcast_axes(shp, A) = check_broadcast_shape(shp, broadcast_axes(A))
# comparing many inputs
@inline function check_broadcast_axes(shp, A, As...)
    check_broadcast_axes(shp, A)
    check_broadcast_axes(shp, As...)
end

## Indexing manipulations
"""
    newindex(argument, I)
    newindex(I, keep, default)

Recompute index `I` such that it appropriately constrains broadcasted dimensions to the source.

Two methods are supported, both allowing for `I` to be specified as either a [`CartesianIndex`](@ref) or
an `Int`.

* `newindex(argument, I)` dynamically constrains `I` based upon the axes of `argument`.
* `newindex(I, keep, default)` constrains `I` using the pre-computed tuples `keeps` and `defaults`.
    * `keep` is a tuple of `Bool`s, where `keep[d] == true` means that dimension `d` in `I` should be preserved as is
    * `default` is a tuple of Integers, specifying what index to use in dimension `d` when `keep[d] == false`.
    Any remaining indices in `I` beyond the length of the `keep` tuple are truncated. The `keep` and `default`
    tuples may be created by `newindexer(argument)`.
"""
Base.@propagate_inbounds newindex(arg, I::CartesianIndex) = CartesianIndex(_newindex(broadcast_axes(arg), I.I))
Base.@propagate_inbounds newindex(arg, I::Integer) = CartesianIndex(_newindex(broadcast_axes(arg), (I,)))
Base.@propagate_inbounds _newindex(ax::Tuple, I::Tuple) = (ifelse(Base.unsafe_length(ax[1])==1, ax[1][1], I[1]), _newindex(tail(ax), tail(I))...)
Base.@propagate_inbounds _newindex(ax::Tuple{}, I::Tuple) = ()
Base.@propagate_inbounds _newindex(ax::Tuple, I::Tuple{}) = (ax[1][1], _newindex(tail(ax), ())...)
Base.@propagate_inbounds _newindex(ax::Tuple{}, I::Tuple{}) = ()

# If dot-broadcasting were already defined, this would be `ifelse.(keep, I, Idefault)`.
@inline newindex(I::CartesianIndex, keep, Idefault) = CartesianIndex(_newindex(I.I, keep, Idefault))
@inline newindex(i::Integer, keep::Tuple{Bool}, idefault) = ifelse(keep[1], i, idefault[1])
@inline newindex(i::Integer, keep::Tuple{}, idefault) = CartesianIndex(())
@inline _newindex(I, keep, Idefault) =
    (ifelse(keep[1], I[1], Idefault[1]), _newindex(tail(I), tail(keep), tail(Idefault))...)
@inline _newindex(I, keep::Tuple{}, Idefault) = ()  # truncate if keep is shorter than I

# newindexer(A) generates `keep` and `Idefault` (for use by `newindex` above)
# for a particular array `A`; `shapeindexer` does so for its axes.
@inline newindexer(A) = shapeindexer(broadcast_axes(A))
@inline shapeindexer(ax) = _newindexer(ax)
@inline _newindexer(indsA::Tuple{}) = (), ()
@inline function _newindexer(indsA::Tuple)
    ind1 = indsA[1]
    keep, Idefault = _newindexer(tail(indsA))
    (Base.length(ind1)!=1, keep...), (first(ind1), Idefault...)
end

@inline function Base.getindex(bc::Broadcasted, I::Union{Integer,CartesianIndex})
    @boundscheck checkbounds(bc, I)
    @inbounds _broadcast_getindex(bc, I)
end
Base.@propagate_inbounds Base.getindex(bc::Broadcasted, i1::Integer, i2::Integer, I::Integer...) = bc[CartesianIndex((i1, i2, I...))]
Base.@propagate_inbounds Base.getindex(bc::Broadcasted) = bc[CartesianIndex(())]

@inline Base.checkbounds(bc::Broadcasted, I::Union{Integer,CartesianIndex}) =
    Base.checkbounds_indices(Bool, axes(bc), (I,)) || Base.throw_boundserror(bc, (I,))


"""
    _broadcast_getindex(A, I)

Index into `A` with `I`, collapsing broadcasted indices to their singleton indices as appropriate.
"""
Base.@propagate_inbounds _broadcast_getindex(A::Union{Ref,AbstractArray{<:Any,0},Number}, I) = A[] # Scalar-likes can just ignore all indices
Base.@propagate_inbounds _broadcast_getindex(::Ref{Type{T}}, I) where {T} = T
# Tuples are statically known to be singleton or vector-like
Base.@propagate_inbounds _broadcast_getindex(A::Tuple{Any}, I) = A[1]
Base.@propagate_inbounds _broadcast_getindex(A::Tuple, I) = A[I[1]]
# Everything else falls back to dynamically dropping broadcasted indices based upon its axes
Base.@propagate_inbounds _broadcast_getindex(A, I) = A[newindex(A, I)]

# In some cases, it's more efficient to sort out which dimensions should be dropped
# ahead of time (often when the size checks aren't able to be lifted out of the loop).
# The Extruded struct computes that information ahead of time and stores it as a pair
# of tuples to optimize indexing later. This is most commonly needed for `Array` and
# other `AbstractArray` subtypes that wrap `Array` and dynamically ask it for its size.
struct Extruded{T, K, D}
    x::T
    keeps::K    # A tuple of booleans, specifying which indices should be passed normally
    defaults::D # A tuple of integers, specifying the index to use when keeps[i] is false (as defaults[i])
end
@inline broadcast_axes(b::Extruded) = broadcast_axes(b.x)
Base.@propagate_inbounds _broadcast_getindex(b::Extruded, i) = b.x[newindex(i, b.keeps, b.defaults)]
extrude(x::AbstractArray) = Extruded(x, newindexer(x)...)
extrude(x) = x

# For Broadcasted
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted{<:Any,<:Any,<:Any,<:Any}, I)
    args = _getindex(bc.args, I)
    return _broadcast_getindex_evalf(bc.f, args...)
end
# Hack around losing Type{T} information in the final args tuple. Julia actually
# knows (in `code_typed`) the _value_ of these types, statically displaying them,
# but inference is currently skipping inferring the type of the types as they are
# transiently placed in a tuple as the argument list is lispily constructed. These
# additional methods recover type stability when a `Type` appears in one of the
# first two arguments of a function.
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted{<:Any,<:Any,<:Any,<:Tuple{Ref{Type{T}},Vararg{Any}}}, I) where {T}
    args = _getindex(tail(bc.args), I)
    return _broadcast_getindex_evalf(bc.f, T, args...)
end
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted{<:Any,<:Any,<:Any,<:Tuple{Any,Ref{Type{T}},Vararg{Any}}}, I) where {T}
    arg1 = _broadcast_getindex(bc.args[1], I)
    args = _getindex(tail(tail(bc.args)), I)
    return _broadcast_getindex_evalf(bc.f, arg1, T, args...)
end
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted{<:Any,<:Any,<:Any,<:Tuple{Ref{Type{T}},Ref{Type{S}},Vararg{Any}}}, I) where {T,S}
    args = _getindex(tail(tail(bc.args)), I)
    return _broadcast_getindex_evalf(bc.f, T, S, args...)
end

# Utilities for _broadcast_getindex
Base.@propagate_inbounds _getindex(args::Tuple, I) = (_broadcast_getindex(args[1], I), _getindex(tail(args), I)...)
Base.@propagate_inbounds _getindex(args::Tuple{Any}, I) = (_broadcast_getindex(args[1], I),)
Base.@propagate_inbounds _getindex(args::Tuple{}, I) = ()

@inline _broadcast_getindex_evalf(f::Tf, args::Vararg{Any,N}) where {Tf,N} = f(args...)  # not propagate_inbounds

"""
    Broadcast.broadcastable(x)

Return either `x` or an object like `x` such that it supports [`axes`](@ref), indexing, and its type supports [`ndims`](@ref).

If `x` supports iteration, the returned value should have the same `axes` and indexing
behaviors as [`collect(x)`](@ref).

If `x` is not an `AbstractArray` but it supports `axes`, indexing, and its type supports
`ndims`, then `broadcastable(::typeof(x))` may be implemented to just return itself.
Further, if `x` defines its own [`BroadcastStyle`](@ref), then it must define its
`broadcastable` method to return itself for the custom style to have any effect.

# Examples
```jldoctest
julia> Broadcast.broadcastable([1,2,3]) # like `identity` since arrays already support axes and indexing
3-element Array{Int64,1}:
 1
 2
 3

julia> Broadcast.broadcastable(Int) # Types don't support axes, indexing, or iteration but are commonly used as scalars
Base.RefValue{Type{Int64}}(Int64)

julia> Broadcast.broadcastable("hello") # Strings break convention of matching iteration and act like a scalar instead
Base.RefValue{String}("hello")
```
"""
broadcastable(x::Union{Symbol,AbstractString,Function,UndefInitializer,Nothing,RoundingMode,Missing,Val}) = Ref(x)
broadcastable(x::Ptr) = Ref(x)
broadcastable(::Type{T}) where {T} = Ref{Type{T}}(T)
broadcastable(x::Union{AbstractArray,Number,Ref,Tuple,Broadcasted}) = x
# Default to collecting iterables — which will error for non-iterables
broadcastable(x) = collect(x)
broadcastable(::Union{AbstractDict, NamedTuple}) = throw(ArgumentError("broadcasting over dictionaries and `NamedTuple`s is reserved"))

## Computation of inferred result type, for empty and concretely inferred cases only
_broadcast_getindex_eltype(bc::Broadcasted) = Base._return_type(bc.f, eltypes(bc.args))
_broadcast_getindex_eltype(A) = eltype(A)  # Tuple, Array, etc.

eltypes(::Tuple{}) = Tuple{}
eltypes(t::Tuple{Any}) = Tuple{_broadcast_getindex_eltype(t[1])}
eltypes(t::Tuple{Any,Any}) = Tuple{_broadcast_getindex_eltype(t[1]), _broadcast_getindex_eltype(t[2])}
eltypes(t::Tuple) = Tuple{_broadcast_getindex_eltype(t[1]), eltypes(tail(t)).types...}

# Inferred eltype of result of broadcast(f, args...)
combine_eltypes(f, args::Tuple) = Base._return_type(f, eltypes(args))

## Broadcasting core

"""
    broadcast(f, As...)

Broadcast the function `f` over the arrays, tuples, collections, [`Ref`](@ref)s and/or scalars `As`.

Broadcasting applies the function `f` over the elements of the container arguments and the
scalars themselves in `As`. Singleton and missing dimensions are expanded to match the
extents of the other arguments by virtually repeating the value. By default, only a limited
number of types are considered scalars, including `Number`s, `String`s, `Symbol`s, `Type`s,
`Function`s and some common singletons like [`missing`](@ref) and [`nothing`](@ref). All other arguments are
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
broadcast(f::Tf, As...) where {Tf} = materialize(broadcasted(f, As...))

# special cases defined for performance
@inline broadcast(f, x::Number...) = f(x...)
@inline broadcast(f, t::NTuple{N,Any}, ts::Vararg{NTuple{N,Any}}) where {N} = map(f, t, ts...)

"""
    broadcast!(f, dest, As...)

Like [`broadcast`](@ref), but store the result of
`broadcast(f, As...)` in the `dest` array.
Note that `dest` is only used to store the result, and does not supply
arguments to `f` unless it is also listed in the `As`,
as in `broadcast!(f, A, A, B)` to perform `A[:] = broadcast(f, A, B)`.

# Examples
```jldoctest
julia> A = [1.0; 0.0]; B = [0.0; 0.0];

julia> broadcast!(+, B, A, (0, -2.0));

julia> B
2-element Array{Float64,1}:
  1.0
 -2.0

julia> A
2-element Array{Float64,1}:
 1.0
 0.0

julia> broadcast!(+, A, A, (0, -2.0));

julia> A
2-element Array{Float64,1}:
  1.0
 -2.0
```
"""
broadcast!(f::Tf, dest, As::Vararg{Any,N}) where {Tf,N} = (materialize!(dest, broadcasted(f, As...)); dest)

"""
    Broadcast.materialize(bc)

Take a lazy `Broadcasted` object and compute the result
"""
@inline materialize(bc::Broadcasted) = copy(instantiate(bc))
materialize(x) = x
@inline function materialize!(dest, bc::Broadcasted{Style}) where {Style}
    return copyto!(dest, instantiate(Broadcasted{Style}(bc.f, bc.args, axes(dest))))
end
@inline function materialize!(dest, x)
    return copyto!(dest, instantiate(Broadcasted(identity, (x,), axes(dest))))
end

## general `copy` methods
@inline copy(bc::Broadcasted{<:AbstractArrayStyle{0}}) = bc[CartesianIndex()]
copy(bc::Broadcasted{<:Union{Nothing,Unknown}}) =
    throw(ArgumentError("broadcasting requires an assigned BroadcastStyle"))

const NonleafHandlingStyles = Union{DefaultArrayStyle,ArrayConflict}

@inline function copy(bc::Broadcasted{Style}) where {Style}
    ElType = combine_eltypes(bc.f, bc.args)
    if Base.isconcretetype(ElType)
        # We can trust it and defer to the simpler `copyto!`
        return copyto!(similar(bc, ElType), bc)
    end
    # When ElType is not concrete, use narrowing. Use the first output
    # value to determine the starting output eltype; copyto_nonleaf!
    # will widen `dest` as needed to accommodate later values.
    bc′ = preprocess(nothing, bc)
    iter = eachindex(bc′)
    y = iterate(iter)
    if y === nothing
        # if empty, take the ElType at face value
        return similar(bc′, ElType)
    end
    # Initialize using the first value
    I, state = y
    @inbounds val = bc′[I]
    dest = similar(bc′, typeof(val))
    @inbounds dest[I] = val
    # Now handle the remaining values
    return copyto_nonleaf!(dest, bc′, iter, state, 1)
end

## general `copyto!` methods
# The most general method falls back to a method that replaces Style->Nothing
# This permits specialization on typeof(dest) without introducing ambiguities
@inline copyto!(dest::AbstractArray, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))

# Performance optimization for the common identity scalar case: dest .= val
@inline function copyto!(dest::AbstractArray, bc::Broadcasted{<:AbstractArrayStyle{0}})
    # Typically, we must independently execute bc for every storage location in `dest`, but:
    # IF we're in the common no-op identity case with no nested args (like `dest .= val`),
    if bc.f === identity && bc.args isa Tuple{Any} && isflat(bc)
        # THEN we can just extract the argument and `fill!` the destination with it
        return fill!(dest, bc.args[1][])
    else
        # Otherwise, fall back to the default implementation like above
        return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    end
end

# For broadcasted assignments like `broadcast!(f, A, ..., A, ...)`, where `A`
# appears on both the LHS and the RHS of the `.=`, then we know we're only
# going to make one pass through the array, and even though `A` is aliasing
# against itself, the mutations won't affect the result as the indices on the
# LHS and RHS will always match. This is not true in general, but with the `.op=`
# syntax it's fairly common for an argument to be `===` a source.
broadcast_unalias(dest, src) = dest === src ? src : unalias(dest, src)
broadcast_unalias(::Nothing, src) = src

# Preprocessing a `Broadcasted` does two things:
# * unaliases any arguments from `dest`
# * "extrudes" the arguments where it is advantageous to pre-compute the broadcasted indices
@inline preprocess(dest, bc::Broadcasted{Style}) where {Style} = Broadcasted{Style}(bc.f, preprocess_args(dest, bc.args), bc.axes)
preprocess(dest, x) = extrude(broadcast_unalias(dest, x))

@inline preprocess_args(dest, args::Tuple) = (preprocess(dest, args[1]), preprocess_args(dest, tail(args))...)
preprocess_args(dest, args::Tuple{Any}) = (preprocess(dest, args[1]),)
preprocess_args(dest, args::Tuple{}) = ()

# Specialize this method if all you want to do is specialize on typeof(dest)
@inline function copyto!(dest::AbstractArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    # Performance optimization: broadcast!(identity, dest, A) is equivalent to copyto!(dest, A) if indices match
    if bc.f === identity && bc.args isa Tuple{AbstractArray} # only a single input argument to broadcast!
        A = bc.args[1]
        if axes(dest) == axes(A)
            return copyto!(dest, A)
        end
    end
    bc′ = preprocess(dest, bc)
    @simd for I in eachindex(bc′)
        @inbounds dest[I] = bc′[I]
    end
    return dest
end

# Performance optimization: for BitArray outputs, we cache the result
# in a "small" Vector{Bool}, and then copy in chunks into the output
@inline function copyto!(dest::BitArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    ischunkedbroadcast(dest, bc) && return chunkedcopyto!(dest, bc)
    tmp = Vector{Bool}(undef, bitcache_size)
    destc = dest.chunks
    ind = cind = 1
    bc′ = preprocess(dest, bc)
    @simd for I in eachindex(bc′)
        @inbounds tmp[ind] = bc′[I]
        ind += 1
        if ind > bitcache_size
            dumpbitcache(destc, cind, tmp)
            cind += bitcache_chunks
            ind = 1
        end
    end
    if ind > 1
        @inbounds tmp[ind:bitcache_size] .= false
        dumpbitcache(destc, cind, tmp)
    end
    return dest
end

# For some BitArray operations, we can work at the level of chunks. The trivial
# implementation just walks over the UInt64 chunks in a linear fashion.
# This requires three things:
#   1. The function must be known to work at the level of chunks (or can be converted to do so)
#   2. The only arrays involved must be BitArrays or scalar Bools
#   3. There must not be any broadcasting beyond scalar — all array sizes must match
# We could eventually allow for all broadcasting and other array types, but that
# requires very careful consideration of all the edge effects.
const ChunkableOp = Union{typeof(&), typeof(|), typeof(xor), typeof(~), typeof(identity),
    typeof(!), typeof(*), typeof(==)} # these are convertible to chunkable ops by liftfuncs
const BroadcastedChunkableOp{Style<:Union{Nothing,BroadcastStyle}, Axes, F<:ChunkableOp, Args<:Tuple} = Broadcasted{Style,Axes,F,Args}
ischunkedbroadcast(R, bc::BroadcastedChunkableOp) = ischunkedbroadcast(R, bc.args)
ischunkedbroadcast(R, args) = false
ischunkedbroadcast(R, args::Tuple{<:BitArray,Vararg{Any}}) = size(R) == size(args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:Bool,Vararg{Any}}) = ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:BroadcastedChunkableOp,Vararg{Any}}) = ischunkedbroadcast(R, args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{}) = true

# Convert compatible functions to chunkable ones. They must also be green-lighted as ChunkableOps
liftfuncs(bc::Broadcasted{Style}) where {Style} = Broadcasted{Style}(bc.f, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(sign)}) where {Style} = Broadcasted{Style}(identity, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(!)}) where {Style} = Broadcasted{Style}(~, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(*)}) where {Style} = Broadcasted{Style}(&, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(==)}) where {Style} = Broadcasted{Style}((~)∘(xor), map(liftfuncs, bc.args), bc.axes)
liftfuncs(x) = x

liftchunks(::Tuple{}) = ()
liftchunks(args::Tuple{<:BitArray,Vararg{Any}}) = (args[1].chunks, liftchunks(tail(args))...)
# Transform scalars to repeated scalars the size of a chunk
liftchunks(args::Tuple{<:Bool,Vararg{Any}}) = (ifelse(args[1], typemax(UInt64), UInt64(0)), liftchunks(tail(args))...)
ithchunk(i) = ()
Base.@propagate_inbounds ithchunk(i, c::Vector{UInt64}, args...) = (c[i], ithchunk(i, args...)...)
Base.@propagate_inbounds ithchunk(i, b::UInt64, args...) = (b, ithchunk(i, args...)...)
@inline function chunkedcopyto!(dest::BitArray, bc::Broadcasted)
    isempty(dest) && return dest
    f = flatten(liftfuncs(bc))
    args = liftchunks(f.args)
    dc = dest.chunks
    @simd for i in eachindex(dc)
        @inbounds dc[i] = f.f(ithchunk(i, args...)...)
    end
    @inbounds dc[end] &= Base._msk_end(dest)
    return dest
end


@noinline throwdm(axdest, axsrc) =
    throw(DimensionMismatch("destination axes $axdest are not compatible with source axes $axsrc"))

function copyto_nonleaf!(dest, bc::Broadcasted, iter, state, count)
    T = eltype(dest)
    while true
        y = iterate(iter, state)
        y === nothing && break
        I, state = y
        @inbounds val = bc[I]
        S = typeof(val)
        if S <: T
            @inbounds dest[I] = val
        else
            # This element type doesn't fit in dest. Allocate a new dest with wider eltype,
            # copy over old values, and continue
            newdest = Base.similar(dest, promote_typejoin(T, S))
            for II in Iterators.take(iter, count)
                newdest[II] = dest[II]
            end
            newdest[I] = val
            return copyto_nonleaf!(newdest, bc, iter, state, count+1)
        end
        count += 1
    end
    return dest
end

## Tuple methods

@inline copy(bc::Broadcasted{Style{Tuple}}) =
    tuplebroadcast(longest_tuple(nothing, bc.args), bc)
@inline tuplebroadcast(::NTuple{N,Any}, bc) where {N} = ntuple(k -> @inbounds(_broadcast_getindex(bc, k)), Val(N))
# This is a little tricky: find the longest tuple (first arg) within the list of arguments (second arg)
# Start with nothing as a placeholder and go until we find the first tuple in the argument list
longest_tuple(::Nothing, t::Tuple{Tuple,Vararg{Any}}) = longest_tuple(t[1], tail(t))
# Or recurse through nested broadcast expressions
longest_tuple(::Nothing, t::Tuple{Broadcasted,Vararg{Any}}) = longest_tuple(longest_tuple(nothing, t[1].args), tail(t))
longest_tuple(::Nothing, t::Tuple) = longest_tuple(nothing, tail(t))
# And then compare it against all other tuples we find in the argument list or nested broadcasts
longest_tuple(l::Tuple, t::Tuple{Tuple,Vararg{Any}}) = longest_tuple(_longest_tuple(l, t[1]), tail(t))
longest_tuple(l::Tuple, t::Tuple) = longest_tuple(l, tail(t))
longest_tuple(l::Tuple, ::Tuple{}) = l
longest_tuple(l::Tuple, t::Tuple{Broadcasted}) = longest_tuple(l, t[1].args)
longest_tuple(l::Tuple, t::Tuple{Broadcasted,Vararg{Any}}) = longest_tuple(longest_tuple(l, t[1].args), tail(t))
# Support only 1-tuples and N-tuples where there are no conflicts in N
_longest_tuple(A::Tuple{Any}, B::Tuple{Any}) = A
_longest_tuple(A::Tuple{Any}, B::NTuple{N,Any}) where N = B
_longest_tuple(A::NTuple{N,Any}, B::Tuple{Any}) where N = A
_longest_tuple(A::NTuple{N,Any}, B::NTuple{N,Any}) where N = A
@noinline _longest_tuple(A, B) =
    throw(DimensionMismatch("tuples $A and $B could not be broadcast to a common size"))

## scalar-range broadcast operations ##
# DefaultArrayStyle and \ are not available at the time of range.jl
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::OrdinalRange) = r
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::StepRangeLen) = r
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::LinRange) = r

broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::OrdinalRange) = range(-first(r), step=-step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen) = StepRangeLen(-r.ref, -r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange) = LinRange(-r.start, -r.stop, length(r))

broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Real, r::AbstractUnitRange) = range(x + first(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractUnitRange, x::Real) = range(first(r) + x, length=length(r))
# For #18336 we need to prevent promotion of the step type:
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractRange, x::Number) = range(first(r) + x, step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::AbstractRange) = range(x + first(r), step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)+x)}(r.ref + x, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x+T(r.ref))}(x + r.ref, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::LinRange, x::Number) = LinRange(r.start + x, r.stop + x, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::LinRange) = LinRange(x + r.start, x + r.stop, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r1::AbstractRange, r2::AbstractRange) = r1 + r2

broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractUnitRange, x::Number) = range(first(r)-x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractRange, x::Number) = range(first(r)-x, step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::AbstractRange) = range(x-first(r), step=-step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)-x)}(r.ref - x, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x-T(r.ref))}(x - r.ref, -r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange, x::Number) = LinRange(r.start - x, r.stop - x, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::LinRange) = LinRange(x - r.start, x - r.stop, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r1::AbstractRange, r2::AbstractRange) = r1 - r2

broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::AbstractRange) = range(x*first(r), step=x*step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::StepRangeLen{T}) where {T} =
    StepRangeLen{typeof(x*T(r.ref))}(x*r.ref, x*r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::LinRange) = LinRange(x * r.start, x * r.stop, r.len)
# separate in case of noncommutative multiplication
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::AbstractRange, x::Number) = range(first(r)*x, step=step(r)*x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)*x)}(r.ref*x, r.step*x, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::LinRange, x::Number) = LinRange(r.start * x, r.stop * x, r.len)

broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::AbstractRange, x::Number) = range(first(r)/x, step=step(r)/x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)/x)}(r.ref/x, r.step/x, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::LinRange, x::Number) = LinRange(r.start / x, r.stop / x, r.len)

broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::AbstractRange) = range(x\first(r), step=x\step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::StepRangeLen) = StepRangeLen(x\r.ref, x\r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::LinRange) = LinRange(x \ r.start, x \ r.stop, r.len)

broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::UnitRange) = big(r.start):big(last(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::StepRange) = big(r.start):big(r.step):big(last(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::StepRangeLen) = StepRangeLen(big(r.ref), big(r.step), length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::LinRange) = LinRange(big(r.start), big(r.stop), length(r))

## In specific instances, we can broadcast masked BitArrays whole chunks at a time
# Very intentionally do not support much functionality here: scalar indexing would be O(n)
struct BitMaskedBitArray{N,M}
    parent::BitArray{N}
    mask::BitArray{M}
    BitMaskedBitArray{N,M}(parent, mask) where {N,M} = new(parent, mask)
end
@inline function BitMaskedBitArray(parent::BitArray{N}, mask::BitArray{M}) where {N,M}
    @boundscheck checkbounds(parent, mask)
    BitMaskedBitArray{N,M}(parent, mask)
end
Base.@propagate_inbounds dotview(B::BitArray, i::BitArray) = BitMaskedBitArray(B, i)
Base.show(io::IO, B::BitMaskedBitArray) = foreach(arg->show(io, arg), (typeof(B), (B.parent, B.mask)))
# Override materialize! to prevent the BitMaskedBitArray from escaping to an overrideable method
@inline materialize!(B::BitMaskedBitArray, bc::Broadcasted{<:Any,<:Any,typeof(identity),Tuple{Bool}}) = fill!(B, bc.args[1])
@inline materialize!(B::BitMaskedBitArray, bc::Broadcasted{<:Any}) = materialize!(SubArray(B.parent, to_indices(B.parent, (B.mask,))), bc)
function Base.fill!(B::BitMaskedBitArray, b::Bool)
    Bc = B.parent.chunks
    Ic = B.mask.chunks
    @inbounds if b
        for i = 1:length(Bc)
            Bc[i] |= Ic[i]
        end
    else
        for i = 1:length(Bc)
            Bc[i] &= ~Ic[i]
        end
    end
    return B
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
    elseif x.head == :comparison
        Expr(:comparison, (iseven(i) && dottable(arg) && arg isa Symbol && isoperator(arg) ?
                               Symbol('.', arg) : arg for (i, arg) in pairs(dotargs))...)
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
        if x.head == :&& || x.head == :||
            error("""
                Using `&&` and `||` is disallowed in `@.` expressions.
                Use `&` or `|` for elementwise logical operations.
                """)
        end
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

@inline broadcasted_kwsyntax(f, args...; kwargs...) = broadcasted((args...)->f(args...; kwargs...), args...)
@inline function broadcasted(f, args...)
    args′ = map(broadcastable, args)
    broadcasted(combine_styles(args′...), f, args′...)
end
# Due to the current Type{T}/DataType specialization heuristics within Tuples,
# the totally generic varargs broadcasted(f, args...) method above loses Type{T}s in
# mapping broadcastable across the args. These additional methods with explicit
# arguments ensure we preserve Type{T}s in the first or second argument position.
@inline function broadcasted(f, arg1, args...)
    arg1′ = broadcastable(arg1)
    args′ = map(broadcastable, args)
    broadcasted(combine_styles(arg1′, args′...), f, arg1′, args′...)
end
@inline function broadcasted(f, arg1, arg2, args...)
    arg1′ = broadcastable(arg1)
    arg2′ = broadcastable(arg2)
    args′ = map(broadcastable, args)
    broadcasted(combine_styles(arg1′, arg2′, args′...), f, arg1′, arg2′, args′...)
end
@inline broadcasted(::S, f, args...) where S<:BroadcastStyle = Broadcasted{S}(f, args)

end # module

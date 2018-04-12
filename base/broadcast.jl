# This file is a part of Julia. License is MIT: https://julialang.org/license

module Broadcast

using .Base.Cartesian
using .Base: Indices, OneTo, linearindices, tail, to_shape, isoperator, promote_typejoin,
             _msk_end, unsafe_bitgetindex, bitcache_chunks, bitcache_size, dumpbitcache, unalias
import .Base: broadcast, broadcast!, copy, copyto!
export BroadcastStyle, broadcast_indices, broadcast_similar, broadcastable, broadcast_skip_axes_instantiation,
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
BroadcastStyle(::Type{<:Ref}) = DefaultArrayStyle{0}()
BroadcastStyle(::Type{T}) where {T} = DefaultArrayStyle{ndims(T)}()

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

### Lazy-wrapper for broadcasting

# `Broadcasted` wrap the arguments to `broadcast(f, args...)`. A statement like
#    y = x .* (x .+ 1)
# will result in code that is essentially
#    y = copy(Broadcasted(*, x, Broadcasted(+, x, 1)))
# `broadcast!` results in `copyto!(dest, Broadcasted(...))`.

# Besides the function `f` and the input `args`, `Broadcasted`
# includes two other fields (`axes` and `indexing`) that, once
# initialized, improve performance when extracting values.  However,
# in some cases (e.g., StaticArrays.jl) these are not used, and for
# performance it's important to be able to bypass their
# initialization. We use `Nothing` type parameters when these have not
# been intialized.

# The use of `Nothing` in place of a `BroadcastStyle` has a different
# application, in the fallback method
#    copyto!(dest, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))
# This allows methods
#    copyto!(dest::DestType,  bc::Broadcasted{Nothing})
# that specialize on `DestType` to be easily disambiguated from
# methods that instead specialize on `BroadcastStyle`,
#    copyto!(dest::AbstractArray, bc::Broadcasted{MyStyle})

struct Broadcasted{Style<:Union{Nothing,BroadcastStyle}, ElType, Axes, Indexing<:Union{Nothing,Tuple}, F, Args<:Tuple}
    f::F
    args::Args
    axes::Axes          # the axes of the resulting object (may be bigger than implied by `args` if this is nested inside a larger `Broadcasted`)
    indexing::Indexing  # index-replacement info computed from `newindexer` below
end

function Broadcasted(f::F, args::Args) where {F, Args<:Tuple}
    style = combine_styles(args...)
    # Unknown is a flag indicating the ElType has not been set
    # using Core.Typeof rather than F preserves inferrability when f is a type
    return Broadcasted{typeof(style), Unknown, Nothing, Nothing, Core.Typeof(f), Args}(f, args, nothing, nothing)
end
Broadcasted{Style}(f::F, args::Args) where {Style<:BroadcastStyle, F, Args<:Tuple} =
    Broadcasted{Style, Unknown, Nothing, Nothing, Core.Typeof(f), Args}(f, args, nothing, nothing)
Broadcasted{Style,ElType}(f::F, args::Args) where {Style<:BroadcastStyle, ElType, F, Args<:Tuple} =
    Broadcasted{Style, ElType, Nothing, Nothing, Core.Typeof(f), Args}(f, args, nothing, nothing)
Broadcasted{Style,ElType}(f::F, args::Args, axes::Tuple) where {Style<:BroadcastStyle, ElType, F, Args<:Tuple} =
    Broadcasted{Style, ElType, typeof(axes), Nothing, Core.Typeof(f), Args}(f, args, axes, nothing)
Broadcasted{Style,ElType}(f::F, args::Args, axes::Tuple, indexing) where {Style<:Union{Nothing,BroadcastStyle}, ElType, F, Args<:Tuple} =
    Broadcasted{Style, ElType, typeof(axes), typeof(indexing), Core.Typeof(f), Args}(f, args, axes, indexing)

Base.convert(::Type{Broadcasted{Nothing}}, bc::Broadcasted{Style,ElType,Axes,Indexing,F,Args}) where {Style,ElType,Axes,Indexing,F,Args} =
    Broadcasted{Nothing,ElType,Axes,Indexing,F,Args}(bc.f, bc.args, bc.axes, bc.indexing)

# Fully-instantiatiated Broadcasted
const BroadcastedF{Style<:Union{Nothing,BroadcastStyle}, ElType, N, F, Args<:Tuple} =
    Broadcasted{Style, ElType, <:Indices{N}, <:Tuple, F, Args}

## Allocating the output container
"""
    broadcast_similar(::BroadcastStyle, ::Type{ElType}, inds, As...)

Allocate an output object for [`broadcast`](@ref), appropriate for the indicated
[`Broadcast.BroadcastStyle`](@ref). `ElType` and `inds` specify the desired element type and indices of the
container. `As...` are the input arguments supplied to `broadcast`.
"""
broadcast_similar(::DefaultArrayStyle{N}, ::Type{ElType}, inds::Indices{N}, bc) where {N,ElType} =
    similar(Array{ElType}, inds)
broadcast_similar(::DefaultArrayStyle{N}, ::Type{Bool}, inds::Indices{N}, bc) where N =
    similar(BitArray, inds)
# In cases of conflict we fall back on Array
broadcast_similar(::ArrayConflict, ::Type{ElType}, inds::Indices, bc) where ElType =
    similar(Array{ElType}, inds)
broadcast_similar(::ArrayConflict, ::Type{Bool}, inds::Indices, bc) =
    similar(BitArray, inds)

## Computing the result's indices. Most types probably won't need to specialize this.
broadcast_indices() = ()
broadcast_indices(::Type{T}) where T = ()
broadcast_indices(A) = broadcast_indices(combine_styles(A), A)
broadcast_indices(::Style{Tuple}, A) = (OneTo(length(A)),)
broadcast_indices(::AbstractArrayStyle{0}, A) = ()
broadcast_indices(::BroadcastStyle, A) = Base.axes(A)
"""
    Base.broadcast_indices(::SrcStyle, A)

Compute the indices for objects `A` with [`BroadcastStyle`](@ref) `SrcStyle`.
If needed, you can specialize this method for your styles.
You should only need to provide a custom implementation for non-AbstractArrayStyles.
"""
broadcast_indices

"""
    Base.broadcast_skip_axes_instantiation(::Broadcasted{MyStyle})::Bool

Define this method to return `true` if `MyStyle` does not require computation of
the axes of the broadcasted object. The only motivation for setting this to `true` is performance.
"""
broadcast_skip_axes_instantiation(bc::Broadcasted)               = false
broadcast_skip_axes_instantiation(bc::Broadcasted{<:AbstractArrayStyle{0}}) = true
broadcast_skip_axes_instantiation(bc::Broadcasted{Unknown})      = true
broadcast_skip_axes_instantiation(bc::Broadcasted{Style{Tuple}}) = true

### End of methods that users will typically have to specialize ###

# Broadcasted traits
Base.eltype(::Type{<:Broadcasted{Style,ElType}}) where {Style,ElType} = ElType
Base.eltype(::Type{<:Broadcasted{Style,Unknown}}) where {Style} =
    error("non-instantiated Broadcasted wrappers do not have eltype assigned")
Base.eltype(bc::Broadcasted) = eltype(typeof(bc))

Base.axes(bc::Broadcasted{Style,ElType}) where {Style,ElType} = bc.axes
Base.axes(::Broadcasted{Style,ElType,Nothing}) where {Style,ElType} =
    error("non-instantiated Broadcasted wrappers do not have axes assigned")

Broadcast.BroadcastStyle(::Type{<:Broadcasted{Style}}) where Style = Style()
Broadcast.BroadcastStyle(::Type{<:Broadcasted{Unknown}}) =
    error("non-instantiated Broadcasted wrappers do not have a style assigned")
Broadcast.BroadcastStyle(::Type{<:Broadcasted{Nothing}}) =
    error("non-instantiated Broadcasted wrappers do not have a style assigned")

argtype(::Type{Broadcasted{Style,ElType,Axes,Indexing,F,Args}}) where {Style,ElType,Axes,Indexing,F,Args} = Args
argtype(bc::Broadcasted) = argtype(typeof(bc))

const NestedTuple = Tuple{<:Broadcasted,Vararg{Any}}
not_nested(bc::Broadcasted)          = not_nested(bc.args)
not_nested(t::Tuple)      = not_nested(tail(t))
not_nested(::NestedTuple) = false
not_nested(::Tuple{})     = true

## Instantiation fills in the "missing" fields in Broadcasted.
instantiate(x) = x
instantiate(x, axes) = x

# Setting ElType
@inline instantiate(bc::Broadcasted{Style,Unknown,Nothing,Nothing}) where {Style} =
    instantiate(instantiate_eltype(bc))
@inline instantiate(bc::Broadcasted{Style,Unknown,Nothing,Nothing}, axes) where {Style} =
    instantiate(instantiate_eltype(bc), axes)
@inline function instantiate_eltype(bc::Broadcasted{Style,Unknown,Nothing,Nothing}) where {Style}
    args = map(instantiate, bc.args) # some of the args may be Broadcasted objects in their own right
    T = combine_eltypes(bc.f, args)
    return Broadcasted{Style,T}(bc.f, args)
end

# Setting axes
@inline function instantiate(bc::Broadcasted{Style,ElType,Nothing,Nothing}) where {Style,ElType}
    if broadcast_skip_axes_instantiation(bc)
        return Style <: Nothing ? instantiate_eltype(bc) : bc
    end
    return instantiate(instantiate_axes(bc))
end
@inline instantiate(bc::Broadcasted{Style,ElType,Nothing,Nothing}, axes) where {Style,ElType} =
    instantiate(instantiate_axes(bc, axes))
@inline function instantiate_axes(bc::Broadcasted{Style,ElType,Nothing,Nothing}) where {Style,ElType}
    axes = combine_indices(bc.args...)
    return instantiate_axes(bc, axes)
end
@inline function instantiate_axes(bc::Broadcasted{Style,ElType,Nothing,Nothing}, axes) where {Style,ElType}
    args = map(x->instantiate(x, axes), bc.args)
    return Broadcasted{Style,ElType}(bc.f, args, axes)
end

# Setting indexing
@inline function instantiate(bc::Broadcasted{Style,ElType,Axes,Nothing}) where {Style,ElType,Axes}
    @inline _newindexer(arg) = newindexer(axes(bc), arg)
    args = map(instantiate, bc.args)
    indexing = map(_newindexer, args)
    return instantiate(Broadcasted{Style,ElType}(bc.f, args, axes(bc), indexing))
end

instantiate(bc::Broadcasted{Style,ElType,Axes,Indexing}) where {Style,ElType,Axes,Indexing<:Tuple} = bc


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
function flatten(bc::Broadcasted{Style,ElType}) where {Style,ElType}
    isflat(bc.args) && return bc
    # concatenate the nested arguments into {a, b, c, d}
    args = cat_nested(x->x.args, bc)
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
        return Broadcasted{Style,ElType}(newf, args)
    end
end

function flatten(bc::BroadcastedF{Style,ElType}) where {Style,ElType}
    isflat(bc.args) && return bc
    # Since bc is instantiated, let's preserve the instatiation in the result
    args, indexing = cat_nested(x->x.args, bc), cat_nested(x->x.indexing, bc)
    let makeargs = make_makeargs(bc)
        newf = @inline function(args::Vararg{Any,N}) where N
            bc.f(makeargs(args...)...)
        end
        return Broadcasted{Style,ElType}(newf, args, axes(bc), indexing)
    end
end

isflat(args::NestedTuple) = false
isflat(args::Tuple) = isflat(tail(args))
isflat(args::Tuple{}) = true

cat_nested(fieldextractor, bc::Broadcasted) = cat_nested(fieldextractor, fieldextractor(bc), ())

cat_nested(fieldextractor, t::Tuple, rest) =
    (t[1], cat_nested(fieldextractor, tail(t), rest)...)
cat_nested(fieldextractor, t::Tuple{<:Broadcasted,Vararg{Any}}, rest) =
    cat_nested(fieldextractor, cat_nested(fieldextractor, fieldextractor(t[1]), tail(t)), rest)
cat_nested(fieldextractor, t::Tuple{}, tail) = cat_nested(fieldextractor, tail, ())
cat_nested(fieldextractor, t::Tuple{}, tail::Tuple{}) = ()

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
@inline newindex(i::Int, keep::Tuple{Bool}, idefault) = ifelse(keep[1], i, idefault)
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

# For Broadcasted
Base.@propagate_inbounds _broadcast_getindex(bc::BroadcastedF{Style, ElType, N, F, Args}, I::Union{Int,CartesianIndex{N}}) where {Style,ElType,N,F,Args} =
    _broadcast_getindex_bc(bc, I)
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted, I)
    broadcast_skip_axes_instantiation(bc) && return _broadcast_getindex_bc(bc, I)
    broadcast_getindex_error(bc, I)
end

# Utilities for _broadcast_getindex
# For most styles
Base.@propagate_inbounds _getidx(arg, I, keep_default) = _broadcast_getindex(arg, newindex(I, keep_default...))
Base.@propagate_inbounds _getindex(args::Tuple, I, indexing::Tuple) =
    (_getidx(args[1], I, indexing[1]), _getindex(tail(args), I, tail(indexing))...)
Base.@propagate_inbounds _getindex(args::Tuple{Any}, I, indexing::Tuple{Any}) =
    (_getidx(args[1], I, indexing[1]),)
Base.@propagate_inbounds _getindex(args::Tuple{}, I, ::Tuple) = ()
Base.@propagate_inbounds _getindex(args::Tuple{}, I, ::Tuple{Any}) = ()
# For styles that bypass construction of indexing
Base.@propagate_inbounds _getindex(args::Tuple, I, ::Nothing) =
    (_broadcast_getindex(args[1], I), _getindex(tail(args), I, nothing)...)
Base.@propagate_inbounds _getindex(args::Tuple{Any}, I, ::Nothing) =
    (_broadcast_getindex(args[1], I),)
Base.@propagate_inbounds _getindex(args::Tuple{}, I, ::Nothing) = ()

Base.@propagate_inbounds function _broadcast_getindex_bc(bc::Broadcasted, I)
    args = _getindex(bc.args, I, bc.indexing)
    return _broadcast_getindex_evalf(bc.f, args...)
end

"""
    broadcastable(x)

Return either `x` or an object like `x` such that it supports `axes`, indexing, and its type supports `ndims`.

If `x` supports iteration, the returned value should have the same `axes` and indexing
behaviors as [`collect(x)`](@ref).

If `x` is not an `AbstractArray` but it supports `axes`, indexing, and its type supports
`ndims`, then `broadcastable(::typeof(x))` may be implemented to just return itself.
Further, if `x` defines its own [`BroadcastStyle`](@ref), then it must define its
`broadcastable` method to return itself for the custom style to have any effect.

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
broadcastable(x::Union{Symbol,AbstractString,Function,UndefInitializer,Nothing,RoundingMode,Missing,Val}) = Ref(x)
broadcastable(x::Ptr) = Ref{Ptr}(x) # Cannot use Ref(::Ptr) until ambiguous deprecation goes through
broadcastable(::Type{T}) where {T} = Ref{Type{T}}(T)
broadcastable(x::Union{AbstractArray,Number,Ref,Tuple,Broadcasted}) = x
# In the future, default to collecting arguments. TODO: uncomment once deprecations are removed
# broadcastable(x) = collect(x)
# broadcastable(::Union{AbstractDict, NamedTuple}) = error("intentionally unimplemented to allow development in 1.x")

@inline _broadcast_getindex_evalf(f::Tf, args::Vararg{Any,N}) where {Tf,N} = f(args...)  # not propagate_inbounds

@noinline function broadcast_getindex_error(bc, I)
    isa(bc, BroadcastedF) && error("axes $(axes(bc)) does not match $I")
    error("indexing requires complete instantiation")
end

# An element type satisfying for all A:
# broadcast_getindex(
#     combine_styles(A),
#     A, broadcast_indices(A)
# )::_broadcast_getindex_eltype(A)
_broadcast_getindex_eltype(A) = _broadcast_getindex_eltype(combine_styles(A), A)
_broadcast_getindex_eltype(::BroadcastStyle, A) = eltype(A)  # Tuple, Array, etc.
_broadcast_getindex_eltype(::DefaultArrayStyle{0}, ::Ref{T}) where {T} = T

eltypes(::Tuple{}) = Tuple{}
eltypes(t::Tuple{Any}) = Tuple{_broadcast_getindex_eltype(t[1])}
eltypes(t::Tuple{Any,Any}) = Tuple{_broadcast_getindex_eltype(t[1]), _broadcast_getindex_eltype(t[2])}
eltypes(t::Tuple) = Tuple{_broadcast_getindex_eltype(t[1]), eltypes(tail(t)).types...}

# Inferred eltype of result of broadcast(f, args...)
combine_eltypes(f, args::Tuple) = Base._return_type(f, eltypes(args))

maptoTuple(f) = Tuple{}
maptoTuple(f, a, b...) = Tuple{f(a), maptoTuple(f, b...).types...}
combine_eltypes(f, A, As...) =
    Base._return_type(f, maptoTuple(_broadcast_getindex_eltype, A, As...))

## Broadcasting core

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
broadcast(f::Tf, As...) where {Tf} = copy(instantiate(make(f, As...)))

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
"""
broadcast!(f::Tf, dest, As::Vararg{Any,N}) where {Tf,N} = (materialize!(dest, make(f, As...)); dest)

"""
    Broadcast.materialize(bc)

Take a lazy `Broadcasted` object and compute the result
"""
materialize(bc::Broadcasted) = copy(instantiate(bc))
materialize(x) = x
function materialize!(dest, bc::Broadcasted)
    args = map(instantiate, bc.args)
    axs = combine_indices(dest, args...)
    return copyto!(dest, instantiate(Broadcasted(bc.f, args), axs))
end
function materialize!(dest, x)
    axs = combine_indices(dest, x)
    return copyto!(dest, instantiate(Broadcasted(identity, (x,)), axs))
end

## general `copy` methods
copy(bc::Broadcasted{<:AbstractArrayStyle{0}, ElType}) where ElType = _broadcast_getindex(bc, 1)
copy(bc::Broadcasted{Nothing}) = error("broadcasting requires an assigned BroadcastStyle")
copy(bc::Broadcasted{Unknown}) = error("broadcasting requires an assigned BroadcastStyle")

const NonleafHandlingStyles = Union{DefaultArrayStyle,ArrayConflict}

function copy(bc::Broadcasted{Style, ElType}) where {Style, ElType}
    # Special handling for types that should be treated incrementally
    if Style<:NonleafHandlingStyles && !Base.isconcretetype(ElType)
        return copy_nonleaf(bc)
    end
    dest = broadcast_similar(Style(), ElType, axes(bc), bc)
    return copyto!(dest, bc)
end

# When ElType is not concrete, use narrowing. Use the first output
# value to determine the starting output eltype; copyto_nonleaf!
# will widen `dest` as needed to accommodate later values.
function copy_nonleaf(bc::Broadcasted{Style,ElType}) where {Style,ElType}
    iter = CartesianIndices(axes(bc))
    state = start(iter)
    if done(iter, state)
        # if empty, take the ElType at face value
        return broadcast_similar(Style(), ElType, axes(bc), bc)
    end
    # Initialize using the first value
    I, state = next(iter, state)
    val = _broadcast_getindex(bc, I)
    dest = broadcast_similar(Style(), typeof(val), axes(bc), bc)
    dest[I] = val
    # Now handle the remaining values
    return copyto_nonleaf!(dest, bc, iter, state, 1)
end

## general `copyto!` methods
# The most general method falls back to a method that replaces Style->Nothing
# This permits specialization on typeof(dest) without introducing ambiguities
@inline copyto!(dest::AbstractArray, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))

# Performance optimization for the Scalar case
@inline function copyto!(dest::AbstractArray, bc::Broadcasted{<:AbstractArrayStyle{0},ElType,Nothing,Nothing}) where ElType
    if not_nested(bc)
        if bc.f === identity && bc.args isa Tuple{Any} # only a single input argument to broadcast!
            # broadcast!(identity, dest, val) is equivalent to fill!(dest, val)
            return fill!(dest, bc.args[1])
        else
            args = bc.args
            @inbounds for I in eachindex(dest)
                dest[I] = bc.f(args...)
            end
            return dest
        end
    end
    # Fall back to the default implementation
    return copyto!(dest, instantiate(instantiate_axes(bc)))
end

# For broadcasted assignments like `broadcast!(f, A, ..., A, ...)`, where `A`
# appears on both the LHS and the RHS of the `.=`, then we know we're only
# going to make one pass through the array, and even though `A` is aliasing
# against itself, the mutations won't affect the result as the indices on the
# LHS and RHS will always match. This is not true in general, but with the `.op=`
# syntax it's fairly common for an argument to be `===` a source.
broadcast_unalias(dest, src) = dest === src ? src : unalias(dest, src)
map_broadcasted_args(f, bc::Broadcasted) = typeof(bc)(bc.f, map(arg->map_broadcasted_args(f, arg), bc.args), bc.axes, bc.indexing)
map_broadcasted_args(f, arg) = f(arg)

# Specialize this method if all you want to do is specialize on typeof(dest)
@inline function copyto!(dest::AbstractArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    # Performance optimization: broadcast!(identity, dest, A) is equivalent to copyto!(dest, A) if indices match
    if bc.f === identity && bc.args isa Tuple{<:AbstractArray} # only a single input argument to broadcast!
        A = bc.args[1]
        if axes(dest) == axes(A)
            return copyto!(dest, A)
        end
    end
    bc′ = map_broadcasted_args(arg->broadcast_unalias(dest, arg), bc)
    @simd for I in CartesianIndices(axes(bc′))
        @inbounds dest[I] = _broadcast_getindex(bc′, I)
    end
    return dest
end

# Performance optimization: for BitArray outputs, we cache the result
# in a "small" Vector{Bool}, and then copy in chunks into the output
function copyto!(dest::BitArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    ischunkedbroadcast(dest, bc) && return chunkedcopyto!(dest, bc)
    tmp = Vector{Bool}(undef, bitcache_size)
    destc = dest.chunks
    ind = cind = 1
    @simd for I in CartesianIndices(axes(bc))
        @inbounds tmp[ind] = _broadcast_getindex(bc, I)
        ind += 1
        if ind > bitcache_size
            dumpbitcache(destc, cind, tmp)
            cind += bitcache_chunks
            ind = 1
        end
    end
    if ind > 1
        @inbounds tmp[ind:bitcache_size] = false
        dumpbitcache(destc, cind, tmp)
    end
    return dest
end

# For some BitArray operations, we can work at the level of chunks. The trivial
# implementation just walks over the UInt64 chunks in a linear fashion.
# This requires three things:
#   1. The function must be known to work at the level of chunks
#   2. The only arrays involved must be BitArrays or scalars
#   3. There must not be any broadcasting beyond scalar — all array sizes must match
# We could eventually allow for all broadcasting and other array types, but that
# requires very careful consideration of all the edge effects.
const ChunkableOp = Union{typeof(&), typeof(|), typeof(xor), typeof(~)}
const BroadcastedChunkableOp{Style<:Union{Nothing,BroadcastStyle}, ElType, Axes, Indexing<:Union{Nothing,Tuple}, F<:ChunkableOp, Args<:Tuple} = Broadcasted{Style,ElType,Axes,Indexing,F,Args}
ischunkedbroadcast(R, bc::BroadcastedChunkableOp) = ischunkedbroadcast(R, bc.args)
ischunkedbroadcast(R, args) = false
ischunkedbroadcast(R, args::Tuple{<:BitArray,Vararg{Any}}) = size(R) == size(args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:Bool,Vararg{Any}}) = ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:BroadcastedChunkableOp,Vararg{Any}}) = ischunkedbroadcast(R, args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{}) = true

liftchunks(::Tuple{}) = ()
liftchunks(args::Tuple{<:BitArray,Vararg{Any}}) = (args[1].chunks, liftchunks(tail(args))...)
# Transform scalars to repeated scalars the size of a chunk
liftchunks(args::Tuple{<:Bool,Vararg{Any}}) = (ifelse(args[1], typemax(UInt64), UInt64(0)), liftchunks(tail(args))...)
ithchunk(i) = ()
Base.@propagate_inbounds ithchunk(i, c::Vector{UInt64}, args...) = (c[i], ithchunk(i, args...)...)
Base.@propagate_inbounds ithchunk(i, b::UInt64, args...) = (b, ithchunk(i, args...)...)
function chunkedcopyto!(dest::BitArray, bc::Broadcasted)
    isempty(dest) && return dest
    f = flatten(bc)
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
    while !done(iter, state)
        I, state = next(iter, state)
        @inbounds val = _broadcast_getindex(bc, I)
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
@inline tuplebroadcast(::NTuple{N,Any}, bc) where {N} =
    ntuple(k -> _broadcast_getindex(bc, k), Val(N))
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
# TODO: WAS THIS IMPORTANT?
# @inline broadcast(f, ::Style{Tuple}, ::Nothing, ::Nothing, A, Bs...) =
#     tuplebroadcast(f, longest_tuple(A, Bs...), A, Bs...)
# @inline tuplebroadcast(f, ::NTuple{N,Any}, As...) where {N} =
#     ntuple(k -> f(tuplebroadcast_getargs(As, k)...), Val(N))
# @inline tuplebroadcast(f, ::NTuple{N,Any}, ::Ref{Type{T}}, As...) where {N,T} =
#     ntuple(k -> f(T, tuplebroadcast_getargs(As, k)...), Val(N))
# longest_tuple(A::Tuple, B::Tuple, Bs...) = longest_tuple(_longest_tuple(A, B), Bs...)
# longest_tuple(A, B::Tuple, Bs...) = longest_tuple(B, Bs...)
# longest_tuple(A::Tuple, B, Bs...) = longest_tuple(A, Bs...)
# longest_tuple(A, B, Bs...) = longest_tuple(Bs...)
# longest_tuple(A::Tuple) = A
# Support only 1-tuples and N-tuples where there are no conflicts in N
_longest_tuple(A::Tuple{Any}, B::Tuple{Any}) = A
_longest_tuple(A::Tuple{Any}, B::NTuple{N,Any}) where N = B
_longest_tuple(A::NTuple{N,Any}, B::Tuple{Any}) where N = A
_longest_tuple(A::NTuple{N,Any}, B::NTuple{N,Any}) where N = A
@noinline _longest_tuple(A, B) =
    throw(DimensionMismatch("tuples $A and $B could not be broadcast to a common size"))

## scalar-range broadcast operations ##
# DefaultArrayStyle and \ are not available at the time of range.jl
make(::DefaultArrayStyle{1}, ::typeof(-), r::OrdinalRange) = range(-first(r), step=-step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen) = StepRangeLen(-r.ref, -r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange) = LinRange(-r.start, -r.stop, length(r))

make(::DefaultArrayStyle{1}, ::typeof(+), x::Real, r::AbstractUnitRange) = range(x + first(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractUnitRange, x::Real) = range(first(r) + x, length=length(r))
# For #18336 we need to prevent promotion of the step type:
make(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractRange, x::Number) = range(first(r) + x, step=step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::AbstractRange) = range(x + first(r), step=step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(+), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)+x)}(r.ref + x, r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x+T(r.ref))}(x + r.ref, r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(+), r::LinRange, x::Number) = LinRange(r.start + x, r.stop + x, length(r))
make(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::LinRange) = LinRange(x + r.start, x + r.stop, length(r))
make(::DefaultArrayStyle{1}, ::typeof(+), r1::AbstractRange, r2::AbstractRange) = r1 + r2

make(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractUnitRange, x::Number) = range(first(r)-x, length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractRange, x::Number) = range(first(r)-x, step=step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::AbstractRange) = range(x-first(r), step=-step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)-x)}(r.ref - x, r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x-T(r.ref))}(x - r.ref, -r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange, x::Number) = LinRange(r.start - x, r.stop - x, length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::LinRange) = LinRange(x - r.start, x - r.stop, length(r))
make(::DefaultArrayStyle{1}, ::typeof(-), r1::AbstractRange, r2::AbstractRange) = r1 - r2

make(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::AbstractRange) = range(x*first(r), step=x*step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::StepRangeLen{T}) where {T} =
    StepRangeLen{typeof(x*T(r.ref))}(x*r.ref, x*r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::LinRange) = LinRange(x * r.start, x * r.stop, r.len)
# separate in case of noncommutative multiplication
make(::DefaultArrayStyle{1}, ::typeof(*), r::AbstractRange, x::Number) = range(first(r)*x, step=step(r)*x, length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(*), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)*x)}(r.ref*x, r.step*x, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(*), r::LinRange, x::Number) = LinRange(r.start * x, r.stop * x, r.len)

make(::DefaultArrayStyle{1}, ::typeof(/), r::AbstractRange, x::Number) = range(first(r)/x, step=step(r)/x, length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(/), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)/x)}(r.ref/x, r.step/x, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(/), r::LinRange, x::Number) = LinRange(r.start / x, r.stop / x, r.len)

make(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::AbstractRange) = range(x\first(r), step=x\step(r), length=length(r))
make(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::StepRangeLen) = StepRangeLen(x\r.ref, x\r.step, length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::LinRange) = LinRange(x \ r.start, x \ r.stop, r.len)

make(::DefaultArrayStyle{1}, ::typeof(big), r::UnitRange) = big(r.start):big(last(r))
make(::DefaultArrayStyle{1}, ::typeof(big), r::StepRange) = big(r.start):big(r.step):big(last(r))
make(::DefaultArrayStyle{1}, ::typeof(big), r::StepRangeLen) = StepRangeLen(big(r.ref), big(r.step), length(r), r.offset)
make(::DefaultArrayStyle{1}, ::typeof(big), r::LinRange) = LinRange(big(r.start), big(r.stop), length(r))


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
    broadcast_getindex!(Base.similar(Array{eltype(src)}, combine_indices(I...)), src, I...)

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

Base.show(io::IO, bc::Broadcasted) = print(io, "Broadcasted(", bc.f, ", ", bc.args, ')')

@inline make_kwsyntax(f, args...; kwargs...) = make((args...)->f(args...; kwargs...), args...)
@inline function make(f, args...)
    args′ = map(broadcastable, args)
    make(combine_styles(args′...), f, args′...)
end
# Due to the current Type{T}/DataType specialization heuristics within Tuples,
# the totally generic varargs make(f, args...) method above loses Type{T}s in
# mapping broadcastable across the args. These additional methods with explicit
# arguments ensure we preserve Type{T}s in the first or second argument position.
@inline function make(f, arg1, args...)
    arg1′ = broadcastable(arg1)
    args′ = map(broadcastable, args)
    make(combine_styles(arg1′, args′...), f, arg1′, args′...)
end
@inline function make(f, arg1, arg2, args...)
    arg1′ = broadcastable(arg1)
    arg2′ = broadcastable(arg2)
    args′ = map(broadcastable, args)
    make(combine_styles(arg1′, arg2′, args′...), f, arg1′, arg2′, args′...)
end
@inline make(::S, f, args...) where S<:BroadcastStyle = Broadcasted{S}(f, args)

end # module

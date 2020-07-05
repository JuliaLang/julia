# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    into(T::Type, iterable) -> collection::T
    into(T::Type) -> iterable -> collection::T

Construct a new `collection` of type `T` that contains the elements in `iterable`.  If
`iterable` is also a container, it acts as a shallow-copy.

If `T` has `eltype`, `keytype`, or `valtype` information, all elements in `collection` are
converted to the destination type to guarantee the constraint `collection isa T`.

If `T` has size or length information (e.g., `NTuple` and `StaticArray`), providing
`collection` with unmatched size or length throws an error.

Unary form `into(T::Type)` returns a callable `iterable -> into(T, iterable)`.

# Extended help

## Example

`into` takes care of the conversion of storage and element types:

```jldoctest
julia> into(Array{Int}, BitVector([0, 1, 0, 0]))
4-element Vector{Int64}:
 0
 1
 0
 0
```

`into` acts like a shallow copy:

```jldoctest
julia> xs = Ref.([1, 2, 3]);

julia> ys = into(Vector, xs)
3-element Vector{Base.RefValue{Int64}}:
 Base.RefValue{Int64}(1)
 Base.RefValue{Int64}(2)
 Base.RefValue{Int64}(3)

julia> ys[1] = Ref(100);

julia> xs
3-element Vector{Base.RefValue{Int64}}:
 Base.RefValue{Int64}(1)
 Base.RefValue{Int64}(2)
 Base.RefValue{Int64}(3)

julia> ys[2][] = 200;

julia> xs
3-element Vector{Base.RefValue{Int64}}:
 Base.RefValue{Int64}(1)
 Base.RefValue{Int64}(200)
 Base.RefValue{Int64}(3)
```

`into` _always_ treats input `iterable` as a collection:

```jldoctest
julia> into(Dict, (:a => 1) => (:b => 2))
Dict{Symbol,Int64} with 2 entries:
  :a => 1
  :b => 2

julia> Dict((:a => 1) => (:b => 2))  # but the constructor may not
Dict{Pair{Symbol,Int64},Pair{Symbol,Int64}} with 1 entry:
  :a=>1 => :b=>2
```

`into(T)` returns a function `iterable -> into(T, iterable)` which is
appropriate for using with [`|>`](@ref):

```jldoctest
julia> 1:3 |> into(NTuple{3})
(1, 2, 3)
```

## Implementation

The owner of the type of `iterable` should implement [`__from__`](@ref __into__).
The owner of the output type `T` should implement [`__into__`](@ref).  If it is desirable to
apply pre- and/or post-processing, the owner of the output type `T` may implement `into`.
"""
into(::Type{T}, iterable) where {T} = __from__(T, iterable)::T
into(::Type{T}) where {T} = Fix1(into, T)

function __into__ end
__from__(::Type{T}, iterable) where {T} = __into__(T, iterable)

"""
    Base.__into__(T::Type, iterable) -> collection::T
    Base.__from__(T::Type, iterable) -> collection::T

Overload-only API for providing the implementation for `into(T, iterable)`.

To avoid method ambiguities, `__into__` should be implemented only by the owner of `T` and
`__from__` should be implemented only by the owner of `typeof(iterable)`.  The owner of
`T` (resp. `typeof(iterable)`) may choose to allow `typeof(iterable)` (resp. `T`) to
overload `__into__` (resp. `__from__`) by documenting specific type bounds for
`T` (resp. `typeof(iterable)`).

`into(T, iterable)` calls `__from__(T, iterable)` which in turn by default calls
`__into__(T, iterable)`.

## Implementation

If `T` is a subtype of `AbstractArray`,

```julia
isequal(vec(collect′(iterable)), vec(collect(collection)))
```

must hold where `collect′` is defined as

```julia
collect′(iterable) =
    if IteratorEltype(collection) isa HasEltype
        collect(eltype(collection), iterable)
    else
        collect(iterable)
    end
```

If `iterable` is a stateful iterator, `collect` inside `collect′` is "run" as if the world
is rolled back to the state just before calling `into`.

If the collections of type `T` do not support duplicates, `issubset` may be used instead of
`isequal`.  In particular, subtypes of `AbstractDict` and `AbstractSet` must satisfy the
above equality with `issubset`.

If the collections of type `T` do not maintain insertion order, `issetequal` may be used
instead of `isequal`.
"""
(__from__, __into__)

__into__(::Type{T}, iterable) where {T<:Array} = collect(iterable)
__into__(::Type{T}, iterable) where {E,T<:Array{E}} = collect(E, iterable)

__into__(::Type{Vector}, iterable) = vec(collect(iterable))::Vector
__into__(::Type{T}, iterable) where {E,T<:Vector{E}} = vec(collect(E, iterable))

__into__(::Type{T}, iterable) where {T<:BitArray} = T(iterable)

__into__(::Type{T}, iterable) where {T<:Union{Dict,IdDict}} = T(iterable)
__into__(::Type{T}, (a, b)::Pair) where {T<:Union{Dict,IdDict}} = T((a, b))

__into__(::Type{T}, iterable) where {T<:ImmutableDict} = foldl(T, iterable)

__into__(::Type{T}, iterable) where {T<:Union{Set,IdSet}} = T(iterable)

@noinline _too_many_items_error(N, x) = throw(ArgumentError(
    "`iterable` contains more than $N element(s);" *
    " $(N+1)-th element is `$x`"
))

@noinline _not_enough_items_error(N, n_actual) = throw(ArgumentError(
    "$N items required; `iterable` contains only $n_actual element(s)"
))

function __into__(::Type{Tuple{}}, iterable)
    y = iterate(iterable)
    y === nothing && return ()
    _too_many_items_error(0, y[1])
end

function __into__(::Type{NTuple{N,Any}}, iterable) where {N}
    y = iterate(iterable)
    y === nothing && _not_enough_items_error(N, 0)
    x, state = y
    collection, state = _foldoneto(((x,), state), Val(N - 1)) do (acc, state), i
        local y
        y = iterate(iterable, state)
        y === nothing && _not_enough_items_error(N, i)
        ((acc..., y[1]), y[2])
    end
    y = iterate(iterable, state)
    y === nothing && return collection
    _too_many_items_error(N, y[1])
end

__into__(::Type{T}, iterable) where {T<:Tuple} = convert(T, Tuple(iterable))
__into__(::Type{T}, iterable) where {N,T<:NTuple{N,Any}} =
    convert(T, __into__(NTuple{N,Any}, iterable))

function __into__(::Type{NTuple{<:Any,T}}, iterable) where {T}
    collection = Tuple(iterable)
    return convert(NTuple{length(collection),T}, collection)
end

__into__(::Type{NTuple{N}}, iterable) where {N} =
    promote(__into__(NTuple{N,Any}, iterable)...)
__into__(::Type{NTuple}, iterable) = promote(iterable...)

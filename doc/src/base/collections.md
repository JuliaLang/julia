# Collections and Data Structures

## [Iteration](@id lib-collections-iteration)

Sequential iteration is implemented by the [`iterate`](@ref) function.
The general `for` loop:

```julia
for i in iter   # or  "for i = iter"
    # body
end
```

is translated into:

```julia
next = iterate(iter)
while next !== nothing
    (i, state) = next
    # body
    next = iterate(iter, state)
end
```

The `state` object may be anything, and should be chosen appropriately for each iterable type.
See the [manual section on the iteration interface](@ref man-interface-iteration) for more details about defining a custom
iterable type.

```@docs
Base.iterate
Base.IteratorSize
Base.IteratorEltype
```

Fully implemented by:

  * [`AbstractRange`](@ref)
  * [`UnitRange`](@ref)
  * `Tuple`
  * `Number`
  * [`AbstractArray`](@ref)
  * [`BitSet`](@ref)
  * [`IdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)
  * `EachLine`
  * `AbstractString`
  * [`Set`](@ref)
  * [`Pair`](@ref)
  * [`NamedTuple`](@ref)

## Constructors and Types

```@docs
Base.AbstractRange
Base.OrdinalRange
Base.AbstractUnitRange
Base.StepRange
Base.UnitRange
Base.LinRange
```

## General Collections

```@docs
Base.isempty
Base.empty!
Base.length
```

Fully implemented by:

  * [`AbstractRange`](@ref)
  * [`UnitRange`](@ref)
  * `Tuple`
  * `Number`
  * [`AbstractArray`](@ref)
  * [`BitSet`](@ref)
  * [`IdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)
  * `AbstractString`
  * [`Set`](@ref)
  * [`NamedTuple`](@ref)

## Iterable Collections

```@docs
Base.in
Base.:∉
Base.eltype
Base.indexin
Base.unique
Base.unique!
Base.allunique
Base.reduce(::Any, ::Any)
Base.foldl(::Any, ::Any)
Base.foldr(::Any, ::Any)
Base.maximum
Base.maximum!
Base.minimum
Base.minimum!
Base.extrema
Base.argmax
Base.argmin
Base.findmax
Base.findmin
Base.findmax!
Base.findmin!
Base.sum
Base.sum!
Base.prod
Base.prod!
Base.any(::Any)
Base.any(::AbstractArray, ::Any)
Base.any!
Base.all(::Any)
Base.all(::AbstractArray, ::Any)
Base.all!
Base.count
Base.any(::Any, ::Any)
Base.all(::Any, ::Any)
Base.foreach
Base.map
Base.map!
Base.mapreduce(::Any, ::Any, ::Any)
Base.mapfoldl(::Any, ::Any, ::Any)
Base.mapfoldr(::Any, ::Any, ::Any)
Base.first
Base.last
Base.step
Base.collect(::Any)
Base.collect(::Type, ::Any)
Base.filter
Base.filter!
Base.replace(::Any, ::Pair...)
Base.replace(::Base.Callable, ::Any)
Base.replace!
```

## Indexable Collections

```@docs
Base.getindex
Base.setindex!
Base.firstindex
Base.lastindex
```

Fully implemented by:

  * [`Array`](@ref)
  * [`BitArray`](@ref)
  * [`AbstractArray`](@ref)
  * `SubArray`

Partially implemented by:

  * [`AbstractRange`](@ref)
  * [`UnitRange`](@ref)
  * `Tuple`
  * `AbstractString`
  * [`Dict`](@ref)
  * [`IdDict`](@ref)
  * [`WeakKeyDict`](@ref)
  * [`NamedTuple`](@ref)

## Dictionaries

[`Dict`](@ref) is the standard dictionary. Its implementation uses [`hash`](@ref)
as the hashing function for the key, and [`isequal`](@ref) to determine equality. Define these
two functions for custom types to override how they are stored in a hash table.

[`IdDict`](@ref) is a special hash table where the keys are always object identities.

[`WeakKeyDict`](@ref) is a hash table implementation where the keys are weak references to objects, and
thus may be garbage collected even when referenced in a hash table.
Like `Dict` it uses `hash` for hashing and `isequal` for equality, unlike `Dict` it does
not convert keys on insertion.

[`Dict`](@ref)s can be created by passing pair objects constructed with `=>` to a [`Dict`](@ref)
constructor: `Dict("A"=>1, "B"=>2)`. This call will attempt to infer type information from the
keys and values (i.e. this example creates a `Dict{String, Int64}`). To explicitly specify types
use the syntax `Dict{KeyType,ValueType}(...)`. For example, `Dict{String,Int32}("A"=>1, "B"=>2)`.

Dictionaries may also be created with generators. For example, `Dict(i => f(i) for i = 1:10)`.

Given a dictionary `D`, the syntax `D[x]` returns the value of key `x` (if it exists) or throws
an error, and `D[x] = y` stores the key-value pair `x => y` in `D` (replacing any existing value
for the key `x`).  Multiple arguments to `D[...]` are converted to tuples; for example, the syntax
`D[x,y]`  is equivalent to `D[(x,y)]`, i.e. it refers to the value keyed by the tuple `(x,y)`.

```@docs
Base.Dict
Base.IdDict
Base.WeakKeyDict
Base.ImmutableDict
Base.haskey
Base.get(::Any, ::Any, ::Any)
Base.get
Base.get!(::Any, ::Any, ::Any)
Base.get!(::Function, ::Any, ::Any)
Base.getkey
Base.delete!
Base.pop!(::Any, ::Any, ::Any)
Base.keys
Base.values
Base.pairs
Base.merge
Base.merge!(::AbstractDict, ::AbstractDict...)
Base.merge!(::Function, ::AbstractDict, ::AbstractDict...)
Base.sizehint!
Base.keytype
Base.valtype
```

Fully implemented by:

  * [`IdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)

Partially implemented by:

  * [`BitSet`](@ref)
  * [`Set`](@ref)
  * [`EnvDict`](@ref Base.EnvDict)
  * [`Array`](@ref)
  * [`BitArray`](@ref)
  * [`ImmutableDict`](@ref Base.ImmutableDict)
  * [`Iterators.Pairs`](@ref)

## Set-Like Collections

```@docs
Base.Set
Base.BitSet
Base.union
Base.union!
Base.intersect
Base.setdiff
Base.setdiff!
Base.symdiff
Base.symdiff!
Base.intersect!
Base.issubset
Base.:⊈
Base.:⊊
Base.issetequal
```

Fully implemented by:

  * [`BitSet`](@ref)
  * [`Set`](@ref)

Partially implemented by:

  * [`Array`](@ref)

## Dequeues

```@docs
Base.push!
Base.pop!
Base.pushfirst!
Base.popfirst!
Base.insert!
Base.deleteat!
Base.splice!
Base.resize!
Base.append!
Base.prepend!
```

Fully implemented by:

  * `Vector` (a.k.a. 1-dimensional [`Array`](@ref))
  * `BitVector` (a.k.a. 1-dimensional [`BitArray`](@ref))

## Utility Collections

```@docs
Base.Pair
Iterators.Pairs
```

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
Base.front
Base.tail
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
Base.AbstractDict
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
Base.mergewith
Base.merge!
Base.mergewith!
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
A set is a collection of elements, just like an array or dictionary, with no duplicated elements.

The two important differences between a set and other types of collection is that in a set you can have only one of each element, and, in a set, the order of elements isn't important (whereas an array can have multiple copies of an element and their order is remembered).
```julia
julia> colors = Set(["yellow", "blue", "green", "red"])
Set(["yellow", "blue", "green", "red"])
```

You can use [`push!`](@Ref) to add elements to a set:
```julia
julia> push!(colors, "black")
Set(["yellow", "blue", "green", "black", "red"])
```

If you try to add something to the set that's already there nothing will happen, because sets don't store repeated elements.

You can't use [`pushfirst!`](@Ref), because that works only for things that have a concept of "first", like arrays.

To see if something is in the set, you can use [`in`](@Ref):
```julia
julia> in("green", colors)
true
```
or using infix notation
```julia
julia> "green" in colors
true
```

There are some standard operations you can do with sets, namely find their `union`, `intersection`, and `difference`, with the functions, [`union`](@Ref), [`intersect`](@Ref), and [`setdiff`](@Ref):
```julia
julia> rainbow = Set(["red","orange","yellow","green","blue","indigo","violet"])
Set(["indigo", "yellow", "orange", "blue", "violet", "green", "red"])
```
The union of two sets is the set of everything that is in one or the other sets. The result is another set:
```julia
julia> union(colors, rainbow)
Set(["indigo", "yellow", "orange", "blue", "violet", "green", "black", "red"])
```
The intersection of two sets is the set that contains every element that belongs to both sets:
```julia
julia> intersect(colors, rainbow)
Set(["yellow", "blue", "green", "red"])
```
The difference between two sets is the set of elements that are in the first set, but not in the second. This time, the order in which you supply the sets matters. The [`setdiff`](@Ref) function finds the elements that are in the first set, `colors`, but not in the second set, `rainbow`:
```julia
julia> setdiff(colors, rainbow)
Set(["black"])
 ```
[Ref.](https://en.wikibooks.org/wiki/Introducing_Julia/Dictionaries_and_sets#Sets)

```@docs
Base.AbstractSet
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
Base.isdisjoint
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

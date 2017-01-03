# Collections and Data Structures

## [Iteration](@id lib-collections-iteration)

Sequential iteration is implemented by the methods [`start()`](@ref), [`done()`](@ref), and [`next()`](@ref).
The general `for` loop:

```julia
for i = I   # or  "for i in I"
    # body
end
```

is translated into:

```julia
state = start(I)
while !done(I, state)
    (i, state) = next(I, state)
    # body
end
```

The `state` object may be anything, and should be chosen appropriately for each iterable type.
See the [manual section on the iteration interface](@ref man-interface-iteration) for more details about defining a custom
iterable type.

```@docs
Base.start
Base.done
Base.next
Base.iteratorsize
Base.iteratoreltype
```

Fully implemented by:

  * `Range`
  * `UnitRange`
  * `Tuple`
  * `Number`
  * `AbstractArray`
  * [`IntSet`](@ref)
  * [`ObjectIdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)
  * `EachLine`
  * `AbstractString`
  * [`Set`](@ref)

## General Collections

```@docs
Base.isempty
Base.empty!
Base.length(::Any)
Base.endof
```

Fully implemented by:

  * `Range`
  * `UnitRange`
  * `Tuple`
  * `Number`
  * `AbstractArray`
  * [`IntSet`](@ref)
  * [`ObjectIdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)
  * `AbstractString`
  * [`Set`](@ref)

## Iterable Collections

```@docs
Base.in
Base.eltype
Base.indexin
Base.findin
Base.unique
Base.allunique
Base.reduce(::Any, ::Any, ::Any)
Base.reduce(::Any, ::Any)
Base.foldl(::Any, ::Any, ::Any)
Base.foldl(::Any, ::Any)
Base.foldr(::Any, ::Any, ::Any)
Base.foldr(::Any, ::Any)
Base.maximum(::Any)
Base.maximum(::Any, ::Any)
Base.maximum!
Base.minimum(::Any)
Base.minimum(::Any, ::Any)
Base.minimum!
Base.extrema(::Any)
Base.extrema(::AbstractArray, ::Any)
Base.indmax
Base.indmin
Base.findmax(::Any)
Base.findmax(::AbstractArray, ::Any)
Base.findmin(::Any)
Base.findmin(::AbstractArray, ::Any)
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
Base.mapreduce(::Any, ::Any, ::Any, ::Any)
Base.mapreduce(::Any, ::Any, ::Any)
Base.mapfoldl(::Any, ::Any, ::Any, ::Any)
Base.mapfoldl(::Any, ::Any, ::Any)
Base.mapfoldr(::Any, ::Any, ::Any, ::Any)
Base.mapfoldr(::Any, ::Any, ::Any)
Base.first
Base.last
Base.step
Base.collect(::Any)
Base.collect(::Type, ::Any)
Base.issubset(::Any, ::Any)
Base.filter
Base.filter!
```

## Indexable Collections

```@docs
Base.getindex(::Any, ::Any...)
Base.setindex!(::Any, ::Any, ::Any...)
```

Fully implemented by:

  * [`Array`](@ref)
  * [`BitArray`](@ref)
  * `AbstractArray`
  * `SubArray`
  * [`ObjectIdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)
  * `AbstractString`

Partially implemented by:

  * `Range`
  * `UnitRange`
  * `Tuple`

## Associative Collections

[`Dict`](@ref) is the standard associative collection. Its implementation uses [`hash()`](@ref)
as the hashing function for the key, and [`isequal()`](@ref) to determine equality. Define these
two functions for custom types to override how they are stored in a hash table.

[`ObjectIdDict`](@ref) is a special hash table where the keys are always object identities.

[`WeakKeyDict`](@ref) is a hash table implementation where the keys are weak references to objects, and
thus may be garbage collected even when referenced in a hash table.

[`Dict`](@ref)s can be created by passing pair objects constructed with `=>()` to a [`Dict`](@ref)
constructor: `Dict("A"=>1, "B"=>2)`. This call will attempt to infer type information from the
keys and values (i.e. this example creates a `Dict{String, Int64}`). To explicitly specify types
use the syntax `Dict{KeyType,ValueType}(...)`. For example, `Dict{String,Int32}("A"=>1, "B"=>2)`.

Associative collections may also be created with generators. For example, `Dict(i => f(i) for i = 1:10)`.

Given a dictionary `D`, the syntax `D[x]` returns the value of key `x` (if it exists) or throws
an error, and `D[x] = y` stores the key-value pair `x => y` in `D` (replacing any existing value
for the key `x`).  Multiple arguments to `D[...]` are converted to tuples; for example, the syntax
`D[x,y]`  is equivalent to `D[(x,y)]`, i.e. it refers to the value keyed by the tuple `(x,y)`.

```@docs
Base.Dict
Base.ObjectIdDict
Base.WeakKeyDict
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
Base.merge
Base.merge!
Base.sizehint!
Base.keytype
Base.valtype
```

Fully implemented by:

  * [`ObjectIdDict`](@ref)
  * [`Dict`](@ref)
  * [`WeakKeyDict`](@ref)

Partially implemented by:

  * [`IntSet`](@ref)
  * [`Set`](@ref)
  * [`EnvHash`](@ref Base.EnvHash)
  * [`Array`](@ref)
  * [`BitArray`](@ref)

## Set-Like Collections

```@docs
Base.Set
Base.IntSet
Base.union
Base.union!
Base.intersect
Base.setdiff
Base.setdiff!
Base.symdiff
Base.symdiff!(::IntSet, ::Integer)
Base.symdiff!(::IntSet, ::Any)
Base.symdiff!(::IntSet, ::IntSet)
Base.intersect!
Base.issubset
```

Fully implemented by:

  * [`IntSet`](@ref)
  * [`Set`](@ref)

Partially implemented by:

  * [`Array`](@ref)

## Dequeues

```@docs
Base.push!
Base.pop!(::Any)
Base.unshift!
Base.shift!
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

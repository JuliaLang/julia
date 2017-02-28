# More about types

If you've used Julia for a while, you understand the fundamental role that types play.  Here we
try to get under the hood, focusing particularly on [Parametric Types](@ref).

## Types and sets (and `Any` and `Union{}`/`Bottom`)

It's perhaps easiest to conceive of Julia's type system in terms of sets. While programs manipulate
individual values, a type refers to a set of values. This is not the same thing as a collection;
for example a `Set` of values is itself a single `Set` value.
Rather, a type describes a set of *possible* values, expressing uncertainty about which value we
have.

A *concrete* type `T` describes the set of values whose direct tag, as returned by the `typeof`
function, is `T`. An *abstract* type describes some possibly-larger set of values.

`Any` describes the entire universe of possible values. `Integer` is a subset of `Any` that includes
`Int`, `Int8`, and other concrete types.
Internally, Julia also makes heavy use of another type known as `Bottom`, which can also be written
as `Union{}`. This corresponds to the empty set.

Julia's types support the standard operations of set theory: you can ask whether `T1` is a "subset"
(subtype) of `T2` with `T1 <: T2`. Likewise, you intersect two types using `typeintersect`, take
their union with `Union`, and compute a type that contains their union with `typejoin`:

```jldoctest
julia> typeintersect(Int, Float64)
Union{}

julia> Union{Int, Float64}
Union{Float64, Int64}

julia> typejoin(Int, Float64)
Real

julia> typeintersect(Signed, Union{UInt8, Int8})
Int8

julia> Union{Signed, Union{UInt8, Int8}}
Union{Signed, UInt8}

julia> typejoin(Signed, Union{UInt8, Int8})
Integer

julia> typeintersect(Tuple{Integer,Float64}, Tuple{Int,Real})
Tuple{Int64,Float64}

julia> Union{Tuple{Integer,Float64}, Tuple{Int,Real}}
Union{Tuple{Int64,Real}, Tuple{Integer,Float64}}

julia> typejoin(Tuple{Integer,Float64}, Tuple{Int,Real})
Tuple{Integer,Real}
```

While these operations may seem abstract, they lie at the heart of Julia.  For example, method
dispatch is implemented by stepping through the items in a method list until reaching one for which
the type of the argument tuple is a subtype of the method signature.
For this algorithm to work, it's important that methods be sorted by their specificity, and that the
search begins with the most specific methods. Consequently, Julia also implements a partial order on
types; this is achieved by functionality that is similar to `<:`, but with differences that will
be discussed below.

## UnionAll types

Julia's type system can also express an *iterated union* of types: a union of types over all values
of some variable. This is needed to describe parametric types where the values of some parameters
are not known.

For example, :obj:`Array` has two parameters as in `Array{Int,2}`. If we did not know the element
type, we could write `Array{T,2} where T`, which is the union of `Array{T,2}` for all values of
`T`: `Union{Array{Int8,2}, Array{Int16,2}, ...}`.

Such a type is represented by a `UnionAll` object, which contains a variable (`T` in this example,
of type `TypeVar`), and a wrapped type (`Array{T,2}` in this example).

Consider the following methods::

```julia
f1(A::Array) = 1
f2(A::Array{Int}) = 2
f3(A::Array{T}) where T<:Any = 3
f4(A::Array{Any}) = 4
```

The signature of `f3` is a `UnionAll` type wrapping a tuple type.
All but `f4` can be called with `a = [1,2]`; all but `f2` can be called with `b = Any[1,2]`.

Let's look at these types a little more closely:

```jldoctest
julia> dump(Array)
UnionAll
  var: TypeVar
    name: Symbol T
    lb: Core.BottomType Union{}
    ub: Any
  body: UnionAll
    var: TypeVar
      name: Symbol N
      lb: Core.BottomType Union{}
      ub: Any
    body: Array{T,N} <: DenseArray{T,N}
```

This indicates that `Array` actually names a `UnionAll` type. There is one `UnionAll` type for
each parameter, nested. The syntax `Array{Int,2}` is equivalent to `Array{Int}{2}`;
internally each `UnionAll` is instantiated with a particular variable value, one at a time,
outermost-first. This gives a natural meaning to the omission of trailing type parameters;
`Array{Int}` gives a type equivalent to `Array{Int,N} where N`.

A `TypeVar` is not itself a type, but rather should be considered part of the structure of a
`UnionAll` type. Type variables have lower and upper bounds on their values (in the fields
`lb` and `ub`). The symbol `name` is purely cosmetic. Internally, `TypeVar`s are compared by
address, so they are defined as mutable types to ensure that "different" type variables can be
distinguished. However, by convention they should not be mutated.

One can construct `TypeVar`s manually:

```jldoctest
julia> TypeVar(:V, Signed, Real)
Signed<:V<:Real
```

There are convenience versions that allow you to omit any of these arguments except the `name`
symbol.

The syntax `Array{T} where T<:Integer` is lowered to

```julia
let T = TypeVar(:T,Integer)
    UnionAll(T, Array{T})
end
```

so it is seldom necessary to construct a `TypeVar` manually (indeed, this is to be avoided).

## Free variables

The concept of a *free* type variable is extremely important in the type system. We say that a
variable `V` is free in type `T` if `T` does not contain the `UnionAll` that introduces variable
`V`. For example, the type `Array{Array{V} where V<:Integer}` has no free variables, but the
`Array{V}` part inside of it does have a free variable, `V`.

A type with free variables is, in some sense, not really a type at all. Consider the type
`Array{Array{T}} where T`, which refers to all homogeneous arrays of arrays.
The inner type `Array{T}`, seen by itself, might seem to refer to any kind of array.
However, every element of the outer array must have the *same* array type, so `Array{T}` cannot
refer to just any old array. One could say that `Array{T}` effectively "occurs" multiple times,
and `T` must have the same value each "time".

For this reason, the function `jl_has_free_typevars` in the C API is very important. Types for
which it returns true will not give meaningful answers in subtyping and other type functions.

## TypeNames

The following two [`Array`](@ref) types are functionally equivalent, yet print differently:

```jldoctest
julia> TV, NV = TypeVar(:T), TypeVar(:N)
(T, N)

julia> Array
Array

julia> Array{TV,NV}
Array{T,N}
```

These can be distinguished by examining the `name` field of the type, which is an object of type
`TypeName`:

```julia
julia> dump(Array{Int,1}.name)
TypeName
  name: Symbol Array
  module: Module Core
  names: empty SimpleVector
  wrapper: UnionAll
    var: TypeVar
      name: Symbol T
      lb: Core.BottomType Union{}
      ub: Any
    body: UnionAll
      var: TypeVar
        name: Symbol N
        lb: Core.BottomType Union{}
        ub: Any
      body: Array{T,N} <: DenseArray{T,N}
  cache: SimpleVector
    ...

  linearcache: SimpleVector
    ...

  hash: Int64 -7900426068641098781
  mt: MethodTable
    name: Symbol Array
    defs: Void nothing
    cache: Void nothing
    max_args: Int64 0
    kwsorter: #undef
    module: Module Core
    : Int64 0
    : Int64 0
```

In this case, the relevant field is `wrapper`, which holds a reference to the top-level type used
to make new `Array` types.

```julia
julia> pointer_from_objref(Array)
Ptr{Void} @0x00007fcc7de64850

julia> pointer_from_objref(Array.body.body.name.wrapper)
Ptr{Void} @0x00007fcc7de64850

julia> pointer_from_objref(Array{TV,NV})
Ptr{Void} @0x00007fcc80c4d930

julia> pointer_from_objref(Array{TV,NV}.name.wrapper)
Ptr{Void} @0x00007fcc7de64850
```

The `wrapper` field of [`Array`](@ref) points to itself, but for `Array{TV,NV}` it points back
to the original definition of the type.

What about the other fields? `hash` assigns an integer to each type.  To examine the `cache`
field, it's helpful to pick a type that is less heavily used than Array. Let's first create our
own type:

```jldoctest
julia> type MyType{T,N} end

julia> MyType{Int,2}
MyType{Int64,2}

julia> MyType{Float32, 5}
MyType{Float32,5}

julia> MyType.body.body.name.cache
svec(MyType{Float32,5}, MyType{Int64,2}, #undef, #undef, #undef, #undef, #undef, #undef)
```

(The cache is pre-allocated to have length 8, but only the first two entries are populated.) Consequently,
when you instantiate a parametric type, each concrete type gets saved in a type cache.  However,
instances containing free type variables are not cached.

## Tuple types

Tuple types constitute an interesting special case.  For dispatch to work on declarations like
`x::Tuple`, the type has to be able to accommodate any tuple.  Let's check the parameters:

```jldoctest
julia> Tuple
Tuple

julia> Tuple.parameters
svec(Vararg{Any,N} where N)
```

Unlike other types, tuple types are covariant in their parameters, so this definition permits
`Tuple` to match any type of tuple:

```jldoctest
julia> typeintersect(Tuple, Tuple{Int,Float64})
Tuple{Int64,Float64}

julia> typeintersect(Tuple{Vararg{Any}}, Tuple{Int,Float64})
Tuple{Int64,Float64}
```

However, if a variadic (`Vararg`) tuple type has free variables it can describe different kinds
of tuples:

```jldoctest
julia> typeintersect(Tuple{Vararg{T} where T}, Tuple{Int,Float64})
Tuple{Int64,Float64}

julia> typeintersect(Tuple{Vararg{T}} where T, Tuple{Int,Float64})
Union{}
```

Notice that when `T` is free with respect to the `Tuple` type (i.e. its binding `UnionAll`
type is outside the `Tuple` type), only one `T` value must work over the whole type.
Therefore a heterogeneous tuple does not match.

Finally, it's worth noting that `Tuple{}` is distinct:

```jldoctest
julia> Tuple{}
Tuple{}

julia> Tuple{}.parameters
svec()

julia> typeintersect(Tuple{}, Tuple{Int})
Union{}
```

What is the "primary" tuple-type?

```julia
julia> pointer_from_objref(Tuple)
Ptr{Void} @0x00007f5998a04370

julia> pointer_from_objref(Tuple{})
Ptr{Void} @0x00007f5998a570d0

julia> pointer_from_objref(Tuple.name.wrapper)
Ptr{Void} @0x00007f5998a04370

julia> pointer_from_objref(Tuple{}.name.wrapper)
Ptr{Void} @0x00007f5998a04370
```

so `Tuple == Tuple{Vararg{Any}}` is indeed the primary type.

## Introduction to the internal machinery: `jltypes.c`

Most operations for dealing with types are found in the files `jltypes.c` and `subtype.c`.
A good way to start is to watch subtyping in action.
Build Julia with `make debug` and fire up Julia within a debugger.
[gdb debugging tips](@ref) has some tips which may be useful.

Because the subtyping code is used heavily in the REPL itself--and hence breakpoints in this
code get triggered often--it will be easiest if you make the following definition:

```julia
julia> function mysubtype(a,b)
           ccall(:jl_breakpoint, Void, (Any,), nothing)
           issubtype(a, b)
       end
```

and then set a breakpoint in `jl_breakpoint`.  Once this breakpoint gets triggered, you can set
breakpoints in other functions.

As a warm-up, try the following:

```julia
mysubtype(Tuple{Int,Float64}, Tuple{Integer,Real})
```

We can make it more interesting by trying a more complex case:

```julia
mysubtype(Tuple{Array{Int,2}, Int8}, Tuple{Array{T}, T} where T)
```

## Subtyping and method sorting

The `type_morespecific` functions are used for imposing a partial order on functions in method
tables (from most-to-least specific). Note that `jl_type_morespecific(a,b)` really means "is `a`
at least as specific as `b`?" and not "is `a` strictly more specific than `b`?"

If `a` is a subtype of `b`, then it is automatically considered more specific.
From there, `type_morespecific` employs some less formal rules.
For example, `subtype` is sensitive to the number of arguments, but `type_morespecific` may not be.
In particular, `Tuple{Int,AbstractFloat}` is more specific than `Tuple{Integer}`, even though it is
not a subtype.  (Of `Tuple{Int,AbstractFloat}` and `Tuple{Integer,Float64}`, neither is more specific
than the other.)  Likewise, `Tuple{Int,Vararg{Int}}` is not a subtype of `Tuple{Integer}`, but it is
considered more specific. However, `morespecific` does get a bonus for length: in particular,
`Tuple{Int,Int}` is more specific than `Tuple{Int,Vararg{Int}}`.

If you're debugging how methods get sorted, it can be convenient to define the function:

```julia
type_morespecific(a, b) = ccall(:jl_type_morespecific, Cint, (Any,Any), a, b)
```

which allows you to test whether tuple type `a` is more specific than tuple type `b`.

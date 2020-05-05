# More about types

If you've used Julia for a while, you understand the fundamental role that types play.  Here we
try to get under the hood, focusing particularly on [Parametric Types](@ref).

## Types and sets (and `Any` and `Union{}`/`Bottom`)

It's perhaps easiest to conceive of Julia's type system in terms of sets. While programs manipulate
individual values, a type refers to a set of values. This is not the same thing as a collection;
for example a [`Set`](@ref) of values is itself a single `Set` value.
Rather, a type describes a set of *possible* values, expressing uncertainty about which value we
have.

A *concrete* type `T` describes the set of values whose direct tag, as returned by the [`typeof`](@ref)
function, is `T`. An *abstract* type describes some possibly-larger set of values.

[`Any`](@ref) describes the entire universe of possible values. [`Integer`](@ref) is a subset of
`Any` that includes `Int`, [`Int8`](@ref), and other concrete types.
Internally, Julia also makes heavy use of another type known as `Bottom`, which can also be written
as `Union{}`. This corresponds to the empty set.

Julia's types support the standard operations of set theory: you can ask whether `T1` is a "subset"
(subtype) of `T2` with `T1 <: T2`. Likewise, you intersect two types using [`typeintersect`](@ref), take
their union with [`Union`](@ref), and compute a type that contains their union with [`typejoin`](@ref):

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
Union{UInt8, Signed}

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

For example, [`Array`](@ref) has two parameters as in `Array{Int,2}`. If we did not know the element
type, we could write `Array{T,2} where T`, which is the union of `Array{T,2}` for all values of
`T`: `Union{Array{Int8,2}, Array{Int16,2}, ...}`.

Such a type is represented by a `UnionAll` object, which contains a variable (`T` in this example,
of type `TypeVar`), and a wrapped type (`Array{T,2}` in this example).

Consider the following methods:

```julia
f1(A::Array) = 1
f2(A::Array{Int}) = 2
f3(A::Array{T}) where {T<:Any} = 3
f4(A::Array{Any}) = 4
```

The signature - as described in [Function calls](@ref) - of `f3` is a `UnionAll` type wrapping a tuple type: `Tuple{typeof(f3), Array{T}} where T`.
All but `f4` can be called with `a = [1,2]`; all but `f2` can be called with `b = Any[1,2]`.

Let's look at these types a little more closely:

```jldoctest
julia> dump(Array)
UnionAll
  var: TypeVar
    name: Symbol T
    lb: Union{}
    ub: Any
  body: UnionAll
    var: TypeVar
      name: Symbol N
      lb: Union{}
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

```julia-repl
julia> dump(Array{Int,1}.name)
TypeName
  name: Symbol Array
  module: Module Core
  names: empty SimpleVector
  wrapper: UnionAll
    var: TypeVar
      name: Symbol T
      lb: Union{}
      ub: Any
    body: UnionAll
      var: TypeVar
        name: Symbol N
        lb: Union{}
        ub: Any
      body: Array{T,N} <: DenseArray{T,N}
  cache: SimpleVector
    ...

  linearcache: SimpleVector
    ...

  hash: Int64 -7900426068641098781
  mt: MethodTable
    name: Symbol Array
    defs: Nothing nothing
    cache: Nothing nothing
    max_args: Int64 0
    kwsorter: #undef
    module: Module Core
    : Int64 0
    : Int64 0
```

In this case, the relevant field is `wrapper`, which holds a reference to the top-level type used
to make new `Array` types.

```julia-repl
julia> pointer_from_objref(Array)
Ptr{Cvoid} @0x00007fcc7de64850

julia> pointer_from_objref(Array.body.body.name.wrapper)
Ptr{Cvoid} @0x00007fcc7de64850

julia> pointer_from_objref(Array{TV,NV})
Ptr{Cvoid} @0x00007fcc80c4d930

julia> pointer_from_objref(Array{TV,NV}.name.wrapper)
Ptr{Cvoid} @0x00007fcc7de64850
```

The `wrapper` field of [`Array`](@ref) points to itself, but for `Array{TV,NV}` it points back
to the original definition of the type.

What about the other fields? `hash` assigns an integer to each type.  To examine the `cache`
field, it's helpful to pick a type that is less heavily used than Array. Let's first create our
own type:

```jldoctest
julia> struct MyType{T,N} end

julia> MyType{Int,2}
MyType{Int64,2}

julia> MyType{Float32, 5}
MyType{Float32,5}
```

When you instantiate a parametric type, each concrete type gets saved in a type
cache (`MyType.body.body.name.cache`). However, instances containing free type
variables are not cached.

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

```julia-repl
julia> pointer_from_objref(Tuple)
Ptr{Cvoid} @0x00007f5998a04370

julia> pointer_from_objref(Tuple{})
Ptr{Cvoid} @0x00007f5998a570d0

julia> pointer_from_objref(Tuple.name.wrapper)
Ptr{Cvoid} @0x00007f5998a04370

julia> pointer_from_objref(Tuple{}.name.wrapper)
Ptr{Cvoid} @0x00007f5998a04370
```

so `Tuple == Tuple{Vararg{Any}}` is indeed the primary type.

## Diagonal types

Consider the type `Tuple{T,T} where T`.
A method with this signature would look like:

```julia
f(x::T, y::T) where {T} = ...
```

According to the usual interpretation of a `UnionAll` type, this `T` ranges over all
types, including `Any`, so this type should be equivalent to `Tuple{Any,Any}`.
However, this interpretation causes some practical problems.

First, a value of `T` needs to be available inside the method definition.
For a call like `f(1, 1.0)`, it's not clear what `T` should be.
It could be `Union{Int,Float64}`, or perhaps [`Real`](@ref).
Intuitively, we expect the declaration `x::T` to mean `T === typeof(x)`.
To make sure that invariant holds, we need `typeof(x) === typeof(y) === T` in this method.
That implies the method should only be called for arguments of the exact same type.

It turns out that being able to dispatch on whether two values have the same type
is very useful (this is used by the promotion system for example), so we have
multiple reasons to want a different interpretation of `Tuple{T,T} where T`.
To make this work we add the following rule to subtyping: if a variable occurs
more than once in covariant position, it is restricted to ranging over only concrete
types.
("Covariant position" means that only `Tuple` and `Union` types occur between an
occurrence of a variable and the `UnionAll` type that introduces it.)
Such variables are called "diagonal variables" or "concrete variables".

So for example, `Tuple{T,T} where T` can be seen as
`Union{Tuple{Int8,Int8}, Tuple{Int16,Int16}, ...}`, where `T` ranges over all
concrete types.
This gives rise to some interesting subtyping results.
For example `Tuple{Real,Real}` is not a subtype of `Tuple{T,T} where T`, because
it includes some types like `Tuple{Int8,Int16}` where the two elements have
different types.
`Tuple{Real,Real}` and `Tuple{T,T} where T` have the non-trivial intersection
`Tuple{T,T} where T<:Real`.
However, `Tuple{Real}` *is* a subtype of `Tuple{T} where T`, because in that case
`T` occurs only once and so is not diagonal.

Next consider a signature like the following:

```julia
f(a::Array{T}, x::T, y::T) where {T} = ...
```

In this case, `T` occurs in invariant position inside `Array{T}`.
That means whatever type of array is passed unambiguously determines
the value of `T` -- we say `T` has an *equality constraint* on it.
Therefore in this case the diagonal rule is not really necessary, since
the array determines `T` and we can then allow `x` and `y` to be of
any subtypes of `T`.
So variables that occur in invariant position are never considered diagonal.
This choice of behavior is slightly controversial --- some feel this definition
should be written as

```julia
f(a::Array{T}, x::S, y::S) where {T, S<:T} = ...
```

to clarify whether `x` and `y` need to have the same type.
In this version of the signature they would, or we could introduce a third variable for
the type of `y` if `x` and `y` can have different types.

The next complication is the interaction of unions and diagonal variables, e.g.

```julia
f(x::Union{Nothing,T}, y::T) where {T} = ...
```

Consider what this declaration means.
`y` has type `T`. `x` then can have either the same type `T`, or else be of type [`Nothing`](@ref).
So all of the following calls should match:

```julia
f(1, 1)
f("", "")
f(2.0, 2.0)
f(nothing, 1)
f(nothing, "")
f(nothing, 2.0)
```

These examples are telling us something: when `x` is `nothing::Nothing`, there are no
extra constraints on `y`.
It is as if the method signature had `y::Any`.
Indeed, we have the following type equivalence:

```julia
(Tuple{Union{Nothing,T},T} where T) == Union{Tuple{Nothing,Any}, Tuple{T,T} where T}
```

The general rule is: a concrete variable in covariant position acts like it's
not concrete if the subtyping algorithm only *uses* it once.
When `x` has type `Nothing`, we don't need to use the `T` in `Union{Nothing,T}`;
we only use it in the second slot.
This arises naturally from the observation that in `Tuple{T} where T` restricting
`T` to concrete types makes no difference; the type is equal to `Tuple{Any}` either way.

However, appearing in *invariant* position disqualifies a variable from being concrete
whether that appearance of the variable is used or not.
Otherwise types can behave differently depending on which other types
they are compared to, making subtyping not transitive. For example, consider

Tuple{Int,Int8,Vector{Integer}} <: Tuple{T,T,Vector{Union{Integer,T}}} where T

If the `T` inside the Union is ignored, then `T` is concrete and the answer is "false"
since the first two types aren't the same.
But consider instead

Tuple{Int,Int8,Vector{Any}} <: Tuple{T,T,Vector{Union{Integer,T}}} where T

Now we cannot ignore the `T` in the Union (we must have T == Any), so `T` is not
concrete and the answer is "true".
That would make the concreteness of `T` depend on the other type, which is not
acceptable since a type must have a clear meaning on its own.
Therefore the appearance of `T` inside `Vector` is considered in both cases.

## Subtyping diagonal variables

The subtyping algorithm for diagonal variables has two components:
(1) identifying variable occurrences, and (2) ensuring that diagonal
variables range over concrete types only.

The first task is accomplished by keeping counters `occurs_inv` and `occurs_cov`
(in `src/subtype.c`) for each variable in the environment, tracking the number
of invariant and covariant occurrences, respectively.
A variable is diagonal when `occurs_inv == 0 && occurs_cov > 1`.

The second task is accomplished by imposing a condition on a variable's lower bound.
As the subtyping algorithm runs, it narrows the bounds of each variable
(raising lower bounds and lowering upper bounds) to keep track of the
range of variable values for which the subtype relation would hold.
When we are done evaluating the body of a `UnionAll` type whose variable is diagonal,
we look at the final values of the bounds.
Since the variable must be concrete, a contradiction occurs if its lower bound
could not be a subtype of a concrete type.
For example, an abstract type like [`AbstractArray`](@ref) cannot be a subtype of a concrete
type, but a concrete type like `Int` can be, and the empty type `Bottom` can be as well.
If a lower bound fails this test the algorithm stops with the answer `false`.

For example, in the problem `Tuple{Int,String} <: Tuple{T,T} where T`, we derive that
this would be true if `T` were a supertype of `Union{Int,String}`.
However, `Union{Int,String}` is an abstract type, so the relation does not hold.

This concreteness test is done by the function `is_leaf_bound`.
Note that this test is slightly different from `jl_is_leaf_type`, since it also returns
`true` for `Bottom`.
Currently this function is heuristic, and does not catch all possible concrete types.
The difficulty is that whether a lower bound is concrete might depend on the values
of other type variable bounds.
For example, `Vector{T}` is equivalent to the concrete type `Vector{Int}` only if
both the upper and lower bounds of `T` equal `Int`.
We have not yet worked out a complete algorithm for this.

## Introduction to the internal machinery

Most operations for dealing with types are found in the files `jltypes.c` and `subtype.c`.
A good way to start is to watch subtyping in action.
Build Julia with `make debug` and fire up Julia within a debugger.
[gdb debugging tips](@ref) has some tips which may be useful.

Because the subtyping code is used heavily in the REPL itself--and hence breakpoints in this
code get triggered often--it will be easiest if you make the following definition:

```julia-repl
julia> function mysubtype(a,b)
           ccall(:jl_breakpoint, Cvoid, (Any,), nothing)
           a <: b
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
tables (from most-to-least specific). Specificity is strict; if `a` is more specific than `b`,
then `a` does not equal `b` and `b` is not more specific than `a`.

If `a` is a strict subtype of `b`, then it is automatically considered more specific.
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

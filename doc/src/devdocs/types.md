# More about types

If you've used Julia for a while, you understand the fundamental role that types play.  Here we
try to get under the hood, focusing particularly on [Parametric Types](@ref).

## Types and sets (and `Any` and `Union{}`/`Bottom`)

It's perhaps easiest to conceive of Julia's type system in terms of sets.  A concrete type corresponds
to a single entity in the space of all possible types; an abstract type refers to a collection
(set) of concrete types.  `Any` is a type that describes the entire universe of possible types;
`Integer` is a subset of `Any` that includes `Int`, `Int8`, and other concrete types.  Internally,
Julia also makes heavy use of another type known as `Bottom`, or equivalently, `Union{}`.  This
corresponds to the empty set.

Julia's types support the standard operations of set theory: you can ask whether `T1` is a "subset"
(subtype) of `T2` with `T1 <: T2`.  Likewise, you intersect two types using `typeintersect`, take
their union with `Union`, and compute a type that contains their union with `typejoin`:

```julia
julia> typeintersect(Int, Float64)
Union{}

julia> Union{Int, Float64}
Union{Float64,Int64}

julia> typejoin(Int, Float64)
Real

julia> typeintersect(Signed, Union{UInt8, Int8})
Int8

julia> Union{Signed, Union{UInt8, Int8}}
Union{Signed,UInt8}

julia> typejoin(Signed, Union{UInt8, Int8})
Integer

julia> typeintersect(Tuple{Integer,Float64}, Tuple{Int,Real})
Tuple{Int64,Float64}

julia> Union{Tuple{Integer,Float64}, Tuple{Int,Real}}
Union{Tuple{Int64,Real},Tuple{Integer,Float64}}

julia> typejoin(Tuple{Integer,Float64}, Tuple{Int,Real})
Tuple{Integer,Real}
```

While these operations may seem abstract, they lie at the heart of Julia.  For example, method
dispatch is implemented by stepping through the items in a method list until reaching one for
which `typeintersect(args, sig)` is not `Union{}`.  (Here, `args` is a tuple-type describing the
types of the arguments, and `sig` is a tuple-type specifying the types in the function's signature.)
 For this algorithm to work, it's important that methods be sorted by their specificity, and that
the search begins with the most specific methods.  Consequently, Julia also implements a partial
order on types; this is achieved by functionality that is similar to `<:`, but with differences
that will be discussed below.

## TypeVars

Many types take parameters; an easy example is [`Array`](@ref), which takes two parameters often
written as `Array{T,N}`.  Let's compare the following methods:

```julia
f1(A::Array) = 1
f2(A::Array{Int}) = 2
f3{T}(A::Array{T}) = 3
f4(A::Array{Any}) = 4
f5{T<:Any}(A::Array{T}) = 5
```

All but `f4` can be called with `a = [1,2]`; all but `f2` can be called with `b = Any[1,2]`.

Let's look at these types a little more closely:

```julia
julia> Array
Array{T,N}

julia> dump(Array)
Array{T,N} <: DenseArray{T,N}
```

This indicates that [`Array`](@ref) is a shorthand for `Array{T,N}`.  If you type this at the
REPL prompt--on its own, not while defining a function or type--you get an error `T not defined`.
So what, exactly, are `T` and `N`? You can learn more by extracting these parameters:

```julia
julia> T,N = Array.parameters
svec(T,N)

julia> dump(T)
TypeVar
  name: Symbol T
  lb: Union{}
  ub: Any
  bound: Bool false
```

A `TypeVar` is one of Julia's built-in types--it's defined in `jltypes.c`, although you can find
a commented-out version in `boot.jl`.  The `name` field is straightforward: it's what's printed
when showing the object.  `lb` and `ub` stand for "lower bound" and "upper bound," respectively:
these are the sets that constrain what types the TypeVar may represent.  In this case, `T`'s lower
bound is `Union{}` (i.e., `Bottom` or the empty set); in other words, this `TypeVar` is not constrained
from below.  The upper bound is `Any`, so neither is it constrained from above.

In a method definition like:

```julia
g{S<:Integer}(x::S) = 0
```

one can extract the underlying `TypeVar`:

```julia
g{S<:Integer}(x::S) = 0
m = first(methods(g))
p = m.sig.parameters
tv = p[2]
dump(tv)
```

```
TypeVar
  name: Symbol S
  lb: Union{}
  ub: Integer <: Real
  bound: Bool true
```

Here `ub` is `Integer`, as specified in the function definition.

The last field of a `TypeVar` is `bound`.  This boolean value specifies whether the `TypeVar`
is defined as one of the function parameters. For example:

```julia
julia> h1(A::Array, b::Real) = 1
h1 (generic function with 1 method)

julia> h2{T<:Real}(A::Array, b::T) = 1
h2 (generic function with 1 method)

julia> h3{T<:Real}(A::Array{T}, b::T) = 1
h3 (generic function with 1 method)

julia> p1 = first(methods(h1)).sig.parameters
svec(#h1,Array{T,N},Real)

julia> p2 = first(methods(h2)).sig.parameters
svec(#h2,Array{T,N},T<:Real)

julia> p3 = first(methods(h3)).sig.parameters
svec(#h3,Array{T<:Real,N},T<:Real)

julia> dump(p1[2].parameters[1])
TypeVar
  name: Symbol T
  lb: Union{}
  ub: Any
  bound: Bool false

julia> dump(p3[2].parameters[1])
TypeVar
  name: Symbol T
  lb: Union{}
  ub: Real <: Number
  bound: Bool true
```

Note that `p2` shows two objects called `T`, but only one of them has the upper bound `Real`;
in contrast, `p3` shows both of them bounded.  This is because in `h3`, the same type `T` is used
in both places, whereas for `h2` the `T` inside the array is simply the default symbol used for
the first parameter of [`Array`](@ref).

One can construct `TypeVar`s manually:

```julia
julia> TypeVar(:V, Signed, Real, false)
Signed<:V<:Real
```

There are convenience versions that allow you to omit any of these arguments except the `name`
symbol.

Armed with this information, we can do some sneaky things that reveal a lot about how Julia does
dispatch:

```julia
julia> TV = TypeVar(:T, false)   # bound = false
T

julia> candid{T}(A::Array{T}, x::T) = 0
candid (generic function with 1 method)

julia> @eval sneaky{T}(A::Array{T}, x::$TV) = 1
sneaky (generic function with 1 method)

julia> methods(candid)
# 1 method for generic function "candid":
candid{T}(A::Array{T,N<:Any}, x::T) in Main at none:1

julia> methods(sneaky)
# 1 method for generic function "sneaky":
sneaky{T}(A::Array{T,N<:Any}, x::T<:Any) in Main at none:1
```

These therefore print identically, but they have very different behavior:

```julia
julia> candid([1],3.2)
ERROR: MethodError: no method matching candid(::Array{Int64,1}, ::Float64)
Closest candidates are:
  candid{T}(::Array{T,N}, !Matched::T) at none:1
 ...

julia> sneaky([1],3.2)
1
```

To see what's happening, it's helpful to use Julia's internal `jl_()` function (defined in `builtins.c`)
for display, because it prints bound `TypeVar` objects with a hash (`#T` instead of `T`):

```julia
julia> jl_(x) = ccall(:jl_, Void, (Any,), x)
jl_ (generic function with 1 method)
```

```julia
julia> jl_(first(methods(candid)).sig)
Tuple{Main.#candid, Array{#T<:Any, N<:Any}, #T<:Any}

julia> jl_(first(methods(sneaky)).sig)
Tuple{Main.#sneaky, Array{#T<:Any, N<:Any}, T<:Any}
```

Even though both print as `T`, in `sneaky` the second `T` is not bound, and hence it isn't constrained
to be the same type as the element type of the [`Array`](@ref).

Some `TypeVar` interactions depend on the `bound` state, even when there are not two or more uses
of the same `TypeVar`. For example:

```julia
julia> S = TypeVar(:S, false); T = TypeVar(:T, true)
T

# These would be the same no matter whether we used S or T
julia> Array{Array{S}} <: Array{Array}
true

julia> Array{Array{S}} <: Array{Array{S}}
true

julia> Array{Array} <: Array{Array{S}}
true

# For these cases, it matters
julia> Array{Array{Int}} <: Array{Array}
false

julia> Array{Array{Int}} <: Array{Array{S}}
false

julia> Array{Array{Int}} <: Array{Array{T}}
true
```

It's this latter construction that allows function declarations like

```julia
foo{T,N}(A::Array{Array{T,N}}) = T,N
```

to match despite the invariance of Julia's type parameters.

## TypeNames

The following two [`Array`](@ref) types are functionally equivalent, yet print differently via
`jl_()`:

```julia
julia> TV, NV = TypeVar(:T), TypeVar(:N)
(T,N)

julia> jl_(Array)
Array

julia> jl_(Array{TV,NV})
Array{T<:Any, N<:Any}
```

These can be distinguished by examining the `name` field of the type, which is an object of type
`TypeName`:

```julia
julia> dump(Array.name)
TypeName
  name: Symbol Array
  module: Module Core
  names: empty SimpleVector
  primary: Array{T,N} <: DenseArray{T,N}
  cache: SimpleVector
    ...

  linearcache: SimpleVector
    ...

  uid: Int64 -7900426068641098781
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

In this case, the relevant field is `primary`, which holds a reference to the "primary" instance
of the type:

```julia
julia> pointer_from_objref(Array)
Ptr{Void} @0x00007fcc7de64850

julia> pointer_from_objref(Array.name.primary)
Ptr{Void} @0x00007fcc7de64850

julia> pointer_from_objref(Array{TV,NV})
Ptr{Void} @0x00007fcc80c4d930

julia> pointer_from_objref(Array{TV,NV}.name.primary)
Ptr{Void} @0x00007fcc7de64850
```

The `primary` field of [`Array`](@ref) points to itself, but for `Array{TV,NV}` it points back
to the default definition of the type.

What about the other fields? `uid` assigns a unique integer to each type.  To examine the `cache`
field, it's helpful to pick a type that is less heavily used than Array. Let's first create our
own type:

```julia
julia> type MyType{T,N} end

julia> MyType{Int,2}
MyType{Int64,2}

julia> MyType{Float32, 5}
MyType{Float32,5}

julia> MyType.name.cache
svec(MyType{Float32,5},MyType{Int64,2},#undef,#undef,#undef,#undef,#undef,#undef)
```

(The cache is pre-allocated to have length 8, but only the first two entries are populated.) Consequently,
when you instantiate a parametric type, each concrete type gets saved in a type-cache.  However,
instances with `TypeVar` parameters are not cached.

## Tuple-types

Tuple-types constitute an interesting special case.  For dispatch to work on declarations like
`x::Tuple`, the type has to be able to be able to accommodate any tuple.  Let's check the parameters:

```julia
julia> Tuple
Tuple

julia> Tuple.parameters
svec(Vararg{Any,N})
```

It's worth noting that the parameter is a type, `Any`, rather than a `TypeVar T<:Any`: compare

```julia
julia> jl_(Tuple.parameters)
svec(Vararg{Any, N<:Any})

julia> jl_(Array.parameters)
svec(T<:Any, N<:Any)
```

Unlike other types, tuple-types are covariant in their parameters, so this definition permits
`Tuple` to match any type of tuple.  This is therefore equivalent to having an unbound `TypeVar`
but distinct from a bound `TypeVar`

```julia
julia> typeintersect(Tuple, Tuple{Int,Float64})
Tuple{Int64,Float64}

julia> typeintersect(Tuple{Vararg{Any}}, Tuple{Int,Float64})
Tuple{Int64,Float64}

julia> T = TypeVar(:T,false)
T

julia> typeintersect(Tuple{Vararg{T}}, Tuple{Int,Float64})
Tuple{Int64,Float64}

julia> T = TypeVar(:T,true)
T

julia> typeintersect(Tuple{Vararg{T}}, Tuple{Int,Float64})
Union{}
```

Finally, it's worth noting that `Tuple{}` is distinct

```julia
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

julia> pointer_from_objref(Tuple.name.primary)
Ptr{Void} @0x00007f5998a04370

julia> pointer_from_objref(Tuple{}.name.primary)
Ptr{Void} @0x00007f5998a04370
```

so `Tuple == Tuple{Vararg{Any}}` is indeed the primary type.

## Introduction to the internal machinery: `jltypes.c`

Many operations for dealing with types are found in the file `jltypes.c`. A good way to start
is to watch type intersection in action.  Build Julia with `make debug` and fire up Julia within
a debugger. [gdb debugging tips](@ref) has some tips which may be useful.

Because the type intersection and matching code is used heavily in the REPL itself--and hence
breakpoints in this code get triggered often--it will be easiest if you make the following definition:

```julia
julia> function myintersect(a,b)
           ccall(:jl_breakpoint, Void, (Any,), nothing)
           typeintersect(a, b)
       end
```

and then set a breakpoint in `jl_breakpoint`.  Once this breakpoint gets triggered, you can set
breakpoints in other functions.

As a warm-up, try the following:

```julia
myintersect(Tuple{Integer,Float64}, Tuple{Int,Real})
```

Set a breakpoint in `intersect_tuple` and continue until it enters this function.  You should
be able to see something like this:

```
Breakpoint 2, intersect_tuple (a=0x7ffdf7409150, b=0x7ffdf74091b0, penv=0x7fffffffcc90, eqc=0x7fffffffcc70, var=covariant) at jltypes.c:405
405     {
(gdb) call jl_(a)
Tuple{Integer, Float64}
(gdb) call jl_(b)
Tuple{Int64, Real}
```

The `var` argument is either `covariant` or `invariant`, the latter being used if you're matching
the type parameters of `Array{T1}` against `Array{T2}`.  The other two inputs to this function
(`penv` and `eqc`) may be currently mysterious, but we'll discuss them in a moment.  For now,
step through the code until you get into the loop over the different entries in the tuple types
`a` and `b`.  The key call is:

```julia
ce = jl_type_intersect(ae,be,penv,eqc,var);
```

which, if you examine `ae`, `be`, and `ce`, you'll see is just type intersection performed on
these entries.

We can make it more interesting by trying a more complex case:

```julia
julia> T = TypeVar(:T, true)
T

julia> myintersect(Tuple{Array{T}, T}, Tuple{Array{Int,2}, Int8})

Breakpoint 1, jl_breakpoint (v=0x7ffdf35e8010) at builtins.c:1559
1559    {
(gdb) b intersect_tuple
Breakpoint 3 at 0x7ffff6dcb07d: file jltypes.c, line 405.
(gdb) c
Continuing.

Breakpoint 3, intersect_tuple (a=0x7ffdf74d7a90, b=0x7ffdf74d7af0, penv=0x7fffffffcc90, eqc=0x7fffffffcc70, var=covariant) at jltypes.c:405
405     {
(gdb) call jl_(a)
Tuple{Array{＃T<:Any, N<:Any}, ＃T<:Any}
(gdb) call jl_(b)
Tuple{Array{Int64, 2}, Int8}
```

Let's watch how this bound `TypeVar` gets handled.  To follow this, you'll need to examine the
variables `penv` and `eqc`, which are defined as:

```
typedef struct {
    jl_value_t **data;
    size_t n;
    jl_svec_t *tvars;
} cenv_t;
```

These start out empty (with `penv->n == eqc->n == 0`).  Once we get into the loop and make the
first call to `jl_type_intersect`, `eqc` (which stands for "equality constraints") has the following
value:

```
(gdb) p eqc->n
$4 = 2
(gdb) call jl_(eqc->data[0])
＃T<:Any
(gdb) call jl_(eqc->data[1])
Int64
```

This is just a `var`, `value` list of pairs, indicating that `T` now has the value `Int64`.  If
you now allow `intersect_tuple` to finish and keep progressing, you'll eventually get to `type_intersection_matching`.
 This function contains a call to `solve_tvar_constraints`.  Roughly speaking, `eqc` defines
`T = Int64`, but `env` defines it as `Int8`; this conflict is detected in `solve_tvar_constraints`
and the resulting return is `jl_bottom_type`, aka `Union{}`.

## Subtyping and method sorting

Armed with this knowledge, you may find yourself surprised by the following:

```julia
julia> typeintersect(Tuple{Array{Int},Float64}, Tuple{Array{T},T})
Union{}

julia> Tuple{Array{Int},Float64} <: Tuple{Array{T},T}
true
```

where `T` is a bound `TypeVar`.  In other words, `A <: B` does not imply that `typeintersect(A, B) == A`.
 A little bit of digging reveals the reason why: `jl_subtype_le` does not use the `cenv_t` constraints
that we just saw in `typeintersect`.

`jltypes.c` contains three closely related collections of functions for testing how types `a`
and `b` are ordered:

  * The `subtype` functions implement `a <: b`. Among other uses, they serve in matching function
    arguments against method signatures in the function cache.
  * The `type_morespecific` functions are used for imposing a partial order on functions in method
    tables (from most-to-least specific). Note that `jl_type_morespecific(a,b,0)` really means "is
    `a` at least as specific as `b`?" and not "is `a` strictly more specific than `b`?"
  * The `type_match` functions are similar to `type_morespecific`, but additionally accept (and employ)
    an environment to constrain typevars. The related `type_match_morespecific` functions call `type_match`
    with an argument `morespecific=1`

All three of these take an argument, `invariant`, which is set to 1 when comparing type parameters
and otherwise is 0.

The rules for these are somewhat different. `subtype` is sensitive to the number arguments, but
`type_morespecific` may not be. In particular, `Tuple{Int,AbstractFloat}` is more specific than
`Tuple{Integer}`, even though it is not a subtype.  (Of `Tuple{Int,AbstractFloat}` and `Tuple{Integer,Float64}`,
neither is more specific than the other.)  Likewise, `Tuple{Int,Vararg{Int}}` is not a subtype
of `Tuple{Integer}`, but it is considered more specific. However, `morespecific` does get a bonus
for length: in particular, `Tuple{Int,Int}` is more specific than `Tuple{Int,Vararg{Int}}`.

If you're debugging how methods get sorted, it can be convenient to define the function:

```julia
args_morespecific(a, b) = ccall(:jl_args_morespecific, Cint, (Any,Any), a, b)
```

which allows you to test whether arg-tuple `a` is more specific than arg-tuple `b`.

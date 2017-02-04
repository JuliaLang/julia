# [Types](@id man-types)

Type systems have traditionally fallen into two quite different camps: static type systems, where
every program expression must have a type computable before the execution of the program, and
dynamic type systems, where nothing is known about types until run time, when the actual values
manipulated by the program are available. Object orientation allows some flexibility in statically
typed languages by letting code be written without the precise types of values being known at
compile time. The ability to write code that can operate on different types is called polymorphism.
All code in classic dynamically typed languages is polymorphic: only by explicitly checking types,
or when objects fail to support operations at run-time, are the types of any values ever restricted.

Julia's type system is dynamic, but gains some of the advantages of static type systems by making
it possible to indicate that certain values are of specific types. This can be of great assistance
in generating efficient code, but even more significantly, it allows method dispatch on the types
of function arguments to be deeply integrated with the language. Method dispatch is explored in
detail in [Methods](@ref), but is rooted in the type system presented here.

The default behavior in Julia when types are omitted is to allow values to be of any type. Thus,
one can write many useful Julia programs without ever explicitly using types. When additional
expressiveness is needed, however, it is easy to gradually introduce explicit type annotations
into previously "untyped" code. Doing so will typically increase both the performance and robustness
of these systems, and perhaps somewhat counterintuitively, often significantly simplify them.

Describing Julia in the lingo of [type systems](https://en.wikipedia.org/wiki/Type_system), it
is: dynamic, nominative and parametric. Generic types can be parameterized, and the hierarchical
relationships between types are [explicitly declared](https://en.wikipedia.org/wiki/Nominal_type_system),
rather than [implied by compatible structure](https://en.wikipedia.org/wiki/Structural_type_system).
One particularly distinctive feature of Julia's type system is that concrete types may not subtype
each other: all concrete types are final and may only have abstract types as their supertypes.
While this might at first seem unduly restrictive, it has many beneficial consequences with surprisingly
few drawbacks. It turns out that being able to inherit behavior is much more important than being
able to inherit structure, and inheriting both causes significant difficulties in traditional
object-oriented languages. Other high-level aspects of Julia's type system that should be mentioned
up front are:

  * There is no division between object and non-object values: all values in Julia are true objects
    having a type that belongs to a single, fully connected type graph, all nodes of which are equally
    first-class as types.
  * There is no meaningful concept of a "compile-time type": the only type a value has is its actual
    type when the program is running. This is called a "run-time type" in object-oriented languages
    where the combination of static compilation with polymorphism makes this distinction significant.
  * Only values, not variables, have types -- variables are simply names bound to values.
  * Both abstract and concrete types can be parameterized by other types. They can also be parameterized
    by symbols, by values of any type for which [`isbits()`](@ref) returns true (essentially, things
    like numbers and bools that are stored like C types or structs with no pointers to other objects),
    and also by tuples thereof. Type parameters may be omitted when they do not need to be referenced
    or restricted.

Julia's type system is designed to be powerful and expressive, yet clear, intuitive and unobtrusive.
Many Julia programmers may never feel the need to write code that explicitly uses types. Some
kinds of programming, however, become clearer, simpler, faster and more robust with declared types.

## Type Declarations

The `::` operator can be used to attach type annotations to expressions and variables in programs.
There are two primary reasons to do this:

1. As an assertion to help confirm that your program works the way you expect,
2. To provide extra type information to the compiler, which can then improve performance in some
   cases

When appended to an expression computing a value, the `::` operator is read as "is an instance
of". It can be used anywhere to assert that the value of the expression on the left is an instance
of the type on the right. When the type on the right is concrete, the value on the left must have
that type as its implementation -- recall that all concrete types are final, so no implementation
is a subtype of any other. When the type is abstract, it suffices for the value to be implemented
by a concrete type that is a subtype of the abstract type. If the type assertion is not true,
an exception is thrown, otherwise, the left-hand value is returned:

```jldoctest
julia> (1+2)::AbstractFloat
ERROR: TypeError: typeassert: expected AbstractFloat, got Int64

julia> (1+2)::Int
3
```

This allows a type assertion to be attached to any expression in-place.

When appended to a variable on the left-hand side of an assignment, or as part of a `local` declaration,
the `::` operator means something a bit different: it declares the variable to always have the
specified type, like a type declaration in a statically-typed language such as C. Every value
assigned to the variable will be converted to the declared type using [`convert()`](@ref):

```jldoctest
julia> function foo()
           x::Int8 = 100
           x
       end
foo (generic function with 1 method)

julia> foo()
100

julia> typeof(ans)
Int8
```

This feature is useful for avoiding performance "gotchas" that could occur if one of the assignments
to a variable changed its type unexpectedly.

This "declaration" behavior only occurs in specific contexts:

```julia
local x::Int8  # in a local declaration
x::Int8 = 10   # as the left-hand side of an assignment
```

and applies to the whole current scope, even before the declaration. Currently, type declarations
cannot be used in global scope, e.g. in the REPL, since Julia does not yet have constant-type
globals.

Declarations can also be attached to function definitions:

```julia
function sinc(x)::Float64
    if x == 0
        return 1
    end
    return sin(pi*x)/(pi*x)
end
```

Returning from this function behaves just like an assignment to a variable with a declared type:
the value is always converted to `Float64`.

## Abstract Types

Abstract types cannot be instantiated, and serve only as nodes in the type graph, thereby describing
sets of related concrete types: those concrete types which are their descendants. We begin with
abstract types even though they have no instantiation because they are the backbone of the type
system: they form the conceptual hierarchy which makes Julia's type system more than just a collection
of object implementations.

Recall that in [Integers and Floating-Point Numbers](@ref), we introduced a variety of concrete
types of numeric values: `Int8`, `UInt8`, `Int16`, `UInt16`, `Int32`, `UInt32`, `Int64`, `UInt64`,
`Int128`, `UInt128`, `Float16`, [`Float32`](@ref), and [`Float64`](@ref).  Although they have
different representation sizes, `Int8`, `Int16`, `Int32`, `Int64`  and `Int128` all have in common
that they are signed integer types. Likewise `UInt8`, `UInt16`, `UInt32`, `UInt64` and `UInt128`
are all unsigned integer types, while `Float16`, [`Float32`](@ref) and [`Float64`](@ref) are distinct
in being floating-point types rather than integers. It is common for a piece of code to make sense,
for example, only if its arguments are some kind of integer, but not really depend on what particular
*kind* of integer.  For example, the greatest common denominator algorithm works for all kinds
of integers, but will not work for floating-point numbers.  Abstract types allow the construction
of a hierarchy of types, providing a context into which concrete types can fit.  This allows you,
for example, to easily program to any type that is an integer, without restricting an algorithm
to a specific type of integer.

Abstract types are declared using the `abstract` keyword. The general syntaxes for declaring an
abstract type are:

```
abstract «name»
abstract «name» <: «supertype»
```

The `abstract` keyword introduces a new abstract type, whose name is given by `«name»`. This
name can be optionally followed by `<:` and an already-existing type, indicating that the newly
declared abstract type is a subtype of this "parent" type.

When no supertype is given, the default supertype is `Any` -- a predefined abstract type that
all objects are instances of and all types are subtypes of. In type theory, `Any` is commonly
called "top" because it is at the apex of the type graph. Julia also has a predefined abstract
"bottom" type, at the nadir of the type graph, which is written as `Union{}`. It is the exact
opposite of `Any`: no object is an instance of `Union{}` and all types are supertypes of `Union{}`.

Let's consider some of the abstract types that make up Julia's numerical hierarchy:

```julia
abstract Number
abstract Real     <: Number
abstract AbstractFloat <: Real
abstract Integer  <: Real
abstract Signed   <: Integer
abstract Unsigned <: Integer
```

The `Number` type is a direct child type of `Any`, and `Real` is its child. In turn, `Real` has
two children (it has more, but only two are shown here; we'll get to the others later): `Integer`
and `AbstractFloat`, separating the world into representations of integers and representations
of real numbers. Representations of real numbers include, of course, floating-point types, but
also include other types, such as rationals. Hence, `AbstractFloat` is a proper subtype of `Real`,
including only floating-point representations of real numbers. Integers are further subdivided
into `Signed` and `Unsigned` varieties.

The `<:` operator in general means "is a subtype of", and, used in declarations like this, declares
the right-hand type to be an immediate supertype of the newly declared type. It can also be used
in expressions as a subtype operator which returns `true` when its left operand is a subtype of
its right operand:

```jldoctest
julia> Integer <: Number
true

julia> Integer <: AbstractFloat
false
```

An important use of abstract types is to provide default implementations for concrete types. To
give a simple example, consider:

```julia
function myplus(x,y)
    x+y
end
```

The first thing to note is that the above argument declarations are equivalent to `x::Any` and
`y::Any`. When this function is invoked, say as `myplus(2,5)`, the dispatcher chooses the most
specific method named `myplus` that matches the given arguments. (See [Methods](@ref) for more
information on multiple dispatch.)

Assuming no method more specific than the above is found, Julia next internally defines and compiles
a method called `myplus` specifically for two `Int` arguments based on the generic function given
above, i.e., it implicitly defines and compiles:

```julia
function myplus(x::Int,y::Int)
    x+y
end
```

and finally, it invokes this specific method.

Thus, abstract types allow programmers to write generic functions that can later be used as the
default method by many combinations of concrete types. Thanks to multiple dispatch, the programmer
has full control over whether the default or more specific method is used.

An important point to note is that there is no loss in performance if the programmer relies on
a function whose arguments are abstract types, because it is recompiled for each tuple of argument
concrete types with which it is invoked. (There may be a performance issue, however, in the case
of function arguments that are containers of abstract types; see [Performance Tips](@ref man-performance-tips).)

## Bits Types

A bits type is a concrete type whose data consists of plain old bits. Classic examples of bits
types are integers and floating-point values. Unlike most languages, Julia lets you declare your
own bits types, rather than providing only a fixed set of built-in bits types. In fact, the standard
bits types are all defined in the language itself:

```julia
bitstype 16 Float16 <: AbstractFloat
bitstype 32 Float32 <: AbstractFloat
bitstype 64 Float64 <: AbstractFloat

bitstype 8  Bool <: Integer
bitstype 32 Char

bitstype 8  Int8     <: Signed
bitstype 8  UInt8    <: Unsigned
bitstype 16 Int16    <: Signed
bitstype 16 UInt16   <: Unsigned
bitstype 32 Int32    <: Signed
bitstype 32 UInt32   <: Unsigned
bitstype 64 Int64    <: Signed
bitstype 64 UInt64   <: Unsigned
bitstype 128 Int128  <: Signed
bitstype 128 UInt128 <: Unsigned
```

The general syntaxes for declaration of a `bitstype` are:

```
bitstype «bits» «name»
bitstype «bits» «name» <: «supertype»
```

The number of bits indicates how much storage the type requires and the name gives the new type
a name. A bits type can optionally be declared to be a subtype of some supertype. If a supertype
is omitted, then the type defaults to having `Any` as its immediate supertype. The declaration
of `Bool` above therefore means that a boolean value takes eight bits to store, and has `Integer`
as its immediate supertype. Currently, only sizes that are multiples of 8 bits are supported.
Therefore, boolean values, although they really need just a single bit, cannot be declared to
be any smaller than eight bits.

The types `Bool`, `Int8` and `UInt8` all have identical representations: they are eight-bit chunks
of memory. Since Julia's type system is nominative, however, they are not interchangeable despite
having identical structure. A fundamental difference between them is that they have different
supertypes: `Bool`'s direct supertype is `Integer`, `Int8`'s is `Signed`, and `UInt8`'s is `Unsigned`.
All other differences between `Bool`, `Int8`, and `UInt8` are matters of behavior -- the way functions
are defined to act when given objects of these types as arguments. This is why a nominative type
system is necessary: if structure determined type, which in turn dictates behavior, then it would
be impossible to make `Bool` behave any differently than `Int8` or `UInt8`.

## Composite Types

[Composite types](https://en.wikipedia.org/wiki/Composite_data_type) are called records, structures
(`struct`s in C), or objects in various languages. A composite type is a collection of named fields,
an instance of which can be treated as a single value. In many languages, composite types are
the only kind of user-definable type, and they are by far the most commonly used user-defined
type in Julia as well.

In mainstream object oriented languages, such as C++, Java, Python and Ruby, composite types also
have named functions associated with them, and the combination is called an "object". In purer
object-oriented languages, such as Ruby or Smalltalk, all values are objects whether they are
composites or not. In less pure object oriented languages, including C++ and Java, some values,
such as integers and floating-point values, are not objects, while instances of user-defined composite
types are true objects with associated methods. In Julia, all values are objects, but functions
are not bundled with the objects they operate on. This is necessary since Julia chooses which
method of a function to use by multiple dispatch, meaning that the types of *all* of a function's
arguments are considered when selecting a method, rather than just the first one (see [Methods](@ref)
for more information on methods and dispatch). Thus, it would be inappropriate for functions to
"belong" to only their first argument. Organizing methods into function objects rather than having
named bags of methods "inside" each object ends up being a highly beneficial aspect of the language
design.

Since composite types are the most common form of user-defined concrete type, they are simply
introduced with the `type` keyword followed by a block of field names, optionally annotated with
types using the `::` operator:

```jldoctest footype
julia> type Foo
           bar
           baz::Int
           qux::Float64
       end
```

Fields with no type annotation default to `Any`, and can accordingly hold any type of value.

New objects of composite type `Foo` are created by applying the `Foo` type object like a function
to values for its fields:

```jldoctest footype
julia> foo = Foo("Hello, world.", 23, 1.5)
Foo("Hello, world.", 23, 1.5)

julia> typeof(foo)
Foo
```

When a type is applied like a function it is called a *constructor*. Two constructors are generated
automatically (these are called *default constructors*). One accepts any arguments and calls
[`convert()`](@ref) to convert them to the types of the fields, and the other accepts arguments
that match the field types exactly. The reason both of these are generated is that this makes
it easier to add new definitions without inadvertently replacing a default constructor.

Since the `bar` field is unconstrained in type, any value will do. However, the value for `baz`
must be convertible to `Int`:

```jldoctest footype
julia> Foo((), 23.5, 1)
ERROR: InexactError()
Stacktrace:
 [1] convert(::Type{Int64}, ::Float64) at ./float.jl:675
 [2] Foo(::Tuple{}, ::Float64, ::Int64) at ./none:2
```

You may find a list of field names using the `fieldnames` function.

```jldoctest footype
julia> fieldnames(foo)
3-element Array{Symbol,1}:
 :bar
 :baz
 :qux
```

You can access the field values of a composite object using the traditional `foo.bar` notation:

```jldoctest footype
julia> foo.bar
"Hello, world."

julia> foo.baz
23

julia> foo.qux
1.5
```

You can also change the values as one would expect:

```jldoctest footype
julia> foo.qux = 2
2

julia> foo.bar = 1//2
1//2
```

Composite types with no fields are singletons; there can be only one instance of such types:

```jldoctest
julia> type NoFields
       end

julia> NoFields() === NoFields()
true
```

The `===` function confirms that the "two" constructed instances of `NoFields` are actually one
and the same. Singleton types are described in further detail [below](@ref man-singleton-types).

There is much more to say about how instances of composite types are created, but that discussion
depends on both [Parametric Types](@ref) and on [Methods](@ref), and is sufficiently important
to be addressed in its own section: [Constructors](@ref man-constructors).

## Immutable Composite Types

It is also possible to define *immutable* composite types by using the keyword `immutable` instead
of `type`:

```julia
immutable Complex
    real::Float64
    imag::Float64
end
```

Such types behave much like other composite types, except that instances of them cannot be modified.
Immutable types have several advantages:

  * They are more efficient in some cases. Types like the `Complex` example above can be packed efficiently
    into arrays, and in some cases the compiler is able to avoid allocating immutable objects entirely.
  * It is not possible to violate the invariants provided by the type's constructors.
  * Code using immutable objects can be easier to reason about.

An immutable object might contain mutable objects, such as arrays, as fields. Those contained
objects will remain mutable; only the fields of the immutable object itself cannot be changed
to point to different objects.

A useful way to think about immutable composites is that each instance is associated with specific
field values -- the field values alone tell you everything about the object. In contrast, a mutable
object is like a little container that might hold different values over time, and so is not identified
with specific field values. In deciding whether to make a type immutable, ask whether two instances
with the same field values would be considered identical, or if they might need to change independently
over time. If they would be considered identical, the type should probably be immutable.

To recap, two essential properties define immutability in Julia:

  * An object with an immutable type is passed around (both in assignment statements and in function
    calls) by copying, whereas a mutable type is passed around by reference.
  * It is not permitted to modify the fields of a composite immutable type.

It is instructive, particularly for readers whose background is C/C++, to consider why these two
properties go hand in hand.  If they were separated, i.e., if the fields of objects passed around
by copying could be modified, then it would become more difficult to reason about certain instances
of generic code.  For example, suppose `x` is a function argument of an abstract type, and suppose
that the function changes a field: `x.isprocessed = true`.  Depending on whether `x` is passed
by copying or by reference, this statement may or may not alter the actual argument in the calling
routine.  Julia sidesteps the possibility of creating functions with unknown effects in this scenario
by forbidding modification of fields of objects passed around by copying.

## Declared Types

The three kinds of types discussed in the previous three sections are actually all closely related.
They share the same key properties:

  * They are explicitly declared.
  * They have names.
  * They have explicitly declared supertypes.
  * They may have parameters.

Because of these shared properties, these types are internally represented as instances of the
same concept, `DataType`, which is the type of any of these types:

```jldoctest
julia> typeof(Real)
DataType

julia> typeof(Int)
DataType
```

A `DataType` may be abstract or concrete. If it is concrete, it has a specified size, storage
layout, and (optionally) field names. Thus a bits type is a `DataType` with nonzero size, but
no field names. A composite type is a `DataType` that has field names or is empty (zero size).

Every concrete value in the system is an instance of some `DataType`.

## Type Unions

A type union is a special abstract type which includes as objects all instances of any of its
argument types, constructed using the special `Union` function:

```jldoctest
julia> IntOrString = Union{Int,AbstractString}
Union{AbstractString, Int64}

julia> 1 :: IntOrString
1

julia> "Hello!" :: IntOrString
"Hello!"

julia> 1.0 :: IntOrString
ERROR: TypeError: typeassert: expected Union{AbstractString, Int64}, got Float64
```

The compilers for many languages have an internal union construct for reasoning about types; Julia
simply exposes it to the programmer.

## Parametric Types

An important and powerful feature of Julia's type system is that it is parametric: types can take
parameters, so that type declarations actually introduce a whole family of new types -- one for
each possible combination of parameter values. There are many languages that support some version
of [generic programming](https://en.wikipedia.org/wiki/Generic_programming), wherein data structures
and algorithms to manipulate them may be specified without specifying the exact types involved.
For example, some form of generic programming exists in ML, Haskell, Ada, Eiffel, C++, Java, C#,
F#, and Scala, just to name a few. Some of these languages support true parametric polymorphism
(e.g. ML, Haskell, Scala), while others support ad-hoc, template-based styles of generic programming
(e.g. C++, Java). With so many different varieties of generic programming and parametric types
in various languages, we won't even attempt to compare Julia's parametric types to other languages,
but will instead focus on explaining Julia's system in its own right. We will note, however, that
because Julia is a dynamically typed language and doesn't need to make all type decisions at compile
time, many traditional difficulties encountered in static parametric type systems can be relatively
easily handled.

All declared types (the `DataType` variety) can be parameterized, with the same syntax in each
case. We will discuss them in the following order: first, parametric composite types, then parametric
abstract types, and finally parametric bits types.

### Parametric Composite Types

Type parameters are introduced immediately after the type name, surrounded by curly braces:

```jldoctest pointtype
julia> type Point{T}
           x::T
           y::T
       end
```

This declaration defines a new parametric type, `Point{T}`, holding two "coordinates" of type
`T`. What, one may ask, is `T`? Well, that's precisely the point of parametric types: it can be
any type at all (or a value of any bits type, actually, although here it's clearly used as a type).
`Point{Float64}` is a concrete type equivalent to the type defined by replacing `T` in the definition
of `Point` with [`Float64`](@ref). Thus, this single declaration actually declares an unlimited
number of types: `Point{Float64}`, `Point{AbstractString}`, `Point{Int64}`, etc. Each of these
is now a usable concrete type:

```jldoctest pointtype
julia> Point{Float64}
Point{Float64}

julia> Point{AbstractString}
Point{AbstractString}
```

The type `Point{Float64}` is a point whose coordinates are 64-bit floating-point values, while
the type `Point{AbstractString}` is a "point" whose "coordinates" are string objects (see [Strings](@ref)).

`Point` itself is also a valid type object, containing all instances `Point{Float64}`, `Point{AbstractString}`,
etc. as subtypes:

```jldoctest pointtype
julia> Point{Float64} <: Point
true

julia> Point{AbstractString} <: Point
true
```

Other types, of course, are not subtypes of it:

```jldoctest pointtype
julia> Float64 <: Point
false

julia> AbstractString <: Point
false
```

Concrete `Point` types with different values of `T` are never subtypes of each other:

```jldoctest pointtype
julia> Point{Float64} <: Point{Int64}
false

julia> Point{Float64} <: Point{Real}
false
```

!!! warning
    This last point is *very* important: even though `Float64 <: Real` we **DO NOT** have `Point{Float64} <: Point{Real}`.

In other words, in the parlance of type theory, Julia's type parameters are *invariant*, rather
than being [covariant (or even contravariant)](https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29). This is for practical reasons: while any instance
of `Point{Float64}` may conceptually be like an instance of `Point{Real}` as well, the two types
have different representations in memory:

  * An instance of `Point{Float64}` can be represented compactly and efficiently as an immediate pair
    of 64-bit values;
  * An instance of `Point{Real}` must be able to hold any pair of instances of `Real`. Since objects
    that are instances of `Real` can be of arbitrary size and structure, in practice an instance of
    `Point{Real}` must be represented as a pair of pointers to individually allocated `Real` objects.

The efficiency gained by being able to store `Point{Float64}` objects with immediate values is
magnified enormously in the case of arrays: an `Array{Float64}` can be stored as a contiguous
memory block of 64-bit floating-point values, whereas an `Array{Real}` must be an array of pointers
to individually allocated `Real` objects -- which may well be [boxed](https://en.wikipedia.org/wiki/Object_type_%28object-oriented_programming%29#Boxing)
64-bit floating-point values, but also might be arbitrarily large, complex objects, which are
declared to be implementations of the `Real` abstract type.

Since `Point{Float64}` is not a subtype of `Point{Real}`, the following method can't be applied
to arguments of type `Point{Float64}`:

```julia
function norm(p::Point{Real})
    sqrt(p.x^2 + p.y^2)
end
```

A correct way to define a method that accepts all arguments of type `Point{T}` where `T` is
a subtype of `Real` is:

```julia
function norm(p::Point{<:Real})
    sqrt(p.x^2 + p.y^2)
end
```

(Equivalently, one could define `function norm{T<:Real}(p::Point{T})` or
`function norm(p::Point{T} where T<:Real)`; see [UnionAll Types](@ref).)

More examples will be discussed later in [Methods](@ref).

How does one construct a `Point` object? It is possible to define custom constructors for composite
types, which will be discussed in detail in [Constructors](@ref man-constructors), but in the absence of any special
constructor declarations, there are two default ways of creating new composite objects, one in
which the type parameters are explicitly given and the other in which they are implied by the
arguments to the object constructor.

Since the type `Point{Float64}` is a concrete type equivalent to `Point` declared with [`Float64`](@ref)
in place of `T`, it can be applied as a constructor accordingly:

```jldoctest pointtype
julia> Point{Float64}(1.0, 2.0)
Point{Float64}(1.0, 2.0)

julia> typeof(ans)
Point{Float64}
```

For the default constructor, exactly one argument must be supplied for each field:

```jldoctest pointtype
julia> Point{Float64}(1.0)
ERROR: MethodError: Cannot `convert` an object of type Float64 to an object of type Point{Float64}
This may have arisen from a call to the constructor Point{Float64}(...),
since type constructors fall back to convert methods.
Stacktrace:
 [1] Point{Float64}(::Float64) at ./sysimg.jl:24

julia> Point{Float64}(1.0,2.0,3.0)
ERROR: MethodError: no method matching Point{Float64}(::Float64, ::Float64, ::Float64)
```

Only one default constructor is generated for parametric types, since overriding it is not possible.
This constructor accepts any arguments and converts them to the field types.

In many cases, it is redundant to provide the type of `Point` object one wants to construct, since
the types of arguments to the constructor call already implicitly provide type information. For
that reason, you can also apply `Point` itself as a constructor, provided that the implied value
of the parameter type `T` is unambiguous:

```jldoctest pointtype
julia> Point(1.0,2.0)
Point{Float64}(1.0, 2.0)

julia> typeof(ans)
Point{Float64}

julia> Point(1,2)
Point{Int64}(1, 2)

julia> typeof(ans)
Point{Int64}
```

In the case of `Point`, the type of `T` is unambiguously implied if and only if the two arguments
to `Point` have the same type. When this isn't the case, the constructor will fail with a [`MethodError`](@ref):

```jldoctest pointtype
julia> Point(1,2.5)
ERROR: MethodError: no method matching Point(::Int64, ::Float64)
Closest candidates are:
  Point{T}(::Any) at sysimg.jl:24
  Point{T}(::T, !Matched::T) at none:2
```

Constructor methods to appropriately handle such mixed cases can be defined, but that will not
be discussed until later on in [Constructors](@ref man-constructors).

### Parametric Abstract Types

Parametric abstract type declarations declare a collection of abstract types, in much the same
way:

```jldoctest pointytype
julia> abstract Pointy{T}
```

With this declaration, `Pointy{T}` is a distinct abstract type for each type or integer value
of `T`. As with parametric composite types, each such instance is a subtype of `Pointy`:

```jldoctest pointytype
julia> Pointy{Int64} <: Pointy
true

julia> Pointy{1} <: Pointy
true
```

Parametric abstract types are invariant, much as parametric composite types are:

```jldoctest pointytype
julia> Pointy{Float64} <: Pointy{Real}
false

julia> Pointy{Real} <: Pointy{Float64}
false
```

The notation `Pointy{<:Real}` can be used to express the Julia analogue of a
*covariant* type, while `Pointy{>:Int}` the analogue of a *contravariant* type,
but technically these represent *sets* of types (see [UnionAll Types](@ref)).
```jldoctest pointytype
julia> Pointy{Float64} <: Pointy{<:Real}
true

julia> Pointy{Real} <: Pointy{>:Int}
true
```

Much as plain old abstract types serve to create a useful hierarchy of types over concrete types,
parametric abstract types serve the same purpose with respect to parametric composite types. We
could, for example, have declared `Point{T}` to be a subtype of `Pointy{T}` as follows:

```jldoctest pointytype
julia> type Point{T} <: Pointy{T}
           x::T
           y::T
       end
```

Given such a declaration, for each choice of `T`, we have `Point{T}` as a subtype of `Pointy{T}`:

```jldoctest pointytype
julia> Point{Float64} <: Pointy{Float64}
true

julia> Point{Real} <: Pointy{Real}
true

julia> Point{AbstractString} <: Pointy{AbstractString}
true
```

This relationship is also invariant:

```jldoctest pointytype
julia> Point{Float64} <: Pointy{Real}
false

julia> Point{Float64} <: Pointy{<:Real}
true
```

What purpose do parametric abstract types like `Pointy` serve? Consider if we create a point-like
implementation that only requires a single coordinate because the point is on the diagonal line
*x = y*:

```jldoctest pointytype
julia> type DiagPoint{T} <: Pointy{T}
           x::T
       end
```

Now both `Point{Float64}` and `DiagPoint{Float64}` are implementations of the `Pointy{Float64}`
abstraction, and similarly for every other possible choice of type `T`. This allows programming
to a common interface shared by all `Pointy` objects, implemented for both `Point` and `DiagPoint`.
This cannot be fully demonstrated, however, until we have introduced methods and dispatch in the
next section, [Methods](@ref).

There are situations where it may not make sense for type parameters to range freely over all
possible types. In such situations, one can constrain the range of `T` like so:

```jldoctest realpointytype
julia> abstract Pointy{T<:Real}
```

With such a declaration, it is acceptable to use any type that is a subtype of `Real` in place
of `T`, but not types that are not subtypes of `Real`:

```jldoctest realpointytype
julia> Pointy{Float64}
Pointy{Float64}

julia> Pointy{Real}
Pointy{Real}

julia> Pointy{AbstractString}
ERROR: TypeError: Pointy: in T, expected T<:Real, got Type{AbstractString}

julia> Pointy{1}
ERROR: TypeError: Pointy: in T, expected T<:Real, got Int64
```

Type parameters for parametric composite types can be restricted in the same manner:

```julia
type Point{T<:Real} <: Pointy{T}
    x::T
    y::T
end
```

To give a real-world example of how all this parametric type machinery can be useful, here is
the actual definition of Julia's `Rational` immutable type (except that we omit the constructor
here for simplicity), representing an exact ratio of integers:

```julia
immutable Rational{T<:Integer} <: Real
    num::T
    den::T
end
```

It only makes sense to take ratios of integer values, so the parameter type `T` is restricted
to being a subtype of `Integer`, and a ratio of integers represents a value on the real number
line, so any `Rational` is an instance of the `Real` abstraction.

### Tuple Types

Tuples are an abstraction of the arguments of a function -- without the function itself. The salient
aspects of a function's arguments are their order and their types. Therefore a tuple type is similar
to a parameterized immutable type where each parameter is the type of one field. For example,
a 2-element tuple type resembles the following immutable type:

```julia
immutable Tuple2{A,B}
    a::A
    b::B
end
```

However, there are three key differences:

  * Tuple types may have any number of parameters.
  * Tuple types are *covariant* in their parameters: `Tuple{Int}` is a subtype of `Tuple{Any}`. Therefore
    `Tuple{Any}` is considered an abstract type, and tuple types are only concrete if their parameters
    are.
  * Tuples do not have field names; fields are only accessed by index.

Tuple values are written with parentheses and commas. When a tuple is constructed, an appropriate
tuple type is generated on demand:

```jldoctest
julia> typeof((1,"foo",2.5))
Tuple{Int64,String,Float64}
```

Note the implications of covariance:

```jldoctest
julia> Tuple{Int,AbstractString} <: Tuple{Real,Any}
true

julia> Tuple{Int,AbstractString} <: Tuple{Real,Real}
false

julia> Tuple{Int,AbstractString} <: Tuple{Real,}
false
```

Intuitively, this corresponds to the type of a function's arguments being a subtype of the function's
signature (when the signature matches).

### Vararg Tuple Types

The last parameter of a tuple type can be the special type `Vararg`, which denotes any number
of trailing elements:

```jldoctest
julia> mytupletype = Tuple{AbstractString,Vararg{Int}}
Tuple{AbstractString,Vararg{Int64,N} where N}

julia> isa(("1",), mytupletype)
true

julia> isa(("1",1), mytupletype)
true

julia> isa(("1",1,2), mytupletype)
true

julia> isa(("1",1,2,3.0), mytupletype)
false
```

Notice that `Vararg{T}` corresponds to zero or more elements of type `T`. Vararg tuple types are
used to represent the arguments accepted by varargs methods (see [Varargs Functions](@ref)).

The type `Vararg{T,N}` corresponds to exactly `N` elements of type `T`.  `NTuple{N,T}` is a convenient
alias for `Tuple{Vararg{T,N}}`, i.e. a tuple type containing exactly `N` elements of type `T`.

#### [Singleton Types](@id man-singleton-types)

There is a special kind of abstract parametric type that must be mentioned here: singleton types.
For each type, `T`, the "singleton type" `Type{T}` is an abstract type whose only instance is
the object `T`. Since the definition is a little difficult to parse, let's look at some examples:

```jldoctest
julia> isa(Float64, Type{Float64})
true

julia> isa(Real, Type{Float64})
false

julia> isa(Real, Type{Real})
true

julia> isa(Float64, Type{Real})
false
```

In other words, [`isa(A,Type{B})`](@ref) is true if and only if `A` and `B` are the same object
and that object is a type. Without the parameter, `Type` is simply an abstract type which has
all type objects as its instances, including, of course, singleton types:

```jldoctest
julia> isa(Type{Float64}, Type)
true

julia> isa(Float64, Type)
true

julia> isa(Real, Type)
true
```

Any object that is not a type is not an instance of `Type`:

```jldoctest
julia> isa(1, Type)
false

julia> isa("foo", Type)
false
```

Until we discuss [Parametric Methods](@ref) and [conversions](@ref conversion-and-promotion), it is difficult to explain
the utility of the singleton type construct, but in short, it allows one to specialize function
behavior on specific type *values*. This is useful for writing methods (especially parametric
ones) whose behavior depends on a type that is given as an explicit argument rather than implied
by the type of one of its arguments.

A few popular languages have singleton types, including Haskell, Scala and Ruby. In general usage,
the term "singleton type" refers to a type whose only instance is a single value. This meaning
applies to Julia's singleton types, but with that caveat that only type objects have singleton
types.

### Parametric Bits Types

Bits types can also be declared parametrically. For example, pointers are represented as boxed
bits types which would be declared in Julia like this:

```julia
# 32-bit system:
bitstype 32 Ptr{T}

# 64-bit system:
bitstype 64 Ptr{T}
```

The slightly odd feature of these declarations as compared to typical parametric composite types,
is that the type parameter `T` is not used in the definition of the type itself -- it is just
an abstract tag, essentially defining an entire family of types with identical structure, differentiated
only by their type parameter. Thus, `Ptr{Float64}` and `Ptr{Int64}` are distinct types, even though
they have identical representations. And of course, all specific pointer types are subtype of
the umbrella `Ptr` type:

```jldoctest
julia> Ptr{Float64} <: Ptr
true

julia> Ptr{Int64} <: Ptr
true
```

## UnionAll Types

We have said that a parametric type like `Ptr` acts as a supertype of all its instances
(`Ptr{Int64}` etc.). How does this work? `Ptr` itself cannot be a normal data type, since without
knowing the type of the referenced data the type clearly cannot be used for memory operations.
The answer is that `Ptr` (or other parametric type like `Array`) is a different kind of type called a
`UnionAll` type. Such a type expresses the *iterated union* of types for all values of some parameter.

`UnionAll` types are usually written using the keyword `where`. For example `Ptr` could be more
accurately written as `Ptr{T} where T`, meaning all values whose type is `Ptr{T}` for some value
of `T`. In this context, the parameter `T` is also often called a "type variable" since it is
like a variable that ranges over types.
Each `where` introduces a single type variable, so these expressions are nested for types with
multiple parameters, for example `Array{T,N} where N where T`.

The type application syntax `A{B,C}` requires `A` to be a `UnionAll` type, and first substitutes `B`
for the outermost type variable in `A`.
The result is expected to be another `UnionAll` type, into which `C` is then substituted.
So `A{B,C}` is equivalent to `A{B}{C}`.
This explains why it is possible to partially instantiate a type, as in `Array{Float64}`: the first
parameter value has been fixed, but the second still ranges over all possible values.
Using explicit `where` syntax, any subset of parameters can be fixed. For example, the type of all
1-dimensional arrays can be written as `Array{T,1} where T`.

Type variables can be restricted with subtype relations.
`Array{T} where T<:Integer` refers to all arrays whose element type is some kind of `Integer`.
The syntax `Array{<:Integer}` is a convenient shorthand for `Array{T} where T<:Integer`.
Type variables can have both lower and upper bounds.
`Array{T} where Int<:T<:Number` refers to all arrays of `Number`s that are able to contain `Int`s
(since `T` must be at least as big as `Int`).
The syntax `where T>:Int` also works to specify only the lower bound of a type variable,
and `Array{>:Int}` is equivalent to `Array{T} where T>:Int`.

Since `where` expressions nest, type variable bounds can refer to outer type variables.
For example `Tuple{T,Array{S}} where S<:AbstractArray{T} where T<:Real` refers to 2-tuples whose first
element is some `Real`, and whose second element is an `Array` of any kind of array whose element type
contains the type of the first tuple element.

The `where` keyword itself can be nested inside a more complex declaration.  For example, consider the
two types created by the following declarations:

```jldoctest
julia> const T1 = Array{Array{T,1} where T, 1}
Array{Array{T,1} where T,1}

julia> const T2 = Array{Array{T,1}, 1} where T
Array{Array{T,1},1} where T
```

Type `T1` defines a 1-dimensional array of 1-dimensional arrays; each
of the inner arrays consists of objects of the same type, but this type may vary from one inner array to the next.
On the other hand, type `T2` defines a 1-dimensional array of 1-dimensional arrays all of whose inner arrays must have the
same type.  Note that `T2` is an abstract type, e.g., `Array{Array{Int,1},1} <: T2`, whereas `T1` is a concrete type. As a consequence, `T1` can be constructed with a zero-argument constructor `a=T1()` but `T2` cannot.

## Type Aliases

Sometimes it is convenient to introduce a new name for an already expressible type. For such occasions,
Julia provides the `typealias` mechanism. For example, `UInt` is type aliased to either `UInt32`
or `UInt64` as is appropriate for the size of pointers on the system:

```julia
# 32-bit system:
julia> UInt
UInt32

# 64-bit system:
julia> UInt
UInt64
```

This is accomplished via the following code in `base/boot.jl`:

```julia
if Int === Int64
    typealias UInt UInt64
else
    typealias UInt UInt32
end
```

Of course, this depends on what `Int` is aliased to -- but that is predefined to be the correct
type -- either `Int32` or `Int64`.

For parametric types, `typealias` can be convenient for providing names for cases where some of
the parameter choices are fixed:

```julia
typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
```

Writing `Vector{Float64}` is equivalent to writing `Array{Float64,1}`, and the umbrella type
`Vector` has as instances all `Array` objects where the second parameter -- the number of array
dimensions -- is 1, regardless of what the element type is. In languages where parametric types
must always be specified in full, this is not especially helpful, but in Julia, this allows one
to write just `Matrix` for the abstract type including all two-dimensional dense arrays of any
element type.

## Operations on Types

Since types in Julia are themselves objects, ordinary functions can operate on them. Some functions
that are particularly useful for working with or exploring types have already been introduced,
such as the `<:` operator, which indicates whether its left hand operand is a subtype of its right
hand operand.

The [`isa`](@ref) function tests if an object is of a given type and returns true or false:

```jldoctest
julia> isa(1, Int)
true

julia> isa(1, AbstractFloat)
false
```

The [`typeof()`](@ref) function, already used throughout the manual in examples, returns the type
of its argument. Since, as noted above, types are objects, they also have types, and we can ask
what their types are:

```jldoctest
julia> typeof(Rational{Int})
DataType

julia> typeof(Union{Real,Float64,Rational})
DataType

julia> typeof(Union{Real,String})
Union
```

What if we repeat the process? What is the type of a type of a type? As it happens, types are
all composite values and thus all have a type of `DataType`:

```jldoctest
julia> typeof(DataType)
DataType

julia> typeof(Union)
DataType
```

`DataType` is its own type.

Another operation that applies to some types is [`supertype()`](@ref), which reveals a type's
supertype. Only declared types (`DataType`) have unambiguous supertypes:

```jldoctest
julia> supertype(Float64)
AbstractFloat

julia> supertype(Number)
Any

julia> supertype(AbstractString)
Any

julia> supertype(Any)
Any
```

If you apply [`supertype()`](@ref) to other type objects (or non-type objects), a [`MethodError`](@ref)
is raised:

```jldoctest
julia> supertype(Union{Float64,Int64})
ERROR: MethodError: no method matching supertype(::Type{Union{Float64, Int64}})
Closest candidates are:
  supertype(!Matched::DataType) at operators.jl:38
  supertype(!Matched::UnionAll) at operators.jl:43
```

## Custom pretty-printing

Often, one wants to customize how instances of a type are displayed.  This is accomplished by
overloading the [`show()`](@ref) function.  For example, suppose we define a type to represent
complex numbers in polar form:

```jldoctest polartype
julia> type Polar{T<:Real} <: Number
           r::T
           Θ::T
       end

julia> Polar(r::Real,Θ::Real) = Polar(promote(r,Θ)...)
Polar
```

Here, we've added a custom constructor function so that it can take arguments of different `Real`
types and promote them to a common type (see [Constructors](@ref man-constructors) and [Conversion and Promotion](@ref conversion-and-promotion)).
(Of course, we would have to define lots of other methods, too, to make it act like a `Number`,
e.g. `+`, `*`, `one`, `zero`, promotion rules and so on.) By default, instances of this type display
rather simply, with information about the type name and the field values, as e.g. `Polar{Float64}(3.0,4.0)`.

If we want it to display instead as `3.0 * exp(4.0im)`, we would define the following method to
print the object to a given output object `io` (representing a file, terminal, buffer, etcetera;
see [Networking and Streams](@ref)):

```jldoctest polartype
julia> Base.show(io::IO, z::Polar) = print(io, z.r, " * exp(", z.Θ, "im)")
```

More fine-grained control over display of `Polar` objects is possible. In particular, sometimes
one wants both a verbose multi-line printing format, used for displaying a single object in the
REPL and other interactive environments, and also a more compact single-line format used for
[`print()`](@ref) or for displaying the object as part of another object (e.g. in an array). Although
by default the `show(io, z)` function is called in both cases, you can define a *different* multi-line
format for displaying an object by overloading a three-argument form of `show` that takes the
`text/plain` MIME type as its second argument (see [Multimedia I/O](@ref)), for example:

```jldoctest polartype
julia> Base.show{T}(io::IO, ::MIME"text/plain", z::Polar{T}) =
           print(io, "Polar{$T} complex number:\n   ", z)
```

(Note that `print(..., z)` here will call the 2-argument `show(io, z)` method.) This results in:

```jldoctest polartype
julia> Polar(3, 4.0)
Polar{Float64} complex number:
   3.0 * exp(4.0im)

julia> [Polar(3, 4.0), Polar(4.0,5.3)]
2-element Array{Polar{Float64},1}:
 3.0 * exp(4.0im)
 4.0 * exp(5.3im)
```

where the single-line `show(io, z)` form is still used for an array of `Polar` values.   Technically,
the REPL calls `display(z)` to display the result of executing a line, which defaults to `show(STDOUT, MIME("text/plain"), z)`,
which in turn defaults to `show(STDOUT, z)`, but you should *not* define new [`display()`](@ref)
methods unless you are defining a new multimedia display handler (see [Multimedia I/O](@ref)).

Moreover, you can also define `show` methods for other MIME types in order to enable richer display
(HTML, images, etcetera) of objects in environments that support this (e.g. IJulia).   For example,
we can define formatted HTML display of `Polar` objects, with superscripts and italics, via:

```jldoctest polartype
julia> Base.show{T}(io::IO, ::MIME"text/html", z::Polar{T}) =
           println(io, "<code>Polar{$T}</code> complex number: ",
                   z.r, " <i>e</i><sup>", z.Θ, " <i>i</i></sup>")
```

A `Polar` object will then display automatically using HTML in an environment that supports HTML
display, but you can call `show` manually to get HTML output if you want:

```jldoctest polartype
julia> show(STDOUT, "text/html", Polar(3.0,4.0))
<code>Polar{Float64}</code> complex number: 3.0 <i>e</i><sup>4.0 <i>i</i></sup>
```

```@raw html
<p>An HTML renderer would display this as: <code>Polar{Float64}</code> complex number: 3.0 <i>e</i><sup>4.0 <i>i</i></sup></p>
```

## "Value types"

In Julia, you can't dispatch on a *value* such as `true` or `false`. However, you can dispatch
on parametric types, and Julia allows you to include "plain bits" values (Types, Symbols, Integers,
floating-point numbers, tuples, etc.) as type parameters.  A common example is the dimensionality
parameter in `Array{T,N}`, where `T` is a type (e.g., `Float64`) but `N` is just an `Int`.

You can create your own custom types that take values as parameters, and use them to control dispatch
of custom types. By way of illustration of this idea, let's introduce a parametric type, `Val{T}`,
which serves as a customary way to exploit this technique for cases where you don't need a more
elaborate hierarchy.

`Val` is defined as:

```jldoctest valtype
julia> immutable Val{T}
       end
```

There is no more to the implementation of `Val` than this.  Some functions in Julia's standard
library accept `Val` types as arguments, and you can also use it to write your own functions.
 For example:

```jldoctest valtype
julia> firstlast(::Type{Val{true}}) = "First"
firstlast (generic function with 1 method)

julia> firstlast(::Type{Val{false}}) = "Last"
firstlast (generic function with 2 methods)

julia> firstlast(Val{true})
"First"

julia> firstlast(Val{false})
"Last"
```

For consistency across Julia, the call site should always pass a `Val`*type* rather than creating
an *instance*, i.e., use `foo(Val{:bar})` rather than `foo(Val{:bar}())`.

It's worth noting that it's extremely easy to mis-use parametric "value" types, including `Val`;
in unfavorable cases, you can easily end up making the performance of your code much *worse*.
 In particular, you would never want to write actual code as illustrated above.  For more information
about the proper (and improper) uses of `Val`, please read the more extensive discussion in [the performance tips](@ref man-performance-tips).

## [Nullable Types: Representing Missing Values](@id man-nullable-types)

In many settings, you need to interact with a value of type `T` that may or may not exist. To
handle these settings, Julia provides a parametric type called [`Nullable{T}`](@ref), which can be thought
of as a specialized container type that can contain either zero or one values. `Nullable{T}` provides
a minimal interface designed to ensure that interactions with missing values are safe. At present,
the interface consists of several possible interactions:

  * Construct a `Nullable` object.
  * Check if a `Nullable` object has a missing value.
  * Access the value of a `Nullable` object with a guarantee that a [`NullException`](@ref)
    will be thrown if the object's value is missing.
  * Access the value of a `Nullable` object with a guarantee that a default value of type
    `T` will be returned if the object's value is missing.
  * Perform an operation on the value (if it exists) of a `Nullable` object, getting a
    `Nullable` result. The result will be missing if the original value was missing.
  * Performing a test on the value (if it exists) of a `Nullable`
    object, getting a result that is missing if either the `Nullable`
    itself was missing, or the test failed.
  * Perform general operations on single `Nullable` objects, propagating the missing data.

### Constructing [`Nullable`](@ref) objects

To construct an object representing a missing value of type `T`, use the `Nullable{T}()` function:

```jldoctest
julia> x1 = Nullable{Int64}()
Nullable{Int64}()

julia> x2 = Nullable{Float64}()
Nullable{Float64}()

julia> x3 = Nullable{Vector{Int64}}()
Nullable{Array{Int64,1}}()
```

To construct an object representing a non-missing value of type `T`, use the `Nullable(x::T)`
function:

```jldoctest
julia> x1 = Nullable(1)
Nullable{Int64}(1)

julia> x2 = Nullable(1.0)
Nullable{Float64}(1.0)

julia> x3 = Nullable([1, 2, 3])
Nullable{Array{Int64,1}}([1, 2, 3])
```

Note the core distinction between these two ways of constructing a `Nullable` object:
in one style, you provide a type, `T`, as a function parameter; in the other style, you provide
a single value of type `T` as an argument.

### Checking if a `Nullable` object has a value

You can check if a `Nullable` object has any value using [`isnull()`](@ref):

```jldoctest
julia> isnull(Nullable{Float64}())
true

julia> isnull(Nullable(0.0))
false
```

### Safely accessing the value of a `Nullable` object

You can safely access the value of a `Nullable` object using [`get()`](@ref):

```jldoctest
julia> get(Nullable{Float64}())
ERROR: NullException()
Stacktrace:
 [1] get(::Nullable{Float64}) at ./nullable.jl:92

julia> get(Nullable(1.0))
1.0
```

If the value is not present, as it would be for `Nullable{Float64}`, a [`NullException`](@ref)
error will be thrown. The error-throwing nature of the `get()` function ensures that any
attempt to access a missing value immediately fails.

In cases for which a reasonable default value exists that could be used when a `Nullable`
object's value turns out to be missing, you can provide this default value as a second argument
to `get()`:

```jldoctest
julia> get(Nullable{Float64}(), 0.0)
0.0

julia> get(Nullable(1.0), 0.0)
1.0
```

!!! tip
    Make sure the type of the default value passed to `get()` and that of the `Nullable`
    object match to avoid type instability, which could hurt performance. Use [`convert()`](@ref)
    manually if needed.

### Performing operations on `Nullable` objects

`Nullable` objects represent values that are possibly missing, and it
is possible to write all code using these objects by first testing to see if
the value is missing with [`isnull()`](@ref), and then doing an appropriate
action. However, there are some common use cases where the code could be more
concise or clear by using a higher-order function.

The [`map`](@ref) function takes as arguments a function `f` and a `Nullable` value
`x`. It produces a `Nullable`:

 - If `x` is a missing value, then it produces a missing value;
 - If `x` has a value, then it produces a `Nullable` containing
   `f(get(x))` as value.

This is useful for performing simple operations on values that might be missing
if the desired behaviour is to simply propagate the missing values forward.

The [`filter`](@ref) function takes as arguments a predicate function `p`
(that is, a function returning a boolean) and a `Nullable` value `x`.
It produces a `Nullable` value:

 - If `x` is a missing value, then it produces a missing value;
 - If `p(get(x))` is true, then it produces the original value `x`;
 - If `p(get(x))` is false, then it produces a missing value.

In this way, `filter` can be thought of as selecting only allowable
values, and converting non-allowable values to missing values.

While `map` and `filter` are useful in specific cases, by far the most useful
higher-order function is [`broadcast`](@ref), which can handle a wide variety of cases,
including making existing operations work and propagate `Nullable`s. An example
will motivate the need for `broadcast`. Suppose we have a function that computes the
greater of two real roots of a quadratic equation, using the quadratic formula:

```jldoctest nullableroot
julia> root(a::Real, b::Real, c::Real) = (-b + √(b^2 - 4a*c)) / 2a
root (generic function with 1 method)
```

We may verify that the result of `root(1, -9, 20)` is `5.0`, as we expect,
since `5.0` is the greater of two real roots of the quadratic equation.

Suppose now that we want to find the greatest real root of a quadratic
equations where the coefficients might be missing values. Having missing values
in datasets is a common occurrence in real-world data, and so it is important
to be able to deal with them. But we cannot find the roots of an equation if we
do not know all the coefficients. The best solution to this will depend on the
particular use case; perhaps we should throw an error. However, for this
example, we will assume that the best solution is to propagate the missing
values forward; that is, if any input is missing, we simply produce a missing
output.

The `broadcast()` function makes this task easy; we can simply pass the
`root` function we wrote to `broadcast`:

```jldoctest nullableroot
julia> broadcast(root, Nullable(1), Nullable(-9), Nullable(20))
Nullable{Float64}(5.0)

julia> broadcast(root, Nullable(1), Nullable{Int}(), Nullable{Int}())
Nullable{Float64}()

julia> broadcast(root, Nullable{Int}(), Nullable(-9), Nullable(20))
Nullable{Float64}()
```

If one or more of the inputs is missing, then the output of
`broadcast()` will be missing.

There exists special syntactic sugar for the `broadcast()` function
using a dot notation:

```jldoctest nullableroot
julia> root.(Nullable(1), Nullable(-9), Nullable(20))
Nullable{Float64}(5.0)
```

In particular, the regular arithmetic operators can be `broadcast()`
conveniently using `.`-prefixed operators:

```jldoctest
julia> Nullable(2) ./ Nullable(3) .+ Nullable(1.0)
Nullable{Float64}(1.66667)
```

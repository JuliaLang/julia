.. _man-types:

*********
 Types    
*********

Type systems have traditionally fallen into two quite different camps:
static type systems, where every program expression must have a type
computable before the execution of the program, and dynamic type
systems, where nothing is known about types until run time, when the
actual values manipulated by the program are available. Object
orientation allows some flexibility in statically typed languages by
letting code be written without the precise types of values being known
at compile time. The ability to write code that can operate on different
types is called polymorphism. All code in classic dynamically typed
languages is polymorphic: only by explicitly checking types, or when
objects fail to support operations at run-time, are the types of any
values ever restricted.

Julia's type system is dynamic, but gains some of the advantages of
static type systems by making it possible to indicate that certain
values are of specific types. This can be of great assistance in
generating efficient code, but even more significantly, it allows method
dispatch on the types of function arguments to be deeply integrated with
the language. Method dispatch is explored in detail in
:ref:`man-methods`, but is rooted in the type system presented
here.

The default behavior in Julia when types are omitted is to allow values
to be of any type. Thus, one can write many useful Julia programs
without ever explicitly using types. When additional expressiveness is
needed, however, it is easy to gradually introduce explicit type
annotations into previously "untyped" code. Doing so will typically
increase both the performance and robustness of these systems, and
perhaps somewhat counterintuitively, often significantly simplify them.

Describing Julia in the lingo of `type
systems <http://en.wikipedia.org/wiki/Type_system>`_, it is: dynamic,
nominative, parametric and dependent. Generic types can be parameterized
by other types and by integers, and the hierarchical relationships
between types are explicitly declared, rather than implied by compatible
structure. One particularly distinctive feature of Julia's type system
is that concrete types may not subtype each other: all concrete types
are final and may only have abstract types as their supertypes. While
this might at first seem unduly restrictive, it has many beneficial
consequences with surprisingly few drawbacks. It turns out that being
able to inherit behavior is much more important than being able to
inherit structure, and inheriting both causes significant difficulties
in traditional object-oriented languages. Other high-level aspects of
Julia's type system that should be mentioned up front are:

-  There is no division between object and non-object values: all values
   in Julia are true objects having a type that belongs to a single,
   fully connected type graph, all nodes of which are equally
   first-class as types.
-  There is no meaningful concept of a "compile-time type": the only
   type a value has is its actual type when the program is running. This
   is called a "run-time type" in object-oriented languages where the
   combination of static compilation with polymorphism makes this
   distinction significant.
-  Only values, not variables, have types — variables are simply names
   bound to values.
-  Both abstract and concrete types can be paramaterized by other types
   and by integers. Type parameters may be completely omitted when they
   do not need to be explicitly referenced or restricted.

Julia's type system is designed to be powerful and expressive, yet
clear, intuitive and unobtrusive. Many Julia programmers may never feel
the need to write code that explicitly uses types. Some kinds of
programming, however, become clearer, simpler, faster and more robust
with declared types.

.. raw:: html

   <div class="sidebar">

A Note On Capitalization. There is no semantic significance to
capitalization of names in Julia, unlike, for example, Ruby, where
identifiers beginning with an uppercase letter (including type names)
are constants. By convention, however, the first letter of each word in
a Julia type name begins with a capital letter and underscores are not
used to separate words. Variables, on the other hand, are conventionally
given lowercase names, with word separation indicated by underscores
("\_"). In numerical code it is not uncommon to use single-letter
uppercase variable names, especially for matrices. Since types rarely
have single-letter names, this does not generally cause confusion,
although type parameter placeholders (see below) also typically use
single-letter uppercase names like T or S.

.. raw:: html

   </div>

Type Declarations
-----------------

The ``::`` operator can be used to attach type annotations to
expressions and variables in programs. There are two primary reasons to
do this:

1. As an assertion to help confirm that your program works the way you
   expect,
2. To provide extra type information to the compiler, which can then
   improve performance in many cases

The ``::`` operator is read as "is an instance of" and can be used
anywhere to assert that the value of the expression on the left is an
instance of the type on the right. When the type on the right is
concrete, the value on the left must have that type as its
implementation — recall that all concrete types are final, so no
implementation is a subtype of any other. When the type is abstract, it
suffices for the value to be implemented by a concrete type that is a
subtype of the abstract type. If the type assertion is not true, an
exception is thrown, otherwise, the left-hand value is returned:

::

    julia> (1+2)::FloatingPoint
    type error: typeassert: expected FloatingPoint, got Int64

    julia> (1+2)::Int
    3

This allows a type assertion to be attached to any expression in-place.

When attached to a variable, the ``::`` operator means something a bit
different: it declares the variable to always have the specified type,
like a type declaration in a statically-typed language such as C. Every
value assigned to the variable will be converted to the declared type
using the ``convert`` function:

::

    julia> function foo()
             x::Int8 = 1000
             x
           end

    julia> foo()
    -24

    julia> typeof(ans)
    Int8

This feature is useful for avoiding performance "gotchas" that could
occur if one of the assignments to a variable changed its type
unexpectedly.

The "declaration" behavior only occurs in specific contexts:

::

    x::Int8        # a variable by itself
    local x::Int8  # in a local declaration
    x::Int8 = 10   # as the left-hand side of an assignment

In value contexts, such as ``f(x::Int8)``, the ``::`` is a type
assertion again and not a declaration.

.. _man-abstract-types:

Abstract Types
--------------

Abstract types cannot be instantiated, and serve only as nodes in the
type graph, thereby describing sets of related concrete types: those
concrete types which are their descendants. We begin with abstract types
even though they have no instantiation because they are the backbone of
the type system: they form the conceptual hierarchy which makes Julia's
type system more than just a collection of object implementations.

Recall that in :ref:`man-integers-and-floating-point-numbers`, we introduced a
variety of concrete types of numeric values: ``Int8``, ``Uint8``,
``Int16``, ``Uint16``, ``Int32``, ``Uint32``, ``Int64``, ``Uint64``,
``Float32``, and ``Float64``. These are all `bits types <#Bits+Types>`_,
which we will discuss in the next section. Although they have different
representation sizes, ``Int8``, ``Int16``, ``Int32`` and ``Int64`` all
have in common that they are signed integer types. Likewise ``Uint8``,
``Uint16``, ``Uint32`` and ``Uint64`` are all unsigned integer types,
while ``Float32`` and ``Float64`` are distinct in being floating-point
types rather than integers. It is common for a piece of code to make
sense, for example, only if its arguments are some kind of integer, but
not really depend on what particular *kind* of integer, as long as the
appropriate low-level implementations of integer operations are used.
For example, the greatest common denominator algorithm works for all
kinds of integers, but will not work for floating-point numbers.
Abstract types allow the construction of a hierarchy of types,
providing a context into which concrete types can fit. This allows you,
for example, to easily program to any type that is an integer, without
restricting an algorithm to a specific type of integer.

Abstract types are declared using the ``abstract`` keyword. The general
syntaxes for declaring an abstract type are:

::

    abstract «name»
    abstract «name» <: «supertype»

The ``abstract`` keyword introduces a new abstract type, whose name is
given by ``«name»``. This name can be optionally followed by ``<:`` and
an already-existing type, indicating that the newly declared abstract
type is a subtype of this "parent" type.

When no supertype is given, the default supertype is ``Any`` — a
predefined abstract type that all objects are instances of and all types
are subtypes of. In type theory, ``Any`` is commonly called "top"
because it is at the apex of the type graph. Julia also has a predefined
abstract "bottom" type, at the nadir of the type graph, which is called
``None``. It is the exact opposite of ``Any``: no object is an instance
of ``None`` and all types are supertypes of ``None``.

As a specific example, let's consider a subset of the abstract types
that make up Julia's numerical hierarchy:

::

    abstract Number
    abstract Real     <: Number
    abstract FloatingPoint <: Real
    abstract Integer  <: Real
    abstract Signed   <: Integer
    abstract Unsigned <: Integer

The ``Number`` type is a direct child type of ``Any``, and ``Real`` is
its child. In turn, ``Real`` has two children (it has more, but only two
are shown here; we'll get to the others later): ``Integer`` and
``FloatingPoint``, separating the world into representations of integers and
representations of real numbers. Representations of real numbers
include, of course, floating-point types, but also include other types,
such as Julia's rationals. Hence, ``FloatingPoint`` is a proper subtype of
``Real``, including only floating-point representations of real numbers.
Integers are further subdivided into ``Signed`` and ``Unsigned``
varieties.

The ``<:`` operator in general means "is a subtype of", and, used in
declarations like this, declares the right-hand type to be an immediate
supertype of the newly declared type. It can also be used in expressions
as a subtype operator which returns ``true`` when its left operand is a
subtype of its right operand:

::

    julia> Integer <: Number
    true

    julia> Integer <: FloatingPoint
    false

Since abstract types have no instantiations and serve as no more than
nodes in the type graph, there is not much more to say about them until
we introduce parametric abstract types later on in `Parametric
Types <#man-parametric-types>`_.

Bits Types
----------

A bits type is a concrete type whose data consists of plain old bits.
Classic examples of bits types are integers and floating-point values.
Unlike most languages, Julia lets you declare your own bits types,
rather than providing only a fixed set of built-in bits types. In fact,
the standard bits types are all defined in the language itself:

::

    bitstype 32 Float32 <: FloatingPoint
    bitstype 64 Float64 <: FloatingPoint

    bitstype 8  Bool <: Integer
    bitstype 32 Char <: Integer

    bitstype 8  Int8   <: Signed
    bitstype 8  Uint8  <: Unsigned
    bitstype 16 Int16  <: Signed
    bitstype 16 Uint16 <: Unsigned
    bitstype 32 Int32  <: Signed
    bitstype 32 Uint32 <: Unsigned
    bitstype 64 Int64  <: Signed
    bitstype 64 Uint64 <: Unsigned

The general syntaxes for declaration of a bitstypes are:

::

    bitstype «bits» «name»
    bitstype «bits» «name» <: «supertype»

The number of bits indicates how much storage the type requires and the
name gives the new type a name. A bits type can optionally be declared
to be a subtype of some supertype. If a supertype is omitted, then the
type defaults to having ``Any`` as its immediate supertype. The
declaration of ``Bool`` above therefore means that a boolean value takes
eight bits to store, and has ``Integer`` as its immediate supertype.
Currently, only sizes that are multiples of 8 bits are supported.
Therefore, boolean values, although they really need just a single bit,
cannot be declared to be any smaller then eight bits.

The types ``Bool``, ``Int8`` and ``Uint8`` all have identical
representations: they are eight-bit chunks of memory. Since Julia's type
system is nominative, however, they are not interchangeable despite
having identical structure. Another fundamental difference between them
is that they have different supertypes: ``Bool``'s direct supertype is
``Integer``, ``Int8``'s is ``Signed``, and ``Uint8``'s is ``Unsigned``.
All other differences between ``Bool``, ``Int8``, and ``Uint8`` are
matters of behavior — the way functions are defined to act when given
objects of these types as arguments. This is why a nominative type
system is necessary: if structure determined type, which in turn
dictates behavior, it would be impossible to make ``Bool`` behave any
differently than ``Int8`` or ``Uint8``.

.. _man-composite-types:

Composite Types
---------------

`Composite types <http://en.wikipedia.org/wiki/Composite_data_type>`_
are called records, structures ("structs" in C), or objects in various
languages. A composite type is a collection of named fields, an instance
of which can be treated as a single value. In many languages, composite
types are the only kind of user-definable type, and they are by far the
most commonly used user-defined type in Julia as well. In mainstream
object oriented languages, such as C++, Java, Python and Ruby, composite
types also have named functions associated with them, and the
combination is called an "object". In purer object-oriented languages,
such as Python and Ruby, all values are objects whether they are
composites or not. In less pure object oriented languages, including C++
and Java, some values, such as integers and floating-point values, are
not objects, while instances of user-defined composite types are true
objects with associated methods. In Julia, all values are objects, as in
Python and Ruby, but functions are not bundled with the objects they
operate on. This is necessary since Julia chooses which method of a
function to use by multiple dispatch, meaning that the types of *all* of
a function's arguments are considered when selecting a method, rather
than just the first one (see :ref:`man-methods` for more
information on methods and dispatch). Thus, it would be inappropriate
for functions to "belong" to only their first argument. Organizing
methods by association with function objects rather than simply having
named bags of methods "inside" each object ends up being a highly
beneficial aspect of the language design.

Since composite types are the most common form of user-defined concrete
type, they are simply introduced with the ``type`` keyword followed by a
block of field names, optionally annotated with types using the ``::``
operator:

::

    type Foo
      bar
      baz::Int
      qux::Float64
    end

Fields with no type annotation default to ``Any``, and can accordingly
hold any type of value.

New objects of composite type ``Foo`` are created by applying the
``Foo`` type object like a function to values for its fields:

::

    julia> foo = Foo("Hello, world.", 23, 1.5)
    Foo("Hello, world.",23,1.5)

    julia> typeof(foo)
    Foo

Since the ``bar`` field is unconstrained in type, any value will do; the
value for ``baz`` must be an ``Int`` and ``qux`` must be a ``Float64``.
The signature of the default constructor is taken directly from the
field type declarations ``(Any,Int,Float64)``, so arguments must match
this implied type signature:

::

    julia> Foo((), 23.5, 1)
    no method Foo((),Float64,Int64)

You can access the field values of a composite object using the
traditional ``foo.bar`` notation:

::

    julia> foo.bar
    "Hello, world."

    julia> foo.baz
    23

    julia> foo.qux
    1.5

You can also change the values as one would expect:

::

    julia> foo.qux = 2
    2.0

    julia> foo.bar = 1//2
    1//2

Composite types with no fields are singletons; there can be only one
instance of such types:

::

    type NoFields
    end

    julia> is(NoFields(), NoFields())
    true

The ``is`` function confirms that the "two" constructed instances of
``NoFields`` are actually one and the same.

There is much more to say about how instances of composite types are
created, but that discussion depends on both `Parametric
Types <#man-parametric-types>`_ and on :ref:`man-methods`, and is
sufficiently important to be addressed in its own section:
:ref:`man-constructors`.

Type Unions
-----------

A type union is a special abstract type which includes as objects all
instances of any of its argument types, constructed using the special
``Union`` function:

::

    julia> IntOrString = Union(Int,String)
    Union(Int,String)

    julia> 1 :: IntOrString
    1

    julia> "Hello!" :: IntOrString
    "Hello!"

    julia> 1.0 :: IntOrString
    type error: typeassert: expected Union(Int,String), got Float64

The compilers for many languages have an internal union construct for
reasoning about types; Julia simply exposes it to the programmer. The
union of no types is the "bottom" type, ``None``:

::

    julia> Union()
    None

Recall from the `discussion above <#Any+and+None>`_ that ``None`` is the
abstract type which is the subtype of all other types, and which no
object is an instance of. Since a zero-argument ``Union`` call has no
argument types for objects to be instances of, it should produce the a
type which no objects are instances of — i.e. ``None``.

Tuple Types
-----------

Tuples are an abstraction of the arguments of a function — without the
function itself. The salient aspects of a function's arguments are their
order and their types. The type of a tuple of values is the tuple of
types of values:

::

    julia> typeof((1,"foo",2.5))
    (Int64,ASCIIString,Float64)

Accordingly, a tuple of types can be used anywhere a type is expected:

::

    julia> (1,"foo",2.5) :: (Int64,String,Any)
    (1,"foo",2.5)

    julia> (1,"foo",2.5) :: (Int64,String,Float32)
    type error: typeassert: expected (Int64,String,Float32), got (Int64,ASCIIString,Float64)

If one of the components of the tuple is not a type, however, you will
get an error:

::

    julia> (1,"foo",2.5) :: (Int64,String,3)
    type error: typeassert: expected Type{T}, got (BitsKind,AbstractKind,Int64)

Note that the empty tuple ``()`` is its own type:

::

    julia> typeof(())
    ()

.. _man-parametric-types:

Parametric Types
----------------

An important and powerful feature of Julia's type system is that it is
parametric: types can take parameters, so that type declarations
actually introduce a whole family of new types — one for each possible
combination of parameter values. There are many languages that support
some version of `generic
programming <http://en.wikipedia.org/wiki/Generic_programming>`_, wherein
data structures and algorithms to manipulate them may be specified
without specifying the exact types involved. For example, some form of
generic programming exists in ML, Haskell, Ada, Eiffel, C++, Java, C#,
F#, and Scala, just to name a few. Some of these languages support true
parametric polymorphism (e.g. ML, Haskell, Scala), while others support
ad-hoc, template-based styles of generic programming (e.g. C++, Java).
With so many different varieties of generic programming and parametric
types in various languages, we won't even attempt to compare Julia's
parametric types to other languages, but will instead focus on
explaining Julia's system in its own right. We will note, however, that
because Julia is a dynamically typed language and doesn't need to make
all type decisions at compile time, many traditional difficulties
encountered in static parametric type systems can be relatively easily
handled.

The only kinds of types that are declared are abstract types, bits
types, and composite types. All such types can be parameterized, with
the same syntax in each case. We will discuss them in in the following
order: first, parametric composite types, then parametric abstract
types, and finally parametric bits types.

Parametric Composite Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

Type parameters are introduced immediately after the type name,
surrounded by curly braces:

::

    type Point{T}
      x::T
      y::T
    end

This declaration defines a new parametric type, ``Point{T}``, holding
two "coordinates" of type ``T``. What, one may ask, is ``T``? Well,
that's precisely the point of parametric types: it can be any type at
all (or an integer, actually, although here it's clearly used as a
type). ``Point{Float64}`` is a concrete type equivalent to the type
defined by replacing ``T`` in the definition of ``Point`` with
``Float64``. Thus, this single declaration actually declares an
unlimited number of types: ``Point{Float64}``, ``Point{String}``,
``Point{Int64}``, etc. Each of these is now a usable concrete type:

::

    julia> Point{Float64}
    Point{Float64}

    julia> Point{String}
    Point{String}

The type ``Point{Float64}`` is a point whose coordinates are 64-bit
floating-point values, while the type ``Point{String}`` is a "point"
whose "coordinates" are string objects (see :ref:`man-strings`).
However, ``Point`` itself is also a valid type object:

::

    julia> Point
    Point{T}

Here the ``T`` is the dummy type symbol used in the original declaration
of ``Point``. What does ``Point`` by itself mean? It is an abstract type
that contains all the specific instances ``Point{Float64}``,
``Point{String}``, etc.:

::

    julia> Point{Float64} <: Point
    true

    julia> Point{String} <: Point
    true

Other types, of course, are not subtypes of it:

::

    julia> Float64 <: Point
    false

    julia> String <: Point
    false

Concrete ``Point`` types with different values of ``T`` are never
subtypes of each other:

::

    julia> Point{Float64} <: Point{Int64}
    false

    julia> Point{Float64} <: Point{Real}
    false

This last point is very important:

    **Even though ``Float64 <: Real`` we DO NOT have
    ``Point{Float64} <: Point{Real}``.**

In other words, in the parlance of type theory, Julia's type parameters
are *invariant*, rather than being covariant (or even contravariant).
This is for practical reasons: while any instance of ``Point{Float64}``
may conceptually be like an instance of ``Point{Real}`` as well, the two
types have different representations in memory:

-  An instance of ``Point{Float64}`` can be represented compactly and
   efficiently as an immediate pair of 64-bit values;
-  An instance of ``Point{Real}`` must be able to hold any pair of
   instances of ``Real``. Since objects that are instances of ``Real``
   can be of arbitrary size and structure, in practice an instance of
   ``Point{Real}`` must be represented as a pair of pointers to
   individually allocated ``Real`` objects.

The efficiency gained by being able to store ``Point{Float64}`` objects
with immediate values is magnified enormously in the case of arrays: an
``Array{Float64}`` can be stored as a contiguous memory block of 64-bit
floating-point values, whereas an ``Array{Real}`` must be an array of
pointers to individually allocated ``Real`` objects — which may well be
`boxed <http://en.wikipedia.org/wiki/Object_type_%28object-oriented_programming%29#Boxing>`_
64-bit floating-point values, but also might be arbitrarily large,
complex objects, which are declared to be implementations of the
``Real`` abstract type.

How does one construct a ``Point`` object? It is possible to define
custom constructors for composite types, which will be discussed in
detail in :ref:`man-constructors`, but in the absence of any
special constructor declarations, there are two default ways of creating
new composite objects, one in which the type parameters are explicitly
given and the other in which they are implied by the arguments to the
object constructor.

Since the type ``Point{Float64}`` is a concrete type equivalent to
``Point`` declared with ``Float64`` in place of ``T``, it can be applied
as a constructor accordingly:

::

    julia> Point{Float64}(1.0,2.0)
    Point(1.0,2.0)

    julia> typeof(ans)
    Point{Float64}

For the default constructor, exactly one argument must be supplied for
each field:

::

    julia> Point{Float64}(1.0)
    no method Point(Float64,)

    julia> Point{Float64}(1.0,2.0,3.0)
    no method Point(Float64,Float64,Float64)

The provided arguments need to match the field types exactly, in this
case ``(Float64,Float64)``, as with all composite type default
constructors.

In many cases, it is redundant to provide the type of ``Point`` object
one wants to construct, since the types of arguments to the constructor
call already implicitly provide type information. For that reason, you
can also apply ``Point`` itself as a constructor, provided that the
implied value of the parameter type ``T`` is unambiguous:

::

    julia> Point(1.0,2.0)
    Point(1.0,2.0)

    julia> typeof(ans)
    Point{Float64}

    julia> Point(1,2)
    Point(1,2)

    julia> typeof(ans)
    Point{Int64}

In the case of ``Point``, the type of ``T`` is unambiguously implied if
and only if the two arguments to ``Point`` have the same type. When this
isn't the case, the constructor will fail with a no method error:

::

    julia> Point(1,2.5)
    no method Point(Int64,Float64)

Constructor methods to appropriately handle such mixed cases can be
defined, but that will not be discussed until later on in
:ref:`man-constructors`.

Parametric Abstract Types
~~~~~~~~~~~~~~~~~~~~~~~~~

Parametric abstract type declarations declare a collection of abstract
types, in much the same way:

::

    abstract Pointy{T}

With this declaration, ``Pointy{T}`` is a distinct abstract type for
each type or integer value of ``T``. As with parametric composite types,
each such instance is a subtype of ``Pointy``:

::

    julia> Pointy{Int64} <: Pointy
    true

    julia> Pointy{1} <: Pointy
    true

Parametric abstract types are invariant, much as parametric composite
types are:

::

    julia> Pointy{Float64} <: Pointy{Real}
    false

    julia> Pointy{Real} <: Pointy{Float64}
    false

Much as plain old abstract types serve to create a useful hierarchy of
types over concrete types, parametric abstract types serve the same
purpose with respect to parametric composite types. We could, for
example, have declared ``Point{T}`` to be a subtype of ``Pointy{T}`` as
follows:

::

    type Point{T} <: Pointy{T}
      x::T
      y::T
    end

Given such a declaration, for each choice of ``T``, we have ``Point{T}``
as a subtype of ``Pointy{T}``:

::

    julia> Point{Float64} <: Pointy{Float64}
    true

    julia> Point{Real} <: Pointy{Real}
    true

    julia> Point{String} <: Pointy{String}
    true

This relationship is also invariant:

::

    julia> Point{Float64} <: Pointy{Real}
    false

What purpose do parametric abstract types like ``Pointy`` serve?
Consider if we create a point-like implementation that only requires a
single coordinate because the point is on the diagonal line *x = y*:

::

    type DiagPoint{T} <: Pointy{T}
      x::T
    end

Now both ``Point{Float64}`` and ``DiagPoint{Float64}`` are
implementations of the ``Pointy{Float64}`` abstraction, and similarly
for every other possible choice of type ``T``. This allows programming
to a common interface shared by all ``Pointy`` objects, implemented for
both ``Point`` and ``DiagPoint``. This cannot be fully demonstrated,
however, until we have introduced methods and dispatch in the next
section, :ref:`man-methods`.

There are situations where it may not make sense for type parameters to
range freely over all possible types. In such situations, one can
constrain the range of ``T`` like so:

::

    abstract Pointy{T<:Real}

With such a declaration, it is acceptable to use any type that is a
subtype of ``Real`` in place of ``T``, but not types that are not
subtypes of ``Real``:

::

    julia> Pointy{Float64}
    Pointy{Float64}

    julia> Pointy{Real}
    Pointy{Real}

    julia> Pointy{String}
    type error: Pointy: in T, expected Real, got AbstractKind

    julia> Pointy{1}
    type error: Pointy: in T, expected Real, got Int64

Type parameters for parametric composite types can be restricted in the
same manner:

::

    type Point{T<:Real} <: Pointy{T}
      x::T
      y::T
    end

To give a couple of real-world examples of how all this parametric type
machinery can be useful, here is the actual definition of Julia's
``Rational`` type, representing an exact ratio of integers:

::

    type Rational{T<:Integer} <: Real
      num::T
      den::T
    end

It only makes sense to take ratios of integer values, so the parameter
type ``T`` is restricted to being a subtype of ``Integer``, and a ratio
of integers represents a value on the real number line, so any
``Rational`` is an instance of the ``Real`` abstraction.

.. _man-singleton-types:

Singleton Types
^^^^^^^^^^^^^^^

There is a special kind of abstract parametric type that must be
mentioned here: singleton types. For each type, ``T``, the "singleton
type" ``Type{T}`` is an abstract type whose only instance is the object
``T``. Since the definition is a little difficult to parse, let's look
at some examples:

::

    julia> isa(Float64, Type{Float64})
    true

    julia> isa(Real, Type{Float64})
    false

    julia> isa(Real, Type{Real})
    true

    julia> isa(Float64, Type{Real})
    false

In other words, ``isa(A,Type{B})`` is true if and only if ``A`` and
``B`` are the same object and that object is a type. Without the
parameter, ``Type`` is simply an abstract type which has all type
objects as its instances, including, of course, singleton types:

::

    julia> isa(Type{Float64},Type)
    true

    julia> isa(Float64,Type)
    true

    julia> isa(Real,Type)
    true

Any object that is not a type is not an instance of ``Type``:

::

    julia> isa(1,Type)
    false

    julia> isa("foo",Type)
    false

Until we discuss :ref:`man-parametric-methods`
and :ref:`conversions <man-conversion>`, it is
difficult to explain the utility of the singleton type construct, but in
short, it allows one to specialize function behavior on specific type
*values*, rather just kinds of types, which is all that would be
possible in the absence of singleton types. This is useful for writing
methods (especially parametric ones) whose behavior depends on a type
that is given as an explicit argument rather than implied by the type of
one of its arguments.

A few popular languages have singleton types, including Haskell, Scala
and Ruby. In general usage, the term "singleton type" refers to a type
whose only instance is a single value. This meaning applies to Julia's
singleton types, but with that caveat that only type objects have
singleton types, whereas in most languages with singleton types, every
object has one.

Parametric Bits Types
~~~~~~~~~~~~~~~~~~~~~

Bits types can also be declared parametrically. For example, pointers
are represented as boxed bits types which would be declared in Julia
like this:

::

    # 32-bit system:
    bitstype 32 Ptr{T}

    # 64-bit system:
    bitstype 64 Ptr{T}

The slightly odd feature of these declarations as compared to typical
parametric composite types, is that the type parameter ``T`` is not used
in the definition of the type itself — it is just an abstract tag,
essentially defining an entire family of types with identical structure,
differentiated only by their type parameter. Thus, ``Ptr{Float64}`` and
``Ptr{Int64}`` are distinct types, even though they have identical
representations. And of course, all specific pointer types are subtype
of the umbrella ``Ptr`` type:

::

    julia> Ptr{Float64} <: Ptr
    true

    julia> Ptr{Int64} <: Ptr
    true

Type Aliases
------------

Sometimes it is convenient to introduce a new name for an already
expressible type. For such occasions, Julia provides the ``typealias``
mechanism. For example, ``Uint`` is type aliased to either ``Uint32`` or
``Uint64`` as is appropriate for the size of pointers on the system:

::

    # 32-bit system:
    julia> Uint
    Uint32

    # 64-bit system:
    julia> Uint
    Uint64

This is accomplished via the following code in ``src/boot.jl``:

::

    if is(Int,Int64)
        typealias Uint Uint64
    else
        typealias Uint Uint32
    end

Of course, this depends on what ``Int`` is aliased to — but that is
pre-defined to be the correct type — either ``Int32`` or ``Int64``.

For parametric types, ``typealias`` can be convenient for providing a
new parametric types name where one of the parameter choices is fixed.
Julia's arrays have type ``Array{T,n}`` where ``T`` is the element type
and ``n`` is the number of array dimensions. For convenience, writing
``Array{Float64}`` allows one to specify the element type without
specifying the dimension:

::

    julia> Array{Float64,1} <: Array{Float64} <: Array
    true

However, there is no way to equally simply restrict just the dimension
but not the element type. Yet, one often needs to program to just
vectors or matrices. For that reason, the following type aliases are
provided:

::

    typealias Vector{T} Array{T,1}
    typealias Matrix{T} Array{T,2}

Writing ``Vector{Float64}`` is equivalent to writing
``Array{Float64,1}``, and the umbrella type ``Vector`` has as instances
all ``Array`` objects where the second parameter — the number of array
dimensions — is 1, regardless of what the element type is. In languages
where parametric types must be always specified in full, this is not
especially helpful, but in Julia, this allows one to write just
``Matrix`` for the abstract type including all two-dimensional dense
arrays of any element type.

Operations on Types
-------------------

Since types in Julia are themselves objects, ordinary functions can
operate on them. Some functions that are particularly useful for working
with or exploring types have already been introduced, such as the ``<:``
operator, which indicates whether its left hand operand is a subtype of
its right hand operand.

The ``isa`` function tests if an object is of a given type and returns
true or false:

::

    julia> isa(1,Int)
    true

    julia> isa(1,FloatingPoint)
    false

The ``typeof`` function, already used throughout the manual in examples,
returns the type of its argument. Since, as noted above, types are
objects, they also have types, and we can ask what their types are. Here
we apply ``typeof`` to an instance of each of the kinds of types
discussed above:

::

    julia> typeof(Real)
    AbstractKind

    julia> typeof(Float64)
    BitsKind

    julia> typeof(Rational)
    CompositeKind

    julia> typeof(Union(Real,Float64,Rational))
    UnionKind

    julia> typeof((Real,Float64,Rational,None))
    (AbstractKind,BitsKind,CompositeKind,UnionKind)

As you can see, the types of types are called, by convention, "kinds":

-  Abstract types have type ``AbstractKind``
-  Bits types have type ``BitsKind``
-  Composite types have type ``CompositeKind``
-  Unions have type ``UnionKind``
-  Tuples of types have a type that is the tuple of their respective
   kinds.

What if we repeat the process? What is the type of a kind? Kinds, as it
happens, are all composite values and thus all have a type of
``CompositeKind``:

::

    julia> typeof(AbstractKind)
    CompositeKind

    julia> typeof(BitsKind)
    CompositeKind

    julia> typeof(CompositeKind)
    CompositeKind

    julia> typeof(UnionKind)
    CompositeKind

The reader may note that ``CompositeKind`` shares with the empty tuple
(see `above <#tuple-types>`_), the distinction of being its own type
(i.e. a fixed point of the ``typeof`` function). This leads any number
of tuple types recursively built with ``()`` and ``CompositeKind`` as
their only atomic values, which are their own type:

::

    julia> typeof(())
    ()

    julia> typeof(CompositeKind)
    CompositeKind

    julia> typeof(((),))
    ((),)

    julia> typeof((CompositeKind,))
    (CompositeKind,)

    julia> typeof(((),CompositeKind))
    ((),CompositeKind)

All fixed points of the ``typeof`` function are like this.

Another operation that applies to some kinds of types is ``super``. Only
abstract types (``AbstractKind``), bits types (``BitsKind``), and
composite types (``CompositeKind``) have a supertype, so these are the
only kinds of types that the ``super`` function applies to:

::

    julia> super(Float64)
    FloatingPoint

    julia> super(Number)
    Any

    julia> super(String)
    Any

    julia> super(Any)
    Any

If you apply ``super`` to other type objects (or non-type objects), a
"no method" error is raised:

::

    julia> super(Union(Float64,Int64))
    no method super(UnionKind,)

    julia> super(None)
    no method super(UnionKind,)

    julia> super((Float64,Int64))
    no method super((BitsKind,BitsKind),)


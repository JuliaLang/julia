.. _man-constructors:

**************
 Constructors  
**************

Constructors are functions that create new objects — specifically,
instances of :ref:`man-composite-types`. In Julia,
type objects also serve as constructor functions: they create new
instances of themselves when applied to an argument tuple as a function.
This much was already mentioned briefly when composite types were
introduced. For example::

    type Foo
      bar
      baz
    end

    julia> foo = Foo(1,2)
    Foo(1,2)

    julia> foo.bar
    1

    julia> foo.baz
    2

For many types, forming new objects by binding their field values
together is all that is ever needed to create instances. There are,
however, cases where more functionality is required when creating
composite objects. Sometimes invariants must be enforced, either by
checking arguments or by transforming them. `Recursive data
structures <http://en.wikipedia.org/wiki/Recursion_%28computer_science%29#Recursive_data_structures_.28structural_recursion.29>`_,
especially those that may be self-referential, often cannot be
constructed cleanly without first being created in an incomplete state
and then altered programmatically to be made whole, as a separate step
from object creation. Sometimes, it's just convenient to be able to
construct objects with fewer or different types of parameters than they
have fields. Julia's system for object construction addresses all of
these cases and more.

Outer Constructor Methods
-------------------------

A constructor is just like any other function in Julia in that its
overall behavior is defined by the combined behavior of its methods.
Accordingly, you can add functionality to a constructor by simply
defining new methods. For example, let's say you want to add a
constructor method for ``Foo`` objects that takes only one argument and
uses the given value for both the ``bar`` and ``baz`` fields. This is
simple::

    Foo(x) = Foo(x,x)

    julia> Foo(1)
    Foo(1,1)

You could also add a zero-argument ``Foo`` constructor method that
supplies default values for both of the ``bar`` and ``baz`` fields::

    Foo() = Foo(0)

    julia> Foo()
    Foo(0,0)

Here the zero-argument constructor method calls the single-argument
constructor method, which in turn calls the automatically provided
two-argument constructor method. For reasons that will become clear very
shortly, additional constructor methods declared as normal methods like
this are called *outer* constructor methods. Outer constructor methods
can only ever create a new instance by calling another constructor
method, such as the automatically provided default one.

.. raw:: html

   <div class="sidebar">

A Note On Nomenclature. While the term “constructor” generally refers to
the entire function which constructs objects of a type, it is common to
abuse terminology slightly and refer to specific constructor methods as
“constructors”. In such situations, it is generally clear from context
that the term is used to mean “constructor method” rather than
“constructor function”, especially as it is often used in the sense of
singling out a particular method of the constructor from all of the
others.

.. raw:: html

   </div>

Inner Constructor Methods
-------------------------

While outer constructor methods succeed in addressing the problem of
providing additional convenience methods for constructing objects, they
fail to address the other two use cases mentioned in the introduction of
this chapter: enforcing invariants, and allowing construction of
self-referential objects. For these problems, one needs *inner*
constructor methods. An inner constructor method is much like an outer
constructor method, with two differences:

1. It is declared inside the block of a type declaration, rather than
   outside of it like normal methods.
2. It has access to a special locally existent function called ``new``
   that creates objects of the block's type.

For example, suppose one wants to declare a type that holds a pair of
real numbers, subject to the constraint that the first number is
not greater than the second one. One could declare it like this::

    type OrderedPair
      x::Real
      y::Real

      OrderedPair(x,y) = x > y ? error("out of order") : new(x,y)
    end

Now ``OrderedPair`` objects can only be constructed such that
``x <= y``::

    julia> OrderedPair(1,2)
    OrderedPair(1,2)

    julia> OrderedPair(2,1)
    out of order
     in OrderedPair at prompt:5

You can still reach in and directly change the field values to violate
this invariant (support for immutable composites is planned but not yet
implemented), but messing around with an object's internals uninvited is
considered poor form. You (or someone else) can also provide additional
outer constructor methods at any later point, but once a type is
declared, there is no way to add more inner constructor methods. Since
outer constructor methods can only create objects by calling other
constructor methods, ultimately, some inner constructor must be called
to create an object. This guarantees that all objects of the declared
type must come into existence by a call to one of the inner constructor
methods provided with the type, thereby giving some degree of real
enforcement of a type's invariants, at least for object creation.

If any inner constructor method is defined, no default constructor
method is provided: it is presumed that you have supplied yourself with
all the inner constructors you need. The default constructor is
equivalent to writing your own inner constructor method that takes all
of the object's fields as parameters (constrained to be of the correct
type, if the corresponding field has a type), and passes them to
``new``, returning the resulting object::

    type Foo
      bar
      baz

      Foo(bar,baz) = new(bar,baz)
    end

This declaration has the same effect as the earlier definition of the
``Foo`` type without an explicit inner constructor method. The following
two types are equivalent — one with a default constructor, the other
with an explicit constructor::

    type T1
      x::Int64
    end

    type T2
      x::Int64
      T2(x::Int64) = new(x)
    end

    julia> T1(1)
    T1(1)

    julia> T2(1)
    T2(1)

    julia> T1(1.0)
    no method T1(Float64,)
     in method_missing at /Users/stefan/projects/julia/base/base.jl:58

    julia> T2(1.0)
    no method T2(Float64,)
     in method_missing at /Users/stefan/projects/julia/base/base.jl:58

It is considered good form to provide as few inner constructor methods
as possible: only those taking all arguments explicitly and enforcing
essential error checking and transformation. Additional convenience
constructor methods, supplying default values or auxiliary
transformations, should be provided as outer constructors that call the
inner constructors to do the heavy lifting. This separation is typically
quite natural.

Incomplete Initialization
-------------------------

The final problem which has still not been addressed is construction of
self-referential objects, or more generally, recursive data structures.
Since the fundamental difficulty may not be immediately obvious, let us
briefly explain it. Consider the following recursive type declaration::

    type SelfReferential
      obj::SelfReferential
    end

This type may appear innocuous enough, until one considers how to
construct an instance of it. If ``a`` is an instance of
``SelfReferential``, then a second instance can be created by the call::

    b = SelfReferential(a)

But how does one construct the first instance when no instance exists to
provide as a valid value for its ``obj`` field? The only solution is to
allow creating an incompletely initialized instance of
``SelfReferential`` with an unassigned ``obj`` field, and using that
incomplete instance as a valid value for the ``obj`` field of another
instance, such as, for example, itself.

To allow for the creation of incompletely initialized objects, Julia
allows the ``new`` function to be called with fewer than the number of
fields that the type has, returning an object with the unspecified
fields uninitialized. The inner constructor method can then use the
incomplete object, finishing its initialization before returning it.
Here, for example, we take another crack at defining the
``SelfReferential`` type, with a zero-argument inner constructor
returning instances having ``obj`` fields pointing to themselves::

    type SelfReferential
      obj::SelfReferential

      SelfReferential() = (x = new(); x.obj = x)
    end

We can verify that this constructor works and constructs objects that
are, in fact, self-referential::

    x = SelfReferential();

    julia> is(x, x)
    true

    julia> is(x, x.obj)
    true

    julia> is(x, x.obj.obj)
    true

Although it is generally a good idea to return a fully initialized
object from an inner constructor, incompletely initialized objects can
be returned::

    type Incomplete
      xx

      Incomplete() = new()
    end

    julia> z = Incomplete();

While you are allowed to create objects with uninitialized fields, any
access to an uninitialized field is an immediate error::

    julia> z.xx
    access to undefined reference

This prevents uninitialized fields from propagating throughout a program
or forcing programmers to continually check for uninitialized fields,
the way they have to check for ``null`` values everywhere in Java: if a
field is uninitialized and it is used in any way, an error is thrown
immediately so no error checking is required. You can also pass
incomplete objects to other functions from inner constructors to
delegate their completion::

    type Lazy
      xx

      Lazy(v) = complete_me(new(), v)
    end

As with incomplete objects returned from constructors, if
``complete_me`` or any of its callees try to access the ``xx`` field of
the ``Lazy`` object before it has been initialized, an error will be
thrown immediately.

Parametric Constructors
-----------------------

Parametric types add a few wrinkles to the constructor story. Recall
from :ref:`man-parametric-types` that, by default,
instances of parametric composite types can be constructed either with
explicitly given type parameters or with type parameters implied by the
types of the arguments given to the constructor. Here are some examples::

    type Point{T<:Real}
      x::T
      y::T
    end

    ## implicit T ##

    julia> Point(1,2)
    Point(1,2)

    julia> Point(1.0,2.5)
    Point(1.0,2.5)

    julia> Point(1,2.5)
    no method Point(Int64,Float64)
     in method_missing at /Users/stefan/projects/julia/base/base.jl:58

    ## explicit T ##

    julia> Point{Int64}(1,2)
    Point(1,2)

    julia> Point{Int64}(1.0,2.5)
    no method Point(Float64,Float64)
     in method_missing at /Users/stefan/projects/julia/base/base.jl:58

    julia> Point{Float64}(1.0,2.5)
    Point(1.0,2.5)

    julia> Point{Float64}(1,2)
    no method Point(Int64,Int64)
     in method_missing at /Users/stefan/projects/julia/base/base.jl:58

As you can see, for constructor calls with explicit type parameters, the
arguments must match that specific type: ``Point{Int64}(1,2)`` works,
but ``Point{Int64}(1.0,2.5)`` does not. When the type is implied by the
arguments to the constructor call, as in ``Point(1,2)``, then the types
of the arguments must agree — otherwise the ``T`` cannot be determined —
but any pair of real arguments with matching type may be given to the
generic ``Point`` constructor.

What's really going on here is that ``Point``, ``Point{Float64}`` and
``Point{Int64}`` are all different constructor functions. In fact,
``Point{T}`` is a distinct constructor function for each type ``T``.
Without any explicitly provided inner constructors, the declaration of
the composite type ``Point{T<:Real}`` automatically provides an inner
constructor, ``Point{T}``, for each possible type ``T<:Real``, that
behaves just like non-parametric default inner constructors do. It also
provides a single general outer ``Point`` constructor that takes pairs
of real arguments, which must be of the same type. This automatic
provision of constructors is equivalent to the following explicit
declaration::

    type Point{T<:Real}
      x::T
      y::T

      Point(x::T, y::T) = new(x,y)
    end

    Point{T<:Real}(x::T, y::T) = Point{T}(x,y)

Some features of parametric constructor definitions at work here deserve
comment. First, inner constructor declarations always define methods of
``Point{T}`` rather than methods of the general ``Point`` constructor
function. Since ``Point`` is not a concrete type, it makes no sense for
it to even have inner constructor methods at all. Thus, the inner method
declaration ``Point(x::T, y::T) = new(x,y)`` provides an inner
constructor method for each value of ``T``. It is thus this method
declaration that defines the behavior of constructor calls with explicit
type parameters like ``Point{Int64}(1,2)`` and
``Point{Float64}(1.0,2.0)``. The outer constructor declaration, on the
other hand, defines a method for the general ``Point`` constructor which
only applies to pairs of values of the same real type. This declaration
makes constructor calls without explicit type parameters, like
``Point(1,2)`` and ``Point(1.0,2.5)``, work. Since the method
declaration restricts the arguments to being of the same type, calls
like ``Point(1,2.5)``, with arguments of different types, result in "no
method" errors.

Suppose we wanted to make the constructor call ``Point(1,2.5)`` work by
"promoting" the integer value ``1`` to the floating-point value ``1.0``.
The simplest way to achieve this is to define the following additional
outer constructor method::

    Point(x::Int64, y::Float64) = Point(convert(Float64,x),y)

This method uses the ``convert`` function to explicitly convert ``x`` to
``Float64`` and then delegates construction to the general constructor
for the case where both arguments are ``Float64``. With this method
definition what was previously a "no method" error now successfully
creates a point of type ``Point{Float64}``::

    julia> Point(1,2.5)
    Point(1.0,2.5)

    julia> typeof(ans)
    Point{Float64}

However, other similar calls still don't work::

    julia> Point(1.5,2)
    no method Point(Float64,Int64)

For a much more general way of making all such calls work sensibly, see
:ref:`man-conversion-and-promotion`. At the risk
of spoiling the suspense, we can reveal here that the all it takes is
the following outer method definition to make all calls to the general
``Point`` constructor work as one would expect::

    Point(x::Real, y::Real) = Point(promote(x,y)...)

The ``promote`` function converts all its arguments to a common type
— in this case ``Float64``. With this method definition, the ``Point``
constructor promotes its arguments the same way that numeric operators
like ``+`` do, and works for all kinds of real numbers::

    julia> Point(1.5,2)
    Point(1.5,2.0)

    julia> Point(1,1//2)
    Point(1//1,1//2)

    julia> Point(1.0,1//2)
    Point(1.0,0.5)

Thus, while the implicit type parameter constructors provided by default
in Julia are fairly strict, it is possible to make them behave in a more
relaxed but sensible manner quite easily. Moreover, since constructors
can leverage all of the power of the type system, methods, and multiple
dispatch, defining sophisticated behavior is typically quite simple.

Case Study: Rational
--------------------

Perhaps the best way to tie all these pieces together is to present a
real world example of a parametric composite type and its constructor
methods. To that end, here is beginning of
`rational.jl <https://github.com/JuliaLang/julia/blob/master/base/rational.jl>`_,
which implements Julia's :ref:`man-rational-numbers`::

    type Rational{T<:Integer} <: Real
        num::T
        den::T

        function Rational(num::T, den::T)
            if num == 0 && den == 0
                error("invalid rational: 0//0")
            end
            g = gcd(den, num)
            num = div(num, g)
            den = div(den, g)
            new(num, den)
        end
    end
    Rational{T<:Integer}(n::T, d::T) = Rational{T}(n,d)
    Rational(n::Integer, d::Integer) = Rational(promote(n,d)...)
    Rational(n::Integer) = Rational(n,one(n))

    //(n::Integer, d::Integer) = Rational(n,d)
    //(x::Rational, y::Integer) = x.num // (x.den*y)
    //(x::Integer, y::Rational) = (x*y.den) // y.num
    //(x::Complex, y::Real) = complex(real(x)//y, imag(x)//y)
    //(x::Real, y::Complex) = x*y'//real(y*y')

    function //(x::Complex, y::Complex)
        xy = x*y'
        yy = real(y*y')
        complex(real(xy)//yy, imag(xy)//yy)
    end

The first line — ``type Rational{T<:Int} <: Real`` — declares that
``Rational`` takes one type parameter of an integer type, and is itself
a real type. The field declarations ``num::T`` and ``den::T`` indicate
that the data held in a ``Rational{T}`` object are a pair of integers of
type ``T``, one representing the rational value's numerator and the
other representing its denominator.

Now things get interesting. ``Rational`` has a single inner constructor
method which checks that both of ``num`` and ``den`` aren't zero and
ensures that every rational is constructed in "lowest terms" with a
non-negative denominator. This is accomplished by dividing the given
numerator and denominator values by their greatest common divisor,
computed using the ``gcd`` function. Since ``gcd`` returns the greatest
common divisor of its arguments with sign matching the first argument
(``den`` here), after this division the new value of ``den`` is
guaranteed to be non-negative. Because this is the only inner
constructor for ``Rational``, we can be certain that ``Rational``
objects are always constructed in this normalized form.

``Rational`` also provides several outer constructor methods for
convenience. The first is the "standard" general constructor that infers
the type parameter ``T`` from the type of the numerator and denominator
when they have the same type. The second applies when the given
numerator and denominator values have different types: it promotes them
to a common type and then delegates construction to the outer
constructor for arguments of matching type. The third outer constructor
turns integer values into rationals by supplying a value of ``1`` as the
denominator.

Following the outer constructor definitions, we have a number of methods
for the ``//`` operator, which provides a syntax for writing rationals.
Before these definitions, ``//`` is a completely undefined operator with
only syntax and no meaning. Afterwards, it behaves just as described in
:ref:`man-rational-numbers`
— its entire behavior is defined in these few lines. The first and most
basic definition just makes ``a//b`` construct a ``Rational`` by
applying the ``Rational`` constructor to ``a`` and ``b`` when they are
integers. When one of the operands of ``//`` is already a rational
number, we construct a new rational for the resulting ratio slightly
differently; this behavior is actually identical to division of a
rational with an integer. Finally, applying ``//`` to complex integral
values creates an instance of ``Complex{Rational}`` — a complex number
whose real and imaginary parts are rationals::

    julia> (1 + 2im)//(1 - 2im)
    -3//5 + 4//5im

    julia> typeof(ans)
    ComplexPair{Rational{Int64}}

    julia> ans <: Complex{Rational}
    true

Thus, although the ``//`` operator usually returns an instance of
``Rational``, if either of its arguments are complex integers, it will
return an instance of ``Complex{Rational}`` instead. The interested
reader should consider perusing the rest of
`rational.jl <https://github.com/JuliaLang/julia/blob/master/base/rational.jl>`_:
it is short, self-contained, and implements an entire basic Julia type
in just a little over a hundred lines of code.

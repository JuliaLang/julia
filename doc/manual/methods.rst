.. _man-methods:

*********
 Methods
*********

Recall from :ref:`man-functions` that a function is an object
that maps a tuple of arguments to a return value, or throws an exception
if no appropriate value can be returned. It is common for the same
conceptual function or operation to be implemented quite differently for
different types of arguments: adding two integers is very different from
adding two floating-point numbers, both of which are distinct from
adding an integer to a floating-point number. Despite their
implementation differences, these operations all fall under the general
concept of "addition". Accordingly, in Julia, these behaviors all belong
to a single object: the ``+`` function.

To facilitate using many different implementations of the same concept
smoothly, functions need not be defined all at once, but can rather be
defined piecewise by providing specific behaviors for certain
combinations of argument types and counts. A definition of one possible
behavior for a function is called a *method*. Thus far, we have
presented only examples of functions defined with a single method,
applicable to all types of arguments. However, the signatures of method
definitions can be annotated to indicate the types of arguments in
addition to their number, and more than a single method definition may
be provided. When a function is applied to a particular tuple of
arguments, the most specific method applicable to those arguments is
applied. Thus, the overall behavior of a function is a patchwork of the
behaviors of its various method definitions. If the patchwork is well
designed, even though the implementations of the methods may be quite
different, the outward behavior of the function will appear seamless and
consistent.

The choice of which method to execute when a function is applied is
called *dispatch*. Julia allows the dispatch process to choose which of
a function's methods to call based on the number of arguments given, and
on the types of all of the function's arguments. This is different than
traditional object-oriented languages, where dispatch occurs based only
on the first argument, which often has a special argument syntax, and is
sometimes implied rather than explicitly written as an
argument. [#]_ Using all of a function's arguments to
choose which method should be invoked, rather than just the first, is
known as `multiple dispatch
<https://en.wikipedia.org/wiki/Multiple_dispatch>`_. Multiple
dispatch is particularly useful for mathematical code, where it makes
little sense to artificially deem the operations to "belong" to one
argument more than any of the others: does the addition operation in
``x + y`` belong to ``x`` any more than it does to ``y``? The
implementation of a mathematical operator generally depends on the types
of all of its arguments. Even beyond mathematical operations, however,
multiple dispatch ends up being a powerful and convenient paradigm
for structuring and organizing programs.

.. [#] In C++ or Java, for example, in a method call like
  ``obj.meth(arg1,arg2)``, the object obj "receives" the method call and is
  implicitly passed to the method via the ``this`` keyword, rather than as an
  explicit method argument. When the current ``this`` object is the receiver of a
  method call, it can be omitted altogether, writing just ``meth(arg1,arg2)``,
  with ``this`` implied as the receiving object.


Defining Methods
----------------

Until now, we have, in our examples, defined only functions with a
single method having unconstrained argument types. Such functions behave
just like they would in traditional dynamically typed languages.
Nevertheless, we have used multiple dispatch and methods almost
continually without being aware of it: all of Julia's standard functions
and operators, like the aforementioned ``+`` function, have many methods
defining their behavior over various possible combinations of argument
type and count.

When defining a function, one can optionally constrain the types of
parameters it is applicable to, using the ``::`` type-assertion
operator, introduced in the section on :ref:`man-composite-types`:

.. doctest::

    julia> f(x::Float64, y::Float64) = 2x + y;

This function definition applies only to calls where ``x`` and ``y`` are
both values of type :obj:`Float64`:

.. doctest::

    julia> f(2.0, 3.0)
    7.0

Applying it to any other types of arguments will result in a :exc:`MethodError`:

.. doctest::

    julia> f(2.0, 3)
    ERROR: MethodError: no method matching f(::Float64, ::Int64)
    Closest candidates are:
      f(::Float64, !Matched::Float64) at none:1
    ...

    julia> f(Float32(2.0), 3.0)
    ERROR: MethodError: no method matching f(::Float32, ::Float64)
    Closest candidates are:
      f(!Matched::Float64, ::Float64) at none:1
    ...

    julia> f(2.0, "3.0")
    ERROR: MethodError: no method matching f(::Float64, ::String)
    Closest candidates are:
      f(::Float64, !Matched::Float64) at none:1
    ...

    julia> f("2.0", "3.0")
    ERROR: MethodError: no method matching f(::String, ::String)
    ...

As you can see, the arguments must be precisely of type :obj:`Float64`.
Other numeric types, such as integers or 32-bit floating-point values,
are not automatically converted to 64-bit floating-point, nor are
strings parsed as numbers. Because :obj:`Float64` is a concrete type and
concrete types cannot be subclassed in Julia, such a definition can only
be applied to arguments that are exactly of type :obj:`Float64`. It may
often be useful, however, to write more general methods where the
declared parameter types are abstract:

.. doctest::

    julia> f(x::Number, y::Number) = 2x - y;

    julia> f(2.0, 3)
    1.0

This method definition applies to any pair of arguments that are
instances of :obj:`Number`. They need not be of the same type, so long as
they are each numeric values. The problem of handling disparate numeric
types is delegated to the arithmetic operations in the expression
``2x - y``.

To define a function with multiple methods, one simply defines the
function multiple times, with different numbers and types of arguments.
The first method definition for a function creates the function object,
and subsequent method definitions add new methods to the existing
function object. The most specific method definition matching the number
and types of the arguments will be executed when the function is
applied. Thus, the two method definitions above, taken together, define
the behavior for ``f`` over all pairs of instances of the abstract type
:obj:`Number` — but with a different behavior specific to pairs of
:obj:`Float64` values. If one of the arguments is a 64-bit float but the
other one is not, then the ``f(Float64,Float64)`` method cannot be
called and the more general ``f(Number,Number)`` method must be used:

.. doctest::

    julia> f(2.0, 3.0)
    7.0

    julia> f(2, 3.0)
    1.0

    julia> f(2.0, 3)
    1.0

    julia> f(2, 3)
    1

The ``2x + y`` definition is only used in the first case, while the
``2x - y`` definition is used in the others. No automatic casting or
conversion of function arguments is ever performed: all conversion in
Julia is non-magical and completely explicit. :ref:`man-conversion-and-promotion`, however, shows how clever
application of sufficiently advanced technology can be indistinguishable
from magic. [Clarke61]_

For non-numeric values, and for fewer or more than two arguments, the
function ``f`` remains undefined, and applying it will still result in a
:obj:`MethodError`:

.. doctest::

    julia> f("foo", 3)
    ERROR: MethodError: no method matching f(::String, ::Int64)
    Closest candidates are:
      f(!Matched::Number, ::Number) at none:1
    ...

    julia> f()
    ERROR: MethodError: no method matching f()
    Closest candidates are:
      f(!Matched::Float64, !Matched::Float64) at none:1
      f(!Matched::Number, !Matched::Number) at none:1
    ...


You can easily see which methods exist for a function by entering the
function object itself in an interactive session:

.. doctest::

    julia> f
    f (generic function with 2 methods)

This output tells us that ``f`` is a function object with two
methods. To find out what the signatures of those methods are, use the
:func:`methods` function:

.. doctest::

    julia> methods(f)
    # 2 methods for generic function "f":
    f(x::Float64, y::Float64) in Main at none:1
    f(x::Number, y::Number) in Main at none:1

which shows that ``f`` has two methods, one taking two :obj:`Float64`
arguments and one taking arguments of type :obj:`Number`. It also
indicates the file and line number where the methods were defined:
because these methods were defined at the REPL, we get the apparent
line number ``none:1``.

In the absence of a type declaration with ``::``, the type of a method
parameter is :obj:`Any` by default, meaning that it is unconstrained since
all values in Julia are instances of the abstract type :obj:`Any`. Thus, we
can define a catch-all method for ``f`` like so:

.. doctest::

    julia> f(x,y) = println("Whoa there, Nelly.");

    julia> f("foo", 1)
    Whoa there, Nelly.

This catch-all is less specific than any other possible method
definition for a pair of parameter values, so it is only be called on
pairs of arguments to which no other method definition applies.

Although it seems a simple concept, multiple dispatch on the types of
values is perhaps the single most powerful and central feature of the
Julia language. Core operations typically have dozens of methods:

.. doctest::
   :options: +SKIP

    julia> methods(+)
    # 166 methods for generic function "+":
    +(a::Float16, b::Float16) at float16.jl:136
    +(x::Float32, y::Float32) at float.jl:206
    +(x::Float64, y::Float64) at float.jl:207
    +(x::Bool, z::Complex{Bool}) at complex.jl:126
    +(x::Bool, y::Bool) at bool.jl:48
    +(x::Bool) at bool.jl:45
    +{T<:AbstractFloat}(x::Bool, y::T) at bool.jl:55
    +(x::Bool, z::Complex) at complex.jl:133
    +(x::Bool, A::AbstractArray{Bool,N<:Any}) at arraymath.jl:105
    +(x::Char, y::Integer) at char.jl:40
    +{T<:Union{Int128,Int16,Int32,Int64,Int8,UInt128,UInt16,UInt32,UInt64,UInt8}}(x::T, y::T) at int.jl:32
    +(z::Complex, w::Complex) at complex.jl:115
    +(z::Complex, x::Bool) at complex.jl:134
    +(x::Real, z::Complex{Bool}) at complex.jl:140
    +(x::Real, z::Complex) at complex.jl:152
    +(z::Complex, x::Real) at complex.jl:153
    +(x::Rational, y::Rational) at rational.jl:179
    ...
    +(a, b, c, xs...) at operators.jl:119

Multiple dispatch together with the flexible parametric type system give
Julia its ability to abstractly express high-level algorithms decoupled
from implementation details, yet generate efficient, specialized code to
handle each case at run time.

Method Ambiguities
------------------

It is possible to define a set of function methods such that there is no
unique most specific method applicable to some combinations of
arguments:

.. doctest::

    julia> g(x::Float64, y) = 2x + y;

    julia> g(x, y::Float64) = x + 2y;

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    ERROR: MethodError: g(::Float64, ::Float64) is ambiguous. Candidates:
      g(x, y::Float64) in Main at none:1
      g(x::Float64, y) in Main at none:1
     ...

Here the call ``g(2.0, 3.0)`` could be handled by either the
``g(Float64, Any)`` or the ``g(Any, Float64)`` method, and neither is
more specific than the other. In such cases, Julia raises a ``MethodError``
rather than arbitrarily picking a method. You can avoid method ambiguities
by specifying an appropriate method for the intersection case:

.. doctest:: unambiguous

    julia> g(x::Float64, y::Float64) = 2x + 2y;

    julia> g(x::Float64, y) = 2x + y;

    julia> g(x, y::Float64) = x + 2y;

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    10.0

It is recommended that the disambiguating method be defined first,
since otherwise the ambiguity exists, if transiently, until the more
specific method is defined.

.. _man-parametric-methods:

Parametric Methods
------------------

Method definitions can optionally have type parameters immediately after
the method name and before the parameter tuple:

.. doctest::

    julia> same_type{T}(x::T, y::T) = true;

    julia> same_type(x,y) = false;

The first method applies whenever both arguments are of the same
concrete type, regardless of what type that is, while the second method
acts as a catch-all, covering all other cases. Thus, overall, this
defines a boolean function that checks whether its two arguments are of
the same type:

.. doctest::

    julia> same_type(1, 2)
    true

    julia> same_type(1, 2.0)
    false

    julia> same_type(1.0, 2.0)
    true

    julia> same_type("foo", 2.0)
    false

    julia> same_type("foo", "bar")
    true

    julia> same_type(Int32(1), Int64(2))
    false

This kind of definition of function behavior by dispatch is quite common
— idiomatic, even — in Julia. Method type parameters are not restricted
to being used as the types of parameters: they can be used anywhere a
value would be in the signature of the function or body of the function.
Here's an example where the method type parameter ``T`` is used as the
type parameter to the parametric type ``Vector{T}`` in the method
signature:

.. doctest::

    julia> myappend{T}(v::Vector{T}, x::T) = [v..., x]
    myappend (generic function with 1 method)

    julia> myappend([1,2,3],4)
    4-element Array{Int64,1}:
     1
     2
     3
     4

    julia> myappend([1,2,3],2.5)
    ERROR: MethodError: no method matching myappend(::Array{Int64,1}, ::Float64)
    Closest candidates are:
      myappend{T}(::Array{T,1}, !Matched::T) at none:1
    ...

    julia> myappend([1.0,2.0,3.0],4.0)
    4-element Array{Float64,1}:
     1.0
     2.0
     3.0
     4.0

    julia> myappend([1.0,2.0,3.0],4)
    ERROR: MethodError: no method matching myappend(::Array{Float64,1}, ::Int64)
    Closest candidates are:
      myappend{T}(::Array{T,1}, !Matched::T) at none:1
    ...

As you can see, the type of the appended element must match the element
type of the vector it is appended to, or else a :exc:`MethodError` is raised.
In the following example, the method type parameter ``T`` is used as the
return value:

.. doctest::

    julia> mytypeof{T}(x::T) = T
    mytypeof (generic function with 1 method)

    julia> mytypeof(1)
    Int64

    julia> mytypeof(1.0)
    Float64

Just as you can put subtype constraints on type parameters in type
declarations (see :ref:`man-parametric-types`), you
can also constrain type parameters of methods::

    same_type_numeric{T<:Number}(x::T, y::T) = true
    same_type_numeric(x::Number, y::Number) = false

    julia> same_type_numeric(1, 2)
    true

    julia> same_type_numeric(1, 2.0)
    false

    julia> same_type_numeric(1.0, 2.0)
    true

    julia> same_type_numeric("foo", 2.0)
    no method same_type_numeric(String,Float64)

    julia> same_type_numeric("foo", "bar")
    no method same_type_numeric(String,String)

    julia> same_type_numeric(Int32(1), Int64(2))
    false

The ``same_type_numeric`` function behaves much like the ``same_type``
function defined above, but is only defined for pairs of numbers.

.. _man-vararg-fixedlen:

Parametrically-constrained Varargs methods
------------------------------------------

Function parameters can also be used to constrain the number of arguments that may be supplied to a "varargs" function (:ref:`man-varargs-functions`).  The notation ``Vararg{T,N}`` is used to indicate such a constraint.  For example:

.. doctest::

    julia> bar(a,b,x::Vararg{Any,2}) = (a,b,x);

    julia> bar(1,2,3)
    ERROR: MethodError: no method matching bar(::Int64, ::Int64, ::Int64)
    ...

    julia> bar(1,2,3,4)
    (1,2,(3,4))

    julia> bar(1,2,3,4,5)
    ERROR: MethodError: no method matching bar(::Int64, ::Int64, ::Int64, ::Int64, ::Int64)
    ...

More usefully, it is possible to constrain varargs methods by a parameter.  For example::

    function getindex{T,N}(A::AbstractArray{T,N}, indexes::Vararg{Number,N})

would be called only when the number of ``indexes`` matches the dimensionality of the array.

.. _man-note-on-optional-and-keyword-arguments:

Note on Optional and keyword Arguments
--------------------------------------

As mentioned briefly in :ref:`man-functions`, optional arguments are
implemented as syntax for multiple method definitions. For example,
this definition::

    f(a=1,b=2) = a+2b

translates to the following three methods::

    f(a,b) = a+2b
    f(a) = f(a,2)
    f() = f(1,2)

This means that calling ``f()`` is equivalent to calling ``f(1,2)``. In
this case the result is ``5``, because ``f(1,2)`` invokes the first
method of ``f`` above. However, this need not always be the case. If you
define a fourth method that is more specialized for integers::

    f(a::Int,b::Int) = a-2b

then the result of both ``f()`` and ``f(1,2)`` is ``-3``. In other words,
optional arguments are tied to a function, not to any specific method of
that function. It depends on the types of the optional arguments which
method is invoked. When optional arguments are defined in terms of a global
variable, the type of the optional argument may even change at run-time.

Keyword arguments behave quite differently from ordinary positional arguments.
In particular, they do not participate in method dispatch. Methods are
dispatched based only on positional arguments, with keyword arguments processed
after the matching method is identified.

Function-like objects
---------------------

Methods are associated with types, so it is possible to make any arbitrary
Julia object "callable" by adding methods to its type.
(Such "callable" objects are sometimes called "functors.")

For example, you can define a type that stores the coefficients of a
polynomial, but behaves like a function evaluating the polynomial::

    immutable Polynomial{R}
        coeffs::Vector{R}
    end

    function (p::Polynomial)(x)
        v = p.coeffs[end]
        for i = (length(p.coeffs)-1):-1:1
            v = v*x + p.coeffs[i]
        end
        return v
    end

Notice that the function is specified by type instead of by name.
In the function body, ``p`` will refer to the object that was called.
A ``Polynomial`` can be used as follows::

    julia> p = Polynomial([1,10,100])
    Polynomial{Int64}([1,10,100])

    julia> p(3)
    931

This mechanism is also the key to how type constructors and closures
(inner functions that refer to their surrounding environment) work
in Julia, discussed :ref:`later in the manual <constructors-and-conversion>`.

Empty generic functions
-----------------------

Occasionally it is useful to introduce a generic function without yet adding
methods.
This can be used to separate interface definitions from implementations.
It might also be done for the purpose of documentation or code readability.
The syntax for this is an empty ``function`` block without a tuple of
arguments::

    function emptyfunc
    end

.. [Clarke61] Arthur C. Clarke, *Profiles of the Future* (1961): Clarke's Third Law.

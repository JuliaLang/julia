.. _man-methods:

*********
 Methods  
*********

Recall from :ref:`man-functions` that a function is an object
that maps a tuple of arguments to a return value, or throws an exception
if no appropriate value can be returned. It is very common for the same
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
behaviors of its various method defintions. If the patchwork is well
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
<http://en.wikipedia.org/wiki/Multiple_dispatch>`_. Multiple
dispatch is particularly useful for mathematical code, where it makes
little sense to artificially deem the operations to "belong" to one
argument more than any of the others: does the addition operation in
``x + y`` belong to ``x`` any more than it does to ``y``? The
implementation of a mathematical operator generally depends on the types
of all of its arguments. Even beyond mathematical operations, however,
multiple dispatch ends up being a very powerful and convenient paradigm
for structuring and organizing programs.

.. [#] In C++ or Java, for example, in a method call like
  ``obj.meth(arg1,arg2)``, the object obj "receives" the method call and is
  implicitly passed to the method via the ``this`` keyword, rather then as an
  explicit method argument. When the current ``this`` object is the receiver of a
  method call, it can be omitted altogether, writing just ``meth(arg1,arg2)``,
  with this implied as the receiving object.


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
operator, introduced in the section on :ref:`man-composite-types`::

    f(x::Float64, y::Float64) = 2x + y

This function definition applies only to calls where ``x`` and ``y`` are
both values of type ``Float64``::

    julia> f(2.0, 3.0)
    7.0

Applying it to any other types of arguments will result in a "no method"
error::

    julia> f(2.0, 3)
    no method f(Float64,Int64)

    julia> f(float32(2.0), 3.0)
    no method f(Float32,Float64)

    julia> f(2.0, "3.0")
    no method f(Float64,ASCIIString)

    julia> f("2.0", "3.0")
    no method f(ASCIIString,ASCIIString)

As you can see, the arguments must be precisely of type ``Float64``.
Other numeric types, such as integers or 32-bit floating-point values,
are not automatically converted to 64-bit floating-point, nor are
strings parsed as numbers. Because ``Float64`` is a concrete type and
concrete types cannot be subclassed in Julia, such a definition can only
be applied to arguments that are exactly of type ``Float64``. It may
often be useful, however, to write more general methods where the
declared parameter types are abstract::

    f(x::Number, y::Number) = 2x - y

    julia> f(2.0, 3)
    1.0

This method definition applies to any pair of arguments that are
instances of ``Number``. They need not be of the same type, so long as
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
``Number`` — but with a different behavior specific to pairs of
``Float64`` values. If one of the arguments is a 64-bit float but the
other one is not, then the ``f(Float64,Float64)`` method cannot be
called and the more general ``f(Number,Number)`` method must be used::

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
from magic.[Clarke61]_

For non-numeric values, and for fewer or more than two arguments, the
function ``f`` remains undefined, and applying it will still result in a
"no method" error::

    julia> f("foo", 3)
    no method f(ASCIIString,Int64)

    julia> f()
    no method f()

You can easily see which methods exist for a function by entering the
function object itself in an interactive session::

    julia> f
    Methods for generic function f
    f(Float64,Float64)
    f(Number,Number)

This output tells us that ``f`` is a function object with two methods:
one taking two ``Float64`` arguments and one taking arguments of type
``Number``.

In the absence of a type declaration with ``::``, the type of a method
parameter is ``Any`` by default, meaning that it is unconstrained since
all values in Julia are instances of the abstract type ``Any``. Thus, we
can define a catch-all method for ``f`` like so::

    julia> f(x,y) = println("Whoa there, Nelly.")

    julia> f("foo", 1)
    Whoa there, Nelly.

This catch-all is less specific than any other possible method
definition for a pair of parameter values, so it is only be called on
pairs of arguments to which no other method definition applies.

Although it seems a simple concept, multiple dispatch on the types of
values is perhaps the single most powerful and central feature of the
Julia language. Core operations typically have dozens of methods::

    julia> +
    Methods for generic function +
    +(Real,Range{T<:Real}) at range.jl:136
    +(Real,Range1{T<:Real}) at range.jl:137
    +(Ranges{T<:Real},Real) at range.jl:138
    +(Ranges{T<:Real},Ranges{T<:Real}) at range.jl:150
    +(Bool,) at bool.jl:45
    +(Bool,Bool) at bool.jl:48
    +(Int64,Int64) at int.jl:224
    +(Int128,Int128) at int.jl:226
    +(Union(Array{Bool,N},SubArray{Bool,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)}),Union(Array{Bool,N},SubArray{Bool,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)})) at array.jl:902
    +{T<:Signed}(T<:Signed,T<:Signed) at int.jl:207
    +(Uint64,Uint64) at int.jl:225
    +(Uint128,Uint128) at int.jl:227
    +{T<:Unsigned}(T<:Unsigned,T<:Unsigned) at int.jl:211
    +(Float32,Float32) at float.jl:113
    +(Float64,Float64) at float.jl:114
    +(Complex{T<:Real},Complex{T<:Real}) at complex.jl:207
    +(Rational{T<:Integer},Rational{T<:Integer}) at rational.jl:101
    +(Bool,Union(Array{Bool,N},SubArray{Bool,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)})) at array.jl:896
    +(Union(Array{Bool,N},SubArray{Bool,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)}),Bool) at array.jl:899
    +(Char,Char) at char.jl:46
    +(Char,Int64) at char.jl:47
    +(Int64,Char) at char.jl:48
    +{T<:Number}(T<:Number,T<:Number) at promotion.jl:68
    +(Number,Number) at promotion.jl:40
    +() at operators.jl:30
    +(Number,) at operators.jl:36
    +(Any,Any,Any) at operators.jl:44
    +(Any,Any,Any,Any) at operators.jl:45
    +(Any,Any,Any,Any,Any) at operators.jl:46
    +(Any,Any,Any,Any...) at operators.jl:48
    +{T}(Ptr{T},Integer) at pointer.jl:52
    +(Integer,Ptr{T}) at pointer.jl:54
    +{T<:Number}(AbstractArray{T<:Number,N},) at abstractarray.jl:232
    +{S,T}(Union(Array{S,N},SubArray{S,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)}),Union(Array{T,N},SubArray{T,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)})) at array.jl:850
    +{T}(Number,Union(Array{T,N},SubArray{T,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)})) at array.jl:857
    +{T}(Union(Array{T,N},SubArray{T,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)}),Number) at array.jl:864
    +{S,T<:Real}(Union(Array{S,N},SubArray{S,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)}),Ranges{T<:Real}) at array.jl:872
    +{S<:Real,T}(Ranges{S<:Real},Union(Array{T,N},SubArray{T,N,A<:Array{T,N},I<:(Union(Int64,Range1{Int64},Range{Int64})...,)})) at array.jl:881
    +(BitArray{N},BitArray{N}) at bitarray.jl:922
    +(BitArray{N},Number) at bitarray.jl:923
    +(Number,BitArray{N}) at bitarray.jl:924
    +(BitArray{N},AbstractArray{T,N}) at bitarray.jl:986
    +(AbstractArray{T,N},BitArray{N}) at bitarray.jl:987
    +{Tv,Ti}(SparseMatrixCSC{Tv,Ti},SparseMatrixCSC{Tv,Ti}) at sparse.jl:536
    +(SparseMatrixCSC{Tv,Ti<:Integer},Union(Array{T,N},Number)) at sparse.jl:626
    +(Union(Array{T,N},Number),SparseMatrixCSC{Tv,Ti<:Integer}) at sparse.jl:627

Multiple dispatch together with the flexible parametric type system give
Julia its ability to abstractly express high-level algorithms decoupled
from implementation details, yet generate efficient, specialized code to
handle each case at run time.

Method Ambiguities
------------------

It is possible to define a set of function methods such that there is no
unique most specific method applicable to some combinations of
arguments::

    julia> g(x::Float64, y) = 2x + y

    julia> g(x, y::Float64) = x + 2y
    Warning: New definition g(Any,Float64) is ambiguous with g(Float64,Any).
             Make sure g(Float64,Float64) is defined first.

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    7.0

Here the call ``g(2.0, 3.0)`` could be handled by either the
``g(Float64, Any)`` or the ``g(Any, Float64)`` method, and neither is
more specific than the other. In such cases, Julia warns you about this
ambiguity, but allows you to proceed, arbitrarily picking a method. You
should avoid method ambiguities by specifying an appropriate method for
the intersection case::

    julia> g(x::Float64, y::Float64) = 2x + 2y

    julia> g(x::Float64, y) = 2x + y

    julia> g(x, y::Float64) = x + 2y

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    10.0

To suppress Julia's warning, the disambiguating method must be defined
first, since otherwise the ambiguity exists, if transiently, until the
more specific method is defined.

.. _man-parametric-methods:

Parametric Methods
------------------

Method definitions can optionally have type parameters immediately after
the method name and before the parameter tuple::

    same_type{T}(x::T, y::T) = true
    same_type(x,y) = false

The first method applies whenever both arguments are of the same
concrete type, regardless of what type that is, while the second method
acts as a catch-all, covering all other cases. Thus, overall, this
defines a boolean function that checks whether its two arguments are of
the same type::

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

    julia> same_type(int32(1), int64(2))
    false

This kind of definition of function behavior by dispatch is quite common
— idiomatic, even — in Julia. Method type parameters are not restricted
to being used as the types of parameters: they can be used anywhere a
value would be in the signature of the function or body of the function.
Here's an example where the method type parameter ``T`` is used as the
type parameter to the parametric type ``Vector{T}`` in the method
signature::

    julia> myappend{T}(v::Vector{T}, x::T) = [v..., x]

    julia> myappend([1,2,3],4)
    4-element Int64 Array:
    1
    2
    3
    4

    julia> myappend([1,2,3],2.5)
    no method myappend(Array{Int64,1},Float64)

    julia> myappend([1.0,2.0,3.0],4.0)
    [1.0,2.0,3.0,4.0]

    julia> myappend([1.0,2.0,3.0],4)
    no method myappend(Array{Float64,1},Int64)

As you can see, the type of the appended element must match the element
type of the vector it is appended to, or a "no method" error is raised.
In the following example, the method type parameter ``T`` is used as the
return value::

    julia> mytypeof{T}(x::T) = T

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
    no method same_type_numeric(ASCIIString,Float64)

    julia> same_type_numeric("foo", "bar")
    no method same_type_numeric(ASCIIString,ASCIIString)

    julia> same_type_numeric(int32(1), int64(2))
    false

The ``same_type_numeric`` function behaves much like the ``same_type``
function defined above, but is only defined for pairs of numbers.

Note on Optional and Named Arguments
------------------------------------

As mentioned briefly in :ref:`man-functions`, optional arguments are
implemented as syntax for multiple method definitions. For example,
this definition::

    f(a=1,b=2) = a+2b

translates to the following three methods::

    f(a,b) = a+2b
    f(a) = f(a,2)
    f() = f(1,2)

Named arguments behave quite differently from ordinary positional arguments.
In particular, they do not participate in method dispatch. Methods are
dispatched based only on positional arguments, with named arguments processed
after the matching method is identified.

.. [Clarke61] Arthur C. Clarke, *Profiles of the Future* (1961): Clarke's Third Law.


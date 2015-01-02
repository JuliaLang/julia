.. _man-style-guide:

.. currentmodule:: Base

*************
 Style Guide
*************

The following sections explain a few aspects of idiomatic Julia coding style.
None of these rules are absolute; they are only suggestions to help familiarize
you with the language and to help you choose among alternative designs.

Write functions, not just scripts
---------------------------------

Writing code as a series of steps at the top level is a quick way to get
started solving a problem, but you should try to divide a program into
functions as soon as possible. Functions are more reusable and testable,
and clarify what steps are being done and what their inputs and outputs are.
Furthermore, code inside functions tends to run much faster than top level
code, due to how Julia's compiler works.

It is also worth emphasizing that functions should take arguments, instead
of operating directly on global variables (aside from constants like :const:`pi`).

Avoid writing overly-specific types
-----------------------------------

Code should be as generic as possible. Instead of writing::

    convert(Complex{Float64}, x)

it's better to use available generic functions::

    complex(float(x))

The second version will convert ``x`` to an appropriate type, instead of
always the same type.

This style point is especially relevant to function arguments. For
example, don't declare an argument to be of type ``Int`` or ``Int32``
if it really could be any integer, expressed with the abstract type
``Integer``.  In fact, in many cases you can omit the argument type
altogether, unless it is needed to disambiguate from other method
definitions, since a :exc:`MethodError` will be thrown anyway if a type
is passed that does not support any of the requisite operations.
(This is known as `duck typing <http://en.wikipedia.org/wiki/Duck_typing>`_.)

For example, consider the following definitions of a function
``addone`` that returns one plus its argument::

    addone(x::Int) = x + 1             # works only for Int
    addone(x::Integer) = x + one(x)    # any integer type
    addone(x::Number) = x + one(x)     # any numeric type
    addone(x) = x + one(x)             # any type supporting + and one

The last definition of ``addone`` handles any type supporting
:func:`one` (which returns 1 in the same type as ``x``, which
avoids unwanted type promotion) and the :obj:`+` function with those
arguments.  The key thing to realize is that there is *no performance
penalty* to defining *only* the general ``addone(x) = x + one(x)``,
because Julia will automatically compile specialized versions as
needed.  For example, the first time you call ``addone(12)``, Julia
will automatically compile a specialized ``addone`` function for
``x::Int`` arguments, with the call to :func:`one` replaced by its inlined
value ``1``.  Therefore, the first three definitions of ``addone``
above are completely redundant.

Handle excess argument diversity in the caller
----------------------------------------------

Instead of::

    function foo(x, y)
        x = int(x); y = int(y)
        ...
    end
    foo(x, y)

use::

    function foo(x::Int, y::Int)
        ...
    end
    foo(int(x), int(y))

This is better style because ``foo`` does not really accept numbers of all
types; it really needs ``Int`` s.

One issue here is that if a function inherently requires integers, it
might be better to force the caller to decide how non-integers should
be converted (e.g. floor or ceiling). Another issue is that declaring
more specific types leaves more "space" for future method definitions.

Append `!` to names of functions that modify their arguments
------------------------------------------------------------

Instead of::

    function double{T<:Number}(a::AbstractArray{T})
        for i = 1:endof(a); a[i] *= 2; end
	a
    end

use::

    function double!{T<:Number}(a::AbstractArray{T})
        for i = 1:endof(a); a[i] *= 2; end
	a
    end

The Julia standard library uses this convention throughout and
contains examples of functions with both copying and modifying forms
(e.g., :func:`sort` and :func:`sort!`), and others which are just modifying
(e.g., :func:`push!`, :func:`pop!`, :func:`splice!`).  It is typical for
such functions to also return the modified array for convenience.

Avoid strange type Unions
-------------------------

Types such as ``Union(Function,AbstractString)`` are often a sign that some design
could be cleaner.

Try to avoid nullable fields
----------------------------

When using ``x::Union(Nothing,T)``, ask whether the option for ``x`` to be
``nothing`` is really necessary. Here are some alternatives to consider:

- Find a safe default value to initialize ``x`` with
- Introduce another type that lacks ``x``
- If there are many fields like ``x``, store them in a dictionary
- Determine whether there is a simple rule for when ``x`` is ``nothing``.
  For example, often the field will start as ``nothing`` but get initialized at
  some well-defined point. In that case, consider leaving it undefined at first.

Avoid elaborate container types
-------------------------------

It is usually not much help to construct arrays like the following::

    a = Array(Union(Int,AbstractString,Tuple,Array), n)

In this case :func:`cell(n) <cell>` is better. It is also more helpful to the compiler
to annotate specific uses (e.g. ``a[i]::Int``) than to try to pack many
alternatives into one type.

Use naming conventions consistent with Julia's ``base/``
--------------------------------------------------------

- modules and type names use capitalization and camel case:
  ``module SparseMatrix``,  ``immutable UnitRange``.
- functions are lowercase (:func:`maximum`, :func:`convert`) and,
  when readable, with multiple words squashed together (:func:`isequal`, :func:`haskey`).
  When necessary, use underscores as word separators.
  Underscores are also used to indicate a combination of
  concepts (:func:`remotecall_fetch` as a more efficient implementation
  of ``remotecall(fetch(...))``) or as modifiers (:func:`sum_kbn`).
- conciseness is valued, but avoid abbreviation
  (:func:`indexin` rather than ``indxin()``) as it becomes difficult to
  remember whether and how particular words are abbreviated.

If a function name requires multiple words, consider whether it might
represent more than one concept and might be better split into pieces.

Don't overuse try-catch
-----------------------

It is better to avoid errors than to rely on catching them.

Don't parenthesize conditions
-----------------------------

Julia doesn't require parens around conditions in ``if`` and ``while``.
Write::

    if a == b

instead of::

    if (a == b)

Don't overuse ...
-----------------

Splicing function arguments can be addictive. Instead of ``[a..., b...]``,
use simply ``[a, b]``, which already concatenates arrays.
:func:`collect(a) <collect>` is better than ``[a...]``, but since ``a`` is already iterable
it is often even better to leave it alone, and not convert it to an array.

Don't use unnecessary static parameters
---------------------------------------

A function signature::

    foo{T<:Real}(x::T) = ...

should be written as::

    foo(x::Real) = ...

instead, especially if ``T`` is not used in the function body.
Even if ``T`` is used, it can be replaced with :func:`typeof(x) <typeof>` if convenient.
There is no performance difference.
Note that this is not a general caution against static parameters, just
against uses where they are not needed.

Note also that container types, specifically may need type parameters in
function calls. See the FAQ :ref:`man-abstract-container-type`
for more information.

Avoid confusion about whether something is an instance or a type
----------------------------------------------------------------

Sets of definitions like the following are confusing::

    foo(::Type{MyType}) = ...
    foo(::MyType) = foo(MyType)

Decide whether the concept in question will be written as ``MyType`` or
``MyType()``, and stick to it.

The preferred style is to use instances by default, and only add
methods involving ``Type{MyType}`` later if they become necessary
to solve some problem.

If a type is effectively an enumeration, it should be defined as a single
(ideally ``immutable``) type, with the enumeration values being instances
of it. Constructors and conversions can check whether values are valid.
This design is preferred over making the enumeration an abstract type,
with the "values" as subtypes.

Don't overuse macros
--------------------

Be aware of when a macro could really be a function instead.

Calling :func:`eval` inside a macro is a particularly dangerous warning sign;
it means the macro will only work when called at the top level. If such
a macro is written as a function instead, it will naturally have access
to the run-time values it needs.

Don't expose unsafe operations at the interface level
-----------------------------------------------------

If you have a type that uses a native pointer::

    type NativeType
        p::Ptr{UInt8}
        ...
    end

don't write definitions like the following::

    getindex(x::NativeType, i) = unsafe_load(x.p, i)

The problem is that users of this type can write ``x[i]`` without realizing
that the operation is unsafe, and then be susceptible to memory bugs.

Such a function should either check the operation to ensure it is safe, or
have ``unsafe`` somewhere in its name to alert callers.

Don't overload methods of base container types
----------------------------------------------

It is possible to write definitions like the following::

    show(io::IO, v::Vector{MyType}) = ...

This would provide custom showing of vectors with a specific new element type.
While tempting, this should be avoided. The trouble is that users will expect
a well-known type like :func:`Vector` to behave in a certain way, and overly
customizing its behavior can make it harder to work with.

Be careful with type equality
-----------------------------

You generally want to use :func:`isa` and ``<:`` (:func:`issubtype`) for testing types,
not ``==``. Checking types for exact equality typically only makes sense
when comparing to a known concrete type (e.g. ``T == Float64``), or if you
*really, really* know what you're doing.

Do not write ``x->f(x)``
------------------------

Since higher-order functions are often called with anonymous functions, it
is easy to conclude that this is desirable or even necessary.
But any function can be passed directly, without being "wrapped" in an
anonymous function. Instead of writing ``map(x->f(x), a)``, write
:func:`map(f, a) <map>`.


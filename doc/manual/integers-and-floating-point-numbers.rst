.. _man-integers-and-floating-point-numbers:

*************************************
 Integers and Floating-Point Numbers  
*************************************

Integers and floating-point values are the basic building blocks of
arithmetic and computation. Built-in representations of such values are
called numeric primitives, while representations of integers and
floating-point numbers as immediate values in code are known as numeric
literals. For example, ``1`` is an integer literal, while ``1.0`` is a
floating-point literal; their binary in-memory representations as
objects are numeric primitives. Julia provides a broad range of
primitive numeric types, and a full complement of arithmetic and bitwise
operators as well as standard mathematical functions are defined over
them. The following are Julia's primitive numeric types:

-  **Integer types:**

   -  ``Int8`` — signed 8-bit integers ranging from −2^7 to 2^7 − 1.
   -  ``Uint8`` — unsigned 8-bit integers ranging from 0 to 2^8 − 1.
   -  ``Int16`` — signed 16-bit integers ranging from −2^15 to 2^15 − 1.
   -  ``Uint16`` — unsigned 16-bit integers ranging from 0 to 2^16 − 1.
   -  ``Int32`` — signed 32-bit integers ranging from −2^31 to 2^31 − 1.
   -  ``Uint32`` — unsigned 32-bit integers ranging from 0 to 2^32 − 1.
   -  ``Int64`` — signed 64-bit integers ranging from −2^63 to 2^63 − 1.
   -  ``Uint64`` — unsigned 64-bit integers ranging from 0 to 2^64 − 1.
   -  ``Bool`` — either ``true`` or ``false``, which correspond
      numerically to 1 and 0.
   -  ``Char`` — a 32-bit numeric type representing a `Unicode
      character <http://en.wikipedia.org/wiki/Unicode>`_ (see
      :ref:`man-strings` for more details).

-  **Floating-point types:**

   -  ``Float32`` — `IEEE 754 32-bit floating-point
      numbers <http://en.wikipedia.org/wiki/Single_precision_floating-point_format>`_.
   -  ``Float64`` — `IEEE 754 64-bit floating-point
      numbers <http://en.wikipedia.org/wiki/Double_precision_floating-point_format>`_.

Additionally, full support for :ref:`man-complex-and-rational-numbers` is built on top of these
primitive numeric types. All numeric types interoperate naturally
without explicit casting, thanks to a flexible type promotion system.
Moreover, this promotion system, detailed in :ref:`man-conversion-and-promotion`, is user-extensible, so
user-defined numeric types can be made to interoperate just as naturally
as built-in types.

Integers
--------

Literal integers are represented in the standard manner::

    julia> 1
    1

    julia> 1234
    1234

The default type for an integer literal depends on whether the target
system has a 32-bit architecture or a 64-bit architecture::

    # 32-bit system:
    julia> typeof(1)
    Int32

    # 64-bit system:
    julia> typeof(1)
    Int64

The type ``Int`` is an alias for the system-native integer type::

    # 32-bit system:
    julia> Int
    Int32

    # 64-bit system:
    julia> Int
    Int64

Similarly, ``Uint`` is an alias for the system-native unsigned integer
type::

    # 32-bit system:
    julia> Uint
    Uint32

    # 64-bit system:
    julia> Uint
    Uint64

Larger integer literals that cannot be represented using only 32 bits
but can be represented in 64 bits always create 64-bit integers,
regardless of the system type::

    # 32-bit or 64-bit system:
    julia> typeof(3000000000)
    Int64

Unsigned integers are input and output using the ``0x`` prefix and
hexadecimal (base 16) digits ``0-9a-f`` (you can also use ``A-F`` for
input). The size of the unsigned value is determined by the number of
hex digits used::

    julia> 0x1
    0x01

    julia> typeof(ans)
    Uint8

    julia> 0x123
    0x0123

    julia> typeof(ans)
    Uint16

    julia> 0x1234567
    0x01234567

    julia> typeof(ans)
    Uint32

    julia> 0x123456789abcdef
    0x0123456789abcdef

    julia> typeof(ans)
    Uint64

This behavior is based on the observation that when one uses unsigned
hex literals for integer values, one typically is using them to
represent a fixed numeric byte sequence, rather than just an integer
value.

The minimum and maximum representable values of primitive numeric types
such as integers are given by the ``typemin`` and ``typemax`` functions::

    julia> (typemin(Int32), typemax(Int32))
    (-2147483648,2147483647)

    julia> for T = {Int8,Int16,Int32,Int64,Uint8,Uint16,Uint32,Uint64}
             println("$(lpad(T,6)): [$(typemin(T)),$(typemax(T))]")
           end
      Int8: [-128,127]
     Int16: [-32768,32767]
     Int32: [-2147483648,2147483647]
     Int64: [-9223372036854775808,9223372036854775807]
     Uint8: [0x00,0xff]
    Uint16: [0x0000,0xffff]
    Uint32: [0x00000000,0xffffffff]
    Uint64: [0x0000000000000000,0xffffffffffffffff]

The values returned by ``typemin`` and ``typemax`` are always of the
given argument type. The above expression uses several features we have
yet to introduce, including :ref:`for loops <man-loops>`,
:ref:`man-strings`, and :ref:`man-string-interpolation`,
but should be easy enough to understand for people with some programming experience.

Floating-Point Numbers
----------------------

Literal floating-point numbers are represented in the standard formats::

    julia> 1.0
    1.0

    julia> 1.
    1.0

    julia> 0.5
    0.5

    julia> .5
    0.5

    julia> -1.23
    -1.23

    julia> 1e10
    1e+10

    julia> 2.5e-4
    0.00025

The above results are all ``Float64`` values. There is no literal format
for ``Float32``, but you can convert values to ``Float32`` easily::

    julia> float32(-1.5)
    -1.5

    julia> typeof(ans)
    Float32

There are three specified standard floating-point values that do not
correspond to a point on the real number line:

-  ``Inf`` — positive infinity — a value greater than all finite
   floating-point values
-  ``-Inf`` — negative infinity — a value less than all finite
   floating-point values
-  ``NaN`` — not a number — a value incomparable to all floating-point
   values (including itself).

For further discussion of how these non-finite floating-point values are
ordered with respect to each other and other floats, see
:ref:`man-numeric-comparisons`. By the
`IEEE 754 standard <http://en.wikipedia.org/wiki/IEEE_754-2008>`_, these
floating-point values are the results of certain arithmetic operations::

    julia> 1/0
    Inf

    julia> -5/0
    -Inf

    julia> 0.000001/0
    Inf

    julia> 0/0
    NaN

    julia> 500 + Inf
    Inf

    julia> 500 - Inf
    -Inf

    julia> Inf + Inf
    Inf

    julia> Inf - Inf
    NaN

    julia> Inf/Inf
    NaN

The ``typemin`` and ``typemax`` functions also apply to floating-point
types::

    julia> (typemin(Float32),typemax(Float32))
    (-Inf,Inf)

    julia> (typemin(Float64),typemax(Float64))
    (-Inf,Inf)

Note that ``Float32`` values ``NaN``, ``Inf`` and ``-Inf`` are shown
identically to their ``Float64`` counterparts.

Floating-point types also support the ``eps`` function, which gives the
distance between ``1.0`` and the next larger representable
floating-point value::

    julia> eps(Float32)
    1.192092896e-07

    julia> eps(Float64)
    2.22044604925031308e-16

These values are ``2^-23`` and ``2^-52`` as ``Float32`` and ``Float64``
values, respectively. The ``eps`` function can also take a
floating-point value as an argument, and gives the absolute difference
between that value and the next representable floating point value. That
is, ``eps(x)`` yields a value of the same type as ``x`` such that
``x + eps(x)`` is the next representable floating-point value larger
than ``x``::

    julia> eps(1.0)
    2.22044604925031308e-16

    julia> eps(1000.)
    1.13686837721616030e-13

    julia> eps(1e-27)
    1.79366203433576585e-43

    julia> eps(0.0)
    4.94065645841246544e-324

As you can see, the distance to the next larger representable
floating-point value is smaller for smaller values and larger for larger
values. In other words, the representable floating-point numbers are
densest in the real number line near zero, and grow sparser
exponentially as one moves farther away from zero. By definition,
``eps(1.0)`` is the same as ``eps(Float64)`` since ``1.0`` is a 64-bit
floating-point value.

.. raw:: html

   <!-- ### Exercises

   - Define an integer variable with value equal to 1. Convert it into a Float64. [Answer](answer_int2float)

   - Round off 3.8 to the nearest integer. [Answer](answer_roundoff) -->

Background and References
~~~~~~~~~~~~~~~~~~~~~~~~~

For a brief but lucid presentation of how floating-point numbers are
represented, see John D. Cook's
`article <http://www.johndcook.com/blog/2009/04/06/anatomy-of-a-floating-point-number/>`_
on the subject as well as his
`introduction <http://www.johndcook.com/blog/2009/04/06/numbers-are-a-leaky-abstraction/>`_
to some of the issues arising from how this representation differs in
behavior from the idealized abstraction of real numbers. For an
excellent, in-depth discussion of floating-point numbers and issues of
numerical accuracy encountered when computing with them, see David
Goldberg's paper `What Every Computer Scientist Should Know About
Floating-Point
Arithmetic <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.102.244&rep=rep1&type=pdf>`_.
For even more extensive documentation of the history of, rationale for,
and issues with floating-point numbers, as well as discussion of many
other topics in numerical computing, see the `collected
writings <http://www.cs.berkeley.edu/~wkahan/>`_ of `William
Kahan <http://en.wikipedia.org/wiki/William_Kahan>`_, commonly known as
the "Father of Floating-Point". Of particular interest may be `An
Interview with the Old Man of
Floating-Point <http://www.cs.berkeley.edu/~wkahan/ieee754status/754story.html>`_.

.. _man-numeric-literal-coefficients:

Numeric Literal Coefficients
----------------------------

To make common numeric formulas and expressions clearer, Julia allows
variables to be immediately preceded by a numeric literal, implying
multiplication. This makes writing polynomial expressions much cleaner::

    julia> x = 3
    3

    julia> 2x^2 - 3x + 1
    10

    julia> 1.5x^2 - .5x + 1
    13.0

It also makes writing exponential functions more elegant::

    julia> 2^2x
    64

You can also use numeric literals as coefficients to parenthesized
expressions::

    julia> 2(x-1)^2 - 3(x-1) + 1
    3

Additionally, parenthesized expressions can be used as coefficients to
variables, implying multiplication of the expression by the variable::

    julia> (x-1)x
    6

Neither juxtaposition of two parenthesized expressions, nor placing a
variable before a parenthesized expression, however, can be used to
imply multiplication::

    julia> (x-1)(x+1)
    type error: apply: expected Function, got Int64

    julia> x(x+1)
    type error: apply: expected Function, got Int64

Both of these expressions are interpreted as function application: any
expression that is not a numeric literal, when immediately followed by a
parenthetical, is interpreted as a function applied to the values in
parentheses (see :ref:`man-functions` for more about functions).
Thus, in both of these cases, an error occurs since the left-hand value
is not a function.

The above syntactic enhancements significantly reduce the visual noise
incurred when writing common mathematical formulae. Note that no
whitespace may come between a numeric literal coefficient and the
identifier or parenthesized expression which it multiplies.

Syntax Conflicts
~~~~~~~~~~~~~~~~

Juxtaposed literal coefficient syntax conflicts with two numeric literal
syntaxes: hexadecimal integer literals and engineering notation for
floating-point literals. Here are some situations where syntactic
conflicts arise:

-  The hexadecimal integer literal expression ``0xff`` could be
   interpreted as the numeric literal ``0`` multiplied by the variable
   ``xff``.
-  The floating-point literal expression ``1e10`` could be interpreted
   as the numeric literal ``1`` multiplied by the variable ``e10``, and
   similarly with the equivalent ``E`` form.

In both cases, we resolve the ambiguity in favor of interpretation as a
numeric literals:

-  Expressions starting with ``0x`` are always hexadecimal literals.
-  Expressions starting with a numeric literal followed by ``e`` or
   ``E`` are always floating-point literals.


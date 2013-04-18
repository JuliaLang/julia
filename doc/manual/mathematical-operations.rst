.. _man-mathematical-operations:

**************************************************
 Mathematical Operations and Elementary Functions 
**************************************************

Julia provides a complete collection of basic arithmetic and bitwise
operators across all of its numeric primitive types, as well as
providing portable, efficient implementations of a comprehensive
collection of standard mathematical functions.

Arithmetic Operators
--------------------

The following `arithmetic operators
<http://en.wikipedia.org/wiki/Arithmetic#Arithmetic_operations>`_
are supported on all primitive numeric types:

==========  ============== ======================================
Expression  Name           Description
==========  ============== ======================================
``+x``      unary plus     the identity operation
``-x``      unary minus    maps values to their additive inverses
``x + y``   binary plus    performs addition
``x - y``   binary minus   performs subtraction
``x * y``   times          performs multiplication
``x / y``   divide         performs division
``x \ y``   inverse divide equivalent to ``y / x``
``x ^ y``   power          raises ``x`` to the ``y``\ th power
``x % y``   remainder      equivalent to ``rem(x,y)``
==========  ============== ======================================

as well as the negation on ``Bool`` types:

==========  ============== ============================================
Expression  Name           Description
==========  ============== ============================================
``!x``      negation       changes ``true`` to ``false`` and vice versa
==========  ============== ============================================

Julia's promotion system makes arithmetic operations on mixtures of argument
types "just work" naturally and automatically. See :ref:`man-conversion-and-promotion`
for details of the promotion system.

Here are some simple examples using arithmetic operators::

    julia> 1 + 2 + 3
    6

    julia> 1 - 2
    -1

    julia> 3*2/12
    0.5

(By convention, we tend to space less tightly binding operators less
tightly, but there are no syntactic constraints.)

Bitwise Operators
-----------------

The following `bitwise
operators <http://en.wikipedia.org/wiki/Bitwise_operation#Bitwise_operators>`_
are supported on all primitive integer types:

===========  =========================================================================
Expression   Name        
===========  =========================================================================
``~x``       bitwise not
``x & y``    bitwise and
``x | y``    bitwise or
``x $ y``    bitwise xor (exclusive or)
``x >>> y``  `logical shift <http://en.wikipedia.org/wiki/Logical_shift>`_ right
``x >> y``   `arithmetic shift <http://en.wikipedia.org/wiki/Arithmetic_shift>`_ right
``x << y``   logical/arithmetic shift left
===========  =========================================================================

Here are some examples with bitwise operators::

    julia> ~123
    -124

    julia> 123 & 234
    106

    julia> 123 | 234
    251

    julia> 123 $ 234
    145

    julia> ~uint32(123)
    0xffffff84

    julia> ~uint8(123)
    0x84

Updating operators
------------------
Every binary arithmetic and bitwise operator also has an updating
version that assigns the result of the operation back into its left
operand. The updating version of the binary operator is formed by placing a
``=`` immediately after the operator. For example, writing ``x += 3`` is
equivalent to writing ``x = x + 3``::

      julia> x = 1
      1

      julia> x += 3
      4

      julia> x
      4

The updating versions of all the binary arithmetic and bitwise operators
are::

    +=  -=  *=  /=  \=  %=  ^=  &=  |=  $=  >>>=  >>=  <<=


.. _man-numeric-comparisons:

Numeric Comparisons
-------------------

Standard comparison operations are defined for all the primitive numeric
types:

======== ========================
Operator Name
======== ========================
``==``   equality
``!=``   inequality
``<``    less than
``<=``   less than or equal to
``>``    greater than
``>=``   greater than or equal to
======== ========================

Here are some simple examples::

    julia> 1 == 1
    true

    julia> 1 == 2
    false

    julia> 1 != 2
    true

    julia> 1 == 1.0
    true

    julia> 1 < 2
    true

    julia> 1.0 > 3
    false

    julia> 1 >= 1.0
    true

    julia> -1 <= 1
    true

    julia> -1 <= -1
    true

    julia> -1 <= -2
    false

    julia> 3 < -0.5
    false

Integers are compared in the standard manner — by comparison of bits.
Floating-point numbers are compared according to the `IEEE 754
standard <http://en.wikipedia.org/wiki/IEEE_754-2008>`_:

-  Finite numbers are ordered in the usual manner.
-  Positive zero is equal but not greater than negative zero.
-  ``Inf`` is equal to itself and greater than everything else except ``NaN``.
-  ``-Inf`` is equal to itself and less then everything else except ``NaN``.
-  ``NaN`` is not equal to, not less than, and not greater than anything,
   including itself.

The last point is potentially surprising and thus worth noting::

    julia> NaN == NaN
    false

    julia> NaN != NaN
    true

    julia> NaN < NaN
    false

    julia> NaN > NaN
    false

and can cause especial headaches with :ref:`Arrays`::

    julia> [1 NaN] == [1 NaN]
    false

Julia provides additional functions to test numbers for special values,
which can be useful in situations like hash key comparisons::

================= ==================================
Function          Tests if
================= ==================================
``isequal(x, y)`` ``x`` and ``y`` are equal in value
``isfinite(x)``   ``x`` is a finite number
``isinf(x)``      ``x`` is infinite
``isnan(x)``      ``x`` is not a number
================= ==================================

``isequal`` considers ``NaN``\ s of the same type to be equal to each other::

    julia> isequal(NaN,NaN)
    true

    julia> isequal([1 NaN], [1 NaN])
    true
    
    julia> isequal(NaN,NaN32)
    false

Mixed-type comparisons between signed integers, unsigned integers, and
floats can be very tricky. A great deal of care has been taken to ensure
that Julia does them correctly.

Chaining comparisons
~~~~~~~~~~~~~~~~~~~~

Unlike most languages, with the `notable exception of
Python <http://en.wikipedia.org/wiki/Python_syntax_and_semantics#Comparison_operators>`_,
comparisons can be arbitrarily chained::

    julia> 1 < 2 <= 2 < 3 == 3 > 2 >= 1 == 1 < 3 != 5
    true

Chaining comparisons is often quite convenient in numerical code.
Chained comparisons use the ``&&`` operator for scalar comparisons,
and the ``&`` operator for element-wise comparisons, which allows them to
work on arrays. For example, ``0 .< A .< 1`` gives a boolean array whose
entries are true where the corresponding elements of ``A`` are between 0
and 1.

Note the evaluation behavior of chained comparisons::

    v(x) = (println(x); x)

    julia> v(1) < v(2) <= v(3)
    2
    1
    3
    true

    julia> v(1) > v(2) <= v(3)
    2
    1
    false

The middle expression is only evaluated once, rather than twice as it
would be if the expression were written as
``v(1) < v(2) && v(2) <= v(3)``. However, the order of evaluations in a
chained comparison is undefined. It is strongly recommended not to use
expressions with side effects (such as printing) in chained comparisons.
If side effects are required, the short-circuit ``&&`` operator should
be used explicitly (see :ref:`man-short-circuit-evaluation`).

.. _man-elementary-functions:

Elementary Functions
--------------------

Julia provides a comprehensive collection of mathematical functions and
operators. These mathematical operations are defined over as broad a
class of numerical values as permit sensible definitions, including
integers, floating-point numbers, rationals, and complexes, wherever
such definitions make sense.

Rounding functions
~~~~~~~~~~~~~~~~~~

============= ==================================  =================
Function      Description                         Return type
============= ==================================  =================
``round(x)``  round ``x`` to the nearest integer  ``FloatingPoint``
``iround(x)`` round ``x`` to the nearest integer  ``Integer``
``floor(x)``  round ``x`` towards ``-Inf``        ``FloatingPoint``
``ifloor(x)`` round ``x`` towards ``-Inf``        ``Integer``
``ceil(x)``   round ``x`` towards ``+Inf``        ``FloatingPoint``
``iceil(x)``  round ``x`` towards ``+Inf``        ``Integer``
``trunc(x)``  round ``x`` towards zero            ``FloatingPoint``
``itrunc(x)`` round ``x`` towards zero            ``Integer``
============= ==================================  =================

Division functions
~~~~~~~~~~~~~~~~~~

=============== =======================================================================
Function        Description
=============== =======================================================================
``div(x,y)``    truncated division; quotient rounded towards zero
``fld(x,y)``    floored division; quotient rounded towards ``-Inf``
``rem(x,y)``    remainder; satisfies ``x == div(x,y)*y + rem(x,y)``; sign matches ``x``
``mod(x,y)``    modulus; satisfies ``x == fld(x,y)*y + mod(x,y)``; sign matches ``y``
``gcd(x,y...)`` greatest common divisor of ``x``, ``y``,...; sign matches ``x``
``lcm(x,y...)`` least common multiple of ``x``, ``y``,...; sign matches ``x``
=============== =======================================================================

Sign and absolute value functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

================= ===========================================================
Function          Description
================= ===========================================================
``abs(x)``        a positive value with the magnitude of ``x``
``abs2(x)``       the squared magnitude of ``x``
``sign(x)``       indicates the sign of ``x``, returning -1, 0, or +1
``signbit(x)``    indicates whether the sign bit is on (1) or off (0)
``copysign(x,y)`` a value with the magnitude of ``x`` and the sign of ``y``
``flipsign(x,y)`` a value with the magnitude of ``x`` and the sign of ``x*y``
================= ===========================================================

Powers, logs and roots
~~~~~~~~~~~~~~~~~~~~~~

============== ==============================================================================
Function       Description
============== ==============================================================================
``sqrt(x)``    the square root of ``x``
``cbrt(x)``    the cube root of ``x``
``hypot(x,y)`` hypotenuse of right-angled triangle with other sides of length ``x`` and ``y``
``exp(x)``     the natural exponential function at ``x``
``expm1(x)``   accurate ``exp(x)-1`` for ``x`` near zero
``ldexp(x,n)`` ``x*2^n`` computed efficiently for integer values of ``n``
``log(x)``     the natural logarithm of ``x``
``log(b,x)``   the base ``b`` logarithm of ``x``
``log2(x)``    the base 2 logarithm of ``x``
``log10(x)``   the base 10 logarithm of ``x``
``log1p(x)``   accurate ``log(1+x)`` for ``x`` near zero
``logb(x)``    returns the binary exponent of ``x``
``erf(x)``     the `error function <http://en.wikipedia.org/wiki/Error_function>`_ at ``x``
``erfc(x)``    the complementary error function ``1-erf(x)``
``gamma(x)``   the `gamma function <http://en.wikipedia.org/wiki/Gamma_function>`_ at ``x``
``lgamma(x)``  accurate ``log(gamma(x))`` for large ``x``
============== ==============================================================================

For an overview of why functions like ``hypot``, ``expm1``, ``log1p``,
and ``erfc`` are necessary and useful, see John D. Cook's excellent pair
of blog posts on the subject: `expm1, log1p,
erfc <http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/>`_,
and
`hypot <http://www.johndcook.com/blog/2010/06/02/whats-so-hard-about-finding-a-hypotenuse/>`_.

Trigonometric and hyperbolic functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All the standard trigonometric and hyperbolic functions are also defined::

    sin    cos    tan    cot    sec    csc
    sinh   cosh   tanh   coth   sech   csch
    asin   acos   atan   acot   asec   acsc
    acoth  asech  acsch  sinc   cosc   atan2

These are all single-argument functions, with the exception of
`atan2 <http://en.wikipedia.org/wiki/Atan2>`_, which gives the angle
in `radians <http://en.wikipedia.org/wiki/Radian>`_ between the *x*-axis
and the point specified by its arguments, interpreted as *x* and *y*
coordinates.

In order to compute trigonometric functions with degrees
instead of radians, suffix the function with ``d``. For example, ``sind(x)``
computes the sine of ``x`` where ``x`` is specified in degrees.
The complete list of trigonometric functions with degree variants is::

    sind   cosd   tand   cotd   secd   cscd
    asind  acosd  atand  acotd  asecd  acscd


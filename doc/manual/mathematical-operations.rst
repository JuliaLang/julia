.. _man-mathematical-operations:

.. currentmodule:: Base

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

Here are some simple examples using arithmetic operators:

.. doctest::

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

Here are some examples with bitwise operators:

.. doctest::

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

    +=  -=  *=  /=  \=  ÷=  %=  ^=  &=  |=  $=  >>>=  >>=  <<=


.. note::
   An updating operator rebinds the variable on the left-hand side.
   As a result, the type of the variable may change.
   
   .. doctest::

      julia> x = 0x01; typeof(x)
      UInt8

      julia> x *= 2 #Same as x = x * 2
      2
      
      julia> isa(x, Int)
      true

.. _man-numeric-comparisons:

Numeric Comparisons
-------------------

Standard comparison operations are defined for all the primitive numeric
types:

=================== ========================
Operator            Name
=================== ========================
:obj:`==`           equality
:obj:`\!=` :obj:`≠` inequality
:obj:`<`            less than
:obj:`<=` :obj:`≤`  less than or equal to
:obj:`>`            greater than
:obj:`>=` :obj:`≥`  greater than or equal to
=================== ========================

Here are some simple examples:

.. doctest::

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

The last point is potentially surprising and thus worth noting:

.. doctest::

    julia> NaN == NaN
    false

    julia> NaN != NaN
    true

    julia> NaN < NaN
    false

    julia> NaN > NaN
    false

and can cause especial headaches with :ref:`Arrays <man-arrays>`:

.. doctest::

    julia> [1 NaN] == [1 NaN]
    false

Julia provides additional functions to test numbers for special values,
which can be useful in situations like hash key comparisons:

=============================== ==================================
Function                        Tests if
=============================== ==================================
:func:`isequal(x, y) <isequal>` ``x`` and ``y`` are identical
:func:`isfinite(x) <isfinite>`  ``x`` is a finite number
:func:`isinf(x) <isinf>`        ``x`` is infinite
:func:`isnan(x) <isnan>`        ``x`` is not a number
=============================== ==================================

:func:`isequal` considers ``NaN``\ s equal to each other:

.. doctest::

    julia> isequal(NaN,NaN)
    true

    julia> isequal([1 NaN], [1 NaN])
    true
    
    julia> isequal(NaN,NaN32)
    true

:func:`isequal` can also be used to distinguish signed zeros:

.. doctest::

    julia> -0.0 == 0.0
    true

    julia> isequal(-0.0, 0.0)
    false

Mixed-type comparisons between signed integers, unsigned integers, and
floats can be tricky. A great deal of care has been taken to ensure
that Julia does them correctly.

For other types, :func:`isequal` defaults to calling :func:`==`, so if you want to
define equality for your own types then you only need to add a :func:`==`
method.  If you define your own equality function, you should probably
define a corresponding :func:`hash` method to ensure that `isequal(x,y)`
implies `hash(x) == hash(y)`.

Chaining comparisons
~~~~~~~~~~~~~~~~~~~~

Unlike most languages, with the `notable exception of
Python <http://en.wikipedia.org/wiki/Python_syntax_and_semantics#Comparison_operators>`_,
comparisons can be arbitrarily chained:

.. doctest::

    julia> 1 < 2 <= 2 < 3 == 3 > 2 >= 1 == 1 < 3 != 5
    true

Chaining comparisons is often quite convenient in numerical code.
Chained comparisons use the :obj:`&&` operator for scalar comparisons,
and the :obj:`&` operator for elementwise comparisons, which allows them to
work on arrays. For example, ``0 .< A .< 1`` gives a boolean array whose
entries are true where the corresponding elements of ``A`` are between 0
and 1.

The operator :obj:`.<` is intended for array objects; the operation
``A .< B`` is valid only if ``A`` and ``B`` have the same dimensions.  The
operator returns an array with boolean entries and with the same dimensions
as ``A`` and ``B``.  Such operators are called *elementwise*; Julia offers a
suite of elementwise operators: :obj:`.*`, :obj:`.+`, etc.  Some of the elementwise
operators can take a scalar operand such as the example ``0 .< A .< 1`` in
the preceding paragraph.
This notation means that the scalar operand should be replicated for each entry of
the array.

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
If side effects are required, the short-circuit :obj:`&&` operator should
be used explicitly (see :ref:`man-short-circuit-evaluation`).

Operator Precedence
~~~~~~~~~~~~~~~~~~~

Julia applies the following order of operations, from highest precedence 
to lowest:

================= =============================================================================================
Category          Operators
================= =============================================================================================
Syntax            ``.`` followed by ``::``
Exponentiation    ``^`` and its elementwise equivalent ``.^``
Fractions         ``//`` and ``.//``
Multiplication    ``* / % & \`` and  ``.* ./ .% .\``
Bitshifts         ``<< >> >>>`` and ``.<< .>> .>>>``
Addition          ``+ - | $`` and ``.+ .-``
Syntax            ``: ..`` followed by ``|>``
Comparisons       ``> < >= <= == === != !== <:`` and ``.> .< .>= .<= .== .!=``
Control flow      ``&&`` followed by ``||`` followed by ``?``
Assignments       ``= += -= *= /= //= \= ^= ÷= %= |= &= $= <<= >>= >>>=`` and ``.+= .-= .*= ./= .//= .\= .^= .÷= .%=``
================= =============================================================================================

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

=========================== ================================== =============
Function                    Description                        Return type
=========================== ================================== =============
:func:`round(x) <round>`    round ``x`` to the nearest integer ``typeof(x)``
:func:`round(T, x) <round>` round ``x`` to the nearest integer ``T``
:func:`floor(x) <floor>`    round ``x`` towards ``-Inf``       ``typeof(x)``
:func:`floor(T, x) <floor>` round ``x`` towards ``-Inf``       ``T``
:func:`ceil(x) <ceil>`      round ``x`` towards ``+Inf``       ``typeof(x)``
:func:`ceil(T, x) <ceil>`   round ``x`` towards ``+Inf``       ``T``
:func:`trunc(x) <trunc>`    round ``x`` towards zero           ``typeof(x)``
:func:`trunc(T, x) <trunc>` round ``x`` towards zero           ``T``
=========================== ================================== =============

Division functions
~~~~~~~~~~~~~~~~~~

============================ =======================================================================
Function                     Description
============================ =======================================================================
:func:`div(x,y) <div>`       truncated division; quotient rounded towards zero
:func:`fld(x,y) <fld>`       floored division; quotient rounded towards ``-Inf``
:func:`cld(x,y) <cld>`       ceiling division; quotient rounded towards ``+Inf``
:func:`rem(x,y) <rem>`       remainder; satisfies ``x == div(x,y)*y + rem(x,y)``; sign matches ``x``
:func:`divrem(x,y) <divrem>` returns ``(div(x,y),rem(x,y))``
:func:`mod(x,y) <mod>`       modulus; satisfies ``x == fld(x,y)*y + mod(x,y)``; sign matches ``y``
:func:`mod2pi(x) <mod2pi>`   modulus with respect to 2pi;  ``0 <= mod2pi(x)  < 2pi``
:func:`gcd(x,y...) <gcd>`    greatest common divisor of ``x``, ``y``,...; sign matches ``x``
:func:`lcm(x,y...) <lcm>`    least common multiple of ``x``, ``y``,...; sign matches ``x``
============================ =======================================================================

Sign and absolute value functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

================================ ===========================================================
Function                         Description
================================ ===========================================================
:func:`abs(x) <abs>`             a positive value with the magnitude of ``x``
:func:`abs2(x) <abs2>`           the squared magnitude of ``x``
:func:`sign(x) <sign>`           indicates the sign of ``x``, returning -1, 0, or +1
:func:`signbit(x) <signbit>`     indicates whether the sign bit is on (true) or off (false)
:func:`copysign(x,y) <copysign>` a value with the magnitude of ``x`` and the sign of ``y``
:func:`flipsign(x,y) <flipsign>` a value with the magnitude of ``x`` and the sign of ``x*y``
================================ ===========================================================

Powers, logs and roots
~~~~~~~~~~~~~~~~~~~~~~

==================================== ==============================================================================
Function                             Description
==================================== ==============================================================================
:func:`sqrt(x) <sqrt>` ``√x``        square root of ``x``
:func:`cbrt(x) <cbrt>` ``∛x``        cube root of ``x``
:func:`hypot(x,y) <hypot>`           hypotenuse of right-angled triangle with other sides of length ``x`` and ``y``
:func:`exp(x) <exp>`                 natural exponential function at ``x``
:func:`expm1(x) <expm1>`             accurate ``exp(x)-1`` for ``x`` near zero
:func:`ldexp(x,n) <ldexp>`           ``x*2^n`` computed efficiently for integer values of ``n``
:func:`log(x) <log>`                 natural logarithm of ``x``
:func:`log(b,x) <log>`               base ``b`` logarithm of ``x``
:func:`log2(x) <log2>`               base 2 logarithm of ``x``
:func:`log10(x) <log10>`             base 10 logarithm of ``x``
:func:`log1p(x) <log1p>`             accurate ``log(1+x)`` for ``x`` near zero
:func:`exponent(x) <exponent>`       binary exponent of ``x``
:func:`significand(x) <significand>` binary significand (a.k.a. mantissa) of a floating-point number ``x``
==================================== ==============================================================================

For an overview of why functions like :func:`hypot`, :func:`expm1`, and :func:`log1p`
are necessary and useful, see John D. Cook's excellent pair
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
    asinh  acosh  atanh  acoth  asech  acsch
    sinc   cosc   atan2

These are all single-argument functions, with the exception of
`atan2 <http://en.wikipedia.org/wiki/Atan2>`_, which gives the angle
in `radians <http://en.wikipedia.org/wiki/Radian>`_ between the *x*-axis
and the point specified by its arguments, interpreted as *x* and *y*
coordinates.

Additionally, :func:`sinpi(x) <sinpi>` and :func:`cospi(x) <cospi>` are provided for more accurate computations
of :func:`sin(pi*x) <sin>` and :func:`cos(pi*x) <cos>` respectively.

In order to compute trigonometric functions with degrees
instead of radians, suffix the function with ``d``. For example, :func:`sind(x) <sind>`
computes the sine of ``x`` where ``x`` is specified in degrees.
The complete list of trigonometric functions with degree variants is::

    sind   cosd   tand   cotd   secd   cscd
    asind  acosd  atand  acotd  asecd  acscd

Special functions
~~~~~~~~~~~~~~~~~

=================================================== ==============================================================================
Function                                            Description
=================================================== ==============================================================================
:func:`erf(x) <erf>`                                `error function <http://en.wikipedia.org/wiki/Error_function>`_ at ``x``
:func:`erfc(x) <erfc>`                              complementary error function, i.e. the accurate version of ``1-erf(x)`` for large ``x``
:func:`erfinv(x) <erfinv>`                          inverse function to :func:`erf`
:func:`erfcinv(x) <erfinvc>`                        inverse function to :func:`erfc`
:func:`erfi(x) <erfi>`                              imaginary error function defined as ``-im * erf(x * im)``, where :const:`im` is the imaginary unit
:func:`erfcx(x) <erfcx>`                            scaled complementary error function, i.e. accurate ``exp(x^2) * erfc(x)`` for large ``x``
:func:`dawson(x) <dawson>`                          scaled imaginary error function, a.k.a. Dawson function, i.e. accurate ``exp(-x^2) * erfi(x) * sqrt(pi) / 2`` for large ``x``
:func:`gamma(x) <gamma>`                            `gamma function <http://en.wikipedia.org/wiki/Gamma_function>`_ at ``x``
:func:`lgamma(x) <lgamma>`                          accurate ``log(gamma(x))`` for large ``x``
:func:`lfact(x) <lfact>`                            accurate ``log(factorial(x))`` for large ``x``; same as ``lgamma(x+1)`` for ``x > 1``, zero otherwise
:func:`digamma(x) <digamma>`                        `digamma function <http://en.wikipedia.org/wiki/Digamma_function>`_ (i.e. the derivative of :func:`lgamma`) at ``x``
:func:`beta(x,y) <beta>`                            `beta function <http://en.wikipedia.org/wiki/Beta_function>`_ at ``x,y``
:func:`lbeta(x,y) <lbeta>`                          accurate ``log(beta(x,y))`` for large ``x`` or ``y``
:func:`eta(x) <eta>`                                `Dirichlet eta function <http://en.wikipedia.org/wiki/Dirichlet_eta_function>`_ at ``x``
:func:`zeta(x) <zeta>`                              `Riemann zeta function <http://en.wikipedia.org/wiki/Riemann_zeta_function>`_ at ``x``
|airylist|                                          `Airy Ai function <http://en.wikipedia.org/wiki/Airy_function>`_ at ``z`` 
|airyprimelist|                                     derivative of the Airy Ai function at ``z`` 
:func:`airybi(z) <airybi>`, ``airy(2,z)``           `Airy Bi function <http://en.wikipedia.org/wiki/Airy_function>`_ at ``z`` 
:func:`airybiprime(z) <airybiprime>`, ``airy(3,z)`` derivative of the Airy Bi function at ``z`` 
:func:`airyx(z) <airyx>`, ``airyx(k,z)``            scaled Airy AI function and ``k`` th derivatives at ``z`` 
:func:`besselj(nu,z) <besselj>`                     `Bessel function <http://en.wikipedia.org/wiki/Bessel_function>`_ of the first kind of order ``nu`` at ``z`` 
:func:`besselj0(z) <besselj0>`                      ``besselj(0,z)``  
:func:`besselj1(z) <besselj1>`                      ``besselj(1,z)``  
:func:`besseljx(nu,z) <besseljx>`                   scaled Bessel function of the first kind of order ``nu`` at ``z`` 
:func:`bessely(nu,z) <bessely>`                     `Bessel function <http://en.wikipedia.org/wiki/Bessel_function>`_ of the second kind of order ``nu`` at ``z``  
:func:`bessely0(z) <bessely0>`                      ``bessely(0,z)``  
:func:`bessely1(z) <bessely0>`                      ``bessely(1,z)``  
:func:`besselyx(nu,z) <besselyx>`                   scaled Bessel function of the second kind of order ``nu`` at ``z``  
:func:`besselh(nu,k,z) <besselh>`                   `Bessel function <http://en.wikipedia.org/wiki/Bessel_function>`_ of the third kind (a.k.a. Hankel function) of order ``nu`` at ``z``; ``k`` must be either ``1`` or ``2``  
:func:`hankelh1(nu,z) <hankelh1>`                   ``besselh(nu, 1, z)``  
:func:`hankelh1x(nu,z) <hankelh1x>`                 scaled ``besselh(nu, 1, z)``  
:func:`hankelh2(nu,z) <hankelh2>`                   ``besselh(nu, 2, z)``  
:func:`hankelh2x(nu,z) <hankelh2x>`                 scaled ``besselh(nu, 2, z)``  
:func:`besseli(nu,z) <besseli>`                     modified `Bessel function <http://en.wikipedia.org/wiki/Bessel_function>`_ of the first kind of order ``nu`` at ``z``  
:func:`besselix(nu,z) <besselix>`                   scaled modified Bessel function of the first kind of order ``nu`` at ``z``  
:func:`besselk(nu,z) <besselk>`                     modified `Bessel function <http://en.wikipedia.org/wiki/Bessel_function>`_ of the second kind of order ``nu`` at ``z``  
:func:`besselkx(nu,z) <besselkx>`                   scaled modified Bessel function of the second kind of order ``nu`` at ``z``  
=================================================== ==============================================================================

.. |airylist| replace:: :func:`airy(z) <airy>`, :func:`airyai(z) <airyai>`, ``airy(0,z)``
.. |airyprimelist| replace:: :func:`airyprime(z) <airyprime>`, :func:`airyaiprime(z) <airyaiprime>`, ``airy(1,z)``



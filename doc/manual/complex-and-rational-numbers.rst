.. _man-complex-and-rational-numbers:

******************************
 Complex and Rational Numbers  
******************************

Julia ships with predefined types representing both complex and rational
numbers, and supports all the mathematical operations discussed in
:ref:`man-mathematical-operations` on them.
Promotions are defined so that operations on any combination of
predefined numeric types, whether primitive or composite, behave as
expected.

.. _man-complex-numbers:

Complex Numbers
---------------

The global constant ``im`` is bound to the complex number *i*,
representing one of the square roots of -1. It was deemed harmful to
co-opt the name ``i`` for a global constant, since it is such a popular
index variable name. Since Julia allows numeric literals to be
:ref:`juxtaposed with identifiers as
coefficients <man-numeric-literal-coefficients>`,
this binding suffices to provide convenient syntax for complex numbers,
similar to the traditional mathematical notation::

    julia> 1 + 2im
    1 + 2im

You can perform all the standard arithmetic operations with complex
numbers::

    julia> (1 + 2im)*(2 - 3im)
    8 + 1im

    julia> (1 + 2im)/(1 - 2im)
    -0.6 + 0.8im

    julia> (1 + 2im) + (1 - 2im)
    2 + 0im

    julia> (-3 + 2im) - (5 - 1im)
    -8 + 3im

    julia> (-1 + 2im)^2
    -3 - 4im

    julia> (-1 + 2im)^2.5
    2.729624464784009 - 6.9606644595719im

    julia> (-1 + 2im)^(1 + 1im)
    -0.27910381075826657 + 0.08708053414102428im

    julia> 3(2 - 5im)
    6 - 15im

    julia> 3(2 - 5im)^2
    -63 - 60im

    julia> 3(2 - 5im)^-1
    0.20689655172413793 + 0.5172413793103449im

The promotion mechanism ensures that combinations of operands of
different types just work::

    julia> 2(1 - 1im)
    2 - 2im

    julia> (2 + 3im) - 1
    1 + 3im

    julia> (1 + 2im) + 0.5
    1.5 + 2.0im

    julia> (2 + 3im) - 0.5im
    2.0 + 2.5im

    julia> 0.75(1 + 2im)
    0.75 + 1.5im

    julia> (2 + 3im) / 2
    1.0 + 1.5im

    julia> (1 - 3im) / (2 + 2im)
    -0.5 - 1.0im

    julia> 2im^2
    -2 + 0im

    julia> 1 + 3/4im
    1.0 - 0.75im

Note that ``3/4im == 3/(4*im) == -(3/4*im)``, since a literal
coefficient binds more tightly than division.

Standard functions to manipulate complex values are provided::

    julia> real(1 + 2im)
    1

    julia> imag(1 + 2im)
    2

    julia> conj(1 + 2im)
    1 - 2im

    julia> abs(1 + 2im)
    2.23606797749979

    julia> abs2(1 + 2im)
    5

As is common, the absolute value of a complex number is its distance
from zero. The ``abs2`` function gives the square of the absolute value,
and is of particular use for complex numbers, where it avoids taking a
square root. The full gamut of other mathematical functions are also
defined for complex numbers::

    julia> sqrt(im)
    0.7071067811865476 + 0.7071067811865475im

    julia> sqrt(1 + 2im)
    1.272019649514069 + 0.7861513777574233im

    julia> cos(1 + 2im)
    2.0327230070196656 - 3.0518977991517997im

    julia> exp(1 + 2im)
    -1.1312043837568138 + 2.471726672004819im

    julia> sinh(1 + 2im)
    -0.48905625904129374 + 1.4031192506220407im

Note that mathematical functions always return real values when applied
to real numbers and complex values when applied to complex numbers.
Thus, ``sqrt``, for example, behaves differently when applied to ``-1``
versus ``-1 + 0im`` even though ``-1 == -1 + 0im``::

    julia> sqrt(-1)
    NaN

    julia> sqrt(-1 + 0im)
    0.0 + 1.0im

If you need to construct a complex number using variables, the literal
numeric coefficient notation will not work, although explicitly writing
the multiplication operation will::

    julia> a = 1; b = 2; a + b*im
    1 + 2im

Constructing complex numbers from variable values like this, however, is
not recommended. Use the ``complex`` function to construct a complex
value directly from its real and imaginary parts instead::

    julia> complex(a,b)
    1 + 2im

This construction is preferred for variable arguments because it is more
efficient than the multiplication and addition construct, but also
because certain values of ``b`` can yield unexpected results::

    julia> 1 + Inf*im
    NaN + Inf*im

    julia> 1 + NaN*im
    NaN + NaN*im

These results are natural and unavoidable consequences of the
interaction between the rules of complex multiplication and IEEE-754
floating-point arithmetic. Using the ``complex`` function to construct
complex values directly, however, gives more intuitive results::

    julia> complex(1,Inf)
    complex(1.0,Inf)

    julia> complex(1,NaN)
    complex(1.0,NaN)

On the other hand, it can be argued that these values do not represent
meaningful complex numbers, and are thus not appreciably different from
the results gotten when multiplying explicitly by ``im``.

.. _man-rational-numbers:

Rational Numbers
----------------

Julia has a rational number type to represent exact ratios of integers.
Rationals are constructed using the ``//`` operator::

    julia> 2//3
    2//3

If the numerator and denominator of a rational have common factors, they
are reduced to lowest terms such that the denominator is non-negative::

    julia> 6//9
    2//3

    julia> -4//8
    -1//2

    julia> 5//-15
    -1//3

    julia> -4//-12
    1//3

This normalized form for a ratio of integers is unique, so equality of
rational values can be tested by checking for equality of the numerator
and denominator. The standardized numerator and denominator of a
rational value can be extracted using the ``num`` and ``den`` functions::

    julia> num(2//3)
    2

    julia> den(2//3)
    3

Direct comparison of the numerator and denominator is generally not
necessary, since the standard arithmetic and comparison operations are
defined for rational values::

    julia> 2//3 == 6//9
    true

    julia> 2//3 == 9//27
    false

    julia> 3//7 < 1//2
    true

    julia> 3//4 > 2//3
    true

    julia> 2//4 + 1//6
    2//3

    julia> 5//12 - 1//4
    1//6

    julia> 5//8 * 3//12
    5//32

    julia> 6//5 / 10//7
    21//25

Rationals can be easily converted to floating-point numbers::

    julia> float(3//4)
    0.75

Conversion from rational to floating-point respects the following
identity for any integral values of ``a`` and ``b``, with the exception
of the case ``a == 0`` and ``b == 0``::

    julia> isequal(float(a//b), a/b)
    true

Constructing infinite rational values is acceptable::

    julia> 5//0
    Inf

    julia> -3//0
    -Inf

    julia> typeof(ans)
    Rational{Int64}

Trying to construct a NaN rational value, however, is not::

    julia> 0//0
    invalid rational: 0//0

As usual, the promotion system makes interactions with other numeric
types effortless::

    julia> 3//5 + 1
    8//5

    julia> 3//5 - 0.5
    0.1

    julia> 2//7 * (1 + 2im)
    2//7 + 4//7im

    julia> 2//7 * (1.5 + 2im)
    0.42857142857142855 + 0.5714285714285714im

    julia> 3//2 / (1 + 2im)
    3//10 - 3//5im

    julia> 1//2 + 2im
    1//2 + 2//1im

    julia> 1 + 2//3im
    1//1 + 2//3im

    julia> 0.5 == 1//2
    true

    julia> 0.33 == 1//3
    false

    julia> 0.33 < 1//3
    true

    julia> 1//3 - 0.33
    0.0033333333333332993


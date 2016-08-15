.. currentmodule:: Base

*************
 Mathematics
*************

.. _mathematical-operators:

Mathematical Operators
----------------------

.. function:: -(x)

   .. Docstring generated from Julia source

   Unary minus operator.

.. _+:
.. function:: +(x, y...)

   .. Docstring generated from Julia source

   Addition operator. ``x+y+z+...`` calls this function with all arguments, i.e. ``+(x, y, z, ...)``\ .

.. _-:
.. function:: -(x, y)

   .. Docstring generated from Julia source

   Subtraction operator.

.. _*:
.. function:: *(x, y...)

   .. Docstring generated from Julia source

   Multiplication operator. ``x*y*z*...`` calls this function with all arguments, i.e. ``*(x, y, z, ...)``\ .

.. _/:
.. function:: /(x, y)

   .. Docstring generated from Julia source

   Right division operator: multiplication of ``x`` by the inverse of ``y`` on the right. Gives floating-point results for integer arguments.

.. _\\:
.. function:: \\(x, y)

   .. Docstring generated from Julia source

   Left division operator: multiplication of ``y`` by the inverse of ``x`` on the left. Gives floating-point results for integer arguments.

.. _^:
.. function:: ^(x, y)

   .. Docstring generated from Julia source

   Exponentiation operator.

.. _.+:
.. function:: .+(x, y)

   .. Docstring generated from Julia source

   Element-wise addition operator.

.. _.-:
.. function:: .-(x, y)

   .. Docstring generated from Julia source

   Element-wise subtraction operator.

.. _.*:
.. function:: .*(x, y)

   .. Docstring generated from Julia source

   Element-wise multiplication operator.

.. _./:
.. function:: ./(x, y)

   .. Docstring generated from Julia source

   Element-wise right division operator.

.. _.\\:
.. function:: .\\(x, y)

   .. Docstring generated from Julia source

   Element-wise left division operator.

.. _.^:
.. function:: .^(x, y)

   .. Docstring generated from Julia source

   Element-wise exponentiation operator.

.. function:: fma(x, y, z)

   .. Docstring generated from Julia source

   Computes ``x*y+z`` without rounding the intermediate result ``x*y``\ . On some systems this is significantly more expensive than ``x*y+z``\ . ``fma`` is used to improve accuracy in certain algorithms. See :func:`muladd`\ .

.. function:: muladd(x, y, z)

   .. Docstring generated from Julia source

   Combined multiply-add, computes ``x*y+z`` in an efficient manner. This may on some systems be equivalent to ``x*y+z``\ , or to ``fma(x,y,z)``\ . ``muladd`` is used to improve performance. See :func:`fma`\ .

.. function:: div(x, y)
              ÷(x, y)

   .. Docstring generated from Julia source

   The quotient from Euclidean division. Computes ``x/y``\ , truncated to an integer.

.. function:: fld(x, y)

   .. Docstring generated from Julia source

   Largest integer less than or equal to ``x/y``\ .

   .. doctest::

       julia> fld(7.3,5.5)
       1.0

.. function:: cld(x, y)

   .. Docstring generated from Julia source

   Smallest integer larger than or equal to ``x/y``\ .

   .. doctest::

       julia> cld(5.5,2.2)
       3.0

.. function:: mod(x, y)

   .. Docstring generated from Julia source

   Modulus after flooring division, returning in the range :math:`[0,y)`\ , if ``y`` is positive, or :math:`(y,0]` if ``y`` is negative.

   .. code-block:: julia

       x == fld(x,y)*y + mod(x,y)

.. function:: mod2pi(x)

   .. Docstring generated from Julia source

   Modulus after division by ``2π``\ , returning in the range :math:`[0,2π)`\ .

   This function computes a floating point representation of the modulus after division by numerically exact ``2π``\ , and is therefore not exactly the same as ``mod(x,2π)``\ , which would compute the modulus of ``x`` relative to division by the floating-point number ``2π``\ .

   .. doctest::

       julia> mod2pi(9*pi/4)
       0.7853981633974481

.. function:: rem(x, y)
              %(x, y)

   .. Docstring generated from Julia source

   Remainder from Euclidean division, returning a value of the same sign as ``x``\ , and smaller in magnitude than ``y``\ . This value is always exact.

   .. code-block:: julia

       x == div(x,y)*y + rem(x,y)

.. function:: divrem(x, y)

   .. Docstring generated from Julia source

   The quotient and remainder from Euclidean division. Equivalent to ``(div(x,y), rem(x,y))`` or ``(x÷y, x%y)``\ .

   .. doctest::

       julia> divrem(3,7)
       (0,3)

       julia> divrem(7,3)
       (2,1)

.. function:: fldmod(x, y)

   .. Docstring generated from Julia source

   The floored quotient and modulus after division. Equivalent to ``(fld(x,y), mod(x,y))``\ .

.. function:: fld1(x, y)

   .. Docstring generated from Julia source

   Flooring division, returning a value consistent with ``mod1(x,y)``

   .. code-block:: julia

       x == fld(x,y)*y + mod(x,y)
       x == (fld1(x,y)-1)*y + mod1(x,y)

.. function:: mod1(x, y)

   .. Docstring generated from Julia source

   Modulus after flooring division, returning a value ``r`` such that ``mod(r, y) == mod(x, y)``  in the range :math:`(0, y]` for positive ``y`` and in the range :math:`[y,0)` for negative ``y``\ .

.. function:: fldmod1(x, y)

   .. Docstring generated from Julia source

   Return ``(fld1(x,y), mod1(x,y))``\ .

.. _//:
.. function:: //(num, den)

   .. Docstring generated from Julia source

   Divide two integers or rational numbers, giving a ``Rational`` result.

.. function:: rationalize([T<:Integer=Int,] x; tol::Real=eps(x))

   .. Docstring generated from Julia source

   Approximate floating point number ``x`` as a ``Rational`` number with components of the given integer type. The result will differ from ``x`` by no more than ``tol``\ . If ``T`` is not provided, it defaults to ``Int``\ .

   .. doctest::

       julia> rationalize(5.6)
       28//5

       julia> a = rationalize(BigInt, 10.3)
       103//10

       julia> typeof(num(a))
       BigInt

.. function:: num(x)

   .. Docstring generated from Julia source

   Numerator of the rational representation of ``x``\ .

.. function:: den(x)

   .. Docstring generated from Julia source

   Denominator of the rational representation of ``x``\ .

.. _<<:
.. function:: <<(x, n)

   .. Docstring generated from Julia source

   Left bit shift operator, ``x << n``\ . For ``n >= 0``\ , the result is ``x`` shifted left by ``n`` bits, filling with ``0``\ s. This is equivalent to ``x * 2^n``\ . For ``n < 0``\ , this is equivalent to ``x >> -n``\ .

   .. doctest::

       julia> Int8(3) << 2
       12

       julia> bits(Int8(3))
       "00000011"

       julia> bits(Int8(12))
       "00001100"

   See also :func:`>>`\ , :func:`>>>`\ .

.. _>>:
.. function:: >>(x, n)

   .. Docstring generated from Julia source

   Right bit shift operator, ``x >> n``\ . For ``n >= 0``\ , the result is ``x`` shifted right by ``n`` bits, where ``n >= 0``\ , filling with ``0``\ s if ``x >= 0``\ , ``1``\ s if ``x < 0``\ , preserving the sign of ``x``\ . This is equivalent to ``fld(x, 2^n)``\ . For ``n < 0``\ , this is equivalent to ``x << -n``\ .

   .. doctest::

       julia> Int8(13) >> 2
       3

       julia> bits(Int8(13))
       "00001101"

       julia> bits(Int8(3))
       "00000011"

       julia> Int8(-14) >> 2
       -4

       julia> bits(Int8(-14))
       "11110010"

       julia> bits(Int8(-4))
       "11111100"

   See also :func:`>>>`\ , :func:`<<`\ .

.. _>>>:
.. function:: >>>(x, n)

   .. Docstring generated from Julia source

   Unsigned right bit shift operator, ``x >>> n``\ . For ``n >= 0``\ , the result is ``x`` shifted right by ``n`` bits, where ``n >= 0``\ , filling with ``0``\ s. For ``n < 0``\ , this is equivalent to ``x << -n``\ .

   For ``Unsigned`` integer types, this is equivalent to :func:`>>`\ . For ``Signed`` integer types, this is equivalent to ``signed(unsigned(x) >> n)``\ .

   .. doctest::

       julia> Int8(-14) >>> 2
       60

       julia> bits(Int8(-14))
       "11110010"

       julia> bits(Int8(60))
       "00111100"

   ``BigInt``\ s are treated as if having infinite size, so no filling is required and this is equivalent to :func:`>>`\ .

   See also :func:`>>`\ , :func:`<<`\ .

.. _\::
.. function:: :(start, [step], stop)

   .. Docstring generated from Julia source

   Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1, and ``a:s:b`` is similar but uses a step size of ``s``\ . These syntaxes call the function ``colon``\ . The colon is also used in indexing to select whole dimensions.

.. function:: colon(start, [step], stop)

   .. Docstring generated from Julia source

   Called by ``:`` syntax for constructing ranges.

.. function:: range(start, [step], length)

   .. Docstring generated from Julia source

   Construct a range by length, given a starting value and optional step (defaults to 1).

.. function:: Base.OneTo(n)

   .. Docstring generated from Julia source

   Define an ``AbstractUnitRange`` that behaves like ``1:n``\ , with the added distinction that the lower limit is guaranteed (by the type system) to be 1.

.. _==:
.. function:: ==(x, y)

   .. Docstring generated from Julia source

   Generic equality operator, giving a single ``Bool`` result. Falls back to ``===``\ . Should be implemented for all types with a notion of equality, based on the abstract value that an instance represents. For example, all numeric types are compared by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement ``==`` by calling ``==`` recursively on all contents.

   New numeric types should implement this function for two arguments of the new type, and handle comparison to other types via promotion rules where possible.

.. _!=:
.. function:: !=(x, y)
              ≠(x,y)

   .. Docstring generated from Julia source

   Not-equals comparison operator. Always gives the opposite answer as ``==``\ . New types should generally not implement this, and rely on the fallback definition ``!=(x,y) = !(x==y)`` instead.

.. _===:
.. function:: ===(x, y)
              ≡(x,y)

   .. Docstring generated from Julia source

   See the :func:`is` operator.

.. _!==:
.. function:: !==(x, y)
              ≢(x,y)

   .. Docstring generated from Julia source

   Equivalent to ``!is(x, y)``\ .

.. _<:
.. function:: <(x, y)

   .. Docstring generated from Julia source

   Less-than comparison operator. New numeric types should implement this function for two arguments of the new type. Because of the behavior of floating-point NaN values, ``<`` implements a partial order. Types with a canonical partial order should implement ``<``\ , and types with a canonical total order should implement ``isless``\ .

.. _<=:
.. function:: <=(x, y)
              ≤(x,y)

   .. Docstring generated from Julia source

   Less-than-or-equals comparison operator.

.. _>:
.. function:: >(x, y)

   .. Docstring generated from Julia source

   Greater-than comparison operator. Generally, new types should implement ``<`` instead of this function, and rely on the fallback definition ``>(x,y) = y<x``\ .

.. _>=:
.. function:: >=(x, y)
              ≥(x,y)

   .. Docstring generated from Julia source

   Greater-than-or-equals comparison operator.

.. _.==:
.. function:: .==(x, y)

   .. Docstring generated from Julia source

   Element-wise equality comparison operator.

.. _.!=:
.. function:: .!=(x, y)
              .≠(x,y)

   .. Docstring generated from Julia source

   Element-wise not-equals comparison operator.

.. _.<:
.. function:: .<(x, y)

   .. Docstring generated from Julia source

   Element-wise less-than comparison operator.

.. _.<=:
.. function:: .<=(x, y)
              .≤(x,y)

   .. Docstring generated from Julia source

   Element-wise less-than-or-equals comparison operator.

.. _.>:
.. function:: .>(x, y)

   .. Docstring generated from Julia source

   Element-wise greater-than comparison operator.

.. _.>=:
.. function:: .>=(x, y)
              .≥(x,y)

   .. Docstring generated from Julia source

   Element-wise greater-than-or-equals comparison operator.

.. function:: cmp(x,y)

   .. Docstring generated from Julia source

   Return -1, 0, or 1 depending on whether ``x`` is less than, equal to, or greater than ``y``\ , respectively. Uses the total order implemented by ``isless``\ . For floating-point numbers, uses ``<`` but throws an error for unordered arguments.

.. _~:
.. function:: ~(x)

   .. Docstring generated from Julia source

   Bitwise not.

.. _&:
.. function:: &(x, y)

   .. Docstring generated from Julia source

   Bitwise and.

.. _|:
.. function:: |(x, y)

   .. Docstring generated from Julia source

   Bitwise or.

.. _$:
.. function:: $(x, y)

   .. Docstring generated from Julia source

   Bitwise exclusive or.

.. _!:
.. function:: !(x)

   .. Docstring generated from Julia source

   Boolean not.

.. _&&:
.. function:: x && y

   .. Docstring generated from Julia source

   Short-circuiting boolean AND.

.. _||:
.. function:: x || y

   .. Docstring generated from Julia source

   Short-circuiting boolean OR.

Mathematical Functions
----------------------

.. function:: isapprox(x, y; rtol::Real=sqrt(eps), atol::Real=0)

   .. Docstring generated from Julia source

   Inexact equality comparison: ``true`` if ``norm(x-y) <= atol + rtol*max(norm(x), norm(y))``\ . The default ``atol`` is zero and the default ``rtol`` depends on the types of ``x`` and ``y``\ .

   For real or complex floating-point values, ``rtol`` defaults to ``sqrt(eps(typeof(real(x-y))))``\ . This corresponds to requiring equality of about half of the significand digits. For other types, ``rtol`` defaults to zero.

   ``x`` and ``y`` may also be arrays of numbers, in which case ``norm`` defaults to ``vecnorm`` but may be changed by passing a ``norm::Function`` keyword argument. (For numbers, ``norm`` is the same thing as ``abs``\ .) When ``x`` and ``y`` are arrays, if ``norm(x-y)`` is not finite (i.e. ``±Inf`` or ``NaN``\ ), the comparison falls back to checking whether all elements of ``x`` and ``y`` are approximately equal component-wise.

   The binary operator ``≈`` is equivalent to ``isapprox`` with the default arguments, and ``x ≉ y`` is equivalent to ``!isapprox(x,y)``\ .

.. function:: sin(x)

   .. Docstring generated from Julia source

   Compute sine of ``x``\ , where ``x`` is in radians.

.. function:: cos(x)

   .. Docstring generated from Julia source

   Compute cosine of ``x``\ , where ``x`` is in radians.

.. function:: tan(x)

   .. Docstring generated from Julia source

   Compute tangent of ``x``\ , where ``x`` is in radians.

.. function:: sind(x)

   .. Docstring generated from Julia source

   Compute sine of ``x``\ , where ``x`` is in degrees.

.. function:: cosd(x)

   .. Docstring generated from Julia source

   Compute cosine of ``x``\ , where ``x`` is in degrees.

.. function:: tand(x)

   .. Docstring generated from Julia source

   Compute tangent of ``x``\ , where ``x`` is in degrees.

.. function:: sinpi(x)

   .. Docstring generated from Julia source

   Compute :math:`\sin(\pi x)` more accurately than ``sin(pi*x)``\ , especially for large ``x``\ .

.. function:: cospi(x)

   .. Docstring generated from Julia source

   Compute :math:`\cos(\pi x)` more accurately than ``cos(pi*x)``\ , especially for large ``x``\ .

.. function:: sinh(x)

   .. Docstring generated from Julia source

   Compute hyperbolic sine of ``x``\ .

.. function:: cosh(x)

   .. Docstring generated from Julia source

   Compute hyperbolic cosine of ``x``\ .

.. function:: tanh(x)

   .. Docstring generated from Julia source

   Compute hyperbolic tangent of ``x``\ .

.. function:: asin(x)

   .. Docstring generated from Julia source

   Compute the inverse sine of ``x``\ , where the output is in radians.

.. function:: acos(x)

   .. Docstring generated from Julia source

   Compute the inverse cosine of ``x``\ , where the output is in radians

.. function:: atan(x)

   .. Docstring generated from Julia source

   Compute the inverse tangent of ``x``\ , where the output is in radians.

.. function:: atan2(y, x)

   .. Docstring generated from Julia source

   Compute the inverse tangent of ``y/x``\ , using the signs of both ``x`` and ``y`` to determine the quadrant of the return value.

.. function:: asind(x)

   .. Docstring generated from Julia source

   Compute the inverse sine of ``x``\ , where the output is in degrees.

.. function:: acosd(x)

   .. Docstring generated from Julia source

   Compute the inverse cosine of ``x``\ , where the output is in degrees.

.. function:: atand(x)

   .. Docstring generated from Julia source

   Compute the inverse tangent of ``x``\ , where the output is in degrees.

.. function:: sec(x)

   .. Docstring generated from Julia source

   Compute the secant of ``x``\ , where ``x`` is in radians.

.. function:: csc(x)

   .. Docstring generated from Julia source

   Compute the cosecant of ``x``\ , where ``x`` is in radians.

.. function:: cot(x)

   .. Docstring generated from Julia source

   Compute the cotangent of ``x``\ , where ``x`` is in radians.

.. function:: secd(x)

   .. Docstring generated from Julia source

   Compute the secant of ``x``\ , where ``x`` is in degrees.

.. function:: cscd(x)

   .. Docstring generated from Julia source

   Compute the cosecant of ``x``\ , where ``x`` is in degrees.

.. function:: cotd(x)

   .. Docstring generated from Julia source

   Compute the cotangent of ``x``\ , where ``x`` is in degrees.

.. function:: asec(x)

   .. Docstring generated from Julia source

   Compute the inverse secant of ``x``\ , where the output is in radians.

.. function:: acsc(x)

   .. Docstring generated from Julia source

   Compute the inverse cosecant of ``x``\ , where the output is in radians.

.. function:: acot(x)

   .. Docstring generated from Julia source

   Compute the inverse cotangent of ``x``\ , where the output is in radians.

.. function:: asecd(x)

   .. Docstring generated from Julia source

   Compute the inverse secant of ``x``\ , where the output is in degrees.

.. function:: acscd(x)

   .. Docstring generated from Julia source

   Compute the inverse cosecant of ``x``\ , where the output is in degrees.

.. function:: acotd(x)

   .. Docstring generated from Julia source

   Compute the inverse cotangent of ``x``\ , where the output is in degrees.

.. function:: sech(x)

   .. Docstring generated from Julia source

   Compute the hyperbolic secant of ``x``

.. function:: csch(x)

   .. Docstring generated from Julia source

   Compute the hyperbolic cosecant of ``x``\ .

.. function:: coth(x)

   .. Docstring generated from Julia source

   Compute the hyperbolic cotangent of ``x``\ .

.. function:: asinh(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic sine of ``x``\ .

.. function:: acosh(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic cosine of ``x``\ .

.. function:: atanh(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic tangent of ``x``\ .

.. function:: asech(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic secant of ``x``\ .

.. function:: acsch(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic cosecant of ``x``\ .

.. function:: acoth(x)

   .. Docstring generated from Julia source

   Compute the inverse hyperbolic cotangent of ``x``\ .

.. function:: sinc(x)

   .. Docstring generated from Julia source

   Compute :math:`\sin(\pi x) / (\pi x)` if :math:`x \neq 0`\ , and :math:`1` if :math:`x = 0`\ .

.. function:: cosc(x)

   .. Docstring generated from Julia source

   Compute :math:`\cos(\pi x) / x - \sin(\pi x) / (\pi x^2)` if :math:`x \neq 0`\ , and :math:`0` if :math:`x = 0`\ . This is the derivative of ``sinc(x)``\ .

.. function:: deg2rad(x)

   .. Docstring generated from Julia source

   Convert ``x`` from degrees to radians.

   .. doctest::

       julia> deg2rad(90)
       1.5707963267948966

.. function:: rad2deg(x)

   .. Docstring generated from Julia source

   Convert ``x`` from radians to degrees.

   .. doctest::

       julia> rad2deg(pi)
       180.0

.. function:: hypot(x, y)

   .. Docstring generated from Julia source

   Compute the hypotenuse :math:`\sqrt{x^2+y^2}` avoiding overflow and underflow.

.. function:: hypot(x...)

   .. Docstring generated from Julia source

   Compute the hypotenuse :math:`\sqrt{\sum x_i^2}` avoiding overflow and underflow.

.. function:: log(x)

   .. Docstring generated from Julia source

   Compute the natural logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments to obtain complex results.

   There is an experimental variant in the ``Base.Math.JuliaLibm`` module, which is typically faster and more accurate.

.. function:: log(b,x)

   .. Docstring generated from Julia source

   Compute the base ``b`` logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments.

   .. doctest::

       julia> log(4,8)
       1.5

       julia> log(4,2)
       0.5

.. function:: log2(x)

   .. Docstring generated from Julia source

   Compute the logarithm of ``x`` to base 2. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log10(x)

   .. Docstring generated from Julia source

   Compute the logarithm of ``x`` to base 10. Throws :obj:`DomainError` for negative ``Real`` arguments.

.. function:: log1p(x)

   .. Docstring generated from Julia source

   Accurate natural logarithm of ``1+x``\ . Throws ``DomainError`` for ``Real`` arguments less than -1.

   There is an experimental variant in the ``Base.Math.JuliaLibm`` module, which is typically faster and more accurate.

.. function:: frexp(val)

   .. Docstring generated from Julia source

   Return ``(x,exp)`` such that ``x`` has a magnitude in the interval :math:`[1/2, 1)` or 0, and ``val`` is equal to :math:`x \times 2^{exp}`\ .

.. function:: exp(x)

   .. Docstring generated from Julia source

   Compute :math:`e^x`\ .

.. function:: exp2(x)

   .. Docstring generated from Julia source

   Compute :math:`2^x`\ .

   .. doctest::

       julia> exp2(5)
       32.0

.. function:: exp10(x)

   .. Docstring generated from Julia source

   Compute :math:`10^x`\ .

.. function:: ldexp(x, n)

   .. Docstring generated from Julia source

   Compute :math:`x \times 2^n`\ .

.. function:: modf(x)

   .. Docstring generated from Julia source

   Return a tuple (fpart,ipart) of the fractional and integral parts of a number. Both parts have the same sign as the argument.

   .. doctest::

       julia> modf(3.5)
       (0.5,3.0)

.. function:: expm1(x)

   .. Docstring generated from Julia source

   Accurately compute :math:`e^x-1`\ .

.. function:: round([T,] x, [digits, [base]], [r::RoundingMode])

   .. Docstring generated from Julia source

   Rounds ``x`` to an integer value according to the provided :obj:`RoundingMode`\ , returning a value of the same type as ``x``\ . When not specifying a rounding mode the global mode will be used (see :func:`rounding`\ ), which by default is round to the nearest integer (:obj:`RoundNearest` mode), with ties (fractional values of 0.5) being rounded to the nearest even integer.

   .. doctest::

       julia> round(1.7)
       2.0

       julia> round(1.5)
       2.0

       julia> round(2.5)
       2.0

   The optional :obj:`RoundingMode` argument will change how the number gets rounded.

   ``round(T, x, [r::RoundingMode])`` converts the result to type ``T``\ , throwing an :exc:`InexactError` if the value is not representable.

   ``round(x, digits)`` rounds to the specified number of digits after the decimal place (or before if negative). ``round(x, digits, base)`` rounds using a base other than 10.

   .. doctest::

       julia> round(pi, 2)
       3.14

       julia> round(pi, 3, 2)
       3.125

   .. note::
      Rounding to specified digits in bases other than 2 can be inexact when operating on binary floating point numbers. For example, the ``Float64`` value represented by ``1.15`` is actually *less* than 1.15, yet will be rounded to 1.2.

      .. doctest::

          julia> x = 1.15
          1.15

          julia> @sprintf "%.20f" x
          "1.14999999999999991118"

          julia> x < 115//100
          true

          julia> round(x, 1)
          1.2


.. type:: RoundingMode

   .. Docstring generated from Julia source

   A type used for controlling the rounding mode of floating point operations (via :func:`rounding`\ /:func:`setrounding` functions), or as optional arguments for rounding to the nearest integer (via the :func:`round` function).

   Currently supported rounding modes are:

   * :obj:`RoundNearest` (default)
   * :obj:`RoundNearestTiesAway`
   * :obj:`RoundNearestTiesUp`
   * :obj:`RoundToZero`
   * :obj:`RoundFromZero` (``BigFloat`` only)
   * :obj:`RoundUp`
   * :obj:`RoundDown`

.. data:: RoundNearest

   .. Docstring generated from Julia source

   The default rounding mode. Rounds to the nearest integer, with ties (fractional values of 0.5) being rounded to the nearest even integer.

.. data:: RoundNearestTiesAway

   .. Docstring generated from Julia source

   Rounds to nearest integer, with ties rounded away from zero (C/C++ :func:`round` behaviour).

.. data:: RoundNearestTiesUp

   .. Docstring generated from Julia source

   Rounds to nearest integer, with ties rounded toward positive infinity (Java/JavaScript :func:`round` behaviour).

.. data:: RoundToZero

   .. Docstring generated from Julia source

   :func:`round` using this rounding mode is an alias for :func:`trunc`\ .

.. data:: RoundUp

   .. Docstring generated from Julia source

   :func:`round` using this rounding mode is an alias for :func:`ceil`\ .

.. data:: RoundDown

   .. Docstring generated from Julia source

   :func:`round` using this rounding mode is an alias for :func:`floor`\ .

.. function:: round(z, RoundingModeReal, RoundingModeImaginary)

   .. Docstring generated from Julia source

   Returns the nearest integral value of the same type as the complex-valued ``z`` to ``z``\ , breaking ties using the specified :obj:`RoundingMode`\ s. The first :obj:`RoundingMode` is used for rounding the real components while the second is used for rounding the imaginary components.

.. function:: ceil([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ``ceil(x)`` returns the nearest integral value of the same type as ``x`` that is greater than or equal to ``x``\ .

   ``ceil(T, x)`` converts the result to type ``T``\ , throwing an ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`\ .

.. function:: floor([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ``floor(x)`` returns the nearest integral value of the same type as ``x`` that is less than or equal to ``x``\ .

   ``floor(T, x)`` converts the result to type ``T``\ , throwing an ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`\ .

.. function:: trunc([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ``trunc(x)`` returns the nearest integral value of the same type as ``x`` whose absolute value is less than or equal to ``x``\ .

   ``trunc(T, x)`` converts the result to type ``T``\ , throwing an ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`\ .

.. function:: unsafe_trunc(T, x)

   .. Docstring generated from Julia source

   ``unsafe_trunc(T, x)`` returns the nearest integral value of type ``T`` whose absolute value is less than or equal to ``x``\ . If the value is not representable by ``T``\ , an arbitrary value will be returned.

.. function:: signif(x, digits, [base])

   .. Docstring generated from Julia source

   Rounds (in the sense of :func:`round`\ ) ``x`` so that there are ``digits`` significant digits, under a base ``base`` representation, default 10. E.g., ``signif(123.456, 2)`` is ``120.0``\ , and ``signif(357.913, 4, 2)`` is ``352.0``\ .

.. function:: min(x, y, ...)

   .. Docstring generated from Julia source

   Return the minimum of the arguments. Operates elementwise over arrays.

.. function:: max(x, y, ...)

   .. Docstring generated from Julia source

   Return the maximum of the arguments. Operates elementwise over arrays.

.. function:: minmax(x, y)

   .. Docstring generated from Julia source

   Return ``(min(x,y), max(x,y))``\ . See also: :func:`extrema` that returns ``(minimum(x), maximum(x))``\ .

   .. doctest::

       julia> minmax('c','b')
       ('b','c')

.. function:: clamp(x, lo, hi)

   .. Docstring generated from Julia source

   Return ``x`` if ``lo <= x <= hi``\ . If ``x < lo``\ , return ``lo``\ . If ``x > hi``\ , return ``hi``\ . Arguments are promoted to a common type. Operates elementwise over ``x`` if ``x`` is an array.

   .. doctest::

       julia> clamp([pi, 1.0, big(10.)], 2., 9.)
       3-element Array{BigFloat,1}:
        3.141592653589793238462643383279502884197169399375105820974944592307816406286198
        2.000000000000000000000000000000000000000000000000000000000000000000000000000000
        9.000000000000000000000000000000000000000000000000000000000000000000000000000000

.. function:: clamp!(array::AbstractArray, lo, hi)

   .. Docstring generated from Julia source

   Restrict values in ``array`` to the specified range, in-place. See also :func:`clamp`\ .

.. function:: abs(x)

   .. Docstring generated from Julia source

   The absolute value of ``x``\ .

   When ``abs`` is applied to signed integers, overflow may occur, resulting in the return of a negative value. This overflow occurs only when ``abs`` is applied to the minimum representable value of a signed integer. That is, when ``x == typemin(typeof(x))``\ , ``abs(x) == x < 0``\ , not ``-x`` as might be expected.

.. function:: Base.checked_abs(x)

   .. Docstring generated from Julia source

   Calculates ``abs(x)``\ , checking for overflow errors where applicable. For example, standard two's complement signed integers (e.g. ``Int``\ ) cannot represent ``abs(typemin(Int))``\ , thus leading to an overflow.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_neg(x)

   .. Docstring generated from Julia source

   Calculates ``-x``\ , checking for overflow errors where applicable. For example, standard two's complement signed integers (e.g. ``Int``\ ) cannot represent ``-typemin(Int)``\ , thus leading to an overflow.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_add(x, y)

   .. Docstring generated from Julia source

   Calculates ``x+y``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_sub(x, y)

   .. Docstring generated from Julia source

   Calculates ``x-y``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_mul(x, y)

   .. Docstring generated from Julia source

   Calculates ``x*y``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_div(x, y)

   .. Docstring generated from Julia source

   Calculates ``div(x,y)``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_rem(x, y)

   .. Docstring generated from Julia source

   Calculates ``x%y``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_fld(x, y)

   .. Docstring generated from Julia source

   Calculates ``fld(x,y)``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_mod(x, y)

   .. Docstring generated from Julia source

   Calculates ``mod(x,y)``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: Base.checked_cld(x, y)

   .. Docstring generated from Julia source

   Calculates ``cld(x,y)``\ , checking for overflow errors where applicable.

   The overflow protection may impose a perceptible performance penalty.

.. function:: abs2(x)

   .. Docstring generated from Julia source

   Squared absolute value of ``x``\ .

.. function:: copysign(x, y)

   .. Docstring generated from Julia source

   Return ``x`` such that it has the same sign as ``y``

.. function:: sign(x)

   .. Docstring generated from Julia source

   Return zero if ``x==0`` and :math:`x/|x|` otherwise (i.e., ±1 for real ``x``\ ).

.. function:: signbit(x)

   .. Docstring generated from Julia source

   Returns ``true`` if the value of the sign of ``x`` is negative, otherwise ``false``\ .

.. function:: flipsign(x, y)

   .. Docstring generated from Julia source

   Return ``x`` with its sign flipped if ``y`` is negative. For example ``abs(x) = flipsign(x,x)``\ .

.. function:: sqrt(x)

   .. Docstring generated from Julia source

   Return :math:`\sqrt{x}`\ . Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.  The prefix operator ``√`` is equivalent to ``sqrt``\ .

.. function:: isqrt(n::Integer)

   .. Docstring generated from Julia source

   Integer square root: the largest integer ``m`` such that ``m*m <= n``\ .

   .. doctest::

       julia> isqrt(5)
       2

.. function:: cbrt(x)

   .. Docstring generated from Julia source

   Return :math:`x^{1/3}`\ .  The prefix operator ``∛`` is equivalent to ``cbrt``\ .

   .. doctest::

       julia> cbrt(big(27))
       3.000000000000000000000000000000000000000000000000000000000000000000000000000000

.. function:: erf(x)

   .. Docstring generated from Julia source

   Compute the error function of ``x``\ , defined by :math:`\frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt` for arbitrary complex ``x``\ .

.. function:: erfc(x)

   .. Docstring generated from Julia source

   Compute the complementary error function of ``x``\ , defined by :math:`1 - \operatorname{erf}(x)`\ .

.. function:: erfcx(x)

   .. Docstring generated from Julia source

   Compute the scaled complementary error function of ``x``\ , defined by :math:`e^{x^2} \operatorname{erfc}(x)`\ . Note also that :math:`\operatorname{erfcx}(-ix)` computes the Faddeeva function :math:`w(x)`\ .

.. function:: erfi(x)

   .. Docstring generated from Julia source

   Compute the imaginary error function of ``x``\ , defined by :math:`-i \operatorname{erf}(ix)`\ .

.. function:: dawson(x)

   .. Docstring generated from Julia source

   Compute the Dawson function (scaled imaginary error function) of ``x``\ , defined by :math:`\frac{\sqrt{\pi}}{2} e^{-x^2} \operatorname{erfi}(x)`\ .

.. function:: erfinv(x)

   .. Docstring generated from Julia source

   Compute the inverse error function of a real ``x``\ , defined by :math:`\operatorname{erf}(\operatorname{erfinv}(x)) = x`\ .

.. function:: erfcinv(x)

   .. Docstring generated from Julia source

   Compute the inverse error complementary function of a real ``x``\ , defined by :math:`\operatorname{erfc}(\operatorname{erfcinv}(x)) = x`\ .

.. function:: real(z)

   .. Docstring generated from Julia source

   Return the real part of the complex number ``z``\ .

.. function:: imag(z)

   .. Docstring generated from Julia source

   Return the imaginary part of the complex number ``z``\ .

.. function:: reim(z)

   .. Docstring generated from Julia source

   Return both the real and imaginary parts of the complex number ``z``\ .

.. function:: conj(z)

   .. Docstring generated from Julia source

   Compute the complex conjugate of a complex number ``z``\ .

.. function:: angle(z)

   .. Docstring generated from Julia source

   Compute the phase angle in radians of a complex number ``z``\ .

.. function:: cis(z)

   .. Docstring generated from Julia source

   Return :math:`\exp(iz)`\ .

.. function:: binomial(n,k)

   .. Docstring generated from Julia source

   Number of ways to choose ``k`` out of ``n`` items.

.. function:: factorial(n)

   .. Docstring generated from Julia source

   Factorial of ``n``\ .  If ``n`` is an :obj:`Integer`\ , the factorial is computed as an integer (promoted to at least 64 bits).  Note that this may overflow if ``n`` is not small, but you can use ``factorial(big(n))`` to compute the result exactly in arbitrary precision. If ``n`` is not an ``Integer``\ , ``factorial(n)`` is equivalent to :func:`gamma(n+1) <gamma>`\ .

.. function:: gcd(x,y)

   .. Docstring generated from Julia source

   Greatest common (positive) divisor (or zero if ``x`` and ``y`` are both zero).

   .. doctest::

       julia> gcd(6,9)
       3

       julia> gcd(6,-9)
       3

.. function:: lcm(x,y)

   .. Docstring generated from Julia source

   Least common (non-negative) multiple.

   .. doctest::

       julia> lcm(2,3)
       6

       julia> lcm(-2,3)
       6

.. function:: gcdx(x,y)

   .. Docstring generated from Julia source

   Computes the greatest common (positive) divisor of ``x`` and ``y`` and their Bézout coefficients, i.e. the integer coefficients ``u`` and ``v`` that satisfy :math:`ux+vy = d = gcd(x,y)`\ . :math:`gcdx(x,y)` returns :math:`(d,u,v)`\ .

   .. doctest::

       julia> gcdx(12, 42)
       (6,-3,1)

   .. doctest::

       julia> gcdx(240, 46)
       (2,-9,47)

   .. note::
      Bézout coefficients are *not* uniquely defined. ``gcdx`` returns the minimal Bézout coefficients that are computed by the extended Euclidean algorithm. (Ref: D. Knuth, TAoCP, 2/e, p. 325, Algorithm X.) For signed integers, these coefficients ``u`` and ``v`` are minimal in the sense that :math:`|u| < |y/d|` and :math:`|v| < |x/d|`\ . Furthermore, the signs of ``u`` and ``v`` are chosen so that ``d`` is positive. For unsigned integers, the coefficients ``u`` and ``v`` might be near their ``typemax``\ , and the identity then holds only via the unsigned integers' modulo arithmetic.


.. function:: ispow2(n::Integer) -> Bool

   .. Docstring generated from Julia source

   Test whether ``n`` is a power of two.

   .. doctest::

       julia> ispow2(4)
       true

       julia> ispow2(5)
       false

.. function:: nextpow2(n::Integer)

   .. Docstring generated from Julia source

   The smallest power of two not less than ``n``\ . Returns 0 for ``n==0``\ , and returns ``-nextpow2(-n)`` for negative arguments.

   .. doctest::

       julia> nextpow2(16)
       16

       julia> nextpow2(17)
       32

.. function:: prevpow2(n::Integer)

   .. Docstring generated from Julia source

   The largest power of two not greater than ``n``\ . Returns 0 for ``n==0``\ , and returns ``-prevpow2(-n)`` for negative arguments.

   .. doctest::

       julia> prevpow2(5)
       4

.. function:: nextpow(a, x)

   .. Docstring generated from Julia source

   The smallest ``a^n`` not less than ``x``\ , where ``n`` is a non-negative integer. ``a`` must be greater than 1, and ``x`` must be greater than 0.

.. function:: prevpow(a, x)

   .. Docstring generated from Julia source

   The largest ``a^n`` not greater than ``x``\ , where ``n`` is a non-negative integer. ``a`` must be greater than 1, and ``x`` must not be less than 1.

.. function:: nextprod([k_1,k_2,...], n)

   .. Docstring generated from Julia source

   Next integer not less than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`\ , :math:`p_2`\ , etc.

.. function:: invmod(x,m)

   .. Docstring generated from Julia source

   Take the inverse of ``x`` modulo ``m``\ : ``y`` such that :math:`x y = 1 \pmod m`\ , with :math:`div(x,y) = 0`\ . This is undefined for :math:`m = 0`\ , or if :math:`gcd(x,m) \neq 1`\ .

   .. doctest::

       julia> invmod(2,5)
       3

       julia> invmod(2,3)
       2

       julia> invmod(5,6)
       5

.. function:: powermod(x::Integer, p::Integer, m)

   .. Docstring generated from Julia source

   Compute :math:`x^p \pmod m`\ .

.. function:: gamma(x)

   .. Docstring generated from Julia source

   Compute the gamma function of ``x``\ .

.. function:: lgamma(x)

   .. Docstring generated from Julia source

   Compute the logarithm of the absolute value of :func:`gamma` for :obj:`Real` ``x``\ , while for :obj:`Complex` ``x`` it computes the logarithm of ``gamma(x)``\ .

.. function:: lfact(x)

   .. Docstring generated from Julia source

   Compute the logarithmic factorial of ``x``

.. function:: digamma(x)

   .. Docstring generated from Julia source

   Compute the digamma function of ``x`` (the logarithmic derivative of ``gamma(x)``\ )

.. function:: invdigamma(x)

   .. Docstring generated from Julia source

   Compute the inverse digamma function of ``x``\ .

.. function:: trigamma(x)

   .. Docstring generated from Julia source

   Compute the trigamma function of ``x`` (the logarithmic second derivative of ``gamma(x)``\ ).

.. function:: polygamma(m, x)

   .. Docstring generated from Julia source

   Compute the polygamma function of order ``m`` of argument ``x`` (the ``(m+1)th`` derivative of the logarithm of ``gamma(x)``\ )

.. function:: airy(k,x)

   .. Docstring generated from Julia source

   The ``k``\ th derivative of the Airy function :math:`\operatorname{Ai}(x)`\ .

.. function:: airyai(x)

   .. Docstring generated from Julia source

   Airy function :math:`\operatorname{Ai}(x)`\ .

.. function:: airyprime(x)

   .. Docstring generated from Julia source

   Airy function derivative :math:`\operatorname{Ai}'(x)`\ .

.. function:: airyaiprime(x)

   .. Docstring generated from Julia source

   Airy function derivative :math:`\operatorname{Ai}'(x)`\ .

.. function:: airybi(x)

   .. Docstring generated from Julia source

   Airy function :math:`\operatorname{Bi}(x)`\ .

.. function:: airybiprime(x)

   .. Docstring generated from Julia source

   Airy function derivative :math:`\operatorname{Bi}'(x)`\ .

.. function:: airyx(k,x)

   .. Docstring generated from Julia source

   scaled ``k``\ th derivative of the Airy function, return :math:`\operatorname{Ai}(x) e^{\frac{2}{3} x \sqrt{x}}` for ``k == 0 || k == 1``\ , and :math:`\operatorname{Ai}(x) e^{- \left| \operatorname{Re} \left( \frac{2}{3} x \sqrt{x} \right) \right|}` for ``k == 2 || k == 3``\ .

.. function:: besselj0(x)

   .. Docstring generated from Julia source

   Bessel function of the first kind of order 0, :math:`J_0(x)`\ .

.. function:: besselj1(x)

   .. Docstring generated from Julia source

   Bessel function of the first kind of order 1, :math:`J_1(x)`\ .

.. function:: besselj(nu, x)

   .. Docstring generated from Julia source

   Bessel function of the first kind of order ``nu``\ , :math:`J_\nu(x)`\ .

.. function:: besseljx(nu, x)

   .. Docstring generated from Julia source

   Scaled Bessel function of the first kind of order ``nu``\ , :math:`J_\nu(x) e^{- | \operatorname{Im}(x) |}`\ .

.. function:: bessely0(x)

   .. Docstring generated from Julia source

   Bessel function of the second kind of order 0, :math:`Y_0(x)`\ .

.. function:: bessely1(x)

   .. Docstring generated from Julia source

   Bessel function of the second kind of order 1, :math:`Y_1(x)`\ .

.. function:: bessely(nu, x)

   .. Docstring generated from Julia source

   Bessel function of the second kind of order ``nu``\ , :math:`Y_\nu(x)`\ .

.. function:: besselyx(nu, x)

   .. Docstring generated from Julia source

   Scaled Bessel function of the second kind of order ``nu``\ , :math:`Y_\nu(x) e^{- | \operatorname{Im}(x) |}`\ .

.. function:: hankelh1(nu, x)

   .. Docstring generated from Julia source

   Bessel function of the third kind of order ``nu``\ , :math:`H^{(1)}_\nu(x)`\ .

.. function:: hankelh1x(nu, x)

   .. Docstring generated from Julia source

   Scaled Bessel function of the third kind of order ``nu``\ , :math:`H^{(1)}_\nu(x) e^{-x i}`\ .

.. function:: hankelh2(nu, x)

   .. Docstring generated from Julia source

   Bessel function of the third kind of order ``nu``\ , :math:`H^{(2)}_\nu(x)`\ .

.. function:: hankelh2x(nu, x)

   .. Docstring generated from Julia source

   Scaled Bessel function of the third kind of order ``nu``\ , :math:`H^{(2)}_\nu(x) e^{x i}`\ .

.. function:: besselh(nu, [k=1,] x)

   .. Docstring generated from Julia source

   Bessel function of the third kind of order ``nu`` (the Hankel function). ``k`` is either 1 or 2, selecting :func:`hankelh1` or :func:`hankelh2`\ , respectively. ``k`` defaults to 1 if it is omitted. (See also :func:`besselhx` for an exponentially scaled variant.)

.. function:: besselhx(nu, [k=1,] z)

   .. Docstring generated from Julia source

   Compute the scaled Hankel function :math:`\exp(∓iz) H_ν^{(k)}(z)`\ , where :math:`k` is 1 or 2, :math:`H_ν^{(k)}(z)` is ``besselh(nu, k, z)``\ , and :math:`∓` is :math:`-` for :math:`k=1` and :math:`+` for :math:`k=2`\ .  ``k`` defaults to 1 if it is omitted.

   The reason for this function is that :math:`H_ν^{(k)}(z)` is asymptotically proportional to :math:`\exp(∓iz)/\sqrt{z}` for large :math:`|z|`\ , and so the :func:`besselh` function is susceptible to overflow or underflow when ``z`` has a large imaginary part.  The ``besselhx`` function cancels this exponential factor (analytically), so it avoids these problems.

.. function:: besseli(nu, x)

   .. Docstring generated from Julia source

   Modified Bessel function of the first kind of order ``nu``\ , :math:`I_\nu(x)`\ .

.. function:: besselix(nu, x)

   .. Docstring generated from Julia source

   Scaled modified Bessel function of the first kind of order ``nu``\ , :math:`I_\nu(x) e^{- | \operatorname{Re}(x) |}`\ .

.. function:: besselk(nu, x)

   .. Docstring generated from Julia source

   Modified Bessel function of the second kind of order ``nu``\ , :math:`K_\nu(x)`\ .

.. function:: besselkx(nu, x)

   .. Docstring generated from Julia source

   Scaled modified Bessel function of the second kind of order ``nu``\ , :math:`K_\nu(x) e^x`\ .

.. function:: beta(x, y)

   .. Docstring generated from Julia source

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`\ .

.. function:: lbeta(x, y)

   .. Docstring generated from Julia source

   Natural logarithm of the absolute value of the beta function :math:`\log(|\operatorname{B}(x,y)|)`\ .

.. function:: eta(x)

   .. Docstring generated from Julia source

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-1)^{n-1}/n^{s}`\ .

.. function:: zeta(s)

   .. Docstring generated from Julia source

   Riemann zeta function :math:`\zeta(s)`\ .

.. function:: zeta(s, z)

   .. Docstring generated from Julia source

   Generalized zeta function :math:`\zeta(s, z)`\ , defined by the sum :math:`\sum_{k=0}^\infty ((k+z)^2)^{-s/2}`\ , where any term with :math:`k+z=0` is excluded.  For :math:`\Re z > 0`\ , this definition is equivalent to the Hurwitz zeta function :math:`\sum_{k=0}^\infty (k+z)^{-s}`\ .   For :math:`z=1`\ , it yields the Riemann zeta function :math:`\zeta(s)`\ .

.. function:: ndigits(n::Integer, b::Integer=10)

   .. Docstring generated from Julia source

   Compute the number of digits in integer ``n`` written in base ``b``\ .

.. function:: widemul(x, y)

   .. Docstring generated from Julia source

   Multiply ``x`` and ``y``\ , giving the result as a larger type.

   .. doctest::

       julia> widemul(Float32(3.), 4.)
       1.200000000000000000000000000000000000000000000000000000000000000000000000000000e+01

.. function:: @evalpoly(z, c...)

   .. Docstring generated from Julia source

   Evaluate the polynomial :math:`\sum_k c[k] z^{k-1}` for the coefficients ``c[1]``\ , ``c[2]``\ , ...; that is, the coefficients are given in ascending order by power of ``z``\ .  This macro expands to efficient inline code that uses either Horner's method or, for complex ``z``\ , a more efficient Goertzel-like algorithm.

Statistics
----------

.. function:: mean(v[, region])

   .. Docstring generated from Julia source

   Compute the mean of whole array ``v``\ , or optionally along the dimensions in ``region``\ .

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: mean(f::Function, v)

   .. Docstring generated from Julia source

   Apply the function ``f`` to each element of ``v`` and take the mean.

.. function:: mean!(r, v)

   .. Docstring generated from Julia source

   Compute the mean of ``v`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: std(v[, region]; corrected::Bool=true, mean=nothing)

   .. Docstring generated from Julia source

   Compute the sample standard deviation of a vector or array ``v``\ , optionally along dimensions in ``region``\ . The algorithm returns an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v - mean(v)).^2) / (length(v) - 1))``\ . A pre-computed ``mean`` may be provided. If ``corrected`` is ``true``\ , then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x)``\ .

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: stdm(v, m::Number; corrected::Bool=true)

   .. Docstring generated from Julia source

   Compute the sample standard deviation of a vector ``v`` with known mean ``m``\ . If ``corrected`` is ``true``\ , then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x)``\ .

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: var(v[, region]; corrected::Bool=true, mean=nothing)

   .. Docstring generated from Julia source

   Compute the sample variance of a vector or array ``v``\ , optionally along dimensions in ``region``\ . The algorithm will return an estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sumabs2(v - mean(v)) / (length(v) - 1)``\ . If ``corrected`` is ``true``\ , then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x)``\ . The mean ``m`` over the region may be provided.

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: varm(v, m[, region]; corrected::Bool=true)

   .. Docstring generated from Julia source

   Compute the sample variance of a collection ``v`` with known mean(s) ``m``\ , optionally over ``region``\ . ``m`` may contain means for each dimension of ``v``\ . If ``corrected`` is ``true``\ , then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x)``\ .

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: middle(x)

   .. Docstring generated from Julia source

   Compute the middle of a scalar value, which is equivalent to ``x`` itself, but of the type of ``middle(x, x)`` for consistency.

.. function:: middle(x, y)

   .. Docstring generated from Julia source

   Compute the middle of two reals ``x`` and ``y``\ , which is equivalent in both value and type to computing their mean (``(x + y) / 2``\ ).

.. function:: middle(range)

   .. Docstring generated from Julia source

   Compute the middle of a range, which consists of computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.

   .. doctest::

       julia> middle(1:10)
       5.5

.. function:: middle(a)

   .. Docstring generated from Julia source

   Compute the middle of an array ``a``\ , which consists of finding its extrema and then computing their mean.

   .. doctest::

       julia> a = [1,2,3.6,10.9]
       4-element Array{Float64,1}:
         1.0
         2.0
         3.6
        10.9

       julia> middle(a)
       5.95

.. function:: median(v[, region])

   .. Docstring generated from Julia source

   Compute the median of an entire array ``v``\ , or, optionally, along the dimensions in ``region``\ . For an even number of elements no exact median element exists, so the result is equivalent to calculating mean of two median elements.

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended.


.. function:: median!(v)

   .. Docstring generated from Julia source

   Like :func:`median`\ , but may overwrite the input vector.

.. function:: midpoints(e)

   .. Docstring generated from Julia source

   Compute the midpoints of the bins with edges ``e``\ . The result is a vector/range of length ``length(e) - 1``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p; sorted=false)

   .. Docstring generated from Julia source

   Compute the quantile(s) of a vector ``v`` at a specified probability or vector ``p``\ . The keyword argument ``sorted`` indicates whether ``v`` can be assumed to be sorted.

   The ``p`` should be on the interval [0,1], and ``v`` should not have any ``NaN`` values.

   Quantiles are computed via linear interpolation between the points ``((k-1)/(n-1), v[k])``\ , for ``k = 1:n`` where ``n = length(v)``\ . This corresponds to Definition 7 of Hyndman and Fan (1996), and is the same as the R default.

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended. ``quantile`` will throw an ``ArgumentError`` in the presence of ``NaN`` values in the data array.


   * Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages", *The American Statistician*, Vol. 50, No. 4, pp. 361-365

.. function:: quantile!([q, ] v, p; sorted=false)

   .. Docstring generated from Julia source

   Compute the quantile(s) of a vector ``v`` at the probabilities ``p``\ , with optional output into array ``q`` (if not provided, a new output array is created). The keyword argument ``sorted`` indicates whether ``v`` can be assumed to be sorted; if ``false`` (the default), then the elements of ``v`` may be partially sorted.

   The elements of ``p`` should be on the interval [0,1], and ``v`` should not have any ``NaN`` values.

   Quantiles are computed via linear interpolation between the points ``((k-1)/(n-1), v[k])``\ , for ``k = 1:n`` where ``n = length(v)``\ . This corresponds to Definition 7 of Hyndman and Fan (1996), and is the same as the R default.

   .. note::
      Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArrays.jl`` package is recommended. ``quantile!`` will throw an ``ArgumentError`` in the presence of ``NaN`` values in the data array.


   * Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages", *The American Statistician*, Vol. 50, No. 4, pp. 361-365

.. function:: cov(x[, corrected=true])

   .. Docstring generated from Julia source

   Compute the variance of the vector ``x``\ . If ``corrected`` is ``true`` (the default) then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x)``\ .

.. function:: cov(X[, vardim=1, corrected=true])

   .. Docstring generated from Julia source

   Compute the covariance matrix of the matrix ``X`` along the dimension ``vardim``\ . If ``corrected`` is ``true`` (the default) then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = size(X, vardim)``\ .

.. function:: cov(x, y[, corrected=true])

   .. Docstring generated from Julia source

   Compute the covariance between the vectors ``x`` and ``y``\ . If ``corrected`` is ``true`` (the default) then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = length(x) = length(y)``\ .

.. function:: cov(X, Y[, vardim=1, corrected=true])

   .. Docstring generated from Julia source

   Compute the covariance between the vectors or matrices ``X`` and ``Y`` along the dimension ``vardim``\ . If ``corrected`` is ``true`` (the default) then the sum is scaled with ``n-1``\ , whereas the sum is scaled with ``n`` if ``corrected`` is ``false`` where ``n = size(X, vardim) = size(Y, vardim)``\ .

.. function:: cor(x)

   .. Docstring generated from Julia source

   Return the number one.

.. function:: cor(X[, vardim=1])

   .. Docstring generated from Julia source

   Compute the Pearson correlation matrix of the matrix ``X`` along the dimension ``vardim``\ .

.. function:: cor(x, y)

   .. Docstring generated from Julia source

   Compute the Pearson correlation between the vectors ``x`` and ``y``\ .

.. function:: cor(X, Y[, vardim=1])

   .. Docstring generated from Julia source

   Compute the Pearson correlation between the vectors or matrices ``X`` and ``Y`` along the dimension ``vardim``\ .

Signal Processing
-----------------

Fast Fourier transform (FFT) functions in Julia are
implemented by calling functions from `FFTW
<http://www.fftw.org>`_.

.. function:: fft(A [, dims])

   .. Docstring generated from Julia source

   Performs a multidimensional FFT of the array ``A``\ . The optional ``dims`` argument specifies an iterable subset of dimensions (e.g. an integer, range, tuple, or array) to transform along. Most efficient if the size of ``A`` along the transformed dimensions is a product of small primes; see ``nextprod()``\ . See also ``plan_fft()`` for even greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier transform (DFT) as defined by

   .. math::

       \operatorname{DFT}(A)[k] =
         \sum_{n=1}^{\operatorname{length}(A)}
         \exp\left(-i\frac{2\pi
         (n-1)(k-1)}{\operatorname{length}(A)} \right) A[n].

   A multidimensional FFT simply performs this operation along each transformed dimension of ``A``\ .

   .. note::
      * Julia starts FFTW up with 1 thread by default. Higher performance is usually possible by increasing number of threads. Use ``FFTW.set_num_threads(Sys.CPU_CORES)`` to use as many threads as cores on your system.
      * This performs a multidimensional FFT by default. FFT libraries in other languages such as Python and Octave perform a one-dimensional FFT along the first non-singleton dimension of the array. This is worth noting while performing comparisons. For more details, refer to the :ref:`man-noteworthy-differences` section of the manual.


.. function:: fft!(A [, dims])

   .. Docstring generated from Julia source

   Same as :func:`fft`\ , but operates in-place on ``A``\ , which must be an array of complex floating-point numbers.

.. function:: ifft(A [, dims])

   .. Docstring generated from Julia source

   Multidimensional inverse FFT.

   A one-dimensional inverse FFT computes

   .. math::

       \operatorname{IDFT}(A)[k] = \frac{1}{\operatorname{length}(A)}
       \sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi (n-1)(k-1)}
       {\operatorname{length}(A)} \right) A[n].

   A multidimensional inverse FFT simply performs this operation along each transformed dimension of ``A``\ .

.. function:: ifft!(A [, dims])

   .. Docstring generated from Julia source

   Same as :func:`ifft`\ , but operates in-place on ``A``\ .

.. function:: bfft(A [, dims])

   .. Docstring generated from Julia source

   Similar to :func:`ifft`\ , but computes an unnormalized inverse (backward) transform, which must be divided by the product of the sizes of the transformed dimensions in order to obtain the inverse. (This is slightly more efficient than :func:`ifft` because it omits a scaling step, which in some applications can be combined with other computational steps elsewhere.)

   .. math::

       \operatorname{BDFT}(A)[k] = \operatorname{length}(A) \operatorname{IDFT}(A)[k]

.. function:: bfft!(A [, dims])

   .. Docstring generated from Julia source

   Same as :func:`bfft`\ , but operates in-place on ``A``\ .

.. function:: plan_fft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Pre-plan an optimized FFT along given dimensions (``dims``\ ) of arrays matching the shape and type of ``A``\ .  (The first two arguments have the same meaning as for :func:`fft`\ .) Returns an object ``P`` which represents the linear operator computed by the FFT, and which contains all of the information needed to compute ``fft(A, dims)`` quickly.

   To apply ``P`` to an array ``A``\ , use ``P * A``\ ; in general, the syntax for applying plans is much like that of matrices.  (A plan can only be applied to arrays of the same size as the ``A`` for which the plan was created.)  You can also apply a plan with a preallocated output array ``Â`` by calling ``A_mul_B!(Â, plan, A)``\ .  (For ``A_mul_B!``\ , however, the input array ``A`` must be a complex floating-point array like the output ``Â``\ .) You can compute the inverse-transform plan by ``inv(P)`` and apply the inverse plan with ``P \ Â`` (the inverse plan is cached and reused for subsequent calls to ``inv`` or ``\``\ ), and apply the inverse plan to a pre-allocated output array ``A`` with ``A_ldiv_B!(A, P, Â)``\ .

   The ``flags`` argument is a bitwise-or of FFTW planner flags, defaulting to ``FFTW.ESTIMATE``\ . e.g. passing ``FFTW.MEASURE`` or ``FFTW.PATIENT`` will instead spend several seconds (or more) benchmarking different possible FFT algorithms and picking the fastest one; see the FFTW manual for more information on planner flags.  The optional ``timelimit`` argument specifies a rough upper bound on the allowed planning time, in seconds. Passing ``FFTW.MEASURE`` or ``FFTW.PATIENT`` may cause the input array ``A`` to be overwritten with zeros during plan creation.

   :func:`plan_fft!` is the same as :func:`plan_fft` but creates a plan that operates in-place on its argument (which must be an array of complex floating-point numbers). :func:`plan_ifft` and so on are similar but produce plans that perform the equivalent of the inverse transforms :func:`ifft` and so on.

.. function:: plan_ifft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Same as :func:`plan_fft`\ , but produces a plan that performs inverse transforms :func:`ifft`\ .

.. function:: plan_bfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Same as :func:`plan_fft`\ , but produces a plan that performs an unnormalized backwards transform :func:`bfft`\ .

.. function:: plan_fft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Same as :func:`plan_fft`\ , but operates in-place on ``A``\ .

.. function:: plan_ifft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Same as :func:`plan_ifft`\ , but operates in-place on ``A``\ .

.. function:: plan_bfft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Same as :func:`plan_bfft`\ , but operates in-place on ``A``\ .

.. function:: rfft(A [, dims])

   .. Docstring generated from Julia source

   Multidimensional FFT of a real array ``A``\ , exploiting the fact that the transform has conjugate symmetry in order to save roughly half the computational time and storage costs compared with :func:`fft`\ . If ``A`` has size ``(n_1, ..., n_d)``\ , the result has size ``(div(n_1,2)+1, ..., n_d)``\ .

   The optional ``dims`` argument specifies an iterable subset of one or more dimensions of ``A`` to transform, similar to :func:`fft`\ . Instead of (roughly) halving the first dimension of ``A`` in the result, the ``dims[1]`` dimension is (roughly) halved in the same way.

.. function:: irfft(A, d [, dims])

   .. Docstring generated from Julia source

   Inverse of :func:`rfft`\ : for a complex array ``A``\ , gives the corresponding real array whose FFT yields ``A`` in the first half. As for :func:`rfft`\ , ``dims`` is an optional subset of dimensions to transform, defaulting to ``1:ndims(A)``\ .

   ``d`` is the length of the transformed real array along the ``dims[1]`` dimension, which must satisfy ``div(d,2)+1 == size(A,dims[1])``\ . (This parameter cannot be inferred from ``size(A)`` since both ``2*size(A,dims[1])-2`` as well as ``2*size(A,dims[1])-1`` are valid sizes for the transformed real array.)

.. function:: brfft(A, d [, dims])

   .. Docstring generated from Julia source

   Similar to :func:`irfft` but computes an unnormalized inverse transform (similar to :func:`bfft`\ ), which must be divided by the product of the sizes of the transformed dimensions (of the real output array) in order to obtain the inverse transform.

.. function:: plan_rfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Pre-plan an optimized real-input FFT, similar to :func:`plan_fft` except for :func:`rfft` instead of :func:`fft`\ . The first two arguments, and the size of the transformed result, are the same as for :func:`rfft`\ .

.. function:: plan_brfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Pre-plan an optimized real-input unnormalized transform, similar to :func:`plan_rfft` except for :func:`brfft` instead of :func:`rfft`\ . The first two arguments and the size of the transformed result, are the same as for :func:`brfft`\ .

.. function:: plan_irfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   Pre-plan an optimized inverse real-input FFT, similar to :func:`plan_rfft` except for :func:`irfft` and :func:`brfft`\ , respectively. The first three arguments have the same meaning as for :func:`irfft`\ .

.. function:: dct(A [, dims])

   .. Docstring generated from Julia source

   Performs a multidimensional type-II discrete cosine transform (DCT) of the array ``A``\ , using the unitary normalization of the DCT. The optional ``dims`` argument specifies an iterable subset of dimensions (e.g. an integer, range, tuple, or array) to transform along.  Most efficient if the size of ``A`` along the transformed dimensions is a product of small primes; see :func:`nextprod`\ . See also :func:`plan_dct` for even greater efficiency.

.. function:: dct!(A [, dims])

   .. Docstring generated from Julia source

   Same as :func:`dct!`\ , except that it operates in-place on ``A``\ , which must be an array of real or complex floating-point values.

.. function:: idct(A [, dims])

   .. Docstring generated from Julia source

   Computes the multidimensional inverse discrete cosine transform (DCT) of the array ``A`` (technically, a type-III DCT with the unitary normalization). The optional ``dims`` argument specifies an iterable subset of dimensions (e.g. an integer, range, tuple, or array) to transform along.  Most efficient if the size of ``A`` along the transformed dimensions is a product of small primes; see :func:`nextprod`\ .  See also :func:`plan_idct` for even greater efficiency.

.. function:: idct!(A [, dims])

   .. Docstring generated from Julia source

   Same as :func:`idct!`\ , but operates in-place on ``A``\ .

.. function:: plan_dct(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Pre-plan an optimized discrete cosine transform (DCT), similar to :func:`plan_fft` except producing a function that computes :func:`dct`\ . The first two arguments have the same meaning as for :func:`dct`\ .

.. function:: plan_dct!(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Same as :func:`plan_dct`\ , but operates in-place on ``A``\ .

.. function:: plan_idct(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Pre-plan an optimized inverse discrete cosine transform (DCT), similar to :func:`plan_fft` except producing a function that computes :func:`idct`\ . The first two arguments have the same meaning as for :func:`idct`\ .

.. function:: plan_idct!(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Same as :func:`plan_idct`\ , but operates in-place on ``A``\ .

.. function:: fftshift(x)

   .. Docstring generated from Julia source

   Swap the first and second halves of each dimension of ``x``\ .

.. function:: fftshift(x,dim)

   .. Docstring generated from Julia source

   Swap the first and second halves of the given dimension of array ``x``\ .

.. function:: ifftshift(x, [dim])

   .. Docstring generated from Julia source

   Undoes the effect of ``fftshift``\ .

.. function:: filt(b, a, x, [si])

   .. Docstring generated from Julia source

   Apply filter described by vectors ``a`` and ``b`` to vector ``x``\ , with an optional initial filter state vector ``si`` (defaults to zeros).

.. function:: filt!(out, b, a, x, [si])

   .. Docstring generated from Julia source

   Same as :func:`filt` but writes the result into the ``out`` argument, which may alias the input ``x`` to modify it in-place.

.. function:: deconv(b,a) -> c

   .. Docstring generated from Julia source

   Construct vector ``c`` such that ``b = conv(a,c) + r``\ . Equivalent to polynomial division.

.. function:: conv(u,v)

   .. Docstring generated from Julia source

   Convolution of two vectors. Uses FFT algorithm.

.. function:: conv2(u,v,A)

   .. Docstring generated from Julia source

   2-D convolution of the matrix ``A`` with the 2-D separable kernel generated by the vectors ``u`` and ``v``\ . Uses 2-D FFT algorithm.

.. function:: conv2(B,A)

   .. Docstring generated from Julia source

   2-D convolution of the matrix ``B`` with the matrix ``A``\ . Uses 2-D FFT algorithm.

.. function:: xcorr(u,v)

   .. Docstring generated from Julia source

   Compute the cross-correlation of two vectors.

.. currentmodule:: Base.FFTW

The following functions are defined within the ``Base.FFTW`` module.

.. function:: r2r(A, kind [, dims])

   .. Docstring generated from Julia source

   Performs a multidimensional real-input/real-output (r2r) transform of type ``kind`` of the array ``A``\ , as defined in the FFTW manual. ``kind`` specifies either a discrete cosine transform of various types (``FFTW.REDFT00``\ , ``FFTW.REDFT01``\ , ``FFTW.REDFT10``\ , or ``FFTW.REDFT11``\ ), a discrete sine transform of various types (``FFTW.RODFT00``\ , ``FFTW.RODFT01``\ , ``FFTW.RODFT10``\ , or ``FFTW.RODFT11``\ ), a real-input DFT with halfcomplex-format output (``FFTW.R2HC`` and its inverse ``FFTW.HC2R``\ ), or a discrete Hartley transform (``FFTW.DHT``\ ).  The ``kind`` argument may be an array or tuple in order to specify different transform types along the different dimensions of ``A``\ ; ``kind[end]`` is used for any unspecified dimensions.  See the FFTW manual for precise definitions of these transform types, at http://www.fftw.org/doc.

   The optional ``dims`` argument specifies an iterable subset of dimensions (e.g. an integer, range, tuple, or array) to transform along. ``kind[i]`` is then the transform type for ``dims[i]``\ , with ``kind[end]`` being used for ``i > length(kind)``\ .

   See also :func:`plan_r2r` to pre-plan optimized r2r transforms.

.. function:: r2r!(A, kind [, dims])

   .. Docstring generated from Julia source

   Same as :func:`r2r`\ , but operates in-place on ``A``\ , which must be an array of real or complex floating-point numbers.

.. function:: plan_r2r(A, kind [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Pre-plan an optimized r2r transform, similar to :func:`Base.plan_fft` except that the transforms (and the first three arguments) correspond to :func:`r2r` and :func:`r2r!`\ , respectively.

.. function:: plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   Similar to :func:`Base.plan_fft`\ , but corresponds to :func:`r2r!`\ .

.. currentmodule:: Base

Numerical Integration
---------------------

Although several external packages are available for numeric integration
and solution of ordinary differential equations, we also provide
some built-in integration support in Julia.

.. function:: quadgk(f, a,b,c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   .. Docstring generated from Julia source

   Numerically integrate the function ``f(x)`` from ``a`` to ``b``\ , and optionally over additional intervals ``b`` to ``c`` and so on. Keyword options include a relative error tolerance ``reltol`` (defaults to ``sqrt(eps)`` in the precision of the endpoints), an absolute error tolerance ``abstol`` (defaults to 0), a maximum number of function evaluations ``maxevals`` (defaults to ``10^7``\ ), and the ``order`` of the integration rule (defaults to 7).

   Returns a pair ``(I,E)`` of the estimated integral ``I`` and an estimated upper bound on the absolute error ``E``\ . If ``maxevals`` is not exceeded then ``E <= max(abstol, reltol*norm(I))`` will hold. (Note that it is useful to specify a positive ``abstol`` in cases where ``norm(I)`` may be zero.)

   The endpoints ``a`` et cetera can also be complex (in which case the integral is performed over straight-line segments in the complex plane). If the endpoints are ``BigFloat``\ , then the integration will be performed in ``BigFloat`` precision as well.

   .. note::
      It is advisable to increase the integration ``order`` in rough proportion to the precision, for smooth integrands.


   More generally, the precision is set by the precision of the integration endpoints (promoted to floating-point types).

   The integrand ``f(x)`` can return any numeric scalar, vector, or matrix type, or in fact any type supporting ``+``\ , ``-``\ , multiplication by real values, and a ``norm`` (i.e., any normed vector space). Alternatively, a different norm can be specified by passing a ``norm``\ -like function as the ``norm`` keyword argument (which defaults to ``vecnorm``\ ).

   .. note::
      Only one-dimensional integrals are provided by this function. For multi-dimensional integration (cubature), there are many different algorithms (often much better than simple nested 1d integrals) and the optimal choice tends to be very problem-dependent. See the Julia external-package listing for available algorithms for multidimensional integration or other specialized tasks (such as integrals of highly oscillatory or singular functions).


   The algorithm is an adaptive Gauss-Kronrod integration technique: the integral in each interval is estimated using a Kronrod rule (``2*order+1`` points) and the error is estimated using an embedded Gauss rule (``order`` points). The interval with the largest error is then subdivided into two intervals and the process is repeated until the desired error tolerance is achieved.

   These quadrature rules work best for smooth functions within each interval, so if your function has a known discontinuity or other singularity, it is best to subdivide your interval to put the singularity at an endpoint. For example, if ``f`` has a discontinuity at ``x=0.7`` and you want to integrate from 0 to 1, you should use ``quadgk(f, 0,0.7,1)`` to subdivide the interval at the point of discontinuity. The integrand is never evaluated exactly at the endpoints of the intervals, so it is possible to integrate functions that diverge at the endpoints as long as the singularity is integrable (for example, a ``log(x)`` or ``1/sqrt(x)`` singularity).

   For real-valued endpoints, the starting and/or ending points may be infinite. (A coordinate transformation is performed internally to map the infinite interval to a finite one.)


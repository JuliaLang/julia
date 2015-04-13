.. currentmodule:: Base

*************
 Mathematics
*************

.. _mathematical-operators:

Mathematical Operators
----------------------

.. function:: -(x)

   Unary minus operator.

.. _+:
.. function:: +(x, y...)

   Addition operator. ``x+y+z+...`` calls this function with all arguments, i.e.
   ``+(x, y, z, ...)``.

.. _-:
.. function:: -(x, y)

   Subtraction operator.

.. _*:
.. function:: *(x, y...)

   Multiplication operator. ``x*y*z*...`` calls this function with all arguments, i.e.
   ``*(x, y, z, ...)``.

.. _/:
.. function:: /(x, y)

   Right division operator: multiplication of ``x`` by the inverse of ``y`` on the right.
   Gives floating-point results for integer arguments.

.. _\\:
.. function:: \\(x, y)

   Left division operator: multiplication of ``y`` by the inverse of ``x`` on the left.
   Gives floating-point results for integer arguments.

.. _^:
.. function:: ^(x, y)

   Exponentiation operator.

.. _.+:
.. function:: .+(x, y)

   Element-wise addition operator.

.. _.-:
.. function:: .-(x, y)

   Element-wise subtraction operator.

.. _.*:
.. function:: .*(x, y)

   Element-wise multiplication operator.

.. _./:
.. function:: ./(x, y)

   Element-wise right division operator.

.. _.\\:
.. function:: .\\(x, y)

   Element-wise left division operator.

.. _.^:
.. function:: .^(x, y)

   Element-wise exponentiation operator.

.. function:: fma(x, y, z)

   Computes ``x*y+z`` without rounding the intermediate result
   ``x*y``. On some systems this is significantly more expensive than
   ``x*y+z``. ``fma`` is used to improve accuracy in certain
   algorithms. See ``muladd``.

.. function:: muladd(x, y, z)

   Combined multiply-add, computes ``x*y+z`` in an efficient manner.
   This may on some systems be equivalent to ``x*y+z``, or to
   ``fma(x,y,z)``. ``muladd`` is used to improve performance. See
   ``fma``.

.. function:: div(x, y)
              ÷(x, y)

   The quotient from Euclidean division. Computes ``x/y``, truncated to an integer.

.. function:: fld(x, y)

   Largest integer less than or equal to ``x/y``.

.. function:: cld(x, y)

   Smallest integer larger than or equal to ``x/y``.

.. function:: mod(x, y)

   Modulus after division, returning in the range [0,``y``), if ``y`` is
   positive, or (``y``,0] if ``y`` is negative.

.. function:: mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the modulus after
   division by numerically exact 2pi, and is therefore not exactly the same as
   mod(x,2pi), which would compute the modulus of x relative to division by the
   floating-point number 2pi.

.. function:: rem(x, y)
              %(x, y)

   Remainder from Euclidean division, returning a value of the same sign
   as``x``, and smaller in magnitude than ``y``. This value is always exact.

.. function:: divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to ``(x÷y, x%y)``.

.. function:: fldmod(x, y)

   The floored quotient and modulus after division. Equivalent to ``(fld(x,y), mod(x,y))``.

.. function:: mod1(x,m)

   Modulus after division, returning in the range (0,m]

.. function:: rem1(x,m)

   Remainder after division, returning in the range (0,m]

.. _//:
.. function:: //(num, den)

   Divide two integers or rational numbers, giving a ``Rational`` result.

.. function:: rationalize([Type=Int,] x; tol=eps(x))

   Approximate floating point number ``x`` as a Rational number with components of the given
   integer type. The result will differ from ``x`` by no more than ``tol``.

.. function:: num(x)

   Numerator of the rational representation of ``x``

.. function:: den(x)

   Denominator of the rational representation of ``x``

.. _<<:
.. function:: <<(x, n)

   Left bit shift operator.

.. _>>:
.. function:: >>(x, n)

   Right bit shift operator, preserving the sign of ``x``.

.. _>>>:
.. function:: >>>(x, n)

   Unsigned right bit shift operator.

.. _\::
.. function:: \:(start, [step], stop)

   Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1,
   and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the
   function ``colon``.
   The colon is also used in indexing to select whole dimensions.

.. function:: colon(start, [step], stop)

   Called by ``:`` syntax for constructing ranges.

.. function:: range(start, [step], length)

   Construct a range by length, given a starting value and optional step (defaults to 1).

.. _==:
.. function:: ==(x, y)

   Generic equality operator, giving a single ``Bool`` result. Falls back to ``===``.
   Should be implemented for all types with a notion of equality, based
   on the abstract value that an instance represents. For example, all numeric types are compared
   by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring
   encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement ``==`` by calling ``==`` recursively on all contents.

   New numeric types should implement this function for two arguments of the new type, and handle
   comparison to other types via promotion rules where possible.

.. _!=:
.. function:: !=(x, y)
              ≠(x,y)

   Not-equals comparison operator. Always gives the opposite answer as ``==``.
   New types should generally not implement this, and rely on the fallback
   definition ``!=(x,y) = !(x==y)`` instead.

.. _===:
.. function:: ===(x, y)
              ≡(x,y)

   See the :func:`is` operator

.. _!==:
.. function:: !==(x, y)
              ≢(x,y)

   Equivalent to ``!is(x, y)``

.. _<:
.. function:: <(x, y)

   Less-than comparison operator. New numeric types should implement this function
   for two arguments of the new type.
   Because of the behavior of floating-point NaN values, ``<`` implements a
   partial order. Types with a canonical partial order should implement ``<``, and
   types with a canonical total order should implement ``isless``.

.. _<=:
.. function:: <=(x, y)
              ≤(x,y)

   Less-than-or-equals comparison operator.

.. _>:
.. function:: >(x, y)

   Greater-than comparison operator. Generally, new types should implement ``<``
   instead of this function, and rely on the fallback definition ``>(x,y) = y<x``.

.. _>=:
.. function:: >=(x, y)
              ≥(x,y)

   Greater-than-or-equals comparison operator.

.. _.==:
.. function:: .==(x, y)

   Element-wise equality comparison operator.

.. _.!=:
.. function:: .!=(x, y)
              .≠(x,y)

   Element-wise not-equals comparison operator.

.. _.<:
.. function:: .<(x, y)

   Element-wise less-than comparison operator.

.. _.<=:
.. function:: .<=(x, y)
              .≤(x,y)

   Element-wise less-than-or-equals comparison operator.

.. _.>:
.. function:: .>(x, y)

   Element-wise greater-than comparison operator.

.. _.>=:
.. function:: .>=(x, y)
              .≥(x,y)

   Element-wise greater-than-or-equals comparison operator.

.. function:: cmp(x,y)

   Return -1, 0, or 1 depending on whether ``x`` is less than, equal to, or greater
   than ``y``, respectively. Uses the total order implemented by ``isless``. For
   floating-point numbers, uses ``<`` but throws an error for unordered arguments.

.. _~:
.. function:: ~(x)

   Bitwise not

.. _&:
.. function:: &(x, y)

   Bitwise and

.. _|:
.. function:: |(x, y)

   Bitwise or

.. _$:
.. function:: $(x, y)

   Bitwise exclusive or

.. _!:
.. function:: !(x)

   Boolean not

.. _&&:
.. function:: x && y

   Short-circuiting boolean and

.. _||:
.. function:: x || y

   Short-circuiting boolean or

.. function:: A_ldiv_Bc(a,b)

   Matrix operator A \\ B\ :sup:`H`

.. function:: A_ldiv_Bt(a,b)

   Matrix operator A \\ B\ :sup:`T`

.. function:: A_mul_B!(Y, A, B) -> Y


   Calculates the matrix-matrix or matrix-vector product *A B* and stores the
   result in *Y*, overwriting the existing value of *Y*.

   .. doctest::

      julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; A_mul_B!(B, A, B);

      julia> B
      2x2 Array{Float64,2}:
       3.0 3.0
       7.0 7.0

.. function:: A_mul_Bc(...)

   Matrix operator A B\ :sup:`H`

.. function:: A_mul_Bt(...)

   Matrix operator A B\ :sup:`T`

.. function:: A_rdiv_Bc(...)

   Matrix operator A / B\ :sup:`H`

.. function:: A_rdiv_Bt(a,b)

   Matrix operator A / B\ :sup:`T`

.. function:: Ac_ldiv_B(...)

   Matrix operator A\ :sup:`H` \\ B

.. function:: Ac_ldiv_Bc(...)

   Matrix operator A\ :sup:`H` \\ B\ :sup:`H`

.. function:: Ac_mul_B(...)

   Matrix operator A\ :sup:`H` B

.. function:: Ac_mul_Bc(...)

   Matrix operator A\ :sup:`H` B\ :sup:`H`

.. function:: Ac_rdiv_B(a,b)

   Matrix operator A\ :sup:`H` / B

.. function:: Ac_rdiv_Bc(a,b)

   Matrix operator A\ :sup:`H` / B\ :sup:`H`

.. function:: At_ldiv_B(...)

   Matrix operator A\ :sup:`T` \\ B

.. function:: At_ldiv_Bt(...)

   Matrix operator A\ :sup:`T` \\ B\ :sup:`T`

.. function:: At_mul_B(...)

   Matrix operator A\ :sup:`T` B

.. function:: At_mul_Bt(...)

   Matrix operator A\ :sup:`T` B\ :sup:`T`

.. function:: At_rdiv_B(a,b)

   Matrix operator A\ :sup:`T` / B

.. function:: At_rdiv_Bt(a,b)

   Matrix operator A\ :sup:`T` / B\ :sup:`T`


Mathematical Functions
----------------------

.. function:: isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

   Inexact equality comparison - behaves slightly different depending on types of input args:

   * For ``FloatingPoint`` numbers, ``isapprox`` returns ``true`` if ``abs(x-y) <= atol + rtol*max(abs(x), abs(y))``.

   * For ``Integer`` and ``Rational`` numbers, ``isapprox`` returns ``true`` if ``abs(x-y) <= atol``. The `rtol` argument is ignored. If one of ``x`` and ``y`` is ``FloatingPoint``, the other is promoted, and the method above is called instead.

   * For ``Complex`` numbers, the distance in the complex plane is compared, using the same criterion as above.

   For default tolerance arguments, ``maxeps = max(eps(abs(x)), eps(abs(y)))``.

.. function:: sin(x)

   Compute sine of ``x``, where ``x`` is in radians

.. function:: cos(x)

   Compute cosine of ``x``, where ``x`` is in radians

.. function:: tan(x)

   Compute tangent of ``x``, where ``x`` is in radians

.. function:: sind(x)

   Compute sine of ``x``, where ``x`` is in degrees

.. function:: cosd(x)

   Compute cosine of ``x``, where ``x`` is in degrees

.. function:: tand(x)

   Compute tangent of ``x``, where ``x`` is in degrees

.. function:: sinpi(x)

   Compute :math:`\sin(\pi x)` more accurately than ``sin(pi*x)``, especially for large ``x``.

.. function:: cospi(x)

   Compute :math:`\cos(\pi x)` more accurately than ``cos(pi*x)``, especially for large ``x``.

.. function:: sinh(x)

   Compute hyperbolic sine of ``x``

.. function:: cosh(x)

   Compute hyperbolic cosine of ``x``

.. function:: tanh(x)

   Compute hyperbolic tangent of ``x``

.. function:: asin(x)

   Compute the inverse sine of ``x``, where the output is in radians

.. function:: acos(x)

   Compute the inverse cosine of ``x``, where the output is in radians

.. function:: atan(x)

   Compute the inverse tangent of ``x``, where the output is in radians

.. function:: atan2(y, x)

   Compute the inverse tangent of ``y/x``, using the signs of both ``x`` and ``y`` to determine the quadrant of the return value.

.. function:: asind(x)

   Compute the inverse sine of ``x``, where the output is in degrees

.. function:: acosd(x)

   Compute the inverse cosine of ``x``, where the output is in degrees

.. function:: atand(x)

   Compute the inverse tangent of ``x``, where the output is in degrees

.. function:: sec(x)

   Compute the secant of ``x``, where ``x`` is in radians

.. function:: csc(x)

   Compute the cosecant of ``x``, where ``x`` is in radians

.. function:: cot(x)

   Compute the cotangent of ``x``, where ``x`` is in radians

.. function:: secd(x)

   Compute the secant of ``x``, where ``x`` is in degrees

.. function:: cscd(x)

   Compute the cosecant of ``x``, where ``x`` is in degrees

.. function:: cotd(x)

   Compute the cotangent of ``x``, where ``x`` is in degrees

.. function:: asec(x)

   Compute the inverse secant of ``x``, where the output is in radians

.. function:: acsc(x)

   Compute the inverse cosecant of ``x``, where the output is in radians

.. function:: acot(x)

   Compute the inverse cotangent of ``x``, where the output is in radians

.. function:: asecd(x)

   Compute the inverse secant of ``x``, where the output is in degrees

.. function:: acscd(x)

   Compute the inverse cosecant of ``x``, where the output is in degrees

.. function:: acotd(x)

   Compute the inverse cotangent of ``x``, where the output is in degrees

.. function:: sech(x)

   Compute the hyperbolic secant of ``x``

.. function:: csch(x)

   Compute the hyperbolic cosecant of ``x``

.. function:: coth(x)

   Compute the hyperbolic cotangent of ``x``

.. function:: asinh(x)

   Compute the inverse hyperbolic sine of ``x``

.. function:: acosh(x)

   Compute the inverse hyperbolic cosine of ``x``

.. function:: atanh(x)

   Compute the inverse hyperbolic tangent of ``x``

.. function:: asech(x)

   Compute the inverse hyperbolic secant of ``x``

.. function:: acsch(x)

   Compute the inverse hyperbolic cosecant of ``x``

.. function:: acoth(x)

   Compute the inverse hyperbolic cotangent of ``x``

.. function:: sinc(x)

   Compute :math:`\sin(\pi x) / (\pi x)` if :math:`x \neq 0`, and :math:`1` if :math:`x = 0`.

.. function:: cosc(x)

   Compute :math:`\cos(\pi x) / x - \sin(\pi x) / (\pi x^2)` if :math:`x \neq 0`, and :math:`0`
   if :math:`x = 0`. This is the derivative of ``sinc(x)``.

.. function:: deg2rad(x)

   Convert ``x`` from degrees to radians

.. function:: rad2deg(x)

   Convert ``x`` from radians to degrees

.. function:: hypot(x, y)

   Compute the :math:`\sqrt{x^2+y^2}` avoiding overflow and underflow

.. function:: log(x)

   Compute the natural logarithm of ``x``. Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.

.. function:: log(b,x)

   Compute the base ``b`` logarithm of ``x``. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log2(x)

   Compute the logarithm of ``x`` to base 2. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log10(x)

   Compute the logarithm of ``x`` to base 10. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log1p(x)

   Accurate natural logarithm of ``1+x``.  Throws ``DomainError`` for ``Real`` arguments less than -1.

.. function:: frexp(val)

   Return ``(x,exp)`` such that ``x`` has a magnitude in the interval ``[1/2, 1)`` or 0,
   and val = :math:`x \times 2^{exp}`.

.. function:: exp(x)

   Compute :math:`e^x`

.. function:: exp2(x)

   Compute :math:`2^x`

.. function:: exp10(x)

   Compute :math:`10^x`

.. function:: ldexp(x, n)

   Compute :math:`x \times 2^n`

.. function:: modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts of a
   number. Both parts have the same sign as the argument.

.. function:: expm1(x)

   Accurately compute :math:`e^x-1`

.. function:: round([T,] x, [digits, [base]], [r::RoundingMode])

   ``round(x)`` rounds ``x`` to an integer value according to the default
   rounding mode (see :func:`get_rounding`), returning a value of the same type as
   ``x``. By default (:obj:`RoundNearest`), this will round to the nearest
   integer, with ties (fractional values of 0.5) being rounded to the even
   integer.

   .. doctest::

      julia> round(1.7)
      2.0

      julia> round(1.5)
      2.0

      julia> round(2.5)
      2.0

   The optional :obj:`RoundingMode` argument will change how the number gets rounded.

   ``round(T, x, [r::RoundingMode])`` converts the result to type ``T``, throwing an
   :exc:`InexactError` if the value is not representable.

   ``round(x, digits)`` rounds to the specified number of digits after the
   decimal place (or before if negative). ``round(x, digits, base)`` rounds
   using a base other than 10.

      .. doctest::

	 julia> round(pi, 2)
	 3.14

	 julia> round(pi, 3, 2)
	 3.125

   .. note::

      Rounding to specified digits in bases other than 2 can be inexact when
      operating on binary floating point numbers. For example, the ``Float64``
      value represented by ``1.15`` is actually *less* than 1.15, yet will be
      rounded to 1.2.

      .. doctest::

	 julia> x = 1.15
	 1.15

	 julia> @sprintf "%.20f" x
	 "1.14999999999999991118"

	 julia> x < 115//100
	 true

	 julia> round(x, 1)
	 1.2

.. data:: RoundingMode

   A type which controls rounding behavior. Currently supported rounding modes are:

   - :obj:`RoundNearest` (default)
   - :obj:`RoundNearestTiesAway`
   - :obj:`RoundNearestTiesUp`
   - :obj:`RoundToZero`
   - :obj:`RoundUp`
   - :obj:`RoundDown`

.. data:: RoundNearest

   The default rounding mode. Rounds to the nearest integer, with ties
   (fractional values of 0.5) being rounded to the nearest even integer.

.. data:: RoundNearestTiesAway

   Rounds to nearest integer, with ties rounded away from zero (C/C++
   :func:`round` behaviour).

.. data:: RoundNearestTiesUp

   Rounds to nearest integer, with ties rounded toward positive infinity
   (Java/JavaScript :func:`round` behaviour).

.. data:: RoundToZero

   :func:`round` using this rounding mode is an alias for :func:`trunc`.

.. data:: RoundUp

   :func:`round` using this rounding mode is an alias for :func:`ceil`.

.. data:: RoundDown

   :func:`round` using this rounding mode is an alias for :func:`floor`.

.. function:: round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex-valued
   ``z`` to ``z``, breaking ties using the specified :obj:`RoundingMode`\ s.
   The first :obj:`RoundingMode` is used for rounding the real components while
   the second is used for rounding the imaginary components.

.. function:: ceil([T,] x, [digits, [base]])

   ``ceil(x)`` returns the nearest integral value of the same type as ``x``
   that is greater than or equal to ``x``.

   ``ceil(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`.

.. function:: floor([T,] x, [digits, [base]])

   ``floor(x)`` returns the nearest integral value of the same type as ``x``
   that is less than or equal to ``x``.

   ``floor(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`.

.. function:: trunc([T,] x, [digits, [base]])

   ``trunc(x)`` returns the nearest integral value of the same type as ``x`` whose absolute
   value is less than or equal to ``x``.

   ``trunc(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`.

.. function:: unsafe_trunc(T, x)

   ``unsafe_trunc(T, x)`` returns the nearest integral value of type ``T`` whose absolute
   value is less than or equal to ``x``. If the value is not representable by
   ``T``, an arbitrary value will be returned.

.. function:: signif(x, digits, [base])

   Rounds (in the sense of ``round``) ``x`` so that there are ``digits`` significant digits, under a base ``base`` representation, default 10. E.g., ``signif(123.456, 2)`` is ``120.0``, and ``signif(357.913, 4, 2)`` is ``352.0``.

.. function:: min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over arrays.

.. function:: max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over arrays.

.. function:: minmax(x, y)

   Return ``(min(x,y), max(x,y))``.
   See also: :func:`extrema` that returns ``(minimum(x), maximum(x))``

.. function:: clamp(x, lo, hi)

   Return x if ``lo <= x <= hi``. If ``x < lo``, return ``lo``. If ``x > hi``, return ``hi``. Arguments are promoted to a common type. Operates elementwise over ``x`` if it is an array.

.. function:: abs(x)

   Absolute value of ``x``

.. function:: abs2(x)

   Squared absolute value of ``x``

.. function:: copysign(x, y)

   Return ``x`` such that it has the same sign as ``y``

.. function:: sign(x)

   Return ``+1`` if ``x`` is positive, ``0`` if ``x == 0``, and ``-1`` if ``x`` is negative.

.. function:: signbit(x)

   Returns ``true`` if the value of the sign of ``x`` is negative, otherwise ``false``.

.. function:: flipsign(x, y)

   Return ``x`` with its sign flipped if ``y`` is negative. For example ``abs(x) = flipsign(x,x)``.

.. function:: sqrt(x)

   Return :math:`\sqrt{x}`. Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.  The prefix operator ``√`` is equivalent to ``sqrt``.

.. function:: isqrt(n)

   Integer square root: the largest integer ``m`` such that ``m*m <= n``.

.. function:: cbrt(x)

   Return :math:`x^{1/3}`.  The prefix operator ``∛`` is equivalent to ``cbrt``.

.. function:: erf(x)

   Compute the error function of ``x``, defined by
   :math:`\frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt`
   for arbitrary complex ``x``.

.. function:: erfc(x)

   Compute the complementary error function of ``x``,
   defined by :math:`1 - \operatorname{erf}(x)`.

.. function:: erfcx(x)

   Compute the scaled complementary error function of ``x``,
   defined by :math:`e^{x^2} \operatorname{erfc}(x)`.  Note
   also that :math:`\operatorname{erfcx}(-ix)` computes the
   Faddeeva function :math:`w(x)`.

.. function:: erfi(x)

   Compute the imaginary error function of ``x``,
   defined by :math:`-i \operatorname{erf}(ix)`.

.. function:: dawson(x)

   Compute the Dawson function (scaled imaginary error function) of ``x``,
   defined by :math:`\frac{\sqrt{\pi}}{2} e^{-x^2} \operatorname{erfi}(x)`.

.. function:: erfinv(x)

   Compute the inverse error function of a real ``x``,
   defined by :math:`\operatorname{erf}(\operatorname{erfinv}(x)) = x`.

.. function:: erfcinv(x)

   Compute the inverse error complementary function of a real ``x``,
   defined by :math:`\operatorname{erfc}(\operatorname{erfcinv}(x)) = x`.

.. function:: real(z)

   Return the real part of the complex number ``z``

.. function:: imag(z)

   Return the imaginary part of the complex number ``z``

.. function:: reim(z)

   Return both the real and imaginary parts of the complex number ``z``

.. function:: conj(z)

   Compute the complex conjugate of a complex number ``z``

.. function:: angle(z)

   Compute the phase angle in radians of a complex number ``z``

.. function:: cis(z)

   Return :math:`\exp(iz)`.

.. function:: binomial(n,k)

   Number of ways to choose ``k`` out of ``n`` items

.. function:: factorial(n)

   Factorial of ``n``.  If ``n`` is an :obj:`Integer`, the factorial
   is computed as an integer (promoted to at least 64 bits).  Note
   that this may overflow if ``n`` is not small, but you can use
   ``factorial(big(n))`` to compute the result exactly in arbitrary
   precision.  If ``n`` is not an ``Integer``, ``factorial(n)`` is
   equivalent to :func:`gamma(n+1) <gamma>`.

.. function:: factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: factor(n) -> Dict

   Compute the prime factorization of an integer ``n``. Returns a dictionary. The keys of the dictionary correspond to the factors, and hence are of the same type as ``n``. The value associated with each key indicates the number of times the factor appears in the factorization.

   .. doctest::

      julia> factor(100) # == 2*2*5*5
      Dict{Int64,Int64} with 2 entries:
        2 => 2
        5 => 2

.. function:: gcd(x,y)

   Greatest common (positive) divisor (or zero if x and y are both zero).

.. function:: lcm(x,y)

   Least common (non-negative) multiple.

.. function:: gcdx(x,y)

   Computes the greatest common (positive) divisor of ``x`` and ``y`` and their Bézout coefficients, i.e. the integer coefficients ``u`` and ``v`` that satisfy :math:`ux+vy = d = gcd(x,y)`.

   .. doctest::

      julia> gcdx(12, 42)
      (6,-3,1)

   .. doctest::

      julia> gcdx(240, 46)
      (2,-9,47)

   .. note::

      Bézout coefficients are *not* uniquely defined. ``gcdx`` returns the minimal Bézout coefficients that are computed by the extended Euclid algorithm. (Ref: D. Knuth, TAoCP, 2/e, p. 325, Algorithm X.) These coefficients ``u`` and ``v`` are minimal in the sense that :math:`|u| < |\frac y d` and :math:`|v| < |\frac x d`. Furthermore, the signs of ``u`` and ``v`` are chosen so that ``d`` is positive.

.. function:: ispow2(n) -> Bool

   Test whether ``n`` is a power of two

.. function:: nextpow2(n)

   The smallest power of two not less than ``n``. Returns 0 for ``n==0``, and returns
   ``-nextpow2(-n)`` for negative arguments.

.. function:: prevpow2(n)

   The largest power of two not greater than ``n``. Returns 0 for ``n==0``, and returns
   ``-prevpow2(-n)`` for negative arguments.

.. function:: nextpow(a, x)

   The smallest ``a^n`` not less than ``x``, where ``n`` is a non-negative integer.
   ``a`` must be greater than 1, and ``x`` must be greater than 0.

.. function:: prevpow(a, x)

   The largest ``a^n`` not greater than ``x``, where ``n`` is a non-negative integer.
   ``a`` must be greater than 1, and ``x`` must not be less than 1.

.. function:: nextprod([k_1,k_2,...], n)

   Next integer not less than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: prevprod([k_1,k_2,...], n)

   Previous integer not greater than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: invmod(x,m)

   Take the inverse of ``x`` modulo ``m``: ``y`` such that :math:`xy = 1 \pmod m`

.. function:: powermod(x, p, m)

   Compute :math:`x^p \pmod m`

.. function:: gamma(x)

   Compute the gamma function of ``x``

.. function:: lgamma(x)

   Compute the logarithm of the absolute value of :func:`gamma` for
   :obj:`Real` ``x``, while for :obj:`Complex` ``x`` it computes the
   logarithm of ``gamma(x)``.

.. function:: lfact(x)

   Compute the logarithmic factorial of ``x``

.. function:: digamma(x)

   Compute the digamma function of ``x`` (the logarithmic derivative of ``gamma(x)``)

.. function:: invdigamma(x)

   Compute the inverse digamma function of ``x``.

.. function:: trigamma(x)

   Compute the trigamma function of ``x`` (the logarithmic second derivative of ``gamma(x)``)

.. function:: polygamma(m, x)

   Compute the polygamma function of order ``m`` of argument ``x`` (the ``(m+1)th`` derivative of the logarithm of ``gamma(x)``)

.. function:: airy(k,x)

   kth derivative of the Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyai(x)

   Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airyaiprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airybi(x)

   Airy function :math:`\operatorname{Bi}(x)`.

.. function:: airybiprime(x)

   Airy function derivative :math:`\operatorname{Bi}'(x)`.

.. function:: airyx(k,x)

   scaled kth derivative of the Airy function, return :math:`\operatorname{Ai}(x) e^{\frac{2}{3} x \sqrt{x}}` for ``k == 0 || k == 1``, and :math:`\operatorname{Ai}(x) e^{- \left| \operatorname{Re} \left( \frac{2}{3} x \sqrt{x} \right) \right|}` for ``k == 2 || k == 3``.

.. function:: besselj0(x)

   Bessel function of the first kind of order 0, :math:`J_0(x)`.

.. function:: besselj1(x)

   Bessel function of the first kind of order 1, :math:`J_1(x)`.

.. function:: besselj(nu, x)

   Bessel function of the first kind of order ``nu``, :math:`J_\nu(x)`.

.. function:: besseljx(nu, x)

   Scaled Bessel function of the first kind of order ``nu``, :math:`J_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: bessely0(x)

   Bessel function of the second kind of order 0, :math:`Y_0(x)`.

.. function:: bessely1(x)

   Bessel function of the second kind of order 1, :math:`Y_1(x)`.

.. function:: bessely(nu, x)

   Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x)`.

.. function:: besselyx(nu, x)

   Scaled Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: hankelh1(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x)`.

.. function:: hankelh1x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x) e^{-x i}`.

.. function:: hankelh2(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x)`.

.. function:: hankelh2x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x) e^{x i}`.

.. function:: besselh(nu, k, x)

   Bessel function of the third kind of order ``nu`` (Hankel function).
   ``k`` is either 1 or 2, selecting ``hankelh1`` or ``hankelh2``, respectively.

.. function:: besseli(nu, x)

   Modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x)`.

.. function:: besselix(nu, x)

   Scaled modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x) e^{- | \operatorname{Re}(x) |}`.

.. function:: besselk(nu, x)

   Modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x)`.

.. function:: besselkx(nu, x)

   Scaled modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x) e^x`.

.. function:: beta(x, y)

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`.

.. function:: lbeta(x, y)

   Natural logarithm of the absolute value of the beta function :math:`\log(|\operatorname{B}(x,y)|)`.

.. function:: eta(x)

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-)^{n-1}/n^{s}`.

.. function:: zeta(s)

   Riemann zeta function :math:`\zeta(s)`.

.. function:: zeta(s, z)

   Hurwitz zeta function :math:`\zeta(s, z)`.  (This is equivalent to
   the Riemann zeta function :math:`\zeta(s)` for the case of ``z=1``.)

.. function:: ndigits(n, b)

   Compute the number of digits in number ``n`` written in base ``b``.

.. function:: widemul(x, y)

   Multiply ``x`` and ``y``, giving the result as a larger type.

.. function:: @evalpoly(z, c...)

   Evaluate the polynomial :math:`\sum_k c[k] z^{k-1}` for the
   coefficients ``c[1]``, ``c[2]``, ...; that is, the coefficients are
   given in ascending order by power of ``z``.  This macro expands to
   efficient inline code that uses either Horner's method or, for
   complex ``z``, a more efficient Goertzel-like algorithm.

Statistics
----------

.. function:: mean(v[, region])

   Compute the mean of whole array ``v``, or optionally along the dimensions in ``region``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: mean!(r, v)

   Compute the mean of ``v`` over the singleton dimensions of ``r``, and write results to ``r``.

.. function:: std(v[, region])

   Compute the sample standard deviation of a vector or array ``v``, optionally along dimensions in ``region``. The algorithm returns an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v - mean(v)).^2) / (length(v) - 1))``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: stdm(v, m)

   Compute the sample standard deviation of a vector ``v`` with known mean ``m``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: var(v[, region])

   Compute the sample variance of a vector or array ``v``, optionally along dimensions in ``region``. The algorithm will return an estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sum((v - mean(v)).^2) / (length(v) - 1)``.
   Note: Julia does not ignore ``NaN`` values in the computation.
   For applications requiring the handling of missing data, the ``DataArray``
   package is recommended.

.. function:: varm(v, m)

   Compute the sample variance of a vector ``v`` with known mean ``m``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself,
   but of the type of ``middle(x, x)`` for consistency.

.. function:: middle(x, y)

   Compute the middle of two reals ``x`` and ``y``, which is equivalent
   in both value and type to computing their mean (``(x + y) / 2``).

.. function:: middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema.
   Since a range is sorted, the mean is performed with the first and last element.

.. function:: middle(array)

   Compute the middle of an array, which consists in finding its extrema and
   then computing their mean.

.. function:: median(v)

   Compute the median of a vector ``v``. ``NaN`` is returned if the data
   contains any ``NaN`` values. For applications requiring the handling of
   missing data, the ``DataArrays`` package is recommended.

.. function:: median!(v)

   Like ``median``, but may overwrite the input vector.

.. function:: hist(v[, n]) -> e, counts

   Compute the histogram of ``v``, optionally using approximately ``n``
   bins. The return values are a range ``e``, which correspond to the
   edges of the bins, and ``counts`` containing the number of elements of
   ``v`` in each bin.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist(v, e) -> e, counts

   Compute the histogram of ``v`` using a vector/range ``e`` as the edges for
   the bins. The result will be a vector of length ``length(e) - 1``, such that the
   element at location ``i`` satisfies ``sum(e[i] .< v .<= e[i+1])``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist!(counts, v, e) -> e, counts

   Compute the histogram of ``v``, using a vector/range ``e`` as the edges for the bins.
   This function writes the resultant counts to a pre-allocated array ``counts``.

.. function:: hist2d(M, e1, e2) -> (edge1, edge2, counts)

   Compute a "2d histogram" of a set of N points specified by N-by-2 matrix ``M``.
   Arguments ``e1`` and ``e2`` are bins for each dimension, specified either as
   integer bin counts or vectors of bin edges. The result is a tuple of
   ``edge1`` (the bin edges used in the first dimension), ``edge2`` (the bin edges
   used in the second dimension), and ``counts``, a histogram matrix of size
   ``(length(edge1)-1, length(edge2)-1)``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist2d!(counts, M, e1, e2) -> (e1, e2, counts)

   Compute a "2d histogram" with respect to the bins delimited by the edges given
   in ``e1`` and ``e2``. This function writes the results to a pre-allocated
   array ``counts``.

.. function:: histrange(v, n)

   Compute *nice* bin ranges for the edges of a histogram of ``v``, using
   approximately ``n`` bins. The resulting step sizes will be 1, 2 or 5
   multiplied by a power of 10.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: midpoints(e)

   Compute the midpoints of the bins with edges ``e``. The result is a
   vector/range of length ``length(e) - 1``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   Compute the quantiles of a vector ``v`` at a specified set of probability values ``p``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   Compute the quantile of a vector ``v`` at the probability ``p``.
   Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile!(v, p)

   Like ``quantile``, but overwrites the input vector.

.. function:: cov(v1[, v2][, vardim=1, corrected=true, mean=nothing])

   Compute the Pearson covariance between the vector(s) in ``v1`` and ``v2``.
   Here, ``v1`` and ``v2`` can be either vectors or matrices.

   This function accepts three keyword arguments:

   - ``vardim``: the dimension of variables. When ``vardim = 1``, variables
     are considered in columns while observations in rows; when ``vardim = 2``,
     variables are in rows while observations in columns. By default, it is
     set to ``1``.

   - ``corrected``: whether to apply Bessel's correction (divide by ``n-1``
     instead of ``n``). By default, it is set to ``true``.

   - ``mean``: allow users to supply mean values that are known. By default,
     it is set to ``nothing``, which indicates that the mean(s) are unknown,
     and the function will compute the mean. Users can use ``mean=0`` to
     indicate that the input data are centered, and hence there's no need to
     subtract the mean.

   The size of the result depends on the size of ``v1`` and ``v2``. When both
   ``v1`` and ``v2`` are vectors, it returns the covariance between them as a
   scalar. When either one is a matrix, it returns a covariance matrix of size
   ``(n1, n2)``, where ``n1`` and ``n2`` are the numbers of slices in ``v1`` and
   ``v2``, which depend on the setting of ``vardim``.

   Note: ``v2`` can be omitted, which indicates ``v2 = v1``.


.. function:: cor(v1[, v2][, vardim=1, mean=nothing])

   Compute the Pearson correlation between the vector(s) in ``v1`` and ``v2``.

   Users can use the keyword argument ``vardim`` to specify the variable
   dimension, and ``mean`` to supply pre-computed mean values.


Signal Processing
-----------------

Fast Fourier transform (FFT) functions in Julia are largely
implemented by calling functions from `FFTW
<http://www.fftw.org>`_. By default, Julia does not use multi-threaded
FFTW. Higher performance may be obtained by experimenting with
multi-threading. Use `FFTW.set_num_threads(np)` to use `np` threads.

.. function:: fft(A [, dims])

   Performs a multidimensional FFT of the array ``A``.  The optional ``dims``
   argument specifies an iterable subset of dimensions (e.g. an integer,
   range, tuple, or array) to transform along.  Most efficient if the
   size of ``A`` along the transformed dimensions is a product of small
   primes; see :func:`nextprod`.  See also :func:`plan_fft` for even
   greater efficiency.

   A one-dimensional FFT computes the one-dimensional discrete Fourier
   transform (DFT) as defined by

   .. math::

      \operatorname{DFT}(A)[k] = \sum_{n=1}^{\operatorname{length}(A)}
      \exp\left(-i\frac{2\pi (n-1)(k-1)}{\operatorname{length}(A)} \right)
      A[n].

   A multidimensional FFT simply performs this operation along each transformed
   dimension of ``A``.

   Higher performance is usually possible with multi-threading. Use
   `FFTW.set_num_threads(np)` to use `np` threads, if you have `np`
   processors.

.. function:: fft!(A [, dims])

   Same as :func:`fft`, but operates in-place on ``A``,
   which must be an array of complex floating-point numbers.

.. function:: ifft(A [, dims])

   Multidimensional inverse FFT.

   A one-dimensional inverse FFT computes

   .. math::

      \operatorname{IDFT}(A)[k] = \frac{1}{\operatorname{length}(A)}
      \sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi (n-1)(k-1)}
      {\operatorname{length}(A)} \right) A[n].

   A multidimensional inverse FFT simply performs this operation along each
   transformed dimension of ``A``.

.. function:: ifft!(A [, dims])

   Same as :func:`ifft`, but operates in-place on ``A``.

.. function:: bfft(A [, dims])

   Similar to :func:`ifft`, but computes an unnormalized inverse (backward)
   transform, which must be divided by the product of the sizes of the
   transformed dimensions in order to obtain the inverse. (This is slightly
   more efficient than :func:`ifft` because it omits a scaling step, which in
   some applications can be combined with other computational steps elsewhere.)

   .. math::

      \operatorname{BDFT}(A)[k] = \operatorname{length}(A) \operatorname{IDFT}(A)[k]

.. function:: bfft!(A [, dims])

   Same as :func:`bfft`, but operates in-place on ``A``.

.. function:: plan_fft(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized FFT along given dimensions (``dims``) of arrays
   matching the shape and type of ``A``.  (The first two arguments have
   the same meaning as for :func:`fft`.)  Returns a function ``plan(A)``
   that computes ``fft(A, dims)`` quickly.

   The ``flags`` argument is a bitwise-or of FFTW planner flags, defaulting
   to ``FFTW.ESTIMATE``.  e.g. passing ``FFTW.MEASURE`` or ``FFTW.PATIENT``
   will instead spend several seconds (or more) benchmarking different
   possible FFT algorithms and picking the fastest one; see the FFTW manual
   for more information on planner flags.  The optional ``timelimit`` argument
   specifies a rough upper bound on the allowed planning time, in seconds.
   Passing ``FFTW.MEASURE`` or ``FFTW.PATIENT`` may cause the input array ``A``
   to be overwritten with zeros during plan creation.

   :func:`plan_fft!` is the same as :func:`plan_fft` but creates a plan
   that operates in-place on its argument (which must be an array of
   complex floating-point numbers).  :func:`plan_ifft` and so on
   are similar but produce plans that perform the equivalent of
   the inverse transforms :func:`ifft` and so on.

.. function:: plan_ifft(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but produces a plan that performs inverse transforms
   :func:`ifft`.

.. function:: plan_bfft(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but produces a plan that performs an unnormalized
   backwards transform :func:`bfft`.

.. function:: plan_fft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_fft`, but operates in-place on ``A``.

.. function:: plan_ifft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_ifft`, but operates in-place on ``A``.

.. function:: plan_bfft!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_bfft`, but operates in-place on ``A``.

.. function:: rfft(A [, dims])

   Multidimensional FFT of a real array A, exploiting the fact that
   the transform has conjugate symmetry in order to save roughly half
   the computational time and storage costs compared with :func:`fft`.
   If ``A`` has size ``(n_1, ..., n_d)``, the result has size
   ``(floor(n_1/2)+1, ..., n_d)``.

   The optional ``dims`` argument specifies an iterable subset of one or
   more dimensions of ``A`` to transform, similar to :func:`fft`.  Instead
   of (roughly) halving the first dimension of ``A`` in the result, the
   ``dims[1]`` dimension is (roughly) halved in the same way.

.. function:: irfft(A, d [, dims])

   Inverse of :func:`rfft`: for a complex array ``A``, gives the
   corresponding real array whose FFT yields ``A`` in the first half.
   As for :func:`rfft`, ``dims`` is an optional subset of dimensions
   to transform, defaulting to ``1:ndims(A)``.

   ``d`` is the length of the transformed real array along the ``dims[1]``
   dimension, which must satisfy ``d == floor(size(A,dims[1])/2)+1``.
   (This parameter cannot be inferred from ``size(A)`` due to the
   possibility of rounding by the ``floor`` function here.)

.. function:: brfft(A, d [, dims])

   Similar to :func:`irfft` but computes an unnormalized inverse transform
   (similar to :func:`bfft`), which must be divided by the product
   of the sizes of the transformed dimensions (of the real output array)
   in order to obtain the inverse transform.

.. function:: plan_rfft(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized real-input FFT, similar to :func:`plan_fft`
   except for :func:`rfft` instead of :func:`fft`.  The first two
   arguments, and the size of the transformed result, are the same as
   for :func:`rfft`.

.. function:: plan_brfft(A, d [, dims [, flags [, timelimit]]])

   Pre-plan an optimized real-input unnormalized transform, similar to
   :func:`plan_rfft` except for :func:`brfft` instead of :func:`rfft`.
   The first two arguments and the size of the transformed result, are
   the same as for :func:`brfft`.

.. function:: plan_irfft(A, d [, dims [, flags [, timelimit]]])

   Pre-plan an optimized inverse real-input FFT, similar to :func:`plan_rfft`
   except for :func:`irfft` and :func:`brfft`, respectively.  The first
   three arguments have the same meaning as for :func:`irfft`.

.. function:: dct(A [, dims])

   Performs a multidimensional type-II discrete cosine transform (DCT)
   of the array ``A``, using the unitary normalization of the DCT.
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_dct` for even greater efficiency.

.. function:: dct!(A [, dims])

   Same as :func:`dct!`, except that it operates in-place
   on ``A``, which must be an array of real or complex floating-point
   values.

.. function:: idct(A [, dims])

   Computes the multidimensional inverse discrete cosine transform (DCT)
   of the array ``A`` (technically, a type-III DCT with the unitary
   normalization).
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_idct` for even greater efficiency.

.. function:: idct!(A [, dims])

   Same as :func:`idct!`, but operates in-place on ``A``.

.. function:: plan_dct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`dct`.
   The first two arguments have the same meaning as for :func:`dct`.

.. function:: plan_dct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_dct`, but operates in-place on ``A``.

.. function:: plan_idct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized inverse discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`idct`.
   The first two arguments have the same meaning as for :func:`idct`.

.. function:: plan_idct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_idct`, but operates in-place on ``A``.

.. function:: fftshift(x)

   Swap the first and second halves of each dimension of ``x``.

.. function:: fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``.

.. function:: ifftshift(x, [dim])

   Undoes the effect of ``fftshift``.

.. function:: filt(b, a, x, [si])

   Apply filter described by vectors ``a`` and ``b`` to vector ``x``, with an
   optional initial filter state vector ``si`` (defaults to zeros).

.. function:: filt!(out, b, a, x, [si])

   Same as :func:`filt` but writes the result into the ``out`` argument,
   which may alias the input ``x`` to modify it in-place.

.. function:: deconv(b,a)

   Construct vector ``c`` such that ``b = conv(a,c) + r``. Equivalent to polynomial division.

.. function:: conv(u,v)

   Convolution of two vectors. Uses FFT algorithm.

.. function:: conv2(u,v,A)

   2-D convolution of the matrix ``A`` with the 2-D separable kernel generated by
   the vectors ``u`` and ``v``.  Uses 2-D FFT algorithm

.. function:: conv2(B,A)

   2-D convolution of the matrix ``B`` with the matrix ``A``.  Uses 2-D FFT algorithm

.. function:: xcorr(u,v)

   Compute the cross-correlation of two vectors.

The following functions are defined within the ``Base.FFTW`` module.

.. currentmodule:: Base.FFTW

.. function:: r2r(A, kind [, dims])

   Performs a multidimensional real-input/real-output (r2r) transform
   of type ``kind`` of the array ``A``, as defined in the FFTW manual.
   ``kind`` specifies either a discrete cosine transform of various types
   (``FFTW.REDFT00``, ``FFTW.REDFT01``, ``FFTW.REDFT10``, or
   ``FFTW.REDFT11``), a discrete sine transform of various types
   (``FFTW.RODFT00``, ``FFTW.RODFT01``, ``FFTW.RODFT10``, or
   ``FFTW.RODFT11``), a real-input DFT with halfcomplex-format output
   (``FFTW.R2HC`` and its inverse ``FFTW.HC2R``), or a discrete
   Hartley transform (``FFTW.DHT``).  The ``kind`` argument may be
   an array or tuple in order to specify different transform types
   along the different dimensions of ``A``; ``kind[end]`` is used
   for any unspecified dimensions.  See the FFTW manual for precise
   definitions of these transform types, at http://www.fftw.org/doc.

   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along. ``kind[i]`` is then the transform type for ``dims[i]``,
   with ``kind[end]`` being used for ``i > length(kind)``.

   See also :func:`plan_r2r` to pre-plan optimized r2r transforms.

.. function:: r2r!(A, kind [, dims])

   Same as :func:`r2r`, but operates in-place on ``A``, which must be
   an array of real or complex floating-point numbers.

.. function:: plan_r2r(A, kind [, dims [, flags [, timelimit]]])

   Pre-plan an optimized r2r transform, similar to :func:`Base.plan_fft`
   except that the transforms (and the first three arguments)
   correspond to :func:`r2r` and :func:`r2r!`, respectively.

.. function:: plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

   Similar to :func:`Base.plan_fft`, but corresponds to :func:`r2r!`.

.. currentmodule:: Base

Numerical Integration
---------------------

Although several external packages are available for numeric integration
and solution of ordinary differential equations, we also provide
some built-in integration support in Julia.

.. function:: quadgk(f, a,b,c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   Numerically integrate the function ``f(x)`` from ``a`` to ``b``,
   and optionally over additional intervals ``b`` to ``c`` and so on.
   Keyword options include a relative error tolerance ``reltol`` (defaults
   to ``sqrt(eps)`` in the precision of the endpoints), an absolute error
   tolerance ``abstol`` (defaults to 0), a maximum number of function
   evaluations ``maxevals`` (defaults to ``10^7``), and the ``order``
   of the integration rule (defaults to 7).

   Returns a pair ``(I,E)`` of the estimated integral ``I`` and an
   estimated upper bound on the absolute error ``E``.  If ``maxevals``
   is not exceeded then ``E <= max(abstol, reltol*norm(I))`` will hold.
   (Note that it is useful to specify a positive ``abstol`` in cases where
   ``norm(I)`` may be zero.)

   The endpoints ``a`` etcetera can also be complex (in which case the
   integral is performed over straight-line segments in the complex
   plane).  If the endpoints are ``BigFloat``, then the integration
   will be performed in ``BigFloat`` precision as well (note: it is
   advisable to increase the integration ``order`` in rough proportion
   to the precision, for smooth integrands).  More generally, the
   precision is set by the precision of the integration endpoints
   (promoted to floating-point types).

   The integrand ``f(x)`` can return any numeric scalar, vector, or matrix
   type, or in fact any type supporting ``+``, ``-``, multiplication
   by real values, and a ``norm`` (i.e., any normed vector space).
   Alternatively, a different norm can be specified by passing a `norm`-like
   function as the `norm` keyword argument (which defaults to `vecnorm`).

   [Only one-dimensional integrals are provided by this function.  For
   multi-dimensional integration (cubature), there are many different
   algorithms (often much better than simple nested 1d integrals)
   and the optimal choice tends to be very problem-dependent.  See
   the Julia external-package listing for available algorithms for
   multidimensional integration or other specialized tasks (such as
   integrals of highly oscillatory or singular functions).]

   The algorithm is an adaptive Gauss-Kronrod integration technique:
   the integral in each interval is estimated using a Kronrod rule
   (``2*order+1`` points) and the error is estimated using an embedded
   Gauss rule (``order`` points).   The interval with the largest
   error is then subdivided into two intervals and the process is repeated
   until the desired error tolerance is achieved.

   These quadrature rules work best for smooth functions within each
   interval, so if your function has a known discontinuity or other
   singularity, it is best to subdivide your interval to put the
   singularity at an endpoint.  For example, if ``f`` has a discontinuity
   at ``x=0.7`` and you want to integrate from 0 to 1, you should use
   ``quadgk(f, 0,0.7,1)`` to subdivide the interval at the point of
   discontinuity.  The integrand is never evaluated exactly at the endpoints
   of the intervals, so it is possible to integrate functions that diverge
   at the endpoints as long as the singularity is integrable (for example,
   a ``log(x)`` or ``1/sqrt(x)`` singularity).

   For real-valued endpoints, the starting and/or ending points may be
   infinite.  (A coordinate transformation is performed internally to
   map the infinite interval to a finite one.)

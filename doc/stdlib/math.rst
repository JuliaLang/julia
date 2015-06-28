.. currentmodule:: Base

*************
 Mathematics
*************

.. _mathematical-operators:

Mathematical Operators
----------------------

.. function:: -(x)

   Unary minus operator.

   ::

       -(x, y)

   Subtraction operator.


.. _+:
.. function:: +(x, y...)

   Addition operator. "x+y+z+..." calls this function with all arguments, i.e. "+(x, y, z, ...)".


.. _-:
.. function:: -(x)

   Unary minus operator.

   ::

       -(x, y)

   Subtraction operator.


.. _*:
.. function:: *(A, B)

   Matrix multiplication

   ::

       *(x, y...)

   Multiplication operator. "x*y*z*..." calls this function with all arguments, i.e. "*(x, y, z, ...)".

   ::

       *(s, t)

   Concatenate strings. The "*" operator is an alias to this function.

   ::

       julia> "Hello " * "world"
       "Hello world"


.. _/:
.. function:: /(x, y)

   Right division operator: multiplication of "x" by the inverse of "y" on the right. Gives floating-point results for integer arguments.


.. _\\:
.. function:: \\(x, y)

   Left division operator: multiplication of ``y`` by the inverse of ``x`` on the left.
   Gives floating-point results for integer arguments.

.. _^:
.. function:: ^(x, y)

   Exponentiation operator.

   ::

       ^(s, n)

   Repeat "n" times the string "s". The "^" operator is an alias to this function.

   ::

       julia> "Test "^3
       "Test Test Test "


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

   Computes "x*y+z" without rounding the intermediate result "x*y". On some systems this is significantly more expensive than "x*y+z". "fma" is used to improve accuracy in certain algorithms. See "muladd".


.. function:: muladd(x, y, z)

   Combined multiply-add, computes "x*y+z" in an efficient manner. This may on some systems be equivalent to "x*y+z", or to "fma(x,y,z)". "muladd" is used to improve performance. See "fma".


.. function:: div(x, y)

   ÷(x, y)

   The quotient from Euclidean division. Computes "x/y", truncated to an integer.


.. function:: fld(x, y)

   Largest integer less than or equal to "x/y".


.. function:: cld(x, y)

   Smallest integer larger than or equal to "x/y".


.. function:: mod(x, y)

   Modulus after division, returning in the range [0,``y``), if "y" is positive, or ("y",0] if "y" is negative.


.. function:: mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the modulus after division by numerically exact 2pi, and is therefore not exactly the same as mod(x,2pi), which would compute the modulus of x relative to division by the floating-point number 2pi.


.. function:: rem(x, y)

   %(x, y)

   Remainder from Euclidean division, returning a value of the same sign as``x``, and smaller in magnitude than "y". This value is always exact.


.. function:: divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to "(x÷y, x%y)".


.. function:: fldmod(x, y)

   The floored quotient and modulus after division. Equivalent to "(fld(x,y), mod(x,y))".


.. function:: mod1(x, m)

   Modulus after division, returning in the range (0,m]


.. function:: rem1(x, m)

   Remainder after division, returning in the range (0,m]


.. _//:
.. function:: //(num, den)

   Divide two integers or rational numbers, giving a "Rational" result.


.. function:: rationalize([Type=Int], x; tol=eps(x))

   Approximate floating point number "x" as a Rational number with components of the given integer type. The result will differ from "x" by no more than "tol".


.. function:: num(x)

   Numerator of the rational representation of "x"


.. function:: den(x)

   Denominator of the rational representation of "x"


.. _<<:
.. function:: <<(x, n)

   Left bit shift operator.


.. _>>:
.. function:: >>(x, n)

   Right bit shift operator, preserving the sign of "x".


.. _>>>:
.. function:: >>>(x, n)

   Unsigned right bit shift operator.


.. _\::
.. function:: \:(start, [step], stop)

   Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1,
   and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the
   function ``colon``.
   The colon is also used in indexing to select whole dimensions.

.. function:: colon(start[, step], stop)

   Called by ":" syntax for constructing ranges.


.. function:: range(start[, step], length)

   Construct a range by length, given a starting value and optional step (defaults to 1).


.. _==:
.. function:: ==(x, y)

   Generic equality operator, giving a single "Bool" result. Falls back to "===". Should be implemented for all types with a notion of equality, based on the abstract value that an instance represents. For example, all numeric types are compared by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement "==" by calling "==" recursively on all contents.

   New numeric types should implement this function for two arguments of the new type, and handle comparison to other types via promotion rules where possible.


.. _!=:
.. function:: !=(x, y)

   ≠(x, y)

   Not-equals comparison operator. Always gives the opposite answer as "==". New types should generally not implement this, and rely on the fallback definition "!=(x,y) = !(x==y)" instead.


.. _===:
.. function:: is(x, y) -> Bool

   ===(x, y) -> Bool ≡(x, y) -> Bool

   Determine whether "x" and "y" are identical, in the sense that no program could distinguish them. Compares mutable objects by address in memory, and compares immutable objects (such as numbers) by contents at the bit level. This function is sometimes called "egal".


.. _!==:
.. function:: !==(x, y)

   ≢(x, y)

   Equivalent to "!is(x, y)"


.. _<:
.. function:: <(x, y)

   Less-than comparison operator. New numeric types should implement this function for two arguments of the new type. Because of the behavior of floating-point NaN values, "<" implements a partial order. Types with a canonical partial order should implement "<", and types with a canonical total order should implement "isless".


.. _<=:
.. function:: <=(x, y)

   ≤(x, y)

   Less-than-or-equals comparison operator.


.. _>:
.. function:: >(x, y)

   Greater-than comparison operator. Generally, new types should implement "<" instead of this function, and rely on the fallback definition ">(x,y) = y<x".


.. _>=:
.. function:: >=(x, y)

   ≥(x, y)

   Greater-than-or-equals comparison operator.


.. _.==:
.. function:: .==(x, y)

   Element-wise equality comparison operator.


.. _.!=:
.. function:: .!=(x, y)

   .≠(x, y)

   Element-wise not-equals comparison operator.


.. _.<:
.. function:: .<(x, y)

   Element-wise less-than comparison operator.


.. _.<=:
.. function:: .<=(x, y)

   .≤(x, y)

   Element-wise less-than-or-equals comparison operator.


.. _.>:
.. function:: .>(x, y)

   Element-wise greater-than comparison operator.


.. _.>=:
.. function:: .>=(x, y)

   .≥(x, y)

   Element-wise greater-than-or-equals comparison operator.


.. function:: cmp(x, y)

   Return -1, 0, or 1 depending on whether "x" is less than, equal to, or greater than "y", respectively. Uses the total order implemented by "isless". For floating-point numbers, uses "<" but throws an error for unordered arguments.


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
.. function:: \$(x, y)

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

.. function:: A_ldiv_Bc(a, b)

   Matrix operator A \ B^H


.. function:: A_ldiv_Bt(a, b)

   Matrix operator A \ B^T


.. function:: A_mul_B!(Y, A, B) -> Y

   Calculates the matrix-matrix or matrix-vector product *A B* and stores the result in *Y*, overwriting the existing value of *Y*.

   ::

       julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; A_mul_B!(B, A, B);

       julia> B
       2x2 Array{Float64,2}:
        3.0  3.0
        7.0  7.0


.. function:: A_mul_Bc(...)

   Matrix operator A B^H


.. function:: A_mul_Bt(...)

   Matrix operator A B^T


.. function:: A_rdiv_Bc(...)

   Matrix operator A / B^H


.. function:: A_rdiv_Bt(a, b)

   Matrix operator A / B^T


.. function:: Ac_ldiv_B(...)

   Matrix operator A^H \ B


.. function:: Ac_ldiv_Bc(...)

   Matrix operator A^H \ B^H


.. function:: Ac_mul_B(...)

   Matrix operator A^H B


.. function:: Ac_mul_Bc(...)

   Matrix operator A^H B^H


.. function:: Ac_rdiv_B(a, b)

   Matrix operator A^H / B


.. function:: Ac_rdiv_Bc(a, b)

   Matrix operator A^H / B^H


.. function:: At_ldiv_B(...)

   Matrix operator A^T \ B


.. function:: At_ldiv_Bt(...)

   Matrix operator A^T \ B^T


.. function:: At_mul_B(...)

   Matrix operator A^T B


.. function:: At_mul_Bt(...)

   Matrix operator A^T B^T


.. function:: At_rdiv_B(a, b)

   Matrix operator A^T / B


.. function:: At_rdiv_Bt(a, b)

   Matrix operator A^T / B^T


Mathematical Functions
----------------------

.. function:: isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

   Inexact equality comparison - behaves slightly different depending on types of input args:

     * For "FloatingPoint" numbers, "isapprox" returns "true" if   "abs(x-y) <= atol + rtol*max(abs(x), abs(y))".

     * For "Integer" and "Rational" numbers, "isapprox" returns   "true" if "abs(x-y) <= atol". The *rtol* argument is ignored.   If one of "x" and "y" is "FloatingPoint", the other is   promoted, and the method above is called instead.

     * For "Complex" numbers, the distance in the complex plane is   compared, using the same criterion as above.

   For default tolerance arguments, "maxeps = max(eps(abs(x)), eps(abs(y)))".


.. function:: sin(x)

   Compute sine of "x", where "x" is in radians


.. function:: cos(x)

   Compute cosine of "x", where "x" is in radians


.. function:: tan(x)

   Compute tangent of "x", where "x" is in radians


.. function:: sind(x)

   Compute sine of "x", where "x" is in degrees


.. function:: cosd(x)

   Compute cosine of "x", where "x" is in degrees


.. function:: tand(x)

   Compute tangent of "x", where "x" is in degrees


.. function:: sinpi(x)

   Compute \sin(\pi x) more accurately than "sin(pi*x)", especially for large "x".


.. function:: cospi(x)

   Compute \cos(\pi x) more accurately than "cos(pi*x)", especially for large "x".


.. function:: sinh(x)

   Compute hyperbolic sine of "x"


.. function:: cosh(x)

   Compute hyperbolic cosine of "x"


.. function:: tanh(x)

   Compute hyperbolic tangent of "x"


.. function:: asin(x)

   Compute the inverse sine of "x", where the output is in radians


.. function:: acos(x)

   Compute the inverse cosine of "x", where the output is in radians


.. function:: atan(x)

   Compute the inverse tangent of "x", where the output is in radians


.. function:: atan2(y, x)

   Compute the inverse tangent of "y/x", using the signs of both "x" and "y" to determine the quadrant of the return value.


.. function:: asind(x)

   Compute the inverse sine of "x", where the output is in degrees


.. function:: acosd(x)

   Compute the inverse cosine of "x", where the output is in degrees


.. function:: atand(x)

   Compute the inverse tangent of "x", where the output is in degrees


.. function:: sec(x)

   Compute the secant of "x", where "x" is in radians


.. function:: csc(x)

   Compute the cosecant of "x", where "x" is in radians


.. function:: cot(x)

   Compute the cotangent of "x", where "x" is in radians


.. function:: secd(x)

   Compute the secant of "x", where "x" is in degrees


.. function:: cscd(x)

   Compute the cosecant of "x", where "x" is in degrees


.. function:: cotd(x)

   Compute the cotangent of "x", where "x" is in degrees


.. function:: asec(x)

   Compute the inverse secant of "x", where the output is in radians


.. function:: acsc(x)

   Compute the inverse cosecant of "x", where the output is in radians


.. function:: acot(x)

   Compute the inverse cotangent of "x", where the output is in radians


.. function:: asecd(x)

   Compute the inverse secant of "x", where the output is in degrees


.. function:: acscd(x)

   Compute the inverse cosecant of "x", where the output is in degrees


.. function:: acotd(x)

   Compute the inverse cotangent of "x", where the output is in degrees


.. function:: sech(x)

   Compute the hyperbolic secant of "x"


.. function:: csch(x)

   Compute the hyperbolic cosecant of "x"


.. function:: coth(x)

   Compute the hyperbolic cotangent of "x"


.. function:: asinh(x)

   Compute the inverse hyperbolic sine of "x"


.. function:: acosh(x)

   Compute the inverse hyperbolic cosine of "x"


.. function:: atanh(x)

   Compute the inverse hyperbolic tangent of "x"


.. function:: asech(x)

   Compute the inverse hyperbolic secant of "x"


.. function:: acsch(x)

   Compute the inverse hyperbolic cosecant of "x"


.. function:: acoth(x)

   Compute the inverse hyperbolic cotangent of "x"


.. function:: sinc(x)

   Compute \sin(\pi x) / (\pi x) if x \neq 0, and 1 if x = 0.


.. function:: cosc(x)

   Compute \cos(\pi x) / x - \sin(\pi x) / (\pi x^2) if x \neq 0, and 0 if x = 0. This is the derivative of "sinc(x)".


.. function:: deg2rad(x)

   Convert "x" from degrees to radians


.. function:: rad2deg(x)

   Convert "x" from radians to degrees


.. function:: hypot(x, y)

   Compute the \sqrt{x^2+y^2} avoiding overflow and underflow


.. function:: log(x)

   Compute the natural logarithm of "x". Throws "DomainError" for negative "Real" arguments. Use complex negative arguments to obtain complex results.

   There is an experimental variant in the "Base.Math.JuliaLibm" module, which is typically faster and more accurate.

   ::

       log(b, x)

   Compute the base "b" logarithm of "x". Throws "DomainError" for negative "Real" arguments.


.. function:: log(x)

   Compute the natural logarithm of "x". Throws "DomainError" for negative "Real" arguments. Use complex negative arguments to obtain complex results.

   There is an experimental variant in the "Base.Math.JuliaLibm" module, which is typically faster and more accurate.

   ::

       log(b, x)

   Compute the base "b" logarithm of "x". Throws "DomainError" for negative "Real" arguments.


.. function:: log2(x)

   Compute the logarithm of "x" to base 2. Throws "DomainError" for negative "Real" arguments.


.. function:: log10(x)

   Compute the logarithm of "x" to base 10. Throws "DomainError" for negative "Real" arguments.


.. function:: log1p(x)

   Accurate natural logarithm of "1+x".  Throws "DomainError" for "Real" arguments less than -1.

   There is an experimental variant in the "Base.Math.JuliaLibm" module, which is typically faster and more accurate.


.. function:: frexp(val)

   Return "(x,exp)" such that "x" has a magnitude in the interval "[1/2, 1)" or 0, and val = x \times 2^{exp}.


.. function:: exp(x)

   Compute e^x


.. function:: exp2(x)

   Compute 2^x


.. function:: exp10(x)

   Compute 10^x


.. function:: ldexp(x, n)

   Compute x \times 2^n


.. function:: modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts of a number. Both parts have the same sign as the argument.


.. function:: expm1(x)

   Accurately compute e^x-1


.. function:: round([T], x[, digits[, base]][, r::RoundingMode])

   "round(x)" rounds "x" to an integer value according to the default rounding mode (see "get_rounding()"), returning a value of the same type as "x". By default ("RoundNearest"), this will round to the nearest integer, with ties (fractional values of 0.5) being rounded to the even integer.

   ::

       julia> round(1.7)
       2.0

       julia> round(1.5)
       2.0

       julia> round(2.5)
       2.0

   The optional "RoundingMode" argument will change how the number gets rounded.

   "round(T, x, [r::RoundingMode])" converts the result to type "T", throwing an "InexactError" if the value is not representable.

   "round(x, digits)" rounds to the specified number of digits after the decimal place (or before if negative). "round(x, digits, base)" rounds using a base other than 10.

   ::

          julia> round(pi, 2)
          3.14

          julia> round(pi, 3, 2)
          3.125

   Note: Rounding to specified digits in bases other than 2 can be   inexact when operating on binary floating point numbers. For   example, the "Float64" value represented by "1.15" is   actually *less* than 1.15, yet will be rounded to 1.2.

   ::

         julia> x = 1.15
         1.15

         julia> @sprintf "%.20f" x
         "1.14999999999999991118"

         julia> x < 115//100
         true

         julia> round(x, 1)
         1.2

       round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex- valued "z" to "z", breaking ties using the specified "RoundingMode"s. The first "RoundingMode" is used for rounding the real components while the second is used for rounding the imaginary components.


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

.. function:: round([T], x[, digits[, base]][, r::RoundingMode])

   "round(x)" rounds "x" to an integer value according to the default rounding mode (see "get_rounding()"), returning a value of the same type as "x". By default ("RoundNearest"), this will round to the nearest integer, with ties (fractional values of 0.5) being rounded to the even integer.

   ::

       julia> round(1.7)
       2.0

       julia> round(1.5)
       2.0

       julia> round(2.5)
       2.0

   The optional "RoundingMode" argument will change how the number gets rounded.

   "round(T, x, [r::RoundingMode])" converts the result to type "T", throwing an "InexactError" if the value is not representable.

   "round(x, digits)" rounds to the specified number of digits after the decimal place (or before if negative). "round(x, digits, base)" rounds using a base other than 10.

   ::

          julia> round(pi, 2)
          3.14

          julia> round(pi, 3, 2)
          3.125

   Note: Rounding to specified digits in bases other than 2 can be   inexact when operating on binary floating point numbers. For   example, the "Float64" value represented by "1.15" is   actually *less* than 1.15, yet will be rounded to 1.2.

   ::

         julia> x = 1.15
         1.15

         julia> @sprintf "%.20f" x
         "1.14999999999999991118"

         julia> x < 115//100
         true

         julia> round(x, 1)
         1.2

       round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex- valued "z" to "z", breaking ties using the specified "RoundingMode"s. The first "RoundingMode" is used for rounding the real components while the second is used for rounding the imaginary components.


.. function:: ceil([T], x[, digits[, base]])

   "ceil(x)" returns the nearest integral value of the same type as "x" that is greater than or equal to "x".

   "ceil(T, x)" converts the result to type "T", throwing an "InexactError" if the value is not representable.

   "digits" and "base" work as for "round()".


.. function:: floor([T], x[, digits[, base]])

   "floor(x)" returns the nearest integral value of the same type as "x" that is less than or equal to "x".

   "floor(T, x)" converts the result to type "T", throwing an "InexactError" if the value is not representable.

   "digits" and "base" work as for "round()".


.. function:: trunc([T], x[, digits[, base]])

   "trunc(x)" returns the nearest integral value of the same type as "x" whose absolute value is less than or equal to "x".

   "trunc(T, x)" converts the result to type "T", throwing an "InexactError" if the value is not representable.

   "digits" and "base" work as for "round()".


.. function:: unsafe_trunc(T, x)

   "unsafe_trunc(T, x)" returns the nearest integral value of type "T" whose absolute value is less than or equal to "x". If the value is not representable by "T", an arbitrary value will be returned.


.. function:: signif(x, digits[, base])

   Rounds (in the sense of "round") "x" so that there are "digits" significant digits, under a base "base" representation, default 10. E.g., "signif(123.456, 2)" is "120.0", and "signif(357.913, 4, 2)" is "352.0".


.. function:: min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over arrays.


.. function:: max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over arrays.


.. function:: minmax(x, y)

   Return "(min(x,y), max(x,y))". See also: "extrema()" that returns "(minimum(x), maximum(x))"


.. function:: clamp(x, lo, hi)

   Return x if "lo <= x <= hi". If "x < lo", return "lo". If "x > hi", return "hi". Arguments are promoted to a common type. Operates elementwise over "x" if it is an array.



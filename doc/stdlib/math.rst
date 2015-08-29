.. currentmodule:: Base

*************
 Mathematics
*************

.. _mathematical-operators:

Mathematical Operators
----------------------

.. function:: -(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       -(x)

   Unary minus operator.

   .. code-block:: julia

       -(x, y)

   Subtraction operator.

.. _+:
.. function:: +(x, y...)

   .. Docstring generated from Julia source

   .. code-block:: julia

       +(x, y...)

   Addition operator. ``x+y+z+...`` calls this function with all arguments, i.e. ``+(x, y, z, ...)``\ .

.. _-:
.. function:: -(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       -(x)

   Unary minus operator.

   .. code-block:: julia

       -(x, y)

   Subtraction operator.

.. _*:
.. function:: *(x, y...)

   .. Docstring generated from Julia source

   ::

              *(s, t)

   Concatenate strings. The ``*`` operator is an alias to this function.


.. _/:
.. function:: /(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       /(x, y)

   Right division operator: multiplication of ``x`` by the inverse of ``y`` on the right. Gives floating-point results for integer arguments.

.. _\\:
.. function:: \\(x, y)

   Left division operator: multiplication of ``y`` by the inverse of ``x`` on the left.
   Gives floating-point results for integer arguments.

.. _^:
.. function:: ^(x, y)

   .. Docstring generated from Julia source

   ::

              ^(s, n)

   Repeat ``n`` times the string ``s``. The ``^`` operator is an alias to this function.

   .. doctest::

   	julia> "Test "^3
   	"Test Test Test "

.. _.+:
.. function:: .+(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .+(x, y)

   Element-wise addition operator.

.. _.-:
.. function:: .-(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .-(x, y)

   Element-wise subtraction operator.

.. _.*:
.. function:: .*(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .*(x, y)

   Element-wise multiplication operator.

.. _./:
.. function:: ./(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ./(x, y)

   Element-wise right division operator.

.. _.\\:
.. function:: .\\(x, y)

   Element-wise left division operator.

.. _.^:
.. function:: .^(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .^(x, y)

   Element-wise exponentiation operator.

.. function:: fma(x, y, z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       fma(x, y, z)

   Computes ``x*y+z`` without rounding the intermediate result ``x*y``\ . On some systems this is significantly more expensive than ``x*y+z``\ . ``fma`` is used to improve accuracy in certain algorithms. See ``muladd``\ .

.. function:: muladd(x, y, z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       muladd(x, y, z)

   Combined multiply-add, computes ``x*y+z`` in an efficient manner. This may on some systems be equivalent to ``x*y+z``\ , or to ``fma(x,y,z)``\ . ``muladd`` is used to improve performance. See ``fma``\ .

.. function:: div(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       div(x, y)
       ÷(x, y)

   The quotient from Euclidean division. Computes ``x/y``\ , truncated to an integer.

.. function:: fld(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       fld(x, y)

   Largest integer less than or equal to ``x/y``\ .

.. function:: cld(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cld(x, y)

   Smallest integer larger than or equal to ``x/y``\ .

.. function:: mod(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mod(x, y)

   Modulus after division, returning in the range [0,``y``\ ), if ``y`` is positive, or (``y``\ ,0] if ``y`` is negative.

.. function:: mod2pi(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the modulus after division by numerically exact 2pi, and is therefore not exactly the same as mod(x,2pi), which would compute the modulus of x relative to division by the floating-point number 2pi.

.. function:: rem(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rem(x, y)
       %(x, y)

   Remainder from Euclidean division, returning a value of the same sign as``x``\ , and smaller in magnitude than ``y``\ . This value is always exact.

.. function:: divrem(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to ``(x÷y, x%y)``\ .

.. function:: fldmod(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       fldmod(x, y)

   The floored quotient and modulus after division. Equivalent to ``(fld(x,y), mod(x,y))``\ .

.. function:: mod1(x,m)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mod1(x,m)

   Modulus after division, returning in the range (0,m]

.. function:: rem1(x,m)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rem1(x,m)

   Remainder after division, returning in the range (0,m]

.. _//:
.. function:: //(num, den)

   .. Docstring generated from Julia source

   .. code-block:: julia

       //(num, den)

   Divide two integers or rational numbers, giving a ``Rational`` result.

.. function:: rationalize([Type=Int,] x; tol=eps(x))

   .. Docstring generated from Julia source

   .. code-block:: julia

       rationalize([Type=Int,] x; tol=eps(x))

   Approximate floating point number ``x`` as a Rational number with components of the given integer type. The result will differ from ``x`` by no more than ``tol``\ .

.. function:: num(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       num(x)

   Numerator of the rational representation of ``x``

.. function:: den(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       den(x)

   Denominator of the rational representation of ``x``

.. _<<:
.. function:: <<(x, n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       <<(x, n)

   Left bit shift operator.

.. _>>:
.. function:: >>(x, n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       >>(x, n)

   Right bit shift operator, preserving the sign of ``x``\ .

.. _>>>:
.. function:: >>>(x, n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       >>>(x, n)

   Unsigned right bit shift operator.

.. _\::
.. function:: \:(start, [step], stop)

   Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1,
   and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the
   function ``colon``.
   The colon is also used in indexing to select whole dimensions.

.. function:: colon(start, [step], stop)

   .. Docstring generated from Julia source

   .. code-block:: julia

       colon(start, [step], stop)

   Called by ``:`` syntax for constructing ranges.

.. function:: range(start, [step], length)

   .. Docstring generated from Julia source

   .. code-block:: julia

       range(start, [step], length)

   Construct a range by length, given a starting value and optional step (defaults to 1).

.. _==:
.. function:: ==(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ==(x, y)

   Generic equality operator, giving a single ``Bool`` result. Falls back to ``===``\ . Should be implemented for all types with a notion of equality, based on the abstract value that an instance represents. For example, all numeric types are compared by numeric value, ignoring type. Strings are compared as sequences of characters, ignoring encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement ``==`` by calling ``==`` recursively on all contents.

   New numeric types should implement this function for two arguments of the new type, and handle comparison to other types via promotion rules where possible.

.. _!=:
.. function:: !=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       !=(x, y)
       ≠(x,y)

   Not-equals comparison operator. Always gives the opposite answer as ``==``\ . New types should generally not implement this, and rely on the fallback definition ``!=(x,y) = !(x==y)`` instead.

.. _===:
.. function:: ===(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       is(x, y) -> Bool
       ===(x,y) -> Bool
       ≡(x,y) -> Bool

   Determine whether ``x`` and ``y`` are identical, in the sense that no program could distinguish them. Compares mutable objects by address in memory, and compares immutable objects (such as numbers) by contents at the bit level. This function is sometimes called ``egal``\ .

.. _!==:
.. function:: !==(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       !==(x, y)
       ≢(x,y)

   Equivalent to ``!is(x, y)``

.. _<:
.. function:: <(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       <(x, y)

   Less-than comparison operator. New numeric types should implement this function for two arguments of the new type. Because of the behavior of floating-point NaN values, ``<`` implements a partial order. Types with a canonical partial order should implement ``<``\ , and types with a canonical total order should implement ``isless``\ .

.. _<=:
.. function:: <=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       <=(x, y)
       ≤(x,y)

   Less-than-or-equals comparison operator.

.. _>:
.. function:: >(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       >(x, y)

   Greater-than comparison operator. Generally, new types should implement ``<`` instead of this function, and rely on the fallback definition ``>(x,y) = y<x``\ .

.. _>=:
.. function:: >=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       >=(x, y)
       ≥(x,y)

   Greater-than-or-equals comparison operator.

.. _.==:
.. function:: .==(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .==(x, y)

   Element-wise equality comparison operator.

.. _.!=:
.. function:: .!=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .!=(x, y)
       .≠(x,y)

   Element-wise not-equals comparison operator.

.. _.<:
.. function:: .<(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .<(x, y)

   Element-wise less-than comparison operator.

.. _.<=:
.. function:: .<=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .<=(x, y)
       .≤(x,y)

   Element-wise less-than-or-equals comparison operator.

.. _.>:
.. function:: .>(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .>(x, y)

   Element-wise greater-than comparison operator.

.. _.>=:
.. function:: .>=(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       .>=(x, y)
       .≥(x,y)

   Element-wise greater-than-or-equals comparison operator.

.. function:: cmp(x,y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cmp(x,y)

   Return -1, 0, or 1 depending on whether ``x`` is less than, equal to, or greater than ``y``\ , respectively. Uses the total order implemented by ``isless``\ . For floating-point numbers, uses ``<`` but throws an error for unordered arguments.

.. _~:
.. function:: ~(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ~(x)

   Bitwise not

.. _&:
.. function:: &(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       &(x, y)

   Bitwise and

.. _|:
.. function:: |(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       |(x, y)

   Bitwise or

.. _$:
.. function:: $(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       \$(x, y)

   Bitwise exclusive or

.. _!:
.. function:: !(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       !(x)

   Boolean not

.. _&&:
.. function:: x && y

   Short-circuiting boolean and

.. _||:
.. function:: x || y

   Short-circuiting boolean or

.. function:: A_ldiv_Bc(a,b)

   .. Docstring generated from Julia source

   ::

              A_ldiv_Bc(a,b)

   Matrix operator A \\ B\ :sup:`H`

.. function:: A_ldiv_Bt(a,b)

   .. Docstring generated from Julia source

   ::

              A_ldiv_Bt(a,b)

   Matrix operator A \\ B\ :sup:`T`

.. function:: A_mul_B!(Y, A, B) -> Y

   .. Docstring generated from Julia source

   ::

              A_mul_B!(Y, A, B) -> Y


   Calculates the matrix-matrix or matrix-vector product *A B* and stores the
   result in *Y*, overwriting the existing value of *Y*.

   .. doctest::

      julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; A_mul_B!(B, A, B);

      julia> B
      2x2 Array{Float64,2}:
       3.0  3.0
       7.0  7.0

.. function:: A_mul_Bc(...)

   .. Docstring generated from Julia source

   ::

              A_mul_Bc(...)

   Matrix operator A B\ :sup:`H`

.. function:: A_mul_Bt(...)

   .. Docstring generated from Julia source

   ::

              A_mul_Bt(...)

   Matrix operator A B\ :sup:`T`

.. function:: A_rdiv_Bc(...)

   .. Docstring generated from Julia source

   ::

              A_rdiv_Bc(...)

   Matrix operator A / B\ :sup:`H`

.. function:: A_rdiv_Bt(a,b)

   .. Docstring generated from Julia source

   ::

              A_rdiv_Bt(a,b)

   Matrix operator A / B\ :sup:`T`

.. function:: Ac_ldiv_B(...)

   .. Docstring generated from Julia source

   ::

              Ac_ldiv_B(...)

   Matrix operator A\ :sup:`H` \\ B

.. function:: Ac_ldiv_Bc(...)

   .. Docstring generated from Julia source

   ::

              Ac_ldiv_Bc(...)

   Matrix operator A\ :sup:`H` \\ B\ :sup:`H`

.. function:: Ac_mul_B(...)

   .. Docstring generated from Julia source

   ::

              Ac_mul_B(...)

   Matrix operator A\ :sup:`H` B

.. function:: Ac_mul_Bc(...)

   .. Docstring generated from Julia source

   ::

              Ac_mul_Bc(...)

   Matrix operator A\ :sup:`H` B\ :sup:`H`

.. function:: Ac_rdiv_B(a,b)

   .. Docstring generated from Julia source

   ::

              Ac_rdiv_B(a,b)

   Matrix operator A\ :sup:`H` / B

.. function:: Ac_rdiv_Bc(a,b)

   .. Docstring generated from Julia source

   ::

              Ac_rdiv_Bc(a,b)

   Matrix operator A\ :sup:`H` / B\ :sup:`H`

.. function:: At_ldiv_B(...)

   .. Docstring generated from Julia source

   ::

              At_ldiv_B(...)

   Matrix operator A\ :sup:`T` \\ B

.. function:: At_ldiv_Bt(...)

   .. Docstring generated from Julia source

   ::

              At_ldiv_Bt(...)

   Matrix operator A\ :sup:`T` \\ B\ :sup:`T`

.. function:: At_mul_B(...)

   .. Docstring generated from Julia source

   ::

              At_mul_B(...)

   Matrix operator A\ :sup:`T` B

.. function:: At_mul_Bt(...)

   .. Docstring generated from Julia source

   ::

              At_mul_Bt(...)

   Matrix operator A\ :sup:`T` B\ :sup:`T`

.. function:: At_rdiv_B(a,b)

   .. Docstring generated from Julia source

   ::

              At_rdiv_B(a,b)

   Matrix operator A\ :sup:`T` / B

.. function:: At_rdiv_Bt(a,b)

   .. Docstring generated from Julia source

   ::

              At_rdiv_Bt(a,b)

   Matrix operator A\ :sup:`T` / B\ :sup:`T`

Mathematical Functions
----------------------

.. function:: isapprox(x, y; rtol::Real=sqrt(eps), atol::Real=0)

   .. Docstring generated from Julia source

   .. code-block:: julia

       isapprox(x, y; rtol::Real=sqrt(eps), atol::Real=0)

   Inexact equality comparison: ``true`` if ``norm(x-y) <= atol + rtol*max(norm(x), norm(y))``\ . The default ``atol`` is zero and the default ``rtol`` depends on the types of ``x`` and ``y``\ .

   For real or complex floating-point values, ``rtol`` defaults to ``sqrt(eps(typeof(real(x-y))))``\ . This corresponds to requiring equality of about half of the significand digits. For other types, ``rtol`` defaults to zero.

   ``x`` and ``y`` may also be arrays of numbers, in which case ``norm`` defaults to ``vecnorm`` but may be changed by passing a ``norm::Function`` keyword argument. (For numbers, ``norm`` is the same thing as ``abs``\ .)

   The binary operator ``≈`` is equivalent to ``isapprox`` with the default arguments, and ``x ≉ y`` is equivalent to ``!isapprox(x,y)``\ .

.. function:: sin(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sin(x)

   Compute sine of ``x``\ , where ``x`` is in radians

.. function:: cos(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cos(x)

   Compute cosine of ``x``\ , where ``x`` is in radians

.. function:: tan(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       tan(x)

   Compute tangent of ``x``\ , where ``x`` is in radians

.. function:: sind(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sind(x)

   Compute sine of ``x``\ , where ``x`` is in degrees

.. function:: cosd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cosd(x)

   Compute cosine of ``x``\ , where ``x`` is in degrees

.. function:: tand(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       tand(x)

   Compute tangent of ``x``\ , where ``x`` is in degrees

.. function:: sinpi(x)

   .. Docstring generated from Julia source

   ::

              sinpi(x)

   Compute :math:`\sin(\pi x)` more accurately than ``sin(pi*x)``, especially for large ``x``.

.. function:: cospi(x)

   .. Docstring generated from Julia source

   ::

              cospi(x)

   Compute :math:`\cos(\pi x)` more accurately than ``cos(pi*x)``, especially for large ``x``.

.. function:: sinh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sinh(x)

   Compute hyperbolic sine of ``x``

.. function:: cosh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cosh(x)

   Compute hyperbolic cosine of ``x``

.. function:: tanh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       tanh(x)

   Compute hyperbolic tangent of ``x``

.. function:: asin(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asin(x)

   Compute the inverse sine of ``x``\ , where the output is in radians

.. function:: acos(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acos(x)

   Compute the inverse cosine of ``x``\ , where the output is in radians

.. function:: atan(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       atan(x)

   Compute the inverse tangent of ``x``\ , where the output is in radians

.. function:: atan2(y, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       atan2(y, x)

   Compute the inverse tangent of ``y/x``\ , using the signs of both ``x`` and ``y`` to determine the quadrant of the return value.

.. function:: asind(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asind(x)

   Compute the inverse sine of ``x``\ , where the output is in degrees

.. function:: acosd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acosd(x)

   Compute the inverse cosine of ``x``\ , where the output is in degrees

.. function:: atand(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       atand(x)

   Compute the inverse tangent of ``x``\ , where the output is in degrees

.. function:: sec(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sec(x)

   Compute the secant of ``x``\ , where ``x`` is in radians

.. function:: csc(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       csc(x)

   Compute the cosecant of ``x``\ , where ``x`` is in radians

.. function:: cot(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cot(x)

   Compute the cotangent of ``x``\ , where ``x`` is in radians

.. function:: secd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       secd(x)

   Compute the secant of ``x``\ , where ``x`` is in degrees

.. function:: cscd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cscd(x)

   Compute the cosecant of ``x``\ , where ``x`` is in degrees

.. function:: cotd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       cotd(x)

   Compute the cotangent of ``x``\ , where ``x`` is in degrees

.. function:: asec(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asec(x)

   Compute the inverse secant of ``x``\ , where the output is in radians

.. function:: acsc(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acsc(x)

   Compute the inverse cosecant of ``x``\ , where the output is in radians

.. function:: acot(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acot(x)

   Compute the inverse cotangent of ``x``\ , where the output is in radians

.. function:: asecd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asecd(x)

   Compute the inverse secant of ``x``\ , where the output is in degrees

.. function:: acscd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acscd(x)

   Compute the inverse cosecant of ``x``\ , where the output is in degrees

.. function:: acotd(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acotd(x)

   Compute the inverse cotangent of ``x``\ , where the output is in degrees

.. function:: sech(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       sech(x)

   Compute the hyperbolic secant of ``x``

.. function:: csch(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       csch(x)

   Compute the hyperbolic cosecant of ``x``

.. function:: coth(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       coth(x)

   Compute the hyperbolic cotangent of ``x``

.. function:: asinh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asinh(x)

   Compute the inverse hyperbolic sine of ``x``

.. function:: acosh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acosh(x)

   Compute the inverse hyperbolic cosine of ``x``

.. function:: atanh(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       atanh(x)

   Compute the inverse hyperbolic tangent of ``x``

.. function:: asech(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       asech(x)

   Compute the inverse hyperbolic secant of ``x``

.. function:: acsch(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acsch(x)

   Compute the inverse hyperbolic cosecant of ``x``

.. function:: acoth(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       acoth(x)

   Compute the inverse hyperbolic cotangent of ``x``

.. function:: sinc(x)

   .. Docstring generated from Julia source

   ::

              sinc(x)

   Compute :math:`\sin(\pi x) / (\pi x)` if :math:`x \neq 0`, and :math:`1` if :math:`x = 0`.

.. function:: cosc(x)

   .. Docstring generated from Julia source

   ::

              cosc(x)

   Compute :math:`\cos(\pi x) / x - \sin(\pi x) / (\pi x^2)` if :math:`x \neq 0`, and :math:`0`
   if :math:`x = 0`. This is the derivative of ``sinc(x)``.

.. function:: deg2rad(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       deg2rad(x)

   Convert ``x`` from degrees to radians

.. function:: rad2deg(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       rad2deg(x)

   Convert ``x`` from radians to degrees

.. function:: hypot(x, y)

   .. Docstring generated from Julia source

   ::

              hypot(x, y)

   Compute the :math:`\sqrt{x^2+y^2}` avoiding overflow and underflow

.. function:: log(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       log(x)

   Compute the natural logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments to obtain complex results.

   There is an experimental variant in the ``Base.Math.JuliaLibm`` module, which is typically faster and more accurate.

   .. code-block:: julia

       log(b,x)

   Compute the base ``b`` logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log(b,x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       log(x)

   Compute the natural logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments to obtain complex results.

   There is an experimental variant in the ``Base.Math.JuliaLibm`` module, which is typically faster and more accurate.

   .. code-block:: julia

       log(b,x)

   Compute the base ``b`` logarithm of ``x``\ . Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log2(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       log2(x)

   Compute the logarithm of ``x`` to base 2. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log10(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       log10(x)

   Compute the logarithm of ``x`` to base 10. Throws ``DomainError`` for negative ``Real`` arguments.

.. function:: log1p(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       log1p(x)

   Accurate natural logarithm of ``1+x``\ . Throws ``DomainError`` for ``Real`` arguments less than -1.

   There is an experimental variant in the ``Base.Math.JuliaLibm`` module, which is typically faster and more accurate.

.. function:: frexp(val)

   .. Docstring generated from Julia source

   ::

              frexp(val)

   Return ``(x,exp)`` such that ``x`` has a magnitude in the interval ``[1/2, 1)`` or 0,
   and val = :math:`x \times 2^{exp}`.

.. function:: exp(x)

   .. Docstring generated from Julia source

   ::

              exp(x)

   Compute :math:`e^x`

.. function:: exp2(x)

   .. Docstring generated from Julia source

   ::

              exp2(x)

   Compute :math:`2^x`

.. function:: exp10(x)

   .. Docstring generated from Julia source

   ::

              exp10(x)

   Compute :math:`10^x`

.. function:: ldexp(x, n)

   .. Docstring generated from Julia source

   ::

              ldexp(x, n)

   Compute :math:`x \times 2^n`

.. function:: modf(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts of a number. Both parts have the same sign as the argument.

.. function:: expm1(x)

   .. Docstring generated from Julia source

   ::

              expm1(x)

   Accurately compute :math:`e^x-1`

.. function:: round([T,] x, [digits, [base]], [r::RoundingMode])

   .. Docstring generated from Julia source

   ::

              round([T,] x, [digits, [base]], [r::RoundingMode])

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

   ::

              round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex-valued
   ``z`` to ``z``, breaking ties using the specified :obj:`RoundingMode`\ s.
   The first :obj:`RoundingMode` is used for rounding the real components while
   the second is used for rounding the imaginary components.

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

   .. Docstring generated from Julia source

   ::

              round([T,] x, [digits, [base]], [r::RoundingMode])

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

   ::

              round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex-valued
   ``z`` to ``z``, breaking ties using the specified :obj:`RoundingMode`\ s.
   The first :obj:`RoundingMode` is used for rounding the real components while
   the second is used for rounding the imaginary components.

.. function:: ceil([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ::

              ceil([T,] x, [digits, [base]])

   ``ceil(x)`` returns the nearest integral value of the same type as ``x``
   that is greater than or equal to ``x``.

   ``ceil(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`.

.. function:: floor([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ::

              floor([T,] x, [digits, [base]])

   ``floor(x)`` returns the nearest integral value of the same type as ``x``
   that is less than or equal to ``x``.

   ``floor(T, x)`` converts the result to type ``T``, throwing an
   ``InexactError`` if the value is not representable.

   ``digits`` and ``base`` work as for :func:`round`.

.. function:: trunc([T,] x, [digits, [base]])

   .. Docstring generated from Julia source

   ::

              trunc(dt::TimeType, ::Type{Period}) -> TimeType

   Truncates the value of ``dt`` according to the provided ``Period`` type.
   E.g. if ``dt`` is ``1996-01-01T12:30:00``, then ``trunc(dt,Day) == 1996-01-01T00:00:00``.

.. function:: unsafe_trunc(T, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       unsafe_trunc(T, x)

   ``unsafe_trunc(T, x)`` returns the nearest integral value of type ``T`` whose absolute value is less than or equal to ``x``\ . If the value is not representable by ``T``\ , an arbitrary value will be returned.

.. function:: signif(x, digits, [base])

   .. Docstring generated from Julia source

   .. code-block:: julia

       signif(x, digits, [base])

   Rounds (in the sense of ``round``\ ) ``x`` so that there are ``digits`` significant digits, under a base ``base`` representation, default 10. E.g., ``signif(123.456, 2)`` is ``120.0``\ , and ``signif(357.913, 4, 2)`` is ``352.0``\ .

.. function:: min(x, y, ...)

   .. Docstring generated from Julia source

   .. code-block:: julia

       min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over arrays.

.. function:: max(x, y, ...)

   .. Docstring generated from Julia source

   .. code-block:: julia

       max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over arrays.

.. function:: minmax(x, y)

   .. Docstring generated from Julia source

   ::

              minmax(x, y)

   Return ``(min(x,y), max(x,y))``.
   See also: :func:`extrema` that returns ``(minimum(x), maximum(x))``

.. function:: clamp(x, lo, hi)

   .. Docstring generated from Julia source

   .. code-block:: julia

       clamp(x, lo, hi)

   Return x if ``lo <= x <= hi``\ . If ``x < lo``\ , return ``lo``\ . If ``x > hi``\ , return ``hi``\ . Arguments are promoted to a common type. Operates elementwise over ``x`` if it is an array.

.. function:: abs(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       abs(x)

   Absolute value of ``x``

.. function:: abs2(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       abs2(x)

   Squared absolute value of ``x``

.. function:: copysign(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       copysign(x, y)

   Return ``x`` such that it has the same sign as ``y``

.. function:: sign(x)

   .. Docstring generated from Julia source

   ::

              sign(x)

   Return zero if ``x==0`` and :math:`x/|x|` otherwise (i.e., ±1 for real ``x``).

.. function:: signbit(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       signbit(x)

   Returns ``true`` if the value of the sign of ``x`` is negative, otherwise ``false``\ .

.. function:: flipsign(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       flipsign(x, y)

   Return ``x`` with its sign flipped if ``y`` is negative. For example ``abs(x) = flipsign(x,x)``\ .

.. function:: sqrt(x)

   .. Docstring generated from Julia source

   ::

              sqrt(x)

   Return :math:`\sqrt{x}`. Throws ``DomainError`` for negative ``Real`` arguments. Use complex negative arguments instead.  The prefix operator ``√`` is equivalent to ``sqrt``.

.. function:: isqrt(n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       isqrt(n)

   Integer square root: the largest integer ``m`` such that ``m*m <= n``\ .

.. function:: cbrt(x)

   .. Docstring generated from Julia source

   ::

              cbrt(x)

   Return :math:`x^{1/3}`.  The prefix operator ``∛`` is equivalent to ``cbrt``.

.. function:: erf(x)

   .. Docstring generated from Julia source

   ::

              erf(x)

   Compute the error function of ``x``, defined by
   :math:`\frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt`
   for arbitrary complex ``x``.

.. function:: erfc(x)

   .. Docstring generated from Julia source

   ::

              erfc(x)

   Compute the complementary error function of ``x``,
   defined by :math:`1 - \operatorname{erf}(x)`.

.. function:: erfcx(x)

   .. Docstring generated from Julia source

   ::

              erfcx(x)

   Compute the scaled complementary error function of ``x``,
   defined by :math:`e^{x^2} \operatorname{erfc}(x)`.  Note
   also that :math:`\operatorname{erfcx}(-ix)` computes the
   Faddeeva function :math:`w(x)`.

.. function:: erfi(x)

   .. Docstring generated from Julia source

   ::

              erfi(x)

   Compute the imaginary error function of ``x``,
   defined by :math:`-i \operatorname{erf}(ix)`.

.. function:: dawson(x)

   .. Docstring generated from Julia source

   ::

              dawson(x)

   Compute the Dawson function (scaled imaginary error function) of ``x``,
   defined by :math:`\frac{\sqrt{\pi}}{2} e^{-x^2} \operatorname{erfi}(x)`.

.. function:: erfinv(x)

   .. Docstring generated from Julia source

   ::

              erfinv(x)

   Compute the inverse error function of a real ``x``,
   defined by :math:`\operatorname{erf}(\operatorname{erfinv}(x)) = x`.

.. function:: erfcinv(x)

   .. Docstring generated from Julia source

   ::

              erfcinv(x)

   Compute the inverse error complementary function of a real ``x``,
   defined by :math:`\operatorname{erfc}(\operatorname{erfcinv}(x)) = x`.

.. function:: real(z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       real(z)

   Return the real part of the complex number ``z``

.. function:: imag(z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       imag(z)

   Return the imaginary part of the complex number ``z``

.. function:: reim(z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       reim(z)

   Return both the real and imaginary parts of the complex number ``z``

.. function:: conj(z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       conj(z)

   Compute the complex conjugate of a complex number ``z``

.. function:: angle(z)

   .. Docstring generated from Julia source

   .. code-block:: julia

       angle(z)

   Compute the phase angle in radians of a complex number ``z``

.. function:: cis(z)

   .. Docstring generated from Julia source

   ::

              cis(z)

   Return :math:`\exp(iz)`.

.. function:: binomial(n,k)

   .. Docstring generated from Julia source

   .. code-block:: julia

       binomial(n,k)

   Number of ways to choose ``k`` out of ``n`` items

.. function:: factorial(n)

   .. Docstring generated from Julia source

   ::

              factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: factorial(n,k)

   .. Docstring generated from Julia source

   ::

              factorial(n,k)

   Compute ``factorial(n)/factorial(k)``

.. function:: factor(n) -> Dict

   .. Docstring generated from Julia source

   ::

              factor(n) -> Dict

   Compute the prime factorization of an integer ``n``. Returns a dictionary. The keys of the dictionary correspond to the factors, and hence are of the same type as ``n``. The value associated with each key indicates the number of times the factor appears in the factorization.

   .. doctest::

      julia> factor(100) # == 2*2*5*5
      Dict{Int64,Int64} with 2 entries:
        2 => 2
        5 => 2

.. function:: gcd(x,y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       gcd(x,y)

   Greatest common (positive) divisor (or zero if x and y are both zero).

.. function:: lcm(x,y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lcm(x,y)

   Least common (non-negative) multiple.

.. function:: gcdx(x,y)

   .. Docstring generated from Julia source

   ::

              gcdx(x,y)

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

   .. Docstring generated from Julia source

   .. code-block:: julia

       ispow2(n) -> Bool

   Test whether ``n`` is a power of two

.. function:: nextpow2(n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       nextpow2(n)

   The smallest power of two not less than ``n``\ . Returns 0 for ``n==0``\ , and returns ``-nextpow2(-n)`` for negative arguments.

.. function:: prevpow2(n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       prevpow2(n)

   The largest power of two not greater than ``n``\ . Returns 0 for ``n==0``\ , and returns ``-prevpow2(-n)`` for negative arguments.

.. function:: nextpow(a, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       nextpow(a, x)

   The smallest ``a^n`` not less than ``x``\ , where ``n`` is a non-negative integer. ``a`` must be greater than 1, and ``x`` must be greater than 0.

.. function:: prevpow(a, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       prevpow(a, x)

   The largest ``a^n`` not greater than ``x``\ , where ``n`` is a non-negative integer. ``a`` must be greater than 1, and ``x`` must not be less than 1.

.. function:: nextprod([k_1,k_2,...], n)

   .. Docstring generated from Julia source

   ::

              nextprod([k_1,k_2,...], n)

   Next integer not less than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: prevprod([k_1,k_2,...], n)

   .. Docstring generated from Julia source

   ::

              prevprod([k_1,k_2,...], n)

   Previous integer not greater than ``n`` that can be written as :math:`\prod k_i^{p_i}` for integers :math:`p_1`, :math:`p_2`, etc.

.. function:: invmod(x,m)

   .. Docstring generated from Julia source

   ::

              invmod(x,m)

   Take the inverse of ``x`` modulo ``m``: ``y`` such that :math:`xy = 1 \pmod m`

.. function:: powermod(x, p, m)

   .. Docstring generated from Julia source

   ::

              powermod(x, p, m)

   Compute :math:`x^p \pmod m`

.. function:: gamma(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       gamma(x)

   Compute the gamma function of ``x``

.. function:: lgamma(x)

   .. Docstring generated from Julia source

   ::

              lgamma(x)

   Compute the logarithm of the absolute value of :func:`gamma` for
   :obj:`Real` ``x``, while for :obj:`Complex` ``x`` it computes the
   logarithm of ``gamma(x)``.

.. function:: lfact(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       lfact(x)

   Compute the logarithmic factorial of ``x``

.. function:: digamma(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       digamma(x)

   Compute the digamma function of ``x`` (the logarithmic derivative of ``gamma(x)``\ )

.. function:: invdigamma(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       invdigamma(x)

   Compute the inverse digamma function of ``x``\ .

.. function:: trigamma(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       trigamma(x)

   Compute the trigamma function of ``x`` (the logarithmic second derivative of ``gamma(x)``\ )

.. function:: polygamma(m, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       polygamma(m, x)

   Compute the polygamma function of order ``m`` of argument ``x`` (the ``(m+1)th`` derivative of the logarithm of ``gamma(x)``\ )

.. function:: airy(k,x)

   .. Docstring generated from Julia source

   ::

              airy(k,x)

   kth derivative of the Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyai(x)

   .. Docstring generated from Julia source

   ::

              airyai(x)

   Airy function :math:`\operatorname{Ai}(x)`.

.. function:: airyprime(x)

   .. Docstring generated from Julia source

   ::

              airyprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airyaiprime(x)

   .. Docstring generated from Julia source

   ::

              airyaiprime(x)

   Airy function derivative :math:`\operatorname{Ai}'(x)`.

.. function:: airybi(x)

   .. Docstring generated from Julia source

   ::

              airybi(x)

   Airy function :math:`\operatorname{Bi}(x)`.

.. function:: airybiprime(x)

   .. Docstring generated from Julia source

   ::

              airybiprime(x)

   Airy function derivative :math:`\operatorname{Bi}'(x)`.

.. function:: airyx(k,x)

   .. Docstring generated from Julia source

   ::

              airyx(k,x)

   scaled kth derivative of the Airy function, return :math:`\operatorname{Ai}(x) e^{\frac{2}{3} x \sqrt{x}}` for ``k == 0 || k == 1``, and :math:`\operatorname{Ai}(x) e^{- \left| \operatorname{Re} \left( \frac{2}{3} x \sqrt{x} \right) \right|}` for ``k == 2 || k == 3``.

.. function:: besselj0(x)

   .. Docstring generated from Julia source

   ::

              besselj0(x)

   Bessel function of the first kind of order 0, :math:`J_0(x)`.

.. function:: besselj1(x)

   .. Docstring generated from Julia source

   ::

              besselj1(x)

   Bessel function of the first kind of order 1, :math:`J_1(x)`.

.. function:: besselj(nu, x)

   .. Docstring generated from Julia source

   ::

              besselj(nu, x)

   Bessel function of the first kind of order ``nu``, :math:`J_\nu(x)`.

.. function:: besseljx(nu, x)

   .. Docstring generated from Julia source

   ::

              besseljx(nu, x)

   Scaled Bessel function of the first kind of order ``nu``, :math:`J_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: bessely0(x)

   .. Docstring generated from Julia source

   ::

              bessely0(x)

   Bessel function of the second kind of order 0, :math:`Y_0(x)`.

.. function:: bessely1(x)

   .. Docstring generated from Julia source

   ::

              bessely1(x)

   Bessel function of the second kind of order 1, :math:`Y_1(x)`.

.. function:: bessely(nu, x)

   .. Docstring generated from Julia source

   ::

              bessely(nu, x)

   Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x)`.

.. function:: besselyx(nu, x)

   .. Docstring generated from Julia source

   ::

              besselyx(nu, x)

   Scaled Bessel function of the second kind of order ``nu``, :math:`Y_\nu(x) e^{- | \operatorname{Im}(x) |}`.

.. function:: hankelh1(nu, x)

   .. Docstring generated from Julia source

   ::

              hankelh1(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x)`.

.. function:: hankelh1x(nu, x)

   .. Docstring generated from Julia source

   ::

              hankelh1x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(1)}_\nu(x) e^{-x i}`.

.. function:: hankelh2(nu, x)

   .. Docstring generated from Julia source

   ::

              hankelh2(nu, x)

   Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x)`.

.. function:: hankelh2x(nu, x)

   .. Docstring generated from Julia source

   ::

              hankelh2x(nu, x)

   Scaled Bessel function of the third kind of order ``nu``, :math:`H^{(2)}_\nu(x) e^{x i}`.

.. function:: besselh(nu, k, x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       besselh(nu, k, x)

   Bessel function of the third kind of order ``nu`` (Hankel function). ``k`` is either 1 or 2, selecting ``hankelh1`` or ``hankelh2``\ , respectively.

.. function:: besseli(nu, x)

   .. Docstring generated from Julia source

   ::

              besseli(nu, x)

   Modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x)`.

.. function:: besselix(nu, x)

   .. Docstring generated from Julia source

   ::

              besselix(nu, x)

   Scaled modified Bessel function of the first kind of order ``nu``, :math:`I_\nu(x) e^{- | \operatorname{Re}(x) |}`.

.. function:: besselk(nu, x)

   .. Docstring generated from Julia source

   ::

              besselk(nu, x)

   Modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x)`.

.. function:: besselkx(nu, x)

   .. Docstring generated from Julia source

   ::

              besselkx(nu, x)

   Scaled modified Bessel function of the second kind of order ``nu``, :math:`K_\nu(x) e^x`.

.. function:: beta(x, y)

   .. Docstring generated from Julia source

   ::

              beta(x, y)

   Euler integral of the first kind :math:`\operatorname{B}(x,y) = \Gamma(x)\Gamma(y)/\Gamma(x+y)`.

.. function:: lbeta(x, y)

   .. Docstring generated from Julia source

   ::

              lbeta(x, y)

   Natural logarithm of the absolute value of the beta function :math:`\log(|\operatorname{B}(x,y)|)`.

.. function:: eta(x)

   .. Docstring generated from Julia source

   ::

              eta(x)

   Dirichlet eta function :math:`\eta(s) = \sum^\infty_{n=1}(-)^{n-1}/n^{s}`.

.. function:: zeta(s)

   .. Docstring generated from Julia source

   ::

              zeta(s, z)

   Hurwitz zeta function :math:`\zeta(s, z)`.  (This is equivalent to
   the Riemann zeta function :math:`\zeta(s)` for the case of ``z=1``.)

.. function:: zeta(s, z)

   .. Docstring generated from Julia source

   ::

              zeta(s, z)

   Hurwitz zeta function :math:`\zeta(s, z)`.  (This is equivalent to
   the Riemann zeta function :math:`\zeta(s)` for the case of ``z=1``.)

.. function:: ndigits(n, b)

   .. Docstring generated from Julia source

   .. code-block:: julia

       ndigits(n, b)

   Compute the number of digits in number ``n`` written in base ``b``\ .

.. function:: widemul(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       widemul(x, y)

   Multiply ``x`` and ``y``\ , giving the result as a larger type.

.. function:: @evalpoly(z, c...)

   .. Docstring generated from Julia source

   ::

              @evalpoly(z, c...)

   Evaluate the polynomial :math:`\sum_k c[k] z^{k-1}` for the
   coefficients ``c[1]``, ``c[2]``, ...; that is, the coefficients are
   given in ascending order by power of ``z``.  This macro expands to
   efficient inline code that uses either Horner's method or, for
   complex ``z``, a more efficient Goertzel-like algorithm.

Statistics
----------

.. function:: mean(v[, region])

   .. Docstring generated from Julia source

   .. code-block:: julia

       mean(v[, region])

   Compute the mean of whole array ``v``\ , or optionally along the dimensions in ``region``\ . Note: Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArray`` package is recommended.

.. function:: mean!(r, v)

   .. Docstring generated from Julia source

   .. code-block:: julia

       mean!(r, v)

   Compute the mean of ``v`` over the singleton dimensions of ``r``\ , and write results to ``r``\ .

.. function:: std(v[, region])

   .. Docstring generated from Julia source

   .. code-block:: julia

       std(v[, region])

   Compute the sample standard deviation of a vector or array ``v``\ , optionally along dimensions in ``region``\ . The algorithm returns an estimator of the generative distribution's standard deviation under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sqrt(sum((v - mean(v)).^2) / (length(v) - 1))``\ . Note: Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArray`` package is recommended.

.. function:: stdm(v, m)

   .. Docstring generated from Julia source

   .. code-block:: julia

       stdm(v, m)

   Compute the sample standard deviation of a vector ``v`` with known mean ``m``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: var(v[, region])

   .. Docstring generated from Julia source

   .. code-block:: julia

       var(v[, region])

   Compute the sample variance of a vector or array ``v``\ , optionally along dimensions in ``region``\ . The algorithm will return an estimator of the generative distribution's variance under the assumption that each entry of ``v`` is an IID drawn from that generative distribution. This computation is equivalent to calculating ``sum((v - mean(v)).^2) / (length(v) - 1)``\ . Note: Julia does not ignore ``NaN`` values in the computation. For applications requiring the handling of missing data, the ``DataArray`` package is recommended.

.. function:: varm(v, m)

   .. Docstring generated from Julia source

   .. code-block:: julia

       varm(v, m)

   Compute the sample variance of a vector ``v`` with known mean ``m``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: middle(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself, but of the type of ``middle(x, x)`` for consistency.

   .. code-block:: julia

       middle(x, y)

   Compute the middle of two reals ``x`` and ``y``\ , which is equivalent in both value and type to computing their mean (``(x + y) / 2``\ ).

   .. code-block:: julia

       middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.

   .. code-block:: julia

       middle(array)

   Compute the middle of an array, which consists in finding its extrema and then computing their mean.

.. function:: middle(x, y)

   .. Docstring generated from Julia source

   .. code-block:: julia

       middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself, but of the type of ``middle(x, x)`` for consistency.

   .. code-block:: julia

       middle(x, y)

   Compute the middle of two reals ``x`` and ``y``\ , which is equivalent in both value and type to computing their mean (``(x + y) / 2``\ ).

   .. code-block:: julia

       middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.

   .. code-block:: julia

       middle(array)

   Compute the middle of an array, which consists in finding its extrema and then computing their mean.

.. function:: middle(range)

   .. Docstring generated from Julia source

   .. code-block:: julia

       middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself, but of the type of ``middle(x, x)`` for consistency.

   .. code-block:: julia

       middle(x, y)

   Compute the middle of two reals ``x`` and ``y``\ , which is equivalent in both value and type to computing their mean (``(x + y) / 2``\ ).

   .. code-block:: julia

       middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.

   .. code-block:: julia

       middle(array)

   Compute the middle of an array, which consists in finding its extrema and then computing their mean.

.. function:: middle(array)

   .. Docstring generated from Julia source

   .. code-block:: julia

       middle(x)

   Compute the middle of a scalar value, which is equivalent to ``x`` itself, but of the type of ``middle(x, x)`` for consistency.

   .. code-block:: julia

       middle(x, y)

   Compute the middle of two reals ``x`` and ``y``\ , which is equivalent in both value and type to computing their mean (``(x + y) / 2``\ ).

   .. code-block:: julia

       middle(range)

   Compute the middle of a range, which consists in computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.

   .. code-block:: julia

       middle(array)

   Compute the middle of an array, which consists in finding its extrema and then computing their mean.

.. function:: median(v[, region])

   .. Docstring generated from Julia source

   .. code-block:: julia

       median(v[, region])

   Compute the median of whole array ``v``\ , or optionally along the dimensions in ``region``\ . ``NaN`` is returned if the data contains any ``NaN`` values. For applications requiring the handling of missing data, the ``DataArrays`` package is recommended.

.. function:: median!(v)

   .. Docstring generated from Julia source

   .. code-block:: julia

       median!(v)

   Like ``median``\ , but may overwrite the input vector.

.. function:: hist(v[, n]) -> e, counts

   .. Docstring generated from Julia source

   .. code-block:: julia

       hist(v[, n]) -> e, counts

   Compute the histogram of ``v``\ , optionally using approximately ``n`` bins. The return values are a range ``e``\ , which correspond to the edges of the bins, and ``counts`` containing the number of elements of ``v`` in each bin. Note: Julia does not ignore ``NaN`` values in the computation.

   .. code-block:: julia

       hist(v, e) -> e, counts

   Compute the histogram of ``v`` using a vector/range ``e`` as the edges for the bins. The result will be a vector of length ``length(e) - 1``\ , such that the element at location ``i`` satisfies ``sum(e[i] .< v .<= e[i+1])``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist(v, e) -> e, counts

   .. Docstring generated from Julia source

   .. code-block:: julia

       hist(v[, n]) -> e, counts

   Compute the histogram of ``v``\ , optionally using approximately ``n`` bins. The return values are a range ``e``\ , which correspond to the edges of the bins, and ``counts`` containing the number of elements of ``v`` in each bin. Note: Julia does not ignore ``NaN`` values in the computation.

   .. code-block:: julia

       hist(v, e) -> e, counts

   Compute the histogram of ``v`` using a vector/range ``e`` as the edges for the bins. The result will be a vector of length ``length(e) - 1``\ , such that the element at location ``i`` satisfies ``sum(e[i] .< v .<= e[i+1])``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist!(counts, v, e) -> e, counts

   .. Docstring generated from Julia source

   .. code-block:: julia

       hist!(counts, v, e) -> e, counts

   Compute the histogram of ``v``\ , using a vector/range ``e`` as the edges for the bins. This function writes the resultant counts to a pre-allocated array ``counts``\ .

.. function:: hist2d(M, e1, e2) -> (edge1, edge2, counts)

   .. Docstring generated from Julia source

   .. code-block:: julia

       hist2d(M, e1, e2) -> (edge1, edge2, counts)

   Compute a "2d histogram" of a set of N points specified by N-by-2 matrix ``M``\ . Arguments ``e1`` and ``e2`` are bins for each dimension, specified either as integer bin counts or vectors of bin edges. The result is a tuple of ``edge1`` (the bin edges used in the first dimension), ``edge2`` (the bin edges used in the second dimension), and ``counts``\ , a histogram matrix of size ``(length(edge1)-1, length(edge2)-1)``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: hist2d!(counts, M, e1, e2) -> (e1, e2, counts)

   .. Docstring generated from Julia source

   .. code-block:: julia

       hist2d!(counts, M, e1, e2) -> (e1, e2, counts)

   Compute a "2d histogram" with respect to the bins delimited by the edges given in ``e1`` and ``e2``\ . This function writes the results to a pre-allocated array ``counts``\ .

.. function:: histrange(v, n)

   .. Docstring generated from Julia source

   .. code-block:: julia

       histrange(v, n)

   Compute *nice* bin ranges for the edges of a histogram of ``v``\ , using approximately ``n`` bins. The resulting step sizes will be 1, 2 or 5 multiplied by a power of 10. Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: midpoints(e)

   .. Docstring generated from Julia source

   .. code-block:: julia

       midpoints(e)

   Compute the midpoints of the bins with edges ``e``\ . The result is a vector/range of length ``length(e) - 1``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   .. Docstring generated from Julia source

   .. code-block:: julia

       quantile(v, p)

   Compute the quantiles of a vector ``v`` at a specified set of probability values ``p``\ . Note: Julia does not ignore ``NaN`` values in the computation.

   .. code-block:: julia

       quantile(v, p)

   Compute the quantile of a vector ``v`` at the probability ``p``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile(v, p)

   .. Docstring generated from Julia source

   .. code-block:: julia

       quantile(v, p)

   Compute the quantiles of a vector ``v`` at a specified set of probability values ``p``\ . Note: Julia does not ignore ``NaN`` values in the computation.

   .. code-block:: julia

       quantile(v, p)

   Compute the quantile of a vector ``v`` at the probability ``p``\ . Note: Julia does not ignore ``NaN`` values in the computation.

.. function:: quantile!(v, p)

   .. Docstring generated from Julia source

   .. code-block:: julia

       quantile!(v, p)

   Like ``quantile``\ , but overwrites the input vector.

.. function:: cov(v1[, v2][, vardim=1, corrected=true, mean=nothing])

   .. Docstring generated from Julia source

   .. code-block:: julia

       cov(v1[, v2][, vardim=1, corrected=true, mean=nothing])

   Compute the Pearson covariance between the vector(s) in ``v1`` and ``v2``\ . Here, ``v1`` and ``v2`` can be either vectors or matrices.

   This function accepts three keyword arguments:

     *   ``vardim``\ : the dimension of variables. When ``vardim = 1``\ , variables are considered in columns while observations in rows; when ``vardim = 2``\ , variables are in rows while observations in columns. By default, it is set to ``1``\ .
     *   ``corrected``\ : whether to apply Bessel's correction (divide by ``n-1`` instead of ``n``\ ). By default, it is set to ``true``\ .
     *   ``mean``\ : allow users to supply mean values that are known. By default, it is set to ``nothing``\ , which indicates that the mean(s) are unknown, and the function will compute the mean. Users can use ``mean=0`` to indicate that the input data are centered, and hence there's no need to subtract the mean.

   The size of the result depends on the size of ``v1`` and ``v2``\ . When both ``v1`` and ``v2`` are vectors, it returns the covariance between them as a scalar. When either one is a matrix, it returns a covariance matrix of size ``(n1, n2)``\ , where ``n1`` and ``n2`` are the numbers of slices in ``v1`` and ``v2``\ , which depend on the setting of ``vardim``\ .

   Note: ``v2`` can be omitted, which indicates ``v2 = v1``\ .

.. function:: cor(v1[, v2][, vardim=1, mean=nothing])

   .. Docstring generated from Julia source

   .. code-block:: julia

       cor(v1[, v2][, vardim=1, mean=nothing])

   Compute the Pearson correlation between the vector(s) in ``v1`` and ``v2``\ .

   Users can use the keyword argument ``vardim`` to specify the variable dimension, and ``mean`` to supply pre-computed mean values.

Signal Processing
-----------------

Fast Fourier transform (FFT) functions in Julia are
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

   .. Docstring generated from Julia source

   ::

              fft!(A [, dims])

   Same as :func:`fft`, but operates in-place on ``A``,
   which must be an array of complex floating-point numbers.

.. function:: ifft(A [, dims])

   .. Docstring generated from Julia source

   ::

             ifft(A [, dims])

   Multidimensional inverse FFT.

   A one-dimensional inverse FFT computes

   .. math::

      \operatorname{IDFT}(A)[k] = \frac{1}{\operatorname{length}(A)}
      \sum_{n=1}^{\operatorname{length}(A)} \exp\left(+i\frac{2\pi (n-1)(k-1)}
      {\operatorname{length}(A)} \right) A[n].

   A multidimensional inverse FFT simply performs this operation along each transformed dimension of `A`.

.. function:: ifft!(A [, dims])

   .. Docstring generated from Julia source

   ::

              ifft!(A [, dims])

   Same as :func:`ifft`, but operates in-place on ``A``.

.. function:: bfft(A [, dims])

   .. Docstring generated from Julia source

   ::

              bfft(A [, dims])

   Similar to :func:`ifft`, but computes an unnormalized inverse (backward)
   transform, which must be divided by the product of the sizes of the
   transformed dimensions in order to obtain the inverse. (This is slightly
   more efficient than :func:`ifft` because it omits a scaling step, which in
   some applications can be combined with other computational steps elsewhere.)

   .. math::

      \operatorname{BDFT}(A)[k] = \operatorname{length}(A) \operatorname{IDFT}(A)[k]

.. function:: bfft!(A [, dims])

   .. Docstring generated from Julia source

   ::

              bfft!(A [, dims])

   Same as :func:`bfft`, but operates in-place on ``A``.

.. function:: plan_fft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_fft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Pre-plan an optimized FFT along given dimensions (``dims``) of arrays
   matching the shape and type of ``A``.  (The first two arguments have
   the same meaning as for :func:`fft`.)  Returns an object ``P`` which
   represents the linear operator computed by the FFT, and which contains
   all of the information needed to compute ``fft(A, dims)`` quickly.

   To apply ``P`` to an array ``A``, use ``P * A``; in general, the
   syntax for applying plans is much like that of matrices.  (A plan
   can only be applied to arrays of the same size as the ``A`` for
   which the plan was created.)  You can also apply a plan with a
   preallocated output array ``Â`` by calling ``A_mul_B!(Â, plan,
   A)``.  You can compute the inverse-transform plan by ``inv(P)`` and
   apply the inverse plan with ``P \ Â`` (the inverse plan is cached
   and reused for subsequent calls to ``inv`` or ``\``), and apply the
   inverse plan to a pre-allocated output array ``A`` with
   ``A_ldiv_B!(A, P, Â)``.

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

.. function:: plan_ifft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_ifft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Same as :func:`plan_fft`, but produces a plan that performs inverse transforms
   :func:`ifft`.

.. function:: plan_bfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_bfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Same as :func:`plan_fft`, but produces a plan that performs an unnormalized
   backwards transform :func:`bfft`.

.. function:: plan_fft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_fft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Same as :func:`plan_fft`, but operates in-place on ``A``.

.. function:: plan_ifft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_ifft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Same as :func:`plan_ifft`, but operates in-place on ``A``.

.. function:: plan_bfft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_bfft!(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Same as :func:`plan_bfft`, but operates in-place on ``A``.

.. function:: rfft(A [, dims])

   .. Docstring generated from Julia source

   ::

              rfft(A [, dims])

   Multidimensional FFT of a real array A, exploiting the fact that
   the transform has conjugate symmetry in order to save roughly half
   the computational time and storage costs compared with :func:`fft`.
   If ``A`` has size ``(n_1, ..., n_d)``, the result has size
   ``(div(n_1,2)+1, ..., n_d)``.

   The optional ``dims`` argument specifies an iterable subset of one or
   more dimensions of ``A`` to transform, similar to :func:`fft`.  Instead
   of (roughly) halving the first dimension of ``A`` in the result, the
   ``dims[1]`` dimension is (roughly) halved in the same way.

.. function:: irfft(A, d [, dims])

   .. Docstring generated from Julia source

   ::

              irfft(A, d [, dims])

   Inverse of :func:`rfft`: for a complex array ``A``, gives the
   corresponding real array whose FFT yields ``A`` in the first half.
   As for :func:`rfft`, ``dims`` is an optional subset of dimensions
   to transform, defaulting to ``1:ndims(A)``.

   ``d`` is the length of the transformed real array along the ``dims[1]``
   dimension, which must satisfy ``div(d,2)+1 == size(A,dims[1])``.
   (This parameter cannot be inferred from ``size(A)`` since both
   ``2*size(A,dims[1])-2`` as well as ``2*size(A,dims[1])-1`` are valid sizes
   for the transformed real array.)

.. function:: brfft(A, d [, dims])

   .. Docstring generated from Julia source

   ::

              brfft(A, d [, dims])

   Similar to :func:`irfft` but computes an unnormalized inverse transform
   (similar to :func:`bfft`), which must be divided by the product
   of the sizes of the transformed dimensions (of the real output array)
   in order to obtain the inverse transform.

.. function:: plan_rfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_rfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Pre-plan an optimized real-input FFT, similar to :func:`plan_fft`
   except for :func:`rfft` instead of :func:`fft`.  The first two
   arguments, and the size of the transformed result, are the same as
   for :func:`rfft`.

.. function:: plan_brfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_brfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Pre-plan an optimized real-input unnormalized transform, similar to
   :func:`plan_rfft` except for :func:`brfft` instead of :func:`rfft`.
   The first two arguments and the size of the transformed result, are
   the same as for :func:`brfft`.

.. function:: plan_irfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   .. Docstring generated from Julia source

   ::

              plan_irfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

   Pre-plan an optimized inverse real-input FFT, similar to :func:`plan_rfft`
   except for :func:`irfft` and :func:`brfft`, respectively.  The first
   three arguments have the same meaning as for :func:`irfft`.

.. function:: dct(A [, dims])

   .. Docstring generated from Julia source

   ::

              dct(A [, dims])

   Performs a multidimensional type-II discrete cosine transform (DCT)
   of the array ``A``, using the unitary normalization of the DCT.
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_dct` for even greater efficiency.

.. function:: dct!(A [, dims])

   .. Docstring generated from Julia source

   ::

              dct!(A [, dims])

   Same as :func:`dct!`, except that it operates in-place
   on ``A``, which must be an array of real or complex floating-point
   values.

.. function:: idct(A [, dims])

   .. Docstring generated from Julia source

   ::

              idct(A [, dims])

   Computes the multidimensional inverse discrete cosine transform (DCT)
   of the array ``A`` (technically, a type-III DCT with the unitary
   normalization).
   The optional ``dims`` argument specifies an iterable subset of
   dimensions (e.g. an integer, range, tuple, or array) to transform
   along.  Most efficient if the size of ``A`` along the transformed
   dimensions is a product of small primes; see :func:`nextprod`.  See
   also :func:`plan_idct` for even greater efficiency.

.. function:: idct!(A [, dims])

   .. Docstring generated from Julia source

   ::

              idct!(A [, dims])

   Same as :func:`idct!`, but operates in-place on ``A``.

.. function:: plan_dct(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_dct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`dct`.
   The first two arguments have the same meaning as for :func:`dct`.

.. function:: plan_dct!(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_dct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_dct`, but operates in-place on ``A``.

.. function:: plan_idct(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_idct(A [, dims [, flags [, timelimit]]])

   Pre-plan an optimized inverse discrete cosine transform (DCT), similar to
   :func:`plan_fft` except producing a function that computes :func:`idct`.
   The first two arguments have the same meaning as for :func:`idct`.

.. function:: plan_idct!(A [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_idct!(A [, dims [, flags [, timelimit]]])

   Same as :func:`plan_idct`, but operates in-place on ``A``.

.. function:: fftshift(x)

   .. Docstring generated from Julia source

   .. code-block:: julia

       fftshift(x)

   Swap the first and second halves of each dimension of ``x``\ .

   .. code-block:: julia

       fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``\ .

.. function:: fftshift(x,dim)

   .. Docstring generated from Julia source

   .. code-block:: julia

       fftshift(x)

   Swap the first and second halves of each dimension of ``x``\ .

   .. code-block:: julia

       fftshift(x,dim)

   Swap the first and second halves of the given dimension of array ``x``\ .

.. function:: ifftshift(x, [dim])

   .. Docstring generated from Julia source

   .. code-block:: julia

       ifftshift(x, [dim])

   Undoes the effect of ``fftshift``\ .

.. function:: filt(b, a, x, [si])

   .. Docstring generated from Julia source

   .. code-block:: julia

       filt(b, a, x, [si])

   Apply filter described by vectors ``a`` and ``b`` to vector ``x``\ , with an optional initial filter state vector ``si`` (defaults to zeros).

.. function:: filt!(out, b, a, x, [si])

   .. Docstring generated from Julia source

   ::

              filt!(out, b, a, x, [si])

   Same as :func:`filt` but writes the result into the ``out`` argument,
   which may alias the input ``x`` to modify it in-place.

.. function:: deconv(b,a)

   .. Docstring generated from Julia source

   .. code-block:: julia

       deconv(b,a)

   Construct vector ``c`` such that ``b = conv(a,c) + r``\ . Equivalent to polynomial division.

.. function:: conv(u,v)

   .. Docstring generated from Julia source

   .. code-block:: julia

       conv(u,v)

   Convolution of two vectors. Uses FFT algorithm.

.. function:: conv2(u,v,A)

   .. Docstring generated from Julia source

   .. code-block:: julia

       conv2(u,v,A)

   2-D convolution of the matrix ``A`` with the 2-D separable kernel generated by the vectors ``u`` and ``v``\ . Uses 2-D FFT algorithm

   .. code-block:: julia

       conv2(B,A)

   2-D convolution of the matrix ``B`` with the matrix ``A``\ . Uses 2-D FFT algorithm

.. function:: conv2(B,A)

   .. Docstring generated from Julia source

   .. code-block:: julia

       conv2(u,v,A)

   2-D convolution of the matrix ``A`` with the 2-D separable kernel generated by the vectors ``u`` and ``v``\ . Uses 2-D FFT algorithm

   .. code-block:: julia

       conv2(B,A)

   2-D convolution of the matrix ``B`` with the matrix ``A``\ . Uses 2-D FFT algorithm

.. function:: xcorr(u,v)

   .. Docstring generated from Julia source

   .. code-block:: julia

       xcorr(u,v)

   Compute the cross-correlation of two vectors.

The following functions are defined within the ``Base.FFTW`` module.

.. currentmodule:: Base.FFTW

.. function:: r2r(A, kind [, dims])

   .. Docstring generated from Julia source

   ::

              r2r(A, kind [, dims])

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

   .. Docstring generated from Julia source

   ::

              r2r!(A, kind [, dims])

   Same as :func:`r2r`, but operates in-place on ``A``, which must be
   an array of real or complex floating-point numbers.

.. function:: plan_r2r(A, kind [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_r2r(A, kind [, dims [, flags [, timelimit]]])

   Pre-plan an optimized r2r transform, similar to :func:`Base.plan_fft`
   except that the transforms (and the first three arguments)
   correspond to :func:`r2r` and :func:`r2r!`, respectively.

.. function:: plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

   .. Docstring generated from Julia source

   ::

              plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

   Similar to :func:`Base.plan_fft`, but corresponds to :func:`r2r!`.

.. currentmodule:: Base

Numerical Integration
---------------------

Although several external packages are available for numeric integration
and solution of ordinary differential equations, we also provide
some built-in integration support in Julia.

.. function:: quadgk(f, a,b,c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   .. Docstring generated from Julia source

   .. code-block:: julia

       quadgk(f, a,b,c...; reltol=sqrt(eps), abstol=0, maxevals=10^7, order=7, norm=vecnorm)

   Numerically integrate the function ``f(x)`` from ``a`` to ``b``\ , and optionally over additional intervals ``b`` to ``c`` and so on. Keyword options include a relative error tolerance ``reltol`` (defaults to ``sqrt(eps)`` in the precision of the endpoints), an absolute error tolerance ``abstol`` (defaults to 0), a maximum number of function evaluations ``maxevals`` (defaults to ``10^7``\ ), and the ``order`` of the integration rule (defaults to 7).

   Returns a pair ``(I,E)`` of the estimated integral ``I`` and an estimated upper bound on the absolute error ``E``\ . If ``maxevals`` is not exceeded then ``E <= max(abstol, reltol*norm(I))`` will hold. (Note that it is useful to specify a positive ``abstol`` in cases where ``norm(I)`` may be zero.)

   The endpoints ``a`` etcetera can also be complex (in which case the integral is performed over straight-line segments in the complex plane). If the endpoints are ``BigFloat``\ , then the integration will be performed in ``BigFloat`` precision as well (note: it is advisable to increase the integration ``order`` in rough proportion to the precision, for smooth integrands). More generally, the precision is set by the precision of the integration endpoints (promoted to floating-point types).

   The integrand ``f(x)`` can return any numeric scalar, vector, or matrix type, or in fact any type supporting ``+``\ , ``-``\ , multiplication by real values, and a ``norm`` (i.e., any normed vector space). Alternatively, a different norm can be specified by passing a ``norm``\ -like function as the ``norm`` keyword argument (which defaults to ``vecnorm``\ ).

   [Only one-dimensional integrals are provided by this function. For multi-dimensional integration (cubature), there are many different algorithms (often much better than simple nested 1d integrals) and the optimal choice tends to be very problem-dependent. See the Julia external-package listing for available algorithms for multidimensional integration or other specialized tasks (such as integrals of highly oscillatory or singular functions).]

   The algorithm is an adaptive Gauss-Kronrod integration technique: the integral in each interval is estimated using a Kronrod rule (``2*order+1`` points) and the error is estimated using an embedded Gauss rule (``order`` points). The interval with the largest error is then subdivided into two intervals and the process is repeated until the desired error tolerance is achieved.

   These quadrature rules work best for smooth functions within each interval, so if your function has a known discontinuity or other singularity, it is best to subdivide your interval to put the singularity at an endpoint. For example, if ``f`` has a discontinuity at ``x=0.7`` and you want to integrate from 0 to 1, you should use ``quadgk(f, 0,0.7,1)`` to subdivide the interval at the point of discontinuity. The integrand is never evaluated exactly at the endpoints of the intervals, so it is possible to integrate functions that diverge at the endpoints as long as the singularity is integrable (for example, a ``log(x)`` or ``1/sqrt(x)`` singularity).

   For real-valued endpoints, the starting and/or ending points may be infinite. (A coordinate transformation is performed internally to map the infinite interval to a finite one.)


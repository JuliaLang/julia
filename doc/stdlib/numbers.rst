.. currentmodule:: Base

*********
 Numbers
*********

Standard Numeric Types
----------------------

``Bool`` ``Int8`` ``UInt8`` ``Int16`` ``UInt16`` ``Int32`` ``UInt32`` ``Int64`` ``UInt64`` ``Int128`` ``UInt128`` ``Float16`` ``Float32`` ``Float64`` ``Complex64`` ``Complex128``

Data Formats
------------

.. function:: bin(n, [pad])

   Convert an integer to a binary string, optionally specifying a number of digits to pad to.

.. function:: hex(n, [pad])

   Convert an integer to a hexadecimal string, optionally specifying a number of digits to pad to.

.. function:: dec(n, [pad])

   Convert an integer to a decimal string, optionally specifying a number of digits to pad to.

.. function:: oct(n, [pad])

   Convert an integer to an octal string, optionally specifying a number of digits to pad to.

.. function:: base(base, n, [pad])

   Convert an integer to a string in the given base, optionally specifying a number of digits to pad to. The base can be specified as either an integer, or as a ``UInt8`` array of character values to use as digit symbols.

.. function:: digits(n, [base], [pad])

   Returns an array of the digits of ``n`` in the given base, optionally padded with
   zeros to a specified size. More significant digits are at higher indexes, such
   that ``n == sum([digits[k]*base^(k-1) for k=1:length(digits)])``.

.. function:: digits!(array, n, [base])

    Fills an array of the digits of ``n`` in the given base. More significant digits are at higher indexes.
    If the array length is insufficient, the least significant digits are filled up to the array length.
    If the array length is excessive, the excess portion is filled with zeros.

.. function:: bits(n)

   A string giving the literal bit representation of a number.

.. function:: parseint([type], str, [base])

   Parse a string as an integer in the given base (default 10), yielding a number of the specified type (default ``Int``).

.. function:: parsefloat([type], str)

   Parse a string as a decimal floating point number, yielding a number of the specified type.

.. function:: big(x)

   Convert a number to a maximum precision representation (typically ``BigInt`` or ``BigFloat``). See ``BigFloat`` for information about some pitfalls with floating-point numbers.

.. function:: bool(x)

   Convert a number or numeric array to boolean

.. function:: int(x)

   Convert a number or array to the default integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an integer.

.. function:: uint(x)

   Convert a number or array to the default unsigned integer type on your platform. Alternatively, ``x`` can be a string, which is parsed as an unsigned integer.

.. function:: integer(x)

   Convert a number or array to integer type. If ``x`` is already of integer type it is unchanged, otherwise it converts it to the default integer type on your platform.

.. function:: signed(x)

   Convert a number to a signed integer. If the argument is unsigned, it is reinterpreted as signed without checking for overflow.

.. function:: unsigned(x) -> Unsigned

   Convert a number to an unsigned integer. If the argument is signed, it is reinterpreted as unsigned without checking for negative values.

.. function:: int8(x)

   Convert a number or array to ``Int8`` data type

.. function:: int16(x)

   Convert a number or array to ``Int16`` data type

.. function:: int32(x)

   Convert a number or array to ``Int32`` data type

.. function:: int64(x)

   Convert a number or array to ``Int64`` data type

.. function:: int128(x)

   Convert a number or array to ``Int128`` data type

.. function:: uint8(x)

   Convert a number or array to ``UInt8`` data type

.. function:: uint16(x)

   Convert a number or array to ``UInt16`` data type

.. function:: uint32(x)

   Convert a number or array to ``UInt32`` data type

.. function:: uint64(x)

   Convert a number or array to ``UInt64`` data type

.. function:: uint128(x)

   Convert a number or array to ``UInt128`` data type

.. function:: float16(x)

   Convert a number or array to ``Float16`` data type

.. function:: float32(x)

   Convert a number or array to ``Float32`` data type

.. function:: float64(x)

   Convert a number or array to ``Float64`` data type

.. function:: float32_isvalid(x, out::Vector{Float32}) -> Bool

   Convert a number or array to ``Float32`` data type, returning true if successful. The result of the conversion is stored in ``out[1]``.

.. function:: float64_isvalid(x, out::Vector{Float64}) -> Bool

   Convert a number or array to ``Float64`` data type, returning true if successful. The result of the conversion is stored in ``out[1]``.

.. function:: float(x)

   Convert a number, array, or string to a ``FloatingPoint`` data type. For numeric data, the smallest suitable ``FloatingPoint`` type is used. Converts strings to ``Float64``.

   This function is not recommended for arrays. It is better to use a more specific function such as ``float32`` or ``float64``.

.. function:: significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary representation, of a floating-point number or array.

   .. doctest::

      julia> significand(15.2)/15.2
      0.125

      julia> significand(15.2)*8
      15.2

.. function:: exponent(x) -> Int

   Get the exponent of a normalized floating-point number.

.. function:: complex64(r, [i])

   Convert to ``r + i*im`` represented as a ``Complex64`` data type. ``i`` defaults to zero.

.. function:: complex128(r, [i])

   Convert to ``r + i*im`` represented as a ``Complex128`` data type. ``i`` defaults to zero.

.. function:: complex(r, [i])

   Convert real numbers or arrays to complex. ``i`` defaults to zero.

.. function:: char(x)

   Convert a number or array to ``Char`` data type

.. function:: bswap(n)

   Byte-swap an integer

.. function:: num2hex(f)

   Get a hexadecimal string of the binary representation of a floating point number

.. function:: hex2num(str)

   Convert a hexadecimal string to the floating point number it represents

.. function:: hex2bytes(s::ASCIIString)

   Convert an arbitrarily long hexadecimal string to its binary representation. Returns an Array{UInt8, 1}, i.e. an array of bytes.

.. function:: bytes2hex(bin_arr::Array{UInt8, 1})

   Convert an array of bytes to its hexadecimal representation. All characters are in lower-case. Returns an ASCIIString.


General Number Functions and Constants
--------------------------------------

.. function:: one(x)

   Get the multiplicative identity element for the type of x (x can also specify the type itself). For matrices, returns an identity matrix of the appropriate size and type.

.. function:: zero(x)

   Get the additive identity element for the type of x (x can also specify the type itself).

.. data:: pi
          π

   The constant pi

.. data:: im

   The imaginary unit

.. data:: e

   The constant e

.. data:: catalan

   Catalan's constant

.. data:: γ

   Euler's constant

.. data:: φ

   The golden ratio

.. data:: Inf

   Positive infinity of type Float64

.. data:: Inf32

   Positive infinity of type Float32

.. data:: Inf16

   Positive infinity of type Float16

.. data:: NaN

   A not-a-number value of type Float64

.. data:: NaN32

   A not-a-number value of type Float32

.. data:: NaN16

   A not-a-number value of type Float16

.. function:: issubnormal(f) -> Bool

   Test whether a floating point number is subnormal

.. function:: isfinite(f) -> Bool

   Test whether a number is finite

.. function:: isinf(f) -> Bool

   Test whether a number is infinite

.. function:: isnan(f) -> Bool

   Test whether a floating point number is not a number (NaN)

.. function:: inf(f)

   Returns positive infinity of the floating point type ``f`` or of the same floating point type as ``f``

.. function:: nan(f)

   Returns NaN (not-a-number) of the floating point type ``f`` or of the same floating point type as ``f``

.. function:: nextfloat(f)

   Get the next floating point number in lexicographic order

.. function:: prevfloat(f) -> FloatingPoint

   Get the previous floating point number in lexicographic order

.. function:: isinteger(x) -> Bool

   Test whether ``x`` or all its elements are numerically equal to some integer

.. function:: isreal(x) -> Bool

   Test whether ``x`` or all its elements are numerically equal to some real number

.. function:: Float32(x [, mode::RoundingMode])

   Create a Float32 from ``x``. If ``x`` is not exactly representable then
   ``mode`` determines how ``x`` is rounded.

   .. doctest::

      julia> Float32(1/3, RoundDown)
      0.3333333f0

      julia> Float32(1/3, RoundUp)
      0.33333334f0

   See ``get_rounding`` for available rounding modes.

.. function:: Float64(x [, mode::RoundingMode])

   Create a Float64 from ``x``. If ``x`` is not exactly representable then
   ``mode`` determines how ``x`` is rounded.

   .. doctest::

      julia> Float64(pi, RoundDown)
      3.141592653589793

      julia> Float64(pi, RoundUp)
      3.1415926535897936

   See ``get_rounding`` for available rounding modes.

.. function:: BigInt(x)

   Create an arbitrary precision integer. ``x`` may be an ``Int`` (or anything that can be converted to an ``Int``) or a ``AbstractString``.
   The usual mathematical operators are defined for this type, and results are promoted to a ``BigInt``.

.. function:: BigFloat(x)

   Create an arbitrary precision floating point number. ``x`` may be
   an ``Integer``, a ``Float64``, a ``AbstractString`` or a ``BigInt``. The
   usual mathematical operators are defined for this type, and results
   are promoted to a ``BigFloat``. Note that because floating-point
   numbers are not exactly-representable in decimal notation,
   ``BigFloat(2.1)`` may not yield what you expect. You may prefer to
   initialize constants using strings, e.g., ``BigFloat("2.1")``.

.. function:: get_rounding(T)

   Get the current floating point rounding mode for type ``T``. Valid modes
   are ``RoundNearest``, ``RoundToZero``, ``RoundUp``, ``RoundDown``, and
   ``RoundFromZero`` (``BigFloat`` only).

.. function:: set_rounding(T, mode)

   Set the rounding mode of floating point type ``T``. Note that this may
   affect other types, for instance changing the rounding mode of ``Float64``
   will change the rounding mode of ``Float32``. See ``get_rounding`` for available modes

.. function:: with_rounding(f::Function, T, mode)

   Change the rounding mode of floating point type ``T`` for the duration of ``f``. It is logically equivalent to::

       old = get_rounding(T)
       set_rounding(T, mode)
       f()
       set_rounding(T, old)

   See ``get_rounding`` for available rounding modes.

Integers
~~~~~~~~

.. function:: count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of ``x``.

   .. doctest::

      julia> count_ones(7)
      3

.. function:: count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of ``x``.

   .. doctest::

      julia> count_zeros(int32(2 ^ 16 - 1))
      16

.. function:: leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of ``x``.

   .. doctest::

      julia> leading_zeros(int32(1))
      31

.. function:: leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of ``x``.

   .. doctest::

      julia> leading_ones(uint32(2 ^ 32 - 2))
      31

.. function:: trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of ``x``.

   .. doctest::

      julia> trailing_zeros(2)
      1

.. function:: trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of ``x``.

   .. doctest::

      julia> trailing_ones(3)
      2

.. function:: isprime(x::Integer) -> Bool

   Returns ``true`` if ``x`` is prime, and ``false`` otherwise.

   .. doctest::

   	julia> isprime(3)
   	true

.. function:: primes(n)

   Returns a collection of the prime numbers <= ``n``.

.. function:: isodd(x::Integer) -> Bool

   Returns ``true`` if ``x`` is odd (that is, not divisible by 2), and ``false`` otherwise.

   .. doctest::

   	julia> isodd(9)
   	true

   	julia> isodd(10)
   	false

.. function:: iseven(x::Integer) -> Bool

   Returns ``true`` is ``x`` is even (that is, divisible by 2), and ``false`` otherwise.

   .. doctest::

   	julia> iseven(9)
   	false

   	julia> iseven(10)
   	true

BigFloats
---------
The `BigFloat` type implements arbitrary-precision floating-point arithmetic using the `GNU MPFR library <http://www.mpfr.org/>`_.

.. function:: precision(num::FloatingPoint)

   Get the precision of a floating point number, as defined by the effective number of bits in the mantissa.

.. function:: get_bigfloat_precision()

   Get the precision (in bits) currently used for BigFloat arithmetic.

.. function:: set_bigfloat_precision(x::Int64)

   Set the precision (in bits) to be used to BigFloat arithmetic.

.. function:: with_bigfloat_precision(f::Function,precision::Integer)

   Change the BigFloat arithmetic precision (in bits) for the duration of ``f``. It is logically equivalent to::

       old = get_bigfloat_precision()
       set_bigfloat_precision(precision)
       f()
       set_bigfloat_precision(old)

Random Numbers
--------------

Random number generation in Julia uses the `Mersenne Twister library <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT>`_ via ``MersenneTwister`` objects.
Julia has a global RNG, which is used by default. Other RNG types can be plugged in by inheriting the ``AbstractRNG`` type;
they can then be used to have multiple streams of random numbers.
Besides ``MersenneTwister``, Julia also provides the ``RandomDevice`` RNG type, which is a wrapper over the OS provided entropy.

Most functions related to random generation accept an optional ``AbstractRNG`` as the first argument, ``rng`` , which defaults to the global one if not provided.
Morever, some of them accept optionally dimension specifications ``dims...`` (which can be given as a tuple) to generate arrays of random values.

A ``MersenneTwister`` or ``RandomDevice`` RNG can generate random numbers of the following types:
``Float16``, ``Float32``, ``Float64``, ``Bool``, ``Int8``, ``UInt8``, ``Int16``, ``UInt16``,
``Int32``, ``UInt32``, ``Int64``, ``UInt64``, ``Int128``, ``UInt128``, ``BigInt``
(or complex numbers of those types). Random floating point numbers are generated uniformly in [0,1).
As ``BigInt`` represents unbounded integers, the interval must be specified (e.g. ``rand(big(1:6))``).

.. function:: srand([rng], [seed])

   Reseed the random number generator. If a ``seed`` is provided, the RNG will give a reproducible sequence of numbers, otherwise Julia will get entropy from the system.
   For ``MersenneTwister``, the ``seed`` may be a non-negative integer, a vector of ``UInt32`` integers or a filename, in which case the seed is read from a file.
   ``RandomDevice`` does not support seeding.

.. function:: MersenneTwister([seed])

   Create a ``MersenneTwister`` RNG object. Different RNG objects can have their own seeds, which may be useful for generating different streams of random numbers.

.. function:: RandomDevice()

   Create a ``RandomDevice`` RNG object. Two such objects will always generate different streams of random numbers.

.. function:: rand([rng], [S], [dims...])

   Pick a random element or array of random elements from the set of values specified by ``S``; ``S`` can be

   * an indexable collection (for example ``1:n`` or ``['x','y','z']``), or

   * a type: the set of values to pick from is then equivalent to ``typemin(S):typemax(S)`` for integers (this is not applicable to ``BigInt``), and to [0,1) for floating point numbers;

   ``S`` defaults to ``Float64``.

.. function:: rand!([rng], A ,[coll])

   Populate the array A with random values. If the indexable collection ``coll`` is specified, the values are picked randomly from ``coll``. This is equivalent to ``copy!(A, rand(rng, coll, size(A)))`` or ``copy!(A, rand(rng, eltype(A), size(A)))`` but without allocating a new array.

.. function:: randbool([rng], [dims...])

   Generate a random boolean value. Optionally, generate a ``BitArray`` of random boolean values.

.. function:: randn([rng], [dims...])

   Generate a normally-distributed random number with mean 0 and standard deviation 1. Optionally generate an array of normally-distributed random numbers.

.. function:: randn!([rng], A::Array{Float64,N})

   Fill the array A with normally-distributed (mean 0, standard deviation 1) random numbers. Also see the rand function.

.. function:: randexp([rng], [dims...])

   Generate a random number according to the exponential distribution with scale 1. Optionally generate an array of such random numbers.

.. function:: randexp!([rng], A::Array{Float64,N})

   Fill the array A with random numbers following the exponential distribution (with scale 1).


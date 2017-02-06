# Mathematical Operations and Elementary Functions

Julia provides a complete collection of basic arithmetic and bitwise operators across all of its
numeric primitive types, as well as providing portable, efficient implementations of a comprehensive
collection of standard mathematical functions.

## Arithmetic Operators

The following [arithmetic operators](https://en.wikipedia.org/wiki/Arithmetic#Arithmetic_operations)
are supported on all primitive numeric types:

| Expression | Name           | Description                            |
|:---------- |:-------------- |:-------------------------------------- |
| `+x`       | unary plus     | the identity operation                 |
| `-x`       | unary minus    | maps values to their additive inverses |
| `x + y`    | binary plus    | performs addition                      |
| `x - y`    | binary minus   | performs subtraction                   |
| `x * y`    | times          | performs multiplication                |
| `x / y`    | divide         | performs division                      |
| `x \ y`    | inverse divide | equivalent to `y / x`                  |
| `x ^ y`    | power          | raises `x` to the `y`th power          |
| `x % y`    | remainder      | equivalent to `rem(x,y)`               |

as well as the negation on `Bool` types:

| Expression | Name     | Description                              |
|:---------- |:-------- |:---------------------------------------- |
| `!x`       | negation | changes `true` to `false` and vice versa |

Julia's promotion system makes arithmetic operations on mixtures of argument types "just work"
naturally and automatically. See [Conversion and Promotion](@ref conversion-and-promotion) for details of the promotion
system.

Here are some simple examples using arithmetic operators:

```jldoctest
julia> 1 + 2 + 3
6

julia> 1 - 2
-1

julia> 3*2/12
0.5
```

(By convention, we tend to space operators more tightly if they get applied before other nearby
operators. For instance, we would generally write `-x + 2` to reflect that first `x` gets negated,
and then `2` is added to that result.)

## Bitwise Operators

The following [bitwise operators](https://en.wikipedia.org/wiki/Bitwise_operation#Bitwise_operators)
are supported on all primitive integer types:

| Expression | Name                                                                     |
|:---------- |:------------------------------------------------------------------------ |
| `~x`       | bitwise not                                                              |
| `x & y`    | bitwise and                                                              |
| `x \| y`   | bitwise or                                                               |
| `x ⊻ y`    | bitwise xor (exclusive or)                                               |
| `x >>> y`  | [logical shift](https://en.wikipedia.org/wiki/Logical_shift) right       |
| `x >> y`   | [arithmetic shift](https://en.wikipedia.org/wiki/Arithmetic_shift) right |
| `x << y`   | logical/arithmetic shift left                                            |

Here are some examples with bitwise operators:

```jldoctest
julia> ~123
-124

julia> 123 & 234
106

julia> 123 | 234
251

julia> 123 ⊻ 234
145

julia> xor(123, 234)
145

julia> ~UInt32(123)
0xffffff84

julia> ~UInt8(123)
0x84
```

## Updating operators

Every binary arithmetic and bitwise operator also has an updating version that assigns the result
of the operation back into its left operand. The updating version of the binary operator is formed
by placing a `=` immediately after the operator. For example, writing `x += 3` is equivalent to
writing `x = x + 3`:

```jldoctest
julia> x = 1
1

julia> x += 3
4

julia> x
4
```

The updating versions of all the binary arithmetic and bitwise operators are:

```
+=  -=  *=  /=  \=  ÷=  %=  ^=  &=  |=  ⊻=  >>>=  >>=  <<=
```

!!! note
    An updating operator rebinds the variable on the left-hand side. As a result, the type of the
    variable may change.

    ```jldoctest
    julia> x = 0x01; typeof(x)
    UInt8

    julia> x *= 2 # Same as x = x * 2
    2

    julia> typeof(x)
    Int64
    ```

## [Vectorized "dot" operators](@id man-dot-operators)

For *every* binary operation like `^`, there is a corresponding
"dot" operation `.^` that is *automatically* defined
to perform `^` element-by-element on arrays. For example,
`[1,2,3] ^ 3` is not defined, since there is no standard
mathematical meaning to "cubing" an array, but `[1,2,3] .^ 3`
is defined as computing the elementwise
(or "vectorized") result `[1^3, 2^3, 3^3]`.  Similarly for unary
operators like `!` or `√`, there is a corresponding `.√` that
applies the operator elementwise.

```jldoctest
julia> [1,2,3] .^ 3
3-element Array{Int64,1}:
  1
  8
 27
```

More specifically, `a .^ b` is parsed as the ["dot" call](@ref man-vectorized)
`(^).(a,b)`, which performs a [broadcast](@ref Broadcasting) operation:
it can combine arrays and scalars, arrays of the same size (performing
the operation elementwise), and even arrays of different shapes (e.g.
combining row and column vectors to produce a matrix). Moreover, like
all vectorized "dot calls," these "dot operators" are
*fusing*. For example, if you compute `2 .* A.^2 .+ sin.(A)` (or
equivalently `@. 2A^2 + sin(A)`, using the [`@.`](@ref @__dot__) macro) for
an array `A`, it performs a *single* loop over `A`, computing `2a^2 + sin(a)`
for each element of `A`. In particular, nested dot calls like `f.(g.(x))`
are fused, and "adjacent" binary operators like `x .+ 3 .* x.^2` are
equivalent to nested dot calls `(+).(x, (*).(3, (^).(x, 2)))`.

Furthermore, "dotted" updating operators like `a .+= b` (or `@. a += b`) are parsed
as `a .= a .+ b`, where `.=` is a fused *in-place* assignment operation
(see the [dot syntax documentation](@ref man-vectorized)).

Note the dot syntax is also applicable to user-defined operators.
For example, if you define `⊗(A,B) = kron(A,B)` to give a convenient
infix syntax `A ⊗ B` for Kronecker products ([`kron`](@ref)), then
`[A,B] .⊗ [C,D]` will compute `[A⊗C, B⊗D]` with no additional coding.

## Numeric Comparisons

Standard comparison operations are defined for all the primitive numeric types:

| Operator                     | Name                     |
|:---------------------------- |:------------------------ |
| [`==`](@ref)                 | equality                 |
| [`!=`](@ref), [`≠`](@ref !=) | inequality               |
| [`<`](@ref)                  | less than                |
| [`<=`](@ref), [`≤`](@ref <=) | less than or equal to    |
| [`>`](@ref)                  | greater than             |
| [`>=`](@ref), [`≥`](@ref >=) | greater than or equal to |

Here are some simple examples:

```jldoctest
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
```

Integers are compared in the standard manner -- by comparison of bits. Floating-point numbers
are compared according to the [IEEE 754 standard](https://en.wikipedia.org/wiki/IEEE_754-2008):

  * Finite numbers are ordered in the usual manner.
  * Positive zero is equal but not greater than negative zero.
  * `Inf` is equal to itself and greater than everything else except `NaN`.
  * `-Inf` is equal to itself and less then everything else except `NaN`.
  * `NaN` is not equal to, not less than, and not greater than anything, including itself.

The last point is potentially surprising and thus worth noting:

```jldoctest
julia> NaN == NaN
false

julia> NaN != NaN
true

julia> NaN < NaN
false

julia> NaN > NaN
false
```

and can cause especial headaches with [Arrays](@ref):

```jldoctest
julia> [1 NaN] == [1 NaN]
false
```

Julia provides additional functions to test numbers for special values, which can be useful in
situations like hash key comparisons:

| Function                | Tests if                  |
|:----------------------- |:------------------------- |
| [`isequal(x, y)`](@ref) | `x` and `y` are identical |
| [`isfinite(x)`](@ref)   | `x` is a finite number    |
| [`isinf(x)`](@ref)      | `x` is infinite           |
| [`isnan(x)`](@ref)      | `x` is not a number       |

[`isequal()`](@ref) considers `NaN`s equal to each other:

```jldoctest
julia> isequal(NaN, NaN)
true

julia> isequal([1 NaN], [1 NaN])
true

julia> isequal(NaN, NaN32)
true
```

`isequal()` can also be used to distinguish signed zeros:

```jldoctest
julia> -0.0 == 0.0
true

julia> isequal(-0.0, 0.0)
false
```

Mixed-type comparisons between signed integers, unsigned integers, and floats can be tricky. A
great deal of care has been taken to ensure that Julia does them correctly.

For other types, `isequal()` defaults to calling [`==()`](@ref), so if you want to define
equality for your own types then you only need to add a [`==()`](@ref) method.  If you define
your own equality function, you should probably define a corresponding [`hash()`](@ref) method
to ensure that `isequal(x,y)` implies `hash(x) == hash(y)`.

### Chaining comparisons

Unlike most languages, with the [notable exception of Python](https://en.wikipedia.org/wiki/Python_syntax_and_semantics#Comparison_operators),
comparisons can be arbitrarily chained:

```jldoctest
julia> 1 < 2 <= 2 < 3 == 3 > 2 >= 1 == 1 < 3 != 5
true
```

Chaining comparisons is often quite convenient in numerical code. Chained comparisons use the
`&&` operator for scalar comparisons, and the [`&`](@ref) operator for elementwise comparisons,
which allows them to work on arrays. For example, `0 .< A .< 1` gives a boolean array whose entries
are true where the corresponding elements of `A` are between 0 and 1.

Note the evaluation behavior of chained comparisons:

```jldoctest
julia> v(x) = (println(x); x)
v (generic function with 1 method)

julia> v(1) < v(2) <= v(3)
2
1
3
true

julia> v(1) > v(2) <= v(3)
2
1
false
```

The middle expression is only evaluated once, rather than twice as it would be if the expression
were written as `v(1) < v(2) && v(2) <= v(3)`. However, the order of evaluations in a chained
comparison is undefined. It is strongly recommended not to use expressions with side effects (such
as printing) in chained comparisons. If side effects are required, the short-circuit `&&` operator
should be used explicitly (see [Short-Circuit Evaluation](@ref)).

### Operator Precedence

Julia applies the following order of operations, from highest precedence to lowest:

| Category       | Operators                                                                                         |
|:-------------- |:------------------------------------------------------------------------------------------------- |
| Syntax         | `.` followed by `::`                                                                              |
| Exponentiation | `^`                                                                                               |
| Fractions      | `//`                                                                                              |
| Multiplication | `* / % & \`                                                                                       |
| Bitshifts      | `<< >> >>>`                                                                                       |
| Addition       | `+ - \| ⊻`                                                                                        |
| Syntax         | `: ..` followed by `\|>`                                                                          |
| Comparisons    | `> < >= <= == === != !== <:`                                                                      |
| Control flow   | `&&` followed by `\|\|` followed by `?`                                                           |
| Assignments    | `= += -= *= /= //= \= ^= ÷= %= \|= &= ⊻= <<= >>= >>>=`                                            |

### Elementary Functions

Julia provides a comprehensive collection of mathematical functions and operators. These mathematical
operations are defined over as broad a class of numerical values as permit sensible definitions,
including integers, floating-point numbers, rationals, and complexes, wherever such definitions
make sense.

Moreover, these functions (like any Julia function) can be applied in "vectorized" fashion to
arrays and other collections with the [dot syntax](@ref man-vectorized) `f.(A)`,
e.g. `sin.(A)` will compute the elementwise sine of each element of an array `A`.

## Numerical Conversions

Julia supports three forms of numerical conversion, which differ in their handling of inexact
conversions.

  * The notation `T(x)` or `convert(T,x)` converts `x` to a value of type `T`.

      * If `T` is a floating-point type, the result is the nearest representable value, which could be
        positive or negative infinity.
      * If `T` is an integer type, an `InexactError` is raised if `x` is not representable by `T`.
  * `x % T` converts an integer `x` to a value of integer type `T` congruent to `x` modulo `2^n`,
    where `n` is the number of bits in `T`. In other words, the binary representation is truncated
    to fit.
  * The [Rounding functions](@ref) take a type `T` as an optional argument. For example, `round(Int,x)`
    is a shorthand for `Int(round(x))`.

The following examples show the different forms.

```jldoctest
julia> Int8(127)
127

julia> Int8(128)
ERROR: InexactError()
Stacktrace:
 [1] Int8(::Int64) at ./sysimg.jl:24

julia> Int8(127.0)
127

julia> Int8(3.14)
ERROR: InexactError()
Stacktrace:
 [1] convert(::Type{Int8}, ::Float64) at ./float.jl:654
 [2] Int8(::Float64) at ./sysimg.jl:24

julia> Int8(128.0)
ERROR: InexactError()
Stacktrace:
 [1] convert(::Type{Int8}, ::Float64) at ./float.jl:654
 [2] Int8(::Float64) at ./sysimg.jl:24

julia> 127 % Int8
127

julia> 128 % Int8
-128

julia> round(Int8,127.4)
127

julia> round(Int8,127.6)
ERROR: InexactError()
Stacktrace:
 [1] trunc(::Type{Int8}, ::Float64) at ./float.jl:647
 [2] round(::Type{Int8}, ::Float64) at ./float.jl:333
```

See [Conversion and Promotion](@ref conversion-and-promotion) for how to define your own conversions and promotions.

### Rounding functions

| Function              | Description                      | Return type |
|:--------------------- |:-------------------------------- |:----------- |
| [`round(x)`](@ref)    | round `x` to the nearest integer | `typeof(x)` |
| [`round(T, x)`](@ref) | round `x` to the nearest integer | `T`         |
| [`floor(x)`](@ref)    | round `x` towards `-Inf`         | `typeof(x)` |
| [`floor(T, x)`](@ref) | round `x` towards `-Inf`         | `T`         |
| [`ceil(x)`](@ref)     | round `x` towards `+Inf`         | `typeof(x)` |
| [`ceil(T, x)`](@ref)  | round `x` towards `+Inf`         | `T`         |
| [`trunc(x)`](@ref)    | round `x` towards zero           | `typeof(x)` |
| [`trunc(T, x)`](@ref) | round `x` towards zero           | `T`         |

### Division functions

| Function              | Description                                                                                               |
|:--------------------- |:--------------------------------------------------------------------------------------------------------- |
| [`div(x,y)`](@ref)    | truncated division; quotient rounded towards zero                                                         |
| [`fld(x,y)`](@ref)    | floored division; quotient rounded towards `-Inf`                                                         |
| [`cld(x,y)`](@ref)    | ceiling division; quotient rounded towards `+Inf`                                                         |
| [`rem(x,y)`](@ref)    | remainder; satisfies `x == div(x,y)*y + rem(x,y)`; sign matches `x`                                       |
| [`mod(x,y)`](@ref)    | modulus; satisfies `x == fld(x,y)*y + mod(x,y)`; sign matches `y`                                         |
| [`mod1(x,y)`](@ref)   | `mod()` with offset 1; returns `r∈(0,y]` for `y>0` or `r∈[y,0)` for `y<0`, where `mod(r, y) == mod(x, y)` |
| [`mod2pi(x)`](@ref)   | modulus with respect to 2pi;  `0 <= mod2pi(x)    < 2pi`                                                   |
| [`divrem(x,y)`](@ref) | returns `(div(x,y),rem(x,y))`                                                                             |
| [`fldmod(x,y)`](@ref) | returns `(fld(x,y),mod(x,y))`                                                                             |
| [`gcd(x,y...)`](@ref) | greatest positive common divisor of `x`, `y`,...                                                          |
| [`lcm(x,y...)`](@ref) | least positive common multiple of `x`, `y`,...                                                            |

### Sign and absolute value functions

| Function                | Description                                                |
|:----------------------- |:---------------------------------------------------------- |
| [`abs(x)`](@ref)        | a positive value with the magnitude of `x`                 |
| [`abs2(x)`](@ref)       | the squared magnitude of `x`                               |
| [`sign(x)`](@ref)       | indicates the sign of `x`, returning -1, 0, or +1          |
| [`signbit(x)`](@ref)    | indicates whether the sign bit is on (true) or off (false) |
| [`copysign(x,y)`](@ref) | a value with the magnitude of `x` and the sign of `y`      |
| [`flipsign(x,y)`](@ref) | a value with the magnitude of `x` and the sign of `x*y`    |

### Powers, logs and roots

| Function                 | Description                                                                |
|:------------------------ |:-------------------------------------------------------------------------- |
| [`sqrt(x)`](@ref), `√x`  | square root of `x`                                                         |
| [`cbrt(x)`](@ref), `∛x`  | cube root of `x`                                                           |
| [`hypot(x,y)`](@ref)     | hypotenuse of right-angled triangle with other sides of length `x` and `y` |
| [`exp(x)`](@ref)         | natural exponential function at `x`                                        |
| [`expm1(x)`](@ref)       | accurate `exp(x)-1` for `x` near zero                                      |
| [`ldexp(x,n)`](@ref)     | `x*2^n` computed efficiently for integer values of `n`                     |
| [`log(x)`](@ref)         | natural logarithm of `x`                                                   |
| [`log(b,x)`](@ref)       | base `b` logarithm of `x`                                                  |
| [`log2(x)`](@ref)        | base 2 logarithm of `x`                                                    |
| [`log10(x)`](@ref)       | base 10 logarithm of `x`                                                   |
| [`log1p(x)`](@ref)       | accurate `log(1+x)` for `x` near zero                                      |
| [`exponent(x)`](@ref)    | binary exponent of `x`                                                     |
| [`significand(x)`](@ref) | binary significand (a.k.a. mantissa) of a floating-point number `x`        |

For an overview of why functions like [`hypot()`](@ref), [`expm1()`](@ref), and [`log1p()`](@ref)
are necessary and useful, see John D. Cook's excellent pair of blog posts on the subject: [expm1, log1p, erfc](http://www.johndcook.com/blog/2010/06/07/math-library-functions-that-seem-unnecessary/),
and [hypot](http://www.johndcook.com/blog/2010/06/02/whats-so-hard-about-finding-a-hypotenuse/).

### Trigonometric and hyperbolic functions

All the standard trigonometric and hyperbolic functions are also defined:

```
sin    cos    tan    cot    sec    csc
sinh   cosh   tanh   coth   sech   csch
asin   acos   atan   acot   asec   acsc
asinh  acosh  atanh  acoth  asech  acsch
sinc   cosc   atan2
```

These are all single-argument functions, with the exception of [atan2](https://en.wikipedia.org/wiki/Atan2),
which gives the angle in [radians](https://en.wikipedia.org/wiki/Radian) between the *x*-axis
and the point specified by its arguments, interpreted as *x* and *y* coordinates.

Additionally, [`sinpi(x)`](@ref) and [`cospi(x)`](@ref) are provided for more accurate computations
of [`sin(pi*x)`](@ref) and [`cos(pi*x)`](@ref) respectively.

In order to compute trigonometric functions with degrees instead of radians, suffix the function
with `d`. For example, [`sind(x)`](@ref) computes the sine of `x` where `x` is specified in degrees.
The complete list of trigonometric functions with degree variants is:

```
sind   cosd   tand   cotd   secd   cscd
asind  acosd  atand  acotd  asecd  acscd
```

### Special functions

| Function                                                      | Description                                                                                                                                                     |
|:------------------------------------------------------------- |:--------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`erf(x)`](@ref)                                              | [error function](https://en.wikipedia.org/wiki/Error_function) at `x`                                                                                           |
| [`erfc(x)`](@ref)                                             | complementary error function, i.e. the accurate version of `1-erf(x)` for large `x`                                                                             |
| [`erfinv(x)`](@ref)                                           | inverse function to [`erf()`](@ref)                                                                                                                             |
| `erfcinv(x)`                                                  | inverse function to [`erfc()`](@ref)                                                                                                                            |
| [`erfi(x)`](@ref)                                             | imaginary error function defined as `-im * erf(x * im)`, where [`im`](@ref) is the imaginary unit                                                               |
| [`erfcx(x)`](@ref)                                            | scaled complementary error function, i.e. accurate `exp(x^2) * erfc(x)` for large `x`                                                                           |
| [`dawson(x)`](@ref)                                           | scaled imaginary error function, a.k.a. Dawson function, i.e. accurate `exp(-x^2) * erfi(x) * sqrt(pi) / 2` for large `x`                                       |
| [`gamma(x)`](@ref)                                            | [gamma function](https://en.wikipedia.org/wiki/Gamma_function) at `x`                                                                                           |
| [`lgamma(x)`](@ref)                                           | accurate `log(gamma(x))` for large `x`                                                                                                                          |
| [`lfact(x)`](@ref)                                            | accurate `log(factorial(x))` for large `x`; same as `lgamma(x+1)` for `x > 1`, zero otherwise                                                                   |
| [`digamma(x)`](@ref)                                          | [digamma function](https://en.wikipedia.org/wiki/Digamma_function) (i.e. the derivative of [`lgamma()`](@ref)) at `x`                                           |
| [`beta(x,y)`](@ref)                                           | [beta function](https://en.wikipedia.org/wiki/Beta_function) at `x,y`                                                                                           |
| [`lbeta(x,y)`](@ref)                                          | accurate `log(beta(x,y))` for large `x` or `y`                                                                                                                  |
| [`eta(x)`](@ref)                                              | [Dirichlet eta function](https://en.wikipedia.org/wiki/Dirichlet_eta_function) at `x`                                                                           |
| [`zeta(x)`](@ref)                                             | [Riemann zeta function](https://en.wikipedia.org/wiki/Riemann_zeta_function) at `x`                                                                             |
| [`airyai(z)`](@ref)                                           | [Airy Ai function](https://en.wikipedia.org/wiki/Airy_function) at `z`                                                                                          |
| [`airyaiprime(z)`](@ref)                                      | derivative of the Airy Ai function at `z`                                                                                                                       |
| [`airybi(z)`](@ref)                                           | [Airy Bi function](https://en.wikipedia.org/wiki/Airy_function) at `z`                                                                                          |
| [`airybiprime(z)`](@ref)                                      | derivative of the Airy Bi function at `z`                                                                                                                       |
| [`airyaix(z)`](@ref), [`airyaiprimex(z)`](@ref), [`airybix(z)`](@ref), [`airybiprimex(z)`](@ref) | scaled Airy AI function and `k` th derivatives at `z`                                                                                                           |
| [`besselj(nu,z)`](@ref)                                       | [Bessel function](https://en.wikipedia.org/wiki/Bessel_function) of the first kind of order `nu` at `z`                                                         |
| [`besselj0(z)`](@ref)                                         | `besselj(0,z)`                                                                                                                                                  |
| [`besselj1(z)`](@ref)                                         | `besselj(1,z)`                                                                                                                                                  |
| [`besseljx(nu,z)`](@ref)                                      | scaled Bessel function of the first kind of order `nu` at `z`                                                                                                   |
| [`bessely(nu,z)`](@ref)                                       | [Bessel function](https://en.wikipedia.org/wiki/Bessel_function) of the second kind of order `nu` at `z`                                                        |
| [`bessely0(z)`](@ref)                                         | `bessely(0,z)`                                                                                                                                                  |
| [`bessely1(z)`](@ref)                                         | `bessely(1,z)`                                                                                                                                                  |
| [`besselyx(nu,z)`](@ref)                                      | scaled Bessel function of the second kind of order `nu` at `z`                                                                                                  |
| [`besselh(nu,k,z)`](@ref)                                     | [Bessel function](https://en.wikipedia.org/wiki/Bessel_function) of the third kind (a.k.a. Hankel function) of order `nu` at `z`; `k` must be either `1` or `2` |
| [`hankelh1(nu,z)`](@ref)                                      | `besselh(nu, 1, z)`                                                                                                                                             |
| [`hankelh1x(nu,z)`](@ref)                                     | scaled `besselh(nu, 1, z)`                                                                                                                                      |
| [`hankelh2(nu,z)`](@ref)                                      | `besselh(nu, 2, z)`                                                                                                                                             |
| [`hankelh2x(nu,z)`](@ref)                                     | scaled `besselh(nu, 2, z)`                                                                                                                                      |
| [`besseli(nu,z)`](@ref)                                       | modified [Bessel function](https://en.wikipedia.org/wiki/Bessel_function) of the first kind of order `nu` at `z`                                                |
| [`besselix(nu,z)`](@ref)                                      | scaled modified Bessel function of the first kind of order `nu` at `z`                                                                                          |
| [`besselk(nu,z)`](@ref)                                       | modified [Bessel function](https://en.wikipedia.org/wiki/Bessel_function) of the second kind of order `nu` at `z`                                               |
| [`besselkx(nu,z)`](@ref)                                      | scaled modified Bessel function of the second kind of order `nu` at `z`                                                                                         |

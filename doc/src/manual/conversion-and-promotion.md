# [Conversion and Promotion](@id conversion-and-promotion)

Julia has a system for promoting arguments of mathematical operators to a common type, which has
been mentioned in various other sections, including [Integers and Floating-Point Numbers](@ref),
[Mathematical Operations and Elementary Functions](@ref), [Types](@ref man-types), and [Methods](@ref).
In this section, we explain how this promotion system works, as well as how to extend it to new
types and apply it to functions besides built-in mathematical operators. Traditionally, programming
languages fall into two camps with respect to promotion of arithmetic arguments:

  * **Automatic promotion for built-in arithmetic types and operators.** In most languages, built-in
    numeric types, when used as operands to arithmetic operators with infix syntax, such as `+`,
    `-`, `*`, and `/`, are automatically promoted to a common type to produce the expected results.
    C, Java, Perl, and Python, to name a few, all correctly compute the sum `1 + 1.5` as the floating-point
    value `2.5`, even though one of the operands to `+` is an integer. These systems are convenient
    and designed carefully enough that they are generally all-but-invisible to the programmer: hardly
    anyone consciously thinks of this promotion taking place when writing such an expression, but
    compilers and interpreters must perform conversion before addition since integers and floating-point
    values cannot be added as-is. Complex rules for such automatic conversions are thus inevitably
    part of specifications and implementations for such languages.
  * **No automatic promotion.** This camp includes Ada and ML -- very "strict" statically typed languages.
    In these languages, every conversion must be explicitly specified by the programmer. Thus, the
    example expression `1 + 1.5` would be a compilation error in both Ada and ML. Instead one must
    write `real(1) + 1.5`, explicitly converting the integer `1` to a floating-point value before
    performing addition. Explicit conversion everywhere is so inconvenient, however, that even Ada
    has some degree of automatic conversion: integer literals are promoted to the expected integer
    type automatically, and floating-point literals are similarly promoted to appropriate floating-point
    types.

In a sense, Julia falls into the "no automatic promotion" category: mathematical operators are
just functions with special syntax, and the arguments of functions are never automatically converted.
However, one may observe that applying mathematical operations to a wide variety of mixed argument
types is just an extreme case of polymorphic multiple dispatch -- something which Julia's dispatch
and type systems are particularly well-suited to handle. "Automatic" promotion of mathematical
operands simply emerges as a special application: Julia comes with pre-defined catch-all dispatch
rules for mathematical operators, invoked when no specific implementation exists for some combination
of operand types. These catch-all rules first promote all operands to a common type using user-definable
promotion rules, and then invoke a specialized implementation of the operator in question for
the resulting values, now of the same type. User-defined types can easily participate in this
promotion system by defining methods for conversion to and from other types, and providing a handful
of promotion rules defining what types they should promote to when mixed with other types.

## Conversion

Conversion of values to various types is performed by the `convert` function. The `convert` function
generally takes two arguments: the first is a type object while the second is a value to convert
to that type; the returned value is the value converted to an instance of given type. The simplest
way to understand this function is to see it in action:

```julia
julia> x = 12
12

julia> typeof(x)
Int64

julia> convert(UInt8, x)
0x0c

julia> typeof(ans)
UInt8

julia> convert(AbstractFloat, x)
12.0

julia> typeof(ans)
Float64

julia> a = Any[1 2 3; 4 5 6]
2x3 Array{Any,2}:
 1  2  3
 4  5  6

julia> convert(Array{Float64}, a)
2x3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
```

Conversion isn't always possible, in which case a no method error is thrown indicating that `convert`
doesn't know how to perform the requested conversion:

```julia
julia> convert(AbstractFloat, "foo")
ERROR: MethodError: Cannot `convert` an object of type String to an object of type AbstractFloat
This may have arisen from a call to the constructor AbstractFloat(...),
since type constructors fall back to convert methods.
 ...
```

Some languages consider parsing strings as numbers or formatting numbers as strings to be conversions
(many dynamic languages will even perform conversion for you automatically), however Julia does
not: even though some strings can be parsed as numbers, most strings are not valid representations
of numbers, and only a very limited subset of them are. Therefore in Julia the dedicated `parse()`
function must be used to perform this operation, making it more explicit.

### Defining New Conversions

To define a new conversion, simply provide a new method for `convert()`. That's really all there
is to it. For example, the method to convert a real number to a boolean is this:

```julia
convert(::Type{Bool}, x::Real) = x==0 ? false : x==1 ? true : throw(InexactError())
```

The type of the first argument of this method is a [singleton type](@ref man-singleton-types), `Type{Bool}`, the only
instance of which is `Bool`. Thus, this method is only invoked when the first argument is the
type value `Bool`. Notice the syntax used for the first argument: the argument name is omitted
prior to the `::` symbol, and only the type is given.  This is the syntax in Julia for a function
argument whose type is specified but whose value is never used in the function body.  In this
example, since the type is a singleton, there would never be any reason to use its value within
the body. When invoked, the method determines whether a numeric value is true or false as a boolean,
by comparing it to one and zero:

```julia
julia> convert(Bool, 1)
true

julia> convert(Bool, 0)
false

julia> convert(Bool, 1im)
ERROR: InexactError()
 in convert(::Type{Bool}, ::Complex{Int64}) at ./complex.jl:23
 ...

julia> convert(Bool, 0im)
false
```

The method signatures for conversion methods are often quite a bit more involved than this example,
especially for parametric types. The example above is meant to be pedagogical, and is not the
actual Julia behaviour. This is the actual implementation in Julia:

```julia
convert{T<:Real}(::Type{T}, z::Complex) = (imag(z)==0 ? convert(T,real(z)) :
                                           throw(InexactError()))

julia> convert(Bool, 1im)
ERROR: InexactError()
 in convert(::Type{Bool}, ::Complex{Int64}) at ./complex.jl:18
 ...
```

### [Case Study: Rational Conversions](@id man-rational-conversion)

To continue our case study of Julia's `Rational` type, here are the conversions declared in [rational.jl](https://github.com/JuliaLang/julia/blob/master/base/rational.jl),
right after the declaration of the type and its constructors:

```julia
convert{T<:Integer}(::Type{Rational{T}}, x::Rational) = Rational(convert(T,x.num),convert(T,x.den))
convert{T<:Integer}(::Type{Rational{T}}, x::Integer) = Rational(convert(T,x), convert(T,1))

function convert{T<:Integer}(::Type{Rational{T}}, x::AbstractFloat, tol::Real)
    if isnan(x); return zero(T)//zero(T); end
    if isinf(x); return sign(x)//zero(T); end
    y = x
    a = d = one(T)
    b = c = zero(T)
    while true
        f = convert(T,round(y)); y -= f
        a, b, c, d = f*a+c, f*b+d, a, b
        if y == 0 || abs(a/b-x) <= tol
            return a//b
        end
        y = 1/y
    end
end
convert{T<:Integer}(rt::Type{Rational{T}}, x::AbstractFloat) = convert(rt,x,eps(x))

convert{T<:AbstractFloat}(::Type{T}, x::Rational) = convert(T,x.num)/convert(T,x.den)
convert{T<:Integer}(::Type{T}, x::Rational) = div(convert(T,x.num),convert(T,x.den))
```

The initial four convert methods provide conversions to rational types. The first method converts
one type of rational to another type of rational by converting the numerator and denominator to
the appropriate integer type. The second method does the same conversion for integers by taking
the denominator to be 1. The third method implements a standard algorithm for approximating a
floating-point number by a ratio of integers to within a given tolerance, and the fourth method
applies it, using machine epsilon at the given value as the threshold. In general, one should
have `a//b == convert(Rational{Int64}, a/b)`.

The last two convert methods provide conversions from rational types to floating-point and integer
types. To convert to floating point, one simply converts both numerator and denominator to that
floating point type and then divides. To convert to integer, one can use the `div` operator for
truncated integer division (rounded towards zero).

## Promotion

Promotion refers to converting values of mixed types to a single common type. Although it is not
strictly necessary, it is generally implied that the common type to which the values are converted
can faithfully represent all of the original values. In this sense, the term "promotion" is appropriate
since the values are converted to a "greater" type -- i.e. one which can represent all of the
input values in a single common type. It is important, however, not to confuse this with object-oriented
(structural) super-typing, or Julia's notion of abstract super-types: promotion has nothing to
do with the type hierarchy, and everything to do with converting between alternate representations.
For instance, although every `Int32` value can also be represented as a `Float64` value, `Int32`
is not a subtype of `Float64`.

Promotion to a common "greater" type is performed in Julia by the `promote` function, which takes
any number of arguments, and returns a tuple of the same number of values, converted to a common
type, or throws an exception if promotion is not possible. The most common use case for promotion
is to convert numeric arguments to a common type:

```julia
julia> promote(1, 2.5)
(1.0,2.5)

julia> promote(1, 2.5, 3)
(1.0,2.5,3.0)

julia> promote(2, 3//4)
(2//1,3//4)

julia> promote(1, 2.5, 3, 3//4)
(1.0,2.5,3.0,0.75)

julia> promote(1.5, im)
(1.5 + 0.0im,0.0 + 1.0im)

julia> promote(1 + 2im, 3//4)
(1//1 + 2//1*im,3//4 + 0//1*im)
```

Floating-point values are promoted to the largest of the floating-point argument types. Integer
values are promoted to the larger of either the native machine word size or the largest integer
argument type. Mixtures of integers and floating-point values are promoted to a floating-point
type big enough to hold all the values. Integers mixed with rationals are promoted to rationals.
Rationals mixed with floats are promoted to floats. Complex values mixed with real values are
promoted to the appropriate kind of complex value.

That is really all there is to using promotions. The rest is just a matter of clever application,
the most typical "clever" application being the definition of catch-all methods for numeric operations
like the arithmetic operators `+`, `-`, `*` and `/`. Here are some of the catch-all method definitions
given in [promotion.jl](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl):

```julia
+(x::Number, y::Number) = +(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)
```

These method definitions say that in the absence of more specific rules for adding, subtracting,
multiplying and dividing pairs of numeric values, promote the values to a common type and then
try again. That's all there is to it: nowhere else does one ever need to worry about promotion
to a common numeric type for arithmetic operations -- it just happens automatically. There are
definitions of catch-all promotion methods for a number of other arithmetic and mathematical functions
in [promotion.jl](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl), but beyond
that, there are hardly any calls to `promote` required in the Julia standard library. The most
common usages of `promote` occur in outer constructors methods, provided for convenience, to allow
constructor calls with mixed types to delegate to an inner type with fields promoted to an appropriate
common type. For example, recall that [rational.jl](https://github.com/JuliaLang/julia/blob/master/base/rational.jl)
provides the following outer constructor method:

```julia
Rational(n::Integer, d::Integer) = Rational(promote(n,d)...)
```

This allows calls like the following to work:

```julia
julia> Rational(Int8(15),Int32(-5))
-3//1

julia> typeof(ans)
Rational{Int32}
```

For most user-defined types, it is better practice to require programmers to supply the expected
types to constructor functions explicitly, but sometimes, especially for numeric problems, it
can be convenient to do promotion automatically.

### Defining Promotion Rules

Although one could, in principle, define methods for the `promote` function directly, this would
require many redundant definitions for all possible permutations of argument types. Instead, the
behavior of `promote` is defined in terms of an auxiliary function called `promote_rule`, which
one can provide methods for. The `promote_rule` function takes a pair of type objects and returns
another type object, such that instances of the argument types will be promoted to the returned
type. Thus, by defining the rule:

```julia
promote_rule(::Type{Float64}, ::Type{Float32} ) = Float64
```

one declares that when 64-bit and 32-bit floating-point values are promoted together, they should
be promoted to 64-bit floating-point. The promotion type does not need to be one of the argument
types, however; the following promotion rules both occur in Julia's standard library:

```julia
promote_rule(::Type{UInt8}, ::Type{Int8}) = Int
promote_rule(::Type{BigInt}, ::Type{Int8}) = BigInt
```

In the latter case, the result type is `BigInt` since `BigInt` is the only type large enough to
hold integers for arbitrary-precision integer arithmetic.  Also note that one does not need to
define both `promote_rule(::Type{A}, ::Type{B})` and `promote_rule(::Type{B}, ::Type{A})` -- the
symmetry is implied by the way `promote_rule` is used in the promotion process.

The `promote_rule` function is used as a building block to define a second function called `promote_type`,
which, given any number of type objects, returns the common type to which those values, as arguments
to `promote` should be promoted. Thus, if one wants to know, in absence of actual values, what
type a collection of values of certain types would promote to, one can use `promote_type`:

```julia
julia> promote_type(Int8, UInt16)
Int64
```

Internally, `promote_type` is used inside of `promote` to determine what type argument values
should be converted to for promotion. It can, however, be useful in its own right. The curious
reader can read the code in [promotion.jl](https://github.com/JuliaLang/julia/blob/master/base/promotion.jl),
which defines the complete promotion mechanism in about 35 lines.

### Case Study: Rational Promotions

Finally, we finish off our ongoing case study of Julia's rational number type, which makes relatively
sophisticated use of the promotion mechanism with the following promotion rules:

```julia
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{S}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:Integer}(::Type{Rational{T}}, ::Type{Rational{S}}) = Rational{promote_type(T,S)}
promote_rule{T<:Integer,S<:AbstractFloat}(::Type{Rational{T}}, ::Type{S}) = promote_type(T,S)
```

The first rule says that promoting a rational number with any other integer type promotes to a
rational type whose numerator/denominator type is the result of promotion of its numerator/denominator
type with the other integer type. The second rule applies the same logic to two different types
of rational numbers, resulting in a rational of the promotion of their respective numerator/denominator
types. The third and final rule dictates that promoting a rational with a float results in the
same type as promoting the numerator/denominator type with the float.

This small handful of promotion rules, together with the [conversion methods discussed above](@ref man-rational-conversion),
are sufficient to make rational numbers interoperate completely naturally with all of Julia's
other numeric types -- integers, floating-point numbers, and complex numbers. By providing appropriate
conversion methods and promotion rules in the same manner, any user-defined numeric type can interoperate
just as naturally with Julia's predefined numerics.

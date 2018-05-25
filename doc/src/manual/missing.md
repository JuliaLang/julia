# [Missing Values](@id missing)

Julia provides support for representing missing values in the statistical sense,
that is for situations where no value is available for a variable in an observation,
but a valid value theoretically exists.
Missing values are represented via the [`missing`](@ref) object, which is the
singleton instance of the type [`Missing`](@ref). `missing` is equivalent to
[`NULL` in SQL](https://en.wikipedia.org/wiki/NULL_(SQL)) and
[`NA` in R](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#NA-handling),
and behaves like them in most situations.

## Propagation of Missing Values

The behavior of `missing` values follows one basic rule: `missing`
values *propagate* automatically when passed to standard operators and functions,
in particular mathematical functions. Uncertainty about the value of one of the operands
induces uncertainty about the result. In practice, this means an operation involving
a `missing` value generally returns `missing`
```jldoctest
julia> missing + 1
missing

julia> "a" * missing
missing

julia> abs(missing)
missing
```

As `missing` is a normal Julia object, this propagation rule only works
for functions which have opted in to implement this behavior. This can be
achieved either via a specific method defined for arguments of type `Missing`,
or simply by accepting arguments of this type, and passing them to functions
which propagate them (like standard operators). Packages should consider
whether it makes sense to propagate missing values when defining new functions,
and define methods appropriately if that is the case. Passing a `missing` value
to a function for which no method accepting arguments of type `Missing` is defined
throws a `MethodError`, just like for any other type.

## Equality and Comparison Operators

Standard equality and comparison operators follow the propagation rule presented
above: if any of the operands is `missing`, the result is `missing`.
Here are a few examples
```jldoctest
julia> missing == 1
missing

julia> missing == missing
missing

julia> missing < 1
missing

julia> 2 >= missing
missing
```

In particular, note that `missing == missing` returns `missing`, so `==` cannot
be used to test whether a value is missing. To test whether `x` is `missing`,
use [`ismissing(x)`](@ref).

Special comparison operators [`isequal`](@ref) and [`===`](@ref) are exceptions
to the propagation rule: they always return a `Bool` value, even in the presence
of `missing` values, considering `missing` as equal to `missing` and as different
from any other value. They can therefore be used to test whether a value is `missing`
```jldoctest
julia> missing === 1
false

julia> isequal(missing, 1)
false

julia> missing === missing
true

julia> isequal(missing, missing)
true
```

The [`isless`](@ref) operator is another exception: `missing` is considered
as greater than any other value. This operator is used by [`sort`](@ref),
which therefore places `missing` values after all other values.
```jldoctest
julia> isless(1, missing)
true

julia> isless(missing, Inf)
false

julia> isless(missing, missing)
false
```

## Logical operators

Logical (or boolean) operators [`|`](@ref), [`&`](@ref) and [`xor`](@ref) are
another special case, as they only propagate `missing` values when it is logically
required. For these operators, whether or not the result is uncertain depends
on the particular operation, following the well-established rules of
[*three-valued logic*](https://en.wikipedia.org/wiki/Three-valued_logic) which are
also implemented by `NULL` in SQL and `NA` in R. This abstract definition actually
corresponds to a relatively natural behavior which is best explained
via concrete examples.

Let us illustrate this principle with the logical "or" operator [`|`](@ref).
Following the rules of boolean logic, if one of the operands is `true`,
the value of the other operand does not have an influence on the result,
which will always be `true`
```jldoctest
julia> true | true
true

julia> true | false
true

julia> false | true
true
```

Based on this observation, we can conclude that if one of the operands is `true`
and the other `missing`, we know that the result is `true` in spite of the
uncertainty about the actual value of one of the operands. If we had
been able to observe the actual value of the second operand, it could only be
`true` or `false`, and in both cases the result would be `true`. Therefore,
in this particular case, missingness does *not* propagate
```jldoctest
julia> true | missing
true

julia> missing | true
true
```

On the contrary, if one of the operands is `false`, the result could be either
`true` or `false` depending on the value of the other operand. Therefore,
if that operand is `missing`, the result has to be `missing` too
```jldoctest
julia> false | true
true

julia> true | false
true

julia> false | false
false

julia> false | missing
missing

julia> missing | false
missing
```

The behavior of the logical "and" operator [`&`](@ref) is similar to that of the
`|` operator, with the difference that missingness does not propagate when
one of the operands is `false`. For example, when that is the case of the first
operand
```jldoctest
julia> false & false
false

julia> false & true
false

julia> false & missing
false
```

On the other hand, missingness propagates when one of the operands is `true`,
for example the first one
```jldoctest
julia> true & true
true

julia> true & false
false

julia> true & missing
missing
```

Finally, the "exclusive or" logical operator [`xor`](@ref) always propagates
`missing` values, since both operands always have an effect on the result.
Also note that the negation operator [`!`](@ref) returns `missing` when the
operand is `missing` just like other unary operators.

## Control Flow and Short-Circuiting Operators

Control flow operators including [`if`](@ref), [`while`](@ref) and the
[ternary operator](@ref man-conditional-evaluation) `x ? y : z`
do not allow for missing values. This is because of the uncertainty about whether
the actual value would be `true` or `false` if we could observe it,
which implies that we do not know how the program should behave. A `TypeError`
is thrown as soon as a `missing` value is encountered in this context
```jldoctest
julia> if missing
           println("here")
       end
ERROR: TypeError: non-boolean (Missing) used in boolean context
```

For the same reason, contrary to logical operators presented above,
the short-circuiting boolean operators [`&&`](@ref) and [`||`](@ref) do not
allow for `missing` values in situations where the value of the operand
determines whether the next operand is evaluated or not. For example
```jldoctest
julia> missing || false
ERROR: TypeError: non-boolean (Missing) used in boolean context

julia> missing && false
ERROR: TypeError: non-boolean (Missing) used in boolean context

julia> true && missing && false
ERROR: TypeError: non-boolean (Missing) used in boolean context
```

On the other hand, no error is thrown when the result can be determined without
the `missing` values. This is the case when the code short-circuits
before evaluating the `missing` operand, and when the `missing` operand is the
last one
```jldoctest
julia> true && missing
missing

julia> false && missing
false
```

## Arrays With Missing Values

Arrays containing missing values can be created like other arrays
```jldoctest
julia> [1, missing]
2-element Array{Union{Missing, Int64},1}:
 1
  missing
```

As this example shows, the element type of such arrays is `Union{Missing, T}`,
with `T` the type of the non-missing values. This simply reflects the fact that
array entries can be either of type `T` (here, `Int64`) or of type `Missing`.
This kind of array uses an efficient memory storage equivalent to an `Array{T}`
holding the actual values combined with an `Array{UInt8}` indicating the type
of the entry (i.e. whether it is `Missing` or `T`).

Arrays allowing for missing values can be constructed with the standard syntax.
Use `Array{Union{Missing, T}}(missing, dims)` to create arrays filled with
missing values:
```jldoctest
julia> Array{Union{Missing, String}}(missing, 2, 3)
2×3 Array{Union{Missing, String},2}:
 missing  missing  missing
 missing  missing  missing
```

An array allowing for `missing` values but which does not contain any such value
can be converted back to an array which does not allow for missing values using
[`convert`](@ref). If the array contains `missing` values, a `MethodError` is thrown
during conversion
```jldoctest
julia> x = Union{Missing, String}["a", "b"]
2-element Array{Union{Missing, String},1}:
 "a"
 "b"

julia> convert(Array{String}, x)
2-element Array{String,1}:
 "a"
 "b"

julia> y = Union{Missing, String}[missing, "b"]
2-element Array{Union{Missing, String},1}:
 missing
 "b"

julia> convert(Array{String}, y)
ERROR: MethodError: Cannot `convert` an object of type Missing to an object of type String
```
## Skipping Missing Values

Since `missing` values propagate with standard mathematical operators, reduction
functions return `missing` when called on arrays which contain missing values
```jldoctest
julia> sum([1, missing])
missing
```

In this situation, use the [`skipmissing`](@ref) function to skip missing values
```jldoctest
julia> sum(skipmissing([1, missing]))
1
```

This convenience function returns an iterator which filters out `missing` values
efficiently. It can therefore be used with any function which supports iterators
```jldoctest
julia> maximum(skipmissing([3, missing, 2, 1]))
3

julia> mean(skipmissing([3, missing, 2, 1]))
2.0

julia> mapreduce(sqrt, +, skipmissing([3, missing, 2, 1]))
4.146264369941973
```

Use [`collect`](@ref) to extract non-`missing` values and store them in an array
```jldoctest
julia> collect(skipmissing([3, missing, 2, 1]))
3-element Array{Int64,1}:
 3
 2
 1
```

## Logical Operations on Arrays

The three-valued logic described above for logical operators is also used
by logical functions applied to arrays. Thus, array equality tests using
the [`==`](@ref) operator return `missing` whenever the result cannot be
determined without knowing the actual value of the `missing` entry. In practice,
this means that `missing` is returned if all non-missing values of the compared
arrays are equal, but one or both arrays contain missing values (possibly at
different positions)
```jldoctest
julia> [1, missing] == [2, missing]
false

julia> [1, missing] == [1, missing]
missing

julia> [1, 2, missing] == [1, missing, 2]
missing
```

As for single values, use [`isequal`](@ref) to treat `missing` values as equal
to other `missing` values but different from non-missing values
```jldoctest
julia> isequal([1, missing], [1, missing])
true

julia> isequal([1, 2, missing], [1, missing, 2])
false
```

Functions [`any`](@ref) and [`all`](@ref) also follow the rules of
three-valued logic, returning `missing` when the result cannot be determined
```jldoctest
julia> all([true, missing])
missing

julia> all([false, missing])
false

julia> any([true, missing])
true

julia> any([false, missing])
missing
```

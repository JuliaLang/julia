# This file is a part of Julia. License is MIT: https://julialang.org/license

## types ##

"""
    <:(T1, T2)::Bool

Subtyping relation, defined between two types. In Julia, a type `S` is said to be a
*subtype* of a type `T` if and only if we have `S <: T`.

For any type `L` and any type `R`, `L <: R` implies that any value `v` of type `L`
is also of type `R`. I.e., `(L <: R) && (v isa L)` implies `v isa R`.

The subtyping relation is a *partial order*. I.e., `<:` is:

* *reflexive*: for any type `T`, `T <: T` holds

* *antisymmetric*: for any type `A` and any type `B`, `(A <: B) && (B <: A)`
  implies `A == B`

* *transitive*: for any type `A`, any type `B` and any type `C`;
  `(A <: B) && (B <: C)` implies `A <: C`

See also info on [Types](@ref man-types), [`Union{}`](@ref), [`Any`](@ref), [`isa`](@ref).

# Examples
```jldoctest
julia> Float64 <: AbstractFloat
true

julia> Vector{Int} <: AbstractArray
true

julia> Matrix{Float64} <: Matrix{AbstractFloat}  # `Matrix` is invariant
false

julia> Tuple{Float64} <: Tuple{AbstractFloat}    # `Tuple` is covariant
true

julia> Union{} <: Int  # The bottom type, `Union{}`, subtypes each type.
true

julia> Union{} <: Float32 <: AbstractFloat <: Real <: Number <: Any  # Operator chaining
true
```

The `<:` keyword also has several syntactic uses which represent the same subtyping relation,
but which do not execute the operator or return a Bool:

* To specify the lower bound and the upper bound on a parameter of a
  [`UnionAll`](@ref) type in a [`where`](@ref) statement.

* To specify the lower bound and the upper bound on a (static) parameter of a
  method, see [Parametric Methods](@ref).

* To define a subtyping relation while declaring a new type, see [`struct`](@ref)
  and [`abstract type`](@ref).
"""
(<:)

import Core: >:

"""
    >:(T1, T2)

Supertype operator, equivalent to `T2 <: T1`.
"""
>:

"""
    supertype(T::Union{DataType, UnionAll})

Return the direct supertype of type `T`.
`T` can be a [`DataType`](@ref) or a [`UnionAll`](@ref) type. Does not support
type [`Union`](@ref)s. Also see info on [Types](@ref man-types).

# Examples
```jldoctest
julia> supertype(Int32)
Signed

julia> supertype(Vector)
DenseVector (alias for DenseArray{T, 1} where T)
```
"""
supertype(T::DataType) = (@_total_meta; T.super)
supertype(T::UnionAll) = (@_total_meta; UnionAll(T.var, supertype(T.body)))

## generic comparison ##

"""
    ==(x, y)

Generic equality operator. Falls back to [`===`](@ref).
Should be implemented for all types with a notion of equality, based on the abstract value
that an instance represents. For example, all numeric types are compared by numeric value,
ignoring type. Strings are compared as sequences of characters, ignoring encoding.
Collections of the same type generally compare their key sets, and if those are `==`, then compare the values
for each of those keys, returning true if all such pairs are `==`.
Other properties are typically not taken into account (such as the exact type).

This operator follows IEEE semantics for floating-point numbers: `0.0 == -0.0` and
`NaN != NaN`.

The result is of type `Bool`, except when one of the operands is [`missing`](@ref),
in which case `missing` is returned
([three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic)).
Collections generally implement three-valued logic akin to [`all`](@ref), returning
missing if any operands contain missing values and all other pairs are equal.
Use [`isequal`](@ref) or [`===`](@ref) to always get a `Bool` result.

# Implementation
New numeric types should implement this function for two arguments of the new type, and
handle comparison to other types via promotion rules where possible.

Equality and hashing are intimately related; two values that are considered [`isequal`](@ref) **must**
have the same [`hash`](@ref) and by default `isequal` falls back to `==`. If a type customizes the behavior of `==` and/or [`isequal`](@ref),
then [`hash`](@ref) must be similarly implemented to ensure `isequal` and `hash` agree. `Set`s, `Dict`s, and many other internal
implementations assume that this invariant holds.

If some type defines `==`, [`isequal`](@ref), and [`isless`](@ref) then it should
also implement [`<`](@ref) to ensure consistency of comparisons.
"""
==

"""
    isequal(x, y)::Bool

Similar to [`==`](@ref), except for the treatment of floating point numbers
and of missing values. `isequal` treats all floating-point `NaN` values as equal
to each other, treats `-0.0` as unequal to `0.0`, and [`missing`](@ref) as equal
to `missing`. Always returns a `Bool` value.

`isequal` is an equivalence relation - it is reflexive (`===` implies `isequal`), symmetric
(`isequal(a, b)` implies `isequal(b, a)`) and transitive (`isequal(a, b)` and
`isequal(b, c)` implies `isequal(a, c)`).

# Implementation
The default implementation of `isequal` calls `==`, so a type that does not involve
floating-point values generally only needs to define `==`.

`isequal` is the comparison function used by hash tables (`Dict`). `isequal(x,y)` must imply
that `hash(x) == hash(y)`.

This typically means that types for which a custom `==` or `isequal` method exists must
implement a corresponding [`hash`](@ref) method (and vice versa). Collections typically
implement `isequal` by calling `isequal` recursively on all contents.

Furthermore, `isequal` is linked with [`isless`](@ref), and they work together to
define a fixed total ordering, where exactly one of `isequal(x, y)`, `isless(x, y)`, or
`isless(y, x)` must be `true` (and the other two `false`).

Scalar types generally do not need to implement `isequal` separate from `==`, unless they
represent floating-point numbers amenable to a more efficient implementation than that
provided as a generic fallback (based on `isnan`, `signbit`, and `==`).

# Examples
```jldoctest
julia> isequal([1., NaN], [1., NaN])
true

julia> [1., NaN] == [1., NaN]
false

julia> 0.0 == -0.0
true

julia> isequal(0.0, -0.0)
false

julia> missing == missing
missing

julia> isequal(missing, missing)
true
```
"""
isequal(x, y) = (x == y)::Bool # all `missing` cases are handled in missing.jl

signequal(x, y) = signbit(x)::Bool == signbit(y)::Bool
signless(x, y) = signbit(x)::Bool & !signbit(y)::Bool

isequal(x::AbstractFloat, y::AbstractFloat) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)::Bool
isequal(x::Real,          y::AbstractFloat) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)::Bool
isequal(x::AbstractFloat, y::Real         ) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)::Bool

"""
    isless(x, y)

Test whether `x` is less than `y`, according to a fixed total order (defined together with
[`isequal`](@ref)). `isless` is not defined for pairs `(x, y)` of all types. However, if it
is defined, it is expected to satisfy the following:
- If `isless(x, y)` is defined, then so is `isless(y, x)` and `isequal(x, y)`,
  and exactly one of those three yields `true`.
- The relation defined by `isless` is transitive, i.e.,
  `isless(x, y) && isless(y, z)` implies `isless(x, z)`.

Values that are normally unordered, such as `NaN`,
are ordered after regular values.
[`missing`](@ref) values are ordered last.

This is the default comparison used by [`sort!`](@ref).

# Implementation
Non-numeric types with a total order should implement this function.
Numeric types only need to implement it if they have special values such as `NaN`.
Types with a partial order should implement [`<`](@ref).
See the documentation on [Alternate Orderings](@ref) for how to define alternate
ordering methods that can be used in sorting and related functions.

# Examples
```jldoctest
julia> isless(1, 3)
true

julia> isless("Red", "Blue")
false
```
"""
function isless end

isless(x::AbstractFloat, y::AbstractFloat) = (!isnan(x) & (isnan(y) | signless(x, y))) | (x < y)
isless(x::Real,          y::AbstractFloat) = (!isnan(x) & (isnan(y) | signless(x, y))) | (x < y)
isless(x::AbstractFloat, y::Real         ) = (!isnan(x) & (isnan(y) | signless(x, y))) | (x < y)

# Performance optimization to reduce branching
# This is useful for sorting tuples of integers
# TODO: remove this when the compiler can optimize the generic version better
# See #48724 and #48753
isless(a::Tuple{BitInteger, BitInteger}, b::Tuple{BitInteger, BitInteger}) =
    isless(a[1], b[1]) | (isequal(a[1], b[1]) & isless(a[2], b[2]))

"""
    isgreater(x, y)

Not the inverse of `isless`! Test whether `x` is greater than `y`, according to
a fixed total order compatible with `min`.

Defined with `isless`, this function is usually `isless(y, x)`, but `NaN` and
[`missing`](@ref) are ordered as smaller than any regular value with `missing`
smaller than `NaN`.

So `isless` defines an ascending total order with `NaN` and `missing` as the
largest values and `isgreater` defines a descending total order with `NaN` and
`missing` as the smallest values.

!!! note

    Like `min`, `isgreater` orders containers (tuples, vectors, etc)
    lexicographically with `isless(y, x)` rather than recursively with itself:

    ```jldoctest
    julia> Base.isgreater(1, NaN) # 1 is greater than NaN
    true

    julia> Base.isgreater((1,), (NaN,)) # But (1,) is not greater than (NaN,)
    false

    julia> sort([1, 2, 3, NaN]; lt=Base.isgreater)
    4-element Vector{Float64}:
       3.0
       2.0
       1.0
     NaN

    julia> sort(tuple.([1, 2, 3, NaN]); lt=Base.isgreater)
    4-element Vector{Tuple{Float64}}:
     (NaN,)
     (3.0,)
     (2.0,)
     (1.0,)
    ```

# Implementation
This is unexported. Types should not usually implement this function. Instead, implement `isless`.
"""
isgreater(x, y) = isunordered(x) || isunordered(y) ? isless(x, y) : isless(y, x)

"""
    isunordered(x)

Return `true` if `x` is a value that is not orderable according to [`<`](@ref), such as `NaN`
or `missing`.

The values that evaluate to `true` with this predicate may be orderable with respect to other
orderings such as [`isless`](@ref).

!!! compat "Julia 1.7"
    This function requires Julia 1.7 or later.
"""
isunordered(x) = false
isunordered(x::AbstractFloat) = isnan(x)
isunordered(x::Missing) = true

==(T::Type, S::Type) = (@_total_meta; ccall(:jl_types_equal, Cint, (Any, Any), T, S) != 0)
!=(T::Type, S::Type) = (@_total_meta; !(T == S))
==(T::TypeVar, S::Type) = false
==(T::Type, S::TypeVar) = false

## comparison fallbacks ##

"""
    !=(x, y)
    ≠(x,y)

Not-equals comparison operator. Always gives the opposite answer as [`==`](@ref).

# Implementation
New types should generally not implement this, and rely on the fallback definition
`!=(x,y) = !(x==y)` instead.

# Examples
```jldoctest
julia> 3 != 2
true

julia> "foo" ≠ "foo"
false
```
"""
!=(x, y) = !(x == y)
const ≠ = !=

"""
    ===(x,y)::Bool
    ≡(x,y)::Bool

Determine whether `x` and `y` are identical, in the sense that no program could distinguish
them. First the types of `x` and `y` are compared. If those are identical, mutable objects
are compared by address in memory and immutable objects (such as numbers) are compared by
contents at the bit level. This function is sometimes called "egal".
It always returns a `Bool` value.

# Examples
```jldoctest
julia> a = [1, 2]; b = [1, 2];

julia> a == b
true

julia> a === b
false

julia> a === a
true
```
"""
===
const ≡ = ===

import Core: !==
"""
    !==(x, y)
    ≢(x,y)

Always gives the opposite answer as [`===`](@ref).

# Examples
```jldoctest
julia> a = [1, 2]; b = [1, 2];

julia> a ≢ b
true

julia> a ≢ a
false
```
"""
!==

const ≢ = !==

"""
    <(x, y)

Less-than comparison operator. Falls back to [`isless`](@ref).
Because of the behavior of floating-point NaN values, this operator implements
a partial order.

# Implementation
New types with a canonical partial order should implement this function for
two arguments of the new type.
Types with a canonical total order should implement [`isless`](@ref) instead.

See also [`isunordered`](@ref).

# Examples
```jldoctest
julia> 'a' < 'b'
true

julia> "abc" < "abd"
true

julia> 5 < 3
false
```
"""
<(x, y) = isless(x, y)

"""
    >(x, y)

Greater-than comparison operator. Falls back to `y < x`.

# Implementation
Generally, new types should implement [`<`](@ref) instead of this function,
and rely on the fallback definition `>(x, y) = y < x`.

# Examples
```jldoctest
julia> 'a' > 'b'
false

julia> 7 > 3 > 1
true

julia> "abc" > "abd"
false

julia> 5 > 3
true
```
"""
>(x, y) = y < x

"""
    <=(x, y)
    ≤(x,y)

Less-than-or-equals comparison operator. Falls back to `(x < y) | (x == y)`.

# Examples
```jldoctest
julia> 'a' <= 'b'
true

julia> 7 ≤ 7 ≤ 9
true

julia> "abc" ≤ "abc"
true

julia> 5 <= 3
false
```
"""
<=(x, y) = (x < y) | (x == y)
const ≤ = <=

"""
    >=(x, y)
    ≥(x,y)

Greater-than-or-equals comparison operator. Falls back to `y <= x`.

# Examples
```jldoctest
julia> 'a' >= 'b'
false

julia> 7 ≥ 7 ≥ 3
true

julia> "abc" ≥ "abc"
true

julia> 5 >= 3
true
```
"""
>=(x, y) = (y <= x)
const ≥ = >=

# this definition allows Number types to implement < instead of isless,
# which is more idiomatic:
isless(x::Real, y::Real) = x<y

"""
    cmp(x,y)

Return -1, 0, or 1 depending on whether `x` is less than, equal to, or greater than `y`,
respectively. Uses the total order implemented by `isless`.

# Examples
```jldoctest
julia> cmp(1, 2)
-1

julia> cmp(2, 1)
1

julia> cmp(2+im, 3-im)
ERROR: MethodError: no method matching isless(::Complex{Int64}, ::Complex{Int64})
[...]
```
"""
cmp(x, y) = isless(x, y) ? -1 : ifelse(isless(y, x), 1, 0)

"""
    cmp(<, x, y)

Return -1, 0, or 1 depending on whether `x` is less than, equal to, or greater than `y`,
respectively. The first argument specifies a less-than comparison function to use.
"""
cmp(<, x, y) = (x < y) ? -1 : ifelse(y < x, 1, 0)

# cmp returns -1, 0, +1 indicating ordering
cmp(x::Integer, y::Integer) = ifelse(isless(x, y), -1, ifelse(isless(y, x), 1, 0))

"""
    max(x, y, ...)

Return the maximum of the arguments, with respect to [`isless`](@ref).
If any of the arguments is [`missing`](@ref), return `missing`.
See also the [`maximum`](@ref) function to take the maximum element from a collection.

# Examples
```jldoctest
julia> max(2, 5, 1)
5

julia> max(5, missing, 6)
missing
```
"""
max(x, y) = ifelse(isless(y, x), x, y)

"""
    min(x, y, ...)

Return the minimum of the arguments, with respect to [`isless`](@ref).
If any of the arguments is [`missing`](@ref), return `missing`.
See also the [`minimum`](@ref) function to take the minimum element from a collection.

# Examples
```jldoctest
julia> min(2, 5, 1)
1

julia> min(4, missing, 6)
missing
```
"""
min(x,y) = ifelse(isless(y, x), y, x)

"""
    minmax(x, y)

Return `(min(x,y), max(x,y))`.

See also [`extrema`](@ref) that returns `(minimum(x), maximum(x))`.

# Examples
```jldoctest
julia> minmax('c','b')
('b', 'c')
```
"""
minmax(x,y) = isless(y, x) ? (y, x) : (x, y)

## definitions providing basic traits of arithmetic operators ##

"""
    identity(x)

The identity function. Returns its argument.

See also: [`one`](@ref), [`oneunit`](@ref), and [`LinearAlgebra`](@ref man-linalg)'s `I`.

# Examples
```jldoctest
julia> identity("Well, what did you expect?")
"Well, what did you expect?"
```
"""
identity(@nospecialize x) = x

+(x::Number) = x
*(x::Number) = x
(&)(x::Integer) = x
(|)(x::Integer) = x
xor(x::Integer) = x

const ⊻ = xor
const ⊼ = nand
const ⊽ = nor

# foldl for argument lists. expand fully up to a point, then
# switch to a loop. this allows small cases like `a+b+c+d` to be managed
# efficiently, without a major slowdown for `+(x...)` when `x` is big.
# n.b.: keep this method count small, so it can be inferred without hitting the
# method count limit in inference
afoldl(op, a) = a
function afoldl(op, a, bs...)
    @_terminates_locally_meta
    l = length(bs)
    i =  0; y = a;            l == i && return y
    #@nexprs 31 i -> (y = op(y, bs[i]); l == i && return y)
    i =  1; y = op(y, bs[i]); l == i && return y
    i =  2; y = op(y, bs[i]); l == i && return y
    i =  3; y = op(y, bs[i]); l == i && return y
    i =  4; y = op(y, bs[i]); l == i && return y
    i =  5; y = op(y, bs[i]); l == i && return y
    i =  6; y = op(y, bs[i]); l == i && return y
    i =  7; y = op(y, bs[i]); l == i && return y
    i =  8; y = op(y, bs[i]); l == i && return y
    i =  9; y = op(y, bs[i]); l == i && return y
    i = 10; y = op(y, bs[i]); l == i && return y
    i = 11; y = op(y, bs[i]); l == i && return y
    i = 12; y = op(y, bs[i]); l == i && return y
    i = 13; y = op(y, bs[i]); l == i && return y
    i = 14; y = op(y, bs[i]); l == i && return y
    i = 15; y = op(y, bs[i]); l == i && return y
    i = 16; y = op(y, bs[i]); l == i && return y
    i = 17; y = op(y, bs[i]); l == i && return y
    i = 18; y = op(y, bs[i]); l == i && return y
    i = 19; y = op(y, bs[i]); l == i && return y
    i = 20; y = op(y, bs[i]); l == i && return y
    i = 21; y = op(y, bs[i]); l == i && return y
    i = 22; y = op(y, bs[i]); l == i && return y
    i = 23; y = op(y, bs[i]); l == i && return y
    i = 24; y = op(y, bs[i]); l == i && return y
    i = 25; y = op(y, bs[i]); l == i && return y
    i = 26; y = op(y, bs[i]); l == i && return y
    i = 27; y = op(y, bs[i]); l == i && return y
    i = 28; y = op(y, bs[i]); l == i && return y
    i = 29; y = op(y, bs[i]); l == i && return y
    i = 30; y = op(y, bs[i]); l == i && return y
    i = 31; y = op(y, bs[i]); l == i && return y
    for i in (i + 1):l
        y = op(y, bs[i])
    end
    return y
end
setfield!(typeof(afoldl).name, :max_args, Int32(34), :monotonic)

for op in (:+, :*, :&, :|, :xor, :min, :max, :kron)
    @eval begin
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.
        ($op)(a, b, c, xs...) = (@inline; afoldl($op, ($op)(($op)(a,b),c), xs...))
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
    end
end

function kron! end

const var"'" = adjoint

"""
    \\(x, y)

Left division operator: multiplication of `y` by the inverse of `x` on the left. Gives
floating-point results for integer arguments.

# Examples
```jldoctest
julia> 3 \\ 6
2.0

julia> inv(3) * 6
2.0

julia> A = [4 3; 2 1]; x = [5, 6];

julia> A \\ x
2-element Vector{Float64}:
  6.5
 -7.0

julia> inv(A) * x
2-element Vector{Float64}:
  6.5
 -7.0
```
"""
\(x,y) = adjoint(adjoint(y)/adjoint(x))

# Core <<, >>, and >>> take either Int or UInt as second arg. Signed shift
# counts can shift in either direction, and are translated here to unsigned
# counts. Integer datatypes only need to implement the unsigned version.

"""
    <<(x, n)

Left bit shift operator, `x << n`. For `n >= 0`, the result is `x` shifted left
by `n` bits, filling with `0`s. This is equivalent to `x * 2^n`. For `n < 0`,
this is equivalent to `x >> -n`.

# Examples
```jldoctest
julia> Int8(3) << 2
12

julia> bitstring(Int8(3))
"00000011"

julia> bitstring(Int8(12))
"00001100"
```
See also [`>>`](@ref), [`>>>`](@ref), [`exp2`](@ref), [`ldexp`](@ref).
"""
function <<(x::Integer, c::Integer)
    @inline
    typemin(Int) <= c <= typemax(Int) && return x << (c % Int)
    (x >= 0 || c >= 0) && return zero(x) << 0  # for type stability
    oftype(x, -1)
end
function <<(x::Integer, c::Unsigned)
    @inline
    if c isa UInt
        throw(MethodError(<<, (x, c)))
    end
    c <= typemax(UInt) ? x << (c % UInt) : zero(x) << UInt(0)
end
<<(x::Integer, c::Int) = c >= 0 ? x << unsigned(c) : x >> unsigned(-c)

"""
    >>(x, n)

Right bit shift operator, `x >> n`. For `n >= 0`, the result is `x` shifted
right by `n` bits, filling with `0`s if `x >= 0`, `1`s if `x < 0`, preserving
the sign of `x`. This is equivalent to `fld(x, 2^n)`. For `n < 0`, this is
equivalent to `x << -n`.

# Examples
```jldoctest
julia> Int8(13) >> 2
3

julia> bitstring(Int8(13))
"00001101"

julia> bitstring(Int8(3))
"00000011"

julia> Int8(-14) >> 2
-4

julia> bitstring(Int8(-14))
"11110010"

julia> bitstring(Int8(-4))
"11111100"
```
See also [`>>>`](@ref), [`<<`](@ref).
"""
function >>(x::Integer, c::Integer)
    @inline
    if c isa UInt
        throw(MethodError(>>, (x, c)))
    end
    typemin(Int) <= c <= typemax(Int) && return x >> (c % Int)
    (x >= 0 || c < 0) && return zero(x) >> 0
    oftype(x, -1)
end
>>(x::Integer, c::Int) = c >= 0 ? x >> unsigned(c) : x << unsigned(-c)

"""
    >>>(x, n)

Unsigned right bit shift operator, `x >>> n`. For `n >= 0`, the result is `x`
shifted right by `n` bits, filling with `0`s. For `n < 0`, this is equivalent
to `x << -n`.

For [`Unsigned`](@ref) integer types, this is equivalent to [`>>`](@ref). For
[`Signed`](@ref) integer types, this is equivalent to `signed(unsigned(x) >> n)`.

# Examples
```jldoctest
julia> Int8(-14) >>> 2
60

julia> bitstring(Int8(-14))
"11110010"

julia> bitstring(Int8(60))
"00111100"
```

[`BigInt`](@ref)s are treated as if having infinite size, so no filling is required and this
is equivalent to [`>>`](@ref).

See also [`>>`](@ref), [`<<`](@ref).
"""
function >>>(x::Integer, c::Integer)
    @inline
    typemin(Int) <= c <= typemax(Int) ? x >>> (c % Int) : zero(x) >>> 0
end
function >>>(x::Integer, c::Unsigned)
    @inline
    if c isa UInt
        throw(MethodError(>>>, (x, c)))
    end
    c <= typemax(UInt) ? x >>> (c % UInt) : zero(x) >>> 0
end
>>>(x::Integer, c::Int) = c >= 0 ? x >>> unsigned(c) : x << unsigned(-c)

# operator alias

"""
    rem(x, y)
    %(x, y)

Remainder from Euclidean division, returning a value of the same sign as `x`, and smaller in
magnitude than `y`. This value is always exact.

See also: [`div`](@ref), [`mod`](@ref), [`mod1`](@ref), [`divrem`](@ref).

# Examples
```jldoctest
julia> x = 15; y = 4;

julia> x % y
3

julia> x == div(x, y) * y + rem(x, y)
true

julia> rem.(-5:5, 3)'
1×11 adjoint(::Vector{Int64}) with eltype Int64:
 -2  -1  0  -2  -1  0  1  2  0  1  2
```
"""
rem
const % = rem

"""
    div(x, y)
    ÷(x, y)

The quotient from Euclidean (integer) division. Generally equivalent
to a mathematical operation x/y without a fractional part.

See also: [`cld`](@ref), [`fld`](@ref), [`rem`](@ref), [`divrem`](@ref).

# Examples
```jldoctest
julia> 9 ÷ 4
2

julia> -5 ÷ 3
-1

julia> 5.0 ÷ 2
2.0

julia> div.(-5:5, 3)'
1×11 adjoint(::Vector{Int64}) with eltype Int64:
 -1  -1  -1  0  0  0  0  0  1  1  1
```
"""
div
const ÷ = div

"""
    mod1(x, y)

Modulus after flooring division, returning a value `r` such that `mod(r, y) == mod(x, y)`
in the range ``(0, y]`` for positive `y` and in the range ``[y,0)`` for negative `y`.

With integer arguments and positive `y`, this is equal to `mod(x, 1:y)`, and hence natural
for 1-based indexing. By comparison, `mod(x, y) == mod(x, 0:y-1)` is natural for computations with
offsets or strides.

See also [`mod`](@ref), [`fld1`](@ref), [`fldmod1`](@ref).

# Examples
```jldoctest
julia> mod1(4, 2)
2

julia> mod1.(-5:5, 3)'
1×11 adjoint(::Vector{Int64}) with eltype Int64:
 1  2  3  1  2  3  1  2  3  1  2

julia> mod1.([-0.1, 0, 0.1, 1, 2, 2.9, 3, 3.1]', 3)
1×8 Matrix{Float64}:
 2.9  3.0  0.1  1.0  2.0  2.9  3.0  0.1
```
"""
mod1(x::T, y::T) where {T<:Real} = (m = mod(x, y); ifelse(m == 0, y, m))


"""
    fld1(x, y)

Flooring division, returning a value consistent with `mod1(x,y)`

See also [`mod1`](@ref), [`fldmod1`](@ref).

# Examples
```jldoctest
julia> x = 15; y = 4;

julia> fld1(x, y)
4

julia> x == fld(x, y) * y + mod(x, y)
true

julia> x == (fld1(x, y) - 1) * y + mod1(x, y)
true
```
"""
fld1(x::T, y::T) where {T<:Real} = (m = mod1(x, y); fld((x - m) + y, y))
function fld1(x::T, y::T) where T<:Integer
    d = div(x, y)
    return d + (!signbit(x ⊻ y) & (d * y != x))
end

"""
    fldmod1(x, y)

Return `(fld1(x,y), mod1(x,y))`.

See also [`fld1`](@ref), [`mod1`](@ref).
"""
fldmod1(x, y) = (fld1(x, y), mod1(x, y))


"""
    widen(x)

If `x` is a type, return a "larger" type, defined so that arithmetic operations
`+` and `-` are guaranteed not to overflow nor lose precision for any combination
of values that type `x` can hold.

For fixed-size integer types less than 128 bits, `widen` will return a type with
twice the number of bits.

If `x` is a value, it is converted to `widen(typeof(x))`.

# Examples
```jldoctest
julia> widen(Int32)
Int64

julia> widen(1.5f0)
1.5
```
"""
widen(x::T) where {T} = convert(widen(T), x)
widen(x::Type{T}) where {T} = throw(MethodError(widen, (T,)))
widen(x::Type{Union{}}, slurp...) = throw(MethodError(widen, (Union{},)))

# function pipelining

"""
    |>(x, f)

Infix operator which applies function `f` to the argument `x`.
This allows `f(g(x))` to be written `x |> g |> f`.
When used with anonymous functions, parentheses are typically required around
the definition to get the intended chain.

# Examples
```jldoctest
julia> 4 |> inv
0.25

julia> [2, 3, 5] |> sum |> inv
0.1

julia> [0 1; 2 3] .|> (x -> x^2) |> sum
14
```
"""
|>(x, f) = f(x)

_stable_typeof(x) = typeof(x)
_stable_typeof(::Type{T}) where {T} = @isdefined(T) ? Type{T} : DataType

"""
    f = Returns(value)

Create a callable `f` such that `f(args...; kw...) === value` holds.

# Examples

```jldoctest
julia> f = Returns(42);

julia> f(1)
42

julia> f("hello", x=32)
42

julia> f.value
42
```

!!! compat "Julia 1.7"
    `Returns` requires at least Julia 1.7.
"""
struct Returns{V} <: Function
    value::V
    Returns{V}(value) where {V} = new{V}(value)
    Returns(value) = new{_stable_typeof(value)}(value)
end

(obj::Returns)(@nospecialize(args...); @nospecialize(kw...)) = obj.value

# function composition

"""
    f ∘ g

Compose functions: i.e. `(f ∘ g)(args...; kwargs...)` means `f(g(args...; kwargs...))`. The `∘` symbol can be
entered in the Julia REPL (and most editors, appropriately configured) by typing `\\circ<tab>`.

Function composition also works in prefix form: `∘(f, g)` is the same as `f ∘ g`.
The prefix form supports composition of multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
and splatting `∘(fs...)` for composing an iterable collection of functions.
The last argument to `∘` execute first.

!!! compat "Julia 1.4"
    Multiple function composition requires at least Julia 1.4.

!!! compat "Julia 1.5"
    Composition of one function ∘(f) requires at least Julia 1.5.

!!! compat "Julia 1.7"
    Using keyword arguments requires at least Julia 1.7.

# Examples
```jldoctest
julia> map(uppercase∘first, ["apple", "banana", "carrot"])
3-element Vector{Char}:
 'A': ASCII/Unicode U+0041 (category Lu: Letter, uppercase)
 'B': ASCII/Unicode U+0042 (category Lu: Letter, uppercase)
 'C': ASCII/Unicode U+0043 (category Lu: Letter, uppercase)

julia> (==(6)∘length).(["apple", "banana", "carrot"])
3-element BitVector:
 0
 1
 1

julia> fs = [
           x -> 2x
           x -> x-1
           x -> x/2
           x -> x+1
       ];

julia> ∘(fs...)(3)
2.0
```
See also [`ComposedFunction`](@ref), [`!f::Function`](@ref).
"""
function ∘ end

"""
    ComposedFunction{Outer,Inner} <: Function

Represents the composition of two callable objects `outer::Outer` and `inner::Inner`. That is
```julia
ComposedFunction(outer, inner)(args...; kw...) === outer(inner(args...; kw...))
```
The preferred way to construct an instance of `ComposedFunction` is to use the composition operator [`∘`](@ref):
```jldoctest
julia> sin ∘ cos === ComposedFunction(sin, cos)
true

julia> typeof(sin∘cos)
ComposedFunction{typeof(sin), typeof(cos)}
```
The composed pieces are stored in the fields of `ComposedFunction` and can be retrieved as follows:
```jldoctest
julia> composition = sin ∘ cos
sin ∘ cos

julia> composition.outer === sin
true

julia> composition.inner === cos
true
```
!!! compat "Julia 1.6"
    ComposedFunction requires at least Julia 1.6. In earlier versions `∘` returns an anonymous function instead.

See also [`∘`](@ref).
"""
struct ComposedFunction{O,I} <: Function
    outer::O
    inner::I
    ComposedFunction{O, I}(outer, inner) where {O, I} = new{O, I}(outer, inner)
    ComposedFunction(outer, inner) = new{Core.Typeof(outer),Core.Typeof(inner)}(outer, inner)
end

(c::ComposedFunction)(x...; kw...) = call_composed(unwrap_composed(c), x, kw)
unwrap_composed(c::ComposedFunction) = (unwrap_composed(c.outer)..., unwrap_composed(c.inner)...)
unwrap_composed(c) = (maybeconstructor(c),)
call_composed(fs, x, kw) = (@inline; fs[1](call_composed(tail(fs), x, kw)))
call_composed(fs::Tuple{Any}, x, kw) = fs[1](x...; kw...)

struct Constructor{F} <: Function end
(::Constructor{F})(args...; kw...) where {F} = (@inline; F(args...; kw...))
maybeconstructor(::Type{F}) where {F} = Constructor{F}()
maybeconstructor(f) = f

∘(f) = f
∘(f, g) = ComposedFunction(f, g)
∘(f, g, h...) = ∘(f ∘ g, h...)

function show(io::IO, c::ComposedFunction)
    c.outer isa ComposedFunction ? show(io, c.outer) : _showcomposed(io, c.outer)
    print(io, " ∘ ")
    _showcomposed(io, c.inner)
end

#shows !f instead of (!) ∘ f when ! is the outermost function
function show(io::IO, c::ComposedFunction{typeof(!)})
    print(io, '!')
    _showcomposed(io, c.inner)
end

_showcomposed(io::IO, x) = show(io, x)
#display operators like + and - inside parens
_showcomposed(io::IO, f::Function) = isoperator(Symbol(f)) ? (print(io, '('); show(io, f); print(io, ')')) : show(io, f)
#nesting for chained composition
_showcomposed(io::IO, f::ComposedFunction) = (print(io, '('); show(io, f); print(io, ')'))
#no nesting when ! is the outer function in a composition chain
_showcomposed(io::IO, f::ComposedFunction{typeof(!)}) = show(io, f)

"""
    !f::Function

Predicate function negation: when the argument of `!` is a function, it returns a composed function which computes the boolean negation of `f`.

See also [`∘`](@ref).

# Examples
```jldoctest
julia> str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
"∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

julia> filter(isletter, str)
"εδxyδfxfyε"

julia> filter(!isletter, str)
"∀  > 0, ∃  > 0: |-| <  ⇒ |()-()| < "
```

!!! compat "Julia 1.9"
    Starting with Julia 1.9, `!f` returns a [`ComposedFunction`](@ref) instead of an anonymous function.
"""
!(f::Function) = (!) ∘ f
!(f::ComposedFunction{typeof(!)}) = f.inner #allows !!f === f

"""
    Fix{N}(f, x)

A type representing a partially-applied version of a function `f`, with the argument
`x` fixed at position `N::Int`. In other words, `Fix{3}(f, x)` behaves similarly to
`(y1, y2, y3...; kws...) -> f(y1, y2, x, y3...; kws...)`.

!!! compat "Julia 1.12"
    This general functionality requires at least Julia 1.12, while `Fix1` and `Fix2`
    are available earlier.

!!! note
    When nesting multiple `Fix`, note that the `N` in `Fix{N}` is _relative_ to the current
    available arguments, rather than an absolute ordering on the target function. For example,
    `Fix{1}(Fix{2}(f, 4), 4)` fixes the first and second arg, while `Fix{2}(Fix{1}(f, 4), 4)`
    fixes the first and third arg.
"""
struct Fix{N,F,T} <: Function
    f::F
    x::T

    function Fix{N}(f::F, x) where {N,F}
        if !(N isa Int)
            throw(ArgumentError(LazyString("expected type parameter in `Fix` to be `Int`, but got `", N, "::", typeof(N), "`")))
        elseif N < 1
            throw(ArgumentError(LazyString("expected `N` in `Fix{N}` to be integer greater than 0, but got ", N)))
        end
        new{N,_stable_typeof(f),_stable_typeof(x)}(f, x)
    end
end

function (f::Fix{N})(args::Vararg{Any,M}; kws...) where {N,M}
    M < N-1 && throw(ArgumentError(LazyString("expected at least ", N-1, " arguments to `Fix{", N, "}`, but got ", M)))
    return f.f(args[begin:begin+(N-2)]..., f.x, args[begin+(N-1):end]...; kws...)
end

# Special cases for improved constant propagation
(f::Fix{1})(arg; kws...) = f.f(f.x, arg; kws...)
(f::Fix{2})(arg; kws...) = f.f(arg, f.x; kws...)

"""
Alias for `Fix{1}`. See [`Fix`](@ref Base.Fix).
"""
const Fix1{F,T} = Fix{1,F,T}

"""
Alias for `Fix{2}`. See [`Fix`](@ref Base.Fix).
"""
const Fix2{F,T} = Fix{2,F,T}


"""
    isequal(x)

Create a function that compares its argument to `x` using [`isequal`](@ref), i.e.
a function equivalent to `y -> isequal(y, x)`.

The returned function is of type `Base.Fix2{typeof(isequal)}`, which can be
used to implement specialized methods.
"""
isequal(x) = Fix2(isequal, x)

"""
    ==(x)

Create a function that compares its argument to `x` using [`==`](@ref), i.e.
a function equivalent to `y -> y == x`.

The returned function is of type `Base.Fix2{typeof(==)}`, which can be
used to implement specialized methods.
"""
==(x) = Fix2(==, x)

"""
    !=(x)

Create a function that compares its argument to `x` using [`!=`](@ref), i.e.
a function equivalent to `y -> y != x`.
The returned function is of type `Base.Fix2{typeof(!=)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.2"
    This functionality requires at least Julia 1.2.
"""
!=(x) = Fix2(!=, x)

"""
    >=(x)

Create a function that compares its argument to `x` using [`>=`](@ref), i.e.
a function equivalent to `y -> y >= x`.
The returned function is of type `Base.Fix2{typeof(>=)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.2"
    This functionality requires at least Julia 1.2.
"""
>=(x) = Fix2(>=, x)

"""
    <=(x)

Create a function that compares its argument to `x` using [`<=`](@ref), i.e.
a function equivalent to `y -> y <= x`.
The returned function is of type `Base.Fix2{typeof(<=)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.2"
    This functionality requires at least Julia 1.2.
"""
<=(x) = Fix2(<=, x)

"""
    >(x)

Create a function that compares its argument to `x` using [`>`](@ref), i.e.
a function equivalent to `y -> y > x`.
The returned function is of type `Base.Fix2{typeof(>)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.2"
    This functionality requires at least Julia 1.2.
"""
>(x) = Fix2(>, x)

"""
    <(x)

Create a function that compares its argument to `x` using [`<`](@ref), i.e.
a function equivalent to `y -> y < x`.
The returned function is of type `Base.Fix2{typeof(<)}`, which can be
used to implement specialized methods.

!!! compat "Julia 1.2"
    This functionality requires at least Julia 1.2.
"""
<(x) = Fix2(<, x)

"""
    splat(f)

Equivalent to
```julia
    my_splat(f) = args->f(args...)
```
i.e. given a function returns a new function that takes one argument and splats
it into the original function. This is useful as an adaptor to pass a
multi-argument function in a context that expects a single argument, but passes
a tuple as that single argument.

# Examples
```jldoctest
julia> map(splat(+), zip(1:3,4:6))
3-element Vector{Int64}:
 5
 7
 9

julia> my_add = splat(+)
splat(+)

julia> my_add((1,2,3))
6
```
"""
splat(f) = Splat(f)

"""
    Base.Splat{F} <: Function

Represents a splatted function. That is
```julia
Base.Splat(f)(args) === f(args...)
```
The preferred way to construct an instance of `Base.Splat` is to use the [`splat`](@ref) function.

!!! compat "Julia 1.9"
    Splat requires at least Julia 1.9. In earlier versions `splat` returns an anonymous function instead.

See also [`splat`](@ref).
"""
struct Splat{F} <: Function
    f::F
    Splat(f) = new{Core.Typeof(f)}(f)
end
(s::Splat)(args) = s.f(args...)
show(io::IO, s::Splat) = (print(io, "splat("); show(io, s.f); print(io, ")"))

## in and related operators

"""
    in(collection)
    ∈(collection)

Create a function that checks whether its argument is [`in`](@ref) `collection`, i.e.
a function equivalent to `y -> y in collection`. See also [`insorted`](@ref) for use
with sorted collections.

The returned function is of type `Base.Fix2{typeof(in)}`, which can be
used to implement specialized methods.
"""
in(x) = Fix2(in, x)

function in(x, itr::Any)
    anymissing = false
    for y in itr
        v = (y == x)
        if ismissing(v)
            anymissing = true
        elseif v
            return true
        end
    end
    return anymissing ? missing : false
end

# Specialized variant of in for Tuple, which can generate typed comparisons for each element
# of the tuple, skipping values that are statically known to be != at compile time.
in(x, itr::Tuple) = _in_tuple(x, itr, false)
# This recursive function will be unrolled at compiletime, and will not generate separate
# llvm-compiled specializations for each step of the recursion.
function _in_tuple(x, @nospecialize(itr::Tuple), anymissing::Bool)
    @inline
    # Base case
    if isempty(itr)
        return anymissing ? missing : false
    end
    # Recursive case
    v = (itr[1] == x)
    if ismissing(v)
        anymissing = true
    elseif v
        return true
    end
    return _in_tuple(x, tail(itr), anymissing)
end

# fallback to the loop implementation after some number of arguments to avoid inference blowup
in(x, itr::Any32) = invoke(in, Tuple{Any,Any}, x, itr)

const ∈ = in
∉(x, itr) = !∈(x, itr)
∉(itr) = Fix2(∉, itr)

"""
    ∋(collection, item)::Bool

Like [`in`](@ref), but with arguments in reverse order.
Avoid adding methods to this function; define `in` instead.
"""
∋(itr, x) = in(x, itr)

"""
    ∋(item)

Create a function that checks whether its argument contains the given `item`, i.e.
a function equivalent to `y -> item in y`.

!!! compat "Julia 1.6"
    This method requires Julia 1.6 or later.
"""
∋(x) = Fix2(∋, x)

∌(itr, x) = !∋(itr, x)
∌(x) = Fix2(∌, x)

"""
    in(item, collection)::Bool
    ∈(item, collection)::Bool

Determine whether an item is in the given collection, in the sense that it is
[`==`](@ref) to one of the values generated by iterating over the collection.
Can equivalently be used with infix syntax:

    item in collection
    item ∈ collection

Return a `Bool` value, except if `item` is [`missing`](@ref) or `collection`
contains `missing` but not `item`, in which case `missing` is returned
([three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic),
matching the behavior of [`any`](@ref) and [`==`](@ref)).
Some collections follow a slightly different definition. For example,
[`Set`](@ref)s check whether the item [`isequal`](@ref) to one of the elements;
[`Dict`](@ref)s look for `key=>value` pairs, and the `key` is compared using
[`isequal`](@ref).

To test for the presence of a key in a dictionary, use [`haskey`](@ref)
or `k in keys(dict)`. For the collections mentioned above,
the result is always a `Bool`.

When broadcasting with `in.(items, collection)` or `items .∈ collection`, both
`items` and `collection` are broadcasted over, which is often not what is intended.
For example, if both arguments are vectors (and the dimensions match), the result is
a vector indicating whether each value in collection `items` is `in` the value at the
corresponding position in `collection`. To get a vector indicating whether each value
in `items` is in `collection`, wrap `collection` in a tuple or a `Ref` like this:
`in.(items, Ref(collection))` or `items .∈ Ref(collection)`.

See also: [`∉`](@ref), [`insorted`](@ref), [`contains`](@ref), [`occursin`](@ref), [`issubset`](@ref).

# Examples
```jldoctest
julia> a = 1:3:20
1:3:19

julia> 4 in a
true

julia> 5 in a
false

julia> missing in [1, 2]
missing

julia> 1 in [2, missing]
missing

julia> 1 in [1, missing]
true

julia> missing in Set([1, 2])
false

julia> (1=>missing) in Dict(1=>10, 2=>20)
missing

julia> [1, 2] .∈ [2, 3]
2-element BitVector:
 0
 0

julia> [1, 2] .∈ ([2, 3],)
2-element BitVector:
 0
 1
```
"""
in

"""
    ∉(item, collection)::Bool
    ∌(collection, item)::Bool

Negation of `∈` and `∋`, i.e. checks that `item` is not in `collection`.

When broadcasting with `items .∉ collection`, both `items` and `collection` are
broadcasted over, which is often not what is intended. For example, if both arguments
are vectors (and the dimensions match), the result is a vector indicating whether
each value in collection `items` is not in the value at the corresponding position
in `collection`. To get a vector indicating whether each value in `items` is not in
`collection`, wrap `collection` in a tuple or a `Ref` like this:
`items .∉ Ref(collection)`.

# Examples
```jldoctest
julia> 1 ∉ 2:4
true

julia> 1 ∉ 1:3
false

julia> [1, 2] .∉ [2, 3]
2-element BitVector:
 1
 1

julia> [1, 2] .∉ ([2, 3],)
2-element BitVector:
 1
 0
```
"""
∉, ∌

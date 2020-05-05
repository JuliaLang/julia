# This file is a part of Julia. License is MIT: https://julialang.org/license

## types ##

"""
    <:(T1, T2)

Subtype operator: returns `true` if and only if all values of type `T1` are
also of type `T2`.

# Examples
```jldoctest
julia> Float64 <: AbstractFloat
true

julia> Vector{Int} <: AbstractArray
true

julia> Matrix{Float64} <: Matrix{AbstractFloat}
false
```
"""
(<:)

"""
    >:(T1, T2)

Supertype operator, equivalent to `T2 <: T1`.
"""
(>:)(@nospecialize(a), @nospecialize(b)) = (b <: a)

"""
    supertype(T::DataType)

Return the supertype of DataType `T`.

# Examples
```jldoctest
julia> supertype(Int32)
Signed
```
"""
function supertype(T::DataType)
    @_pure_meta
    T.super
end

function supertype(T::UnionAll)
    @_pure_meta
    UnionAll(T.var, supertype(T.body))
end

## generic comparison ##

"""
    ==(x, y)

Generic equality operator. Falls back to [`===`](@ref).
Should be implemented for all types with a notion of equality, based on the abstract value
that an instance represents. For example, all numeric types are compared by numeric value,
ignoring type. Strings are compared as sequences of characters, ignoring encoding.
For collections, `==` is generally called recursively on all contents,
though other properties (like the shape for arrays) may also be taken into account.

This operator follows IEEE semantics for floating-point numbers: `0.0 == -0.0` and
`NaN != NaN`.

The result is of type `Bool`, except when one of the operands is [`missing`](@ref),
in which case `missing` is returned
([three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic)).
For collections, `missing` is returned if at least one of the operands contains
a `missing` value and all non-missing values are equal.
Use [`isequal`](@ref) or [`===`](@ref) to always get a `Bool` result.

# Implementation
New numeric types should implement this function for two arguments of the new type, and
handle comparison to other types via promotion rules where possible.

[`isequal`](@ref) falls back to `==`, so new methods of `==` will be used by the
[`Dict`](@ref) type to compare keys. If your type will be used as a dictionary key, it
should therefore also implement [`hash`](@ref).
"""
==(x, y) = x === y

"""
    isequal(x, y)

Similar to [`==`](@ref), except for the treatment of floating point numbers
and of missing values. `isequal` treats all floating-point `NaN` values as equal
to each other, treats `-0.0` as unequal to `0.0`, and [`missing`](@ref) as equal
to `missing`. Always returns a `Bool` value.

# Implementation
The default implementation of `isequal` calls `==`, so a type that does not involve
floating-point values generally only needs to define `==`.

`isequal` is the comparison function used by hash tables (`Dict`). `isequal(x,y)` must imply
that `hash(x) == hash(y)`.

This typically means that types for which a custom `==` or `isequal` method exists must
implement a corresponding `hash` method (and vice versa). Collections typically implement
`isequal` by calling `isequal` recursively on all contents.

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
```
"""
isequal(x, y) = x == y

signequal(x, y) = signbit(x)::Bool == signbit(y)::Bool
signless(x, y) = signbit(x)::Bool & !signbit(y)::Bool

isequal(x::AbstractFloat, y::AbstractFloat) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)
isequal(x::Real,          y::AbstractFloat) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)
isequal(x::AbstractFloat, y::Real         ) = (isnan(x) & isnan(y)) | signequal(x, y) & (x == y)

"""
    isless(x, y)

Test whether `x` is less than `y`, according to a fixed total order.
`isless` is not defined on all pairs of values `(x, y)`. However, if it
is defined, it is expected to satisfy the following:
- If `isless(x, y)` is defined, then so is `isless(y, x)` and `isequal(x, y)`,
  and exactly one of those three yields `true`.
- The relation defined by `isless` is transitive, i.e.,
  `isless(x, y) && isless(y, z)` implies `isless(x, z)`.

Values that are normally unordered, such as `NaN`,
are ordered in an arbitrary but consistent fashion.
[`missing`](@ref) values are ordered last.

This is the default comparison used by [`sort`](@ref).

# Implementation
Non-numeric types with a total order should implement this function.
Numeric types only need to implement it if they have special values such as `NaN`.
Types with a partial order should implement [`<`](@ref).

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


function ==(T::Type, S::Type)
    @_pure_meta
    return ccall(:jl_types_equal, Cint, (Any, Any), T, S) != 0
end
function !=(T::Type, S::Type)
    @_pure_meta
    return !(T == S)
end
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
    ===(x,y) -> Bool
    ≡(x,y) -> Bool

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
!==(@nospecialize(x), @nospecialize(y)) = !(x === y)
const ≢ = !==

"""
    <(x, y)

Less-than comparison operator. Falls back to [`isless`](@ref).
Because of the behavior of floating-point NaN values, this operator implements
a partial order.

# Implementation
New numeric types with a canonical partial order should implement this function for
two arguments of the new type.
Types with a canonical total order should implement [`isless`](@ref) instead.
(x < y) | (x == y)

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
    ifelse(condition::Bool, x, y)

Return `x` if `condition` is `true`, otherwise return `y`. This differs from `?` or `if` in
that it is an ordinary function, so all the arguments are evaluated first. In some cases,
using `ifelse` instead of an `if` statement can eliminate the branch in generated code and
provide higher performance in tight loops.

# Examples
```jldoctest
julia> ifelse(1 > 2, 1, 2)
2
```
"""
ifelse

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

Return the maximum of the arguments. See also the [`maximum`](@ref) function
to take the maximum element from a collection.

# Examples
```jldoctest
julia> max(2, 5, 1)
5
```
"""
max(x, y) = ifelse(isless(y, x), x, y)

"""
    min(x, y, ...)

Return the minimum of the arguments. See also the [`minimum`](@ref) function
to take the minimum element from a collection.

# Examples
```jldoctest
julia> min(2, 5, 1)
1
```
"""
min(x,y) = ifelse(isless(y, x), y, x)

"""
    minmax(x, y)

Return `(min(x,y), max(x,y))`. See also: [`extrema`](@ref) that returns `(minimum(x), maximum(x))`.

# Examples
```jldoctest
julia> minmax('c','b')
('b', 'c')
```
"""
minmax(x,y) = isless(y, x) ? (y, x) : (x, y)

"""
    extrema(itr) -> Tuple

Compute both the minimum and maximum element in a single pass, and return them as a 2-tuple.

# Examples
```jldoctest
julia> extrema(2:10)
(2, 10)

julia> extrema([9,pi,4.5])
(3.141592653589793, 9.0)
```
"""
extrema(itr) = _extrema_itr(identity, itr)

"""
    extrema(f, itr) -> Tuple

Compute both the minimum and maximum of `f` applied to each element in `itr` and return
them as a 2-tuple. Only one pass is made over `itr`.

!!! compat "Julia 1.2"
    This method requires Julia 1.2 or later.

# Examples
```jldoctest
julia> extrema(sin, 0:π)
(0.0, 0.9092974268256817)
```
"""
extrema(f, itr) = _extrema_itr(f, itr)

function _extrema_itr(f, itr)
    y = iterate(itr)
    y === nothing && throw(ArgumentError("collection must be non-empty"))
    (v, s) = y
    vmin = vmax = f(v)
    while true
        y = iterate(itr, s)
        y === nothing && break
        (x, s) = y
        fx = f(x)
        vmax = max(fx, vmax)
        vmin = min(fx, vmin)
    end
    return (vmin, vmax)
end

extrema(x::Real) = (x, x)
extrema(f, x::Real) = (y = f(x); (y, y))

## definitions providing basic traits of arithmetic operators ##

"""
    identity(x)

The identity function. Returns its argument.

# Examples
```jldoctest
julia> identity("Well, what did you expect?")
"Well, what did you expect?"
```
"""
identity(x) = x

+(x::Number) = x
*(x::Number) = x
(&)(x::Integer) = x
(|)(x::Integer) = x
xor(x::Integer) = x

const ⊻ = xor

# foldl for argument lists. expand recursively up to a point, then
# switch to a loop. this allows small cases like `a+b+c+d` to be inlined
# efficiently, without a major slowdown for `+(x...)` when `x` is big.
afoldl(op,a) = a
afoldl(op,a,b) = op(a,b)
afoldl(op,a,b,c...) = afoldl(op, op(a,b), c...)
function afoldl(op,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,qs...)
    y = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(a,b),c),d),e),f),g),h),i),j),k),l),m),n),o),p)
    for x in qs; y = op(y,x); end
    y
end

for op in (:+, :*, :&, :|, :xor, :min, :max, :kron)
    @eval begin
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.
        ($op)(a, b, c, xs...) = afoldl($op, ($op)(($op)(a,b),c), xs...)
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
    end
end

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
2-element Array{Float64,1}:
  6.5
 -7.0

julia> inv(A) * x
2-element Array{Float64,1}:
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
See also [`>>`](@ref), [`>>>`](@ref).
"""
function <<(x::Integer, c::Integer)
    @_inline_meta
    typemin(Int) <= c <= typemax(Int) && return x << (c % Int)
    (x >= 0 || c >= 0) && return zero(x) << 0  # for type stability
    oftype(x, -1)
end
function <<(x::Integer, c::Unsigned)
    @_inline_meta
    if c isa UInt
        throw(MethodError(<<, (x, c)))
    end
    c <= typemax(UInt) ? x << (c % UInt) : zero(x) << UInt(0)
end
<<(x::Integer, c::Int) = c >= 0 ? x << unsigned(c) : x >> unsigned(-c)

"""
    >>(x, n)

Right bit shift operator, `x >> n`. For `n >= 0`, the result is `x` shifted
right by `n` bits, where `n >= 0`, filling with `0`s if `x >= 0`, `1`s if `x <
0`, preserving the sign of `x`. This is equivalent to `fld(x, 2^n)`. For `n <
0`, this is equivalent to `x << -n`.

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
    @_inline_meta
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
shifted right by `n` bits, where `n >= 0`, filling with `0`s. For `n < 0`, this
is equivalent to `x << -n`.

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
    @_inline_meta
    typemin(Int) <= c <= typemax(Int) ? x >>> (c % Int) : zero(x) >>> 0
end
function >>>(x::Integer, c::Unsigned)
    @_inline_meta
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

# Examples
```jldoctest
julia> x = 15; y = 4;

julia> x % y
3

julia> x == div(x, y) * y + rem(x, y)
true
```
"""
rem
const % = rem

"""
    div(x, y)
    ÷(x, y)

The quotient from Euclidean division. Computes `x/y`, truncated to an integer.

# Examples
```jldoctest
julia> 9 ÷ 4
2

julia> -5 ÷ 3
-1

julia> 5.0 ÷ 2
2.0
```
"""
div
const ÷ = div

"""
    mod1(x, y)

Modulus after flooring division, returning a value `r` such that `mod(r, y) == mod(x, y)`
in the range ``(0, y]`` for positive `y` and in the range ``[y,0)`` for negative `y`.

See also: [`fld1`](@ref), [`fldmod1`](@ref).

# Examples
```jldoctest
julia> mod1(4, 2)
2

julia> mod1(4, 3)
1
```
"""
mod1(x::T, y::T) where {T<:Real} = (m = mod(x, y); ifelse(m == 0, y, m))


"""
    fld1(x, y)

Flooring division, returning a value consistent with `mod1(x,y)`

See also: [`mod1`](@ref), [`fldmod1`](@ref).

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
fld1(x::T, y::T) where {T<:Real} = (m = mod1(x, y); fld(x + y - m, y))
function fld1(x::T, y::T) where T<:Integer
    d = div(x, y)
    return d + (!signbit(x ⊻ y) & (d * y != x))
end

"""
    fldmod1(x, y)

Return `(fld1(x,y), mod1(x,y))`.

See also: [`fld1`](@ref), [`mod1`](@ref).
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

# function pipelining

"""
    |>(x, f)

Applies a function to the preceding argument. This allows for easy function chaining.

# Examples
```jldoctest
julia> [1:5;] |> x->x.^2 |> sum |> inv
0.01818181818181818
```
"""
|>(x, f) = f(x)

# function composition

"""
    f ∘ g

Compose functions: i.e. `(f ∘ g)(args...)` means `f(g(args...))`. The `∘` symbol can be
entered in the Julia REPL (and most editors, appropriately configured) by typing `\\circ<tab>`.

Function composition also works in prefix form: `∘(f, g)` is the same as `f ∘ g`.
The prefix form supports composition of multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
and splatting `∘(fs...)` for composing an iterable collection of functions.

!!! compat "Julia 1.4"
    Multiple function composition requires at least Julia 1.4.

!!! compat "Julia 1.5"
    Composition of one function ∘(f)  requires at least Julia 1.5.

# Examples
```jldoctest
julia> map(uppercase∘first, ["apple", "banana", "carrot"])
3-element Array{Char,1}:
 'A': ASCII/Unicode U+0041 (category Lu: Letter, uppercase)
 'B': ASCII/Unicode U+0042 (category Lu: Letter, uppercase)
 'C': ASCII/Unicode U+0043 (category Lu: Letter, uppercase)

julia> fs = [
           x -> 2x
           x -> x/2
           x -> x-1
           x -> x+1
       ];

julia> ∘(fs...)(3)
3.0
```
"""
function ∘ end
∘(f) = f
∘(f, g) = (x...)->f(g(x...))
∘(f, g, h...) = ∘(f ∘ g, h...)

"""
    !f::Function

Predicate function negation: when the argument of `!` is a function, it returns a
function which computes the boolean negation of `f`.

# Examples
```jldoctest
julia> str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
"∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

julia> filter(isletter, str)
"εδxyδfxfyε"

julia> filter(!isletter, str)
"∀  > 0, ∃  > 0: |-| <  ⇒ |()-()| < "
```
"""
!(f::Function) = (x...)->!f(x...)

"""
    Fix1(f, x)

A type representing a partially-applied version of the two-argument function
`f`, with the first argument fixed to the value "x". In other words,
`Fix1(f, x)` behaves similarly to `y->f(x, y)`.
"""
struct Fix1{F,T} <: Function
    f::F
    x::T

    Fix1(f::F, x::T) where {F,T} = new{F,T}(f, x)
    Fix1(f::Type{F}, x::T) where {F,T} = new{Type{F},T}(f, x)
end

(f::Fix1)(y) = f.f(f.x, y)

"""
    Fix2(f, x)

A type representing a partially-applied version of the two-argument function
`f`, with the second argument fixed to the value "x". In other words,
`Fix2(f, x)` behaves similarly to `y->f(y, x)`.
"""
struct Fix2{F,T} <: Function
    f::F
    x::T

    Fix2(f::F, x::T) where {F,T} = new{F,T}(f, x)
    Fix2(f::Type{F}, x::T) where {F,T} = new{Type{F},T}(f, x)
end

(f::Fix2)(y) = f.f(y, f.x)

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

Defined as
```julia
    splat(f) = args->f(args...)
```
i.e. given a function returns a new function that takes one argument and splats
its argument into the original function. This is useful as an adaptor to pass
a multi-argument function in a context that expects a single argument, but
passes a tuple as that single argument.

# Example usage:
```jldoctest
julia> map(Base.splat(+), zip(1:3,4:6))
3-element Array{Int64,1}:
 5
 7
 9
```
"""
splat(f) = args->f(args...)

## in & contains

"""
    in(x)

Create a function that checks whether its argument is [`in`](@ref) `x`, i.e.
a function equivalent to `y -> y in x`.

The returned function is of type `Base.Fix2{typeof(in)}`, which can be
used to implement specialized methods.
"""
in(x) = Fix2(in, x)

function in(x, itr)
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

const ∈ = in
∋(itr, x) = ∈(x, itr)
∉(x, itr) = !∈(x, itr)
∌(itr, x) = !∋(itr, x)

"""
    in(item, collection) -> Bool
    ∈(item, collection) -> Bool
    ∋(collection, item) -> Bool

Determine whether an item is in the given collection, in the sense that it is
[`==`](@ref) to one of the values generated by iterating over the collection.
Returns a `Bool` value, except if `item` is [`missing`](@ref) or `collection`
contains `missing` but not `item`, in which case `missing` is returned
([three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic),
matching the behavior of [`any`](@ref) and [`==`](@ref)).

Some collections follow a slightly different definition. For example,
[`Set`](@ref)s check whether the item [`isequal`](@ref) to one of the elements.
[`Dict`](@ref)s look for `key=>value` pairs, and the key is compared using
[`isequal`](@ref). To test for the presence of a key in a dictionary,
use [`haskey`](@ref) or `k in keys(dict)`. For these collections, the result
is always a `Bool` and never `missing`.

To determine whether an item is not in a given collection, see [`:∉`](@ref).
You may also negate the `in` by doing `!(a in b)` which is logically similar to "not in".

When broadcasting with `in.(items, collection)` or `items .∈ collection`, both
`item` and `collection` are broadcasted over, which is often not what is intended.
For example, if both arguments are vectors (and the dimensions match), the result is
a vector indicating whether each value in collection `items` is `in` the value at the
corresponding position in `collection`. To get a vector indicating whether each value
in `items` is in `collection`, wrap `collection` in a tuple or a `Ref` like this:
`in.(items, Ref(collection))` or `items .∈ Ref(collection)`.

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

julia> !(21 in a)
true

julia> !(19 in a)
false

julia> [1, 2] .∈ [2, 3]
2-element BitArray{1}:
 0
 0

julia> [1, 2] .∈ ([2, 3],)
2-element BitArray{1}:
 0
 1
```
"""
in, ∋

"""
    ∉(item, collection) -> Bool
    ∌(collection, item) -> Bool

Negation of `∈` and `∋`, i.e. checks that `item` is not in `collection`.

When broadcasting with `items .∉ collection`, both `item` and `collection` are
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
2-element BitArray{1}:
 1
 1

julia> [1, 2] .∉ ([2, 3],)
2-element BitArray{1}:
 1
 0
```
"""
∉, ∌

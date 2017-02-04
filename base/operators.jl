# This file is a part of Julia. License is MIT: http://julialang.org/license

## types ##

typealias Dims{N} NTuple{N,Int}
typealias DimsInteger{N} NTuple{N,Integer}
typealias Indices{N} NTuple{N,AbstractUnitRange}

"""
    <:(T1, T2)

Subtype operator, equivalent to `issubtype(T1,T2)`.

```jldoctest
julia> Float64 <: AbstractFloat
true

julia> Vector{Int} <: AbstractArray
true

julia> Matrix{Float64} <: Matrix{AbstractFloat}
false
```
"""
const (<:) = issubtype

"""
    supertype(T::DataType)

Return the supertype of DataType `T`.

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

==(x, y) = x === y

"""
    isequal(x, y)

Similar to `==`, except treats all floating-point `NaN` values as equal to each other, and
treats `-0.0` as unequal to `0.0`. The default implementation of `isequal` calls `==`, so if
you have a type that doesn't have these floating-point subtleties then you probably only
need to define `==`.

`isequal` is the comparison function used by hash tables (`Dict`). `isequal(x,y)` must imply
that `hash(x) == hash(y)`.

This typically means that if you define your own `==` function then you must define a
corresponding `hash` (and vice versa). Collections typically implement `isequal` by calling
`isequal` recursively on all contents.

Scalar types generally do not need to implement `isequal` separate from `==`, unless they
represent floating-point numbers amenable to a more efficient implementation than that
provided as a generic fallback (based on `isnan`, `signbit`, and `==`).

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

isequal(x::AbstractFloat, y::AbstractFloat) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)
isequal(x::Real,          y::AbstractFloat) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)
isequal(x::AbstractFloat, y::Real         ) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)

isless(x::AbstractFloat, y::AbstractFloat) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)
isless(x::Real,          y::AbstractFloat) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)
isless(x::AbstractFloat, y::Real         ) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)

function ==(T::Type, S::Type)
    @_pure_meta
    typeseq(T, S)
end
function !=(T::Type, S::Type)
    @_pure_meta
    !(T == S)
end
==(T::TypeVar, S::Type) = false
==(T::Type, S::TypeVar) = false

## comparison fallbacks ##

"""
    !=(x, y)
    ≠(x,y)

Not-equals comparison operator. Always gives the opposite answer as `==`. New types should
generally not implement this, and rely on the fallback definition `!=(x,y) = !(x==y)` instead.

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
them. Compares mutable objects by address in memory, and compares immutable objects (such as
numbers) by contents at the bit level. This function is sometimes called `egal`.

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

Equivalent to `!(x === y)`.

```jldoctest
julia> a = [1, 2]; b = [1, 2];

julia> a ≢ b
true

julia> a ≢ a
false
```
"""
!==(x, y) = !(x === y)
const ≢ = !==

"""
    <(x, y)

Less-than comparison operator. New numeric types should implement this function for two
arguments of the new type. Because of the behavior of floating-point NaN values, `<`
implements a partial order. Types with a canonical partial order should implement `<`, and
types with a canonical total order should implement `isless`.

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

Greater-than comparison operator. Generally, new types should implement `<` instead of this
function, and rely on the fallback definition `>(x, y) = y < x`.

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

Less-than-or-equals comparison operator.

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
<=(x, y) = !(y < x)
const ≤ = <=

"""
    >=(x, y)
    ≥(x,y)

Greater-than-or-equals comparison operator.

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
lexcmp(x::Real, y::Real) = isless(x,y) ? -1 : ifelse(isless(y,x), 1, 0)

"""
    ifelse(condition::Bool, x, y)

Return `x` if `condition` is `true`, otherwise return `y`. This differs from `?` or `if` in
that it is an ordinary function, so all the arguments are evaluated first. In some cases,
using `ifelse` instead of an `if` statement can eliminate the branch in generated code and
provide higher performance in tight loops.

```jldoctest
julia> ifelse(1 > 2, 1, 2)
2
```
"""
ifelse(c::Bool, x, y) = select_value(c, x, y)

"""
    cmp(x,y)

Return -1, 0, or 1 depending on whether `x` is less than, equal to, or greater than `y`,
respectively. Uses the total order implemented by `isless`. For floating-point numbers, uses `<`
but throws an error for unordered arguments.

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
    lexcmp(x, y)

Compare `x` and `y` lexicographically and return -1, 0, or 1 depending on whether `x` is
less than, equal to, or greater than `y`, respectively. This function should be defined for
lexicographically comparable types, and `lexless` will call `lexcmp` by default.

```jldoctest
julia> lexcmp("abc", "abd")
-1

julia> lexcmp("abc", "abc")
0
```
"""
lexcmp(x, y) = cmp(x, y)

"""
    lexless(x, y)

Determine whether `x` is lexicographically less than `y`.

```jldoctest
julia> lexless("abc", "abd")
true
```
"""
lexless(x, y) = lexcmp(x,y) < 0

# cmp returns -1, 0, +1 indicating ordering
cmp(x::Integer, y::Integer) = ifelse(isless(x, y), -1, ifelse(isless(y, x), 1, 0))

"""
    max(x, y, ...)

Return the maximum of the arguments. See also the [`maximum`](@ref) function
to take the maximum element from a collection.

```jldoctest
julia> max(2, 5, 1)
5
```
"""
max(x, y) = ifelse(y < x, x, y)

"""
    min(x, y, ...)

Return the minimum of the arguments. See also the [`minimum`](@ref) function
to take the minimum element from a collection.

```jldoctest
julia> min(2, 5, 1)
1
```
"""
min(x,y) = ifelse(y < x, y, x)

"""
    minmax(x, y)

Return `(min(x,y), max(x,y))`. See also: [`extrema`](@ref) that returns `(minimum(x), maximum(x))`.

```jldoctest
julia> minmax('c','b')
('b', 'c')
```
"""
minmax(x,y) = y < x ? (y, x) : (x, y)

scalarmax(x,y) = max(x,y)
scalarmax(x::AbstractArray, y::AbstractArray) = throw(ArgumentError("ordering is not well-defined for arrays"))
scalarmax(x               , y::AbstractArray) = throw(ArgumentError("ordering is not well-defined for arrays"))
scalarmax(x::AbstractArray, y               ) = throw(ArgumentError("ordering is not well-defined for arrays"))

scalarmin(x,y) = min(x,y)
scalarmin(x::AbstractArray, y::AbstractArray) = throw(ArgumentError("ordering is not well-defined for arrays"))
scalarmin(x               , y::AbstractArray) = throw(ArgumentError("ordering is not well-defined for arrays"))
scalarmin(x::AbstractArray, y               ) = throw(ArgumentError("ordering is not well-defined for arrays"))

## definitions providing basic traits of arithmetic operators ##

"""
    identity(x)

The identity function. Returns its argument.

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

"""
    \\(x, y)

Left division operator: multiplication of `y` by the inverse of `x` on the left. Gives
floating-point results for integer arguments.

```jldoctest
julia> 3 \\ 6
2.0

julia> inv(3) * 6
2.0

julia> A = [1 2; 3 4]; x = [5, 6];

julia> A \\ x
2-element Array{Float64,1}:
 -4.0
  4.5

julia> inv(A) * x
2-element Array{Float64,1}:
 -4.0
  4.5
```
"""
\(x,y) = (y'/x')'

# Core <<, >>, and >>> take either Int or UInt as second arg. Signed shift
# counts can shift in either direction, and are translated here to unsigned
# counts. Integer datatypes only need to implement the unsigned version.

"""
    <<(x, n)

Left bit shift operator, `x << n`. For `n >= 0`, the result is `x` shifted left
by `n` bits, filling with `0`s. This is equivalent to `x * 2^n`. For `n < 0`,
this is equivalent to `x >> -n`.

```jldoctest
julia> Int8(3) << 2
12

julia> bits(Int8(3))
"00000011"

julia> bits(Int8(12))
"00001100"
```
See also [`>>`](@ref), [`>>>`](@ref).
"""
function <<(x::Integer, c::Integer)
    typemin(Int) <= c <= typemax(Int) && return x << (c % Int)
    (x >= 0 || c >= 0) && return zero(x)
    oftype(x, -1)
end
<<(x::Integer, c::Unsigned) = c <= typemax(UInt) ? x << (c % UInt) : zero(x)
<<(x::Integer, c::Int) = c >= 0 ? x << unsigned(c) : x >> unsigned(-c)

"""
    >>(x, n)

Right bit shift operator, `x >> n`. For `n >= 0`, the result is `x` shifted
right by `n` bits, where `n >= 0`, filling with `0`s if `x >= 0`, `1`s if `x <
0`, preserving the sign of `x`. This is equivalent to `fld(x, 2^n)`. For `n <
0`, this is equivalent to `x << -n`.


```jldoctest
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
```
See also [`>>>`](@ref), [`<<`](@ref).
"""
function >>(x::Integer, c::Integer)
    typemin(Int) <= c <= typemax(Int) && return x >> (c % Int)
    (x >= 0 || c < 0) && return zero(x)
    oftype(x, -1)
end
>>(x::Integer, c::Unsigned) = c <= typemax(UInt) ? x >> (c % UInt) : zero(x)
>>(x::Integer, c::Int) = c >= 0 ? x >> unsigned(c) : x << unsigned(-c)

"""
    >>>(x, n)

Unsigned right bit shift operator, `x >>> n`. For `n >= 0`, the result is `x`
shifted right by `n` bits, where `n >= 0`, filling with `0`s. For `n < 0`, this
is equivalent to `x << -n`.

For `Unsigned` integer types, this is equivalent to [`>>`](@ref). For
`Signed` integer types, this is equivalent to `signed(unsigned(x) >> n)`.

```jldoctest
julia> Int8(-14) >>> 2
60

julia> bits(Int8(-14))
"11110010"

julia> bits(Int8(60))
"00111100"
```
`BigInt`s are treated as if having infinite size, so no filling is required and this
is equivalent to [`>>`](@ref).

See also [`>>`](@ref), [`<<`](@ref).
"""
>>>(x::Integer, c::Integer) =
    typemin(Int) <= c <= typemax(Int) ? x >>> (c % Int) : zero(x)
>>>(x::Integer, c::Unsigned) = c <= typemax(UInt) ? x >>> (c % UInt) : zero(x)
>>>(x::Integer, c::Int) = c >= 0 ? x >>> unsigned(c) : x << unsigned(-c)

# fallback div, fld, and cld implementations
# NOTE: C89 fmod() and x87 FPREM implicitly provide truncating float division,
# so it is used here as the basis of float div().
div{T<:Real}(x::T, y::T) = convert(T,round((x-rem(x,y))/y))

"""
    fld(x, y)

Largest integer less than or equal to `x/y`.

```jldoctest
julia> fld(7.3,5.5)
1.0
```
"""
fld{T<:Real}(x::T, y::T) = convert(T,round((x-mod(x,y))/y))

"""
    cld(x, y)

Smallest integer larger than or equal to `x/y`.
```jldoctest
julia> cld(5.5,2.2)
3.0
```
"""
cld{T<:Real}(x::T, y::T) = convert(T,round((x-modCeil(x,y))/y))
#rem{T<:Real}(x::T, y::T) = convert(T,x-y*trunc(x/y))
#mod{T<:Real}(x::T, y::T) = convert(T,x-y*floor(x/y))
modCeil{T<:Real}(x::T, y::T) = convert(T,x-y*ceil(x/y))

# operator alias

"""
    rem(x, y)
    %(x, y)

Remainder from Euclidean division, returning a value of the same sign as `x`, and smaller in
magnitude than `y`. This value is always exact.

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

```jldoctest
julia> 9 ÷ 4
2

julia> -5 ÷ 3
-1
```
"""
div
const ÷ = div

"""
    mod1(x, y)

Modulus after flooring division, returning a value `r` such that `mod(r, y) == mod(x, y)`
in the range ``(0, y]`` for positive `y` and in the range ``[y,0)`` for negative `y`.

```jldoctest
julia> mod1(4, 2)
2

julia> mod1(4, 3)
1
```
"""
mod1{T<:Real}(x::T, y::T) = (m = mod(x, y); ifelse(m == 0, y, m))
# efficient version for integers
mod1{T<:Integer}(x::T, y::T) = mod(x + y - T(1), y) + T(1)


"""
    fld1(x, y)

Flooring division, returning a value consistent with `mod1(x,y)`

See also: [`mod1`](@ref).

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
fld1{T<:Real}(x::T, y::T) = (m=mod(x,y); fld(x-m,y))
# efficient version for integers
fld1{T<:Integer}(x::T, y::T) = fld(x+y-T(1),y)

"""
    fldmod1(x, y)

Return `(fld1(x,y), mod1(x,y))`.

See also: [`fld1`](@ref), [`mod1`](@ref).
"""
fldmod1{T<:Real}(x::T, y::T) = (fld1(x,y), mod1(x,y))
# efficient version for integers
fldmod1{T<:Integer}(x::T, y::T) = (fld1(x,y), mod1(x,y))

# transpose

"""
    ctranspose(A)

The conjugate transposition operator (`'`).

# Example

```jldoctest
julia> A =  [3+2im 9+2im; 8+7im  4+6im]
2×2 Array{Complex{Int64},2}:
 3+2im  9+2im
 8+7im  4+6im

julia> ctranspose(A)
2×2 Array{Complex{Int64},2}:
 3-2im  8-7im
 9-2im  4-6im
```
"""
ctranspose(x) = conj(transpose(x))
conj(x) = x

# transposed multiply

"""
    Ac_mul_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ⋅B``.
"""
Ac_mul_B(a,b)  = ctranspose(a)*b

"""
    A_mul_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A⋅Bᴴ``.
"""
A_mul_Bc(a,b)  = a*ctranspose(b)

"""
    Ac_mul_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ Bᴴ``.
"""
Ac_mul_Bc(a,b) = ctranspose(a)*ctranspose(b)

"""
    At_mul_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ⋅B``.
"""
At_mul_B(a,b)  = transpose(a)*b

"""
    A_mul_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A⋅Bᵀ``.
"""
A_mul_Bt(a,b)  = a*transpose(b)

"""
    At_mul_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ⋅Bᵀ``.
"""
At_mul_Bt(a,b) = transpose(a)*transpose(b)

# transposed divide

"""
    Ac_rdiv_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ / B``.
"""
Ac_rdiv_B(a,b)  = ctranspose(a)/b

"""
    A_rdiv_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A / Bᴴ``.
"""
A_rdiv_Bc(a,b)  = a/ctranspose(b)

"""
    Ac_rdiv_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ / Bᴴ``.
"""
Ac_rdiv_Bc(a,b) = ctranspose(a)/ctranspose(b)

"""
    At_rdiv_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ / B``.
"""
At_rdiv_B(a,b)  = transpose(a)/b

"""
    A_rdiv_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A / Bᵀ``.
"""
A_rdiv_Bt(a,b)  = a/transpose(b)

"""
    At_rdiv_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ / Bᵀ``.
"""
At_rdiv_Bt(a,b) = transpose(a)/transpose(b)

"""
    Ac_ldiv_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ`` \\ ``B``.
"""
Ac_ldiv_B(a,b)  = ctranspose(a)\b

"""
    A_ldiv_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A`` \\ ``Bᴴ``.
"""
A_ldiv_Bc(a,b)  = a\ctranspose(b)

"""
    Ac_ldiv_Bc(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ`` \\ ``Bᴴ``.
"""
Ac_ldiv_Bc(a,b) = ctranspose(a)\ctranspose(b)

"""
    At_ldiv_B(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ`` \\ ``B``.
"""
At_ldiv_B(a,b)  = transpose(a)\b

"""
    A_ldiv_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``A`` \\ ``Bᵀ``.
"""
A_ldiv_Bt(a,b)  = a\transpose(b)

"""
    At_ldiv_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᵀ`` \\ ``Bᵀ``.
"""
At_ldiv_Bt(a,b) = At_ldiv_B(a,transpose(b))

"""
    Ac_ldiv_Bt(A, B)

For matrices or vectors ``A`` and ``B``, calculates ``Aᴴ`` \\ ``Bᵀ``.
"""
Ac_ldiv_Bt(a,b) = Ac_ldiv_B(a,transpose(b))

widen{T<:Number}(x::T) = convert(widen(T), x)

"""
    eltype(type)

Determine the type of the elements generated by iterating a collection of the given `type`.
For associative collection types, this will be a `Pair{KeyType,ValType}`. The definition
`eltype(x) = eltype(typeof(x))` is provided for convenience so that instances can be passed
instead of types. However the form that accepts a type argument should be defined for new
types.

```jldoctest
julia> eltype(ones(Float32,2,2))
Float32

julia> eltype(ones(Int8,2,2))
Int8
```
"""
eltype(::Type) = Any
eltype(::Type{Any}) = Any
eltype(t::DataType) = eltype(supertype(t))
eltype(x) = eltype(typeof(x))

# function pipelining

"""
    |>(x, f)

Applies a function to the preceding argument. This allows for easy function chaining.

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
Example:

```jldoctest
julia> map(uppercase∘hex, 250:255)
6-element Array{String,1}:
 "FA"
 "FB"
 "FC"
 "FD"
 "FE"
 "FF"
```
"""
∘(f, g) = (x...)->f(g(x...))


"""
    !f::Function

Predicate function negation: when the argument of `!` is a function, it returns a
function which computes the boolean negation of `f`. Example:

```jldoctest
julia> str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
"∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

julia> filter(isalpha, str)
"εδxyδfxfyε"

julia> filter(!isalpha, str)
"∀  > 0, ∃  > 0: |-| <  ⇒ |()-()| < "
```
"""
!(f::Function) = (x...)->!f(x...)

# array shape rules

promote_shape(::Tuple{}, ::Tuple{}) = ()

function promote_shape(a::Tuple{Int,}, b::Tuple{Int,})
    if a[1] != b[1]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

function promote_shape(a::Tuple{Int,Int}, b::Tuple{Int,})
    if a[1] != b[1] || a[2] != 1
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

promote_shape(a::Tuple{Int,}, b::Tuple{Int,Int}) = promote_shape(b, a)

function promote_shape(a::Tuple{Int, Int}, b::Tuple{Int, Int})
    if a[1] != b[1] || a[2] != b[2]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

"""
    promote_shape(s1, s2)

Check two array shapes for compatibility, allowing trailing singleton dimensions, and return
whichever shape has more dimensions.

```jldoctest
julia> a = ones(3,4,1,1,1);

julia> b = ones(3,4);

julia> promote_shape(a,b)
(Base.OneTo(3), Base.OneTo(4), Base.OneTo(1), Base.OneTo(1), Base.OneTo(1))

julia> promote_shape((2,3,1,4), (2, 3, 1, 4, 1))
(2, 3, 1, 4, 1)
```
"""
function promote_shape(a::Dims, b::Dims)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    return a
end

function promote_shape(a::AbstractArray, b::AbstractArray)
    promote_shape(indices(a), indices(b))
end

function promote_shape(a::Indices, b::Indices)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1:1
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    return a
end

function throw_setindex_mismatch(X, I)
    if length(I) == 1
        throw(DimensionMismatch("tried to assign $(length(X)) elements to $(I[1]) destinations"))
    else
        throw(DimensionMismatch("tried to assign $(dims2string(size(X))) array to $(dims2string(I)) destination"))
    end
end

# check for valid sizes in A[I...] = X where X <: AbstractArray
# we want to allow dimensions that are equal up to permutation, but only
# for permutations that leave array elements in the same linear order.
# those are the permutations that preserve the order of the non-singleton
# dimensions.
function setindex_shape_check(X::AbstractArray, I::Integer...)
    li = ndims(X)
    lj = length(I)
    i = j = 1
    while true
        ii = length(indices(X,i))
        jj = I[j]
        if i == li || j == lj
            while i < li
                i += 1
                ii *= length(indices(X,i))
            end
            while j < lj
                j += 1
                jj *= I[j]
            end
            if ii != jj
                throw_setindex_mismatch(X, I)
            end
            return
        end
        if ii == jj
            i += 1
            j += 1
        elseif ii == 1
            i += 1
        elseif jj == 1
            j += 1
        else
            throw_setindex_mismatch(X, I)
        end
    end
end

setindex_shape_check(X::AbstractArray) =
    (_length(X)==1 || throw_setindex_mismatch(X,()))

setindex_shape_check(X::AbstractArray, i::Integer) =
    (_length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check{T}(X::AbstractArray{T,1}, i::Integer) =
    (_length(X)==i || throw_setindex_mismatch(X, (i,)))

setindex_shape_check{T}(X::AbstractArray{T,1}, i::Integer, j::Integer) =
    (_length(X)==i*j || throw_setindex_mismatch(X, (i,j)))

function setindex_shape_check{T}(X::AbstractArray{T,2}, i::Integer, j::Integer)
    if length(X) != i*j
        throw_setindex_mismatch(X, (i,j))
    end
    sx1 = length(indices(X,1))
    if !(i == 1 || i == sx1 || sx1 == 1)
        throw_setindex_mismatch(X, (i,j))
    end
end
setindex_shape_check(X, I...) = nothing # Non-arrays broadcast to all idxs

# convert to a supported index type (array or Int)
"""
    to_index(A, i)

Convert index `i` to an `Int` or array of indices to be used as an index into array `A`.

Custom array types may specialize `to_index(::CustomArray, i)` to provide
special indexing behaviors. Note that some index types (like `Colon`) require
more context in order to transform them into an array of indices; those get
converted in the more complicated `to_indices` function. By default, this
simply calls the generic `to_index(i)`. This must return either an `Int` or an
`AbstractArray` of scalar indices that are supported by `A`.
"""
to_index(A, i) = to_index(i)

"""
    to_index(i)

Convert index `i` to an `Int` or array of `Int`s to be used as an index for all arrays.

Custom index types may specialize `to_index(::CustomIndex)` to provide special
indexing behaviors. This must return either an `Int` or an `AbstractArray` of
`Int`s.
"""
to_index(i::Integer) = convert(Int,i)::Int
to_index(I::AbstractArray{Bool}) = LogicalIndex(I)
to_index(I::AbstractArray) = I
to_index{T<:Union{AbstractArray, Colon}}(I::AbstractArray{T}) = throw(ArgumentError("invalid index: $I"))
to_index(::Colon) = throw(ArgumentError("colons must be converted by to_indices(...)"))
to_index(i) = throw(ArgumentError("invalid index: $i"))

# The general to_indices is mostly defined in multidimensional.jl, but this
# definition is required for bootstrap:
"""
    to_indices(A, I::Tuple)

Convert the tuple `I` to a tuple of indices for use in indexing into array `A`.

The returned tuple must only contain either `Int`s or `AbstractArray`s of
scalar indices that are supported by array `A`. It will error upon encountering
a novel index type that it does not know how to process.

For simple index types, it defers to the unexported `Base.to_index(A, i)` to
process each index `i`. While this internal function is not intended to be
called directly, `Base.to_index` may be extended by custom array or index types
to provide custom indexing behaviors.

More complicated index types may require more context about the dimension into
which they index. To support those cases, `to_indices(A, I)` calls
`to_indices(A, indices(A), I)`, which then recursively walks through both the
given tuple of indices and the dimensional indices of `A` in tandem. As such,
not all index types are guaranteed to propagate to `Base.to_index`.
"""
to_indices(A, I::Tuple) = (@_inline_meta; to_indices(A, indices(A), I))
to_indices(A, inds, ::Tuple{}) = ()
to_indices(A, inds, I::Tuple{Any, Vararg{Any}}) =
    (@_inline_meta; (to_index(A, I[1]), to_indices(A, _maybetail(inds), tail(I))...))

_maybetail(::Tuple{}) = ()
_maybetail(t::Tuple) = tail(t)

"""
   Slice(indices)

Represent an AbstractUnitRange of indices as a vector of the indices themselves.

Upon calling `to_indices()`, Colons are converted to Slice objects to represent
the indices over which the Colon spans. Slice objects are themselves unit
ranges with the same indices as those they wrap. This means that indexing into
Slice objects with an integer always returns that exact integer, and they
iterate over all the wrapped indices, even supporting offset indices.
"""
immutable Slice{T<:AbstractUnitRange} <: AbstractUnitRange{Int}
    indices::T
end
indices(S::Slice) = (S.indices,)
unsafe_indices(S::Slice) = (S.indices,)
indices1(S::Slice) = S.indices
first(S::Slice) = first(S.indices)
last(S::Slice) = last(S.indices)
errmsg(A) = error("size not supported for arrays with indices $(indices(A)); see http://docs.julialang.org/en/latest/devdocs/offset-arrays/")
size(S::Slice) = first(S.indices) == 1 ? (length(S.indices),) : errmsg(S)
length(S::Slice) = first(S.indices) == 1 ? length(S.indices) : errmsg(S)
unsafe_length(S::Slice) = first(S.indices) == 1 ? unsafe_length(S.indices) : errmsg(S)
getindex(S::Slice, i::Int) = (@_inline_meta; @boundscheck checkbounds(S, i); i)
show(io::IO, r::Slice) = print(io, "Base.Slice(", r.indices, ")")
start(S::Slice) = start(S.indices)
next(S::Slice, s) = next(S.indices, s)
done(S::Slice, s) = done(S.indices, s)

# Addition/subtraction of ranges
for f in (:+, :-)
    @eval begin
        function $f(r1::OrdinalRange, r2::OrdinalRange)
            r1l = length(r1)
            (r1l == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match")))
            range($f(first(r1),first(r2)), $f(step(r1),step(r2)), r1l)
        end

        function $f{T}(r1::LinSpace{T}, r2::LinSpace{T})
            len = r1.len
            (len == r2.len ||
             throw(DimensionMismatch("argument dimensions must match")))
            linspace(convert(T, $f(first(r1), first(r2))),
                     convert(T, $f(last(r1), last(r2))), len)
        end

        $f(r1::Union{StepRangeLen, OrdinalRange, LinSpace},
           r2::Union{StepRangeLen, OrdinalRange, LinSpace}) =
               $f(promote_noncircular(r1, r2)...)
    end
end

function +{T,S}(r1::StepRangeLen{T,S}, r2::StepRangeLen{T,S})
    len = length(r1)
    (len == length(r2) ||
        throw(DimensionMismatch("argument dimensions must match")))
    StepRangeLen(first(r1)+first(r2), step(r1)+step(r2), len)
end

-(r1::StepRangeLen, r2::StepRangeLen) = +(r1, -r2)

# Pair

immutable Pair{A,B}
    first::A
    second::B
end

const => = Pair

start(p::Pair) = 1
done(p::Pair, i) = i>2
next(p::Pair, i) = (getfield(p,i), i+1)
eltype{A,B}(p::Pair{A,B}) = Union{A,B}

indexed_next(p::Pair, i::Int, state) = (getfield(p,i), i+1)

hash(p::Pair, h::UInt) = hash(p.second, hash(p.first, h))

==(p::Pair, q::Pair) = (p.first==q.first) & (p.second==q.second)
isequal(p::Pair, q::Pair) = isequal(p.first,q.first) & isequal(p.second,q.second)

isless(p::Pair, q::Pair) = ifelse(!isequal(p.first,q.first), isless(p.first,q.first),
                                                             isless(p.second,q.second))
getindex(p::Pair,i::Int) = getfield(p,i)
getindex(p::Pair,i::Real) = getfield(p, convert(Int, i))
reverse{A,B}(p::Pair{A,B}) = Pair{B,A}(p.second, p.first)

endof(p::Pair) = 2
length(p::Pair) = 2
first(p::Pair) = p.first
last(p::Pair) = p.second

convert{A,B}(::Type{Pair{A,B}}, x::Pair{A,B}) = x
function convert{A,B}(::Type{Pair{A,B}}, x::Pair)
    Pair{A, B}(convert(A, x[1]), convert(B, x[2]))
end

promote_rule{A1, B1, A2, B2}(::Type{Pair{A1, B1}}, ::Type{Pair{A2, B2}}) =
    Pair{promote_type(A1, A2), promote_type(B1, B2)}

# some operators not defined yet
global //, >:, <|, hcat, hvcat, ⋅, ×, ∈, ∉, ∋, ∌, ⊆, ⊈, ⊊, ∩, ∪, √, ∛

this_module = current_module()
baremodule Operators

export
    !,
    !=,
    !==,
    ===,
    xor,
    %,
    ÷,
    &,
    *,
    +,
    -,
    /,
    //,
    <,
    <:,
    >:,
    <<,
    <=,
    ==,
    >,
    >=,
    ≥,
    ≤,
    ≠,
    >>,
    >>>,
    \,
    ^,
    |,
    |>,
    <|,
    ~,
    ⋅,
    ×,
    ∈,
    ∉,
    ∋,
    ∌,
    ⊆,
    ⊈,
    ⊊,
    ∩,
    ∪,
    √,
    ∛,
    ⊻,
    ∘,
    colon,
    hcat,
    vcat,
    hvcat,
    getindex,
    setindex!,
    transpose,
    ctranspose

import ..this_module: !, !=, xor, %, ÷, &, *, +, -,
    /, //, <, <:, <<, <=, ==, >, >=, >>, >>>,
    <|, |>, \, ^, |, ~, !==, ===, >:, colon, hcat, vcat, hvcat, getindex, setindex!,
    transpose, ctranspose,
    ≥, ≤, ≠, ⋅, ×, ∈, ∉, ∋, ∌, ⊆, ⊈, ⊊, ∩, ∪, √, ∛, ⊻, ∘

end

# This file is a part of Julia. License is MIT: https://julialang.org/license

## reductions ##

###### Generic (map)reduce functions ######

if Int === Int32
    const SmallSigned = Union{Int8,Int16}
    const SmallUnsigned = Union{UInt8,UInt16}
else
    const SmallSigned = Union{Int8,Int16,Int32}
    const SmallUnsigned = Union{UInt8,UInt16,UInt32}
end

abstract type AbstractBroadcasted end
const AbstractArrayOrBroadcasted = Union{AbstractArray, AbstractBroadcasted}

"""
    Base.add_sum(x, y)

The reduction operator used in `sum`. The main difference from [`+`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
add_sum(x, y) = x + y
add_sum(x::SmallSigned, y::SmallSigned) = Int(x) + Int(y)
add_sum(x::SmallUnsigned, y::SmallUnsigned) = UInt(x) + UInt(y)
add_sum(x::Real, y::Real)::Real = x + y

"""
    Base.mul_prod(x, y)

The reduction operator used in `prod`. The main difference from [`*`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
mul_prod(x, y) = x * y
mul_prod(x::SmallSigned, y::SmallSigned) = Int(x) * Int(y)
mul_prod(x::SmallUnsigned, y::SmallUnsigned) = UInt(x) * UInt(y)
mul_prod(x::Real, y::Real)::Real = x * y

## foldl && mapfoldl

function mapfoldl_impl(f::F, op::OP, nt, itr) where {F,OP}
    op′, itr′ = _xfadjoint(BottomRF(op), Generator(f, itr))
    return foldl_impl(op′, nt, itr′)
end

function foldl_impl(op::OP, nt, itr) where {OP}
    v = _foldl_impl(op, nt, itr)
    v isa _InitialValue && return reduce_empty_iter(op, itr)
    return v
end

function _foldl_impl(op::OP, init, itr) where {OP}
    # Unroll the while loop once; if init is known, the call to op may
    # be evaluated at compile time
    y = iterate(itr)
    y === nothing && return init
    v = op(init, y[1])
    while true
        y = iterate(itr, y[2])
        y === nothing && break
        v = op(v, y[1])
    end
    return v
end

struct _InitialValue end

"""
    BottomRF(rf) -> rf′

"Bottom" reducing function.  This is a thin wrapper around the `op` argument
passed to `foldl`-like functions for handling the initial invocation to call
[`reduce_first`](@ref).
"""
struct BottomRF{T}
    rf::T
end

@inline (op::BottomRF)(::_InitialValue, x) = reduce_first(op.rf, x)
@inline (op::BottomRF)(acc, x) = op.rf(acc, x)

"""
    MappingRF(f, rf) -> rf′

Create a mapping reducing function `rf′(acc, x) = rf(acc, f(x))`.
"""
struct MappingRF{F, T}
    f::F
    rf::T
end

@inline (op::MappingRF)(acc, x) = op.rf(acc, op.f(x))

"""
    FilteringRF(f, rf) -> rf′

Create a filtering reducing function `rf′(acc, x) = f(x) ? rf(acc, x) : acc`.
"""
struct FilteringRF{F, T}
    f::F
    rf::T
end

@inline (op::FilteringRF)(acc, x) = op.f(x) ? op.rf(acc, x) : acc

"""
    FlatteningRF(rf) -> rf′

Create a flattening reducing function that is roughly equivalent to
`rf′(acc, x) = foldl(rf, x; init=acc)`.
"""
struct FlatteningRF{T}
    rf::T
end

@inline function (op::FlatteningRF)(acc, x)
    op′, itr′ = _xfadjoint(op.rf, x)
    return _foldl_impl(op′, acc, itr′)
end

"""
    _xfadjoint(op, itr) -> op′, itr′

Given a pair of reducing function `op` and an iterator `itr`, return a pair
`(op′, itr′)` of similar types.  If the iterator `itr` is transformed by an
iterator transform `ixf` whose adjoint transducer `xf` is known, `op′ = xf(op)`
and `itr′ = ixf⁻¹(itr)` is returned.  Otherwise, `op` and `itr` are returned
as-is.  For example, transducer `rf -> MappingRF(f, rf)` is the adjoint of
iterator transform `itr -> Generator(f, itr)`.

Nested iterator transforms are converted recursively.  That is to say,
given `op` and

    itr = (ixf₁ ∘ ixf₂ ∘ ... ∘ ixfₙ)(itr′)

what is returned is `itr′` and

    op′ = (xfₙ ∘ ... ∘ xf₂ ∘ xf₁)(op)
"""
_xfadjoint(op, itr) = (op, itr)
_xfadjoint(op, itr::Generator) =
    if itr.f === identity
        _xfadjoint(op, itr.iter)
    else
        _xfadjoint(MappingRF(itr.f, op), itr.iter)
    end
_xfadjoint(op, itr::Filter) =
    _xfadjoint(FilteringRF(itr.flt, op), itr.itr)
_xfadjoint(op, itr::Flatten) =
    _xfadjoint(FlatteningRF(op), itr.it)

"""
    mapfoldl(f, op, itr; [init])

Like [`mapreduce`](@ref), but with guaranteed left associativity, as in [`foldl`](@ref).
If provided, the keyword argument `init` will be used exactly once. In general, it will be
necessary to provide `init` to work with empty collections.
"""
mapfoldl(f, op, itr; init=_InitialValue()) = mapfoldl_impl(f, op, init, itr)

"""
    foldl(op, itr; [init])

Like [`reduce`](@ref), but with guaranteed left associativity. If provided, the keyword
argument `init` will be used exactly once. In general, it will be necessary to provide
`init` to work with empty collections.

# Examples
```jldoctest
julia> foldl(=>, 1:4)
((1 => 2) => 3) => 4

julia> foldl(=>, 1:4; init=0)
(((0 => 1) => 2) => 3) => 4
```
"""
foldl(op, itr; kw...) = mapfoldl(identity, op, itr; kw...)

## foldr & mapfoldr

function mapfoldr_impl(f, op, nt, itr)
    op′, itr′ = _xfadjoint(BottomRF(FlipArgs(op)), Generator(f, itr))
    return foldl_impl(op′, nt, _reverse(itr′))
end

_reverse(itr) = Iterators.reverse(itr)
_reverse(itr::Tuple) = reverse(itr)  #33235

struct FlipArgs{F}
    f::F
end

@inline (f::FlipArgs)(x, y) = f.f(y, x)

"""
    mapfoldr(f, op, itr; [init])

Like [`mapreduce`](@ref), but with guaranteed right associativity, as in [`foldr`](@ref). If
provided, the keyword argument `init` will be used exactly once. In general, it will be
necessary to provide `init` to work with empty collections.
"""
mapfoldr(f, op, itr; init=_InitialValue()) = mapfoldr_impl(f, op, init, itr)


"""
    foldr(op, itr; [init])

Like [`reduce`](@ref), but with guaranteed right associativity. If provided, the keyword
argument `init` will be used exactly once. In general, it will be necessary to provide
`init` to work with empty collections.

# Examples
```jldoctest
julia> foldr(=>, 1:4)
1 => (2 => (3 => 4))

julia> foldr(=>, 1:4; init=0)
1 => (2 => (3 => (4 => 0)))
```
"""
foldr(op, itr; kw...) = mapfoldr(identity, op, itr; kw...)

## reduce & mapreduce

# `mapreduce_impl()` is called by `mapreduce()` (via `_mapreduce()`, when `A`
# supports linear indexing) and does actual calculations (for `A[ifirst:ilast]` subset).
# For efficiency, no parameter validity checks are done, it's the caller's responsibility.
# `ifirst:ilast` range is assumed to be a valid non-empty subset of `A` indices.

# This is a generic implementation of `mapreduce_impl()`,
# certain `op` (e.g. `min` and `max`) may have their own specialized versions.
@noinline function mapreduce_impl(f, op, A::AbstractArrayOrBroadcasted,
                                  ifirst::Integer, ilast::Integer, blksize::Int)
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        return mapreduce_first(f, op, a1)
    elseif ifirst + blksize > ilast
        # sequential portion
        @inbounds a1 = A[ifirst]
        @inbounds a2 = A[ifirst+1]
        v = op(f(a1), f(a2))
        @simd for i = ifirst + 2 : ilast
            @inbounds ai = A[i]
            v = op(v, f(ai))
        end
        return v
    else
        # pairwise portion
        imid = (ifirst + ilast) >> 1
        v1 = mapreduce_impl(f, op, A, ifirst, imid, blksize)
        v2 = mapreduce_impl(f, op, A, imid+1, ilast, blksize)
        return op(v1, v2)
    end
end

mapreduce_impl(f, op, A::AbstractArrayOrBroadcasted, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

"""
    mapreduce(f, op, itrs...; [init])

Apply function `f` to each element(s) in `itrs`, and then reduce the result using the binary
function `op`. If provided, `init` must be a neutral element for `op` that will be returned
for empty collections. It is unspecified whether `init` is used for non-empty collections.
In general, it will be necessary to provide `init` to work with empty collections.

[`mapreduce`](@ref) is functionally equivalent to calling
`reduce(op, map(f, itr); init=init)`, but will in general execute faster since no
intermediate collection needs to be created. See documentation for [`reduce`](@ref) and
[`map`](@ref).

!!! compat "Julia 1.2"
    `mapreduce` with multiple iterators requires Julia 1.2 or later.

# Examples
```jldoctest
julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
14
```

The associativity of the reduction is implementation-dependent. Additionally, some
implementations may reuse the return value of `f` for elements that appear multiple times in
`itr`. Use [`mapfoldl`](@ref) or [`mapfoldr`](@ref) instead for
guaranteed left or right associativity and invocation of `f` for every value.
"""
mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
mapreduce(f, op, itrs...; kw...) = reduce(op, Generator(f, itrs...); kw...)

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
pairwise_blocksize(f, op) = 1024

# This combination appears to show a benefit from a larger block size
pairwise_blocksize(::typeof(abs2), ::typeof(+)) = 4096


# handling empty arrays
_empty_reduce_error() = throw(ArgumentError("reducing over an empty collection is not allowed"))

"""
    Base.reduce_empty(op, T)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref) or [`foldr`](@ref)
with reduction `op` over an empty array with element type of `T`.

If not defined, this will throw an `ArgumentError`.
"""
reduce_empty(op, ::Type{T}) where {T} = _empty_reduce_error()
reduce_empty(::typeof(+), ::Type{Union{}}) = _empty_reduce_error()
reduce_empty(::typeof(+), ::Type{T}) where {T} = zero(T)
reduce_empty(::typeof(+), ::Type{Bool}) = zero(Int)
reduce_empty(::typeof(*), ::Type{Union{}}) = _empty_reduce_error()
reduce_empty(::typeof(*), ::Type{T}) where {T} = one(T)
reduce_empty(::typeof(*), ::Type{<:AbstractChar}) = ""
reduce_empty(::typeof(&), ::Type{Bool}) = true
reduce_empty(::typeof(|), ::Type{Bool}) = false

reduce_empty(::typeof(add_sum), ::Type{Union{}}) = _empty_reduce_error()
reduce_empty(::typeof(add_sum), ::Type{T}) where {T} = reduce_empty(+, T)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallSigned}  = zero(Int)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallUnsigned} = zero(UInt)
reduce_empty(::typeof(mul_prod), ::Type{Union{}}) = _empty_reduce_error()
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T} = reduce_empty(*, T)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallSigned}  = one(Int)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallUnsigned} = one(UInt)

reduce_empty(op::BottomRF, ::Type{T}) where {T} = reduce_empty(op.rf, T)
reduce_empty(op::MappingRF, ::Type{T}) where {T} = mapreduce_empty(op.f, op.rf, T)
reduce_empty(op::FilteringRF, ::Type{T}) where {T} = reduce_empty(op.rf, T)
reduce_empty(op::FlipArgs, ::Type{T}) where {T} = reduce_empty(op.f, T)

"""
    Base.mapreduce_empty(f, op, T)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an empty array with element type
of `T`.

If not defined, this will throw an `ArgumentError`.
"""
mapreduce_empty(f, op, T) = _empty_reduce_error()
mapreduce_empty(::typeof(identity), op, T) = reduce_empty(op, T)
mapreduce_empty(::typeof(abs), op, T)      = abs(reduce_empty(op, T))
mapreduce_empty(::typeof(abs2), op, T)     = abs2(reduce_empty(op, T))

mapreduce_empty(f::typeof(abs),  ::typeof(max), T) = abs(zero(T))
mapreduce_empty(f::typeof(abs2), ::typeof(max), T) = abs2(zero(T))

# For backward compatibility:
mapreduce_empty_iter(f, op, itr, ItrEltype) =
    reduce_empty_iter(MappingRF(f, op), itr, ItrEltype)

@inline reduce_empty_iter(op, itr) = reduce_empty_iter(op, itr, IteratorEltype(itr))
@inline reduce_empty_iter(op, itr, ::HasEltype) = reduce_empty(op, eltype(itr))
reduce_empty_iter(op, itr, ::EltypeUnknown) = _empty_reduce_error()

# handling of single-element iterators
"""
    Base.reduce_first(op, x)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref`) or
[`foldr`](@ref) with reduction `op` over an iterator which contains a single element
`x`. This value may also used to initialise the recursion, so that `reduce(op, [x, y])`
may call `op(reduce_first(op, x), y)`.

The default is `x` for most types. The main purpose is to ensure type stability, so
additional methods should only be defined for cases where `op` gives a result with
different types than its inputs.
"""
reduce_first(op, x) = x
reduce_first(::typeof(+), x::Bool) = Int(x)
reduce_first(::typeof(*), x::AbstractChar) = string(x)

reduce_first(::typeof(add_sum), x) = reduce_first(+, x)
reduce_first(::typeof(add_sum), x::SmallSigned)   = Int(x)
reduce_first(::typeof(add_sum), x::SmallUnsigned) = UInt(x)
reduce_first(::typeof(mul_prod), x) = reduce_first(*, x)
reduce_first(::typeof(mul_prod), x::SmallSigned)   = Int(x)
reduce_first(::typeof(mul_prod), x::SmallUnsigned) = UInt(x)

"""
    Base.mapreduce_first(f, op, x)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an iterator which contains a
single element `x`. This value may also used to initialise the recursion, so that
`mapreduce(f, op, [x, y])` may call `op(reduce_first(op, f, x), f(y))`.

The default is `reduce_first(op, f(x))`.
"""
mapreduce_first(f, op, x) = reduce_first(op, f(x))

_mapreduce(f, op, A::AbstractArrayOrBroadcasted) = _mapreduce(f, op, IndexStyle(A), A)

function _mapreduce(f, op, ::IndexLinear, A::AbstractArrayOrBroadcasted)
    inds = LinearIndices(A)
    n = length(inds)
    if n == 0
        return mapreduce_empty_iter(f, op, A, IteratorEltype(A))
    elseif n == 1
        @inbounds a1 = A[first(inds)]
        return mapreduce_first(f, op, a1)
    elseif n < 16 # process short array here, avoid mapreduce_impl() compilation
        @inbounds i = first(inds)
        @inbounds a1 = A[i]
        @inbounds a2 = A[i+=1]
        s = op(f(a1), f(a2))
        while i < last(inds)
            @inbounds Ai = A[i+=1]
            s = op(s, f(Ai))
        end
        return s
    else
        return mapreduce_impl(f, op, A, first(inds), last(inds))
    end
end

mapreduce(f, op, a::Number) = mapreduce_first(f, op, a)

_mapreduce(f, op, ::IndexCartesian, A::AbstractArrayOrBroadcasted) = mapfoldl(f, op, A)

"""
    reduce(op, itr; [init])

Reduce the given collection `itr` with the given binary operator `op`. If provided, the
initial value `init` must be a neutral element for `op` that will be returned for empty
collections. It is unspecified whether `init` is used for non-empty collections.

For empty collections, providing `init` will be necessary, except for some special cases
(e.g. when `op` is one of `+`, `*`, `max`, `min`, `&`, `|`) when Julia can determine the
neutral element of `op`.

Reductions for certain commonly-used operators may have special implementations, and
should be used instead: `maximum(itr)`, `minimum(itr)`, `sum(itr)`, `prod(itr)`,
 `any(itr)`, `all(itr)`.

The associativity of the reduction is implementation dependent. This means that you can't
use non-associative operations like `-` because it is undefined whether `reduce(-,[1,2,3])`
should be evaluated as `(1-2)-3` or `1-(2-3)`. Use [`foldl`](@ref) or
[`foldr`](@ref) instead for guaranteed left or right associativity.

Some operations accumulate error. Parallelism will be easier if the reduction can be
executed in groups. Future versions of Julia might change the algorithm. Note that the
elements are not reordered if you use an ordered collection.

# Examples
```jldoctest
julia> reduce(*, [2; 3; 4])
24

julia> reduce(*, [2; 3; 4]; init=-1)
-24
```
"""
reduce(op, itr; kw...) = mapreduce(identity, op, itr; kw...)

reduce(op, a::Number) = a  # Do we want this?

###### Specific reduction functions ######

## sum

"""
    sum(f, itr; [init])

Sum the results of calling function `f` on each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be
the additive identity (i.e. zero) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> sum(abs2, [2; 3; 4])
29
```

Note the important difference between `sum(A)` and `reduce(+, A)` for arrays
with small integer eltype:

```jldoctest
julia> sum(Int8[100, 28])
128

julia> reduce(+, Int8[100, 28])
-128
```

In the former case, the integers are widened to system word size and therefore
the result is 128. In the latter case, no such widening happens and integer
overflow results in -128.
"""
sum(f, a; kw...) = mapreduce(f, add_sum, a; kw...)

"""
    sum(itr; [init])

Returns the sum of all elements in a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be
the additive identity (i.e. zero) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> sum(1:20)
210

julia> sum(1:20; init = 0.0)
210.0
```
"""
sum(a; kw...) = sum(identity, a; kw...)
sum(a::AbstractArray{Bool}; kw...) =
    kw.data === NamedTuple() ? count(a) : reduce(add_sum, a; kw...)

## prod
"""
    prod(f, itr; [init])

Returns the product of `f` applied to each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be the
multiplicative identity (i.e. one) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> prod(abs2, [2; 3; 4])
576
```
"""
prod(f, a; kw...) = mapreduce(f, mul_prod, a; kw...)

"""
    prod(itr; [init])

Returns the product of all elements of a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be the
multiplicative identity (i.e. one) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> prod(1:5)
120

julia> prod(1:5; init = 1.0)
120.0
```
"""
prod(a; kw...) = mapreduce(identity, mul_prod, a; kw...)

## maximum & minimum
_fast(::typeof(min),x,y) = min(x,y)
_fast(::typeof(max),x,y) = max(x,y)
function _fast(::typeof(max), x::AbstractFloat, y::AbstractFloat)
    ifelse(isnan(x),
        x,
        ifelse(x > y, x, y))
end

function _fast(::typeof(min),x::AbstractFloat, y::AbstractFloat)
    ifelse(isnan(x),
        x,
        ifelse(x < y, x, y))
end

isbadzero(::typeof(max), x::AbstractFloat) = (x == zero(x)) & signbit(x)
isbadzero(::typeof(min), x::AbstractFloat) = (x == zero(x)) & !signbit(x)
isbadzero(op, x) = false
isgoodzero(::typeof(max), x) = isbadzero(min, x)
isgoodzero(::typeof(min), x) = isbadzero(max, x)

function mapreduce_impl(f, op::Union{typeof(max), typeof(min)},
                        A::AbstractArrayOrBroadcasted, first::Int, last::Int)
    a1 = @inbounds A[first]
    v1 = mapreduce_first(f, op, a1)
    v2 = v3 = v4 = v1
    chunk_len = 256
    start = first + 1
    simdstop  = start + chunk_len - 4
    while simdstop <= last - 3
        # short circuit in case of NaN
        v1 == v1 || return v1
        v2 == v2 || return v2
        v3 == v3 || return v3
        v4 == v4 || return v4
        @inbounds for i in start:4:simdstop
            v1 = _fast(op, v1, f(A[i+0]))
            v2 = _fast(op, v2, f(A[i+1]))
            v3 = _fast(op, v3, f(A[i+2]))
            v4 = _fast(op, v4, f(A[i+3]))
        end
        checkbounds(A, simdstop+3)
        start += chunk_len
        simdstop += chunk_len
    end
    v = op(op(v1,v2),op(v3,v4))
    for i in start:last
        @inbounds ai = A[i]
        v = op(v, f(ai))
    end

    # enforce correct order of 0.0 and -0.0
    # e.g. maximum([0.0, -0.0]) === 0.0
    # should hold
    if isbadzero(op, v)
        for i in first:last
            x = @inbounds A[i]
            isgoodzero(op,x) && return x
        end
    end
    return v
end

"""
    maximum(f, itr; [init])

Returns the largest result of calling function `f` on each element of `itr`.

The value returned for empty `itr` can be specified by `init`. It must be the
a neutral element for `max` (i.e. which is less than or equal to any
other element) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> maximum(length, ["Julion", "Julia", "Jule"])
6

julia> maximum(length, []; init=-1)
-1
```
"""
maximum(f, a; kw...) = mapreduce(f, max, a; kw...)

"""
    minimum(f, itr; [init])

Returns the smallest result of calling function `f` on each element of `itr`.

The value returned for empty `itr` can be specified by `init`. It must be the
a neutral element for `min` (i.e. which is greater than or equal to any
other element) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> minimum(length, ["Julion", "Julia", "Jule"])
4

julia> minimum(length, []; init=typemax(Int64))
9223372036854775807

julia> minimum(tanh, Real[]; init=1.0)
1.0
```
"""
minimum(f, a; kw...) = mapreduce(f, min, a; kw...)

"""
    maximum(itr; [init])

Returns the largest element in a collection.

The value returned for empty `itr` can be specified by `init`. It must be the
a neutral element for `max` (i.e. which is less than or equal to any
other element) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> maximum(-20.5:10)
9.5

julia> maximum([1,2,3])
3

julia> maximum(())
ERROR: ArgumentError: reducing over an empty collection is not allowed
Stacktrace:
[...]

julia> maximum((); init=-Inf)
-Inf
```
"""
maximum(a; kw...) = mapreduce(identity, max, a; kw...)

"""
    minimum(itr; [init])

Returns the smallest element in a collection.

The value returned for empty `itr` can be specified by `init`. It must be the
a neutral element for `min` (i.e. which is greater than or equal to any
other element) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

# Examples
```jldoctest
julia> minimum(-20.5:10)
-20.5

julia> minimum([1,2,3])
1

julia> minimum([])
ERROR: ArgumentError: reducing over an empty collection is not allowed
Stacktrace:
[...]

julia> minimum([]; init=Inf)
Inf
```
"""
minimum(a; kw...) = mapreduce(identity, min, a; kw...)

## all & any

"""
    any(itr) -> Bool

Test whether any elements of a boolean collection are `true`, returning `true` as
soon as the first `true` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
 1
 0
 0
 1

julia> any(a)
true

julia> any((println(i); v) for (i, v) in enumerate(a))
1
true

julia> any([missing, true])
true

julia> any([false, missing])
missing
```
"""
any(itr) = any(identity, itr)

"""
    all(itr) -> Bool

Test whether all elements of a boolean collection are `true`, returning `false` as
soon as the first `false` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
 1
 0
 0
 1

julia> all(a)
false

julia> all((println(i); v) for (i, v) in enumerate(a))
1
2
false

julia> all([missing, false])
false

julia> all([true, missing])
missing
```
"""
all(itr) = all(identity, itr)

"""
    any(p, itr) -> Bool

Determine whether predicate `p` returns `true` for any elements of `itr`, returning
`true` as soon as the first item in `itr` for which `p` returns `true` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true

julia> any(i -> (println(i); i > 3), 1:10)
1
2
3
4
true

julia> any(i -> i > 0, [1, missing])
true

julia> any(i -> i > 0, [-1, missing])
missing

julia> any(i -> i > 0, [-1, 0])
false
```
"""
any(f, itr) = _any(f, itr, :)

function _any(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        elseif v
            return true
        end
    end
    return anymissing ? missing : false
end

"""
    all(p, itr) -> Bool

Determine whether predicate `p` returns `true` for all elements of `itr`, returning
`false` as soon as the first item in `itr` for which `p` returns `false` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true

julia> all(i -> (println(i); i < 3), 1:10)
1
2
3
false

julia> all(i -> i > 0, [1, missing])
missing

julia> all(i -> i > 0, [-1, missing])
false

julia> all(i -> i > 0, [1, 2])
true
```
"""
all(f, itr) = _all(f, itr, :)

function _all(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        # this syntax allows throwing a TypeError for non-Bool, for consistency with any
        elseif v
            continue
        else
            return false
        end
    end
    return anymissing ? missing : true
end

## count

_bool(f::Function) = x->f(x)::Bool

"""
    count(p, itr) -> Integer
    count(itr) -> Integer

Count the number of elements in `itr` for which predicate `p` returns `true`.
If `p` is omitted, counts the number of `true` elements in `itr` (which
should be a collection of boolean values).

# Examples
```jldoctest
julia> count(i->(4<=i<=6), [2,3,4,5,6])
3

julia> count([true, false, true, true])
3
```
"""
count(itr) = count(identity, itr)

count(f, itr) = mapreduce(_bool(f), add_sum, itr, init=0)

function count(::typeof(identity), x::Array{Bool})
    n = 0
    chunks = length(x) ÷ sizeof(UInt)
    mask = 0x0101010101010101 % UInt
    GC.@preserve x begin
        ptr = Ptr{UInt}(pointer(x))
        for i in 1:chunks
            n += count_ones(unsafe_load(ptr, i) & mask)
        end
    end
    for i in sizeof(UInt)*chunks+1:length(x)
        n += x[i]
    end
    return n
end

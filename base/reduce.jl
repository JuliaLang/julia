# This file is a part of Julia. License is MIT: https://julialang.org/license

## reductions ##

###### Generic (map)reduce functions ######

abstract type AbstractBroadcasted end
const AbstractArrayOrBroadcasted = Union{AbstractArray, AbstractBroadcasted}

"""
    Base.add_sum(x, y)

The reduction operator used in `sum`. The main difference from [`+`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
add_sum(x, y) = x + y
add_sum(x::BitSignedSmall, y::BitSignedSmall) = Int(x) + Int(y)
add_sum(x::BitUnsignedSmall, y::BitUnsignedSmall) = UInt(x) + UInt(y)
add_sum(x::Real, y::Real)::Real = x + y

"""
    Base.mul_prod(x, y)

The reduction operator used in `prod`. The main difference from [`*`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
mul_prod(x, y) = x * y
mul_prod(x::BitSignedSmall, y::BitSignedSmall) = Int(x) * Int(y)
mul_prod(x::BitUnsignedSmall, y::BitUnsignedSmall) = UInt(x) * UInt(y)
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
    # Unroll the loop once to check if the iterator is empty.
    # If init is known, the call to op may be evaluated at compile time
    y = iterate(itr)
    y === nothing && return init
    v = op(init, y[1])
    # Using a for loop is more performant than a while loop (see #56492)
    # This unrolls the loop a second time before entering the body
    for x in Iterators.rest(itr, y[2])
        v = op(v, x)
    end
    return v
end

function _foldl_impl(op, init, itr::Union{Tuple,NamedTuple})
    length(itr) <= 32 && return afoldl(op, init, itr...)
    @invoke _foldl_impl(op, init, itr::Any)
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
    MappingRF(f::F, rf::T) where {F,T} = new{F,T}(f, rf)
    MappingRF(::Type{f}, rf::T) where {f,T} = new{Type{f},T}(f, rf)
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
function _xfadjoint(op, itr)
    itr′, wrap = _xfadjoint_unwrap(itr)
    wrap(op), itr′
end

_xfadjoint_unwrap(itr) = itr, identity
function _xfadjoint_unwrap(itr::Generator)
    itr′, wrap = _xfadjoint_unwrap(itr.iter)
    itr.f === identity && return itr′, wrap
    return itr′, wrap ∘ Fix1(MappingRF, itr.f)
end
function _xfadjoint_unwrap(itr::Filter)
    itr′, wrap = _xfadjoint_unwrap(itr.itr)
    return itr′, wrap ∘ Fix1(FilteringRF, itr.flt)
end
function _xfadjoint_unwrap(itr::Flatten)
    itr′, wrap = _xfadjoint_unwrap(itr.it)
    return itr′, wrap ∘ FlatteningRF
end

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

See also [`mapfoldl`](@ref), [`foldr`](@ref), [`accumulate`](@ref).

# Examples
```jldoctest
julia> foldl(=>, 1:4)
((1 => 2) => 3) => 4

julia> foldl(=>, 1:4; init=0)
(((0 => 1) => 2) => 3) => 4

julia> accumulate(=>, (1,2,3,4))
(1, 1 => 2, (1 => 2) => 3, ((1 => 2) => 3) => 4)
```
"""
foldl(op, itr; kw...) = mapfoldl(identity, op, itr; kw...)

## foldr & mapfoldr

function mapfoldr_impl(f, op, nt, itr)
    op′, itr′ = _xfadjoint(BottomRF(FlipArgs(op)), Generator(f, itr))
    return foldl_impl(op′, nt, _reverse_iter(itr′))
end

_reverse_iter(itr) = Iterators.reverse(itr)
_reverse_iter(itr::Union{Tuple,NamedTuple}) = length(itr) <= 32 ? reverse(itr) : Iterators.reverse(itr) #33235

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
    elseif ilast - ifirst < blksize
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
        imid = ifirst + (ilast - ifirst) >> 1
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
mapreduce(f, op, itr, itrs...; kw...) = reduce(op, Generator(f, itr, itrs...); kw...)

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
pairwise_blocksize(f, op) = 1024

# This combination appears to show a benefit from a larger block size
pairwise_blocksize(::typeof(abs2), ::typeof(+)) = 4096


# handling empty arrays
_empty_reduce_error() = throw(ArgumentError("reducing over an empty collection is not allowed; consider supplying `init` to the reducer"))
reduce_empty(f, T) = _empty_reduce_error()
mapreduce_empty(f, op, T) = _empty_reduce_error()
reduce_empty(f, ::Type{Union{}}, splat...) = _empty_reduce_error()
mapreduce_empty(f, op, ::Type{Union{}}, splat...) = _empty_reduce_error()

"""
    Base.reduce_empty(op, T)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref) or [`foldr`](@ref)
with reduction `op` over an empty array with element type of `T`.

This should only be defined in unambiguous cases; for example,

```julia
Base.reduce_empty(::typeof(+), ::Type{T}) where T = zero(T)
```

is justified (the sum of zero elements is zero), whereas
`reduce_empty(::typeof(max), ::Type{Any})` is not (the maximum value of an empty collection
is generally ambiguous, and especially so when the element type is unknown).

As an alternative, consider supplying an `init` value to the reducer.
"""
reduce_empty(::typeof(+), ::Type{T}) where {T} = zero(T)
reduce_empty(::typeof(+), ::Type{Bool}) = zero(Int)
reduce_empty(::typeof(*), ::Type{T}) where {T} = one(T)
reduce_empty(::typeof(*), ::Type{<:AbstractChar}) = ""
reduce_empty(::typeof(&), ::Type{Bool}) = true
reduce_empty(::typeof(|), ::Type{Bool}) = false

reduce_empty(::typeof(add_sum), ::Type{T}) where {T} = reduce_empty(+, T)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:BitSignedSmall}  = zero(Int)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:BitUnsignedSmall} = zero(UInt)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T} = reduce_empty(*, T)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:BitSignedSmall}  = one(Int)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:BitUnsignedSmall} = one(UInt)

reduce_empty(op::BottomRF, ::Type{T}) where {T} = reduce_empty(op.rf, T)
reduce_empty(op::MappingRF, ::Type{T}) where {T} = mapreduce_empty(op.f, op.rf, T)
reduce_empty(op::FilteringRF, ::Type{T}) where {T} = reduce_empty(op.rf, T)
reduce_empty(op::FlipArgs, ::Type{T}) where {T} = reduce_empty(op.f, T)

"""
    Base.mapreduce_empty(f, op, T)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an empty array with element type
of `T`. See [`Base.reduce_empty`](@ref) for more information.
"""
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
reduce_empty_iter(op, itr, ::EltypeUnknown) = throw(ArgumentError("""
    reducing over an empty collection of unknown element type is not allowed.
    You may be able to prevent this error by supplying an `init` value to the reducer."""))


# handling of single-element iterators
"""
    Base.reduce_first(op, x)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref`) or
[`foldr`](@ref) with reduction `op` over an iterator which contains a single element
`x`. This value may also be used to initialise the recursion, so that `reduce(op, [x, y])`
may call `op(reduce_first(op, x), y)`.

The default is `x` for most types. The main purpose is to ensure type stability, so
additional methods should only be defined for cases where `op` gives a result with
different types than its inputs.
"""
reduce_first(op, x) = x
reduce_first(::typeof(+), x::Bool) = Int(x)
reduce_first(::typeof(*), x::AbstractChar) = string(x)

reduce_first(::typeof(add_sum), x) = reduce_first(+, x)
reduce_first(::typeof(add_sum), x::BitSignedSmall)   = Int(x)
reduce_first(::typeof(add_sum), x::BitUnsignedSmall) = UInt(x)
reduce_first(::typeof(mul_prod), x) = reduce_first(*, x)
reduce_first(::typeof(mul_prod), x::BitSignedSmall)   = Int(x)
reduce_first(::typeof(mul_prod), x::BitUnsignedSmall) = UInt(x)

"""
    Base.mapreduce_first(f, op, x)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an iterator which contains a
single element `x`. This value may also be used to initialise the recursion, so that
`mapreduce(f, op, [x, y])` may call `op(mapreduce_first(f, op, x), f(y))`.

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
should be used instead: [`maximum`](@ref)`(itr)`, [`minimum`](@ref)`(itr)`, [`sum`](@ref)`(itr)`,
[`prod`](@ref)`(itr)`, [`any`](@ref)`(itr)`, [`all`](@ref)`(itr)`.
There are efficient methods for concatenating certain arrays of arrays
by calling `reduce(`[`vcat`](@ref)`, arr)` or `reduce(`[`hcat`](@ref)`, arr)`.

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

julia> reduce(*, Int[]; init=1)
1
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

Return the sum of all elements in a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be
the additive identity (i.e. zero) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

See also: [`reduce`](@ref), [`mapreduce`](@ref), [`count`](@ref), [`union`](@ref).

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
    isempty(kw) ? count(a) : reduce(add_sum, a; kw...)

## prod
"""
    prod(f, itr; [init])

Return the product of `f` applied to each element of `itr`.

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

Return the product of all elements of a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

The value returned for empty `itr` can be specified by `init`. It must be the
multiplicative identity (i.e. one) as it is unspecified whether `init` is used
for non-empty collections.

!!! compat "Julia 1.6"
    Keyword argument `init` requires Julia 1.6 or later.

See also: [`reduce`](@ref), [`cumprod`](@ref), [`any`](@ref).

# Examples
```jldoctest
julia> prod(1:5)
120

julia> prod(1:5; init = 1.0)
120.0
```
"""
prod(a; kw...) = mapreduce(identity, mul_prod, a; kw...)

## maximum, minimum, & extrema
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
        for i in start:4:simdstop
            v1 = _fast(op, v1, f(@inbounds(A[i+0])))
            v2 = _fast(op, v2, f(@inbounds(A[i+1])))
            v3 = _fast(op, v3, f(@inbounds(A[i+2])))
            v4 = _fast(op, v4, f(@inbounds(A[i+3])))
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

Return the largest result of calling function `f` on each element of `itr`.

The value returned for empty `itr` can be specified by `init`. It must be
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

julia> maximum(sin, Real[]; init=-1.0)  # good, since output of sin is >= -1
-1.0
```
"""
maximum(f, a; kw...) = mapreduce(f, max, a; kw...)

"""
    minimum(f, itr; [init])

Return the smallest result of calling function `f` on each element of `itr`.

The value returned for empty `itr` can be specified by `init`. It must be
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

julia> minimum(sin, Real[]; init=1.0)  # good, since output of sin is <= 1
1.0
```
"""
minimum(f, a; kw...) = mapreduce(f, min, a; kw...)

"""
    maximum(itr; [init])

Return the largest element in a collection.

The value returned for empty `itr` can be specified by `init`. It must be
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
ERROR: ArgumentError: reducing over an empty collection is not allowed; consider supplying `init` to the reducer
Stacktrace:
[...]

julia> maximum((); init=-Inf)
-Inf
```
"""
maximum(a; kw...) = mapreduce(identity, max, a; kw...)

"""
    minimum(itr; [init])

Return the smallest element in a collection.

The value returned for empty `itr` can be specified by `init`. It must be
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
ERROR: ArgumentError: reducing over an empty collection is not allowed; consider supplying `init` to the reducer
Stacktrace:
[...]

julia> minimum([]; init=Inf)
Inf
```
"""
minimum(a; kw...) = mapreduce(identity, min, a; kw...)

"""
    extrema(itr; [init]) -> (mn, mx)

Compute both the minimum `mn` and maximum `mx` element in a single pass, and return them
as a 2-tuple.

The value returned for empty `itr` can be specified by `init`. It must be a 2-tuple whose
first and second elements are neutral elements for `min` and `max` respectively
(i.e. which are greater/less than or equal to any other element). As a consequence, when
`itr` is empty the returned `(mn, mx)` tuple will satisfy `mn ≥ mx`. When `init` is
specified it may be used even for non-empty `itr`.

!!! compat "Julia 1.8"
    Keyword argument `init` requires Julia 1.8 or later.

# Examples
```jldoctest
julia> extrema(2:10)
(2, 10)

julia> extrema([9,pi,4.5])
(3.141592653589793, 9.0)

julia> extrema([]; init = (Inf, -Inf))
(Inf, -Inf)
```
"""
extrema(itr; kw...) = extrema(identity, itr; kw...)

"""
    extrema(f, itr; [init]) -> (mn, mx)

Compute both the minimum `mn` and maximum `mx` of `f` applied to each element in `itr` and
return them as a 2-tuple. Only one pass is made over `itr`.

The value returned for empty `itr` can be specified by `init`. It must be a 2-tuple whose
first and second elements are neutral elements for `min` and `max` respectively
(i.e. which are greater/less than or equal to any other element). It is used for non-empty
collections. Note: it implies that, for empty `itr`, the returned value `(mn, mx)` satisfies
`mn ≥ mx` even though for non-empty `itr` it  satisfies `mn ≤ mx`.  This is a "paradoxical"
but yet expected result.

!!! compat "Julia 1.2"
    This method requires Julia 1.2 or later.

!!! compat "Julia 1.8"
    Keyword argument `init` requires Julia 1.8 or later.

# Examples
```jldoctest
julia> extrema(sin, 0:π)
(0.0, 0.9092974268256817)

julia> extrema(sin, Real[]; init = (1.0, -1.0))  # good, since -1 ≤ sin(::Real) ≤ 1
(1.0, -1.0)
```
"""
extrema(f, itr; kw...) = mapreduce(ExtremaMap(f), _extrema_rf, itr; kw...)

# Not using closure since `extrema(type, itr)` is a very likely use-case and it's better
# to avoid type-instability (#23618).
struct ExtremaMap{F} <: Function
    f::F
end
ExtremaMap(::Type{T}) where {T} = ExtremaMap{Type{T}}(T)
@inline (f::ExtremaMap)(x) = (y = f.f(x); (y, y))

@inline _extrema_rf((min1, max1), (min2, max2)) = (min(min1, min2), max(max1, max2))
# optimization for IEEEFloat
function _extrema_rf(x::NTuple{2,T}, y::NTuple{2,T}) where {T<:IEEEFloat}
    (x1, x2), (y1, y2) = x, y
    anynan = isnan(x1)|isnan(y1)
    z1 = ifelse(anynan, x1-y1, ifelse(signbit(x1-y1), x1, y1))
    z2 = ifelse(anynan, x1-y1, ifelse(signbit(x2-y2), y2, x2))
    z1, z2
end

## findmax, findmin, argmax & argmin

"""
    findmax(f, domain) -> (f(x), index)

Return a pair of a value in the codomain (outputs of `f`) and the index or key of
the corresponding value in the `domain` (inputs to `f`) such that `f(x)` is maximised.
If there are multiple maximal points, then the first one will be returned.

`domain` must be a non-empty iterable supporting [`keys`](@ref). Indices
are of the same type as those returned by [`keys(domain)`](@ref).

Values are compared with `isless`.

!!! compat "Julia 1.7"
    This method requires Julia 1.7 or later.

# Examples

```jldoctest
julia> findmax(identity, 5:9)
(9, 5)

julia> findmax(-, 1:10)
(-1, 1)

julia> findmax(first, [(1, :a), (3, :b), (3, :c)])
(3, 2)

julia> findmax(cos, 0:π/2:2π)
(1.0, 1)
```
"""
findmax(f, domain) = _findmax(f, domain, :)
_findmax(f, domain, ::Colon) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmax, pairs(domain) )
_rf_findmax((fm, im), (fx, ix)) = isless(fm, fx) ? (fx, ix) : (fm, im)

"""
    findmax(itr) -> (x, index)

Return the maximal element of the collection `itr` and its index or key.
If there are multiple maximal elements, then the first one will be returned.
Values are compared with `isless`.

Indices are of the same type as those returned by [`keys(itr)`](@ref)
and [`pairs(itr)`](@ref).

See also: [`findmin`](@ref), [`argmax`](@ref), [`maximum`](@ref).

# Examples

```jldoctest
julia> findmax([8, 0.1, -9, pi])
(8.0, 1)

julia> findmax([1, 7, 7, 6])
(7, 2)

julia> findmax([1, 7, 7, NaN])
(NaN, 4)
```
"""
findmax(itr) = _findmax(itr, :)
_findmax(a, ::Colon) = findmax(identity, a)

"""
    findmin(f, domain) -> (f(x), index)

Return a pair of a value in the codomain (outputs of `f`) and the index or key of
the corresponding value in the `domain` (inputs to `f`) such that `f(x)` is minimised.
If there are multiple minimal points, then the first one will be returned.

`domain` must be a non-empty iterable.

Indices are of the same type as those returned by [`keys(domain)`](@ref)
and [`pairs(domain)`](@ref).

`NaN` is treated as less than all other values except `missing`.

!!! compat "Julia 1.7"
    This method requires Julia 1.7 or later.

# Examples

```jldoctest
julia> findmin(identity, 5:9)
(5, 1)

julia> findmin(-, 1:10)
(-10, 10)

julia> findmin(first, [(2, :a), (2, :b), (3, :c)])
(2, 1)

julia> findmin(cos, 0:π/2:2π)
(-1.0, 3)
```

"""
findmin(f, domain) = _findmin(f, domain, :)
_findmin(f, domain, ::Colon) = mapfoldl( ((k, v),) -> (f(v), k), _rf_findmin, pairs(domain) )
_rf_findmin((fm, im), (fx, ix)) = isgreater(fm, fx) ? (fx, ix) : (fm, im)

"""
    findmin(itr) -> (x, index)

Return the minimal element of the collection `itr` and its index or key.
If there are multiple minimal elements, then the first one will be returned.
`NaN` is treated as less than all other values except `missing`.

Indices are of the same type as those returned by [`keys(itr)`](@ref)
and [`pairs(itr)`](@ref).

See also: [`findmax`](@ref), [`argmin`](@ref), [`minimum`](@ref).

# Examples

```jldoctest
julia> findmin([8, 0.1, -9, pi])
(-9.0, 3)

julia> findmin([1, 7, 7, 6])
(1, 1)

julia> findmin([1, 7, 7, NaN])
(NaN, 4)
```
"""
findmin(itr) = _findmin(itr, :)
_findmin(a, ::Colon) = findmin(identity, a)

"""
    argmax(f, domain)

Return a value `x` from `domain` for which `f(x)` is maximised.
If there are multiple maximal values for `f(x)` then the first one will be found.

`domain` must be a non-empty iterable.

Values are compared with `isless`.

!!! compat "Julia 1.7"
    This method requires Julia 1.7 or later.

See also [`argmin`](@ref), [`findmax`](@ref).

# Examples
```jldoctest
julia> argmax(abs, -10:5)
-10

julia> argmax(cos, 0:π/2:2π)
0.0
```
"""
argmax(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmax, domain)[2]

"""
    argmax(itr)

Return the index or key of the maximal element in a collection.
If there are multiple maximal elements, then the first one will be returned.

The collection must not be empty.

Indices are of the same type as those returned by [`keys(itr)`](@ref)
and [`pairs(itr)`](@ref).

Values are compared with `isless`.

See also: [`argmin`](@ref), [`findmax`](@ref).

# Examples
```jldoctest
julia> argmax([8, 0.1, -9, pi])
1

julia> argmax([1, 7, 7, 6])
2

julia> argmax([1, 7, 7, NaN])
4
```
"""
argmax(itr) = findmax(itr)[2]

"""
    argmin(f, domain)

Return a value `x` from `domain` for which `f(x)` is minimised.
If there are multiple minimal values for `f(x)` then the first one will be found.

`domain` must be a non-empty iterable.

`NaN` is treated as less than all other values except `missing`.

!!! compat "Julia 1.7"
    This method requires Julia 1.7 or later.

See also [`argmax`](@ref), [`findmin`](@ref).

# Examples
```jldoctest
julia> argmin(sign, -10:5)
-10

julia> argmin(x -> -x^3 + x^2 - 10, -5:5)
5

julia> argmin(acos, 0:0.1:1)
1.0
```
"""
argmin(f, domain) = mapfoldl(x -> (f(x), x), _rf_findmin, domain)[2]

"""
    argmin(itr)

Return the index or key of the minimal element in a collection.
If there are multiple minimal elements, then the first one will be returned.

The collection must not be empty.

Indices are of the same type as those returned by [`keys(itr)`](@ref)
and [`pairs(itr)`](@ref).

`NaN` is treated as less than all other values except `missing`.

See also: [`argmax`](@ref), [`findmin`](@ref).

# Examples
```jldoctest
julia> argmin([8, 0.1, -9, pi])
3

julia> argmin([7, 1, 1, 6])
2

julia> argmin([7, 1, 1, NaN])
4
```
"""
argmin(itr) = findmin(itr)[2]

## count

_bool(f) = x->f(x)::Bool

"""
    count([f=identity,] itr; init=0)::Integer

Count the number of elements in `itr` for which the function `f` returns `true`.
If `f` is omitted, count the number of `true` elements in `itr` (which
should be a collection of boolean values). `init` optionally specifies the value
to start counting from and therefore also determines the output type.

!!! compat "Julia 1.6"
    `init` keyword was added in Julia 1.6.

See also: [`any`](@ref), [`sum`](@ref).

# Examples
```jldoctest
julia> count(i->(4<=i<=6), [2,3,4,5,6])
3

julia> count([true, false, true, true])
3

julia> count(>(3), 1:7, init=0x03)
0x07
```
"""
count(itr; init=0) = count(identity, itr; init)

count(f, itr; init=0) = _simple_count(f, itr, init)

_simple_count(pred, itr, init) = sum(_bool(pred), itr; init)

function _simple_count(::typeof(identity), x::Array{Bool}, init::T=0) where {T}
    n::T = init
    chunks = length(x) ÷ sizeof(UInt)
    mask = 0x0101010101010101 % UInt
    GC.@preserve x begin
        ptr = Ptr{UInt}(pointer(x))
        for i in 1:chunks
            n = (n + count_ones(unsafe_load(ptr, i) & mask)) % T
        end
    end
    for i in sizeof(UInt)*chunks+1:length(x)
        n = (n + x[i]) % T
    end
    return n
end

# A few common reductions for ranges with init specified
for (fred, f) in ((maximum, max), (minimum, min), (sum, add_sum))
    @eval function _foldl_impl(op::typeof(BottomRF($f)), init, r::AbstractRange)
        isempty(r) ? init : op(init, $fred(r))
    end
end

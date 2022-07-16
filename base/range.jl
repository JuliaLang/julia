# This file is a part of Julia. License is MIT: https://julialang.org/license

(:)(a::Real, b::Real) = (:)(promote(a, b)...)

(:)(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

(:)(start::T, stop::T) where {T} = (:)(start, oftype(stop >= start ? stop - start : start - stop, 1), stop)

# promote start and stop, leaving step alone
(:)(start::A, step, stop::C) where {A<:Real, C<:Real} =
    (:)(convert(promote_type(A, C), start), step, convert(promote_type(A, C), stop))

# AbstractFloat specializations
(:)(a::T, b::T) where {T<:AbstractFloat} = (:)(a, T(1), b)

(:)(a::T, b::AbstractFloat, c::T) where {T<:Real} = (:)(promote(a, b, c)...)
(:)(a::T, b::AbstractFloat, c::T) where {T<:AbstractFloat} = (:)(promote(a, b, c)...)
(:)(a::T, b::Real, c::T) where {T<:AbstractFloat} = (:)(promote(a, b, c)...)

(:)(start::T, step::T, stop::T) where {T<:AbstractFloat} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
(:)(start::T, step::T, stop::T) where {T<:Real} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
_colon(::Ordered, ::Any, start::T, step, stop::T) where {T} = StepRange(start, step, stop)
# for T<:Union{Float16,Float32,Float64} see twiceprecision.jl
_colon(::Ordered, ::ArithmeticRounds, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Integer, (stop-start)/step)+1)
_colon(::Any, ::Any, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Integer, (stop-start)/step)+1)

"""
    (:)(start, [step], stop)

Range operator. `a:b` constructs a range from `a` to `b` with a step size of 1 (a [`UnitRange`](@ref))
, and `a:s:b` is similar but uses a step size of `s` (a [`StepRange`](@ref)).

`:` is also used in indexing to select whole dimensions, e.g. in `A[:, 1]`.
"""
(:)(start::T, step, stop::T) where {T} = _colon(start, step, stop)
(:)(start::T, step, stop::T) where {T<:Real} = _colon(start, step, stop)
# without the second method above, the first method above is ambiguous with
# (:)(start::A, step, stop::C) where {A<:Real,C<:Real}
function _colon(start::T, step, stop::T) where T
    T′ = typeof(start+zero(step))
    StepRange(convert(T′,start), step, convert(T′,stop))
end

"""
    range(start, stop, length)
    range(start, stop; length, step)
    range(start; length, stop, step)
    range(;start, length, stop, step)

Construct a specialized array with evenly spaced elements and optimized storage (an [`AbstractRange`](@ref)) from the arguments.
Mathematically a range is uniquely determined by any three of `start`, `step`, `stop` and `length`.
Valid invocations of range are:
* Call `range` with any three of `start`, `step`, `stop`, `length`.
* Call `range` with two of `start`, `stop`, `length`. In this case `step` will be assumed
  to be one. If both arguments are Integers, a [`UnitRange`](@ref) will be returned.
* Call `range` with one of `stop` or `length`. `start` and `step` will be assumed to be one.

See Extended Help for additional details on the returned type.

# Examples
```jldoctest
julia> range(1, length=100)
1:100

julia> range(1, stop=100)
1:100

julia> range(1, step=5, length=100)
1:5:496

julia> range(1, step=5, stop=100)
1:5:96

julia> range(1, 10, length=101)
1.0:0.09:10.0

julia> range(1, 100, step=5)
1:5:96

julia> range(stop=10, length=5)
6:10

julia> range(stop=10, step=1, length=5)
6:1:10

julia> range(start=1, step=1, stop=10)
1:1:10

julia> range(; length = 10)
Base.OneTo(10)

julia> range(; stop = 6)
Base.OneTo(6)

julia> range(; stop = 6.5)
1.0:1.0:6.0
```
If `length` is not specified and `stop - start` is not an integer multiple of `step`, a range that ends before `stop` will be produced.
```jldoctest
julia> range(1, 3.5, step=2)
1.0:2.0:3.0
```

Special care is taken to ensure intermediate values are computed rationally.
To avoid this induced overhead, see the [`LinRange`](@ref) constructor.

!!! compat "Julia 1.1"
    `stop` as a positional argument requires at least Julia 1.1.

!!! compat "Julia 1.7"
    The versions without keyword arguments and `start` as a keyword argument
    require at least Julia 1.7.

!!! compat "Julia 1.8"
    The versions with `stop` as a sole keyword argument,
    or `length` as a sole keyword argument require at least Julia 1.8.


# Extended Help

`range` will produce a `Base.OneTo` when the arguments are Integers and
* Only `length` is provided
* Only `stop` is provided

`range` will produce a `UnitRange` when the arguments are Integers and
* Only `start`  and `stop` are provided
* Only `length` and `stop` are provided

A `UnitRange` is not produced if `step` is provided even if specified as one.
"""
function range end

range(start; stop=nothing, length::Union{Integer,Nothing}=nothing, step=nothing) =
    _range(start, step, stop, length)
range(start, stop; length::Union{Integer,Nothing}=nothing, step=nothing) = _range(start, step, stop, length)
range(start, stop, length::Integer) = _range(start, nothing, stop, length)

range(;start=nothing, stop=nothing, length::Union{Integer, Nothing}=nothing, step=nothing) =
    _range(start, step, stop, length)

_range(start::Nothing, step::Nothing, stop::Nothing, len::Nothing) = range_error(start, step, stop, len)
_range(start::Nothing, step::Nothing, stop::Nothing, len::Any    ) = range_length(len)
_range(start::Nothing, step::Nothing, stop::Any    , len::Nothing) = range_stop(stop)
_range(start::Nothing, step::Nothing, stop::Any    , len::Any    ) = range_stop_length(stop, len)
_range(start::Nothing, step::Any    , stop::Nothing, len::Nothing) = range_error(start, step, stop, len)
_range(start::Nothing, step::Any    , stop::Nothing, len::Any    ) = range_error(start, step, stop, len)
_range(start::Nothing, step::Any    , stop::Any    , len::Nothing) = range_error(start, step, stop, len)
_range(start::Nothing, step::Any    , stop::Any    , len::Any    ) = range_step_stop_length(step, stop, len)
_range(start::Any    , step::Nothing, stop::Nothing, len::Nothing) = range_error(start, step, stop, len)
_range(start::Any    , step::Nothing, stop::Nothing, len::Any    ) = range_start_length(start, len)
_range(start::Any    , step::Nothing, stop::Any    , len::Nothing) = range_start_stop(start, stop)
_range(start::Any    , step::Nothing, stop::Any    , len::Any    ) = range_start_stop_length(start, stop, len)
_range(start::Any    , step::Any    , stop::Nothing, len::Nothing) = range_error(start, step, stop, len)
_range(start::Any    , step::Any    , stop::Nothing, len::Any    ) = range_start_step_length(start, step, len)
_range(start::Any    , step::Any    , stop::Any    , len::Nothing) = range_start_step_stop(start, step, stop)
_range(start::Any    , step::Any    , stop::Any    , len::Any    ) = range_error(start, step, stop, len)

# Length as the only argument
range_length(len::Integer) = OneTo(len)

# Stop as the only argument
range_stop(stop) = range_start_stop(oftype(stop, 1), stop)
range_stop(stop::Integer) = range_length(stop)

function range_step_stop_length(step, a, len::Integer)
    start = a - step * (len - oneunit(len))
    if start isa Signed
        # overflow in recomputing length from stop is okay
        return StepRange{typeof(start),typeof(step)}(start, step, convert(typeof(start), a))
    end
    return StepRangeLen{typeof(start),typeof(start),typeof(step)}(start, step, len)
end

# Stop and length as the only argument
function range_stop_length(a, len::Integer)
    step = oftype(a - a, 1) # assert that step is representable
    start = a - (len - oneunit(len))
    if start isa Signed
        # overflow in recomputing length from stop is okay
        return UnitRange(start, oftype(start, a))
    end
    return StepRangeLen{typeof(start),typeof(start),typeof(step)}(start, step, len)
end

# Start and length as the only argument
function range_start_length(a, len::Integer)
    step = oftype(a - a, 1) # assert that step is representable
    stop = a + (len - oneunit(len))
    if stop isa Signed
        # overflow in recomputing length from stop is okay
        return UnitRange(oftype(stop, a), stop)
    end
    return StepRangeLen{typeof(stop),typeof(a),typeof(step)}(a, step, len)
end

range_start_stop(start, stop) = start:stop

function range_start_step_length(a, step, len::Integer)
    stop = a + step * (len - oneunit(len))
    if stop isa Signed
        # overflow in recomputing length from stop is okay
        return StepRange{typeof(stop),typeof(step)}(convert(typeof(stop), a), step, stop)
    end
    return StepRangeLen{typeof(stop),typeof(a),typeof(step)}(a, step, len)
end

range_start_step_stop(start, step, stop) = start:step:stop

function range_error(start, step, stop, length)
    hasstart  = start !== nothing
    hasstep   = step  !== nothing
    hasstop   = stop  !== nothing
    haslength = start !== nothing

    hint = if hasstart && hasstep && hasstop && haslength
        "Try specifying only three arguments"
    elseif !hasstop && !haslength
        "At least one of `length` or `stop` must be specified."
    elseif !hasstep && !haslength
        "At least one of `length` or `step` must be specified."
    elseif !hasstart && !hasstop
        "At least one of `start` or `stop` must be specified."
    else
        "Try specifying more arguments."
    end

    msg = """
    Cannot construct range from arguments:
    start = $start
    step = $step
    stop = $stop
    length = $length
    $hint
    """
    throw(ArgumentError(msg))
end

## 1-dimensional ranges ##

"""
    AbstractRange{T}

Supertype for ranges with elements of type `T`.
[`UnitRange`](@ref) and other types are subtypes of this.
"""
abstract type AbstractRange{T} <: AbstractArray{T,1} end

RangeStepStyle(::Type{<:AbstractRange}) = RangeStepIrregular()
RangeStepStyle(::Type{<:AbstractRange{<:Integer}}) = RangeStepRegular()

convert(::Type{T}, r::AbstractRange) where {T<:AbstractRange} = r isa T ? r : T(r)

## ordinal ranges

"""
    OrdinalRange{T, S} <: AbstractRange{T}

Supertype for ordinal ranges with elements of type `T` with
spacing(s) of type `S`. The steps should be always-exact
multiples of [`oneunit`](@ref), and `T` should be a "discrete"
type, which cannot have values smaller than `oneunit`. For example,
`Integer` or `Date` types would qualify, whereas `Float64` would not (since this
type can represent values smaller than `oneunit(Float64)`.
[`UnitRange`](@ref), [`StepRange`](@ref), and other types are subtypes of this.
"""
abstract type OrdinalRange{T,S} <: AbstractRange{T} end

"""
    AbstractUnitRange{T} <: OrdinalRange{T, T}

Supertype for ranges with a step size of [`oneunit(T)`](@ref) with elements of type `T`.
[`UnitRange`](@ref) and other types are subtypes of this.
"""
abstract type AbstractUnitRange{T} <: OrdinalRange{T,T} end

"""
    StepRange{T, S} <: OrdinalRange{T, S}

Ranges with elements of type `T` with spacing of type `S`. The step
between each element is constant, and the range is defined in terms
of a `start` and `stop` of type `T` and a `step` of type `S`. Neither
`T` nor `S` should be floating point types. The syntax `a:b:c` with `b > 1`
and `a`, `b`, and `c` all integers creates a `StepRange`.

# Examples
```jldoctest
julia> collect(StepRange(1, Int8(2), 10))
5-element Vector{Int64}:
 1
 3
 5
 7
 9

julia> typeof(StepRange(1, Int8(2), 10))
StepRange{Int64, Int8}

julia> typeof(1:3:6)
StepRange{Int64, Int64}
```
"""
struct StepRange{T,S} <: OrdinalRange{T,S}
    start::T
    step::S
    stop::T

    function StepRange{T,S}(start, step, stop) where {T,S}
        start = convert(T, start)
        step = convert(S, step)
        stop = convert(T, stop)
        return new(start, step, steprange_last(start, step, stop))
    end
end

# to make StepRange constructor inlineable, so optimizer can see `step` value
function steprange_last(start, step, stop)::typeof(stop)
    if isa(start, AbstractFloat) || isa(step, AbstractFloat)
        throw(ArgumentError("StepRange should not be used with floating point"))
    end
    if isa(start, Integer) && !isinteger(start + step)
        throw(ArgumentError("StepRange{<:Integer} cannot have non-integer step"))
    end
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if stop == start
        last = stop
    else
        if (step > z) != (stop > start)
            last = steprange_last_empty(start, step, stop)
        else
            # Compute absolute value of difference between `start` and `stop`
            # (to simplify handling both signed and unsigned T and checking for signed overflow):
            absdiff, absstep = stop > start ? (stop - start, step) : (start - stop, -step)

            # Compute remainder as a nonnegative number:
            if absdiff isa Signed && absdiff < zero(absdiff)
                # unlikely, but handle the signed overflow case with unsigned rem
                remain = convert(typeof(absdiff), unsigned(absdiff) % absstep)
            else
                remain = convert(typeof(absdiff), absdiff % absstep)
            end
            # Move `stop` closer to `start` if there is a remainder:
            last = stop > start ? stop - remain : stop + remain
        end
    end
    return last
end

function steprange_last_empty(start::Integer, step, stop)::typeof(stop)
    # empty range has a special representation where stop = start-1,
    # which simplifies arithmetic for Signed numbers
    if step > zero(step)
        last = start - oneunit(step)
    else
        last = start + oneunit(step)
    end
    return last
end
# For types where x+oneunit(x) may not be well-defined use the user-given value for stop
steprange_last_empty(start, step, stop) = stop

StepRange{T}(start, step::S, stop) where {T,S} = StepRange{T,S}(start, step, stop)
StepRange(start::T, step::S, stop::T) where {T,S} = StepRange{T,S}(start, step, stop)

"""
    UnitRange{T<:Real}

A range parameterized by a `start` and `stop` of type `T`, filled
with elements spaced by `1` from `start` until `stop` is exceeded.
The syntax `a:b` with `a` and `b` both `Integer`s creates a `UnitRange`.

# Examples
```jldoctest
julia> collect(UnitRange(2.3, 5.2))
3-element Vector{Float64}:
 2.3
 3.3
 4.3

julia> typeof(1:10)
UnitRange{Int64}
```
"""
struct UnitRange{T<:Real} <: AbstractUnitRange{T}
    start::T
    stop::T
    UnitRange{T}(start::T, stop::T) where {T<:Real} = new(start, unitrange_last(start, stop))
end
UnitRange{T}(start, stop) where {T<:Real} = UnitRange{T}(convert(T, start), convert(T, stop))
UnitRange(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)
UnitRange(start, stop) = UnitRange(promote(start, stop)...)

# if stop and start are integral, we know that their difference is a multiple of 1
unitrange_last(start::Integer, stop::Integer) =
    stop >= start ? stop : convert(typeof(stop), start - oneunit(start - stop))
# otherwise, use `floor` as a more efficient way to compute modulus with step=1
unitrange_last(start, stop) =
    stop >= start ? convert(typeof(stop), start + floor(stop - start)) :
                    convert(typeof(stop), start - oneunit(start - stop))

unitrange(x::AbstractUnitRange) = UnitRange(x) # convenience conversion for promoting the range type

if isdefined(Main, :Base)
    # Constant-fold-able indexing into tuples to functionally expose Base.tail and Base.front
    function getindex(@nospecialize(t::Tuple), r::AbstractUnitRange)
        @inline
        require_one_based_indexing(r)
        if length(r) <= 10
            return ntuple(i -> t[i + first(r) - 1], length(r))
        elseif first(r) == 1
            last(r) == length(t)   && return t
            last(r) == length(t)-1 && return front(t)
            last(r) == length(t)-2 && return front(front(t))
        elseif last(r) == length(t)
            first(r) == 2 && return tail(t)
            first(r) == 3 && return tail(tail(t))
        end
        return (eltype(t)[t[ri] for ri in r]...,)
    end
end

"""
    Base.OneTo(n)

Define an `AbstractUnitRange` that behaves like `1:n`, with the added
distinction that the lower limit is guaranteed (by the type system) to
be 1.
"""
struct OneTo{T<:Integer} <: AbstractUnitRange{T}
    stop::T
    function OneTo{T}(stop) where {T<:Integer}
        throwbool(r)  = (@noinline; throw(ArgumentError("invalid index: $r of type Bool")))
        T === Bool && throwbool(stop)
        return new(max(zero(T), stop))
    end

    function OneTo{T}(r::AbstractRange) where {T<:Integer}
        throwstart(r) = (@noinline; throw(ArgumentError("first element must be 1, got $(first(r))")))
        throwstep(r)  = (@noinline; throw(ArgumentError("step must be 1, got $(step(r))")))
        throwbool(r)  = (@noinline; throw(ArgumentError("invalid index: $r of type Bool")))
        first(r) == 1 || throwstart(r)
        step(r)  == 1 || throwstep(r)
        T === Bool && throwbool(r)
        return new(max(zero(T), last(r)))
    end
end
OneTo(stop::T) where {T<:Integer} = OneTo{T}(stop)
OneTo(r::AbstractRange{T}) where {T<:Integer} = OneTo{T}(r)
oneto(r) = OneTo(r)

## Step ranges parameterized by length

"""
    StepRangeLen(         ref::R, step::S, len, [offset=1]) where {  R,S}
    StepRangeLen{T,R,S}(  ref::R, step::S, len, [offset=1]) where {T,R,S}
    StepRangeLen{T,R,S,L}(ref::R, step::S, len, [offset=1]) where {T,R,S,L}

A range `r` where `r[i]` produces values of type `T` (in the first
form, `T` is deduced automatically), parameterized by a `ref`erence
value, a `step`, and the `len`gth. By default `ref` is the starting
value `r[1]`, but alternatively you can supply it as the value of
`r[offset]` for some other index `1 <= offset <= len`. In conjunction
with `TwicePrecision` this can be used to implement ranges that are
free of roundoff error.

!!! compat "Julia 1.7"
    The 4th type parameter `L` requires at least Julia 1.7.
"""
struct StepRangeLen{T,R,S,L<:Integer} <: AbstractRange{T}
    ref::R       # reference value (might be smallest-magnitude value in the range)
    step::S      # step value
    len::L       # length of the range
    offset::L    # the index of ref

    function StepRangeLen{T,R,S,L}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S,L}
        if T <: Integer && !isinteger(ref + step)
            throw(ArgumentError("StepRangeLen{<:Integer} cannot have non-integer step"))
        end
        len = convert(L, len)
        len >= zero(len) || throw(ArgumentError("length cannot be negative, got $len"))
        offset = convert(L, offset)
        L1 = oneunit(typeof(len))
        L1 <= offset <= max(L1, len) || throw(ArgumentError("StepRangeLen: offset must be in [1,$len], got $offset"))
        return new(ref, step, len, offset)
    end
end

StepRangeLen{T,R,S}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S} =
    StepRangeLen{T,R,S,promote_type(Int,typeof(len))}(ref, step, len, offset)
StepRangeLen(ref::R, step::S, len::Integer, offset::Integer = 1) where {R,S} =
    StepRangeLen{typeof(ref+zero(step)),R,S,promote_type(Int,typeof(len))}(ref, step, len, offset)
StepRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S} =
    StepRangeLen{T,R,S,promote_type(Int,typeof(len))}(ref, step, len, offset)

## range with computed step

"""
    LinRange{T,L}

A range with `len` linearly spaced elements between its `start` and `stop`.
The size of the spacing is controlled by `len`, which must
be an `Integer`.

# Examples
```jldoctest
julia> LinRange(1.5, 5.5, 9)
9-element LinRange{Float64, Int64}:
 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5
```

Compared to using [`range`](@ref), directly constructing a `LinRange` should
have less overhead but won't try to correct for floating point errors:
```jldoctest
julia> collect(range(-0.1, 0.3, length=5))
5-element Vector{Float64}:
 -0.1
  0.0
  0.1
  0.2
  0.3

julia> collect(LinRange(-0.1, 0.3, 5))
5-element Vector{Float64}:
 -0.1
 -1.3877787807814457e-17
  0.09999999999999999
  0.19999999999999998
  0.3
```
"""
struct LinRange{T,L<:Integer} <: AbstractRange{T}
    start::T
    stop::T
    len::L
    lendiv::L

    function LinRange{T,L}(start::T, stop::T, len::L) where {T,L<:Integer}
        len >= 0 || throw(ArgumentError("range($start, stop=$stop, length=$len): negative length"))
        onelen = oneunit(typeof(len))
        if len == onelen
            start == stop || throw(ArgumentError("range($start, stop=$stop, length=$len): endpoints differ"))
            return new(start, stop, len, len)
        end
        lendiv = max(len - onelen, onelen)
        if T <: Integer && !iszero(mod(stop-start, lendiv))
            throw(ArgumentError("LinRange{<:Integer} cannot have non-integer step"))
        end
        return new(start, stop, len, lendiv)
    end
end

function LinRange{T,L}(start, stop, len::Integer) where {T,L}
    LinRange{T,L}(convert(T, start), convert(T, stop), convert(L, len))
end

function LinRange{T}(start, stop, len::Integer) where T
    LinRange{T,promote_type(Int,typeof(len))}(start, stop, len)
end

function LinRange(start, stop, len::Integer)
    T = typeof((zero(stop) - zero(start)) / oneunit(len))
    LinRange{T}(start, stop, len)
end

range_start_stop_length(start, stop, len::Integer) =
    range_start_stop_length(promote(start, stop)..., len)
range_start_stop_length(start::T, stop::T, len::Integer) where {T} = LinRange(start, stop, len)
range_start_stop_length(start::T, stop::T, len::Integer) where {T<:Integer} =
    _linspace(float(T), start, stop, len)
## for Float16, Float32, and Float64 we hit twiceprecision.jl to lift to higher precision StepRangeLen
# for all other types we fall back to a plain old LinRange
_linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where T = LinRange{T}(start, stop, len)

function show(io::IO, r::LinRange{T}) where {T}
    print(io, "LinRange{")
    show(io, T)
    print(io, "}(")
    show(io, first(r))
    print(io, ", ")
    show(io, last(r))
    print(io, ", ")
    show(io, length(r))
    print(io, ')')
end

"""
`print_range(io, r)` prints out a nice looking range r in terms of its elements
as if it were `collect(r)`, dependent on the size of the
terminal, and taking into account whether compact numbers should be shown.
It figures out the width in characters of each element, and if they
end up too wide, it shows the first and last elements separated by a
horizontal ellipsis. Typical output will look like `1.0, 2.0, …, 5.0, 6.0`.

`print_range(io, r, pre, sep, post, hdots)` uses optional
parameters `pre` and `post` characters for each printed row,
`sep` separator string between printed elements,
`hdots` string for the horizontal ellipsis.
"""
function print_range(io::IO, r::AbstractRange,
                     pre::AbstractString = " ",
                     sep::AbstractString = ", ",
                     post::AbstractString = "",
                     hdots::AbstractString = ", \u2026, ") # horiz ellipsis
    # This function borrows from print_matrix() in show.jl
    # and should be called by show and display
    sz = displaysize(io)
    if !haskey(io, :compact)
        io = IOContext(io, :compact => true)
    end
    screenheight, screenwidth = sz[1] - 4, sz[2]
    screenwidth -= length(pre) + length(post)
    postsp = ""
    sepsize = length(sep)
    m = 1 # treat the range as a one-row matrix
    n = length(r)
    # Figure out spacing alignments for r, but only need to examine the
    # left and right edge columns, as many as could conceivably fit on the
    # screen, with the middle columns summarized by horz, vert, or diag ellipsis
    maxpossiblecols = div(screenwidth, 1+sepsize) # assume each element is at least 1 char + 1 separator
    colsr = n <= maxpossiblecols ? (1:n) : [1:div(maxpossiblecols,2)+1; (n-div(maxpossiblecols,2)):n]
    rowmatrix = reshape(r[colsr], 1, length(colsr)) # treat the range as a one-row matrix for print_matrix_row
    nrow, idxlast = size(rowmatrix, 2), last(axes(rowmatrix, 2))
    A = alignment(io, rowmatrix, 1:m, 1:length(rowmatrix), screenwidth, screenwidth, sepsize, nrow) # how much space range takes
    if n <= length(A) # cols fit screen, so print out all elements
        print(io, pre) # put in pre chars
        print_matrix_row(io,rowmatrix,A,1,1:n,sep,idxlast) # the entire range
        print(io, post) # add the post characters
    else # cols don't fit so put horiz ellipsis in the middle
        # how many chars left after dividing width of screen in half
        # and accounting for the horiz ellipsis
        c = div(screenwidth-length(hdots)+1,2)+1 # chars remaining for each side of rowmatrix
        alignR = reverse(alignment(io, rowmatrix, 1:m, length(rowmatrix):-1:1, c, c, sepsize, nrow)) # which cols of rowmatrix to put on the right
        c = screenwidth - sum(map(sum,alignR)) - (length(alignR)-1)*sepsize - length(hdots)
        alignL = alignment(io, rowmatrix, 1:m, 1:length(rowmatrix), c, c, sepsize, nrow) # which cols of rowmatrix to put on the left
        print(io, pre)   # put in pre chars
        print_matrix_row(io, rowmatrix,alignL,1,1:length(alignL),sep,idxlast) # left part of range
        print(io, hdots) # horizontal ellipsis
        print_matrix_row(io, rowmatrix,alignR,1,length(rowmatrix)-length(alignR)+1:length(rowmatrix),sep,idxlast) # right part of range
        print(io, post)  # post chars
    end
end

## interface implementations

length(r::AbstractRange) = error("length implementation missing") # catch mistakes
size(r::AbstractRange) = (length(r),)

isempty(r::StepRange) =
    # steprange_last(r.start, r.step, r.stop) == r.stop
    (r.start != r.stop) & ((r.step > zero(r.step)) != (r.stop > r.start))
isempty(r::AbstractUnitRange) = first(r) > last(r)
isempty(r::StepRangeLen) = length(r) == 0
isempty(r::LinRange) = length(r) == 0

"""
    step(r)

Get the step size of an [`AbstractRange`](@ref) object.

# Examples
```jldoctest
julia> step(1:10)
1

julia> step(1:2:10)
2

julia> step(2.5:0.3:10.9)
0.3

julia> step(range(2.5, stop=10.9, length=85))
0.1
```
"""
step(r::StepRange) = r.step
step(r::AbstractUnitRange{T}) where {T} = oneunit(T) - zero(T)
step(r::StepRangeLen) = r.step
step(r::StepRangeLen{T}) where {T<:AbstractFloat} = T(r.step)
step(r::LinRange) = (last(r)-first(r))/r.lendiv

# high-precision step
step_hp(r::StepRangeLen) = r.step
step_hp(r::AbstractRange) = step(r)

axes(r::AbstractRange) = (oneto(length(r)),)

# n.b. checked_length for these is defined iff checked_add and checked_sub are
# defined between the relevant types
function checked_length(r::OrdinalRange{T}) where T
    s = step(r)
    start = first(r)
    if isempty(r)
        return Integer(div(start - start, oneunit(s)))
    end
    stop = last(r)
    if isless(s, zero(s))
        diff = checked_sub(start, stop)
        s = -s
    else
        diff = checked_sub(stop, start)
    end
    a = div(diff, s)
    return Integer(checked_add(a, oneunit(a)))
end

function checked_length(r::AbstractUnitRange{T}) where T
    # compiler optimization: remove dead cases from above
    if isempty(r)
        return Integer(first(r) - first(r))
    end
    a = checked_sub(last(r), first(r))
    return Integer(checked_add(a, oneunit(a)))
end

function length(r::OrdinalRange{T}) where T
    s = step(r)
    start = first(r)
    if isempty(r)
        return Integer(div(start - start, oneunit(s)))
    end
    stop = last(r)
    if isless(s, zero(s))
        diff = start - stop
        s = -s
    else
        diff = stop - start
    end
    a = div(diff, s)
    return Integer(a + oneunit(a))
end

function length(r::AbstractUnitRange{T}) where T
    @inline
    start, stop = first(r), last(r)
    a = oneunit(zero(stop) - zero(start))
    if a isa Signed || stop >= start
        a += stop - start # Signed are allowed to go negative
    else
        a = zero(a) # Unsigned don't necessarily underflow
    end
    return Integer(a)
end

length(r::OneTo) = Integer(r.stop - zero(r.stop))
length(r::StepRangeLen) = r.len
length(r::LinRange) = r.len

let bigints = Union{Int, UInt, Int64, UInt64, Int128, UInt128}
    global length, checked_length
    # compile optimization for which promote_type(T, Int) == T
    length(r::OneTo{T}) where {T<:bigints} = r.stop
    # slightly more accurate length and checked_length in extreme cases
    # (near typemax) for types with known `unsigned` functions
    function length(r::OrdinalRange{T}) where T<:bigints
        s = step(r)
        isempty(r) && return zero(T)
        diff = last(r) - first(r)
        # if |s| > 1, diff might have overflowed, but unsigned(diff)÷s should
        # therefore still be valid (if the result is representable at all)
        # n.b. !(s isa T)
        if s isa Unsigned || -1 <= s <= 1 || s == -s
            a = div(diff, s) % T
        elseif s < 0
            a = div(unsigned(-diff), -s) % T
        else
            a = div(unsigned(diff), s) % T
        end
        return a + oneunit(T)
    end
    function checked_length(r::OrdinalRange{T}) where T<:bigints
        s = step(r)
        isempty(r) && return zero(T)
        stop, start = last(r), first(r)
        # n.b. !(s isa T)
        if s > 1
            diff = stop - start
            a = convert(T, div(unsigned(diff), s))
        elseif s < -1
            diff = start - stop
            a = convert(T, div(unsigned(diff), -s))
        elseif s > 0
            a = div(checked_sub(stop, start), s)
        else
            a = div(checked_sub(start, stop), -s)
        end
        return checked_add(convert(T, a), oneunit(T))
    end
end

# some special cases to favor default Int type
let smallints = (Int === Int64 ?
                Union{Int8, UInt8, Int16, UInt16, Int32, UInt32} :
                Union{Int8, UInt8, Int16, UInt16})
    global length, checked_length
    # n.b. !(step isa T)
    function length(r::OrdinalRange{<:smallints})
        s = step(r)
        isempty(r) && return 0
        return div(Int(last(r)) - Int(first(r)), s) + 1
    end
    length(r::AbstractUnitRange{<:smallints}) = Int(last(r)) - Int(first(r)) + 1
    length(r::OneTo{<:smallints}) = Int(r.stop)
    checked_length(r::OrdinalRange{<:smallints}) = length(r)
    checked_length(r::AbstractUnitRange{<:smallints}) = length(r)
    checked_length(r::OneTo{<:smallints}) = length(r)
end

first(r::OrdinalRange{T}) where {T} = convert(T, r.start)
first(r::OneTo{T}) where {T} = oneunit(T)
first(r::StepRangeLen) = unsafe_getindex(r, 1)
first(r::LinRange) = r.start

last(r::OrdinalRange{T}) where {T} = convert(T, r.stop) # via steprange_last
last(r::StepRangeLen) = unsafe_getindex(r, length(r))
last(r::LinRange) = r.stop

minimum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : first(r)
maximum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : last(r)
minimum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : min(first(r), last(r))
maximum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : max(first(r), last(r))

"""
    argmin(r::AbstractRange)

Ranges can have multiple minimal elements. In that case
`argmin` will return a minimal index, but not necessarily the
first one.
"""
function argmin(r::AbstractRange)
    if isempty(r)
        throw(ArgumentError("range must be non-empty"))
    elseif step(r) > 0
        firstindex(r)
    else
        lastindex(r)
    end
end

"""
    argmax(r::AbstractRange)

Ranges can have multiple maximal elements. In that case
`argmax` will return a maximal index, but not necessarily the
first one.
"""
function argmax(r::AbstractRange)
    if isempty(r)
        throw(ArgumentError("range must be non-empty"))
    elseif step(r) > 0
        lastindex(r)
    else
        firstindex(r)
    end
end

extrema(r::AbstractRange) = (minimum(r), maximum(r))

# Ranges are immutable
copy(r::AbstractRange) = r


## iteration

function iterate(r::Union{StepRangeLen,LinRange}, i::Integer=zero(length(r)))
    @inline
    i += oneunit(i)
    length(r) < i && return nothing
    unsafe_getindex(r, i), i
end

iterate(r::OrdinalRange) = isempty(r) ? nothing : (first(r), first(r))

function iterate(r::OrdinalRange{T}, i) where {T}
    @inline
    i == last(r) && return nothing
    next = convert(T, i + step(r))
    (next, next)
end

## indexing

_in_unit_range(v::UnitRange, val, i::Integer) = i > 0 && val <= v.stop && val >= v.start

function getindex(v::UnitRange{T}, i::Integer) where T
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    val = convert(T, v.start + (i - oneunit(i)))
    @boundscheck _in_unit_range(v, val, i) || throw_boundserror(v, i)
    val
end

const OverflowSafe = Union{Bool,Int8,Int16,Int32,Int64,Int128,
                           UInt8,UInt16,UInt32,UInt64,UInt128}

function getindex(v::UnitRange{T}, i::Integer) where {T<:OverflowSafe}
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    val = v.start + (i - oneunit(i))
    @boundscheck _in_unit_range(v, val, i) || throw_boundserror(v, i)
    val % T
end

function getindex(v::OneTo{T}, i::Integer) where T
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    @boundscheck ((i > 0) & (i <= v.stop)) || throw_boundserror(v, i)
    convert(T, i)
end

function getindex(v::AbstractRange{T}, i::Integer) where T
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    ret = convert(T, first(v) + (i - oneunit(i))*step_hp(v))
    ok = ifelse(step(v) > zero(step(v)),
                (ret <= last(v)) & (ret >= first(v)),
                (ret <= first(v)) & (ret >= last(v)))
    @boundscheck ((i > 0) & ok) || throw_boundserror(v, i)
    ret
end

function getindex(r::Union{StepRangeLen,LinRange}, i::Integer)
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    @boundscheck checkbounds(r, i)
    unsafe_getindex(r, i)
end

# This is separate to make it useful even when running with --check-bounds=yes
function unsafe_getindex(r::StepRangeLen{T}, i::Integer) where T
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    u = i - r.offset
    T(r.ref + u*r.step)
end

function _getindex_hiprec(r::StepRangeLen, i::Integer)  # without rounding by T
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    u = i - r.offset
    r.ref + u*r.step
end

function unsafe_getindex(r::LinRange, i::Integer)
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    lerpi(i-oneunit(i), r.lendiv, r.start, r.stop)
end

function lerpi(j::Integer, d::Integer, a::T, b::T) where T
    @inline
    t = j/d # ∈ [0,1]
    # compute approximately fma(t, b, -fma(t, a, a))
    return T((1-t)*a + t*b)
end

getindex(r::AbstractRange, ::Colon) = copy(r)

function getindex(r::AbstractUnitRange, s::AbstractUnitRange{T}) where {T<:Integer}
    @inline
    @boundscheck checkbounds(r, s)

    if T === Bool
        return range(first(s) ? first(r) : last(r), length = last(s))
    else
        f = first(r)
        start = oftype(f, f + first(s) - firstindex(r))
        len = length(s)
        stop = oftype(f, start + (len - oneunit(len)))
        return range(start, stop)
    end
end

function getindex(r::OneTo{T}, s::OneTo) where T
    @inline
    @boundscheck checkbounds(r, s)
    return OneTo(T(s.stop))
end

function getindex(r::AbstractUnitRange, s::StepRange{T}) where {T<:Integer}
    @inline
    @boundscheck checkbounds(r, s)

    if T === Bool
        return range(first(s) ? first(r) : last(r), step=oneunit(eltype(r)), length=last(s))
    else
        f = first(r)
        start = oftype(f, f + s.start - firstindex(r))
        st = step(s)
        len = length(s)
        stop = oftype(f, start + (len - oneunit(len)) * st)
        return range(start, stop; step=st)
    end
end

function getindex(r::StepRange, s::AbstractRange{T}) where {T<:Integer}
    @inline
    @boundscheck checkbounds(r, s)

    if T === Bool
        if length(s) == 0
            start, len = first(r), 0
        elseif length(s) == 1
            if first(s)
                start, len = first(r), 1
            else
                start, len = first(r), 0
            end
        else # length(s) == 2
            start, len = last(r), 1
        end
        return range(start, step=step(r); length=len)
    else
        f = r.start
        fs = first(s)
        st = r.step
        start = oftype(f, f + (fs - oneunit(fs)) * st)
        st = st * step(s)
        len = length(s)
        stop = oftype(f, start + (len - oneunit(len)) * st)
        return range(start, stop; step=st)
    end
end

function getindex(r::StepRangeLen{T}, s::OrdinalRange{S}) where {T, S<:Integer}
    @inline
    @boundscheck checkbounds(r, s)

    len = length(s)
    sstep = step_hp(s)
    rstep = step_hp(r)
    L = typeof(len)
    if S === Bool
        rstep *= one(sstep)
        if len == 0
            return StepRangeLen{T}(first(r), rstep, zero(L), oneunit(L))
        elseif len == 1
            if first(s)
                return StepRangeLen{T}(first(r), rstep, oneunit(L), oneunit(L))
            else
                return StepRangeLen{T}(first(r), rstep, zero(L), oneunit(L))
            end
        else # len == 2
            return StepRangeLen{T}(last(r), rstep, oneunit(L), oneunit(L))
        end
    else
        # Find closest approach to offset by s
        ind = LinearIndices(s)
        offset = L(max(min(1 + round(L, (r.offset - first(s))/sstep), last(ind)), first(ind)))
        ref = _getindex_hiprec(r, first(s) + (offset - oneunit(offset)) * sstep)
        return StepRangeLen{T}(ref, rstep*sstep, len, offset)
    end
end

function getindex(r::LinRange{T}, s::OrdinalRange{S}) where {T, S<:Integer}
    @inline
    @boundscheck checkbounds(r, s)

    len = length(s)
    L = typeof(len)
    if S === Bool
        if len == 0
            return LinRange{T}(first(r), first(r), len)
        elseif len == 1
            if first(s)
                return LinRange{T}(first(r), first(r), len)
            else
                return LinRange{T}(first(r), first(r), zero(L))
            end
        else # length(s) == 2
            return LinRange{T}(last(r), last(r), oneunit(L))
        end
    else
        vfirst = unsafe_getindex(r, first(s))
        vlast  = unsafe_getindex(r, last(s))
        return LinRange{T}(vfirst, vlast, len)
    end
end

show(io::IO, r::AbstractRange) = print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
show(io::IO, r::UnitRange) = print(io, repr(first(r)), ':', repr(last(r)))
show(io::IO, r::OneTo) = print(io, "Base.OneTo(", r.stop, ")")
function show(io::IO, r::StepRangeLen)
    if step(r) != 0
        print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
    else
        # ugly temporary printing, to avoid 0:0:0 etc.
        print(io, "StepRangeLen(", repr(first(r)), ", ", repr(step(r)), ", ", repr(length(r)), ")")
    end
end

function ==(r::T, s::T) where {T<:AbstractRange}
    isempty(r) && return isempty(s)
    _has_length_one(r) && return _has_length_one(s) & (first(r) == first(s))
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
end

function ==(r::OrdinalRange, s::OrdinalRange)
    isempty(r) && return isempty(s)
    _has_length_one(r) && return _has_length_one(s) & (first(r) == first(s))
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
end

==(r::AbstractUnitRange, s::AbstractUnitRange) =
    (isempty(r) & isempty(s)) | ((first(r) == first(s)) & (last(r) == last(s)))

==(r::OneTo, s::OneTo) = last(r) == last(s)

==(r::T, s::T) where {T<:Union{StepRangeLen,LinRange}} =
    (isempty(r) & isempty(s)) | ((first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s)))

function ==(r::Union{StepRange{T},StepRangeLen{T,T}}, s::Union{StepRange{T},StepRangeLen{T,T}}) where {T}
    isempty(r) && return isempty(s)
    _has_length_one(r) && return _has_length_one(s) & (first(r) == first(s))
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
end

_has_length_one(r::OrdinalRange) = first(r) == last(r)
_has_length_one(r::AbstractRange) = isone(length(r))

function ==(r::AbstractRange, s::AbstractRange)
    lr = length(r)
    if lr != length(s)
        return false
    elseif iszero(lr)
        return true
    end
    yr, ys = iterate(r), iterate(s)
    while yr !== nothing
        yr[1] == ys[1] || return false
        yr, ys = iterate(r, yr[2]), iterate(s, ys[2])
    end
    return true
end

intersect(r::OneTo, s::OneTo) = OneTo(min(r.stop,s.stop))
union(r::OneTo, s::OneTo) = OneTo(max(r.stop,s.stop))

intersect(r::AbstractUnitRange{<:Integer}, s::AbstractUnitRange{<:Integer}) = max(first(r),first(s)):min(last(r),last(s))

intersect(i::Integer, r::AbstractUnitRange{<:Integer}) = range(max(i, first(r)), length=in(i, r))

intersect(r::AbstractUnitRange{<:Integer}, i::Integer) = intersect(i, r)

function intersect(r::AbstractUnitRange{<:Integer}, s::StepRange{<:Integer})
    T = promote_type(eltype(r), eltype(s))
    if isempty(s)
        StepRange{T}(first(r), +step(s), first(r)-step(s))
    else
        sta, ste, sto = first_step_last_ascending(s)
        lo = first(r)
        hi = last(r)
        i0 = max(sta, lo + mod(sta - lo, ste))
        i1 = min(sto, hi - mod(hi - sta, ste))
        StepRange{T}(i0, ste, i1)
    end
end

function first_step_last_ascending(r::StepRange)
    if step(r) < zero(step(r))
        last(r), -step(r), first(r)
    else
        first(r), +step(r), last(r)
    end
end

function intersect(r::StepRange{<:Integer}, s::AbstractUnitRange{<:Integer})
    if step(r) < 0
        reverse(intersect(s, reverse(r)))
    else
        intersect(s, r)
    end
end

function intersect(r::StepRange, s::StepRange)
    T = promote_type(eltype(r), eltype(s))
    S = promote_type(typeof(step(r)), typeof(step(s)))
    if isempty(r) || isempty(s)
        return StepRange{T,S}(first(r), step(r), first(r)-step(r))
    end

    start1, step1, stop1 = first_step_last_ascending(r)
    start2, step2, stop2 = first_step_last_ascending(s)
    a = lcm(step1, step2)

    g, x, y = gcdx(step1, step2)

    if !iszero(rem(start1 - start2, g))
        # Unaligned, no overlap possible.
        if  step(r) < zero(step(r))
            return StepRange{T,S}(stop1, -a, stop1+a)
        else
            return StepRange{T,S}(start1, a, start1-a)
        end
    end

    z = div(start1 - start2, g)
    b = start1 - x * z * step1
    # Possible points of the intersection of r and s are
    # ..., b-2a, b-a, b, b+a, b+2a, ...
    # Determine where in the sequence to start and stop.
    m = max(start1 + mod(b - start1, a), start2 + mod(b - start2, a))
    n = min(stop1 - mod(stop1 - b, a), stop2 - mod(stop2 - b, a))
    step(r) < zero(step(r)) ? StepRange{T,S}(n, -a, m) : StepRange{T,S}(m, a, n)
end

function intersect(r1::AbstractRange, r2::AbstractRange)
    # To iterate over the shorter range
    length(r1) > length(r2) && return intersect(r2, r1)

    r1 = unique(r1)
    T = promote_eltype(r1, r2)

    return T[x for x in r1 if x ∈ r2]
end

function intersect(r1::AbstractRange, r2::AbstractRange, r3::AbstractRange, r::AbstractRange...)
    i = intersect(intersect(r1, r2), r3)
    for t in r
        i = intersect(i, t)
    end
    i
end

# _findin (the index of intersection)
function _findin(r::AbstractRange{<:Integer}, span::AbstractUnitRange{<:Integer})
    local ifirst
    local ilast
    fspan = first(span)
    lspan = last(span)
    fr = first(r)
    lr = last(r)
    sr = step(r)
    if sr > 0
        ifirst = fr >= fspan ? 1 : ceil(Integer,(fspan-fr)/sr)+1
        ilast = lr <= lspan ? length(r) : length(r) - ceil(Integer,(lr-lspan)/sr)
    elseif sr < 0
        ifirst = fr <= lspan ? 1 : ceil(Integer,(lspan-fr)/sr)+1
        ilast = lr >= fspan ? length(r) : length(r) - ceil(Integer,(lr-fspan)/sr)
    else
        ifirst = fr >= fspan ? 1 : length(r)+1
        ilast = fr <= lspan ? length(r) : 0
    end
    r isa AbstractUnitRange ? (ifirst:ilast) : (ifirst:1:ilast)
end

issubset(r::OneTo, s::OneTo) = r.stop <= s.stop

issubset(r::AbstractUnitRange{<:Integer}, s::AbstractUnitRange{<:Integer}) =
    isempty(r) || (first(r) >= first(s) && last(r) <= last(s))

## linear operations on ranges ##

-(r::OrdinalRange) = range(-first(r), step=negate(step(r)), length=length(r))
-(r::StepRangeLen{T,R,S,L}) where {T,R,S,L} =
    StepRangeLen{T,R,S,L}(-r.ref, -r.step, r.len, r.offset)
function -(r::LinRange)
    start = -r.start
    LinRange{typeof(start)}(start, -r.stop, length(r))
end

# promote eltype if at least one container wouldn't change, otherwise join container types.
el_same(::Type{T}, a::Type{<:AbstractArray{T,n}}, b::Type{<:AbstractArray{T,n}}) where {T,n}   = a # we assume a === b
el_same(::Type{T}, a::Type{<:AbstractArray{T,n}}, b::Type{<:AbstractArray{S,n}}) where {T,S,n} = a
el_same(::Type{T}, a::Type{<:AbstractArray{S,n}}, b::Type{<:AbstractArray{T,n}}) where {T,S,n} = b
el_same(::Type, a, b) = promote_typejoin(a, b)

promote_result(::Type{<:AbstractArray}, ::Type{<:AbstractArray}, ::Type{T}, ::Type{S}) where {T,S} = (@inline; promote_type(T,S))
promote_result(::Type{T}, ::Type{S}, ::Type{Bottom}, ::Type{Bottom}) where {T<:AbstractArray,S<:AbstractArray} = (@inline; promote_typejoin(T,S))
# If no promote_rule is defined, both directions give Bottom. In that case use typejoin on the eltypes instead and give Array as the container.
promote_result(::Type{<:AbstractArray{T,n}}, ::Type{<:AbstractArray{S,n}}, ::Type{Bottom}, ::Type{Bottom}) where {T,S,n} = (@inline; Array{promote_type(T,S),n})

promote_rule(a::Type{UnitRange{T1}}, b::Type{UnitRange{T2}}) where {T1,T2} =
    el_same(promote_type(T1, T2), a, b)
UnitRange{T}(r::UnitRange{T}) where {T<:Real} = r
UnitRange{T}(r::UnitRange) where {T<:Real} = UnitRange{T}(r.start, r.stop)

promote_rule(a::Type{OneTo{T1}}, b::Type{OneTo{T2}}) where {T1,T2} =
    el_same(promote_type(T1, T2), a, b)
OneTo{T}(r::OneTo{T}) where {T<:Integer} = r
OneTo{T}(r::OneTo) where {T<:Integer} = OneTo{T}(r.stop)

promote_rule(a::Type{UnitRange{T1}}, ::Type{UR}) where {T1,UR<:AbstractUnitRange} =
    promote_rule(a, UnitRange{eltype(UR)})
UnitRange{T}(r::AbstractUnitRange) where {T<:Real} = UnitRange{T}(first(r), last(r))
UnitRange(r::AbstractUnitRange) = UnitRange(first(r), last(r))

AbstractUnitRange{T}(r::AbstractUnitRange{T}) where {T} = r
AbstractUnitRange{T}(r::UnitRange) where {T} = UnitRange{T}(r)
AbstractUnitRange{T}(r::OneTo) where {T} = OneTo{T}(r)

OrdinalRange{T, S}(r::OrdinalRange) where {T, S} = StepRange{T, S}(r)
OrdinalRange{T, T}(r::AbstractUnitRange) where {T} = AbstractUnitRange{T}(r)

function promote_rule(::Type{StepRange{T1a,T1b}}, ::Type{StepRange{T2a,T2b}}) where {T1a,T1b,T2a,T2b}
    Tb = promote_type(T1b, T2b)
    # el_same only operates on array element type, so just promote second type parameter
    el_same(promote_type(T1a, T2a), StepRange{T1a,Tb}, StepRange{T2a,Tb})
end
StepRange{T1,T2}(r::StepRange{T1,T2}) where {T1,T2} = r

promote_rule(a::Type{StepRange{T1a,T1b}}, ::Type{UR}) where {T1a,T1b,UR<:AbstractUnitRange} =
    promote_rule(a, StepRange{eltype(UR), eltype(UR)})
StepRange{T1,T2}(r::AbstractRange) where {T1,T2} =
    StepRange{T1,T2}(convert(T1, first(r)), convert(T2, step(r)), convert(T1, last(r)))
StepRange(r::AbstractUnitRange{T}) where {T} =
    StepRange{T,T}(first(r), step(r), last(r))
(StepRange{T1,T2} where T1)(r::AbstractRange) where {T2} = StepRange{eltype(r),T2}(r)

function promote_rule(::Type{StepRangeLen{T1,R1,S1,L1}},::Type{StepRangeLen{T2,R2,S2,L2}}) where {T1,T2,R1,R2,S1,S2,L1,L2}
    R, S, L = promote_type(R1, R2), promote_type(S1, S2), promote_type(L1, L2)
    el_same(promote_type(T1, T2), StepRangeLen{T1,R,S,L}, StepRangeLen{T2,R,S,L})
end
StepRangeLen{T,R,S,L}(r::StepRangeLen{T,R,S,L}) where {T,R,S,L} = r
StepRangeLen{T,R,S,L}(r::StepRangeLen) where {T,R,S,L} =
    StepRangeLen{T,R,S,L}(convert(R, r.ref), convert(S, r.step), convert(L, r.len), convert(L, r.offset))
StepRangeLen{T}(r::StepRangeLen) where {T} =
    StepRangeLen(convert(T, r.ref), convert(T, r.step), r.len, r.offset)

promote_rule(a::Type{StepRangeLen{T,R,S,L}}, ::Type{OR}) where {T,R,S,L,OR<:AbstractRange} =
    promote_rule(a, StepRangeLen{eltype(OR), eltype(OR), eltype(OR), Int})
StepRangeLen{T,R,S,L}(r::AbstractRange) where {T,R,S,L} =
    StepRangeLen{T,R,S,L}(R(first(r)), S(step(r)), length(r))
StepRangeLen{T}(r::AbstractRange) where {T} =
    StepRangeLen(T(first(r)), T(step(r)), length(r))
StepRangeLen(r::AbstractRange) = StepRangeLen{eltype(r)}(r)

function promote_rule(a::Type{LinRange{T1,L1}}, b::Type{LinRange{T2,L2}}) where {T1,T2,L1,L2}
    L = promote_type(L1, L2)
    el_same(promote_type(T1, T2), LinRange{T1,L}, LinRange{T2,L})
end
LinRange{T,L}(r::LinRange{T,L}) where {T,L} = r
LinRange{T,L}(r::AbstractRange) where {T,L} = LinRange{T,L}(first(r), last(r), length(r))
LinRange{T}(r::AbstractRange) where {T} = LinRange{T,typeof(length(r))}(first(r), last(r), length(r))
LinRange(r::AbstractRange{T}) where {T} = LinRange{T}(r)

promote_rule(a::Type{LinRange{T,L}}, ::Type{OR}) where {T,L,OR<:OrdinalRange} =
    promote_rule(a, LinRange{eltype(OR),L})

promote_rule(::Type{LinRange{A,L}}, b::Type{StepRangeLen{T2,R2,S2,L2}}) where {A,L,T2,R2,S2,L2} =
    promote_rule(StepRangeLen{A,A,A,L}, b)

## concatenation ##

function vcat(rs::AbstractRange{T}...) where T
    n::Int = 0
    for ra in rs
        n += length(ra)
    end
    a = Vector{T}(undef, n)
    i = 1
    for ra in rs, x in ra
        @inbounds a[i] = x
        i += 1
    end
    return a
end

Array{T,1}(r::AbstractRange{T}) where {T} = vcat(r)
collect(r::AbstractRange) = vcat(r)

_reverse(r::OrdinalRange, ::Colon) = (:)(last(r), negate(step(r)), first(r))
function _reverse(r::StepRangeLen, ::Colon)
    # If `r` is empty, `length(r) - r.offset + 1 will be nonpositive hence
    # invalid. As `reverse(r)` is also empty, any offset would work so we keep
    # `r.offset`
    offset = isempty(r) ? r.offset : length(r)-r.offset+1
    return typeof(r)(r.ref, negate(r.step), length(r), offset)
end
_reverse(r::LinRange{T}, ::Colon) where {T} = typeof(r)(r.stop, r.start, length(r))

## sorting ##

issorted(r::AbstractUnitRange) = true
issorted(r::AbstractRange) = length(r) <= 1 || step(r) >= zero(step(r))

sort(r::AbstractUnitRange) = r
sort!(r::AbstractUnitRange) = r

sort(r::AbstractRange) = issorted(r) ? r : reverse(r)

sortperm(r::AbstractUnitRange) = 1:length(r)
sortperm(r::AbstractRange) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum(r::AbstractRange{<:Real})
    l = length(r)
    # note that a little care is required to avoid overflow in l*(l-1)/2
    return l * first(r) + (iseven(l) ? (step(r) * (l-1)) * (l>>1)
                                     : (step(r) * l) * ((l-1)>>1))
end

function _in_range(x, r::AbstractRange)
    if !isfinite(x)
        return false
    elseif iszero(step(r))
        return !isempty(r) && first(r) == x
    else
        n = round(Integer, (x - first(r)) / step(r)) + 1
        return n >= 1 && n <= length(r) && r[n] == x
    end
end
in(x::Real, r::AbstractRange{<:Real}) = _in_range(x, r)
# This method needs to be defined separately since -(::T, ::T) can be implemented
# even if -(::T, ::Real) is not
in(x::T, r::AbstractRange{T}) where {T} = _in_range(x, r)

in(x::Integer, r::AbstractUnitRange{<:Integer}) = (first(r) <= x) & (x <= last(r))

in(x::Real, r::AbstractRange{T}) where {T<:Integer} =
    isinteger(x) && !isempty(r) &&
    (iszero(step(r)) ? x == first(r) : (x >= minimum(r) && x <= maximum(r) &&
        (mod(convert(T,x),step(r))-mod(first(r),step(r)) == 0)))
in(x::AbstractChar, r::AbstractRange{<:AbstractChar}) =
    !isempty(r) &&
    (iszero(step(r)) ? x == first(r) : (x >= minimum(r) && x <= maximum(r) &&
        (mod(Int(x) - Int(first(r)), step(r)) == 0)))

# Addition/subtraction of ranges

function _define_range_op(@nospecialize f)
    @eval begin
        function $f(r1::OrdinalRange, r2::OrdinalRange)
            r1l = length(r1)
            (r1l == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match: length of r1 is $r1l, length of r2 is $(length(r2))")))
            StepRangeLen($f(first(r1), first(r2)), $f(step(r1), step(r2)), r1l)
        end

        function $f(r1::LinRange{T}, r2::LinRange{T}) where T
            len = r1.len
            (len == r2.len ||
             throw(DimensionMismatch("argument dimensions must match: length of r1 is $len, length of r2 is $(r2.len)")))
            LinRange{T}(convert(T, $f(first(r1), first(r2))),
                        convert(T, $f(last(r1), last(r2))), len)
        end

        $f(r1::Union{StepRangeLen, OrdinalRange, LinRange},
           r2::Union{StepRangeLen, OrdinalRange, LinRange}) =
               $f(promote(r1, r2)...)
    end
end
_define_range_op(:+)
_define_range_op(:-)

function +(r1::StepRangeLen{T,S}, r2::StepRangeLen{T,S}) where {T,S}
    len = length(r1)
    (len == length(r2) ||
     throw(DimensionMismatch("argument dimensions must match: length of r1 is $len, length of r2 is $(length(r2))")))
    StepRangeLen(first(r1)+first(r2), step(r1)+step(r2), len)
end

-(r1::StepRangeLen, r2::StepRangeLen) = +(r1, -r2)

# Modular arithmetic on ranges

"""
    mod(x::Integer, r::AbstractUnitRange)

Find `y` in the range `r` such that ``x ≡ y (mod n)``, where `n = length(r)`,
i.e. `y = mod(x - first(r), n) + first(r)`.

See also [`mod1`](@ref).

# Examples
```jldoctest
julia> mod(0, Base.OneTo(3))  # mod1(0, 3)
3

julia> mod(3, 0:2)  # mod(3, 3)
0
```

!!! compat "Julia 1.3"
     This method requires at least Julia 1.3.
"""
mod(i::Integer, r::OneTo) = mod1(i, last(r))
mod(i::Integer, r::AbstractUnitRange{<:Integer}) = mod(i-first(r), length(r)) + first(r)

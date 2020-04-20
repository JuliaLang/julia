# This file is a part of Julia. License is MIT: https://julialang.org/license

(:)(a::Real, b::Real) = (:)(promote(a,b)...)

(:)(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

(:)(start::T, stop::T) where {T} = (:)(start, oftype(stop-start, 1), stop)

# promote start and stop, leaving step alone
(:)(start::A, step, stop::C) where {A<:Real,C<:Real} =
    (:)(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))

# AbstractFloat specializations
(:)(a::T, b::T) where {T<:AbstractFloat} = (:)(a, T(1), b)

(:)(a::T, b::AbstractFloat, c::T) where {T<:Real} = (:)(promote(a,b,c)...)
(:)(a::T, b::AbstractFloat, c::T) where {T<:AbstractFloat} = (:)(promote(a,b,c)...)
(:)(a::T, b::Real, c::T) where {T<:AbstractFloat} = (:)(promote(a,b,c)...)

(:)(start::T, step::T, stop::T) where {T<:AbstractFloat} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
(:)(start::T, step::T, stop::T) where {T<:Real} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
_colon(::Ordered, ::Any, start::T, step, stop::T) where {T} = StepRange(start, step, stop)
# for T<:Union{Float16,Float32,Float64} see twiceprecision.jl
_colon(::Ordered, ::ArithmeticRounds, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)
_colon(::Any, ::Any, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)

"""
    (:)(start, [step], stop)

Range operator. `a:b` constructs a range from `a` to `b` with a step size of 1 (a [`UnitRange`](@ref))
, and `a:s:b` is similar but uses a step size of `s` (a [`StepRange`](@ref)).

`:` is also used in indexing to select whole dimensions
 and for [`Symbol`](@ref) literals, as in e.g. `:hello`.
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
    range(start[, stop]; length, stop, step=1)

Given a starting value, construct a range either by length or from `start` to `stop`,
optionally with a given step (defaults to 1, a [`UnitRange`](@ref)).
One of `length` or `stop` is required.  If `length`, `stop`, and `step` are all specified, they must agree.

If `length` and `stop` are provided and `step` is not, the step size will be computed
automatically such that there are `length` linearly spaced elements in the range.

If `step` and `stop` are provided and `length` is not, the overall range length will be computed
automatically such that the elements are `step` spaced.

Special care is taken to ensure intermediate values are computed rationally.
To avoid this induced overhead, see the [`LinRange`](@ref) constructor.

`stop` may be specified as either a positional or keyword argument.

!!! compat "Julia 1.1"
    `stop` as a positional argument requires at least Julia 1.1.

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
```
"""
range(start; length::Union{Integer,Nothing}=nothing, stop=nothing, step=nothing) =
    _range(start, step, stop, length)

range(start, stop; length::Union{Integer,Nothing}=nothing, step=nothing) =
    _range2(start, step, stop, length)

_range2(start, ::Nothing, stop, ::Nothing) =
    throw(ArgumentError("At least one of `length` or `step` must be specified"))

_range2(start, step, stop, length) = _range(start, step, stop, length)

# Range from start to stop: range(a, [step=s,] stop=b), no length
_range(start, step,      stop, ::Nothing) = (:)(start, step, stop)
_range(start, ::Nothing, stop, ::Nothing) = (:)(start, stop)

# Range of a given length: range(a, [step=s,] length=l), no stop
_range(a::Real,          ::Nothing,         ::Nothing, len::Integer) = UnitRange{typeof(a)}(a, oftype(a, a+len-1))
_range(a::AbstractFloat, ::Nothing,         ::Nothing, len::Integer) = _range(a, oftype(a, 1),   nothing, len)
_range(a::AbstractFloat, st::AbstractFloat, ::Nothing, len::Integer) = _range(promote(a, st)..., nothing, len)
_range(a::Real,          st::AbstractFloat, ::Nothing, len::Integer) = _range(float(a), st,      nothing, len)
_range(a::AbstractFloat, st::Real,          ::Nothing, len::Integer) = _range(a, float(st),      nothing, len)
_range(a,                ::Nothing,         ::Nothing, len::Integer) = _range(a, oftype(a-a, 1), nothing, len)

_range(a::T, step::T, ::Nothing, len::Integer) where {T <: AbstractFloat} =
    _rangestyle(OrderStyle(T), ArithmeticStyle(T), a, step, len)
_range(a::T, step, ::Nothing, len::Integer) where {T} =
    _rangestyle(OrderStyle(T), ArithmeticStyle(T), a, step, len)
_rangestyle(::Ordered, ::ArithmeticWraps, a::T, step::S, len::Integer) where {T,S} =
    StepRange{T,S}(a, step, convert(T, a+step*(len-1)))
_rangestyle(::Any, ::Any, a::T, step::S, len::Integer) where {T,S} =
    StepRangeLen{typeof(a+0*step),T,S}(a, step, len)

# Malformed calls
_range(start,     step,      ::Nothing, ::Nothing) = # range(a, step=s)
    throw(ArgumentError("At least one of `length` or `stop` must be specified"))
_range(start,     ::Nothing, ::Nothing, ::Nothing) = # range(a)
    throw(ArgumentError("At least one of `length` or `stop` must be specified"))
_range(::Nothing, ::Nothing, ::Nothing, ::Nothing) = # range(nothing)
    throw(ArgumentError("At least one of `length` or `stop` must be specified"))
_range(start::Real, step::Real, stop::Real, length::Integer) = # range(a, step=s, stop=b, length=l)
    throw(ArgumentError("Too many arguments specified; try passing only one of `stop` or `length`"))
_range(::Nothing, ::Nothing, ::Nothing, ::Integer) = # range(nothing, length=l)
    throw(ArgumentError("Can't start a range at `nothing`"))

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
5-element Array{Int64,1}:
 1
 3
 5
 7
 9

julia> typeof(StepRange(1, Int8(2), 10))
StepRange{Int64,Int8}

julia> typeof(1:3:6)
StepRange{Int64,Int64}
```
"""
struct StepRange{T,S} <: OrdinalRange{T,S}
    start::T
    step::S
    stop::T

    function StepRange{T,S}(start::T, step::S, stop::T) where {T,S}
        new(start, step, steprange_last(start,step,stop))
    end
end

# to make StepRange constructor inlineable, so optimizer can see `step` value
function steprange_last(start::T, step, stop) where T
    if isa(start,AbstractFloat) || isa(step,AbstractFloat)
        throw(ArgumentError("StepRange should not be used with floating point"))
    end
    if isa(start,Integer) && !isinteger(step)
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
            if T <: Signed && absdiff < zero(absdiff)
                # handle signed overflow with unsigned rem
                remain = convert(T, unsigned(absdiff) % absstep)
            else
                remain = absdiff % absstep
            end
            # Move `stop` closer to `start` if there is a remainder:
            last = stop > start ? stop - remain : stop + remain
        end
    end
    last
end

function steprange_last_empty(start::Integer, step, stop)
    # empty range has a special representation where stop = start-1
    # this is needed to avoid the wrap-around that can happen computing
    # start - step, which leads to a range that looks very large instead
    # of empty.
    if step > zero(step)
        last = start - oneunit(stop-start)
    else
        last = start + oneunit(stop-start)
    end
    last
end
# For types where x+oneunit(x) may not be well-defined
steprange_last_empty(start, step, stop) = start - step

StepRange(start::T, step::S, stop::T) where {T,S} = StepRange{T,S}(start, step, stop)

"""
    UnitRange{T<:Real}

A range parameterized by a `start` and `stop` of type `T`, filled
with elements spaced by `1` from `start` until `stop` is exceeded.
The syntax `a:b` with `a` and `b` both `Integer`s creates a `UnitRange`.

# Examples
```jldoctest
julia> collect(UnitRange(2.3, 5.2))
3-element Array{Float64,1}:
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
    UnitRange{T}(start, stop) where {T<:Real} = new(start, unitrange_last(start,stop))
end
UnitRange(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

unitrange_last(::Bool, stop::Bool) = stop
unitrange_last(start::T, stop::T) where {T<:Integer} =
    ifelse(stop >= start, stop, convert(T,start-oneunit(stop-start)))
unitrange_last(start::T, stop::T) where {T} =
    ifelse(stop >= start, convert(T,start+floor(stop-start)),
                          convert(T,start-oneunit(stop-start)))

if isdefined(Main, :Base)
    function getindex(t::Tuple, r::AbstractUnitRange{<:Real})
        n = length(r)
        n == 0 && return ()
        a = Vector{eltype(t)}(undef, n)
        o = first(r) - 1
        for i = 1:n
            el = t[o + i]
            @inbounds a[i] = el
        end
        (a...,)
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
    OneTo{T}(stop) where {T<:Integer} = new(max(zero(T), stop))
    function OneTo{T}(r::AbstractRange) where {T<:Integer}
        throwstart(r) = (@_noinline_meta; throw(ArgumentError("first element must be 1, got $(first(r))")))
        throwstep(r)  = (@_noinline_meta; throw(ArgumentError("step must be 1, got $(step(r))")))
        first(r) == 1 || throwstart(r)
        step(r)  == 1 || throwstep(r)
        return new(max(zero(T), last(r)))
    end
end
OneTo(stop::T) where {T<:Integer} = OneTo{T}(stop)
OneTo(r::AbstractRange{T}) where {T<:Integer} = OneTo{T}(r)

## Step ranges parameterized by length

"""
    StepRangeLen{T,R,S}(ref::R, step::S, len, [offset=1]) where {T,R,S}
    StepRangeLen(       ref::R, step::S, len, [offset=1]) where {  R,S}

A range `r` where `r[i]` produces values of type `T` (in the second
form, `T` is deduced automatically), parameterized by a `ref`erence
value, a `step`, and the `len`gth. By default `ref` is the starting
value `r[1]`, but alternatively you can supply it as the value of
`r[offset]` for some other index `1 <= offset <= len`. In conjunction
with `TwicePrecision` this can be used to implement ranges that are
free of roundoff error.
"""
struct StepRangeLen{T,R,S} <: AbstractRange{T}
    ref::R       # reference value (might be smallest-magnitude value in the range)
    step::S      # step value
    len::Int     # length of the range
    offset::Int  # the index of ref

    function StepRangeLen{T,R,S}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S}
        if T <: Integer && !isinteger(step)
            throw(ArgumentError("StepRangeLen{<:Integer} cannot have non-integer step"))
        end
        len >= 0 || throw(ArgumentError("length cannot be negative, got $len"))
        1 <= offset <= max(1,len) || throw(ArgumentError("StepRangeLen: offset must be in [1,$len], got $offset"))
        new(ref, step, len, offset)
    end
end

StepRangeLen(ref::R, step::S, len::Integer, offset::Integer = 1) where {R,S} =
    StepRangeLen{typeof(ref+0*step),R,S}(ref, step, len, offset)
StepRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S} =
    StepRangeLen{T,R,S}(ref, step, len, offset)

## range with computed step

"""
    LinRange{T}

A range with `len` linearly spaced elements between its `start` and `stop`.
The size of the spacing is controlled by `len`, which must
be an `Int`.

# Examples
```jldoctest
julia> LinRange(1.5, 5.5, 9)
9-element LinRange{Float64}:
 1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5
```

Compared to using [`range`](@ref), directly constructing a `LinRange` should
have less overhead but won't try to correct for floating point errors:
```julia
julia> collect(range(-0.1, 0.3, length=5))
5-element Array{Float64,1}:
 -0.1
  0.0
  0.1
  0.2
  0.3

julia> collect(LinRange(-0.1, 0.3, 5))
5-element Array{Float64,1}:
 -0.1
 -1.3877787807814457e-17
  0.09999999999999999
  0.19999999999999998
  0.3
```
"""
struct LinRange{T} <: AbstractRange{T}
    start::T
    stop::T
    len::Int
    lendiv::Int

    function LinRange{T}(start,stop,len) where T
        len >= 0 || throw(ArgumentError("range($start, stop=$stop, length=$len): negative length"))
        if len == 1
            start == stop || throw(ArgumentError("range($start, stop=$stop, length=$len): endpoints differ"))
            return new(start, stop, 1, 1)
        end
        lendiv = max(len-1, 1)
        if T <: Integer && !iszero(mod(stop-start, lendiv))
            throw(ArgumentError("LinRange{<:Integer} cannot have non-integer step"))
        end
        new(start,stop,len,lendiv)
    end
end

function LinRange(start, stop, len::Integer)
    T = typeof((stop-start)/len)
    LinRange{T}(start, stop, len)
end

function _range(start::T, ::Nothing, stop::S, len::Integer) where {T,S}
    a, b = promote(start, stop)
    _range(a, nothing, b, len)
end
_range(start::T, ::Nothing, stop::T, len::Integer) where {T<:Real} = LinRange{T}(start, stop, len)
_range(start::T, ::Nothing, stop::T, len::Integer) where {T} = LinRange{T}(start, stop, len)
_range(start::T, ::Nothing, stop::T, len::Integer) where {T<:Integer} =
    _linspace(float(T), start, stop, len)
## for Float16, Float32, and Float64 we hit twiceprecision.jl to lift to higher precision StepRangeLen
# for all other types we fall back to a plain old LinRange
_linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where T = LinRange{T}(start, stop, len)

function show(io::IO, r::LinRange)
    print(io, "range(")
    show(io, first(r))
    print(io, ", stop=")
    show(io, last(r))
    print(io, ", length=")
    show(io, length(r))
    print(io, ')')
end

"""
`print_range(io, r)` prints out a nice looking range r in terms of its elements
as if it were `collect(r)`, dependent on the size of the
terminal, and taking into account whether compact numbers should be shown.
It figures out the width in characters of each element, and if they
end up too wide, it shows the first and last elements separated by a
horizontal ellipsis. Typical output will look like `1.0,2.0,3.0,…,4.0,5.0,6.0`.

`print_range(io, r, pre, sep, post, hdots)` uses optional
parameters `pre` and `post` characters for each printed row,
`sep` separator string between printed elements,
`hdots` string for the horizontal ellipsis.
"""
function print_range(io::IO, r::AbstractRange,
                     pre::AbstractString = " ",
                     sep::AbstractString = ",",
                     post::AbstractString = "",
                     hdots::AbstractString = ",\u2026,") # horiz ellipsis
    # This function borrows from print_matrix() in show.jl
    # and should be called by show and display
    limit = get(io, :limit, false)
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
    A = alignment(io, rowmatrix, 1:m, 1:length(rowmatrix), screenwidth, screenwidth, sepsize) # how much space range takes
    if n <= length(A) # cols fit screen, so print out all elements
        print(io, pre) # put in pre chars
        print_matrix_row(io,rowmatrix,A,1,1:n,sep) # the entire range
        print(io, post) # add the post characters
    else # cols don't fit so put horiz ellipsis in the middle
        # how many chars left after dividing width of screen in half
        # and accounting for the horiz ellipsis
        c = div(screenwidth-length(hdots)+1,2)+1 # chars remaining for each side of rowmatrix
        alignR = reverse(alignment(io, rowmatrix, 1:m, length(rowmatrix):-1:1, c, c, sepsize)) # which cols of rowmatrix to put on the right
        c = screenwidth - sum(map(sum,alignR)) - (length(alignR)-1)*sepsize - length(hdots)
        alignL = alignment(io, rowmatrix, 1:m, 1:length(rowmatrix), c, c, sepsize) # which cols of rowmatrix to put on the left
        print(io, pre)   # put in pre chars
        print_matrix_row(io, rowmatrix,alignL,1,1:length(alignL),sep) # left part of range
        print(io, hdots) # horizontal ellipsis
        print_matrix_row(io, rowmatrix,alignR,1,length(rowmatrix)-length(alignR)+1:length(rowmatrix),sep) # right part of range
        print(io, post)  # post chars
    end
end

## interface implementations

size(r::AbstractRange) = (length(r),)

isempty(r::StepRange) =
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
step(r::AbstractUnitRange{T}) where{T} = oneunit(T) - zero(T)
step(r::StepRangeLen) = r.step
step(r::StepRangeLen{T}) where {T<:AbstractFloat} = T(r.step)
step(r::LinRange) = (last(r)-first(r))/r.lendiv

step_hp(r::StepRangeLen) = r.step
step_hp(r::AbstractRange) = step(r)

unsafe_length(r::AbstractRange) = length(r)  # generic fallback

function unsafe_length(r::StepRange)
    n = Integer(div((r.stop - r.start) + r.step, r.step))
    isempty(r) ? zero(n) : n
end
length(r::StepRange) = unsafe_length(r)
unsafe_length(r::AbstractUnitRange) = Integer(last(r) - first(r) + step(r))
unsafe_length(r::OneTo) = Integer(r.stop - zero(r.stop))
length(r::AbstractUnitRange) = unsafe_length(r)
length(r::OneTo) = unsafe_length(r)
length(r::StepRangeLen) = r.len
length(r::LinRange) = r.len

# Needed to fold the `firstindex` call in SimdLoop.simd_index
firstindex(::UnitRange) = 1
firstindex(::StepRange) = 1
firstindex(::LinRange) = 1

function length(r::StepRange{T}) where T<:Union{Int,UInt,Int64,UInt64,Int128,UInt128}
    isempty(r) && return zero(T)
    if r.step > 1
        return checked_add(convert(T, div(unsigned(r.stop - r.start), r.step)), one(T))
    elseif r.step < -1
        return checked_add(convert(T, div(unsigned(r.start - r.stop), -r.step)), one(T))
    elseif r.step > 0
        return checked_add(div(checked_sub(r.stop, r.start), r.step), one(T))
    else
        return checked_add(div(checked_sub(r.start, r.stop), -r.step), one(T))
    end
end

function length(r::AbstractUnitRange{T}) where T<:Union{Int,Int64,Int128}
    @_inline_meta
    checked_add(checked_sub(last(r), first(r)), one(T))
end
length(r::OneTo{T}) where {T<:Union{Int,Int64}} = T(r.stop)

length(r::AbstractUnitRange{T}) where {T<:Union{UInt,UInt64,UInt128}} =
    r.stop < r.start ? zero(T) : checked_add(last(r) - first(r), one(T))

# some special cases to favor default Int type
let smallint = (Int === Int64 ?
                Union{Int8,UInt8,Int16,UInt16,Int32,UInt32} :
                Union{Int8,UInt8,Int16,UInt16})
    global length

    function length(r::StepRange{<:smallint})
        isempty(r) && return Int(0)
        div(Int(r.stop)+Int(r.step) - Int(r.start), Int(r.step))
    end

    length(r::AbstractUnitRange{<:smallint}) = Int(last(r)) - Int(first(r)) + 1
    length(r::OneTo{<:smallint}) = Int(r.stop)
end

first(r::OrdinalRange{T}) where {T} = convert(T, r.start)
first(r::OneTo{T}) where {T} = oneunit(T)
first(r::StepRangeLen) = unsafe_getindex(r, 1)
first(r::LinRange) = r.start

last(r::OrdinalRange{T}) where {T} = convert(T, r.stop)
last(r::StepRangeLen) = unsafe_getindex(r, length(r))
last(r::LinRange) = r.stop

minimum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : first(r)
maximum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : last(r)
minimum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : min(first(r), last(r))
maximum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : max(first(r), last(r))

extrema(r::AbstractRange) = (minimum(r), maximum(r))

# Ranges are immutable
copy(r::AbstractRange) = r


## iteration

function iterate(r::Union{LinRange,StepRangeLen}, i::Int=1)
    @_inline_meta
    length(r) < i && return nothing
    unsafe_getindex(r, i), i + 1
end

iterate(r::OrdinalRange) = isempty(r) ? nothing : (first(r), first(r))

function iterate(r::OrdinalRange{T}, i) where {T}
    @_inline_meta
    i == last(r) && return nothing
    next = convert(T, i + step(r))
    (next, next)
end

## indexing

_in_unit_range(v::UnitRange, val, i::Integer) = i > 0 && val <= v.stop && val >= v.start

function getindex(v::UnitRange{T}, i::Integer) where T
    @_inline_meta
    val = convert(T, v.start + (i - 1))
    @boundscheck _in_unit_range(v, val, i) || throw_boundserror(v, i)
    val
end

const OverflowSafe = Union{Bool,Int8,Int16,Int32,Int64,Int128,
                           UInt8,UInt16,UInt32,UInt64,UInt128}

function getindex(v::UnitRange{T}, i::Integer) where {T<:OverflowSafe}
    @_inline_meta
    val = v.start + (i - 1)
    @boundscheck _in_unit_range(v, val, i) || throw_boundserror(v, i)
    val % T
end

function getindex(v::OneTo{T}, i::Integer) where T
    @_inline_meta
    @boundscheck ((i > 0) & (i <= v.stop)) || throw_boundserror(v, i)
    convert(T, i)
end

function getindex(v::AbstractRange{T}, i::Integer) where T
    @_inline_meta
    ret = convert(T, first(v) + (i - 1)*step_hp(v))
    ok = ifelse(step(v) > zero(step(v)),
                (ret <= last(v)) & (ret >= first(v)),
                (ret <= first(v)) & (ret >= last(v)))
    @boundscheck ((i > 0) & ok) || throw_boundserror(v, i)
    ret
end

function getindex(r::Union{StepRangeLen,LinRange}, i::Integer)
    @_inline_meta
    @boundscheck checkbounds(r, i)
    unsafe_getindex(r, i)
end

# This is separate to make it useful even when running with --check-bounds=yes
function unsafe_getindex(r::StepRangeLen{T}, i::Integer) where T
    u = i - r.offset
    T(r.ref + u*r.step)
end

function _getindex_hiprec(r::StepRangeLen, i::Integer)  # without rounding by T
    u = i - r.offset
    r.ref + u*r.step
end

function unsafe_getindex(r::LinRange, i::Integer)
    lerpi(i-1, r.lendiv, r.start, r.stop)
end

function lerpi(j::Integer, d::Integer, a::T, b::T) where T
    @_inline_meta
    t = j/d
    T((1-t)*a + t*b)
end

getindex(r::AbstractRange, ::Colon) = copy(r)

function getindex(r::AbstractUnitRange, s::AbstractUnitRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    f = first(r)
    st = oftype(f, f + first(s)-1)
    range(st, length=length(s))
end

function getindex(r::OneTo{T}, s::OneTo) where T
    @_inline_meta
    @boundscheck checkbounds(r, s)
    OneTo(T(s.stop))
end

function getindex(r::AbstractUnitRange, s::StepRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    st = oftype(first(r), first(r) + s.start-1)
    range(st, step=step(s), length=length(s))
end

function getindex(r::StepRange, s::AbstractRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    st = oftype(r.start, r.start + (first(s)-1)*step(r))
    range(st, step=step(r)*step(s), length=length(s))
end

function getindex(r::StepRangeLen{T}, s::OrdinalRange{<:Integer}) where {T}
    @_inline_meta
    @boundscheck checkbounds(r, s)
    # Find closest approach to offset by s
    ind = LinearIndices(s)
    offset = max(min(1 + round(Int, (r.offset - first(s))/step(s)), last(ind)), first(ind))
    ref = _getindex_hiprec(r, first(s) + (offset-1)*step(s))
    return StepRangeLen{T}(ref, r.step*step(s), length(s), offset)
end

function getindex(r::LinRange{T}, s::OrdinalRange{<:Integer}) where {T}
    @_inline_meta
    @boundscheck checkbounds(r, s)
    vfirst = unsafe_getindex(r, first(s))
    vlast  = unsafe_getindex(r, last(s))
    return LinRange{T}(vfirst, vlast, length(s))
end

show(io::IO, r::AbstractRange) = print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
show(io::IO, r::UnitRange) = print(io, repr(first(r)), ':', repr(last(r)))
show(io::IO, r::OneTo) = print(io, "Base.OneTo(", r.stop, ")")

==(r::T, s::T) where {T<:AbstractRange} =
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::OrdinalRange, s::OrdinalRange) =
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::T, s::T) where {T<:Union{StepRangeLen,LinRange}} =
    (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
==(r::Union{StepRange{T},StepRangeLen{T,T}}, s::Union{StepRange{T},StepRangeLen{T,T}}) where {T} =
    (first(r) == first(s)) & (last(r) == last(s)) & (step(r) == step(s))

function ==(r::AbstractRange, s::AbstractRange)
    lr = length(r)
    if lr != length(s)
        return false
    end
    yr, ys = iterate(r), iterate(s)
    while yr !== nothing
        yr[1] == ys[1] || return false
        yr, ys = iterate(r, yr[2]), iterate(s, ys[2])
    end
    return true
end

intersect(r::OneTo, s::OneTo) = OneTo(min(r.stop,s.stop))

intersect(r::AbstractUnitRange{<:Integer}, s::AbstractUnitRange{<:Integer}) = max(first(r),first(s)):min(last(r),last(s))

intersect(i::Integer, r::AbstractUnitRange{<:Integer}) =
    i < first(r) ? (first(r):i) :
    i > last(r)  ? (i:last(r))  : (i:i)

intersect(r::AbstractUnitRange{<:Integer}, i::Integer) = intersect(i, r)

function intersect(r::AbstractUnitRange{<:Integer}, s::StepRange{<:Integer})
    if isempty(s)
        range(first(r), length=0)
    elseif step(s) == 0
        intersect(first(s), r)
    elseif step(s) < 0
        intersect(r, reverse(s))
    else
        sta = first(s)
        ste = step(s)
        sto = last(s)
        lo = first(r)
        hi = last(r)
        i0 = max(sta, lo + mod(sta - lo, ste))
        i1 = min(sto, hi - mod(hi - sta, ste))
        i0:ste:i1
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
    if isempty(r) || isempty(s)
        return range(first(r), step=step(r), length=0)
    elseif step(s) < zero(step(s))
        return intersect(r, reverse(s))
    elseif step(r) < zero(step(r))
        return reverse(intersect(reverse(r), s))
    end

    start1 = first(r)
    step1 = step(r)
    stop1 = last(r)
    start2 = first(s)
    step2 = step(s)
    stop2 = last(s)
    a = lcm(step1, step2)

    # if a == 0
    #     # One or both ranges have step 0.
    #     if step1 == 0 && step2 == 0
    #         return start1 == start2 ? r : AbstractRange(start1, 0, 0)
    #     elseif step1 == 0
    #         return start2 <= start1 <= stop2 && rem(start1 - start2, step2) == 0 ? r : AbstractRange(start1, 0, 0)
    #     else
    #         return start1 <= start2 <= stop1 && rem(start2 - start1, step1) == 0 ? (start2:step1:start2) : AbstractRange(start1, step1, 0)
    #     end
    # end

    g, x, y = gcdx(step1, step2)

    if !iszero(rem(start1 - start2, g))
        # Unaligned, no overlap possible.
        return range(start1, step=a, length=0)
    end

    z = div(start1 - start2, g)
    b = start1 - x * z * step1
    # Possible points of the intersection of r and s are
    # ..., b-2a, b-a, b, b+a, b+2a, ...
    # Determine where in the sequence to start and stop.
    m = max(start1 + mod(b - start1, a), start2 + mod(b - start2, a))
    n = min(stop1 - mod(stop1 - b, a), stop2 - mod(stop2 - b, a))
    m:a:n
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
    isempty(r) || first(r) >= first(s) && last(r) <= last(s)

## linear operations on ranges ##

-(r::OrdinalRange) = range(-first(r), step=-step(r), length=length(r))
-(r::StepRangeLen{T,R,S}) where {T,R,S} =
    StepRangeLen{T,R,S}(-r.ref, -r.step, length(r), r.offset)
-(r::LinRange) = LinRange(-r.start, -r.stop, length(r))


# promote eltype if at least one container wouldn't change, otherwise join container types.
el_same(::Type{T}, a::Type{<:AbstractArray{T,n}}, b::Type{<:AbstractArray{T,n}}) where {T,n}   = a
el_same(::Type{T}, a::Type{<:AbstractArray{T,n}}, b::Type{<:AbstractArray{S,n}}) where {T,S,n} = a
el_same(::Type{T}, a::Type{<:AbstractArray{S,n}}, b::Type{<:AbstractArray{T,n}}) where {T,S,n} = b
el_same(::Type, a, b) = promote_typejoin(a, b)

promote_rule(a::Type{UnitRange{T1}}, b::Type{UnitRange{T2}}) where {T1,T2} =
    el_same(promote_type(T1,T2), a, b)
UnitRange{T}(r::UnitRange{T}) where {T<:Real} = r
UnitRange{T}(r::UnitRange) where {T<:Real} = UnitRange{T}(r.start, r.stop)

promote_rule(a::Type{OneTo{T1}}, b::Type{OneTo{T2}}) where {T1,T2} =
    el_same(promote_type(T1,T2), a, b)
OneTo{T}(r::OneTo{T}) where {T<:Integer} = r
OneTo{T}(r::OneTo) where {T<:Integer} = OneTo{T}(r.stop)

promote_rule(a::Type{UnitRange{T1}}, ::Type{UR}) where {T1,UR<:AbstractUnitRange} =
    promote_rule(a, UnitRange{eltype(UR)})
UnitRange{T}(r::AbstractUnitRange) where {T<:Real} = UnitRange{T}(first(r), last(r))
UnitRange(r::AbstractUnitRange) = UnitRange(first(r), last(r))

AbstractUnitRange{T}(r::AbstractUnitRange{T}) where {T} = r
AbstractUnitRange{T}(r::UnitRange) where {T} = UnitRange{T}(r)
AbstractUnitRange{T}(r::OneTo) where {T} = OneTo{T}(r)

promote_rule(::Type{StepRange{T1a,T1b}}, ::Type{StepRange{T2a,T2b}}) where {T1a,T1b,T2a,T2b} =
    el_same(promote_type(T1a,T2a),
            # el_same only operates on array element type, so just promote second type parameter
            StepRange{T1a, promote_type(T1b,T2b)},
            StepRange{T2a, promote_type(T1b,T2b)})
StepRange{T1,T2}(r::StepRange{T1,T2}) where {T1,T2} = r

promote_rule(a::Type{StepRange{T1a,T1b}}, ::Type{UR}) where {T1a,T1b,UR<:AbstractUnitRange} =
    promote_rule(a, StepRange{eltype(UR), eltype(UR)})
StepRange{T1,T2}(r::AbstractRange) where {T1,T2} =
    StepRange{T1,T2}(convert(T1, first(r)), convert(T2, step(r)), convert(T1, last(r)))
StepRange(r::AbstractUnitRange{T}) where {T} =
    StepRange{T,T}(first(r), step(r), last(r))
(::Type{StepRange{T1,T2} where T1})(r::AbstractRange) where {T2} = StepRange{eltype(r),T2}(r)

promote_rule(::Type{StepRangeLen{T1,R1,S1}},::Type{StepRangeLen{T2,R2,S2}}) where {T1,T2,R1,R2,S1,S2} =
    el_same(promote_type(T1,T2),
            StepRangeLen{T1,promote_type(R1,R2),promote_type(S1,S2)},
            StepRangeLen{T2,promote_type(R1,R2),promote_type(S1,S2)})
StepRangeLen{T,R,S}(r::StepRangeLen{T,R,S}) where {T,R,S} = r
StepRangeLen{T,R,S}(r::StepRangeLen) where {T,R,S} =
    StepRangeLen{T,R,S}(convert(R, r.ref), convert(S, r.step), length(r), r.offset)
StepRangeLen{T}(r::StepRangeLen) where {T} =
    StepRangeLen(convert(T, r.ref), convert(T, r.step), length(r), r.offset)

promote_rule(a::Type{StepRangeLen{T,R,S}}, ::Type{OR}) where {T,R,S,OR<:AbstractRange} =
    promote_rule(a, StepRangeLen{eltype(OR), eltype(OR), eltype(OR)})
StepRangeLen{T,R,S}(r::AbstractRange) where {T,R,S} =
    StepRangeLen{T,R,S}(R(first(r)), S(step(r)), length(r))
StepRangeLen{T}(r::AbstractRange) where {T} =
    StepRangeLen(T(first(r)), T(step(r)), length(r))
StepRangeLen(r::AbstractRange) = StepRangeLen{eltype(r)}(r)

promote_rule(a::Type{LinRange{T1}}, b::Type{LinRange{T2}}) where {T1,T2} =
    el_same(promote_type(T1,T2), a, b)
LinRange{T}(r::LinRange{T}) where {T} = r
LinRange{T}(r::AbstractRange) where {T} = LinRange{T}(first(r), last(r), length(r))
LinRange(r::AbstractRange{T}) where {T} = LinRange{T}(r)

promote_rule(a::Type{LinRange{T}}, ::Type{OR}) where {T,OR<:OrdinalRange} =
    promote_rule(a, LinRange{eltype(OR)})

promote_rule(::Type{LinRange{L}}, b::Type{StepRangeLen{T,R,S}}) where {L,T,R,S} =
    promote_rule(StepRangeLen{L,L,L}, b)

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

reverse(r::OrdinalRange) = (:)(last(r), -step(r), first(r))
function reverse(r::StepRangeLen)
    # If `r` is empty, `length(r) - r.offset + 1 will be nonpositive hence
    # invalid. As `reverse(r)` is also empty, any offset would work so we keep
    # `r.offset`
    offset = isempty(r) ? r.offset : length(r)-r.offset+1
    StepRangeLen(r.ref, -r.step, length(r), offset)
end
reverse(r::LinRange)     = LinRange(r.stop, r.start, length(r))

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
    if step(r) == 0
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
    isinteger(x) && !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(convert(T,x),step(r))-mod(first(r),step(r)) == 0)
in(x::AbstractChar, r::AbstractRange{<:AbstractChar}) =
    !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(Int(x) - Int(first(r)), step(r)) == 0)

# Addition/subtraction of ranges

function _define_range_op(@nospecialize f)
    @eval begin
        function $f(r1::OrdinalRange, r2::OrdinalRange)
            r1l = length(r1)
            (r1l == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match: length of r1 is $r1l, length of r2 is $(length(r2))")))
            range($f(first(r1), first(r2)), step=$f(step(r1), step(r2)), length=r1l)
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

See also: [`mod1`](@ref).

# Examples
```jldoctest
julia> mod(0, Base.OneTo(3))
3

julia> mod(3, 0:2)
0
```

!!! compat "Julia 1.3"
     This method requires at least Julia 1.3.
"""
mod(i::Integer, r::OneTo) = mod1(i, last(r))
mod(i::Integer, r::AbstractUnitRange{<:Integer}) = mod(i-first(r), length(r)) + first(r)

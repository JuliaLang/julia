# This file is a part of Julia. License is MIT: https://julialang.org/license

(:)(a::Real, b::Real) = (:)(promote(a,b)...)

(:)(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

(:)(start::T, stop::T) where {T} = (:)(start, oftype(stop-start, 1), stop)

# first promote start and stop, leaving step alone
(:)(start::A, step, stop::C) where {A<:Real,C<:Real} =
    (:)(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))
(:)(start::T, step::Real, stop::T) where {T<:Real} = (:)(promote(start, step, stop)...)

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

Range operator. `a:b` constructs a range from `a` to `b` with a step size of 1, and `a:s:b`
is similar but uses a step size of `s`.

`:` is also used in indexing to select whole dimensions.
"""
(:)(start::T, step, stop::T) where {T} = _colon(start, step, stop)
(:)(start::T, step, stop::T) where {T<:Real} = _colon(start, step, stop)
# without the second method above, the first method above is ambiguous with
# (:)(start::A, step, stop::C) where {A<:Real,C<:Real}
function _colon(start::T, step, stop::T) where T
    T′ = typeof(start+step)
    StepRange(convert(T′,start), step, convert(T′,stop))
end

"""
    range(start; length, stop, step=1)

Given a starting value, construct a range either by length or from `start` to `stop`,
optionally with a given step (defaults to 1). One of `length` or `step` is required.
If `length`, `stop`, and `step` are all specified, they must agree.

If `length` and `stop` are provided and `step` is not, the step size will be computed
automatically such that there are `length` linearly spaced elements in the range.
"""
range(start; length::Union{Integer,Nothing}=nothing, stop=nothing, step=nothing) =
    _range(start, step, stop, length)

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

abstract type AbstractRange{T} <: AbstractArray{T,1} end

RangeStepStyle(::Type{<:AbstractRange}) = RangeStepIrregular()
RangeStepStyle(::Type{<:AbstractRange{<:Integer}}) = RangeStepRegular()

convert(::Type{T}, r::AbstractRange) where {T<:AbstractRange} = r isa T ? r : T(r)

## ordinal ranges

abstract type OrdinalRange{T,S} <: AbstractRange{T} end
abstract type AbstractUnitRange{T} <: OrdinalRange{T,Int} end

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
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if stop == start
        last = stop
    else
        if (step > z) != (stop > start)
            last = steprange_last_empty(start, step, stop)
        else
            diff = stop - start
            if T<:Signed && (diff > zero(diff)) != (stop > start)
                # handle overflowed subtraction with unsigned rem
                if diff > zero(diff)
                    remain = -convert(T, unsigned(-diff) % step)
                else
                    remain = convert(T, unsigned(diff) % step)
                end
            else
                remain = steprem(start,stop,step)
            end
            last = stop - remain
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

steprem(start,stop,step) = (stop-start) % step

StepRange(start::T, step::S, stop::T) where {T,S} = StepRange{T,S}(start, step, stop)

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
    getindex(t::Tuple, r::AbstractUnitRange{<:Real}) =
        (o = first(r) - 1; ntuple(n -> t[o + n], length(r)))
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
end
OneTo(stop::T) where {T<:Integer} = OneTo{T}(stop)

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
        new(start,stop,len,max(len-1,1))
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
horizontal elipsis. Typical output will look like `1.0,2.0,3.0,…,4.0,5.0,6.0`.

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

Get the step size of an `AbstractRange` object.
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
step(r::AbstractUnitRange) = 1
step(r::StepRangeLen{T}) where {T} = T(r.step)
step(r::LinRange) = (last(r)-first(r))/r.lendiv

step_hp(r::StepRangeLen) = r.step
step_hp(r::AbstractRange) = step(r)

unsafe_length(r::AbstractRange) = length(r)  # generic fallback

function unsafe_length(r::StepRange)
    n = Integer(div(r.stop+r.step - r.start, r.step))
    isempty(r) ? zero(n) : n
end
length(r::StepRange) = unsafe_length(r)
unsafe_length(r::AbstractUnitRange) = Integer(last(r) - first(r) + 1)
unsafe_length(r::OneTo) = r.stop
length(r::AbstractUnitRange) = unsafe_length(r)
length(r::OneTo) = unsafe_length(r)
length(r::StepRangeLen) = r.len
length(r::LinRange) = r.len

function length(r::StepRange{T}) where T<:Union{Int,UInt,Int64,UInt64}
    isempty(r) && return zero(T)
    if r.step > 1
        return checked_add(convert(T, div(unsigned(r.stop - r.start), r.step)), one(T))
    elseif r.step < -1
        return checked_add(convert(T, div(unsigned(r.start - r.stop), -r.step)), one(T))
    else
        checked_add(div(checked_sub(r.stop, r.start), r.step), one(T))
    end
end

function length(r::AbstractUnitRange{T}) where T<:Union{Int,Int64}
    @_inline_meta
    checked_add(checked_sub(last(r), first(r)), one(T))
end
length(r::OneTo{T}) where {T<:Union{Int,Int64}} = T(r.stop)

length(r::AbstractUnitRange{T}) where {T<:Union{UInt,UInt64}} =
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

# Ranges are immutable
copy(r::AbstractRange) = r


## iteration

start(r::LinRange) = 1
done(r::LinRange, i::Int) = length(r) < i
function next(r::LinRange, i::Int)
    @_inline_meta
    unsafe_getindex(r, i), i+1
end

start(r::StepRange) = oftype(r.start + r.step, r.start)
next(r::StepRange{T}, i) where {T} = (convert(T,i), i+r.step)
done(r::StepRange, i) = isempty(r) | (i < min(r.start, r.stop)) | (i > max(r.start, r.stop))
done(r::StepRange, i::Integer) =
    isempty(r) | (i == oftype(i, r.stop) + r.step)

start(r::StepRangeLen) = 1
next(r::StepRangeLen{T}, i) where {T} = unsafe_getindex(r, i), i+1
done(r::StepRangeLen, i) = i > length(r)

start(r::UnitRange{T}) where {T} = oftype(r.start + oneunit(T), r.start)
next(r::AbstractUnitRange{T}, i) where {T} = (convert(T, i), i + oneunit(T))
done(r::AbstractUnitRange{T}, i) where {T} = i == oftype(i, r.stop) + oneunit(T)

start(r::OneTo{T}) where {T} = oneunit(T)

# some special cases to favor default Int type to avoid overflow
let smallint = (Int === Int64 ?
                Union{Int8,UInt8,Int16,UInt16,Int32,UInt32} :
                Union{Int8,UInt8,Int16,UInt16})
    global start
    global next
    start(r::StepRange{<:smallint}) = convert(Int, r.start)
    next(r::StepRange{T}, i) where {T<:smallint} = (i % T, i + r.step)
    start(r::UnitRange{<:smallint}) = convert(Int, r.start)
    next(r::AbstractUnitRange{T}, i) where {T<:smallint} = (i % T, i + 1)
    start(r::OneTo{<:smallint}) = 1
end

## indexing

function getindex(v::UnitRange{T}, i::Integer) where T
    @_inline_meta
    ret = convert(T, first(v) + i - 1)
    @boundscheck ((i > 0) & (ret <= v.stop) & (ret >= v.start)) || throw_boundserror(v, i)
    ret
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
                (ret <= v.stop) & (ret >= v.start),
                (ret <= v.start) & (ret >= v.stop))
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
    ind = linearindices(s)
    offset = max(min(1 + round(Int, (r.offset - first(s))/step(s)), last(ind)), first(ind))
    ref = _getindex_hiprec(r, first(s) + (offset-1)*step(s))
    return StepRangeLen{T}(ref, r.step*step(s), length(s), offset)
end

function getindex(r::LinRange, s::OrdinalRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    vfirst = unsafe_getindex(r, first(s))
    vlast  = unsafe_getindex(r, last(s))
    return LinRange(vfirst, vlast, length(s))
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
    u, v = start(r), start(s)
    while !done(r, u)
        x, u = next(r, u)
        y, v = next(s, v)
        if x != y
            return false
        end
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
    elseif step(s) < 0
        return intersect(r, reverse(s))
    elseif step(r) < 0
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

    if rem(start1 - start2, g) != 0
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

## linear operations on ranges ##

-(r::OrdinalRange) = range(-first(r), step=-step(r), length=length(r))
-(r::StepRangeLen{T,R,S}) where {T,R,S} =
    StepRangeLen{T,R,S}(-r.ref, -r.step, length(r), r.offset)
-(r::LinRange) = LinRange(-r.start, -r.stop, length(r))

*(x::Number, r::AbstractRange) = range(x*first(r), step=x*step(r), length=length(r))
*(x::Number, r::StepRangeLen{T}) where {T} =
    StepRangeLen{typeof(x*T(r.ref))}(x*r.ref, x*r.step, length(r), r.offset)
*(x::Number, r::LinRange) = LinRange(x * r.start, x * r.stop, r.len)
# separate in case of noncommutative multiplication
*(r::AbstractRange, x::Number) = range(first(r)*x, step=step(r)*x, length=length(r))
*(r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)*x)}(r.ref*x, r.step*x, length(r), r.offset)
*(r::LinRange, x::Number) = LinRange(r.start * x, r.stop * x, r.len)

/(r::AbstractRange, x::Number) = range(first(r)/x, step=step(r)/x, length=length(r))
/(r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)/x)}(r.ref/x, r.step/x, length(r), r.offset)
/(r::LinRange, x::Number) = LinRange(r.start / x, r.stop / x, r.len)
# also, separate in case of noncommutative multiplication (division)
\(x::Number, r::AbstractRange) = range(x\first(r), step=x\step(r), length=x\length(r))
\(x::Number, r::StepRangeLen) = StepRangeLen(x\r.ref, x\r.step, length(r), r.offset)
\(x::Number, r::LinRange) = LinRange(x \ r.start, x \ r.stop, r.len)

## scalar-range broadcast operations ##

broadcast(::typeof(-), r::OrdinalRange) = range(-first(r), step=-step(r), length=length(r))
broadcast(::typeof(-), r::StepRangeLen) = StepRangeLen(-r.ref, -r.step, length(r), r.offset)
broadcast(::typeof(-), r::LinRange) = LinRange(-r.start, -r.stop, length(r))

broadcast(::typeof(+), x::Real, r::AbstractUnitRange) = range(x + first(r), length=length(r))
# For #18336 we need to prevent promotion of the step type:
broadcast(::typeof(+), x::Number, r::AbstractUnitRange) = range(x + first(r), step=step(r), length=length(r))
broadcast(::typeof(+), x::Number, r::AbstractRange) = (x+first(r)):step(r):(x+last(r))
function broadcast(::typeof(+), x::Number, r::StepRangeLen{T}) where T
    newref = x + r.ref
    StepRangeLen{typeof(T(r.ref) + x)}(newref, r.step, length(r), r.offset)
end
function broadcast(::typeof(+), x::Number, r::LinRange)
    LinRange(x + r.start, x + r.stop, r.len)
end
broadcast(::typeof(+), r::AbstractRange, x::Number) = broadcast(+, x, r)  # assumes addition is commutative

broadcast(::typeof(-), x::Number, r::AbstractRange) = (x-first(r)):-step(r):(x-last(r))
broadcast(::typeof(-), x::Number, r::StepRangeLen) = broadcast(+, x, -r)
function broadcast(::typeof(-), x::Number, r::LinRange)
    LinRange(x - r.start, x - r.stop, r.len)
end

broadcast(::typeof(-), r::AbstractRange, x::Number) = broadcast(+, -x, r)  # assumes addition is commutative

broadcast(::typeof(*), x::Number, r::AbstractRange) = range(x*first(r), step=x*step(r), length=length(r))
broadcast(::typeof(*), x::Number, r::StepRangeLen)  = StepRangeLen(x*r.ref, x*r.step, length(r), r.offset)
broadcast(::typeof(*), x::Number, r::LinRange)      = LinRange(x * r.start, x * r.stop, r.len)
# separate in case of noncommutative multiplication
broadcast(::typeof(*), r::AbstractRange, x::Number) = range(first(r)*x, step=step(r)*x, length=length(r))
broadcast(::typeof(*), r::StepRangeLen, x::Number)  = StepRangeLen(r.ref*x, r.step*x, length(r), r.offset)
broadcast(::typeof(*), r::LinRange, x::Number)      = LinRange(r.start * x, r.stop * x, r.len)

broadcast(::typeof(/), r::AbstractRange, x::Number) = range(first(r)/x, step=step(r)/x, length=length(r))
broadcast(::typeof(/), r::StepRangeLen, x::Number)  = StepRangeLen(r.ref/x, r.step/x, length(r), r.offset)
broadcast(::typeof(/), r::LinRange, x::Number)      = LinRange(r.start / x, r.stop / x, r.len)
# also, separate in case of noncommutative multiplication (division)
broadcast(::typeof(\), x::Number, r::AbstractRange) = range(x\first(r), step=x\step(r), length=x\length(r))
broadcast(::typeof(\), x::Number, r::StepRangeLen)  = StepRangeLen(x\r.ref, x\r.step, length(r), r.offset)
broadcast(::typeof(\), x::Number, r::LinRange)      = LinRange(x \ r.start, x \ r.stop, r.len)

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

# +/- of ranges is defined in operators.jl (to be able to use @eval etc.)

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
reverse(r::StepRangeLen) = StepRangeLen(r.ref, -r.step, length(r), length(r)-r.offset+1)
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

function mean(r::AbstractRange{<:Real})
    isempty(r) && throw(ArgumentError("mean of an empty range is undefined"))
    (first(r) + last(r)) / 2
end

median(r::AbstractRange{<:Real}) = mean(r)

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
             throw(DimensionMismatch("argument dimensions must match")))
            range($f(first(r1), first(r2)), step=$f(step(r1), step(r2)), length=r1l)
        end

        function $f(r1::LinRange{T}, r2::LinRange{T}) where T
            len = r1.len
            (len == r2.len ||
             throw(DimensionMismatch("argument dimensions must match")))
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
        throw(DimensionMismatch("argument dimensions must match")))
    StepRangeLen(first(r1)+first(r2), step(r1)+step(r2), len)
end

-(r1::StepRangeLen, r2::StepRangeLen) = +(r1, -r2)

broadcast(::typeof(+), r1::AbstractRange, r2::AbstractRange) = r1 + r2
broadcast(::typeof(-), r1::AbstractRange, r2::AbstractRange) = r1 - r2

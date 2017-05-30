# This file is a part of Julia. License is MIT: https://julialang.org/license

colon(a::Real, b::Real) = colon(promote(a,b)...)

colon(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

range(a::Real, len::Integer) = UnitRange{typeof(a)}(a, oftype(a, a+len-1))

colon(start::T, stop::T) where {T} = colon(start, oftype(stop-start, 1), stop)

range(a, len::Integer) = range(a, oftype(a-a, 1), len)

# first promote start and stop, leaving step alone
colon(start::A, step, stop::C) where {A<:Real,C<:Real} =
    colon(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))
colon(start::T, step::Real, stop::T) where {T<:Real} = colon(promote(start, step, stop)...)

"""
    colon(start, [step], stop)

Called by `:` syntax for constructing ranges.

```jldoctest
julia> colon(1, 2, 5)
1:2:5
```
"""
colon(start::T, step::T, stop::T) where {T<:AbstractFloat} =
    _colon(TypeOrder(T), TypeArithmetic(T), start, step, stop)
colon(start::T, step::T, stop::T) where {T<:Real} =
    _colon(TypeOrder(T), TypeArithmetic(T), start, step, stop)
_colon(::HasOrder, ::Any, start::T, step, stop::T) where {T} = StepRange(start, step, stop)
# for T<:Union{Float16,Float32,Float64} see twiceprecision.jl
_colon(::HasOrder, ::ArithmeticRounds, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)
_colon(::Any, ::Any, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)

"""
    :(start, [step], stop)

Range operator. `a:b` constructs a range from `a` to `b` with a step size of 1, and `a:s:b`
is similar but uses a step size of `s`. These syntaxes call the function `colon`. The colon
is also used in indexing to select whole dimensions.
"""
colon(start::T, step, stop::T) where {T} = _colon(start, step, stop)
colon(start::T, step, stop::T) where {T<:Real} = _colon(start, step, stop)
# without the second method above, the first method above is ambiguous with
# colon{A<:Real,C<:Real}(start::A, step, stop::C)
function _colon(start::T, step, stop::T) where T
    T′ = typeof(start+step)
    StepRange(convert(T′,start), step, convert(T′,stop))
end

"""
    range(start, [step], length)

Construct a range by length, given a starting value and optional step (defaults to 1).
"""
range(a::T, step, len::Integer) where {T} = _range(TypeOrder(T), TypeArithmetic(T), a, step, len)
_range(::HasOrder, ::ArithmeticOverflows, a::T, step::S, len::Integer) where {T,S} =
    StepRange{T,S}(a, step, convert(T, a+step*(len-1)))
_range(::Any, ::Any, a::T, step::S, len::Integer) where {T,S} =
    StepRangeLen{typeof(a+0*step),T,S}(a, step, len)

# AbstractFloat specializations
colon(a::T, b::T) where {T<:AbstractFloat} = colon(a, T(1), b)
range(a::AbstractFloat, len::Integer) = range(a, oftype(a, 1), len)

colon(a::T, b::AbstractFloat, c::T) where {T<:Real} = colon(promote(a,b,c)...)
colon(a::T, b::AbstractFloat, c::T) where {T<:AbstractFloat} = colon(promote(a,b,c)...)
colon(a::T, b::Real, c::T) where {T<:AbstractFloat} = colon(promote(a,b,c)...)

range(a::AbstractFloat, st::AbstractFloat, len::Integer) = range(promote(a, st)..., len)
range(a::Real, st::AbstractFloat, len::Integer) = range(float(a), st, len)
range(a::AbstractFloat, st::Real, len::Integer) = range(a, float(st), len)

## 1-dimensional ranges ##

abstract type Range{T} <: AbstractArray{T,1} end

## ordinal ranges

abstract type OrdinalRange{T,S} <: Range{T} end
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

## Step ranges parametrized by length

"""
    StepRangeLen{T,R,S}(ref::R, step::S, len, [offset=1])

A range `r` where `r[i]` produces values of type `T`, parametrized by
a `ref`erence value, a `step`, and the `len`gth.  By default `ref` is
the starting value `r[1]`, but alternatively you can supply it as the
value of `r[offset]` for some other index `1 <= offset <= len`.  In
conjunction with `TwicePrecision` this can be used to implement ranges
that are free of roundoff error.
"""
struct StepRangeLen{T,R,S} <: Range{T}
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

## linspace and logspace

struct LinSpace{T} <: Range{T}
    start::T
    stop::T
    len::Int
    lendiv::Int

    function LinSpace{T}(start,stop,len) where T
        len >= 0 || throw(ArgumentError("linspace($start, $stop, $len): negative length"))
        if len == 1
            start == stop || throw(ArgumentError("linspace($start, $stop, $len): endpoints differ"))
            return new(start, stop, 1, 1)
        end
        new(start,stop,len,max(len-1,1))
    end
end

function LinSpace(start, stop, len::Integer)
    T = typeof((stop-start)/len)
    LinSpace{T}(start, stop, len)
end

"""
    linspace(start, stop, n=50)

Construct a range of `n` linearly spaced elements from `start` to `stop`.

```jldoctest
julia> linspace(1.3,2.9,9)
1.3:0.2:2.9
```
"""
linspace(start, stop, len::Real=50) = linspace(promote_noncircular(start, stop)..., Int(len))
linspace(start::T, stop::T, len::Real=50) where {T} = linspace(start, stop, Int(len))

linspace(start::Real, stop::Real, len::Integer) = linspace(promote(start, stop)..., len)
linspace(start::T, stop::T, len::Integer) where {T<:Integer} = linspace(Float64, start, stop, len, 1)
# for Float16, Float32, and Float64 see twiceprecision.jl
linspace(start::T, stop::T, len::Integer) where {T<:Real} = LinSpace{T}(start, stop, len)
linspace(start::T, stop::T, len::Integer) where {T} = LinSpace{T}(start, stop, len)

function show(io::IO, r::LinSpace)
    print(io, "linspace(")
    show(io, first(r))
    print(io, ',')
    show(io, last(r))
    print(io, ',')
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
function print_range(io::IO, r::Range,
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

"""
    logspace(start::Real, stop::Real, n::Integer=50)

Construct a vector of `n` logarithmically spaced numbers from `10^start` to `10^stop`.

```jldoctest
julia> logspace(1.,10.,5)
5-element Array{Float64,1}:
   10.0
 1778.28
    3.16228e5
    5.62341e7
    1.0e10
```
"""
logspace(start::Real, stop::Real, n::Integer=50) = 10.^linspace(start, stop, n)

## interface implementations

size(r::Range) = (length(r),)

isempty(r::StepRange) =
    (r.start != r.stop) & ((r.step > zero(r.step)) != (r.stop > r.start))
isempty(r::AbstractUnitRange) = first(r) > last(r)
isempty(r::StepRangeLen) = length(r) == 0
isempty(r::LinSpace) = length(r) == 0

"""
    step(r)

Get the step size of a `Range` object.
```jldoctest
julia> step(1:10)
1

julia> step(1:2:10)
2

julia> step(2.5:0.3:10.9)
0.3

julia> step(linspace(2.5,10.9,85))
0.1
```
"""
step(r::StepRange) = r.step
step(r::AbstractUnitRange) = 1
step(r::StepRangeLen) = r.step
step(r::LinSpace) = (last(r)-first(r))/r.lendiv

unsafe_length(r::Range) = length(r)  # generic fallback

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
length(r::LinSpace) = r.len

function length{T<:Union{Int,UInt,Int64,UInt64}}(r::StepRange{T})
    isempty(r) && return zero(T)
    if r.step > 1
        return checked_add(convert(T, div(unsigned(r.stop - r.start), r.step)), one(T))
    elseif r.step < -1
        return checked_add(convert(T, div(unsigned(r.start - r.stop), -r.step)), one(T))
    else
        checked_add(div(checked_sub(r.stop, r.start), r.step), one(T))
    end
end

function length{T<:Union{Int,Int64}}(r::AbstractUnitRange{T})
    @_inline_meta
    checked_add(checked_sub(last(r), first(r)), one(T))
end
length{T<:Union{Int,Int64}}(r::OneTo{T}) = T(r.stop)

length{T<:Union{UInt,UInt64}}(r::AbstractUnitRange{T}) =
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
first(r::LinSpace) = r.start

last(r::OrdinalRange{T}) where {T} = convert(T, r.stop)
last(r::StepRangeLen) = unsafe_getindex(r, length(r))
last(r::LinSpace) = r.stop

minimum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : first(r)
maximum(r::AbstractUnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : last(r)
minimum(r::Range)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : min(first(r), last(r))
maximum(r::Range)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : max(first(r), last(r))

# Ranges are immutable
copy(r::Range) = r


## iteration

start(r::LinSpace) = 1
done(r::LinSpace, i::Int) = length(r) < i
function next(r::LinSpace, i::Int)
    @_inline_meta
    unsafe_getindex(r, i), i+1
end

start(r::StepRange) = oftype(r.start + r.step, r.start)
next(r::StepRange{T}, i) where {T} = (convert(T,i), i+r.step)
done(r::StepRange, i) = isempty(r) | (i < min(r.start, r.stop)) | (i > max(r.start, r.stop))
done(r::StepRange, i::Integer) =
    isempty(r) | (i == oftype(i, r.stop) + r.step)

# see also twiceprecision.jl
start(r::StepRangeLen) = (unsafe_getindex(r, 1), 1)
next(r::StepRangeLen{T}, s) where {T} = s[1], (T(s[1]+r.step), s[2]+1)
done(r::StepRangeLen, s) = s[2] > length(r)

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

function getindex(v::Range{T}, i::Integer) where T
    @_inline_meta
    ret = convert(T, first(v) + (i - 1)*step(v))
    ok = ifelse(step(v) > zero(step(v)),
                (ret <= v.stop) & (ret >= v.start),
                (ret <= v.start) & (ret >= v.stop))
    @boundscheck ((i > 0) & ok) || throw_boundserror(v, i)
    ret
end

function getindex(r::Union{StepRangeLen,LinSpace}, i::Integer)
    @_inline_meta
    @boundscheck checkbounds(r, i)
    unsafe_getindex(r, i)
end

# This is separate to make it useful even when running with --check-bounds=yes
function unsafe_getindex(r::StepRangeLen{T}, i::Integer) where T
    u = i - r.offset
    T(r.ref + u*r.step)
end

function unsafe_getindex(r::LinSpace, i::Integer)
    lerpi.(i-1, r.lendiv, r.start, r.stop)
end

function lerpi{T}(j::Integer, d::Integer, a::T, b::T)
    @_inline_meta
    t = j/d
    T((1-t)*a + t*b)
end

getindex(r::Range, ::Colon) = copy(r)

function getindex(r::AbstractUnitRange, s::AbstractUnitRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    f = first(r)
    st = oftype(f, f + first(s)-1)
    range(st, length(s))
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
    range(st, step(s), length(s))
end

function getindex(r::StepRange, s::Range{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    st = oftype(r.start, r.start + (first(s)-1)*step(r))
    range(st, step(r)*step(s), length(s))
end

function getindex(r::StepRangeLen, s::OrdinalRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    vfirst = unsafe_getindex(r, first(s))
    return StepRangeLen(vfirst, r.step*step(s), length(s))
end

function getindex(r::LinSpace, s::OrdinalRange{<:Integer})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    vfirst = unsafe_getindex(r, first(s))
    vlast  = unsafe_getindex(r, last(s))
    return LinSpace(vfirst, vlast, length(s))
end

show(io::IO, r::Range) = print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
show(io::IO, r::UnitRange) = print(io, repr(first(r)), ':', repr(last(r)))
show(io::IO, r::OneTo) = print(io, "Base.OneTo(", r.stop, ")")

==(r::T, s::T) where {T<:Range} =
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::OrdinalRange, s::OrdinalRange) =
    (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::T, s::T) where {T<:Union{StepRangeLen,LinSpace}} =
    (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
==(r::Union{StepRange{T},StepRangeLen{T,T}}, s::Union{StepRange{T},StepRangeLen{T,T}}) where {T} =
    (first(r) == first(s)) & (last(r) == last(s)) & (step(r) == step(s))

function ==(r::Range, s::Range)
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
        range(first(r), 0)
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
        return range(first(r), step(r), 0)
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
    #         return start1 == start2 ? r : Range(start1, 0, 0)
    #     elseif step1 == 0
    #         return start2 <= start1 <= stop2 && rem(start1 - start2, step2) == 0 ? r : Range(start1, 0, 0)
    #     else
    #         return start1 <= start2 <= stop1 && rem(start2 - start1, step1) == 0 ? (start2:step1:start2) : Range(start1, step1, 0)
    #     end
    # end

    g, x, y = gcdx(step1, step2)

    if rem(start1 - start2, g) != 0
        # Unaligned, no overlap possible.
        return range(start1, a, 0)
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

function intersect(r1::Range, r2::Range, r3::Range, r::Range...)
    i = intersect(intersect(r1, r2), r3)
    for t in r
        i = intersect(i, t)
    end
    i
end

# findin (the index of intersection)
function _findin(r::Range{<:Integer}, span::AbstractUnitRange{<:Integer})
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
    ifirst, ilast
end

function findin(r::AbstractUnitRange{<:Integer}, span::AbstractUnitRange{<:Integer})
    ifirst, ilast = _findin(r, span)
    ifirst:ilast
end

function findin(r::Range{<:Integer}, span::AbstractUnitRange{<:Integer})
    ifirst, ilast = _findin(r, span)
    ifirst:1:ilast
end

## linear operations on ranges ##

-(r::OrdinalRange) = range(-first(r), -step(r), length(r))
-(r::StepRangeLen) = StepRangeLen(-r.ref, -r.step, length(r), r.offset)
-(r::LinSpace) = LinSpace(-r.start, -r.stop, length(r))

+(x::Real, r::AbstractUnitRange) = range(x + first(r), length(r))
# For #18336 we need to prevent promotion of the step type:
+(x::Number, r::AbstractUnitRange) = range(x + first(r), step(r), length(r))
+(x::Number, r::Range) = (x+first(r)):step(r):(x+last(r))
function +(x::Number, r::StepRangeLen)
    newref = x + r.ref
    StepRangeLen{eltype(newref),typeof(newref),typeof(r.step)}(newref, r.step, length(r), r.offset)
end
function +(x::Number, r::LinSpace)
    LinSpace(x + r.start, x + r.stop, r.len)
end
+(r::Range, x::Number) = x + r  # assumes addition is commutative

-(x::Number, r::Range)      = (x-first(r)):-step(r):(x-last(r))
-(x::Number, r::StepRangeLen) = +(x, -r)
function -(x::Number, r::LinSpace)
    LinSpace(x - r.start, x - r.stop, r.len)
end

-(r::Range, x::Number) = +(-x, r)

*(x::Number, r::Range)        = range(x*first(r), x*step(r), length(r))
*(x::Number, r::StepRangeLen) = StepRangeLen(x*r.ref, x*r.step, length(r), r.offset)
*(x::Number, r::LinSpace)     = LinSpace(x * r.start, x * r.stop, r.len)
# separate in case of noncommutative multiplication
*(r::Range, x::Number)        = range(first(r)*x, step(r)*x, length(r))
*(r::StepRangeLen, x::Number) = StepRangeLen(r.ref*x, r.step*x, length(r), r.offset)
*(r::LinSpace, x::Number)     = LinSpace(r.start * x, r.stop * x, r.len)

/(r::Range, x::Number)        = range(first(r)/x, step(r)/x, length(r))
/(r::StepRangeLen, x::Number) = StepRangeLen(r.ref/x, r.step/x, length(r), r.offset)
/(r::LinSpace, x::Number)     = LinSpace(r.start / x, r.stop / x, r.len)

/(x::Number, r::Range) = [ x/y for y=r ]

promote_rule(::Type{UnitRange{T1}},::Type{UnitRange{T2}}) where {T1,T2} =
    UnitRange{promote_type(T1,T2)}
convert(::Type{UnitRange{T}}, r::UnitRange{T}) where {T<:Real} = r
convert(::Type{UnitRange{T}}, r::UnitRange) where {T<:Real} = UnitRange{T}(r.start, r.stop)

promote_rule(::Type{OneTo{T1}},::Type{OneTo{T2}}) where {T1,T2} =
    OneTo{promote_type(T1,T2)}
convert(::Type{OneTo{T}}, r::OneTo{T}) where {T<:Real} = r
convert(::Type{OneTo{T}}, r::OneTo) where {T<:Real} = OneTo{T}(r.stop)

promote_rule(::Type{UnitRange{T1}}, ::Type{UR}) where {T1,UR<:AbstractUnitRange} =
    UnitRange{promote_type(T1,eltype(UR))}
convert(::Type{UnitRange{T}}, r::AbstractUnitRange) where {T<:Real} = UnitRange{T}(first(r), last(r))
convert(::Type{UnitRange}, r::AbstractUnitRange) = UnitRange(first(r), last(r))

promote_rule(::Type{StepRange{T1a,T1b}},::Type{StepRange{T2a,T2b}}) where {T1a,T1b,T2a,T2b} =
    StepRange{promote_type(T1a,T2a),promote_type(T1b,T2b)}
convert(::Type{StepRange{T1,T2}}, r::StepRange{T1,T2}) where {T1,T2} = r

promote_rule(::Type{StepRange{T1a,T1b}},::Type{UR}) where {T1a,T1b,UR<:AbstractUnitRange} =
    StepRange{promote_type(T1a,eltype(UR)),promote_type(T1b,eltype(UR))}
convert(::Type{StepRange{T1,T2}}, r::Range) where {T1,T2} =
    StepRange{T1,T2}(convert(T1, first(r)), convert(T2, step(r)), convert(T1, last(r)))
convert(::Type{StepRange}, r::AbstractUnitRange{T}) where {T} =
    StepRange{T,T}(first(r), step(r), last(r))

promote_rule(::Type{StepRangeLen{T1,R1,S1}},::Type{StepRangeLen{T2,R2,S2}}) where {T1,T2,R1,R2,S1,S2} =
    StepRangeLen{promote_type(T1,T2), promote_type(R1,R2), promote_type(S1,S2)}
convert(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen{T,R,S}) where {T,R,S} = r
convert(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen) where {T,R,S} =
    StepRangeLen{T,R,S}(convert(R, r.ref), convert(S, r.step), length(r), r.offset)
convert(::Type{StepRangeLen{T}}, r::StepRangeLen) where {T} =
    StepRangeLen(convert(T, r.ref), convert(T, r.step), length(r), r.offset)

promote_rule(::Type{StepRangeLen{T,R,S}}, ::Type{OR}) where {T,R,S,OR<:Range} =
    StepRangeLen{promote_type(T,eltype(OR)),promote_type(R,eltype(OR)),promote_type(S,eltype(OR))}
convert(::Type{StepRangeLen{T,R,S}}, r::Range) where {T,R,S} =
    StepRangeLen{T,R,S}(R(first(r)), S(step(r)), length(r))
convert(::Type{StepRangeLen{T}}, r::Range) where {T} =
    StepRangeLen(T(first(r)), T(step(r)), length(r))
convert(::Type{StepRangeLen}, r::Range) = convert(StepRangeLen{eltype(r)}, r)

promote_rule(::Type{LinSpace{T1}},::Type{LinSpace{T2}}) where {T1,T2} =
    LinSpace{promote_type(T1,T2)}
convert(::Type{LinSpace{T}}, r::LinSpace{T}) where {T} = r
convert(::Type{LinSpace{T}}, r::Range) where {T} =
    LinSpace{T}(first(r), last(r), length(r))
convert(::Type{LinSpace}, r::Range{T}) where {T} =
    convert(LinSpace{T}, r)

promote_rule(::Type{LinSpace{T}}, ::Type{OR}) where {T,OR<:OrdinalRange} =
    LinSpace{promote_type(T,eltype(OR))}

promote_rule(::Type{LinSpace{L}}, ::Type{StepRangeLen{T,R,S}}) where {L,T,R,S} =
    StepRangeLen{promote_type(L,T),promote_type(L,R),promote_type(L,S)}

# +/- of ranges is defined in operators.jl (to be able to use @eval etc.)

## concatenation ##

function vcat(rs::Range{T}...) where T
    n::Int = 0
    for ra in rs
        n += length(ra)
    end
    a = Vector{T}(n)
    i = 1
    for ra in rs, x in ra
        @inbounds a[i] = x
        i += 1
    end
    return a
end

convert(::Type{Array{T,1}}, r::Range{T}) where {T} = vcat(r)
collect(r::Range) = vcat(r)

reverse(r::OrdinalRange) = colon(last(r), -step(r), first(r))
reverse(r::StepRangeLen) = StepRangeLen(r.ref, -r.step, length(r), length(r)-r.offset+1)
reverse(r::LinSpace)     = LinSpace(r.stop, r.start, length(r))

## sorting ##

issorted(r::AbstractUnitRange) = true
issorted(r::Range) = length(r) <= 1 || step(r) >= zero(step(r))

sort(r::AbstractUnitRange) = r
sort!(r::AbstractUnitRange) = r

sort(r::Range) = issorted(r) ? r : reverse(r)

sortperm(r::AbstractUnitRange) = 1:length(r)
sortperm(r::Range) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum(r::Range{<:Real})
    l = length(r)
    # note that a little care is required to avoid overflow in l*(l-1)/2
    return l * first(r) + (iseven(l) ? (step(r) * (l-1)) * (l>>1)
                                     : (step(r) * l) * ((l-1)>>1))
end

function mean(r::Range{<:Real})
    isempty(r) && throw(ArgumentError("mean of an empty range is undefined"))
    (first(r) + last(r)) / 2
end

median(r::Range{<:Real}) = mean(r)

function in(x, r::Range)
    n = step(r) == 0 ? 1 : round(Integer,(x-first(r))/step(r))+1
    n >= 1 && n <= length(r) && r[n] == x
end

in(x::Integer, r::AbstractUnitRange{<:Integer}) = (first(r) <= x) & (x <= last(r))
in(x, r::Range{T}) where {T<:Integer} =
    isinteger(x) && !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(convert(T,x),step(r))-mod(first(r),step(r)) == 0)
in(x::Char, r::Range{Char}) =
    !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(Int(x) - Int(first(r)), step(r)) == 0)

# Addition/subtraction of ranges

function _define_range_op(f::ANY)
    @eval begin
        function $f(r1::OrdinalRange, r2::OrdinalRange)
            r1l = length(r1)
            (r1l == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match")))
            range($f(first(r1),first(r2)), $f(step(r1),step(r2)), r1l)
        end

        function $f(r1::LinSpace{T}, r2::LinSpace{T}) where T
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
_define_range_op(:+)
_define_range_op(:-)

function +(r1::StepRangeLen{T,S}, r2::StepRangeLen{T,S}) where {T,S}
    len = length(r1)
    (len == length(r2) ||
        throw(DimensionMismatch("argument dimensions must match")))
    StepRangeLen(first(r1)+first(r2), step(r1)+step(r2), len)
end

-(r1::StepRangeLen, r2::StepRangeLen) = +(r1, -r2)

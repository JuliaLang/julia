# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date/DateTime Ranges

StepRange{<:Dates.DatePeriod,<:Real}(start, step, stop) =
    throw(ArgumentError("must specify step as a Period when constructing Dates ranges"))
Base.:(:)(a::T, b::T) where {T<:Date} = (:)(a, Day(1), b)

# Given a start and end date, how many steps/periods are in between. Packages define `guess`
# for their own TimeTypes, for example by forwarding to the DateTime method.
function guess(a::DateTime, b::DateTime, c)
    n = floor((Int128(value(b)) - Int128(value(a))) / toms(c))
    -0x1p63 <= n < 0x1p63 || throw_stepcount_overflow(a, b, c)
    return Int64(n)
end
function guess(a::DateTime, b::DateTime, c::FixedPeriod)
    if !(c isa Union{Microsecond, Nanosecond})
        ms, huge = Base.Checked.mul_with_overflow(value(c), toms(oneunit(c)))
        span, wraps = Base.Checked.sub_with_overflow(value(b), value(a))
        # Dividing typemin(Int64) by -1 overflows, so that step takes the widened path
        huge | wraps | (ms == -1) || return fld(span, ms)
    end
    step = Int128(value(c)) * tons(oneunit(c))
    # DateTime adds the step in Int64 milliseconds, which would wrap around
    typemin(Int64) <= div(step, tons(Millisecond(1))) <= typemax(Int64) || throw_msstep_overflow(c)
    n = fld((Int128(value(b)) - Int128(value(a))) * tons(Millisecond(1)), step)
    typemin(Int64) <= n <= typemax(Int64) || throw_stepcount_overflow(a, b, c)
    return n % Int64
end
@noinline throw_msstep_overflow(c) =
    throw(OverflowError("$c does not fit in the milliseconds of a DateTime"))
@noinline throw_stepcount_overflow(a, b, c) =
    throw(OverflowError("the number of steps of $c from $a to $b does not fit in Int64"))
@noinline throw_difference_overflow(a, b) = throw(OverflowError("the difference between $b and $a overflows"))
guess(a::Date, b::Date, c) = Int64(div(value(b - a), days(c)))
# Integer division keeps step counts above 2^53 exact
function guess(a::T, b::T, c) where {T<:Timestamp}
    scale = widen(timestamp_period_scale(typeof(c)))
    s = widen(timestamp_scale(T))
    return floor(Int64, div(widen(value(b)) * s - widen(value(a)) * s, widen(value(c)) * scale))
end
len(a::Time, b::Time, c) = Int64(div(value(b - a), tons(c)))
# a + c * n, rounded the same way, also when c * n overflows the period. Unsigned counts are
# widened, as a negative step times an unsigned count would wrap around.
addsteps(a, c, n) = a + c * (n isa Unsigned ? signed(widen(n)) : n)
function addsteps(a::DateTime, c::Union{Microsecond, Nanosecond}, n)
    if n isa Union{Int, Int64}
        m, huge = Base.Checked.mul_with_overflow(value(c), Int64(n))
        huge || return a + typeof(c)(m)
    end
    # A step times a count below 2^64 fits in Int128, so the step is not converted to nanoseconds first
    p = -Int128(2)^64 < n < Int128(2)^64 ? Int128(value(c)) * Int128(n) : big(value(c)) * n
    return DateTime(UTM(value(a) + div(p, tons(Millisecond(1)) ÷ tons(oneunit(c)), RoundNearest) % Int64))
end
# DateTime steps below a millisecond round to repeated elements
submillisecond(::TimeType, step) = false
function submillisecond(::DateTime, step::Union{Microsecond, Nanosecond})
    ms = value(typeof(step)(Millisecond(1)))
    return -ms < value(step) < ms
end
# Whether `a + c * n`, whose product overflows, lies past `b == a + span`. Adding a fixed period
# rounds to the unit of `span`, so an offset at least one unit beyond `b` is past it. A closer
# offset is added near `b` by the type itself. Rounding can depend on the parity of the offset,
# as ties to even do, so the part added keeps that parity and the sign of the step.
@noinline function pastend(b, c, n, span)
    c isa FixedPeriod && span isa FixedPeriod || return false
    unit = tons(oneunit(span))
    d = big(value(c)) * n * tons(oneunit(c)) - big(value(span)) * unit
    (c > zero(c) ? d > 0 : d < 0) || return false
    abs(d) >= unit && return true
    e = isodd(value(span)) ? (c > zero(c) ? 1 : -1) : 0
    k = (d + e * unit) // tons(oneunit(c))
    isinteger(k) || return false
    return b - e * oneunit(span) + typeof(c)(numerator(k)) != b
end
# Whether `c * n` overflows the count type of `c`
function stepoverflows(c, n)
    v = value(c)
    p, huge = Base.Checked.mul_with_overflow(promote(v, n)...)
    return huge || Base.hastypemax(typeof(v)) && !(typemin(typeof(v)) <= p <= typemax(typeof(v)))
end
# Whether `a + c * n` lies between `a` and `b == a + span`. Past the last element, `past` is
# true and the product can overflow.
function stepwithin(a, b, c, span, n, past)
    if stepoverflows(c, n)
        past && pastend(b, c, n, span) && return false
        throw_stepcount_overflow(a, b, c)
    end
    v = addsteps(a, c, n)
    return c > zero(c) ? a <= v <= b : b <= v <= a  # `a` stops a wraparound
end
# The largest n for which a + c * n does not pass b. It multiplies the step like indexing does,
# as repeated additions can round differently, and corrects a guess that is off either way. It
# compares only consecutive counts, as elements that wrap around can land between `a` and `b`.
function len(a, b, c)
    # Beyond this span, the elements next to the guess can wrap around
    span = b - a
    (span > zero(span)) == (b > a) || throw_difference_overflow(a, b)
    n = guess(a, b, c)
    # One loop for both directions, as separate loops get vectorized with a setup on every call
    while true
        if n > 0 && !stepwithin(a, b, c, span, n, false)
            n -= 1
        elseif stepwithin(a, b, c, span, Base.checked_add(n, one(n)), true)
            n += 1
        else
            return n
        end
    end
end
# The correction above rejects spans close to the whole type, so count DateTime steps exactly
function len(a::DateTime, b::DateTime, c::Union{Microsecond, Nanosecond})
    unit = Int128(tons(Millisecond(1)))
    span = abs(Int128(value(b)) - Int128(value(a)))
    st = abs(Int128(value(c)) * tons(oneunit(c)))
    # Step n lands on div(n * st, unit, RoundNearest), and that ties to an even millisecond
    n = fld(span * unit + div(unit, 2) - isodd(span), st)
    n < typemax(Int64) || throw_stepcount_overflow(a, b, c)
    return n % Int64
end
# The largest n for which a + c * n does not pass b. Adding months keeps the time of day and
# clamps the day, so only the step landing in the month of b can pass it.
function len(a::T, b::T, c::Union{Year, Quarter, Month}) where {T <: Union{Date, DateTime, Timestamp}}
    months = widen(value(c)) * value(Month(oneunit(c)))  # Month(c) overflows for huge steps
    ya, ma, da = yearmonthday(a)
    yb, mb, db = yearmonthday(b)
    diff = 12 * (yb - ya) + mb - ma
    n, r = typemin(Int64) <= months <= typemax(Int64) ? fldmod(diff, months % Int64) : fldmod(diff, months)
    if iszero(r)
        landed = (min(da, daysinmonth(yb, mb)), nsofday(a))
        target = (db, nsofday(b))
        n -= months > 0 ? landed > target : landed < target
    end
    return n % typeof(diff)
end
# Period ranges hook into Int64 overflow detection
Base.length(r::StepRange{<:Period}) = length(StepRange(value(r.start), value(r.step), value(r.stop)))
Base.checked_length(r::StepRange{<:Period}) = Base.checked_length(StepRange(value(r.start), value(r.step), value(r.stop)))

# A fixed step is a whole number of the units of `value` for Date, DateTime, and Timestamp, so
# their ranges behave like integer ranges at the ends of the type. `valuestep` returns whether
# such a step is negative, its magnitude in these units, and whether the magnitude overflows.
# A step that is not a whole number of these units is not fixed.
valuestep(::TimeType, step) = nothing
valuestep(::Date, step::Day) = unsignedstep(value(step), 1)
valuestep(::Date, step::Week) = unsignedstep(value(step), 7)
valuestep(::DateTime, step::Union{Week, Day, Hour, Minute, Second, Millisecond}) =
    unsignedstep(value(step), toms(oneunit(step)))
function valuestep(::DateTime, step::Union{Microsecond, Nanosecond})
    ms, rest = divrem(value(step), value(typeof(step)(Millisecond(1))))
    return iszero(rest) ? unsignedstep(ms, 1) : nothing
end
function valuestep(::Timestamp{P}, step::Union{FixedPeriod, TimePeriod}) where {P}
    ticks, rest = timestamp_period_divrem(P, step)
    iszero(rest) || return nothing
    U = distancetype(timestamp_count_type(P))
    huge = Base.hastypemax(U) && abs(ticks) > typemax(U)
    return ticks < 0, huge ? zero(U) : U(abs(ticks)), huge
end
# The type that holds every distance between two values of type T
distancetype(::Type{T}) where {T <: Base.BitInteger} = unsigned(T)
distancetype(::Type{T}) where {T <: Integer} = T
function unsignedstep(n, unit)
    m, huge = Base.Checked.mul_with_overflow(unsigned(abs(n)), unsigned(oftype(n, unit)))
    return n < 0, m, huge
end
# The distance from `a` to `b` in the direction of the step, exact if `b` lies on that side
valuedistance(a, b, neg) = (neg ? value(a) - value(b) : value(b) - value(a)) % distancetype(typeof(value(a)))
# A value before `x` in the direction of `st`, which throws if no value precedes `x`, as for
# integer ranges. Other types move one unit of their differences into the past, as the step
# can round back to `x` or reach a nonexistent local time, and time zone rules can end in the
# future.
valuebefore(x, st) = x - oneunit(x - x)
function valuebefore(x::Union{Date, DateTime, Timestamp}, st)
    v = value(x)
    w, wraps = st > zero(st) ? Base.sub_with_overflow(v, one(v)) : Base.add_with_overflow(v, one(v))
    wraps && throw_emptyrange_overflow(x)
    return withvalue(x, w)
end
@noinline throw_emptyrange_overflow(x) = throw(OverflowError("cannot represent an empty range starting at $x"))
function valuebefore(x::Time, st)
    x == (st > zero(st) ? typemin(x) : typemax(x)) && throw_emptyrange_overflow(x)
    return Time(Nanosecond(value(x) + (st > zero(st) ? -1 : 1)))
end
# An empty range with step `st` between `x` and `y`. Distinct ends need no arithmetic, and an
# end equal to `x` is replaced by `valuebefore(x, st)`.
function emptyrange(x, y, st)
    x == y && (y = valuebefore(x, st))
    return st > zero(st) ? StepRange(max(x, y), st, min(x, y)) : StepRange(min(x, y), st, max(x, y))
end
# The number of steps after the first element of a nonempty range with a fixed step
stepcount(r, (neg, m, huge)) = huge ? zero(m) : valuedistance(r.start, r.stop, neg) ÷ m

# Like for integer ranges, lengths of values narrower than Int promote to Int
function Base.length(r::StepRange{<:TimeType})
    vs = valuestep(r.start, r.step)
    isnothing(vs) && return isempty(r) ? Int64(0) : len(r.start, r.stop, r.step) + 1
    L = promote_type(typeof(value(r.start)), Int)
    isempty(r) && return zero(L)
    return stepcount(r, vs) % L + one(L)
end
@noinline throw_length_overflow(r, L) = throw(OverflowError("the length of $r does not fit in $L"))
function Base.checked_length(r::StepRange{<:TimeType})
    vs = valuestep(r.start, r.step)
    isnothing(vs) && return length(r)
    L = promote_type(typeof(value(r.start)), Int)
    isempty(r) && return zero(L)
    q = stepcount(r, vs)
    !Base.hastypemax(L) || q < typemax(L) || throw_length_overflow(r, L)
    return q % L + one(L)
end
# Like for integer ranges, the bounds do not depend on a length that wraps around
function Base.checkbounds(::Type{Bool}, r::StepRange{<:TimeType}, i::Union{Signed, Unsigned})
    vs = valuestep(r.start, r.step)
    isnothing(vs) && return 0 < i <= length(r)
    return !isempty(r) && 0 < i && i - one(i) <= stepcount(r, vs)
end
# Fixed steps add the offset in the unsigned value type, like integer ranges, so no period overflows
function Base.unsafe_getindex(r::StepRange{<:TimeType}, i::Integer)
    vs = valuestep(r.start, r.step)
    isnothing(vs) && return addsteps(r.start, r.step, i - oneunit(i))
    return addvaluesteps(r.start, vs, i - oneunit(i))
end
function addvaluesteps(x, (neg, m, _), k)
    U = typeof(m)
    offset = k % U * m
    v = value(x) % U
    return withvalue(x, (neg ? v - offset : v + offset) % typeof(value(x)))
end
# `range` with a step and a length steps like `start:step:stop` does, from its start or its
# stop. Base computes the other end only for its type, which multiplies the step as a period,
# so that product can overflow. Adding the step once, or no step to a range that never takes
# it, still throws for a step that cannot be added to the reference.
function steprangelen(ref::T, step, len, offset) where {T}
    r = StepRangeLen{T, T, typeof(step)}(ref, step, len, offset)
    ref + (len > 1 ? step : zero(step))
    return r
end
Base.range_start_step_length(a::T, step::Period, len::Integer) where {T <: Union{Date, DateTime, Timestamp}} =
    steprangelen(a, step, len, 1)
# An empty range starts at `stop + step`, as in Base
Base.range_step_stop_length(step::Period, b::T, len::Integer) where {T <: Union{Date, DateTime, Timestamp}} =
    iszero(len) ? steprangelen(b + step, step, len, 1) : steprangelen(b, step, len, len)
function Base.unsafe_getindex(r::StepRangeLen{T, T, <:Period}, i::Integer) where {T <: TimeType}
    vs = valuestep(r.ref, r.step)
    # A reversed range indexes before its reference, which an unsigned offset cannot express
    k = r.offset isa Unsigned ? signed(widen(i)) - signed(widen(r.offset)) : oftype(r.offset, i) - r.offset
    return isnothing(vs) || vs[3] ? addsteps(r.ref, r.step, k) : addvaluesteps(r.ref, vs, k)
end
# A slice starts at r[first(s)] and keeps length(s) elements. Fixed steps end at r[last(s)].
# Other steps continue from the slice start, so later elements can differ from r[s] and pass
# last(r).
@noinline throw_slicestep_overflow(r, s) =
    throw(OverflowError("the slice step $(step(s)) times the range step does not fit in $(typeof(r.step))"))
# The step of r[s]. A slice with at most one element never takes its step, so it keeps the
# step of r if the product does not fit.
function slicestep(r, s)
    v = widen(value(r.step)) * signed(widen(step(s)))
    V = typeof(value(r.step))
    fits = !Base.hastypemax(V) || typemin(V) <= v <= typemax(V)
    fits || length(s) < 2 || throw_slicestep_overflow(r, s)
    return fits ? typeof(r.step)(v) : r.step
end
@inline function Base.getindex(r::StepRangeLen{T, T, <:Period}, s::OrdinalRange{S}) where {T <: TimeType, S <: Integer}
    S === Bool && return invoke(getindex, Tuple{StepRangeLen{T}, OrdinalRange{S}}, r, s)
    @boundscheck checkbounds(r, s)
    st = slicestep(r, s)
    ref = checkbounds(Bool, r, first(s)) ? Base.unsafe_getindex(r, first(s)) : r.ref
    return StepRangeLen{T, T, typeof(st)}(ref, st, length(s))
end
# Whether an integer selector spaces its elements evenly between its ends. A float reference
# or step, as in LinRange{Int}, rounds them unevenly, and those of a StepRangeLen can wrap around.
evenlyspaced(::AbstractRange) = false
evenlyspaced(::OrdinalRange) = true
function evenlyspaced(s::StepRangeLen{<:Integer, <:Integer, <:Integer})
    exact(i) = widen(s.ref) + (widen(i) - s.offset) * widen(s.step)
    return isempty(s) || exact(1) == first(s) && exact(length(s)) == last(s)
end
@inline function Base.getindex(r::StepRange{<:TimeType}, s::AbstractRange{T}) where {T <: Integer}
    T === Bool && return invoke(getindex, Tuple{StepRange, AbstractRange{T}}, r, s)
    if !evenlyspaced(s)
        @boundscheck all(i -> checkbounds(Bool, r, i), s) || Base.throw_boundserror(r, s)
        return [Base.unsafe_getindex(r, i) for i in s]
    end
    @boundscheck isempty(s) || checkbounds(Bool, r, first(s)) && checkbounds(Bool, r, last(s)) ||
        Base.throw_boundserror(r, s)
    st = slicestep(r, s)
    if isempty(s)
        # Index 1 needs no length, which other types compute only if they define `guess`
        i = first(s)
        (isone(i) ? !isempty(r) : checkbounds(Bool, r, i)) || return emptyrange(first(r), last(r), st)
        x = Base.unsafe_getindex(r, i)
        return emptyrange(x, x, st)
    end
    start = Base.unsafe_getindex(r, first(s))
    if isnothing(valuestep(r.start, r.step))
        n = length(s) - oneunit(length(s))
        stop = addsteps(start, st, n)
        # Steps from the slice start round differently and can wrap around an end of the type
        iszero(n) || (st > zero(st)) == (stop > start) || throw_slicestop_overflow(start, st, n)
    else
        stop = Base.unsafe_getindex(r, last(s))
    end
    return range(start, stop; step = st)
end
@noinline throw_slicestop_overflow(start, st, n) =
    throw(OverflowError("$start + $n * $st lies outside $(typeof(start))"))

# Overload Base.steprange_last because `step::Period` may be a variable amount of time (e.g. for Month and Year)
function Base.steprange_last(start::T, step, stop) where T<:TimeType
    step isa AbstractFloat && throw(ArgumentError("StepRange should not be used with floating point"))
    step == zero(step) && throw(ArgumentError("step cannot be zero"))
    submillisecond(start, step) &&
        throw(ArgumentError("a DateTime range step must be at least one millisecond"))
    stop == start && return stop
    vs = valuestep(start, step)
    if (step > zero(step)) != (stop > start)
        isnothing(vs) && return Base.steprange_last_empty(start, step, stop)
        # An empty fixed-step range ends one unit before `start`, as an empty integer range does
        return valuebefore(start, step)
    end
    isnothing(vs) && return addsteps(start, step, len(start, stop, step))
    neg, m, huge = vs
    huge && return start
    remain = valuedistance(start, stop, neg) % m % typeof(value(stop))
    return withvalue(stop, neg ? value(stop) + remain : value(stop) - remain)
end

import Base.in
function in(x::T, r::StepRange{T}) where T<:TimeType
    isempty(r) && return false
    neg = step(r) < zero(step(r))
    lo, hi = neg ? (last(r), first(r)) : (first(r), last(r))
    lo <= x <= hi || return false
    vs = valuestep(first(r), step(r))
    isnothing(vs) && return Base.unsafe_getindex(r, len(first(r), x, step(r)) + 1) == x
    _, m, huge = vs
    d = valuedistance(first(r), x, neg)
    return iszero(huge ? d : d % m)
end

# Fixed steps count the remaining steps, which always fit in the unsigned value type, even
# when the length wraps around. Calendar steps count from the start, as adding a month
# twice can differ from adding two.
function Base.iterate(r::StepRange{<:TimeType})
    isempty(r) && return nothing
    vs = valuestep(r.start, r.step)
    isnothing(vs) && return r.start, (length(r), Int64(1))
    return r.start, (r.start, stepcount(r, vs))
end
function Base.iterate(r::StepRange{T}, (x, k)::Tuple{T, Integer}) where {T<:TimeType}
    iszero(k) && return nothing
    y = x + r.step
    return y, (y, k - one(k))
end
Base.iterate(r::StepRange{<:TimeType}, (l, i)::Tuple{Integer, Integer}) =
    l <= i ? nothing : (addsteps(r.start, r.step, i), (l, i + 1))

# Empty ranges stay empty, as calendar arithmetic can move both ends to the same date
+(x::Period, r::AbstractRange{<:TimeType}) =
    isempty(r) ? emptyrange(x + first(r), x + last(r), step(r)) : (x + first(r)):step(r):(x + last(r))
+(r::AbstractRange{<:TimeType}, x::Period) = x + r
-(r::AbstractRange{<:TimeType}, x::Period) =
    isempty(r) ? emptyrange(first(r) - x, last(r) - x, step(r)) : (first(r) - x):step(r):(last(r) - x)
*(x::Period, r::AbstractRange{<:Real}) = (x * first(r)):(x * step(r)):(x * last(r))
*(r::AbstractRange{<:Real}, x::Period) = x * r
/(r::AbstractRange{<:P}, x::P) where {P<:Period} = (first(r)/x):(step(r)/x):(last(r)/x)

# Combinations of types and periods for which the range step is regular
Base.RangeStepStyle(::Type{<:OrdinalRange{<:TimeType, <:FixedPeriod}}) =
    Base.RangeStepRegular()

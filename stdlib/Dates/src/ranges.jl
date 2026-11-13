# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date/DateTime Ranges

StepRange{<:Dates.DatePeriod,<:Real}(start, step, stop) =
    throw(ArgumentError("must specify step as a Period when constructing Dates ranges"))
Base.:(:)(a::T, b::T) where {T<:Date} = (:)(a, Day(1), b)

# Given a start and end date, how many steps/periods are in between
guess(a::DateTime, b::DateTime, c) = floor(Int64, (Int128(value(b)) - Int128(value(a))) / toms(c))
# Integer division keeps step counts above 2^53 exact
function guess(a::T, b::T, c) where {T<:Timestamp}
    # Calendar steps use an average length for this estimate; len corrects it below.
    scale = widen(c isa Union{Year,Quarter,Month} ? tons(oneunit(c)) : timestamp_period_scale(typeof(c)))
    s = widen(timestamp_scale(T))
    return floor(Int64, div(widen(value(b)) * s - widen(value(a)) * s, widen(value(c)) * scale))
end
guess(a::Date, b::Date, c) = Int64(div(value(b - a), days(c)))
len(a::Time, b::Time, c) = Int64(div(value(b - a), tons(c)))
function len(a, b, c)
    lo, hi, st = min(a, b), max(a, b), abs(c)
    i = guess(a, b, c)
    v = lo + st * i
    prev = v  # Ensure `v` does not overflow
    while v <= hi && prev <= v
        prev = v
        v += st
        i += 1
    end
    return i - 1
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
    isnothing(vs) && return r.start + r.step * (i - oneunit(i))
    return addvaluesteps(r.start, vs, i - oneunit(i))
end
function addvaluesteps(x, (neg, m, _), k)
    U = typeof(m)
    offset = k % U * m
    v = value(x) % U
    return withvalue(x, (neg ? v - offset : v + offset) % typeof(value(x)))
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
        stop = start + st * (length(s) - oneunit(length(s)))
    else
        stop = Base.unsafe_getindex(r, last(s))
    end
    return range(start, stop; step = st)
end

# Overload Base.steprange_last because `step::Period` may be a variable amount of time (e.g. for Month and Year)
function Base.steprange_last(start::T, step, stop) where T<:TimeType
    step isa AbstractFloat && throw(ArgumentError("StepRange should not be used with floating point"))
    step == zero(step) && throw(ArgumentError("step cannot be zero"))
    stop == start && return stop
    vs = valuestep(start, step)
    if (step > zero(step)) != (stop > start)
        isnothing(vs) && return Base.steprange_last_empty(start, step, stop)
        # An empty fixed-step range ends one unit before `start`, as an empty integer range does
        return valuebefore(start, step)
    end
    if isnothing(vs)
        diff = stop - start
        (diff > zero(diff)) != (stop > start) && throw(OverflowError("Difference between stop and start overflowed"))
        return start + step * len(start, stop, step)
    end
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
    isnothing(vs) && return r[len(first(r), x, step(r)) + 1] == x
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
    l <= i ? nothing : (r.start + r.step * i, (l, i + 1))

+(x::Period, r::AbstractRange{<:TimeType}) = (x + first(r)):step(r):(x + last(r))
+(r::AbstractRange{<:TimeType}, x::Period) = x + r
-(r::AbstractRange{<:TimeType}, x::Period) = (first(r)-x):step(r):(last(r)-x)
*(x::Period, r::AbstractRange{<:Real}) = (x * first(r)):(x * step(r)):(x * last(r))
*(r::AbstractRange{<:Real}, x::Period) = x * r
/(r::AbstractRange{<:P}, x::P) where {P<:Period} = (first(r)/x):(step(r)/x):(last(r)/x)

# Combinations of types and periods for which the range step is regular
Base.RangeStepStyle(::Type{<:OrdinalRange{<:TimeType, <:FixedPeriod}}) =
    Base.RangeStepRegular()

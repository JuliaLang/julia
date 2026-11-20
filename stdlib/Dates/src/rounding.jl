# This file is a part of Julia. License is MIT: https://julialang.org/license

# The epochs used for date rounding are based on ISO 8601's "year zero" notation
const DATEEPOCH = value(Date(0))
const DATETIMEEPOCH = value(DateTime(0))

# According to ISO 8601, the first day of the first week of year 0000 is 0000-01-03
const WEEKEPOCH = value(Date(0, 1, 3))

const ConvertiblePeriod = Union{TimePeriod, Week, Day}
const TimeTypeOrPeriod = Union{TimeType, ConvertiblePeriod}
const BuiltinDatePeriod = Union{Year, Quarter, Month, Week, Day}
const BuiltinTimePeriod = Union{Hour, Minute, Second, Millisecond, Microsecond, Nanosecond}

"""
    epochdays2date(days)::Date

Take the number of days since the rounding epoch (`0000-01-01T00:00:00`) and return the
corresponding `Date`. Throws an `InexactError` if the result lies outside `Date`.
"""
epochdays2date(i) = Date(UTD(Int64(DATEEPOCH + Int128(i))))

"""
    epochms2datetime(milliseconds)::DateTime

Take the number of milliseconds since the rounding epoch (`0000-01-01T00:00:00`) and
return the corresponding `DateTime`. Throws an `InexactError` if the result lies outside
`DateTime`.
"""
epochms2datetime(i) = DateTime(UTM(Int64(DATETIMEEPOCH + Int128(i))))

"""
    date2epochdays(dt::Date)::Int64

Take the given `Date` and return the number of days since the rounding epoch
(`0000-01-01T00:00:00`) as an [`Int64`](@ref). Throws an `OverflowError` if the number does
not fit in an `Int64`.
"""
date2epochdays(dt::Date) = Base.checked_sub(value(dt), DATEEPOCH)

"""
    datetime2epochms(dt::DateTime)::Int64

Take the given `DateTime` and return the number of milliseconds since the rounding epoch
(`0000-01-01T00:00:00`) as an [`Int64`](@ref). Throws an `OverflowError` if the number does
not fit in an `Int64`.
"""
datetime2epochms(dt::DateTime) = Base.checked_sub(value(dt), DATETIMEEPOCH)

# Exact floor and ceiling of `x` to multiples of `period` from `epoch`. They are equal only if
# `x` is a multiple and `exact`. The distance between `x` and `epoch` always fits in UInt64.
@inline function fixedbounds(x::Int64, exact::Bool, period::Int64, epoch::Int64)
    step = period % UInt64
    excess = if x >= epoch
        (x - epoch) % UInt64 % step
    else
        step - one(step) - ((epoch - x) % UInt64 - one(step)) % step
    end
    f = Int128(x) - excess
    return f, iszero(excess) & exact ? f : f + step
end
# Wider counts and periods, which keep both bounds far from the limits of their types
@inline function fixedbounds(x::Integer, exact::Bool, period::Integer, epoch::Integer)
    excess = mod(x - epoch, period)
    f = x - excess
    return f, iszero(excess) & exact ? f : f + period
end

# The Rata Die day of the first day of month `m` (0 = March) of the year `y` that starts in
# March. Such years end with the leap day, so their months start (153m + 2) ÷ 5 days in.
@inline function marchmonthstartday(y::Integer, m::Integer)
    start(y) = 365y + fld(y, 4) - fld(y, 100) + fld(y, 400) + (153m + 2) ÷ 5 + 1 - DAYS_MAR_TO_JAN
    fits = typemin(Int64) ÷ DAYS_PER_LEAP_YEAR < y < typemax(Int64) ÷ DAYS_PER_LEAP_YEAR
    return fits ? Int128(start(y % Int64)) : start(y isa Int64 ? Int128(y) : y)
end
# The first day of month `i`, counted from 0000-01, and of year `y`
@inline monthstartday(i::Integer) = marchmonthstartday(fldmod(i - 2, 12)...)
@inline yearstartday(y::Integer) = marchmonthstartday(y - 1, 10)

@inline function monthbounds(months::Integer, exact::Bool, step::Integer)
    f = months - mod(months, step)
    first = monthstartday(f)
    return first, f == months && exact ? first : monthstartday(f + step)
end

# Exact floor and ceiling of day `days` in days. `exact` is false if a time of day follows it.
@inline function daybounds(days, exact, p::Week)
    n, huge = Base.Checked.mul_with_overflow(value(p), Int64(7))
    huge && return fixedbounds(widen(days), exact, widemul(value(p), Int64(7)), WEEKEPOCH)
    return fixedbounds(days, exact, n, WEEKEPOCH)
end
@inline daybounds(days, exact, p::Day) = fixedbounds(days, exact, value(p), DATEEPOCH)
@inline function daybounds(days, exact, p::Union{Quarter, Month})
    y, m, d = @inline yearmonthday(days)
    step = widen(value(p)) * (p isa Quarter ? 3 : 1)
    exact &= d == 1
    if days isa Int64 && p isa Month && isone(value(p))
        # The month of `days` itself, found without counting months from 0000-01
        first = Int128(days) - (d - 1)
        return first, exact ? first : first + daysinmonth(y, m)
    end
    # Smaller steps keep the bounds of the months since 0000-01 within Int64
    step <= typemax(Int64) ÷ 2 && return monthbounds(12y + m - 1, exact, step % Int64)
    return monthbounds(12y + m - 1, exact, step)
end
@inline function daybounds(days, exact, p::Year)
    y, m, d = @inline yearmonthday(days)
    k = value(p)
    k <= typemax(Int64) ÷ 2 || return monthbounds(12y + m - 1, exact & (d == 1), widen(k) * 12)
    f = y - mod(y, k)
    first = yearstartday(f)
    return first, f == y && exact & (m == 1) & (d == 1) ? first : yearstartday(f + k)
end

@noinline throw_nonpositive_period(p) = throw(DomainError(p))
# Exact floor and ceiling of `dt` at resolution `p`, in the units of `value(dt)`
@inline function roundingbounds(dt::Date, p::BuiltinDatePeriod)
    value(p) < 1 && throw_nonpositive_period(p)
    return daybounds(value(dt), true, p)
end
@inline function roundingbounds(dt::DateTime, p::BuiltinDatePeriod)
    value(p) < 1 && throw_nonpositive_period(p)
    days, ms = fldmod(value(dt), MS_PER_DAY)
    f, c = daybounds(days, iszero(ms), p)
    return MS_PER_DAY * f, MS_PER_DAY * c
end
# Package periods convert to milliseconds
@inline function roundingbounds(dt::DateTime, p::TimePeriod)
    value(p) < 1 && throw_nonpositive_period(p)
    return fixedbounds(value(dt), true, value(Millisecond(p)), DATETIMEEPOCH)
end
# Coarser periods than a millisecond can exceed the milliseconds of Int64
@inline function roundingbounds(dt::DateTime, p::Union{Hour, Minute, Second, Millisecond})
    value(p) < 1 && throw_nonpositive_period(p)
    ms, huge = Base.Checked.mul_with_overflow(value(p), toms(oneunit(p)))
    huge && return fixedbounds(widen(value(dt)), true, widemul(value(p), toms(oneunit(p))), DATETIMEEPOCH)
    return fixedbounds(value(dt), true, ms, DATETIMEEPOCH)
end

@noinline throw_rounding_overflow(op, dt, p) =
    throw(OverflowError("the $op of $dt to a multiple of $(typeof(p)) lies outside the range of $(typeof(dt))"))
@inline roundedvalue(op, dt, p, v) =
    typemin(Int64) <= v <= typemax(Int64) ? withvalue(dt, v % Int64) : throw_rounding_overflow(op, dt, p)

# Only the returned bound must fit in the type, so these do not add `p` to the floor
for (T, P) in ((Date, BuiltinDatePeriod), (DateTime, BuiltinDatePeriod), (DateTime, BuiltinTimePeriod))
    @eval begin
        @inline Base.floor(dt::$T, p::$P) = roundedvalue(:floor, dt, p, roundingbounds(dt, p)[1])
        @inline Base.ceil(dt::$T, p::$P) = roundedvalue(:ceil, dt, p, roundingbounds(dt, p)[2])
        @inline function floorceil(dt::$T, p::$P)
            f, c = roundingbounds(dt, p)
            return roundedvalue(:floor, dt, p, f), roundedvalue(:ceil, dt, p, c)
        end
        @inline function Base.round(dt::$T, p::$P, ::RoundingMode{:NearestTiesUp})
            f, c = roundingbounds(dt, p)
            return roundedvalue(:round, dt, p, value(dt) - f < c - value(dt) ? f : c)
        end
    end
end
# Package periods define or derive `floor`, which the generic `ceil`, `floorceil`, and `round` build on
Base.floor(dt::DateTime, p::DatePeriod) = roundedvalue(:floor, dt, p, widemul(value(floor(Date(dt), p)), MS_PER_DAY))
Base.floor(dt::DateTime, p::TimePeriod) = roundedvalue(:floor, dt, p, roundingbounds(dt, p)[1])

function Base.floor(t::Time, p::TimePeriod)
    value(p) < 1 && throw(DomainError(p))
    nanoseconds = value(t)
    return Time(Nanosecond(nanoseconds - mod(nanoseconds, value(Nanosecond(p)))))
end

# The bound `ns`, in exact nanoseconds since the Unix epoch (Int128, or a Rational below a
# nanosecond), as Timestamp{P}. It must fit in Timestamp{P}, or an OverflowError is thrown, and
# must be a whole tick of P, or an InexactError is thrown.
function timestamp_rounding_value(dt::Timestamp{P}, p, ns, op::Symbol) where {P}
    ticks, remainder = divrem(ns, timestamp_scale(P))
    lo, hi = value(typemin(P)), value(typemax(P))
    # A bound less than a tick past an end truncates to that end
    (lo < ticks || ticks == lo && remainder >= 0) && (ticks < hi || ticks == hi && remainder <= 0) ||
        throw_rounding_overflow(op, dt, p)
    iszero(remainder) || Core.throw_inexacterror(op, Timestamp{P}, dt)
    return Timestamp{P}(UTInstant(P(ticks)))
end
# The bound `v`, in ticks if `inticks` and otherwise in nanoseconds
@inline function timestamp_boundvalue(dt::Timestamp{P}, p, v, inticks::Bool, op::Symbol) where {P}
    inticks || return timestamp_rounding_value(dt, p, v, op)
    value(typemin(P)) <= v <= value(typemax(P)) || throw_rounding_overflow(op, dt, p)
    return Timestamp{P}(UTInstant(P(v % timestamp_count_type(P))))
end

# `p` as a whole number of ticks of P in Int64, or `nothing`
@inline function tickstep(::Type{P}, p::TimePeriod) where {P}
    s, ps = timestamp_scale(P), timestamp_period_scale(typeof(p))
    timestamp_count_type(P) === Int64 && value(p) isa Int64 && s isa Int64 && ps isa Int64 || return nothing
    if iszero(ps % s)
        n, huge = Base.Checked.mul_with_overflow(value(p), ps ÷ s)
        return huge ? nothing : n
    end
    k = s ÷ ps
    return iszero(s % ps) && iszero(value(p) % k) ? value(p) ÷ k : nothing
end

# The ticks from 0000-01-01 to the Unix epoch modulo `st`, from `tpd` ticks per day
@inline function epochresidue(tpd::Int64, st::Int64)
    d = UNIXEPOCHDAYS - DATEEPOCH
    r = tpd % st
    return r <= typemax(Int64) ÷ d ? (d * r) % st : (widemul(d, r) % st) % Int64
end
# `epochns` modulo `step` where a fraction of a nanosecond would scale it past Int128
roundingepoch(epochns, x::Integer, step::Integer) = epochns
roundingepoch(epochns, x, step::Integer) = mod(epochns, step)
function roundingepoch(epochns, x, step::Rational)
    n, d = numerator(step), denominator(step)
    m, huge = Base.Checked.mul_with_overflow(promote(epochns, d)...)
    return (huge ? oftype(n, mod(widemul(epochns, d), n)) : mod(m, n)) // d
end

# The value of `dt` and its floor and ceiling. A period of whole ticks rounds in ticks with
# 64-bit division, which only needs the epoch modulo the step. Other periods round in exact
# nanoseconds (Int128, or a Rational below a nanosecond), so a bound past the range does not
# wrap. As for Date and DateTime, periods count from 0000-01-01.
@inline function timestamp_rounding_bounds(dt::Timestamp{P}, p::TimePeriod) where {P}
    value(p) < 1 && throw_nonpositive_period(p)
    st = tickstep(P, p)
    if !isnothing(st)
        e = epochresidue(timestamp_ticks_per_day(P), st)
        return Int128(value(dt)), fixedbounds(value(dt), true, st, -e)..., true
    end
    epochns = Int128(UNIXEPOCHDAYS - DATEEPOCH) * NS_PER_DAY
    x = signed(widen(value(dt))) * widen(timestamp_scale(typeof(dt)))
    step = signed(widen(value(p))) * widen(timestamp_period_scale(typeof(p)))
    f = x - mod(x + roundingepoch(epochns, x, step), step)
    return x, f, x == f ? f : f + step, false
end

@inline function Base.floor(dt::Timestamp, p::TimePeriod)
    _, f, _, inticks = timestamp_rounding_bounds(dt, p)
    return timestamp_boundvalue(dt, p, f, inticks, :floor)
end
@inline function Base.ceil(dt::Timestamp, p::TimePeriod)
    _, _, c, inticks = timestamp_rounding_bounds(dt, p)
    return timestamp_boundvalue(dt, p, c, inticks, :ceil)
end
@inline function floorceil(dt::Timestamp, p::TimePeriod)
    _, f, c, inticks = timestamp_rounding_bounds(dt, p)
    return timestamp_boundvalue(dt, p, f, inticks, :floor), timestamp_boundvalue(dt, p, c, inticks, :ceil)
end
@inline function Base.round(dt::Timestamp, p::TimePeriod, ::RoundingMode{:NearestTiesUp})
    x, f, c, inticks = timestamp_rounding_bounds(dt, p)
    return timestamp_boundvalue(dt, p, x - f < c - x ? f : c, inticks, :round)
end

# Timestamp rounding to a DatePeriod works in days, which become ticks only once they fit.
# Returns the day and the ticks after its start, and the bounds as days.
@inline function timestamp_daybounds(dt::Timestamp{P}, p::BuiltinDatePeriod) where {P}
    value(p) < 1 && throw_nonpositive_period(p)
    days, rest = fldmod(value(dt), timestamp_ticks_per_day(P))
    d = signedcount(days) + UNIXEPOCHDAYS
    return d, rest, daybounds(d, iszero(rest), p)...
end
@inline function timestamp_dayvalue(dt::Timestamp{P}, p, d, op::Symbol) where {P}
    tpd = timestamp_ticks_per_day(P)
    n = d - UNIXEPOCHDAYS
    cld(value(typemin(P)), tpd) <= n <= fld(value(typemax(P)), tpd) || throw_rounding_overflow(op, dt, p)
    return Timestamp{P}(UTInstant(P(timestamp_count_type(P)(n) * tpd)))
end

@inline Base.floor(dt::Timestamp, p::BuiltinDatePeriod) = timestamp_dayvalue(dt, p, timestamp_daybounds(dt, p)[3], :floor)
@inline Base.ceil(dt::Timestamp, p::BuiltinDatePeriod) = timestamp_dayvalue(dt, p, timestamp_daybounds(dt, p)[4], :ceil)
@inline function floorceil(dt::Timestamp, p::BuiltinDatePeriod)
    _, _, f, c = timestamp_daybounds(dt, p)
    return timestamp_dayvalue(dt, p, f, :floor), timestamp_dayvalue(dt, p, c, :ceil)
end
@inline function Base.round(dt::Timestamp{P}, p::BuiltinDatePeriod, ::RoundingMode{:NearestTiesUp}) where {P}
    d, rest, f, c = timestamp_daybounds(dt, p)
    # Compares the distances in whole days and the ticks after `d`, which cannot overflow
    more = (c - d) - (d - f)
    down = more >= 2 || more == 1 && rest < timestamp_ticks_per_day(P) - rest
    return timestamp_dayvalue(dt, p, down ? f : c, :round)
end

"""
    floor(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` down to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`.

For convenience, `precision` may be a type instead of a value: `floor(x, Dates.Hour)` is a
shortcut for `floor(x, Dates.Hour(1))`.

```jldoctest
julia> floor(Day(16), Week)
2 weeks

julia> floor(Minute(44), Minute(15))
30 minutes

julia> floor(Hour(36), Day)
1 day
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.floor(x::ConvertiblePeriod, precision::T) where T <: ConvertiblePeriod
    value(precision) < 1 && throw(DomainError(precision))
    _x, _precision = promote(x, precision)
    return T(_x - mod(_x, _precision))
end

"""
    floor(dt::TimeType, p::Period)::TimeType

Return the nearest `Date`, `DateTime`, or `Timestamp` less than or equal to `dt`
at resolution `p`.

For convenience, `p` may be a type instead of a value: `floor(dt, Dates.Hour)` is a shortcut
for `floor(dt, Dates.Hour(1))`.

```jldoctest
julia> floor(Date(1985, 8, 16), Month)
1985-08-01

julia> floor(DateTime(2013, 2, 13, 0, 31, 20), Minute(15))
2013-02-13T00:30:00

julia> floor(DateTime(2016, 8, 6, 12, 0, 0), Day)
2016-08-06T00:00:00
```
"""
Base.floor(::Dates.TimeType, ::Dates.Period)

"""
    ceil(dt::TimeType, p::Period)::TimeType

Return the nearest `Date`, `DateTime`, or `Timestamp` greater than or equal to
`dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `ceil(dt, Dates.Hour)` is a shortcut
for `ceil(dt, Dates.Hour(1))`.

```jldoctest
julia> ceil(Date(1985, 8, 16), Month)
1985-09-01

julia> ceil(DateTime(2013, 2, 13, 0, 31, 20), Minute(15))
2013-02-13T00:45:00

julia> ceil(DateTime(2016, 8, 6, 12, 0, 0), Day)
2016-08-07T00:00:00
```
"""
function Base.ceil(dt::TimeType, p::Period)
    c, wrapped = ceilfromfloor(dt, floor(dt, p), p)
    wrapped && throw_rounding_overflow(:ceil, dt, p)
    return c
end
# The ceiling `f + p` and whether it wrapped around the end of the type. `p` is positive, so
# only a wrap puts it at or before `f`. A `Time` wraps at midnight by design.
@inline function ceilfromfloor(dt, f, p)
    dt == f && return f, false
    c = f + p
    return c, !(dt isa Time) && c <= f
end

"""
    ceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` up to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`.

For convenience, `precision` may be a type instead of a value: `ceil(x, Dates.Hour)` is a
shortcut for `ceil(x, Dates.Hour(1))`.

```jldoctest
julia> ceil(Day(16), Week)
3 weeks

julia> ceil(Minute(44), Minute(15))
45 minutes

julia> ceil(Hour(36), Day)
2 days
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.ceil(x::ConvertiblePeriod, precision::ConvertiblePeriod)
    f = floor(x, precision)
    return (x == f) ? f : f + precision
end

"""
    floorceil(dt::TimeType, p::Period) -> (TimeType, TimeType)

Simultaneously return the `floor` and `ceil` of a `Date`, `DateTime`, or
`Timestamp` at resolution `p`. More efficient than calling both `floor` and
`ceil` individually.
"""
function floorceil(dt::TimeType, p::Period)
    f = floor(dt, p)
    c, wrapped = ceilfromfloor(dt, f, p)
    wrapped && throw_rounding_overflow(:ceil, dt, p)
    return f, c
end

"""
    floorceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> (T, T)

Simultaneously return the `floor` and `ceil` of a `Period` at resolution `precision`.  More efficient
than calling both `floor` and `ceil` individually.
"""
function floorceil(x::ConvertiblePeriod, precision::ConvertiblePeriod)
    f = floor(x, precision)
    return f, (x == f) ? f : f + precision
end

"""
    round(dt::TimeType, p::Period, [r::RoundingMode]) -> TimeType

Return the `Date`, `DateTime`, or `Timestamp` nearest to `dt` at resolution `p`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 9:30 to the nearest hour) will be rounded up.

For convenience, `p` may be a type instead of a value: `round(dt, Dates.Hour)` is a shortcut
for `round(dt, Dates.Hour(1))`.

```jldoctest
julia> round(Date(1985, 8, 16), Month)
1985-08-01

julia> round(DateTime(2013, 2, 13, 0, 31, 20), Minute(15))
2013-02-13T00:30:00

julia> round(DateTime(2016, 8, 6, 12, 0, 0), Day)
2016-08-07T00:00:00
```

Valid rounding modes for `round(::TimeType, ::Period, ::RoundingMode)` are
`RoundNearestTiesUp` (default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).
"""
# A ceiling past the end of the type throws, even where the floor is nearer
function Base.round(dt::TimeType, p::Period, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(dt, p)
    return (dt - f) < (c - dt) ? f : c
end

"""
    round(x::Period, precision::T, [r::RoundingMode]) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 90 minutes to the nearest hour) will be rounded
up.

For convenience, `precision` may be a type instead of a value: `round(x, Dates.Hour)` is a
shortcut for `round(x, Dates.Hour(1))`.

```jldoctest
julia> round(Day(16), Week)
2 weeks

julia> round(Minute(44), Minute(15))
45 minutes

julia> round(Hour(36), Day)
2 days
```

Valid rounding modes for `round(::Period, ::T, ::RoundingMode)` are `RoundNearestTiesUp`
(default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.round(x::ConvertiblePeriod, precision::ConvertiblePeriod, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(x, precision)
    _x, _f, _c = promote(x, f, c)
    return (_x - _f) < (_c - _x) ? f : c
end

@inline Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Down}) = Base.floor(x, p)
@inline Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Up}) = Base.ceil(x, p)

# No implementation of other `RoundingMode`s: rounding to nearest "even" is skipped because
# "even" is not defined for Period; rounding toward/away from zero is skipped because ISO
# 8601's year 0000 is not really "zero".
Base.round(::TimeTypeOrPeriod, p::Period, ::RoundingMode) = throw(DomainError(p))

# Default to RoundNearestTiesUp. The forwarders inline, so constant periods reach the kernels.
@inline Base.round(x::TimeTypeOrPeriod, p::Period) = Base.round(x, p, RoundNearestTiesUp)

# Make rounding functions callable using Period types in addition to values.
@inline Base.floor(x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.floor(x, oneunit(P))
@inline Base.ceil(x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.ceil(x, oneunit(P))
Base.floor(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.floor(Date(x), oneunit(P))
Base.ceil(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.ceil(Date(x), oneunit(P))

@inline function Base.round(x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period
    return Base.round(x, oneunit(P), r)
end

function Base.round(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period
    return Base.round(Date(x), oneunit(P), r)
end

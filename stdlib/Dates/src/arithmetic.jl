# This file is a part of Julia. License is MIT: https://julialang.org/license

# Instant arithmetic
(+)(x::Instant) = x
(-)(x::T, y::T) where {T<:Instant} = x.periods - y.periods

# TimeType arithmetic
(+)(x::TimeType) = x
(-)(x::T, y::T) where {T<:TimeType} = x.instant - y.instant
(-)(x::T, y::T) where {T<:AbstractDateTime} = x.instant - y.instant
(-)(x::AbstractDateTime, y::AbstractDateTime) = -(promote(x, y)...)

# Date-Time arithmetic
"""
    dt::Date + t::Time -> DateTime

The addition of a `Date` with a `Time` produces a `DateTime`. The hour, minute, second, and millisecond parts of
the `Time` are used along with the year, month, and day of the `Date` to create the new `DateTime`.
Non-zero microseconds or nanoseconds in the `Time` type will result in an `InexactError` being thrown.
"""
(+)(dt::Date, t::Time) = DateTime(dt ,t)
(+)(t::Time, dt::Date) = DateTime(dt, t)

# TimeType-Month arithmetic
# The day count of `dt` moved by `n * unit` months, with the day clamped to the month. It wraps
# around Int64 like fixed periods, so it does not use the constructors, which reject values
# beyond the ends. `totaldays` divides the year, so a month count beyond Int64 is widened.
@inline function addmonths(dt::TimeType, n::Int64, unit::Int64)
    y, m, d = yearmonthday(dt)
    if unit % 12 == 0
        # Whole years keep the month, so they need no division. `totaldays` subtracts one from
        # the year in January and February.
        years, huge = Base.Checked.mul_with_overflow(n, unit ÷ 12)
        ny, wraps = Base.Checked.add_with_overflow(y, years)
        huge | wraps | (ny == typemin(ny)) || return clampeddays(ny, m, d)
    else
        months, huge = Base.Checked.mul_with_overflow(n, unit)
        k, wraps = Base.Checked.add_with_overflow(m - 1, months)
        if !(huge | wraps)
            q, r = fldmod(k, 12)
            # Inlining helps here, but adds branch misses to the year path
            return @inline clampeddays(y + q, r + 1, d)
        end
    end
    return wideaddmonths(y, d, m - 1 + widemul(n, unit))
end
@noinline function wideaddmonths(y, d, k)
    q, r = fldmod(k, 12)
    return clampeddays(y + q, Int64(r) + 1, d)
end
# The day count of day `d` of year `y` and month `m`, clamped to the length of the month
clampeddays(y, m, d) = totaldays(y, m, min(d, daysinmonth(y, m))) % Int64
for (P, unit) in ((Year, 12), (Quarter, 3), (Month, 1)), (op, sign) in ((:+, 1), (:-, -1))
    @eval begin
        ($op)(dt::Date, p::$P) = Date(UTD(addmonths(dt, value(p), $(Int64(sign * unit)))))
        ($op)(dt::DateTime, p::$P) =
            DateTime(UTM(MS_PER_DAY * addmonths(dt, value(p), $(Int64(sign * unit))) + mod(value(dt), MS_PER_DAY)))
    end
end
(+)(x::Date, y::Week) = return Date(UTD(value(x) + 7 * value(y)))
(-)(x::Date, y::Week) = return Date(UTD(value(x) - 7 * value(y)))
(+)(x::Date, y::Day)  = return Date(UTD(value(x) + value(y)))
(-)(x::Date, y::Day)  = return Date(UTD(value(x) - value(y)))
(+)(x::DateTime, y::Period) = return DateTime(UTM(value(x) + toms(y)))
(-)(x::DateTime, y::Period) = return DateTime(UTM(value(x) - toms(y)))
(+)(x::Time, y::TimePeriod) = return Time(Nanosecond(value(x) + tons(y)))
(-)(x::Time, y::TimePeriod) = return Time(Nanosecond(value(x) - tons(y)))
# The period `y` as a count of P and the nanoseconds left over
timestamp_period_divrem(::Type{P}, y::Union{FixedPeriod,TimePeriod}) where {P} =
    divrem(widen(value(y)) * widen(timestamp_period_scale(typeof(y))), timestamp_scale(P))
# The period `y` as a count of P. Throws an InexactError if `y` has more precision than P.
function timestamp_period_ticks(::Type{P}, y::Union{FixedPeriod,TimePeriod}) where {P}
    ticks, remainder = timestamp_period_divrem(P, y)
    iszero(remainder) || throw(InexactError(:convert, P, y))
    return ticks
end
# Arithmetic keeps the resolution P and wraps at the ends of the range, like DateTime
# arithmetic. Calendar periods change the date like Date arithmetic and keep the time
# of day.
for op in (:+, :-)
    @eval begin
        function ($op)(x::Timestamp{P}, y::Union{Year,Quarter,Month}) where {P}
            ticksperday = timestamp_ticks_per_day(P)
            epochdays = value(($op)(Date(x), y)) - UNIXEPOCHDAYS
            return Timestamp{P}(UTInstant(P((epochdays * ticksperday + mod(value(x), ticksperday)) % timestamp_count_type(P))))
        end
        ($op)(x::Timestamp{P}, y::Union{FixedPeriod,TimePeriod}) where {P} =
            Timestamp{P}(UTInstant(P(($op)(value(x), timestamp_period_ticks(P, y)) % timestamp_count_type(P))))
    end
end
(+)(y::Period, x::TimeType) = x + y

# Missing support
(+)(x::AbstractTime, y::Missing) = missing
(+)(x::Missing, y::AbstractTime) = missing
(-)(x::AbstractTime, y::Missing) = missing
(-)(x::Missing, y::AbstractTime) = missing

# AbstractArray{TimeType}, AbstractArray{TimeType}
(-)(x::OrdinalRange{T}, y::OrdinalRange{T}) where {T<:TimeType} = Vector(x) - Vector(y)
(-)(x::AbstractRange{T}, y::AbstractRange{T}) where {T<:TimeType} = Vector(x) - Vector(y)

# Allow dates, times, and time zones to broadcast as unwrapped scalars
Base.Broadcast.broadcastable(x::AbstractTime) = Ref(x)
Base.Broadcast.broadcastable(x::TimeZone) = Ref(x)

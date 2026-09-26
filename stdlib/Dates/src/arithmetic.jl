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

# TimeType-Year arithmetic
function (+)(dt::DateTime, y::Year)
    oy, m, d = yearmonthday(dt); ny = oy + value(y); ld = daysinmonth(ny, m)
    return DateTime(ny, m, d <= ld ? d : ld, hour(dt), minute(dt), second(dt), millisecond(dt))
end
function (+)(dt::Date,y::Year)
    oy, m, d = yearmonthday(dt); ny = oy + value(y); ld = daysinmonth(ny, m)
    return Date(ny, m, d <= ld ? d : ld)
end
function (-)(dt::DateTime,y::Year)
    oy, m, d = yearmonthday(dt); ny = oy - value(y); ld = daysinmonth(ny, m)
    return DateTime(ny, m, d <= ld ? d : ld, hour(dt), minute(dt), second(dt), millisecond(dt))
end
function (-)(dt::Date,y::Year)
    oy, m, d = yearmonthday(dt); ny = oy - value(y); ld = daysinmonth(ny, m)
    return Date(ny, m, d <= ld ? d : ld)
end

# TimeType-Month arithmetic
# monthwrap adds two months with wraparound behavior (i.e. 12 + 1 == 1)
monthwrap(m1, m2) = (v = mod1(m1 + m2, 12); return v < 0 ? 12 + v : v)
# yearwrap takes a starting year/month and a month to add and returns
# the resulting year with wraparound behavior (i.e. 2000-12 + 1 == 2001)
yearwrap(y, m1, m2) = y + fld(m1 + m2 - 1, 12)

function (+)(dt::DateTime, z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y, m, value(z))
    mm = monthwrap(m, value(z)); ld = daysinmonth(ny, mm)
    return DateTime(ny, mm, d <= ld ? d : ld, hour(dt), minute(dt), second(dt), millisecond(dt))
end

function (+)(dt::Date, z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y, m, value(z))
    mm = monthwrap(m, value(z)); ld = daysinmonth(ny, mm)
    return Date(ny, mm, d <= ld ? d : ld)
end
function (-)(dt::DateTime, z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y, m, -value(z))
    mm = monthwrap(m, -value(z)); ld = daysinmonth(ny, mm)
    return DateTime(ny, mm, d <= ld ? d : ld, hour(dt), minute(dt), second(dt), millisecond(dt))
end
function (-)(dt::Date, z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y, m, -value(z))
    mm = monthwrap(m, -value(z)); ld = daysinmonth(ny, mm)
    return Date(ny, mm, d <= ld ? d : ld)
end

(+)(x::Date, y::Quarter) = x + Month(y)
(-)(x::Date, y::Quarter) = x - Month(y)
(+)(x::DateTime, y::Quarter) = x + Month(y)
(-)(x::DateTime, y::Quarter) = x - Month(y)
(+)(x::Date, y::Week) = return Date(UTD(value(x) + 7 * value(y)))
(-)(x::Date, y::Week) = return Date(UTD(value(x) - 7 * value(y)))
(+)(x::Date, y::Day)  = return Date(UTD(value(x) + value(y)))
(-)(x::Date, y::Day)  = return Date(UTD(value(x) - value(y)))
(+)(x::DateTime, y::Period) = return DateTime(UTM(value(x) + toms(y)))
(-)(x::DateTime, y::Period) = return DateTime(UTM(value(x) - toms(y)))
(+)(x::Time, y::TimePeriod) = return Time(Nanosecond(value(x) + tons(y)))
(-)(x::Time, y::TimePeriod) = return Time(Nanosecond(value(x) - tons(y)))
# The period `y` as a count of P. Throws an InexactError if `y` has more precision than P.
function timestamp_period_ticks(::Type{P}, y::Union{FixedPeriod,TimePeriod}) where {P}
    unit, scale = tons(oneunit(y)), timestamp_scale(P)
    unit >= scale && return value(y) * (unit ÷ scale)
    ticks, remainder = divrem(value(y), scale ÷ unit)
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

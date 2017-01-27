# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractTime

"""
    Period
    Year
    Month
    Week
    Day
    Hour
    Minute
    Second
    Millisecond
    Microsecond
    Nanosecond

`Period` types represent discrete, human representations of time.
"""
abstract Period     <: AbstractTime
abstract DatePeriod <: Period
abstract TimePeriod <: Period

for T in (:Year, :Month, :Week, :Day)
    @eval immutable $T <: DatePeriod
        value::Int64
        $T(v::Number) = new(v)
    end
end
for T in (:Hour, :Minute, :Second, :Millisecond, :Microsecond, :Nanosecond)
    @eval immutable $T <: TimePeriod
        value::Int64
        $T(v::Number) = new(v)
    end
end

"""
    Year(v)
    Month(v)
    Week(v)
    Day(v)
    Hour(v)
    Minute(v)
    Second(v)
    Millisecond(v)
    Microsecond(v)
    Nanosecond(v)

Construct a `Period` type with the given `v` value. Input must be losslessly convertible
to an `Int64`.
"""
Period(v)

"""
    Instant

`Instant` types represent integer-based, machine representations of time as continuous
timelines starting from an epoch.
"""
abstract Instant <: AbstractTime

"""
    UTInstant{T}

The `UTInstant` represents a machine timeline based on UT time (1 day = one revolution of
the earth). The `T` is a `Period` parameter that indicates the resolution or precision of
the instant.
"""
immutable UTInstant{P<:Period} <: Instant
    periods::P
end

# Convenience default constructors
UTM(x) = UTInstant(Millisecond(x))
UTD(x) = UTInstant(Day(x))

# Calendar types provide rules for interpretating instant
# timelines in human-readable form.
abstract Calendar <: AbstractTime

# ISOCalendar implements the ISO 8601 standard (en.wikipedia.org/wiki/ISO_8601)
# Notably based on the proleptic Gregorian calendar
# ISOCalendar provides interpretation rules for UTInstants to civil date and time parts
immutable ISOCalendar <: Calendar end

abstract TimeZone
immutable UTC <: TimeZone end

"""
    TimeType

`TimeType` types wrap `Instant` machine instances to provide human representations of the
machine instant. Both `DateTime` and `Date` are subtypes of `TimeType`.
"""
abstract TimeType <: AbstractTime

"""
    DateTime

`DateTime` wraps a `UTInstant{Millisecond}` and interprets it according to the proleptic
Gregorian calendar.
"""
immutable DateTime <: TimeType
    instant::UTInstant{Millisecond}
    DateTime(instant::UTInstant{Millisecond}) = new(instant)
end

"""
    Date

`Date` wraps a `UTInstant{Day}` and interprets it according to the proleptic Gregorian calendar.
"""
immutable Date <: TimeType
    instant::UTInstant{Day}
    Date(instant::UTInstant{Day}) = new(instant)
end

"""
    Time

`Time` wraps a `Nanosecond` and represents a specific moment in a 24-hour day.
"""
immutable Time <: TimeType
    instant::Nanosecond
    Time(instant::Nanosecond) = new(instant)
end

# Convert y,m,d to # of Rata Die days
# Works by shifting the beginning of the year to March 1,
# so a leap day is the very last day of the year
const SHIFTEDMONTHDAYS = (306, 337, 0, 31, 61, 92, 122, 153, 184, 214, 245, 275)
function totaldays(y, m, d)
    # If we're in Jan/Feb, shift the given year back one
    z = m < 3 ? y - 1 : y
    mdays = SHIFTEDMONTHDAYS[m]
    # days + month_days + year_days
    return d + mdays + 365z + fld(z, 4) - fld(z, 100) + fld(z, 400) - 306
end

# If the year is divisible by 4, except for every 100 years, except for every 400 years
isleapyear(y) = ((y % 4 == 0) && (y % 100 != 0)) || (y % 400 == 0)

# Number of days in month
const DAYSINMONTH = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daysinmonth(y,m) = DAYSINMONTH[m] + (m == 2 && isleapyear(y))

### CONSTRUCTORS ###
# Core constructors
"""
    DateTime(y, [m, d, h, mi, s, ms]) -> DateTime

Construct a `DateTime` type by parts. Arguments must be convertible to `Int64`.
"""
function DateTime(y::Int64, m::Int64=1, d::Int64=1,
                  h::Int64=0, mi::Int64=0, s::Int64=0, ms::Int64=0)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y, m) + 1 || throw(ArgumentError("Day: $d out of range (1:$(daysinmonth(y, m)))"))
    -1 < h < 24 || throw(ArgumentError("Hour: $h out of range (0:23)"))
    -1 < mi < 60 || throw(ArgumentError("Minute: $mi out of range (0:59)"))
    -1 < s < 60 || throw(ArgumentError("Second: $s out of range (0:59)"))
    -1 < ms < 1000 || throw(ArgumentError("Millisecond: $ms out of range (0:999)"))
    rata = ms + 1000 * (s + 60mi + 3600h + 86400 * totaldays(y, m, d))
    return DateTime(UTM(rata))
end

"""
    Date(y, [m, d]) -> Date

Construct a `Date` type by parts. Arguments must be convertible to `Int64`.
"""
function Date(y::Int64, m::Int64=1, d::Int64=1)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y, m) + 1 || throw(ArgumentError("Day: $d out of range (1:$(daysinmonth(y, m)))"))
    return Date(UTD(totaldays(y, m, d)))
end

"""
    Time(h, [mi, s, ms, us, ns]) -> Time

Construct a `Time` type by parts. Arguments must be convertible to `Int64`.
"""
function Time(h::Int64, mi::Int64=0, s::Int64=0, ms::Int64=0, us::Int64=0, ns::Int64=0)
    -1 < h < 24 || throw(ArgumentError("Hour: $h out of range (0:23)"))
    -1 < mi < 60 || throw(ArgumentError("Minute: $mi out of range (0:59)"))
    -1 < s < 60 || throw(ArgumentError("Second: $s out of range (0:59)"))
    -1 < ms < 1000 || throw(ArgumentError("Millisecond: $ms out of range (0:999)"))
    -1 < us < 1000 || throw(ArgumentError("Microsecond: $us out of range (0:999)"))
    -1 < ns < 1000 || throw(ArgumentError("Nanosecond: $ns out of range (0:999)"))
    return Time(Nanosecond(ns + 1000us + 1000000ms + 1000000000s + 60000000000mi + 3600000000000h))
end

# Convenience constructors from Periods
function DateTime(y::Year, m::Month=Month(1), d::Day=Day(1),
                  h::Hour=Hour(0), mi::Minute=Minute(0),
                  s::Second=Second(0), ms::Millisecond=Millisecond(0))
    return DateTime(value(y), value(m), value(d),
                        value(h), value(mi), value(s), value(ms))
end

Date(y::Year, m::Month=Month(1), d::Day=Day(1)) = Date(value(y), value(m), value(d))

function Time(h::Hour, mi::Minute=Minute(0), s::Second=Second(0),
              ms::Millisecond=Millisecond(0),
              us::Microsecond=Microsecond(0), ns::Nanosecond=Nanosecond(0))
    return Time(value(h), value(mi), value(s), value(ms), value(us), value(ns))
end

# To allow any order/combination of Periods

"""
    DateTime(periods::Period...) -> DateTime

Construct a `DateTime` type by `Period` type parts. Arguments may be in any order. DateTime
parts not provided will default to the value of `Dates.default(period)`.
"""
function DateTime(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0); ms = Millisecond(0)
    for p in periods
        isa(p, Year) && (y = p::Year)
        isa(p, Month) && (m = p::Month)
        isa(p, Day) && (d = p::Day)
        isa(p, Hour) && (h = p::Hour)
        isa(p, Minute) && (mi = p::Minute)
        isa(p, Second) && (s = p::Second)
        isa(p, Millisecond) && (ms = p::Millisecond)
    end
    return DateTime(y, m, d, h, mi, s, ms)
end

"""
    Date(period::Period...) -> Date

Construct a `Date` type by `Period` type parts. Arguments may be in any order. `Date` parts
not provided will default to the value of `Dates.default(period)`.
"""
function Date(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    for p in periods
        isa(p, Year) && (y = p::Year)
        isa(p, Month) && (m = p::Month)
        isa(p, Day) && (d = p::Day)
    end
    return Date(y, m, d)
end

"""
    Time(period::TimePeriod...) -> Time

Construct a `Time` type by `Period` type parts. Arguments may be in any order. `Time` parts
not provided will default to the value of `Dates.default(period)`.
"""
function Time(periods::TimePeriod...)
    h = Hour(0); mi = Minute(0); s = Second(0)
    ms = Millisecond(0); us = Microsecond(0); ns = Nanosecond(0)
    for p in periods
        isa(p, Hour) && (h = p::Hour)
        isa(p, Minute) && (mi = p::Minute)
        isa(p, Second) && (s = p::Second)
        isa(p, Millisecond) && (ms = p::Millisecond)
        isa(p, Microsecond) && (us = p::Microsecond)
        isa(p, Nanosecond) && (ns = p::Nanosecond)
    end
    return Time(h, mi, s, ms, us, ns)
end

# Fallback constructors
DateTime(y, m=1, d=1, h=0, mi=0, s=0, ms=0) = DateTime(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms))
Date(y, m=1, d=1) = Date(Int64(y), Int64(m), Int64(d))
Time(h, mi=0, s=0, ms=0, us=0, ns=0) = Time(Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns))

# Traits, Equality
Base.isfinite{T<:TimeType}(::Union{Type{T}, T}) = true
calendar(dt::DateTime) = ISOCalendar
calendar(dt::Date) = ISOCalendar

"""
    eps(::DateTime) -> Millisecond
    eps(::Date) -> Day
    eps(::Time) -> Nanosecond

Returns `Millisecond(1)` for `DateTime` values, `Day(1)` for `Date` values, and `Nanosecond(1)` for `Time` values.
"""
Base.eps

Base.eps(dt::DateTime) = Millisecond(1)
Base.eps(dt::Date) = Day(1)
Base.eps(t::Time) = Nanosecond(1)

Base.typemax(::Union{DateTime, Type{DateTime}}) = DateTime(146138512, 12, 31, 23, 59, 59)
Base.typemin(::Union{DateTime, Type{DateTime}}) = DateTime(-146138511, 1, 1, 0, 0, 0)
Base.typemax(::Union{Date, Type{Date}}) = Date(252522163911149, 12, 31)
Base.typemin(::Union{Date, Type{Date}}) = Date(-252522163911150, 1, 1)
Base.typemax(::Union{Time, Type{Time}}) = Time(23, 59, 59, 999, 999, 999)
Base.typemin(::Union{Time, Type{Time}}) = Time(0)
# Date-DateTime promotion, isless, ==
Base.eltype{T<:Period}(::Type{T}) = T
Base.promote_rule(::Type{Date}, x::Type{DateTime}) = DateTime
Base.isless{T<:TimeType}(x::T, y::T) = isless(value(x), value(y))
Base.isless(x::TimeType, y::TimeType) = isless(Base.promote_noncircular(x, y)...)
=={T<:TimeType}(x::T, y::T) = ==(value(x), value(y))
function ==(a::Time, b::Time)
    return hour(a) == hour(b) && minute(a) == minute(b) &&
        second(a) == second(b) && millisecond(a) == millisecond(b) &&
        microsecond(a) == microsecond(b) && nanosecond(a) == nanosecond(b)
end
==(x::TimeType, y::TimeType) = ===(promote(x, y)...)

import Base: sleep, Timer, timedwait
sleep(time::Period) = sleep(toms(time) / 1000)
Timer(time::Period, repeat::Period=Second(0)) = Timer(toms(time) / 1000, toms(repeat) / 1000)
timedwait(testcb::Function, time::Period) = timedwait(testcb, toms(time) / 1000)

(::Type{Base.TypeOrder}){T<:AbstractTime}(::Type{T}) = Base.HasOrder()
(::Type{Base.TypeArithmetic}){T<:AbstractTime}(::Type{T}) = Base.ArithmeticOverflows()

# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractTime end

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
abstract type Period     <: AbstractTime end
abstract type DatePeriod <: Period end
abstract type TimePeriod <: Period end

for T in (:Year, :Month, :Week, :Day)
    @eval struct $T <: DatePeriod
        value::Int64
        $T(v::Number) = new(v)
    end
end
for T in (:Hour, :Minute, :Second, :Millisecond, :Microsecond, :Nanosecond)
    @eval struct $T <: TimePeriod
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
to an [`Int64`](@ref).
"""
Period(v)

"""
    Instant

`Instant` types represent integer-based, machine representations of time as continuous
timelines starting from an epoch.
"""
abstract type Instant <: AbstractTime end

"""
    UTInstant{T}

The `UTInstant` represents a machine timeline based on UT time (1 day = one revolution of
the earth). The `T` is a `Period` parameter that indicates the resolution or precision of
the instant.
"""
struct UTInstant{P<:Period} <: Instant
    periods::P
end

# Convenience default constructors
UTM(x) = UTInstant(Millisecond(x))
UTD(x) = UTInstant(Day(x))

# Calendar types provide rules for interpretating instant
# timelines in human-readable form.
abstract type Calendar <: AbstractTime end

# ISOCalendar implements the ISO 8601 standard (en.wikipedia.org/wiki/ISO_8601)
# Notably based on the proleptic Gregorian calendar
# ISOCalendar provides interpretation rules for UTInstants to civil date and time parts
struct ISOCalendar <: Calendar end

abstract type TimeZone end
struct UTC <: TimeZone end

"""
    TimeType

`TimeType` types wrap `Instant` machine instances to provide human representations of the
machine instant. `Time`, `DateTime` and `Date` are subtypes of `TimeType`.
"""
abstract type TimeType <: AbstractTime end

abstract type AbstractDateTime <: TimeType end

"""
    DateTime

`DateTime` wraps a `UTInstant{Millisecond}` and interprets it according to the proleptic
Gregorian calendar.
"""
struct DateTime <: AbstractDateTime
    instant::UTInstant{Millisecond}
    DateTime(instant::UTInstant{Millisecond}) = new(instant)
end

"""
    Date

`Date` wraps a `UTInstant{Day}` and interprets it according to the proleptic Gregorian calendar.
"""
struct Date <: TimeType
    instant::UTInstant{Day}
    Date(instant::UTInstant{Day}) = new(instant)
end

"""
    Time

`Time` wraps a `Nanosecond` and represents a specific moment in a 24-hour day.
"""
struct Time <: TimeType
    instant::Nanosecond
    Time(instant::Nanosecond) = new(mod(instant, 86400000000000))
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

### UTILITIES ###

# These are necessary because the type constructors for TimeType subtypes can
# throw, and we want to be able to use tryparse without requiring a try/catch.
# This is made easier by providing a helper function that checks arguments, so
# we can validate arguments in tryparse.

"""
    validargs(::Type{<:TimeType}, args...) -> Union{ArgumentError, Nothing}

Determine whether the given arguments consitute valid inputs for the given type.
Returns either an `ArgumentError`, or [`nothing`](@ref) in case of success.
"""
function validargs end

"""
    argerror([msg]) -> Union{ArgumentError, Nothing}

Return an `ArgumentError` object with the given message,
or [`nothing`](@ref) if no message is provided. For use by `validargs`.
"""
argerror(msg::String) = ArgumentError(msg)
argerror() = nothing

# Julia uses 24-hour clocks internally, but user input can be AM/PM with 12pm == noon and 12am == midnight.
@enum AMPM AM PM TWENTYFOURHOUR
function adjusthour(h::Int64, ampm::AMPM)
    ampm == TWENTYFOURHOUR && return h
    ampm == PM && h < 12 && return h + 12
    ampm == AM && h == 12 && return Int64(0)
    return h
end

### CONSTRUCTORS ###
# Core constructors
"""
    DateTime(y, [m, d, h, mi, s, ms]) -> DateTime

Construct a `DateTime` type by parts. Arguments must be convertible to [`Int64`](@ref).
"""
function DateTime(y::Int64, m::Int64=1, d::Int64=1,
                  h::Int64=0, mi::Int64=0, s::Int64=0, ms::Int64=0, ampm::AMPM=TWENTYFOURHOUR)
    err = validargs(DateTime, y, m, d, h, mi, s, ms, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    rata = ms + 1000 * (s + 60mi + 3600h + 86400 * totaldays(y, m, d))
    return DateTime(UTM(rata))
end

function validargs(::Type{DateTime}, y::Int64, m::Int64, d::Int64,
                   h::Int64, mi::Int64, s::Int64, ms::Int64, ampm::AMPM=TWENTYFOURHOUR)
    0 < m < 13 || return argerror("Month: $m out of range (1:12)")
    0 < d < daysinmonth(y, m) + 1 || return argerror("Day: $d out of range (1:$(daysinmonth(y, m)))")
    if ampm == TWENTYFOURHOUR # 24-hour clock
        -1 < h < 24 || (h == 24 && mi==s==ms==0) ||
            return argerror("Hour: $h out of range (0:23)")
    else
        0 < h < 13 || return argerror("Hour: $h out of range (1:12)")
    end
    -1 < mi < 60 || return argerror("Minute: $mi out of range (0:59)")
    -1 < s < 60 || return argerror("Second: $s out of range (0:59)")
    -1 < ms < 1000 || return argerror("Millisecond: $ms out of range (0:999)")
    return argerror()
end

DateTime(dt::Base.Libc.TmStruct) = DateTime(1900 + dt.year, 1 + dt.month, dt.mday, dt.hour, dt.min, dt.sec)

"""
    Date(y, [m, d]) -> Date

Construct a `Date` type by parts. Arguments must be convertible to [`Int64`](@ref).
"""
function Date(y::Int64, m::Int64=1, d::Int64=1)
    err = validargs(Date, y, m, d)
    err === nothing || throw(err)
    return Date(UTD(totaldays(y, m, d)))
end

function validargs(::Type{Date}, y::Int64, m::Int64, d::Int64)
    0 < m < 13 || return argerror("Month: $m out of range (1:12)")
    0 < d < daysinmonth(y, m) + 1 || return argerror("Day: $d out of range (1:$(daysinmonth(y, m)))")
    return argerror()
end

Date(dt::Base.Libc.TmStruct) = Date(1900 + dt.year, 1 + dt.month, dt.mday)

"""
    Time(h, [mi, s, ms, us, ns]) -> Time

Construct a `Time` type by parts. Arguments must be convertible to [`Int64`](@ref).
"""
function Time(h::Int64, mi::Int64=0, s::Int64=0, ms::Int64=0, us::Int64=0, ns::Int64=0, ampm::AMPM=TWENTYFOURHOUR)
    err = validargs(Time, h, mi, s, ms, us, ns, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    return Time(Nanosecond(ns + 1000us + 1000000ms + 1000000000s + 60000000000mi + 3600000000000h))
end

function validargs(::Type{Time}, h::Int64, mi::Int64, s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM=TWENTYFOURHOUR)
    if ampm == TWENTYFOURHOUR # 24-hour clock
        -1 < h < 24 || return argerror("Hour: $h out of range (0:23)")
    else
        0 < h < 13 || return argerror("Hour: $h out of range (1:12)")
    end
    -1 < mi < 60 || return argerror("Minute: $mi out of range (0:59)")
    -1 < s < 60 || return argerror("Second: $s out of range (0:59)")
    -1 < ms < 1000 || return argerror("Millisecond: $ms out of range (0:999)")
    -1 < us < 1000 || return argerror("Microsecond: $us out of range (0:999)")
    -1 < ns < 1000 || return argerror("Nanosecond: $ns out of range (0:999)")
    return argerror()
end

Time(dt::Base.Libc.TmStruct) = Time(dt.hour, dt.min, dt.sec)

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
function DateTime(period::Period, periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0); ms = Millisecond(0)
    for p in (period, periods...)
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
function Date(period::Period, periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    for p in (period, periods...)
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
function Time(period::TimePeriod, periods::TimePeriod...)
    h = Hour(0); mi = Minute(0); s = Second(0)
    ms = Millisecond(0); us = Microsecond(0); ns = Nanosecond(0)
    for p in (period, periods...)
        isa(p, Hour) && (h = p::Hour)
        isa(p, Minute) && (mi = p::Minute)
        isa(p, Second) && (s = p::Second)
        isa(p, Millisecond) && (ms = p::Millisecond)
        isa(p, Microsecond) && (us = p::Microsecond)
        isa(p, Nanosecond) && (ns = p::Nanosecond)
    end
    return Time(h, mi, s, ms, us, ns)
end

# Convenience constructor for DateTime from Date and Time
"""
    DateTime(d::Date, t::Time)

Construct a `DateTime` type by `Date` and `Time`.
Non-zero microseconds or nanoseconds in the `Time` type will result in an
`InexactError`.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

```jldoctest
julia> d = Date(2018, 1, 1)
2018-01-01

julia> t = Time(8, 15, 42)
08:15:42

julia> DateTime(d, t)
2018-01-01T08:15:42
```
"""
function DateTime(dt::Date, t::Time)
    (microsecond(t) > 0 || nanosecond(t) > 0) && throw(InexactError(:DateTime, DateTime, t))
    y, m, d = yearmonthday(dt)
    return DateTime(y, m, d, hour(t), minute(t), second(t), millisecond(t))
end

# Fallback constructors
DateTime(y, m=1, d=1, h=0, mi=0, s=0, ms=0, ampm::AMPM=TWENTYFOURHOUR) = DateTime(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms), ampm)
Date(y, m=1, d=1) = Date(Int64(y), Int64(m), Int64(d))
Time(h, mi=0, s=0, ms=0, us=0, ns=0, ampm::AMPM=TWENTYFOURHOUR) = Time(Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns), ampm)

# Traits, Equality
Base.isfinite(::Union{Type{T}, T}) where {T<:TimeType} = true
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
Base.promote_rule(::Type{Date}, x::Type{DateTime}) = DateTime
Base.isless(x::T, y::T) where {T<:TimeType} = isless(value(x), value(y))
Base.isless(x::TimeType, y::TimeType) = isless(promote(x, y)...)
(==)(x::T, y::T) where {T<:TimeType} = (==)(value(x), value(y))
(==)(x::TimeType, y::TimeType) = (===)(promote(x, y)...)
Base.min(x::AbstractTime) = x
Base.max(x::AbstractTime) = x
Base.minmax(x::AbstractTime) = (x, x)
Base.hash(x::Time, h::UInt) =
    hash(hour(x), hash(minute(x), hash(second(x),
        hash(millisecond(x), hash(microsecond(x), hash(nanosecond(x), h))))))

import Base: sleep, Timer, timedwait
sleep(time::Period) = sleep(toms(time) / 1000)
Timer(time::Period; interval::Period = Second(0)) =
    Timer(toms(time) / 1000, interval = toms(interval) / 1000)
timedwait(testcb::Function, time::Period) = timedwait(testcb, toms(time) / 1000)

Base.OrderStyle(::Type{<:AbstractTime}) = Base.Ordered()
Base.ArithmeticStyle(::Type{<:AbstractTime}) = Base.ArithmeticWraps()

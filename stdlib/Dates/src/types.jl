# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractTime end

"""
    Period
    Year
    Quarter
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

"""
    DatePeriod
    Year
    Quarter
    Month
    Week
    Day

Intervals of time greater than or equal to a day.
Conventional comparisons between `DatePeriod`s are not all valid.
(eg `Week(1) == Day(7)`, but `Year(1) != Day(365)`)
"""
abstract type DatePeriod <: Period end

"""
    TimePeriod
    Hour
    Minute
    Second
    Millisecond
    Microsecond
    Nanosecond

Intervals of time less than a day.
Conversions between all `TimePeriod`s are permissible.
(eg `Hour(1) == Minute(60) == Second(3600)`)
"""
abstract type TimePeriod <: Period end

for T in (:Year, :Quarter, :Month, :Week, :Day)
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
    Quarter(v)
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
the instant. The enclosing `TimeType` determines the epoch: `Date` and `DateTime`
use the Rata Die epoch, while `Timestamp` uses the Unix epoch.
"""
struct UTInstant{P<:Period} <: Instant
    periods::P
end

# Convenience default constructors
UTM(x) = UTInstant(Millisecond(x))
UTD(x) = UTInstant(Day(x))
UTN(x) = UTInstant(Nanosecond(x))

# Calendar types provide rules for interpreting instant
# timelines in human-readable form.
abstract type Calendar <: AbstractTime end

# ISOCalendar implements the ISO 8601 standard (en.wikipedia.org/wiki/ISO_8601)
# Notably based on the proleptic Gregorian calendar
# ISOCalendar provides interpretation rules for UTInstants to civil date and time parts
struct ISOCalendar <: Calendar end

"""
    TimeZone

Geographic zone generally based on longitude determining what the time is at a certain location.
Some time zones observe daylight savings (eg EST -> EDT).
For implementations and more support, see the [`TimeZones.jl`](https://github.com/JuliaTime/TimeZones.jl) package
"""
abstract type TimeZone end

"""
    UTC

`UTC`, or Coordinated Universal Time, is the [`TimeZone`](@ref) from which all others are measured.
It is associated with the time at 0° longitude. It is not adjusted for daylight savings.
"""
struct UTC <: TimeZone end

"""
    TimeType

`TimeType` types wrap `Instant` machine instances to provide human representations of the
machine instant. `Time`, `DateTime`, `Timestamp`, and `Date` are subtypes of `TimeType`.
"""
abstract type TimeType <: AbstractTime end

abstract type AbstractDateTime <: TimeType end

"""
    DateTime

`DateTime` represents a point in time according to the proleptic Gregorian calendar.
The finest resolution of the time is millisecond (i.e., microseconds or
nanoseconds cannot be represented by this type). The type supports fixed-point
arithmetic, and thus is prone to underflowing (and overflowing). A notable
consequence is rounding when adding a `Microsecond` or a `Nanosecond`:

```jldoctest
julia> dt = DateTime(2023, 8, 19, 17, 45, 32, 900)
2023-08-19T17:45:32.900

julia> dt + Millisecond(1)
2023-08-19T17:45:32.901

julia> dt + Microsecond(1000) # 1000us == 1ms
2023-08-19T17:45:32.901

julia> dt + Microsecond(999) # 999us rounded to 1000us
2023-08-19T17:45:32.901

julia> dt + Microsecond(1499) # 1499 rounded to 1000us
2023-08-19T17:45:32.901
```
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

"""
    Timestamp{P}

`Timestamp` represents a point in time according to the proleptic Gregorian
calendar with resolution `P`, one of `Second`, `Millisecond`, `Microsecond`,
or `Nanosecond`. Its value is an `Int64` count of units of `P` since the Unix
epoch, `1970-01-01T00:00:00`. `Timestamp(args...)` defaults to `Timestamp{Nanosecond}`;
`Timestamp(ts::Timestamp)` preserves the input's resolution. Use a concrete
`Timestamp{P}` for array elements and struct fields of a known resolution.

Nanosecond resolution in 64 bits bounds the representable range to
`1677-09-21T00:12:43.145224192` through `2262-04-11T23:47:16.854775807`
(`typemin(Timestamp)` and `typemax(Timestamp)`); constructing a `Timestamp`
outside this range throws an `ArgumentError`. Like `DateTime`, the type uses
fixed-point arithmetic and is thus prone to underflowing and overflowing:
adding a period that would leave the representable range wraps around rather
than throwing. In particular, `typemax(Timestamp{P}) + P(1)` is
`typemin(Timestamp{P})`.

Coarser resolutions cover approximately ±292 billion years (`Second`),
±292 million years (`Millisecond`), or ±292 thousand years (`Microsecond`).
`DateTime` keeps its existing representation and API. Unlike `DateTime`,
`Timestamp{Millisecond}` counts from the Unix epoch.

Constructors, parsing, and conversions between resolutions require exact
representation. Use `floor`, `ceil`, or `round` before converting to a coarser
resolution to discard precision explicitly. Period arithmetic preserves `P`
and requires a duration representable in units of `P`. Unlike `DateTime`, it
throws an `InexactError` instead of rounding a finer duration. Rounding throws an
`InexactError` if the requested result is not representable; it does not wrap.

The types compare directly, even outside their shared range. Mixed arithmetic
promotes to the finer resolution, so both values must fit that resolution.
For promotion with `DateTime`, its resolution is `Millisecond`:

```jldoctest
julia> ts = Timestamp(2020, 1, 1, 0, 0, 0, 1, 0, 500)
2020-01-01T00:00:00.0010005

julia> ts - DateTime(2020, 1, 1)
1000500 nanoseconds

julia> DateTime(ts) # floors to millisecond resolution
2020-01-01T00:00:00.001
```

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.

# Extended help

The representation is the same as Apache Arrow's `timestamp[ns]`, numpy's
`datetime64[ns]`, and pandas timestamps, so nanosecond-timestamp value buffers
from those systems can be reinterpreted as `Timestamp{Nanosecond}`s directly. Note that
numpy reserves `typemin(Int64)` as its `NaT` sentinel, so the pandas range
begins one nanosecond after `typemin(Timestamp)`.

`Dates.value(ts)` is the raw count since the Unix epoch. `convert(P, ts)`
returns that count as a period in units of `P`, requiring exact conversion;
`P(ts)` instead returns the corresponding calendar component. These raw counts
differ from the Rata Die milliseconds returned by `convert(Millisecond, dt)`
for a `DateTime`, even when `ts == dt`.
"""
struct Timestamp{P<:Union{Second,Millisecond,Microsecond,Nanosecond}} <: AbstractDateTime
    instant::UTInstant{P}
    Timestamp{P}(instant::UTInstant{P}) where {P} = new{P}(instant)
end

Timestamp(args...; kwargs...) = Timestamp{Nanosecond}(args...; kwargs...)
Timestamp(instant::UTInstant{P}) where {P} = Timestamp{P}(instant)
Timestamp(ts::Timestamp) = ts

timestamp_scale(::Type{Second}) = Int64(1000000000)
timestamp_scale(::Type{Millisecond}) = Int64(1000000)
timestamp_scale(::Type{Microsecond}) = Int64(1000)
timestamp_scale(::Type{Nanosecond}) = Int64(1)
timestamp_scale(::Type{Timestamp{P}}) where {P} = timestamp_scale(P)
timestamp_ticks_per_day(::Type{P}) where {P} = NS_PER_DAY ÷ timestamp_scale(P)
timestamp_finer(::Type{P}, ::Type{Q}) where {P,Q} =
    timestamp_scale(P) <= timestamp_scale(Q) ? P : Q

function timestamp_ticks(::Type{P}, ns::Integer) where {P}
    ticks, remainder = divrem(ns, timestamp_scale(P))
    iszero(remainder) || throw(InexactError(:convert, Timestamp{P}, ns))
    return ticks
end

timestamp_from_day(::Type{Timestamp{P}}, rata, ns) where {P} =
    Timestamp{P}(UTInstant(P(timestamp_ticks(P, (Int128(rata) - UNIXEPOCHDAYS) * NS_PER_DAY + ns))))


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

# Timestamp instants count units of P from the Unix epoch.
const NS_PER_DAY = 86400000000000
const UNIXEPOCHDAYS = totaldays(1970, 1, 1)

# If the year is divisible by 4, except for every 100 years, except for every 400 years
isleapyear(y::Integer) = (y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0))

# Number of days in month
const DAYSINMONTH = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daysinmonth(y,m) = DAYSINMONTH[m] + (m == 2 && isleapyear(y))

### UTILITIES ###

# These are necessary because the type constructors for TimeType subtypes can
# throw, and we want to be able to use tryparse without requiring a try/catch.
# This is made easier by providing a helper function that checks arguments, so
# we can validate arguments in tryparse.

"""
    validargs(::Type{<:TimeType}, args...)::Union{ArgumentError, Nothing}

Determine whether the given arguments constitute valid inputs for the given type.
Returns either an `ArgumentError`, or [`nothing`](@ref) in case of success.
"""
function validargs end

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
    DateTime(y, [m, d, h, mi, s, ms])::DateTime

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
    0 < m < 13 || return ArgumentError("Month: $m out of range (1:12)")
    0 < d < daysinmonth(y, m) + 1 || return ArgumentError("Day: $d out of range (1:$(daysinmonth(y, m)))")
    if ampm == TWENTYFOURHOUR # 24-hour clock
        -1 < h < 24 || (h == 24 && mi==s==ms==0) ||
            return ArgumentError("Hour: $h out of range (0:23)")
    else
        0 < h < 13 || return ArgumentError("Hour: $h out of range (1:12)")
    end
    -1 < mi < 60 || return ArgumentError("Minute: $mi out of range (0:59)")
    -1 < s < 60 || return ArgumentError("Second: $s out of range (0:59)")
    -1 < ms < 1000 || return ArgumentError("Millisecond: $ms out of range (0:999)")
    return nothing
end

DateTime(dt::Base.Libc.TmStruct) = DateTime(1900 + dt.year, 1 + dt.month, dt.mday, dt.hour, dt.min, dt.sec)

"""
    Date(y, [m, d])::Date

Construct a `Date` type by parts. Arguments must be convertible to [`Int64`](@ref).
"""
function Date(y::Int64, m::Int64=1, d::Int64=1)
    err = validargs(Date, y, m, d)
    err === nothing || throw(err)
    return Date(UTD(totaldays(y, m, d)))
end

function validargs(::Type{Date}, y::Int64, m::Int64, d::Int64)
    0 < m < 13 || return ArgumentError("Month: $m out of range (1:12)")
    0 < d < daysinmonth(y, m) + 1 || return ArgumentError("Day: $d out of range (1:$(daysinmonth(y, m)))")
    return nothing
end

Date(dt::Base.Libc.TmStruct) = Date(1900 + dt.year, 1 + dt.month, dt.mday)

"""
    Time(h, [mi, s, ms, us, ns])::Time

Construct a `Time` type by parts. Arguments must be convertible to [`Int64`](@ref).
The `ns` argument can contain a full fractional second from `0` through
`999999999`. The combined `ms`, `us`, and `ns` arguments must be less than one
second.

!!! compat "Julia 1.14"
    Support for a full fractional second in `ns` requires Julia 1.14 or later.
"""
function Time(h::Int64, mi::Int64=0, s::Int64=0, ms::Int64=0, us::Int64=0, ns::Int64=0, ampm::AMPM=TWENTYFOURHOUR)
    err = validargs(Time, h, mi, s, ms, us, ns, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    return Time(Nanosecond(ns + 1000us + 1000000ms + 1000000000s + 60000000000mi + 3600000000000h))
end

function validargs(::Type{Time}, h::Int64, mi::Int64, s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM=TWENTYFOURHOUR)
    if ampm == TWENTYFOURHOUR # 24-hour clock
        -1 < h < 24 || return ArgumentError("Hour: $h out of range (0:23)")
    else
        0 < h < 13 || return ArgumentError("Hour: $h out of range (1:12)")
    end
    -1 < mi < 60 || return ArgumentError("Minute: $mi out of range (0:59)")
    -1 < s < 60 || return ArgumentError("Second: $s out of range (0:59)")
    -1 < ms < 1000 || return ArgumentError("Millisecond: $ms out of range (0:999)")
    -1 < us < 1000 || return ArgumentError("Microsecond: $us out of range (0:999)")
    # ns may carry a full fractional second (e.g. from parsing ".123456789" with
    # the `n` format code) as long as the parts together stay below one second
    -1 < ns < 1000000000 || return ArgumentError("Nanosecond: $ns out of range (0:999999999)")
    1000000ms + 1000us + ns < 1000000000 ||
        return ArgumentError("Sub-second parts must together be less than one second")
    return nothing
end

Time(dt::Base.Libc.TmStruct) = Time(dt.hour, dt.min, dt.sec)

"""
    Timestamp{P}(y, [m, d, h, mi, s, ms, us, ns])::Timestamp{P}

Construct a `Timestamp` type by parts. Arguments must be convertible to
[`Int64`](@ref) and the result must lie within the representable range
(`typemin(Timestamp{P})` to `typemax(Timestamp{P})`) at resolution `P`.
The `ns` argument can contain a full fractional second, but the
combined `ms`, `us`, and `ns` arguments must be less than one second.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
function Timestamp{P}(y::Int64, m::Int64=1, d::Int64=1, h::Int64=0, mi::Int64=0, s::Int64=0,
                   ms::Int64=0, us::Int64=0, ns::Int64=0, ampm::AMPM=TWENTYFOURHOUR) where {P}
    err = validargs(Timestamp{P}, y, m, d, h, mi, s, ms, us, ns, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600h)
    return timestamp_from_day(Timestamp{P}, totaldays(y, m, d), nsofday)
end

function validargs(::Type{Timestamp{P}}, y::Int64, m::Int64, d::Int64, h::Int64, mi::Int64,
                   s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM=TWENTYFOURHOUR) where {P}
    year(typemin(Timestamp{P})) <= y <= year(typemax(Timestamp{P})) ||
        return ArgumentError("Year: $y out of range for Timestamp{$P}")
    0 < m < 13 || return ArgumentError("Month: $m out of range (1:12)")
    0 < d < daysinmonth(y, m) + 1 || return ArgumentError("Day: $d out of range (1:$(daysinmonth(y, m)))")
    if ampm == TWENTYFOURHOUR # 24-hour clock
        -1 < h < 24 || (h == 24 && mi==s==ms==us==ns==0) ||
            return ArgumentError("Hour: $h out of range (0:23)")
    else
        0 < h < 13 || return ArgumentError("Hour: $h out of range (1:12)")
    end
    -1 < mi < 60 || return ArgumentError("Minute: $mi out of range (0:59)")
    -1 < s < 60 || return ArgumentError("Second: $s out of range (0:59)")
    -1 < ms < 1000 || return ArgumentError("Millisecond: $ms out of range (0:999)")
    -1 < us < 1000 || return ArgumentError("Microsecond: $us out of range (0:999)")
    -1 < ns < 1000000000 || return ArgumentError("Nanosecond: $ns out of range (0:999999999)")
    1000000ms + 1000us + ns < 1000000000 ||
        return ArgumentError("Sub-second parts must together be less than one second")
    epochdays = totaldays(y, m, d) - UNIXEPOCHDAYS
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600 * adjusthour(h, ampm))
    ticks, remainder = divrem(nsofday, timestamp_scale(P))
    iszero(remainder) || return ArgumentError("Fractional second is not exactly representable as Timestamp{$P}")
    fldmod(typemin(Int64), timestamp_ticks_per_day(P)) <= (epochdays, ticks) <=
        fldmod(typemax(Int64), timestamp_ticks_per_day(P)) ||
        return ArgumentError("Timestamp: $y-$m-$d out of range ($(typemin(Timestamp{P})) to $(typemax(Timestamp{P})))")
    return nothing
end

validargs(::Type{Timestamp}, args...) = validargs(Timestamp{Nanosecond}, args...)

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

function Timestamp{P}(y::Year, m::Month=Month(1), d::Day=Day(1),
                   h::Hour=Hour(0), mi::Minute=Minute(0), s::Second=Second(0),
                   ms::Millisecond=Millisecond(0),
                   us::Microsecond=Microsecond(0), ns::Nanosecond=Nanosecond(0)) where {P}
    return Timestamp{P}(value(y), value(m), value(d), value(h), value(mi), value(s),
                     value(ms), value(us), value(ns))
end

# To allow any order/combination of Periods

"""
    DateTime(periods::Period...)::DateTime

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
    Date(period::Period...)::Date

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
    Time(period::TimePeriod...)::Time

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

"""
    Timestamp(periods::Period...)::Timestamp

Construct a `Timestamp` type by `Period` type parts. Arguments may be in any order.
`Timestamp` parts not provided will default to the unix epoch, `1970-01-01T00:00:00`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
function Timestamp{P}(period::Period, periods::Period...) where {P}
    y = Year(1970); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0)
    ms = Millisecond(0); us = Microsecond(0); ns = Nanosecond(0)
    for p in (period, periods...)
        isa(p, Year) && (y = p::Year)
        isa(p, Month) && (m = p::Month)
        isa(p, Day) && (d = p::Day)
        isa(p, Hour) && (h = p::Hour)
        isa(p, Minute) && (mi = p::Minute)
        isa(p, Second) && (s = p::Second)
        isa(p, Millisecond) && (ms = p::Millisecond)
        isa(p, Microsecond) && (us = p::Microsecond)
        isa(p, Nanosecond) && (ns = p::Nanosecond)
    end
    return Timestamp{P}(y, m, d, h, mi, s, ms, us, ns)
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

"""
    Timestamp(d::Date, [t::Time])::Timestamp

Construct a `Timestamp` from a `Date` and, optionally, a `Time` giving the
nanosecond-resolution time of day. Throws an `ArgumentError` if the result lies
outside the representable `Timestamp` range.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.

```jldoctest
julia> Timestamp(Date(2018, 1, 1), Time(8, 15, 42, 0, 0, 5))
2018-01-01T08:15:42.000000005
```
"""
function Timestamp{P}(d::Date, t::Time=Time(0)) where {P}
    ticks, remainder = divrem(value(t), timestamp_scale(P))
    iszero(remainder) || throw(InexactError(:convert, Timestamp{P}, t))
    epochdays = Int128(value(d)) - UNIXEPOCHDAYS
    typemin(Int64) <= epochdays * timestamp_ticks_per_day(P) + ticks <= typemax(Int64) ||
        throw(ArgumentError("Date out of range for Timestamp{$P}"))
    return timestamp_from_day(Timestamp{P}, value(d), value(t))
end

# Fallback constructors
DateTime(y, m=1, d=1, h=0, mi=0, s=0, ms=0, ampm::AMPM=TWENTYFOURHOUR) = DateTime(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms), ampm)
Date(y, m=1, d=1) = Date(Int64(y), Int64(m), Int64(d))
Time(h, mi=0, s=0, ms=0, us=0, ns=0, ampm::AMPM=TWENTYFOURHOUR) = Time(Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns), ampm)
Timestamp{P}(y, m=1, d=1, h=0, mi=0, s=0, ms=0, us=0, ns=0, ampm::AMPM=TWENTYFOURHOUR) where {P} =
    Timestamp{P}(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns), ampm)

# Traits, Equality
Base.isfinite(::Union{Type{T}, T}) where {T<:TimeType} = true
calendar(dt::DateTime) = ISOCalendar
calendar(dt::Date) = ISOCalendar
calendar(dt::Timestamp) = ISOCalendar

"""
    eps(::Type{DateTime})::Millisecond
    eps(::Type{Date})::Day
    eps(::Type{Time})::Nanosecond
    eps(::Type{Timestamp})::Nanosecond
    eps(::Type{Timestamp{P}})::P
    eps(::TimeType)::Period

Return the smallest unit value supported by the `TimeType`.

# Examples
```jldoctest
julia> eps(DateTime)
1 millisecond

julia> eps(Date)
1 day

julia> eps(Time)
1 nanosecond

julia> eps(Timestamp)
1 nanosecond
```
"""
Base.eps(::Union{Type{DateTime}, Type{Date}, Type{Time}, Type{<:Timestamp}, TimeType})

Base.eps(::Type{DateTime}) = Millisecond(1)
Base.eps(::Type{Date}) = Day(1)
Base.eps(::Type{Time}) = Nanosecond(1)
Base.eps(::Type{Timestamp}) = Nanosecond(1)
Base.eps(::Type{Timestamp{P}}) where {P} = P(1)
Base.eps(::T) where T <: TimeType = eps(T)::Period

# zero returns dt::T - dt::T
Base.zero(::Type{DateTime}) = Millisecond(0)
Base.zero(::Type{Date}) = Day(0)
Base.zero(::Type{Time}) = Nanosecond(0)
Base.zero(::Type{Timestamp}) = Nanosecond(0)
Base.zero(::Type{Timestamp{P}}) where {P} = P(0)
Base.zero(::T) where T <: TimeType = zero(T)::Period


Base.typemax(::Union{DateTime, Type{DateTime}}) = DateTime(146138512, 12, 31, 23, 59, 59)
Base.typemin(::Union{DateTime, Type{DateTime}}) = DateTime(-146138511, 1, 1, 0, 0, 0)
Base.typemax(::Union{Date, Type{Date}}) = Date(252522163911149, 12, 31)
Base.typemin(::Union{Date, Type{Date}}) = Date(-252522163911150, 1, 1)
Base.typemax(::Union{Time, Type{Time}}) = Time(23, 59, 59, 999, 999, 999)
Base.typemin(::Union{Time, Type{Time}}) = Time(0)
Base.typemax(::Type{Timestamp}) = typemax(Timestamp{Nanosecond})
Base.typemax(::Type{Timestamp{P}}) where {P} = Timestamp{P}(UTInstant(P(typemax(Int64))))
Base.typemax(x::Timestamp) = typemax(typeof(x))
Base.typemin(::Type{Timestamp}) = typemin(Timestamp{Nanosecond})
Base.typemin(::Type{Timestamp{P}}) where {P} = Timestamp{P}(UTInstant(P(typemin(Int64))))
Base.typemin(x::Timestamp) = typemin(typeof(x))
# Date-DateTime promotion, isless, ==
Base.promote_rule(::Type{Date}, x::Type{DateTime}) = DateTime
Base.promote_rule(::Type{Date}, ::Type{Timestamp{P}}) where {P} = Timestamp{P}
Base.promote_rule(::Type{DateTime}, ::Type{Timestamp{P}}) where {P} = Timestamp{timestamp_finer(P, Millisecond)}
Base.promote_rule(::Type{Timestamp{P}}, ::Type{Timestamp{Q}}) where {P,Q} = Timestamp{timestamp_finer(P, Q)}
Base.isless(x::Timestamp, y::Timestamp) = isless((days(x), nsofday(x)), (days(y), nsofday(y)))
(==)(x::Timestamp, y::Timestamp) = days(x) == days(y) && nsofday(x) == nsofday(y)
Base.isless(x::T, y::T) where {T<:Timestamp} = isless(value(x), value(y))
(==)(x::T, y::T) where {T<:Timestamp} = value(x) == value(y)
Base.isless(x::T, y::T) where {T<:TimeType} = isless(value(x), value(y))
Base.isless(x::TimeType, y::TimeType) = isless(promote(x, y)...)
(==)(x::T, y::T) where {T<:TimeType} = (==)(value(x), value(y))
(==)(x::TimeType, y::TimeType) = (===)(promote(x, y)...)
# Comparisons between Timestamp and the wider-ranged Date/DateTime bypass
# promotion (which would throw for instants outside the Timestamp range) and
# instead compare (day, time-of-day) pairs, which never overflow.
Base.isless(x::Timestamp, y::DateTime) = isless((days(x), nsofday(x)), (days(y), 1000000 * msofday(y)))
Base.isless(x::DateTime, y::Timestamp) = isless((days(x), 1000000 * msofday(x)), (days(y), nsofday(y)))
Base.isless(x::Timestamp, y::Date) = isless((days(x), nsofday(x)), (value(y), Int64(0)))
Base.isless(x::Date, y::Timestamp) = isless((value(x), Int64(0)), (days(y), nsofday(y)))
(==)(x::Timestamp, y::DateTime) = days(x) == days(y) && nsofday(x) == 1000000 * msofday(y)
(==)(x::DateTime, y::Timestamp) = y == x
(==)(x::Timestamp, y::Date) = days(x) == value(y) && nsofday(x) == 0
(==)(x::Date, y::Timestamp) = y == x
Base.min(x::AbstractTime) = x
Base.max(x::AbstractTime) = x
Base.minmax(x::AbstractTime) = (x, x)
Base.hash(x::Time, h::UInt) =
    hash(hour(x), hash(minute(x), hash(second(x),
        hash(millisecond(x), hash(microsecond(x), hash(nanosecond(x), h))))))
# Date, DateTime, and Timestamp compare equal across types when they denote the
# same instant, so they hash alike: on the (day, nanosecond of day) pair.
Base.hash(x::Date, h::UInt) = hash(Int64(0), hash(days(x), h))
Base.hash(x::DateTime, h::UInt) = hash(1000000 * msofday(x), hash(days(x), h))
Base.hash(x::Timestamp, h::UInt) = hash(nsofday(x), hash(days(x), h))

Base.sleep(duration::Period) = sleep(seconds(duration))

function Base.Timer(delay::Period; interval::Period=Second(0))
    Timer(seconds(delay), interval=seconds(interval))
end

function Base.timedwait(testcb, timeout::Period; pollint::Period=Millisecond(100))
    timedwait(testcb, seconds(timeout), pollint=seconds(pollint))
end

Base.OrderStyle(::Type{<:AbstractTime}) = Base.Ordered()
Base.ArithmeticStyle(::Type{<:AbstractTime}) = Base.ArithmeticWraps()

# minimal Base.TOML support
Date(d::Base.TOML.Date) = Date(d.year, d.month, d.day)
Time(t::Base.TOML.Time) = Time(t.hour, t.minute, t.second, t.ms)
DateTime(dt::Base.TOML.DateTime) = DateTime(Date(dt.date), Time(dt.time))

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
the instant. The epoch depends on the type that holds the instant: `Date` and `DateTime`
count from the Rata Die epoch, and `Timestamp` counts from the Unix epoch.
"""
struct UTInstant{P<:Period} <: Instant
    periods::P
end

# Convenience default constructors
UTM(x) = UTInstant(Millisecond(x))
UTD(x) = UTInstant(Day(x))

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

`Timestamp{P}` represents a point in time according to the proleptic Gregorian
calendar. It stores the number of `P` units since the Unix epoch, `1970-01-01T00:00:00`.
The built-in resolutions `Second`, `Millisecond`, `Microsecond`, and `Nanosecond` store
an `Int64`. `Timestamp(...)` without a type parameter creates a `Timestamp{Nanosecond}`,
except that `Timestamp(ts::Timestamp)` returns `ts`.

The resolution sets the range. `Timestamp{Nanosecond}` covers
`1677-09-21T00:12:43.145224192` through `2262-04-11T23:47:16.854775807`.
Each coarser resolution covers 1000 times as much time: about ±292 thousand
years around 1970 for `Microsecond`, ±292 million years for `Millisecond`, and
±292 billion years for `Second`.

Converting a timestamp to a coarser `Timestamp` resolution, or adding a period
finer than `P`, throws an `InexactError` if it would lose precision. Use
[`floor`](@ref), [`ceil`](@ref), or [`round`](@ref) to drop the extra precision
first. Like `DateTime`, period arithmetic wraps at the ends of the range:
`typemax(Timestamp{P}) + P(1) == typemin(Timestamp{P})`.

Comparisons between timestamps of any resolution, `Date`, and `DateTime` values
are exact and never throw. Other operations that mix resolutions, such as
subtraction, convert both values to the finer resolution. A `DateTime` counts as
`Millisecond`:

```jldoctest
julia> ts = Timestamp(2020, 1, 1, 0, 0, 0, 1, 0, 500)
2020-01-01T00:00:00.0010005

julia> ts - DateTime(2020, 1, 1)
1000500 nanoseconds

julia> Timestamp{Millisecond}(floor(ts, Millisecond))
2020-01-01T00:00:00.001

julia> DateTime(ts) # floors to the millisecond
2020-01-01T00:00:00.001
```

`Timestamp` without a type parameter is not a concrete type. For array elements
and struct fields, use a concrete type such as `Timestamp{Nanosecond}`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.

# Extended help

With a built-in resolution, `Timestamp{P}` has the same bits as an Apache Arrow
`timestamp` or a NumPy `datetime64` with the same unit, so buffers can be reinterpreted
directly. NumPy uses `typemin(Int64)` for `NaT` (not a time).

`Dates.value(ts)` returns the count since the Unix epoch, and `convert(P, ts)`
returns the same count as a `P`. In contrast, calling a period type returns a calendar
field: for example, `Second(ts)` is the second of the minute, from 0 through 59.
`DateTime` counts from a different epoch, so `Dates.value(ts)` and `Dates.value(dt)`
differ even when `ts == dt`.

A package can add a resolution with its own `TimePeriod` type, such as a 128-bit count
of picoseconds. The type needs methods for `Dates.value`, `typemin`, `typemax`, and
`Dates.tons`, which returns the length of a period in nanoseconds (a `Rational` for a
unit shorter than a nanosecond). For years whose day count overflows `Int64`, also add a
method for `Dates.timestamp_totaldays(P, y, m, d)`. Printing and the `n` format code
round digits finer than a nanosecond down.
"""
struct Timestamp{P<:TimePeriod} <: AbstractDateTime
    instant::UTInstant{P}
    Timestamp{P}(instant::UTInstant{P}) where {P} = new{P}(instant)
end

Timestamp(args...; kwargs...) = Timestamp{Nanosecond}(args...; kwargs...)
Timestamp(instant::UTInstant{P}) where {P} = Timestamp{P}(instant)
Timestamp(ts::Timestamp) = ts

# Integer type of a count of P
timestamp_count_type(::Type{P}) where {P} = typeof(value(zero(P)))
# Rata Die day number of a date. A package period with a huge range can use a wider type.
timestamp_totaldays(::Type{P}, y, m, d) where {P} = totaldays(y, m, d)
# Nanoseconds per unit of P (a Rational below a nanosecond), and units of P per day
timestamp_scale(::Type{P}) where {P<:Period} = tons(oneunit(P))
timestamp_scale(::Type{Timestamp{P}}) where {P} = timestamp_scale(P)
timestamp_ticks_per_day(::Type{P}) where {P} = NS_PER_DAY ÷ timestamp_scale(P)
# The finer of two resolutions
timestamp_finer(::Type{P}, ::Type{Q}) where {P,Q} =
    timestamp_scale(P) <= timestamp_scale(Q) ? P : Q

# `ns` nanoseconds as a count of P. Throws an InexactError if `ns` has more precision than P.
function timestamp_ticks(::Type{P}, ns::Real) where {P}
    ticks, remainder = divrem(ns, timestamp_scale(P))
    iszero(remainder) || throw(InexactError(:convert, Timestamp{P}, ns))
    return ticks
end

# The Timestamp{P} at `ns` nanoseconds after the start of Rata Die day `rata`. Throws an
# InexactError if the result is out of range or has more precision than P.
function timestamp_from_day(::Type{Timestamp{P}}, rata, ns) where {P}
    ns = (Int128(rata) - UNIXEPOCHDAYS) * NS_PER_DAY + ns
    return Timestamp{P}(UTInstant(P(timestamp_ticks(P, ns))))
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

# Nanoseconds per day, and the Rata Die day number of the Unix epoch 1970-01-01
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
`ns` can be from `0` through `999999999`, so it can hold a full fraction of a second.
Together, `ms`, `us`, and `ns` must be less than one second.

!!! compat "Julia 1.14"
    Before Julia 1.14, `ns` must be less than `1000`.
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
    # `ns` can hold a full fraction of a second, as parsed by the `n` format code
    -1 < ns < 1000000000 || return ArgumentError("Nanosecond: $ns out of range (0:999999999)")
    1000000ms + 1000us + ns < 1000000000 ||
        return ArgumentError("Sub-second parts must together be less than one second")
    return nothing
end

Time(dt::Base.Libc.TmStruct) = Time(dt.hour, dt.min, dt.sec)

"""
    Timestamp{P}(y, [m, d, h, mi, s, ms, us, ns])::Timestamp{P}
    Timestamp(y, [m, d, h, mi, s, ms, us, ns])::Timestamp{Nanosecond}

Construct a `Timestamp` type by parts. Arguments must be convertible to
[`Int64`](@ref). As for [`Time`](@ref), `ns` can hold a full fraction of a second.
Throws an `ArgumentError` if the result is out of range or has more precision than `P`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
function Timestamp{P}(y::Int64, m::Int64=1, d::Int64=1, h::Int64=0, mi::Int64=0, s::Int64=0,
                   ms::Int64=0, us::Int64=0, ns::Int64=0, ampm::AMPM=TWENTYFOURHOUR) where {P}
    err = validargs(Timestamp{P}, y, m, d, h, mi, s, ms, us, ns, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600h)
    return timestamp_from_day(Timestamp{P}, timestamp_totaldays(P, y, m, d), nsofday)
end

function validargs(::Type{Timestamp{P}}, y::Int64, m::Int64, d::Int64, h::Int64, mi::Int64,
                   s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM=TWENTYFOURHOUR) where {P}
    # Check the year first: `totaldays` below can overflow for years far out of range
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
    epochdays = timestamp_totaldays(P, y, m, d) - UNIXEPOCHDAYS
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600 * adjusthour(h, ampm))
    ticks, remainder = divrem(nsofday, timestamp_scale(P))
    iszero(remainder) || return ArgumentError("Fractional second is not exactly representable as Timestamp{$P}")
    # Compare (day, time of day) pairs with the range limits, so the check cannot overflow
    fldmod(value(typemin(P)), timestamp_ticks_per_day(P)) <= (epochdays, ticks) <=
        fldmod(value(typemax(P)), timestamp_ticks_per_day(P)) ||
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
Parts not provided default to those of the Unix epoch, `1970-01-01T00:00:00`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
function Timestamp{P}(period::Period, periods::Period...) where {P}
    y = Year(1970); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0)
    ms = Millisecond(0); us = Microsecond(0); ns = Nanosecond(0)
    for p in (period, periods...)
        p isa Union{Year,Month,Day,Hour,Minute,Second,Millisecond,Microsecond,Nanosecond} ||
            throw(ArgumentError("unsupported timestamp part; add custom periods with +"))
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
    Timestamp{P}(d::Date, [t::Time])::Timestamp{P}
    Timestamp(d::Date, [t::Time])::Timestamp{Nanosecond}

Construct a `Timestamp` from a `Date` and an optional `Time` of day. Throws an
`ArgumentError` if the result is out of range, or an `InexactError` if `t` has more
precision than `P`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.

```jldoctest
julia> Timestamp(Date(2018, 1, 1), Time(8, 15, 42, 0, 0, 5))
2018-01-01T08:15:42.000000005
```
"""
function Timestamp{P}(d::Date, t::Time=Time(0)) where {P}
    ticks = (Int128(value(d)) - UNIXEPOCHDAYS) * timestamp_ticks_per_day(P) + timestamp_ticks(P, value(t))
    value(typemin(P)) <= ticks <= value(typemax(P)) || throw(ArgumentError("Date out of range for Timestamp{$P}"))
    return Timestamp{P}(UTInstant(P(ticks)))
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
Base.typemax(::Type{Timestamp{P}}) where {P} = Timestamp{P}(UTInstant(typemax(P)))
Base.typemax(x::Timestamp) = typemax(typeof(x))
Base.typemin(::Type{Timestamp}) = typemin(Timestamp{Nanosecond})
Base.typemin(::Type{Timestamp{P}}) where {P} = Timestamp{P}(UTInstant(typemin(P)))
Base.typemin(x::Timestamp) = typemin(typeof(x))
# Date-DateTime promotion, isless, ==
Base.promote_rule(::Type{Date}, x::Type{DateTime}) = DateTime
Base.promote_rule(::Type{Date}, ::Type{Timestamp{P}}) where {P} = Timestamp{P}
# Promote to the finer resolution. Package periods may have no period promotion rules.
Base.promote_rule(::Type{DateTime}, ::Type{Timestamp{P}}) where {P} = Timestamp{timestamp_finer(P, Millisecond)}
Base.promote_rule(::Type{Timestamp{P}}, ::Type{Timestamp{Q}}) where {P,Q} = Timestamp{timestamp_finer(P, Q)}
Base.isless(x::T, y::T) where {T<:TimeType} = isless(value(x), value(y))
Base.isless(x::TimeType, y::TimeType) = isless(promote(x, y)...)
(==)(x::T, y::T) where {T<:TimeType} = (==)(value(x), value(y))
(==)(x::TimeType, y::TimeType) = (===)(promote(x, y)...)
# A Timestamp compares with a Date, a DateTime, or a Timestamp of another resolution
# by (day, nanosecond of day). Promotion would throw for values outside the range of
# the promoted type.
Base.isless(x::Timestamp, y::Union{Date,DateTime,Timestamp}) = isless((days(x), nsofday(x)), (days(y), nsofday(y)))
Base.isless(x::Union{Date,DateTime}, y::Timestamp) = isless((days(x), nsofday(x)), (days(y), nsofday(y)))
(==)(x::Timestamp, y::Union{Date,DateTime,Timestamp}) = days(x) == days(y) && nsofday(x) == nsofday(y)
(==)(x::Union{Date,DateTime}, y::Timestamp) = y == x
Base.isless(x::T, y::T) where {T<:Timestamp} = isless(value(x), value(y))
(==)(x::T, y::T) where {T<:Timestamp} = value(x) == value(y)
Base.min(x::AbstractTime) = x
Base.max(x::AbstractTime) = x
Base.minmax(x::AbstractTime) = (x, x)
Base.hash(x::Time, h::UInt) =
    hash(hour(x), hash(minute(x), hash(second(x),
        hash(millisecond(x), hash(microsecond(x), hash(nanosecond(x), h))))))
# Equal Date, DateTime, and Timestamp values hash alike: the hash uses only the
# (day, nanosecond of day) pair
Base.hash(x::Union{Date,DateTime,Timestamp}, h::UInt) = hash(nsofday(x), hash(days(x), h))

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

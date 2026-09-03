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
the instant.
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
    Timestamp

`Timestamp` represents a point in time according to the proleptic Gregorian
calendar with nanosecond resolution. It wraps a `UTInstant{Nanosecond}`: an
`Int64` count of nanoseconds since the unix epoch `1970-01-01T00:00:00`, the
same physical representation as Apache Arrow's `timestamp[ns]`, numpy's
`datetime64[ns]`, and pandas timestamps. This makes `Timestamp` 8 bytes and
`isbits`, and lets value buffers be shared with those systems without
conversion, though each layers its own conventions on top: numpy reserves
`typemin(Int64)` as its `NaT` sentinel (so pandas' range starts one
nanosecond later), and Arrow carries time zone and validity information
separately.

Nanosecond resolution in 64 bits bounds the representable range to
`1677-09-21T00:12:43.145224192` through `2262-04-11T23:47:16.854775807`
(`typemin(Timestamp)`/`typemax(Timestamp)`); constructing a `Timestamp` outside
this range throws an `ArgumentError`. Like `DateTime`, the type uses fixed-point
arithmetic and is thus prone to underflowing and overflowing: adding a period
that would leave the representable range wraps around rather than throwing.
For instants outside this range, or when nanosecond resolution is not needed,
use [`DateTime`](@ref), which covers ±146 million years at millisecond
resolution. The two types compare directly. Mixed arithmetic promotes
`DateTime` to `Timestamp`, so its value must be in the `Timestamp` range:

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
"""
struct Timestamp <: AbstractDateTime
    instant::UTInstant{Nanosecond}
    Timestamp(instant::UTInstant{Nanosecond}) = new(instant)
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

# Timestamp instants count nanoseconds from the unix epoch rather than the
# Rata Die epoch: year 0 lies outside the ~±292 year range that nanoseconds
# afford an Int64, so Rata Die <-> Timestamp conversions work at day granularity.
const NS_PER_DAY = 86400000000000
const UNIXEPOCHDAYS = totaldays(1970, 1, 1) # Rata Die days for 1970-01-01
# The representable range as (days since the unix epoch, nanoseconds of day)
const TIMESTAMPMIN = fldmod(typemin(Int64), NS_PER_DAY)
const TIMESTAMPMAX = fldmod(typemax(Int64), NS_PER_DAY)
# Nanoseconds since the unix epoch for a Rata Die day number and nanosecond of
# day. This wraps rather than validating, so the arithmetic and rounding built
# on it wrap at the ends of the range like DateTime's.
unixns(rata, nsofday) = (rata - UNIXEPOCHDAYS) * NS_PER_DAY + nsofday

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
    Timestamp(y, [m, d, h, mi, s, ms, us, ns])::Timestamp

Construct a `Timestamp` type by parts. Arguments must be convertible to
[`Int64`](@ref) and the result must lie within the representable range
(`typemin(Timestamp)` to `typemax(Timestamp)`, from 1677-09-21 through
2262-04-11). The `ns` argument can contain a full fractional second, but the
combined `ms`, `us`, and `ns` arguments must be less than one second.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
function Timestamp(y::Int64, m::Int64=1, d::Int64=1, h::Int64=0, mi::Int64=0, s::Int64=0,
                   ms::Int64=0, us::Int64=0, ns::Int64=0, ampm::AMPM=TWENTYFOURHOUR)
    err = validargs(Timestamp, y, m, d, h, mi, s, ms, us, ns, ampm)
    err === nothing || throw(err)
    h = adjusthour(h, ampm)
    # validargs bounded the result to Int64, so the wrapping arithmetic in unixns
    # is exact here even on the two partial boundary days, where the day product alone wraps
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600h)
    return Timestamp(UTN(unixns(totaldays(y, m, d), nsofday)))
end

function validargs(::Type{Timestamp}, y::Int64, m::Int64, d::Int64, h::Int64, mi::Int64,
                   s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM=TWENTYFOURHOUR)
    1677 <= y <= 2262 || return ArgumentError("Year: $y out of range (1677:2262)")
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
    TIMESTAMPMIN <= (epochdays, nsofday) <= TIMESTAMPMAX ||
        return ArgumentError("Timestamp: $y-$m-$d out of range ($(typemin(Timestamp)) to $(typemax(Timestamp)))")
    return nothing
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

function Timestamp(y::Year, m::Month=Month(1), d::Day=Day(1),
                   h::Hour=Hour(0), mi::Minute=Minute(0), s::Second=Second(0),
                   ms::Millisecond=Millisecond(0),
                   us::Microsecond=Microsecond(0), ns::Nanosecond=Nanosecond(0))
    return Timestamp(value(y), value(m), value(d), value(h), value(mi), value(s),
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
function Timestamp(period::Period, periods::Period...)
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
    return Timestamp(y, m, d, h, mi, s, ms, us, ns)
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
function Timestamp(d::Date, t::Time=Time(0))
    epochdays = value(d) - UNIXEPOCHDAYS
    TIMESTAMPMIN <= (epochdays, value(t)) <= TIMESTAMPMAX ||
        throw(ArgumentError("Timestamp: $d out of range ($(typemin(Timestamp)) to $(typemax(Timestamp)))"))
    return Timestamp(UTN(unixns(value(d), value(t))))
end

# Fallback constructors
DateTime(y, m=1, d=1, h=0, mi=0, s=0, ms=0, ampm::AMPM=TWENTYFOURHOUR) = DateTime(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms), ampm)
Date(y, m=1, d=1) = Date(Int64(y), Int64(m), Int64(d))
Time(h, mi=0, s=0, ms=0, us=0, ns=0, ampm::AMPM=TWENTYFOURHOUR) = Time(Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns), ampm)
Timestamp(y, m=1, d=1, h=0, mi=0, s=0, ms=0, us=0, ns=0, ampm::AMPM=TWENTYFOURHOUR) =
    Timestamp(Int64(y), Int64(m), Int64(d), Int64(h), Int64(mi), Int64(s), Int64(ms), Int64(us), Int64(ns), ampm)

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
Base.eps(::Union{Type{DateTime}, Type{Date}, Type{Time}, Type{Timestamp}, TimeType})

Base.eps(::Type{DateTime}) = Millisecond(1)
Base.eps(::Type{Date}) = Day(1)
Base.eps(::Type{Time}) = Nanosecond(1)
Base.eps(::Type{Timestamp}) = Nanosecond(1)
Base.eps(::T) where T <: TimeType = eps(T)::Period

# zero returns dt::T - dt::T
Base.zero(::Type{DateTime}) = Millisecond(0)
Base.zero(::Type{Date}) = Day(0)
Base.zero(::Type{Time}) = Nanosecond(0)
Base.zero(::Type{Timestamp}) = Nanosecond(0)
Base.zero(::T) where T <: TimeType = zero(T)::Period


Base.typemax(::Union{DateTime, Type{DateTime}}) = DateTime(146138512, 12, 31, 23, 59, 59)
Base.typemin(::Union{DateTime, Type{DateTime}}) = DateTime(-146138511, 1, 1, 0, 0, 0)
Base.typemax(::Union{Date, Type{Date}}) = Date(252522163911149, 12, 31)
Base.typemin(::Union{Date, Type{Date}}) = Date(-252522163911150, 1, 1)
Base.typemax(::Union{Time, Type{Time}}) = Time(23, 59, 59, 999, 999, 999)
Base.typemin(::Union{Time, Type{Time}}) = Time(0)
Base.typemax(::Union{Timestamp, Type{Timestamp}}) = Timestamp(UTN(typemax(Int64)))
Base.typemin(::Union{Timestamp, Type{Timestamp}}) = Timestamp(UTN(typemin(Int64)))
# Date-DateTime promotion, isless, ==
Base.promote_rule(::Type{Date}, x::Type{DateTime}) = DateTime
Base.promote_rule(::Type{Date}, ::Type{Timestamp}) = Timestamp
Base.promote_rule(::Type{DateTime}, ::Type{Timestamp}) = Timestamp
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

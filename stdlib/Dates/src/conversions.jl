# This file is a part of Julia. License is MIT: https://julialang.org/license

# Conversion/Promotion

"""
    Date(dt::TimeType)

Convert a `DateTime` or `Timestamp` to a `Date`. The time-of-day parts are
truncated, so only the year, month, and day parts are used in construction.
"""
Date(dt::TimeType) = convert(Date, dt)

"""
    DateTime(dt::TimeType)

Convert a `Date` or `Timestamp` to a `DateTime`. For a `Date`, the time-of-day
parts are zero. For a `Timestamp`, the value is floored to millisecond resolution.
"""
DateTime(dt::TimeType) = convert(DateTime, dt)

"""
    Time(dt::AbstractDateTime)

Convert an `AbstractDateTime`, such as a `DateTime` or `Timestamp`, to a `Time`,
preserving its time-of-day part.

!!! compat "Julia 1.14"
    This method was generalized from `DateTime` to `AbstractDateTime` in Julia 1.14.
"""
Time(dt::AbstractDateTime) = convert(Time, dt)

"""
    Timestamp(dt::TimeType)

Convert a `Date` or `DateTime` to a `Timestamp`. Parts finer than the input's
resolution are assumed to be zero. Throws an `ArgumentError` ([`InexactError`](@ref)
for a `DateTime`) if the instant lies outside the representable `Timestamp` range.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
Timestamp(dt::TimeType) = convert(Timestamp, dt)

Base.convert(::Type{DateTime}, dt::Date) = DateTime(UTM(value(dt) * 86400000))
Base.convert(::Type{Date}, dt::DateTime) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::DateTime) = Time(Nanosecond((value(dt) % 86400000) * 1000000))
# Timestamp <-> DateTime/Date/Time. DateTime -> Timestamp throws an InexactError
# for instants outside the Timestamp range; Timestamp -> DateTime floors to the
# millisecond (fld also rounds pre-1970 instants toward the past, matching
# Date(::DateTime)).
function Base.convert(::Type{Timestamp}, dt::DateTime)
    UNIXEPOCH - typemax(Int64) ÷ 1000000 <= value(dt) <= UNIXEPOCH + typemax(Int64) ÷ 1000000 ||
        throw(InexactError(:convert, Timestamp, dt))
    return Timestamp(UTN((value(dt) - UNIXEPOCH) * 1000000))
end
Base.convert(::Type{Timestamp}, dt::Date) = Timestamp(dt)
Base.convert(::Type{DateTime}, dt::Timestamp) = DateTime(UTM(fld(value(dt), 1000000) + UNIXEPOCH))
Base.convert(::Type{Date}, dt::Timestamp) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::Timestamp) = Time(Nanosecond(nsofday(dt)))

Base.convert(::Type{DateTime},x::Millisecond)  = DateTime(Dates.UTInstant(x))  # Converts Rata Die milliseconds to a DateTime
Base.convert(::Type{Millisecond},dt::DateTime) = Millisecond(value(dt))        # Converts DateTime to Rata Die milliseconds
Base.convert(::Type{Date},x::Day)  = Date(Dates.UTInstant(x))  # Converts Rata Die days to a Date
Base.convert(::Type{Day},dt::Date) = Day(value(dt))            # Converts Date to Rata Die days
Base.convert(::Type{Timestamp},x::Nanosecond)  = Timestamp(UTInstant(x))       # Converts unix nanoseconds to a Timestamp
Base.convert(::Type{Nanosecond},dt::Timestamp) = Nanosecond(value(dt))         # Converts Timestamp to unix nanoseconds

### External Conversions
const UNIXEPOCH = value(DateTime(1970)) #Rata Die milliseconds for 1970-01-01T00:00:00

"""
    unix2datetime(x::Real; localtime::Bool=false)::DateTime

Take the number of seconds since unix epoch `1970-01-01T00:00:00` (UTC) and convert to the
corresponding `DateTime`. If `localtime` is `true`, then the output is in the host
system's local time zone, otherwise it is in UTC/GMT.
"""
function unix2datetime(x::Real; localtime::Bool=false)
    # Rounding should match `now` below
    ms = trunc(Int64, Int64(1000) * x)
    if localtime
        s, ms = divrem(ms, 1000)
        return DateTime(Libc.TmStruct(s)) + Millisecond(ms)
    else
        rata = UNIXEPOCH + ms
        return DateTime(UTM(rata))
    end
end

"""
    datetime2unix(dt::DateTime)::Float64

Take the given `DateTime` and return the number of seconds
since the unix epoch `1970-01-01T00:00:00` as a [`Float64`](@ref).
"""
datetime2unix(dt::DateTime) = (value(dt) - UNIXEPOCH) / 1000.0

"""
    unix2timestamp(x)::Timestamp

Take the number of seconds since unix epoch `1970-01-01T00:00:00` (UTC) and
convert to the corresponding `Timestamp`. Note that a [`Float64`](@ref) second
count near the present carries only about microsecond precision; construct a
`Timestamp` from an integer nanosecond count
(`convert(Timestamp, Nanosecond(ns))`) when full nanosecond precision is
required.

!!! compat "Julia 1.14"
    This function requires Julia 1.14 or later.
"""
unix2timestamp(x::Real) = Timestamp(UTN(trunc(Int64, Int64(1000000000) * x)))
function unix2timestamp(x::Integer)
    -(typemax(Int64) ÷ 1000000000) <= x <= typemax(Int64) ÷ 1000000000 ||
        throw(InexactError(:unix2timestamp, Timestamp, x))
    return Timestamp(UTN(Int64(x) * 1000000000))
end

"""
    timestamp2unix(dt::Timestamp)::Float64

Take the given `Timestamp` and return the number of seconds since the unix
epoch `1970-01-01T00:00:00` as a [`Float64`](@ref). Note that the returned
value carries only about microsecond precision near the present;
`Dates.value(dt)` is the exact count of nanoseconds since the unix epoch.

!!! compat "Julia 1.14"
    This function requires Julia 1.14 or later.
"""
timestamp2unix(dt::Timestamp) = value(dt) / 1.0e9

"""
    now()::DateTime

Return a `DateTime` corresponding to the user's system time including the system timezone
locale.
"""
function now()
    tv = Libc.TimeVal()
    tm = Libc.TmStruct(tv.sec)
    return DateTime(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec, div(tv.usec, 1000))
end

"""
    today()::Date

Return the date portion of `now()`.
"""
today() = Date(now())

"""
    now(::Type{UTC})::DateTime

Return a `DateTime` corresponding to the user's system time as UTC/GMT.
For other time zones, see the TimeZones.jl package.

# Examples
```jldoctest; filter = r"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d{3})?" => "2023-01-04T10:52:24.864"
julia> now(UTC)
2023-01-04T10:52:24.864
```
"""
now(::Type{UTC}) = unix2datetime(time())

# uv_timespec_t; unlike the platform-dependent C `struct timespec`, its fields
# have fixed-width types on all supported platforms
struct UVTimespec
    sec::Int64
    nsec::Int32
end

function unix_now_ns()
    ts = Ref{UVTimespec}()
    # 1 is UV_CLOCK_REALTIME; on Windows libuv reads the precise system clock
    err = ccall(:uv_clock_gettime, Cint, (Cint, Ref{UVTimespec}), 1, ts)
    err == 0 || Base.uv_error("uv_clock_gettime", err)
    return ts[]
end

"""
    now(::Type{Timestamp})::Timestamp

Return a `Timestamp` corresponding to the user's system time including the
system timezone locale, at the full resolution of the system clock.

!!! compat "Julia 1.14"
    This method requires Julia 1.14 or later.
"""
function now(::Type{Timestamp})
    ts = unix_now_ns()
    tm = Libc.TmStruct(ts.sec)
    return Timestamp(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec,
                     0, 0, Int64(ts.nsec))
end

"""
    now(::Type{Timestamp}, ::Type{UTC})::Timestamp

Return a `Timestamp` corresponding to the user's system time as UTC/GMT, at the
full resolution of the system clock.

!!! compat "Julia 1.14"
    This method requires Julia 1.14 or later.
"""
function now(::Type{Timestamp}, ::Type{UTC})
    ts = unix_now_ns()
    return Timestamp(UTN(ts.sec * 1000000000 + ts.nsec))
end

"""
    rata2datetime(days)::DateTime

Take the number of Rata Die days since epoch `0000-12-31T00:00:00` and return the
corresponding `DateTime`.
"""
rata2datetime(days) = DateTime(yearmonthday(days)...)

"""
    datetime2rata(dt::TimeType)::Int64

Return the number of Rata Die days since epoch from the given `Date`,
`DateTime`, or `Timestamp`.
"""
datetime2rata(dt::TimeType) = days(dt)

# Julian conversions
const JULIANEPOCH = value(DateTime(-4713, 11, 24, 12))

"""
    julian2datetime(julian_days)::DateTime

Take the number of Julian calendar days since epoch `-4713-11-24T12:00:00` and return the
corresponding `DateTime`.
"""
function julian2datetime(f)
    rata = JULIANEPOCH + round(Int64, Int64(86400000) * f)
    return DateTime(UTM(rata))
end

"""
    datetime2julian(dt::DateTime)::Float64

Take the given `DateTime` and return the number of Julian calendar days since the julian
epoch `-4713-11-24T12:00:00` as a [`Float64`](@ref).
"""
datetime2julian(dt::DateTime) = (value(dt) - JULIANEPOCH) / 86400000.0

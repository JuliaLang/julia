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

Convert a `Date` or `Timestamp` to a `DateTime`. A `Date` becomes midnight of that
day, and a `Timestamp` is floored to the millisecond.
"""
DateTime(dt::TimeType) = convert(DateTime, dt)

"""
    Time(dt::AbstractDateTime)

Convert a `DateTime` or `Timestamp` to a `Time` that holds its time of day.
"""
Time(dt::AbstractDateTime) = convert(Time, dt)

"""
    Timestamp{P}(dt::TimeType)

Convert a `Date`, `DateTime`, or `Timestamp` to `Timestamp{P}`. Throws an error if
the value is out of range or has more precision than `P`.

!!! compat "Julia 1.14"
    `Timestamp` requires Julia 1.14 or later.
"""
Timestamp{P}(dt::TimeType) where {P} = convert(Timestamp{P}, dt)
Base.convert(::Type{Timestamp}, dt::Union{Date,DateTime}) = convert(Timestamp{Nanosecond}, dt)
Base.convert(::Type{Timestamp{P}}, dt::Timestamp{P}) where {P} = dt
Base.convert(::Type{Timestamp{P}}, dt::Timestamp{Q}) where {P,Q} =
    Timestamp{P}(UTInstant(P(timestamp_ticks(P, Int128(value(dt)) * timestamp_scale(Q)))))

Base.convert(::Type{DateTime}, dt::Date) = DateTime(UTM(value(dt) * 86400000))
Base.convert(::Type{Date}, dt::DateTime) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::DateTime) = Time(Nanosecond((value(dt) % 86400000) * 1000000))
# DateTime to Timestamp throws an InexactError if the value does not fit.
# Timestamp to DateTime floors to the millisecond.
function Base.convert(::Type{Timestamp{P}}, dt::DateTime) where {P}
    ticks = timestamp_ticks(P, (Int128(value(dt)) - UNIXEPOCH) * 1000000)
    return Timestamp{P}(UTInstant(P(ticks)))
end
Base.convert(::Type{Timestamp{P}}, dt::Date) where {P} = Timestamp{P}(dt)
Base.convert(::Type{DateTime}, dt::Timestamp{P}) where {P} =
    DateTime(UTM(Int64(fld(Int128(value(dt)) * timestamp_scale(P), 1000000) + UNIXEPOCH)))
Base.convert(::Type{Date}, dt::Timestamp) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::Timestamp) = Time(Nanosecond(nsofday(dt)))

Base.convert(::Type{DateTime},x::Millisecond)  = DateTime(Dates.UTInstant(x))  # Converts Rata Die milliseconds to a DateTime
Base.convert(::Type{Millisecond},dt::DateTime) = Millisecond(value(dt))        # Converts DateTime to Rata Die milliseconds
Base.convert(::Type{Date},x::Day)  = Date(Dates.UTInstant(x))  # Converts Rata Die days to a Date
Base.convert(::Type{Day},dt::Date) = Day(value(dt))            # Converts Date to Rata Die days
Base.convert(::Type{Timestamp},x::Nanosecond)  = Timestamp(UTInstant(x))       # Converts Unix nanoseconds to a Timestamp
# Convert between a Timestamp and a period counted from the Unix epoch
Base.convert(::Type{P}, dt::Timestamp{Q}) where {P<:TimePeriod,Q} =
    P(timestamp_ticks(P, Int128(value(dt)) * timestamp_scale(Q)))
Base.convert(::Type{Timestamp{P}}, x::Q) where {P,Q<:TimePeriod} =
    Timestamp{P}(UTInstant(P(timestamp_ticks(P, Int128(value(x)) * timestamp_scale(Q)))))

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
    unix2timestamp(x::Real)::Timestamp{Nanosecond}
    unix2timestamp(Timestamp{P}, x::Real)::Timestamp{P}

Take the number of seconds since the Unix epoch `1970-01-01T00:00:00` (UTC) and
convert it to a `Timestamp`, rounding toward zero to resolution `P`. A [`Float64`](@ref)
count of seconds near the present has only about microsecond precision. For exact
nanoseconds, use `convert(Timestamp, Nanosecond(ns))`.

!!! compat "Julia 1.14"
    This function requires Julia 1.14 or later.
"""
unix2timestamp(x::Real) = unix2timestamp(Timestamp{Nanosecond}, x)
unix2timestamp(::Type{Timestamp}, x::Real) = unix2timestamp(Timestamp{Nanosecond}, x)
unix2timestamp(::Type{Timestamp{P}}, x::Real) where {P} =
    Timestamp{P}(UTInstant(P(trunc(timestamp_count_type(P), (1000000000 ÷ timestamp_scale(P)) * x))))
function unix2timestamp(::Type{Timestamp{P}}, x::Integer) where {P}
    scale = 1000000000 ÷ timestamp_scale(P)
    cld(value(typemin(P)), scale) <= x <= fld(value(typemax(P)), scale) ||
        throw(InexactError(:unix2timestamp, Timestamp{P}, x))
    return Timestamp{P}(UTInstant(P(timestamp_count_type(P)(x) * scale)))
end

"""
    timestamp2unix(dt::Timestamp)::Float64

Take the given `Timestamp` and return the number of seconds since the Unix epoch
`1970-01-01T00:00:00` as a [`Float64`](@ref). Near the present, the result has only
about microsecond precision; `Dates.value(dt)` is the exact count.

!!! compat "Julia 1.14"
    This function requires Julia 1.14 or later.
"""
timestamp2unix(dt::Timestamp{P}) where {P} = Float64(value(dt) / (1000000000 ÷ timestamp_scale(P)))

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

# libuv's uv_timespec_t, which has the same field types on all platforms
struct UVTimespec
    sec::Int64
    nsec::Int32
end

# Read the system real-time clock at full resolution (libuv uses the precise clock
# on Windows)
function clock_realtime()
    ts = Ref{UVTimespec}()
    err = ccall(:uv_clock_gettime, Cint, (Cint, Ref{UVTimespec}), 1, ts) # 1 is UV_CLOCK_REALTIME
    err == 0 || Base.uv_error("uv_clock_gettime", err)
    return ts[]
end

"""
    now(::Type{Timestamp})::Timestamp{Nanosecond}
    now(::Type{Timestamp{P}})::Timestamp{P}

Return a `Timestamp` corresponding to the user's system time including the system
timezone locale, floored to resolution `P`.

!!! compat "Julia 1.14"
    This method requires Julia 1.14 or later.
"""
now(::Type{Timestamp}) = now(Timestamp{Nanosecond})
function now(::Type{Timestamp{P}}) where {P}
    ts = clock_realtime()
    tm = Libc.TmStruct(ts.sec)
    ns = fld(Int64(ts.nsec), timestamp_scale(P)) * timestamp_scale(P)
    return Timestamp{P}(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec, 0, 0, ns)
end

"""
    now(::Type{Timestamp}, ::Type{UTC})::Timestamp{Nanosecond}
    now(::Type{Timestamp{P}}, ::Type{UTC})::Timestamp{P}

Return a `Timestamp` corresponding to the user's system time as UTC/GMT, floored to
resolution `P`.

!!! compat "Julia 1.14"
    This method requires Julia 1.14 or later.
"""
now(::Type{Timestamp}, ::Type{UTC}) = now(Timestamp{Nanosecond}, UTC)
function now(::Type{Timestamp{P}}, ::Type{UTC}) where {P}
    ts = clock_realtime()
    scale = timestamp_scale(P)
    return Timestamp{P}(UTInstant(P(ts.sec * (1000000000 ÷ scale) + fld(ts.nsec, scale))))
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

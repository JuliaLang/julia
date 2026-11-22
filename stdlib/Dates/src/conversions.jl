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
    Timestamp{P}(UTInstant(P(timestamp_ticks(P, widen(value(dt)) * widen(timestamp_scale(Q))))))

function Base.convert(::Type{DateTime}, dt::Date)
    ms, wraps = Base.Checked.mul_with_overflow(value(dt), MS_PER_DAY)
    wraps && Core.throw_inexacterror(:convert, DateTime, dt)
    return DateTime(UTM(ms))
end
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
    DateTime(UTM(Int64(fld(widen(value(dt)) * widen(timestamp_scale(P)), 1000000) + UNIXEPOCH)))
Base.convert(::Type{Date}, dt::Timestamp) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::Timestamp) = Time(Nanosecond(nsofday(dt)))

Base.convert(::Type{DateTime},x::Millisecond)  = DateTime(Dates.UTInstant(x))  # Converts Rata Die milliseconds to a DateTime
Base.convert(::Type{Millisecond},dt::DateTime) = Millisecond(value(dt))        # Converts DateTime to Rata Die milliseconds
Base.convert(::Type{Date},x::Day)  = Date(Dates.UTInstant(x))  # Converts Rata Die days to a Date
Base.convert(::Type{Day},dt::Date) = Day(value(dt))            # Converts Date to Rata Die days
Base.convert(::Type{Timestamp},x::Nanosecond)  = Timestamp(UTInstant(x))       # Converts Unix nanoseconds to a Timestamp
# Convert between a Timestamp and a period counted from the Unix epoch
Base.convert(::Type{P}, dt::Timestamp{Q}) where {P<:TimePeriod,Q} =
    P(timestamp_ticks(P, widen(value(dt)) * widen(timestamp_scale(Q)), timestamp_period_scale(P)))
Base.convert(::Type{Timestamp{P}}, x::Q) where {P,Q<:TimePeriod} =
    Timestamp{P}(UTInstant(P(timestamp_ticks(P, widen(value(x)) * widen(timestamp_period_scale(Q))))))

### External Conversions
const UNIXEPOCH = value(DateTime(1970)) #Rata Die milliseconds for 1970-01-01T00:00:00

"""
    unix2datetime(x::Real; localtime::Bool=false)::DateTime

Take the number of seconds since unix epoch `1970-01-01T00:00:00` (UTC) and convert to the
corresponding `DateTime`. If `localtime` is `true`, then the output is in the host
system's local time zone, otherwise it is in UTC/GMT. A result outside `DateTime` throws an
`InexactError`, while a time that the system cannot convert to local time throws an
`ArgumentError`.
"""
function unix2datetime(x::Real; localtime::Bool=false)
    # Rounding should match `now` below. Milliseconds from the Unix epoch can exceed Int64.
    ms = scaledcount(x, MS_PER_SECOND, RoundToZero)
    localtime && return localunix2datetime(x, ms)
    rata = UNIXEPOCH + ms
    typemin(Int64) <= rata <= typemax(Int64) || Core.throw_inexacterror(:unix2datetime, DateTime, x)
    return DateTime(UTM(rata % Int64))
end
# Separate, so that the conversion to UTC inlines
@noinline function localunix2datetime(x, ms)
    s, ms = divrem(ms, MS_PER_SECOND)
    # TmStruct takes its seconds as an Int
    typemin(Int) <= s <= typemax(Int) ||
        throw(ArgumentError("cannot convert $x seconds since the Unix epoch to local time"))
    tm = Libc.TmStruct(s)
    y, m, d = 1900 + Int64(tm.year), 1 + Int64(tm.month), Int64(tm.mday)
    # totaldays indexes by month, so only a valid month gives a count to check against the range
    rata = 0 < m < 13 ? rata_ms(Int128(totaldays(y, m, d)), tm.hour, tm.min, tm.sec, ms) : Int128(0)
    typemin(Int64) <= rata <= typemax(Int64) || throw(InexactError(:unix2datetime, DateTime, x))
    # TmStruct does not report a failure of localtime_r, which leaves invalid fields
    err = validargs(DateTime, y, m, d, Int64(tm.hour), Int64(tm.min), Int64(tm.sec), Int64(0))
    isnothing(err) || throw(err)
    return DateTime(UTM(rata % Int64))
end
# `unit * x` rounded to an Int128. Rounding a float to Int128 is a library call, so floats
# within Int64 round to Int64. Counts beyond `unit * typemax(Int64)` lie outside every
# DateTime, so they are clamped to that bound.
scaledcount(x::Real, unit, r) = round(Int128, Int128(unit) * x, r)
scaledcount(x::Union{Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64}, unit, r) = Int128(unit) * x
scaledcount(x::Integer, unit, r) = Int128(unit) * clampcount(x, typemax(Int64))
scaledcount(x::Rational, unit, r) = clampcount(scaledrational(x, unit, r), Int128(typemax(Int64)) * unit)
clampcount(x, m) = x > m ? m : x < -m ? -m : oftype(m, x)
# `unit * x` rounded to an integer. Numerators up to 64 bits are widened, so only wider ones
# can need a BigInt.
@inline function scaledrational(x::Rational, unit::Integer, r)
    m = numerator(x)
    m = m isa Union{Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64} ? Int128(m) : m
    n, huge = Base.Checked.mul_with_overflow(promote(m, unit)...)
    return huge ? div(big(m) * unit, denominator(x), r) : div(n, denominator(x), r)
end
function scaledcount(x::Base.IEEEFloat, unit, r)
    y = Int64(unit) * x
    # Floats below 2^63 round to integers below 2^63, so the truncation is exact
    return -0x1p63 <= y < 0x1p63 ? Int128(unsafe_trunc(Int64, round(y, r))) : round(Int128, y, r)
end

# `x - epoch`, rounded once to Float64, also when the difference overflows Int64
function epochoffset(x::Int64, epoch::Int64)
    d, wraps = Base.Checked.sub_with_overflow(x, epoch)
    return wraps ? Float64(widen(x) - epoch) : Float64(d)
end

"""
    datetime2unix(dt::DateTime)::Float64

Take the given `DateTime` and return the number of seconds
since the unix epoch `1970-01-01T00:00:00` as a [`Float64`](@ref).
"""
datetime2unix(dt::DateTime) = epochoffset(value(dt), UNIXEPOCH) / MS_PER_SECOND

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
function unix2timestamp(::Type{Timestamp{P}}, x::Rational) where {P}
    ticks = scaledrational(x, NS_PER_SECOND ÷ timestamp_scale(P), RoundToZero)
    return Timestamp{P}(UTInstant(P(convert(timestamp_count_type(P), ticks))))
end
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
    base = Timestamp{P}(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec)
    ticks = widen(value(base)) + fld(Int64(ts.nsec), timestamp_scale(P))
    return Timestamp{P}(UTInstant(P(ticks)))
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
function rata2datetime(days)
    ms = scaledcount(days isa Integer ? days : Int64(days), MS_PER_DAY, RoundToZero)
    typemin(Int64) <= ms <= typemax(Int64) || Core.throw_inexacterror(:rata2datetime, DateTime, days)
    return DateTime(UTM(ms % Int64))
end

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
    rata = JULIANEPOCH + scaledcount(f, MS_PER_DAY, RoundNearest)
    typemin(Int64) <= rata <= typemax(Int64) || Core.throw_inexacterror(:julian2datetime, DateTime, f)
    return DateTime(UTM(rata % Int64))
end

"""
    datetime2julian(dt::DateTime)::Float64

Take the given `DateTime` and return the number of Julian calendar days since the julian
epoch `-4713-11-24T12:00:00` as a [`Float64`](@ref).
"""
datetime2julian(dt::DateTime) = epochoffset(value(dt), JULIANEPOCH) / MS_PER_DAY

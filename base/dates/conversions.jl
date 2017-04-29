# This file is a part of Julia. License is MIT: http://julialang.org/license

# Conversion/Promotion

"""
    Date(dt::DateTime) -> Date

Converts a `DateTime` to a `Date`. The hour, minute, second, and millisecond parts of
the `DateTime` are truncated, so only the year, month and day parts are used in
construction.
"""
Date(dt::TimeType) = convert(Date, dt)

"""
    DateTime(dt::Date) -> DateTime

Converts a `Date` to a `DateTime`. The hour, minute, second, and millisecond parts of
the new `DateTime` are assumed to be zero.
"""
DateTime(dt::TimeType) = convert(DateTime, dt)

"""
    Time(dt::DateTime) -> Time

Converts a `DateTime` to a `Time`. The hour, minute, second, and millisecond parts of
the `DateTime` are used to create the new `Time`. Microsecond and nanoseconds are zero by default.
"""
Time(dt::DateTime) = convert(Time, dt)

Base.convert(::Type{DateTime}, dt::Date) = DateTime(UTM(value(dt) * 86400000))
Base.convert(::Type{Date}, dt::DateTime) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::DateTime) = Time(Nanosecond((value(dt) % 86400000) * 1000000))

Base.convert(::Type{DateTime},x::Millisecond)  = DateTime(Dates.UTInstant(x))  # Converts Rata Die milliseconds to a DateTime
Base.convert(::Type{Millisecond},dt::DateTime) = Millisecond(value(dt))        # Converts DateTime to Rata Die milliseconds
Base.convert(::Type{Date},x::Day)  = Date(Dates.UTInstant(x))  # Converts Rata Die days to a Date
Base.convert(::Type{Day},dt::Date) = Day(value(dt))            # Converts Date to Rata Die days

### External Conversions
const UNIXEPOCH = value(DateTime(1970)) #Rata Die milliseconds for 1970-01-01T00:00:00

"""
    unix2datetime(x) -> DateTime

Takes the number of seconds since unix epoch `1970-01-01T00:00:00` and converts to the
corresponding `DateTime`.
"""
function unix2datetime(x)
    rata = UNIXEPOCH + round(Int64, Int64(1000) * x)
    return DateTime(UTM(rata))
end
"""
    datetime2unix(dt::DateTime) -> Float64

Takes the given `DateTime` and returns the number of seconds
since the unix epoch `1970-01-01T00:00:00` as a `Float64`.
"""
datetime2unix(dt::DateTime) = (value(dt) - UNIXEPOCH) / 1000.0

"""
    now() -> DateTime

Returns a `DateTime` corresponding to the user's system time including the system timezone
locale.
"""
function now()
    tv = Libc.TimeVal()
    tm = Libc.TmStruct(tv.sec)
    return DateTime(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec, div(tv.usec, 1000))
end

"""
    today() -> Date

Returns the date portion of `now()`.
"""
today() = Date(now())

"""
    now(::Type{UTC}) -> DateTime

Returns a `DateTime` corresponding to the user's system time as UTC/GMT.
"""
now(::Type{UTC}) = unix2datetime(time())

"""
    rata2datetime(days) -> DateTime

Takes the number of Rata Die days since epoch `0000-12-31T00:00:00` and returns the
corresponding `DateTime`.
"""
rata2datetime(days) = DateTime(yearmonthday(days)...)

"""
    datetime2rata(dt::TimeType) -> Int64

Returns the number of Rata Die days since epoch from the given `Date` or `DateTime`.
"""
datetime2rata(dt::TimeType) = days(dt)

# Julian conversions
const JULIANEPOCH = value(DateTime(-4713, 11, 24, 12))

"""
    julian2datetime(julian_days) -> DateTime

Takes the number of Julian calendar days since epoch `-4713-11-24T12:00:00` and returns the
corresponding `DateTime`.
"""
function julian2datetime(f)
    rata = JULIANEPOCH + round(Int64, Int64(86400000) * f)
    return DateTime(UTM(rata))
end

"""
    datetime2julian(dt::DateTime) -> Float64

Takes the given `DateTime` and returns the number of Julian calendar days since the julian
epoch `-4713-11-24T12:00:00` as a `Float64`.
"""
datetime2julian(dt::DateTime) = (value(dt) - JULIANEPOCH) / 86400000.0

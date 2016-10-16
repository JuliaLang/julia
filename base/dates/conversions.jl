# This file is a part of Julia. License is MIT: http://julialang.org/license

# Conversion/Promotion

"""
    Date(dt::DateTime) -> Date

Converts a `DateTime` to a `Date`. The hour, minute, second, and millisecond parts of
the `DateTime` are truncated, so only the year, month and day parts are used in
construction.
"""
Date(dt::TimeType) = convert(Date,dt)

"""
    DateTime(dt::Date) -> DateTime

Converts a `Date` to a `DateTime`. The hour, minute, second, and millisecond parts of
the new `DateTime` are assumed to be zero.
"""
DateTime(dt::TimeType) = convert(DateTime,dt)

Base.convert(::Type{DateTime},dt::Date) = DateTime(UTM(value(dt)*86400000))
Base.convert(::Type{Date},dt::DateTime) = Date(UTD(days(dt)))

"""
    convert{T<:Real}(::Type{Date}, x::T) -> Date
Converts a number of type `T` to a Date. `x` should be the number of Rata Die days since epoch.
See `convert(Int64,dt::Date)` for inverse.
"""
Base.convert{R<:Real}(::Type{Date}, x::R) = Date(UTD(x))


ComputerTime(sec::Int) = ComputerTime(sec, 0)

function Base.convert(::Type{DateTime}, ht::HumanTime)
   DateTime(ht.year+1900,ht.month+1,ht.mday,ht.hour,ht.min,ht.sec)
end

function Base.convert(::Type{DateTime}, ht::ComputerTime)
   DateTime(ht.year+1900,ht.month+1,ht.mday,ht.hour,ht.min,ht.sec)
end

function Base.convert(::Type{HumanTime}, ct::ComputerTime)
    sec = ct.sec
    ht = HumanTime()
    # TODO: add support for UTC via gmtime_r()
    ccall(:localtime_r, Ptr{HumanTime}, (Ptr{Int}, Ptr{HumanTime}), &sec, &ht)
    return tm
end

const UNIXEPOCH = value(DateTime(1970)) #Rata Die milliseconds for 1970-01-01T00:00:00

# Note, there is a timezone issue here: DateTimes are in local time, but ComputerTimes need to be converted to UTC
Base.convert(::Type{ComputerTime}, dt::DateTime) = ComputerTime( (value(dt) - UNIXEPOCH)/1000.0 )

"""
    now() -> DateTime

Returns a `DateTime` corresponding to the user's system time including the system timezone
locale.
"""
function now() = convert(DateTime, Libc.TimeVal() )

"""
    today() -> Date

Returns the date portion of `now()`.
"""
today() = Date(now())

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
const JULIANEPOCH = value(DateTime(-4713,11,24,12))

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
datetime2julian(dt::DateTime) = (value(dt) - JULIANEPOCH)/86400000.0

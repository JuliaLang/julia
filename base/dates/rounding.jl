# This file is a part of Julia. License is MIT: http://julialang.org/license

# The epochs used for date rounding are based ISO 8601's "year zero" notation
const DATEEPOCH = value(Date(0))
const DATETIMEEPOCH = value(DateTime(0))

# According to ISO 8601, the first day of the first week of year 0000 is 0000-01-03
const WEEKEPOCH = value(Date(0, 1, 3))

"""
    epochdays2date(days) -> Date

Takes the number of days since the rounding epoch (`0000-01-01T00:00:00`) and returns the
corresponding `Date`.
"""
epochdays2date(i) = Date(UTD(DATEEPOCH + Int64(i)))

"""
    epochms2datetime(milliseconds) -> DateTime

Takes the number of milliseconds since the rounding epoch (`0000-01-01T00:00:00`) and
returns the corresponding `DateTime`.
"""
epochms2datetime(i) = DateTime(UTM(DATETIMEEPOCH + Int64(i)))

"""
    date2epochdays(dt::Date) -> Int64

Takes the given `Date` and returns the number of days since the rounding epoch
(`0000-01-01T00:00:00`) as an `Int64`.
"""
date2epochdays(dt::Date) = value(dt) - DATEEPOCH

"""
    datetime2epochms(dt::DateTime) -> Int64

Takes the given `DateTime` and returns the number of milliseconds since the rounding epoch
(`0000-01-01T00:00:00`) as an `Int64`.
"""
datetime2epochms(dt::DateTime) = value(dt) - DATETIMEEPOCH

function Base.floor(dt::Date, p::Year)
    value(p) < 1 && throw(DomainError())
    years = year(dt)
    return Date(years - mod(years, value(p)))
end

function Base.floor(dt::Date, p::Month)
    value(p) < 1 && throw(DomainError())
    y, m = yearmonth(dt)
    months_since_epoch = y * 12 + m - 1
    month_offset = months_since_epoch - mod(months_since_epoch, value(p))
    target_month = mod(month_offset, 12) + 1
    target_year = div(month_offset, 12) - (month_offset < 0 && target_month != 1)
    return Date(target_year, target_month)
end

function Base.floor(dt::Date, p::Week)
    value(p) < 1 && throw(DomainError())
    days = value(dt) - WEEKEPOCH
    days = days - mod(days, value(Day(p)))
    return Date(UTD(WEEKEPOCH + Int64(days)))
end

function Base.floor(dt::Date, p::Day)
    value(p) < 1 && throw(DomainError())
    days = date2epochdays(dt)
    return epochdays2date(days - mod(days, value(p)))
end

Base.floor(dt::DateTime, p::DatePeriod) = DateTime(Base.floor(Date(dt), p))

function Base.floor(dt::DateTime, p::TimePeriod)
    value(p) < 1 && throw(DomainError())
    milliseconds = datetime2epochms(dt)
    return epochms2datetime(milliseconds - mod(milliseconds, value(Millisecond(p))))
end

"""
    floor(dt::TimeType, p::Period) -> TimeType

Returns the nearest `Date` or `DateTime` less than or equal to `dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `floor(dt, Dates.Hour)` is a shortcut
for `floor(dt, Dates.Hour(1))`.

```jldoctest
julia> floor(Date(1985, 8, 16), Dates.Month)
1985-08-01

julia> floor(DateTime(2013, 2, 13, 0, 31, 20), Dates.Minute(15))
2013-02-13T00:30:00

julia> floor(DateTime(2016, 8, 6, 12, 0, 0), Dates.Day)
2016-08-06T00:00:00
```
"""
Base.floor(::Dates.TimeType, ::Dates.Period)

"""
    ceil(dt::TimeType, p::Period) -> TimeType

Returns the nearest `Date` or `DateTime` greater than or equal to `dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `ceil(dt, Dates.Hour)` is a shortcut
for `ceil(dt, Dates.Hour(1))`.

```jldoctest
julia> ceil(Date(1985, 8, 16), Dates.Month)
1985-09-01

julia> ceil(DateTime(2013, 2, 13, 0, 31, 20), Dates.Minute(15))
2013-02-13T00:45:00

julia> ceil(DateTime(2016, 8, 6, 12, 0, 0), Dates.Day)
2016-08-07T00:00:00
```
"""
function Base.ceil(dt::TimeType, p::Period)
    f = floor(dt, p)
    return (dt == f) ? f : f + p
end

"""
    floorceil(dt::TimeType, p::Period) -> (TimeType, TimeType)

Simultaneously return the `floor` and `ceil` of a `Date` or `DateTime` at resolution `p`.
More efficient than calling both `floor` and `ceil` individually.
"""
function floorceil(dt::TimeType, p::Period)
    f = floor(dt, p)
    return f, (dt == f) ? f : f + p
end

"""
    round(dt::TimeType, p::Period, [r::RoundingMode]) -> TimeType

Returns the `Date` or `DateTime` nearest to `dt` at resolution `p`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 9:30 to the nearest hour) will be rounded up.

For convenience, `p` may be a type instead of a value: `round(dt, Dates.Hour)` is a shortcut
for `round(dt, Dates.Hour(1))`.

```jldoctest
julia> round(Date(1985, 8, 16), Dates.Month)
1985-08-01

julia> round(DateTime(2013, 2, 13, 0, 31, 20), Dates.Minute(15))
2013-02-13T00:30:00

julia> round(DateTime(2016, 8, 6, 12, 0, 0), Dates.Day)
2016-08-07T00:00:00
```

Valid rounding modes for `round(::TimeType, ::Period, ::RoundingMode)` are
`RoundNearestTiesUp` (default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).
"""
function Base.round(dt::TimeType, p::Period, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(dt, p)
    return (dt - f) < (c - dt) ? f : c
end

Base.round(dt::TimeType, p::Period, r::RoundingMode{:Down}) = Base.floor(dt, p)
Base.round(dt::TimeType, p::Period, r::RoundingMode{:Up}) = Base.ceil(dt, p)

# No implementation of other `RoundingMode`s: rounding to nearest "even" is skipped because
# "even" is not defined for Period; rounding toward/away from zero is skipped because ISO
# 8601's year 0000 is not really "zero".
Base.round(::TimeType, ::Period, ::RoundingMode) = throw(DomainError())

# Default to RoundNearestTiesUp.
Base.round(dt::TimeType, p::Period) = Base.round(dt, p, RoundNearestTiesUp)

# Make rounding functions callable using Period types in addition to values.
Base.floor{T <: Period}(dt::TimeType, p::Type{T}) = Base.floor(dt, p(1))
Base.ceil{T <: Period}(dt::TimeType, p::Type{T}) = Base.ceil(dt, p(1))

function Base.round{T<:Period}(dt::TimeType, p::Type{T}, r::RoundingMode=RoundNearestTiesUp)
    return Base.round(dt, p(1), r)
end

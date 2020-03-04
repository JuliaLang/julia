# This file is a part of Julia. License is MIT: https://julialang.org/license

# The epochs used for date rounding are based ISO 8601's "year zero" notation
const DATEEPOCH = value(Date(0))
const DATETIMEEPOCH = value(DateTime(0))

# According to ISO 8601, the first day of the first week of year 0000 is 0000-01-03
const WEEKEPOCH = value(Date(0, 1, 3))

const ConvertiblePeriod = Union{TimePeriod, Week, Day}
const TimeTypeOrPeriod = Union{TimeType, ConvertiblePeriod}

"""
    epochdays2date(days) -> Date

Take the number of days since the rounding epoch (`0000-01-01T00:00:00`) and return the
corresponding `Date`.
"""
epochdays2date(i) = Date(UTD(DATEEPOCH + Int64(i)))

"""
    epochms2datetime(milliseconds) -> DateTime

Take the number of milliseconds since the rounding epoch (`0000-01-01T00:00:00`) and
return the corresponding `DateTime`.
"""
epochms2datetime(i) = DateTime(UTM(DATETIMEEPOCH + Int64(i)))

"""
    date2epochdays(dt::Date) -> Int64

Take the given `Date` and return the number of days since the rounding epoch
(`0000-01-01T00:00:00`) as an [`Int64`](@ref).
"""
date2epochdays(dt::Date) = value(dt) - DATEEPOCH

"""
    datetime2epochms(dt::DateTime) -> Int64

Take the given `DateTime` and return the number of milliseconds since the rounding epoch
(`0000-01-01T00:00:00`) as an [`Int64`](@ref).
"""
datetime2epochms(dt::DateTime) = value(dt) - DATETIMEEPOCH

function Base.floor(p::Year, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    years = year(dt)
    return Date(years - mod(years, value(p)))
end

function Base.floor(p::Month, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    y, m = yearmonth(dt)
    months_since_epoch = y * 12 + m - 1
    month_offset = months_since_epoch - mod(months_since_epoch, value(p))
    target_month = mod(month_offset, 12) + 1
    target_year = div(month_offset, 12) - (month_offset < 0 && target_month != 1)
    return Date(target_year, target_month)
end

function Base.floor(p::Week, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    days = value(dt) - WEEKEPOCH
    days = days - mod(days, value(Day(p)))
    return Date(UTD(WEEKEPOCH + Int64(days)))
end

function Base.floor(p::Day, dt::Date, )
    value(p) < 1 && throw(DomainError(p))
    days = date2epochdays(dt)
    return epochdays2date(days - mod(days, value(p)))
end

Base.floor(p::DatePeriod, dt::DateTime) = DateTime(Base.floor(p, Date(dt)))

function Base.floor(p::TimePeriod, dt::DateTime)
    value(p) < 1 && throw(DomainError(p))
    milliseconds = datetime2epochms(dt)
    return epochms2datetime(milliseconds - mod(milliseconds, value(Millisecond(p))))
end

"""
    floor(precision::T, x::Period) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` down to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`.

For convenience, `precision` may be a type instead of a value: `floor(Dates.Hour, x)` is a
shortcut for `floor(Dates.Hour(1), x)`.

```jldoctest
julia> floor(Dates.Week, Dates.Day(16))
2 weeks

julia> floor(Dates.Minute(15), Dates.Minute(44))
30 minutes

julia> floor(Dates.Day, Dates.Hour(36))
1 day
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.floor(precision::T, x::ConvertiblePeriod) where T <: ConvertiblePeriod
    value(precision) < 1 && throw(DomainError(precision))
    _x, _precision = promote(x, precision)
    return T(_x - mod(_x, _precision))
end

"""
    floor(p::Period, dt::TimeType) -> TimeType

Return the nearest `Date` or `DateTime` less than or equal to `dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `floor(Dates.Hour, dt)` is a shortcut
for `floor(Dates.Hour(1), dt)`.

```jldoctest
julia> floor(Dates.Month, Date(1985, 8, 16))
1985-08-01

julia> floor(Dates.Minute(15), DateTime(2013, 2, 13, 0, 31, 20))
2013-02-13T00:30:00

julia> floor(Dates.Day, DateTime(2016, 8, 6, 12, 0, 0))
2016-08-06T00:00:00
```
"""
Base.floor(::Dates.Period, ::Dates.TimeType)


"""
    floor(dt::TimeType, p::Period) -> TimeType
Calls floor(p::Period, dt::TimeType)
"""
function Base.floor(dt::TimeType, p::Period)
    return Base.floor(p, dt)
end

"""
    ceil(p::Period, dt::TimeType) -> TimeType

Return the nearest `Date` or `DateTime` greater than or equal to `dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `ceil(Dates.Hour, dt)` is a shortcut
for `ceil(Dates.Hour(1), dt)`.

```jldoctest
julia> ceil(Dates.Month, Date(1985, 8, 16))
1985-09-01

julia> ceil(Dates.Minute(15), DateTime(2013, 2, 13, 0, 31, 20))
2013-02-13T00:45:00

julia> ceil(Dates.Day, DateTime(2016, 8, 6, 12, 0, 0))
2016-08-07T00:00:00
```
"""
function Base.ceil(p::Period, dt::TimeType)
    f = floor(p, dt)
    return (dt == f) ? f : f + p
end

"""
     ceil(dt::TimeType, p::Period) -> TimeType
Calls ceil(p::Period, dt::TimeType)
"""
function Base.ceil(dt::TimeType, p::Period)
    return Base.ceil(p, dt)
end

"""
    ceil(precision::T, x::Period) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` up to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`.

For convenience, `precision` may be a type instead of a value: `ceil(Dates.Hour, x)` is a
shortcut for `ceil(Dates.Hour(1), x)`.

```jldoctest
julia> ceil(Dates.Week, Dates.Day(16))
3 weeks

julia> ceil(Dates.Minute(15), Dates.Minute(44))
45 minutes

julia> ceil(Dates.Day, Dates.Hour(36))
2 days
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.ceil(precision::ConvertiblePeriod, x::ConvertiblePeriod)
    f = floor(precision, x)
    return (x == f) ? f : f + precision
end

"""
    floorceil(p::Period, dt::TimeType) -> (TimeType, TimeType)

Simultaneously return the `floor` and `ceil` of a `Date` or `DateTime` at resolution `p`.
More efficient than calling both `floor` and `ceil` individually.
"""
function floorceil(p::Period, dt::TimeType)
    f = floor(p, dt)
    return f, (dt == f) ? f : f + p
end

function floorceil(dt::TimeType, p::Period)
    return floorceil(p, dt)
end

"""
    floorceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> (T, T)

Simultaneously return the `floor` and `ceil` of `Period` at resolution `p`.  More efficient
than calling both `floor` and `ceil` individually.
"""
function floorceil(precision::ConvertiblePeriod, x::ConvertiblePeriod)
    f = floor(precision, x)
    return f, (x == f) ? f : f + precision
end

"""
    round(p::Period, dt::TimeType, [r::RoundingMode]) -> TimeType

Return the `Date` or `DateTime` nearest to `dt` at resolution `p`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 9:30 to the nearest hour) will be rounded up.

For convenience, `p` may be a type instead of a value: `round(Dates.Hour, dt)` is a shortcut
for `round(Dates.Hour(1), dt)`.

```jldoctest
julia> round(Dates.Month, Date(1985, 8, 16))
1985-08-01

julia> round(Dates.Minute(15), DateTime(2013, 2, 13, 0, 31, 20))
2013-02-13T00:30:00

julia> round(Dates.Day, DateTime(2016, 8, 6, 12, 0, 0))
2016-08-07T00:00:00
```

Valid rounding modes for `round(::Period, ::TimeType, ::RoundingMode)` are
`RoundNearestTiesUp` (default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).
"""
function Base.round(p::Period, dt::TimeType, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(p, dt)
    return (dt - f) < (c - dt) ? f : c
end

"""
    round(dt::TimeType, p::Period, [r::RoundingMode]) -> TimeType
"""
function Base.round(dt::TimeType, p::Period, r::RoundingMode{:NearestTiesUp})
    return Base.round(p, dt, r)
end

"""
    round(precision::T, x::Period, [r::RoundingMode]) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 90 minutes to the nearest hour) will be rounded
up.

For convenience, `precision` may be a type instead of a value: `round(Dates.Hour, x)` is a
shortcut for `round(Dates.Hour(1), x)`.

```jldoctest
julia> round(Dates.Week, Dates.Day(16))
2 weeks

julia> round(Dates.Minute(15), Dates.Minute(44))
45 minutes

julia> round(Dates.Day, Dates.Hour(36))
2 days
```

Valid rounding modes for `round(::T, ::Period, ::RoundingMode)` are `RoundNearestTiesUp`
(default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.round(precision::ConvertiblePeriod, x::ConvertiblePeriod, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(precision, x)
    _x, _f, _c = promote(x, f, c)
    return (_x - _f) < (_c - _x) ? f : c
end

Base.round(p::Period, x::TimeTypeOrPeriod, r::RoundingMode{:Down}) = Base.floor(p, x)
Base.round(p::Period, x::TimeTypeOrPeriod, r::RoundingMode{:Up}) = Base.ceil(p, x)

# No implementation of other `RoundingMode`s: rounding to nearest "even" is skipped because
# "even" is not defined for Period; rounding toward/away from zero is skipped because ISO
# 8601's year 0000 is not really "zero".
Base.round(p::Period, ::TimeTypeOrPeriod, ::RoundingMode) = throw(DomainError(p))

# Default to RoundNearestTiesUp.
Base.round(p::Period, x::TimeTypeOrPeriod) = Base.round(p, x, RoundNearestTiesUp)

# Make rounding functions callable using Period types in addition to values.
Base.floor(::Type{P}, x::TimeTypeOrPeriod) where P <: Period = Base.floor(oneunit(P), x)

Base.ceil(::Type{P}, x::TimeTypeOrPeriod) where P <: Period = Base.ceil(oneunit(P), x)

function Base.round(::Type{P}, x::TimeTypeOrPeriod, r::RoundingMode=RoundNearestTiesUp) where P <: Period
    return Base.round(oneunit(P), x, r)
end

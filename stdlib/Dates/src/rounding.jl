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

@depreciate Base.floor(dt::Date, p::Year) = Base.floor(p, dt)
function Base.floor(p::Year, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    years = year(dt)
    return Date(years - mod(years, value(p)))
end

@deprecate Base.floor(dt::Date, p::Month) = Base.floor(p, dt)
function Base.floor(p::Month, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    y, m = yearmonth(dt)
    months_since_epoch = y * 12 + m - 1
    month_offset = months_since_epoch - mod(months_since_epoch, value(p))
    target_month = mod(month_offset, 12) + 1
    target_year = div(month_offset, 12) - (month_offset < 0 && target_month != 1)
    return Date(target_year, target_month)
end

@deprecate Base.floor(dt::Date, p::Quarter) = Base.floor(p, dt)
function Base.floor(p::Quarter, dt::Date)
    return floor(dt, Month(p))
end

@deprecate Base.floor(dt::Date, p::Week) = Base.floor(p, dt)
function Base.floor(p::Week, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    days = value(dt) - WEEKEPOCH
    days = days - mod(days, value(Day(p)))
    return Date(UTD(WEEKEPOCH + Int64(days)))
end

@deprecate Base.floor(dt::Date, p::Day) = Base.floor(p, dt)
function Base.floor(p::Day, dt::Date)
    value(p) < 1 && throw(DomainError(p))
    days = date2epochdays(dt)
    return epochdays2date(days - mod(days, value(p)))
end

@deprecate Base.floor(dt::DateTime, p::DatePeriod) = Base.floor(p, dt)
Base.floor(p::DatePeriod, dt::DateTime) = DateTime(Base.floor(p, Date(dt)))

@deprecate Base.floor(dt::DateTime, p::TimePeriod) = Base.floor(p, dt)
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
julia> floor(Week, Day(16))
2 weeks

julia> floor(Minute(15), Minute(44))
30 minutes

julia> floor(Day, Hour(36))
1 day
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
@deprecate Base.floor(x::ConvertiblePeriod, precision::T) = Base.floor(precision, x)
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
julia> floor(Month, Date(1985, 8, 16))
1985-08-01

julia> floor(Minute(15), DateTime(2013, 2, 13, 0, 31, 20))
2013-02-13T00:30:00

julia> floor(Day, DateTime(2016, 8, 6, 12, 0, 0))
2016-08-06T00:00:00
```
"""
Base.floor(::Dates.Period, ::Dates.TimeType)

"""
    ceil(dt::TimeType, p::Period) -> TimeType

Return the nearest `Date` or `DateTime` greater than or equal to `dt` at resolution `p`.

For convenience, `p` may be a type instead of a value: `ceil(dt, Dates.Hour)` is a shortcut
for `ceil(dt, Dates.Hour(1))`.

```jldoctest
julia> ceil(Date(1985, 8, 16), Month)
1985-09-01

julia> ceil(DateTime(2013, 2, 13, 0, 31, 20), Minute(15))
2013-02-13T00:45:00

julia> ceil(DateTime(2016, 8, 6, 12, 0, 0), Day)
2016-08-07T00:00:00
```
"""
function Base.ceil(dt::TimeType, p::Period)
    f = floor(dt, p)
    return (dt == f) ? f : f + p
end

"""
    ceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` up to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`.

For convenience, `precision` may be a type instead of a value: `ceil(x, Dates.Hour)` is a
shortcut for `ceil(x, Dates.Hour(1))`.

```jldoctest
julia> ceil(Day(16), Week)
3 weeks

julia> ceil(Minute(44), Minute(15))
45 minutes

julia> ceil(Hour(36), Day)
2 days
```

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.ceil(x::ConvertiblePeriod, precision::ConvertiblePeriod)
    f = floor(x, precision)
    return (x == f) ? f : f + precision
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
    floorceil(x::Period, precision::T) where T <: Union{TimePeriod, Week, Day} -> (T, T)

Simultaneously return the `floor` and `ceil` of `Period` at resolution `p`.  More efficient
than calling both `floor` and `ceil` individually.
"""
function floorceil(x::ConvertiblePeriod, precision::ConvertiblePeriod)
    f = floor(x, precision)
    return f, (x == f) ? f : f + precision
end

"""
    round(dt::TimeType, p::Period, [r::RoundingMode]) -> TimeType

Return the `Date` or `DateTime` nearest to `dt` at resolution `p`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 9:30 to the nearest hour) will be rounded up.

For convenience, `p` may be a type instead of a value: `round(dt, Dates.Hour)` is a shortcut
for `round(dt, Dates.Hour(1))`.

```jldoctest
julia> round(Date(1985, 8, 16), Month)
1985-08-01

julia> round(DateTime(2013, 2, 13, 0, 31, 20), Minute(15))
2013-02-13T00:30:00

julia> round(DateTime(2016, 8, 6, 12, 0, 0), Day)
2016-08-07T00:00:00
```

Valid rounding modes for `round(::TimeType, ::Period, ::RoundingMode)` are
`RoundNearestTiesUp` (default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).
"""
function Base.round(dt::TimeType, p::Period, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(dt, p)
    return (dt - f) < (c - dt) ? f : c
end

"""
    round(x::Period, precision::T, [r::RoundingMode]) where T <: Union{TimePeriod, Week, Day} -> T

Round `x` to the nearest multiple of `precision`. If `x` and `precision` are different
subtypes of `Period`, the return value will have the same type as `precision`. By default
(`RoundNearestTiesUp`), ties (e.g., rounding 90 minutes to the nearest hour) will be rounded
up.

For convenience, `precision` may be a type instead of a value: `round(x, Dates.Hour)` is a
shortcut for `round(x, Dates.Hour(1))`.

```jldoctest
julia> round(Day(16), Week)
2 weeks

julia> round(Minute(44), Minute(15))
45 minutes

julia> round(Hour(36), Day)
2 days
```

Valid rounding modes for `round(::Period, ::T, ::RoundingMode)` are `RoundNearestTiesUp`
(default), `RoundDown` (`floor`), and `RoundUp` (`ceil`).

Rounding to a `precision` of `Month`s or `Year`s is not supported, as these `Period`s are of
inconsistent length.
"""
function Base.round(x::ConvertiblePeriod, precision::ConvertiblePeriod, r::RoundingMode{:NearestTiesUp})
    f, c = floorceil(x, precision)
    _x, _f, _c = promote(x, f, c)
    return (_x - _f) < (_c - _x) ? f : c
end

Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Down}) = Base.floor(x, p)
Base.round(x::TimeTypeOrPeriod, p::Period, r::RoundingMode{:Up}) = Base.ceil(x, p)

# No implementation of other `RoundingMode`s: rounding to nearest "even" is skipped because
# "even" is not defined for Period; rounding toward/away from zero is skipped because ISO
# 8601's year 0000 is not really "zero".
Base.round(::TimeTypeOrPeriod, p::Period, ::RoundingMode) = throw(DomainError(p))

# Default to RoundNearestTiesUp.
Base.round(x::TimeTypeOrPeriod, p::Period) = Base.round(x, p, RoundNearestTiesUp)

# Make rounding functions callable using Period types in addition to values.
Base.floor(x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.floor(x, oneunit(P))
Base.ceil(x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.ceil(x, oneunit(P))
Base.floor(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.floor(Date(x), oneunit(P))
Base.ceil(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}) where P <: Period = Base.ceil(Date(x), oneunit(P))

function Base.round(x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period
    return Base.round(x, oneunit(P), r)
end

function Base.round(::Type{Date}, x::TimeTypeOrPeriod, ::Type{P}, r::RoundingMode=RoundNearestTiesUp) where P <: Period
    return Base.round(Date(x), oneunit(P), r)
end

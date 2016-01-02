# This file is a part of Julia. License is MIT: http://julialang.org/license

# Dates

"""
    DateTime(f::Function, y[, m, d, h, mi, s]; step=Day(1), negate=false, limit=10000) -> DateTime

Create a `DateTime` through the adjuster API. The starting point will be constructed from
the provided `y, m, d...` arguments, and will be adjusted until `f::Function` returns
`true`. The step size in adjusting can be provided manually through the `step` keyword. If
`negate=true`, then the adjusting will stop when `f::Function` returns `false` instead of
`true`. `limit` provides a limit to the max number of iterations the adjustment API will
pursue before throwing an error (in the case that `f::Function` is never satisfied).
"""
Dates.DateTime(f::Function, y)

"""
    DateTime(dt::Date) -> DateTime

Converts a `Date` type to a `DateTime`. The hour, minute, second, and millisecond parts of
the new `DateTime` are assumed to be zero.
"""
Dates.DateTime(dt::Date)

"""
    DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a `DateTime` type by parsing the `dt` date string following the pattern given in
the `format` string. The following codes can be used for constructing format strings:

| Code       | Matches   | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996, 96  | Returns year of 1996, 0096                                   |
| `m`        | 1, 01     | Matches 1 or 2-digit months                                  |
| `u`        | Jan       | Matches abbreviated months according to the `locale` keyword |
| `U`        | January   | Matches full month names according to the `locale` keyword   |
| `d`        | 1, 01     | Matches 1 or 2-digit days                                    |
| `H`        | 00        | Matches hours                                                |
| `M`        | 00        | Matches minutes                                              |
| `S`        | 00        | Matches seconds                                              |
| `s`        | .500      | Matches milliseconds                                         |
| `e`        | Mon, Tues | Matches abbreviated days of the week                         |
| `E`        | Monday    | Matches full name days of the week                           |
| `yyyymmdd` | 19960101  | Matches fixed-width year, month, and day                     |

All characters not listed above are treated as delimiters between date and time slots. So a
`dt` string of "1996-01-15T00:00:00.0" would have a `format` string like "y-m-dTH:M:S.s".
"""
Dates.DateTime(dt::AbstractString, format::AbstractString)

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Similar form as above for parsing a `DateTime`, but passes a `DateFormat` object instead of
a raw formatting string. It is more efficient if similarly formatted date strings will be
parsed repeatedly to first create a `DateFormat` object then use this method for parsing.
"""
Dates.DateTime(dt::AbstractString, df::Dates.DateFormat)


"""
    Date(f::Function, y[, m]; step=Day(1), negate=false, limit=10000) -> Date

Create a `Date` through the adjuster API. The starting point will be constructed from the
provided `y, m` arguments, and will be adjusted until `f::Function` returns `true`. The step
size in adjusting can be provided manually through the `step` keyword. If `negate=true`,
then the adjusting will stop when `f::Function` returns `false` instead of `true`. `limit`
provides a limit to the max number of iterations the adjustment API will pursue before
throwing an error (given that `f::Function` is never satisfied).
"""
Dates.Date(f::Function, y)

"""
    Date(dt::DateTime) -> Date

Converts a `DateTime` type to a `Date`. The hour, minute, second, and millisecond parts of
the `DateTime` are truncated, so only the year, month and day parts are used in
construction.
"""
Dates.Date(dt::DateTime)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` type by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as `DateTime` above.
"""
Dates.Date(dt::AbstractString, format::AbstractString)

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Dates.Date(dt::AbstractString, df::Dates.DateFormat)


"""
    year(dt::TimeType) -> Int64
    month(dt::TimeType) -> Int64
    week(dt::TimeType) -> Int64
    day(dt::TimeType) -> Int64
    hour(dt::TimeType) -> Int64
    minute(dt::TimeType) -> Int64
    second(dt::TimeType) -> Int64
    millisecond(dt::TimeType) -> Int64

Return the field part of a `Date` or `DateTime` as an `Int64`.
"""
Dates.year

"""
    Year(dt::TimeType) -> Year
    Month(dt::TimeType) -> Month
    Week(dt::TimeType) -> Week
    Day(dt::TimeType) -> Day
    Hour(dt::TimeType) -> Hour
    Minute(dt::TimeType) -> Minute
    Second(dt::TimeType) -> Second
    Millisecond(dt::TimeType) -> Millisecond

Return the field part of a `Date` or `DateTime` as a `Period` type.
"""
Dates.Year(dt::Dates.TimeType)

"""
    Year(v)
    Month(v)
    Week(v)
    Day(v)
    Hour(v)
    Minute(v)
    Second(v)
    Millisecond(v)

Construct a `Period` type with the given `v` value. Input must be losslessly convertible to
an `Int64`.
"""
Dates.Year(v)

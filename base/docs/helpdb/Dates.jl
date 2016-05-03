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

Converts a `Date` to a `DateTime`. The hour, minute, second, and millisecond parts of
the new `DateTime` are assumed to be zero.
"""
Dates.DateTime(dt::Date)


"""
    DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the `format` string. The following character codes can be used to construct the `format`
string:

| Code       | Matches   | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996, 96  | Returns year of 1996, 0096                                   |
| `Y`        | 1996, 96  | Returns year of 1996, 0096. Equivalent to `y`                |
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

Characters not listed above are normally treated as delimiters between date and time slots.
For example a `dt` string of "1996-01-15T00:00:00.0" would have a `format` string like
"y-m-dTH:M:S.s". If you need to use a code character as a delimiter you can escape it using
backslash. The date "1995y01m" would have the format "y\\ym\\m".
"""
Dates.DateTime(dt::AbstractString, format::AbstractString)

"""
    format(dt::TimeType, format::AbstractString; locale="english") -> AbstractString

Construct a string by using a `TimeType` object and applying the provided `format`. The
following character codes can be used to construct the `format` string:

| Code       | Examples  | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 6         | Numeric year with a fixed width                              |
| `Y`        | 1996      | Numeric year with a minimum width                            |
| `m`        | 1, 12     | Numeric month with a minimum width                           |
| `u`        | Jan       | Month name shortened to 3-chars according to the `locale`    |
| `U`        | January   | Full month name according to the `locale` keyword            |
| `d`        | 1, 31     | Day of the month with a minimum width                        |
| `H`        | 0, 23     | Hour (24-hour clock) with a minimum width                    |
| `M`        | 0, 59     | Minute with a minimum width                                  |
| `S`        | 0, 59     | Second with a minimum width                                  |
| `s`        | 000, 500  | Millisecond with a minimum width of 3                        |
| `e`        | Mon, Tue  | Abbreviated days of the week                                 |
| `E`        | Monday    | Full day of week name                                        |

The number of sequential code characters indicate the width of the code. A format of
`yyyy-mm` specifies that the code `y` should have a width of four while `m` a width of two.
Codes that yield numeric digits have an associated mode: fixed-width or minimum-width.
The fixed-width mode left-pads the value with zeros when it is shorter than the specified
width and truncates the value when longer. Minimum-width mode works the same as fixed-width
except that it does not truncate values longer than the width.

When creating a `format` you can use any non-code characters as a separator. For example to
generate the string "1996-01-15T00:00:00" you could use `format`: "yyyy-mm-ddTHH:MM:SS".
Note that if you need to use a code character as a literal you can use the escape character
backslash. The string "1996y01m" can be produced with the format "yyyy\\ymm\\m".
"""
Dates.format(dt::Dates.TimeType, format::AbstractString)


"""
    DateFormat(format::AbstractString, locale::AbstractString="english") -> DateFormat

Construct a date formatting object that can be used for parsing date strings or
formatting a date object as a string. For details on the syntax for `format` see
[`DateTime(::AbstractString, ::AbstractString)`](:ref:`parsing <man-date-parsing>`) and
[`format`](:ref:`formatting <man-date-formatting>`).
"""
Dates.DateFormat(f::AbstractString, locale::AbstractString="english")

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](:func:`Dates.DateFormat`) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
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

Converts a `DateTime` to a `Date`. The hour, minute, second, and millisecond parts of
the `DateTime` are truncated, so only the year, month and day parts are used in
construction.
"""
Dates.Date(dt::DateTime)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Dates.Date(dt::AbstractString, format::AbstractString)

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Dates.Date(dt::AbstractString, df::Dates.DateFormat)

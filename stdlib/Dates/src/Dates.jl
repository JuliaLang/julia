# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Dates

The `Dates` module provides `Date`, `DateTime`, `Time` types, and related functions.

The types are not aware of time zones, based on UT seconds
(86400 seconds a day, avoiding leap seconds), and
use the proleptic Gregorian calendar, as specified in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601).
For time zone functionality, see the TimeZones.jl package.

```jldoctest
julia> dt = DateTime(2017,12,31,23,59,59,999)
2017-12-31T23:59:59.999

julia> d1 = Date(Dates.Month(12), Dates.Year(2017))
2017-12-01

julia> d2 = Date("2017-12-31", Dates.DateFormat("y-m-d"))
2017-12-31

julia> Dates.yearmonthday(d2)
(2017, 12, 31)

julia> d2-d1
30 days
```

Please see the manual section on [`Date`](@ref) and [`DateTime`](@ref)
for more information.
"""
module Dates

import Base: ==, div, fld, mod, rem, gcd, lcm, +, -, *, /, %, broadcast
using Printf: @sprintf

using Base.Iterators

include("types.jl")
include("periods.jl")
include("accessors.jl")
include("query.jl")
include("arithmetic.jl")
include("conversions.jl")
include("ranges.jl")
include("adjusters.jl")
include("rounding.jl")
include("io.jl")
include("parse.jl")
include("deprecated.jl")

export Period, DatePeriod, TimePeriod,
       Year, Month, Week, Day, Hour, Minute, Second, Millisecond,
       Microsecond, Nanosecond,
       TimeZone, UTC, TimeType, DateTime, Date, Time,
       # periods.jl
       canonicalize,
       # accessors.jl
       yearmonthday, yearmonth, monthday, year, month, week, day,
       hour, minute, second, millisecond, dayofmonth,
       microsecond, nanosecond,
       # query.jl
       dayofweek, isleapyear, daysinmonth, daysinyear, dayofyear, dayname, dayabbr,
       dayofweekofmonth, daysofweekinmonth, monthname, monthabbr,
       quarterofyear, dayofquarter,
       Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday,
       Mon, Tue, Wed, Thu, Fri, Sat, Sun,
       January, February, March, April, May, June,
       July, August, September, October, November, December,
       Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec,
       # conversions.jl
       unix2datetime, datetime2unix, now, today,
       rata2datetime, datetime2rata, julian2datetime, datetime2julian,
       # adjusters.jl
       firstdayofweek, lastdayofweek,
       firstdayofmonth, lastdayofmonth,
       firstdayofyear, lastdayofyear,
       firstdayofquarter, lastdayofquarter,
       adjust, tonext, toprev, tofirst, tolast,
       # io.jl
       ISODateTimeFormat, ISODateFormat, ISOTimeFormat, DateFormat, RFC1123Format, @dateformat_str

end # module

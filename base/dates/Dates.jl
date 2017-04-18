# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
The Dates module provides `Date`, `DateTime`, `Time` types, and related funtions.

The types are *timezone-unaware*, based on UT seconds (1/86400 of a day),
and the proleptic Gregorian calendar, as specified in ISO 8601.
For time zone functionality, see the TimeZones.jl package.

Try
`dt = DateTime(2017,12,31,23,59,59,999)`,
`d1 = Date(Dates.Month(12), Dates.Year(2017))`,
`d2 = Date("2017-12-31", Dates.DateFormat("y-m-d"))`,
`Dates.yearmonthday(d2)`,
`d2-d1`.

Please see the docs for Date and DateTime for more information.
"""
module Dates

importall ..Base.Operators
import ..Base.broadcast

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
       ISODateTimeFormat, ISODateFormat, DateFormat, RFC1123Format, @dateformat_str

end # module

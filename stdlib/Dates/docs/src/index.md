# Dates

```@meta
DocTestSetup = :(using Dates)
```

The `Dates` module provides two types for working with dates: [`Date`](@ref) and [`DateTime`](@ref),
representing day and millisecond precision, respectively; both are subtypes of the abstract [`TimeType`](@ref).
The motivation for distinct types is simple: some operations are much simpler, both in terms of
code and mental reasoning, when the complexities of greater precision don't have to be dealt with.
For example, since the [`Date`](@ref) type only resolves to the precision of a single date (i.e.
no hours, minutes, or seconds), normal considerations for time zones, daylight savings/summer
time, and leap seconds are unnecessary and avoided.

Both [`Date`](@ref) and [`DateTime`](@ref) are basically immutable [`Int64`](@ref) wrappers.
The single `instant` field of either type is actually a `UTInstant{P}` type, which
represents a continuously increasing machine timeline based on the UT second [^1]. The
[`DateTime`](@ref) type is not aware of time zones (*naive*, in Python parlance),
analogous to a *LocalDateTime* in Java 8. Additional time zone functionality
can be added through the [TimeZones.jl package](https://github.com/JuliaTime/TimeZones.jl/), which
compiles the [IANA time zone database](http://www.iana.org/time-zones). Both [`Date`](@ref) and
[`DateTime`](@ref) are based on the [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) standard, which follows the proleptic Gregorian calendar.
One note is that the ISO 8601 standard is particular about BC/BCE dates. In general, the last
day of the BC/BCE era, 1-12-31 BC/BCE, was followed by 1-1-1 AD/CE, thus no year zero exists.
The ISO standard, however, states that 1 BC/BCE is year zero, so `0000-12-31` is the day before
`0001-01-01`, and year `-0001` (yes, negative one for the year) is 2 BC/BCE, year `-0002` is 3
BC/BCE, etc.

[^1]:
    The notion of the UT second is actually quite fundamental. There are basically two different notions
    of time generally accepted, one based on the physical rotation of the earth (one full rotation
    = 1 day), the other based on the SI second (a fixed, constant value). These are radically different!
    Think about it, a "UT second", as defined relative to the rotation of the earth, may have a different
    absolute length depending on the day! Anyway, the fact that [`Date`](@ref) and [`DateTime`](@ref)
    are based on UT seconds is a simplifying, yet honest assumption so that things like leap seconds
    and all their complexity can be avoided. This basis of time is formally called [UT](https://en.wikipedia.org/wiki/Universal_Time)
    or UT1. Basing types on the UT second basically means that every minute has 60 seconds and every
    day has 24 hours and leads to more natural calculations when working with calendar dates.

## Constructors

[`Date`](@ref) and [`DateTime`](@ref) types can be constructed by integer or [`Period`](@ref)
types, by parsing, or through adjusters (more on those later):

```jldoctest
julia> DateTime(2013)
2013-01-01T00:00:00

julia> DateTime(2013,7)
2013-07-01T00:00:00

julia> DateTime(2013,7,1)
2013-07-01T00:00:00

julia> DateTime(2013,7,1,12)
2013-07-01T12:00:00

julia> DateTime(2013,7,1,12,30)
2013-07-01T12:30:00

julia> DateTime(2013,7,1,12,30,59)
2013-07-01T12:30:59

julia> DateTime(2013,7,1,12,30,59,1)
2013-07-01T12:30:59.001

julia> Date(2013)
2013-01-01

julia> Date(2013,7)
2013-07-01

julia> Date(2013,7,1)
2013-07-01

julia> Date(Dates.Year(2013),Dates.Month(7),Dates.Day(1))
2013-07-01

julia> Date(Dates.Month(7),Dates.Year(2013))
2013-07-01
```

[`Date`](@ref) or [`DateTime`](@ref) parsing is accomplished by the use of format strings. Format
strings work by the notion of defining *delimited* or *fixed-width* "slots" that contain a period
to parse and passing the text to parse and format string to a [`Date`](@ref) or [`DateTime`](@ref)
constructor, of the form `Date("2015-01-01","y-m-d")` or `DateTime("20150101","yyyymmdd")`.

Delimited slots are marked by specifying the delimiter the parser should expect between two subsequent
periods; so `"y-m-d"` lets the parser know that between the first and second slots in a date string
like `"2014-07-16"`, it should find the `-` character. The `y`, `m`, and `d` characters let the
parser know which periods to parse in each slot.

Fixed-width slots are specified by repeating the period character the number of times corresponding
to the width with no delimiter between characters. So `"yyyymmdd"` would correspond to a date
string like `"20140716"`. The parser distinguishes a fixed-width slot by the absence of a delimiter,
noting the transition `"yyyymm"` from one period character to the next.

Support for text-form month parsing is also supported through the `u` and `U` characters, for
abbreviated and full-length month names, respectively. By default, only English month names are
supported, so `u` corresponds to "Jan", "Feb", "Mar", etc. And `U` corresponds to "January", "February",
"March", etc. Similar to other name=>value mapping functions [`dayname`](@ref) and [`monthname`](@ref),
custom locales can be loaded by passing in the `locale=>Dict{String,Int}` mapping to the `MONTHTOVALUEABBR`
and `MONTHTOVALUE` dicts for abbreviated and full-name month names, respectively.

One note on parsing performance: using the `Date(date_string,format_string)` function is fine
if only called a few times. If there are many similarly formatted date strings to parse however,
it is much more efficient to first create a [`Dates.DateFormat`](@ref), and pass it instead of
a raw format string.

```jldoctest
julia> df = DateFormat("y-m-d");

julia> dt = Date("2015-01-01",df)
2015-01-01

julia> dt2 = Date("2015-01-02",df)
2015-01-02
```

You can also use the `dateformat""` string macro. This macro creates the `DateFormat` object once when the macro is expanded and uses the same `DateFormat` object even if a code snippet is run multiple times.

```jldoctest
julia> for i = 1:10^5
           Date("2015-01-01", dateformat"y-m-d")
       end
```

A full suite of parsing and formatting tests and examples is available in [`stdlib/Dates/test/io.jl`](https://github.com/JuliaLang/julia/blob/master/stdlib/Dates/test/io.jl).

## Durations/Comparisons

Finding the length of time between two [`Date`](@ref) or [`DateTime`](@ref) is straightforward
given their underlying representation as `UTInstant{Day}` and `UTInstant{Millisecond}`, respectively.
The difference between [`Date`](@ref) is returned in the number of [`Day`](@ref), and [`DateTime`](@ref)
in the number of [`Millisecond`](@ref). Similarly, comparing [`TimeType`](@ref) is a simple matter
of comparing the underlying machine instants (which in turn compares the internal [`Int64`](@ref) values).

```jldoctest
julia> dt = Date(2012,2,29)
2012-02-29

julia> dt2 = Date(2000,2,1)
2000-02-01

julia> dump(dt)
Date
  instant: Dates.UTInstant{Day}
    periods: Day
      value: Int64 734562

julia> dump(dt2)
Date
  instant: Dates.UTInstant{Day}
    periods: Day
      value: Int64 730151

julia> dt > dt2
true

julia> dt != dt2
true

julia> dt + dt2
ERROR: MethodError: no method matching +(::Date, ::Date)
[...]

julia> dt * dt2
ERROR: MethodError: no method matching *(::Date, ::Date)
[...]

julia> dt / dt2
ERROR: MethodError: no method matching /(::Date, ::Date)

julia> dt - dt2
4411 days

julia> dt2 - dt
-4411 days

julia> dt = DateTime(2012,2,29)
2012-02-29T00:00:00

julia> dt2 = DateTime(2000,2,1)
2000-02-01T00:00:00

julia> dt - dt2
381110400000 milliseconds
```

## Accessor Functions

Because the [`Date`](@ref) and [`DateTime`](@ref) types are stored as single [`Int64`](@ref) values, date
parts or fields can be retrieved through accessor functions. The lowercase accessors return the
field as an integer:

```jldoctest tdate
julia> t = Date(2014, 1, 31)
2014-01-31

julia> Dates.year(t)
2014

julia> Dates.month(t)
1

julia> Dates.week(t)
5

julia> Dates.day(t)
31
```

While propercase return the same value in the corresponding [`Period`](@ref) type:

```jldoctest tdate
julia> Dates.Year(t)
2014 years

julia> Dates.Day(t)
31 days
```

Compound methods are provided, as they provide a measure of efficiency if multiple fields are
needed at the same time:

```jldoctest tdate
julia> Dates.yearmonth(t)
(2014, 1)

julia> Dates.monthday(t)
(1, 31)

julia> Dates.yearmonthday(t)
(2014, 1, 31)
```

One may also access the underlying `UTInstant` or integer value:

```jldoctest tdate
julia> dump(t)
Date
  instant: Dates.UTInstant{Day}
    periods: Day
      value: Int64 735264

julia> t.instant
Dates.UTInstant{Day}(Day(735264))

julia> Dates.value(t)
735264
```

## Query Functions

Query functions provide calendrical information about a [`TimeType`](@ref). They include information
about the day of the week:

```jldoctest tdate2
julia> t = Date(2014, 1, 31)
2014-01-31

julia> Dates.dayofweek(t)
5

julia> Dates.dayname(t)
"Friday"

julia> Dates.dayofweekofmonth(t) # 5th Friday of January
5
```

Month of the year:

```jldoctest tdate2
julia> Dates.monthname(t)
"January"

julia> Dates.daysinmonth(t)
31
```

As well as information about the [`TimeType`](@ref)'s year and quarter:

```jldoctest tdate2
julia> Dates.isleapyear(t)
false

julia> Dates.dayofyear(t)
31

julia> Dates.quarterofyear(t)
1

julia> Dates.dayofquarter(t)
31
```

The [`dayname`](@ref) and [`monthname`](@ref) methods can also take an optional `locale` keyword
that can be used to return the name of the day or month of the year for other languages/locales.
There are also versions of these functions returning the abbreviated names, namely
[`dayabbr`](@ref) and [`monthabbr`](@ref).
First the mapping is loaded into the `LOCALES` variable:

```jldoctest tdate2
julia> french_months = ["janvier", "février", "mars", "avril", "mai", "juin",
                        "juillet", "août", "septembre", "octobre", "novembre", "décembre"];

julia> french_monts_abbrev = ["janv","févr","mars","avril","mai","juin",
                              "juil","août","sept","oct","nov","déc"];

julia> french_days = ["lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"];

julia> Dates.LOCALES["french"] = Dates.DateLocale(french_months, french_monts_abbrev, french_days, [""]);
```

 The above mentioned functions can then be used to perform the queries:

```jldoctest tdate2
julia> Dates.dayname(t;locale="french")
"vendredi"

julia> Dates.monthname(t;locale="french")
"janvier"

julia> Dates.monthabbr(t;locale="french")
"janv"
```

Since the abbreviated versions of the days are not loaded, trying to use the
function `dayabbr` will error.

```jldoctest tdate2
julia> Dates.dayabbr(t;locale="french")
ERROR: BoundsError: attempt to access 1-element Vector{String} at index [5]
Stacktrace:
[...]
```


## TimeType-Period Arithmetic

It's good practice when using any language/date framework to be familiar with how date-period
arithmetic is handled as there are some [tricky issues](https://codeblog.jonskeet.uk/2010/12/01/the-joys-of-date-time-arithmetic/)
to deal with (though much less so for day-precision types).

The `Dates` module approach tries to follow the simple principle of trying to change as
little as possible when doing [`Period`](@ref) arithmetic. This approach is also often known as
*calendrical* arithmetic or what you would probably guess if someone were to ask you the same
calculation in a conversation. Why all the fuss about this? Let's take a classic example: add
1 month to January 31st, 2014. What's the answer? Javascript will say [March 3](https://markhneedham.com/blog/2009/01/07/javascript-add-a-month-to-a-date/)
(assumes 31 days). PHP says [March 2](https://stackoverflow.com/questions/5760262/php-adding-months-to-a-date-while-not-exceeding-the-last-day-of-the-month)
(assumes 30 days). The fact is, there is no right answer. In the `Dates` module, it gives
the result of February 28th. How does it figure that out? I like to think of the classic 7-7-7
gambling game in casinos.

Now just imagine that instead of 7-7-7, the slots are Year-Month-Day, or in our example, 2014-01-31.
When you ask to add 1 month to this date, the month slot is incremented, so now we have 2014-02-31.
Then the day number is checked if it is greater than the last valid day of the new month; if it
is (as in the case above), the day number is adjusted down to the last valid day (28). What are
the ramifications with this approach? Go ahead and add another month to our date, `2014-02-28 + Month(1) == 2014-03-28`.
What? Were you expecting the last day of March? Nope, sorry, remember the 7-7-7 slots. As few
slots as possible are going to change, so we first increment the month slot by 1, 2014-03-28,
and boom, we're done because that's a valid date. On the other hand, if we were to add 2 months
to our original date, 2014-01-31, then we end up with 2014-03-31, as expected. The other ramification
of this approach is a loss in associativity when a specific ordering is forced (i.e. adding things
in different orders results in different outcomes). For example:

```jldoctest
julia> (Date(2014,1,29)+Dates.Day(1)) + Dates.Month(1)
2014-02-28

julia> (Date(2014,1,29)+Dates.Month(1)) + Dates.Day(1)
2014-03-01
```

What's going on there? In the first line, we're adding 1 day to January 29th, which results in
2014-01-30; then we add 1 month, so we get 2014-02-30, which then adjusts down to 2014-02-28.
In the second example, we add 1 month *first*, where we get 2014-02-29, which adjusts down to
2014-02-28, and *then* add 1 day, which results in 2014-03-01. One design principle that helps
in this case is that, in the presence of multiple Periods, the operations will be ordered by the
Periods' *types*, not their value or positional order; this means `Year` will always be added
first, then `Month`, then `Week`, etc. Hence the following *does* result in associativity and
Just Works:

```jldoctest
julia> Date(2014,1,29) + Dates.Day(1) + Dates.Month(1)
2014-03-01

julia> Date(2014,1,29) + Dates.Month(1) + Dates.Day(1)
2014-03-01
```

Tricky? Perhaps. What is an innocent `Dates` user to do? The bottom line is to be aware
that explicitly forcing a certain associativity, when dealing with months, may lead to some unexpected
results, but otherwise, everything should work as expected. Thankfully, that's pretty much the
extent of the odd cases in date-period arithmetic when dealing with time in UT (avoiding the "joys"
of dealing with daylight savings, leap seconds, etc.).

As a bonus, all period arithmetic objects work directly with ranges:

```jldoctest
julia> dr = Date(2014,1,29):Day(1):Date(2014,2,3)
Date("2014-01-29"):Day(1):Date("2014-02-03")

julia> collect(dr)
6-element Vector{Date}:
 2014-01-29
 2014-01-30
 2014-01-31
 2014-02-01
 2014-02-02
 2014-02-03

julia> dr = Date(2014,1,29):Dates.Month(1):Date(2014,07,29)
Date("2014-01-29"):Month(1):Date("2014-07-29")

julia> collect(dr)
7-element Vector{Date}:
 2014-01-29
 2014-02-28
 2014-03-29
 2014-04-29
 2014-05-29
 2014-06-29
 2014-07-29
```

## Adjuster Functions

As convenient as date-period arithmetic is, often the kinds of calculations needed on dates
take on a *calendrical* or *temporal* nature rather than a fixed number of periods. Holidays are
a perfect example; most follow rules such as "Memorial Day = Last Monday of May", or "Thanksgiving
= 4th Thursday of November". These kinds of temporal expressions deal with rules relative to the
calendar, like first or last of the month, next Tuesday, or the first and third Wednesdays, etc.

The `Dates` module provides the *adjuster* API through several convenient methods that
aid in simply and succinctly expressing temporal rules. The first group of adjuster methods deal
with the first and last of weeks, months, quarters, and years. They each take a single [`TimeType`](@ref)
as input and return or *adjust to* the first or last of the desired period relative to the input.

```jldoctest
julia> Dates.firstdayofweek(Date(2014,7,16)) # Adjusts the input to the Monday of the input's week
2014-07-14

julia> Dates.lastdayofmonth(Date(2014,7,16)) # Adjusts to the last day of the input's month
2014-07-31

julia> Dates.lastdayofquarter(Date(2014,7,16)) # Adjusts to the last day of the input's quarter
2014-09-30
```

The next two higher-order methods, [`tonext`](@ref), and [`toprev`](@ref), generalize working
with temporal expressions by taking a `DateFunction` as first argument, along with a starting
[`TimeType`](@ref). A `DateFunction` is just a function, usually anonymous, that takes a single
[`TimeType`](@ref) as input and returns a [`Bool`](@ref), `true` indicating a satisfied
adjustment criterion.
For example:

```jldoctest
julia> istuesday = x->Dates.dayofweek(x) == Dates.Tuesday; # Returns true if the day of the week of x is Tuesday

julia> Dates.tonext(istuesday, Date(2014,7,13)) # 2014-07-13 is a Sunday
2014-07-15

julia> Dates.tonext(Date(2014,7,13), Dates.Tuesday) # Convenience method provided for day of the week adjustments
2014-07-15
```

This is useful with the do-block syntax for more complex temporal expressions:

```jldoctest
julia> Dates.tonext(Date(2014,7,13)) do x
           # Return true on the 4th Thursday of November (Thanksgiving)
           Dates.dayofweek(x) == Dates.Thursday &&
           Dates.dayofweekofmonth(x) == 4 &&
           Dates.month(x) == Dates.November
       end
2014-11-27
```

The [`Base.filter`](@ref) method can be used to obtain all valid dates/moments in a specified
range:

```jldoctest
# Pittsburgh street cleaning; Every 2nd Tuesday from April to November
# Date range from January 1st, 2014 to January 1st, 2015
julia> dr = Dates.Date(2014):Day(1):Dates.Date(2015);

julia> filter(dr) do x
           Dates.dayofweek(x) == Dates.Tue &&
           Dates.April <= Dates.month(x) <= Dates.Nov &&
           Dates.dayofweekofmonth(x) == 2
       end
8-element Vector{Date}:
 2014-04-08
 2014-05-13
 2014-06-10
 2014-07-08
 2014-08-12
 2014-09-09
 2014-10-14
 2014-11-11
```

Additional examples and tests are available in [`stdlib/Dates/test/adjusters.jl`](https://github.com/JuliaLang/julia/blob/master/stdlib/Dates/test/adjusters.jl).

## Period Types

Periods are a human view of discrete, sometimes irregular durations of time. Consider 1 month;
it could represent, in days, a value of 28, 29, 30, or 31 depending on the year and month context.
Or a year could represent 365 or 366 days in the case of a leap year. [`Period`](@ref) types are
simple [`Int64`](@ref) wrappers and are constructed by wrapping any `Int64` convertible type, i.e. `Year(1)`
or `Month(3.0)`. Arithmetic between [`Period`](@ref) of the same type behave like integers, and
limited `Period-Real` arithmetic is available.  You can extract the underlying integer with
[`Dates.value`](@ref).

```jldoctest
julia> y1 = Dates.Year(1)
1 year

julia> y2 = Dates.Year(2)
2 years

julia> y3 = Dates.Year(10)
10 years

julia> y1 + y2
3 years

julia> div(y3,y2)
5

julia> y3 - y2
8 years

julia> y3 % y2
0 years

julia> div(y3,3) # mirrors integer division
3 years

julia> Dates.value(Dates.Millisecond(10))
10
```

## Rounding

[`Date`](@ref) and [`DateTime`](@ref) values can be rounded to a specified resolution (e.g., 1
month or 15 minutes) with [`floor`](@ref), [`ceil`](@ref), or [`round`](@ref):

```jldoctest
julia> floor(Date(1985, 8, 16), Dates.Month)
1985-08-01

julia> ceil(DateTime(2013, 2, 13, 0, 31, 20), Dates.Minute(15))
2013-02-13T00:45:00

julia> round(DateTime(2016, 8, 6, 20, 15), Dates.Day)
2016-08-07T00:00:00
```

Unlike the numeric [`round`](@ref) method, which breaks ties toward the even number by default,
the [`TimeType`](@ref)[`round`](@ref) method uses the `RoundNearestTiesUp` rounding mode. (It's
difficult to guess what breaking ties to nearest "even" [`TimeType`](@ref) would entail.) Further
details on the available `RoundingMode` s can be found in the [API reference](@ref stdlib-dates-api).

Rounding should generally behave as expected, but there are a few cases in which the expected
behaviour is not obvious.

### Rounding Epoch

In many cases, the resolution specified for rounding (e.g., `Dates.Second(30)`) divides evenly
into the next largest period (in this case, `Dates.Minute(1)`). But rounding behaviour in cases
in which this is not true may lead to confusion. What is the expected result of rounding a [`DateTime`](@ref)
to the nearest 10 hours?

```jldoctest
julia> round(DateTime(2016, 7, 17, 11, 55), Dates.Hour(10))
2016-07-17T12:00:00
```

That may seem confusing, given that the hour (12) is not divisible by 10. The reason that `2016-07-17T12:00:00`
was chosen is that it is 17,676,660 hours after `0000-01-01T00:00:00`, and 17,676,660 is divisible
by 10.

As Julia [`Date`](@ref) and [`DateTime`](@ref) values are represented according to the ISO 8601
standard, `0000-01-01T00:00:00` was chosen as base (or "rounding epoch") from which to begin the
count of days (and milliseconds) used in rounding calculations. (Note that this differs slightly
from Julia's internal representation of [`Date`](@ref) s using Rata Die notation; but since the
ISO 8601 standard is most visible to the end user, `0000-01-01T00:00:00` was chosen as the rounding
epoch instead of the `0000-12-31T00:00:00` used internally to minimize confusion.)

The only exception to the use of `0000-01-01T00:00:00` as the rounding epoch is when rounding
to weeks. Rounding to the nearest week will always return a Monday (the first day of the week
as specified by ISO 8601). For this reason, we use `0000-01-03T00:00:00` (the first day of the
first week of year 0000, as defined by ISO 8601) as the base when rounding to a number of weeks.

Here is a related case in which the expected behaviour is not necessarily obvious: What happens
when we round to the nearest `P(2)`, where `P` is a [`Period`](@ref) type? In some cases (specifically,
when `P <: Dates.TimePeriod`) the answer is clear:

```jldoctest
julia> round(DateTime(2016, 7, 17, 8, 55, 30), Dates.Hour(2))
2016-07-17T08:00:00

julia> round(DateTime(2016, 7, 17, 8, 55, 30), Dates.Minute(2))
2016-07-17T08:56:00
```

This seems obvious, because two of each of these periods still divides evenly into the next larger
order period. But in the case of two months (which still divides evenly into one year), the answer
may be surprising:

```jldoctest
julia> round(DateTime(2016, 7, 17, 8, 55, 30), Dates.Month(2))
2016-07-01T00:00:00
```

Why round to the first day in July, even though it is month 7 (an odd number)? The key is that
months are 1-indexed (the first month is assigned 1), unlike hours, minutes, seconds, and milliseconds
(the first of which are assigned 0).

This means that rounding a [`DateTime`](@ref) to an even multiple of seconds, minutes, hours,
or years (because the ISO 8601 specification includes a year zero) will result in a [`DateTime`](@ref)
with an even value in that field, while rounding a [`DateTime`](@ref) to an even multiple of months
will result in the months field having an odd value. Because both months and years may contain
an irregular number of days, whether rounding to an even number of days will result in an even
value in the days field is uncertain.

See the [API reference](@ref stdlib-dates-api) for additional information
on methods exported from the `Dates` module.

# [API reference](@id stdlib-dates-api)

## Dates and Time Types

```@docs
Dates.Period
Dates.CompoundPeriod
Dates.Instant
Dates.UTInstant
Dates.TimeType
Dates.DateTime
Dates.Date
Dates.Time
Dates.TimeZone
Dates.UTC
```

## Dates Functions

```@docs
Dates.DateTime(::Int64, ::Int64, ::Int64, ::Int64, ::Int64, ::Int64, ::Int64)
Dates.DateTime(::Dates.Period)
Dates.DateTime(::Function, ::Any...)
Dates.DateTime(::Dates.TimeType)
Dates.DateTime(::AbstractString, ::AbstractString)
Dates.format(::Dates.TimeType, ::AbstractString)
Dates.DateFormat
Dates.@dateformat_str
Dates.DateTime(::AbstractString, ::Dates.DateFormat)
Dates.Date(::Int64, ::Int64, ::Int64)
Dates.Date(::Dates.Period)
Dates.Date(::Function, ::Any, ::Any, ::Any)
Dates.Date(::Dates.TimeType)
Dates.Date(::AbstractString, ::AbstractString)
Dates.Date(::AbstractString, ::Dates.DateFormat)
Dates.Time(::Int64::Int64, ::Int64, ::Int64, ::Int64, ::Int64)
Dates.Time(::Dates.TimePeriod)
Dates.Time(::Function, ::Any...)
Dates.Time(::Dates.DateTime)
Dates.now()
Dates.now(::Type{Dates.UTC})
Base.eps
```

### Accessor Functions

```@docs
Dates.year
Dates.month
Dates.week
Dates.day
Dates.hour
Dates.minute
Dates.second
Dates.millisecond
Dates.microsecond
Dates.nanosecond
Dates.Year(::Dates.TimeType)
Dates.Month(::Dates.TimeType)
Dates.Week(::Dates.TimeType)
Dates.Day(::Dates.TimeType)
Dates.Hour(::DateTime)
Dates.Minute(::DateTime)
Dates.Second(::DateTime)
Dates.Millisecond(::DateTime)
Dates.Microsecond(::Dates.Time)
Dates.Nanosecond(::Dates.Time)
Dates.yearmonth
Dates.monthday
Dates.yearmonthday
```

### Query Functions

```@docs
Dates.dayname
Dates.dayabbr
Dates.dayofweek
Dates.dayofmonth
Dates.dayofweekofmonth
Dates.daysofweekinmonth
Dates.monthname
Dates.monthabbr
Dates.daysinmonth
Dates.isleapyear
Dates.dayofyear
Dates.daysinyear
Dates.quarterofyear
Dates.dayofquarter
```

### Adjuster Functions

```@docs
Base.trunc(::Dates.TimeType, ::Type{Dates.Period})
Dates.firstdayofweek
Dates.lastdayofweek
Dates.firstdayofmonth
Dates.lastdayofmonth
Dates.firstdayofyear
Dates.lastdayofyear
Dates.firstdayofquarter
Dates.lastdayofquarter
Dates.tonext(::Dates.TimeType, ::Int)
Dates.toprev(::Dates.TimeType, ::Int)
Dates.tofirst
Dates.tolast
Dates.tonext(::Function, ::Dates.TimeType)
Dates.toprev(::Function, ::Dates.TimeType)
```

### Periods

```@docs
Dates.Period(::Any)
Dates.CompoundPeriod(::Vector{<:Dates.Period})
Dates.value
Dates.default
```

### Rounding Functions

`Date` and `DateTime` values can be rounded to a specified resolution (e.g., 1 month or 15 minutes)
with `floor`, `ceil`, or `round`.

```@docs
Base.floor(::Dates.TimeType, ::Dates.Period)
Base.ceil(::Dates.TimeType, ::Dates.Period)
Base.round(::Dates.TimeType, ::Dates.Period, ::RoundingMode{:NearestTiesUp})
```

Most `Period` values can also be rounded to a specified resolution:

```@docs
Base.floor(::Dates.ConvertiblePeriod, ::T) where T <: Dates.ConvertiblePeriod
Base.ceil(::Dates.ConvertiblePeriod, ::Dates.ConvertiblePeriod)
Base.round(::Dates.ConvertiblePeriod, ::Dates.ConvertiblePeriod, ::RoundingMode{:NearestTiesUp})
```

The following functions are not exported:

```@docs
Dates.floorceil
Dates.epochdays2date
Dates.epochms2datetime
Dates.date2epochdays
Dates.datetime2epochms
```

### Conversion Functions

```@docs
Dates.today
Dates.unix2datetime
Dates.datetime2unix
Dates.julian2datetime
Dates.datetime2julian
Dates.rata2datetime
Dates.datetime2rata
```

### Constants

Days of the Week:

| Variable    | Abbr. | Value (Int) |
|:----------- |:----- |:----------- |
| `Monday`    | `Mon` | 1           |
| `Tuesday`   | `Tue` | 2           |
| `Wednesday` | `Wed` | 3           |
| `Thursday`  | `Thu` | 4           |
| `Friday`    | `Fri` | 5           |
| `Saturday`  | `Sat` | 6           |
| `Sunday`    | `Sun` | 7           |

Months of the Year:

| Variable    | Abbr. | Value (Int) |
|:----------- |:----- |:----------- |
| `January`   | `Jan` | 1           |
| `February`  | `Feb` | 2           |
| `March`     | `Mar` | 3           |
| `April`     | `Apr` | 4           |
| `May`       | `May` | 5           |
| `June`      | `Jun` | 6           |
| `July`      | `Jul` | 7           |
| `August`    | `Aug` | 8           |
| `September` | `Sep` | 9           |
| `October`   | `Oct` | 10          |
| `November`  | `Nov` | 11          |
| `December`  | `Dec` | 12          |

```@meta
DocTestSetup = nothing
```

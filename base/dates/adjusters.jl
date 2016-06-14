# This file is a part of Julia. License is MIT: http://julialang.org/license

### truncation
Base.trunc(dt::Date, p::Type{Year}) = Date(UTD(totaldays(year(dt), 1, 1)))
Base.trunc(dt::Date, p::Type{Month}) = firstdayofmonth(dt)
Base.trunc(dt::Date, p::Type{Day}) = dt

Base.trunc(dt::DateTime, p::Type{Year}) = DateTime(trunc(Date(dt), Year))
Base.trunc(dt::DateTime, p::Type{Month}) = DateTime(trunc(Date(dt), Month))
Base.trunc(dt::DateTime, p::Type{Day}) = DateTime(Date(dt))
Base.trunc(dt::DateTime, p::Type{Hour}) = dt - Minute(dt) - Second(dt) - Millisecond(dt)
Base.trunc(dt::DateTime, p::Type{Minute}) = dt - Second(dt) - Millisecond(dt)
Base.trunc(dt::DateTime, p::Type{Second}) = dt - Millisecond(dt)
Base.trunc(dt::DateTime, p::Type{Millisecond}) = dt

"""
    trunc(dt::TimeType, ::Type{Period}) -> TimeType

Truncates the value of `dt` according to the provided `Period` type. E.g. if `dt` is
`1996-01-01T12:30:00`, then `trunc(dt,Day) == 1996-01-01T00:00:00`.
"""
Dates.trunc(::Dates.TimeType, ::Type{Dates.Period})


# Rounding

# TODO: Docstrings

function Base.floor(dt::Date, p::Year)
    years = year(dt)
    return Date(years - mod(years, value(p)), 1, 1)
end

function Base.floor(dt::Date, p::Month)
    months_since_epoch = year(dt) * 12 + month(dt) - 1
    month_offset = months_since_epoch - mod(months_since_epoch, value(p))
    target_month = mod(month_offset, 12) + 1
    target_year = div(month_offset, 12) - (month_offset < 0 && target_month != 1)
    return Date(target_year, target_month, 1)
end

# According to ISO 8601, the first day of the first week of year 0000 is 0000-01-03.
const ISO8601WEEKEPOCH = value(Date(0, 1, 3))
function Base.floor(dt::Date, p::Week)
    days = value(dt) - ISO8601WEEKEPOCH
    days = days - mod(days, value(p) * 7)
    return Date(UTD(ISO8601WEEKEPOCH + Int64(days)))
end

# For days, the math is straightforward.
function Base.floor(dt::Date, p::Day)
    days = date2iso8601(dt)
    return iso86012date(days - mod(days, value(p)))
end

Base.floor(dt::DateTime, p::DatePeriod) = DateTime(Base.floor(Date(dt), p))

# All TimePeriods are convertible to milliseconds, so the math is straightforward.
function Base.floor(dt::DateTime, p::TimePeriod)
    milliseconds = datetime2iso8601(dt)
    return iso86012datetime(milliseconds - mod(milliseconds, value(Millisecond(p))))
end

function Base.ceil(dt::TimeType, p::Period)
    f = floor(dt, p)
    return (dt == f) ? f : f + p
end

function Base.round(dt::TimeType, p::Period, r::RoundingMode{:NearestTiesUp})
    f = floor(dt, p)
    c = ceil(dt, p)
    return (dt - f < c - dt) ? f : c
end

Base.round(dt::TimeType, p::Period, r::RoundingMode{:Up}) = Base.ceil(dt, p)
Base.round(dt::TimeType, p::Period, r::RoundingMode{:Down}) = Base.floor(dt, p)

# No implementation of rounding to nearest "even", because "even" is not defined for Period.
Base.round(::TimeType, ::Period, ::RoundingMode{:Nearest}) = throw(DomainError())

# No implementation of rounding toward/away from ISO 8601's arbitrary zero-point.
Base.round(::TimeType, ::Period, ::RoundingMode{:NearestTiesAway}) = throw(DomainError())
Base.round(::TimeType, ::Period, ::RoundingMode{:ToZero}) = throw(DomainError())

# Default to RoundNearestTiesUp.
Base.round(dt::TimeType, p::Period) = Base.round(dt, p, RoundNearestTiesUp)

# Callable using Period types in addition to values.
Base.floor{T <: Period}(dt::TimeType, p::Type{T}) = Base.floor(dt, p(1))
Base.ceil{T <: Period}(dt::TimeType, p::Type{T}) = Base.ceil(dt, p(1))

function Base.round{T <: Period}(dt::TimeType, p::Type{T}, r=RoundNearestTiesUp)
    return Base.round(dt, p(1), r)
end

#=
TODO: Update documentation for dates and for RoundingMode to indicate that the default is
RoundNearestTiesUp

TODO: Add example calls for rounding to the documentation and to the PR itself!

TODO: Add some of this to the documentation.

Who has opinions on date rounding? Specifically, I need to define a zero-point for rounding
a TimeType to a multiple of a Period.

Candidate "zero dates" include:
1. `0000-01-01T00:00:00.000`, the first day of the first year specified by ISO 8601, which
   Julia uses for external representations (display, etc.) of Date and DateTime
2. `0001-01-01T00:00:00.000`, the Rata Die base date, which is how Julia represents dates
   internally
3. `YYYY-01-01T00:00:00.000`, where `YYYY` is the year of the date to be rounded
4. `YYYY-01-01T00:00:00.000` for rounding to `DatePeriod`s and `YYYY-MM-DDT00:00:00.000` for
   rounding `TimePeriod`s.
5. "Closest current state" (better name to be determined), where the "zero date" is
   dependent on the Period type to be used in rounding. Rounding a date to the nearest X
   milliseconds would use the date's current second as its base; rounding a date to the
   nearest X minutes would use the date's current hour as its base, etc. (For rounding to X
   weeks we'd have to make a judgment about whether we use current month or current year;
   year probably makes the most sense, as programming things that are defined as "biweekly"
   might work better than if we used months.)

For most use cases we will encounter, it essentially doesn't matter, because when we round
to a multiple of a Period, it's usually something that divides easily and nicely and hides
the problem. This example works the same regardless of what we choose:

```julia> dt = DateTime(2016, 1, 2, 3, 21)
2016-01-02T03:21:00

julia> floor(dt, Dates.Minute(15))
2016-01-02T03:15:00
```

However, this example...

```julia> dt = DateTime(2016, 1, 2, 3, 21)
2016-01-02T03:21:00

julia> floor(dt, Dates.Minute(11))
```

...will return `2016-01-02T03:11:00` if we go with #5 (starting with current hour),
`2016-01-02T03:18:00` if we go with #4 (starting with current day), `2016-01-02T03:19:00` if
we go with #3 (starting with the current year) or #1, and `2016-01-02T03:17:00` if we go
with #2.

#1, with a "zero date" of `0001-01-01` is cleanest (and probably fastest) for rounding days,
hours, minutes, seconds, and milliseconds. #2 would be similarly efficient, but would
present a challenge, because for dates between 0000-01-01 and 0001-01-01 `floor` would
actually round "up" (toward year 0001) instead of "down" (toward year 0000) because all
dates before 0000-12-31 are represented in Rata Die as negative numbers (even though Julia
displays the year 0000 as non-negative).

#1 also has the advantage of being a (fairly) intuitive choice, given the way Julia choses
to represent its dates. For this reason, we elected to implement rounding based on #1.

TODO: Add additional rationale. (It's common to want to round a datetime to the nearest
fifteen minutes or nearest hour or execute a task every two weeks...)

TODO: Add this rationale for rounding weeks to the docs.

Since `0000-01-01` is a Saturday, it can't be our "base date" for rounding to a certain
number of weeks (unless we want `floor(now(), Week(1))` to return last Saturday, which we
obviously don't).

Instead, rounding to a number of weeks should round to the appropriate Monday (because, much
as it vexes me, Monday is the first day of the week in ISO 8601).

The most intuitive approach to date rounding might be to use the first day of the week that
contains `0000-01-01` (which is `-0001-12-27`) as the "base date" for weeks. If we do this,
`floor(now(), Week(1))` returning this past Monday and `floor(Date0, 1, 1), Week(1))`
returning `-0001-12-27`. However, I contend that this would be wrong.

Instead, I propose that we use the first day of the first week in 0000 as the base date.
Because the ISO defines the "first week" of a year as the week with the first Thursday in it
(for those unaware of this, surprise! :ghost:), this would make the base date for weeks
`0000-01-03` (the first Monday in 0000, which I guess is also intuitive in a way).
`floor(now(), Week(1))` and `floor(Date0, 1, 1), Week(1))` would have the same result as the
other system, but the results for rounding to every second week, every third week, etc.
would be different.

TODO: Note that calling `ceil` with a number of years will round up to the start of a
year (January 1st). This is expected behaviour.

TODO: Note that when flooring/ceiling dates to an even number of months (e.g., every
two months), this will inevitable result in an odd month number. This is because months
are one-indexed (i.e., the first month, January, is assigned "01").
So `floor(Date(2016, 2, 13), Month(2))` will return `2016-01-01`.
This is also true for days (as they are also one-indexed), but it is obscured by the fact
that (most) years have an odd number of days, so which "day" is relevant when we're
concerned with "every second day" becomes less obvious the further we get from the epoch.

TODO: CompoundPeriods left out because every solution I came up with was horrendously
inefficient.
=#


# Adjusters
"""
    firstdayofweek(dt::TimeType) -> TimeType

Adjusts `dt` to the Monday of its week.
"""
function firstdayofweek end

firstdayofweek(dt::Date) = Date(UTD(value(dt) - dayofweek(dt) + 1))
firstdayofweek(dt::DateTime) = DateTime(firstdayofweek(Date(dt)))

"""
    lastdayofweek(dt::TimeType) -> TimeType

Adjusts `dt` to the Sunday of its week.
"""
function lastdayofweek end

lastdayofweek(dt::Date) = Date(UTD(value(dt) + (7 - dayofweek(dt))))
lastdayofweek(dt::DateTime) = DateTime(lastdayofweek(Date(dt)))

@vectorize_1arg TimeType firstdayofweek
@vectorize_1arg TimeType lastdayofweek

"""
    firstdayofmonth(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its month.
"""
function firstdayofmonth end

firstdayofmonth(dt::Date) = Date(UTD(value(dt) - day(dt) + 1))
firstdayofmonth(dt::DateTime) = DateTime(firstdayofmonth(Date(dt)))

"""
    lastdayofmonth(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its month.
"""
function lastdayofmonth end

function lastdayofmonth(dt::Date)
    y, m, d = yearmonthday(dt)
    return Date(UTD(value(dt) + daysinmonth(y, m) - d))
end
lastdayofmonth(dt::DateTime) = DateTime(lastdayofmonth(Date(dt)))

@vectorize_1arg TimeType firstdayofmonth
@vectorize_1arg TimeType lastdayofmonth

"""
    firstdayofyear(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its year.
"""
function firstdayofyear end

firstdayofyear(dt::Date) = Date(UTD(value(dt) - dayofyear(dt) + 1))
firstdayofyear(dt::DateTime) = DateTime(firstdayofyear(Date(dt)))

"""
    lastdayofyear(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its year.
"""
function lastdayofyear end

function lastdayofyear(dt::Date)
    y, m, d = yearmonthday(dt)
    return Date(UTD(value(dt) + daysinyear(y) - dayofyear(y, m, d)))
end
lastdayofyear(dt::DateTime) = DateTime(lastdayofyear(Date(dt)))

@vectorize_1arg TimeType firstdayofyear
@vectorize_1arg TimeType lastdayofyear

"""
    firstdayofquarter(dt::TimeType) -> TimeType

Adjusts `dt` to the first day of its quarter.
"""
function firstdayofquarter end

function firstdayofquarter(dt::Date)
    y,m = yearmonth(dt)
    mm = m < 4 ? 1 : m < 7 ? 4 : m < 10 ? 7 : 10
    return Date(y, mm, 1)
end
firstdayofquarter(dt::DateTime) = DateTime(firstdayofquarter(Date(dt)))

"""
    lastdayofquarter(dt::TimeType) -> TimeType

Adjusts `dt` to the last day of its quarter.
"""
function lastdayofquarter end

function lastdayofquarter(dt::Date)
    y,m = yearmonth(dt)
    mm, d = m < 4 ? (3, 31) : m < 7 ? (6, 30) : m < 10 ? (9, 30) : (12, 31)
    return Date(y, mm, d)
end
lastdayofquarter(dt::DateTime) = DateTime(lastdayofquarter(Date(dt)))

@vectorize_1arg TimeType firstdayofquarter
@vectorize_1arg TimeType lastdayofquarter

# Temporal Adjusters
immutable DateFunction
    f::Function
    # validate boolean, single-arg inner constructor
    function DateFunction(f::ANY, negate::Bool, dt::TimeType)
        isa(f(dt), Bool) || throw(ArgumentError("Provided function must take a single TimeType argument and return true or false"))
        return new(negate ? x -> !f(x)::Bool : f)
    end
end
Base.show(io::IO, df::DateFunction) = println(io, df.f)

# Core adjuster
function adjust(df::DateFunction, start, step, limit)
    for i = 1:limit
        df.f(start) && return start
        start += step
    end
    throw(ArgumentError("Adjustment limit reached: $limit iterations"))
end

function adjust(func::Function, start; step::Period=Day(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, start), start, step, limit)
end

# Constructors using DateFunctions

"""
    Date(f::Function, y[, m, d]; step=Day(1), negate=false, limit=10000) -> Date

Create a `Date` through the adjuster API. The starting point will be constructed from the
provided `y, m, d` arguments, and will be adjusted until `f::Function` returns `true`. The
step size in adjusting can be provided manually through the `step` keyword. If
`negate=true`, then the adjusting will stop when `f::Function` returns `false` instead of
`true`. `limit` provides a limit to the max number of iterations the adjustment API will
pursue before throwing an error (given that `f::Function` is never satisfied).
"""
function Date(func::Function, y, m=1, d=1;step::Period=Day(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, Date(y, m, d)), Date(y, m, d), step, limit)
end

"""
    DateTime(f::Function, y[, m, d, h, mi, s]; step=Day(1), negate=false, limit=10000) -> DateTime

Create a `DateTime` through the adjuster API. The starting point will be constructed from
the provided `y, m, d...` arguments, and will be adjusted until `f::Function` returns
`true`. The step size in adjusting can be provided manually through the `step` keyword. If
`negate=true`, then the adjusting will stop when `f::Function` returns `false` instead of
`true`. `limit` provides a limit to the max number of iterations the adjustment API will
pursue before throwing an error (in the case that `f::Function` is never satisfied).
"""
DateTime(::Function, args...)

function DateTime(func::Function, y, m=1; step::Period=Day(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, DateTime(y, m)), DateTime(y, m), step, limit)
end
function DateTime(func::Function, y, m, d; step::Period=Hour(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, DateTime(y)), DateTime(y, m, d), step, limit)
end
function DateTime(func::Function, y, m, d, h; step::Period=Minute(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, DateTime(y)), DateTime(y, m, d, h), step, limit)
end
function DateTime(func::Function, y, m, d, h, mi; step::Period=Second(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, DateTime(y)), DateTime(y, m, d, h, mi), step, limit)
end
function DateTime(func::Function, y, m, d, h, mi, s; step::Period=Millisecond(1), negate::Bool=false, limit::Int=10000)
    return adjust(DateFunction(func, negate, DateTime(y)), DateTime(y, m, d, h, mi, s), step, limit)
end

# Return the next TimeType that falls on dow
ISDAYOFWEEK = Dict(Mon => DateFunction(ismonday, false, Date(0)),
                   Tue => DateFunction(istuesday, false, Date(0)),
                   Wed => DateFunction(iswednesday, false, Date(0)),
                   Thu => DateFunction(isthursday, false, Date(0)),
                   Fri => DateFunction(isfriday, false, Date(0)),
                   Sat => DateFunction(issaturday, false, Date(0)),
                   Sun => DateFunction(issunday, false, Date(0)))

# "same" indicates whether the current date can be considered or not
"""
    tonext(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

Adjusts `dt` to the next day of week corresponding to `dow` with `1 = Monday, 2 = Tuesday,
etc`. Setting `same=true` allows the current `dt` to be considered as the next `dow`,
allowing for no adjustment to occur.
"""
tonext(dt::TimeType, dow::Int; same::Bool=false) = adjust(ISDAYOFWEEK[dow], same ? dt : dt+Day(1), Day(1), 7)

# Return the next TimeType where func evals true using step in incrementing
"""
    tonext(func::Function,dt::TimeType;step=Day(1),negate=false,limit=10000,same=false) -> TimeType

Adjusts `dt` by iterating at most `limit` iterations by `step` increments until `func`
returns `true`. `func` must take a single `TimeType` argument and return a `Bool`. `same`
allows `dt` to be considered in satisfying `func`. `negate` will make the adjustment process
terminate when `func` returns `false` instead of `true`.
"""
function tonext(func::Function, dt::TimeType;step::Period=Day(1), negate::Bool=false, limit::Int=10000, same::Bool=false)
    return adjust(DateFunction(func, negate, dt), same ? dt : dt+step, step, limit)
end

"""
    toprev(dt::TimeType,dow::Int;same::Bool=false) -> TimeType

Adjusts `dt` to the previous day of week corresponding to `dow` with `1 = Monday, 2 =
Tuesday, etc`. Setting `same=true` allows the current `dt` to be considered as the previous
`dow`, allowing for no adjustment to occur.
"""
toprev(dt::TimeType, dow::Int; same::Bool=false) = adjust(ISDAYOFWEEK[dow], same ? dt : dt+Day(-1), Day(-1), 7)

"""
    toprev(func::Function,dt::TimeType;step=Day(-1),negate=false,limit=10000,same=false) -> TimeType

Adjusts `dt` by iterating at most `limit` iterations by `step` increments until `func`
returns `true`. `func` must take a single `TimeType` argument and return a `Bool`. `same`
allows `dt` to be considered in satisfying `func`. `negate` will make the adjustment process
terminate when `func` returns `false` instead of `true`.
"""
function toprev(func::Function, dt::TimeType; step::Period=Day(-1), negate::Bool=false, limit::Int=10000, same::Bool=false)
    return adjust(DateFunction(func, negate, dt), same ? dt : dt+step, step, limit)
end

# Return the first TimeType that falls on dow in the Month or Year
"""
    tofirst(dt::TimeType,dow::Int;of=Month) -> TimeType

Adjusts `dt` to the first `dow` of its month. Alternatively, `of=Year` will adjust to the
first `dow` of the year.
"""
function tofirst(dt::TimeType, dow::Int; of::Union{Type{Year}, Type{Month}}=Month)
    dt = of <: Month ? firstdayofmonth(dt) : firstdayofyear(dt)
    return adjust(ISDAYOFWEEK[dow], dt, Day(1), 366)
end

# Return the last TimeType that falls on dow in the Month or Year
"""
    tolast(dt::TimeType,dow::Int;of=Month) -> TimeType

Adjusts `dt` to the last `dow` of its month. Alternatively, `of=Year` will adjust to the
last `dow` of the year.
"""
function tolast(dt::TimeType, dow::Int; of::Union{Type{Year}, Type{Month}}=Month)
    dt = of <: Month ? lastdayofmonth(dt) : lastdayofyear(dt)
    return adjust(ISDAYOFWEEK[dow], dt, Day(-1), 366)
end

function recur{T<:TimeType}(fun::Function, start::T, stop::T; step::Period=Day(1), negate::Bool=false, limit::Int=10000)
    ((start != stop) & ((step > zero(step)) != (stop > start))) && return T[]
    a = T[]
    check = start <= stop ? 1 : -1
    df = Dates.DateFunction(fun, negate, start)
    while true
        next = Dates.adjust(df, start, step, limit)
        cmp(next, stop) == check && break
        push!(a, next)
        start = next + step
    end
    return a
end

"""
    recur{T<:TimeType}(func::Function,dr::StepRange{T};negate=false,limit=10000) -> Vector{T}

`func` takes a single TimeType argument and returns a `Bool` indicating whether the input
should be "included" in the final set. `recur` applies `func` over each element in the range
of `dr`, including those elements for which `func` returns `true` in the resulting Array,
unless `negate=true`, then only elements where `func` returns `false` are included.
"""
function recur{T<:TimeType}(fun::Function, dr::StepRange{T};negate::Bool=false, limit::Int=10000)
    return recur(fun, first(dr), last(dr); step=step(dr), negate=negate, limit=limit)
end

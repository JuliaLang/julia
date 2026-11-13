# This file is a part of Julia. License is MIT: https://julialang.org/license

# Convert # of Rata Die days to proleptic Gregorian calendar y,m,d,w
# Reference: https://www.researchgate.net/profile/Peter-Baum/publication/316558298_Date_Algorithms/links/5f90c3f992851c14bcdb0da6/Date-Algorithms.pdf
const DAYS_PER_400Y = 146097
# Days in four Julian years, or quarter days in one
const QUARTER_DAYS_PER_JULIAN_YEAR = 4 * 365 + 1
# Days from March 1 to January 1. Years start in March, so the leap day is their last day.
const DAYS_MAR_TO_JAN = 306
# Hundredths of a day in a Gregorian century (36524.25 days) and in a Julian year (365.25 days)
const CENTURY_HUNDREDTHS = 100 * DAYS_PER_400Y ÷ 4
const JULIAN_YEAR_HUNDREDTHS = 100 * QUARTER_DAYS_PER_JULIAN_YEAR ÷ 4
# For day `r` in 0:DAYS_PER_400Y-1 of a 400-year cycle, return the year within the cycle,
# the month from 3 (March) to 14 (February of the next year), and the day of month. All
# operands stay small and non-negative.
@inline function cycleday2yearmonthday(r::Integer)
    z = r % UInt64 + DAYS_MAR_TO_JAN        # day count with 1 = March 1 of year 0
    h = 100z - 25                           # 100(z - 1/4), fixed point in hundredths of a day
    a = h ÷ CENTURY_HUNDREDTHS              # whole Gregorian centuries
    b = a - a ÷ 4                           # century leap days the Gregorian calendar skips
    y = (100b + h) ÷ JULIAN_YEAR_HUNDREDTHS # March-based year, counting z + b in Julian years
    c = b + z - 365y - y ÷ 4                # day of that year, 1 = March 1
    m = (5c + 456) ÷ 153                    # month, 3 (March) to 14 (February)
    d = c - (153m - 457) ÷ 5                # day of month
    return y % Int64, m % Int64, d % Int64
end
function yearmonthday(days::Integer)
    q, r = fldmod(days, DAYS_PER_400Y)
    y, m, d = cycleday2yearmonthday(r)
    y += 400q
    return m > 12 ? (y + 1, m - 12, d) : (y, m, d)
end
year(days::Integer) = (@inline yearmonthday(days))[1]
yearmonth(days::Integer) = (@inline yearmonthday(days))[1:2]
month(days::Integer) = (@inline yearmonthday(days))[2]
monthday(days::Integer) = (@inline yearmonthday(days))[2:3]
day(days::Integer) = (@inline yearmonthday(days))[3]

# ISO year utils
# https://en.wikipedia.org/wiki/Talk:ISO_week_date#Algorithms
# The 400-year cycle plus one inserted week splits into four "centuries" of equally many weeks,
# each treated as Julian years of 365.25 days. Year positions are counted in quarter days.
const WEEKS_PER_400Y = DAYS_PER_400Y ÷ 7
const WEEKS_PER_CENTURY = (WEEKS_PER_400Y + 1) ÷ 4
const QUARTER_DAYS_PER_WEEK = 4 * 7
# Position of the first week of each century in its Julian year, validated for every day
const CENTURY_PHASE = (15, 23, 3, 11)
function week(days)
    # Weeks since the Monday 0001-01-01 that starts the cycle
    weeks = div(mod1(days, DAYS_PER_400Y) - 1, 7)
    weeks += weeks >= 2 * WEEKS_PER_CENTURY - 1
    century, weekofcentury = divrem(weeks, WEEKS_PER_CENTURY)
    position = (weekofcentury * QUARTER_DAYS_PER_WEEK + CENTURY_PHASE[century + 1]) %
        QUARTER_DAYS_PER_JULIAN_YEAR
    return div(position, QUARTER_DAYS_PER_WEEK) + 1
end

"""
Return the number of ISO weeks in the given year (see https://en.wikipedia.org/wiki/ISO_week_date).

# Examples
```jldoctest
julia> weeksinyear(Year(2022))
52

julia> weeksinyear(Year(2020))
53
```
!!! compat "Julia 1.13"
    This function requires Julia 1.13 or later.
"""
function weeksinyear(y::Year)
    # Years 400 apart share their weekdays, and the years at the ends of Date are partly outside it
    y = Year(mod(value(y), 400))
    firstday = firstdayofyear(Date(y))
    lastday = lastdayofyear(Date(y))

    if dayofweek(firstday) == 4 || dayofweek(lastday) == 4
        return 53
    end
    return 52
end

"""
Return the ISO year that contains `dt` (see https://en.wikipedia.org/wiki/ISO_week_date).

# Examples
```jldoctest
julia> isoyear(Date(2022, 1, 1))
2021 years

julia> isoyear(Date(2021, 12, 31))
2021 years
```
!!! compat "Julia 1.13"
    This function requires Julia 1.13 or later.
"""
isoyear(dt::Union{Date, DateTime, Timestamp}) = Year(isoyearweek(days(dt))[1])
# The ISO year and week of a day count. Late December can lie in week 1 of the next ISO year,
# and early January in week 52 or 53 of the previous one.
function isoyearweek(days)
    y, m = yearmonth(days)
    w = week(days)
    return y - (w >= 52 && m == 1) + (w == 1 && m == 12), w
end

"""
Return the ISO week date that corresponds to `dt` (see
https://en.wikipedia.org/wiki/ISO_week_date).

The return type is a tuple of three `Int64` values: the ISO year, the ISO week
number, and the ISO weekday (from 1 through 7).

# Examples
```jldoctest
julia> isoweekdate(Date(2023, 03, 06))
(2023, 10, 1)

julia> isoweekdate(Date(2023, 01, 01))
(2022, 52, 7)
```
!!! compat "Julia 1.13"
    This function requires Julia 1.13 or later.
"""
function isoweekdate(dt::Union{Date, DateTime, Timestamp})
    y, w = isoyearweek(days(dt))
    return value(Year(y)), w, dayofweek(dt)
end

function quarter(days)
    m = month(days)
    return m < 4 ? 1 : m < 7 ? 2 : m < 10 ? 3 : 4
end


# Accessor functions
value(dt::TimeType) = dt.instant.periods.value
value(dt::Timestamp) = value(dt.instant.periods)
value(t::Time) = t.instant.value
withvalue(x, v) = typeof(x)(UTInstant(typeof(x.instant.periods)(v)))
days(dt::Date) = value(dt)
days(dt::DateTime) = fld(value(dt), 86400000)
days(dt::Timestamp{P}) where {P} = signedcount(fld(value(dt), timestamp_ticks_per_day(P))) + UNIXEPOCHDAYS
# Days before the Unix epoch need a sign. A day has at least 86400 ticks, so `signed` cannot overflow.
signedcount(n::Integer) = n isa Unsigned ? signed(n) : n
# Nanoseconds since midnight
nsofday(dt::Date) = Int64(0)
nsofday(dt::DateTime) = 1000000 * mod(value(dt), 86400000)
nsofday(dt::Timestamp{P}) where {P} = mod(value(dt), timestamp_ticks_per_day(P)) * timestamp_scale(P)
year(dt::TimeType) = year(days(dt))
quarter(dt::TimeType) = quarter(days(dt))
month(dt::TimeType) = month(days(dt))
week(dt::TimeType) = week(days(dt))
day(dt::TimeType) = day(days(dt))
hour(dt::DateTime)   = mod(fld(value(dt), 3600000), 24)
minute(dt::DateTime) = mod(fld(value(dt), 60000), 60)
second(dt::DateTime) = mod(fld(value(dt), 1000), 60)
millisecond(dt::DateTime) = mod(value(dt), 1000)
hour(t::Time)   = mod(fld(value(t), 3600000000000), Int64(24))
minute(t::Time) = mod(fld(value(t), 60000000000), Int64(60))
second(t::Time) = mod(fld(value(t), 1000000000), Int64(60))
millisecond(t::Time) = mod(fld(value(t), Int64(1000000)), Int64(1000))
microsecond(t::Time) = mod(fld(value(t), Int64(1000)), Int64(1000))
nanosecond(t::Time) = mod(value(t), Int64(1000))
# A time field of `dt` that is `unit` nanoseconds long, counted modulo `modulus`
# (24 for hours).
@inline timestamp_part(dt::Timestamp{P}, unit, modulus) where {P} =
    Int64(mod(fld(nsofday(dt), unit), modulus))
hour(dt::Timestamp) = timestamp_part(dt, 3600000000000, Int64(24))
minute(dt::Timestamp) = timestamp_part(dt, 60000000000, Int64(60))
second(dt::Timestamp) = timestamp_part(dt, Int64(1000000000), Int64(60))
millisecond(dt::Timestamp) = timestamp_part(dt, Int64(1000000), Int64(1000))
microsecond(dt::Timestamp) = timestamp_part(dt, Int64(1000), Int64(1000))
nanosecond(dt::Timestamp) = timestamp_part(dt, Int64(1), Int64(1000))

dayofmonth(dt::TimeType) = day(dt)

yearmonth(dt::TimeType) = yearmonth(days(dt))
monthday(dt::TimeType) = monthday(days(dt))
yearmonthday(dt::TimeType) = yearmonthday(days(dt))

# Documentation for exported accessors
for func in (:year, :month, :quarter)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::TimeType)::Int64

        The $($name) of a `Date`, `DateTime`, or `Timestamp` as an [`Int64`](@ref).
        """ $func(dt::TimeType)
    end
end

"""
    week(dt::TimeType)::Int64

Return the [ISO week number](https://en.wikipedia.org/wiki/ISO_week_date) of a
`Date`, `DateTime`, or `Timestamp` as an [`Int64`](@ref). Note that the first
week of a year is the week that contains the first Thursday of the year, which
can result in dates prior to January 4th being in the last week of the previous
year. For example, `week(Date(2005, 1, 1))` is the 53rd week of 2004.

# Examples
```jldoctest
julia> week(Date(1989, 6, 22))
25

julia> week(Date(2005, 1, 1))
53

julia> week(Date(2004, 12, 31))
53
```
"""
week(dt::TimeType)

for func in (:day, :dayofmonth)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::TimeType)::Int64

        The day of month of a `Date`, `DateTime`, or `Timestamp` as an [`Int64`](@ref).
        """ $func(dt::TimeType)
    end
end

"""
    hour(dt::DateTime)::Int64

The hour of day of a `DateTime` as an [`Int64`](@ref).
"""
hour(dt::DateTime)

for func in (:minute, :second, :millisecond)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::DateTime)::Int64

        The $($name) of a `DateTime` as an [`Int64`](@ref).
        """ $func(dt::DateTime)
    end
end

for parts in (["year", "month"], ["month", "day"], ["year", "month", "day"])
    name = join(parts)
    func = Symbol(name)
    @eval begin
        @doc """
            $($name)(dt::TimeType) -> ($(join(repeated(Int64, length($parts)), ", ")))

        Simultaneously return the $(join($parts, ", ", " and ")) parts of a `Date`,
        `DateTime`, or `Timestamp`.
        """ $func(dt::TimeType)
    end
end

for func in (:hour, :minute, :second, :millisecond, :microsecond, :nanosecond)
    name = string(func)
    @eval begin
        @doc """
            $($name)(t::Union{Time,Timestamp})::Int64

        The $($name) of a `Time` or `Timestamp` as an [`Int64`](@ref).
        """ $func(t::Time)
    end
end

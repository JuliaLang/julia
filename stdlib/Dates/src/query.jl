# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date Locales

struct DateLocale
    months::Vector{String}
    months_abbr::Vector{String}
    days_of_week::Vector{String}
    days_of_week_abbr::Vector{String}
    month_value::Dict{String, Int}
    month_abbr_value::Dict{String, Int}
    day_of_week_value::Dict{String, Int}
    day_of_week_abbr_value::Dict{String, Int}
end

function locale_dict(names::Vector{<:AbstractString})
    result = Dict{String, Int}()

    # Keep both the common case-sensitive version of the name and an all lowercase
    # version for case-insensitive matches. Storing both allows us to avoid using the
    # lowercase function during parsing.
    for i in 1:length(names)
        name = names[i]
        result[name] = i
        result[lowercase(name)] = i
    end
    return result
end

"""
    DateLocale(["January", "February",...], ["Jan", "Feb",...],
               ["Monday", "Tuesday",...], ["Mon", "Tue",...])

Create a locale for parsing or printing textual month names.

Arguments:

- `months::Vector`: 12 month names
- `months_abbr::Vector`: 12 abbreviated month names
- `days_of_week::Vector`: 7 days of week
- `days_of_week_abbr::Vector`: 7 days of week abbreviated

This object is passed as the last argument to `tryparsenext` and `format`
methods defined for each `AbstractDateToken` type.
"""
function DateLocale(months::Vector, months_abbr::Vector,
                    days_of_week::Vector, days_of_week_abbr::Vector)
    DateLocale(
        months, months_abbr, days_of_week, days_of_week_abbr,
        locale_dict(months), locale_dict(months_abbr),
        locale_dict(days_of_week), locale_dict(days_of_week_abbr),
    )
end

const ENGLISH = DateLocale(
    ["January", "February", "March", "April", "May", "June",
     "July", "August", "September", "October", "November", "December"],
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
    ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
)

const LOCALES = Dict{String, DateLocale}("english" => ENGLISH)

for (fn, field) in zip(
    [:dayname_to_value, :dayabbr_to_value, :monthname_to_value, :monthabbr_to_value],
    [:day_of_week_value, :day_of_week_abbr_value, :month_value, :month_abbr_value],
)
    @eval @inline function $fn(word::AbstractString, locale::DateLocale)
        # Maximize performance by attempting to avoid the use of `lowercase` and trying
        # a case-sensitive lookup first
        value = get(locale.$field, word, 0)
        if value == 0
            value = get(locale.$field, lowercase(word), 0)
        end
        value
    end
end

# Date functions

### Core query functions

# Monday = 1....Sunday = 7
dayofweek(days) = mod1(days, 7)

# Number of days in year
"""
    daysinyear(dt::TimeType) -> Int

Return 366 if the year of `dt` is a leap year, otherwise return 365.

# Examples
```jldoctest
julia> Dates.daysinyear(1999)
365

julia> Dates.daysinyear(2000)
366
```
"""
daysinyear(y) = 365 + isleapyear(y)

# Day of the year
const MONTHDAYS = (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
dayofyear(y, m, d) = MONTHDAYS[m] + d + (m > 2 && isleapyear(y))

### Days of the Week
"""
    dayofweek(dt::TimeType) -> Int64

Return the day of the week as an [`Int64`](@ref) with `1 = Monday, 2 = Tuesday, etc.`.

# Examples
```jldoctest
julia> Dates.dayofweek(Date("2000-01-01"))
6
```
"""
dayofweek(dt::TimeType) = dayofweek(days(dt))

const Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = 1, 2, 3, 4, 5, 6, 7
const Mon, Tue, Wed, Thu, Fri, Sat, Sun = 1, 2, 3, 4, 5, 6, 7

dayname(day::Integer, locale::DateLocale) = locale.days_of_week[day]
dayabbr(day::Integer, locale::DateLocale) = locale.days_of_week_abbr[day]
dayname(day::Integer; locale::AbstractString="english") = dayname(day, LOCALES[locale])
dayabbr(day::Integer; locale::AbstractString="english") = dayabbr(day, LOCALES[locale])

"""
    dayname(dt::TimeType; locale="english") -> String

Return the full day name corresponding to the day of the week of the `Date` or `DateTime` in
the given `locale`.

# Examples
```jldoctest
julia> Dates.dayname(Date("2000-01-01"))
"Saturday"
```
"""
function dayname(dt::TimeType;locale::AbstractString="english")
    dayname(dayofweek(dt); locale=locale)
end

"""
    dayabbr(dt::TimeType; locale="english") -> String

Return the abbreviated name corresponding to the day of the week of the `Date` or `DateTime`
in the given `locale`.

# Examples
```jldoctest
julia> Dates.dayabbr(Date("2000-01-01"))
"Sat"
```
"""
function dayabbr(dt::TimeType;locale::AbstractString="english")
    dayabbr(dayofweek(dt); locale=locale)
end

# Convenience methods for each day
ismonday(dt::TimeType) = dayofweek(dt) == Mon
istuesday(dt::TimeType) = dayofweek(dt) == Tue
iswednesday(dt::TimeType) = dayofweek(dt) == Wed
isthursday(dt::TimeType) = dayofweek(dt) == Thu
isfriday(dt::TimeType) = dayofweek(dt) == Fri
issaturday(dt::TimeType) = dayofweek(dt) == Sat
issunday(dt::TimeType) = dayofweek(dt) == Sun

# i.e. 1st Monday? 2nd Monday? 3rd Wednesday? 5th Sunday?
"""
    dayofweekofmonth(dt::TimeType) -> Int

For the day of week of `dt`, return which number it is in `dt`'s month. So if the day of
the week of `dt` is Monday, then `1 = First Monday of the month, 2 = Second Monday of the
month, etc.` In the range 1:5.

# Examples
```jldoctest
julia> Dates.dayofweekofmonth(Date("2000-02-01"))
1

julia> Dates.dayofweekofmonth(Date("2000-02-08"))
2

julia> Dates.dayofweekofmonth(Date("2000-02-15"))
3
```
"""
function dayofweekofmonth(dt::TimeType)
    d = day(dt)
    return d < 8 ? 1 : d < 15 ? 2 : d < 22 ? 3 : d < 29 ? 4 : 5
end

# Total number of a day of week in the month
# e.g. are there 4 or 5 Mondays in this month?
const TWENTYNINE = BitSet([1, 8, 15, 22, 29])
const THIRTY = BitSet([1, 2, 8, 9, 15, 16, 22, 23, 29, 30])
const THIRTYONE = BitSet([1, 2, 3, 8, 9, 10, 15, 16, 17, 22, 23, 24, 29, 30, 31])

"""
    daysofweekinmonth(dt::TimeType) -> Int

For the day of week of `dt`, return the total number of that day of the week in `dt`'s
month. Returns 4 or 5. Useful in temporal expressions for specifying the last day of a week
in a month by including `dayofweekofmonth(dt) == daysofweekinmonth(dt)` in the adjuster
function.

# Examples
```jldoctest
julia> Dates.daysofweekinmonth(Date("2005-01-01"))
5

julia> Dates.daysofweekinmonth(Date("2005-01-04"))
4
```
"""
function daysofweekinmonth(dt::TimeType)
    y, m, d = yearmonthday(dt)
    ld = daysinmonth(y, m)
    return ld == 28 ? 4 : ld == 29 ? ((d in TWENTYNINE) ? 5 : 4) :
           ld == 30 ? ((d in THIRTY) ? 5 : 4) :
           (d in THIRTYONE) ? 5 : 4
end

### Months
const January, February, March, April, May, June = 1, 2, 3, 4, 5, 6
const July, August, September, October, November, December = 7, 8, 9, 10, 11, 12
const Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12

monthname(month::Integer, locale::DateLocale) = locale.months[month]
monthabbr(month::Integer, locale::DateLocale) = locale.months_abbr[month]
monthname(month::Integer; locale::AbstractString="english") = monthname(month, LOCALES[locale])
monthabbr(month::Integer; locale::AbstractString="english") = monthabbr(month, LOCALES[locale])

"""
    monthname(dt::TimeType; locale="english") -> String

Return the full name of the month of the `Date` or `DateTime` in the given `locale`.

# Examples
```jldoctest
julia> Dates.monthname(Date("2005-01-04"))
"January"
```
"""
function monthname(dt::TimeType; locale::AbstractString="english")
    monthname(month(dt); locale=locale)
end

"""
    monthabbr(dt::TimeType; locale="english") -> String

Return the abbreviated month name of the `Date` or `DateTime` in the given `locale`.

# Examples
```jldoctest
julia> Dates.monthabbr(Date("2005-01-04"))
"Jan"
```
"""
function monthabbr(dt::TimeType; locale::AbstractString="english")
    monthabbr(month(dt); locale=locale)
end

"""
    daysinmonth(dt::TimeType) -> Int

Return the number of days in the month of `dt`. Value will be 28, 29, 30, or 31.

# Examples
```jldoctest
julia> Dates.daysinmonth(Date("2000-01"))
31

julia> Dates.daysinmonth(Date("2001-02"))
28

julia> Dates.daysinmonth(Date("2000-02"))
29
```
"""
daysinmonth(dt::TimeType) = ((y, m) = yearmonth(dt); return daysinmonth(y, m))

### Years
"""
    isleapyear(dt::TimeType) -> Bool

Return `true` if the year of `dt` is a leap year.

# Examples
```jldoctest
julia> Dates.isleapyear(Date("2004"))
true

julia> Dates.isleapyear(Date("2005"))
false
```
"""
isleapyear(dt::TimeType) = isleapyear(year(dt))

"""
    dayofyear(dt::TimeType) -> Int

Return the day of the year for `dt` with January 1st being day 1.
"""
dayofyear(dt::TimeType) = ((y, m, d) = yearmonthday(dt); return dayofyear(y, m, d))

daysinyear(dt::TimeType) = 365 + isleapyear(dt)

### Quarters
"""
    quarterofyear(dt::TimeType) -> Int

Return the quarter that `dt` resides in. Range of value is 1:4.
"""
function quarterofyear(dt::TimeType)
    m = month(dt)
    return m < 4 ? 1 : m < 7 ? 2 : m < 10 ? 3 : 4
end
const QUARTERDAYS = (0, 90, 181, 273)

"""
    dayofquarter(dt::TimeType) -> Int

Return the day of the current quarter of `dt`. Range of value is 1:92.
"""
dayofquarter(dt::TimeType) = dayofyear(dt) - QUARTERDAYS[quarterofyear(dt)]

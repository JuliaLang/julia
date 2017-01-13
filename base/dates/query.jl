# This file is a part of Julia. License is MIT: http://julialang.org/license

# Date Locales

immutable DateLocale
    months::Vector{String}
    months_abbr::Vector{String}
    days_of_week::Vector{String}
    days_of_week_abbr::Vector{String}
    month_to_value::Dict{String, Int}
    month_to_value_abbr::Dict{String, Int}
end

"""
    DateLocale(["January, "February",...], ["Jan", "Feb",...],
               ["Monday", "Tuesday",...], ["Mon", "Tue",...])

Create a locale for parsing textual month names. First argument is
a vector of 12 month names, the second is a vector of 12 abbreviated
month names.  This object is passed as the last argument to
`tryparsenext` and `format` defined for each `AbstractDateToken` type.
"""
function DateLocale(months::Vector, months_abbr::Vector,
                    days_of_week::Vector, days_of_week_abbr::Vector)
    to_val = Dict{String, Int}(lowercase(months[i])=>i for i in 1:length(months))
    to_val_abbr = Dict{String, Int}(lowercase(months_abbr[i])=>i for i in 1:length(months_abbr))
    DateLocale(months, months_abbr, days_of_week,
               days_of_week_abbr, to_val, to_val_abbr)
end

const ENGLISH = DateLocale(["January", "February", "March", "April",
                            "May", "June", "July", "August", "September",
                            "October", "November", "December"],
                           ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
                           ["Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday"],
                           ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])

const LOCALES = Dict{String, DateLocale}("english" => ENGLISH)


# Date functions

### Core query functions

# Monday = 1....Sunday = 7
dayofweek(days) = mod1(days,7)

# Number of days in year
"""
    daysinyear(dt::TimeType) -> Int

Returns 366 if the year of `dt` is a leap year, otherwise returns 365.
"""
daysinyear(y) = 365 + isleapyear(y)

# Day of the year
const MONTHDAYS = [0,31,59,90,120,151,181,212,243,273,304,334]
dayofyear(y,m,d) = MONTHDAYS[m] + d + (m > 2 && isleapyear(y))

### Days of the Week
"""
    dayofweek(dt::TimeType) -> Int64

Returns the day of the week as an `Int64` with `1 = Monday, 2 = Tuesday, etc.`.
"""
dayofweek(dt::TimeType) = dayofweek(days(dt))

const Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday = 1,2,3,4,5,6,7
const Mon,Tue,Wed,Thu,Fri,Sat,Sun = 1,2,3,4,5,6,7

dayname(dt::Integer, locale::DateLocale) = locale.days_of_week[dt]
dayabbr(dt::Integer, locale::DateLocale) = locale.days_of_week_abbr[dt]
dayname(dt::Integer;locale::AbstractString="english") = dayname(dt, LOCALES[locale])
dayabbr(dt::Integer;locale::AbstractString="english") = dayabbr(dt, LOCALES[locale])

"""
    dayname(dt::TimeType; locale="english") -> AbstractString

Return the full day name corresponding to the day of the week of the `Date` or `DateTime` in
the given `locale`.
"""
dayname(dt::TimeType;locale::AbstractString="english") =
   dayname(dayofweek(dt); locale=locale)

"""
    dayabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated name corresponding to the day of the week of the `Date` or `DateTime`
in the given `locale`.
"""
dayabbr(dt::TimeType;locale::AbstractString="english") =
   dayabbr(dayofweek(dt); locale=locale)

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

For the day of week of `dt`, returns which number it is in `dt`'s month. So if the day of
the week of `dt` is Monday, then `1 = First Monday of the month, 2 = Second Monday of the
month, etc.` In the range 1:5.
"""
dayofweekofmonth(dt::TimeType) = (d = day(dt); return d < 8 ? 1 :
    d < 15 ? 2 : d < 22 ? 3 : d < 29 ? 4 : 5)

# Total number of a day of week in the month
# e.g. are there 4 or 5 Mondays in this month?
const TWENTYNINE = IntSet([1,8,15,22,29])
const THIRTY = IntSet([1,2,8,9,15,16,22,23,29,30])
const THIRTYONE = IntSet([1,2,3,8,9,10,15,16,17,22,23,24,29,30,31])

"""
    daysofweekinmonth(dt::TimeType) -> Int

For the day of week of `dt`, returns the total number of that day of the week in `dt`'s
month. Returns 4 or 5. Useful in temporal expressions for specifying the last day of a week
in a month by including `dayofweekofmonth(dt) == daysofweekinmonth(dt)` in the adjuster
function.
"""
function daysofweekinmonth(dt::TimeType)
    y,m,d = yearmonthday(dt)
    ld = daysinmonth(y,m)
    return ld == 28 ? 4 : ld == 29 ? ((d in TWENTYNINE) ? 5 : 4) :
           ld == 30 ? ((d in THIRTY) ? 5 : 4) :
           (d in THIRTYONE) ? 5 : 4
end

### Months
const January,February,March,April,May,June = 1,2,3,4,5,6
const July,August,September,October,November,December = 7,8,9,10,11,12
const Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec = 1,2,3,4,5,6,7,8,9,10,11,12

monthname(dt::Integer, locale::DateLocale) = locale.months[dt]
monthabbr(dt::Integer, locale::DateLocale) = locale.months_abbr[dt]
monthname(dt::Integer; locale::AbstractString="english") =
    monthname(dt, LOCALES[locale])
monthabbr(dt::Integer; locale::AbstractString="english") =
    monthabbr(dt, LOCALES[locale])

"""
    monthname(dt::TimeType; locale="english") -> AbstractString

Return the full name of the month of the `Date` or `DateTime` in the given `locale`.
"""
monthname(dt::TimeType; locale::AbstractString="english") =
    monthname(month(dt); locale=locale)

"""
    monthabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated month name of the `Date` or `DateTime` in the given `locale`.
"""
monthabbr(dt::TimeType; locale::AbstractString="english") =
    monthabbr(month(dt); locale=locale)

"""
    daysinmonth(dt::TimeType) -> Int

Returns the number of days in the month of `dt`. Value will be 28, 29, 30, or 31.
"""
daysinmonth(dt::TimeType) = ((y,m) = yearmonth(dt); return daysinmonth(y,m))

### Years
"""
    isleapyear(dt::TimeType) -> Bool

Returns `true` if the year of `dt` is a leap year.
"""
isleapyear(dt::TimeType) = isleapyear(year(dt))

"""
    dayofyear(dt::TimeType) -> Int

Returns the day of the year for `dt` with January 1st being day 1.
"""
dayofyear(dt::TimeType) = ((y,m,d) = yearmonthday(dt); return dayofyear(y,m,d))

daysinyear(dt::TimeType) = 365 + isleapyear(dt)

### Quarters
"""
    quarterofyear(dt::TimeType) -> Int

Returns the quarter that `dt` resides in. Range of value is 1:4.
"""
function quarterofyear(dt::TimeType)
    m = month(dt)
    return m < 4 ? 1 : m < 7 ? 2 : m < 10 ? 3 : 4
end
const QUARTERDAYS = (0,90,181,273)

"""
    dayofquarter(dt::TimeType) -> Int

Returns the day of the current quarter of `dt`. Range of value is 1:92.
"""
dayofquarter(dt::TimeType) = dayofyear(dt) - QUARTERDAYS[quarterofyear(dt)]

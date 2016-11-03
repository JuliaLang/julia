# This file is a part of Julia. License is MIT: http://julialang.org/license

# Date functions

### Core query functions

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
@enum DayOfWeek Monday=1 Tuesday=2 Wednesday=3 Thursday=4 Friday=5 Saturday=6 Sunday=7

"""
    DayOfWeek(n::Integer)

An `Enum` with `DayOfWeek(1) == Monday`, `DayOfWeek(2) == Tuesday`, etc. Accepts an
arbitrary-sized integer, from which days will be computed modulo 7.

```jldoctest
julia> DayOfWeek(8) == Monday
true
```
"""
DayOfWeek(n::Integer) = convert(DayOfWeek, mod1(n, 7))

"""
    DayOfWeek(dt::TimeType)

Returns the day of the week of `dt` as an element of the enum `DayOfWeek`.
"""
DayOfWeek(dt::TimeType) = DayOfWeek(days(dt))


const Mon,Tue,Wed,Thu,Fri,Sat,Sun = Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday

const english_daysofweek = Dict(k => string(k) for k in instances(DayOfWeek))
const VALUETODAYOFWEEK = Dict{String,Dict{DayOfWeek,String}}("english"=>english_daysofweek)

const english_daysofweekabbr = Dict(k => string(k)[1:3] for k in instances(DayOfWeek))
const VALUETODAYOFWEEKABBR = Dict{String,Dict{DayOfWeek,String}}("english"=>english_daysofweekabbr)

dayname(dow::DayOfWeek;locale::AbstractString="english") = VALUETODAYOFWEEK[locale][dow]
dayabbr(dow::DayOfWeek;locale::AbstractString="english") = VALUETODAYOFWEEKABBR[locale][dow]

"""
    dayname(dt::TimeType; locale="english") -> AbstractString

Return the full day name corresponding to the day of the week of the `Date` or `DateTime` in
the given `locale`.
"""
dayname(dt::TimeType;locale::AbstractString="english") = VALUETODAYOFWEEK[locale][DayOfWeek(dt)]

"""
    dayabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated name corresponding to the day of the week of the `Date` or `DateTime`
in the given `locale`.
"""
dayabbr(dt::TimeType;locale::AbstractString="english") = VALUETODAYOFWEEKABBR[locale][DayOfWeek(dt)]

# Convenience methods for each day
ismonday(dt::TimeType) = DayOfWeek(dt) == Mon
istuesday(dt::TimeType) = DayOfWeek(dt) == Tue
iswednesday(dt::TimeType) = DayOfWeek(dt) == Wed
isthursday(dt::TimeType) = DayOfWeek(dt) == Thu
isfriday(dt::TimeType) = DayOfWeek(dt) == Fri
issaturday(dt::TimeType) = DayOfWeek(dt) == Sat
issunday(dt::TimeType) = DayOfWeek(dt) == Sun

# i.e. 1st Monday? 2nd Monday? 3rd Wednesday? 5th Sunday?
"""
    dayofweekofmonth(dt::TimeType) -> Int

For the day of week of `dt`, returns which number it is in `dt`'s month. So if the day of
the week of `dt` is Monday, then `1 = First Monday of the month, 2 = Second Monday of the
month, etc.` In the range 1:5.

See also `daysofweekinmonth`.
"""
dayofweekofmonth(dt::TimeType) = fld1(dayofmonth(dt), 7)

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
const english_months = Dict(1=>"January",2=>"February",3=>"March",4=>"April",
                            5=>"May",6=>"June",7=>"July",8=>"August",9=>"September",
                            10=>"October",11=>"November",12=>"December")
const VALUETOMONTH = Dict{String,Dict{Int,String}}("english"=>english_months)
const englishabbr_months = Dict(1=>"Jan",2=>"Feb",3=>"Mar",4=>"Apr",
                                5=>"May",6=>"Jun",7=>"Jul",8=>"Aug",9=>"Sep",
                                10=>"Oct",11=>"Nov",12=>"Dec")
const VALUETOMONTHABBR = Dict{String,Dict{Int,String}}("english"=>englishabbr_months)
monthname(dt::Integer;locale::AbstractString="english") = VALUETOMONTH[locale][dt]
monthabbr(dt::Integer;locale::AbstractString="english") = VALUETOMONTHABBR[locale][dt]

"""
    monthname(dt::TimeType; locale="english") -> AbstractString

Return the full name of the month of the `Date` or `DateTime` in the given `locale`.
"""
monthname(dt::TimeType;locale::AbstractString="english") = VALUETOMONTH[locale][month(dt)]

"""
    monthabbr(dt::TimeType; locale="english") -> AbstractString

Return the abbreviated month name of the `Date` or `DateTime` in the given `locale`.
"""
monthabbr(dt::TimeType;locale::AbstractString="english") = VALUETOMONTHABBR[locale][month(dt)]

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
const QUARTERDAYS = [0,90,181,273]

"""
    dayofquarter(dt::TimeType) -> Int

Returns the day of the current quarter of `dt`. Range of value is 1:92.
"""
dayofquarter(dt::TimeType) = dayofyear(dt) - QUARTERDAYS[quarterofyear(dt)]

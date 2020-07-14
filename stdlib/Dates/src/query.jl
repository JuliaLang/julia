# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date Locales

struct DateLocale
    months::Vector{String}
    months_abbr::Vector{String}
    days_of_week::Vector{String}
    days_of_week_abbr::Vector{String}
    month_value::Dict{String, Int64}
    month_abbr_value::Dict{String, Int64}
    day_of_week_value::Dict{String, Int64}
    day_of_week_abbr_value::Dict{String, Int64}
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
for (ii, day_ind, short_day, long_day) in ((1, "first", :Mon, :Monday), (2, "second", :Tue, :Tuesday), (3, "third", :Wed, :Wednesday), (4, "fourth", :Thu, :Thursday), (5, "fifth", :Fri, :Friday), (6, "sixth", :Sat, :Saturday), (7, "seventh", :Sun, :Sunday))
    short_name = string(short_day)
    long_name = string(long_day)
    name_ind = day_ind
    ind_str = string(ii)
    @eval begin
        @doc """
        $($long_name)
        $($short_name)

        The $($name_ind) day of the week.

        # Examples
        ```jldoctest
        julia> $($long_name)
        $($ind_str)

        julia> $($short_name)
        $($ind_str)
        ```
        """ ($long_day, $short_day)
   end
end
dayname(day::Integer, locale::DateLocale) = locale.days_of_week[day]
dayabbr(day::Integer, locale::DateLocale) = locale.days_of_week_abbr[day]
dayname(day::Integer; locale::AbstractString="english") = dayname(day, LOCALES[locale])
dayabbr(day::Integer; locale::AbstractString="english") = dayabbr(day, LOCALES[locale])

"""
    dayname(dt::TimeType; locale="english") -> String
    dayname(day::Integer; locale="english") -> String

Return the full day name corresponding to the day of the week of the `Date` or `DateTime` in
the given `locale`. Also accepts `Integer`.

# Examples
```jldoctest
julia> Dates.dayname(Date("2000-01-01"))
"Saturday"

julia> Dates.dayname(4)
"Thursday"
```
"""
function dayname(dt::TimeType;locale::AbstractString="english")
    dayname(dayofweek(dt); locale=locale)
end

"""
    dayabbr(dt::TimeType; locale="english") -> String
    dayabbr(day::Integer; locale="english") -> String

Return the abbreviated name corresponding to the day of the week of the `Date` or `DateTime`
in the given `locale`. Also accepts `Integer`.

# Examples
```jldoctest
julia> Dates.dayabbr(Date("2000-01-01"))
"Sat"

julia> Dates.dayabbr(3)
"Wed"
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
"""
    January

The first month of the year.

# Examples
```jldoctest
julia> January
1
```
"""
const January = 1

"""
    Jan

Abbreviation for [`January`](@ref).

# Examples
```jldoctest
julia> Jan
1
```
"""
const Jan = 1

"""
    February

The second month of the year.

# Examples
```jldoctest
julia> February
2
```
"""
const February = 2

"""
    Feb

Abbreviation for [`February`](@ref).

# Examples
```jldoctest
julia> Feb
2
```
"""
const Feb = 2

"""
    March

The third month of the year.

# Examples
```jldoctest
julia> March
3
```
"""
const March = 3

"""
    Mar

Abbreviation for [`March`](@ref).

# Examples
```jldoctest
julia> Mar
3
```
"""
const Mar = 3

"""
    April

The fourth month of the year.

# Examples
```jldoctest
julia> April
4
```
"""
const April = 4

"""
    Apr

Abbreviation for [`April`](@ref).

# Examples
```jldoctest
julia> Apr
4
```
"""
const Apr = 4

"""
    May

The fifth month of the year.

# Examples
```jldoctest
julia> May
5
```
"""
const May = 5

"""
    June

The sixth month of the year.

# Examples
```jldoctest
julia> June
6
```
"""
const June = 6

"""
    Jun

Abbreviation for [`June`](@ref).

# Examples
```jldoctest
julia> Jun
6
```
"""
const Jun = 6

"""
    July

The seventh month of the year.

# Examples
```jldoctest
julia> July
7
```
"""
const July = 7

"""
    Jul

Abbreviation for [`July`](@ref).

# Examples
```jldoctest
julia> Jul
7
```
"""
const Jul = 7

"""
    August

The eighth month of the year.

# Examples
```jldoctest
julia> August
8
```
"""
const August = 8

"""
    Aug

Abbreviation for [`August`](@ref).

# Examples
```jldoctest
julia> Aug
8
```
"""
const Aug = 8

"""
    September

The ninth month of the year.

# Examples
```jldoctest
julia> September
9
```
"""
const September = 9

"""
    Sep

Abbreviation for [`September`](@ref).

# Examples
```jldoctest
julia> Sep
9
```
"""
const Sep = 9

"""
    October

The tenth month of the year.

# Examples
```jldoctest
julia> October
10
```
"""
const October = 10

"""
    Oct

Abbreviation for [`October`](@ref).

# Examples
```jldoctest
julia> Oct
10
```
"""
const Oct = 10

"""
    November

The eleventh month of the year.

# Examples
```jldoctest
julia> November
11
```
"""
const November = 11

"""
    Nov

Abbreviation for [`November`](@ref).

# Examples
```jldoctest
julia> Nov
11
```
"""
const Nov = 11

"""
    December

The last month of the year.

# Examples
```jldoctest
julia> December
12
```
"""
const December = 12

"""
    Dec

Abbreviation for [`December`](@ref).

# Examples
```jldoctest
julia> Dec
12
```
"""
const Dec = 12

monthname(month::Integer, locale::DateLocale) = locale.months[month]
monthabbr(month::Integer, locale::DateLocale) = locale.months_abbr[month]
monthname(month::Integer; locale::AbstractString="english") = monthname(month, LOCALES[locale])
monthabbr(month::Integer; locale::AbstractString="english") = monthabbr(month, LOCALES[locale])

"""
    monthname(dt::TimeType; locale="english") -> String
    monthname(month::Integer, locale="english") -> String


Return the full name of the month of the `Date` or `DateTime` or `Integer` in the given `locale`.

# Examples
```jldoctest
julia> Dates.monthname(Date("2005-01-04"))
"January"

julia> Dates.monthname(2)
"February"
```
"""
function monthname(dt::TimeType; locale::AbstractString="english")
    monthname(month(dt); locale=locale)
end

"""
    monthabbr(dt::TimeType; locale="english") -> String
    monthabbr(month::Integer, locale="english") -> String

Return the abbreviated month name of the `Date` or `DateTime` or `Integer` in the given `locale`.

# Examples
```jldoctest
julia> Dates.monthabbr(Date("2005-01-04"))
"Jan"

julia> monthabbr(2)
"Feb"
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

const QUARTERDAYS = (0, 31, 59, 0, 30, 61, 0, 31, 62, 0, 31, 61)

"""
    dayofquarter(dt::TimeType) -> Int

Return the day of the current quarter of `dt`. Range of value is 1:92.
"""
function dayofquarter(dt::TimeType)
    (y, m, d) = yearmonthday(dt)
    return QUARTERDAYS[m] + d + (m == 3 && isleapyear(y))
end

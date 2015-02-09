# Date functions

### Core query functions

# Monday = 1....Sunday = 7
dayofweek(days) = mod1(days,7)

# Number of days in year
daysinyear(y) = 365 + isleapyear(y)

# Day of the year
const MONTHDAYS = [0,31,59,90,120,151,181,212,243,273,304,334]
dayofyear(y,m,d) = MONTHDAYS[m] + d + (m > 2 && isleapyear(y))

### Days of the Week
dayofweek(dt::TimeType) = dayofweek(days(dt))

const Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday = 1,2,3,4,5,6,7
const Mon,Tue,Wed,Thu,Fri,Sat,Sun = 1,2,3,4,5,6,7
const english_daysofweek = Dict(1=>"Monday",2=>"Tuesday",3=>"Wednesday",
                                4=>"Thursday",5=>"Friday",6=>"Saturday",7=>"Sunday")
const VALUETODAYOFWEEK = Dict{UTF8String,Dict{Int,UTF8String}}("english"=>english_daysofweek)
const english_daysofweekabbr = Dict(1=>"Mon",2=>"Tue",3=>"Wed",
                                    4=>"Thu",5=>"Fri",6=>"Sat",7=>"Sun")
const VALUETODAYOFWEEKABBR = Dict{UTF8String,Dict{Int,UTF8String}}("english"=>english_daysofweekabbr)
dayname(dt::Integer;locale::AbstractString="english") = VALUETODAYOFWEEK[locale][dt]
dayabbr(dt::Integer;locale::AbstractString="english") = VALUETODAYOFWEEKABBR[locale][dt]
dayname(dt::TimeType;locale::AbstractString="english") = VALUETODAYOFWEEK[locale][dayofweek(dt)]
dayabbr(dt::TimeType;locale::AbstractString="english") = VALUETODAYOFWEEKABBR[locale][dayofweek(dt)]

# Convenience methods for each day
ismonday(dt::TimeType) = dayofweek(dt) == Mon
istuesday(dt::TimeType) = dayofweek(dt) == Tue
iswednesday(dt::TimeType) = dayofweek(dt) == Wed
isthursday(dt::TimeType) = dayofweek(dt) == Thu
isfriday(dt::TimeType) = dayofweek(dt) == Fri
issaturday(dt::TimeType) = dayofweek(dt) == Sat
issunday(dt::TimeType) = dayofweek(dt) == Sun

# i.e. 1st Monday? 2nd Monday? 3rd Wednesday? 5th Sunday?
dayofweekofmonth(dt::TimeType) = (d = day(dt); return d < 8 ? 1 :
    d < 15 ? 2 : d < 22 ? 3 : d < 29 ? 4 : 5)

# Total number of a day of week in the month
# e.g. are there 4 or 5 Mondays in this month?
const TWENTYNINE = IntSet([1,8,15,22,29])
const THIRTY = IntSet([1,2,8,9,15,16,22,23,29,30])
const THIRTYONE = IntSet([1,2,3,8,9,10,15,16,17,22,23,24,29,30,31])
function daysofweekinmonth(dt::TimeType)
    y,m,d = yearmonthday(dt)
    ld = daysinmonth(y,m)
    return ld == 28 ? 4 : ld == 29 ? ((d in TWENTYNINE) ? 5 : 4) :
           ld == 30 ? ((d in THIRTY) ? 5 : 4) :
           (d in THIRTYONE) ? 5 : 4
end

@vectorize_1arg TimeType dayname
@vectorize_1arg TimeType dayabbr
@vectorize_1arg TimeType dayofweek
@vectorize_1arg TimeType dayofweekofmonth
@vectorize_1arg TimeType daysofweekinmonth

### Months
const January,February,March,April,May,June = 1,2,3,4,5,6
const July,August,September,October,November,December = 7,8,9,10,11,12
const Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec = 1,2,3,4,5,6,7,8,9,10,11,12
const english_months = Dict(1=>"January",2=>"February",3=>"March",4=>"April",
                            5=>"May",6=>"June",7=>"July",8=>"August",9=>"September",
                            10=>"October",11=>"November",12=>"December")
const VALUETOMONTH = Dict{UTF8String,Dict{Int,UTF8String}}("english"=>english_months)
const englishabbr_months = Dict(1=>"Jan",2=>"Feb",3=>"Mar",4=>"Apr",
                                5=>"May",6=>"Jun",7=>"Jul",8=>"Aug",9=>"Sep",
                                10=>"Oct",11=>"Nov",12=>"Dec")
const VALUETOMONTHABBR = Dict{UTF8String,Dict{Int,UTF8String}}("english"=>englishabbr_months)
monthname(dt::Integer;locale::AbstractString="english") = VALUETOMONTH[locale][dt]
monthabbr(dt::Integer;locale::AbstractString="english") = VALUETOMONTHABBR[locale][dt]
monthname(dt::TimeType;locale::AbstractString="english") = VALUETOMONTH[locale][month(dt)]
monthabbr(dt::TimeType;locale::AbstractString="english") = VALUETOMONTHABBR[locale][month(dt)]

daysinmonth(dt::TimeType) = ((y,m) = yearmonth(dt); return daysinmonth(y,m))

@vectorize_1arg TimeType monthname
@vectorize_1arg TimeType monthabbr
@vectorize_1arg TimeType daysinmonth

### Years
isleapyear(dt::TimeType) = isleapyear(year(dt))

dayofyear(dt::TimeType) = ((y,m,d) = yearmonthday(dt); return dayofyear(y,m,d))

daysinyear(dt::TimeType) = 365 + isleapyear(dt)

@vectorize_1arg TimeType isleapyear
@vectorize_1arg TimeType dayofyear
@vectorize_1arg TimeType daysinyear

### Quarters
function quarterofyear(dt::TimeType)
    m = month(dt)
    return m < 4 ? 1 : m < 7 ? 2 : m < 10 ? 3 : 4
end
const QUARTERDAYS = [0,90,181,273]
dayofquarter(dt::TimeType) = dayofyear(dt) - QUARTERDAYS[quarterofyear(dt)]

@vectorize_1arg TimeType quarterofyear
@vectorize_1arg TimeType dayofquarter

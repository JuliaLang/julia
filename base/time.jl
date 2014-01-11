module Time

import Base: hash, isless, isequal, isfinite, convert, precision,
             typemax, typemin, zero, one, string, show,
             step, next, colon, last, +, -, *, /, div

export Calendar, ISOCalendar, Timezone, UTC,
    Date, Datetime, UTCDatetime, ISODatetime,
    Period, Year, Month, Week, Day, Hour, Minute, Second, Millisecond,
    # accessors
    year, month, week, day, hour, minute, second, millisecond,
    # date functions
    monthname, monthabr, dayname, dayabr, unix2datetime, now,
    isleap, lastdayofmonth, dayofweek, dayofyear,
    dayofweekofmonth, daysofweekinmonth, firstdayofweek, lastdayofweek,
    recur, calendar, timezone, precision, ISOFormat,
    # consts
    Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday,
    Mon,Tue,Wed,Thu,Fri,Sat,Sun,
    January, February, March, April, May, June, July,
    August, September, October, November, December,
    Jan,Feb,Mar,Apr,Jun,Jul,Aug,Sep,Oct,Nov,Dec

abstract AbstractTime
abstract Calendar <: AbstractTime

# ISOCalendar Implements the ISO 8601 standard (en.wikipedia.org/wiki/ISO_8601)
# Notably based on the proleptic Gregorian calendar
immutable ISOCalendar <: Calendar end

abstract Timezone <: AbstractTime
immutable UTC <: Timezone end

abstract Period     <: AbstractTime
abstract DatePeriod <: Period
abstract TimePeriod <: Period

immutable Year           <: DatePeriod
    years::Int64
    Year(x::Real)        = new(int64(x))
end
immutable Month          <: DatePeriod
    months::Int64
    Month(x::Real)       = new(int64(x))
end
immutable Week           <: DatePeriod
    weeks::Int64
    Week(x::Real)        = new(int64(x))
end
immutable Day            <: DatePeriod
    days::Int64
    Day(x::Real)         = new(int64(x))
end
immutable Hour           <: TimePeriod
    h::Int64
    Hour(x::Real)        = new(int64(x))
end
immutable Minute         <: TimePeriod
    m::Int64
    Minute(x::Real)      = new(int64(x))
end
immutable Second         <: TimePeriod
    s::Int64
    Second(x::Real)      = new(int64(x))
end
immutable Millisecond    <: TimePeriod
    ms::Int64
    Millisecond(x::Real) = new(int64(x))
end

# TimeTypes wrap Period types to provide human representations of time
abstract TimeType <: AbstractTime

# The Datetime type is a generic Period wrapper
immutable Datetime{P<:Period,T<:Timezone,C<:Calendar} <: TimeType
    instant::P
end 
typealias UTCDatetime Datetime{Millisecond,UTC,ISOCalendar}
typealias ISODatetime{T} Datetime{Millisecond,T,ISOCalendar}

immutable Date <: TimeType
    instant::Day
    Date(x::Day) = new(x)
end

# Convert y,m,d to # of Rata Die days
yeardays(y) = 365y + div(y,4) - div(y,100) + div(y,400)
const monthdays = [306,337,0,31,61,92,122,153,184,214,245,275]
function totaldays(y,m,d)
    @inbounds mdays = monthdays[m]
    return d + mdays + yeardays(m < 3 ? y - 1 : y) - 306
end
include("leaps.jl")

# UTC constructor with defaults
function Datetime(y,m=1,d=1,h=0,mi=0,s=0,ms=0,tz::Type{UTC}=UTC)
    # RFC: Month is the only Period we check because it can throw a
    # BoundsError() in construction (see totaldays())
    # all other Periods "roll over"; should we make these consistent?
    0 < m < 13 || error("Month out of range")
    rata = ms + 1000*(s + 60mi + 3600h + 86400*totaldays(y,m,d))
    return UTCDatetime(Millisecond(
        rata + (s == 60 ? setleapsecond(rata) : setleaps(rata))))
end
# Generic constructor to handle Timezone data loading
# TODO: Keyword arg to disambiguate daylight savings time input?
# function Datetime(y,m,d,h,mi,s,ms,tz)
     # Timezone data lazy loading
     # Define Datetime constructor for tz
     # getoffset, getoffsetsecond, setoffset, setoffsetsecond
     # string abr
     # return apply(Datetime,(y,m,d,h,mi,s,ms,tz))
# end
# Datetime(y,m,d,h,mi,s,tz::Timezone) = Datetime(y,m,d,h,mi,s,0,tz)
# Datetime(y,m,d,h,mi,s,ms,tz::String) = Datetime(y,m,d,h,mi,s,0,timezone(tz))
# Datetime(y,m,d,h,mi,s,tz::String) = Datetime(y,m,d,h,mi,s,0,timezone(tz))
# timezone(tz::String) = lookup olsen timezone string

function Date(y,m=1,d=1)
    0 < m < 13 || error("Month out of range")
    return Date(Day(totaldays(y,m,d)))
end

# Convert # of Rata Die days to proleptic Gregorian calendar y,m,d,w
# Reference: http://mysite.verizon.net/aesir_research/date/date0.htm
function _day2date(days)
    z = days + 306; h = 100z - 25; a = div(h,3652425); b = a - div(a,4);
    y = div(100b+h,36525); c = b + z - 365y - div(y,4); m = div(5c+456,153); 
    d = c - div(153m-457,5); return m > 12 ? (y+1,m-12,d) : (y,m,d)
end
function _year(days)
   z = days + 306; h = 100z - 25; a = div(h,3652425); b = a - div(a,4);
   y = div(100b+h,36525); c = b + z - 365y - div(y,4); m = div(5c+456,153); 
   return m > 12 ? y+1 : y
end 
function _month(days)
    z = days + 306; h = 100z - 25; a = div(h,3652425); b = a - div(a,4);
    y = div(100b+h,36525); c = b + z - 365y - div(y,4); m = div(5c+456,153); 
    return m > 12 ? m-12 : m
end
function _day(days)
    z = days + 306; h = 100z - 25; a = div(h,3652425); b = a - div(a,4);
    y = div(100b+h,36525); c = b + z - 365y - div(y,4); m = div(5c+456,153); 
    return c - div(153m-457,5)
end
# https://en.wikipedia.org/wiki/Talk:ISO_week_date#Algorithms
function _week(days)
    w = div(abs(days-1),7) % 20871
    c,w = divrem((w + (w >= 10435)),5218)
    w = (w*28+[15,23,3,11][c+1]) % 1461
    return div(w,28) + 1
end

# Accessor functions
getoffset(::Type{UTC},ms)       = getleaps(ms)
getoffsetsecond(::Type{UTC},ms) = getleapsecond(ms)
getabr(::Type{UTC},ms)          = "UTC"
setoffset(::Type{UTC},ms)       = setleaps(ms)

_days(dt::Date)               = dt.instant.days
_days{T}(dt::ISODatetime{T})  = div(dt.instant.ms - getoffset(T,dt.instant.ms),86400000)
year(dt::TimeType)            = _year(_days(dt))
month(dt::TimeType)           = _month(_days(dt))
week(dt::TimeType)            = _week(_days(dt))
day(dt::TimeType)             = _day(_days(dt))
hour{T}(dt::ISODatetime{T})   = div(dt.instant.ms - getoffset(T,dt.instant.ms),3600000) % 24
minute{T}(dt::ISODatetime{T}) = div(dt.instant.ms - getoffset(T,dt.instant.ms),60000) % 60
function second{T}(dt::ISODatetime{T})
    s = div(dt.instant.ms - getoffsetsecond(T,dt.instant.ms),1000) % 60
    return s == 0 ? (dt.instant.ms - (dt.instant.ms % 1000) in GETLEAPS ? 60 : 0) : s
end
millisecond(dt::Datetime) = dt.instant.ms % 1000

# Conversion/Promotion
Datetime(dt::Date) = Datetime(year(dt),month(dt),day(dt))
#Datetime(dt::Date,tz::Type{T}) = Datetime(year(dt),month(dt),day(dt),0,0,0,0,tz)
#Datetime(dt::Date,tz::String) = Datetime(year(dt),month(dt),day(dt),0,0,0,0,timezone(tz))
Date(dt::Datetime) = Date(year(dt),month(dt),day(dt))
#different calendars?
#different timezones?
#different precision levels?

# Traits, Equality
hash(dt::TimeType) = hash(dt.instant)
isless{T<:TimeType}(x::T,y::T) = isless(x.instant,y.instant)
isequal{T<:TimeType}(x::T,y::T) = isequal(x.instant,y.instant)
isfinite(::TimeType) = true
calendar{P,T,C}(dt::Datetime{P,T,C}) = C
calendar(dt::Date) = ISOCalendar
timezone{P,T,C}(dt::Datetime{P,T,C}) = T
precision{P,T,C}(dt::Datetime{P,T,C}) = P
precision(dt::Date) = Day
typemax{D<:Datetime}(::Type{D}) = Datetime(292277024,12,31,23,59,59)
typemin{D<:Datetime}(::Type{D}) = Datetime(-292277023,1,1,0,0,0)
typemax(::Type{Date}) = Date(252522163911149,12,31)
typemin(::Type{Date}) = Date(-252522163911150,1,1)

# TODO: optimize this?
function string{T}(dt::ISODatetime{T})
    y,m,d = _day2date(_days(dt))
    h,mi,s = hour(dt),minute(dt),second(dt)
    yy = y < 0 ? lpad(y,5,"0") : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    hh = lpad(h,2,"0")
    mii = lpad(mi,2,"0")
    ss = lpad(s,2,"0")
    ms = millisecond(dt) == 0 ? "" : string(millisecond(dt)/1000)[2:end]
    abr = getabr(T,dt.instant.ms)
    return "$yy-$mm-$(dd)T$hh:$mii:$ss$ms $abr"
end
show{T}(io::IO,x::ISODatetime{T}) = print(io,string(x))
function string(dt::Date)
    y,m,d = _day2date(dt.instant.days)
    yy = y < 0 ? lpad(y,5,"0") : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    return "$yy-$mm-$dd"
end
show(io::IO,x::Date) = print(io,string(x))

# Date functions
const Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday = 1,2,3,4,5,6,7
const January,February,March,April,May,June = 1,2,3,4,5,6
const July,August,September,October,November,December = 7,8,9,10,11,12
const Mon,Tue,Wed,Thu,Fri,Sat,Sun = 1,2,3,4,5,6,0
const Jan,Feb,Mar,Apr,Jun,Jul,Aug,Sep,Oct,Nov,Dec = 1,2,3,4,5,6,7,8,9,10,11,12
const DAYSOFWEEK = [1=>"Monday",2=>"Tuesday",3=>"Wednesday",
                    4=>"Thursday",5=>"Friday",6=>"Saturday",0=>"Sunday"]
const DAYSOFWEEKABR = [1=>"Mon",2=>"Tue",3=>"Wed",
                       4=>"Thu",5=>"Fri",6=>"Sat",0=>"Sun"]
const MONTHS = [1=>"January",2=>"February",3=>"March",4=>"April",
                5=>"May",6=>"June",7=>"July",8=>"August",9=>"September",
                10=>"October",11=>"November",12=>"December"]
const MONTHSABR = [1=>"Jan",2=>"Feb",3=>"Mar",4=>"Apr",
                   5=>"May",6=>"Jun",7=>"Jul",8=>"Aug",9=>"Sep",
                   10=>"Oct",11=>"Nov",12=>"Dec"]
# These two are used in Datetime parsing
const RMONTHS = ["january"=>1,"february"=>2,"march"=>3,"april"=>4,
                 "may"=>5,"june"=>6,"july"=>7,"august"=>8,"september"=>9,
                 "october"=>10,"november"=>11,"december"=>12]
const RMONTHSABR = ["jan"=>1,"feb"=>2,"mar"=>3,"apr"=>4,
                    "may"=>5,"jun"=>6,"jul"=>7,"aug"=>8,"sep"=>9,
                    "oct"=>10,"nov"=>11,"dec"=>12]
monthname(dt::TimeType) = get(MONTHS,month(dt),"January")
monthabr(dt::TimeType) = get(MONTHSABR,month(dt),"Jan")
dayname(dt::TimeType) = get(DAYSOFWEEK,dayofweek(dt),"Monday")
dayabr(dt::TimeType) = get(DAYSOFWEEKABR,dayofweek(dt),"Mon")

const DAYSINMONTH = [31,28,31,30,31,30,31,31,30,31,30,31]
_isleap(y) = ((y % 4 == 0) && (y % 100 != 0)) || (y % 400 == 0)
function _lastdayofmonth(y,m)
    @inbounds d = DAYSINMONTH[m]
    return d + (m == 2 && _isleap(y))
end
isleap(dt::TimeType) = _isleap(year(dt))
lastdayofmonth(dt::TimeType) = _lastdayofmonth(year(dt),month(dt))
dayofweek(dt::TimeType) = _days(dt) % 7
# i.e. 1st Monday? 2nd Monday? 3rd Wednesday? 5th Sunday?
dayofweekofmonth(dt::TimeType) = (d = day(dt); return d < 8 ? 1 : 
    d < 15 ? 2 : d < 22 ? 3 : d < 29 ? 4 : 5)
# Total number of a day of week in the month
# i.e. are there 4 or 5 Mondays in this month?
function daysofweekinmonth(dt::TimeType)
    d,ld = day(dt),lastdayofmonth(dt)
    return ld == 28 ? 4 : ld == 29 ? ((d in [1,8,15,22,29]) ? 5 : 4) :
           ld == 30 ? ((d in [1,2,8,9,15,16,22,23,29,30]) ? 5 : 4) :
           (d in [1,2,3,8,9,10,15,16,17,22,23,24,29,30,31]) ? 5 : 4
end
# TODO: check these with daylight savings
function firstdayofweek(dt::UTCDatetime)
    d = dayofweek(dt)
    return UTCDatetime(Millisecond(dt.instant.ms - 86400000*(d-1)))
end
firstdayofweek(dt::Date) = Date(dt.instant - dayofweek(dt) + 1)
function lastdayofweek (dt::UTCDatetime)
    d = dayofweek(dt)
    return UTCDatetime(Millisecond(dt.instant.ms + 86400000*(7-d)))
end
lastdayofweek(dt::Date) = Date(dt.instant + (7-dayofweek(dt)))
dayofyear(dt::TimeType) = _days(dt) - totaldays(year(dt),1,1) + 1
@vectorize_1arg TimeType isleap
@vectorize_1arg TimeType lastdayofmonth
@vectorize_1arg TimeType dayofweek
@vectorize_1arg TimeType dayofweekofmonth
@vectorize_1arg TimeType daysofweekinmonth
@vectorize_1arg TimeType firstdayofweek
@vectorize_1arg TimeType lastdayofweek
@vectorize_1arg TimeType dayofyear

const UNIXEPOCH = 62135683200000 #Rata Die milliseconds for 1970-01-01T00:00:00 UTC
function unix2datetime{T<:Timezone}(x,::Type{T}=UTC)
    return Datetime{Millisecond,T,ISOCalendar}(Millisecond(
        UNIXEPOCH + x + getoffset(T,UNIXEPOCH + x)))
end
now{T<:Timezone}(tz::Type{T}=UTC) = unix2datetime(int64(1000*time()),tz)

#wrapping arithmetic
addwrap(y,m) = (v = (y + m)    % 12; return v == 0 ? 12 : v)
subwrap(y,m) = (v = abs(y - m) % 12; return v == 0 ? 12 : v)
addwrap(y,m,m2) = y + (m + m2 > 12 ? max(1, div(m2,12)) : 0)
subwrap(y,m,m2) = y - (m - m2 < 1  ? max(1, div(m2,12)) : 0)

#Datetime arithmetic
for op in (:+,:*,:%,:/)
    @eval ($op)(x::TimeType,y::TimeType) = error("Operation not defined for TimeTypes")
end
(+)(x::TimeType) = x
(-)(x::TimeType,y::TimeType) = x.instant - y.instant

function (+)(dt::Datetime,y::Year)
    oy,m,d = _day2date(_days(dt)); ny = oy+y.years; ld = _lastdayofmonth(ny,m)
    return Datetime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt) == 60 ? 59 : second(dt))
end
function (+)(dt::Date,y::Year)
    oy,m,d = _day2date(_days(dt)); ny = oy+y.years; ld = _lastdayofmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end
function (-)(dt::Datetime,y::Year)
    oy,m,d = _day2date(_days(dt)); ny = oy-y.years; ld = _lastdayofmonth(ny,m)
    return Datetime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt) == 60 ? 59 : second(dt))
end
function (-)(dt::Date,y::Year)
    oy,m,d = _day2date(_days(dt)); ny = oy-y.years; ld = _lastdayofmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end
function (+)(dt::Datetime,z::Month) 
    y,m,d = _day2date(_days(dt))
    ny = addwrap(y,m,z.months)
    mm = addwrap(m,z.months); ld = _lastdayofmonth(ny,mm)
    return Datetime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt) == 60 ? 59 : second(dt))
end
function (+)(dt::Date,z::Month) 
    y,m,d = _day2date(_days(dt))
    ny = addwrap(y,m,z.months)
    mm = addwrap(m,z.months); ld = _lastdayofmonth(ny,mm)
    return Date(ny,mm,d <= ld ? d : ld)
end
function (-)(dt::Datetime,z::Month) 
    y,m,d = _day2date(_days(dt))
    ny = subwrap(y,m,z.months)
    mm = subwrap(m,z.months); ld = _lastdayofmonth(ny,mm)
    return Datetime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt) == 60 ? 59 : second(dt))
end
function (-)(dt::Date,z::Month) 
    y,m,d = _day2date(_days(dt))
    ny = subwrap(y,m,z.months)
    mm = subwrap(m,z.months); ld = _lastdayofmonth(ny,mm)
    return Date(ny,mm,d <= ld ? d : ld)
end
(+)(x::Date,y::Week) = return Date(x.instant + 7*y.weeks)
(-)(x::Date,y::Week) = return Date(x.instant - 7*y.weeks)
(+)(x::Date,y::Day)  = return Date(x.instant + y.days)
(-)(x::Date,y::Day)  = return Date(x.instant - y.days)
(+){T}(x::ISODatetime{T},y::Week)   = (x=x.instant.ms+604800000*value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(-){T}(x::ISODatetime{T},y::Week)   = (x=x.instant.ms-604800000*value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(+){T}(x::ISODatetime{T},y::Day)    = (x=x.instant.ms+86400000 *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(-){T}(x::ISODatetime{T},y::Day)    = (x=x.instant.ms-86400000 *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(+){T}(x::ISODatetime{T},y::Hour)   = (x=x.instant.ms+3600000  *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(-){T}(x::ISODatetime{T},y::Hour)   = (x=x.instant.ms-3600000  *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(+){T}(x::ISODatetime{T},y::Minute) = (x=x.instant.ms+60000    *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(-){T}(x::ISODatetime{T},y::Minute) = (x=x.instant.ms-60000    *value(y)-getoffset(T,x.instant.ms); return ISODatetime{T}(Millisecond(x+setoffset(T,x))))
(+){T}(x::ISODatetime{T},y::Second) = ISODatetime{T}(Millisecond(x.instant.ms+1000*y.s))
(-){T}(x::ISODatetime{T},y::Second) = ISODatetime{T}(Millisecond(x.instant.ms-1000*y.s))
(+){T}(x::ISODatetime{T},y::Millisecond) = ISODatetime{T}(Millisecond(x.instant.ms+y.ms))
(-){T}(x::ISODatetime{T},y::Millisecond) = ISODatetime{T}(Millisecond(x.instant.ms-y.ms))
(+)(y::Period,x::TimeType) = x + y
(-)(y::Period,x::TimeType) = x - y
typealias TimeTypePeriod Union(TimeType,Period)
(+){T<:TimeTypePeriod}(x::TimeTypePeriod, y::AbstractArray{T}) = reshape([x + y[i] for i in 1:length(y)], size(y))
(+){T<:TimeTypePeriod}(x::AbstractArray{T}, y::TimeTypePeriod) = reshape([x[i] + y for i in 1:length(x)], size(x))
(-){T<:TimeTypePeriod}(x::TimeTypePeriod, y::AbstractArray{T}) = reshape([x - y[i] for i in 1:length(y)], size(y))
(-){T<:TimeTypePeriod}(x::AbstractArray{T}, y::TimeTypePeriod) = reshape([x[i] - y for i in 1:length(x)], size(x))

# Temporal Expressions
function recur{T<:Time.TimeType}(fun::Function,start::T,stop::T,step::Time.Period=Time.Day(1))
    a = T[]
    i = start
    while i <= stop
        fun(i) && (push!(a,i))
        i += step
    end
    return a
end

# Datetime Parsing
# TODO: Handle generic offsets, i.e. +08:00, -05:00
type DateFormat
    year::Regex
    month::Regex
    monthoption::Int
    day::Regex
    sep::String
end
type TimeFormat
    regs::Array{Regex,1}
    tms::Array{String,1}
end
type DatetimeFormat
    date::DateFormat
    time::TimeFormat
    sep::String
end
# y-m-d
function DateFormat(dt::String)
    sep = (s = match(r"\W",dt)) == nothing ? "" : s.match
    y = m = d = r""
    monthoption = 0
    if sep == "" #separator-less format strings
        sep = r"([ymd])(?!\1)" #match character changes
        st = 1
        for i in eachmatch(sep,dt)
            mat = dt[st:i.offset]
            if 'y' in mat
                y = st == 1 ? Regex("^\\d{$(length(st:i.offset))}") :
                    st == 3 ? Regex("(?<=^\\d{2})\\d{$(length(st:i.offset))}") :
                    Regex("\\d{$(length(st:i.offset))}\$")
            elseif 'm' in mat
                m = st == 1 ? Regex("^\\d{$(length(st:i.offset))}") :
                    st == 3 ? Regex("(?<=^\\d{2})\\d{$(length(st:i.offset))}") :
                    st == 5 ? Regex("(?<=^\\d{4})\\d{$(length(st:i.offset))}") :
                    Regex("\\d{$(length(st:i.offset))}\$")
            else
                d = st == 1 ? Regex("^\\d{$(length(st:i.offset))}") :
                    st == 3 ? Regex("(?<=^\\d{2})\\d{$(length(st:i.offset))}") :
                    st == 5 ? Regex("(?<=^\\d{4})\\d{$(length(st:i.offset))}") :
                    Regex("\\d{$(length(st:i.offset))}\$")
            end
            st = i.offset+1
        end
        sep = ""
    else
        f = split(dt,sep)
        for i = 1:length(f)
            if 'y' in f[i]
                y = i == 1 ? Regex("^\\d+?(?=\\$sep)") : i == endof(f) ? Regex("(?<=\\$sep)\\d+?\$") : Regex("(?<=$sep)\\d+?(?=\\$sep)")
            elseif 'm' in f[i]
                l = length(f[i])
                if l > 2
                    m = i == 1 ? Regex("^.+?(?=\\$sep)") : i == endof(f) ? Regex("(?<=\\$sep).+?\$") : Regex("(?<=$sep).+?(?=\\$sep)")
                    monthoption = l > 3 ? 2 : 1
                else
                    m = i == 1 ? Regex("^\\d+?(?=\\$sep)") : i == endof(f) ? Regex("(?<=\\$sep)\\d+?\$") : Regex("(?<=$sep)\\d+?(?=\\$sep)")
                    monthoption = 0
                end
            else # contains(f[i],"d")
                d = i == 1 ? Regex("^\\d+?(?=\\$sep)") : i == endof(f) ? Regex("(?<=\\$sep)\\d+?\$") : Regex("(?<=$sep)\\d+?(?=\\$sep)")
            end
        end
    end
    return DateFormat(y,m,monthoption,d,sep)
end
function _format(dt::String,format::DateFormat)
    y = (yy = match(format.year,dt)) == nothing ? "0" : yy.match
    m = (mm = match(format.month,dt)) == nothing ? "0" : mm.match
    m = format.monthoption == 0 ? m : format.monthoption == 1 ? 
            get(Time.RMONTHSABR,lowercase(m),1) : get(Time.RMONTHS,lowercase(m),1)
    d = (dd = match(format.day,dt)) == nothing ? "0" : dd.match
    y == "" && (y = 0)
    m == "" && (m = 1)
    d == "" && (d = 1)
    return (int(y),int(m),int(d))
end
function TimeFormat(tm::String)
    regs = Array(Regex,0)
    tms = Array(String,0)
    for i = 1:(length(tm)-1)
        if tm[i] in "HMSsz" # if character
            if !in(tm[i+1],"HMSsz")
                sep = tm[i+1] == ' ' ? "\\s" : "\\$(tm[i+1])"
                if tm[i] in "HMSs"
                    push!(regs,Regex("\\d+?(?=$sep)"))
                    push!(tms,"$(tm[i])")
                else # tm[i] in "z"
                    push!(regs,Regex(".+?(?=$sep)"))
                    push!(tms,"$(tm[i])")
                end
            end
        elseif match(r"\W","$(tm[i])") != nothing # if delimiter
            sep = tm[i] == ' ' ? "\\s" : "\\$(tm[i])"
            push!(regs,Regex("$sep+"))
            push!(tms,string(tm[i]))
        else # unsupported character
           # pass
        end
        if (i+1) == endof(tm)
            if tm[i] in "HMSs"
                push!(regs,Regex("\\d+?\$"))
                push!(tms,"$(tm[i])")
            else # tm[i] in "z"
                push!(regs,Regex(".+?\$"))
                push!(tms,"$(tm[i])")
            end
            break
        end
    end
    return TimeFormat(regs,tms)
end
function _format(dt::String,f::TimeFormat)
    cursor = 1
    H = M = S = s = 0
    z = "UTC"
    for i = 1:length(f.regs)
        m = match(f.regs[i],dt[cursor:end])
        t = f.tms[i]
        if t == "H"
            H = m.match
        elseif t == "M"
            M = m.match
        elseif t == "S"
            S = m.match
        elseif t == "s"
            s = m.match
        elseif t == "z"
            z = m.match
        else # delimiter
           # pass 
        end
        cursor += length(m.match)
    end
    return (int(H),int(M),int(S),int(s)*10,eval(symbol(z)))
end
# Parses a format string
function DatetimeFormat(dt::String,sep::String="")
    if sep == ""
        sep = (s = match(r"(?<=[ymd])\W+(?=[HMSz])",dt)) == nothing ? "" : s.match
    end
    tm = ""
    if sep != ""
        dt, tm = split(dt,sep,2)
    end
    return DatetimeFormat(DateFormat(dt),TimeFormat(tm),sep)
end
function Datetime(dt::String,format::String;sep::String="")
    f = DatetimeFormat(format,sep)
    return Datetime(dt,f)
end
const ISOFormat = DatetimeFormat("yyyy-mm-ddTHH:MM:SS zzz","T")
function Datetime(dt::String,f::DatetimeFormat)
    if f.sep != ""
        dt, tm = split(dt,f.sep,2)
    else
        tm = ""
    end
    y, m, d = _format(dt,f.date)
    H, M, S, s, z = _format(tm,f.time)
    return Datetime(y,m,d,H,M,S,s,z)
end
function Date(dt::String,format::String)
    f = DateFormat(format)
    y, m, d = _format(dt,f)
    return Date(y,m,d)
end
# Best guess at Date
function Date(s::String)
    if ismatch(r"[\/|\-|\.|,|\s]",s)
        m = match(r"[\/|\-|\.|,|\s]",s)
        a,b,c = split(s,m.match)
        y = length(a) == 4 ? int64(a) : length(c) == 4 ? int64(c) : 0
        a,b,c = int64(a),int64(b),int64(c)
        y == 0 && (y = c > 49 ? c + 1900 : c + 2000)
        m,d = y == a ? (b,c) : (a,b)
        return m > 12 ? Date(y,d,m) : Date(y,m,d)
    else
        error("Can't parse Date, please use Date(datestring,format)")
    end
end
function Datetime{T<:String}(y::AbstractArray{T},x::T)
    f = DatetimeFormat(x)
    return reshape([Datetime(y[i],f) for i in 1:length(y)], size(y))
end
function Date{T<:String}(y::AbstractArray{T},x::T)
    f = DateFormat(x)
    return reshape([Date(y[i],f) for i in 1:length(y)], size(y))
end
function Date{T<:String}(y::AbstractArray{T})
    return reshape([Date(y[i]) for i in 1:length(y)], size(y))
end

#Period types
value(x::Year)          = x.years
value(x::Month)         = x.months
value(x::Week)          = x.weeks
value(x::Day)           = x.days
value(x::Hour)          = x.h
value(x::Minute)        = x.m
value(x::Second)        = x.s
value(x::Millisecond)   = x.ms
convert{R<:Real}(::Type{R},x::Period) = convert(R,value(x))

#Print/show/traits
_units(x::Period) = " " * lowercase(string(typeof(x).name)) * (abs(value(x)) == 1 ? "" : "s")
string{P<:Period}(x::P) = string(value(x),_units(x))
show(io::IO,x::Period) = print(io,string(x))
typemin{P<:Period}(::Type{P}) = P(typemin(Int64))
typemax{P<:Period}(::Type{P}) = P(typemax(Int64))
zero{P<:Period}(::Type{P}) = P(int64(0))
one{P<:Period}(::Type{P}) =  P(int64(1))

(-){P<:Period}(x::P) = P(-value(x))
isless{P<:Period}(x::P,y::P) = isless(value(x),value(y))
isequal{P<:Period}(x::P,y::P) = isequal(value(x),value(y))

#Period Arithmetic:
for op in (:+,:-,:*,:%,:div)
    @eval begin
    #Period-Period
    ($op){P<:Period}(x::P,y::P) = P(($op)(value(x),value(y)))
    ($op){P<:Period}(x::P, y::AbstractArray{P}) = reshape([P(($op)(value(x), value(y[i]))) for i in 1:length(y)], size(y))
    ($op){P<:Period}(x::AbstractArray{P}, y::P) = reshape([P(($op)(value(x[i]), value(y))) for i in 1:length(x)], size(x))
    #Period-Real
    ($op){P<:Period}(x::P,y::Real) = P(($op)(value(x),int64(y)))
    ($op){P<:Period}(x::Real,y::P) = P(($op)(int64(x),value(y)))
    end
end

periodisless{P<:Period}(::Type{P},::Type{Year}) = true
periodisless{P<:Period}(::Type{P},::Type{Month}) = true
periodisless(::Type{Year},::Type{Month}) = false
periodisless{P<:Period}(::Type{P},::Type{Week}) = true
periodisless(::Type{Year},::Type{Week}) = false
periodisless(::Type{Month},::Type{Week}) = false
periodisless{P<:Period}(::Type{P},::Type{Day}) = true
periodisless(::Type{Year},::Type{Day}) = false
periodisless(::Type{Month},::Type{Day}) = false
periodisless(::Type{Week},::Type{Day}) = false
periodisless{P<:Period}(::Type{P},::Type{Hour}) = false
periodisless(::Type{Minute},::Type{Hour}) = true
periodisless(::Type{Second},::Type{Hour}) = true
periodisless(::Type{Millisecond},::Type{Hour}) = true
periodisless{P<:Period}(::Type{P},::Type{Minute}) = false
periodisless(::Type{Second},::Type{Minute}) = true
periodisless(::Type{Millisecond},::Type{Minute}) = true
periodisless{P<:Period}(::Type{P},::Type{Second}) = false
periodisless(::Type{Millisecond},::Type{Second}) = true
periodisless{P<:Period}(::Type{P},::Type{Millisecond}) = false

periodisless{P1<:Period,P2<:Period}(x::P1,y::P2) = periodisless(P1,P2)
isless(x::Period,y::Period) = error("Can't compare Periods of different types")
isequal(x::Period,y::Period) = error("Can't compare Periods of different types")
isless(x::Period,y::Real) = error("Can't compare Period-Real")
isequal(x::Period,y::Real) = error("Can't compare Period-Real")
isless(y::Real,x::Period) = error("Can't compare Period-Real")
isequal(y::Real,x::Period) = error("Can't compare Period-Real")

# Stores multiple periods in greatest to least order by units, not values
type CompoundPeriod
    periods::Array{Period,1}
end
function string(x::CompoundPeriod)
    s = ""
    for p in x.periods
        s *= ", " * string(p)
    end
    return s[3:end]
end
show(io::IO,x::CompoundPeriod) = print(io,string(x))
# Year(1) + Day(1)
(+){P1<:Period,P2<:Period}(x::P1,y::P2) = CompoundPeriod(sort!(Period[x,y],rev=true,lt=periodisless))
(+)(x::CompoundPeriod,y::Period) = (sort!(push!(x.periods,y) ,rev=true,lt=periodisless); return x)
# Year(1) - Month(1)
(-){P1<:Period,P2<:Period}(x::P1,y::P2) = CompoundPeriod(sort!(Period[x,-y],rev=true,lt=periodisless))
(-)(x::CompoundPeriod,y::Period) = (sort!(push!(x.periods,-y),rev=true,lt=periodisless); return x)

function (+)(x::TimeType,y::CompoundPeriod)
    for p in y.periods
        x += p
    end
    return x
end
(+)(x::CompoundPeriod,y::TimeType) = y + x

#Range support for Period types
immutable PeriodRange{P<:Period} <: Ranges{Period}
    start::P
    step::P
    len::Int64
end
step(r::PeriodRange)  = r.step
last{P}(r::PeriodRange{P}) = r.start + P((r.len-1)*r.step)
next{P}(r::PeriodRange{P},i) = (r.start + P(i*r.step), i+1)
show(io::IO,r::PeriodRange) = print(io,r.start,':',r.step,':',last(r))

colon{P<:Period}(t1::P, s::P, t2::P) = PeriodRange{P}(t1, s, div(t2-t1,s) + int64(1))
colon{P<:Period}(t1::P, t2::P) = PeriodRange{P}(t1, one(P), value(t2)-value(t1) + int64(1))
(+){P<:Period}(r::PeriodRange{P},p::P) = PeriodRange{P}(r.start+p,r.step,r.len)
(-){P<:Period}(r::PeriodRange{P},p::P) = PeriodRange{P}(r.start-p,r.step,r.len)

end #module Time
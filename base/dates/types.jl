# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractTime

abstract Period     <: AbstractTime
abstract DatePeriod <: Period
abstract TimePeriod <: Period

for T in (:Year,:Month,:Week,:Day)
    @eval immutable $T <: DatePeriod
        value::Int64
        $T(v::Number) = new(v)
    end
end
for T in (:Hour,:Minute,:Second,:Millisecond)
    @eval immutable $T <: TimePeriod
        value::Int64
        $T(v::Number) = new(v)
    end
end

# Instant types represent different monotonically increasing timelines
abstract Instant <: AbstractTime

# UTInstant is based on UT seconds, or 1/86400th of a turn of the earth
immutable UTInstant{P<:Period} <: Instant
    periods::P
end

# Convenience default constructors
UTM(x) = UTInstant(Millisecond(x))
UTD(x) = UTInstant(Day(x))

# Calendar types provide rules for interpretating instant
# timelines in human-readable form.
abstract Calendar <: AbstractTime

# ISOCalendar implements the ISO 8601 standard (en.wikipedia.org/wiki/ISO_8601)
# Notably based on the proleptic Gregorian calendar
# ISOCalendar provides interpretation rules for UTInstants to civil date and time parts
immutable ISOCalendar <: Calendar end

abstract TimeZone
immutable UTC <: TimeZone end

# TimeTypes wrap Instants to provide human representations of time
abstract TimeType <: AbstractTime

# DateTime is a millisecond precision UTInstant interpreted by ISOCalendar
immutable DateTime <: TimeType
    instant::UTInstant{Millisecond}
    DateTime(instant::UTInstant{Millisecond}) = new(instant)
end

# DateTime is a day precision UTInstant interpreted by ISOCalendar
immutable Date <: TimeType
    instant::UTInstant{Day}
    Date(instant::UTInstant{Day}) = new(instant)
end

# Fallback constructors
_c(x) = convert(Int64,x)
DateTime(y,m=1,d=1,h=0,mi=0,s=0,ms=0) = DateTime(_c(y),_c(m),_c(d),_c(h),_c(mi),_c(s),_c(ms))
Date(y,m=1,d=1) = Date(_c(y),_c(m),_c(d))

# Convert y,m,d to # of Rata Die days
# Works by shifting the beginning of the year to March 1,
# so a leap day is the very last day of the year
const SHIFTEDMONTHDAYS = [306,337,0,31,61,92,122,153,184,214,245,275]
function totaldays(y,m,d)
    # If we're in Jan/Feb, shift the given year back one
    z = m < 3 ? y - 1 : y
    mdays = SHIFTEDMONTHDAYS[m]
    # days + month_days + year_days
    return d + mdays + 365z + fld(z,4) - fld(z,100) + fld(z,400) - 306
end

# If the year is divisible by 4, except for every 100 years, except for every 400 years
isleapyear(y) = ((y % 4 == 0) && (y % 100 != 0)) || (y % 400 == 0)

# Number of days in month
const DAYSINMONTH = [31,28,31,30,31,30,31,31,30,31,30,31]
daysinmonth(y,m) = DAYSINMONTH[m] + (m == 2 && isleapyear(y))

### CONSTRUCTORS ###
# Core constructors
"""
    DateTime(y, [m, d, h, mi, s, ms]) -> DateTime

Construct a `DateTime` type by parts. Arguments must be convertible to `Int64`.
"""
function DateTime(y::Int64,m::Int64=1,d::Int64=1,
                  h::Int64=0,mi::Int64=0,s::Int64=0,ms::Int64=0)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y,m)+1 || throw(ArgumentError("Day: $d out of range (1:$(daysinmonth(y,m)))"))
    -1 < h < 24 || throw(ArgumentError("Hour: $h out of range (0:23)"))
    -1 < mi < 60 || throw(ArgumentError("Minute: $mi out of range (0:59)"))
    -1 < s < 60 || throw(ArgumentError("Second: $s out of range (0:59)"))
    -1 < ms < 1000 || throw(ArgumentError("Millisecond: $ms out of range (0:999)"))
    rata = ms + 1000*(s + 60mi + 3600h + 86400*totaldays(y,m,d))
    return DateTime(UTM(rata))
end

"""
    Date(y, [m, d]) -> Date

Construct a `Date` type by parts. Arguments must be convertible to `Int64`.
"""
function Date(y::Int64,m::Int64=1,d::Int64=1)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y,m)+1 || throw(ArgumentError("Day: $d out of range (1:$(daysinmonth(y,m)))"))
    return Date(UTD(totaldays(y,m,d)))
end

# Convenience constructors from Periods
function DateTime(y::Year,m::Month=Month(1),d::Day=Day(1),
                  h::Hour=Hour(0),mi::Minute=Minute(0),
                  s::Second=Second(0),ms::Millisecond=Millisecond(0))
    return DateTime(value(y),value(m),value(d),
                        value(h),value(mi),value(s),value(ms))
end

Date(y::Year,m::Month=Month(1),d::Day=Day(1)) = Date(value(y),value(m),value(d))

# To allow any order/combination of Periods

"""
    DateTime(periods::Period...) -> DateTime

Construct a `DateTime` type by `Period` type parts. Arguments may be in any order. DateTime
parts not provided will default to the value of `Dates.default(period)`.
"""
function DateTime(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0); ms = Millisecond(0)
    for p in periods
        isa(p, Year) && (y = p::Year)
        isa(p, Month) && (m = p::Month)
        isa(p, Day) && (d = p::Day)
        isa(p, Hour) && (h = p::Hour)
        isa(p, Minute) && (mi = p::Minute)
        isa(p, Second) && (s = p::Second)
        isa(p, Millisecond) && (ms = p::Millisecond)
    end
    return DateTime(y,m,d,h,mi,s,ms)
end

"""
    Date(period::Period...) -> Date

Construct a `Date` type by `Period` type parts. Arguments may be in any order. `Date` parts
not provided will default to the value of `Dates.default(period)`.
"""
function Date(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    for p in periods
        isa(p, Year) && (y = p::Year)
        isa(p, Month) && (m = p::Month)
        isa(p, Day) && (d = p::Day)
    end
    return Date(y,m,d)
end

# Traits, Equality
Base.isfinite{T<:TimeType}(::Union{Type{T},T}) = true
calendar(dt::DateTime) = ISOCalendar
calendar(dt::Date) = ISOCalendar

"""
    eps(::DateTime) -> Millisecond
    eps(::Date) -> Day

Returns `Millisecond(1)` for `DateTime` values and `Day(1)` for `Date` values.
"""
Base.eps

Base.eps(dt::DateTime) = Millisecond(1)
Base.eps(dt::Date) = Day(1)

Base.typemax(::Union{DateTime,Type{DateTime}}) = DateTime(146138512,12,31,23,59,59)
Base.typemin(::Union{DateTime,Type{DateTime}}) = DateTime(-146138511,1,1,0,0,0)
Base.typemax(::Union{Date,Type{Date}}) = Date(252522163911149,12,31)
Base.typemin(::Union{Date,Type{Date}}) = Date(-252522163911150,1,1)
# Date-DateTime promotion, isless, ==
Base.promote_rule(::Type{Date},x::Type{DateTime}) = DateTime
Base.isless(x::Date,y::Date) = isless(value(x),value(y))
Base.isless(x::DateTime,y::DateTime) = isless(value(x),value(y))
Base.isless(x::TimeType,y::TimeType) = isless(promote(x,y)...)
==(x::TimeType,y::TimeType) = ===(promote(x,y)...)

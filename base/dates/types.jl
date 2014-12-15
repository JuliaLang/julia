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
end

# DateTime is a day precision UTInstant interpreted by ISOCalendar
immutable Date <: TimeType
    instant::UTInstant{Day}
end

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
function DateTime(y::Int64,m::Int64=1,d::Int64=1,
                  h::Int64=0,mi::Int64=0,s::Int64=0,ms::Int64=0)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y,m)+1 || throw(ArgumentError("Day: $d out of range (1:$daysinmonth(y,m))"))
    -1 < h < 24 || throw(ArgumentError("Hour: $h out of range (1:23)"))
    -1 < mi < 60 || throw(ArgumentError("Minute: $mi out of range (1:59)"))
    -1 < s < 60 || throw(ArgumentError("Second: $s out of range (1:59)"))
    -1 < ms < 1000 || throw(ArgumentError("Millisecond: $ms out of range (1:999)"))
    rata = ms + 1000*(s + 60mi + 3600h + 86400*totaldays(y,m,d))
    return DateTime(UTM(rata))
end
function Date(y::Int64,m::Int64=1,d::Int64=1)
    0 < m < 13 || throw(ArgumentError("Month: $m out of range (1:12)"))
    0 < d < daysinmonth(y,m)+1 || throw(ArgumentError("Day: $d out of range (1:$daysinmonth(y,m))"))
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
function DateTime(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    h = Hour(0); mi = Minute(0); s = Second(0); ms = Millisecond(0)
    for p in periods
        typeof(p) <: Year && (y = p)
        typeof(p) <: Month && (m = p)
        typeof(p) <: Day && (d = p)
        typeof(p) <: Hour && (h = p)
        typeof(p) <: Minute && (mi = p)
        typeof(p) <: Second && (s = p)
        typeof(p) <: Millisecond && (ms = p)
    end
    return DateTime(y,m,d,h,mi,s,ms)
end
function Date(periods::Period...)
    y = Year(1); m = Month(1); d = Day(1)
    for p in periods
        typeof(p) <: Year && (y = p)
        typeof(p) <: Month && (m = p)
        typeof(p) <: Day && (d = p)
    end
    return Date(y,m,d)
end

# Fallback constructors
_c(x) = convert(Int64,x)
DateTime(y,m=1,d=1,h=0,mi=0,s=0,ms=0) = DateTime(_c(y),_c(m),_c(d),_c(h),_c(mi),_c(s),_c(ms))
Date(y,m=1,d=1) = Date(_c(y),_c(m),_c(d))

# Traits, Equality
Base.isfinite{T<:TimeType}(::Union(Type{T},T)) = true
calendar(dt::DateTime) = ISOCalendar
calendar(dt::Date) = ISOCalendar
Base.precision(dt::DateTime) = Millisecond
Base.precision(dt::Date) = Day
Base.typemax(::Union(DateTime,Type{DateTime})) = DateTime(146138512,12,31,23,59,59)
Base.typemin(::Union(DateTime,Type{DateTime})) = DateTime(-146138511,1,1,0,0,0)
Base.typemax(::Union(Date,Type{Date})) = Date(252522163911149,12,31)
Base.typemin(::Union(Date,Type{Date})) = Date(-252522163911150,1,1)
# Date-DateTime promotion, isless, ==
Base.promote_rule(::Type{Date},x::Type{DateTime}) = DateTime
Base.isless(x::Date,y::Date) = isless(value(x),value(y))
Base.isless(x::DateTime,y::DateTime) = isless(value(x),value(y))
Base.isless(x::TimeType,y::TimeType) = isless(promote(x,y)...)
==(x::TimeType,y::TimeType) = ===(promote(x,y)...)

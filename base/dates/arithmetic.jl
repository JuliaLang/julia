# Instant arithmetic
for op in (:+,:*,:%,:/)
    @eval ($op)(x::Instant,y::Instant) = throw(ArgumentError("Operation not defined for Instants"))
end
(+)(x::Instant) = x
(-){T<:Instant}(x::T,y::T) = x.periods - y.periods

# DateTime arithmetic
for op in (:+,:*,:%,:/)
    @eval ($op)(x::TimeType,y::TimeType) = throw(ArgumentError("Operation not defined for TimeTypes"))
end
(+)(x::TimeType) = x
(-){T<:TimeType}(x::T,y::T) = x.instant - y.instant

function (+)(dt::DateTime,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy+value(y); ld = daysinmonth(ny,m)
    return DateTime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt))
end
function (+)(dt::Date,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy+value(y); ld = daysinmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end
function (-)(dt::DateTime,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy-value(y); ld = daysinmonth(ny,m)
    return DateTime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt))
end
function (-)(dt::Date,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy-value(y); ld = daysinmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end

# Date/DateTime-Month arithmetic
# monthwrap adds two months with wraparound behavior (i.e. 12 + 1 == 1)
monthwrap(m1,m2) = (v = mod1(m1+m2,12); return v < 0 ? 12 + v : v)
# yearwrap takes a starting year/month and a month to add and returns
# the resulting year with wraparound behavior (i.e. 2000-12 + 1 == 2001)
yearwrap(y,m1,m2) = y + fld(m1 + m2 - 1,12)

function (+)(dt::DateTime,z::Month) 
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,value(z))
    mm = monthwrap(m,value(z)); ld = daysinmonth(ny,mm)
    return DateTime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt))
end
function (+)(dt::Date,z::Month) 
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,value(z))
    mm = monthwrap(m,value(z)); ld = daysinmonth(ny,mm)
    return Date(ny,mm,d <= ld ? d : ld)
end
function (-)(dt::DateTime,z::Month) 
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,-value(z))
    mm = monthwrap(m,-value(z)); ld = daysinmonth(ny,mm)
    return DateTime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt))
end
function (-)(dt::Date,z::Month) 
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,-value(z))
    mm = monthwrap(m,-value(z)); ld = daysinmonth(ny,mm)
    return Date(ny,mm,d <= ld ? d : ld)
end
(+)(x::Date,y::Week) = return Date(UTD(value(x) + 7*value(y)))
(-)(x::Date,y::Week) = return Date(UTD(value(x) - 7*value(y)))
(+)(x::Date,y::Day)  = return Date(UTD(value(x) + y))
(-)(x::Date,y::Day)  = return Date(UTD(value(x) - y))
(+)(x::DateTime,y::Week)   = return DateTime(UTM(value(x)+604800000*value(y)))
(-)(x::DateTime,y::Week)   = return DateTime(UTM(value(x)-604800000*value(y)))
(+)(x::DateTime,y::Day)    = return DateTime(UTM(value(x)+86400000 *value(y)))
(-)(x::DateTime,y::Day)    = return DateTime(UTM(value(x)-86400000 *value(y)))
(+)(x::DateTime,y::Hour)   = return DateTime(UTM(value(x)+3600000  *value(y)))
(-)(x::DateTime,y::Hour)   = return DateTime(UTM(value(x)-3600000  *value(y)))
(+)(x::DateTime,y::Minute) = return DateTime(UTM(value(x)+60000    *value(y)))
(-)(x::DateTime,y::Minute) = return DateTime(UTM(value(x)-60000    *value(y)))
(+)(x::DateTime,y::Second)      = return DateTime(UTM(value(x)+1000*value(y)))
(-)(x::DateTime,y::Second)      = return DateTime(UTM(value(x)-1000*value(y)))
(+)(x::DateTime,y::Millisecond) = return DateTime(UTM(value(x)+value(y)))
(-)(x::DateTime,y::Millisecond) = return DateTime(UTM(value(x)-value(y)))
(+)(y::Period,x::TimeType) = x + y
(-)(y::Period,x::TimeType) = x - y

(.+){T<:TimeType}(x::AbstractArray{T}, y::Period) = reshape([i + y for i in x], size(x))
(.-){T<:TimeType}(x::AbstractArray{T}, y::Period) = reshape([i - y for i in x], size(x))
(.+){T<:TimeType}(y::Period, x::AbstractArray{T}) = x .+ y
(.-){T<:TimeType}(y::Period, x::AbstractArray{T}) = x .- y
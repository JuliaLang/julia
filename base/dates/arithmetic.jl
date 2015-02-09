# Instant arithmetic
(+)(x::Instant) = x
(-){T<:Instant}(x::T,y::T) = x.periods - y.periods

# TimeType arithmetic
(+)(x::TimeType) = x
(-){T<:TimeType}(x::T,y::T) = x.instant - y.instant

# TimeType-Year arithmetic
function (+)(dt::DateTime,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy+value(y); ld = daysinmonth(ny,m)
    return DateTime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt),millisecond(dt))
end
function (+)(dt::Date,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy+value(y); ld = daysinmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end
function (-)(dt::DateTime,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy-value(y); ld = daysinmonth(ny,m)
    return DateTime(ny,m,d <= ld ? d : ld,hour(dt),minute(dt),second(dt),millisecond(dt))
end
function (-)(dt::Date,y::Year)
    oy,m,d = yearmonthday(dt); ny = oy-value(y); ld = daysinmonth(ny,m)
    return Date(ny,m,d <= ld ? d : ld)
end

# TimeType-Month arithmetic
# monthwrap adds two months with wraparound behavior (i.e. 12 + 1 == 1)
monthwrap(m1,m2) = (v = mod1(m1+m2,12); return v < 0 ? 12 + v : v)
# yearwrap takes a starting year/month and a month to add and returns
# the resulting year with wraparound behavior (i.e. 2000-12 + 1 == 2001)
yearwrap(y,m1,m2) = y + fld(m1 + m2 - 1,12)

function (+)(dt::DateTime,z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,value(z))
    mm = monthwrap(m,value(z)); ld = daysinmonth(ny,mm)
    return DateTime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt),millisecond(dt))
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
    return DateTime(ny,mm,d <= ld ? d : ld,hour(dt),minute(dt),second(dt),millisecond(dt))
end
function (-)(dt::Date,z::Month)
    y,m,d = yearmonthday(dt)
    ny = yearwrap(y,m,-value(z))
    mm = monthwrap(m,-value(z)); ld = daysinmonth(ny,mm)
    return Date(ny,mm,d <= ld ? d : ld)
end
(+)(x::Date,y::Week) = return Date(UTD(value(x) + 7*value(y)))
(-)(x::Date,y::Week) = return Date(UTD(value(x) - 7*value(y)))
(+)(x::Date,y::Day)  = return Date(UTD(value(x) + value(y)))
(-)(x::Date,y::Day)  = return Date(UTD(value(x) - value(y)))
(+)(x::DateTime,y::Period)   = return DateTime(UTM(value(x)+toms(y)))
(-)(x::DateTime,y::Period)   = return DateTime(UTM(value(x)-toms(y)))
(+)(y::Period,x::TimeType) = x + y
(-)(y::Period,x::TimeType) = x - y

(.+){T<:TimeType}(x::AbstractArray{T}, y::Period) = reshape(T[i + y for i in x], size(x))
(.-){T<:TimeType}(x::AbstractArray{T}, y::Period) = reshape(T[i - y for i in x], size(x))
(.+){T<:TimeType}(y::Period, x::AbstractArray{T}) = x .+ y
(.-){T<:TimeType}(y::Period, x::AbstractArray{T}) = x .- y

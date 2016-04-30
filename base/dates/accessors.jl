# This file is a part of Julia. License is MIT: http://julialang.org/license

# Convert # of Rata Die days to proleptic Gregorian calendar y,m,d,w
# Reference: http://mysite.verizon.net/aesir_research/date/date0.htm
function yearmonthday(days)
    z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
    y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
    d = c - div(153m-457,5); return m > 12 ? (y+1,m-12,d) : (y,m,d)
end
function year(days)
   z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
   y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
   return m > 12 ? y+1 : y
end
function yearmonth(days)
    z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
    y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
    return m > 12 ? (y+1,m-12) : (y,m)
end
function month(days)
    z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
    y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
    return m > 12 ? m-12 : m
end
function monthday(days)
    z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
    y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
    d = c - div(153m-457,5); return m > 12 ? (m-12,d) : (m,d)
end
function day(days)
    z = days + 306; h = 100z - 25; a = fld(h,3652425); b = a - fld(a,4);
    y = fld(100b+h,36525); c = b + z - 365y - fld(y,4); m = div(5c+456,153);
    return c - div(153m-457,5)
end
# https://en.wikipedia.org/wiki/Talk:ISO_week_date#Algorithms
function week(days)
    w = div(abs(days-1),7) % 20871
    c,w = divrem((w + (w >= 10435)),5218)
    w = (w*28+[15,23,3,11][c+1]) % 1461
    return div(w,28) + 1
end

# Accessor functions
value(dt::TimeType) = dt.instant.periods.value
days(dt::Date) = value(dt)
days(dt::DateTime) = fld(value(dt),86400000)
year(dt::TimeType) = year(days(dt))
month(dt::TimeType) = month(days(dt))
week(dt::TimeType) = week(days(dt))
day(dt::TimeType) = day(days(dt))
hour(dt::DateTime)   = mod(fld(value(dt),3600000),24)
minute(dt::DateTime) = mod(fld(value(dt),60000),60)
second(dt::DateTime) = mod(fld(value(dt),1000),60)
millisecond(dt::DateTime) = mod(value(dt),1000)

dayofmonth(dt::TimeType) = day(dt)

yearmonth(dt::TimeType) = yearmonth(days(dt))
monthday(dt::TimeType) = monthday(days(dt))
yearmonthday(dt::TimeType) = yearmonthday(days(dt))

@vectorize_1arg TimeType year
@vectorize_1arg TimeType month
@vectorize_1arg TimeType day
@vectorize_1arg TimeType week
@vectorize_1arg DateTime hour
@vectorize_1arg DateTime minute
@vectorize_1arg DateTime second
@vectorize_1arg DateTime millisecond

@vectorize_1arg TimeType dayofmonth
@vectorize_1arg TimeType yearmonth
@vectorize_1arg TimeType monthday
@vectorize_1arg TimeType yearmonthday


# Documentation for exported accessors
for func in (:year, :month)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::TimeType) -> Int64

        The $($name) of a `Date` or `DateTime` as an `Int64`.
        """ $func(dt::TimeType)
    end
end

"""
    week(dt::TimeType) -> Int64

Return the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) of a `Date` or
`DateTime` as an `Int64`. Note that the first week of a year is the week that contains the
first Thursday of the year which can result in dates prior to January 4th being in the last
week of the previous year. For example `week(Date(2005,1,1))` is the 53rd week of 2004.
"""
week(dt::TimeType)

for func in (:day, :dayofmonth)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::TimeType) -> Int64

        The day of month of a `Date` or `DateTime` as an `Int64`.
        """ $func(dt::TimeType)
    end
end

"""
    hour(dt::DateTime) -> Int64

The hour of day of a `DateTime` as an `Int64`.
"""
hour(dt::DateTime)

for func in (:minute, :second, :millisecond)
    name = string(func)
    @eval begin
        @doc """
            $($name)(dt::DateTime) -> Int64

        The $($name) of a `DateTime` as an `Int64`.
        """ $func(dt::DateTime)
    end
end

for parts in (["year", "month"], ["month", "day"], ["year", "month", "day"])
    name = join(parts)
    func = Symbol(name)
    @eval begin
        @doc """
            $($name)(dt::TimeType) -> ($(join(repeated(Int64, length($parts)), ", ")))

        Simultaneously return the $(join($parts, ", ", " and ")) parts of a `Date` or
        `DateTime`.
        """ $func(dt::TimeType)
    end
end

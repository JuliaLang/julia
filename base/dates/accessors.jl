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

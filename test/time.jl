@test Time.yeardays(0) == 0
@test Time.yeardays(1) == 365
@test Time.yeardays(-1) == -365
@test Time.yeardays(100) == 36524
@test Time.yeardays(400) == 146097

@test Time.totaldays(0,12,31) == 0
@test Time.totaldays(1,1,1) == 1
@test Time.totaldays(1,1,2) == 2
@test Time.totaldays(2013,1,1) == 734869

@test Time._day2date(-1) == (0,12,30)
@test Time._year(-1) == 0
@test Time._month(-1) == 12
@test Time._day(-1) == 30
@test Time._day2date(0) == (0,12,31)
@test Time._year(0) == 0
@test Time._month(0) == 12
@test Time._day(0) == 31
@test Time._day2date(1) == (1,1,1)
@test Time._year(1) == 1
@test Time._month(1) == 1
@test Time._day(1) == 1
@test Time._day2date(730120) == (2000,1,1)
@test Time._year(730120) == 2000
@test Time._month(730120) == 1
@test Time._day(730120) == 1

test = Time.Datetime{Time.Millisecond,Time.UTC,Time.ISOCalendar}(Time.Millisecond(63492681625000))
@test Time.Datetime(2013) == test
@test Time.Datetime(2013,1) == test
@test Time.Datetime(2013,1,1) == test
@test Time.Datetime(2013,1,1,0) == test
@test Time.Datetime(2013,1,1,0,0) == test
@test Time.Datetime(2013,1,1,0,0,0) == test
@test Time.Datetime(2013,1,1,0,0,0,0) == test
@test Time.Datetime(2013,1,1,0,0,0,0,Time.UTC) == test
test = Time.Date(Time.Day(734869))
@test Time.Date(2013) == test
@test Time.Date(2013,1) == test
@test Time.Date(2013,1,1) == test

# Months must be in range
@test_throws Time.Datetime(2013,0,1)
@test_throws Time.Datetime(2013,13,1)

# Days/Hours/Minutes/Seconds/Milliseconds roll back/forward
@test Time.Datetime(2013,1,0) == Time.Datetime(2012,12,31)
@test Time.Datetime(2013,1,32) == Time.Datetime(2013,2,1)
@test Time.Datetime(2013,1,1,24) == Time.Datetime(2013,1,2)
@test Time.Datetime(2013,1,1,-1) == Time.Datetime(2012,12,31,23)
@test Time.Date(2013,1,0) == Time.Date(2012,12,31)
@test Time.Date(2013,1,32) == Time.Date(2013,2,1)

# Datetime arithmetic
dt = Time.Datetime(1999,12,27)
@test dt + Time.Year(1) == Time.Datetime(2000,12,27)
@test dt + Time.Year(100) == Time.Datetime(2099,12,27)
@test dt + Time.Year(1000) == Time.Datetime(2999,12,27)
@test dt - Time.Year(1) == Time.Datetime(1998,12,27)
@test dt - Time.Year(100) == Time.Datetime(1899,12,27)
@test dt - Time.Year(1000) == Time.Datetime(999,12,27)
dt = Time.Datetime(2000,2,29)
@test dt + Time.Year(1) == Time.Datetime(2001,2,28)
@test dt - Time.Year(1) == Time.Datetime(1999,2,28)
@test dt + Time.Year(4) == Time.Datetime(2004,2,29)
@test dt - Time.Year(4) == Time.Datetime(1996,2,29)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Year(1) == Time.Datetime(1973,6,30,23,59,59)
@test dt - Time.Year(1) == Time.Datetime(1971,6,30,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Year(1) == Time.Datetime(1973,6,30,23,59,59)
@test dt - Time.Year(1) == Time.Datetime(1971,6,30,23,59,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Month(1) == Time.Datetime(2000,1,27)
@test dt + Time.Month(100) == Time.Datetime(2007,4,27)
@test dt + Time.Month(1000) == Time.Datetime(2082,4,27)
@test dt - Time.Month(1) == Time.Datetime(1999,11,27)
@test dt - Time.Month(100) == Time.Datetime(1991,4,27)
@test dt - Time.Month(1000) == Time.Datetime(1916,4,27)
dt = Time.Datetime(2000,2,29)
@test dt + Time.Month(1) == Time.Datetime(2000,3,29)
@test dt - Time.Month(1) == Time.Datetime(2000,1,29)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Month(1) == Time.Datetime(1972,7,30,23,59,59)
@test dt - Time.Month(1) == Time.Datetime(1972,5,30,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Month(1) == Time.Datetime(1972,7,30,23,59,59)
@test dt - Time.Month(1) == Time.Datetime(1972,5,30,23,59,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Week(1) == Time.Datetime(2000,1,3)
@test dt + Time.Week(100) == Time.Datetime(2001,11,26)
@test dt + Time.Week(1000) == Time.Datetime(2019,2,25)
@test dt - Time.Week(1) == Time.Datetime(1999,12,20)
@test dt - Time.Week(100) == Time.Datetime(1998,1,26)
@test dt - Time.Week(1000) == Time.Datetime(1980,10,27)
dt = Time.Datetime(2000,2,29)
@test dt + Time.Week(1) == Time.Datetime(2000,3,7)
@test dt - Time.Week(1) == Time.Datetime(2000,2,22)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Week(1) == Time.Datetime(1972,7,7,23,59,59)
@test dt - Time.Week(1) == Time.Datetime(1972,6,23,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Week(1) == Time.Datetime(1972,7,7,23,59,59)
@test dt - Time.Week(1) == Time.Datetime(1972,6,23,23,59,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Day(1) == Time.Datetime(1999,12,28)
@test dt + Time.Day(100) == Time.Datetime(2000,4,5)
@test dt + Time.Day(1000) == Time.Datetime(2002,9,22)
@test dt - Time.Day(1) == Time.Datetime(1999,12,26)
@test dt - Time.Day(100) == Time.Datetime(1999,9,18)
@test dt - Time.Day(1000) == Time.Datetime(1997,4,1)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Day(1) == Time.Datetime(1972,7,1,23,59,59)
@test dt - Time.Day(1) == Time.Datetime(1972,6,29,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Day(1) == Time.Datetime(1972,7,1,23,59,59)
@test dt - Time.Day(1) == Time.Datetime(1972,6,29,23,59,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Hour(1) == Time.Datetime(1999,12,27,1)
@test dt + Time.Hour(100) == Time.Datetime(1999,12,31,4)
@test dt + Time.Hour(1000) == Time.Datetime(2000,2,6,16)
@test dt - Time.Hour(1) == Time.Datetime(1999,12,26,23)
@test dt - Time.Hour(100) == Time.Datetime(1999,12,22,20)
@test dt - Time.Hour(1000) == Time.Datetime(1999,11,15,8)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Hour(1) == Time.Datetime(1972,7,1,0,59,59)
@test dt - Time.Hour(1) == Time.Datetime(1972,6,30,22,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Hour(1) == Time.Datetime(1972,7,1,0,59,59)
@test dt - Time.Hour(1) == Time.Datetime(1972,6,30,22,59,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Minute(1) == Time.Datetime(1999,12,27,0,1)
@test dt + Time.Minute(100) == Time.Datetime(1999,12,27,1,40)
@test dt + Time.Minute(1000) == Time.Datetime(1999,12,27,16,40)
@test dt - Time.Minute(1) == Time.Datetime(1999,12,26,23,59)
@test dt - Time.Minute(100) == Time.Datetime(1999,12,26,22,20)
@test dt - Time.Minute(1000) == Time.Datetime(1999,12,26,7,20)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Minute(1) == Time.Datetime(1972,7,1,0,0,59)
@test dt - Time.Minute(1) == Time.Datetime(1972,6,30,23,58,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Minute(1) == Time.Datetime(1972,7,1,0,0,59)
@test dt - Time.Minute(1) == Time.Datetime(1972,6,30,23,58,59)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Second(1) == Time.Datetime(1999,12,27,0,0,1)
@test dt + Time.Second(100) == Time.Datetime(1999,12,27,0,1,40)
@test dt + Time.Second(1000) == Time.Datetime(1999,12,27,0,16,40)
@test dt - Time.Second(1) == Time.Datetime(1999,12,26,23,59,59)
@test dt - Time.Second(100) == Time.Datetime(1999,12,26,23,58,20)
@test dt - Time.Second(1000) == Time.Datetime(1999,12,26,23,43,20)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Second(1) == Time.Datetime(1972,7,1)
@test dt - Time.Second(1) == Time.Datetime(1972,6,30,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Second(1) == Time.Datetime(1972,6,30,23,59,60)
@test dt - Time.Second(1) == Time.Datetime(1972,6,30,23,59,58)

dt = Time.Datetime(1999,12,27)
@test dt + Time.Millisecond(1) == Time.Datetime(1999,12,27,0,0,0,1)
@test dt + Time.Millisecond(100) == Time.Datetime(1999,12,27,0,0,0,100)
@test dt + Time.Millisecond(1000) == Time.Datetime(1999,12,27,0,0,1)
@test dt - Time.Millisecond(1) == Time.Datetime(1999,12,26,23,59,59,999)
@test dt - Time.Millisecond(100) == Time.Datetime(1999,12,26,23,59,59,900)
@test dt - Time.Millisecond(1000) == Time.Datetime(1999,12,26,23,59,59)
dt = Time.Datetime(1972,6,30,23,59,60)
@test dt + Time.Millisecond(1) == Time.Datetime(1972,6,30,23,59,60,1)
@test dt - Time.Millisecond(1) == Time.Datetime(1972,6,30,23,59,59,999)
dt = Time.Datetime(1972,6,30,23,59,59)
@test dt + Time.Millisecond(1) == Time.Datetime(1972,6,30,23,59,59,1)
@test dt - Time.Millisecond(1) == Time.Datetime(1972,6,30,23,59,58,999)

dt = Time.Date(1999,12,27)
@test dt + Time.Year(1) == Time.Date(2000,12,27)
@test dt + Time.Year(100) == Time.Date(2099,12,27)
@test dt + Time.Year(1000) == Time.Date(2999,12,27)
@test dt - Time.Year(1) == Time.Date(1998,12,27)
@test dt - Time.Year(100) == Time.Date(1899,12,27)
@test dt - Time.Year(1000) == Time.Date(999,12,27)
dt = Time.Date(2000,2,29)
@test dt + Time.Year(1) == Time.Date(2001,2,28)
@test dt - Time.Year(1) == Time.Date(1999,2,28)
@test dt + Time.Year(4) == Time.Date(2004,2,29)
@test dt - Time.Year(4) == Time.Date(1996,2,29)

dt = Time.Date(1999,12,27)
@test dt + Time.Month(1) == Time.Date(2000,1,27)
@test dt + Time.Month(100) == Time.Date(2007,4,27)
@test dt + Time.Month(1000) == Time.Date(2082,4,27)
@test dt - Time.Month(1) == Time.Date(1999,11,27)
@test dt - Time.Month(100) == Time.Date(1991,4,27)
@test dt - Time.Month(1000) == Time.Date(1916,4,27)
dt = Time.Date(2000,2,29)
@test dt + Time.Month(1) == Time.Date(2000,3,29)
@test dt - Time.Month(1) == Time.Date(2000,1,29)

dt = Time.Date(1999,12,27)
@test dt + Time.Week(1) == Time.Date(2000,1,3)
@test dt + Time.Week(100) == Time.Date(2001,11,26)
@test dt + Time.Week(1000) == Time.Date(2019,2,25)
@test dt - Time.Week(1) == Time.Date(1999,12,20)
@test dt - Time.Week(100) == Time.Date(1998,1,26)
@test dt - Time.Week(1000) == Time.Date(1980,10,27)
dt = Time.Date(2000,2,29)
@test dt + Time.Week(1) == Time.Date(2000,3,7)
@test dt - Time.Week(1) == Time.Date(2000,2,22)

dt = Time.Date(1999,12,27)
@test dt + Time.Day(1) == Time.Date(1999,12,28)
@test dt + Time.Day(100) == Time.Date(2000,4,5)
@test dt + Time.Day(1000) == Time.Date(2002,9,22)
@test dt - Time.Day(1) == Time.Date(1999,12,26)
@test dt - Time.Day(100) == Time.Date(1999,9,18)
@test dt - Time.Day(1000) == Time.Date(1997,4,1)

# week function
dt = Time.Datetime(1999,12,27)
dt1 = Time.Date(1999,12,27)
check = (52,52,52,52,52,52,52,1,1,1,1,1,1,1,2,2,2,2,2,2,2)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.Datetime(2000,12,25)
dt1 = Time.Date(2000,12,25)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.Datetime(2030,12,23)
dt1 = Time.Date(2030,12,23)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.Datetime(2004,12,20)
dt1 = Time.Date(2004,12,20)
check = (52,52,52,52,52,52,52,53,53,53,53,53,53,53,1,1,1,1,1,1,1)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end

# Leapseconds
a = Time.Datetime(1972,6,29,23,59,59)
b = Time.Datetime(1972,6,29,23,59,60) #not a leapsecond, rolls forward
@test b.instant.ms - a.instant.ms == 1000
@test string(b) == "1972-06-30T00:00:00 UTC"

a = Time.Datetime(1972,6,30,23,59,59)
b = Time.Datetime(1972,6,30,23,59,60)
c = Time.Datetime(1972,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1972-06-30T23:59:60 UTC"
@test string(c) == "1972-07-01T00:00:00 UTC"
a = Time.Datetime(1972,12,31,23,59,59)
b = Time.Datetime(1972,12,31,23,59,60)
c = Time.Datetime(1973,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1972-12-31T23:59:60 UTC"
@test string(c) == "1973-01-01T00:00:00 UTC"
a = Time.Datetime(1973,12,31,23,59,59)
b = Time.Datetime(1973,12,31,23,59,60)
c = Time.Datetime(1974,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1973-12-31T23:59:60 UTC"
@test string(c) == "1974-01-01T00:00:00 UTC"
a = Time.Datetime(1974,12,31,23,59,59)
b = Time.Datetime(1974,12,31,23,59,60)
c = Time.Datetime(1975,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1974-12-31T23:59:60 UTC"
@test string(c) == "1975-01-01T00:00:00 UTC"
a = Time.Datetime(1975,12,31,23,59,59)
b = Time.Datetime(1975,12,31,23,59,60)
c = Time.Datetime(1976,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1975-12-31T23:59:60 UTC"
@test string(c) == "1976-01-01T00:00:00 UTC"
a = Time.Datetime(1976,12,31,23,59,59)
b = Time.Datetime(1976,12,31,23,59,60)
c = Time.Datetime(1977,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1976-12-31T23:59:60 UTC"
@test string(c) == "1977-01-01T00:00:00 UTC"
a = Time.Datetime(1977,12,31,23,59,59)
b = Time.Datetime(1977,12,31,23,59,60)
c = Time.Datetime(1978,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1977-12-31T23:59:60 UTC"
@test string(c) == "1978-01-01T00:00:00 UTC"
a = Time.Datetime(1978,12,31,23,59,59)
b = Time.Datetime(1978,12,31,23,59,60)
c = Time.Datetime(1979,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1978-12-31T23:59:60 UTC"
@test string(c) == "1979-01-01T00:00:00 UTC"
a = Time.Datetime(1979,12,31,23,59,59)
b = Time.Datetime(1979,12,31,23,59,60)
c = Time.Datetime(1980,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1979-12-31T23:59:60 UTC"
@test string(c) == "1980-01-01T00:00:00 UTC"
a = Time.Datetime(1981,6,30,23,59,59)
b = Time.Datetime(1981,6,30,23,59,60)
c = Time.Datetime(1981,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1981-06-30T23:59:60 UTC"
@test string(c) == "1981-07-01T00:00:00 UTC"
a = Time.Datetime(1982,6,30,23,59,59)
b = Time.Datetime(1982,6,30,23,59,60)
c = Time.Datetime(1982,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1982-06-30T23:59:60 UTC"
@test string(c) == "1982-07-01T00:00:00 UTC"
a = Time.Datetime(1983,6,30,23,59,59)
b = Time.Datetime(1983,6,30,23,59,60)
c = Time.Datetime(1983,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1983-06-30T23:59:60 UTC"
@test string(c) == "1983-07-01T00:00:00 UTC"
a = Time.Datetime(1985,6,30,23,59,59)
b = Time.Datetime(1985,6,30,23,59,60)
c = Time.Datetime(1985,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1985-06-30T23:59:60 UTC"
@test string(c) == "1985-07-01T00:00:00 UTC"
a = Time.Datetime(1987,12,31,23,59,59)
b = Time.Datetime(1987,12,31,23,59,60)
c = Time.Datetime(1988,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1987-12-31T23:59:60 UTC"
@test string(c) == "1988-01-01T00:00:00 UTC"
a = Time.Datetime(1989,12,31,23,59,59)
b = Time.Datetime(1989,12,31,23,59,60)
c = Time.Datetime(1990,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1989-12-31T23:59:60 UTC"
@test string(c) == "1990-01-01T00:00:00 UTC"
a = Time.Datetime(1990,12,31,23,59,59)
b = Time.Datetime(1990,12,31,23,59,60)
c = Time.Datetime(1991,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1990-12-31T23:59:60 UTC"
@test string(c) == "1991-01-01T00:00:00 UTC"
a = Time.Datetime(1992,6,30,23,59,59)
b = Time.Datetime(1992,6,30,23,59,60)
c = Time.Datetime(1992,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1992-06-30T23:59:60 UTC"
@test string(c) == "1992-07-01T00:00:00 UTC"
a = Time.Datetime(1993,6,30,23,59,59)
b = Time.Datetime(1993,6,30,23,59,60)
c = Time.Datetime(1993,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1993-06-30T23:59:60 UTC"
@test string(c) == "1993-07-01T00:00:00 UTC"
a = Time.Datetime(1994,6,30,23,59,59)
b = Time.Datetime(1994,6,30,23,59,60)
c = Time.Datetime(1994,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1994-06-30T23:59:60 UTC"
@test string(c) == "1994-07-01T00:00:00 UTC"
a = Time.Datetime(1995,12,31,23,59,59)
b = Time.Datetime(1995,12,31,23,59,60)
c = Time.Datetime(1996,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1995-12-31T23:59:60 UTC"
@test string(c) == "1996-01-01T00:00:00 UTC"
a = Time.Datetime(1997,6,30,23,59,59)
b = Time.Datetime(1997,6,30,23,59,60)
c = Time.Datetime(1997,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1997-06-30T23:59:60 UTC"
@test string(c) == "1997-07-01T00:00:00 UTC"
a = Time.Datetime(1998,12,31,23,59,59)
b = Time.Datetime(1998,12,31,23,59,60)
c = Time.Datetime(1999,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1998-12-31T23:59:60 UTC"
@test string(c) == "1999-01-01T00:00:00 UTC"
a = Time.Datetime(2005,12,31,23,59,59)
b = Time.Datetime(2005,12,31,23,59,60)
c = Time.Datetime(2006,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2005-12-31T23:59:60 UTC"
@test string(c) == "2006-01-01T00:00:00 UTC"
a = Time.Datetime(2008,12,31,23,59,59)
b = Time.Datetime(2008,12,31,23,59,60)
c = Time.Datetime(2009,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2008-12-31T23:59:60 UTC"
@test string(c) == "2009-01-01T00:00:00 UTC"
a = Time.Datetime(2012,6,30,23,59,59)
b = Time.Datetime(2012,6,30,23,59,60)
c = Time.Datetime(2012,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2012-06-30T23:59:60 UTC"
@test string(c) == "2012-07-01T00:00:00 UTC"

a = Time.Datetime(1972,6,30,23,59,59)
b = Time.Datetime(1972,6,30,23,59,60)
c = Time.Datetime(1972,7,1,0,0,0)
@test Time.calendar(a) == Time.ISOCalendar
@test Time.timezone(a) == Time.UTC
@test Time.precision(a) == Time.Millisecond
@test string(typemax(Time.Datetime)) == "292277024-12-31T23:59:59 UTC"
@test string(typemin(Time.Datetime)) == "-292277023-01-01T00:00:00 UTC"

# Date functions
@test Time._lastdayofmonth(2000,1) == 31
@test Time._lastdayofmonth(2000,2) == 29
@test Time._lastdayofmonth(2000,3) == 31
@test Time._lastdayofmonth(2000,4) == 30
@test Time._lastdayofmonth(2000,5) == 31
@test Time._lastdayofmonth(2000,6) == 30
@test Time._lastdayofmonth(2000,7) == 31
@test Time._lastdayofmonth(2000,8) == 31
@test Time._lastdayofmonth(2000,9) == 30
@test Time._lastdayofmonth(2000,10) == 31
@test Time._lastdayofmonth(2000,11) == 30
@test Time._lastdayofmonth(2000,12) == 31
@test Time._lastdayofmonth(2001,2) == 28

@test Time.isleap(Time.Datetime(1900)) == false
@test Time.isleap(Time.Datetime(2000)) == true
@test Time.isleap(Time.Datetime(2004)) == true
@test Time.isleap(Time.Datetime(2008)) == true
@test Time.isleap(Time.Datetime(0)) == true
@test Time.isleap(Time.Datetime(1)) == false
@test Time.isleap(Time.Datetime(-1)) == false
@test Time.isleap(Time.Datetime(4)) == true
@test Time.isleap(Time.Datetime(-4)) == true

@test Time.dayofweek(Time.Datetime(2013,12,22)) == 0
@test Time.dayofweek(Time.Datetime(2013,12,23)) == 1
@test Time.dayofweek(Time.Datetime(2013,12,24)) == 2
@test Time.dayofweek(Time.Datetime(2013,12,25)) == 3
@test Time.dayofweek(Time.Datetime(2013,12,26)) == 4
@test Time.dayofweek(Time.Datetime(2013,12,27)) == 5
@test Time.dayofweek(Time.Datetime(2013,12,28)) == 6
@test Time.dayofweek(Time.Datetime(2013,12,29)) == 0

@test Time.dayofweekofmonth(Time.Datetime(2013,12,1)) == 1
@test Time.dayofweekofmonth(Time.Datetime(2013,12,8)) == 2
@test Time.dayofweekofmonth(Time.Datetime(2013,12,15)) == 3
@test Time.dayofweekofmonth(Time.Datetime(2013,12,22)) == 4
@test Time.dayofweekofmonth(Time.Datetime(2013,12,29)) == 5

@test Time.daysofweekinmonth(Time.Datetime(2013,12,1)) == 5

@test Time.dayofyear(Time.Datetime(2000,1,1)) == 1
@test Time.dayofyear(Time.Datetime(2004,1,1)) == 1
@test Time.dayofyear(Time.Datetime(20013,1,1)) == 1
@test Time.dayofyear(Time.Datetime(2000,12,31)) == 366
@test Time.dayofyear(Time.Datetime(2001,12,31)) == 365

@test Time.firstdayofweek(Time.Datetime(2013,12,24)) == Time.Datetime(2013,12,23)
@test Time.lastdayofweek(Time.Datetime(2013,12,24)) == Time.Datetime(2013,12,29)

@test Time.addwrap(1,11) == 12 #month 1 plus 11 months == month 12
@test Time.addwrap(1,12) == 1  #month 1 plus 12 months == month 1
@test Time.addwrap(1,24) == 1  #month 1 plus 24 months == month 1
@test Time.subwrap(12,1) == 11 #month 12 minus 1 month == month 11
@test Time.addwrap(2000,12,1) == 2001 #year 2000 month 12 plus 1 month == year 2001

#DatetimeRange
dt1 = Time.Datetime(2000,1,1)
dt2 = Time.Datetime(2010,1,1)
y = Time.Year(1)
r = dt1:y:dt2
@test first(r) == Time.Datetime(2000,1,1)
@test last(r) == Time.Datetime(2010,1,1)
@test typeof(r.step) == Time.Year
@test length(r) == 11
dt2 = Time.Datetime(2001,1,1)
r = dt1:Time.Month(1):dt2
@test length(r) == 13
@test last(r) == Time.Datetime(2001,1,1)
@test typeof(r.step) == Time.Month
r = dt1:Time.Week(2):dt2
@test length(r) == 27
@test last(r) == Time.Datetime(2000,12,30)
@test typeof(r.step) == Time.Week
dt2 = Time.Datetime(2000,3,1)
r = dt2:Time.Day(-1):dt1
@test length(r) == 61
@test last(r) == Time.Datetime(2000,1,1)
@test typeof(r.step) == Time.Day
@test last(r) + Time.Year(1) == Time.Datetime(2001,1,1)
dt2 = Time.Datetime(2000,1,31)
r = dt1:Time.Hour(24):dt2
@test length(r) == 31
@test last(r) == Time.Datetime(2000,1,31)
@test typeof(r.step) == Time.Hour
r = dt1:Time.Minute(1440):dt2
@test length(r) == 31
@test last(r) == Time.Datetime(2000,1,31)
@test typeof(r.step) == Time.Minute
r = dt1:Time.Second(86400):dt2
@test length(r) == 31
@test last(r) == Time.Datetime(2000,1,31)
@test typeof(r.step) == Time.Second
dt2 = Time.Datetime(2000,1,1,1)
r = dt1:Time.Millisecond(60000):dt2
@test length(r) == 61
@test last(r) == Time.Datetime(2000,1,1,1)
@test typeof(r.step) == Time.Millisecond
dt2 = Time.Datetime(2000,1,31)
f = x->Time.dayofweek(x)==1 #Mondays
r = Time.recur(f,dt1:Time.Day(1):dt2)
@test length(r) == 4
@test last(r) == Time.Datetime(2000,1,24)
@test [r] == [Time.Datetime(2000,1,3)+((i-1)*Time.Week(1)) for i = 1:4]

dt1 = Time.Date(2000,1,1)
dt2 = Time.Date(2010,1,1)
y = Time.Year(1)
r = dt1:y:dt2
@test first(r) == Time.Date(2000,1,1)
@test last(r) == Time.Date(2010,1,1)
@test typeof(r.step) == Time.Year
@test length(r) == 11
dt2 = Time.Date(2001,1,1)
r = dt1:Time.Month(1):dt2
@test length(r) == 13
@test last(r) == Time.Date(2001,1,1)
@test typeof(r.step) == Time.Month
r = dt1:Time.Week(2):dt2
@test length(r) == 27
@test last(r) == Time.Date(2000,12,30)
@test typeof(r.step) == Time.Week
dt2 = Time.Date(2000,3,1)
r = dt2:Time.Day(-1):dt1
@test length(r) == 61
@test last(r) == Time.Date(2000,1,1)
@test typeof(r.step) == Time.Day
@test last(r) + Time.Year(1) == Time.Date(2001,1,1)
dt2 = Time.Date(2000,1,31)
f = x->Time.dayofweek(x)==1 #Mondays
r = Time.recur(f,dt1:Time.Day(1):dt2)
@test length(r) == 4
@test last(r) == Time.Date(2000,1,24)
@test [r] == [Time.Date(2000,1,3)+((i-1)*Time.Week(1)) for i = 1:4]

# Datetime parsing
#'1996-January-15'
dt = Time.Datetime(1996,1,15)
a = "96-01-15"
f = "yy-mm-dd"
@test Time.Datetime(a,f) + Time.Year(1900) == dt
b = "96/Feb/15"
f = "yy/mmm/dd"
@test Time.Datetime(b,f) + Time.Year(1900) == dt + Time.Month(1)
c = "96:15:01"
f = "yy:dd:mm"
@test Time.Datetime(c,f) + Time.Year(1900) == dt
d = "1996,Jan,15"
f = "yyyy,mmm,dd"
@test Time.Datetime(d,f) == dt
e = "1996.January.15"
f = "yyyy.mmmm.dd"
@test Time.Datetime(e,f) == dt
f = "1996 1 15"
fo = "yyyy m dd"
@test Time.Datetime(f,fo) == dt
g = "1996-1-1"
f = "yyyy-m-d"
@test Time.Datetime(g,f) == dt - Time.Day(14)
# h = "1996-01-15 10 pm"
# f = "yyyy-mm-dd HH aa"
# Time.Datetime(h,f)
# i = "1996-01-15 10 am"
# f = "yyyy-mm-dd HH aa"
j = "1996-01-15 UTC"
f = "yyyy-mm-dd zzz"
@test Time.Datetime(j,f) == dt
k = "1996-01-15 10:00:00 UTC"
f = "yyyy-mm-dd HH:MM:SS zzz"
@test Time.Datetime(k,f) == dt + Time.Hour(10)
l = "1996-01-15 10:10:10.25 UTC"
f = "yyyy-mm-dd HH:MM:SS.ss zzz"
@test Time.Datetime(l,f) == dt + Time.Hour(10) + Time.Minute(10) + Time.Second(10) + Time.Millisecond(250)

# m = "1996-01-15 10:00:00 -0800"
# f = "yyyy-mm-dd HH:MM:SS.ss zzz"
# n = "1996-01-15 10:00:00 +0800"
# o = "1996-01-15 10:00:00-08:00"
# p = "1996-01-15 10:00:00+08:45"
# q = "1996-01-15 10:00:00 GMT+08:45"

r = "1/15/1996" # Excel
f = "m/dd/yyyy"
@test Time.Datetime(r,f) == dt
s = "19960115"
f = "yyyymmdd"
@test Time.Datetime(s,f) == dt
# t = "1996-01-15 10 PST"
# f = "yyyy-mm-dd HH zzz"
# Time.Datetime(t,f) == dt + Time.Hour(10)
# u = "1996-01-15 10:00 PST"
# f = "yyyy-mm-dd HH:MM zzz"
# Time.Datetime(u,f) == dt + Time.Hour(10)
v = "1996-01-15 10:00:00"
f = "yyyy-mm-dd HH:MM:SS"
@test Time.Datetime(v,f) == dt + Time.Hour(10)
w = "1996-01-15T10:00:00 UTC"
f = "yyyy-mm-ddTHH:MM:SS zzz"
@test Time.Datetime(w,f;sep="T") == dt + Time.Hour(10)
# x = "1996-01-15 10:00:00 America/Chicago"
# f = "yyyy-mm-dd HH:MM:SS zzzz"
# Time.Datetime(x,f) == dt + Time.Hour(10)
y = "1996/1"
f = "yyyy/m"
@test Time.Datetime(y,f) == dt - Time.Day(14)
z = "1996"
f = "yyyy"
@test Time.Datetime(z,f) == dt - Time.Day(14)
aa = "1/5/1996"
f = "m/d/yyyy"
@test Time.Datetime(aa,f) == dt - Time.Day(10)
bb = "5/1/1996"
f = "d/m/yyyy"
@test Time.Datetime(bb,f) == dt - Time.Day(10)
cc = "01151996"
f = "mmddyyyy"
@test Time.Datetime(cc,f) == dt
dd = "15011996"
f = "ddmmyyyy"
@test Time.Datetime(dd,f) == dt
ee = "01199615"
f = "mmyyyydd"
@test Time.Datetime(ee,f) == dt
ff = "1996-15-Jan"
f = "yyyy-dd-mmm"
@test Time.Datetime(ff,f) == dt
gg = "Jan-1996-15"
f = "mmm-yyyy-dd"
@test Time.Datetime(gg,f) == dt

#Parsing TODO
 #optional millisecond?
 #hh for 1-12, requires 'aa' for am/pm
 #zzzz for olson timezone string

# Period testing
@test -Time.Year(1) == Time.Year(-1)
@test Time.Year(1) > Time.Year(0)
@test (Time.Year(1) < Time.Year(0)) == false
@test Time.Year(1) == Time.Year(1)
@test Time.Year(1) + Time.Year(1) == Time.Year(2)
@test Time.Year(1) - Time.Year(1) == Time.Year(0)
@test Time.Year(1) * Time.Year(1) == Time.Year(1)
@test Time.Year(10) % Time.Year(4) == Time.Year(2)
@test div(Time.Year(10),Time.Year(3)) == Time.Year(3)
@test div(Time.Year(10),Time.Year(4)) == Time.Year(2)
t = Time.Year(1)
t2 = Time.Year(2)
@test ([t,t,t,t,t] + Time.Year(1)) == ([t2,t2,t2,t2,t2])
@test ([t2,t2,t2,t2,t2] - Time.Year(1)) == ([t,t,t,t,t])
@test ([t,t,t,t,t] * Time.Year(1)) == ([t,t,t,t,t])
@test ([t,t,t,t,t] % t2) == ([t,t,t,t,t])
@test div([t,t,t,t,t],Time.Year(1)) == ([t,t,t,t,t])

#Period arithmetic
y = Time.Year(1)
m = Time.Month(1)
w = Time.Week(1)
d = Time.Day(1)
h = Time.Hour(1)
mi = Time.Minute(1)
s = Time.Second(1)
ms = Time.Millisecond(1)
@test typeof(y+m) <: Time.CompoundPeriod
@test typeof(m+y) <: Time.CompoundPeriod
@test typeof(y+w) <: Time.CompoundPeriod
@test typeof(y+d) <: Time.CompoundPeriod
@test typeof(y+h) <: Time.CompoundPeriod
@test typeof(y+mi) <: Time.CompoundPeriod
@test typeof(y+s) <: Time.CompoundPeriod
@test typeof(y+ms) <: Time.CompoundPeriod
@test_throws y > m
@test_throws d < w
@test typemax(Time.Year) == Time.Year(typemax(Int64))
@test typemax(Time.Year) + y == Time.Year(-9223372036854775808)
@test typemin(Time.Year) == Time.Year(-9223372036854775808)
#Period-Real arithmetic
@test y + 1 == Time.Year(2)
@test 1 + y == Time.Year(2)
@test y + 1.0 == Time.Year(2)
@test y * 4 == Time.Year(4)
@test y * 4f0 == Time.Year(4)
@test y * 3//4 == Time.Year(1)
@test div(y,2) == Time.Year(0)
@test div(2,y) == Time.Year(2)
@test div(y,y) == Time.Year(1)
@test y*10 % 5 == Time.Year(0)
@test 5 % y*10 == Time.Year(0)
@test_throws y > 3
@test_throws 4 < y
@test_throws 1 == y
t = [y,y,y,y,y]
@test t .+ Time.Year(2) == [Time.Year(3),Time.Year(3),Time.Year(3),Time.Year(3),Time.Year(3)]
dt = Time.Datetime(2012,12,21)
test = ((((((((dt + y) - m) + w) - d) + h) - mi) + s) - ms)
@test test == dt + y - m + w - d + h - mi + s - ms
@test test == y - m + w - d + dt + h - mi + s - ms
@test test == dt - m + y - d + w - mi + h - ms + s
@test test == dt + (y - m + w - d + h - mi + s - ms)
@test test == dt + y - m + w - d + (h - mi + s - ms)
#associative
@test (dt + Time.Year(4)) + Time.Day(1) == dt + (Time.Year(4) + Time.Day(1))

#PeriodRange
t = Time.Year(100)
y = Time.Year(1)
r = y:t
@test size(r) == (100,)
@test length(r) == 100
@test step(r) == Time.Year(1)
@test first(r) == y
@test last(r) == t
@test first(r + y) == Time.Year(2)
@test last(r + y) == Time.Year(101)
@test last(y:Time.Year(2):t) == Time.Year(99)
@test [Time.Year(0):Time.Year(25):t][4] == Time.Year(75)
@test_throws y:m:t
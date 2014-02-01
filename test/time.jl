using Base.Test
# Date internal algorithms
@test Time.totaldays(0,2,28) == -307
@test Time.totaldays(0,2,29) == -306
@test Time.totaldays(0,3,1) == -305
@test Time.totaldays(0,12,31) == 0
@test Time.totaldays(1,1,1) == 1
@test Time.totaldays(1,1,2) == 2
@test Time.totaldays(2013,1,1) == 734869

@test Time._day2date(-306) == (0,2,29)
@test Time._day2date(-305) == (0,3,1)
@test Time._day2date(-2) == (0,12,29)
@test Time._day2date(-1) == (0,12,30)
@test Time._day2date(0) == (0,12,31)
@test Time._day2date(1) == (1,1,1)
@test Time._year(-1) == 0
@test Time._month(-1) == 12
@test Time._day(-1) == 30
@test Time._year(0) == 0
@test Time._month(0) == 12
@test Time._day(0) == 31
@test Time._year(1) == 1
@test Time._month(1) == 1
@test Time._day(1) == 1
@test Time._day2date(730120) == (2000,1,1)
@test Time._year(730120) == 2000
@test Time._month(730120) == 1
@test Time._day(730120) == 1

function test_dates(from,to)
	y = m = d = 0
	test_day = Time.totaldays(from,1,1)
	for y in from:to
		for m = 1:12
			for d = 1:Time._lastdayofmonth(y,m)
				days = Time.totaldays(y,m,d)
				@test days == test_day
				@test (y,m,d) == Time._day2date(days)
				test_day += 1
			end
		end
	end
end
test_dates(-2000,2000)

test = Time.DateTime{Time.Millisecond,0,Time.ISOCalendar}(Time.Millisecond(63492681625000))
@test Time.DateTime(2013) == test
@test Time.DateTime(2013,1) == test
@test Time.DateTime(2013,1,1) == test
@test Time.DateTime(2013,1,1,0) == test
@test Time.DateTime(2013,1,1,0,0) == test
@test Time.DateTime(2013,1,1,0,0,0) == test
@test Time.DateTime(2013,1,1,0,0,0,0) == test
test = Time.Date(Time.Day(734869))
@test Time.Date(2013) == test
@test Time.Date(2013,1) == test
@test Time.Date(2013,1,1) == test
@test string(Time.Date(1,1,1)) == "0001-01-01" # January 1st, 1 AD/CE
@test string(Time.Date(0,12,31)) == "0000-12-31" # December 31, 1 BC/BCE
@test Time.Date(1,1,1) - Time.Date(0,12,31) == Time.Day(1)
@test Date(Time.Day(-306)) == Date(0,2,29)
@test string(Time.Date(0,1,1)) == "0000-01-01" # January 1st, 1 BC/BCE
@test string(Time.Date(-1,1,1)) == "-0001-01-01" # January 1st, 2 BC/BCE
@test string(Time.Date(-1000000,1,1)) == "-1000000-01-01"
@test string(Time.Date(1000000,1,1)) == "1000000-01-01"

# Test various input types for Date/DateTime
test = Date(1,1,1)
@test Date(int8(1),int8(1),int8(1)) == test
@test Date(uint8(1),uint8(1),uint8(1)) == test
@test Date(int16(1),int16(1),int16(1)) == test
@test Date(uint8(1),uint8(1),uint8(1)) == test
@test Date(int32(1),int32(1),int32(1)) == test
@test Date(uint32(1),uint32(1),uint32(1)) == test
@test Date(int64(1),int64(1),int64(1)) == test
@test Date(uint64(1),uint64(1),uint64(1)) == test
@test Date(int128(1),int128(1),int128(1)) == test
@test Date(uint128(1),uint128(1),uint128(1)) == test
@test Date(big(1),big(1),big(1)) == test
@test Date(big(1),big(1),big(1)) == test
@test Date('\x01','\x01','\x01') == test
@test Date(true,true,true) == test
@test Date(false,true,false) == test - Time.Year(1) - Time.Day(1)
@test Date(false,true,true) == test - Time.Year(1)
@test Date(true,true,false) == test - Time.Day(1)
@test_throws Date(BigFloat(1),BigFloat(1),BigFloat(1))
@test_throws Date(complex(1),complex(1),complex(1))
@test_throws Date(float64(1),float64(1),float64(1))
@test_throws Date(float32(1),float32(1),float32(1))
@test_throws Date(float16(1),float16(1),float16(1))
@test_throws Date(Rational(1),Rational(1),Rational(1))

# Test year, month, day, hour, minute
function test_dates()
	y = m = d = h = mi = 0
	for y in [-2013,-1,0,1,2013]
		for m = 1:12
			for d = 1:Time._lastdayofmonth(y,m)
				for h = 0:23
					for mi = 0:59
						dt = Time.DateTime(y,m,d,h,mi)
						@test y == Time.year(dt)
						@test m == Time.month(dt)
						@test d == Time.day(dt)
						@test h == Time.hour(dt)
						@test mi == Time.minute(dt)
						#@test s == Time.second(dt)
						#@test ms == Time.millisecond(dt)
					end
				end
			end
		end
	end
end
test_dates()

# Test second, millisecond
function test_dates()
	y = m = d = h = mi = s = ms = 0
	for y in [-2013,-1,0,1,2013]
		for m in [1,6,12]
			for d in [1,15,Time._lastdayofmonth(y,m)]
				for h in [0,12,23]
					for s = 0:59
						for ms in [0,1,500,999]
							dt = Time.DateTime(y,m,d,h,mi,s,ms)
							@test y == Time.year(dt)
							@test m == Time.month(dt)
							@test d == Time.day(dt)
							@test h == Time.hour(dt)
							@test s == Time.second(dt)
							@test ms == Time.millisecond(dt)
						end
					end
				end
			end
		end
	end
end
test_dates()

function test_dates(from,to)
	y = m = d = 0
	for y in from:to
		for m = 1:12
			for d = 1:Time._lastdayofmonth(y,m)
				dt = Time.Date(y,m,d)
				@test y == Time.year(dt)
				@test m == Time.month(dt)
				@test d == Time.day(dt)
			end
		end
	end
end
test_dates(-2000,2000)

# Months must be in range
@test_throws Time.DateTime(2013,0,1)
@test_throws Time.DateTime(2013,13,1)

# Days/Hours/Minutes/Seconds/Milliseconds roll back/forward
@test Time.DateTime(2013,1,0) == Time.DateTime(2012,12,31)
@test Time.DateTime(2013,1,32) == Time.DateTime(2013,2,1)
@test Time.DateTime(2013,1,1,24) == Time.DateTime(2013,1,2)
@test Time.DateTime(2013,1,1,-1) == Time.DateTime(2012,12,31,23)
@test Time.Date(2013,1,0) == Time.Date(2012,12,31)
@test Time.Date(2013,1,32) == Time.Date(2013,2,1)

# DateTime arithmetic
dt = Time.DateTime(1999,12,27)
@test dt + Time.Year(1) == Time.DateTime(2000,12,27)
@test dt + Time.Year(100) == Time.DateTime(2099,12,27)
@test dt + Time.Year(1000) == Time.DateTime(2999,12,27)
@test dt - Time.Year(1) == Time.DateTime(1998,12,27)
@test dt - Time.Year(100) == Time.DateTime(1899,12,27)
@test dt - Time.Year(1000) == Time.DateTime(999,12,27)
dt = Time.DateTime(2000,2,29)
@test dt + Time.Year(1) == Time.DateTime(2001,2,28)
@test dt - Time.Year(1) == Time.DateTime(1999,2,28)
@test dt + Time.Year(4) == Time.DateTime(2004,2,29)
@test dt - Time.Year(4) == Time.DateTime(1996,2,29)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Year(1) == Time.DateTime(1973,6,30,23,59,59)
@test dt - Time.Year(1) == Time.DateTime(1971,6,30,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Year(1) == Time.DateTime(1973,6,30,23,59,59)
@test dt - Time.Year(1) == Time.DateTime(1971,6,30,23,59,59)
@test dt + Time.Year(-1) == Time.DateTime(1971,6,30,23,59,59)

# Wrapping arithemtic for Months
@test Time.monthwrap(1,-14) == 11
@test Time.monthwrap(1,-13) == 12
@test Time.monthwrap(1,-12) == 1
@test Time.monthwrap(1,-11) == 2
@test Time.monthwrap(1,-10) == 3
@test Time.monthwrap(1,-9) == 4
@test Time.monthwrap(1,-8) == 5
@test Time.monthwrap(1,-7) == 6
@test Time.monthwrap(1,-6) == 7
@test Time.monthwrap(1,-5) == 8
@test Time.monthwrap(1,-4) == 9
@test Time.monthwrap(1,-3) == 10
@test Time.monthwrap(1,-2) == 11
@test Time.monthwrap(1,-1) == 12
@test Time.monthwrap(1,0) == 1
@test Time.monthwrap(1,1) == 2
@test Time.monthwrap(1,2) == 3
@test Time.monthwrap(1,3) == 4
@test Time.monthwrap(1,4) == 5
@test Time.monthwrap(1,5) == 6
@test Time.monthwrap(1,6) == 7
@test Time.monthwrap(1,7) == 8
@test Time.monthwrap(1,8) == 9
@test Time.monthwrap(1,9) == 10
@test Time.monthwrap(1,10) == 11
@test Time.monthwrap(1,11) == 12
@test Time.monthwrap(1,12) == 1
@test Time.monthwrap(1,13) == 2
@test Time.monthwrap(1,24) == 1
@test Time.monthwrap(12,-14) == 10
@test Time.monthwrap(12,-13) == 11
@test Time.monthwrap(12,-12) == 12
@test Time.monthwrap(12,-11) == 1
@test Time.monthwrap(12,-2) == 10
@test Time.monthwrap(12,-1) == 11
@test Time.monthwrap(12,0) == 12
@test Time.monthwrap(12,1) == 1
@test Time.monthwrap(12,2) == 2
@test Time.monthwrap(12,11) == 11
@test Time.monthwrap(12,12) == 12
@test Time.monthwrap(12,13) == 1

@test Time.yearwrap(2000,1,-3600) == 1700
@test Time.yearwrap(2000,1,-37) == 1996
@test Time.yearwrap(2000,1,-36) == 1997
@test Time.yearwrap(2000,1,-35) == 1997
@test Time.yearwrap(2000,1,-25) == 1997
@test Time.yearwrap(2000,1,-24) == 1998
@test Time.yearwrap(2000,1,-23) == 1998
@test Time.yearwrap(2000,1,-14) == 1998
@test Time.yearwrap(2000,1,-13) == 1998
@test Time.yearwrap(2000,1,-12) == 1999
@test Time.yearwrap(2000,1,-11) == 1999
@test Time.yearwrap(2000,1,-2) == 1999
@test Time.yearwrap(2000,1,-1) == 1999
@test Time.yearwrap(2000,1,0) == 2000
@test Time.yearwrap(2000,1,1) == 2000
@test Time.yearwrap(2000,1,11) == 2000
@test Time.yearwrap(2000,1,12) == 2001
@test Time.yearwrap(2000,1,13) == 2001
@test Time.yearwrap(2000,1,23) == 2001
@test Time.yearwrap(2000,1,24) == 2002
@test Time.yearwrap(2000,1,25) == 2002
@test Time.yearwrap(2000,1,36) == 2003
@test Time.yearwrap(2000,1,3600) == 2300
@test Time.yearwrap(2000,2,-2) == 1999
@test Time.yearwrap(2000,3,10) == 2001
@test Time.yearwrap(2000,4,-4) == 1999
@test Time.yearwrap(2000,5,8) == 2001
@test Time.yearwrap(2000,6,-18) == 1998
@test Time.yearwrap(2000,6,-6) == 1999
@test Time.yearwrap(2000,6,6) == 2000
@test Time.yearwrap(2000,6,7) == 2001
@test Time.yearwrap(2000,6,19) == 2002
@test Time.yearwrap(2000,12,-3600) == 1700
@test Time.yearwrap(2000,12,-36) == 1997
@test Time.yearwrap(2000,12,-35) == 1998
@test Time.yearwrap(2000,12,-24) == 1998
@test Time.yearwrap(2000,12,-23) == 1999
@test Time.yearwrap(2000,12,-14) == 1999
@test Time.yearwrap(2000,12,-13) == 1999
@test Time.yearwrap(2000,12,-12) == 1999
@test Time.yearwrap(2000,12,-11) == 2000
@test Time.yearwrap(2000,12,-2) == 2000
@test Time.yearwrap(2000,12,-1) == 2000
@test Time.yearwrap(2000,12,0) == 2000
@test Time.yearwrap(2000,12,1) == 2001
@test Time.yearwrap(2000,12,11) == 2001
@test Time.yearwrap(2000,12,12) == 2001
@test Time.yearwrap(2000,12,13) == 2002
@test Time.yearwrap(2000,12,24) == 2002
@test Time.yearwrap(2000,12,25) == 2003
@test Time.yearwrap(2000,12,36) == 2003
@test Time.yearwrap(2000,12,37) == 2004
@test Time.yearwrap(2000,12,3600) == 2300

dt = Time.DateTime(1999,12,27)
@test dt + Time.Month(1) == Time.DateTime(2000,1,27)
@test dt + Time.Month(-1) == Time.DateTime(1999,11,27)
@test dt + Time.Month(-11) == Time.DateTime(1999,1,27)
@test dt + Time.Month(11) == Time.DateTime(2000,11,27)
@test dt + Time.Month(-12) == Time.DateTime(1998,12,27)
@test dt + Time.Month(12) == Time.DateTime(2000,12,27)
@test dt + Time.Month(13) == Time.DateTime(2001,1,27)
@test dt + Time.Month(100) == Time.DateTime(2008,4,27)
@test dt + Time.Month(1000) == Time.DateTime(2083,4,27)
@test dt - Time.Month(1) == Time.DateTime(1999,11,27)
@test dt - Time.Month(-1) == Time.DateTime(2000,1,27)
@test dt - Time.Month(100) == Time.DateTime(1991,8,27)
@test dt - Time.Month(1000) == Time.DateTime(1916,8,27)
dt = Time.DateTime(2000,2,29)
@test dt + Time.Month(1) == Time.DateTime(2000,3,29)
@test dt - Time.Month(1) == Time.DateTime(2000,1,29)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Month(1) == Time.DateTime(1972,7,30,23,59,59)
@test dt - Time.Month(1) == Time.DateTime(1972,5,30,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Month(1) == Time.DateTime(1972,7,30,23,59,59)
@test dt - Time.Month(1) == Time.DateTime(1972,5,30,23,59,59)
@test dt + Time.Month(-1) == Time.DateTime(1972,5,30,23,59,59)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Week(1) == Time.DateTime(2000,1,3)
@test dt + Time.Week(100) == Time.DateTime(2001,11,26)
@test dt + Time.Week(1000) == Time.DateTime(2019,2,25)
@test dt - Time.Week(1) == Time.DateTime(1999,12,20)
@test dt - Time.Week(100) == Time.DateTime(1998,1,26)
@test dt - Time.Week(1000) == Time.DateTime(1980,10,27)
dt = Time.DateTime(2000,2,29)
@test dt + Time.Week(1) == Time.DateTime(2000,3,7)
@test dt - Time.Week(1) == Time.DateTime(2000,2,22)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Week(1) == Time.DateTime(1972,7,7,23,59,59)
@test dt - Time.Week(1) == Time.DateTime(1972,6,23,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Week(1) == Time.DateTime(1972,7,7,23,59,59)
@test dt - Time.Week(1) == Time.DateTime(1972,6,23,23,59,59)
@test dt + Time.Week(-1) == Time.DateTime(1972,6,23,23,59,59)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Day(1) == Time.DateTime(1999,12,28)
@test dt + Time.Day(100) == Time.DateTime(2000,4,5)
@test dt + Time.Day(1000) == Time.DateTime(2002,9,22)
@test dt - Time.Day(1) == Time.DateTime(1999,12,26)
@test dt - Time.Day(100) == Time.DateTime(1999,9,18)
@test dt - Time.Day(1000) == Time.DateTime(1997,4,1)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Day(1) == Time.DateTime(1972,7,1,23,59,59)
@test dt - Time.Day(1) == Time.DateTime(1972,6,29,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Day(1) == Time.DateTime(1972,7,1,23,59,59)
@test dt - Time.Day(1) == Time.DateTime(1972,6,29,23,59,59)
@test dt + Time.Day(-1) == Time.DateTime(1972,6,29,23,59,59)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Hour(1) == Time.DateTime(1999,12,27,1)
@test dt + Time.Hour(100) == Time.DateTime(1999,12,31,4)
@test dt + Time.Hour(1000) == Time.DateTime(2000,2,6,16)
@test dt - Time.Hour(1) == Time.DateTime(1999,12,26,23)
@test dt - Time.Hour(100) == Time.DateTime(1999,12,22,20)
@test dt - Time.Hour(1000) == Time.DateTime(1999,11,15,8)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Hour(1) == Time.DateTime(1972,7,1,0,59,59)
@test dt - Time.Hour(1) == Time.DateTime(1972,6,30,22,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Hour(1) == Time.DateTime(1972,7,1,0,59,59)
@test dt - Time.Hour(1) == Time.DateTime(1972,6,30,22,59,59)
@test dt + Time.Hour(-1) == Time.DateTime(1972,6,30,22,59,59)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Minute(1) == Time.DateTime(1999,12,27,0,1)
@test dt + Time.Minute(100) == Time.DateTime(1999,12,27,1,40)
@test dt + Time.Minute(1000) == Time.DateTime(1999,12,27,16,40)
@test dt - Time.Minute(1) == Time.DateTime(1999,12,26,23,59)
@test dt - Time.Minute(100) == Time.DateTime(1999,12,26,22,20)
@test dt - Time.Minute(1000) == Time.DateTime(1999,12,26,7,20)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Minute(1) == Time.DateTime(1972,7,1,0,0,59)
@test dt - Time.Minute(1) == Time.DateTime(1972,6,30,23,58,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Minute(1) == Time.DateTime(1972,7,1,0,0,59)
@test dt - Time.Minute(1) == Time.DateTime(1972,6,30,23,58,59)
@test dt + Time.Minute(-1) == Time.DateTime(1972,6,30,23,58,59)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Second(1) == Time.DateTime(1999,12,27,0,0,1)
@test dt + Time.Second(100) == Time.DateTime(1999,12,27,0,1,40)
@test dt + Time.Second(1000) == Time.DateTime(1999,12,27,0,16,40)
@test dt - Time.Second(1) == Time.DateTime(1999,12,26,23,59,59)
@test dt - Time.Second(100) == Time.DateTime(1999,12,26,23,58,20)
@test dt - Time.Second(1000) == Time.DateTime(1999,12,26,23,43,20)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Second(1) == Time.DateTime(1972,7,1)
@test dt - Time.Second(1) == Time.DateTime(1972,6,30,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Second(1) == Time.DateTime(1972,6,30,23,59,60)
@test dt - Time.Second(1) == Time.DateTime(1972,6,30,23,59,58)
@test dt + Time.Second(-1) == Time.DateTime(1972,6,30,23,59,58)

dt = Time.DateTime(1999,12,27)
@test dt + Time.Millisecond(1) == Time.DateTime(1999,12,27,0,0,0,1)
@test dt + Time.Millisecond(100) == Time.DateTime(1999,12,27,0,0,0,100)
@test dt + Time.Millisecond(1000) == Time.DateTime(1999,12,27,0,0,1)
@test dt - Time.Millisecond(1) == Time.DateTime(1999,12,26,23,59,59,999)
@test dt - Time.Millisecond(100) == Time.DateTime(1999,12,26,23,59,59,900)
@test dt - Time.Millisecond(1000) == Time.DateTime(1999,12,26,23,59,59)
dt = Time.DateTime(1972,6,30,23,59,60)
@test dt + Time.Millisecond(1) == Time.DateTime(1972,6,30,23,59,60,1)
@test dt - Time.Millisecond(1) == Time.DateTime(1972,6,30,23,59,59,999)
dt = Time.DateTime(1972,6,30,23,59,59)
@test dt + Time.Millisecond(1) == Time.DateTime(1972,6,30,23,59,59,1)
@test dt - Time.Millisecond(1) == Time.DateTime(1972,6,30,23,59,58,999)
@test dt + Time.Millisecond(-1) == Time.DateTime(1972,6,30,23,59,58,999)

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
@test dt + Time.Month(100) == Time.Date(2008,4,27)
@test dt + Time.Month(1000) == Time.Date(2083,4,27)
@test dt - Time.Month(1) == Time.Date(1999,11,27)
@test dt - Time.Month(100) == Time.Date(1991,8,27)
@test dt - Time.Month(1000) == Time.Date(1916,8,27)
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
dt = Time.DateTime(1999,12,27)
dt1 = Time.Date(1999,12,27)
check = (52,52,52,52,52,52,52,1,1,1,1,1,1,1,2,2,2,2,2,2,2)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.DateTime(2000,12,25)
dt1 = Time.Date(2000,12,25)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.DateTime(2030,12,23)
dt1 = Time.Date(2030,12,23)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end
dt = Time.DateTime(2004,12,20)
dt1 = Time.Date(2004,12,20)
check = (52,52,52,52,52,52,52,53,53,53,53,53,53,53,1,1,1,1,1,1,1)
for i = 1:21
	@test Time.week(dt) == check[i]
	@test Time.week(dt1) == check[i]
	dt = dt + Time.Day(1)
	dt1 = dt1 + Time.Day(1)
end

# Leapseconds
a = Time.DateTime(1972,6,29,23,59,59)
b = Time.DateTime(1972,6,29,23,59,60) #not a leapsecond, rolls forward
@test b.instant.ms - a.instant.ms == 1000
@test string(b) == "1972-06-30T00:00:00 UTC"

a = Time.DateTime(1972,6,30,23,59,59)
b = Time.DateTime(1972,6,30,23,59,60)
c = Time.DateTime(1972,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1972-06-30T23:59:60 UTC"
@test string(c) == "1972-07-01T00:00:00 UTC"
a = Time.DateTime(1972,12,31,23,59,59)
b = Time.DateTime(1972,12,31,23,59,60)
c = Time.DateTime(1973,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1972-12-31T23:59:60 UTC"
@test string(c) == "1973-01-01T00:00:00 UTC"
a = Time.DateTime(1973,12,31,23,59,59)
b = Time.DateTime(1973,12,31,23,59,60)
c = Time.DateTime(1974,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1973-12-31T23:59:60 UTC"
@test string(c) == "1974-01-01T00:00:00 UTC"
a = Time.DateTime(1974,12,31,23,59,59)
b = Time.DateTime(1974,12,31,23,59,60)
c = Time.DateTime(1975,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1974-12-31T23:59:60 UTC"
@test string(c) == "1975-01-01T00:00:00 UTC"
a = Time.DateTime(1975,12,31,23,59,59)
b = Time.DateTime(1975,12,31,23,59,60)
c = Time.DateTime(1976,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1975-12-31T23:59:60 UTC"
@test string(c) == "1976-01-01T00:00:00 UTC"

a = Time.DateTime(1976,12,31,23,59,59)
b = Time.DateTime(1976,12,31,23,59,60)
c = Time.DateTime(1977,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1976-12-31T23:59:60 UTC"
@test string(c) == "1977-01-01T00:00:00 UTC"
a = Time.DateTime(1977,12,31,23,59,59)
b = Time.DateTime(1977,12,31,23,59,60)
c = Time.DateTime(1978,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1977-12-31T23:59:60 UTC"
@test string(c) == "1978-01-01T00:00:00 UTC"
a = Time.DateTime(1978,12,31,23,59,59)
b = Time.DateTime(1978,12,31,23,59,60)
c = Time.DateTime(1979,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1978-12-31T23:59:60 UTC"
@test string(c) == "1979-01-01T00:00:00 UTC"
a = Time.DateTime(1979,12,31,23,59,59)
b = Time.DateTime(1979,12,31,23,59,60)
c = Time.DateTime(1980,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1979-12-31T23:59:60 UTC"
@test string(c) == "1980-01-01T00:00:00 UTC"
a = Time.DateTime(1981,6,30,23,59,59)
b = Time.DateTime(1981,6,30,23,59,60)
c = Time.DateTime(1981,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1981-06-30T23:59:60 UTC"
@test string(c) == "1981-07-01T00:00:00 UTC"
a = Time.DateTime(1982,6,30,23,59,59)
b = Time.DateTime(1982,6,30,23,59,60)
c = Time.DateTime(1982,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1982-06-30T23:59:60 UTC"
@test string(c) == "1982-07-01T00:00:00 UTC"
a = Time.DateTime(1983,6,30,23,59,59)
b = Time.DateTime(1983,6,30,23,59,60)
c = Time.DateTime(1983,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1983-06-30T23:59:60 UTC"
@test string(c) == "1983-07-01T00:00:00 UTC"
a = Time.DateTime(1985,6,30,23,59,59)
b = Time.DateTime(1985,6,30,23,59,60)
c = Time.DateTime(1985,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1985-06-30T23:59:60 UTC"
@test string(c) == "1985-07-01T00:00:00 UTC"
a = Time.DateTime(1987,12,31,23,59,59)
b = Time.DateTime(1987,12,31,23,59,60)
c = Time.DateTime(1988,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1987-12-31T23:59:60 UTC"
@test string(c) == "1988-01-01T00:00:00 UTC"
a = Time.DateTime(1989,12,31,23,59,59)
b = Time.DateTime(1989,12,31,23,59,60)
c = Time.DateTime(1990,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1989-12-31T23:59:60 UTC"
@test string(c) == "1990-01-01T00:00:00 UTC"
a = Time.DateTime(1990,12,31,23,59,59)
b = Time.DateTime(1990,12,31,23,59,60)
c = Time.DateTime(1991,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1990-12-31T23:59:60 UTC"
@test string(c) == "1991-01-01T00:00:00 UTC"
a = Time.DateTime(1992,6,30,23,59,59)
b = Time.DateTime(1992,6,30,23,59,60)
c = Time.DateTime(1992,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1992-06-30T23:59:60 UTC"
@test string(c) == "1992-07-01T00:00:00 UTC"
a = Time.DateTime(1993,6,30,23,59,59)
b = Time.DateTime(1993,6,30,23,59,60)
c = Time.DateTime(1993,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1993-06-30T23:59:60 UTC"
@test string(c) == "1993-07-01T00:00:00 UTC"
a = Time.DateTime(1994,6,30,23,59,59)
b = Time.DateTime(1994,6,30,23,59,60)
c = Time.DateTime(1994,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1994-06-30T23:59:60 UTC"
@test string(c) == "1994-07-01T00:00:00 UTC"
a = Time.DateTime(1995,12,31,23,59,59)
b = Time.DateTime(1995,12,31,23,59,60)
c = Time.DateTime(1996,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1995-12-31T23:59:60 UTC"
@test string(c) == "1996-01-01T00:00:00 UTC"
a = Time.DateTime(1997,6,30,23,59,59)
b = Time.DateTime(1997,6,30,23,59,60)
c = Time.DateTime(1997,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1997-06-30T23:59:60 UTC"
@test string(c) == "1997-07-01T00:00:00 UTC"
a = Time.DateTime(1998,12,31,23,59,59)
b = Time.DateTime(1998,12,31,23,59,60)
c = Time.DateTime(1999,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "1998-12-31T23:59:60 UTC"
@test string(c) == "1999-01-01T00:00:00 UTC"
a = Time.DateTime(2005,12,31,23,59,59)
b = Time.DateTime(2005,12,31,23,59,60)
c = Time.DateTime(2006,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2005-12-31T23:59:60 UTC"
@test string(c) == "2006-01-01T00:00:00 UTC"
a = Time.DateTime(2008,12,31,23,59,59)
b = Time.DateTime(2008,12,31,23,59,60)
c = Time.DateTime(2009,1,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2008-12-31T23:59:60 UTC"
@test string(c) == "2009-01-01T00:00:00 UTC"
a = Time.DateTime(2012,6,30,23,59,59)
b = Time.DateTime(2012,6,30,23,59,60)
c = Time.DateTime(2012,7,1,0,0,0)
@test b.instant.ms - a.instant.ms == 1000
@test c.instant.ms - b.instant.ms == 1000
@test string(b) == "2012-06-30T23:59:60 UTC"
@test string(c) == "2012-07-01T00:00:00 UTC"

a = Time.DateTime(1972,6,30,23,59,59)
@test Time.calendar(a) == Time.ISOCalendar
@test Time.timezone(a) == Time.UTC
@test Time.precision(a) == Time.Millisecond
@test string(typemax(Time.DateTime)) == "292277024-12-31T23:59:59 UTC"
@test string(typemin(Time.DateTime)) == "-292277023-01-01T00:00:00 UTC"
@test string(typemax(Time.Date)) == "252522163911149-12-31"
@test string(typemin(Time.Date)) == "-252522163911150-01-01"

# Name functions
jan = Time.DateTime(2013,1,1)
feb = Time.DateTime(2013,2,2)
mar = Time.DateTime(2013,3,3)
apr = Time.DateTime(2013,4,4)
may = Time.DateTime(2013,5,5)
jun = Time.DateTime(2013,6,7)
jul = Time.DateTime(2013,7,7)
aug = Time.DateTime(2013,8,8)
sep = Time.DateTime(2013,9,9)
oct = Time.DateTime(2013,10,10)
nov = Time.DateTime(2013,11,11)
dec = Time.DateTime(2013,12,11)
monthnames = ["January","February","March","April",
                "May","June","July","August","September",
                "October","November","December"]
daysofweek = [Time.Tue,Time.Sat,Time.Sun,Time.Thu,Time.Sun,Time.Fri,
			  Time.Sun,Time.Thu,Time.Mon,Time.Thu,Time.Mon,Time.Wed]
dows = ["Tuesday","Saturday","Sunday","Thursday","Sunday","Friday",
		"Sunday","Thursday","Monday","Thursday","Monday","Wednesday"]
for (i,dt) in enumerate([jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec])
	@test Time.month(dt) == i
	@test Time.monthname(dt) == monthnames[i]
	@test Time.monthabbr(dt) == monthnames[i][1:3]
	@test Time.dayofweek(dt) == daysofweek[i]
	@test Time.dayname(dt) == dows[i]
	@test Time.dayabbr(dt) == dows[i][1:3]
end

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

@test Time.isleap(Time.DateTime(1900)) == false
@test Time.isleap(Time.DateTime(2000)) == true
@test Time.isleap(Time.DateTime(2004)) == true
@test Time.isleap(Time.DateTime(2008)) == true
@test Time.isleap(Time.DateTime(0)) == true
@test Time.isleap(Time.DateTime(1)) == false
@test Time.isleap(Time.DateTime(-1)) == false
@test Time.isleap(Time.DateTime(4)) == true
@test Time.isleap(Time.DateTime(-4)) == true

@test Time.dayofweek(Time.DateTime(2013,12,22)) == 0
@test Time.dayofweek(Time.DateTime(2013,12,23)) == 1
@test Time.dayofweek(Time.DateTime(2013,12,24)) == 2
@test Time.dayofweek(Time.DateTime(2013,12,25)) == 3
@test Time.dayofweek(Time.DateTime(2013,12,26)) == 4
@test Time.dayofweek(Time.DateTime(2013,12,27)) == 5
@test Time.dayofweek(Time.DateTime(2013,12,28)) == 6
@test Time.dayofweek(Time.DateTime(2013,12,29)) == 0

@test Time.dayofweekofmonth(Time.DateTime(2013,12,1)) == 1
@test Time.dayofweekofmonth(Time.DateTime(2013,12,8)) == 2
@test Time.dayofweekofmonth(Time.DateTime(2013,12,15)) == 3
@test Time.dayofweekofmonth(Time.DateTime(2013,12,22)) == 4
@test Time.dayofweekofmonth(Time.DateTime(2013,12,29)) == 5

@test Time.daysofweekinmonth(Time.DateTime(2013,12,1)) == 5

@test Time.dayofyear(Time.DateTime(2000,1,1)) == 1
@test Time.dayofyear(Time.DateTime(2004,1,1)) == 1
@test Time.dayofyear(Time.DateTime(20013,1,1)) == 1
@test Time.dayofyear(Time.DateTime(2000,12,31)) == 366
@test Time.dayofyear(Time.DateTime(2001,12,31)) == 365
dt = Time.DateTime(2000,1,1)
for i = 1:366
	@test Time.dayofyear(dt) == i
	dt += Time.Day(1)
end
dt = Time.DateTime(2001,1,1)
for i = 1:365
	@test Time.dayofyear(dt) == i
	dt += Time.Day(1)
end

a = Time.Date(2014,1,5)
b = Time.Date(2014,1,6)
c = Time.Date(2014,1,7)
d = Time.Date(2014,1,8)
e = Time.Date(2014,1,9)
f = Time.Date(2014,1,10)
g = Time.Date(2014,1,11)
@test Time.firstdayofweek(a) == a
@test Time.firstdayofweek(b) == a
@test Time.firstdayofweek(c) == a
@test Time.firstdayofweek(d) == a
@test Time.firstdayofweek(e) == a
@test Time.firstdayofweek(f) == a
@test Time.firstdayofweek(g) == a
dt = a
for i = 0:364
	@test Time.firstdayofweek(dt) == a + Time.Week(div(i,7))
	dt += Time.Day(1)
end
a = Time.DateTime(2014,1,5)
b = Time.DateTime(2014,1,6)
c = Time.DateTime(2014,1,7)
d = Time.DateTime(2014,1,8)
e = Time.DateTime(2014,1,9)
f = Time.DateTime(2014,1,10)
g = Time.DateTime(2014,1,11)
@test Time.firstdayofweek(a) == a
@test Time.firstdayofweek(b) == a
@test Time.firstdayofweek(c) == a
@test Time.firstdayofweek(d) == a
@test Time.firstdayofweek(e) == a
@test Time.firstdayofweek(f) == a
@test Time.firstdayofweek(g) == a
dt = a
for i = 0:364
	@test Time.firstdayofweek(dt) == a + Time.Week(div(i,7))
	dt += Time.Day(1)
end
@test Time.firstdayofweek(Time.DateTime(2013,12,24)) == Time.DateTime(2013,12,22)
a = Time.Date(2014,1,5)
b = Time.Date(2014,1,6)
c = Time.Date(2014,1,7)
d = Time.Date(2014,1,8)
e = Time.Date(2014,1,9)
f = Time.Date(2014,1,10)
g = Time.Date(2014,1,11)
@test Time.lastdayofweek(a) == g
@test Time.lastdayofweek(b) == g
@test Time.lastdayofweek(c) == g
@test Time.lastdayofweek(d) == g
@test Time.lastdayofweek(e) == g
@test Time.lastdayofweek(f) == g
@test Time.lastdayofweek(g) == g
dt = a
for i = 0:364
	@test Time.lastdayofweek(dt) == g + Time.Week(div(i,7))
	dt += Time.Day(1)
end
a = Time.DateTime(2014,1,5)
b = Time.DateTime(2014,1,6)
c = Time.DateTime(2014,1,7)
d = Time.DateTime(2014,1,8)
e = Time.DateTime(2014,1,9)
f = Time.DateTime(2014,1,10)
g = Time.DateTime(2014,1,11)
@test Time.lastdayofweek(a) == g
@test Time.lastdayofweek(b) == g
@test Time.lastdayofweek(c) == g
@test Time.lastdayofweek(d) == g
@test Time.lastdayofweek(e) == g
@test Time.lastdayofweek(f) == g
@test Time.lastdayofweek(g) == g
dt = a
for i = 0:364
	@test Time.lastdayofweek(dt) == g + Time.Week(div(i,7))
	dt += Time.Day(1)
end
@test Time.lastdayofweek(Time.DateTime(2013,12,24)) == Time.DateTime(2013,12,28)

@test Date(DateTime(Date(2012,7,1))) == Date(2012,7,1)
@test Time.unix2date(Time.date2unix(DateTime(2000,1,1))) == DateTime(2000,1,1)
@test Time.DateTime(1970).instant.ms == Time.UNIXEPOCH
#@test Time.date2unix(now()) == time()

startdate = Date(2014,1,1)
stopdate = Date(2014,2,1)
januarymondays2014 = [Date(2014,1,6),Date(2014,1,13),Date(2014,1,20),Date(2014,1,27)]
@test Time.recur(x->Time.dayofweek(x)==Time.Monday,startdate,stopdate) == januarymondays2014
@test Time.recur(x->!(Time.dayofweek(x)==Time.Monday),startdate,stopdate;inclusion=false) == januarymondays2014

# Tests from here: http://en.wikipedia.org/wiki/Unix_time
@test string(Time.unix2date(1095379198.75)) == string("2004-09-16T23:59:58.75 UTC")
@test string(Time.unix2date(1095379199.00)) == string("2004-09-16T23:59:59 UTC")
@test string(Time.unix2date(1095379199.25)) == string("2004-09-16T23:59:59.25 UTC")
@test string(Time.unix2date(1095379199.50)) == string("2004-09-16T23:59:59.5 UTC")
@test string(Time.unix2date(1095379199.75)) == string("2004-09-16T23:59:59.75 UTC")
@test string(Time.unix2date(1095379200.00)) == string("2004-09-17T00:00:00 UTC")
@test string(Time.unix2date(1095379200.25)) == string("2004-09-17T00:00:00.25 UTC")
@test string(Time.unix2date(1095379200.50)) == string("2004-09-17T00:00:00.5 UTC")
@test string(Time.unix2date(1095379200.75)) == string("2004-09-17T00:00:00.75 UTC")
@test string(Time.unix2date(1095379201.00)) == string("2004-09-17T00:00:01 UTC")
@test string(Time.unix2date(1095379201.25)) == string("2004-09-17T00:00:01.25 UTC")
@test string(Time.unix2date(915148798.75)) == string("1998-12-31T23:59:58.75 UTC")
@test string(Time.unix2date(915148799.00)) == string("1998-12-31T23:59:59 UTC")
@test string(Time.unix2date(915148799.25)) == string("1998-12-31T23:59:59.25 UTC")
@test string(Time.unix2date(915148799.50)) == string("1998-12-31T23:59:59.5 UTC")
@test string(Time.unix2date(915148799.75)) == string("1998-12-31T23:59:59.75 UTC")
@test string(Time.unix2date(915148800.00)) == string("1999-01-01T00:00:00 UTC")
@test string(Time.unix2date(915148800.25)) == string("1999-01-01T00:00:00.25 UTC")
@test string(Time.unix2date(915148800.50)) == string("1999-01-01T00:00:00.5 UTC")
@test string(Time.unix2date(915148800.75)) == string("1999-01-01T00:00:00.75 UTC")
@test string(Time.unix2date(915148801.00)) == string("1999-01-01T00:00:01 UTC")
@test string(Time.unix2date(915148801.25)) == string("1999-01-01T00:00:01.25 UTC")

@test Date(Time.ratadays2date(734869)...) == Date(2013,1,1)
@test Time.date2ratadays(Date(Time.ratadays2date(734869)...)) == 734869

# Tests from here: http://mysite.verizon.net/aesir_research/date/back.htm#JDN
@test Time.julian2date(1721119.5) == DateTime(0,3,1)
@test Time.julian2date(1721424.5) == DateTime(0,12,31)
@test Time.julian2date(1721425.5) == DateTime(1,1,1)
@test Time.julian2date(2299149.5) == DateTime(1582,10,4)
@test Time.julian2date(2415020.5) == DateTime(1900,1,1)
@test Time.julian2date(2415385.5) == DateTime(1901,1,1)
@test Time.julian2date(2440587.5) == DateTime(1970,1,1)
@test Time.julian2date(2444239.5) == DateTime(1980,1,1)
@test Time.julian2date(2452695.625) == DateTime(2003,2,25,3)
@test Time.date2julian(DateTime(2013,12,3,21)) == 2456630.375

# DateTime parsing
#'1996-January-15'
dt = Time.DateTime(1996,1,15)
f = "yy-mm-dd"
a = "96-01-15"
@test Time.DateTime(a,f) + Time.Year(1900) == dt
a1 = "96-1-15"
@test Time.DateTime(a1,f) + Time.Year(1900) == dt
a2 = "96-1-1"
@test Time.DateTime(a2,f) + Time.Year(1900) + Time.Day(14) == dt
a3 = "1996-1-15"
@test Time.DateTime(a3,f) == dt
a4 = "1996-Jan-15"
@test_throws Time.DateTime(a4,f) # Trying to use month name, but specified only "mm"

f = "yy/mmm/dd"
b = "96/Feb/15"
@test Time.DateTime(b,f) + Time.Year(1900) == dt + Time.Month(1)
b1 = "1996/Feb/15"
@test Time.DateTime(b1,f) == dt + Time.Month(1)
b2 = "96/Feb/1"
@test Time.DateTime(b2,f) + Time.Year(1900) + Time.Day(14) == dt + Time.Month(1)
# Here we've specifed the month name, yet fail to parse and default to January
b3 = "96/2/15"
@test Time.DateTime(b3,f) == dt - Time.Year(1900) 

f = "yy:dd:mm"
c = "96:15:01"
@test Time.DateTime(c,f) + Time.Year(1900) == dt
c1 = "1996:15:01"
@test Time.DateTime(c1,f) == dt
c2 = "96:15:1"
@test Time.DateTime(c2,f) + Time.Year(1900) == dt
c3 = "96:1:01"
@test Time.DateTime(c3,f) + Time.Year(1900) + Time.Day(14) == dt
c4 = "96:15:01 # random comment"
@test_throws Time.DateTime(c4,f) # Currently doesn't handle trailing comments

f = "yyyy,mmm,dd"
d = "1996,Jan,15"
@test Time.DateTime(d,f) == dt
d1 = "96,Jan,15"
@test Time.DateTime(d1,f) + Time.Year(1900) == dt
d2 = "1996,Jan,1"
@test Time.DateTime(d2,f) + Time.Day(14) == dt
d3 = "1996,2,15"
@test Time.DateTime(d3,f) != Time.DateTime(1996,2,15) # Same as above

f = "yyyy.mmmm.dd"
e = "1996.January.15"
@test Time.DateTime(e,f) == dt
e1 = "96.January.15"
@test Time.DateTime(e1,f) + Time.Year(1900) == dt

fo = "yyyy m dd"
f = "1996 1 15"
@test Time.DateTime(f,fo) == dt
f1 = "1996 01 15"
@test Time.DateTime(f1,fo) == dt
f2 = "1996 1 1"
@test Time.DateTime(f2,fo) + Time.Day(14) == dt

j = "1996-01-15 UTC"
f = "yyyy-mm-dd zzz"
@test Time.DateTime(j,f) == dt
k = "1996-01-15 10:00:00 UTC"
f = "yyyy-mm-dd HH:MM:SS zzz"
@test Time.DateTime(k,f) == dt + Time.Hour(10)
l = "1996-01-15 10:10:10.25 UTC"
f = "yyyy-mm-dd HH:MM:SS.ss zzz"
@test Time.DateTime(l,f) == dt + Time.Hour(10) + Time.Minute(10) + Time.Second(10) + Time.Millisecond(250)

r = "1/15/1996" # Excel
f = "m/dd/yyyy"
@test Time.DateTime(r,f) == dt
s = "19960115"
f = "yyyymmdd"
@test Time.DateTime(s,f) == dt
v = "1996-01-15 10:00:00"
f = "yyyy-mm-dd HH:MM:SS"
@test Time.DateTime(v,f) == dt + Time.Hour(10)
w = "1996-01-15T10:00:00 UTC"
f = "yyyy-mm-ddTHH:MM:SS zzz"
@test Time.DateTime(w,f;sep="T") == dt + Time.Hour(10)

f = "yyyy/m"
y = "1996/1"
@test Time.DateTime(y,f) == dt - Time.Day(14)
y1 = "1996/1/15"
@test_throws Time.DateTime(y1,f)
y2 = "96/1"
@test Time.DateTime(y2,f) + Time.Year(1900) == dt - Time.Day(14)

f = "yyyy"
z = "1996"
@test Time.DateTime(z,f) == dt - Time.Day(14)
z1 = "1996-3"
@test Time.DateTime(z1,f) != Time.DateTime(1996,3)
z2 = "1996-3-1"
@test Time.DateTime(z2,f) != Time.DateTime(1996,3)

aa = "1/5/1996"
f = "m/d/yyyy"
@test Time.DateTime(aa,f) == dt - Time.Day(10)
bb = "5/1/1996"
f = "d/m/yyyy"
@test Time.DateTime(bb,f) == dt - Time.Day(10)
cc = "01151996"
f = "mmddyyyy"
@test Time.DateTime(cc,f) == dt
dd = "15011996"
f = "ddmmyyyy"
@test Time.DateTime(dd,f) == dt
ee = "01199615"
f = "mmyyyydd"
@test Time.DateTime(ee,f) == dt
ff = "1996-15-Jan"
f = "yyyy-dd-mmm"
@test Time.DateTime(ff,f) == dt
gg = "Jan-1996-15"
f = "mmm-yyyy-dd"
@test Time.DateTime(gg,f) == dt

@test_throws DateTime("18/05/2009","mm/dd/yyyy") # switched month and day
@test_throws DateTime("18/05/2009 16","mm/dd/yyyy hh") # same
@test DateTime("18/05/2009 16:12","mm/dd/yyyy hh:mm") == DateTime(2009,12,16) # here they used mm instead of MM for minutes
@test_throws DateTime("18:05:2009","mm/dd/yyyy")
@test Date("2009年12月01日","yyyy年mm月dd日") == Date(2009,12,1)
@test Date("2009-12-01","yyyy-mm-dd") == Date(2009,12,1)

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
@test Time.DateTime(y) == Time.DateTime(1)
@test Time.DateTime(y,m) == Time.DateTime(1,1)
@test Time.DateTime(y,m,d) == Time.DateTime(1,1,1)
@test Time.DateTime(y,m,d,h) == Time.DateTime(1,1,1,1)
@test Time.DateTime(y,m,d,h,mi) == Time.DateTime(1,1,1,1,1)
@test Time.DateTime(y,m,d,h,mi,s) == Time.DateTime(1,1,1,1,1,1)
@test Time.DateTime(y,m,d,h,mi,s,ms) == Time.DateTime(1,1,1,1,1,1,1)
@test_throws Time.DateTime(Time.Day(10),Time.Month(2),y)
@test_throws Time.DateTime(Time.Second(10),Time.Month(2),y,Time.Hour(4))
@test_throws Time.DateTime(Time.Year(1),Time.Month(2),Time.Day(1),Time.Hour(4),Time.Second(10))
@test Time.Date(y) == Time.Date(1)
@test Time.Date(y,m) == Time.Date(1,1)
@test Time.Date(y,m,d) == Time.Date(1,1,1)
@test_throws Time.Date(m)
@test_throws Time.Date(d,y)
@test_throws Time.Date(d,m)
@test_throws Time.Date(m,y)
@test Time.Year(int8(1)) == y
@test Time.Year(uint8(1)) == y
@test Time.Year(int16(1)) == y
@test Time.Year(uint16(1)) == y
@test Time.Year(int(1)) == y
@test Time.Year(uint(1)) == y
@test Time.Year(int64(1)) == y
@test Time.Year(uint64(1)) == y
@test Time.Year(int128(1)) == y
@test Time.Year(uint128(1)) == y
@test Time.Year(big(1)) == y
@test_throws Time.Year(BigFloat(1)) == y
@test_throws Time.Year(float(1)) == y
@test_throws Time.Year(float32(1)) == y
@test_throws Time.Year(float16(1)) == y
@test_throws Time.Year(Rational(1)) == y
@test_throws Time.Year(complex(1)) == y
@test Time.Year(true) == y
@test Time.Year(false) != y
@test Time.Year('\x01') == y
@test_throws Time.Year(:hey) == y
@test Time.Year(real(1)) == y
@test_throws Time.Year(m) == y
@test_throws Time.Year(w) == y
@test_throws Time.Year(d) == y
@test_throws Time.Year(h) == y
@test_throws Time.Year(mi) == y
@test_throws Time.Year(s) == y
@test_throws Time.Year(ms) == y
@test_throws Time.Year(Date(2013,1,1))
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
dt = Time.DateTime(2012,12,21)
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
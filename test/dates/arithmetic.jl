# This file is a part of Julia. License is MIT: http://julialang.org/license

# DateTime arithmetic
a = Dates.DateTime(2013,1,1,0,0,0,1)
b = Dates.DateTime(2013,1,1,0,0,0,0)
@test a - b == Dates.Millisecond(1)
@test Dates.DateTime(2013,1,2) - b == Dates.Millisecond(86400000)

# DateTime-Year arithmetic
dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Year(1) == Dates.DateTime(2000,12,27)
@test dt + Dates.Year(100) == Dates.DateTime(2099,12,27)
@test dt + Dates.Year(1000) == Dates.DateTime(2999,12,27)
@test dt - Dates.Year(1) == Dates.DateTime(1998,12,27)
@test dt - Dates.Year(100) == Dates.DateTime(1899,12,27)
@test dt - Dates.Year(1000) == Dates.DateTime(999,12,27)
dt = Dates.DateTime(2000,2,29)
@test dt + Dates.Year(1) == Dates.DateTime(2001,2,28)
@test dt - Dates.Year(1) == Dates.DateTime(1999,2,28)
@test dt + Dates.Year(4) == Dates.DateTime(2004,2,29)
@test dt - Dates.Year(4) == Dates.DateTime(1996,2,29)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Year(1) == Dates.DateTime(1973,6,30,23,59,59)
@test dt - Dates.Year(1) == Dates.DateTime(1971,6,30,23,59,59)
@test dt + Dates.Year(-1) == Dates.DateTime(1971,6,30,23,59,59)
@test dt - Dates.Year(-1) == Dates.DateTime(1973,6,30,23,59,59)

# Wrapping arithemtic for Months
# This ends up being trickier than expected because
# the user might do 2014-01-01 + Month(-14)
# monthwrap figures out the resulting month
# when adding/subtracting months from a date
@test Dates.monthwrap(1,-14) == 11
@test Dates.monthwrap(1,-13) == 12
@test Dates.monthwrap(1,-12) == 1
@test Dates.monthwrap(1,-11) == 2
@test Dates.monthwrap(1,-10) == 3
@test Dates.monthwrap(1,-9) == 4
@test Dates.monthwrap(1,-8) == 5
@test Dates.monthwrap(1,-7) == 6
@test Dates.monthwrap(1,-6) == 7
@test Dates.monthwrap(1,-5) == 8
@test Dates.monthwrap(1,-4) == 9
@test Dates.monthwrap(1,-3) == 10
@test Dates.monthwrap(1,-2) == 11
@test Dates.monthwrap(1,-1) == 12
@test Dates.monthwrap(1,0) == 1
@test Dates.monthwrap(1,1) == 2
@test Dates.monthwrap(1,2) == 3
@test Dates.monthwrap(1,3) == 4
@test Dates.monthwrap(1,4) == 5
@test Dates.monthwrap(1,5) == 6
@test Dates.monthwrap(1,6) == 7
@test Dates.monthwrap(1,7) == 8
@test Dates.monthwrap(1,8) == 9
@test Dates.monthwrap(1,9) == 10
@test Dates.monthwrap(1,10) == 11
@test Dates.monthwrap(1,11) == 12
@test Dates.monthwrap(1,12) == 1
@test Dates.monthwrap(1,13) == 2
@test Dates.monthwrap(1,24) == 1
@test Dates.monthwrap(12,-14) == 10
@test Dates.monthwrap(12,-13) == 11
@test Dates.monthwrap(12,-12) == 12
@test Dates.monthwrap(12,-11) == 1
@test Dates.monthwrap(12,-2) == 10
@test Dates.monthwrap(12,-1) == 11
@test Dates.monthwrap(12,0) == 12
@test Dates.monthwrap(12,1) == 1
@test Dates.monthwrap(12,2) == 2
@test Dates.monthwrap(12,11) == 11
@test Dates.monthwrap(12,12) == 12
@test Dates.monthwrap(12,13) == 1

# yearwrap figures out the resulting year
# when adding/subtracting months from a date
@test Dates.yearwrap(2000,1,-3600) == 1700
@test Dates.yearwrap(2000,1,-37) == 1996
@test Dates.yearwrap(2000,1,-36) == 1997
@test Dates.yearwrap(2000,1,-35) == 1997
@test Dates.yearwrap(2000,1,-25) == 1997
@test Dates.yearwrap(2000,1,-24) == 1998
@test Dates.yearwrap(2000,1,-23) == 1998
@test Dates.yearwrap(2000,1,-14) == 1998
@test Dates.yearwrap(2000,1,-13) == 1998
@test Dates.yearwrap(2000,1,-12) == 1999
@test Dates.yearwrap(2000,1,-11) == 1999
@test Dates.yearwrap(2000,1,-2) == 1999
@test Dates.yearwrap(2000,1,-1) == 1999
@test Dates.yearwrap(2000,1,0) == 2000
@test Dates.yearwrap(2000,1,1) == 2000
@test Dates.yearwrap(2000,1,11) == 2000
@test Dates.yearwrap(2000,1,12) == 2001
@test Dates.yearwrap(2000,1,13) == 2001
@test Dates.yearwrap(2000,1,23) == 2001
@test Dates.yearwrap(2000,1,24) == 2002
@test Dates.yearwrap(2000,1,25) == 2002
@test Dates.yearwrap(2000,1,36) == 2003
@test Dates.yearwrap(2000,1,3600) == 2300
@test Dates.yearwrap(2000,2,-2) == 1999
@test Dates.yearwrap(2000,3,10) == 2001
@test Dates.yearwrap(2000,4,-4) == 1999
@test Dates.yearwrap(2000,5,8) == 2001
@test Dates.yearwrap(2000,6,-18) == 1998
@test Dates.yearwrap(2000,6,-6) == 1999
@test Dates.yearwrap(2000,6,6) == 2000
@test Dates.yearwrap(2000,6,7) == 2001
@test Dates.yearwrap(2000,6,19) == 2002
@test Dates.yearwrap(2000,12,-3600) == 1700
@test Dates.yearwrap(2000,12,-36) == 1997
@test Dates.yearwrap(2000,12,-35) == 1998
@test Dates.yearwrap(2000,12,-24) == 1998
@test Dates.yearwrap(2000,12,-23) == 1999
@test Dates.yearwrap(2000,12,-14) == 1999
@test Dates.yearwrap(2000,12,-13) == 1999
@test Dates.yearwrap(2000,12,-12) == 1999
@test Dates.yearwrap(2000,12,-11) == 2000
@test Dates.yearwrap(2000,12,-2) == 2000
@test Dates.yearwrap(2000,12,-1) == 2000
@test Dates.yearwrap(2000,12,0) == 2000
@test Dates.yearwrap(2000,12,1) == 2001
@test Dates.yearwrap(2000,12,11) == 2001
@test Dates.yearwrap(2000,12,12) == 2001
@test Dates.yearwrap(2000,12,13) == 2002
@test Dates.yearwrap(2000,12,24) == 2002
@test Dates.yearwrap(2000,12,25) == 2003
@test Dates.yearwrap(2000,12,36) == 2003
@test Dates.yearwrap(2000,12,37) == 2004
@test Dates.yearwrap(2000,12,3600) == 2300

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Month(1) == Dates.DateTime(2000,1,27)
@test dt + Dates.Month(-1) == Dates.DateTime(1999,11,27)
@test dt + Dates.Month(-11) == Dates.DateTime(1999,1,27)
@test dt + Dates.Month(11) == Dates.DateTime(2000,11,27)
@test dt + Dates.Month(-12) == Dates.DateTime(1998,12,27)
@test dt + Dates.Month(12) == Dates.DateTime(2000,12,27)
@test dt + Dates.Month(13) == Dates.DateTime(2001,1,27)
@test dt + Dates.Month(100) == Dates.DateTime(2008,4,27)
@test dt + Dates.Month(1000) == Dates.DateTime(2083,4,27)
@test dt - Dates.Month(1) == Dates.DateTime(1999,11,27)
@test dt - Dates.Month(-1) == Dates.DateTime(2000,1,27)
@test dt - Dates.Month(100) == Dates.DateTime(1991,8,27)
@test dt - Dates.Month(1000) == Dates.DateTime(1916,8,27)
dt = Dates.DateTime(2000,2,29)
@test dt + Dates.Month(1) == Dates.DateTime(2000,3,29)
@test dt - Dates.Month(1) == Dates.DateTime(2000,1,29)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Month(1) == Dates.DateTime(1972,7,30,23,59,59)
@test dt - Dates.Month(1) == Dates.DateTime(1972,5,30,23,59,59)
@test dt + Dates.Month(-1) == Dates.DateTime(1972,5,30,23,59,59)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Week(1) == Dates.DateTime(2000,1,3)
@test dt + Dates.Week(100) == Dates.DateTime(2001,11,26)
@test dt + Dates.Week(1000) == Dates.DateTime(2019,2,25)
@test dt - Dates.Week(1) == Dates.DateTime(1999,12,20)
@test dt - Dates.Week(100) == Dates.DateTime(1998,1,26)
@test dt - Dates.Week(1000) == Dates.DateTime(1980,10,27)
dt = Dates.DateTime(2000,2,29)
@test dt + Dates.Week(1) == Dates.DateTime(2000,3,7)
@test dt - Dates.Week(1) == Dates.DateTime(2000,2,22)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Week(1) == Dates.DateTime(1972,7,7,23,59,59)
@test dt - Dates.Week(1) == Dates.DateTime(1972,6,23,23,59,59)
@test dt + Dates.Week(-1) == Dates.DateTime(1972,6,23,23,59,59)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Day(1) == Dates.DateTime(1999,12,28)
@test dt + Dates.Day(100) == Dates.DateTime(2000,4,5)
@test dt + Dates.Day(1000) == Dates.DateTime(2002,9,22)
@test dt - Dates.Day(1) == Dates.DateTime(1999,12,26)
@test dt - Dates.Day(100) == Dates.DateTime(1999,9,18)
@test dt - Dates.Day(1000) == Dates.DateTime(1997,4,1)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Day(1) == Dates.DateTime(1972,7,1,23,59,59)
@test dt - Dates.Day(1) == Dates.DateTime(1972,6,29,23,59,59)
@test dt + Dates.Day(-1) == Dates.DateTime(1972,6,29,23,59,59)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Hour(1) == Dates.DateTime(1999,12,27,1)
@test dt + Dates.Hour(100) == Dates.DateTime(1999,12,31,4)
@test dt + Dates.Hour(1000) == Dates.DateTime(2000,2,6,16)
@test dt - Dates.Hour(1) == Dates.DateTime(1999,12,26,23)
@test dt - Dates.Hour(100) == Dates.DateTime(1999,12,22,20)
@test dt - Dates.Hour(1000) == Dates.DateTime(1999,11,15,8)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Hour(1) == Dates.DateTime(1972,7,1,0,59,59)
@test dt - Dates.Hour(1) == Dates.DateTime(1972,6,30,22,59,59)
@test dt + Dates.Hour(-1) == Dates.DateTime(1972,6,30,22,59,59)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Minute(1) == Dates.DateTime(1999,12,27,0,1)
@test dt + Dates.Minute(100) == Dates.DateTime(1999,12,27,1,40)
@test dt + Dates.Minute(1000) == Dates.DateTime(1999,12,27,16,40)
@test dt - Dates.Minute(1) == Dates.DateTime(1999,12,26,23,59)
@test dt - Dates.Minute(100) == Dates.DateTime(1999,12,26,22,20)
@test dt - Dates.Minute(1000) == Dates.DateTime(1999,12,26,7,20)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Minute(1) == Dates.DateTime(1972,7,1,0,0,59)
@test dt - Dates.Minute(1) == Dates.DateTime(1972,6,30,23,58,59)
@test dt + Dates.Minute(-1) == Dates.DateTime(1972,6,30,23,58,59)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Second(1) == Dates.DateTime(1999,12,27,0,0,1)
@test dt + Dates.Second(100) == Dates.DateTime(1999,12,27,0,1,40)
@test dt + Dates.Second(1000) == Dates.DateTime(1999,12,27,0,16,40)
@test dt - Dates.Second(1) == Dates.DateTime(1999,12,26,23,59,59)
@test dt - Dates.Second(100) == Dates.DateTime(1999,12,26,23,58,20)
@test dt - Dates.Second(1000) == Dates.DateTime(1999,12,26,23,43,20)

dt = Dates.DateTime(1999,12,27)
@test dt + Dates.Millisecond(1) == Dates.DateTime(1999,12,27,0,0,0,1)
@test dt + Dates.Millisecond(100) == Dates.DateTime(1999,12,27,0,0,0,100)
@test dt + Dates.Millisecond(1000) == Dates.DateTime(1999,12,27,0,0,1)
@test dt - Dates.Millisecond(1) == Dates.DateTime(1999,12,26,23,59,59,999)
@test dt - Dates.Millisecond(100) == Dates.DateTime(1999,12,26,23,59,59,900)
@test dt - Dates.Millisecond(1000) == Dates.DateTime(1999,12,26,23,59,59)
dt = Dates.DateTime(1972,6,30,23,59,59)
@test dt + Dates.Millisecond(1) == Dates.DateTime(1972,6,30,23,59,59,1)
@test dt - Dates.Millisecond(1) == Dates.DateTime(1972,6,30,23,59,58,999)
@test dt + Dates.Millisecond(-1) == Dates.DateTime(1972,6,30,23,59,58,999)

dt = Dates.Date(1999,12,27)
@test dt + Dates.Year(1) == Dates.Date(2000,12,27)
@test dt + Dates.Year(100) == Dates.Date(2099,12,27)
@test dt + Dates.Year(1000) == Dates.Date(2999,12,27)
@test dt - Dates.Year(1) == Dates.Date(1998,12,27)
@test dt - Dates.Year(100) == Dates.Date(1899,12,27)
@test dt - Dates.Year(1000) == Dates.Date(999,12,27)
dt = Dates.Date(2000,2,29)
@test dt + Dates.Year(1) == Dates.Date(2001,2,28)
@test dt - Dates.Year(1) == Dates.Date(1999,2,28)
@test dt + Dates.Year(4) == Dates.Date(2004,2,29)
@test dt - Dates.Year(4) == Dates.Date(1996,2,29)

dt = Dates.Date(1999,12,27)
@test dt + Dates.Month(1) == Dates.Date(2000,1,27)
@test dt + Dates.Month(100) == Dates.Date(2008,4,27)
@test dt + Dates.Month(1000) == Dates.Date(2083,4,27)
@test dt - Dates.Month(1) == Dates.Date(1999,11,27)
@test dt - Dates.Month(100) == Dates.Date(1991,8,27)
@test dt - Dates.Month(1000) == Dates.Date(1916,8,27)
dt = Dates.Date(2000,2,29)
@test dt + Dates.Month(1) == Dates.Date(2000,3,29)
@test dt - Dates.Month(1) == Dates.Date(2000,1,29)

dt = Dates.Date(1999,12,27)
@test dt + Dates.Week(1) == Dates.Date(2000,1,3)
@test dt + Dates.Week(100) == Dates.Date(2001,11,26)
@test dt + Dates.Week(1000) == Dates.Date(2019,2,25)
@test dt - Dates.Week(1) == Dates.Date(1999,12,20)
@test dt - Dates.Week(100) == Dates.Date(1998,1,26)
@test dt - Dates.Week(1000) == Dates.Date(1980,10,27)
dt = Dates.Date(2000,2,29)
@test dt + Dates.Week(1) == Dates.Date(2000,3,7)
@test dt - Dates.Week(1) == Dates.Date(2000,2,22)

dt = Dates.Date(1999,12,27)
@test dt + Dates.Day(1) == Dates.Date(1999,12,28)
@test dt + Dates.Day(100) == Dates.Date(2000,4,5)
@test dt + Dates.Day(1000) == Dates.Date(2002,9,22)
@test dt - Dates.Day(1) == Dates.Date(1999,12,26)
@test dt - Dates.Day(100) == Dates.Date(1999,9,18)
@test dt - Dates.Day(1000) == Dates.Date(1997,4,1)

# Vectorized arithmetic
a = Dates.Date(2014,1,1)
dr = [a,a,a,a,a,a,a,a,a,a]
b = a + Dates.Year(1)
@test dr .+ Dates.Year(1) == repmat([b],10)
b = a + Dates.Month(1)
@test dr .+ Dates.Month(1) == repmat([b],10)
b = a + Dates.Day(1)
@test dr .+ Dates.Day(1) == repmat([b],10)
b = a - Dates.Year(1)
@test dr .- Dates.Year(1) == repmat([b],10)
b = a - Dates.Month(1)
@test dr .- Dates.Month(1) == repmat([b],10)
b = a - Dates.Day(1)
@test dr .- Dates.Day(1) == repmat([b],10)

# Vectorized arithmetic
b = a + Dates.Year(1)
@test dr .+ Dates.Year(1) == repmat([b],10)
b = a + Dates.Month(1)
@test dr .+ Dates.Month(1) == repmat([b],10)
b = a + Dates.Day(1)
@test dr .+ Dates.Day(1) == repmat([b],10)
b = a - Dates.Year(1)
@test dr .- Dates.Year(1) == repmat([b],10)
b = a - Dates.Month(1)
@test dr .- Dates.Month(1) == repmat([b],10)
b = a - Dates.Day(1)
@test dr .- Dates.Day(1) == repmat([b],10)

# Month arithmetic minimizes "edit distance", or number of changes
# needed to get a correct answer
# This approach results in a few cases of non-associativity
a = Dates.Date(2012,1,29)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,1,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,2,29)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,3,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,4,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,5,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,6,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,8,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,9,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,10,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)
a = Dates.Date(2012,11,30)
@test (a+Dates.Day(1))+Dates.Month(1) != (a+Dates.Month(1))+Dates.Day(1)


dt = Dates.DateTime(2000,1,1,12,30,45,500)
dt2 = dt + Dates.Year(1)
@test Dates.year(dt2) == 2001
@test Dates.month(dt2) == 1
@test Dates.day(dt2) == 1
@test Dates.hour(dt2) == 12
@test Dates.minute(dt2) == 30
@test Dates.second(dt2) == 45
@test Dates.millisecond(dt2) == 500

t1 = [Date(2009,1,1) Date(2009,1,2) Date(2009,1,3); Date(2009,2,1) Date(2009,2,2) Date(2009,2,3)]
t2 = [Date(2009,1,2) Date(2009,2,2) Date(2010,1,3); Date(2010,2,1) Date(2009,3,2) Date(2009,2,4)]
t3 = [DateTime(2009,1,1), DateTime(2009,1,2), DateTime(2009,1,3)]
t4 = [DateTime(2009,1,1,0,0,1), DateTime(2009,1,2,0,1), DateTime(2009,1,3,1)]

# TimeType, Array{TimeType}
@test Date(2010,1,1) .- t1 == [Day(365) Day(364) Day(363); Day(334) Day(333) Day(332)]
@test t1 .- Date(2010,1,1) == [Day(-365) Day(-364) Day(-363); Day(-334) Day(-333) Day(-332)]
@test DateTime(2009,1,1) .- t3 == [Millisecond(0), Millisecond(-86400000), Millisecond(-172800000)]
@test t3 .- DateTime(2009,1,1) == [Millisecond(0), Millisecond(86400000), Millisecond(172800000)]
@test Date(2010,1,1) - t1 == [Day(365) Day(364) Day(363); Day(334) Day(333) Day(332)]
@test t1 - Date(2010,1,1) == [Day(-365) Day(-364) Day(-363); Day(-334) Day(-333) Day(-332)]
@test DateTime(2009,1,1) - t3 == [Millisecond(0), Millisecond(-86400000), Millisecond(-172800000)]
@test t3 - DateTime(2009,1,1) == [Millisecond(0), Millisecond(86400000), Millisecond(172800000)]

# GeneralPeriod, Array{TimeType}
@test Day(1) .+ t1 == [Date(2009,1,2) Date(2009,1,3) Date(2009,1,4); Date(2009,2,2) Date(2009,2,3) Date(2009,2,4)]
@test Hour(1) .+ t3 == [DateTime(2009,1,1,1), DateTime(2009,1,2,1), DateTime(2009,1,3,1)]
@test t1 .+ Day(1) == [Date(2009,1,2) Date(2009,1,3) Date(2009,1,4); Date(2009,2,2) Date(2009,2,3) Date(2009,2,4)]
@test t3 .+ Hour(1) == [DateTime(2009,1,1,1), DateTime(2009,1,2,1), DateTime(2009,1,3,1)]
@test Day(1) + t1 == [Date(2009,1,2) Date(2009,1,3) Date(2009,1,4); Date(2009,2,2) Date(2009,2,3) Date(2009,2,4)]
@test Hour(1) + t3 == [DateTime(2009,1,1,1), DateTime(2009,1,2,1), DateTime(2009,1,3,1)]
@test t1 + Day(1) == [Date(2009,1,2) Date(2009,1,3) Date(2009,1,4); Date(2009,2,2) Date(2009,2,3) Date(2009,2,4)]
@test t3 + Hour(1) == [DateTime(2009,1,1,1), DateTime(2009,1,2,1), DateTime(2009,1,3,1)]

@test (Month(1) + Day(1)) .+ t1 == [Date(2009,2,2) Date(2009,2,3) Date(2009,2,4); Date(2009,3,2) Date(2009,3,3) Date(2009,3,4)]
@test (Hour(1) + Minute(1)) .+ t3 == [DateTime(2009,1,1,1,1), DateTime(2009,1,2,1,1), DateTime(2009,1,3,1,1)]
@test t1 .+ (Month(1) + Day(1)) == [Date(2009,2,2) Date(2009,2,3) Date(2009,2,4); Date(2009,3,2) Date(2009,3,3) Date(2009,3,4)]
@test t3 .+ (Hour(1) + Minute(1)) == [DateTime(2009,1,1,1,1), DateTime(2009,1,2,1,1), DateTime(2009,1,3,1,1)]
@test (Month(1) + Day(1)) + t1 == [Date(2009,2,2) Date(2009,2,3) Date(2009,2,4); Date(2009,3,2) Date(2009,3,3) Date(2009,3,4)]
@test (Hour(1) + Minute(1)) + t3 == [DateTime(2009,1,1,1,1), DateTime(2009,1,2,1,1), DateTime(2009,1,3,1,1)]
@test t1 + (Month(1) + Day(1)) == [Date(2009,2,2) Date(2009,2,3) Date(2009,2,4); Date(2009,3,2) Date(2009,3,3) Date(2009,3,4)]
@test t3 + (Hour(1) + Minute(1)) == [DateTime(2009,1,1,1,1), DateTime(2009,1,2,1,1), DateTime(2009,1,3,1,1)]

@test Day(1) .- t1 == [Date(2008,12,31) Date(2009,1,1) Date(2009,1,2); Date(2009,1,31) Date(2009,2,1) Date(2009,2,2)]
@test Hour(1) .- t3 == [DateTime(2008,12,31,23), DateTime(2009,1,1,23), DateTime(2009,1,2,23)]
@test t1 .- Day(1) == [Date(2008,12,31) Date(2009,1,1) Date(2009,1,2); Date(2009,1,31) Date(2009,2,1) Date(2009,2,2)]
@test t3 .- Hour(1) == [DateTime(2008,12,31,23), DateTime(2009,1,1,23), DateTime(2009,1,2,23)]
@test Day(1) - t1 == [Date(2008,12,31) Date(2009,1,1) Date(2009,1,2); Date(2009,1,31) Date(2009,2,1) Date(2009,2,2)]
@test Hour(1) - t3 == [DateTime(2008,12,31,23), DateTime(2009,1,1,23), DateTime(2009,1,2,23)]
@test t1 - Day(1) == [Date(2008,12,31) Date(2009,1,1) Date(2009,1,2); Date(2009,1,31) Date(2009,2,1) Date(2009,2,2)]
@test t3 - Hour(1) == [DateTime(2008,12,31,23), DateTime(2009,1,1,23), DateTime(2009,1,2,23)]

@test (Month(1) + Day(1)) .- t1 == [Date(2008,11,30) Date(2008,12,1) Date(2008,12,2); Date(2008,12,31) Date(2009,1,1) Date(2009,1,2)]
@test (Hour(1) + Minute(1)) .- t3 == [DateTime(2008,12,31,22,59), DateTime(2009,1,1,22,59), DateTime(2009,1,2,22,59)]
@test t1 .- (Month(1) + Day(1)) == [Date(2008,11,30) Date(2008,12,1) Date(2008,12,2); Date(2008,12,31) Date(2009,1,1) Date(2009,1,2)]
@test t3 .- (Hour(1) + Minute(1)) == [DateTime(2008,12,31,22,59), DateTime(2009,1,1,22,59), DateTime(2009,1,2,22,59)]
@test (Month(1) + Day(1)) - t1 == [Date(2008,11,30) Date(2008,12,1) Date(2008,12,2); Date(2008,12,31) Date(2009,1,1) Date(2009,1,2)]
@test (Hour(1) + Minute(1)) - t3 == [DateTime(2008,12,31,22,59), DateTime(2009,1,1,22,59), DateTime(2009,1,2,22,59)]
@test t1 - (Month(1) + Day(1)) == [Date(2008,11,30) Date(2008,12,1) Date(2008,12,2); Date(2008,12,31) Date(2009,1,1) Date(2009,1,2)]
@test t3 - (Hour(1) + Minute(1)) == [DateTime(2008,12,31,22,59), DateTime(2009,1,1,22,59), DateTime(2009,1,2,22,59)]

# Array{TimeType}, Array{TimeType}
@test t2 - t1 == [Day(1) Day(31) Day(365); Day(365) Day(28) Day(1)]
@test t4 - t3 == [Millisecond(1000), Millisecond(60000), Millisecond(3600000)]
@test (Date(2009,1,1):Week(1):Date(2009,1,21)) - (Date(2009,1,1):Day(1):Date(2009,1,3)) == [0d, 6d, 12d]
@test (DateTime(2009,1,1,1,1,1):Second(1):DateTime(2009,1,1,1,1,3)) - (DateTime(2009,1,1,1,1):Second(1):DateTime(2009,1,1,1,1,2)) == [1s, 1s, 1s]


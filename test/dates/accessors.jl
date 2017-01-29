# This file is a part of Julia. License is MIT: http://julialang.org/license

# yearmonthday is the opposite of totaldays
# taking Rata Die Day # and returning proleptic Gregorian date
@test Dates.yearmonthday(-306) == (0, 2, 29)
@test Dates.yearmonth(-306) == (0, 2)
@test Dates.monthday(-306) == (2, 29)
@test Dates.yearmonthday(-305) == (0, 3, 1)
@test Dates.yearmonth(-305) == (0, 3)
@test Dates.monthday(-305) == (3, 1)
@test Dates.yearmonthday(-2) == (0, 12, 29)
@test Dates.yearmonth(-2) == (0, 12)
@test Dates.monthday(-2) == (12, 29)
@test Dates.yearmonthday(-1) == (0, 12, 30)
@test Dates.yearmonth(-1) == (0, 12)
@test Dates.monthday(-1) == (12, 30)
@test Dates.yearmonthday(0) == (0, 12, 31)
@test Dates.yearmonth(-0) == (0, 12)
@test Dates.monthday(-0) == (12, 31)
@test Dates.yearmonthday(1) == (1, 1, 1)
@test Dates.yearmonth(1) == (1, 1)
@test Dates.monthday(1) == (1, 1)
# year, month, and day return the indivial components
# of yearmonthday, avoiding additional calculations when possible
@test Dates.year(-1) == 0
@test Dates.month(-1) == 12
@test Dates.day(-1) == 30
@test Dates.year(0) == 0
@test Dates.month(0) == 12
@test Dates.day(0) == 31
@test Dates.year(1) == 1
@test Dates.month(1) == 1
@test Dates.day(1) == 1
@test Dates.yearmonthday(730120) == (2000, 1, 1)
@test Dates.year(730120) == 2000
@test Dates.month(730120) == 1
@test Dates.day(730120) == 1

# Test totaldays and yearmonthday from January 1st of "from" to December 31st of "to"
# test_dates(-10000, 10000) takes about 15 seconds
# test_dates(year(typemin(Date)), year(typemax(Date))) is full range
# and would take.......a really long time
let from=0, to=2100, y=0, m=0, d=0
    test_day = Dates.totaldays(from, 1, 1)
    for y in from:to
        for m = 1:12
            for d = 1:Dates.daysinmonth(y, m)
                days = Dates.totaldays(y, m, d)
                @test days == test_day
                @test (y, m, d) == Dates.yearmonthday(days)
                test_day += 1
            end
        end
    end
end

# Test year, month, day, hour, minute
let y=0, m=0, d=0, h=0, mi=0
    for m = 1:12
        for d = 1:Dates.daysinmonth(y, m)
            for h = 0:23
                for mi = 0:59
                    dt = Dates.DateTime(y, m, d, h, mi)
                    @test y == Dates.year(dt)
                    @test m == Dates.month(dt)
                    @test d == Dates.day(dt)
                    @test d == Dates.dayofmonth(dt)
                    @test h == Dates.hour(dt)
                    @test mi == Dates.minute(dt)
                    @test (m, d) == Dates.monthday(dt)
                end
            end
        end
    end
end

# Test second, millisecond
let y=0, m=0, d=0, h=0, mi=0, s=0, ms=0
    for y in [-2013, -1, 0, 1, 2013]
        for m in [1, 6, 12]
            for d in [1, 15, Dates.daysinmonth(y, m)]
                for h in [0, 12, 23]
                    for s = 0:59
                        for ms in [0, 1, 500, 999]
                            dt = Dates.DateTime(y, m, d, h, mi, s, ms)
                            @test y == Dates.year(dt)
                            @test m == Dates.month(dt)
                            @test d == Dates.day(dt)
                            @test h == Dates.hour(dt)
                            @test s == Dates.second(dt)
                            @test ms == Dates.millisecond(dt)
                        end
                    end
                end
            end
        end
    end
end

let from=0, to=2100, y=0, m=0, d=0
    for y in from:to
        for m = 1:12
            for d = 1:Dates.daysinmonth(y, m)
                dt = Dates.Date(y, m, d)
                @test y == Dates.year(dt)
                @test m == Dates.month(dt)
                @test d == Dates.day(dt)
            end
        end
    end
end

# test hour, minute, second
let h=0, mi=0, s=0, ms=0, us=0, ns=0
    for h = (0, 23), mi = (0, 59), s = (0, 59),
        ms in (0, 1, 500, 999), us in (0, 1, 500, 999), ns in (0, 1, 500, 999)
        t = Dates.Time(h, mi, s, ms, us, ns)
        @test h == Dates.hour(t)
        @test mi == Dates.minute(t)
        @test s == Dates.second(t)
        @test ms == Dates.millisecond(t)
        @test us == Dates.microsecond(t)
        @test ns == Dates.nanosecond(t)
    end
end

# week function
# Tests from https://en.wikipedia.org/wiki/ISO_week_date
@test Dates.week(Dates.Date(2005, 1, 1)) == 53
@test Dates.week(Dates.Date(2005, 1, 2)) == 53
@test Dates.week(Dates.Date(2005, 12, 31)) == 52
@test Dates.week(Dates.Date(2007, 1, 1)) == 1
@test Dates.week(Dates.Date(2007, 12, 30)) == 52
@test Dates.week(Dates.Date(2007, 12, 31)) == 1
@test Dates.week(Dates.Date(2008, 1, 1)) == 1
@test Dates.week(Dates.Date(2008, 12, 28)) == 52
@test Dates.week(Dates.Date(2008, 12, 29)) == 1
@test Dates.week(Dates.Date(2008, 12, 30)) == 1
@test Dates.week(Dates.Date(2008, 12, 31)) == 1
@test Dates.week(Dates.Date(2009, 1, 1)) == 1
@test Dates.week(Dates.Date(2009, 12, 31)) == 53
@test Dates.week(Dates.Date(2010, 1, 1)) == 53
@test Dates.week(Dates.Date(2010, 1, 2)) == 53
@test Dates.week(Dates.Date(2010, 1, 2)) == 53
# Tests from http://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=1999
dt = Dates.DateTime(1999, 12, 27)
dt1 = Dates.Date(1999, 12, 27)
check = (52, 52, 52, 52, 52, 52, 52, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2)
for i = 1:21
    @test Dates.week(dt) == check[i]
    @test Dates.week(dt1) == check[i]
    dt = dt + Dates.Day(1)
    dt1 = dt1 + Dates.Day(1)
end
# Tests from http://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2000
dt = Dates.DateTime(2000, 12, 25)
dt1 = Dates.Date(2000, 12, 25)
for i = 1:21
    @test Dates.week(dt) == check[i]
    @test Dates.week(dt1) == check[i]
    dt = dt + Dates.Day(1)
    dt1 = dt1 + Dates.Day(1)
end
# Test from http://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2030
dt = Dates.DateTime(2030, 12, 23)
dt1 = Dates.Date(2030, 12, 23)
for i = 1:21
    @test Dates.week(dt) == check[i]
    @test Dates.week(dt1) == check[i]
    dt = dt + Dates.Day(1)
    dt1 = dt1 + Dates.Day(1)
end
# Tests from http://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2004
dt = Dates.DateTime(2004, 12, 20)
dt1 = Dates.Date(2004, 12, 20)
check = (52, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 1, 1, 1, 1, 1, 1, 1)
for i = 1:21
    @test Dates.week(dt) == check[i]
    @test Dates.week(dt1) == check[i]
    dt = dt + Dates.Day(1)
    dt1 = dt1 + Dates.Day(1)
end

# Vectorized accessors
a = Dates.Date(2014, 1, 1)
dr = [a, a, a, a, a, a, a, a, a, a]
@test Dates.year.(dr) == repmat([2014], 10)
@test Dates.month.(dr) == repmat([1], 10)
@test Dates.day.(dr) == repmat([1], 10)

a = Dates.DateTime(2014, 1, 1)
dr = [a, a, a, a, a, a, a, a, a, a]
@test Dates.year.(dr) == repmat([2014], 10)
@test Dates.month.(dr) == repmat([1], 10)
@test Dates.day.(dr) == repmat([1], 10)
@test Dates.hour.(dr) == repmat([0], 10)
@test Dates.minute.(dr) == repmat([0], 10)
@test Dates.second.(dr) == repmat([0], 10)
@test Dates.millisecond.(dr) == repmat([0], 10)

b = Dates.Time(1, 2, 3, 4, 5, 6)
tr = [b, b, b, b, b, b, b, b, b, b]
@test Dates.hour.(tr) == repmat([1], 10)
@test Dates.minute.(tr) == repmat([2], 10)
@test Dates.second.(tr) == repmat([3], 10)
@test Dates.millisecond.(tr) == repmat([4], 10)
@test Dates.microsecond.(tr) == repmat([5], 10)
@test Dates.nanosecond.(tr) == repmat([6], 10)
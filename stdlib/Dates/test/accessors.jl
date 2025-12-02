# This file is a part of Julia. License is MIT: https://julialang.org/license

module AccessorsTest

using Dates
using Test

@testset "yearmonthday/yearmonth/monthday" begin
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
    @test Dates.yearmonthday(730120) == (2000, 1, 1)
end
@testset "year/month/day" begin
    # year, month, and day return the individual components
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
    @test Dates.year(730120) == 2000
    @test Dates.month(730120) == 1
    @test Dates.day(730120) == 1
end
@testset "totaldays/yearmonthday over many years" begin
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
end

@testset "year, month, day, hour, minute" begin
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
end
@testset "second, millisecond" begin
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
end
@testset "year, month, day, hour, minute, second over many years" begin
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
end
@testset "week" begin
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
    # Tests from https://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=1999
    dt = Dates.DateTime(1999, 12, 27)
    dt1 = Dates.Date(1999, 12, 27)
    check = (52, 52, 52, 52, 52, 52, 52, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2)
    for i = 1:21
        @test Dates.week(dt) == check[i]
        @test Dates.week(dt1) == check[i]
        dt = dt + Dates.Day(1)
        dt1 = dt1 + Dates.Day(1)
    end
    # Tests from https://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2000
    dt = Dates.DateTime(2000, 12, 25)
    dt1 = Dates.Date(2000, 12, 25)
    for i = 1:21
        @test Dates.week(dt) == check[i]
        @test Dates.week(dt1) == check[i]
        dt = dt + Dates.Day(1)
        dt1 = dt1 + Dates.Day(1)
    end
    # Test from https://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2030
    dt = Dates.DateTime(2030, 12, 23)
    dt1 = Dates.Date(2030, 12, 23)
    for i = 1:21
        @test Dates.week(dt) == check[i]
        @test Dates.week(dt1) == check[i]
        dt = dt + Dates.Day(1)
        dt1 = dt1 + Dates.Day(1)
    end
    # Tests from https://www.epochconverter.com/date-and-time/weeknumbers-by-year.php?year=2004
    dt = Dates.DateTime(2004, 12, 20)
    dt1 = Dates.Date(2004, 12, 20)
    check = (52, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 1, 1, 1, 1, 1, 1, 1)
    for i = 1:21
        @test Dates.week(dt) == check[i]
        @test Dates.week(dt1) == check[i]
        dt = dt + Dates.Day(1)
        dt1 = dt1 + Dates.Day(1)
    end
end
@testset "ISO year utils" begin
    # Tests from https://www.epochconverter.com/weeks
    @test Dates.weeksinyear(Dates.Year(2023)) == 52
    @test Dates.weeksinyear(Dates.Year(2022)) == 52
    @test Dates.weeksinyear(Dates.Year(2021)) == 52
    @test Dates.weeksinyear(Dates.Year(2020)) == 53
    @test Dates.weeksinyear(Dates.Year(2019)) == 52
    @test Dates.weeksinyear(Dates.Year(2018)) == 52
    @test Dates.weeksinyear(Dates.Year(2017)) == 52
    @test Dates.weeksinyear(Dates.Year(2016)) == 52
    @test Dates.weeksinyear(Dates.Year(2015)) == 53
    @test Dates.weeksinyear(Dates.Year(2014)) == 52
    @test Dates.weeksinyear(Dates.Year(2013)) == 52
    @test Dates.weeksinyear(Dates.Year(2012)) == 52
    @test Dates.weeksinyear(Dates.Year(2011)) == 52
    @test Dates.weeksinyear(Dates.Year(2010)) == 52
    @test Dates.weeksinyear(Dates.Year(2009)) == 53

    # From python datetime isocalendar
    @test Dates.isoweekdate(Dates.Date(2023, 03, 06)) == (2023, 10, 1)
    @test Dates.isoweekdate(Dates.Date(2023, 03, 07)) == (2023, 10, 2)
    @test Dates.isoweekdate(Dates.Date(2023, 03, 08)) == (2023, 10, 3)
    @test Dates.isoweekdate(Dates.Date(2022, 12, 29)) == (2022, 52, 4)
    @test Dates.isoweekdate(Dates.Date(2022, 12, 30)) == (2022, 52, 5)
    @test Dates.isoweekdate(Dates.Date(2022, 12, 31)) == (2022, 52, 6)
    @test Dates.isoweekdate(Dates.Date(2023, 01, 01)) == (2022, 52, 7)
    @test Dates.isoweekdate(Dates.Date(2023, 01, 02)) == (2023, 1, 1)
    @test Dates.isoweekdate(Dates.Date(2023, 01, 03)) == (2023, 1, 2)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 28)) == (2021, 52, 2)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 29)) == (2021, 52, 3)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 30)) == (2021, 52, 4)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 31)) == (2021, 52, 5)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 01)) == (2021, 52, 6)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 02)) == (2021, 52, 7)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 03)) == (2022, 1, 1)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 04)) == (2022, 1, 2)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 05)) == (2022, 1, 3)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 06)) == (2022, 1, 4)
    @test Dates.isoweekdate(Dates.Date(2020, 12, 29)) == (2020, 53, 2)
    @test Dates.isoweekdate(Dates.Date(2020, 12, 30)) == (2020, 53, 3)
    @test Dates.isoweekdate(Dates.Date(2020, 12, 31)) == (2020, 53, 4)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 01)) == (2020, 53, 5)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 02)) == (2020, 53, 6)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 03)) == (2020, 53, 7)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 04)) == (2021, 1, 1)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 05)) == (2021, 1, 2)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 31)) == (2021, 52, 5)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 01)) == (2021, 52, 6)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 02)) == (2021, 52, 7)
    @test Dates.isoweekdate(Dates.Date(2020, 12, 31)) == (2020, 53, 4)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 01)) == (2020, 53, 5)
    @test Dates.isoweekdate(Dates.Date(2021, 01, 02)) == (2020, 53, 6)
    @test Dates.isoweekdate(Dates.Date(2021, 12, 31)) == (2021, 52, 5)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 01)) == (2021, 52, 6)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 02)) == (2021, 52, 7)
    @test Dates.isoweekdate(Dates.Date(2022, 01, 03)) == (2022, 1, 1)
    @test Dates.isoweekdate(Dates.Date(2019, 12, 31)) == (2020, 1, 2)
    @test Dates.isoweekdate(Dates.Date(2020, 01, 01)) == (2020, 1, 3)
    @test Dates.isoweekdate(Dates.Date(2020, 01, 02)) == (2020, 1, 4)
    @test Dates.isoweekdate(Dates.Date(2018, 12, 31)) == (2019, 1, 1)
    @test Dates.isoweekdate(Dates.Date(2019, 01, 01)) == (2019, 1, 2)
    @test Dates.isoweekdate(Dates.Date(2019, 01, 02)) == (2019, 1, 3)
end
@testset "Vectorized accessors" begin
    a = Dates.Date(2014, 1, 1)
    dr = [a, a, a, a, a, a, a, a, a, a]
    @test Dates.year.(dr) == repeat([2014], 10)
    @test Dates.month.(dr) == repeat([1], 10)
    @test Dates.day.(dr) == repeat([1], 10)

    a = Dates.DateTime(2014, 1, 1)
    dr = [a, a, a, a, a, a, a, a, a, a]
    @test Dates.year.(dr) == repeat([2014], 10)
    @test Dates.month.(dr) == repeat([1], 10)
    @test Dates.day.(dr) == repeat([1], 10)
    @test Dates.hour.(dr) == repeat([0], 10)
    @test Dates.minute.(dr) == repeat([0], 10)
    @test Dates.second.(dr) == repeat([0], 10)
    @test Dates.millisecond.(dr) == repeat([0], 10)

    b = Dates.Time(1, 2, 3, 4, 5, 6)
    tr = [b, b, b, b, b, b, b, b, b, b]
    @test Dates.hour.(tr) == repeat([1], 10)
    @test Dates.minute.(tr) == repeat([2], 10)
    @test Dates.second.(tr) == repeat([3], 10)
    @test Dates.millisecond.(tr) == repeat([4], 10)
    @test Dates.microsecond.(tr) == repeat([5], 10)
    @test Dates.nanosecond.(tr) == repeat([6], 10)
end

end

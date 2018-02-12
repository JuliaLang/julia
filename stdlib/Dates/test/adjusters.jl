# This file is a part of Julia. License is MIT: https://julialang.org/license

module AdjustersTest

using Test
using Dates

@testset "trunc" begin
    dt = Dates.Date(2012, 12, 21)
    @test trunc(dt, Dates.Year) == Dates.Date(2012)
    @test trunc(dt, Dates.Month) == Dates.Date(2012, 12)
    @test trunc(dt, Dates.Day) == Dates.Date(2012, 12, 21)
    dt = Dates.DateTime(2012, 12, 21, 16, 30, 20, 200)
    @test trunc(dt, Dates.Year) == Dates.DateTime(2012)
    @test trunc(dt, Dates.Month) == Dates.DateTime(2012, 12)
    @test trunc(dt, Dates.Day) == Dates.DateTime(2012, 12, 21)
    @test trunc(dt, Dates.Hour) == Dates.DateTime(2012, 12, 21, 16)
    @test trunc(dt, Dates.Minute) == Dates.DateTime(2012, 12, 21, 16, 30)
    @test trunc(dt, Dates.Second) == Dates.DateTime(2012, 12, 21, 16, 30, 20)
    @test trunc(dt, Dates.Millisecond) == Dates.DateTime(2012, 12, 21, 16, 30, 20, 200)
    t = Dates.Time(1, 2, 3, 4, 5, 6)
    @test trunc(t, Dates.Hour) == Dates.Time(1)
    @test trunc(t, Dates.Minute) == Dates.Time(1, 2)
    @test trunc(t, Dates.Second) == Dates.Time(1, 2, 3)
    @test trunc(t, Dates.Millisecond) == Dates.Time(1, 2, 3, 4)
    @test trunc(t, Dates.Microsecond) == Dates.Time(1, 2, 3, 4, 5)
    @test trunc(t, Dates.Nanosecond) == Dates.Time(1, 2, 3, 4, 5, 6)
end

Jan = Dates.DateTime(2013, 1, 1) #Tuesday
Feb = Dates.DateTime(2013, 2, 2) #Saturday
Mar = Dates.DateTime(2013, 3, 3) #Sunday
Apr = Dates.DateTime(2013, 4, 4) #Thursday
May = Dates.DateTime(2013, 5, 5) #Sunday
Jun = Dates.DateTime(2013, 6, 7) #Friday
Jul = Dates.DateTime(2013, 7, 7) #Sunday
Aug = Dates.DateTime(2013, 8, 8) #Thursday
Sep = Dates.DateTime(2013, 9, 9) #Monday
Oct = Dates.DateTime(2013, 10, 10) #Thursday
Nov = Dates.DateTime(2013, 11, 11) #Monday
Dec = Dates.DateTime(2013, 12, 11) #Wednesday

@testset "Date functions" begin
    @testset "lastdayofmonth" begin
        @test Dates.lastdayofmonth(Jan) == Dates.DateTime(2013, 1, 31)
        @test Dates.lastdayofmonth(Feb) == Dates.DateTime(2013, 2, 28)
        @test Dates.lastdayofmonth(Mar) == Dates.DateTime(2013, 3, 31)
        @test Dates.lastdayofmonth(Apr) == Dates.DateTime(2013, 4, 30)
        @test Dates.lastdayofmonth(May) == Dates.DateTime(2013, 5, 31)
        @test Dates.lastdayofmonth(Jun) == Dates.DateTime(2013, 6, 30)
        @test Dates.lastdayofmonth(Jul) == Dates.DateTime(2013, 7, 31)
        @test Dates.lastdayofmonth(Aug) == Dates.DateTime(2013, 8, 31)
        @test Dates.lastdayofmonth(Sep) == Dates.DateTime(2013, 9, 30)
        @test Dates.lastdayofmonth(Oct) == Dates.DateTime(2013, 10, 31)
        @test Dates.lastdayofmonth(Nov) == Dates.DateTime(2013, 11, 30)
        @test Dates.lastdayofmonth(Dec) == Dates.DateTime(2013, 12, 31)

        @test Dates.lastdayofmonth(Date(Jan)) == Dates.Date(2013, 1, 31)
        @test Dates.lastdayofmonth(Date(Feb)) == Dates.Date(2013, 2, 28)
        @test Dates.lastdayofmonth(Date(Mar)) == Dates.Date(2013, 3, 31)
        @test Dates.lastdayofmonth(Date(Apr)) == Dates.Date(2013, 4, 30)
        @test Dates.lastdayofmonth(Date(May)) == Dates.Date(2013, 5, 31)
        @test Dates.lastdayofmonth(Date(Jun)) == Dates.Date(2013, 6, 30)
        @test Dates.lastdayofmonth(Date(Jul)) == Dates.Date(2013, 7, 31)
        @test Dates.lastdayofmonth(Date(Aug)) == Dates.Date(2013, 8, 31)
        @test Dates.lastdayofmonth(Date(Sep)) == Dates.Date(2013, 9, 30)
        @test Dates.lastdayofmonth(Date(Oct)) == Dates.Date(2013, 10, 31)
        @test Dates.lastdayofmonth(Date(Nov)) == Dates.Date(2013, 11, 30)
        @test Dates.lastdayofmonth(Date(Dec)) == Dates.Date(2013, 12, 31)
    end
    @testset "firstdayofmonth" begin
        @test Dates.firstdayofmonth(Jan) == Dates.DateTime(2013, 1, 1)
        @test Dates.firstdayofmonth(Feb) == Dates.DateTime(2013, 2, 1)
        @test Dates.firstdayofmonth(Mar) == Dates.DateTime(2013, 3, 1)
        @test Dates.firstdayofmonth(Apr) == Dates.DateTime(2013, 4, 1)
        @test Dates.firstdayofmonth(May) == Dates.DateTime(2013, 5, 1)
        @test Dates.firstdayofmonth(Jun) == Dates.DateTime(2013, 6, 1)
        @test Dates.firstdayofmonth(Jul) == Dates.DateTime(2013, 7, 1)
        @test Dates.firstdayofmonth(Aug) == Dates.DateTime(2013, 8, 1)
        @test Dates.firstdayofmonth(Sep) == Dates.DateTime(2013, 9, 1)
        @test Dates.firstdayofmonth(Oct) == Dates.DateTime(2013, 10, 1)
        @test Dates.firstdayofmonth(Nov) == Dates.DateTime(2013, 11, 1)
        @test Dates.firstdayofmonth(Dec) == Dates.DateTime(2013, 12, 1)

        @test Dates.firstdayofmonth(Date(Jan)) == Dates.Date(2013, 1, 1)
        @test Dates.firstdayofmonth(Date(Feb)) == Dates.Date(2013, 2, 1)
        @test Dates.firstdayofmonth(Date(Mar)) == Dates.Date(2013, 3, 1)
        @test Dates.firstdayofmonth(Date(Apr)) == Dates.Date(2013, 4, 1)
        @test Dates.firstdayofmonth(Date(May)) == Dates.Date(2013, 5, 1)
        @test Dates.firstdayofmonth(Date(Jun)) == Dates.Date(2013, 6, 1)
        @test Dates.firstdayofmonth(Date(Jul)) == Dates.Date(2013, 7, 1)
        @test Dates.firstdayofmonth(Date(Aug)) == Dates.Date(2013, 8, 1)
        @test Dates.firstdayofmonth(Date(Sep)) == Dates.Date(2013, 9, 1)
        @test Dates.firstdayofmonth(Date(Oct)) == Dates.Date(2013, 10, 1)
        @test Dates.firstdayofmonth(Date(Nov)) == Dates.Date(2013, 11, 1)
        @test Dates.firstdayofmonth(Date(Dec)) == Dates.Date(2013, 12, 1)
    end
    @testset "firstdayofweek" begin
        # 2014-01-06 is a Monday = 1st day of week
        a = Dates.Date(2014, 1, 6)
        b = Dates.Date(2014, 1, 7)
        c = Dates.Date(2014, 1, 8)
        d = Dates.Date(2014, 1, 9)
        e = Dates.Date(2014, 1, 10)
        f = Dates.Date(2014, 1, 11)
        g = Dates.Date(2014, 1, 12)
        @test Dates.firstdayofweek(a) == a
        @test Dates.firstdayofweek(b) == a
        @test Dates.firstdayofweek(c) == a
        @test Dates.firstdayofweek(d) == a
        @test Dates.firstdayofweek(e) == a
        @test Dates.firstdayofweek(f) == a
        @test Dates.firstdayofweek(g) == a
        # Test firstdayofweek over the course of the year
        dt = a
        for i = 0:364
            @test Dates.firstdayofweek(dt) == a + Dates.Week(div(i, 7))
            dt += Dates.Day(1)
        end
        a = Dates.DateTime(2014, 1, 6)
        b = Dates.DateTime(2014, 1, 7)
        c = Dates.DateTime(2014, 1, 8)
        d = Dates.DateTime(2014, 1, 9)
        e = Dates.DateTime(2014, 1, 10)
        f = Dates.DateTime(2014, 1, 11)
        g = Dates.DateTime(2014, 1, 12)
        @test Dates.firstdayofweek(a) == a
        @test Dates.firstdayofweek(b) == a
        @test Dates.firstdayofweek(c) == a
        @test Dates.firstdayofweek(d) == a
        @test Dates.firstdayofweek(e) == a
        @test Dates.firstdayofweek(f) == a
        @test Dates.firstdayofweek(g) == a
        dt = a
        for i = 0:364
            @test Dates.firstdayofweek(dt) == a + Dates.Week(div(i, 7))
            dt += Dates.Day(1)
        end
        @test Dates.firstdayofweek(Dates.DateTime(2013, 12, 24)) == Dates.DateTime(2013, 12, 23)
    end
    @testset "lastdayofweek" begin
        # Sunday = last day of week
        # 2014-01-12 is a Sunday
        a = Dates.Date(2014, 1, 6)
        b = Dates.Date(2014, 1, 7)
        c = Dates.Date(2014, 1, 8)
        d = Dates.Date(2014, 1, 9)
        e = Dates.Date(2014, 1, 10)
        f = Dates.Date(2014, 1, 11)
        g = Dates.Date(2014, 1, 12)
        @test Dates.lastdayofweek(a) == g
        @test Dates.lastdayofweek(b) == g
        @test Dates.lastdayofweek(c) == g
        @test Dates.lastdayofweek(d) == g
        @test Dates.lastdayofweek(e) == g
        @test Dates.lastdayofweek(f) == g
        @test Dates.lastdayofweek(g) == g
        dt = a
        for i = 0:364
            @test Dates.lastdayofweek(dt) == g + Dates.Week(div(i, 7))
            dt += Dates.Day(1)
        end
        a = Dates.DateTime(2014, 1, 6)
        b = Dates.DateTime(2014, 1, 7)
        c = Dates.DateTime(2014, 1, 8)
        d = Dates.DateTime(2014, 1, 9)
        e = Dates.DateTime(2014, 1, 10)
        f = Dates.DateTime(2014, 1, 11)
        g = Dates.DateTime(2014, 1, 12)
        @test Dates.lastdayofweek(a) == g
        @test Dates.lastdayofweek(b) == g
        @test Dates.lastdayofweek(c) == g
        @test Dates.lastdayofweek(d) == g
        @test Dates.lastdayofweek(e) == g
        @test Dates.lastdayofweek(f) == g
        @test Dates.lastdayofweek(g) == g
        dt = a
        for i = 0:364
            @test Dates.lastdayofweek(dt) == g + Dates.Week(div(i, 7))
            dt += Dates.Day(1)
        end
        @test Dates.lastdayofweek(Dates.DateTime(2013, 12, 24)) == Dates.DateTime(2013, 12, 29)
    end
    @testset "first/lastdayofquarter" begin
        @test Dates.firstdayofquarter(Dates.Date(2014, 2, 2)) == Dates.Date(2014, 1, 1)
        @test Dates.firstdayofquarter(Dates.Date(2014, 5, 2)) == Dates.Date(2014, 4, 1)
        @test Dates.firstdayofquarter(Dates.Date(2014, 8, 2)) == Dates.Date(2014, 7, 1)
        @test Dates.firstdayofquarter(Dates.Date(2014, 12, 2)) == Dates.Date(2014, 10, 1)

        @test Dates.firstdayofquarter(Dates.DateTime(2014, 2, 2)) == Dates.DateTime(2014, 1, 1)
        @test Dates.firstdayofquarter(Dates.DateTime(2014, 5, 2)) == Dates.DateTime(2014, 4, 1)
        @test Dates.firstdayofquarter(Dates.DateTime(2014, 8, 2)) == Dates.DateTime(2014, 7, 1)
        @test Dates.firstdayofquarter(Dates.DateTime(2014, 12, 2)) == Dates.DateTime(2014, 10, 1)

        @test Dates.lastdayofquarter(Dates.Date(2014, 2, 2)) == Dates.Date(2014, 3, 31)
        @test Dates.lastdayofquarter(Dates.Date(2014, 5, 2)) == Dates.Date(2014, 6, 30)
        @test Dates.lastdayofquarter(Dates.Date(2014, 8, 2)) == Dates.Date(2014, 9, 30)
        @test Dates.lastdayofquarter(Dates.Date(2014, 12, 2)) == Dates.Date(2014, 12, 31)

        @test Dates.lastdayofquarter(Dates.DateTime(2014, 2, 2)) == Dates.DateTime(2014, 3, 31)
        @test Dates.lastdayofquarter(Dates.DateTime(2014, 5, 2)) == Dates.DateTime(2014, 6, 30)
        @test Dates.lastdayofquarter(Dates.DateTime(2014, 8, 2)) == Dates.DateTime(2014, 9, 30)
        @test Dates.lastdayofquarter(Dates.DateTime(2014, 12, 2)) == Dates.DateTime(2014, 12, 31)
    end
    @testset "first/lastdayofyear" begin
        firstday = Dates.Date(2014, 1, 1)
        lastday = Dates.Date(2014, 12, 31)
        for i = 0:364
            dt = firstday + Dates.Day(i)

            @test Dates.firstdayofyear(dt) == firstday
            @test Dates.firstdayofyear(DateTime(dt)) == DateTime(firstday)

            @test Dates.lastdayofyear(dt) == lastday
            @test Dates.lastdayofyear(DateTime(dt)) == DateTime(lastday)
        end
    end
end
# Adjusters
@testset "Adjuster Constructors" begin
    @test Dates.Date(Dates.ismonday, 2014) == Dates.Date(2014, 1, 6)
    @test Dates.Date(Dates.ismonday, 2014, 5) == Dates.Date(2014, 5, 5)

    @test Dates.DateTime(Dates.ismonday, 2014) == Dates.DateTime(2014, 1, 6)
    @test Dates.DateTime(Dates.ismonday, 2014, 5) == Dates.DateTime(2014, 5, 5)
    @test Dates.DateTime(x->Dates.hour(x)==12, 2014, 5, 21) == Dates.DateTime(2014, 5, 21, 12)
    @test Dates.DateTime(x->Dates.minute(x)==30, 2014, 5, 21, 12) == Dates.DateTime(2014, 5, 21, 12, 30)
    @test Dates.DateTime(x->Dates.second(x)==30, 2014, 5, 21, 12, 30) == Dates.DateTime(2014, 5, 21, 12, 30, 30)
    @test Dates.DateTime(x->Dates.millisecond(x)==500, 2014, 5, 21, 12, 30, 30) == Dates.DateTime(2014, 5, 21, 12, 30, 30, 500)
end
@testset "tonext, toprev, tofirst, tolast" begin
    dt = Dates.Date(2014, 5, 21)
    @test Dates.tonext(dt, Dates.Wed) == Dates.Date(2014, 5, 28)
    @test Dates.tonext(dt, Dates.Wed; same=true) == dt
    @test Dates.tonext(dt, Dates.Thu) == Dates.Date(2014, 5, 22)
    @test Dates.tonext(dt, Dates.Fri) == Dates.Date(2014, 5, 23)
    @test Dates.tonext(dt, Dates.Sat) == Dates.Date(2014, 5, 24)
    @test Dates.tonext(dt, Dates.Sun) == Dates.Date(2014, 5, 25)
    @test Dates.tonext(dt, Dates.Mon) == Dates.Date(2014, 5, 26)
    @test Dates.tonext(dt, Dates.Tue) == Dates.Date(2014, 5, 27)
    # No dayofweek function for out of range values
    @test_throws KeyError Dates.tonext(dt, 8)

    @test Dates.tonext(Dates.Date(0), Dates.Mon) == Dates.Date(0, 1, 3)
    @testset "func, diff steps, same" begin
        @test Dates.tonext(Dates.iswednesday, dt) == Dates.Date(2014, 5, 28)
        @test Dates.tonext(Dates.iswednesday, dt; same=true) == dt
        @test Dates.tonext(Dates.isthursday, dt) == Dates.Date(2014, 5, 22)
        @test Dates.tonext(Dates.isfriday, dt) == Dates.Date(2014, 5, 23)
        @test Dates.tonext(Dates.issaturday, dt) == Dates.Date(2014, 5, 24)
        @test Dates.tonext(Dates.issunday, dt) == Dates.Date(2014, 5, 25)
        @test Dates.tonext(Dates.ismonday, dt) == Dates.Date(2014, 5, 26)
        @test Dates.tonext(Dates.istuesday, dt) == Dates.Date(2014, 5, 27)
        @test Dates.tonext(Dates.ismonday, Dates.Date(0)) == Dates.Date(0, 1, 3)

        @test Dates.tonext(!Dates.iswednesday, dt) == Dates.Date(2014, 5, 22)
        @test Dates.tonext(!Dates.isthursday, dt) == Dates.Date(2014, 5, 23)
    end

    # Reach adjust limit
    @test_throws ArgumentError Dates.tonext(Dates.iswednesday, dt; limit=6)

    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(2)) == Dates.Date(2014, 6, 4)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(3)) == Dates.Date(2014, 6, 11)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(4)) == Dates.Date(2014, 6, 18)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(5)) == Dates.Date(2014, 6, 25)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(6)) == Dates.Date(2014, 7, 2)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Day(7)) == Dates.Date(2014, 5, 28)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(1)) == Dates.Date(2014, 5, 28)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(2)) == Dates.Date(2014, 6, 4)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(3)) == Dates.Date(2014, 6, 11)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(4)) == Dates.Date(2014, 6, 18)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(5)) == Dates.Date(2014, 6, 25)
    @test Dates.tonext(Dates.iswednesday, dt;step=Dates.Week(6)) == Dates.Date(2014, 7, 2)

    @test Dates.tonext(Dates.iswednesday, dt; same=true) == dt
    @test Dates.tonext(Dates.isthursday, dt) == Dates.Date(2014, 5, 22)
end

@testset "toprev" begin
    dt = Dates.Date(2014, 5, 21)
    @test Dates.toprev(dt, Dates.Wed) == Dates.Date(2014, 5, 14)
    @test Dates.toprev(dt, Dates.Wed; same=true) == dt
    @test Dates.toprev(dt, Dates.Thu) == Dates.Date(2014, 5, 15)
    @test Dates.toprev(dt, Dates.Fri) == Dates.Date(2014, 5, 16)
    @test Dates.toprev(dt, Dates.Sat) == Dates.Date(2014, 5, 17)
    @test Dates.toprev(dt, Dates.Sun) == Dates.Date(2014, 5, 18)
    @test Dates.toprev(dt, Dates.Mon) == Dates.Date(2014, 5, 19)
    @test Dates.toprev(dt, Dates.Tue) == Dates.Date(2014, 5, 20)
    # No dayofweek function for out of range values
    @test_throws KeyError Dates.toprev(dt, 8)

    @test Dates.toprev(Dates.Date(0), Dates.Mon) == Dates.Date(-1, 12, 27)
end
@testset "tofirst" begin
    dt = Dates.Date(2014, 5, 21)
    @test Dates.tofirst(dt, Dates.Mon) == Dates.Date(2014, 5, 5)
    @test Dates.tofirst(dt, Dates.Tue) == Dates.Date(2014, 5, 6)
    @test Dates.tofirst(dt, Dates.Wed) == Dates.Date(2014, 5, 7)
    @test Dates.tofirst(dt, Dates.Thu) == Dates.Date(2014, 5, 1)
    @test Dates.tofirst(dt, Dates.Fri) == Dates.Date(2014, 5, 2)
    @test Dates.tofirst(dt, Dates.Sat) == Dates.Date(2014, 5, 3)
    @test Dates.tofirst(dt, Dates.Sun) == Dates.Date(2014, 5, 4)

    @test Dates.tofirst(dt, Dates.Mon, of=Dates.Year) == Dates.Date(2014, 1, 6)
    @test Dates.tofirst(dt, Dates.Tue, of=Dates.Year) == Dates.Date(2014, 1, 7)
    @test Dates.tofirst(dt, Dates.Wed, of=Dates.Year) == Dates.Date(2014, 1, 1)
    @test Dates.tofirst(dt, Dates.Thu, of=Dates.Year) == Dates.Date(2014, 1, 2)
    @test Dates.tofirst(dt, Dates.Fri, of=Dates.Year) == Dates.Date(2014, 1, 3)
    @test Dates.tofirst(dt, Dates.Sat, of=Dates.Year) == Dates.Date(2014, 1, 4)
    @test Dates.tofirst(dt, Dates.Sun, of=Dates.Year) == Dates.Date(2014, 1, 5)

    @test Dates.tofirst(Dates.Date(0), Dates.Mon) == Dates.Date(0, 1, 3)
end
@testset "tolast" begin
    dt = Dates.Date(2014, 5, 21)
    @test Dates.tolast(dt, Dates.Mon) == Dates.Date(2014, 5, 26)
    @test Dates.tolast(dt, Dates.Tue) == Dates.Date(2014, 5, 27)
    @test Dates.tolast(dt, Dates.Wed) == Dates.Date(2014, 5, 28)
    @test Dates.tolast(dt, Dates.Thu) == Dates.Date(2014, 5, 29)
    @test Dates.tolast(dt, Dates.Fri) == Dates.Date(2014, 5, 30)
    @test Dates.tolast(dt, Dates.Sat) == Dates.Date(2014, 5, 31)
    @test Dates.tolast(dt, Dates.Sun) == Dates.Date(2014, 5, 25)

    @test Dates.tolast(dt, Dates.Mon, of=Dates.Year) == Dates.Date(2014, 12, 29)
    @test Dates.tolast(dt, Dates.Tue, of=Dates.Year) == Dates.Date(2014, 12, 30)
    @test Dates.tolast(dt, Dates.Wed, of=Dates.Year) == Dates.Date(2014, 12, 31)
    @test Dates.tolast(dt, Dates.Thu, of=Dates.Year) == Dates.Date(2014, 12, 25)
    @test Dates.tolast(dt, Dates.Fri, of=Dates.Year) == Dates.Date(2014, 12, 26)
    @test Dates.tolast(dt, Dates.Sat, of=Dates.Year) == Dates.Date(2014, 12, 27)
    @test Dates.tolast(dt, Dates.Sun, of=Dates.Year) == Dates.Date(2014, 12, 28)

    @test Dates.tolast(Dates.Date(0), Dates.Mon) == Dates.Date(0, 1, 31)
end
@testset "filter" begin # was recur
    startdate = Dates.Date(2014, 1, 1)
    stopdate = Dates.Date(2014, 2, 1)
    @test length(filter(x->true, startdate:Dates.Day(1):stopdate))  == 32
    @test length(filter(x->true, stopdate:Dates.Day(-1):startdate)) == 32

    Januarymondays2014 = [Dates.Date(2014, 1, 6), Dates.Date(2014, 1, 13), Dates.Date(2014, 1, 20), Dates.Date(2014, 1, 27)]
    @test filter(Dates.ismonday, startdate:Dates.Day(1):stopdate) == Januarymondays2014

    @test_throws MethodError filter((x, y)->x + y, Dates.Date(2013):Dates.Day(1):Dates.Date(2014))
    @test_throws MethodError Dates.DateFunction((x, y)->x + y, Date(0))
    @test_throws ArgumentError Dates.DateFunction((dt)->2, Date(0))
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 2))) == 32
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 1))) == 1
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 2))) == 2
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 3))) == 3
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 4))) == 4
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 5))) == 5
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 6))) == 6
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 7))) == 7
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2013, 1, 8))) == 8
    @test length(filter(x->true, Dates.Date(2013):Dates.Month(1):Dates.Date(2013, 1, 1))) == 1
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(-1):Dates.Date(2012, 1, 1))) == 367

    # Empty range
    @test length(filter(x->true, Dates.Date(2013):Dates.Day(1):Dates.Date(2012, 1, 1))) == 0

    # All leap days in 20th century
    @test length(filter(Dates.Date(1900):Dates.Day(1):Dates.Date(2000)) do x
        Dates.month(x) == Dates.Feb && Dates.day(x) == 29
    end) == 24
end

@testset "Holiday test cases" begin
    dt = Dates.Date(2014, 5, 21)
    dr = Dates.Date(2014):Dates.Day(1):Dates.Date(2015)
    @testset "Thanksgiving" begin
        # 4th Thursday of November
        thanksgiving = x->Dates.dayofweek(x) == Dates.Thu &&
                              Dates.month(x) == Dates.Nov &&
                   Dates.dayofweekofmonth(x) == 4

        d = Dates.Date(2014, 6, 5)

        @test Dates.tonext(d) do x
            thanksgiving(x)
        end == Dates.Date(2014, 11, 27)

        @test Dates.toprev(d) do x
            thanksgiving(x)
        end == Dates.Date(2013, 11, 28)
    end
    @testset "Pittsburgh street cleaning" begin
        @test length(filter(dr) do x
            Dates.dayofweek(x) == Dates.Tue &&
            Dates.April < Dates.month(x) < Dates.Nov &&
            Dates.dayofweekofmonth(x) == 2
        end) == 6
    end
    @testset "U.S. Federal Holidays" begin
        newyears(y) = (y, 1, 1)
        independenceday(y) = (y, 7, 4)
        veteransday(y) = (y, 11, 11)
        christmas(y) = (y, 12, 25)

        isnewyears(dt) = Dates.yearmonthday(dt) == newyears(Dates.year(dt))
        isindependenceday(dt) = Dates.yearmonthday(dt) == independenceday(Dates.year(dt))
        isveteransday(dt) = Dates.yearmonthday(dt) == veteransday(Dates.year(dt))
        ischristmas(dt) = Dates.yearmonthday(dt) == christmas(Dates.year(dt))
        ismartinlutherking(dt) = Dates.dayofweek(dt) == Dates.Mon &&
            Dates.month(dt) == Dates.Jan && Dates.dayofweekofmonth(dt) == 3
        ispresidentsday(dt) = Dates.dayofweek(dt) == Dates.Mon &&
            Dates.month(dt) == Dates.Feb && Dates.dayofweekofmonth(dt) == 3
        # Last Monday of May
        ismemorialday(dt) = Dates.dayofweek(dt) == Dates.Mon &&
                            Dates.month(dt) == Dates.May &&
                            Dates.dayofweekofmonth(dt) == Dates.daysofweekinmonth(dt)
        islaborday(dt) = Dates.dayofweek(dt) == Dates.Mon &&
            Dates.month(dt) == Dates.Sep && Dates.dayofweekofmonth(dt) == 1
        iscolumbusday(dt) = Dates.dayofweek(dt) == Dates.Mon &&
            Dates.month(dt) == Dates.Oct && Dates.dayofweekofmonth(dt) == 2
        isthanksgiving(dt) = Dates.dayofweek(dt) == Dates.Thu &&
            Dates.month(dt) == Dates.Nov && Dates.dayofweekofmonth(dt) == 4

        function easter(y)
            # Butcher's Algorithm: http://www.smart.net/~mmontes/butcher.html
            a = y % 19
            b = div(y, 100)
            c = y % 100
            d = div(b, 4)
            e = b % 4
            f = div(b + 8, 25)
            g = div(b - f + 1, 3)
            h = (19 * a + b - d - g + 15) % 30
            i = div(c, 4)
            k = c % 4
            l = (32 + 2 * e + 2 * i - h - k) % 7
            m = div(a + 11 * h + 22 * l, 451)
            month = div(h + l - 7 * m + 114, 31)
            p = (h + l - 7 * m + 114) % 31
            return (y, month, p + 1)
        end
        iseaster(dt) = Dates.yearmonthday(dt) == easter(Dates.year(dt))

        HOLIDAYS = x->isnewyears(x) || isindependenceday(x) ||
                      isveteransday(x) || ischristmas(x) ||
                      ismartinlutherking(x) || ispresidentsday(x) ||
                      ismemorialday(x) || islaborday(x) ||
                      iscolumbusday(x) || isthanksgiving(x)

        @test length(filter(HOLIDAYS, dr)) == 11

        OBSERVEDHOLIDAYS = x->begin
            # If the holiday is on a weekday
            if HOLIDAYS(x) && Dates.dayofweek(x) < Dates.Saturday
                return true
            # Holiday is observed Monday if falls on Sunday
            elseif Dates.dayofweek(x) == 1 && HOLIDAYS(x - Dates.Day(1))
                return true
            # Holiday is observed Friday if falls on Saturday
            elseif Dates.dayofweek(x) == 5 && HOLIDAYS(x + Dates.Day(1))
                return true
            else
                return false
            end
        end

        observed = filter(OBSERVEDHOLIDAYS, Dates.Date(1999):Dates.Day(1):Dates.Date(2000))
        @test length(observed) == 11
        @test observed[10] == Dates.Date(1999, 12, 24)
        @test observed[11] == Dates.Date(1999, 12, 31)

        # Get all business/working days of 2014
        # Since we have already defined observed holidays,
        # we just look at weekend days and negate the result
        @test length(filter(Dates.Date(2014):Dates.Day(1):Dates.Date(2015)) do x
            !(OBSERVEDHOLIDAYS(x) ||
            Dates.dayofweek(x) > 5)
        end) == 251
    end
end

@testset "proof-of-concepts" begin
    # First day of the next month for each day of 2014
    @test length([Dates.firstdayofmonth(i + Dates.Month(1))
        for i in Dates.Date(2014):Dates.Day(1):Dates.Date(2014, 12, 31)]) == 365

    # From those goofy email forwards claiming a "special, lucky month"
    # that has 5 Fridays, 5 Saturdays, and 5 Sundays and that it only
    # occurs every 823 years
    @test length(filter(Date(2000):Dates.Month(1):Date(2016)) do dt
        sum = 0
        for i = 1:7
            sum += Dates.dayofweek(dt) > 4 ? Dates.daysofweekinmonth(dt) : 0
            dt += Dates.Day(1)
        end
        return sum == 15
    end) == 15 # On average, there's one of those months every year

    r = Dates.Time(x->Dates.second(x) == 5, 1)
    @test r == Dates.Time(1, 0, 5)

    r = filter(x->Dates.second(x) == 5, Dates.Time(0):Dates.Second(1):Dates.Time(10))
    @test length(r) == 600
    @test first(r) == Dates.Time(0, 0, 5)
    @test last(r) == Dates.Time(9, 59, 5)
end
end # module

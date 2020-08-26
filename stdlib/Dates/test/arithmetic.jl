# This file is a part of Julia. License is MIT: https://julialang.org/license

module ArithmeticTest

using Test
using Dates

@testset "Time arithmetic" begin
    a = Dates.Time(23, 59, 59)
    b = Dates.Time(11, 59, 59)
    @test Dates.CompoundPeriod(a - b) == Dates.Hour(12)
end
@testset "Wrapping arithmetic for Months" begin
    # This ends up being trickier than expected because
    # the user might do 2014-01-01 + Month(-14)
    # monthwrap figures out the resulting month
    # when adding/subtracting months from a date
    @test Dates.monthwrap(1, -14) == 11
    @test Dates.monthwrap(1, -13) == 12
    @test Dates.monthwrap(1, -12) == 1
    @test Dates.monthwrap(1, -11) == 2
    @test Dates.monthwrap(1, -10) == 3
    @test Dates.monthwrap(1, -9) == 4
    @test Dates.monthwrap(1, -8) == 5
    @test Dates.monthwrap(1, -7) == 6
    @test Dates.monthwrap(1, -6) == 7
    @test Dates.monthwrap(1, -5) == 8
    @test Dates.monthwrap(1, -4) == 9
    @test Dates.monthwrap(1, -3) == 10
    @test Dates.monthwrap(1, -2) == 11
    @test Dates.monthwrap(1, -1) == 12
    @test Dates.monthwrap(1, 0) == 1
    @test Dates.monthwrap(1, 1) == 2
    @test Dates.monthwrap(1, 2) == 3
    @test Dates.monthwrap(1, 3) == 4
    @test Dates.monthwrap(1, 4) == 5
    @test Dates.monthwrap(1, 5) == 6
    @test Dates.monthwrap(1, 6) == 7
    @test Dates.monthwrap(1, 7) == 8
    @test Dates.monthwrap(1, 8) == 9
    @test Dates.monthwrap(1, 9) == 10
    @test Dates.monthwrap(1, 10) == 11
    @test Dates.monthwrap(1, 11) == 12
    @test Dates.monthwrap(1, 12) == 1
    @test Dates.monthwrap(1, 13) == 2
    @test Dates.monthwrap(1, 24) == 1
    @test Dates.monthwrap(12, -14) == 10
    @test Dates.monthwrap(12, -13) == 11
    @test Dates.monthwrap(12, -12) == 12
    @test Dates.monthwrap(12, -11) == 1
    @test Dates.monthwrap(12, -2) == 10
    @test Dates.monthwrap(12, -1) == 11
    @test Dates.monthwrap(12, 0) == 12
    @test Dates.monthwrap(12, 1) == 1
    @test Dates.monthwrap(12, 2) == 2
    @test Dates.monthwrap(12, 11) == 11
    @test Dates.monthwrap(12, 12) == 12
    @test Dates.monthwrap(12, 13) == 1
end
@testset "Wrapping arithmetic for years" begin
    # yearwrap figures out the resulting year
    # when adding/subtracting months from a date
    @test Dates.yearwrap(2000, 1, -3600) == 1700
    @test Dates.yearwrap(2000, 1, -37) == 1996
    @test Dates.yearwrap(2000, 1, -36) == 1997
    @test Dates.yearwrap(2000, 1, -35) == 1997
    @test Dates.yearwrap(2000, 1, -25) == 1997
    @test Dates.yearwrap(2000, 1, -24) == 1998
    @test Dates.yearwrap(2000, 1, -23) == 1998
    @test Dates.yearwrap(2000, 1, -14) == 1998
    @test Dates.yearwrap(2000, 1, -13) == 1998
    @test Dates.yearwrap(2000, 1, -12) == 1999
    @test Dates.yearwrap(2000, 1, -11) == 1999
    @test Dates.yearwrap(2000, 1, -2) == 1999
    @test Dates.yearwrap(2000, 1, -1) == 1999
    @test Dates.yearwrap(2000, 1, 0) == 2000
    @test Dates.yearwrap(2000, 1, 1) == 2000
    @test Dates.yearwrap(2000, 1, 11) == 2000
    @test Dates.yearwrap(2000, 1, 12) == 2001
    @test Dates.yearwrap(2000, 1, 13) == 2001
    @test Dates.yearwrap(2000, 1, 23) == 2001
    @test Dates.yearwrap(2000, 1, 24) == 2002
    @test Dates.yearwrap(2000, 1, 25) == 2002
    @test Dates.yearwrap(2000, 1, 36) == 2003
    @test Dates.yearwrap(2000, 1, 3600) == 2300
    @test Dates.yearwrap(2000, 2, -2) == 1999
    @test Dates.yearwrap(2000, 3, 10) == 2001
    @test Dates.yearwrap(2000, 4, -4) == 1999
    @test Dates.yearwrap(2000, 5, 8) == 2001
    @test Dates.yearwrap(2000, 6, -18) == 1998
    @test Dates.yearwrap(2000, 6, -6) == 1999
    @test Dates.yearwrap(2000, 6, 6) == 2000
    @test Dates.yearwrap(2000, 6, 7) == 2001
    @test Dates.yearwrap(2000, 6, 19) == 2002
    @test Dates.yearwrap(2000, 12, -3600) == 1700
    @test Dates.yearwrap(2000, 12, -36) == 1997
    @test Dates.yearwrap(2000, 12, -35) == 1998
    @test Dates.yearwrap(2000, 12, -24) == 1998
    @test Dates.yearwrap(2000, 12, -23) == 1999
    @test Dates.yearwrap(2000, 12, -14) == 1999
    @test Dates.yearwrap(2000, 12, -13) == 1999
    @test Dates.yearwrap(2000, 12, -12) == 1999
    @test Dates.yearwrap(2000, 12, -11) == 2000
    @test Dates.yearwrap(2000, 12, -2) == 2000
    @test Dates.yearwrap(2000, 12, -1) == 2000
    @test Dates.yearwrap(2000, 12, 0) == 2000
    @test Dates.yearwrap(2000, 12, 1) == 2001
    @test Dates.yearwrap(2000, 12, 11) == 2001
    @test Dates.yearwrap(2000, 12, 12) == 2001
    @test Dates.yearwrap(2000, 12, 13) == 2002
    @test Dates.yearwrap(2000, 12, 24) == 2002
    @test Dates.yearwrap(2000, 12, 25) == 2003
    @test Dates.yearwrap(2000, 12, 36) == 2003
    @test Dates.yearwrap(2000, 12, 37) == 2004
    @test Dates.yearwrap(2000, 12, 3600) == 2300
end
@testset "DateTime arithmetic" begin
    # Dates.DateTime arithmetic
    a = Dates.DateTime(2013, 1, 1, 0, 0, 0, 1)
    b = Dates.DateTime(2013, 1, 1, 0, 0, 0, 0)
    @test a - b == Dates.Millisecond(1)
    @test Dates.DateTime(2013, 1, 2) - b == Dates.Millisecond(86400000)

    @testset "Dates.DateTime-Year arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Year(1) == Dates.DateTime(2000, 12, 27)
        @test dt + Dates.Year(100) == Dates.DateTime(2099, 12, 27)
        @test dt + Dates.Year(1000) == Dates.DateTime(2999, 12, 27)
        @test dt - Dates.Year(1) == Dates.DateTime(1998, 12, 27)
        @test dt - Dates.Year(100) == Dates.DateTime(1899, 12, 27)
        @test dt - Dates.Year(1000) == Dates.DateTime(999, 12, 27)
        dt = Dates.DateTime(2000, 2, 29)
        @test dt + Dates.Year(1) == Dates.DateTime(2001, 2, 28)
        @test dt - Dates.Year(1) == Dates.DateTime(1999, 2, 28)
        @test dt + Dates.Year(4) == Dates.DateTime(2004, 2, 29)
        @test dt - Dates.Year(4) == Dates.DateTime(1996, 2, 29)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Year(1) == Dates.DateTime(1973, 6, 30, 23, 59, 59)
        @test dt - Dates.Year(1) == Dates.DateTime(1971, 6, 30, 23, 59, 59)
        @test dt + Dates.Year(-1) == Dates.DateTime(1971, 6, 30, 23, 59, 59)
        @test dt - Dates.Year(-1) == Dates.DateTime(1973, 6, 30, 23, 59, 59)

        dt = Dates.DateTime(2000, 1, 1, 12, 30, 45, 500)
        dt2 = dt + Dates.Year(1)
        @test Dates.year(dt2) == 2001
        @test Dates.month(dt2) == 1
        @test Dates.day(dt2) == 1
        @test Dates.hour(dt2) == 12
        @test Dates.minute(dt2) == 30
        @test Dates.second(dt2) == 45
        @test Dates.millisecond(dt2) == 500
    end
    @testset "DateTime-Quarter arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Quarter(1) == Dates.DateTime(2000, 3, 27)
        @test dt + Dates.Quarter(-1) == Dates.DateTime(1999, 9, 27)
    end

    @testset "DateTime-Month arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Month(1) == Dates.DateTime(2000, 1, 27)
        @test dt + Dates.Month(-1) == Dates.DateTime(1999, 11, 27)
        @test dt + Dates.Month(-11) == Dates.DateTime(1999, 1, 27)
        @test dt + Dates.Month(11) == Dates.DateTime(2000, 11, 27)
        @test dt + Dates.Month(-12) == Dates.DateTime(1998, 12, 27)
        @test dt + Dates.Month(12) == Dates.DateTime(2000, 12, 27)
        @test dt + Dates.Month(13) == Dates.DateTime(2001, 1, 27)
        @test dt + Dates.Month(100) == Dates.DateTime(2008, 4, 27)
        @test dt + Dates.Month(1000) == Dates.DateTime(2083, 4, 27)
        @test dt - Dates.Month(1) == Dates.DateTime(1999, 11, 27)
        @test dt - Dates.Month(-1) == Dates.DateTime(2000, 1, 27)
        @test dt - Dates.Month(100) == Dates.DateTime(1991, 8, 27)
        @test dt - Dates.Month(1000) == Dates.DateTime(1916, 8, 27)
        dt = Dates.DateTime(2000, 2, 29)
        @test dt + Dates.Month(1) == Dates.DateTime(2000, 3, 29)
        @test dt - Dates.Month(1) == Dates.DateTime(2000, 1, 29)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Month(1) == Dates.DateTime(1972, 7, 30, 23, 59, 59)
        @test dt - Dates.Month(1) == Dates.DateTime(1972, 5, 30, 23, 59, 59)
        @test dt + Dates.Month(-1) == Dates.DateTime(1972, 5, 30, 23, 59, 59)
    end
    @testset "DateTime-Week arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Week(1) == Dates.DateTime(2000, 1, 3)
        @test dt + Dates.Week(100) == Dates.DateTime(2001, 11, 26)
        @test dt + Dates.Week(1000) == Dates.DateTime(2019, 2, 25)
        @test dt - Dates.Week(1) == Dates.DateTime(1999, 12, 20)
        @test dt - Dates.Week(100) == Dates.DateTime(1998, 1, 26)
        @test dt - Dates.Week(1000) == Dates.DateTime(1980, 10, 27)
        dt = Dates.DateTime(2000, 2, 29)
        @test dt + Dates.Week(1) == Dates.DateTime(2000, 3, 7)
        @test dt - Dates.Week(1) == Dates.DateTime(2000, 2, 22)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Week(1) == Dates.DateTime(1972, 7, 7, 23, 59, 59)
        @test dt - Dates.Week(1) == Dates.DateTime(1972, 6, 23, 23, 59, 59)
        @test dt + Dates.Week(-1) == Dates.DateTime(1972, 6, 23, 23, 59, 59)
    end
    @testset "DateTime-Day arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Day(1) == Dates.DateTime(1999, 12, 28)
        @test dt + Dates.Day(100) == Dates.DateTime(2000, 4, 5)
        @test dt + Dates.Day(1000) == Dates.DateTime(2002, 9, 22)
        @test dt - Dates.Day(1) == Dates.DateTime(1999, 12, 26)
        @test dt - Dates.Day(100) == Dates.DateTime(1999, 9, 18)
        @test dt - Dates.Day(1000) == Dates.DateTime(1997, 4, 1)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Day(1) == Dates.DateTime(1972, 7, 1, 23, 59, 59)
        @test dt - Dates.Day(1) == Dates.DateTime(1972, 6, 29, 23, 59, 59)
        @test dt + Dates.Day(-1) == Dates.DateTime(1972, 6, 29, 23, 59, 59)
    end
    @testset "DateTime-Hour arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Hour(1) == Dates.DateTime(1999, 12, 27, 1)
        @test dt + Dates.Hour(100) == Dates.DateTime(1999, 12, 31, 4)
        @test dt + Dates.Hour(1000) == Dates.DateTime(2000, 2, 6, 16)
        @test dt - Dates.Hour(1) == Dates.DateTime(1999, 12, 26, 23)
        @test dt - Dates.Hour(100) == Dates.DateTime(1999, 12, 22, 20)
        @test dt - Dates.Hour(1000) == Dates.DateTime(1999, 11, 15, 8)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Hour(1) == Dates.DateTime(1972, 7, 1, 0, 59, 59)
        @test dt - Dates.Hour(1) == Dates.DateTime(1972, 6, 30, 22, 59, 59)
        @test dt + Dates.Hour(-1) == Dates.DateTime(1972, 6, 30, 22, 59, 59)
    end
    @testset "DateTime-Minute arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Minute(1) == Dates.DateTime(1999, 12, 27, 0, 1)
        @test dt + Dates.Minute(100) == Dates.DateTime(1999, 12, 27, 1, 40)
        @test dt + Dates.Minute(1000) == Dates.DateTime(1999, 12, 27, 16, 40)
        @test dt - Dates.Minute(1) == Dates.DateTime(1999, 12, 26, 23, 59)
        @test dt - Dates.Minute(100) == Dates.DateTime(1999, 12, 26, 22, 20)
        @test dt - Dates.Minute(1000) == Dates.DateTime(1999, 12, 26, 7, 20)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Minute(1) == Dates.DateTime(1972, 7, 1, 0, 0, 59)
        @test dt - Dates.Minute(1) == Dates.DateTime(1972, 6, 30, 23, 58, 59)
        @test dt + Dates.Minute(-1) == Dates.DateTime(1972, 6, 30, 23, 58, 59)
    end
    @testset "DateTime-Second arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Second(1) == Dates.DateTime(1999, 12, 27, 0, 0, 1)
        @test dt + Dates.Second(100) == Dates.DateTime(1999, 12, 27, 0, 1, 40)
        @test dt + Dates.Second(1000) == Dates.DateTime(1999, 12, 27, 0, 16, 40)
        @test dt - Dates.Second(1) == Dates.DateTime(1999, 12, 26, 23, 59, 59)
        @test dt - Dates.Second(100) == Dates.DateTime(1999, 12, 26, 23, 58, 20)
        @test dt - Dates.Second(1000) == Dates.DateTime(1999, 12, 26, 23, 43, 20)
    end
    @testset "DateTime-Millisecond arithmetic" begin
        dt = Dates.DateTime(1999, 12, 27)
        @test dt + Dates.Millisecond(1) == Dates.DateTime(1999, 12, 27, 0, 0, 0, 1)
        @test dt + Dates.Millisecond(100) == Dates.DateTime(1999, 12, 27, 0, 0, 0, 100)
        @test dt + Dates.Millisecond(1000) == Dates.DateTime(1999, 12, 27, 0, 0, 1)
        @test dt - Dates.Millisecond(1) == Dates.DateTime(1999, 12, 26, 23, 59, 59, 999)
        @test dt - Dates.Millisecond(100) == Dates.DateTime(1999, 12, 26, 23, 59, 59, 900)
        @test dt - Dates.Millisecond(1000) == Dates.DateTime(1999, 12, 26, 23, 59, 59)
        dt = Dates.DateTime(1972, 6, 30, 23, 59, 59)
        @test dt + Dates.Millisecond(1) == Dates.DateTime(1972, 6, 30, 23, 59, 59, 1)
        @test dt - Dates.Millisecond(1) == Dates.DateTime(1972, 6, 30, 23, 59, 58, 999)
        @test dt + Dates.Millisecond(-1) == Dates.DateTime(1972, 6, 30, 23, 59, 58, 999)
    end
end
@testset "Date arithmetic" begin
    @testset "Date-Year arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Year(1) == Dates.Date(2000, 12, 27)
        @test dt + Dates.Year(100) == Dates.Date(2099, 12, 27)
        @test dt + Dates.Year(1000) == Dates.Date(2999, 12, 27)
        @test dt - Dates.Year(1) == Dates.Date(1998, 12, 27)
        @test dt - Dates.Year(100) == Dates.Date(1899, 12, 27)
        @test dt - Dates.Year(1000) == Dates.Date(999, 12, 27)
        dt = Dates.Date(2000, 2, 29)
        @test dt + Dates.Year(1) == Dates.Date(2001, 2, 28)
        @test dt - Dates.Year(1) == Dates.Date(1999, 2, 28)
        @test dt + Dates.Year(4) == Dates.Date(2004, 2, 29)
        @test dt - Dates.Year(4) == Dates.Date(1996, 2, 29)
    end
    @testset "Date-Quarter arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Quarter(1) == Dates.Date(2000, 3, 27)
        @test dt - Dates.Quarter(1) == Dates.Date(1999, 9, 27)
    end
    @testset "Date-Month arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Month(1) == Dates.Date(2000, 1, 27)
        @test dt + Dates.Month(100) == Dates.Date(2008, 4, 27)
        @test dt + Dates.Month(1000) == Dates.Date(2083, 4, 27)
        @test dt - Dates.Month(1) == Dates.Date(1999, 11, 27)
        @test dt - Dates.Month(100) == Dates.Date(1991, 8, 27)
        @test dt - Dates.Month(1000) == Dates.Date(1916, 8, 27)
        dt = Dates.Date(2000, 2, 29)
        @test dt + Dates.Month(1) == Dates.Date(2000, 3, 29)
        @test dt - Dates.Month(1) == Dates.Date(2000, 1, 29)
    end
    @testset "Date-Week arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Week(1) == Dates.Date(2000, 1, 3)
        @test dt + Dates.Week(100) == Dates.Date(2001, 11, 26)
        @test dt + Dates.Week(1000) == Dates.Date(2019, 2, 25)
        @test dt - Dates.Week(1) == Dates.Date(1999, 12, 20)
        @test dt - Dates.Week(100) == Dates.Date(1998, 1, 26)
        @test dt - Dates.Week(1000) == Dates.Date(1980, 10, 27)
        dt = Dates.Date(2000, 2, 29)
        @test dt + Dates.Week(1) == Dates.Date(2000, 3, 7)
        @test dt - Dates.Week(1) == Dates.Date(2000, 2, 22)
    end
    @testset "Date-Day arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Day(1) == Dates.Date(1999, 12, 28)
        @test dt + Dates.Day(100) == Dates.Date(2000, 4, 5)
        @test dt + Dates.Day(1000) == Dates.Date(2002, 9, 22)
        @test dt - Dates.Day(1) == Dates.Date(1999, 12, 26)
        @test dt - Dates.Day(100) == Dates.Date(1999, 9, 18)
        @test dt - Dates.Day(1000) == Dates.Date(1997, 4, 1)
    end
    @testset "Date-Time arithmetic" begin
        dt = Dates.Date(1999, 12, 27)
        @test dt + Dates.Time(0, 0, 0) == Dates.DateTime(1999, 12, 27, 0, 0, 0)
        @test dt + Dates.Time(1, 0, 0) == Dates.DateTime(1999, 12, 27, 1, 0, 0)
        @test dt + Dates.Time(0, 1, 0) == Dates.DateTime(1999, 12, 27, 0, 1, 0)
        @test dt + Dates.Time(0, 0, 1) == Dates.DateTime(1999, 12, 27, 0, 0, 1)
        @test Dates.Time(0, 0, 1) + dt == Dates.DateTime(1999, 12, 27, 0, 0, 1)

        t = Dates.Time(0, 0, 0) + Dates.Hour(24)
        @test dt + t == Dates.DateTime(1999, 12, 27, 0, 0, 0)
    end
end
@testset "Time-TimePeriod arithmetic" begin
    t = Dates.Time(0)
    @test t + Dates.Hour(1) == Dates.Time(1)
    @test t - Dates.Hour(1) == Dates.Time(23)
    @test t - Dates.Nanosecond(1) == Dates.Time(23, 59, 59, 999, 999, 999)
    @test t + Dates.Nanosecond(-1) == Dates.Time(23, 59, 59, 999, 999, 999)
    @test t + Dates.Hour(24) == t
    @test t + Dates.Nanosecond(86400000000000) == t
    @test t - Dates.Nanosecond(86400000000000) == t
    @test t + Dates.Minute(1) == Dates.Time(0, 1)
    @test t + Dates.Second(1) == Dates.Time(0, 0, 1)
    @test t + Dates.Millisecond(1) == Dates.Time(0, 0, 0, 1)
    @test t + Dates.Microsecond(1) == Dates.Time(0, 0, 0, 0, 1)
    @test_throws MethodError t + Dates.Day(1)
    @testset "Time-TimePeriod arithmetic inequalities" begin
        t = Dates.Time(4, 20)
        @test t - Dates.Nanosecond(1) < t
        @test t + Dates.Nanosecond(1) > t
        @test t + Dates.Hour(24) < typemax(Dates.Time)
        @test t - Dates.Hour(24) > typemin(Dates.Time)
        @test t + Dates.Hour(23) < t
        @test t + Dates.Hour(25) > t
    end
end
@testset "Month arithmetic and non-associativity" begin
    # Month arithmetic minimizes "edit distance", or number of changes
    # needed to get a correct answer
    # This approach results in a few cases of non-associativity
    a = Dates.Date(2012, 1, 29)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 1, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 2, 29)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 3, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 4, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 5, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 6, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 8, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 9, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 10, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
    a = Dates.Date(2012, 11, 30)
    @test (a + Dates.Day(1)) + Dates.Month(1) != (a + Dates.Month(1)) + Dates.Day(1)
end
@testset "Vectorized arithmetic" begin
    # Vectorized Time arithmetic
    a = Dates.Time(1, 1, 1)
    dr = [a, a, a, a, a, a, a, a, a, a]
    b = a + Dates.Hour(1)
    @test dr .+ Dates.Hour(1) == repeat([b], 10)
    b = a + Dates.Second(1)
    @test dr .+ Dates.Second(1) == repeat([b], 10)
    b = a + Dates.Millisecond(1)
    @test dr .+ Dates.Millisecond(1) == repeat([b], 10)
    b = a + Dates.Microsecond(1)
    @test dr .+ Dates.Microsecond(1) == repeat([b], 10)
    b = a + Dates.Nanosecond(1)
    @test dr .+ Dates.Nanosecond(1) == repeat([b], 10)

    b = a - Dates.Hour(1)
    @test dr .- Dates.Hour(1) == repeat([b], 10)
    b = a - Dates.Second(1)
    @test dr .- Dates.Second(1) == repeat([b], 10)
    b = a - Dates.Millisecond(1)
    @test dr .- Dates.Millisecond(1) == repeat([b], 10)
    b = a - Dates.Microsecond(1)
    @test dr .- Dates.Microsecond(1) == repeat([b], 10)
    b = a - Dates.Nanosecond(1)
    @test dr .- Dates.Nanosecond(1) == repeat([b], 10)

    # Vectorized arithmetic
    a = Dates.Date(2014, 1, 1)
    dr = [a, a, a, a, a, a, a, a, a, a]
    b = a + Dates.Year(1)
    @test dr .+ Dates.Year(1) == repeat([b], 10)
    b = a + Dates.Month(1)
    @test dr .+ Dates.Month(1) == repeat([b], 10)
    b = a + Dates.Day(1)
    @test dr .+ Dates.Day(1) == repeat([b], 10)
    b = a - Dates.Year(1)
    @test dr .- Dates.Year(1) == repeat([b], 10)
    b = a - Dates.Month(1)
    @test dr .- Dates.Month(1) == repeat([b], 10)
    b = a - Dates.Day(1)
    @test dr .- Dates.Day(1) == repeat([b], 10)

    # Vectorized arithmetic
    b = a + Dates.Year(1)
    @test dr .+ Dates.Year(1) == repeat([b], 10)
    b = a + Dates.Month(1)
    @test dr .+ Dates.Month(1) == repeat([b], 10)
    b = a + Dates.Day(1)
    @test dr .+ Dates.Day(1) == repeat([b], 10)
    b = a - Dates.Year(1)
    @test dr .- Dates.Year(1) == repeat([b], 10)
    b = a - Dates.Month(1)
    @test dr .- Dates.Month(1) == repeat([b], 10)
    b = a - Dates.Day(1)
    @test dr .- Dates.Day(1) == repeat([b], 10)
    t1 = [Dates.Date(2009, 1, 1) Dates.Date(2009, 1, 2) Dates.Date(2009, 1, 3); Dates.Date(2009, 2, 1) Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3)]
    t2 = [Dates.Date(2009, 1, 2) Dates.Date(2009, 2, 2) Dates.Date(2010, 1, 3); Dates.Date(2010, 2, 1) Dates.Date(2009, 3, 2) Dates.Date(2009, 2, 4)]
    t3 = [Dates.DateTime(2009, 1, 1), Dates.DateTime(2009, 1, 2), Dates.DateTime(2009, 1, 3)]
    t4 = [Dates.DateTime(2009, 1, 1, 0, 0, 1), Dates.DateTime(2009, 1, 2, 0, 1), Dates.DateTime(2009, 1, 3, 1)]
    t5 = [Dates.Time(0, 0, 0) Dates.Time(0, 0, 1) Dates.Time(0, 0, 2); Dates.Time(0, 1, 0) Dates.Time(0, 2, 0) Dates.Time(0, 3, 0)]

    @testset "TimeType, Array{TimeType}" begin
        @test Dates.Date(2010, 1, 1) .- t1 == [Dates.Day(365) Dates.Day(364) Dates.Day(363); Dates.Day(334) Dates.Day(333) Dates.Day(332)]
        @test t1 .- Dates.Date(2010, 1, 1) == [Dates.Day(-365) Dates.Day(-364) Dates.Day(-363); Dates.Day(-334) Dates.Day(-333) Dates.Day(-332)]
        @test Dates.DateTime(2009, 1, 1) .- t3 == [Dates.Millisecond(0), Dates.Millisecond(-86400000), Dates.Millisecond(-172800000)]
        @test t3 .- Dates.DateTime(2009, 1, 1) == [Dates.Millisecond(0), Dates.Millisecond(86400000), Dates.Millisecond(172800000)]
        @test Dates.Time(2) .- t5 == [Dates.Nanosecond(7200000000000) Dates.Nanosecond(7199000000000) Dates.Nanosecond(7198000000000);
                                      Dates.Nanosecond(7140000000000) Dates.Nanosecond(7080000000000) Dates.Nanosecond(7020000000000)]
    end
    @testset "GeneralPeriod, Array{TimeType}" begin
        @test Dates.Day(1) .+ t1 == [Dates.Date(2009, 1, 2) Dates.Date(2009, 1, 3) Dates.Date(2009, 1, 4); Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4)]
        @test Dates.Hour(1) .+ t3 == [Dates.DateTime(2009, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1), Dates.DateTime(2009, 1, 3, 1)]
        @test t1 .+ Dates.Day(1) == [Dates.Date(2009, 1, 2) Dates.Date(2009, 1, 3) Dates.Date(2009, 1, 4); Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4)]
        @test t3 .+ Dates.Hour(1) == [Dates.DateTime(2009, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1), Dates.DateTime(2009, 1, 3, 1)]

        @test (Dates.Month(1) + Dates.Day(1)) .+ t1 == [Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4); Dates.Date(2009, 3, 2) Dates.Date(2009, 3, 3) Dates.Date(2009, 3, 4)]
        @test (Dates.Hour(1) + Dates.Minute(1)) .+ t3 == [Dates.DateTime(2009, 1, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1, 1), Dates.DateTime(2009, 1, 3, 1, 1)]
        @test t1 .+ (Dates.Month(1) + Dates.Day(1)) == [Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4); Dates.Date(2009, 3, 2) Dates.Date(2009, 3, 3) Dates.Date(2009, 3, 4)]
        @test t3 .+ (Dates.Hour(1) + Dates.Minute(1)) == [Dates.DateTime(2009, 1, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1, 1), Dates.DateTime(2009, 1, 3, 1, 1)]

        @test t1 .- Dates.Day(1) == [Dates.Date(2008, 12, 31) Dates.Date(2009, 1, 1) Dates.Date(2009, 1, 2); Dates.Date(2009, 1, 31) Dates.Date(2009, 2, 1) Dates.Date(2009, 2, 2)]
        @test t3 .- Dates.Hour(1) == [Dates.DateTime(2008, 12, 31, 23), Dates.DateTime(2009, 1, 1, 23), Dates.DateTime(2009, 1, 2, 23)]

        @test t1 .- (Dates.Month(1) + Dates.Day(1)) == [Dates.Date(2008, 11, 30) Dates.Date(2008, 12, 1) Dates.Date(2008, 12, 2); Dates.Date(2008, 12, 31) Dates.Date(2009, 1, 1) Dates.Date(2009, 1, 2)]
        @test t3 .- (Dates.Hour(1) + Dates.Minute(1)) == [Dates.DateTime(2008, 12, 31, 22, 59), Dates.DateTime(2009, 1, 1, 22, 59), Dates.DateTime(2009, 1, 2, 22, 59)]
    end
    @testset "#20205" begin
        # ensure commutative subtraction methods are not defined
        @test_throws MethodError Dates.Day(1) .- t1
        @test_throws MethodError Dates.Hour(1) .- t3
        @test_throws MethodError (Dates.Month(1) + Dates.Day(1)) .- t1
        @test_throws MethodError (Dates.Hour(1) + Dates.Minute(1)) .- t3
    end
    @testset "deprecated" begin
        @test Dates.Date(2010, 1, 1) - t1 == [Dates.Day(365) Dates.Day(364) Dates.Day(363); Dates.Day(334) Dates.Day(333) Dates.Day(332)]
        @test t1 - Dates.Date(2010, 1, 1) == [Dates.Day(-365) Dates.Day(-364) Dates.Day(-363); Dates.Day(-334) Dates.Day(-333) Dates.Day(-332)]
        @test Dates.DateTime(2009, 1, 1) - t3 == [Dates.Millisecond(0), Dates.Millisecond(-86400000), Dates.Millisecond(-172800000)]
        @test t3 - Dates.DateTime(2009, 1, 1) == [Dates.Millisecond(0), Dates.Millisecond(86400000), Dates.Millisecond(172800000)]
        @test Dates.Day(1) + t1 == [Dates.Date(2009, 1, 2) Dates.Date(2009, 1, 3) Dates.Date(2009, 1, 4); Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4)]
        @test Dates.Hour(1) + t3 == [Dates.DateTime(2009, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1), Dates.DateTime(2009, 1, 3, 1)]
        @test t1 + Dates.Day(1) == [Dates.Date(2009, 1, 2) Dates.Date(2009, 1, 3) Dates.Date(2009, 1, 4); Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4)]
        @test t3 + Dates.Hour(1) == [Dates.DateTime(2009, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1), Dates.DateTime(2009, 1, 3, 1)]
        @test t5 + Dates.Hour(1) == [Dates.Time(1, 0, 0) Dates.Time(1, 0, 1) Dates.Time(1, 0, 2); Dates.Time(1, 1, 0) Dates.Time(1, 2, 0) Dates.Time(1, 3, 0)]
        @test (Dates.Month(1) + Dates.Day(1)) + t1 == [Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4); Dates.Date(2009, 3, 2) Dates.Date(2009, 3, 3) Dates.Date(2009, 3, 4)]
        @test (Dates.Hour(1) + Dates.Minute(1)) + t3 == [Dates.DateTime(2009, 1, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1, 1), Dates.DateTime(2009, 1, 3, 1, 1)]
        @test t1 + (Dates.Month(1) + Dates.Day(1)) == [Dates.Date(2009, 2, 2) Dates.Date(2009, 2, 3) Dates.Date(2009, 2, 4); Dates.Date(2009, 3, 2) Dates.Date(2009, 3, 3) Dates.Date(2009, 3, 4)]
        @test t3 + (Dates.Hour(1) + Dates.Minute(1)) == [Dates.DateTime(2009, 1, 1, 1, 1), Dates.DateTime(2009, 1, 2, 1, 1), Dates.DateTime(2009, 1, 3, 1, 1)]
        @test t1 - Dates.Day(1) == [Dates.Date(2008, 12, 31) Dates.Date(2009, 1, 1) Dates.Date(2009, 1, 2); Dates.Date(2009, 1, 31) Dates.Date(2009, 2, 1) Dates.Date(2009, 2, 2)]
        @test t3 - Dates.Hour(1) == [Dates.DateTime(2008, 12, 31, 23), Dates.DateTime(2009, 1, 1, 23), Dates.DateTime(2009, 1, 2, 23)]
        @test t1 - (Dates.Month(1) + Dates.Day(1)) == [Dates.Date(2008, 11, 30) Dates.Date(2008, 12, 1) Dates.Date(2008, 12, 2); Dates.Date(2008, 12, 31) Dates.Date(2009, 1, 1) Dates.Date(2009, 1, 2)]
        @test t3 - (Dates.Hour(1) + Dates.Minute(1)) == [Dates.DateTime(2008, 12, 31, 22, 59), Dates.DateTime(2009, 1, 1, 22, 59), Dates.DateTime(2009, 1, 2, 22, 59)]
        @test t2 - t1 == [Dates.Day(1) Dates.Day(31) Dates.Day(365); Dates.Day(365) Dates.Day(28) Dates.Day(1)]
        @test t4 - t3 == [Dates.Millisecond(1000), Dates.Millisecond(60000), Dates.Millisecond(3600000)]
        @test (Dates.Date(2009, 1, 1):Dates.Week(1):Dates.Date(2009, 1, 21)) - (Dates.Date(2009, 1, 1):Dates.Day(1):Dates.Date(2009, 1, 3)) == [Dates.Day(0), Dates.Day(6), Dates.Day(12)]
        @test (Dates.DateTime(2009, 1, 1, 1, 1, 1):Dates.Second(1):Dates.DateTime(2009, 1, 1, 1, 1, 3)) - (Dates.DateTime(2009, 1, 1, 1, 1):Dates.Second(1):Dates.DateTime(2009, 1, 1, 1, 1, 2)) == [Dates.Second(1), Dates.Second(1), Dates.Second(1)]
        @test_throws MethodError Dates.Day(1) - t1
        @test_throws MethodError Dates.Hour(1) - t3
        @test_throws MethodError (Dates.Month(1) + Dates.Day(1)) - t1
        @test_throws MethodError (Dates.Hour(1) + Dates.Minute(1)) - t3
    end
    @testset "TimeZone" begin
        # best we can get in Dates as there is no other tz functionality
        @test ((a, b) -> now(typeof(a))).(UTC(), [1,2,3]) isa Vector{DateTime}
    end
end

@testset "Missing arithmetic" begin
    for t ∈ [Date; Time; subtypes(DatePeriod); subtypes(TimePeriod)]
        @test ismissing(t(1) + missing)
        @test ismissing(missing + t(1))
        @test ismissing(t(1) - missing)
        @test ismissing(missing - t(1))
    end
end

end

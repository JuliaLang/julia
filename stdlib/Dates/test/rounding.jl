# This file is a part of Julia. License is MIT: https://julialang.org/license

module RoundingTests

using Test
using Dates

@testset "conversion to and from the rounding epoch (ISO 8601 year 0000)" begin
    @test Dates.epochdays2date(-1) == Dates.Date(-1, 12, 31)
    @test Dates.epochdays2date(0) == Dates.Date(0, 1, 1)
    @test Dates.epochdays2date(1) == Dates.Date(0, 1, 2)
    @test Dates.epochdays2date(736329) == Dates.Date(2016, 1, 1)
    @test Dates.epochms2datetime(-86400000) == Dates.DateTime(-1, 12, 31)
    @test Dates.epochms2datetime(0) == Dates.DateTime(0, 1, 1)
    @test Dates.epochms2datetime(86400000) == Dates.DateTime(0, 1, 2)
    @test Dates.epochms2datetime(Int64(736329) * 86400000) == Dates.DateTime(2016, 1, 1)
    @test Dates.date2epochdays(Dates.Date(-1, 12, 31)) == -1
    @test Dates.date2epochdays(Dates.Date(0, 1, 1)) == 0
    @test Dates.date2epochdays(Dates.Date(2016, 1, 1)) == 736329
    @test Dates.datetime2epochms(Dates.DateTime(-1, 12, 31)) == -86400000
    @test Dates.datetime2epochms(Dates.DateTime(0, 1, 1)) == 0
    @test Dates.datetime2epochms(Dates.DateTime(2016, 1, 1)) == Int64(736329) * 86400000
end
@testset "Basic rounding" begin
    dt = Dates.Date(2016, 2, 28)    # Sunday
    @test floor(Dates.Year, dt) == Dates.Date(2016)
    @test floor(Dates.Year(5), dt) == Dates.Date(2015)
    @test floor(Dates.Year(10), dt) == Dates.Date(2010)
    @test floor(Dates.Month, dt) == Dates.Date(2016, 2)
    @test floor(Dates.Month(6), dt) == Dates.Date(2016, 1)
    @test floor(Dates.Week, dt) == Dates.toprev(dt, Dates.Monday)
    @test ceil(Dates.Year, dt) == Dates.Date(2017)
    @test ceil(Dates.Year(5), dt) == Dates.Date(2020)
    @test ceil(Dates.Month, dt) == Dates.Date(2016, 3)
    @test ceil(Dates.Month(6), dt) == Dates.Date(2016, 7)
    @test ceil(Dates.Week, dt) == Dates.tonext(dt, Dates.Monday)
    @test round(dt, Dates.Year) == Dates.Date(2016)
    @test round(dt, Dates.Month) == Dates.Date(2016, 3)
    @test round(dt, Dates.Week) == Dates.Date(2016, 2, 29)

    dt = Dates.DateTime(2016, 2, 28, 15, 10, 50, 500)
    @test floor(Dates.Day, dt) == Dates.DateTime(2016, 2, 28)
    @test floor(Dates.Hour, dt) == Dates.DateTime(2016, 2, 28, 15)
    @test floor(Dates.Hour(2), dt) == Dates.DateTime(2016, 2, 28, 14)
    @test floor(Dates.Hour(12), dt) == Dates.DateTime(2016, 2, 28, 12)
    @test floor(Dates.Minute, dt) == Dates.DateTime(2016, 2, 28, 15, 10)
    @test floor(Dates.Minute(15), dt) == Dates.DateTime(2016, 2, 28, 15, 0)
    @test floor(Dates.Second, dt) == Dates.DateTime(2016, 2, 28, 15, 10, 50)
    @test floor(Dates.Second(30), dt) == Dates.DateTime(2016, 2, 28, 15, 10, 30)
    @test ceil(Dates.Day, dt) == Dates.DateTime(2016, 2, 29)
    @test ceil(Dates.Hour, dt) == Dates.DateTime(2016, 2, 28, 16)
    @test ceil(Dates.Hour(2), dt) == Dates.DateTime(2016, 2, 28, 16)
    @test ceil(Dates.Hour(12), dt) == Dates.DateTime(2016, 2, 29, 0)
    @test ceil(Dates.Minute, dt) == Dates.DateTime(2016, 2, 28, 15, 11)
    @test ceil(Dates.Minute(15), dt) == Dates.DateTime(2016, 2, 28, 15, 15)
    @test ceil(Dates.Second, dt) == Dates.DateTime(2016, 2, 28, 15, 10, 51)
    @test ceil(Dates.Second(30), dt) == Dates.DateTime(2016, 2, 28, 15, 11, 0)
    @test round(dt, Dates.Day) == Dates.DateTime(2016, 2, 29)
    @test round(dt, Dates.Hour) == Dates.DateTime(2016, 2, 28, 15)
    @test round(dt, Dates.Hour(2)) == Dates.DateTime(2016, 2, 28, 16)
    @test round(dt, Dates.Hour(12)) == Dates.DateTime(2016, 2, 28, 12)
    @test round(dt, Dates.Minute) == Dates.DateTime(2016, 2, 28, 15, 11)
    @test round(dt, Dates.Minute(15)) == Dates.DateTime(2016, 2, 28, 15, 15)
    @test round(dt, Dates.Second) == Dates.DateTime(2016, 2, 28, 15, 10, 51)
    @test round(dt, Dates.Second(30)) == Dates.DateTime(2016, 2, 28, 15, 11, 0)
end
@testset "Rounding for dates at the rounding epoch (year 0000)" begin
    dt = Dates.DateTime(0)
    @test floor(Dates.Year, dt) == dt
    @test floor(Dates.Month, dt) == dt
    @test floor(Dates.Week, dt) == Dates.Date(-1, 12, 27)   # Monday prior to 0000-01-01
    @test floor(Dates.Week, Dates.Date(-1, 12, 27)) == Dates.Date(-1, 12, 27)
    @test floor(Dates.Day, dt) == dt
    @test floor(Dates.Hour, dt) == dt
    @test floor(Dates.Minute, dt) == dt
    @test floor(Dates.Second, dt) == dt
    @test ceil(Dates.Year, dt) == dt
    @test ceil(Dates.Month, dt) == dt
    @test ceil(Dates.Week, dt) == Dates.Date(0, 1, 3)       # Monday following 0000-01-01
    @test ceil(Dates.Week, Dates.Date(0, 1, 3)) == Dates.Date(0, 1, 3)
    @test ceil(Dates.Day, dt) == dt
    @test ceil(Dates.Hour, dt) == dt
    @test ceil(Dates.Minute, dt) == dt
    @test ceil(Dates.Second, dt) == dt
end
@testset "Rounding for multiples of a period" begin
    # easiest to test close to rounding epoch
    dt = Dates.DateTime(0, 1, 19, 19, 19, 19, 19)
    @test floor(Dates.Year(2), dt) == DateTime(0)
    @test floor(Dates.Month(2), dt) == DateTime(0, 1)       # Odd number; months are 1-indexed
    @test floor(Dates.Week(2), dt) == DateTime(0, 1, 17)    # Third Monday of 0000
    @test floor(Dates.Day(2), dt) == DateTime(0, 1, 19)     # Odd number; days are 1-indexed
    @test floor(Dates.Hour(2), dt) == DateTime(0, 1, 19, 18)
    @test floor(Dates.Minute(2), dt) == DateTime(0, 1, 19, 19, 18)
    @test floor(Dates.Second(2), dt) == DateTime(0, 1, 19, 19, 19, 18)
    @test ceil(Dates.Year(2), dt) == DateTime(2)
    @test ceil(Dates.Month(2), dt) == DateTime(0, 3)        # Odd number; months are 1-indexed
    @test ceil(Dates.Week(2), dt) == DateTime(0, 1, 31)     # Fifth Monday of 0000
    @test ceil(Dates.Day(2), dt) == DateTime(0, 1, 21)      # Odd number; days are 1-indexed
    @test ceil(Dates.Hour(2), dt) == DateTime(0, 1, 19, 20)
    @test ceil(Dates.Minute(2), dt) == DateTime(0, 1, 19, 19, 20)
    @test ceil(Dates.Second(2), dt) == DateTime(0, 1, 19, 19, 19, 20)
end
@testset "Rounding for dates with negative years" begin
    dt = Dates.DateTime(-1, 12, 29, 19, 19, 19, 19)
    @test floor(Dates.Year(2), dt) == DateTime(-2)
    @test floor(Dates.Month(2), dt) == DateTime(-1, 11)     # Odd number; months are 1-indexed
    @test floor(Dates.Week(2), dt) == DateTime(-1, 12, 20)  # 2 weeks prior to 0000-01-03
    @test floor(Dates.Day(2), dt) == DateTime(-1, 12, 28)   # Even; 4 days prior to 0000-01-01
    @test floor(Dates.Hour(2), dt) == DateTime(-1, 12, 29, 18)
    @test floor(Dates.Minute(2), dt) == DateTime(-1, 12, 29, 19, 18)
    @test floor(Dates.Second(2), dt) == DateTime(-1, 12, 29, 19, 19, 18)
    @test ceil(Dates.Year(2), dt) == DateTime(0)
    @test ceil(Dates.Month(2), dt) == DateTime(0, 1)        # Odd number; months are 1-indexed
    @test ceil(Dates.Week(2), dt) == DateTime(0, 1, 3)      # First Monday of 0000
    @test ceil(Dates.Day(2), dt) == DateTime(-1, 12, 30)    # Even; 2 days prior to 0000-01-01
    @test ceil(Dates.Hour(2), dt) == DateTime(-1, 12, 29, 20)
    @test ceil(Dates.Minute(2), dt) == DateTime(-1, 12, 29, 19, 20)
    @test ceil(Dates.Second(2), dt) == DateTime(-1, 12, 29, 19, 19, 20)
end
@testset "Rounding for dates that should not need rounding" begin
    for dt in [Dates.DateTime(2016, 1, 1), Dates.DateTime(-2016, 1, 1)]
        local dt
        for p in [Dates.Year, Dates.Month, Dates.Day, Dates.Hour, Dates.Minute, Dates.Second]
            local p
            @test floor(p, dt) == dt
            @test ceil(p, dt) == dt
        end
    end
end
@testset "Various available RoundingModes" begin
    dt = Dates.DateTime(2016, 2, 28, 12)
    @test round(dt, Dates.Day, RoundNearestTiesUp) == Dates.DateTime(2016, 2, 29)
    @test round(dt, Dates.Day, RoundUp) == Dates.DateTime(2016, 2, 29)
    @test round(dt, Dates.Day, RoundDown) == Dates.DateTime(2016, 2, 28)
    @test_throws DomainError round(dt, Dates.Day, RoundNearest)
    @test_throws DomainError round(dt, Dates.Day, RoundNearestTiesAway)
    @test_throws DomainError round(dt, Dates.Day, RoundToZero)
    @test round(dt, Dates.Day) == round(dt, Dates.Day, RoundNearestTiesUp)
end
@testset "Rounding datetimes to invalid resolutions" begin
    dt = Dates.DateTime(2016, 2, 28, 12, 15)
    for p in [Dates.Year, Dates.Month, Dates.Week, Dates.Day, Dates.Hour]
        local p
        for v in [-1, 0]
            @test_throws DomainError floor(p(v), dt)
            @test_throws DomainError ceil(p(v), dt)
            @test_throws DomainError round(dt, p(v))
        end
    end
end
@testset "Rounding for periods" begin
    x = Dates.Second(172799)
    @test floor(Dates.Week, x) == Dates.Week(0)
    @test floor(Dates.Day, x) == Dates.Day(1)
    @test floor(Dates.Hour, x) == Dates.Hour(47)
    @test floor(Dates.Minute, x) == Dates.Minute(2879)
    @test floor(Dates.Second, x) == Dates.Second(172799)
    @test floor(Dates.Millisecond, x) == Dates.Millisecond(172799000)
    @test ceil(Dates.Week, x) == Dates.Week(1)
    @test ceil(Dates.Day, x) == Dates.Day(2)
    @test ceil(Dates.Hour, x) == Dates.Hour(48)
    @test ceil(Dates.Minute, x) == Dates.Minute(2880)
    @test ceil(Dates.Second, x) == Dates.Second(172799)
    @test ceil(Dates.Millisecond, x) == Dates.Millisecond(172799000)
    @test round(x, Dates.Week) == Dates.Week(0)
    @test round(x, Dates.Day) == Dates.Day(2)
    @test round(x, Dates.Hour) == Dates.Hour(48)
    @test round(x, Dates.Minute) == Dates.Minute(2880)
    @test round(x, Dates.Second) == Dates.Second(172799)
    @test round(x, Dates.Millisecond) == Dates.Millisecond(172799000)

    x = Dates.Nanosecond(2000999999)
    @test floor(Dates.Second, x) == Dates.Second(2)
    @test floor(Dates.Millisecond, x) == Dates.Millisecond(2000)
    @test floor(Dates.Microsecond, x) == Dates.Microsecond(2000999)
    @test floor(Dates.Nanosecond, x) == x
    @test ceil(Dates.Second, x) == Dates.Second(3)
    @test ceil(Dates.Millisecond, x) == Dates.Millisecond(2001)
    @test ceil(Dates.Microsecond, x) == Dates.Microsecond(2001000)
    @test ceil(Dates.Nanosecond, x) == x
    @test round(x, Dates.Second) == Dates.Second(2)
    @test round(x, Dates.Millisecond) == Dates.Millisecond(2001)
    @test round(x, Dates.Microsecond) == Dates.Microsecond(2001000)
    @test round(x, Dates.Nanosecond) == x
end
@testset "Rounding for periods that should not need rounding" begin
    for x in [Dates.Week(3), Dates.Day(14), Dates.Second(604800)]
        local x
        for p in [Dates.Week, Dates.Day, Dates.Hour, Dates.Second, Dates.Millisecond, Dates.Microsecond, Dates.Nanosecond]
            local p
            @test floor(p, x) == p(x)
            @test ceil(p, x) == p(x)
        end
    end
end
@testset "Various available RoundingModes for periods" begin
    x = Dates.Hour(36)
    @test round(x, Dates.Day, RoundNearestTiesUp) == Dates.Day(2)
    @test round(x, Dates.Day, RoundUp) == Dates.Day(2)
    @test round(x, Dates.Day, RoundDown) == Dates.Day(1)
    @test_throws DomainError round(x, Dates.Day, RoundNearest)
    @test_throws DomainError round(x, Dates.Day, RoundNearestTiesAway)
    @test_throws DomainError round(x, Dates.Day, RoundToZero)
    @test round(x, Dates.Day) == round(x, Dates.Day, RoundNearestTiesUp)
end
@testset "Rounding periods to invalid resolutions" begin
    x = Dates.Hour(86399)
    for p in [Dates.Week, Dates.Day, Dates.Hour, Dates.Second, Dates.Millisecond, Dates.Microsecond, Dates.Nanosecond]
        local p
        for v in [-1, 0]
            @test_throws DomainError floor(p(v), x)
            @test_throws DomainError ceil(p(v), x)
            @test_throws DomainError round(x, p(v))
        end
    end
    for p in [Dates.Year, Dates.Month]
        local p
        for v in [-1, 0, 1]
            @test_throws MethodError floor(p(v), x)
            @test_throws MethodError ceil(p(v), x)
            @test_throws DomainError round(x, p(v))
        end
    end
end

end


# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test conversion to and from the rounding epoch (ISO 8601 year 0000)
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

# Basic rounding tests
dt = Dates.Date(2016, 2, 28)    # Sunday
@test floor(dt, Dates.Year) == Dates.Date(2016)
@test floor(dt, Dates.Year(5)) == Dates.Date(2015)
@test floor(dt, Dates.Year(10)) == Dates.Date(2010)
@test floor(dt, Dates.Month) == Dates.Date(2016, 2)
@test floor(dt, Dates.Month(6)) == Dates.Date(2016, 1)
@test floor(dt, Dates.Week) == Dates.toprev(dt, Dates.Monday)
@test ceil(dt, Dates.Year) == Dates.Date(2017)
@test ceil(dt, Dates.Year(5)) == Dates.Date(2020)
@test ceil(dt, Dates.Month) == Dates.Date(2016, 3)
@test ceil(dt, Dates.Month(6)) == Dates.Date(2016, 7)
@test ceil(dt, Dates.Week) == Dates.tonext(dt, Dates.Monday)
@test round(dt, Dates.Year) == Dates.Date(2016)
@test round(dt, Dates.Month) == Dates.Date(2016, 3)
@test round(dt, Dates.Week) == Dates.Date(2016, 2, 29)

dt = Dates.DateTime(2016, 2, 28, 15, 10, 50, 500)
@test floor(dt, Dates.Day) == Dates.DateTime(2016, 2, 28)
@test floor(dt, Dates.Hour) == Dates.DateTime(2016, 2, 28, 15)
@test floor(dt, Dates.Hour(2)) == Dates.DateTime(2016, 2, 28, 14)
@test floor(dt, Dates.Hour(12)) == Dates.DateTime(2016, 2, 28, 12)
@test floor(dt, Dates.Minute) == Dates.DateTime(2016, 2, 28, 15, 10)
@test floor(dt, Dates.Minute(15)) == Dates.DateTime(2016, 2, 28, 15, 0)
@test floor(dt, Dates.Second) == Dates.DateTime(2016, 2, 28, 15, 10, 50)
@test floor(dt, Dates.Second(30)) == Dates.DateTime(2016, 2, 28, 15, 10, 30)
@test ceil(dt, Dates.Day) == Dates.DateTime(2016, 2, 29)
@test ceil(dt, Dates.Hour) == Dates.DateTime(2016, 2, 28, 16)
@test ceil(dt, Dates.Hour(2)) == Dates.DateTime(2016, 2, 28, 16)
@test ceil(dt, Dates.Hour(12)) == Dates.DateTime(2016, 2, 29, 0)
@test ceil(dt, Dates.Minute) == Dates.DateTime(2016, 2, 28, 15, 11)
@test ceil(dt, Dates.Minute(15)) == Dates.DateTime(2016, 2, 28, 15, 15)
@test ceil(dt, Dates.Second) == Dates.DateTime(2016, 2, 28, 15, 10, 51)
@test ceil(dt, Dates.Second(30)) == Dates.DateTime(2016, 2, 28, 15, 11, 0)
@test round(dt, Dates.Day) == Dates.DateTime(2016, 2, 29)
@test round(dt, Dates.Hour) == Dates.DateTime(2016, 2, 28, 15)
@test round(dt, Dates.Hour(2)) == Dates.DateTime(2016, 2, 28, 16)
@test round(dt, Dates.Hour(12)) == Dates.DateTime(2016, 2, 28, 12)
@test round(dt, Dates.Minute) == Dates.DateTime(2016, 2, 28, 15, 11)
@test round(dt, Dates.Minute(15)) == Dates.DateTime(2016, 2, 28, 15, 15)
@test round(dt, Dates.Second) == Dates.DateTime(2016, 2, 28, 15, 10, 51)
@test round(dt, Dates.Second(30)) == Dates.DateTime(2016, 2, 28, 15, 11, 0)

# Rounding for dates at the rounding epoch (year 0000)
dt = Dates.DateTime(0)
@test floor(dt, Dates.Year) == dt
@test floor(dt, Dates.Month) == dt
@test floor(dt, Dates.Week) == Dates.Date(-1, 12, 27)   # Monday prior to 0000-01-01
@test floor(Dates.Date(-1, 12, 27), Dates.Week) == Dates.Date(-1, 12, 27)
@test floor(dt, Dates.Day) == dt
@test floor(dt, Dates.Hour) == dt
@test floor(dt, Dates.Minute) == dt
@test floor(dt, Dates.Second) == dt
@test ceil(dt, Dates.Year) == dt
@test ceil(dt, Dates.Month) == dt
@test ceil(dt, Dates.Week) == Dates.Date(0, 1, 3)       # Monday following 0000-01-01
@test ceil(Dates.Date(0, 1, 3), Dates.Week) == Dates.Date(0, 1, 3)
@test ceil(dt, Dates.Day) == dt
@test ceil(dt, Dates.Hour) == dt
@test ceil(dt, Dates.Minute) == dt
@test ceil(dt, Dates.Second) == dt

# Test rounding for multiples of a period (easiest to test close to rounding epoch)
dt = Dates.DateTime(0, 1, 19, 19, 19, 19, 19)
@test floor(dt, Dates.Year(2)) == DateTime(0)
@test floor(dt, Dates.Month(2)) == DateTime(0, 1)       # Odd number; months are 1-indexed
@test floor(dt, Dates.Week(2)) == DateTime(0, 1, 17)    # Third Monday of 0000
@test floor(dt, Dates.Day(2)) == DateTime(0, 1, 19)     # Odd number; days are 1-indexed
@test floor(dt, Dates.Hour(2)) == DateTime(0, 1, 19, 18)
@test floor(dt, Dates.Minute(2)) == DateTime(0, 1, 19, 19, 18)
@test floor(dt, Dates.Second(2)) == DateTime(0, 1, 19, 19, 19, 18)
@test ceil(dt, Dates.Year(2)) == DateTime(2)
@test ceil(dt, Dates.Month(2)) == DateTime(0, 3)        # Odd number; months are 1-indexed
@test ceil(dt, Dates.Week(2)) == DateTime(0, 1, 31)     # Fifth Monday of 0000
@test ceil(dt, Dates.Day(2)) == DateTime(0, 1, 21)      # Odd number; days are 1-indexed
@test ceil(dt, Dates.Hour(2)) == DateTime(0, 1, 19, 20)
@test ceil(dt, Dates.Minute(2)) == DateTime(0, 1, 19, 19, 20)
@test ceil(dt, Dates.Second(2)) == DateTime(0, 1, 19, 19, 19, 20)

# Test rounding for dates with negative years
dt = Dates.DateTime(-1, 12, 29, 19, 19, 19, 19)
@test floor(dt, Dates.Year(2)) == DateTime(-2)
@test floor(dt, Dates.Month(2)) == DateTime(-1, 11)     # Odd number; months are 1-indexed
@test floor(dt, Dates.Week(2)) == DateTime(-1, 12, 20)  # 2 weeks prior to 0000-01-03
@test floor(dt, Dates.Day(2)) == DateTime(-1, 12, 28)   # Even; 4 days prior to 0000-01-01
@test floor(dt, Dates.Hour(2)) == DateTime(-1, 12, 29, 18)
@test floor(dt, Dates.Minute(2)) == DateTime(-1, 12, 29, 19, 18)
@test floor(dt, Dates.Second(2)) == DateTime(-1, 12, 29, 19, 19, 18)
@test ceil(dt, Dates.Year(2)) == DateTime(0)
@test ceil(dt, Dates.Month(2)) == DateTime(0, 1)        # Odd number; months are 1-indexed
@test ceil(dt, Dates.Week(2)) == DateTime(0, 1, 3)      # First Monday of 0000
@test ceil(dt, Dates.Day(2)) == DateTime(-1, 12, 30)    # Even; 2 days prior to 0000-01-01
@test ceil(dt, Dates.Hour(2)) == DateTime(-1, 12, 29, 20)
@test ceil(dt, Dates.Minute(2)) == DateTime(-1, 12, 29, 19, 20)
@test ceil(dt, Dates.Second(2)) == DateTime(-1, 12, 29, 19, 19, 20)

# Test rounding for dates that should not need rounding
for dt in [Dates.DateTime(2016, 1, 1), Dates.DateTime(-2016, 1, 1)]
    for p in [Dates.Year, Dates.Month, Dates.Day, Dates.Hour, Dates.Minute, Dates.Second]
        @test floor(dt, p) == dt
        @test ceil(dt, p) == dt
    end
end

# Test available RoundingModes
dt = Dates.DateTime(2016, 2, 28, 12)
@test round(dt, Dates.Day, RoundNearestTiesUp) == Dates.DateTime(2016, 2, 29)
@test round(dt, Dates.Day, RoundUp) == Dates.DateTime(2016, 2, 29)
@test round(dt, Dates.Day, RoundDown) == Dates.DateTime(2016, 2, 28)
@test_throws DomainError round(dt, Dates.Day, RoundNearest)
@test_throws DomainError round(dt, Dates.Day, RoundNearestTiesAway)
@test_throws DomainError round(dt, Dates.Day, RoundToZero)
@test round(dt, Dates.Day) == round(dt, Dates.Day, RoundNearestTiesUp)

# Test rounding to invalid resolutions
dt = Dates.DateTime(2016, 2, 28, 12, 15)
for p in [Dates.Year, Dates.Month, Dates.Week, Dates.Day, Dates.Hour]
    for v in [-1, 0]
        @test_throws DomainError floor(dt, p(v))
        @test_throws DomainError ceil(dt, p(v))
        @test_throws DomainError round(dt, p(v))
    end
end

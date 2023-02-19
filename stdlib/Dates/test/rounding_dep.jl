# This file is a part of Julia. License is MIT: https://julialang.org/license

module RoundingTestsDep

using Test
using Dates

@testset "Rounding DateTime to Date" begin
    now_ = DateTime(2020, 9, 1, 13)
    for p in (Year, Month, Day)
        for r in (RoundUp, RoundDown)
            @test round(Date, now_, p, r) == round(Date(now_), p, r)
        end
        @test round(Date, now_, p) == round(Date, now_, p, RoundNearestTiesUp)
        @test floor(Date, now_, p) == round(Date, now_, p, RoundDown)
        @test ceil(Date, now_, p)  == round(Date, now_, p, RoundUp)
    end
end
@testset "Rounding for periods that should not need rounding" begin
    for x in [Dates.Week(3), Dates.Day(14), Dates.Second(604800)]
        local x
        for p in [Dates.Week, Dates.Day, Dates.Hour, Dates.Second, Dates.Millisecond, Dates.Microsecond, Dates.Nanosecond]
            local p
            @test floor(x, p) == p(x)
            @test ceil(x, p) == p(x)
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
            @test_throws DomainError floor(x, p(v))
            @test_throws DomainError ceil(x, p(v))
            @test_throws DomainError round(x, p(v))
        end
    end
    for p in [Dates.Year, Dates.Month]
        local p
        for v in [-1, 0, 1]
            @test_throws MethodError floor(x, p(v))
            @test_throws MethodError ceil(x, p(v))
            @test_throws DomainError round(x, p(v))
        end
    end
end

end

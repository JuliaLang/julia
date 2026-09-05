# This file is a part of Julia. License is MIT: https://julialang.org/license

module TimestampTests

using Test
using Dates

# Test the nanosecond Timestamp API.

@testset "Construction and validation" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    @test ts isa Timestamp
    @test Timestamp(2026) == Timestamp(2026, 1, 1)
    @test Timestamp(2026, 8) == Timestamp(2026, 8, 1)
    # the unix epoch is the zero instant
    @test Dates.value(Timestamp(1970)) == 0
    @test Timestamp(Dates.UTN(0)) == Timestamp(1970)
    # 24:00 rolls over like DateTime
    @test Timestamp(2026, 8, 31, 24) == Timestamp(2026, 9, 1)
    # AM/PM
    @test Timestamp(2026, 8, 31, 12, 0, 0, 0, 0, 0, Dates.PM) == Timestamp(2026, 8, 31, 12)
    @test Timestamp(2026, 8, 31, 12, 0, 0, 0, 0, 0, Dates.AM) == Timestamp(2026, 8, 31, 0)
    # from Periods, in any order
    @test Timestamp(Year(2026), Month(8), Day(31)) == Timestamp(2026, 8, 31)
    @test Timestamp(Hour(1)) == Timestamp(1970, 1, 1, 1)  # parts default to the unix epoch
    @test Timestamp(Nanosecond(789), Year(2026), Millisecond(123)) ==
        Timestamp(2026, 1, 1, 0, 0, 0, 123, 0, 789)
    @test Timestamp(Year(2026), Month(8), Day(31), Hour(13), Minute(45), Second(30),
                    Millisecond(123), Microsecond(456), Nanosecond(789)) == ts
    # from Date/Time
    @test Timestamp(Date(2026, 8, 31)) == Timestamp(2026, 8, 31)
    @test Timestamp(Date(2026, 8, 31), Time(13, 45, 30, 123, 456, 789)) == ts
    # argument validation
    @test_throws ArgumentError Timestamp(2026, 13, 1)
    @test_throws ArgumentError Timestamp(2026, 2, 30)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 25)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 60)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 0, 60)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 0, 0, 1000)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 0, 0, 0, 1000)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 0, 0, 0, 0, 1000000000)
    # ns may carry a full fractional second as long as the total stays below 1s
    @test Timestamp(2026, 1, 1, 0, 0, 0, 0, 0, 999999999) ==
        Timestamp(2026, 1, 1, 0, 0, 0, 999, 999, 999)
    @test_throws ArgumentError Timestamp(2026, 1, 1, 0, 0, 0, 1, 0, 999999999)
    # out-of-range instants throw ArgumentError, not overflow silently
    @test_throws ArgumentError Timestamp(1677, 9, 20)
    @test_throws ArgumentError Timestamp(2262, 4, 12)
    @test_throws ArgumentError Timestamp(1000)
    @test_throws ArgumentError Timestamp(3000)
    @test_throws ArgumentError Timestamp(Date(3000))
    # years are bounds-checked before any calendar arithmetic could wrap into range
    wrapped_year = Int64(202021879422134420)
    @test Dates.validargs(Timestamp, wrapped_year, Int64(1), Int64(1), zeros(Int64, 6)...) isa ArgumentError
    @test_throws ArgumentError Timestamp(wrapped_year)
    @test tryparse(Timestamp, string(wrapped_year)) === nothing
    # the exact boundary instants are constructible
    @test Timestamp(2262, 4, 11, 23, 47, 16, 854, 775, 807) == typemax(Timestamp)
    @test Timestamp(1677, 9, 21, 0, 12, 43, 145, 224, 192) == typemin(Timestamp)
    @test_throws ArgumentError Timestamp(2262, 4, 11, 23, 47, 16, 854, 775, 808)
    @test_throws ArgumentError Timestamp(1677, 9, 21, 0, 12, 43, 145, 224, 191)
end

@testset "Accessors" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    @test year(ts) == 2026
    @test month(ts) == 8
    @test day(ts) == 31
    @test hour(ts) == 13
    @test minute(ts) == 45
    @test second(ts) == 30
    @test millisecond(ts) == 123
    @test microsecond(ts) == 456
    @test nanosecond(ts) == 789
    @test yearmonthday(ts) == (2026, 8, 31)
    @test week(ts) == 36
    @test dayofweek(ts) == Dates.Monday
    @test quarterofyear(ts) == 3
    @test dayname(ts) == "Monday"
    @test dayofyear(ts) == 243
    @test Dates.days(ts) == Dates.days(Date(2026, 8, 31))
    # pre-1970 instants (negative values) decompose correctly
    ts2 = Timestamp(1969, 12, 31, 23, 59, 59, 999, 999, 999)
    @test Dates.value(ts2) == -1
    @test (year(ts2), month(ts2), day(ts2)) == (1969, 12, 31)
    @test (hour(ts2), minute(ts2), second(ts2)) == (23, 59, 59)
    @test (millisecond(ts2), microsecond(ts2), nanosecond(ts2)) == (999, 999, 999)
    # Period extraction constructors
    @test Year(ts) == Year(2026)
    @test Month(ts) == Month(8)
    @test Day(ts) == Day(31)
    @test Hour(ts) == Hour(13)
    @test Minute(ts) == Minute(45)
    @test Second(ts) == Second(30)
    @test Millisecond(ts) == Millisecond(123)
    @test Microsecond(ts) == Microsecond(456)
    @test Nanosecond(ts) == Nanosecond(789)
    @test eps(Timestamp) == Nanosecond(1)
    @test eps(ts) == Nanosecond(1)
    @test zero(Timestamp) == Nanosecond(0)
    @test isfinite(Timestamp) && isfinite(ts)
end

@testset "Conversions and promotion" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    dt = DateTime(2026, 8, 31, 13, 45, 30, 123)
    # DateTime -> Timestamp is exact
    @test Timestamp(dt) == Timestamp(2026, 8, 31, 13, 45, 30, 123)
    @test convert(Timestamp, dt) == Timestamp(dt)
    # Timestamp -> DateTime floors to the millisecond
    @test DateTime(ts) == dt
    @test DateTime(Timestamp(1969, 12, 31, 23, 59, 59, 999, 999, 999)) ==
        DateTime(1969, 12, 31, 23, 59, 59, 999)
    @test Date(ts) == Date(2026, 8, 31)
    @test Time(ts) == Time(13, 45, 30, 123, 456, 789)
    # out-of-range DateTime -> Timestamp throws
    @test_throws InexactError Timestamp(DateTime(300000, 1, 1))
    @test_throws InexactError convert(Timestamp, DateTime(1600, 1, 1))
    # instants beyond DateTime's own range must not wrap into range either
    @test_throws InexactError Timestamp(DateTime(Dates.UTM(typemin(Int64) + Dates.UNIXEPOCH)))
    # raw value conversions (unix nanoseconds)
    @test convert(Nanosecond, ts) == Nanosecond(Dates.value(ts))
    @test convert(Timestamp, Nanosecond(Dates.value(ts))) == ts
    # unix conversions
    @test unix2timestamp(0) == Timestamp(1970)
    @test unix2timestamp(86400) == Timestamp(1970, 1, 2)
    @test timestamp2unix(Timestamp(1970, 1, 2)) == 86400.0
    @test unix2timestamp(timestamp2unix(Timestamp(2026, 8, 31))) == Timestamp(2026, 8, 31)
    @test unix2timestamp(9223372036) == Timestamp(2262, 4, 11, 23, 47, 16)
    @test_throws InexactError unix2timestamp(9223372037)
    @test_throws InexactError unix2timestamp(typemin(Int64))
    @test_throws InexactError unix2timestamp(9223372037.0)
    @test Dates.datetime2rata(ts) == Dates.datetime2rata(Date(2026, 8, 31))
    # promotion
    @test promote(dt, ts) isa Tuple{Timestamp, Timestamp}
    @test promote(Date(2026), Timestamp(2026)) isa Tuple{Timestamp, Timestamp}
    @test vcat([dt], [ts]) isa Vector{Timestamp}
end

@testset "Comparisons" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    dt = DateTime(2026, 8, 31, 13, 45, 30, 123)
    @test ts > dt
    @test dt < ts
    @test ts != dt
    @test Timestamp(dt) == dt
    @test dt == Timestamp(dt)
    @test Timestamp(2026, 8, 31) == Date(2026, 8, 31)
    @test Date(2026, 8, 31) == Timestamp(2026, 8, 31)
    @test ts != Date(2026, 8, 31)
    @test Date(2026, 8, 31) < ts
    @test ts < Date(2026, 9, 1)
    # comparisons against instants far outside the Timestamp range must not throw
    @test DateTime(300000, 1, 1) > ts
    @test DateTime(-300000, 1, 1) < ts
    @test Date(300000, 1, 1) > ts
    @test Date(-300000, 1, 1) < ts
    @test !(DateTime(300000, 1, 1) == ts)
    @test !(ts == Date(300000, 1, 1))
    @test isless(ts, DateTime(300000, 1, 1))
    @test min(ts, DateTime(300000, 1, 1)) === ts
    @test isoyear(Timestamp(2021, 1, 1)) == Year(2020)
    @test isoweekdate(Timestamp(2021, 1, 1)) == (2020, 53, 5)
    # total order within the type
    a = Timestamp(2026, 1, 1)
    @test sort([a + Nanosecond(2), a, a + Nanosecond(1)]) ==
        [a, a + Nanosecond(1), a + Nanosecond(2)]
end

@testset "Arithmetic" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    # fixed periods are exact to the nanosecond
    @test ts + Nanosecond(1) - Nanosecond(1) == ts
    @test ts + Microsecond(3) == Timestamp(2026, 8, 31, 13, 45, 30, 123, 459, 789)
    @test ts + Millisecond(900) == Timestamp(2026, 8, 31, 13, 45, 31, 23, 456, 789)
    @test ts + Second(30) == Timestamp(2026, 8, 31, 13, 46, 0, 123, 456, 789)
    @test ts + Hour(11) == Timestamp(2026, 9, 1, 0, 45, 30, 123, 456, 789)
    @test ts + Day(1) == Timestamp(2026, 9, 1, 13, 45, 30, 123, 456, 789)
    @test ts + Week(1) == Timestamp(2026, 9, 7, 13, 45, 30, 123, 456, 789)
    @test Nanosecond(1) + ts == ts + Nanosecond(1)
    # calendar periods clamp the day like DateTime and carry sub-second parts
    @test ts + Month(6) == Timestamp(2027, 2, 28, 13, 45, 30, 123, 456, 789)
    @test ts - Month(6) == Timestamp(2026, 2, 28, 13, 45, 30, 123, 456, 789)
    @test ts + Year(1) == Timestamp(2027, 8, 31, 13, 45, 30, 123, 456, 789)
    @test ts - Year(1) == Timestamp(2025, 8, 31, 13, 45, 30, 123, 456, 789)
    @test ts + Quarter(1) == Timestamp(2026, 11, 30, 13, 45, 30, 123, 456, 789)
    @test ts - Quarter(1) == Timestamp(2026, 5, 31, 13, 45, 30, 123, 456, 789)
    # leap-day clamping
    @test Timestamp(2024, 2, 29, 0, 0, 0, 0, 0, 1) + Year(1) ==
        Timestamp(2025, 2, 28, 0, 0, 0, 0, 0, 1)
    # differences are Nanosecond
    @test (ts + Nanosecond(5)) - ts == Nanosecond(5)
    @test ts - Timestamp(2026, 8, 31) == Nanosecond(49530123456789)
    @test ts - DateTime(2026, 8, 31, 13, 45, 30, 123) == Nanosecond(456789)
    @test DateTime(2026, 8, 31, 13, 45, 30, 123) - ts == Nanosecond(-456789)
    # Timestamp - Date is a MethodError, matching DateTime - Date
    @test_throws MethodError ts - Date(2026, 8, 31)
    # compound periods
    @test ts + (Hour(1) + Minute(30)) == Timestamp(2026, 8, 31, 15, 15, 30, 123, 456, 789)
    @test ts + Day(1) + Hour(1) == Timestamp(2026, 9, 1, 14, 45, 30, 123, 456, 789)
    # like DateTime, arithmetic is fixed-point and wraps at the ends of the range
    @test typemax(Timestamp) + Nanosecond(1) == typemin(Timestamp)
    @test typemin(Timestamp) - Nanosecond(1) == typemax(Timestamp)
    @test typemax(Timestamp) - typemin(Timestamp) == Nanosecond(-1)
    @test typemax(Timestamp) - Month(1) == Timestamp(2262, 3, 11, 23, 47, 16, 854, 775, 807)
    @test typemax(Timestamp) + Month(1) == typemin(Timestamp) + Day(30) - Nanosecond(1)
    @test typemin(Timestamp) - Year(1) == typemax(Timestamp) - Day(365) + Nanosecond(1)
    # broadcast
    @test [ts, ts] .+ Nanosecond(1) == [ts + Nanosecond(1), ts + Nanosecond(1)]
end

@testset "Rounding and truncation" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    @test floor(ts, Hour) == Timestamp(2026, 8, 31, 13)
    @test floor(ts, Minute(15)) == Timestamp(2026, 8, 31, 13, 45)
    @test floor(ts, Second) == Timestamp(2026, 8, 31, 13, 45, 30)
    @test floor(ts, Millisecond) == Timestamp(2026, 8, 31, 13, 45, 30, 123)
    @test floor(ts, Microsecond) == Timestamp(2026, 8, 31, 13, 45, 30, 123, 456)
    @test floor(ts, Nanosecond) == ts
    @test floor(ts, Day) == Timestamp(2026, 8, 31)
    @test floor(ts, Week) == Timestamp(2026, 8, 31) # a Monday
    @test floor(ts, Month) == Timestamp(2026, 8)
    @test floor(ts, Quarter) == Timestamp(2026, 7)
    @test floor(ts, Year) == Timestamp(2026)
    @test ceil(ts, Minute) == Timestamp(2026, 8, 31, 13, 46)
    @test ceil(ts, Month) == Timestamp(2026, 9)
    @test round(ts, Second) == Timestamp(2026, 8, 31, 13, 45, 30)
    @test round(ts, Hour) == Timestamp(2026, 8, 31, 14)
    @test_throws DomainError floor(ts, Nanosecond(-1))
    # rounding to time periods wraps like the arithmetic it is built from, so
    # results near the ends of the range are right whenever they are representable
    @test ceil(typemin(Timestamp), Nanosecond(10)) == typemin(Timestamp) + Nanosecond(8)
    @test ceil(typemin(Timestamp), Hour) == Timestamp(1677, 9, 21, 1)
    @test round(typemin(Timestamp), Minute) == Timestamp(1677, 9, 21, 0, 13)
    @test floor(typemax(Timestamp), Minute) == Timestamp(2262, 4, 11, 23, 47)
    @test ceil(typemin(Timestamp), Day) == Timestamp(1677, 9, 22)
    # rounding agrees with DateTime for any period (shared 0000-01-01 anchor)
    dt = DateTime(2026, 8, 31, 13, 45, 30, 123)
    for p in (Hour(7), Minute(11), Second(17), Hour(1), Day(1), Millisecond(21))
        @test DateTime(floor(Timestamp(dt), p)) == floor(dt, p)
    end
    # pre-1970 rounding (floored, not truncated toward zero)
    @test floor(Timestamp(1969, 12, 31, 23, 59, 59, 999), Second) ==
        Timestamp(1969, 12, 31, 23, 59, 59)
    # trunc
    @test trunc(ts, Year) == Timestamp(2026)
    @test trunc(ts, Quarter) == Timestamp(2026, 7)
    @test trunc(ts, Month) == Timestamp(2026, 8)
    @test trunc(ts, Day) == Timestamp(2026, 8, 31)
    @test trunc(ts, Hour) == Timestamp(2026, 8, 31, 13)
    @test trunc(ts, Minute) == Timestamp(2026, 8, 31, 13, 45)
    @test trunc(ts, Second) == Timestamp(2026, 8, 31, 13, 45, 30)
    @test trunc(ts, Millisecond) == Timestamp(2026, 8, 31, 13, 45, 30, 123)
    @test trunc(ts, Microsecond) == Timestamp(2026, 8, 31, 13, 45, 30, 123, 456)
    @test trunc(ts, Nanosecond) == ts
end

@testset "Adjusters" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    @test firstdayofweek(ts) == Timestamp(2026, 8, 31)
    @test lastdayofweek(ts) == Timestamp(2026, 9, 6)
    @test firstdayofmonth(ts) == Timestamp(2026, 8, 1)
    @test lastdayofmonth(ts) == Timestamp(2026, 8, 31)
    @test firstdayofyear(ts) == Timestamp(2026, 1, 1)
    @test lastdayofyear(ts) == Timestamp(2026, 12, 31)
    @test firstdayofquarter(ts) == Timestamp(2026, 7, 1)
    @test lastdayofquarter(ts) == Timestamp(2026, 9, 30)
    @test tonext(d -> dayofweek(d) == Dates.Thursday, ts) ==
        Timestamp(2026, 9, 3, 13, 45, 30, 123, 456, 789)
    @test toprev(d -> dayofweek(d) == Dates.Friday, ts) ==
        Timestamp(2026, 8, 28, 13, 45, 30, 123, 456, 789)
    # function-based adjuster constructors, mirroring DateTime's arity ladder
    starts = (
        (2026,),
        (2026, 8),
        (2026, 8, 31),
        (2026, 8, 31, 13),
        (2026, 8, 31, 13, 45),
        (2026, 8, 31, 13, 45, 30),
        (2026, 8, 31, 13, 45, 30, 123),
        (2026, 8, 31, 13, 45, 30, 123, 456),
    )
    default_steps = (Day(1), Day(1), Hour(1), Minute(1), Second(1),
                     Millisecond(1), Microsecond(1), Nanosecond(1))
    for (args, step) in zip(starts, default_steps)
        start = Timestamp(args...)
        @test Timestamp(t -> t == start + step, args...; limit=2) == start + step
    end
    @test Timestamp(t -> hour(t) == 12, 2026, 8, 31; step=Hour(1)) == Timestamp(2026, 8, 31, 12)
    @test Timestamp(t -> second(t) == 40, 2026, 8, 31, 10; step=Second(1)) ==
        Timestamp(2026, 8, 31, 10, 0, 40)
    @test Timestamp(t -> microsecond(t) == 3, 2026, 8, 31, 10, 0, 0, 0; step=Microsecond(1)) ==
        Timestamp(2026, 8, 31, 10, 0, 0, 0, 3)
    @test Timestamp(t -> nanosecond(t) == 4, 2026, 8, 31, 10, 0, 0, 0, 0; step=Nanosecond(1)) ==
        Timestamp(2026, 8, 31, 10, 0, 0, 0, 0, 4)
    @test_throws ArgumentError Timestamp(t -> false, 2026, 8, 31; limit=3)
    # the predicate is validated at the supplied start, including the partial lower-bound year
    @test Timestamp(t -> true, 1677, 9, 21, 0, 12, 43, 145, 225) ==
        Timestamp(1677, 9, 21, 0, 12, 43, 145, 225)
    @test Timestamp(t -> t == typemax(Timestamp), 2262, 4, 11, 23, 47, 16, 854, 775;
                    step=Nanosecond(1), limit=808) == typemax(Timestamp)
end

@testset "Ranges" begin
    a = Timestamp(2026, 1, 1)
    r = a:Nanosecond(250):(a + Microsecond(1))
    @test length(r) == 5
    @test collect(r) == [a + Nanosecond(250i) for i in 0:4]
    @test (a + Nanosecond(500)) in r
    @test !((a + Nanosecond(400)) in r)
    r2 = a:Month(1):Timestamp(2026, 12, 31)
    @test length(r2) == 12
    @test last(collect(r2)) == Timestamp(2026, 12, 1)
    @test length(a:Day(1):Timestamp(2026, 1, 31)) == 31
    @test isempty(a:Nanosecond(1):(a - Nanosecond(1)))
    # calendar-stepped ranges may end within one step of typemax
    @test length(Timestamp(2262, 1, 1):Month(1):Timestamp(2262, 4, 11)) == 4
    # step counts are computed exactly, beyond Float64's 2^53 integer precision
    r53 = a:Nanosecond(1):(a + Nanosecond(Int64(2)^55 + 5))
    @test length(r53) == Int64(2)^55 + 6
    @test last(r53) == a + Nanosecond(Int64(2)^55 + 5)
    @test reverse(a:Nanosecond(250):(a + Microsecond(1))) == (a + Microsecond(1)):Nanosecond(-250):a
    @test (a:Nanosecond(250):(a + Microsecond(1)))[2:3] == [a + Nanosecond(250), a + Nanosecond(500)]
end

@testset "Parsing and formatting" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    # default ISO round trip
    @test string(ts) == "2026-08-31T13:45:30.123456789"
    @test Timestamp(string(ts)) == ts
    @test sprint(show, ts) == "Dates.Timestamp(\"2026-08-31T13:45:30.123456789\")"
    @test sprint(show, MIME("text/plain"), ts) == "2026-08-31T13:45:30.123456789"
    # trailing zeros are stripped, and stripped forms parse back
    @test string(Timestamp(2026, 8, 31)) == "2026-08-31T00:00:00"
    @test string(Timestamp(2026, 8, 31, 1, 2, 3, 500)) == "2026-08-31T01:02:03.5"
    @test string(Timestamp(2026, 8, 31, 1, 2, 3, 0, 0, 5)) == "2026-08-31T01:02:03.000000005"
    @test Timestamp("2026-08-31T01:02:03.5") == Timestamp(2026, 8, 31, 1, 2, 3, 500)
    @test Timestamp("2026-08-31T13:45:30") == Timestamp(2026, 8, 31, 13, 45, 30)
    @test Timestamp("2026-08-31") == Timestamp(2026, 8, 31)
    # digits past the ninth must be zero, as with `s` past the third
    @test Timestamp("2026-08-31T00:00:00.1234567890") ==
        Timestamp(2026, 8, 31, 0, 0, 0, 123, 456, 789)
    @test_throws ArgumentError Timestamp("2026-08-31T00:00:00.1234567891")
    # custom formats, including the millisecond `s` code
    @test Timestamp("31/08/2026 13:45", "dd/mm/yyyy HH:MM") == Timestamp(2026, 8, 31, 13, 45)
    @test Timestamp("2026-08-31T13:45:30.123", dateformat"yyyy-mm-dd\THH:MM:SS.s") ==
        Timestamp(2026, 8, 31, 13, 45, 30, 123)
    @test Timestamp("2026-08-31 1:45pm", "yyyy-mm-dd I:MMp") == Timestamp(2026, 8, 31, 13, 45)
    @test Dates.format(ts, "yyyy-mm-dd HH:MM:SS.n") == "2026-08-31 13:45:30.123456789"
    @test Dates.format(Timestamp(2026, 8, 31, 1, 2, 3, 500), "HH:MM:SS.n") == "01:02:03.5"
    @test Dates.format(Timestamp(2026, 8, 31, 1, 2, 3, 500), "HH:MM:SS.nnnnnnnnn") ==
        "01:02:03.500000000"
    @test Dates.format(ts, ISOTimestampFormat) == "2026-08-31T13:45:30.123456789"
    @test Timestamp(Dates.format(ts, ISOTimestampFormat)) == ts
    # parse/tryparse API
    @test parse(Timestamp, "2026-08-31T13:45:30.123456789") == ts
    @test tryparse(Timestamp, "2026-08-31T13:45:30.123456789") == ts
    @test tryparse(Timestamp, "2026-99-31") === nothing
    @test tryparse(Timestamp, "3000-01-01") === nothing
    @test tryparse(Timestamp, "1000-01-01") === nothing
    @test tryparse(Timestamp, "garbage") === nothing
    @test tryparse(Timestamp, "18446744073709553636-01-01") === nothing
    long_zero_fraction = "2020-01-01T00:00:00.1" * "0"^72
    @test tryparse(Timestamp, long_zero_fraction) == Timestamp(2020, 1, 1, 0, 0, 0, 100)
    long_nonzero_fraction = "2020-01-01T00:00:00.123456789" * "0"^64 * "1"
    @test tryparse(Timestamp, long_nonzero_fraction) === nothing
    @test_throws ArgumentError parse(Timestamp, "3000-01-01")
    @test_throws ArgumentError Timestamp("")
    # Timestamp values format with DateTime-compatible codes
    @test Dates.format(ts, "yyyy-mm-dd HH:MM:SS.s") == "2026-08-31 13:45:30.123"
    @test Dates.format(ts, "e, dd u yyyy HH:MM:SS") == "Mon, 31 Aug 2026 13:45:30"
end

@testset "now and today" begin
    ts = now(Timestamp)
    @test ts isa Timestamp
    @test abs(DateTime(ts) - now()) < Millisecond(60000)
    tsu = now(Timestamp, UTC)
    @test tsu isa Timestamp
    @test abs(DateTime(tsu) - now(UTC)) < Millisecond(60000)
end

@testset "Data layout" begin
    ts = Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)
    @test isbitstype(Timestamp)
    @test sizeof(Timestamp) == 8
    v = [ts, ts + Nanosecond(1)]
    @test reinterpret(Int64, v) == [Dates.value(ts), Dates.value(ts) + 1]
    @test reinterpret(Timestamp, reinterpret(Int64, v)) == v
    @test hash(ts) == hash(Timestamp(Dates.UTN(Dates.value(ts))))
    @test hash(ts) != hash(ts + Nanosecond(1))
    d = Dict(ts => 1)
    @test d[Timestamp(2026, 8, 31, 13, 45, 30, 123, 456, 789)] == 1
    # equal instants hash alike across Date, DateTime, and Timestamp
    @test hash(Timestamp(2026, 8, 31)) == hash(DateTime(2026, 8, 31)) == hash(Date(2026, 8, 31))
    @test hash(Timestamp(2026, 8, 31, 13, 45, 30, 123)) == hash(DateTime(2026, 8, 31, 13, 45, 30, 123))
    mixed = Dict{Any, Int}(Date(2026, 8, 31) => 1, DateTime(2026, 8, 31, 12) => 2)
    @test mixed[Timestamp(2026, 8, 31)] == 1
    @test mixed[Timestamp(2026, 8, 31, 12)] == 2
    @test mixed[DateTime(2026, 8, 31)] == 1
end

end

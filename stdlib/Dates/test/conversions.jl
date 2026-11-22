# This file is a part of Julia. License is MIT: https://julialang.org/license

module ConversionTests

using Test
using Dates

@testset "conversion to/from UNIX" begin
    # The ends of DateTime lie further from the epochs than Int64 milliseconds reach
    for dt in (DateTime(Dates.UTM(typemin(Int64))), DateTime(Dates.UTM(typemax(Int64))))
        @test Dates.datetime2unix(dt) == Float64(big(Dates.value(dt)) - Dates.UNIXEPOCH) / 1000
        @test Dates.datetime2julian(dt) == Float64(big(Dates.value(dt)) - Dates.JULIANEPOCH) / 86400000
        @test Dates.julian2datetime((big(Dates.value(dt)) - Dates.JULIANEPOCH) // 86400000) == dt
        x = (big(Dates.value(dt)) - Dates.UNIXEPOCH) // 1000
        @test Dates.unix2datetime(x) == dt
        @test_throws InexactError Dates.unix2datetime(x + sign(Dates.value(dt)) // 1000)
    end
    s = Int64(cld(big(typemin(Int64)) - Dates.UNIXEPOCH, 1000))
    @test Dates.unix2datetime(s) == Dates.unix2datetime(s // 1) == DateTime(Dates.UTM(Dates.UNIXEPOCH + 1000 * Int128(s)))
    @test_throws InexactError Dates.unix2datetime(s - 1)
    # Wide integers and rationals scale without overflow
    for x in (Int128(1) << 125, -(Int128(1) << 125), typemax(UInt128), big(2)^100, typemax(Int128) // 3, big(2)^200 // 3)
        @test_throws InexactError Dates.unix2datetime(x)
        @test_throws InexactError Dates.julian2datetime(x)
    end
    @test Dates.unix2datetime(Int128(1095379200)) == DateTime(2004, 9, 17)
    @test Dates.unix2datetime(typemax(Int128) // (typemax(Int128) - 1)) == DateTime(1970, 1, 1, 0, 0, 1)
    @test Dates.unix2datetime(typemax(UInt128) // (typemax(UInt128) - 1)) == DateTime(1970, 1, 1, 0, 0, 1)
    @test Dates.unix2datetime(-4381516797 // 4) == Dates.unix2datetime(-1095379199.25)
    @test Dates.julian2datetime(typemax(Int128) // (typemax(Int128) - 1)) == DateTime(-4713, 11, 25, 12)
    @test Dates.julian2datetime(1 // 172800000) == Dates.julian2datetime(0)
    @test Dates.julian2datetime(3 // 172800000) == Dates.julian2datetime(0) + Dates.Millisecond(2)
    # Results in local time are checked too, beyond any time zone offset. Windows localtime
    # rejects times before 1970 and leaves the fields invalid, and TmStruct takes an Int.
    @test_throws (Sys.iswindows() || Sys.WORD_SIZE == 32 ? ArgumentError : InexactError) Dates.unix2datetime(s - 2 * 86400; localtime = true)
    # localtime_r fails for years beyond a C int, which TmStruct does not report
    x = (Int128(Dates.value(Date(1970 + Int64(2)^32))) - Dates.UNIXEPOCHDAYS) * 86400
    @test_throws ArgumentError Dates.unix2datetime(x; localtime = true)
    @test Dates.unix2datetime(1095379199.75; localtime = true) - Dates.unix2datetime(1095379199.75) ==
        Dates.unix2datetime(1095379199; localtime = true) - Dates.unix2datetime(1095379199)
    # Test conversion to and from unix
    @test Dates.unix2datetime(Dates.datetime2unix(DateTime(2000, 1, 1))) == DateTime(2000, 1, 1)
    @test Dates.value(Dates.DateTime(1970)) == Dates.UNIXEPOCH

    # Tests from here: https://en.wikipedia.org/wiki/Unix_time
    @test string(Dates.unix2datetime(1095379198.75)) == string("2004-09-16T23:59:58.750")
    @test string(Dates.unix2datetime(1095379199.00)) == string("2004-09-16T23:59:59")
    @test string(Dates.unix2datetime(1095379199.25)) == string("2004-09-16T23:59:59.250")
    @test string(Dates.unix2datetime(1095379199.50)) == string("2004-09-16T23:59:59.500")
    @test string(Dates.unix2datetime(1095379199.75)) == string("2004-09-16T23:59:59.750")
    @test string(Dates.unix2datetime(1095379200.00)) == string("2004-09-17T00:00:00")
    @test string(Dates.unix2datetime(1095379200.25)) == string("2004-09-17T00:00:00.250")
    @test string(Dates.unix2datetime(1095379200.50)) == string("2004-09-17T00:00:00.500")
    @test string(Dates.unix2datetime(1095379200.75)) == string("2004-09-17T00:00:00.750")
    @test string(Dates.unix2datetime(1095379201.00)) == string("2004-09-17T00:00:01")
    @test string(Dates.unix2datetime(1095379201.25)) == string("2004-09-17T00:00:01.250")
    @test string(Dates.unix2datetime(915148798.75)) == string("1998-12-31T23:59:58.750")
    @test string(Dates.unix2datetime(915148799.00)) == string("1998-12-31T23:59:59")
    @test string(Dates.unix2datetime(915148799.25)) == string("1998-12-31T23:59:59.250")
    @test string(Dates.unix2datetime(915148799.50)) == string("1998-12-31T23:59:59.500")
    @test string(Dates.unix2datetime(915148799.75)) == string("1998-12-31T23:59:59.750")
    @test string(Dates.unix2datetime(915148800.00)) == string("1999-01-01T00:00:00")
    @test string(Dates.unix2datetime(915148800.25)) == string("1999-01-01T00:00:00.250")
    @test string(Dates.unix2datetime(915148800.50)) == string("1999-01-01T00:00:00.500")
    @test string(Dates.unix2datetime(915148800.75)) == string("1999-01-01T00:00:00.750")
    @test string(Dates.unix2datetime(915148801.00)) == string("1999-01-01T00:00:01")
    @test string(Dates.unix2datetime(915148801.25)) == string("1999-01-01T00:00:01.250")
end

@testset "conversions with localtime=true" begin
    unix2localdatetime(x::Real) = DateTime(Libc.TmStruct(floor(x))) + Millisecond(round(Int, (x - floor(x)) * 1000))
    @test unix2datetime(1095379198.75, localtime=true) == unix2localdatetime(1095379198.75)
end

@testset "conversion to/from Rata Die" begin
    @test Date(Dates.rata2datetime(734869)) == Dates.Date(2013, 1, 1)
    @test Dates.datetime2rata(Dates.rata2datetime(734869)) == 734869
    # Days outside DateTime do not wrap around, and integral non-integers are days too
    lim = fld(typemax(Int64), 86400000)
    @test Dates.rata2datetime(lim) == DateTime(Date(Dates.UTD(lim)))
    @test Dates.rata2datetime(-lim) == DateTime(Date(Dates.UTD(-lim)))
    for d in (lim + 1, -lim - 1, typemax(Int64), typemin(Int64), Int128(1) << 100)
        @test_throws InexactError Dates.rata2datetime(d)
    end
    @test Dates.rata2datetime(734869.0) == Dates.rata2datetime(734869 // 1) == Dates.rata2datetime(734869)
    @test Dates.rata2datetime(3e9) == Dates.rata2datetime(3_000_000_000)
    @test_throws InexactError Dates.rata2datetime(734869.5)
end

@testset "conversion to/from Julian calendar" begin
    # Tests from here: http://mysite.verizon.net/aesir_research/date/back.htm#JDN
    @test Dates.julian2datetime(1721119.5) == Dates.DateTime(0, 3, 1)
    @test Dates.julian2datetime(1721424.5) == Dates.DateTime(0, 12, 31)
    @test Dates.julian2datetime(1721425.5) == Dates.DateTime(1, 1, 1)
    @test Dates.julian2datetime(2299149.5) == Dates.DateTime(1582, 10, 4)
    @test Dates.julian2datetime(2415020.5) == Dates.DateTime(1900, 1, 1)
    @test Dates.julian2datetime(2415385.5) == Dates.DateTime(1901, 1, 1)
    @test Dates.julian2datetime(2440587.5) == Dates.DateTime(1970, 1, 1)
    @test Dates.julian2datetime(2444239.5) == Dates.DateTime(1980, 1, 1)
    @test Dates.julian2datetime(2452695.625) == Dates.DateTime(2003, 2, 25, 3)
    @test Dates.datetime2julian(Dates.DateTime(2013, 12, 3, 21)) == 2456630.375
end
@testset "now, today, and UTC" begin
    @test typeof(Dates.now()) <: Dates.DateTime
    @test typeof(Dates.today()) <: Dates.Date
    @test typeof(Dates.now(Dates.UTC)) <: Dates.DateTime

    if Sys.isapple()
        withenv("TZ" => "UTC") do
            a = Dates.now()
            b = Dates.now(Dates.UTC)
            c = Dates.now()
            @test a <= b <= c
        end
    end
    a = Dates.now()
    b = now(Dates.UTC)
    c = Dates.now()
    @test abs(a - b) < Dates.Hour(16) + abs(c - a)
end
@testset "Issue #9171, #9169" begin
    let t = Dates.Period[Dates.Week(2), Dates.Day(14), Dates.Hour(14 * 24), Dates.Minute(14 * 24 * 60), Dates.Second(14 * 24 * 60 * 60), Dates.Millisecond(14 * 24 * 60 * 60 * 1000)]
        for i = 1:length(t)
            Pi = typeof(t[i])
            for j = 1:length(t)
                @test t[i] == t[j]
            end
            for j = i+1:length(t)
                Pj = typeof(t[j])
                tj1 = t[j] + Pj(1)
                @test t[i] < tj1
                @test_throws InexactError Pi(tj1)
                @test_throws InexactError Pj(Pi(typemax(Int64)))
                @test_throws InexactError Pj(Pi(typemin(Int64)))
            end
        end
    end
    @test Dates.Year(3) == Dates.Month(36)
    @test_throws MethodError Int(Dates.Month(36))
    @test Dates.Year(3) < Dates.Month(37)
    @test_throws InexactError convert(Dates.Year, Dates.Month(37))
    @test_throws InexactError Dates.Month(Dates.Year(typemax(Int64)))
end

@testset "Conversions to/from numbers" begin
    # Ensure that conversion of 32-bit integers work
    let dt = DateTime(1915, 1, 1, 12)
        unix = Int32(Dates.datetime2unix(dt))
        julian = Int32(Dates.datetime2julian(dt))

        @test Dates.unix2datetime(unix) == dt
        @test Dates.julian2datetime(julian) == dt
    end

    a = Dates.DateTime(2000)
    b = Dates.Date(2000)
    @test Dates.value(b) == 730120
    @test Dates.value(a) == 63082368000000
    @test convert(Dates.DateTime, Dates.Millisecond(63082368000000)) == a
    @test convert(Dates.Millisecond, a) == Dates.Millisecond(63082368000000)
    @test Dates.DateTime(Dates.UTM(63082368000000)) == a
    @test Dates.DateTime(Dates.UTM(63082368000000.0)) == a
    @test convert(Dates.Date, Dates.Day(730120)) == b
    @test convert(Dates.Day, b) == Dates.Day(730120)
    @test Dates.Date(Dates.UTD(730120)) == b
    @test Dates.Date(Dates.UTD(730120.0)) == b
    @test Dates.Date(Dates.UTD(Int32(730120))) == b

    dt = Dates.DateTime(2000, 1, 1, 23, 59, 59, 50)
    t = Dates.Time(dt)
    @test Dates.hour(t) == 23
    @test Dates.minute(t) == 59
    @test Dates.second(t) == 59
    @test Dates.millisecond(t) == 50
    @test Dates.microsecond(t) == 0
    @test Dates.nanosecond(t) == 0
end

end

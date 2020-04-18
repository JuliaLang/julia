# This file is a part of Julia. License is MIT: https://julialang.org/license

module TypesTest

using Test
using Dates

# Date internal algorithms
@testset "totaldays" begin
    @test Dates.totaldays(0, 2, 28) == -307
    @test Dates.totaldays(0, 2, 29) == -306
    @test Dates.totaldays(0, 3, 1) == -305
    @test Dates.totaldays(0, 12, 31) == 0
    # Rata Die Days # start from 0001-01-01
    @test Dates.totaldays(1, 1, 1) == 1
    @test Dates.totaldays(1, 1, 2) == 2
    @test Dates.totaldays(2013, 1, 1) == 734869
end
@testset "daysinmonth" begin
    @test Dates.daysinmonth(2000, 1) == 31
    @test Dates.daysinmonth(2000, 2) == 29
    @test Dates.daysinmonth(2000, 3) == 31
    @test Dates.daysinmonth(2000, 4) == 30
    @test Dates.daysinmonth(2000, 5) == 31
    @test Dates.daysinmonth(2000, 6) == 30
    @test Dates.daysinmonth(2000, 7) == 31
    @test Dates.daysinmonth(2000, 8) == 31
    @test Dates.daysinmonth(2000, 9) == 30
    @test Dates.daysinmonth(2000, 10) == 31
    @test Dates.daysinmonth(2000, 11) == 30
    @test Dates.daysinmonth(2000, 12) == 31
    @test Dates.daysinmonth(2001, 2) == 28
end
@testset "isleapyear" begin
    @test Dates.isleapyear(1900) == false
    @test Dates.isleapyear(2000) == true
    @test Dates.isleapyear(2004) == true
    @test Dates.isleapyear(2008) == true
    @test Dates.isleapyear(0) == true
    @test Dates.isleapyear(1) == false
    @test Dates.isleapyear(-1) == false
    @test Dates.isleapyear(4) == true
    @test Dates.isleapyear(-4) == true
end
# Create "test" check manually
y = Dates.Year(1)
m = Dates.Month(1)
w = Dates.Week(1)
d = Dates.Day(1)
h = Dates.Hour(1)
mi = Dates.Minute(1)
s = Dates.Second(1)
ms = Dates.Millisecond(1)
@testset "DateTime construction by parts" begin
    test = Dates.DateTime(Dates.UTM(63492681600000))
    @test Dates.DateTime(2013) == test
    @test Dates.DateTime(2013, 1) == test
    @test Dates.DateTime(2013, 1, 1) == test
    @test Dates.DateTime(2013, 1, 1, 0) == test
    @test Dates.DateTime(2013, 1, 1, 0, 0) == test
    @test Dates.DateTime(2013, 1, 1, 0, 0, 0) == test
    @test Dates.DateTime(2013, 1, 1, 0, 0, 0, 0) == test

    @test Dates.DateTime(y) == Dates.DateTime(1)
    @test Dates.DateTime(y, m) == Dates.DateTime(1, 1)
    @test Dates.DateTime(y, m, d) == Dates.DateTime(1, 1, 1)
    @test Dates.DateTime(y, m, d, h) == Dates.DateTime(1, 1, 1, 1)
    @test Dates.DateTime(y, m, d, h, mi) == Dates.DateTime(1, 1, 1, 1, 1)
    @test Dates.DateTime(y, m, d, h, mi, s) == Dates.DateTime(1, 1, 1, 1, 1, 1)
    @test Dates.DateTime(y, m, d, h, mi, s, ms) == Dates.DateTime(1, 1, 1, 1, 1, 1, 1)
    @test Dates.DateTime(Dates.Day(10), Dates.Month(2), y) == Dates.DateTime(1, 2, 10)
    @test Dates.DateTime(Dates.Second(10), Dates.Month(2), y, Dates.Hour(4)) == Dates.DateTime(1, 2, 1, 4, 0, 10)
    @test Dates.DateTime(Dates.Year(1), Dates.Month(2), Dates.Day(1),
                         Dates.Hour(4), Dates.Second(10)) == Dates.DateTime(1, 2, 1, 4, 0, 10)
end

@testset "Date construction by parts" begin
    test = Dates.Date(Dates.UTD(734869))
    @test Dates.Date(2013) == test
    @test Dates.Date(2013, 1) == test
    @test Dates.Date(2013, 1, 1) == test
    @test Dates.Date(y) == Dates.Date(1)
    @test Dates.Date(y, m) == Dates.Date(1, 1)
    @test Dates.Date(y, m, d) == Dates.Date(1, 1, 1)
    @test Dates.Date(m) == Dates.Date(1, 1, 1)
    @test Dates.Date(d, y) == Dates.Date(1, 1, 1)
    @test Dates.Date(d, m) == Dates.Date(1, 1, 1)
    @test Dates.Date(m, y) == Dates.Date(1, 1, 1)
    @test Dates.Date(Dates.Day(10), Dates.Month(2), y) == Dates.Date(1, 2, 10)
end

@testset "Time construction by parts" begin
    t = Dates.Time(Dates.Nanosecond(82800000000000))
    @test Dates.Time(23) == t
    @test Dates.Time(23, 0) == t
    @test Dates.Time(23, 0, 0) == t
    @test Dates.Time(23, 0, 0, 0) == t
    @test Dates.Time(23, 0, 0, 0, 0) == t
    @test Dates.Time(23, 0, 0, 0, 0, 0) == t
    us = Dates.Microsecond(1)
    ns = Dates.Nanosecond(1)
    @test Dates.Time(h) == Dates.Time(1)
    @test Dates.Time(h, mi) == Dates.Time(1, 1)
    @test Dates.Time(h, mi, s) == Dates.Time(1, 1, 1)
    @test Dates.Time(h, mi, s, ms) == Dates.Time(1, 1, 1, 1)
    @test Dates.Time(h, mi, s, ms, us) == Dates.Time(1, 1, 1, 1, 1)
    @test Dates.Time(h, mi, s, ms, us, ns) == Dates.Time(1, 1, 1, 1, 1, 1)
    @test Dates.Time(us, h, s, ns, mi, ms) == Dates.Time(1, 1, 1, 1, 1, 1)
    @test Dates.Time(Dates.Second(10), Dates.Minute(2), us, Dates.Hour(4)) == Dates.Time(4, 2, 10, 0, 1)
    @test Dates.Time(Dates.Hour(4), Dates.Second(10), Dates.Millisecond(15),
                     Dates.Microsecond(20), Dates.Nanosecond(25)) == Dates.Time(4, 0, 10, 15, 20, 25)
end

@testset "various input types for Date/DateTime" begin
    test = Dates.Date(1, 1, 1)
    @test Dates.Date(Int8(1), Int8(1), Int8(1)) == test
    @test Dates.Date(UInt8(1), UInt8(1), UInt8(1)) == test
    @test Dates.Date(Int16(1), Int16(1), Int16(1)) == test
    @test Dates.Date(UInt8(1), UInt8(1), UInt8(1)) == test
    @test Dates.Date(Int32(1), Int32(1), Int32(1)) == test
    @test Dates.Date(UInt32(1), UInt32(1), UInt32(1)) == test
    @test Dates.Date(Int64(1), Int64(1), Int64(1)) == test
    @test Dates.Date('\x01', '\x01', '\x01') == test
    @test Dates.Date(true, true, true) == test
    @test_throws ArgumentError Dates.Date(false, true, false)
    @test Dates.Date(false, true, true) == test - Dates.Year(1)
    @test_throws ArgumentError Dates.Date(true, true, false)
    @test Dates.Date(UInt64(1), UInt64(1), UInt64(1)) == test
    @test Dates.Date(-1, UInt64(1), UInt64(1)) == test - Dates.Year(2)
    @test Dates.Date(Int128(1), Int128(1), Int128(1)) == test
    @test_throws InexactError Dates.Date(170141183460469231731687303715884105727, Int128(1), Int128(1))
    @test Dates.Date(UInt128(1), UInt128(1), UInt128(1)) == test
    @test Dates.Date(big(1), big(1), big(1)) == test
    @test Dates.Date(big(1), big(1), big(1)) == test
    # Potentially won't work if can't losslessly convert to Int64
    @test Dates.Date(BigFloat(1), BigFloat(1), BigFloat(1)) == test
    @test Dates.Date(complex(1), complex(1), complex(1)) == test
    @test Dates.Date(Float64(1), Float64(1), Float64(1)) == test
    @test Dates.Date(Float32(1), Float32(1), Float32(1)) == test
    @test Dates.Date(Float16(1), Float16(1), Float16(1)) == test
    @test Dates.Date(Rational(1), Rational(1), Rational(1)) == test
    @test_throws InexactError Dates.Date(BigFloat(1.2), BigFloat(1), BigFloat(1))
    @test_throws InexactError Dates.Date(1 + im, complex(1), complex(1))
    @test_throws InexactError Dates.Date(1.2, 1.0, 1.0)
    @test_throws InexactError Dates.Date(1.2f0, 1.f0, 1.f0)
    @test_throws InexactError Dates.Date(3//4, Rational(1), Rational(1)) == test

    # Months, days, hours, minutes, seconds, and milliseconds must be in range
    @test_throws ArgumentError Dates.Date(2013, 0, 1)
    @test_throws ArgumentError Dates.Date(2013, 13, 1)
    @test_throws ArgumentError Dates.Date(2013, 1, 0)
    @test_throws ArgumentError Dates.Date(2013, 1, 32)
    @test_throws ArgumentError Dates.DateTime(2013, 0, 1)
    @test_throws ArgumentError Dates.DateTime(2013, 13, 1)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 0)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 32)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 25)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, -1)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, -1)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, 60)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, 0, -1)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, 0, 60)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, 0, 0, -1)
    @test_throws ArgumentError Dates.DateTime(2013, 1, 1, 0, 0, 0, 1000)
    @test_throws ArgumentError Dates.Time(24)
    @test_throws ArgumentError Dates.Time(-1)
    @test_throws ArgumentError Dates.Time(0, -1)
    @test_throws ArgumentError Dates.Time(0, 60)
    @test_throws ArgumentError Dates.Time(0, 0, -1)
    @test_throws ArgumentError Dates.Time(0, 0, 60)
    @test_throws ArgumentError Dates.Time(0, 0, 0, -1)
    @test_throws ArgumentError Dates.Time(0, 0, 0, 1000)
    @test_throws ArgumentError Dates.Time(0, 0, 0, 0, -1)
    @test_throws ArgumentError Dates.Time(0, 0, 0, 0, 1000)
    @test_throws ArgumentError Dates.Time(0, 0, 0, 0, 0, -1)
    @test_throws ArgumentError Dates.Time(0, 0, 0, 0, 0, 1000)
end
a = Dates.DateTime(2000)
b = Dates.Date(2000)
c = Dates.Time(0)
@testset "DateTime traits" begin
    @test Dates.calendar(a) == Dates.ISOCalendar
    @test Dates.calendar(b) == Dates.ISOCalendar
    @test eps(DateTime) == Dates.Millisecond(1)
    @test eps(Date) == Dates.Day(1)
    @test eps(Time) == Dates.Nanosecond(1)
    @test eps(a) == Dates.Millisecond(1)
    @test eps(b) == Dates.Day(1)
    @test eps(c) == Dates.Nanosecond(1)
    @test string(typemax(Dates.DateTime)) == "146138512-12-31T23:59:59"
    @test string(typemin(Dates.DateTime)) == "-146138511-01-01T00:00:00"
    @test typemax(Dates.DateTime) - typemin(Dates.DateTime) == Dates.Millisecond(9223372017043199000)
    @test string(typemax(Dates.Date)) == "252522163911149-12-31"
    @test string(typemin(Dates.Date)) == "-252522163911150-01-01"
    @test string(typemax(Dates.Time)) == "23:59:59.999999999"
    @test string(typemin(Dates.Time)) == "00:00:00"
    @test isfinite(Dates.Date)
    @test isfinite(Dates.DateTime)
    @test isfinite(Dates.Time)
    @test c == c
    @test c == (c + Dates.Hour(24))
    @test hash(c) == hash(c + Dates.Hour(24))
    @test hash(c + Dates.Nanosecond(10)) == hash(c + Dates.Hour(24) + Dates.Nanosecond(10))
end
@testset "Date-DateTime conversion/promotion" begin
    global a, b, c, d
    @test Dates.DateTime(a) == a
    @test Dates.Date(a) == b
    @test Dates.DateTime(b) == a
    @test Dates.Date(b) == b
    @test a == b
    @test a == a
    @test b == a
    @test b == b
    @test !(a < b)
    @test !(b < a)
    c = Dates.DateTime(2000)
    d = Dates.Date(2000)
    @test ==(a, c)
    @test ==(c, a)
    @test ==(d, b)
    @test ==(b, d)
    @test ==(a, d)
    @test ==(d, a)
    @test ==(b, c)
    @test ==(c, b)
    b = Dates.Date(2001)
    @test b > a
    @test a < b
    @test a != b
    @test Dates.Date(Dates.DateTime(Dates.Date(2012, 7, 1))) == Dates.Date(2012, 7, 1)
end

@testset "min and max" begin
    for (a, b) in [(Dates.Date(2000), Dates.Date(2001)),
                    (Dates.Time(10), Dates.Time(11)),
                    (Dates.DateTime(3000), Dates.DateTime(3001)),
                    (Dates.Week(42), Dates.Week(1972)),
                    (Dates.Quarter(3), Dates.Quarter(52))]
        @test min(a, b) == a
        @test min(b, a) == a
        @test min(a) == a
        @test max(a, b) == b
        @test max(b, a) == b
        @test max(b) == b
        @test minmax(a, b) == (a, b)
        @test minmax(b, a) == (a, b)
        @test minmax(a) == (a, a)
    end
end

@testset "issue #31524" begin
    dt1 = Libc.strptime("%Y-%M-%dT%H:%M:%SZ", "2018-11-16T10:26:14Z")
    dt2 = Base.Libc.TmStruct(14, 30, 5, 10, 1, 99, 3, 40, 0)

    time = Time(dt1)
    @test typeof(time) == Time
    @test time == Dates.Time(10, 26, 14)

    date = Date(dt2)
    @test typeof(date) == Date
    @test date == Dates.Date(1999, 2, 10)

    datetime = DateTime(dt2)
    @test typeof(datetime) == DateTime
    @test datetime == Dates.DateTime(1999, 2, 10, 5, 30, 14)

end

@testset "timedwait" begin
    @test timedwait(() -> false, Second(0); pollint=Millisecond(1)) === :timed_out
end

end

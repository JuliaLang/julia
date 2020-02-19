# This file is a part of Julia. License is MIT: https://julialang.org/license

module PeriodsTest

using Dates
using Test

@testset "basic arithmetic" begin
    @test -Dates.Year(1) == Dates.Year(-1)
    @test Dates.Year(1) > Dates.Year(0)
    @test (Dates.Year(1) < Dates.Year(0)) == false
    @test Dates.Year(1) == Dates.Year(1)
    @test Dates.Year(1) != 1
    @test Dates.Year(1) + Dates.Year(1) == Dates.Year(2)
    @test Dates.Year(1) - Dates.Year(1) == zero(Dates.Year)
    @test 1 == one(Dates.Year)
    @test_throws MethodError Dates.Year(1) * Dates.Year(1) == Dates.Year(1)
    t = Dates.Year(1)
    t2 = Dates.Year(2)
    @test ([t, t, t, t, t] .+ Dates.Year(1)) == ([t2, t2, t2, t2, t2])
    @test (Dates.Year(1) .+ [t, t, t, t, t]) == ([t2, t2, t2, t2, t2])
    @test ([t2, t2, t2, t2, t2] .- Dates.Year(1)) == ([t, t, t, t, t])
    @test_throws MethodError ([t, t, t, t, t] .* Dates.Year(1)) == ([t, t, t, t, t])
    @test ([t, t, t, t, t] * 1) == ([t, t, t, t, t])
    @test ([t, t, t, t, t] .% t2) == ([t, t, t, t, t])
    @test div.([t, t, t, t, t], Dates.Year(1)) == ([1, 1, 1, 1, 1])
    @test mod.([t, t, t, t, t], Dates.Year(2)) == ([t, t, t, t, t])
    @test [t, t, t] / t2 == [0.5, 0.5, 0.5]
    @test abs(-t) == t
    @test sign(t) == sign(t2) == 1
    @test sign(-t) == sign(-t2) == -1
    @test sign(Dates.Year(0)) == 0
end
@testset "div/mod/gcd/lcm/rem" begin
    @test Dates.Year(10) % Dates.Year(4) == Dates.Year(2)
    @test gcd(Dates.Year(10), Dates.Year(4)) == Dates.Year(2)
    @test lcm(Dates.Year(10), Dates.Year(4)) == Dates.Year(20)
    @test div(Dates.Year(10), Dates.Year(3)) == 3
    @test div(Dates.Year(10), Dates.Year(4)) == 2
    @test div(Dates.Year(10), 4) == Dates.Year(2)
    @test Dates.Year(10) / Dates.Year(4) == 2.5

    @test mod(Dates.Year(10), Dates.Year(4)) == Dates.Year(2)
    @test mod(Dates.Year(-10), Dates.Year(4)) == Dates.Year(2)
    @test mod(Dates.Year(10), 4) == Dates.Year(2)
    @test mod(Dates.Year(-10), 4) == Dates.Year(2)

    @test rem(Dates.Year(10), Dates.Year(4)) == Dates.Year(2)
    @test rem(Dates.Year(-10), Dates.Year(4)) == Dates.Year(-2)
    @test rem(Dates.Year(10), 4) == Dates.Year(2)
    @test rem(Dates.Year(-10), 4) == Dates.Year(-2)
end

y = Dates.Year(1)
m = Dates.Month(1)
w = Dates.Week(1)
d = Dates.Day(1)
h = Dates.Hour(1)
mi = Dates.Minute(1)
s = Dates.Second(1)
ms = Dates.Millisecond(1)
us = Dates.Microsecond(1)
ns = Dates.Nanosecond(1)
emptyperiod = ((y + d) - d) - y
@testset "Period arithmetic" begin
    @test Dates.Year(y) == y
    @test Dates.Month(m) == m
    @test Dates.Week(w) == w
    @test Dates.Day(d) == d
    @test Dates.Hour(h) == h
    @test Dates.Minute(mi) == mi
    @test Dates.Second(s) == s
    @test Dates.Millisecond(ms) == ms
    @test Dates.Microsecond(us) == us
    @test Dates.Nanosecond(ns) == ns
    @test Dates.Year(convert(Int8, 1)) == y
    @test Dates.Year(convert(UInt8, 1)) == y
    @test Dates.Year(convert(Int16, 1)) == y
    @test Dates.Year(convert(UInt16, 1)) == y
    @test Dates.Year(convert(Int32, 1)) == y
    @test Dates.Year(convert(UInt32, 1)) == y
    @test Dates.Year(convert(Int64, 1)) == y
    @test Dates.Year(convert(UInt64, 1)) == y
    @test Dates.Year(convert(Int128, 1)) == y
    @test Dates.Year(convert(UInt128, 1)) == y
    @test Dates.Year(convert(BigInt, 1)) == y
    @test Dates.Year(convert(BigFloat, 1)) == y
    @test Dates.Year(convert(Complex, 1)) == y
    @test Dates.Year(convert(Rational, 1)) == y
    @test Dates.Year(convert(Float16, 1)) == y
    @test Dates.Year(convert(Float32, 1)) == y
    @test Dates.Year(convert(Float64, 1)) == y
    @test y == y
    @test m == m
    @test w == w
    @test d == d
    @test h == h
    @test mi == mi
    @test s == s
    @test ms == ms
    @test us == us
    @test ns == ns
    y2 = Dates.Year(2)
    @test y < y2
    @test y2 > y
    @test y != y2

    @test Dates.Year(Int8(1)) == y
    @test Dates.Year(UInt8(1)) == y
    @test Dates.Year(Int16(1)) == y
    @test Dates.Year(UInt16(1)) == y
    @test Dates.Year(Int(1)) == y
    @test Dates.Year(UInt(1)) == y
    @test Dates.Year(Int64(1)) == y
    @test Dates.Year(UInt64(1)) == y
    @test Dates.Year(UInt128(1)) == y
    @test Dates.Year(UInt128(1)) == y
    @test Dates.Year(big(1)) == y
    @test Dates.Year(BigFloat(1)) == y
    @test Dates.Year(float(1)) == y
    @test Dates.Year(Float32(1)) == y
    @test Dates.Year(Rational(1)) == y
    @test Dates.Year(complex(1)) == y
    @test_throws InexactError Dates.Year(BigFloat(1.2)) == y
    @test_throws InexactError Dates.Year(1.2) == y
    @test_throws InexactError Dates.Year(Float32(1.2)) == y
    @test_throws InexactError Dates.Year(3//4) == y
    @test_throws InexactError Dates.Year(complex(1.2)) == y
    @test_throws InexactError Dates.Year(Float16(1.2)) == y
    @test Dates.Year(true) == y
    @test Dates.Year(false) != y
    @test_throws MethodError Dates.Year(:hey) == y
    @test Dates.Year(real(1)) == y
    @test_throws InexactError Dates.Year(m) == y
    @test_throws MethodError Dates.Year(w) == y
    @test_throws MethodError Dates.Year(d) == y
    @test_throws MethodError Dates.Year(h) == y
    @test_throws MethodError Dates.Year(mi) == y
    @test_throws MethodError Dates.Year(s) == y
    @test_throws MethodError Dates.Year(ms) == y
    @test Dates.Year(Dates.Date(2013, 1, 1)) == Dates.Year(2013)
    @test Dates.Year(Dates.DateTime(2013, 1, 1)) == Dates.Year(2013)
    @test typeof(y + m) <: Dates.CompoundPeriod
    @test typeof(m + y) <: Dates.CompoundPeriod
    @test typeof(y + w) <: Dates.CompoundPeriod
    @test typeof(y + d) <: Dates.CompoundPeriod
    @test typeof(y + h) <: Dates.CompoundPeriod
    @test typeof(y + mi) <: Dates.CompoundPeriod
    @test typeof(y + s) <: Dates.CompoundPeriod
    @test typeof(y + ms) <: Dates.CompoundPeriod
    @test typeof(y + us) <: Dates.CompoundPeriod
    @test typeof(y + ns) <: Dates.CompoundPeriod
    @test y > m
    @test d < w
    @test mi < h
    @test ms < h
    @test ms < mi
    @test us < ms
    @test ns < ms
    @test ns < us
    @test ns < w
    @test us < w
    @test typemax(Dates.Year) == Dates.Year(typemax(Int64))
    @test typemax(Dates.Year) + y == Dates.Year(-9223372036854775808)
    @test typemin(Dates.Year) == Dates.Year(-9223372036854775808)
end
@testset "Period-Real arithmetic" begin
    @test_throws MethodError y + 1 == Dates.Year(2)
    @test_throws MethodError y + true == Dates.Year(2)
    @test_throws InexactError y + Dates.Year(1.2)
    @test y + Dates.Year(1f0) == Dates.Year(2)
    @test y * 4 == Dates.Year(4)
    @test y * 4f0 == Dates.Year(4)
    @test Dates.Year(2) * 0.5 == y
    @test Dates.Year(2) * 3//2 == Dates.Year(3)
    @test_throws InexactError y * 0.5
    @test_throws InexactError y * 3//4
    @test Dates.Year(4) / 2 == Dates.Year(2)
    @test Dates.Year(4) / 2f0 == Dates.Year(2)
    @test Dates.Year(4) / 0.5 == Dates.Year(8)
    @test Dates.Year(4) / 2//3 == Dates.Year(6)
    @test_throws InexactError Dates.Year(4) / 3.0
    @test_throws InexactError Dates.Year(4) / 3//2
    @test div(y, 2) == Dates.Year(0)
    @test_throws MethodError div(2, y) == Dates.Year(2)
    @test div(y, y) == 1
    @test y*10 % Dates.Year(5) == Dates.Year(0)
    @test_throws MethodError (y > 3) == false
    @test_throws MethodError (4 < y) == false
    @test 1 != y
    t = [y, y, y, y, y]
    @test t .+ Dates.Year(2) == [Dates.Year(3), Dates.Year(3), Dates.Year(3), Dates.Year(3), Dates.Year(3)]

    let x = Dates.Year(5), y = Dates.Year(2)
        @test div(x, y) * y + rem(x, y) == x
        @test fld(x, y) * y + mod(x, y) == x
    end
end
@testset "Associativity" begin
    dt = Dates.DateTime(2012, 12, 21)
    test = ((((((((dt + y) - m) + w) - d) + h) - mi) + s) - ms)
    @test test == dt + y - m + w - d + h - mi + s - ms
    @test test == y - m + w - d + dt + h - mi + s - ms
    @test test == dt - m + y - d + w - mi + h - ms + s
    @test test == dt + (y - m + w - d + h - mi + s - ms)
    @test test == dt + y - m + w - d + (h - mi + s - ms)
    @test (dt + Dates.Year(4)) + Dates.Day(1) == dt + (Dates.Year(4) + Dates.Day(1))
    @test Dates.Date(2014, 1, 29) + Dates.Month(1) + Dates.Day(1) + Dates.Month(1) + Dates.Day(1) ==
        Dates.Date(2014, 1, 29) + Dates.Day(1) + Dates.Month(1) + Dates.Month(1) + Dates.Day(1)
    @test Dates.Date(2014, 1, 29) + Dates.Month(1) + Dates.Day(1) == Dates.Date(2014, 1, 29) + Dates.Day(1) + Dates.Month(1)
end
@testset "traits" begin
    @test Dates._units(Dates.Year(0)) == " years"
    @test Dates._units(Dates.Year(1)) == " year"
    @test Dates._units(Dates.Year(-1)) == " year"
    @test Dates._units(Dates.Year(2)) == " years"
    @test Dates.string(Dates.Year(0)) == "0 years"
    @test Dates.string(Dates.Year(1)) == "1 year"
    @test Dates.string(Dates.Year(-1)) == "-1 year"
    @test Dates.string(Dates.Year(2)) == "2 years"
    @test zero(Dates.Year) == Dates.Year(0)
    @test zero(Dates.Year(10)) == Dates.Year(0)
    @test zero(Dates.Month) == Dates.Month(0)
    @test zero(Dates.Month(10)) == Dates.Month(0)
    @test zero(Dates.Day) == Dates.Day(0)
    @test zero(Dates.Day(10)) == Dates.Day(0)
    @test zero(Dates.Hour) == Dates.Hour(0)
    @test zero(Dates.Hour(10)) == Dates.Hour(0)
    @test zero(Dates.Minute) == Dates.Minute(0)
    @test zero(Dates.Minute(10)) == Dates.Minute(0)
    @test zero(Dates.Second) == Dates.Second(0)
    @test zero(Dates.Second(10)) == Dates.Second(0)
    @test zero(Dates.Millisecond) == Dates.Millisecond(0)
    @test zero(Dates.Millisecond(10)) == Dates.Millisecond(0)
    @test Dates.Year(-1) < Dates.Year(1)
    @test !(Dates.Year(-1) > Dates.Year(1))
    @test Dates.Year(1) == Dates.Year(1)
    @test Dates.Year(1) != 1
    @test 1 != Dates.Year(1)
    @test Dates.Month(-1) < Dates.Month(1)
    @test !(Dates.Month(-1) > Dates.Month(1))
    @test Dates.Month(1) == Dates.Month(1)
    @test Dates.Day(-1) < Dates.Day(1)
    @test !(Dates.Day(-1) > Dates.Day(1))
    @test Dates.Day(1) == Dates.Day(1)
    @test Dates.Hour(-1) < Dates.Hour(1)
    @test !(Dates.Hour(-1) > Dates.Hour(1))
    @test Dates.Hour(1) == Dates.Hour(1)
    @test Dates.Minute(-1) < Dates.Minute(1)
    @test !(Dates.Minute(-1) > Dates.Minute(1))
    @test Dates.Minute(1) == Dates.Minute(1)
    @test Dates.Second(-1) < Dates.Second(1)
    @test !(Dates.Second(-1) > Dates.Second(1))
    @test Dates.Second(1) == Dates.Second(1)
    @test Dates.Millisecond(-1) < Dates.Millisecond(1)
    @test !(Dates.Millisecond(-1) > Dates.Millisecond(1))
    @test Dates.Millisecond(1) == Dates.Millisecond(1)
    @test_throws MethodError Dates.Year(1) < Dates.Millisecond(1)
    @test_throws MethodError Dates.Millisecond(1) < Dates.Year(1)

    # issue #27076
    @test Dates.Year(1) != Dates.Millisecond(1)
    @test Dates.Millisecond(1) != Dates.Year(1)
end

struct Beat <: Dates.Period
    value::Int64
end

Beat(p::Period) = Beat(Dates.toms(p) ÷ 86400)

@testset "comparisons with new subtypes of Period" begin
    # https://en.wikipedia.org/wiki/Swatch_Internet_Time
    Dates.value(b::Beat) = b.value
    Dates.toms(b::Beat) = Dates.value(b) * 86400
    Dates._units(b::Beat) = " beat" * (abs(Dates.value(b)) == 1 ? "" : "s")
    Base.promote_rule(::Type{Dates.Day}, ::Type{Beat}) = Dates.Millisecond
    Base.convert(::Type{T}, b::Beat) where {T<:Dates.Millisecond} = T(Dates.toms(b))

    @test Beat(1000) == Dates.Day(1)
    @test Beat(1) < Dates.Day(1)
end

@testset "basic properties" begin
    @test Dates.Year("1") == y
    @test Dates.Month("1") == m
    @test Dates.Week("1") == w
    @test Dates.Day("1") == d
    @test Dates.Hour("1") == h
    @test Dates.Minute("1") == mi
    @test Dates.Second("1") == s
    @test Dates.Millisecond("1") == ms
    @test Dates.Microsecond("1") == us
    @test Dates.Nanosecond("1") == ns
    @test_throws ArgumentError Dates.Year("1.0")
    @test Dates.Year(parse(Float64, "1.0")) == y

    dt = Dates.DateTime(2014)
    @test typeof(Dates.Year(dt)) <: Dates.Year
    @test typeof(Dates.Month(dt)) <: Dates.Month
    @test typeof(Dates.Week(dt)) <: Dates.Week
    @test typeof(Dates.Day(dt)) <: Dates.Day
    @test typeof(Dates.Hour(dt)) <: Dates.Hour
    @test typeof(Dates.Minute(dt)) <: Dates.Minute
    @test typeof(Dates.Second(dt)) <: Dates.Second
    @test typeof(Dates.Millisecond(dt)) <: Dates.Millisecond
end
@testset "Default values" begin
    @test Dates.default(Dates.Year) == y
    @test Dates.default(Dates.Month) == m
    @test Dates.default(Dates.Week) == w
    @test Dates.default(Dates.Day) == d
    @test Dates.default(Dates.Hour) == zero(Dates.Hour)
    @test Dates.default(Dates.Minute) == zero(Dates.Minute)
    @test Dates.default(Dates.Second) == zero(Dates.Second)
    @test Dates.default(Dates.Millisecond) == zero(Dates.Millisecond)
    @test Dates.default(Dates.Microsecond) == zero(Dates.Microsecond)
    @test Dates.default(Dates.Nanosecond) == zero(Dates.Nanosecond)
end
@testset "Conversions" begin
    @test Dates.toms(ms) == Dates.value(Dates.Millisecond(ms)) == 1
    @test Dates.toms(s)  == Dates.value(Dates.Millisecond(s)) == 1000
    @test Dates.toms(mi) == Dates.value(Dates.Millisecond(mi)) == 60000
    @test Dates.toms(h)  == Dates.value(Dates.Millisecond(h)) == 3600000
    @test Dates.toms(d)  == Dates.value(Dates.Millisecond(d)) == 86400000
    @test Dates.toms(w)  == Dates.value(Dates.Millisecond(w)) == 604800000

    @test Dates.days(ms) == Dates.days(s) == Dates.days(mi) == Dates.days(h) == 0
    @test Dates.days(Dates.Millisecond(86400000)) == 1
    @test Dates.days(Dates.Second(86400)) == 1
    @test Dates.days(Dates.Minute(1440)) == 1
    @test Dates.days(Dates.Hour(24)) == 1
    @test Dates.days(d) == 1
    @test Dates.days(w) == 7
end
@testset "issue #9214" begin
    @test 2s + (7ms + 1ms) == (2s + 7ms) + 1ms == 1ms + (2s + 7ms) == 1ms + (1s + 7ms) + 1s == 1ms + (2s + 3d + 7ms) + (-3d) == (1ms + (2s + 3d)) + (7ms - 3d) == (1ms + (2s + 3d)) - (3d - 7ms)
    @test 1ms - (2s + 7ms) == -((2s + 7ms) - 1ms) == (-6ms) - 2s
    @test emptyperiod == ((d + y) - y) - d == ((d + y) - d) - y
    @test emptyperiod == 2y + (m - d) + ms - ((m - d) + 2y + ms)
    @test emptyperiod == 0ms
    @test string(emptyperiod) == "empty period"
    @test string(ms + mi + d + m + y + w + h + s + 2y + m) == "3 years, 2 months, 1 week, 1 day, 1 hour, 1 minute, 1 second, 1 millisecond"
    @test 8d - s == 1w + 23h + 59mi + 59s
    @test h + 3mi == 63mi
    @test y - m == 11m
end
@testset "compound periods and types" begin
    # compound periods should avoid automatically converting period types
    @test (d - h).periods == Dates.Period[d, -h]
    @test d - h == 23h
    @test !isequal(d - h, 23h)
    @test isequal(d - h, 2d - 2h - 1d + 1h)
    @test sprint(show, y + m) == string(y + m)
    @test convert(Dates.CompoundPeriod, y) + m == y + m
end
@testset "compound period simplification" begin
    # reduce compound periods into the most basic form
    @test Dates.canonicalize(h - mi).periods == Dates.Period[59mi]
    @test Dates.canonicalize(-h + mi).periods == Dates.Period[-59mi]
    @test Dates.canonicalize(-y + d).periods == Dates.Period[-y, d]
    @test Dates.canonicalize(-y + m - w + d).periods == Dates.Period[-11m, -6d]
    @test Dates.canonicalize(-y + m - w + ms).periods == Dates.Period[-11m, -6d, -23h, -59mi, -59s, -999ms]
    @test Dates.canonicalize(y - m + w - d + h - mi + s - ms).periods == Dates.Period[11m, 6d, 59mi, 999ms]
    @test Dates.canonicalize(-y + m - w + d - h + mi - s + ms).periods == Dates.Period[-11m, -6d, -59mi, -999ms]

    @test Dates.Date(2009, 2, 1) - (Dates.Month(1) + Dates.Day(1)) == Dates.Date(2008, 12, 31)
    @test_throws MethodError (Dates.Month(1) + Dates.Day(1)) - Dates.Date(2009,2,1)
end
@testset "unary ops and vectorized period arithmetic" begin
    pa = [1y 1m 1w 1d; 1h 1mi 1s 1ms]
    cpa = [1y + 1s 1m + 1s 1w + 1s 1d + 1s; 1h + 1s 1mi + 1s 2m + 1s 1s + 1ms]

    @test +pa == pa == -(-pa)
    @test -pa == map(-, pa)
    @test 1y .+ pa == [2y 1y + 1m 1y + 1w 1y + 1d; 1y + 1h 1y + 1mi 1y + 1s 1y + 1ms]
    @test (1y + 1m) .+ pa == [2y + 1m 1y + 2m 1y + 1m + 1w 1y + 1m + 1d; 1y + 1m + 1h 1y + 1m + 1mi 1y + 1m + 1s 1y + 1m + 1ms]
    @test pa .+ 1y == [2y 1y + 1m 1y + 1w 1y + 1d; 1y + 1h 1y + 1mi 1y + 1s 1y + 1ms]
    @test pa .+ (1y + 1m) == [2y + 1m 1y + 2m 1y + 1m + 1w 1y + 1m + 1d; 1y + 1m + 1h 1y + 1m + 1mi 1y + 1m + 1s 1y + 1m + 1ms]

    @test 1y .+ cpa == [2y + 1s 1y + 1m + 1s 1y + 1w + 1s 1y + 1d + 1s; 1y + 1h + 1s 1y + 1mi + 1s 1y + 2m + 1s 1y + 1ms + 1s]
    @test (1y + 1m) .+ cpa == [2y + 1m + 1s 1y + 2m + 1s 1y + 1m + 1w + 1s 1y + 1m + 1d + 1s; 1y + 1m + 1h + 1s 1y + 1m + 1mi + 1s 1y + 3m + 1s 1y + 1m + 1s + 1ms]
    @test cpa .+ 1y == [2y + 1s 1y + 1m + 1s 1y + 1w + 1s 1y + 1d + 1s; 1y + 1h + 1s 1y + 1mi + 1s 1y + 2m + 1s 1y + 1ms + 1s]
    @test cpa .+ (1y + 1m) == [2y + 1m + 1s 1y + 2m + 1s 1y + 1m + 1w + 1s 1y + 1m + 1d + 1s; 1y + 1m + 1h + 1s 1y + 1m + 1mi + 1s 1y + 3m + 1s 1y + 1m + 1s + 1ms]

    @test 1y .+ pa == [2y 1y + 1m 1y + 1w 1y + 1d; 1y + 1h 1y + 1mi 1y + 1s 1y + 1ms]
    @test (1y + 1m) .+ pa == [2y + 1m 1y + 2m 1y + 1m + 1w 1y + 1m + 1d; 1y + 1m + 1h 1y + 1m + 1mi 1y + 1m + 1s 1y + 1m + 1ms]
    @test pa .+ 1y == [2y 1y + 1m 1y + 1w 1y + 1d; 1y + 1h 1y + 1mi 1y + 1s 1y + 1ms]
    @test pa .+ (1y + 1m) == [2y + 1m 1y + 2m 1y + 1m + 1w 1y + 1m + 1d; 1y + 1m + 1h 1y + 1m + 1mi 1y + 1m + 1s 1y + 1m + 1ms]

    @test 1y .+ cpa == [2y + 1s 1y + 1m + 1s 1y + 1w + 1s 1y + 1d + 1s; 1y + 1h + 1s 1y + 1mi + 1s 1y + 2m + 1s 1y + 1ms + 1s]
    @test (1y + 1m) .+ cpa == [2y + 1m + 1s 1y + 2m + 1s 1y + 1m + 1w + 1s 1y + 1m + 1d + 1s; 1y + 1m + 1h + 1s 1y + 1m + 1mi + 1s 1y + 3m + 1s 1y + 1m + 1s + 1ms]
    @test cpa .+ 1y == [2y + 1s 1y + 1m + 1s 1y + 1w + 1s 1y + 1d + 1s; 1y + 1h + 1s 1y + 1mi + 1s 1y + 2m + 1s 1y + 1ms + 1s]
    @test cpa .+ (1y + 1m) == [2y + 1m + 1s 1y + 2m + 1s 1y + 1m + 1w + 1s 1y + 1m + 1d + 1s; 1y + 1m + 1h + 1s 1y + 1m + 1mi + 1s 1y + 3m + 1s 1y + 1m + 1s + 1ms]

    @test 1y .- pa == [0y 1y-1m 1y-1w 1y-1d; 1y-1h 1y-1mi 1y-1s 1y-1ms]
    @test (1y + 1m) .- pa == [1m 1y 1y + 1m-1w 1y + 1m-1d; 1y + 1m-1h 1y + 1m-1mi 1y + 1m-1s 1y + 1m-1ms]
    @test pa .- (1y + 1m) == [-1m -1y -1y-1m + 1w -1y-1m + 1d; -1y-1m + 1h -1y-1m + 1mi -1y-1m + 1s -1y-1m + 1ms]
    @test pa .- 1y == [0y 1m-1y -1y + 1w -1y + 1d; -1y + 1h -1y + 1mi -1y + 1s -1y + 1ms]

    @test 1y .- cpa == [-1s 1y-1m-1s 1y-1w-1s 1y-1d-1s; 1y-1h-1s 1y-1mi-1s 1y-2m-1s 1y-1ms-1s]
    @test (1y + 1m) .- cpa == [1m-1s 1y-1s 1y + 1m-1w-1s 1y + 1m-1d-1s; 1y + 1m-1h-1s 1y + 1m-1mi-1s 1y-1m-1s 1y + 1m-1s-1ms]
    @test cpa .- 1y == [1s -1y + 1m + 1s -1y + 1w + 1s -1y + 1d + 1s; -1y + 1h + 1s -1y + 1mi + 1s -1y + 2m + 1s -1y + 1ms + 1s]
    @test cpa .- (1y + 1m) == [-1m + 1s -1y + 1s -1y-1m + 1w + 1s -1y-1m + 1d + 1s; -1y-1m + 1h + 1s -1y-1m + 1mi + 1s -1y + 1m + 1s -1y + -1m + 1s + 1ms]

    @test [1y 1m; 1w 1d] + [1h 1mi; 1s 1ms] == [1y + 1h 1m + 1mi; 1w + 1s 1d + 1ms]
    @test [1y 1m; 1w 1d] - [1h 1mi; 1s 1ms] == [1y-1h 1m-1mi; 1w-1s 1d-1ms]
    @test [1y 1m; 1w 1d] - [1h 1mi; 1s 1ms] - [1y-1h 1m-1mi; 1w-1s 1d-1ms] == [emptyperiod emptyperiod; emptyperiod emptyperiod]

    @test [1y + 1s 1m + 1s; 1w + 1s 1d + 1s] + [1h 1mi; 1s 1ms] == [1y + 1h + 1s 1m + 1mi + 1s; 1w + 2s 1d + 1s + 1ms]
    @test [1y + 1s 1m + 1s; 1w + 1s 1d + 1s] - [1h 1mi; 1s 1ms] == [1y-1h + 1s 1m-1mi + 1s; 1w 1d + 1s-1ms]

    @test [1y 1m; 1w 1d] + [1h + 1s 1mi + 1s; 1m + 1s 1s + 1ms] == [1y + 1h + 1s 1m + 1mi + 1s; 1w + 1m + 1s 1d + 1s + 1ms]
    @test [1y 1m; 1w 1d] - [1h + 1s 1mi + 1s; 1m + 1s 1s + 1ms] == [1y-1h-1s 1m-1mi-1s; 1w-1m-1s 1d-1s-1ms]

    @test [1y + 1s 1m + 1s; 1w + 1s 1d + 1s] + [1y + 1h 1y + 1mi; 1y + 1s 1y + 1ms] == [2y + 1h + 1s 1y + 1m + 1mi + 1s; 1y + 1w + 2s 1y + 1d + 1s + 1ms]
    @test [1y + 1s 1m + 1s; 1w + 1s 1d + 1s] - [1y + 1h 1y + 1mi; 1y + 1s 1y + 1ms] == [1s-1h 1m + 1s-1y-1mi; 1w-1y 1d + 1s-1y-1ms]
end
@testset "Equality and hashing between FixedPeriod types" begin
    let types = (Dates.Week, Dates.Day, Dates.Hour, Dates.Minute,
                 Dates.Second, Dates.Millisecond, Dates.Microsecond, Dates.Nanosecond)
        for i in 1:length(types), j in i:length(types), x in (0, 1, 235, -4677, 15250)
            local T, U, y, z
            T = types[i]
            U = types[j]
            y = T(x)
            z = convert(U, y)
            @test y == z
            @test hash(y) == hash(z)
        end
    end
end
@testset "Equality and hashing between OtherPeriod types" begin
    for x in (0, 1, 235, -4677, 15250)
        local x, y, z
        y = Dates.Year(x)
        z = convert(Dates.Month, y)
        @test y == z
        @test hash(y) == hash(z)
    end
end

@testset "#30832" begin
    @test Dates.toms(Dates.Second(1) + Dates.Nanosecond(1)) == 1e3
    @test Dates.tons(Dates.Second(1) + Dates.Nanosecond(1)) == 1e9 + 1
    @test Dates.toms(Dates.Second(1) + Dates.Microsecond(1)) == 1e3
end

end

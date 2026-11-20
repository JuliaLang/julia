# This file is a part of Julia. License is MIT: https://julialang.org/license

module RangesTest

using Test
using Dates

# A TimeType that forwards `guess` to DateTime, as TimeZones does for ZonedDateTime
struct ForwardedTime <: Dates.TimeType
    dt::Dates.DateTime
end
Dates.value(x::ForwardedTime) = Dates.value(x.dt)
Base.:+(x::ForwardedTime, p::Dates.Period) = ForwardedTime(x.dt + p)
Base.:-(x::ForwardedTime, p::Dates.Period) = ForwardedTime(x.dt - p)
Base.:-(x::ForwardedTime, y::ForwardedTime) = x.dt - y.dt
Dates.guess(a::ForwardedTime, b::ForwardedTime, c) = Dates.guess(a.dt, b.dt, c)

let
    for T in (Dates.Date, Dates.DateTime)
        f1 = T(2014); l1 = T(2013, 12, 31)
        f2 = T(2014); l2 = T(2014)
        f3 = T(-2000); l3 = T(2000)
        f4 = typemin(T); l4 = typemax(T)
        # Over the whole type, a step of one or two stored units makes more than typemax(Int64)
        # elements. These ranges are tested separately below.
        function overflows(f, l, step)
            s = Dates.valuestep(f, step)
            return (f, l) == (f4, l4) && !isnothing(s) && s[2] <= 2
        end

        # Built-in periods only, as other test files define package periods
        for P in (Dates.Year, Dates.Quarter, Dates.Month, Dates.Week, Dates.Day)
            for pos_step in (P(1), P(2), P(50), P(2048), P(10000))
                # empty range
                dr = f1:pos_step:l1
                len = length(dr)
                @test len == 0
                @test isa(len, Int64)
                @test isempty(dr)
                @test first(dr) == f1
                @test last(dr) < f1
                @test length([i for i in dr]) == 0
                @test_throws ArgumentError minimum(dr)
                @test_throws ArgumentError maximum(dr)
                @test_throws BoundsError dr[1]
                @test findall(in(dr), dr) == Int64[]
                @test [dr;] == T[]
                @test isempty(reverse(dr))
                @test length(reverse(dr)) == 0
                @test first(reverse(dr)) < f1
                @test last(reverse(dr)) >= f1
                @test issorted(dr)
                @test sortperm(dr) === StepRange{Int64,Int}(1:1:0)
                @test !(f1 in dr)
                @test !(l1 in dr)
                @test !(f1 - pos_step in dr)
                @test !(l1 + pos_step in dr)
                @test dr == []
                @test hash(dr) == hash([])

                for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                    dr = f:pos_step:l
                    overflows(f, l, pos_step) && continue
                    len = length(dr)
                    @test len > 0
                    @test isa(len, Int64)
                    @test !isempty(dr)
                    @test first(dr) == f
                    @test last(dr) <= l
                    @test minimum(dr) == first(dr)
                    @test maximum(dr) == last(dr)
                    @test dr[1] == f
                    @test dr[end] <= l
                    @test iterate(dr)[1] == first(dr)

                    if len < 10000
                        dr1 = [i for i in dr]
                        @test length(dr1) == len
                        @test findall(in(dr), dr) == [1:len;]
                        @test length([dr;]) == len
                        @test dr == dr1
                        @test hash(dr) == hash(dr1)
                    end
                    @test !isempty(reverse(dr))
                    @test length(reverse(dr)) == len
                    @test last(reverse(dr)) == f
                    @test issorted(dr)
                    @test f in dr

                end
            end
            for neg_step in (P(-1), P(-2), P(-50), P(-2048), P(-10000))
                # empty range
                dr = l1:neg_step:f1
                len = length(dr)
                @test len == 0
                @test isa(len, Int64)
                @test isempty(dr)
                @test first(dr) == l1
                @test last(dr) > l1
                @test length([i for i in dr]) == 0
                @test_throws ArgumentError minimum(dr)
                @test_throws ArgumentError maximum(dr)
                @test_throws BoundsError dr[1]
                @test findall(in(dr), dr) == Int64[]
                @test [dr;] == T[]
                @test isempty(reverse(dr))
                @test length(reverse(dr)) == 0
                @test first(reverse(dr)) > l1
                @test last(reverse(dr)) <= l1
                @test issorted(dr)
                @test sortperm(dr) === StepRange{Int64,Int}(1:1:0)
                @test !(l1 in dr)
                @test !(l1 in dr)
                @test !(l1 - neg_step in dr)
                @test !(l1 + neg_step in dr)
                @test dr == []
                @test hash(dr) == hash([])

                for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                    dr = l:neg_step:f
                    overflows(f, l, neg_step) && continue
                    len = length(dr)
                    @test len > 0
                    @test isa(len, Int64)
                    @test !isempty(dr)
                    @test first(dr) == l
                    @test last(dr) >= f
                    @test minimum(dr) == last(dr)
                    @test maximum(dr) == first(dr)
                    @test dr[1] == l
                    @test dr[end] >= f
                    @test iterate(dr)[1] == first(dr)

                    if len < 10000
                        dr1 = [i for i in dr]
                        @test length(dr1) == len
                        @test findall(in(dr), dr) == [1:len;]
                        @test length([dr;]) == len
                        @test dr == dr1
                        @test hash(dr) == hash(dr1)
                    end
                    @test !isempty(reverse(dr))
                    @test length(reverse(dr)) == len
                    @test issorted(dr) == (len <= 1)
                    @test l in dr
                end
            end
        end
        if T == Dates.DateTime
            for P in (Dates.Hour, Dates.Minute, Dates.Second, Dates.Millisecond, Dates.Microsecond, Dates.Nanosecond)
                P in (Dates.Microsecond, Dates.Nanosecond) && continue
                for pos_step in (P(1), P(2), P(50), P(2048), P(10000))
                    # empty range
                    dr = f1:pos_step:l1
                    len = length(dr)
                    @test len == 0
                    @test isa(len, Int64)
                    @test isempty(dr)
                    @test first(dr) == f1
                    @test last(dr) < f1
                    @test length([i for i in dr]) == 0
                    @test_throws ArgumentError minimum(dr)
                    @test_throws ArgumentError maximum(dr)
                    @test_throws BoundsError dr[1]
                    @test findall(in(dr), dr) == Int64[]
                    @test [dr;] == T[]
                    @test isempty(reverse(dr))
                    @test length(reverse(dr)) == 0
                    @test first(reverse(dr)) < f1
                    @test last(reverse(dr)) >= f1
                    @test issorted(dr)
                    @test sortperm(dr) === StepRange{Int64,Int}(1:1:0)
                    @test !(f1 in dr)
                    @test !(l1 in dr)
                    @test !(f1 - pos_step in dr)
                    @test !(l1 + pos_step in dr)
                    @test dr == []
                    @test hash(dr) == hash([])

                    for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                        dr = f:pos_step:l
                        overflows(f, l, pos_step) && continue
                        len = length(dr)
                        @test len > 0
                        @test isa(len, Int64)
                        @test !isempty(dr)
                        @test first(dr) == f
                        @test last(dr) <= l
                        @test minimum(dr) == first(dr)
                        @test maximum(dr) == last(dr)
                        @test dr[1] == f
                        @test dr[end] <= l
                        @test iterate(dr)[1] == first(dr)

                        if len < 10000
                            dr1 = [i for i in dr]
                            @test length(dr1) == len
                            @test findall(in(dr), dr) == [1:len;]
                            @test length([dr;]) == len
                            @test dr == dr1
                            @test hash(dr) == hash(dr1)
                        end
                        @test !isempty(reverse(dr))
                        @test length(reverse(dr)) == len
                        @test last(reverse(dr)) == f
                        @test issorted(dr)
                        @test f in dr

                    end
                end
                for neg_step in (P(-1), P(-2), P(-50), P(-2048), P(-10000))
                    # empty range
                    dr = l1:neg_step:f1
                    len = length(dr)
                    @test len == 0
                    @test isa(len, Int64)
                    @test isempty(dr)
                    @test first(dr) == l1
                    @test last(dr) > l1
                    @test length([i for i in dr]) == 0
                    @test_throws ArgumentError minimum(dr)
                    @test_throws ArgumentError maximum(dr)
                    @test_throws BoundsError dr[1]
                    @test findall(in(dr), dr) == Int64[]
                    @test [dr;] == T[]
                    @test isempty(reverse(dr))
                    @test length(reverse(dr)) == 0
                    @test first(reverse(dr)) > l1
                    @test last(reverse(dr)) <= l1
                    @test issorted(dr)
                    @test sortperm(dr) === StepRange{Int64,Int}(1:1:0)
                    @test !(l1 in dr)
                    @test !(l1 in dr)
                    @test !(l1 - neg_step in dr)
                    @test !(l1 + neg_step in dr)
                    @test dr == []
                    @test hash(dr) == hash([])

                    for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                        dr = l:neg_step:f
                        overflows(f, l, neg_step) && continue
                        len = length(dr)
                        @test len > 0
                        @test isa(len, Int64)
                        @test !isempty(dr)
                        @test first(dr) == l
                        @test last(dr) >= f
                        @test minimum(dr) == last(dr)
                        @test maximum(dr) == first(dr)
                        @test dr[1] == l
                        @test dr[end] >= f
                        @test iterate(dr)[1] == first(dr)

                        if len < 10000
                            dr1 = [i for i in dr]
                            @test length(dr1) == len
                            @test findall(in(dr), dr) == [1:len;]
                            @test length([dr;]) == len
                            @test dr == dr1
                            @test hash(dr) == hash(dr1)
                        end
                        @test !isempty(reverse(dr))
                        @test length(reverse(dr)) == len
                        @test last(reverse(dr)) >= l
                        @test issorted(dr) == (len <= 1)
                        @test l in dr

                    end
                end
            end
        end
    end
end

# Dates are physical units, and ranges should require an explicit step.
# See #19896 and https://discourse.julialang.org/t/type-restriction-on-unitrange/6557/12
if VERSION >= v"1.0.0"
    @test_throws MethodError Dates.DateTime(2013, 1, 1):Dates.DateTime(2013, 2, 1)
end

# All the range representations we want to test
# Date ranges
dr  = Dates.DateTime(2013, 1, 1):Dates.Day(1):Dates.DateTime(2013, 2, 1)
dr1 = Dates.DateTime(2013, 1, 1):Dates.Day(1):Dates.DateTime(2013, 1, 1)
dr2 = Dates.DateTime(2013, 1, 1):Dates.Day(1):Dates.DateTime(2012, 2, 1) # empty range
dr3 = Dates.DateTime(2013, 1, 1):Dates.Day(-1):Dates.DateTime(2012) # negative step
# Big ranges
dr4 = Dates.DateTime(0):Dates.Day(1):Dates.DateTime(20000, 1, 1)
dr5 = Dates.DateTime(0):Dates.Day(1):Dates.DateTime(200000, 1, 1)
dr6 = Dates.DateTime(0):Dates.Day(1):Dates.DateTime(2000000, 1, 1)
dr7 = Dates.DateTime(0):Dates.Day(1):Dates.DateTime(20000000, 1, 1)
dr8 = Dates.DateTime(0):Dates.Day(1):Dates.DateTime(200000000, 1, 1)
dr9 = typemin(Dates.DateTime):Dates.Day(1):typemax(Dates.DateTime)
# Other steps
dr10 = typemax(Dates.DateTime):Dates.Day(-1):typemin(Dates.DateTime)
dr11 = typemin(Dates.DateTime):Dates.Week(1):typemax(Dates.DateTime)

dr12 = typemin(Dates.DateTime):Dates.Month(1):typemax(Dates.DateTime)
dr13 = typemin(Dates.DateTime):Dates.Year(1):typemax(Dates.DateTime)

dr14 = typemin(Dates.DateTime):Dates.Week(10):typemax(Dates.DateTime)
dr15 = typemin(Dates.DateTime):Dates.Month(100):typemax(Dates.DateTime)
dr16 = typemin(Dates.DateTime):Dates.Year(1000):typemax(Dates.DateTime)
dr17 = typemax(Dates.DateTime):Dates.Week(-10000):typemin(Dates.DateTime)
dr18 = typemax(Dates.DateTime):Dates.Month(-100000):typemin(Dates.DateTime)
dr19 = typemax(Dates.DateTime):Dates.Year(-1000000):typemin(Dates.DateTime)
dr20 = typemin(Dates.DateTime):Dates.Day(2):typemax(Dates.DateTime)

drs = Any[dr, dr1, dr2, dr3, dr4, dr5, dr6, dr7, dr8, dr9, dr10,
          dr11, dr12, dr13, dr14, dr15, dr16, dr17, dr18, dr19, dr20]
drs2 = map(x->Dates.Date(first(x)):step(x):Dates.Date(last(x)), drs)

@test map(length, drs) == map(x->size(x)[1], drs)
@test map(length, drs) == map(x->length(Dates.Date(first(x)):step(x):Dates.Date(last(x))), drs)
@test map(length, drs) == map(x->length(reverse(x)), drs)
@test all(x->findall(in(x), x)==[1:length(x);], drs[1:4])
@test isempty(dr2)
@test all(x->reverse(x) == range(last(x), step=-step(x), length=length(x)), drs)
@test all(x->minimum(x) == (step(x) < zero(step(x)) ? last(x) : first(x)), drs[4:end])
@test all(x->maximum(x) == (step(x) < zero(step(x)) ? first(x) : last(x)), drs[4:end])
@test all(drs[1:3]) do dd
    for (i, d) in enumerate(dd)
        @test d == (first(dd) + Dates.Day(i - 1))
    end
    true
end
@test_throws MethodError dr .+ 1
a = Dates.DateTime(2013, 1, 1)
b = Dates.DateTime(2013, 2, 1)
@test map!(x->x + Dates.Day(1), Vector{Dates.DateTime}(undef, 32), dr) == [(a + Dates.Day(1)):Dates.Day(1):(b + Dates.Day(1));]
@test map(x->x + Dates.Day(1), dr) == [(a + Dates.Day(1)):Dates.Day(1):(b + Dates.Day(1));]

@test map(x->a in x, drs[1:4]) == [true, true, false, true]
@test a in dr
@test b in dr
@test Dates.DateTime(2013, 1, 3) in dr
@test Dates.DateTime(2013, 1, 15) in dr
@test Dates.DateTime(2013, 1, 26) in dr
@test !(Dates.DateTime(2012, 1, 1) in dr)

@test all(x->sort(x) == (step(x) < zero(step(x)) ? reverse(x) : x), drs)
@test all(x->step(x) < zero(step(x)) ? issorted(reverse(x)) : issorted(x), drs)

@test length(b:Dates.Day(-1):a) == 32
@test length(b:Dates.Day(1):a) == 0
@test length(b:Dates.Day(1):a) == 0
@test length(a:Dates.Day(2):b) == 16
@test last(a:Dates.Day(2):b) == Dates.DateTime(2013, 1, 31)
@test length(a:Dates.Day(7):b) == 5
@test last(a:Dates.Day(7):b) == Dates.DateTime(2013, 1, 29)
@test length(a:Dates.Day(32):b) == 1
@test last(a:Dates.Day(32):b) == Dates.DateTime(2013, 1, 1)
@test (a:Dates.Day(1):b)[1] == Dates.DateTime(2013, 1, 1)
@test (a:Dates.Day(1):b)[2] == Dates.DateTime(2013, 1, 2)
@test (a:Dates.Day(1):b)[7] == Dates.DateTime(2013, 1, 7)
@test (a:Dates.Day(1):b)[end] == b
@test first(a:Dates.Day(1):Dates.DateTime(20000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.DateTime(200000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.DateTime(2000000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.DateTime(20000000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.DateTime(200000000, 1, 1)) == a
@test first(a:Dates.Day(1):typemax(Dates.DateTime)) == a
@test first(typemin(Dates.DateTime):Dates.Day(1):typemax(Dates.DateTime)) == typemin(Dates.DateTime)

# Date ranges
dr  = Dates.Date(2013, 1, 1):Dates.Day(1):Dates.Date(2013, 2, 1)
dr1 = Dates.Date(2013, 1, 1):Dates.Day(1):Dates.Date(2013, 1, 1)
dr2 = Dates.Date(2013, 1, 1):Dates.Day(1):Dates.Date(2012, 2, 1) # empty range
dr3 = Dates.Date(2013, 1, 1):Dates.Day(-1):Dates.Date(2012, 1, 1) # negative step
# Big ranges
dr4 = Dates.Date(0):Dates.Day(1):Dates.Date(20000, 1, 1)
dr5 = Dates.Date(0):Dates.Day(1):Dates.Date(200000, 1, 1)
dr6 = Dates.Date(0):Dates.Day(1):Dates.Date(2000000, 1, 1)
dr7 = Dates.Date(0):Dates.Day(1):Dates.Date(20000000, 1, 1)
dr8 = Dates.Date(0):Dates.Day(1):Dates.Date(200000000, 1, 1)
dr9 = typemin(Dates.Date):Dates.Day(1):(typemin(Dates.Date) + Dates.Day(typemax(Int64) - 1))
# Other steps
dr10 = typemax(Dates.Date):Dates.Day(-1):(typemax(Dates.Date) - Dates.Day(typemax(Int64) - 1))
dr11 = typemin(Dates.Date):Dates.Week(1):typemax(Dates.Date)
dr12 = typemin(Dates.Date):Dates.Month(1):typemax(Dates.Date)
dr13 = typemin(Dates.Date):Dates.Year(1):typemax(Dates.Date)
dr14 = typemin(Dates.Date):Dates.Week(10):typemax(Dates.Date)
dr15 = typemin(Dates.Date):Dates.Month(100):typemax(Dates.Date)
dr16 = typemin(Dates.Date):Dates.Year(1000):typemax(Dates.Date)
dr17 = typemax(Dates.Date):Dates.Week(-10000):typemin(Dates.Date)
dr18 = typemax(Dates.Date):Dates.Month(-100000):typemin(Dates.Date)
dr19 = typemax(Dates.Date):Dates.Year(-1000000):typemin(Dates.Date)
dr20 = typemin(Dates.Date):Dates.Day(3):typemax(Dates.Date)

drs = Any[dr, dr1, dr2, dr3, dr4, dr5, dr6, dr7, dr8, dr9, dr10,
          dr11, dr12, dr13, dr14, dr15, dr16, dr17, dr18, dr19, dr20]

@test map(length, drs) == map(x->size(x)[1], drs)
@test all(x->findall(in(x), x) == [1:length(x);], drs[1:4])
@test isempty(dr2)
@test all(x->reverse(x) == last(x): - step(x):first(x), drs)
@test all(x->minimum(x) == (step(x) < zero(step(x)) ? last(x) : first(x)), drs[4:end])
@test all(x->maximum(x) == (step(x) < zero(step(x)) ? first(x) : last(x)), drs[4:end])
@test all(drs[1:3]) do dd
    for (i, d) in enumerate(dd)
        @test d == (first(dd) + Dates.Day(i - 1))
    end
    true
end
@test_throws MethodError dr .+ 1
a = Dates.Date(2013, 1, 1)
b = Dates.Date(2013, 2, 1)
@test map!(x->x + Dates.Day(1), Vector{Dates.Date}(undef, 32), dr) == [(a + Dates.Day(1)):Dates.Day(1):(b + Dates.Day(1));]
@test map(x->x + Dates.Day(1), dr) == [(a + Dates.Day(1)):Dates.Day(1):(b + Dates.Day(1));]

@test map(x->a in x, drs[1:4]) == [true, true, false, true]
@test a in dr
@test b in dr
@test Dates.Date(2013, 1, 3) in dr
@test Dates.Date(2013, 1, 15) in dr
@test Dates.Date(2013, 1, 26) in dr
@test !(Dates.Date(2012, 1, 1) in dr)

@test all(x->sort(x) == (step(x) < zero(step(x)) ? reverse(x) : x), drs)
@test all(x->step(x) < zero(step(x)) ? issorted(reverse(x)) : issorted(x), drs)

@test length(b:Dates.Day(-1):a) == 32
@test length(b:Dates.Day(1):a) == 0
@test length(a:Dates.Day(2):b) == 16
@test last(a:Dates.Day(2):b) == Dates.Date(2013, 1, 31)
@test length(a:Dates.Day(7):b) == 5
@test last(a:Dates.Day(7):b) == Dates.Date(2013, 1, 29)
@test length(a:Dates.Day(32):b) == 1
@test last(a:Dates.Day(32):b) == Dates.Date(2013, 1, 1)
@test (a:Dates.Day(1):b)[1] == Dates.Date(2013, 1, 1)
@test (a:Dates.Day(1):b)[2] == Dates.Date(2013, 1, 2)
@test (a:Dates.Day(1):b)[7] == Dates.Date(2013, 1, 7)
@test (a:Dates.Day(1):b)[end] == b
@test first(a:Dates.Day(1):Dates.Date(20000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.Date(200000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.Date(2000000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.Date(20000000, 1, 1)) == a
@test first(a:Dates.Day(1):Dates.Date(200000000, 1, 1)) == a
@test first(a:Dates.Day(1):typemax(Dates.Date)) == a
@test first(typemin(Dates.Date):Dates.Day(1):typemax(Dates.Date)) == typemin(Dates.Date)
@test length(dr9) == length(dr10) == typemax(Int64)

@test length(typemin(Dates.Date):Dates.Week(1):typemax(Dates.Date)) == 2635249153387078803
# Big Month/Year ranges
@test length(typemin(Dates.Date):Dates.Month(1):typemax(Dates.Date)) == 606065638266397310
@test length(typemin(Dates.Date):Dates.Year(1):typemax(Dates.Date)) == 50505469855533110
@test length(typemin(Dates.DateTime):Dates.Month(1):typemax(Dates.DateTime)) == 7014648592
@test length(typemin(Dates.DateTime):Dates.Year(1):typemax(Dates.DateTime)) == 584554050

@test length(typemin(Dates.DateTime):Dates.Week(1):typemax(Dates.DateTime)) == 30500568905
@test length(typemin(Dates.DateTime):Dates.Day(1):typemax(Dates.DateTime)) == 213503982335
@test length(typemin(Dates.DateTime):Dates.Hour(1):typemax(Dates.DateTime)) == 5124095576031
@test length(typemin(Dates.DateTime):Dates.Minute(1):typemax(Dates.DateTime)) == 307445734561826
@test length(typemin(Dates.DateTime):Dates.Second(1):typemax(Dates.DateTime)) == 18446744073709552
# Like integer ranges, length wraps around when the element count does not fit in Int64 (#48209)
for (T, P) in ((Dates.Date, Dates.Day), (Dates.DateTime, Dates.Millisecond))
    r = typemin(T):P(1):typemax(T)
    @test last(r) == typemax(T)
    @test length(r) == length(typemin(Int64):1:typemax(Int64))
    @test_throws OverflowError Base.checked_length(r)
    @test typemax(T) in r
    @test first(Iterators.drop(r, 2)) == typemin(T) + P(2)
    @test first(Iterators.drop(reverse(r), 2)) == typemax(T) - P(2)
    @test r[typemax(Int64)] == typemin(T) + P(typemax(Int64) - 1)
    @test r[big(2)^63 + 1] == r[UInt64(2)^63 + 1] == typemin(T) + P(typemax(Int64)) + P(1)
    @test r[big(2)^64] == typemax(T)
    @test_throws BoundsError r[big(2)^64 + 1]
    @test_throws BoundsError r[0]
    @test r[2:3] == [typemin(T) + P(1), typemin(T) + P(2)]
    # Like for integer ranges, no empty range starts at an end of the type
    @test_throws OverflowError r[1:0]
    @test_throws OverflowError reverse(r)[1:0]
    @test isempty(r[2:1]) && first(r[2:1]) == r[2]
    r = typemin(T):P(2):typemax(T)
    @test last(r) == typemax(T) - P(1)
    @test length(r) == length(typemin(Int64):2:typemax(Int64))
    @test_throws OverflowError Base.checked_length(r)
    @test first(Iterators.drop(r, 2)) == typemin(T) + P(4)
    # A slice step that does not fit in the period cannot form a range
    @test_throws OverflowError r[1:Int64(2)^62:(Int64(2)^62 + 1)]
    @test_throws OverflowError r[UInt64(1):UInt64(2)^62:(UInt64(2)^62 + 1)]
    @test r[1:(Int64(2)^62 - 1):Int64(2)^62] == [r[1], r[Int64(2)^62]]
end
# Single-element ranges at the ends of the type, for every kind of step
for T in (Dates.Date, Dates.DateTime), x in (typemin(T), typemax(T))
    for s in (Dates.Day(1), Dates.Day(-1), Dates.Week(typemax(Int64)), Dates.Month(1), Dates.Month(-1),
              Dates.Quarter(typemax(Int64)), Dates.Year(typemin(Int64)))
        r = x:s:x
        @test length(r) == 1 && collect(r) == [x] && last(r) == x && x in r
    end
    T === Dates.DateTime && @test collect(x:Dates.Microsecond(-1500):x) == [x]
end
# Steps larger than the range of the stored count
for r in (Dates.Date(2000):Dates.Week(Int64(2)^61):Dates.Date(2001),
          Dates.Date(2001):Dates.Week(-Int64(2)^61):Dates.Date(2000),
          Dates.DateTime(2000):Dates.Week(Int64(2)^41):Dates.DateTime(2001))
    @test length(r) == 1
    @test collect(r) == [first(r)]
    @test last(reverse(r)) == first(r)
    @test_throws BoundsError r[2]
end
# Steps with a fraction of a millisecond round each element from the start, as indexing does
let t = Dates.DateTime(2026)
    for r in (t:Dates.Microsecond(1500):(t + Dates.Millisecond(6)),
              t:Dates.Nanosecond(1500000):(t + Dates.Millisecond(6)),
              (t + Dates.Millisecond(6)):Dates.Microsecond(-1500):t)
        elements = [r[i] for i in 1:length(r)]
        @test sort(Dates.value.(elements .- t)) == [0, 2, 3, 4, 6]
        @test collect(r) == elements
        @test last(r) == elements[end]
        @test all(in(r), elements)
        @test !(t + Dates.Millisecond(1) in r) && !(t + Dates.Millisecond(5) in r)
        @test !(typemax(Dates.DateTime) in r) && !(typemin(Dates.DateTime) in r)
    end
end
# Fractional steps whose multiples do not fit in the period
for (c, stop) in ((Dates.Nanosecond(1_500_000), Dates.DateTime(300)),
                  (Dates.Microsecond(1_500), Dates.DateTime(300_000)))
    local r = Dates.DateTime(0):c:stop
    span = Dates.value(stop - Dates.DateTime(0))
    n = length(r)
    @test div(3 * (n - 1), 2, RoundNearest) <= span < div(3 * n, 2, RoundNearest)
    @test last(r) == r[n] == Dates.DateTime(0) + Dates.Millisecond(div(3 * (n - 1), 2, RoundNearest))
    @test r[3] == Dates.DateTime(0) + Dates.Millisecond(3)
    @test last(r) in r
    @test first(r[end:end]) == last(r)
    @test first(r[(n - 4):2:end]) == r[n - 4] && length(r[(n - 4):2:end]) == 3
    # A range with a given length steps the same way
    k = n - 1
    @test k > typemax(Int64) ÷ Dates.value(c)
    s = range(Dates.DateTime(0); step = c, length = k)
    @test s[k] == last(s) == r[k]
    @test first(s[(k - 1):k]) == r[k - 1] && length(s[(k - 1):k]) == 2
    @test reverse(range(Dates.DateTime(0); step = c, length = UInt64(3))) == [r[3], r[2], r[1]]
    # A range with a given stop steps back from it, and a slice step must fit in the period
    b = r[k]
    q = range(; stop = b, step = c, length = k)
    unit = Dates.value(typeof(c)(Dates.Millisecond(1)))
    @test last(q) == b
    @test first(q) == Dates.DateTime(Dates.UTM(Dates.value(b) + div(big(Dates.value(c)) * (1 - k), unit, RoundNearest)))
    @test_throws OverflowError s[1:(k - 1):k]
    @test collect(s[1:(k - 1):1]) == [r[1]]
end
@test reverse(range(Dates.Date(2020); step = Dates.Month(1), length = UInt64(3))) ==
    [Dates.Date(2020, m) for m in 3:-1:1]
# Steps of more microseconds than fit in nanoseconds, times an offset beyond Int64
let n = typemax(Int64), a = Dates.DateTime(0)
    for c in (Dates.Microsecond(n), Dates.Microsecond(-n))
        exact = Dates.value(a) + div(big(Dates.value(c)) * (n - 1), 1000, RoundNearest)
        @test Dates.value(range(a; step = c, length = n)[n]) == exact % Int64
    end
end
@test_throws OverflowError Dates.DateTime(Dates.UTM(typemin(Int64))):Dates.Microsecond(1500):Dates.DateTime(Dates.UTM(typemax(Int64)))
# Fractional step counts are exact up to typemax(Int64)
let a = Dates.DateTime(Dates.UTM(typemin(Int64))), c = Dates.Microsecond(1500)
    stop(n) = Dates.DateTime(Dates.UTM(Int64(typemin(Int64) + div(3 * big(n), 2, RoundNearest))))
    @test length(a:c:stop(typemax(Int64) - 1)) == typemax(Int64)
    @test_throws OverflowError a:c:stop(typemax(Int64))
    @test_throws OverflowError ForwardedTime(a):c:ForwardedTime(stop(typemax(Int64) - 2))
end
# DateTime cannot resolve steps below a millisecond, which would repeat elements
@test_throws ArgumentError Dates.DateTime(0):Dates.Microsecond(999):Dates.DateTime(1)
@test_throws ArgumentError Dates.DateTime(1):Dates.Nanosecond(-1):Dates.DateTime(1)
# Slices start at the parent's element and keep the number of indices
let t = Dates.DateTime(2026), k = 6 * Int64(10)^18
    for r in (t:Dates.Microsecond(1500):(t + Dates.Millisecond(9)),
              Dates.DateTime(300):Dates.Nanosecond(-1_500_000):Dates.DateTime(0))
        @test first(r[end:end]) == last(r)
        @test first(r[2:end]) == r[2] && length(r[2:end]) == length(r) - 1
        @test r[UInt128(3)] == r[UInt64(3)] == r[3]
        for s in (1:0, 3:2, 4:-1:5, UInt(3):UInt(2), 2:2, UInt(2):UInt(2), 3:-2:1)
            @test collect(r[s]) == [r[i] for i in s]
        end
    end
    r = Dates.DateTime(0):Dates.Microsecond(1000):Dates.DateTime(200_000_000)
    @test r[k:(k + 1)] == [r[k], r[k + 1]]
    @test (t:Dates.Microsecond(1500):t)[true:true] == [t]
    # Steps from the slice start can round past the end of the type
    hi, lo = Dates.DateTime(Dates.UTM(typemax(Int64))), Dates.DateTime(Dates.UTM(typemin(Int64)))
    @test_throws OverflowError ((hi - Dates.Millisecond(3)):Dates.Microsecond(1500):hi)[2:3]
    @test_throws OverflowError ((lo + Dates.Millisecond(3)):Dates.Microsecond(-1500):lo)[2:3]
    r = Dates.Date(2026):Dates.Day(-1):Dates.Date(2025)
    @test r[UInt(2):UInt(3)] == [Dates.Date(2025, 12, 31), Dates.Date(2025, 12, 30)]
    @test isempty(r[UInt(3):UInt(2)])
end
# Calendar steps never pass the stop, also for negative steps
@test last(Dates.Date(2024, 1, 31):Dates.Month(1):Dates.Date(2024, 3, 29)) == Dates.Date(2024, 2, 29)
@test collect(Dates.Date(2025, 2, 28):Dates.Month(-1):Dates.Date(2025, 1, 31)) == [Dates.Date(2025, 2, 28)]
# Calendar steps of more months than fit in Int64 give single-element ranges
for (a, b) in ((Dates.Date(2000), Dates.Date(2001)), (Dates.DateTime(2000), Dates.DateTime(2001)))
    for c in (Dates.Year(typemax(Int64)), Dates.Quarter(Int64(2)^62), Dates.Year(Int64(2)^62))
        @test collect(a:c:b) == [a]
        @test collect(b:-c:a) == [b]
    end
    @test collect(b:Dates.Year(typemin(Int64)):a) == [b]
end
# Other TimeTypes count calendar steps from the start, even when their `guess` overestimates
let f(args...) = ForwardedTime(Dates.DateTime(args...))
    for (a, c, b) in ((f(2026, 1, 1), Dates.Month(1), f(2026, 1, 31, 12)),
                      (f(2024, 1, 1), Dates.Year(1), f(2024, 12, 31, 12)),
                      (f(2025, 1, 1), Dates.Year(-1), f(2024, 1, 1, 12)),
                      (f(2023, 3, 30), Dates.Month(-1), f(2023, 2, 28, 12)))
        @test collect(a:c:b) == [a]
        @test last(a:c:b) == a
    end
    @test collect(f(2026, 1, 31):Dates.Month(1):f(2026, 5, 1)) == f.(2026, 1:4, [31, 28, 31, 30])
    # Long ranges are counted without stepping through them, and overflow instead of wrapping
    lo, hi = ForwardedTime(Dates.DateTime(Dates.UTM(typemin(Int64)))), ForwardedTime(Dates.DateTime(Dates.UTM(typemax(Int64))))
    @test length(ForwardedTime(Dates.DateTime(Dates.UTM(0))):Dates.Hour(1):ForwardedTime(Dates.DateTime(Dates.UTM(Int64(2)^62)))) ==
        fld(Int64(2)^62, 3_600_000) + 1
    @test_throws OverflowError lo:Dates.Hour(1):hi
    @test_throws OverflowError lo:Dates.Millisecond(1):ForwardedTime(Dates.DateTime(Dates.UTM(Int64(2)^40)))
    # DateTime cannot add a step that does not fit in its milliseconds
    @test_throws OverflowError f(1):Dates.Hour(145 * Int64(10)^15):f(194851351)
    @test length(f(1):Dates.Hour(Int64(2)^41):f(194851351)) == 1
    # The step product can overflow one step past the last element
    g(v) = ForwardedTime(Dates.DateTime(Dates.UTM(Int64(v))))
    h = Int64(2)^62
    @test collect(g(0):Dates.Millisecond(h):g(h)) == [g(0), g(h)]
    # An exact element one unit past the stop
    @test collect(g(0):Dates.Millisecond(h):g(typemax(Int64))) == [g(0), g(h)]
    @test collect(g(-1):Dates.Millisecond(-h):g(typemin(Int64))) == [g(-1), g(-1 - h)]
    @test collect(g(h):Dates.Millisecond(typemin(Int64)):g(-h)) == [g(h), g(-h)]
    # also for steps finer than the milliseconds of the span
    for p in (Dates.Microsecond(h), Dates.Nanosecond(h))
        @test collect(g(0):p:(g(0) + p + Dates.Millisecond(1))) == [g(0), g(0) + p]
    end
    # The third element rounds onto the stop, so it cannot be skipped without its product
    p = Dates.Microsecond(h + 100)
    stop = Int64(div(2 * Int128(Dates.value(p)), 1000, RoundNearest))
    @test length(Dates.DateTime(Dates.UTM(0)):p:Dates.DateTime(Dates.UTM(stop))) == 3
    @test_throws OverflowError g(0):p:g(stop)
    # The third element lies less than a unit past the stop, and DateTime rounds it beyond
    p = Dates.Microsecond(h + 900)
    stop = Int64(fld(2 * Int128(Dates.value(p)), 1000))
    @test collect(g(0):p:g(stop)) == [g(0), g(0) + p]
    @test collect(g(0):-p:g(-stop)) == [g(0), g(0) - p]
    @test_throws OverflowError g(0):p:g(stop + 1)
    # The third element ties between the odd stop and the next millisecond, and rounds to even
    p = Dates.Microsecond(Int64(4611686018427389750))
    stop = Int64(fld(2 * Int128(Dates.value(p)), 1000))
    @test isodd(stop) && collect(g(0):p:g(stop)) == [g(0), g(0) + p]
    @test collect(g(0):-p:g(-stop)) == [g(0), g(0) - p]
    @test_throws OverflowError g(0):p:g(stop + 1)
    @test length(Dates.DateTime(Dates.UTM(0)):p:Dates.DateTime(Dates.UTM(stop + 1))) == 3
end
# Unsigned indices into ranges with negative calendar steps
let r = Dates.Date(2026):Dates.Month(-1):Dates.Date(2025)
    @test r[UInt(3)] == r[3] == Dates.Date(2025, 11)
    @test r[UInt(2):UInt(3)] == [r[2], r[3]]
end
# Counting from a low guess stops at the first element past the stop, although a later one
# wraps around to between the start and the stop
struct LowGuessTime <: Dates.TimeType
    dt::Dates.DateTime
end
Dates.value(x::LowGuessTime) = Dates.value(x.dt)
Base.:+(x::LowGuessTime, p::Dates.Period) = LowGuessTime(x.dt + p)
Base.:-(x::LowGuessTime, p::Dates.Period) = LowGuessTime(x.dt - p)
Base.:-(x::LowGuessTime, y::LowGuessTime) = x.dt - y.dt
Dates.guess(a::LowGuessTime, b::LowGuessTime, c) = max(Dates.guess(a.dt, b.dt, c) - 1, 0)
let a = LowGuessTime(Dates.DateTime(1)), c = Dates.Hour(1_708_200_000_000), b = a + c
    @test a <= a + 3c <= b
    @test collect(a:c:b) == [a, b]
end
# Steps whose counters are unbounded
struct BigDay <: Dates.DatePeriod
    value::BigInt
end
Dates.value(p::BigDay) = p.value
Base.:+(d::Dates.Date, p::BigDay) = d + Dates.Day(Int64(p.value))
Base.:-(d::Dates.Date, p::BigDay) = d - Dates.Day(Int64(p.value))
Dates.guess(a::Dates.Date, b::Dates.Date, c::BigDay) = fld(big(Dates.value(b) - Dates.value(a)), c.value)
let r = Dates.Date(2026, 1, 1):BigDay(1):Dates.Date(2026, 1, 4)
    @test length(r) == 4
    @test last(r) == Dates.Date(2026, 1, 4)
end
# Package periods without their own estimate use the one for Date
struct Fortnight <: Dates.DatePeriod
    value::Int64
end
Dates.days(p::Fortnight) = 14 * p.value
Base.:+(d::Dates.Date, p::Fortnight) = d + Dates.Day(14 * p.value)
Base.:-(d::Dates.Date, p::Fortnight) = d - Dates.Day(14 * p.value)
@test collect(Dates.Date(2026):Fortnight(1):Dates.Date(2026, 2, 15)) == Dates.Date.(2026, [1, 1, 1, 2], [1, 15, 29, 12])
# and those with counts narrower than Int64
struct ShortDay <: Dates.DatePeriod
    value::Int32
end
Dates.days(p::ShortDay) = p.value
Base.:+(d::Dates.Date, p::ShortDay) = d + Dates.Day(p.value)
Base.:-(d::Dates.Date, p::ShortDay) = d - Dates.Day(p.value)
@test collect(Dates.Date(2000):ShortDay(1):Dates.Date(2000, 1, 3)) == Dates.Date.(2000, 1, 1:3)
# Period arithmetic keeps empty ranges empty, although it can move both ends to the same date
let e = Dates.Date(2026, 1, 31):Dates.Week(1):Dates.Date(2026, 1, 1)
    @test isempty(e + Dates.Month(1)) && isempty(Dates.Month(1) + e)
    @test isempty((Dates.Date(2026, 3, 31):Dates.Day(1):Dates.Date(2026, 3, 30)) - Dates.Month(1))
end
# Other types keep the ends of empty ranges, as subtracting the step can round back to the
# start or reach a time that does not exist
struct GapTime <: Dates.TimeType
    dt::Dates.DateTime
end
Dates.value(x::GapTime) = Dates.value(x.dt)
function Base.:+(x::GapTime, p::Dates.Period)
    y = x.dt + p
    (Dates.month(y), Dates.day(y), Dates.hour(y)) == (3, 10, 2) && throw(ArgumentError("$y does not exist"))
    return GapTime(y)
end
Base.:-(x::GapTime, p::Dates.Period) = x + -p
Base.:-(x::GapTime, y::GapTime) = x.dt - y.dt
let f(args...) = ForwardedTime(Dates.DateTime(args...)), g(args...) = GapTime(Dates.DateTime(args...))
    for r in (f(2026, 1, 2):Dates.Microsecond(500):f(2026, 1, 1), f(2026, 1, 1):Dates.Microsecond(-500):f(2026, 1, 2),
              g(2024, 4, 10, 2, 30):Dates.Month(1):g(2024, 4, 9, 2, 30))
        @test isempty(r)
        @test isempty(r + Dates.Hour(0)) && isempty(Dates.Hour(0) + r) && isempty(r - Dates.Hour(0))
        @test isempty(r[1:0]) && isempty(r[1:-1:2])
    end
    # Calendar shifts can move both ends to the same date, and a single element leaves one end
    for r in (f(2026, 1, 31):Dates.Microsecond(500):f(2026, 1, 30), f(2026, 1, 30):Dates.Microsecond(-500):f(2026, 1, 31))
        @test isempty(r + Dates.Month(1)) && isempty(Dates.Month(1) + r)
        @test isempty((r + Dates.Month(2)) - Dates.Month(1))
    end
    for r in (f(2026, 1, 1):Dates.Microsecond(500):f(2026, 1, 1), g(2024, 4, 10, 2, 30):Dates.Month(1):g(2024, 4, 10, 2, 30))
        @test !isempty(r) && first(r) == last(r)
        @test isempty(r[1:0]) && isempty(r[1:-1:2])
    end
end
# Time zone rules can end in the future, so an empty slice next to the end stays before it
struct CutoffTime <: Dates.TimeType
    dt::Dates.DateTime
end
Dates.value(x::CutoffTime) = Dates.value(x.dt)
const CUTOFF = Dates.DateTime(2038)
Base.:+(x::CutoffTime, p::Dates.Period) =
    (y = x.dt + p; y < CUTOFF ? CutoffTime(y) : throw(ArgumentError("$y is past the cutoff")))
Base.:-(x::CutoffTime, p::Dates.Period) = x + -p
Base.:-(x::CutoffTime, y::CutoffTime) = x.dt - y.dt
let x = CutoffTime(CUTOFF - Dates.Millisecond(1))
    for st in (Dates.Hour(1), Dates.Hour(-1))
        @test isempty((x:st:x)[1:0])
    end
    @test_throws ArgumentError range(CutoffTime(Dates.DateTime(2037, 12, 30)); step = Dates.Day(1), length = 3)
end
# An empty slice starts at its first index, like integer ranges
let r = Dates.Date(2026):Dates.Day(1):Dates.Date(2026, 1, 5)
    @test first(r[3:2]) == r[3] && first(r[3:-1:4]) == r[3]
end
@test_throws OverflowError Dates.guess(Dates.DateTime(Dates.UTM(0)), Dates.DateTime(Dates.UTM(typemin(Int64))), Dates.Millisecond(-1))
# Length-based ranges keep Base's ends and errors, and a slice keeps a step it never takes
@test last(range(; stop = Dates.Date(2000), step = Dates.Day(2), length = 0)) == Dates.Date(2000)
@test_throws MethodError range(Dates.Date(2000); step = Dates.Hour(1), length = 1)
let r = range(Dates.Date(2000); step = Dates.Day(Int64(1) << 62), length = 2)
    @test step(r[1:4:1]) == step(r)
end
# A slice of a length-based range starts at its first selected element
let r = range(; stop = Dates.Date(2024, 3, 31), step = Dates.Month(1), length = 3)
    @test first(r[1:2]) == r[1]
    r = range(; stop = typemax(Dates.Date), step = Dates.Day(1), length = typemax(Int64))
    @test collect(r[1:2]) == [r[1], r[2]]
end
# A slice step that does not fit in the period is never taken by at most one element
let r = Dates.Date(2000):Dates.Day(2):Dates.Date(2001)
    @test collect(r[1:Int64(2)^62:1]) == [r[1]]
    @test isempty(r[2:Int64(2)^62:1])
end
# Integer selectors whose step is not an integer
let r = Dates.Date(2026, 1, 1):Dates.Day(1):Dates.Date(2026, 1, 10)
    s = r[LinRange{Int}(1, 5, 3)]
    @test s isa Vector{Dates.Date} && s == r[1:2:5]
    r = Dates.Date(Dates.UTD(typemin(Int64))):Dates.Day(1):Dates.Date(Dates.UTD(typemax(Int64)))
    @test r[LinRange{Int}(1, 3, 3)] == r[1:3]
end
# A selector step can be wider than its elements
let r = Dates.Date(2000):Dates.Day(1):Dates.Date(2000, 1, 3)
    @test collect(r[StepRange(Int8(1), 100_000, Int8(1))]) == [r[1]]
    @test isempty(r[StepRange(Int8(1), 100_000, Int8(0))])
    @test r[StepRange(Int8(3), -1, Int8(1))] == reverse(r)
end
# Their float step or reference loses precision and can round their elements unevenly
let r = Dates.Date(Dates.UTD(0)):Dates.Day(1):Dates.Date(Dates.UTD(Int64(2)^55)), n = Int64(2)^53 + 2
    for s in (LinRange{Int64}(1, n, 2), LinRange{Int64}(n, 1, 2), LinRange{Int64}(n - 2, n, 3),
              StepRangeLen{Int64}(Float64(n - 2), 1, 3))
        @test collect(r[s]) == [r[i] for i in s]
    end
    @test collect(LinRange{Int64}(n - 2, n, 3)) == [n - 2, n - 2, n]
    @test collect(StepRangeLen{Int64}(Float64(n - 2), 1, 3)) == [n - 2, n - 2, n]
end
# They index from the start without forming the offset as a period, which would overflow
let r = Dates.DateTime(0):Dates.Nanosecond(1_000_000):Dates.DateTime(300), n = length(r)
    for s in (LinRange{Int64}(n - 2, n, 3), StepRangeLen(n - 2, 1, 3))
        @test collect(r[s]) == [r[i] for i in s]
    end
end
# The elements of an integer StepRangeLen can wrap around between valid ends
let r = Dates.Date(2026, 1, 1):Dates.Day(1):Dates.Date(2026, 1, 7), s = StepRangeLen(1, typemin(Int64) + 1, 3)
    @test collect(s) == [1, typemin(Int64) + 2, 3]
    @test_throws BoundsError r[s]
    @test r[StepRangeLen(2, 2, 3)] === r[2:2:6]
end

c = Dates.Date(2013, 6, 1)
@test length(a:Dates.Month(1):c) == 6
@test [a:Dates.Month(1):c;] == [a + Dates.Month(1)*i for i in 0:5]
@test [a:Dates.Month(2):Dates.Date(2013, 1, 2);] == [a]
@test [c:Dates.Month(-1):a;] == reverse([a:Dates.Month(1):c;])

@test length(range(Date(2000), step=Dates.Day(1), length=366)) == 366
let n = 100000
    local a = Dates.Date(2000)
    for i = 1:n
        @test length(range(a, step=Dates.Day(1), length=i)) == i
    end
    return a + Dates.Day(n)
end

@test typeof(step(Dates.DateTime(2000):Dates.Day(1):Dates.DateTime(2001))) == Dates.Day

a = Dates.Date(2013, 1, 1)
b = Dates.Date(2013, 2, 1)
d = Dates.Date(2020, 1, 1)
@test length(a:Dates.Year(1):d) == 8
@test first(a:Dates.Year(1):d) == a
@test last(a:Dates.Year(1):d) == d
@test length(a:Dates.Month(12):d) == 8
@test first(a:Dates.Month(12):d) == a
@test last(a:Dates.Month(12):d) == d
@test length(a:Dates.Week(52):d) == 8
@test first(a:Dates.Week(52):d) == a
@test last(a:Dates.Week(52):d) == Dates.Date(2019, 12, 24)
@test length(a:Dates.Day(365):d) == 8
@test first(a:Dates.Day(365):d) == a
@test last(a:Dates.Day(365):d) == Dates.Date(2019, 12, 31)

a = Dates.Date(2013, 1, 1)
b = Dates.Date(2013, 2, 1)
@test length(a:Dates.Year(1):Dates.Date(2020, 2, 1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020, 6, 1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020, 11, 1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020, 12, 31)) == 8
@test length(a:Dates.Year(1):Dates.Date(2021, 1, 1)) == 9
@test length(Dates.Date(2000):Dates.Year(-10):Dates.Date(1900)) == 11
@test length(Dates.Date(2000, 6, 23):Dates.Year(-10):Dates.Date(1900, 2, 28)) == 11
@test length(Dates.Date(2000, 1, 1):Dates.Year(1):Dates.Date(2000, 2, 1)) == 1

let n = 100000
    local a, b
    a= b = Dates.Date(0)
    for i = 1:n
        @test length(a:Dates.Year(1):b) == i
        b += Dates.Year(1)
    end
end

let n = 10000,
    a = Dates.Date(1985, 12, 5),
    b = Dates.Date(1986, 12, 27),
    c = Dates.DateTime(1985, 12, 5),
    d = Dates.DateTime(1986, 12, 27)
    for i = 1:n
        @test length(a:Dates.Month(1):b) == 13
        @test length(a:Dates.Year(1):b) == 2
        @test length(c:Dates.Month(1):d) == 13
        @test length(c:Dates.Year(1):d) == 2
        a += Dates.Day(1)
        b += Dates.Day(1)
    end
end

let n = 100000
    local a, b
    a = b = Dates.Date(2000)
    for i = 1:n
        @test length(a:Dates.Month(1):b) == i
        b += Dates.Month(1)
    end
end

@test length(Dates.Year(1):Dates.Year(1):Dates.Year(10)) == 10
@test length(Dates.Year(10):Dates.Year(-1):Dates.Year(1)) == 10
@test length(Dates.Year(10):Dates.Year(-2):Dates.Year(1)) == 5
@test length(typemin(Dates.Year):Dates.Year(1):typemax(Dates.Year)) == 0 # overflow
@test_throws MethodError Dates.Date(0):Dates.DateTime(2000)
@test_throws MethodError Dates.Date(0):Dates.Year(10)
@test length(range(Dates.Date(2000), step=Dates.Day(1), length=366)) == 366
@test last(range(Dates.Date(2000), step=Dates.Day(1), length=366)) == Dates.Date(2000, 12, 31)
@test last(range(Dates.Date(2001), step=Dates.Day(1), length=365)) == Dates.Date(2001, 12, 31)
@test last(range(Dates.Date(2000), step=Dates.Day(1), length=367)) == last(range(Dates.Date(2000), step=Dates.Month(12), length=2)) == last(range(Dates.Date(2000), step=Dates.Year(1), length=2))
@test last(range(Dates.DateTime(2000), step=Dates.Day(366), length=2)) == last(range(Dates.DateTime(2000), step=Dates.Hour(8784), length=2))

# Issue 5
lastdaysofmonth = [Dates.Date(2014, i, Dates.daysinmonth(2014, i)) for i=1:12]
@test [Date(2014, 1, 31):Dates.Month(1):Date(2015);] == lastdaysofmonth

# Range addition/subtraction:
let d = Dates.Day(1)
    @test (Dates.Date(2000):d:Dates.Date(2001)) + d == (Dates.Date(2000) + d:d:Dates.Date(2001) + d)
    @test (Dates.Date(2000):d:Dates.Date(2001)) - d == (Dates.Date(2000) - d:d:Dates.Date(2001) - d)
end

# Time ranges
dr  = Dates.Time(23, 1, 1):Dates.Second(1):Dates.Time(23, 2, 1)
dr1 = Dates.Time(23, 1, 1):Dates.Second(1):Dates.Time(23, 1, 1)
dr2 = Dates.Time(23, 1, 1):Dates.Second(1):Dates.Time(22, 2, 1) # empty range
dr3 = Dates.Time(23, 1, 1):Dates.Minute(-1):Dates.Time(22, 1, 1) # negative step
# Big ranges
dr8 = typemin(Dates.Time):Dates.Second(1):typemax(Dates.Time)
dr9 = typemin(Dates.Time):Dates.Nanosecond(1):typemax(Dates.Time)
# Other steps
dr10 = typemax(Dates.Time):Dates.Microsecond(-1):typemin(Dates.Time)
dr11 = typemin(Dates.Time):Dates.Millisecond(1):typemax(Dates.Time)
dr12 = typemin(Dates.Time):Dates.Minute(1):typemax(Dates.Time)
dr13 = typemin(Dates.Time):Dates.Hour(1):typemax(Dates.Time)
dr14 = typemin(Dates.Time):Dates.Millisecond(10):typemax(Dates.Time)
dr15 = typemin(Dates.Time):Dates.Minute(100):typemax(Dates.Time)
dr16 = typemin(Dates.Time):Dates.Hour(1000):typemax(Dates.Time)
dr17 = typemax(Dates.Time):Dates.Millisecond(-10000):typemin(Dates.Time)
dr18 = typemax(Dates.Time):Dates.Minute(-100):typemin(Dates.Time)
dr19 = typemax(Dates.Time):Dates.Hour(-10):typemin(Dates.Time)
dr20 = typemin(Dates.Time):Dates.Microsecond(2):typemax(Dates.Time)

drs = Any[dr, dr1, dr2, dr3, dr8, dr9, dr10,
          dr11, dr12, dr13, dr14, dr15, dr16, dr17, dr18, dr19, dr20]

@test map(length, drs) == map(x->size(x)[1], drs)
@test all(x->findall(in(x), x) == [1:length(x);], drs[1:4])
@test isempty(dr2)
@test all(x->reverse(x) == last(x): - step(x):first(x), drs)
@test all(x->minimum(x) == (step(x) < zero(step(x)) ? last(x) : first(x)), drs[4:end])
@test all(x->maximum(x) == (step(x) < zero(step(x)) ? first(x) : last(x)), drs[4:end])
@test_throws MethodError dr .+ 1

a = Dates.Time(23, 1, 1)
@test map(x->a in x, drs[1:4]) == [true, true, false, true]
@test a in dr

@test all(x->sort(x) == (step(x) < zero(step(x)) ? reverse(x) : x), drs)
@test all(x->step(x) < zero(step(x)) ? issorted(reverse(x)) : issorted(x), drs)

# Slices of Time ranges, also with a step of a whole day, which returns to the same Time
for r in (Dates.Time(1):Dates.Hour(12):Dates.Time(13), Dates.Time(13):Dates.Hour(-12):Dates.Time(1),
          typemin(Dates.Time):Dates.Hour(12):typemax(Dates.Time),
          typemax(Dates.Time):Dates.Hour(-12):typemin(Dates.Time))
    for s in (1:0, 1:2:0, 3:-2:4, 1:1, 2:2, 1:2, 2:-1:1)
        # Like for integer ranges, no empty range starts at an end of the day
        if isempty(s) && first(s) == 1 && first(r) in (typemin(Dates.Time), typemax(Dates.Time))
            @test_throws OverflowError r[s]
        else
            @test collect(r[s]) == [r[i] for i in s]
        end
    end
end

# A far-away value lies outside the bounds, also for calendar steps
@test !(typemin(Dates.Date) in Dates.Date(2026):Dates.Month(1):Dates.Date(2027))
@test !(1 in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !(Complex(1, 0) in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !(π in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !("a" in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))

@test hash(Any[Date("2018-1-03"), Date("2018-1-04"), Date("2018-1-05")]) ==
      hash([Date("2018-1-03"), Date("2018-1-04"), Date("2018-1-05")]) ==
      hash(Date("2018-1-03"):Day(1):Date("2018-1-05"))

@testset "range overflow" begin
    # DateTime ranges interactions with overflow. If not handled correctly `Dates.len` could
    # infinite loop
    @test length(DateTime(0):typemax(Millisecond):DateTime(0)) == 1
    @test length(typemax(DateTime):typemax(Millisecond):typemax(DateTime)) == 1

    # Overflow interaction is easier to comprehend with using UTM extremes
    utm_typemin = DateTime(Dates.UTM(typemin(Int64)))
    utm_typemax = DateTime(Dates.UTM(typemax(Int64)))

    @test length(utm_typemax:Millisecond(1):utm_typemax) == 1
    @test length(utm_typemin:-Millisecond(1):utm_typemin) == 1
end

# Issue #45816
@testset "period ranges" begin
    @test length(Day(1):Day(1):Day(5)) == 5
    r = StepRange{Day,Int}(Day(1), 1, Day(3))
    @test (first(r), step(r), last(r)) === (Day(1), 1, Day(3))
end

@testset "default step for date ranges" begin
    r = Date(2000, 1, 1):Date(2000, 12, 31)
    @test step(r) === Day(1)
    @test length(r) == 366
end

end  # RangesTest module

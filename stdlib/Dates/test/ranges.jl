# This file is a part of Julia. License is MIT: https://julialang.org/license

module RangesTest

using Test
using Dates

using InteractiveUtils: subtypes

let
    for T in (Dates.Date, Dates.DateTime)
        f1 = T(2014); l1 = T(2013, 12, 31)
        f2 = T(2014); l2 = T(2014)
        f3 = T(-2000); l3 = T(2000)
        f4 = typemin(T); l4 = typemax(T)

        for P in subtypes(Dates.DatePeriod)
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
                @test sortperm(dr) == 1:1:0
                @test !(f1 in dr)
                @test !(l1 in dr)
                @test !(f1 - pos_step in dr)
                @test !(l1 + pos_step in dr)
                @test dr == []
                @test hash(dr) == hash([])

                for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                    dr = f:pos_step:l
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
                    @test next(dr, start(dr)) == (first(dr), 1)

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
                @test sortperm(dr) == 1:1:0
                @test !(l1 in dr)
                @test !(l1 in dr)
                @test !(l1 - neg_step in dr)
                @test !(l1 + neg_step in dr)
                @test dr == []
                @test hash(dr) == hash([])

                for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                    dr = l:neg_step:f
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
                    @test next(dr, start(dr)) == (first(dr), 1)

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
            for P in subtypes(Dates.TimePeriod)
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
                    @test sortperm(dr) == 1:1:0
                    @test !(f1 in dr)
                    @test !(l1 in dr)
                    @test !(f1 - pos_step in dr)
                    @test !(l1 + pos_step in dr)
                    @test dr == []
                    @test hash(dr) == hash([])

                    for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                        dr = f:pos_step:l
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
                        @test next(dr, start(dr)) == (first(dr), 1)

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
                    @test sortperm(dr) == 1:1:0
                    @test !(l1 in dr)
                    @test !(l1 in dr)
                    @test !(l1 - neg_step in dr)
                    @test !(l1 + neg_step in dr)
                    @test dr == []
                    @test hash(dr) == hash([])

                    for (f, l) in ((f2, l2), (f3, l3), (f4, l4))
                        dr = l:neg_step:f
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
                        @test next(dr, start(dr)) == (first(dr), 1)

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
dr9 = typemin(Dates.Date):Dates.Day(1):typemax(Dates.Date)
# Other steps
dr10 = typemax(Dates.Date):Dates.Day(-1):typemin(Dates.Date)
dr11 = typemin(Dates.Date):Dates.Week(1):typemax(Dates.Date)
dr12 = typemin(Dates.Date):Dates.Month(1):typemax(Dates.Date)
dr13 = typemin(Dates.Date):Dates.Year(1):typemax(Dates.Date)
dr14 = typemin(Dates.Date):Dates.Week(10):typemax(Dates.Date)
dr15 = typemin(Dates.Date):Dates.Month(100):typemax(Dates.Date)
dr16 = typemin(Dates.Date):Dates.Year(1000):typemax(Dates.Date)
dr17 = typemax(Dates.Date):Dates.Week(-10000):typemin(Dates.Date)
dr18 = typemax(Dates.Date):Dates.Month(-100000):typemin(Dates.Date)
dr19 = typemax(Dates.Date):Dates.Year(-1000000):typemin(Dates.Date)
dr20 = typemin(Dates.Date):Dates.Day(2):typemax(Dates.Date)

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

@test length(typemin(Dates.Date):Dates.Week(1):typemax(Dates.Date)) == 26351950414948059
# Big Month/Year ranges
@test length(typemin(Dates.Date):Dates.Month(1):typemax(Dates.Date)) == 6060531933867600
@test length(typemin(Dates.Date):Dates.Year(1):typemax(Dates.Date)) == 505044327822300
@test length(typemin(Dates.DateTime):Dates.Month(1):typemax(Dates.DateTime)) == 3507324288
@test length(typemin(Dates.DateTime):Dates.Year(1):typemax(Dates.DateTime)) == 292277024

@test length(typemin(Dates.DateTime):Dates.Week(1):typemax(Dates.DateTime)) == 15250284420
@test length(typemin(Dates.DateTime):Dates.Day(1):typemax(Dates.DateTime)) == 106751990938
@test length(typemin(Dates.DateTime):Dates.Hour(1):typemax(Dates.DateTime)) == 2562047782512
@test length(typemin(Dates.DateTime):Dates.Minute(1):typemax(Dates.DateTime)) == 153722866950720
@test length(typemin(Dates.DateTime):Dates.Second(1):typemax(Dates.DateTime)) == 9223372017043200
@test length(typemin(DateTime):Dates.Millisecond(1):typemax(DateTime)) == 9223372017043199001

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
@test_throws OverflowError length(typemin(Dates.Year):Dates.Year(1):typemax(Dates.Year))
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

@test !(1 in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !(Complex(1, 0) in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !(π in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))
@test !("a" in Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05))

end

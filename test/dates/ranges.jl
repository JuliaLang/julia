# Ranges
a = Dates.Date(2013,1,1)
b = Dates.Date(2013,2,1)
dr = a:b
@test size(dr) == (32,)
@test length(dr) == 32
@test findin(dr,dr) == [1:32]
@test findin(a:Dates.Date(2013,1,14),dr) == [1:14]
@test findin(a:Dates.Date(2012,12,31),dr) == Int64[]
@test isempty(a:Dates.Date(2012,12,31))
@test reverse(dr) == b:Dates.Day(-1):a
@test map!(x->x+Dates.Day(1),Array(Date,32),dr) == [(a+Dates.Day(1)):(b+Dates.Day(1))]
@test map(x->x+Dates.Day(1),dr) == [(a+Dates.Day(1)):(b+Dates.Day(1))]
@test minimum(dr) == a
@test maximum(dr) == b
for (i,d) in enumerate(dr)
    @test d == a + Dates.Day(i-1)
end
for (i,d) in enumerate(a:Dates.Day(2):b)
    @test d == a + Dates.Day((i-1)*2)
end
@test_throws MethodError dr + 1 #TODO
@test a in dr
@test b in dr
@test Dates.Date(2013,1,3) in dr
@test Dates.Date(2013,1,15) in dr
@test Dates.Date(2013,1,26) in dr
@test !(Dates.Date(2012,1,1) in dr)
em = a:Dates.Date(2012,12,31)
@test !(a in em) #empty range
@test sort(dr) == dr
@test sort(em) == em
@test issorted(dr)
@test issorted(em)
@test !issorted(reverse(dr))
@test sort(reverse(dr)) == dr
# dr + Dates.Day(1) #TODO
@test length(b:Dates.Day(-1):a) == 32
@test length(b:a) == 0
@test length(b:Dates.Day(1):a) == 0
@test length(a:Dates.Day(2):b) == 16
@test last(a:Dates.Day(2):b) == Dates.Date(2013,1,31)
@test length(a:Dates.Day(7):b) == 5
@test last(a:Dates.Day(7):b) == Dates.Date(2013,1,29)
@test length(a:Dates.Day(32):b) == 1
@test last(a:Dates.Day(32):b) == Dates.Date(2013,1,1)
@test (a:b)[1] == Dates.Date(2013,1,1)
@test (a:b)[2] == Dates.Date(2013,1,2)
@test (a:b)[7] == Dates.Date(2013,1,7)
@test (a:b)[end] == b
@test first(a:Dates.Date(20000,1,1)) == a
@test first(a:Dates.Date(200000,1,1)) == a
@test first(a:Dates.Date(2000000,1,1)) == a
@test first(a:Dates.Date(20000000,1,1)) == a
@test first(a:Dates.Date(200000000,1,1)) == a
@test first(a:typemax(Dates.Date)) == a
@test first(typemin(Dates.Date):typemax(Dates.Date)) == typemin(Dates.Date)
@test length(typemin(Dates.Date):Dates.Week(1):typemax(Dates.Date)) == 26351950414948059
#toobig
@test_throws ArgumentError length(typemin(Dates.Date):Dates.Month(1):typemax(Dates.Date))
@test_throws ArgumentError length(typemin(Dates.Date):Dates.Year(1):typemax(Dates.Date))
@test_throws ArgumentError length(typemin(Dates.DateTime):Dates.Month(1):typemax(Dates.DateTime))
@test_throws ArgumentError length(typemin(Dates.DateTime):Dates.Year(1):typemax(Dates.DateTime))

@test length(typemin(Dates.DateTime):Dates.Week(1):typemax(Dates.DateTime)) == 15250284420
@test length(typemin(Dates.DateTime):Dates.Day(1):typemax(Dates.DateTime)) == 106751990938
@test length(typemin(Dates.DateTime):Dates.Hour(1):typemax(Dates.DateTime)) == 2562047782512
@test length(typemin(Dates.DateTime):Dates.Minute(1):typemax(Dates.DateTime)) == 153722866950720
@test length(typemin(Dates.DateTime):Dates.Second(1):typemax(Dates.DateTime)) == 9223372017043200
@test length(typemin(DateTime):typemax(DateTime)) == 9223372017043199001

c = Dates.Date(2013,6,1)
@test length(a:Dates.Month(1):c) == 6
@test [a:Dates.Month(1):c] == [a + Dates.Month(1)*i for i in 0:5]
@test [a:Dates.Month(2):Dates.Date(2013,1,2)] == [a]
@test [c:Dates.Month(-1):a] == reverse([a:Dates.Month(1):c])

d = Dates.Date(2020,1,1)
@test length(a:Dates.Year(1):d) == 8
@test first(a:Dates.Year(1):d) == a
@test last(a:Dates.Year(1):d) == d
@test length(a:Dates.Month(12):d) == 8
@test first(a:Dates.Month(12):d) == a
@test last(a:Dates.Month(12):d) == d
@test length(a:Dates.Week(52):d) == 8
@test first(a:Dates.Week(52):d) == a
@test last(a:Dates.Week(52):d) == Dates.Date(2019,12,24)
@test length(a:Dates.Day(365):d) == 8
@test first(a:Dates.Day(365):d) == a
@test last(a:Dates.Day(365):d) == Dates.Date(2019,12,31)

@test length(a:Dates.Year(1):Dates.Date(2020,2,1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020,6,1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020,11,1)) == 8
@test length(a:Dates.Year(1):Dates.Date(2020,12,31)) == 8
@test length(a:Dates.Year(1):Dates.Date(2021,1,1)) == 9
@test length(Dates.Date(2000):Dates.Year(-10):Dates.Date(1900)) == 11
@test length(Dates.Date(2000,6,23):Dates.Year(-10):Dates.Date(1900,2,28)) == 11
@test length(Dates.Date(2000,1,1):Dates.Year(1):Dates.Date(2000,2,1)) == 1

@test length(Dates.Year(1):Dates.Year(10)) == 10
@test length(Dates.Year(10):Dates.Year(-1):Dates.Year(1)) == 10
@test length(Dates.Year(10):Dates.Year(-2):Dates.Year(1)) == 5
@test_throws OverflowError length(typemin(Dates.Year):typemax(Dates.Year))
@test_throws MethodError Dates.Date(0):Dates.DateTime(2000)
@test_throws MethodError Dates.Date(0):Dates.Year(10)
@test length(range(Dates.Date(2000),366)) == 366
@test last(range(Dates.Date(2000),366)) == Dates.Date(2000,12,31)
@test last(range(Dates.Date(2001),365)) == Dates.Date(2001,12,31)
@test last(range(Dates.Date(2000),367)) == last(range(Dates.Date(2000),Dates.Month(12),2)) == last(range(Dates.Date(2000),Dates.Year(1),2))
@test last(range(Dates.DateTime(2000),Dates.Day(366),2)) == last(range(Dates.DateTime(2000),Dates.Hour(8784),2))

# Issue 5
lastdaysofmonth = [Dates.Date(2014,i,Dates.daysinmonth(2014,i)) for i=1:12]
@test [Date(2014,1,31):Dates.Month(1):Date(2015)] == lastdaysofmonth
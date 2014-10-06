# Period testing
@test -Dates.Year(1) == Dates.Year(-1)
@test Dates.Year(1) > Dates.Year(0)
@test (Dates.Year(1) < Dates.Year(0)) == false
@test Dates.Year(1) == Dates.Year(1)
@test Dates.Year(1) + Dates.Year(1) == Dates.Year(2)
@test Dates.Year(1) - Dates.Year(1) == Dates.Year(0)
@test Dates.Year(1) * Dates.Year(1) == Dates.Year(1)
@test Dates.Year(10) % Dates.Year(4) == Dates.Year(2)
@test div(Dates.Year(10),Dates.Year(3)) == Dates.Year(3)
@test div(Dates.Year(10),Dates.Year(4)) == Dates.Year(2)
t = Dates.Year(1)
t2 = Dates.Year(2)
@test ([t,t,t,t,t] .+ Dates.Year(1)) == ([t2,t2,t2,t2,t2])
@test (Dates.Year(1) .+ [t,t,t,t,t]) == ([t2,t2,t2,t2,t2])
@test ([t2,t2,t2,t2,t2] .- Dates.Year(1)) == ([t,t,t,t,t])
@test ([t,t,t,t,t] .* Dates.Year(1)) == ([t,t,t,t,t])
@test ([t,t,t,t,t] .% t2) == ([t,t,t,t,t])
@test div([t,t,t,t,t],Dates.Year(1)) == ([t,t,t,t,t])

#Period arithmetic
y = Dates.Year(1)
m = Dates.Month(1)
w = Dates.Week(1)
d = Dates.Day(1)
h = Dates.Hour(1)
mi = Dates.Minute(1)
s = Dates.Second(1)
ms = Dates.Millisecond(1)
@test Dates.Year(y) == y
@test Dates.Month(m) == m
@test Dates.Week(w) == w
@test Dates.Day(d) == d
@test Dates.Hour(h) == h
@test Dates.Minute(mi) == mi
@test Dates.Second(s) == s
@test Dates.Millisecond(ms) == ms
@test typeof(int8(y)) <: Int8
@test typeof(uint8(y)) <: Uint8
@test typeof(int16(y)) <: Int16
@test typeof(uint16(y)) <: Uint16
@test typeof(int32(y)) <: Int32
@test typeof(uint32(y)) <: Uint32
@test typeof(int64(y)) <: Int64
@test typeof(uint64(y)) <: Uint64
@test typeof(int128(y)) <: Int128
@test typeof(uint128(y)) <: Uint128
@test typeof(convert(BigInt,y)) <: BigInt
@test typeof(convert(BigFloat,y)) <: BigFloat
@test typeof(convert(Complex,y)) <: Complex
@test typeof(convert(Rational,y)) <: Rational
@test typeof(float16(y)) <: Float16
@test typeof(float32(y)) <: Float32
@test typeof(float64(y)) <: Float64
@test Dates.Year(convert(Int8,1)) == y
@test Dates.Year(convert(Uint8,1)) == y
@test Dates.Year(convert(Int16,1)) == y
@test Dates.Year(convert(Uint16,1)) == y
@test Dates.Year(convert(Int32,1)) == y
@test Dates.Year(convert(Uint32,1)) == y
@test Dates.Year(convert(Int64,1)) == y
@test Dates.Year(convert(Uint64,1)) == y
@test Dates.Year(convert(Int128,1)) == y
@test Dates.Year(convert(Uint128,1)) == y
@test Dates.Year(convert(BigInt,1)) == y
@test Dates.Year(convert(BigFloat,1)) == y
@test Dates.Year(convert(Complex,1)) == y
@test Dates.Year(convert(Rational,1)) == y
@test Dates.Year(convert(Float16,1)) == y
@test Dates.Year(convert(Float32,1)) == y
@test Dates.Year(convert(Float64,1)) == y
@test y == y
@test m == m
@test w == w
@test d == d
@test h == h
@test mi == mi
@test s == s
@test ms == ms
@test_throws ArgumentError y != m
@test_throws ArgumentError m != w
@test_throws ArgumentError w != d
@test_throws ArgumentError d != h
@test_throws ArgumentError h != mi
@test_throws ArgumentError mi != s
@test_throws ArgumentError s != ms
@test_throws ArgumentError ms != y
y2 = Dates.Year(2)
@test y < y2
@test y2 > y
@test y != y2

@test Dates.Year(int8(1)) == y
@test Dates.Year(uint8(1)) == y
@test Dates.Year(int16(1)) == y
@test Dates.Year(uint16(1)) == y
@test Dates.Year(int(1)) == y
@test Dates.Year(uint(1)) == y
@test Dates.Year(int64(1)) == y
@test Dates.Year(uint64(1)) == y
@test Dates.Year(int128(1)) == y
@test Dates.Year(uint128(1)) == y
@test Dates.Year(big(1)) == y
@test Dates.Year(BigFloat(1)) == y
@test Dates.Year(float(1)) == y
@test Dates.Year(float32(1)) == y
@test Dates.Year(Rational(1)) == y
@test Dates.Year(complex(1)) == y
@test_throws InexactError Dates.Year(BigFloat(1.2)) == y
@test_throws InexactError Dates.Year(1.2) == y
@test_throws InexactError Dates.Year(float32(1.2)) == y
@test_throws InexactError Dates.Year(3//4) == y
@test_throws InexactError Dates.Year(complex(1.2)) == y
@test_throws InexactError Dates.Year(float16(1.2)) == y
@test Dates.Year(true) == y
@test Dates.Year(false) != y
@test Dates.Year('\x01') == y
@test_throws MethodError Dates.Year(:hey) == y
@test Dates.Year(real(1)) == y
@test_throws ArgumentError Dates.Year(m) == y
@test_throws ArgumentError Dates.Year(w) == y
@test_throws ArgumentError Dates.Year(d) == y
@test_throws ArgumentError Dates.Year(h) == y
@test_throws ArgumentError Dates.Year(mi) == y
@test_throws ArgumentError Dates.Year(s) == y
@test_throws ArgumentError Dates.Year(ms) == y
@test Dates.Year(Dates.Date(2013,1,1)) == Dates.Year(2013)
@test Dates.Year(Dates.DateTime(2013,1,1)) == Dates.Year(2013)
@test typeof(y+m) <: Dates.CompoundPeriod
@test typeof(m+y) <: Dates.CompoundPeriod
@test typeof(y+w) <: Dates.CompoundPeriod
@test typeof(y+d) <: Dates.CompoundPeriod
@test typeof(y+h) <: Dates.CompoundPeriod
@test typeof(y+mi) <: Dates.CompoundPeriod
@test typeof(y+s) <: Dates.CompoundPeriod
@test typeof(y+ms) <: Dates.CompoundPeriod
@test_throws ArgumentError y > m
@test_throws ArgumentError d < w
@test typemax(Dates.Year) == Dates.Year(typemax(Int64))
@test typemax(Dates.Year) + y == Dates.Year(-9223372036854775808)
@test typemin(Dates.Year) == Dates.Year(-9223372036854775808)
#Period-Real arithmetic
@test y + 1 == Dates.Year(2)
@test 1 + y == Dates.Year(2)
@test y + true == Dates.Year(2)
@test true + y == Dates.Year(2)
@test y + '\x01' == Dates.Year(2)
@test '\x01' + y == Dates.Year(2)
@test y + 1.0 == Dates.Year(2)
@test_throws InexactError y + 1.2
@test y + 1f0 == Dates.Year(2)
@test_throws InexactError y + 1.2f0
@test y + BigFloat(1) == Dates.Year(2)
@test_throws InexactError y + BigFloat(1.2)
@test y + 1.0 == Dates.Year(2)
@test_throws InexactError y + 1.2
@test y * 4 == Dates.Year(4)
@test y * 4f0 == Dates.Year(4)
@test_throws InexactError y * 3//4 == Dates.Year(1)
@test div(y,2) == Dates.Year(0)
@test div(2,y) == Dates.Year(2)
@test div(y,y) == Dates.Year(1)
@test y*10 % 5 == Dates.Year(0)
@test 5 % y*10 == Dates.Year(0)
@test (y > 3) == false
@test (4 < y) == false
@test 1 == y
t = [y,y,y,y,y]
@test t .+ Dates.Year(2) == [Dates.Year(3),Dates.Year(3),Dates.Year(3),Dates.Year(3),Dates.Year(3)]
dt = Dates.DateTime(2012,12,21)
# Associativity
test = ((((((((dt + y) - m) + w) - d) + h) - mi) + s) - ms)
@test test == dt + y - m + w - d + h - mi + s - ms
@test test == y - m + w - d + dt + h - mi + s - ms
@test test == dt - m + y - d + w - mi + h - ms + s
@test test == dt + (y - m + w - d + h - mi + s - ms)
@test test == dt + y - m + w - d + (h - mi + s - ms)
@test (dt + Dates.Year(4)) + Dates.Day(1) == dt + (Dates.Year(4) + Dates.Day(1))
@test Dates.Date(2014,1,29) + Dates.Month(1) + Dates.Day(1) + Dates.Month(1) + Dates.Day(1) ==
    Dates.Date(2014,1,29) + Dates.Day(1) + Dates.Month(1) + Dates.Month(1) + Dates.Day(1)
@test Dates.Date(2014,1,29) + Dates.Month(1) + Dates.Day(1) == Dates.Date(2014,1,29) + Dates.Day(1) + Dates.Month(1)
# traits
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
@test one(Dates.Year) == Dates.Year(1)
@test one(Dates.Year(10)) == Dates.Year(1)
@test one(Dates.Month) == Dates.Month(1)
@test one(Dates.Month(10)) == Dates.Month(1)
@test one(Dates.Day) == Dates.Day(1)
@test one(Dates.Day(10)) == Dates.Day(1)
@test one(Dates.Hour) == Dates.Hour(1)
@test one(Dates.Hour(10)) == Dates.Hour(1)
@test one(Dates.Minute) == Dates.Minute(1)
@test one(Dates.Minute(10)) == Dates.Minute(1)
@test one(Dates.Second) == Dates.Second(1)
@test one(Dates.Second(10)) == Dates.Second(1)
@test one(Dates.Millisecond) == Dates.Millisecond(1)
@test one(Dates.Millisecond(10)) == Dates.Millisecond(1)
@test Dates.Year(-1) < Dates.Year(1)
@test !(Dates.Year(-1) > Dates.Year(1))
@test Dates.Year(1) == Dates.Year(1)
@test Dates.Year(1) == 1
@test 1 == Dates.Year(1)
@test (Dates.Year(1) < 1) == false
@test (1 < Dates.Year(1)) == false
@test Dates.Month(-1) < Dates.Month(1)
@test !(Dates.Month(-1) > Dates.Month(1))
@test Dates.Month(1) == Dates.Month(1)
@test Dates.Month(1) == 1
@test 1 == Dates.Month(1)
@test (Dates.Month(1) < 1) == false
@test (1 < Dates.Month(1)) == false
@test Dates.Day(-1) < Dates.Day(1)
@test !(Dates.Day(-1) > Dates.Day(1))
@test Dates.Day(1) == Dates.Day(1)
@test Dates.Day(1) == 1
@test 1 == Dates.Day(1)
@test (Dates.Day(1) < 1) == false
@test (1 < Dates.Day(1)) == false
@test Dates.Hour(-1) < Dates.Hour(1)
@test !(Dates.Hour(-1) > Dates.Hour(1))
@test Dates.Hour(1) == Dates.Hour(1)
@test Dates.Hour(1) == 1
@test 1 == Dates.Hour(1)
@test (Dates.Hour(1) < 1) == false
@test (1 < Dates.Hour(1)) == false
@test Dates.Minute(-1) < Dates.Minute(1)
@test !(Dates.Minute(-1) > Dates.Minute(1))
@test Dates.Minute(1) == Dates.Minute(1)
@test Dates.Minute(1) == 1
@test 1 == Dates.Minute(1)
@test (Dates.Minute(1) < 1) == false
@test (1 < Dates.Minute(1)) == false
@test Dates.Second(-1) < Dates.Second(1)
@test !(Dates.Second(-1) > Dates.Second(1))
@test Dates.Second(1) == Dates.Second(1)
@test Dates.Second(1) == 1
@test 1 == Dates.Second(1)
@test (Dates.Second(1) < 1) == false
@test (1 < Dates.Second(1)) == false
@test Dates.Millisecond(-1) < Dates.Millisecond(1)
@test !(Dates.Millisecond(-1) > Dates.Millisecond(1))
@test Dates.Millisecond(1) == Dates.Millisecond(1)
@test Dates.Millisecond(1) == 1
@test 1 == Dates.Millisecond(1)
@test (Dates.Millisecond(1) < 1) == false
@test (1 < Dates.Millisecond(1)) == false
@test_throws ArgumentError Dates.Year(1) < Dates.Millisecond(1)
@test_throws ArgumentError Dates.Year(1) == Dates.Millisecond(1)

@test Dates.Year("1") == y
@test Dates.Month("1") == m
@test Dates.Week("1") == w
@test Dates.Day("1") == d
@test Dates.Hour("1") == h
@test Dates.Minute("1") == mi
@test Dates.Second("1") == s
@test Dates.Millisecond("1") == ms
@test_throws ErrorException Dates.Year("1.0")
@test Dates.Year(parsefloat("1.0")) == y

dt = Dates.DateTime(2014)
@test typeof(Dates.Year(dt)) <: Dates.Year
@test typeof(Dates.Month(dt)) <: Dates.Month
@test typeof(Dates.Week(dt)) <: Dates.Week
@test typeof(Dates.Day(dt)) <: Dates.Day
@test typeof(Dates.Hour(dt)) <: Dates.Hour
@test typeof(Dates.Minute(dt)) <: Dates.Minute
@test typeof(Dates.Second(dt)) <: Dates.Second
@test typeof(Dates.Millisecond(dt)) <: Dates.Millisecond

# Default values
@test Dates.default(Dates.Year) == y
@test Dates.default(Dates.Month) == m
@test Dates.default(Dates.Week) == w
@test Dates.default(Dates.Day) == d
@test Dates.default(Dates.Hour) == zero(Dates.Hour)
@test Dates.default(Dates.Minute) == zero(Dates.Minute)
@test Dates.default(Dates.Second) == zero(Dates.Second)
@test Dates.default(Dates.Millisecond) == zero(Dates.Millisecond)

# Conversions
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
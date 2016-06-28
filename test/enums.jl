# This file is a part of Julia. License is MIT: http://julialang.org/license

module TestEnums
using Base.Test
@test_throws MethodError convert(Enum, 1.0)

@test_throws ArgumentError eval(:(@enum Foo))

@enum Fruit apple orange kiwi
@test typeof(Fruit) == DataType
@test isbits(Fruit)
@test typeof(apple) <: Fruit <: Enum
@test Int(apple) == 0
@test Int(orange) == 1
@test Int(kiwi) == 2
@test Fruit(0) == apple
@test Fruit(1) == orange
@test Fruit(2) == kiwi
@test_throws ArgumentError Fruit(3)
@test_throws ArgumentError Fruit(-1)
@test Fruit(0x00) == apple
@test Fruit(big(0)) == apple
@test_throws MethodError Fruit(0.0)
@test typemin(Fruit) == apple
@test typemax(Fruit) == kiwi
@test convert(Fruit,0) == apple
@test convert(Fruit,1) == orange
@test convert(Fruit,2) == kiwi
@test_throws ArgumentError convert(Fruit,3)
@test_throws ArgumentError convert(Fruit,-1)
@test convert(UInt8,apple) === 0x00
@test convert(UInt16,orange) === 0x0001
@test convert(UInt128,kiwi) === 0x00000000000000000000000000000002
@test typeof(convert(BigInt,apple)) <: BigInt
@test convert(BigInt,apple) == 0
@test convert(Bool,apple) == false
@test convert(Bool,orange) == true
@test_throws InexactError convert(Bool,kiwi)
@test instances(Fruit) == (apple, orange, kiwi)

f(x::Fruit) = "hey, I'm a Fruit"
@test f(apple) == "hey, I'm a Fruit"

d = Dict(apple=>"apple",orange=>"orange",kiwi=>"kiwi")
@test d[apple] == "apple"
@test d[orange] == "orange"
@test d[kiwi] == "kiwi"
vals = [apple,orange,kiwi]
for (i,enum) in enumerate(instances(Fruit))
    @test enum == vals[i]
end

@enum(QualityofFrenchFood, ReallyGood)
@test length(instances(QualityofFrenchFood)) == 1
@test typeof(ReallyGood) <: QualityofFrenchFood <: Enum
@test Int(ReallyGood) == 0

@enum Binary _zero=0 _one=1 _two=10 _three=11
@test Int(_zero) === 0
@test Int(_one) === 1
@test Int(_two) === 10
@test Int(_three) === 11
@enum Negative _neg1=-1 _neg2=-2
@test Int(_neg1) === -1
@test Int(_neg2) === -2
@test_throws InexactError convert(UInt8, _neg1)
@enum Negative2 _neg5=-5 _neg4 _neg3
@test Int(_neg5) === -5
@test Int(_neg4) === -4
@test Int(_neg3) === -3

@test_throws ArgumentError eval(:(@enum Test1 _zerofp=0.0))
@test_throws ArgumentError eval(:(@enum Test11 _zerofp2=0.5))
@enum Test111 _zerobi=BigInt(1)
@test Integer(_zerobi) == 1

# can't use non-identifiers as enum members
@test_throws ArgumentError eval(:(@enum(Test2, ?)))
@test_throws ArgumentError eval(:(@enum Test22 1=2))

# other Integer types of enum members
# TODO - not yet supported
#=
@enum Test3 _one_Test3=0x01 _two_Test3=0x02 _three_Test3=0x03
@test typeof(_one_Test3.val) <: UInt8
@test _one_Test3.val === 0x01
@test length(instances(Test3)) == 3

@enum Test4 _one_Test4=0x01 _two_Test4=0x0002 _three_Test4=0x03
@test _one_Test4.val === 0x0001
@test _two_Test4.val === 0x0002
@test _three_Test4.val === 0x0003
@test typeof(_one_Test4.val) <: UInt16

@enum Test5 _one_Test5=0x01 _two_Test5=0x00000002 _three_Test5=0x00000003
@test _one_Test5.val === 0x00000001
@test _two_Test5.val === 0x00000002
@test _three_Test5.val === 0x00000003
@test typeof(_one_Test5.val) <: UInt32

@enum Test6 _one_Test6=0x00000000000000000000000000000001 _two_Test6=0x00000000000000000000000000000002
@test _one_Test6.val === 0x00000000000000000000000000000001
@test _two_Test6.val === 0x00000000000000000000000000000002
@test typeof(_one_Test6.val) <: UInt128

@enum Test7 _zero_Test7=0b0 _one_Test7=0b1 _two_Test7=0b10
@test _zero_Test7.val === 0x00
@test _one_Test7.val === 0x01
@test _two_Test7.val === 0x02
@test typeof(_zero_Test7.val) <: UInt8

@test_throws ArgumentError eval(:(@enum Test8 _zero="zero"))
@test_throws ArgumentError eval(:(@enum Test9 _zero='0'))

@enum Test8 _zero_Test8=zero(Int64)
@test typeof(_zero_Test8.val) <: Int64
@test _zero_Test8.val === Int64(0)

@enum Test9 _zero_Test9 _one_Test9=0x01 _two_Test9
@test typeof(_zero_Test9.val) <: Int
@test _zero_Test9.val === 0
@test typeof(_one_Test9.val) <: Int
@test _one_Test9.val === 1
@test typeof(_two_Test9.val) <: Int
@test _two_Test9.val === 2

@enum Test10 _zero_Test10=0x00 _one_Test10 _two_Test10
@test typeof(_zero_Test10.val) <: UInt8
@test _zero_Test10.val === 0x00
@test typeof(_one_Test10.val) <: UInt8
@test _one_Test10.val === 0x01
@test typeof(_two_Test10.val) <: UInt8
@test _two_Test10.val === 0x02
=#

# test macro handles keyword arguments
@enum(Test11, _zero_Test11=2,
              _one_Test11,
              _two_Test11=5,
              _three_Test11)

@test Int(_zero_Test11) == 2
@test Int(_one_Test11) == 3
@test Int(_two_Test11) == 5
@test Int(_three_Test11) == 6

# don't allow enum value to overflow
@test_throws ArgumentError @eval(@enum EnumOvf x=typemax(Int32) y)

# test for unique Enum values
@test_throws ArgumentError eval(:(@enum(Test14, _zero_Test14, _one_Test14, _two_Test14=0)))

@test repr(apple) == "apple::$(string(Fruit)) = 0"
@test string(apple) == "apple"

@test reprmime("text/plain", Fruit) == "Enum $(string(Fruit)):\napple = 0\norange = 1\nkiwi = 2"
@test reprmime("text/plain", orange) == "orange::TestEnums.Fruit = 1"

@enum LogLevel DEBUG INFO WARN ERROR CRITICAL
@test DEBUG < CRITICAL

# serialization
let b = IOBuffer()
    serialize(b, apple)
    seekstart(b)
    @test deserialize(b) === apple
end

end

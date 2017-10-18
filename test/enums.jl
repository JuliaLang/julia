# This file is a part of Julia. License is MIT: https://julialang.org/license

# For curmod_*
include("testenv.jl")

using Test

@test_throws MethodError convert(Enum, 1.0)

macro macrocall(ex)
    @assert Meta.isexpr(ex, :macrocall)
    ex.head = :call
    for i in 2:length(ex.args)
        ex.args[i] = QuoteNode(ex.args[i])
    end
    insert!(ex.args, 3, __module__)
    return esc(ex)
end

@test_throws ArgumentError("no arguments given for Enum Foo") @macrocall(@enum Foo)

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

@test_throws ArgumentError("invalid value for Enum Test1, _zerofp = 0.0=0.0; values must be integers") @macrocall(@enum Test1 _zerofp=0.0)
@test_throws ArgumentError("invalid value for Enum Test11, _zerofp2 = 0.5=0.5; values must be integers") @macrocall(@enum Test11 _zerofp2=0.5)
@enum Test111 _zerobi=BigInt(1)
@test Integer(_zerobi) == 1

# can't use non-identifiers as enum members
@test_throws ArgumentError("""invalid argument for Enum Test2: if x
                                  1
                              else
                                  2
                              end""") @macrocall(@enum Test2  x ? 1 : 2)
@test_throws ArgumentError("invalid argument for Enum Test22: 1 = 2") @macrocall(@enum Test22 1=2)

# other Integer types of enum members
@enum Test3::UInt8 _one_Test3=0x01 _two_Test3=0x02 _three_Test3=0x03
@test Test3.size == 1
@test convert(UInt8, _one_Test3) === 0x01
@test length(instances(Test3)) == 3

@enum Test4::UInt16 _one_Test4=0x01 _two_Test4=0x0002 _three_Test4=0x03
@test Test4.size == 2

@enum Test5::UInt32 _one_Test5=0x01 _two_Test5=0x00000002 _three_Test5=0x00000003
@test Test5.size == 4

@enum Test6::UInt128 _one_Test6=0x00000000000000000000000000000001 _two_Test6=0x00000000000000000000000000000002
@test Test6.size == 16
@test typeof(convert(Integer, _one_Test6)) == UInt128

# enum values must be integers
@test_throws ArgumentError("invalid value for Enum Test7, _zero = \"zero\"=zero; values must be integers") @macrocall(@enum Test7 _zero="zero")
@test_throws ArgumentError("invalid value for Enum Test8, _zero = '0'=0; values must be integers") @macrocall(@enum Test8 _zero='0')
@test_throws ArgumentError("invalid value for Enum Test9, _zero = 0.5=0.5; values must be integers") @macrocall(@enum Test9 _zero=0.5)

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
@test_throws ArgumentError("overflow in value \"y\" of Enum EnumOvf") @macrocall(@enum EnumOvf x=typemax(Int32) y)

# test for unique Enum values
@test_throws ArgumentError("values for Enum Test14 are not unique") @macrocall(@enum(Test14, _zero_Test14, _one_Test14, _two_Test14=0))

@test repr(apple) == "apple::$(string(Fruit)) = 0"
@test string(apple) == "apple"

@test reprmime("text/plain", Fruit) == "Enum $(string(Fruit)):\napple = 0\norange = 1\nkiwi = 2"
@test reprmime("text/plain", orange) == "orange::$(curmod_prefix)Fruit = 1"

@enum LogLevel DEBUG INFO WARN ERROR CRITICAL
@test DEBUG < CRITICAL

# serialization
let b = IOBuffer()
    serialize(b, apple)
    seekstart(b)
    @test deserialize(b) === apple
end
